{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

-- | `olifant lsp` (olifant lsp spec §2): the language server, speaking LSP
-- over stdio. This module only maps LSP messages onto "Olifant.Server",
-- which holds the projects, the open documents and the jobs.
module Olifant.Lsp (runLsp, runLspWithHandles, serverDefinition) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Lens ((%~), (&), (.~), (?~), (^.))
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (Bool, Number, Object, String), object, toJSON, (.=))
import Colog.Core qualified as L
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Functor ((<&>))
import System.IO (Handle, BufferMode (NoBuffering), hFlush, hPutStrLn, hSetBinaryMode, hSetBuffering, stderr, stdin, stdout)
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text qualified as T
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as M
import Data.Proxy (Proxy (..))
import Data.Scientific (toBoundedInteger)
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server hiding (ServerConfig (..))
import Language.LSP.Server qualified as Lsp
import Language.LSP.VFS (virtualFileText, virtualFileVersion)
import Olifant.App (Ports (..))
import Olifant.Generation (Plan (..), ProgressEvent)
import Olifant.Cli.Output (doctorJson, listJson, planJson, showJson)
import Olifant.App (Project (..), ShowFailure (..), doctorChecks, listGenerators, showGenerator, subsOf)
import Olifant.Catalog (describeCatalogError)
import Olifant.Actions (Action (..), Completion (..), Nearby (..))
import Olifant.Rules (Finding (..), FixOffer (..))
import Olifant.Snippet (Snippet (..))
import Olifant.Rules qualified as R
import Olifant.Workspace (Workspace (..))
import Olifant.Server
import System.FilePath (splitDirectories, joinPath)

-- | The server's state behind a lock, its background jobs, the plans of
-- the suggestions it has offered (by id, for `olifant/plan`), and whether
-- the test-only `olifant/debugState` answers.
data Env = Env
  { envPorts :: Ports
  , envState :: MVar ServerState
  , envJobs :: Jobs
  , envOffered :: IORef (Int, M.Map Int Offered)
  , envDebug :: IORef Bool
  }

-- | A suggestion's plan, for the document version it was planned against.
data Offered = Offered {ofPath :: FilePath, ofVersion :: Int, ofSuggestion :: Suggestion}

-- | The server over stdin and stdout (what `olifant lsp` runs).
runLsp :: Ports -> IO Int
runLsp ports = runLspWithHandles ports stdin stdout

-- | The server over two handles, from the client and to it.
runLspWithHandles :: Ports -> Handle -> Handle -> IO Int
runLspWithHandles ports hin hout = do
  mapM_ (\h -> hSetBuffering h NoBuffering >> hSetBinaryMode h True) [hin, hout]
  definition <- serverDefinition ports
  runServerWithConfig
    Lsp.ServerConfig
      { Lsp.ioLogger = problems
      , Lsp.lspLogger = L.hoistLogAction liftIO problems
      , Lsp.inwards = BS.hGetSome hin 65536
      , Lsp.outwards = \out -> BSL.hPut hout out >> hFlush hout
      , Lsp.prepareOutwards = prependHeader . advertise
      , Lsp.parseInwards = parseHeaders
      }
    definition
  where
    -- errors to stderr (never stdout: that is the protocol); a client
    -- without settings for us is not one (the library always asks)
    problems = L.LogAction $ \msg ->
      let text = show (L.getMsg msg)
       in if L.getSeverity msg == L.Error && not ("BadConfigurationResponse" `isInfixOf` text) then hPutStrLn stderr ("olifant lsp: " <> text) else pure ()

-- | The initialize response, with what the protocol library cannot yet
-- say: inline completion (LSP 3.18), which the server answers.
advertise :: BSL.ByteString -> BSL.ByteString
advertise out
  | not ("\"capabilities\"" `BS.isInfixOf` BSL.toStrict out) = out
  | otherwise = case Aeson.decode out of
      Just (Object o)
        | Just (Object result) <- KM.lookup "result" o
        , Just (Object caps) <- KM.lookup "capabilities" result ->
            Aeson.encode (Object (KM.insert "result" (Object (KM.insert "capabilities" (Object (KM.insert "inlineCompletionProvider" (Bool True) caps)) result)) o))
      _ -> out

serverDefinition :: Ports -> IO (ServerDefinition ())
serverDefinition ports = do
  env <- Env ports <$> (newServerState ports >>= newMVar) <*> newJobs <*> newIORef (0, M.empty) <*> newIORef False
  pure
    ServerDefinition
      { parseConfig = \_ _ -> Right ()
      , onConfigChange = const (pure ())
      , defaultConfig = ()
      , configSection = "olifant"
      , doInitialize = \lspEnv req -> configure env (req ^. L.params . L.initializationOptions) >> pure (Right lspEnv)
      , staticHandlers = \_ -> handlers env
      , interpretHandler = \lspEnv -> Iso (runLspT lspEnv) liftIO
      , options = defaultOptions {optTextDocumentSync = Just sync, optServerInfo = Just (ServerInfo "olifant" Nothing), optExecuteCommandCommands = Just ["olifant.doctor"]}
      }
  where
    -- open/close and incremental changes; saves are noticed (regions)
    sync = TextDocumentSyncOptions (Just True) (Just TextDocumentSyncKind_Incremental) (Just False) (Just True) (Just (InR (SaveOptions (Just False))))

-- | `initializationOptions`: `debounce_ms`, `model`, and `debug` (tests).
configure :: Env -> Maybe Value -> IO ()
configure env = \case
  Just (Object o) -> do
    writeIORef (envDebug env) (KM.lookup "debug" o == Just (Bool True))
    modifyMVar_ (envState env) $ \s ->
      pure
        s
          { ssConfig =
              (ssConfig s)
                { scDebounceMs = maybe (scDebounceMs (ssConfig s)) id (KM.lookup "debounce_ms" o >>= \case Number n -> toBoundedInteger n; _ -> Nothing)
                , scModel = case KM.lookup "model" o of Just (String m) -> Just m; _ -> scModel (ssConfig s)
                , scFills = case KM.lookup "fills" o of Just (String f) -> Just (T.unpack f); _ -> scFills (ssConfig s)
                }
          }
  _ -> pure ()

handlers :: Env -> Handlers (LspM ())
handlers env =
  mconcat
    [ notificationHandler SMethod_Initialized (\_ -> pure ())
    , notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
        let item = msg ^. L.params . L.textDocument
        atPath (item ^. L.uri) $ \path -> update env (openDoc path (fromIntegral (item ^. L.version)) (item ^. L.text))
    , notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
        let uri = msg ^. L.params . L.textDocument . L.uri
        getVirtualFile (toNormalizedUri uri) >>= \case
          Nothing -> pure ()
          Just vf -> atPath uri $ \path -> do
            let version = fromIntegral (virtualFileVersion vf)
            update env (changeDoc path version (virtualFileText vf))
            -- what follows from this edit, once typing settles
            case lastEditedLine (msg ^. L.params . L.contentChanges) of
              Nothing -> pure ()
              Just line -> do
                lspEnv <- getLspEnv
                liftIO (suggestLater env lspEnv path version line)
    , notificationHandler SMethod_TextDocumentDidClose $ \msg ->
        atPath (msg ^. L.params . L.textDocument . L.uri) (update env . closeDoc)
    , -- regions follow a save: the other files' through applyEdit (the saved
      -- file's came with willSaveWaitUntil)
      notificationHandler SMethod_TextDocumentDidSave $ \msg ->
        atPath (msg ^. L.params . L.textDocument . L.uri) $ \path -> do
          edits <- liftIO (withSnapshot env (regionEdits path))
          case filter ((/= path) . fePath) edits of
            [] -> pure ()
            others -> void (sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams (Just "Olifant: regions") (workspaceEdit others)) (\_ -> pure ()))
    , requestHandler SMethod_TextDocumentWillSaveWaitUntil $ \req respond -> do
        edits <- case uriToFilePath (req ^. L.params . L.textDocument . L.uri) of
          Nothing -> pure []
          Just path -> filter ((== path) . fePath) <$> liftIO (withSnapshot env (regionEdits path))
        respond (Right (InL [TextEdit (toRange (feRange e)) (feText e) | e <- edits]))
    , -- the generator picker: every generator with its inputs, and one
      -- generator's choices given what is set so far
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/generators")) $ \req respond ->
        liftIO (projectOfRequest env (req ^. L.params)) >>= \case
          Nothing -> respond (Right (listJson True (const []) (const M.empty) [] []))
          Just project -> do
            (entries, broken, views) <- liftIO (listGenerators (envPorts env) project True)
            respond (Right (listJson True (subsOf (prCatalog project)) views entries broken))
    , requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/choices")) $ \req respond -> do
        let params = req ^. L.params
            field k = case params of Object o -> KM.lookup k o; _ -> Nothing
            sets = case field "sets" of Just (Object m) -> M.fromList [(Key.toText k, v) | (k, String v) <- KM.toList m]; _ -> M.empty
        liftIO (projectOfRequest env params) >>= \case
          Nothing -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) "the document is in no Olifant project" Nothing))
          Just project -> case field "generator" of
            Just (String ref) ->
              liftIO (showGenerator (envPorts env) project ref sets) >>= \case
                Left (ShowCatalog err) -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) (describeCatalogError err) Nothing))
                Left (ShowInvalid why) -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
                Right (e, vs) -> respond (Right (showJson (M.fromList vs) (subsOf (prCatalog project) e) e))
            _ -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/choices needs a generator" Nothing))
    , requestHandler SMethod_WorkspaceExecuteCommand $ \req respond -> case req ^. L.params . L.command of
        "olifant.doctor" -> do
          let uri = case req ^. L.params . L.arguments of
                Just (String u : _) -> Just u
                _ -> Nothing
          found <- liftIO (maybe (pure Nothing) (\u -> projectOfRequest env (object ["textDocument" .= object ["uri" .= u]])) uri)
          case found of
            Nothing -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) "olifant.doctor needs a document of an Olifant project" Nothing))
            Just project -> liftIO (doctorChecks (envPorts env) project) >>= respond . Right . InL . doctorJson
        other -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) ("no command " <> other) Nothing))
    , notificationHandler SMethod_WorkspaceDidChangeWorkspaceFolders (\_ -> pure ())
    , notificationHandler SMethod_WorkspaceDidChangeConfiguration (\_ -> pure ())
    , -- a project's `.olifant/` changed: reload that project
      notificationHandler SMethod_WorkspaceDidChangeWatchedFiles $ \msg ->
        forM_ (msg ^. L.params . L.changes) $ \ev ->
          atPath (ev ^. L.uri) $ \path -> case projectOfConfig path of
            Just root -> liftIO (modifyMVar_ (envState env) (catalogChanged root))
            Nothing -> pure ()
    , -- rule findings, pulled (spec §2)
      requestHandler SMethod_TextDocumentDiagnostic $ \req respond -> do
        items <- maybe (pure []) (liftIO . fmap (map fst) . diagnosticsOf env) (uriToFilePath (req ^. L.params . L.textDocument . L.uri))
        respond (Right (DocumentDiagnosticReport (InL (RelatedFullDocumentDiagnosticReport AString Nothing items Nothing))))
    , -- the fixes of the findings on the lines asked about, resolved lazily
      requestHandler SMethod_TextDocumentCodeAction $ \req respond -> do
        let Range (Position from _) (Position to _) = req ^. L.params . L.range
        actions <- case uriToFilePath (req ^. L.params . L.textDocument . L.uri) of
          Nothing -> pure []
          Just path -> liftIO (concat <$> sequence [fixActions env path (fromIntegral from + 1) (fromIntegral to + 1), holeActions env path (fromIntegral from + 1), cursorActions env path (req ^. L.params . L.range . L.start)])
        respond (Right (InL (map InR actions)))
    , requestHandler SMethod_CodeActionResolve $ \req respond -> do
        let action = req ^. L.params
        let failed why = respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
        case (fixData =<< (action ^. L.data_), actionData =<< (action ^. L.data_)) of
          (Just (path, rule, line, n, sets), _) ->
            liftIO (withSnapshot env (fixEdits path rule line n sets)) >>= \case
              Left why -> failed why
              Right edits -> respond (Right (action & L.edit ?~ workspaceEdit edits))
          (_, Just (path, line, generator, sets)) ->
            planProgress (action ^. L.title) (\progress -> withSnapshot env (actionPlanWith progress True path line generator sets)) >>= \case
              Left why -> failed why
              -- a snippet's body (tab stops) rides along for editors that expand it
              Right (root, plan) -> do
                s <- liftIO (readMVar (envState env))
                respond (Right (action & L.edit ?~ workspaceEdit (editsOfPlan s root plan) & L.data_ %~ fmap (withSnippet (planSnippet plan))))
          _ -> case holeData =<< (action ^. L.data_) of
            Just (path, line) ->
              planProgress "Fill with the model" (\progress -> withSnapshot env (fillHoleEdits progress path line)) >>= \case
                Left why -> failed why
                Right edits -> respond (Right (action & L.edit ?~ workspaceEdit edits))
            Nothing -> respond (Right action)
    , -- ⚡ at the end of each line with actions; the hint opens the menu there
      requestHandler SMethod_TextDocumentInlayHint $ \req respond -> do
        let Range (Position from _) (Position to _) = req ^. L.params . L.range
            uri = req ^. L.params . L.textDocument . L.uri
        hints <- case uriToFilePath uri of
          Nothing -> pure []
          Just path -> liftIO $ do
            (lines', _) <- withSnapshot env (hintsFor path)
            text <- documentText env path
            let ls = T.splitOn "\n" text
            pure
              [ InlayHint (Position (fromIntegral l0) (fromIntegral (utf16 (lineAt ls l0)))) (InR [InlayHintLabelPart label Nothing Nothing (Just (Command label "olifant.showActions" (Just [toJSON uri, toJSON (Position (fromIntegral l0) 0)])))]) Nothing Nothing Nothing (Just True) Nothing Nothing
              | LineHint line acts <- lines'
              , let l0 = line - 1
              , fromIntegral from <= l0 && l0 <= fromIntegral to
              , let plain = [a | (a, False) <- acts]
              , first : rest <- [plain]
              , let label = "⚡ " <> actLabel first <> (if null rest then "" else " +" <> T.pack (show (length rest)))
              ]
        respond (Right (InL hints))
    , -- the generators that nearly apply in this file, and why not
      requestHandler SMethod_TextDocumentHover $ \req respond -> do
        let Position l _ = req ^. L.params . L.position
        hover <- case uriToFilePath (req ^. L.params . L.textDocument . L.uri) of
          Nothing -> pure Nothing
          Just path -> liftIO $ do
            (lines', near) <- withSnapshot env (hintsFor path)
            let here = concat [map fst acts | LineHint line acts <- lines', line == fromIntegral l + 1]
                text = T.intercalate "\n\n" (["⚡ " <> actLabel a <> " (" <> actGenerator a <> ")" | a <- here] ++ ["nearby: " <> nbGenerator n <> " — " <> nbWhy n | n <- near])
            pure (if T.null text then Nothing else Just (Hover (InL (MarkupContent MarkupKind_Markdown text)) Nothing))
        respond (Right (maybe (InR Null) InL hover))
    , -- snippets, each with its body placed at the cursor (tab stops)
      requestHandler SMethod_TextDocumentCompletion $ \req respond -> do
        items <- case uriToFilePath (req ^. L.params . L.textDocument . L.uri) of
          Nothing -> pure []
          Just path -> liftIO (snippetItems env path (req ^. L.params . L.position))
        respond (Right (InL items))
    , -- the chosen snippet, planned again with the model filling its holes
      requestHandler SMethod_CompletionItemResolve $ \req respond -> do
        let item = req ^. L.params
        case actionData =<< (item ^. L.data_) of
          Nothing -> respond (Right item)
          Just (path, line, generator, sets) ->
            liftIO (withSnapshot env (actionEdits True path line generator sets)) >>= \case
              Right (_, Just sn) -> respond (Right (item & L.textEdit %~ fmap (\case InL e -> InL (e & L.newText .~ snBody sn); InR e -> InR (e & L.newText .~ snBody sn))))
              _ -> respond (Right item)
    , -- the snippet anticipated here, unasked (LSP 3.18 inline completion)
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "textDocument/inlineCompletion")) $ \req respond -> do
        let params = req ^. L.params
            found = do
              Object o <- pure params
              Object doc <- KM.lookup "textDocument" o
              String uri <- KM.lookup "uri" doc
              Object pos <- KM.lookup "position" o
              Number l <- KM.lookup "line" pos
              Number c <- KM.lookup "character" pos
              (,) <$> uriToFilePath (Uri uri) <*> (Position <$> toBoundedInteger l <*> toBoundedInteger c)
        items <- case found of
          Nothing -> pure []
          Just (path, position) -> liftIO (ghostItems env path position)
        respond (Right (object ["items" .= items]))
    , -- an offered suggestion's plan, while its document is as it was
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/plan")) $ \req respond -> do
        let wanted = case req ^. L.params of
              Object o | Just (Number n) <- KM.lookup "planId" o -> toBoundedInteger n
              _ -> Nothing
        offered <- liftIO ((\(_, m) -> (`M.lookup` m) =<< wanted) <$> readIORef (envOffered env))
        s <- liftIO (readMVar (envState env))
        case offered of
          Nothing -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) "no such plan (offered plans last until their document changes)" Nothing))
          Just o
            | fmap fst (M.lookup (ofPath o) (ssDocs s)) /= Just (ofVersion o) -> respond (Left (TResponseError (InL LSPErrorCodes_ContentModified) "the document changed since this was suggested" Nothing))
            | otherwise ->
                let sg = ofSuggestion o
                 in respond (Right (object ["plan" .= planJson False (sgPlan sg), "edit" .= workspaceEdit (editsOfPlan s (sgRoot sg) (sgPlan sg))]))
    , -- the holes left in the project of a document: where, which, what for
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/holes")) $ \req respond -> do
        let path = case req ^. L.params of
              Object o | Just (Object doc) <- KM.lookup "textDocument" o, Just (String uri) <- KM.lookup "uri" doc -> uriToFilePath (Uri uri)
              _ -> Nothing
        holes <- maybe (pure []) (liftIO . withSnapshot env . holesFor) path
        respond (Right (object ["holes" .= [object ["path" .= hoPath h, "line" .= hoLine h, "name" .= hoName h, "kind" .= hoKind h, "instruction" .= hoInstruction h] | h <- holes]]))
    , requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/debugState")) $ \_ respond ->
        liftIO (readIORef (envDebug env)) >>= \case
          False -> respond (Left (TResponseError (InR ErrorCodes_MethodNotFound) "olifant/debugState is for tests (initializationOptions.debug)" Nothing))
          True -> liftIO (debugState env) >>= respond . Right
    ]

-- | Each open document, its version and its project (loading it).
debugState :: Env -> IO Value
debugState env = modifyMVar (envState env) $ \s0 -> do
  let docs = M.toList (ssDocs s0)
  (s, rows) <- foldr (\(path, (version, _)) acc -> do
                         (s', rs) <- acc
                         (s'', found) <- projectFor path s'
                         pure (s'', object ["path" .= path, "version" .= version, "project" .= fmap fst found] : rs))
                     (pure (s0, [])) docs
  pure (s, object ["documents" .= rows])

-- | The project of the document a request names (`textDocument.uri`).
projectOfRequest :: Env -> Value -> IO (Maybe Project)
projectOfRequest env params = case params of
  Object o
    | Just (Object doc) <- KM.lookup "textDocument" o
    , Just (String uri) <- KM.lookup "uri" doc
    , Just path <- uriToFilePath (Uri uri) ->
        fmap snd <$> withSnapshot env (projectFor path)
  _ -> pure Nothing

toRange :: ((Int, Int), (Int, Int)) -> Range
toRange ((l1, c1), (l2, c2)) = Range (Position (fromIntegral l1) (fromIntegral c1)) (Position (fromIntegral l2) (fromIntegral c2))

-- | The line (1-based) an edit ends on, in the text after it: where to
-- look for what follows. None for a whole-document change.
lastEditedLine :: [TextDocumentContentChangeEvent] -> Maybe Int
lastEditedLine changes = case reverse changes of
  TextDocumentContentChangeEvent (InL partial) : _ ->
    let Range (Position l _) _ = partial ^. L.range
     in Just (fromIntegral l + 1 + T.count "\n" (partial ^. L.text))
  _ -> Nothing

-- | After the debounce (a newer edit of the file cancels it), plan what
-- follows from the edit and push it, unless the document has moved on.
suggestLater :: Env -> LanguageContextEnv () -> FilePath -> Int -> Int -> IO ()
suggestLater env lspEnv path version line = do
  debounce <- scDebounceMs . ssConfig <$> readMVar (envState env)
  void . submit (envJobs env) ("suggest " <> T.pack path) debounce $ \_ -> do
    found <- withSnapshot env (suggestionFor path line)
    current <- fmap fst . M.lookup path . ssDocs <$> readMVar (envState env)
    case found of
      Just sg | current == Just version -> do
        n <- atomicModifyIORef' (envOffered env) (\(k, m) -> ((k + 1, M.insert (k + 1) (Offered path version sg) (M.filter ((/= path) . ofPath) m)), k + 1))
        runLspT lspEnv . sendNotification (SMethod_CustomMethod (Proxy :: Proxy "olifant/suggestion")) $
          object
            [ "uri" .= filePathToUri path
            , "version" .= version
            , "kind" .= sgKind sg
            , "label" .= sgLabel sg
            , "summary" .= sgSummary sg
            , "line" .= sgLine sg
            , "lines" .= sgLines sg
            , "planId" .= n
            ]
      _ -> pure ()

-- | Run against the state as it is now, without holding it: what the work
-- loaded (projects) is kept; documents changed meanwhile are not undone.
withSnapshot :: Env -> (ServerState -> IO (ServerState, a)) -> IO a
withSnapshot env work = do
  before <- readMVar (envState env)
  (after, a) <- work before
  modifyMVar_ (envState env) (\now -> pure now {ssProjects = M.union (ssProjects now) (ssProjects after)})
  pure a

-- | A file's findings as diagnostics (with the finding each is).
diagnosticsOf :: Env -> FilePath -> IO [(Diagnostic, Finding)]
diagnosticsOf env path = do
  fs <- withSnapshot env (findingsFor path)
  text <- documentText env path
  let ls = T.splitOn "\n" text
      lineText n = fromMaybe "" (listToMaybe (drop (n - 1) ls))
      position (l, c) = let (l', c') = editorPosition (lineText l) (l, c) in Position (fromIntegral l') (fromIntegral c')
      range f = maybe (Range (Position 0 0) (Position 0 0)) (\(a, b) -> Range (position a) (position b)) (fRange f)
  pure
    [ (Diagnostic (range f) (Just sev) (Just (InR (fRule f))) Nothing (Just "olifant") (fMessage f) Nothing Nothing Nothing, f)
    | f <- fs
    , Just sev <- [severityOf (fSeverity f)]
    ]
  where
    severityOf = \case
      R.Error -> Just DiagnosticSeverity_Error
      R.Warn -> Just DiagnosticSeverity_Warning
      R.Hint -> Just DiagnosticSeverity_Hint
      R.Off -> Nothing

-- | Each fix of the findings on lines @from@–@to@ (1-based), as a quickfix
-- to resolve: its data says which fix, with the choices it still needs.
fixActions :: Env -> FilePath -> Int -> Int -> IO [CodeAction]
fixActions env path from to = do
  diagnostics <- diagnosticsOf env path
  version <- fmap fst . M.lookup path . ssDocs <$> readMVar (envState env)
  let here = [(d, f, l1) | (d, f) <- diagnostics, let (l1, l2) = maybe (1, 1) (\((a, _), (b, _)) -> (a, b)) (fRange f), l1 <= to, from <= l2]
      actions =
        [ CodeAction
            { _title = fromMaybe (foGenerator o) (foLabel o)
            , _kind = Just CodeActionKind_QuickFix
            , _diagnostics = Just [d]
            , _isPreferred = Just (i == 1)
            , _disabled = Nothing
            , _edit = Nothing
            , _command = Nothing
            , _data_ = Just (object ["kind" .= ("fix" :: T.Text), "path" .= path, "rule" .= fRule f, "line" .= line, "index" .= i, "sets" .= M.fromList (foSets o), "missing" .= map fst (foChoices o), "choices" .= M.fromList (foChoices o), "version" .= version])
            }
        | (d, f, line) <- here
        , (i, o) <- zip [1 :: Int ..] (fFixes f)
        ]
  pure actions

-- | The ⚡ actions at a cursor, to resolve: each one's data says which
-- generator, what it sets, what it still needs (and the choices for it).
cursorActions :: Env -> FilePath -> Position -> IO [CodeAction]
cursorActions env path (Position l c) = do
  text <- documentText env path
  let lineText = fromMaybe "" (listToMaybe (drop (fromIntegral l) (T.splitOn "\n" text)))
      (line, col) = bytePosition lineText (fromIntegral l, fromIntegral c)
  acts <- withSnapshot env (actionsFor path (line, col))
  version <- fmap fst . M.lookup path . ssDocs <$> readMVar (envState env)
  pure
    [ CodeAction
        { _title = actLabel a
        , _kind = Just CodeActionKind_RefactorRewrite
        , _diagnostics = Nothing
        , _isPreferred = Nothing
        , _disabled = Nothing
        , _edit = Nothing
        , _command = Nothing
        , _data_ = Just (object ["kind" .= ("action" :: T.Text), "path" .= path, "generator" .= actGenerator a, "sets" .= M.fromList (actSets a), "missing" .= actMissing a, "choices" .= M.fromList (actChoices a), "snippet" .= snippet, "line" .= line, "col" .= col, "version" .= version])
        }
    | (a, snippet) <- acts
    ]

-- | Completion items for the snippets at a cursor: each replaces the
-- cursor's line with its body (tab stops), filtered by what the line holds.
snippetItems :: Env -> FilePath -> Position -> IO [CompletionItem]
snippetItems env path (Position l c) = do
  text <- documentText env path
  let lineText = lineAt (T.splitOn "\n" text) (fromIntegral l)
      (line, col) = bytePosition lineText (fromIntegral l, fromIntegral c)
      range = Range (Position l 0) (Position l (fromIntegral (utf16 lineText)))
  snippets <- withSnapshot env (completionsFor path (line, col))
  pure
    [ CompletionItem
        { _label = cpLabel cp
        , _labelDetails = Nothing
        , _kind = Just CompletionItemKind_Snippet
        , _tags = Nothing
        , _detail = Just (cpGenerator cp)
        , _documentation = Nothing
        , _deprecated = Nothing
        , _preselect = Nothing
        , _sortText = Nothing
        , _filterText = Just (if T.null (T.strip lineText) then cpLabel cp else lineText)
        , _insertText = Nothing
        , _insertTextFormat = Just InsertTextFormat_Snippet
        , _insertTextMode = Nothing
        , _textEdit = Just (InL (TextEdit range (snBody sn)))
        , _textEditText = Nothing
        , _additionalTextEdits = Nothing
        , _commitCharacters = Nothing
        , _command = Nothing
        , _data_ = Just (object ["kind" .= ("action" :: T.Text), "path" .= path, "generator" .= cpGenerator cp, "sets" .= M.fromList (cpSets cp), "line" .= line])
        }
    | (cp, sn) <- snippets
    ]

-- | The inline completion at a cursor: the anticipated snippet, unfilled,
-- replacing the cursor's line.
ghostItems :: Env -> FilePath -> Position -> IO [Value]
ghostItems env path (Position l c) = do
  text <- documentText env path
  let lineText = lineAt (T.splitOn "\n" text) (fromIntegral l)
      (line, col) = bytePosition lineText (fromIntegral l, fromIntegral c)
  withSnapshot env (ghostFor path (line, col)) <&> \case
    Nothing -> []
    Just sn ->
      [ object
          [ "insertText" .= object ["kind" .= ("snippet" :: T.Text), "value" .= snBody sn]
          , "range" .= Range (Position l 0) (Position l (fromIntegral (utf16 lineText)))
          , "filterText" .= lineText
          ]
      ]

-- | "Fill with the model" on a line holding a hole.
holeActions :: Env -> FilePath -> Int -> IO [CodeAction]
holeActions env path line =
  withSnapshot env (holeAt path line) <&> \case
    Nothing -> []
    Just (_, _, h) ->
      [ CodeAction
          { _title = "Fill with the model"
          , _kind = Just CodeActionKind_RefactorRewrite
          , _diagnostics = Nothing
          , _isPreferred = Nothing
          , _disabled = Nothing
          , _edit = Nothing
          , _command = Nothing
          , _data_ = Just (object ["kind" .= ("hole" :: T.Text), "path" .= path, "line" .= line, "name" .= hoName h])
          }
      ]

holeData :: Value -> Maybe (FilePath, Int)
holeData = \case
  Object o | KM.lookup "kind" o == Just (String "hole") -> do
    String path <- KM.lookup "path" o
    Number line <- KM.lookup "line" o
    (T.unpack path,) <$> toBoundedInteger line
  _ -> Nothing

-- | Work under a progress report (checking, asking the model, testing).
planProgress :: T.Text -> ((ProgressEvent -> IO ()) -> IO a) -> LspM () a
planProgress title work =
  withIndefiniteProgress title Nothing Cancellable $ \report -> do
    lspEnv <- getLspEnv
    liftIO (work (runLspT lspEnv . report . describeProgress))

-- | An action's data: the file, the cursor's line, the generator, and the
-- inputs set (its own and the choices made).
actionData :: Value -> Maybe (FilePath, Int, T.Text, [(T.Text, T.Text)])
actionData = \case
  Object o | KM.lookup "kind" o == Just (String "action") -> do
    String path <- KM.lookup "path" o
    Number line <- KM.lookup "line" o
    String generator <- KM.lookup "generator" o
    (T.unpack path,,generator,setsIn o) <$> toBoundedInteger line
  _ -> Nothing

setsIn :: KM.KeyMap Value -> [(T.Text, T.Text)]
setsIn o = case KM.lookup "sets" o of
  Just (Object m) -> [(Key.toText k, v) | (k, String v) <- KM.toList m]
  _ -> []

-- | The snippet a plan placed at the cursor, in an action's data.
withSnippet :: Maybe Snippet -> Value -> Value
withSnippet snippet = \case
  Object o | Just sn <- snippet -> Object (KM.insert "placed" (object ["line" .= snLine sn, "body" .= snBody sn, "ghost" .= snGhost sn]) o)
  other -> other

-- | A fix's data: the file, rule, line, which fix, and the choices made.
fixData :: Value -> Maybe (FilePath, T.Text, Int, Int, [(T.Text, T.Text)])
fixData = \case
  Object o | KM.lookup "kind" o == Just (String "fix") -> do
    String path <- KM.lookup "path" o
    String rule <- KM.lookup "rule" o
    Number line <- KM.lookup "line" o
    Number n <- KM.lookup "index" o
    (,,,,) (T.unpack path) rule <$> toBoundedInteger line <*> toBoundedInteger n <*> pure (setsIn o)
  _ -> Nothing

-- | Edits as one workspace edit: new files created, then each change at
-- its document's version (the editor refuses it if the text moved on). An
-- edit of several files asks to be confirmed (spec §2).
workspaceEdit :: [FileEdit] -> WorkspaceEdit
workspaceEdit edits = WorkspaceEdit Nothing (Just (concatMap change edits)) annotations
  where
    several = length edits > 1
    plan = ChangeAnnotationIdentifier "olifant-plan"
    annotations
      | several = Just (M.singleton plan (ChangeAnnotation "Olifant plan" (Just True) (Just (T.pack (show (length edits)) <> " files"))))
      | otherwise = Nothing
    change e =
      let uri = filePathToUri (fePath e)
          ((l1, c1), (l2, c2)) = feRange e
          range = Range (Position (fromIntegral l1) (fromIntegral c1)) (Position (fromIntegral l2) (fromIntegral c2))
          text = if several then InR (AnnotatedTextEdit range (feText e) plan) else InL (TextEdit range (feText e))
          edit = TextDocumentEdit (OptionalVersionedTextDocumentIdentifier uri (maybe (InR Null) (InL . fromIntegral) (feVersion e))) [text]
       in [InR (InL (CreateFile (if several then Just plan else Nothing) AString uri Nothing)) | feCreate e] ++ [InL edit]

lineAt :: [T.Text] -> Int -> T.Text
lineAt ls n = fromMaybe "" (listToMaybe (drop n ls))

utf16 :: T.Text -> Int
utf16 = T.foldl' (\n c -> n + if fromEnum c > 0xFFFF then 2 else 1) 0

-- | A document's text: the editor's when it is open, else disk's.
documentText :: Env -> FilePath -> IO T.Text
documentText env path = do
  s <- readMVar (envState env)
  case M.lookup path (ssDocs s) of
    Just (_, text) -> pure text
    Nothing -> fromMaybe "" <$> readText (pFiles (ssPorts s)) path

update :: MonadIO m => Env -> (ServerState -> ServerState) -> m ()
update env f = liftIO (modifyMVar_ (envState env) (pure . f))

atPath :: Applicative m => Uri -> (FilePath -> m ()) -> m ()
atPath uri act = maybe (pure ()) act (uriToFilePath uri)

-- | `/root/.olifant/config.toml` → `/root`.
projectOfConfig :: FilePath -> Maybe FilePath
projectOfConfig path
  | "/.olifant/" `isInfixOf` path = Just (joinPath (takeWhile (/= ".olifant") (splitDirectories path)))
  | otherwise = Nothing
