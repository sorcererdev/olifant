{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

-- | `olifant lsp` (olifant lsp spec §2): the language server, speaking LSP
-- over stdio. This module only maps LSP messages onto "Olifant.Server",
-- which holds the projects, the open documents and the jobs.
module Olifant.Lsp (runLsp, runLspWithHandles, serverDefinition) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (SomeAsyncException, SomeException, catch, displayException, finally, fromException, throwIO)
import Olifant.Process (Cancel, neverCancel, newCancel)
import Control.Lens ((%~), (&), (.~), (?~), (^.))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (Bool, Number, Object, String), object, toJSON, (.=))
import Colog.Core qualified as L
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text.Encoding qualified as TE
import Data.Functor ((<&>))
import System.IO (Handle, BufferMode (NoBuffering), hFlush, hPutStrLn, hSetBinaryMode, hSetBuffering, stderr, stdin, stdout)
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text qualified as T
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
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
import Olifant.Output (doctorJson, fileActionsJson, listJson, planJson, showJson)
import Olifant.App (Project (..), ShowFailure (..), doctorChecks, listGenerators, showGenerator, subsOf)
import Olifant.Catalog (catalogPackRoots, describeCatalogError)
import Olifant.Actions (Action (..), Completion (..), Nearby (..))
import Olifant.Rules (Finding (..), FixOffer (..))
import Olifant.Snippet (Snippet (..))
import Olifant.Rules qualified as R
import Olifant.Workspace (Workspace (..))
import Olifant.Server

-- | The server's state behind a lock, its background jobs, the plans of
-- the suggestions it has offered (by id, for `olifant/plan`), and whether
-- the test-only `olifant/debugState` answers.
data Env = Env
  { envPorts :: Ports
  , envState :: MVar ServerState
  , envJobs :: Jobs
  , envOffered :: IORef (Int, M.Map Int Offered)
  , envDebug :: IORef Bool
  , -- | the globs the client already watches for us
    envWatched :: IORef (S.Set T.Text)
  , -- | the requests running in the background, by id: how to cancel each
    envRunning :: IORef (M.Map T.Text (IO ()))
  , -- | the token of the work this Env was handed to (a job, a request);
    -- the work's snapshots carry it (`ssCancel`)
    envCancel :: Cancel
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
  env <- Env ports <$> (newServerState ports >>= newMVar) <*> newJobs <*> newIORef (0, M.empty) <*> newIORef False <*> newIORef S.empty <*> newIORef M.empty <*> pure neverCancel
  pure
    ServerDefinition
      { parseConfig = \_ _ -> Right ()
      , onConfigChange = const (pure ())
      , defaultConfig = ()
      , configSection = "olifant"
      , doInitialize = \lspEnv req -> configure env (req ^. L.params . L.initializationOptions) >> pure (Right lspEnv)
      , staticHandlers = \_ -> guarded (handlers env)
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

-- | Every handler, kept from killing the server: lsp runs them on its
-- message loop without catching, so an IO exception (a file deleted while
-- read, an unreadable folder) would end the process (review I4). A
-- request answers an internal error instead; a notification is logged.
guarded :: Handlers (LspM ()) -> Handlers (LspM ())
guarded = mapHandlers onRequest onNotification
  where
    onRequest h req respond = rescue (h req respond) (\why -> respond (Left (TResponseError (InR ErrorCodes_InternalError) why Nothing)))
    onNotification h msg = rescue (h msg) (\why -> liftIO (hPutStrLn stderr ("olifant lsp: " <> T.unpack why)))
    rescue :: LspM () () -> (T.Text -> LspM () ()) -> LspM () ()
    rescue work recover = do
      lspEnv <- getLspEnv
      liftIO $
        runLspT lspEnv work `catch` \(e :: SomeException) -> case fromException e :: Maybe SomeAsyncException of
          Just _ -> throwIO e
          Nothing -> runLspT lspEnv (recover (T.pack (displayException e)))

handlers :: Env -> Handlers (LspM ())
handlers env =
  mconcat
    [ -- the client gave up on a request: stop its work
      notificationHandler SMethod_CancelRequest $ \msg -> do
        let key = case msg ^. L.params . L.id of InL n -> T.pack (show n); InR t -> t
        liftIO (readIORef (envRunning env)) >>= maybe (pure ()) liftIO . M.lookup key
    , -- watch every project's `.olifant/`; packs are watched as projects load
      notificationHandler SMethod_Initialized (\_ -> watch env [GlobPattern (InL (Pattern "**/.olifant/**"))])
    , notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
        let item = msg ^. L.params . L.textDocument
        atPath (item ^. L.uri) $ \path -> do
          update env (openDoc path (fromIntegral (item ^. L.version)) (item ^. L.text))
          -- its project loads now; watch its files (config, generators, the
          -- code agents and the CLI change on disk) and its packs, each
          -- relative to its own folder: an editor scopes a plain glob to the
          -- workspace folders it had when it registered (review I3, I8)
          found <- liftIO (withSnapshot env (projectFor path))
          watch env [everything r | Just (root, p) <- [found], r <- root : catalogPackRoots (prCatalog p)]
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
          update env fileSaved
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
    , -- rule findings, pulled (spec §2)
      requestHandler SMethod_TextDocumentDiagnostic $ \req respond -> do
        items <- case uriToFilePath (req ^. L.params . L.textDocument . L.uri) of
          Nothing -> pure []
          Just path -> liftIO $ do
            broken <- withSnapshot env (brokenFor path)
            -- a project that does not load says why, on top of every file (review I6)
            let why w = Diagnostic (Range (Position 0 0) (Position 0 0)) (Just DiagnosticSeverity_Error) (Just (InR "config")) Nothing (Just "olifant") ("this project's Olifant configuration does not load: " <> w) Nothing Nothing Nothing
            (maybe [] (pure . why) broken ++) . map fst <$> diagnosticsOf env path
        respond (Right (DocumentDiagnosticReport (InL (RelatedFullDocumentDiagnosticReport AString Nothing items Nothing))))
    , -- the fixes of the findings on the lines asked about, resolved lazily
      requestHandler SMethod_TextDocumentCodeAction $ \req respond -> do
        let Range (Position from _) (Position to _) = req ^. L.params . L.range
        actions <- case uriToFilePath (req ^. L.params . L.textDocument . L.uri) of
          Nothing -> pure []
          Just path -> liftIO (concat <$> sequence [fixActions env path (fromIntegral from + 1) (fromIntegral to + 1), holeActions env path (fromIntegral from + 1), cursorActions env path (req ^. L.params . L.range . L.start)])
        respond (Right (InL (map InR actions)))
    , -- planned on its own thread: a model fill or a test run takes seconds,
      -- and the server reads on meanwhile (other requests, progress)
      requestHandler SMethod_CodeActionResolve $ \req respond -> inBackground env req respond $ \job -> do
        let action = req ^. L.params
        let failed why = respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
        case (fixData =<< (action ^. L.data_), actionData =<< (action ^. L.data_)) of
          (Just (path, rule, line, n, sets), _) ->
            planProgress (action ^. L.title) (\progress -> withPlanned job (fixPlan True progress path rule line n sets)) >>= \case
              Left why -> failed why
              Right planned -> respond (Right (resolved action planned))
          (_, Just (path, line, generator, sets)) ->
            planProgress (action ^. L.title) (\progress -> withPlanned job (actionPlanWith progress True path line generator sets)) >>= \case
              Left why -> failed why
              -- a snippet's body (tab stops) rides along for editors that expand it
              Right planned@(_, (_, plan)) -> respond (Right (resolved action planned & L.data_ %~ fmap (withSnippet (planSnippet plan))))
          _ -> case holeData =<< (action ^. L.data_) of
            Just (path, line) ->
              planProgress "Fill with the model" (\progress -> withPlanned job (fillHolePlan progress path line)) >>= \case
                Left why -> failed why
                Right planned -> respond (Right (resolved action planned))
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
      requestHandler SMethod_CompletionItemResolve $ \req respond -> inBackground env req respond $ \job -> do
        let item = req ^. L.params
        case actionData =<< (item ^. L.data_) of
          Nothing -> respond (Right item)
          Just (path, line, generator, sets) ->
            liftIO (withSnapshot job (actionEdits True path line generator sets)) >>= \case
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
    , -- every action in a file, where each applies (durandal's hints read
      -- this; other editors get inlay hints)
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/fileActions")) $ \req respond -> do
        let path = case req ^. L.params of
              Object o | Just (Object doc) <- KM.lookup "textDocument" o, Just (String uri) <- KM.lookup "uri" doc -> uriToFilePath (Uri uri)
              _ -> Nothing
        found <- maybe (pure Nothing) (liftIO . withSnapshot env . fileActionsFor) path
        respond (Right (maybe (fileActionsJson [] [] []) (\(gens, acts, near) -> fileActionsJson gens acts near) found))
    , -- what follows at a line (the editor knows the cursor; pushes guess it
      -- from the last edit), offered as pushed ones are, its plan with it
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/suggest")) $ \req respond -> inBackground env req respond $ \job -> do
        let params = req ^. L.params
            found = do
              Object o <- pure params
              Object doc <- KM.lookup "textDocument" o
              String uri <- KM.lookup "uri" doc
              Number l <- KM.lookup "line" o
              (,) <$> uriToFilePath (Uri uri) <*> toBoundedInteger l
        case found of
          Nothing -> respond (Right Aeson.Null)
          Just (path, line) -> do
            liftIO (withSnapshotState job (suggestionFor path line)) >>= \case
              (at, Just sg) | Just v <- fst <$> M.lookup path (ssDocs at) -> do
                n <- liftIO (offer job path v sg)
                respond (Right (object (suggestionFields path v n sg ++ ["plan" .= planJson False (sgPlan sg)] ++ ["edit" .= workspaceEdit (editsOfPlan at (sgRoot sg) (sgPlan sg))])))
              _ -> respond (Right Aeson.Null)
    , -- an offered suggestion's plan, while its document is as it was
      -- (`fill`: planned again with the model, as accepting it does)
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/plan")) $ \req respond -> inBackground env req respond $ \job -> do
        let wanted = case req ^. L.params of
              Object o | Just (Number n) <- KM.lookup "planId" o -> toBoundedInteger n
              _ -> Nothing
            fill = case req ^. L.params of
              Object o -> KM.lookup "fill" o == Just (Bool True)
              _ -> False
        offered <- liftIO ((\(_, m) -> (`M.lookup` m) =<< wanted) <$> readIORef (envOffered job))
        s <- liftIO (readMVar (envState job))
        -- or a generator from the inputs chosen (the picker), for a document's project
        let chosen = case req ^. L.params of
              Object o
                | Just (String generator) <- KM.lookup "generator" o
                , Just (Object doc) <- KM.lookup "textDocument" o
                , Just (String uri) <- KM.lookup "uri" doc
                , Just path <- uriToFilePath (Uri uri) ->
                    Just (path, generator, setsIn o)
              _ -> Nothing
        case (chosen, offered) of
          (Just (path, generator, sets), _) ->
            planProgress generator (\progress -> withPlanned job (actionPlanWith progress True path 1 generator sets)) >>= \case
              Left why -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
              Right (at, (root, plan)) -> respond (Right (object ["plan" .= planJson False plan, "edit" .= workspaceEdit (editsOfPlan at root plan)]))
          (_, Nothing) -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) "no such plan (offered plans last until their document changes)" Nothing))
          (_, Just o)
            | fmap fst (M.lookup (ofPath o) (ssDocs s)) /= Just (ofVersion o) -> respond (Left (TResponseError (InL LSPErrorCodes_ContentModified) "the document changed since this was suggested" Nothing))
            | not fill ->
                let sg = ofSuggestion o
                 in respond (Right (object ["plan" .= planJson False (sgPlan sg), "edit" .= workspaceEdit (editsOfPlan s (sgRoot sg) (sgPlan sg))]))
            | otherwise -> do
                let sg = ofSuggestion o
                planProgress (sgLabel sg) (\progress -> withPlanned job (actionPlanWith progress True (ofPath o) (sgAt sg) (sgGenerator sg) (sgSets sg))) >>= \case
                  Left why -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
                  Right (at, (root, plan)) -> respond (Right (object ["plan" .= planJson False plan, "edit" .= workspaceEdit (editsOfPlan at root plan)]))
    , -- the project of a document in the shared vocabulary (spec §1b)
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/model")) $ \req respond -> do
        let path = case req ^. L.params of
              Object o | Just (Object doc) <- KM.lookup "textDocument" o, Just (String uri) <- KM.lookup "uri" doc -> uriToFilePath (Uri uri)
              _ -> Nothing
        model <- maybe (pure Nothing) (liftIO . withSnapshot env . modelFor) path
        respond (Right (maybe Aeson.Null toJSON model))
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
  pure (s, object ["documents" .= rows, "projects" .= M.mapKeys T.pack (ssLoads s), "broken" .= M.fromList [(T.pack root, why) | (root, Broken why _) <- M.toList (ssProjects s)]])

-- | Ask the client to watch more files (dynamic registration: the protocol
-- has no static capability for it), once per glob; a change reloads the
-- projects whose `.olifant/` or packs hold it.
watch :: Env -> [GlobPattern] -> LspM () ()
watch env globs = do
  let keyed = [(TE.decodeUtf8 (BSL.toStrict (Aeson.encode g)), g) | g <- globs]
  fresh <- liftIO (atomicModifyIORef' (envWatched env) (\seen -> let new = [kg | kg@(k, _) <- keyed, k `S.notMember` seen] in (foldr (S.insert . fst) seen new, map snd new)))
  if null fresh
    then pure ()
    else void $
      registerCapability mempty SMethod_WorkspaceDidChangeWatchedFiles (DidChangeWatchedFilesRegistrationOptions [FileSystemWatcher g Nothing | g <- fresh]) $ \msg ->
        liftIO (modifyMVar_ (envState env) (filesChanged [p | ev <- msg ^. L.params . L.changes, Just p <- [uriToFilePath (ev ^. L.uri)]]))

-- | Every file under a folder, as a pattern relative to it.
everything :: FilePath -> GlobPattern
everything dir = GlobPattern (InR (RelativePattern (InR (filePathToUri dir)) (Pattern "**/*")))

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
  void . submit (envJobs env) ("suggest " <> T.pack path) debounce $ \token -> do
    -- superseded by the next edit: its checks stop (review I5)
    found <- withSnapshot env {envCancel = token} (suggestionFor path line)
    current <- fmap fst . M.lookup path . ssDocs <$> readMVar (envState env)
    case found of
      Just sg | current == Just version -> do
        n <- offer env path version sg
        runLspT lspEnv . sendNotification (SMethod_CustomMethod (Proxy :: Proxy "olifant/suggestion")) $ object (suggestionFields path version n sg)
      _ -> pure ()

-- | Keep a suggestion's plan for `olifant/plan`, while its file is at
-- that version (a file's offers for older versions go): its id.
offer :: Env -> FilePath -> Int -> Suggestion -> IO Int
offer env path version sg =
  atomicModifyIORef' (envOffered env) $ \(k, m) ->
    let current = M.filter (\o -> ofPath o /= path || ofVersion o >= version) m
     in ((k + 1, M.insert (k + 1) (Offered path version sg) current), k + 1)

-- | A suggestion as editors read it (olifant/suggestion, olifant/suggest).
suggestionFields :: FilePath -> Int -> Int -> Suggestion -> [(Key.Key, Value)]
suggestionFields path version n sg =
  [ "uri" .= filePathToUri path
  , "version" .= version
  , "kind" .= sgKind sg
  , "label" .= sgLabel sg
  , "summary" .= sgSummary sg
  , "line" .= sgLine sg
  , "lines" .= sgLines sg
  , "at" .= sgAt sg
  , "planId" .= n
  ]

-- | Run against the state as it is now, without holding it: what the work
-- loaded (projects) is kept; documents changed meanwhile are not undone.
withSnapshot :: Env -> (ServerState -> IO (ServerState, a)) -> IO a
withSnapshot env work = snd <$> withSnapshotState env work

-- | A plan made on a snapshot, with the state it was made on: edits are
-- stamped with that state's document versions.
withPlanned :: Env -> (ServerState -> IO (ServerState, Either e (FilePath, Plan))) -> IO (Either e (ServerState, (FilePath, Plan)))
withPlanned env work = withSnapshotState env work <&> \(at, planned) -> (at,) <$> planned

-- | 'withSnapshot', with the state the work ran on (the documents as they
-- were when it started).
withSnapshotState :: Env -> (ServerState -> IO (ServerState, a)) -> IO (ServerState, a)
withSnapshotState env work = do
  before <- (\s -> s {ssCancel = envCancel env}) <$> readMVar (envState env)
  (after, a) <- work before
  let loaded = M.filter (> 0) (M.unionWith (-) (ssLoads after) (ssLoads before))
  modifyMVar_ (envState env) $ \now ->
    pure
      now
        { ssProjects = M.union (ssProjects now) (ssProjects after)
        , ssLoads = M.unionWith (+) (ssLoads now) loaded
        , -- what it worked out stands only if nothing changed meanwhile
          ssListings = M.union (ssListings now) (M.filter ((== ssFilesStamp now) . fst) (ssListings after))
        , ssMemo = M.union (ssMemo now) (M.filter ((== ssGeneration now) . fst) (ssMemo after))
        }
  pure (after, a)

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
    Just (generator, sets, sn) ->
      [ object
          [ "insertText" .= object ["kind" .= ("snippet" :: T.Text), "value" .= snBody sn]
          , "range" .= Range (Position l 0) (Position l (fromIntegral (utf16 lineText)))
          , "filterText" .= lineText
          , -- beyond LSP: the ghost as the engine draws it (holes as ⟨name⟩),
            -- and what completionItem/resolve takes to fill it with the model
            "ghost" .= snGhost sn
          , "data" .= object ["kind" .= ("action" :: T.Text), "path" .= path, "generator" .= generator, "sets" .= M.fromList sets, "line" .= line]
          ]
      ]

-- | A resolved code action: its plan's edit, and the plan itself in its
-- data (the editors' plan JSON, files before and after, for a preview).
-- Its edits are for the documents as they were planned (the versions of
-- that state), never as they are when it answers (review C1).
resolved :: CodeAction -> (ServerState, (FilePath, Plan)) -> CodeAction
resolved action (at, (root, plan)) = action & L.edit ?~ workspaceEdit (editsOfPlan at root plan) & L.data_ %~ fmap withPlan
  where
    withPlan = \case
      Object o -> Object (KM.insert "plan" (planJson False plan) o)
      other -> other

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

-- | Answer a request from a thread of its own, so slow work never holds up
-- the server; a failure there is answered as an error, never left hanging.
-- It runs under a token of its own, tripped by the client's
-- `$/cancelRequest` for it: its checks, verifies and model calls stop,
-- their processes killed (review I5).
inBackground :: Env -> TRequestMessage m -> (Either (TResponseError m) a -> LspM () ()) -> (Env -> LspM () ()) -> LspM () ()
inBackground env req respond work = do
  lspEnv <- getLspEnv
  (token, trip) <- liftIO newCancel
  let key = requestKey (req ^. L.id)
  liftIO (atomicModifyIORef' (envRunning env) (\m -> (M.insert key trip m, ())))
  void . liftIO . forkIO $
    ( runLspT lspEnv (work env {envCancel = token}) `catch` \(e :: SomeException) ->
        runLspT lspEnv (respond (Left (TResponseError (InR ErrorCodes_InternalError) ("olifant lsp: " <> T.pack (displayException e)) Nothing)))
    )
      `finally` atomicModifyIORef' (envRunning env) (\m -> (M.delete key m, ()))

-- | A request id as `$/cancelRequest` names it.
requestKey :: LspId m -> T.Text
requestKey = \case
  IdInt n -> T.pack (show n)
  IdString t -> t

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

