{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | `olifant lsp` (olifant lsp spec §2): the language server, speaking LSP
-- over stdio. This module only maps LSP messages onto "Olifant.Server",
-- which holds the projects, the open documents and the jobs.
module Olifant.Lsp (runLsp, serverDefinition) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Lens ((&), (?~), (^.))
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (Bool, Number, Object, String), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text qualified as T
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as M
import Data.Proxy (Proxy (..))
import Data.Scientific (toBoundedInteger)
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText, virtualFileVersion)
import Olifant.App (Ports (..))
import Olifant.Rules (Finding (..), FixOffer (..))
import Olifant.Rules qualified as R
import Olifant.Workspace (Workspace (..))
import Olifant.Server
import System.FilePath (splitDirectories, joinPath)

-- | The server's state behind a lock, and whether the test-only
-- `olifant/debugState` answers.
data Env = Env {envState :: MVar ServerState, envDebug :: IORef Bool}

runLsp :: Ports -> IO Int
runLsp ports = serverDefinition ports >>= runServer

serverDefinition :: Ports -> IO (ServerDefinition ())
serverDefinition ports = do
  env <- Env <$> (newServerState ports >>= newMVar) <*> newIORef False
  pure
    ServerDefinition
      { parseConfig = \_ _ -> Right ()
      , onConfigChange = const (pure ())
      , defaultConfig = ()
      , configSection = "olifant"
      , doInitialize = \lspEnv req -> configure env (req ^. L.params . L.initializationOptions) >> pure (Right lspEnv)
      , staticHandlers = \_ -> handlers env
      , interpretHandler = \lspEnv -> Iso (runLspT lspEnv) liftIO
      , options = defaultOptions {optTextDocumentSync = Just sync, optServerInfo = Just (ServerInfo "olifant" Nothing)}
      }
  where
    -- open/close and incremental changes; saves are noticed (regions)
    sync = TextDocumentSyncOptions (Just True) (Just TextDocumentSyncKind_Incremental) (Just False) (Just False) (Just (InR (SaveOptions (Just False))))

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
          Just vf -> atPath uri $ \path -> update env (changeDoc path (fromIntegral (virtualFileVersion vf)) (virtualFileText vf))
    , notificationHandler SMethod_TextDocumentDidClose $ \msg ->
        atPath (msg ^. L.params . L.textDocument . L.uri) (update env . closeDoc)
    , notificationHandler SMethod_TextDocumentDidSave (\_ -> pure ())
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
          Just path -> liftIO (fixActions env path (fromIntegral from + 1) (fromIntegral to + 1))
        respond (Right (InL (map InR actions)))
    , requestHandler SMethod_CodeActionResolve $ \req respond -> do
        let action = req ^. L.params
        case fixData =<< (action ^. L.data_) of
          Nothing -> respond (Right action)
          Just (path, rule, line, n, sets) ->
            liftIO (withSnapshot env (fixEdits path rule line n sets)) >>= \case
              Left why -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
              Right edits -> respond (Right (action & L.edit ?~ workspaceEdit edits))
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

-- | A fix's data: the file, rule, line, which fix, and the choices made.
fixData :: Value -> Maybe (FilePath, T.Text, Int, Int, [(T.Text, T.Text)])
fixData = \case
  Object o | KM.lookup "kind" o == Just (String "fix") -> do
    String path <- KM.lookup "path" o
    String rule <- KM.lookup "rule" o
    Number line <- KM.lookup "line" o
    Number n <- KM.lookup "index" o
    let sets = case KM.lookup "sets" o of
          Just (Object m) -> [(Key.toText k, v) | (k, String v) <- KM.toList m]
          _ -> []
    (,,,,) (T.unpack path) rule <$> toBoundedInteger line <*> toBoundedInteger n <*> pure sets
  _ -> Nothing

-- | Edits as one workspace edit: new files created, then each change at
-- its document's version (the editor refuses it if the text moved on).
workspaceEdit :: [FileEdit] -> WorkspaceEdit
workspaceEdit edits = WorkspaceEdit Nothing (Just (concatMap change edits)) Nothing
  where
    change e =
      let uri = filePathToUri (fePath e)
          ((l1, c1), (l2, c2)) = feRange e
          range = Range (Position (fromIntegral l1) (fromIntegral c1)) (Position (fromIntegral l2) (fromIntegral c2))
          edit = TextDocumentEdit (OptionalVersionedTextDocumentIdentifier uri (maybe (InR Null) (InL . fromIntegral) (feVersion e))) [InL (TextEdit range (feText e))]
       in [InR (InL (CreateFile Nothing AString uri Nothing)) | feCreate e] ++ [InL edit]

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
