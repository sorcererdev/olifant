{-# LANGUAGE DataKinds #-}

-- | `olifant lsp` (olifant lsp spec §2): the language server, speaking LSP
-- over stdio. This module only maps LSP messages onto "Olifant.Server",
-- which holds the projects, the open documents and the jobs.
module Olifant.Lsp (runLsp, serverDefinition) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KM
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
import Olifant.App (Ports)
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

update :: MonadIO m => Env -> (ServerState -> ServerState) -> m ()
update env f = liftIO (modifyMVar_ (envState env) (pure . f))

atPath :: Applicative m => Uri -> (FilePath -> m ()) -> m ()
atPath uri act = maybe (pure ()) act (uriToFilePath uri)

-- | `/root/.olifant/config.toml` → `/root`.
projectOfConfig :: FilePath -> Maybe FilePath
projectOfConfig path
  | "/.olifant/" `isInfixOf` path = Just (joinPath (takeWhile (/= ".olifant") (splitDirectories path)))
  | otherwise = Nothing
