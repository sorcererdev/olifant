{-# LANGUAGE DataKinds #-}

module Olifant.LspSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_)
import Data.Aeson (Value (Array, Bool, Object), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Functor ((<&>))
import Control.Lens ((^.))
import Language.LSP.Protocol.Lens qualified as L
import System.Exit (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode)
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime (..), fromGregorian)
import Language.LSP.Protocol.Message (SMethod (..), TResponseMessage (..))
import Language.LSP.Protocol.Types
import Language.LSP.Server (runServerWithHandles)
import Language.LSP.Test
import Olifant.App (Ports (..))
import Olifant.Language (noChecker, noSyntax)
import Olifant.Language.Check (noVerifier)
import Olifant.Lsp
import Olifant.Workspace (diskWorkspace)
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (createPipe)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Lsp (olifant lsp over stdio)" $ do
  it "starts, follows a document's edits, and exits cleanly when asked" $
    withProject [("src/x.py", "x = 1\n")] $ \dir -> do
      ((state, version), code) <- inProcess dir $ do
        doc <- openDoc "src/x.py" "python"
        changeDoc doc [TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument "x = 2\n"))]
        VersionedTextDocumentIdentifier _ v <- getVersionedDoc doc
        st <- debugState
        pure (st, v)
      code `shouldBe` Just 0
      let doc = object ["path" .= (dir </> "src/x.py"), "version" .= version, "project" .= dir]
      lookupKey "documents" state `shouldBe` Just (Array (pure doc))
  it "is the `olifant lsp` command: the built binary initialises and opens a document" $ do
    (built, out, _) <- readCreateProcessWithExitCode (proc "cabal" ["list-bin", "-v0", "olifant"]) ""
    if built /= ExitSuccess then pendingWith "cabal list-bin olifant failed" else
      withProject [("src/x.py", "x = 1\n")] $ \dir -> do
        name <- runSessionWithConfig defaultConfig (T.unpack (T.strip (T.pack out)) <> " lsp") fullLatestClientCaps dir $ do
          _ <- openDoc "src/x.py" "python"
          initializeResponse <&> \case
            TResponseMessage _ _ (Right result) -> fmap (\(ServerInfo n _) -> n) (result ^. L.serverInfo)
            _ -> Nothing
        name `shouldBe` Just "olifant"

-- | The server in this process over pipes, a client session against it
-- (debugState on); the server's exit code once the session has ended.
inProcess :: FilePath -> Session a -> IO (a, Maybe Int)
inProcess dir session = do
  (inRead, inWrite) <- createPipe
  (outRead, outWrite) <- createPipe
  definition <- serverDefinition ports
  done <- newEmptyMVar
  _ <- forkIO (runServerWithHandles mempty mempty inRead outWrite definition >>= putMVar done)
  a <- runSessionWithHandles inWrite outRead defaultConfig {lspConfig = KM.fromList [("debug", Bool True)]} fullLatestClientCaps dir session
  code <- timeout 5000000 (takeMVar done)
  pure (a, code)
  where
    ports = Ports (diskWorkspace "/") noSyntax noChecker noVerifier (pure (UTCTime (fromGregorian 2026 10 6) 0)) Nothing Nothing

debugState :: Session Value
debugState =
  request (SMethod_CustomMethod (Proxy :: Proxy "olifant/debugState")) Aeson.Null >>= \case
    TResponseMessage _ _ (Right v) -> pure v
    TResponseMessage _ _ (Left err) -> fail (show err)

lookupKey :: Text -> Value -> Maybe Value
lookupKey k = \case
  Object o -> KM.lookup (Key.fromText k) o
  _ -> Nothing

-- | A project on disk (an empty `.olifant/config.toml`) with some files.
withProject :: [(FilePath, Text)] -> (FilePath -> IO a) -> IO a
withProject files act = withSystemTempDirectory "olifant-lsp" $ \dir0 -> do
  dir <- canonicalizePath dir0
  forM_ ((".olifant/config.toml", "") : files) $ \(p, t) -> createDirectoryIfMissing True (takeDirectory (dir </> p)) >> TIO.writeFile (dir </> p) t
  act dir
