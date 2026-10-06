module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import Olifant.GitInit (gitInit)
import Olifant.Cli (DoctorPorts (..), Runtime (..), parseDiffHunks, runCli)
import Olifant.Model (probeBackend)
import Data.Maybe (fromMaybe, isJust)
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.Process (readProcessWithExitCode)
import Olifant.Language (processCheckerWith, processVerifierCancellable)
import Olifant.Language.AstGrep (astGrepSyntax)
import Olifant.Workspace (diskWorkspace)
import System.Directory (XdgDirectory (XdgCache, XdgConfig), getCurrentDirectory, getHomeDirectory, getXdgDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hIsTerminalDevice, stderr, stdout)

main :: IO ()
main = do
  args <- getArgs
  cwd <- getCurrentDirectory
  tty <- hIsTerminalDevice stdout
  home <- getHomeDirectory
  userConfigDir <- getXdgDirectory XdgConfig "olifant"
  cache <- getXdgDirectory XdgCache "olifant"
  -- stdin is read at most once; every reader sees the same text
  stdinText <- newIORef Nothing
  let readStdin = readIORef stdinText >>= \case
        Just t -> pure t
        Nothing -> do
          t <- TIO.getContents
          writeIORef stdinText (Just t)
          pure t
  result <-
    try . runCli
      Runtime
        { rtRoot = diskWorkspace "/"
        , rtProjectDir = cwd
        , rtUserConfigDir = Just userConfigDir
        , rtHomeDir = Just home
        , rtReadStdin = readStdin
        , rtNow = getCurrentTime
        , rtStdoutIsTty = tty
        , rtGitInit = gitInit
        , rtVerifier = (`processVerifierCancellable` 120)
        , rtOut = TIO.putStr
        , rtErr = TIO.hPutStr stderr
        , rtSyntax = astGrepSyntax cache
        , rtChecker = processCheckerWith
        , rtGitChanged = gitChanged
        , rtDoctor =
            DoctorPorts
              findExecutable
              (T.pack . fromMaybe "" <$> lookupEnv "PATH")
              probeBackend
              (fmap isJust . lookupEnv . T.unpack)
        }
      $ args
  case result of
    Right code -> exitWith code
    Left (e :: SomeException) -> TIO.hPutStrLn stderr ("olifant: internal error: " <> T.pack (displayException e)) >> exitWith (ExitFailure 1)

-- | Lines changed since a git ref, relative to a directory (the project
-- root), and the untracked files there (spec §5.10, `check --changed-since`).
gitChanged :: FilePath -> T.Text -> IO (Either T.Text ([(FilePath, [(Int, Int)])], [FilePath]))
gitChanged dir ref = do
  diff <- git ["diff", "--relative", "--no-color", "--no-ext-diff", "-U0", "--src-prefix=a/", "--dst-prefix=b/", T.unpack ref, "--"]
  untracked <- git ["ls-files", "--others", "--exclude-standard"]
  pure $ case (diff, untracked) of
    (Left e, _) -> Left e
    (_, Left e) -> Left e
    (Right d, Right u) -> Right (parseDiffHunks d, map T.unpack (filter (not . T.null) (T.lines u)))
  where
    git args = do
      (code, out, err) <- readProcessWithExitCode "git" (["-C", dir, "-c", "core.quotePath=false"] ++ args) ""
      pure $ case code of
        ExitSuccess -> Right (T.pack out)
        ExitFailure _ -> Left ("git " <> T.pack (unwords args) <> ": " <> T.strip (T.pack err))

