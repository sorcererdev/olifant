module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import Olifant.Cli (DoctorPorts (..), Runtime (..), runCli)
import Olifant.Model (probeBackend)
import Data.Maybe (fromMaybe, isJust)
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import Olifant.Language (processChecker)
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
  result <-
    try . runCli
      Runtime
        { rtRoot = diskWorkspace "/"
        , rtProjectDir = cwd
        , rtUserConfigDir = Just userConfigDir
        , rtHomeDir = Just home
        , rtReadStdin = TIO.getContents
        , rtNow = getCurrentTime
        , rtStdoutIsTty = tty
        , rtOut = TIO.putStr
        , rtErr = TIO.hPutStr stderr
        , rtSyntax = astGrepSyntax cache
        , rtChecker = processChecker
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
