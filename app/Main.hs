module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import Olifant.Cli (Runtime (..), runCli)
import Olifant.Workspace (diskWorkspace)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hIsTerminalDevice, stderr, stdout)

main :: IO ()
main = do
  args <- getArgs
  cwd <- getCurrentDirectory
  tty <- hIsTerminalDevice stdout
  result <-
    try . runCli
      Runtime
        { rtRoot = diskWorkspace "/"
        , rtProjectDir = cwd
        , rtNow = getCurrentTime
        , rtStdoutIsTty = tty
        , rtOut = TIO.putStr
        , rtErr = TIO.hPutStr stderr
        }
      $ args
  case result of
    Right code -> exitWith code
    Left (e :: SomeException) -> TIO.hPutStrLn stderr ("olifant: internal error: " <> T.pack (displayException e)) >> exitWith (ExitFailure 1)
