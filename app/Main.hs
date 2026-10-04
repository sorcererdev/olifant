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
import System.Process (readProcessWithExitCode)
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
  diff <- git ["diff", "--relative", "--no-color", "-U0", T.unpack ref, "--"]
  untracked <- git ["ls-files", "--others", "--exclude-standard"]
  pure $ case (diff, untracked) of
    (Left e, _) -> Left e
    (_, Left e) -> Left e
    (Right d, Right u) -> Right (hunks (T.lines d), map T.unpack (filter (not . T.null) (T.lines u)))
  where
    git args = do
      (code, out, err) <- readProcessWithExitCode "git" ("-C" : dir : args) ""
      pure $ case code of
        ExitSuccess -> Right (T.pack out)
        ExitFailure _ -> Left ("git " <> T.pack (unwords args) <> ": " <> T.strip (T.pack err))
    hunks = go Nothing []
      where
        go _ acc [] = reverse acc
        go cur acc (l : ls)
          | Just p <- T.stripPrefix "+++ b/" l = go (Just (T.unpack p)) acc ls
          | "@@ " `T.isPrefixOf` l, Just f <- cur, Just r <- range l = go cur (add f r acc) ls
          | otherwise = go cur acc ls
        add f r acc = case lookup f acc of
          Just rs -> (f, r : rs) : filter ((/= f) . fst) acc
          Nothing -> (f, [r]) : acc
        -- "@@ -a,b +c,d @@": lines c .. c+d-1 (d defaults to 1; 0 is a pure deletion)
        range l = case T.words l of
          (_ : _ : plus : _) | Just spec <- T.stripPrefix "+" plus ->
            let (start, count) = T.breakOn "," spec
                c = read (T.unpack start) :: Int
                d = if T.null count then 1 else read (T.unpack (T.drop 1 count)) :: Int
             in if d == 0 then Nothing else Just (c, c + d - 1)
          _ -> Nothing
