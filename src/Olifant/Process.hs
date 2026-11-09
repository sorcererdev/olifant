-- | Running a tool with binary stdin/stdout/stderr (no locale surprises).
-- Nothing here throws: a tool that cannot start, exits before reading its
-- input, or breaks a pipe is reported as 'Left', so callers can turn it into
-- a miss or a check failure instead of crashing Olifant.
module Olifant.Process (gitIn, gitBaseFiles, gitBaseText, gitStateStamp, gitTracked, runBytes, runBytesWithin, Outcome (..), runBytesCancellable, Cancel, newCancel, neverCancel, cancelAfter, cancelled, eitherCancel) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, SomeAsyncException, SomeException, displayException, evaluate, fromException, onException, try, tryJust)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Set (Set)
import Data.Set qualified as S
import Olifant.Process.Cancel (Cancel, cancelAfter, cancelled, eitherCancel, neverCancel, newCancel)
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, hSetBinaryMode)
import System.Process (CreateProcess (..), StdStream (..), createProcess, getPid, getProcessExitCode, interruptProcessGroupOf, proc, terminateProcess, waitForProcess)

runBytes :: FilePath -> [String] -> Maybe FilePath -> ByteString -> IO (Either Text (ExitCode, ByteString, ByteString))
runBytes exe args dir input = do
  result <- try $ do
    (Just hin, Just hout, Just herr, ph) <-
      createProcess (proc exe args) {cwd = dir, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
    mapM_ (`hSetBinaryMode` True) [hin, hout, herr]
    outV <- newEmptyMVar
    errV <- newEmptyMVar
    let drain h v = forkIO ((try (BS.hGetContents h >>= evaluate) :: IO (Either IOException ByteString)) >>= putMVar v)
    _ <- drain hout outV
    _ <- drain herr errV
    -- The tool may exit without reading everything (EPIPE): that is its
    -- answer, not our failure; its exit code and stderr say what happened.
    _ <- try (BS.hPut hin input) :: IO (Either IOException ())
    _ <- try (hClose hin) :: IO (Either IOException ())
    out <- takeMVar outV
    err <- takeMVar errV
    code <- waitForProcess ph
    pure $ case (out, err) of
      (Right o, Right e) -> Right (code, o, e)
      (Left x, _) -> Left (firstLine (displayException x))
      (_, Left x) -> Left (firstLine (displayException x))
  pure $ case result of
    Left (x :: SomeException) -> Left (T.pack exe <> ": " <> firstLine (displayException x))
    Right r -> r

-- | An exception's message without GHC's call-stack backtrace, which is noise
-- in a user-facing error.
firstLine :: String -> Text
firstLine = T.strip . T.pack . takeWhile (/= '\n')

-- | 'runBytes', stopped after @secs@ seconds: 'Nothing' when it ran too
-- long (its process group is interrupted, then it is terminated).
runBytesWithin :: Int -> FilePath -> [String] -> Maybe FilePath -> ByteString -> IO (Either Text (Maybe (ExitCode, ByteString, ByteString)))
runBytesWithin secs exe args dir input =
  runBytesCancellable neverCancel (Just secs) exe args dir input <&> fmap finished
  where
    finished = \case
      Finished code out err -> Just (code, out, err)
      _ -> Nothing

-- | How a stoppable command ended.
data Outcome = Finished ExitCode ByteString ByteString | TimedOut | Cancelled
  deriving stock (Eq, Show)

-- | 'runBytes' in its own process group, stopped when the token is tripped
-- or after @secs@ seconds (if given): the whole group is killed, so the
-- tools it started (pyright under uv, a model call) stop too.
runBytesCancellable :: Cancel -> Maybe Int -> FilePath -> [String] -> Maybe FilePath -> ByteString -> IO (Either Text Outcome)
runBytesCancellable token secs exe args dir input = do
  -- only failures are answers; an interruption of this thread (a client's
  -- cancel) kills the group below and goes on
  result <- tryJust synchronous $ do
    (Just hin, Just hout, Just herr, ph) <-
      createProcess (proc exe args) {cwd = dir, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, create_group = True}
    let stopGroup = do
          _ <- try (interruptProcessGroupOf ph) :: IO (Either SomeException ())
          _ <- try (getPid ph >>= mapM_ (signalProcessGroup sigKILL)) :: IO (Either SomeException ())
          terminateProcess ph
    -- everything after the start is under the kill: an interruption anywhere
    -- (a client's cancel) stops the group, so no process is left behind
    flip onException (stopGroup >> try (waitForProcess ph) :: IO (Either SomeException ExitCode)) $ do
      mapM_ (`hSetBinaryMode` True) [hin, hout, herr]
      outV <- newEmptyMVar
      errV <- newEmptyMVar
      let drain h v = forkIO ((try (BS.hGetContents h >>= evaluate) :: IO (Either IOException ByteString)) >>= putMVar v)
      _ <- drain hout outV
      _ <- drain herr errV
      -- written from a thread of its own: a tool that never reads its input
      -- (a big text past the pipe's buffer) must not hold up the clock below;
      -- killed, it breaks the pipe and the writer ends
      _ <- forkIO $ do
        _ <- try (BS.hPut hin input) :: IO (Either IOException ())
        void (try (hClose hin) :: IO (Either IOException ()))
      -- polled, not `timeout` around waitForProcess: without the threaded
      -- runtime that call blocks everything, so a timeout could never fire
      let poll ticks = getProcessExitCode ph >>= \case
            Just code -> pure (Right code)
            Nothing ->
              cancelled token >>= \case
                True -> pure (Left Cancelled)
                False
                  | maybe False (\s -> ticks >= s * 20) secs -> pure (Left TimedOut)
                  | otherwise -> threadDelay 50000 >> poll (ticks + 1 :: Int)
      poll 0 >>= \case
        Left stopped -> do
          stopGroup
          _ <- waitForProcess ph
          _ <- takeMVar outV
          _ <- takeMVar errV
          pure (Right stopped)
        Right code -> do
          out <- takeMVar outV
          err <- takeMVar errV
          pure $ case (out, err) of
            (Right o, Right e) -> Right (Finished code o e)
            (Left x, _) -> Left (firstLine (displayException x))
            (_, Left x) -> Left (firstLine (displayException x))
  pure $ case result of
    Left x -> Left (T.pack exe <> ": " <> firstLine (displayException x))
    Right r -> r

-- | A failure to answer, not an interruption of the thread (which must go on).
synchronous :: SomeException -> Maybe SomeException
synchronous e = case fromException e :: Maybe SomeAsyncException of
  Just _ -> Nothing
  Nothing -> Just e

-- | The files git tracks under a folder, relative to it; nothing when the
-- folder is in no git repository (or git is not installed). What a project
-- "already had", for recognition: an untracked file is new.
gitTracked :: FilePath -> IO (Maybe (Set FilePath))
gitTracked dir =
  runBytes "git" ["-C", dir, "ls-files", "-z"] Nothing "" <&> \case
    Right (ExitSuccess, out, _) -> Just (S.fromList [T.unpack (TE.decodeUtf8Lenient p) | p <- BS.split 0 out, not (BS.null p)])
    _ -> Nothing

-- | The files at the base (the merge base of HEAD and REF), project-relative.
gitBaseFiles :: FilePath -> Text -> IO (Either Text [FilePath])
gitBaseFiles dir ref =
  gitIn dir ["merge-base", "HEAD", T.unpack ref] >>= \case
    Left why -> pure (Left why)
    Right out ->
      gitIn dir ["ls-tree", "-r", "--name-only", T.unpack (T.strip out)] <&> fmap (map T.unpack . filter (not . T.null) . T.lines)

-- | A file's text at the base: the merge base of HEAD and REF (the commit's
-- id too, to key caches). Right Nothing: the file is not in that commit (it
-- is new); Left: why there is no base (no such ref, not a repository).
gitBaseText :: FilePath -> Text -> FilePath -> IO (Either Text (Text, Maybe Text))
gitBaseText dir ref path =
  git ["merge-base", "HEAD", T.unpack ref] >>= \case
    Left why -> pure (Left why)
    Right out -> do
      let commit = T.strip out
      let showAt p = git ["show", T.unpack commit <> ":" <> p]
          absent why = any (`T.isInfixOf` why) ["does not exist", "exists on disk, but not in"]
      showAt path >>= \case
        Right text -> pure (Right (commit, Just text))
        Left why
          | absent why ->
              -- moved since the base (git mv): its old self, not new work
              git ["diff", "-M", "--name-status", T.unpack commit] >>= \case
                Right listing
                  | old : _ <- [o | l <- T.lines listing, ("R" : o : n : _) <- [T.splitOn "\t" l `withStatus` 1], T.unpack n == path] ->
                      showAt (T.unpack old) <&> \case
                        Right text -> Right (commit, Just text)
                        Left _ -> Right (commit, Nothing)
                _ -> pure (Right (commit, Nothing))
          | otherwise -> pure (Left why)
  where
    git = gitIn dir
    -- `R087`: the status letter, its score dropped
    withStatus (st : rest) n = T.take n st : rest
    withStatus [] _ = []

gitIn :: FilePath -> [String] -> IO (Either Text Text)
gitIn dir args =
  runBytes "git" (["-C", dir, "-c", "core.quotePath=false"] ++ args) Nothing "" <&> \case
    Right (ExitSuccess, out, _) -> Right (TE.decodeUtf8Lenient out)
    Right (_, _, err) -> Left ("git " <> T.pack (unwords args) <> ": " <> T.strip (TE.decodeUtf8Lenient err))
    Left why -> Left why

-- | What the base (the merge base of HEAD and REF) depends on, read with no
-- process: HEAD, the branch it names, REF and packed-refs. A checkout, a
-- commit, a pull or a fetch of REF changes it; equal stamps, the same base.
gitStateStamp :: FilePath -> Text -> IO Text
gitStateStamp dir ref = do
  gitDir <- findGitDir dir
  case gitDir of
    Nothing -> pure ""
    Just g -> do
      headText <- readOr (g </> "HEAD")
      let branch = T.strip <$> T.stripPrefix "ref:" (T.strip headText)
          common = g
      named <- maybe (pure "") (\b -> readOr (common </> T.unpack b)) branch
      refText <- readOr (common </> "refs" </> "heads" </> T.unpack ref)
      packed <- readOr (common </> "packed-refs")
      pure (T.intercalate "\0" [headText, named, refText, packed])
  where
    readOr p = (either (const "") id <$> (try (TE.decodeUtf8Lenient <$> BS.readFile p) :: IO (Either IOException Text)))
    -- `.git` is a folder, or (a worktree) a file naming one
    findGitDir d = do
      let dotGit = d </> ".git"
      isDir <- doesDirectoryExist dotGit
      isFile <- doesFileExist dotGit
      if isDir
        then pure (Just dotGit)
        else
          if isFile
            then (\t -> T.unpack . T.strip <$> T.stripPrefix "gitdir:" (T.strip t)) <$> readOr dotGit
            else
              let up = takeDirectory d
               in if up == d then pure Nothing else findGitDir up

