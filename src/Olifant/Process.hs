-- | Running a tool with binary stdin/stdout/stderr (no locale surprises).
-- Nothing here throws: a tool that cannot start, exits before reading its
-- input, or breaks a pipe is reported as 'Left', so callers can turn it into
-- a miss or a check failure instead of crashing Olifant.
module Olifant.Process (runBytes, runBytesWithin) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, SomeException, displayException, evaluate, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode)
import System.IO (hClose, hSetBinaryMode)
import System.Process (CreateProcess (..), StdStream (..), createProcess, getProcessExitCode, interruptProcessGroupOf, proc, terminateProcess, waitForProcess)

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
runBytesWithin secs exe args dir input = do
  result <- try $ do
    (Just hin, Just hout, Just herr, ph) <-
      createProcess (proc exe args) {cwd = dir, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, create_group = True}
    mapM_ (`hSetBinaryMode` True) [hin, hout, herr]
    outV <- newEmptyMVar
    errV <- newEmptyMVar
    let drain h v = forkIO ((try (BS.hGetContents h >>= evaluate) :: IO (Either IOException ByteString)) >>= putMVar v)
    _ <- drain hout outV
    _ <- drain herr errV
    _ <- try (BS.hPut hin input) :: IO (Either IOException ())
    _ <- try (hClose hin) :: IO (Either IOException ())
    -- polled, not `timeout` around waitForProcess: without the threaded
    -- runtime that call blocks everything, so a timeout could never fire
    let poll left = getProcessExitCode ph >>= \case
          Just code -> pure (Just code)
          Nothing | left <= (0 :: Int) -> pure Nothing
          Nothing -> threadDelay 50000 >> poll (left - 1)
    finished <- poll (secs * 20)
    case finished of
      Nothing -> do
        _ <- try (interruptProcessGroupOf ph) :: IO (Either SomeException ())
        terminateProcess ph
        _ <- waitForProcess ph
        _ <- takeMVar outV
        _ <- takeMVar errV
        pure (Right Nothing)
      Just code -> do
        out <- takeMVar outV
        err <- takeMVar errV
        pure $ case (out, err) of
          (Right o, Right e) -> Right (Just (code, o, e))
          (Left x, _) -> Left (firstLine (displayException x))
          (_, Left x) -> Left (firstLine (displayException x))
  pure $ case result of
    Left (x :: SomeException) -> Left (T.pack exe <> ": " <> firstLine (displayException x))
    Right r -> r
