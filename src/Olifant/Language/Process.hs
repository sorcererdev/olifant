-- | Running a tool with binary stdin/stdout/stderr (no locale surprises).
module Olifant.Language.Process (runBytes) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import System.Exit (ExitCode)
import System.IO (hClose, hSetBinaryMode)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)

runBytes :: FilePath -> [String] -> Maybe FilePath -> ByteString -> IO (ExitCode, ByteString, ByteString)
runBytes exe args dir input = do
  (Just hin, Just hout, Just herr, ph) <-
    createProcess (proc exe args) {cwd = dir, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  mapM_ (`hSetBinaryMode` True) [hin, hout, herr]
  outV <- newEmptyMVar
  errV <- newEmptyMVar
  _ <- forkIO (BS.hGetContents hout >>= putMVar outV)
  _ <- forkIO (BS.hGetContents herr >>= putMVar errV)
  BS.hPut hin input >> hClose hin
  out <- takeMVar outV
  err <- takeMVar errV
  code <- waitForProcess ph
  pure (code, out, err)
