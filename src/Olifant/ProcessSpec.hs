module Olifant.ProcessSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Data.ByteString qualified as BS
import Data.Set qualified as S
import Data.Text qualified as T
import Olifant.Process (Outcome (..), gitBaseFiles, gitBaseText, gitTracked, newCancel, neverCancel, runBytes, runBytesCancellable)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.Timeout (timeout)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Process" $ do
  describe "runBytesCancellable with an input the tool never reads (4b m1 review I2)" $ do
    -- 200 KB is past any pipe's buffer: writing it blocks until the tool reads
    let big = BS.replicate 200000 120
        sleeper pidFile = ["-c", "echo $$ > " <> pidFile <> "; sleep 30"]
        gone pidFile = do
          pid <- T.unpack . T.strip . T.pack <$> readFile pidFile
          runBytes "kill" ["-0", pid] Nothing "" >>= \case
            Right (ExitSuccess, _, _) -> pure False
            _ -> pure True
    it "a cancel stops it at once and kills it" $
      withSystemTempDirectory "olifant-cancel" $ \dir -> do
        (token, trip) <- newCancel
        _ <- forkIO (threadDelay 500000 >> trip)
        timeout 5000000 (runBytesCancellable token Nothing "sh" (sleeper (dir </> "pid")) Nothing big) `shouldReturn` Just (Right Cancelled)
        gone (dir </> "pid") `shouldReturn` True
    it "an interruption while the input is unwritten kills it: no process is left" $
      withSystemTempDirectory "olifant-interrupt" $ \dir -> do
        timeout 1000000 (runBytesCancellable neverCancel Nothing "sh" (sleeper (dir </> "pid")) Nothing big) `shouldReturn` Nothing
        gone (dir </> "pid") `shouldReturn` True
  describe "gitTracked" $ do
    it "lists the files git tracks under a folder (relative to it), not the untracked ones" $
      withSystemTempDirectory "olifant-tracked" $ \dir -> do
        createDirectoryIfMissing True (dir </> "app/src")
        writeFile (dir </> "app/src/old.py") "x = 1\n"
        writeFile (dir </> "top.txt") "t\n"
        callProcess "git" ["-C", dir, "init", "-q"]
        callProcess "git" ["-C", dir, "add", "."]
        callProcess "git" ["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "first"]
        writeFile (dir </> "app/src/new.py") "y = 2\n"
        gitTracked (dir </> "app") `shouldReturn` Just (S.fromList ["src/old.py"])
    it "says nothing for a folder under no git repository" $
      withSystemTempDirectory "olifant-untracked" $ \dir ->
        gitTracked dir `shouldReturn` Nothing
  describe "gitBaseText" $ do
    it "reads a file at the merge base of HEAD and a ref (its commit too); absent there, Nothing; no such ref, why" $
      withSystemTempDirectory "olifant-base" $ \dir -> do
        let git args = callProcess "git" (["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t"] ++ args)
        writeFile (dir </> "a.py") "x = 1\n"
        git ["init", "-q", "-b", "main"]
        git ["add", "."]
        git ["commit", "-q", "-m", "first"]
        git ["checkout", "-q", "-b", "feature"]
        writeFile (dir </> "a.py") "x = 2\n"
        writeFile (dir </> "b.py") "y = 1\n"
        git ["add", "."]
        git ["commit", "-q", "-m", "second"]
        a <- gitBaseText dir "main" "a.py"
        fmap snd a `shouldBe` Right (Just "x = 1\n")
        fmap (T.length . fst) a `shouldBe` Right 40
        fmap snd <$> gitBaseText dir "main" "b.py" `shouldReturn` Right Nothing
        gitBaseText dir "nope" "a.py" >>= (`shouldSatisfy` either (const True) (const False))
    it "follows a rename: a file moved since the base reads as its old self, not as new (review #6)" $
      withSystemTempDirectory "olifant-base" $ \dir -> do
        let git args = callProcess "git" (["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t"] ++ args)
        writeFile (dir </> "payments.py") "def charge(card):\n    return card\n\ndef refund(card):\n    return card\n"
        git ["init", "-q", "-b", "main"]
        git ["add", "."]
        git ["commit", "-q", "-m", "first"]
        git ["checkout", "-q", "-b", "feature"]
        git ["mv", "payments.py", "billing.py"]
        fmap snd <$> gitBaseText dir "main" "billing.py" `shouldReturn` Right (Just "def charge(card):\n    return card\n\ndef refund(card):\n    return card\n")
  describe "gitBaseFiles" $
    it "lists the files at the merge base (review #7)" $
      withSystemTempDirectory "olifant-base" $ \dir -> do
        let git args = callProcess "git" (["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t"] ++ args)
        createDirectoryIfMissing True (dir </> "legacy")
        writeFile (dir </> "legacy/a.py") "x = 1\n"
        writeFile (dir </> "legacy/gone.py") "y = 1\n"
        git ["init", "-q", "-b", "main"]
        git ["add", "."]
        git ["commit", "-q", "-m", "first"]
        git ["checkout", "-q", "-b", "feature"]
        git ["rm", "-q", "legacy/gone.py"]
        gitBaseFiles dir "main" `shouldReturn` Right ["legacy/a.py", "legacy/gone.py"]
