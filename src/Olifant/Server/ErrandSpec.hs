module Olifant.Server.ErrandSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (finally)
import Data.Text qualified as T
import Olifant.Catalog (PlansConfig (..), defaultPlans)
import Olifant.Process (neverCancel, newCancel)
import Olifant.Server.Errand
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Server.Errand (the claude -p runner)" $ do
  let cfg = defaultPlans
      shimCfg = do
        d <- getCurrentDirectory
        pure cfg {pcRunner = d </> "src/Olifant/Server/testdata/claude-shim.sh"}
  it "builds read-only args for ask and validate, edit args for do" $ do
    runnerArgs cfg Ask [] `shouldBe` ["-p", "--output-format", "json", "--model", "sonnet", "--allowedTools", "Read,Grep,Glob"]
    runnerArgs cfg Validate [] `shouldBe` runnerArgs cfg Ask []
    runnerArgs cfg {pcDoTools = ["Bash(cabal test*)"]} Do ["Bash(uv run pytest*)"]
      `shouldBe` ["-p", "--output-format", "json", "--model", "sonnet", "--permission-mode", "acceptEdits", "--allowedTools", "Read,Grep,Glob,Edit,Write,Bash(uv run pytest*),Bash(cabal test*)"]
  it "runs the shim, parses result, seconds and cost, and unsets CLAUDECODE" $ do
    c <- shimCfg
    old <- lookupEnv "CLAUDECODE"
    let restore = maybe (unsetEnv "CLAUDECODE") (setEnv "CLAUDECODE") old
    (setEnv "CLAUDECODE" "1" >> runClaude neverCancel c Ask [] "/tmp" "QUESTION: where\n") `finally` restore >>= \r -> do
      fmap rrCost r `shouldBe` Right (Just 0.0123)
      fmap rrSeconds r `shouldBe` Right 1.2
      fmap (T.isInfixOf "@" . rrText) r `shouldBe` Right True
  it "reports a failing runner with its first line" $ do
    c <- shimCfg
    r <- runClaude neverCancel c Ask [] "/tmp" "SHIM:FAIL"
    either (T.isInfixOf "no credentials") (const False) r `shouldBe` True
    either (T.isInfixOf "exited 2") (const False) r `shouldBe` True
  it "reports printed non-JSON (a login prompt) as Left with the text" $ do
    c <- shimCfg
    r <- runClaude neverCancel c Ask [] "/tmp" "SHIM:LOGIN"
    either (T.isInfixOf "Please run /login") (const False) r `shouldBe` True
  it "a JSON reply without cost or duration is zero seconds and no cost" $ do
    c <- shimCfg
    r <- runClaude neverCancel c Ask [] "/tmp" "SHIM:PROSE"
    r `shouldBe` Right (RunResult "Sure! Here is what I found." 0 Nothing)
  it "times out" $ do
    c <- shimCfg
    r <- runClaude neverCancel c {pcTimeout = 1} Ask [] "/tmp" "SHIM:SLEEP"
    r `shouldBe` Left "timed out after 1 s"
  it "cancels" $ do
    c <- shimCfg
    (tok, trip) <- newCancel
    _ <- forkIO (threadDelay 200000 >> trip)
    r <- runClaude tok c Ask [] "/tmp" "SHIM:SLEEP"
    r `shouldBe` Left "cancelled"
