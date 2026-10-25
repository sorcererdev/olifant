module Olifant.Server.ErrandSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (finally)
import Data.Text qualified as T
import Olifant.Catalog (PlansConfig (..), defaultPlans)
import Olifant.Process (neverCancel, newCancel)
import Olifant.Plans (Anchor (..), parsePlan)
import Olifant.Server.Errand
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Server.Errand (the claude -p runner)" $ do
  let cfg = defaultPlans
      plan = "## Day page\n- ? where does the day page decide\n- a claim @src/a.py:Foo\n- [ ] the step\n  - test: it works @tests/t.py:test_it\n## Next\n- later\n"
      ls = parsePlan plan
      q = ls !! 1
      heading = ls !! 0
      step = ls !! 3
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
    either (T.isInfixOf "no credentials" . rfWhy) (const False) r `shouldBe` True
    either (T.isInfixOf "exited 2" . rfWhy) (const False) r `shouldBe` True
  it "reports printed non-JSON (a login prompt) as Left with the text" $ do
    c <- shimCfg
    r <- runClaude neverCancel c Ask [] "/tmp" "SHIM:LOGIN"
    either (T.isInfixOf "Please run /login" . rfWhy) (const False) r `shouldBe` True
    either (Just . rfStatus) (const Nothing) r `shouldBe` Just Error
  it "a JSON reply without cost or duration is zero seconds and no cost" $ do
    c <- shimCfg
    r <- runClaude neverCancel c Ask [] "/tmp" "SHIM:PROSE"
    r `shouldBe` Right (RunResult "Sure! Here is what I found." 0 Nothing Nothing Nothing Nothing [] 0)
  it "times out" $ do
    c <- shimCfg
    r <- runClaude neverCancel c {pcTimeout = 1} Ask [] "/tmp" "SHIM:SLEEP"
    r `shouldBe` Left (RunFailure Timeout "timed out after 1 s")
  it "cancels" $ do
    c <- shimCfg
    (tok, trip) <- newCancel
    _ <- forkIO (threadDelay 200000 >> trip)
    r <- runClaude tok c Ask [] "/tmp" "SHIM:SLEEP"
    r `shouldBe` Left (RunFailure Cancelled "cancelled")
  it "keeps session, turns, tokens, models and denials from the runner's JSON" $ do
    c <- shimCfg
    Right rr <- runClaude neverCancel c Ask [] "/tmp" "QUESTION: anything"
    rrTurns rr `shouldBe` Just 1
    rrTokens rr `shouldBe` Just (Tokens 2 40 1000 5000)
    fmap muCost (lookup "claude-sonnet-5-5" (rrModels rr)) `shouldBe` Just (Just 0.0123)
    fmap muTokens (lookup "claude-sonnet-5-5" (rrModels rr)) `shouldBe` Just (Tokens 2 40 1000 5000)
    rrDenials rr `shouldBe` 0
    fmap (T.isPrefixOf "shim-") (rrSession rr) `shouldBe` Just True
  it "an is_error result is an Error failure with the runner's sentence" $ do
    c <- shimCfg
    runClaude neverCancel c Ask [] "/tmp" "SHIM:ERRORJSON" >>= (`shouldBe` Left (RunFailure Error "the runner reported an error"))
  it "a non-zero exit is an Error failure" $ do
    c <- shimCfg
    either (Just . rfStatus) (const Nothing) <$> runClaude neverCancel c Ask [] "/tmp" "SHIM:FAIL" >>= (`shouldBe` Just Error)
  it "a timeout is a Timeout failure" $ do
    c <- shimCfg
    either (Just . rfStatus) (const Nothing) <$> runClaude neverCancel c {pcTimeout = 1} Ask [] "/tmp" "SHIM:SLEEP" >>= (`shouldBe` Just Timeout)
  it "status names" $
    map statusName [Ok, Empty, Error, Timeout, Cancelled] `shouldBe` ["ok", "empty", "error", "timeout", "cancelled"]
  describe "prompts and answers" $ do
    it "ask keeps anchored claims and one conclusion, drops prose" $ do
      let (bs, dropped) = parseAnswer Ask ls q "Sure.\n- the table has no day @src/a.py:Table\n- closure is absent @src/b.py:Repo\n- so a model is needed\n- ? what about x\n"
      map ghText (concatMap blLines bs) `shouldBe` ["  - the table has no day   @src/a.py:Table", "  - closure is absent   @src/b.py:Repo", "  - so a model is needed"]
      dropped `shouldBe` 2
      map blAfterRow bs `shouldBe` [1]
    it "validate places blocks after the matching line, else at the section's end" $ do
      let (bs, _) = parseAnswer Validate ls heading "AFTER: - a claim @src/a.py:Foo\n  - ✓ holds @src/a.py:Foo\nAFTER: nowhere\n- ? why\n"
      map blAfterRow bs `shouldBe` [2, 4]
      map blAfterText bs `shouldBe` [Just "- a claim @src/a.py:Foo", Just "nowhere"]
    it "do turns DONE and BLOCKED into the step's report" $ do
      let (bs, _) = parseAnswer Do ls step "DONE @src/a.py:Foo.bar\nDONE @src/c.py:Baz\nBLOCKED no port\n"
      map ghText (concatMap blLines bs) `shouldBe` ["- [ ] the step   @src/a.py:Foo.bar", "  - also @src/c.py:Baz", "  - ✗ blocked: no port"]
      map blAfterRow bs `shouldBe` [3]
    it "never fails on odd input" $ do
      parseAnswer Ask ls q "" `shouldBe` ([], 0)
      fst (parseAnswer Validate ls heading "AFTER:") `shouldBe` []
      snd (parseAnswer Validate ls heading "AFTER:") `shouldBe` 1
      parseAnswer Do ls step "DONE\r\nDONE nothing\r\n" `shouldBe` ([], 2)
      length (fst (parseAnswer Ask ls q "- a @x.py:A\r\n")) `shouldBe` 1
      parseAnswer Ask ls heading "- a @x.py:A" `shouldBe` ([], 0)
    it "the prompt carries the plan, the units and the marker" $ do
      let p = errandPrompt Ask plan ls q [Grounded (Anchor "src/a.py" "Foo" 0 0) "class Foo:" "class Foo:\n    pass"]
      p `shouldSatisfy` T.isInfixOf "<doc>\n"
      p `shouldSatisfy` T.isInfixOf "QUESTION: where does the day page decide"
      p `shouldSatisfy` T.isInfixOf "@src/a.py:Foo\nclass Foo:"
      p `shouldSatisfy` T.isInfixOf "relative to the project root"
    it "validate and do prompts carry the section, step and its test" $ do
      errandPrompt Validate plan ls heading [] `shouldSatisfy` T.isInfixOf "SECTION: Day page\n## Day page"
      let d = errandPrompt Do plan ls step []
      d `shouldSatisfy` T.isInfixOf "STEP: the step\n  - test: it works @tests/t.py:test_it"
      errandPrompt Do plan ls (ls !! 4) [] `shouldSatisfy` T.isInfixOf "TEST: it works @tests/t.py:test_it"
    it "cuts a long body at 2000 characters and limits grounding to 20 units" $ do
      T.length (cutBody (T.replicate 3000 "x")) `shouldBe` 2002
      T.takeEnd 1 (cutBody (T.replicate 3000 "x")) `shouldBe` "…"
      cutBody "short" `shouldBe` "short"
      groundingLimit `shouldBe` 20
