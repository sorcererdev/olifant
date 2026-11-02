module Olifant.Server.ErrandSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (finally)
import Data.Text qualified as T
import Olifant.Catalog (PlansConfig (..), defaultPlans)
import Olifant.Process (neverCancel, newCancel)
import Olifant.Plans (Anchor (..), parsePlan)
import Olifant.Language (shippedLanguage)
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
    runnerArgs cfg Ask [] Nothing `shouldBe` ["-p", "--output-format", "json", "--model", "sonnet", "--permission-prompts", "none", "--allowedTools", "Read,Grep,Glob"]
    runnerArgs cfg Validate [] Nothing `shouldBe` runnerArgs cfg Ask [] Nothing
    runnerArgs cfg {pcDoTools = ["Bash(cabal test*)"]} Do ["Bash(uv run pytest*)"] Nothing
      `shouldBe` ["-p", "--output-format", "json", "--model", "sonnet", "--permission-prompts", "none", "--permission-mode", "acceptEdits", "--allowedTools", "Read,Grep,Glob,Edit,Write,Bash(uv run pytest*),Bash(cabal test*)"]
  it "resumes a do's thread in a fork of its session" $
    drop 4 (runnerArgs cfg Do [] (Just "s1")) `shouldBe` ["sonnet", "--permission-prompts", "none", "--resume", "s1", "--fork-session", "--permission-mode", "acceptEdits", "--allowedTools", "Read,Grep,Glob,Edit,Write"]
  it "runs the shim, parses result, seconds and cost, and unsets CLAUDECODE" $ do
    c <- shimCfg
    old <- lookupEnv "CLAUDECODE"
    let restore = maybe (unsetEnv "CLAUDECODE") (setEnv "CLAUDECODE") old
    (setEnv "CLAUDECODE" "1" >> runClaude neverCancel c Ask [] Nothing "/tmp" "QUESTION: where\n") `finally` restore >>= \r -> do
      fmap rrCost r `shouldBe` Right (Just 0.0123)
      fmap rrSeconds r `shouldBe` Right 1.2
      fmap (T.isInfixOf "@" . rrText) r `shouldBe` Right True
  it "reports a failing runner with its first line" $ do
    c <- shimCfg
    r <- runClaude neverCancel c Ask [] Nothing "/tmp" "SHIM:FAIL"
    either (T.isInfixOf "no credentials" . rfWhy) (const False) r `shouldBe` True
    either (T.isInfixOf "exited 2" . rfWhy) (const False) r `shouldBe` True
  it "reports printed non-JSON (a login prompt) as Left with the text" $ do
    c <- shimCfg
    r <- runClaude neverCancel c Ask [] Nothing "/tmp" "SHIM:LOGIN"
    either (T.isInfixOf "Please run /login" . rfWhy) (const False) r `shouldBe` True
    either (Just . rfStatus) (const Nothing) r `shouldBe` Just Error
  it "a JSON reply without cost or duration is zero seconds and no cost" $ do
    c <- shimCfg
    r <- runClaude neverCancel c Ask [] Nothing "/tmp" "SHIM:PROSE"
    r `shouldBe` Right (RunResult "Sure! Here is what I found." 0 Nothing (Just "shim-prose") Nothing Nothing [] 0 [])
  it "times out" $ do
    c <- shimCfg
    r <- runClaude neverCancel c {pcTimeout = 1} Ask [] Nothing "/tmp" "SHIM:SLEEP"
    r `shouldBe` Left (RunFailure Timeout "timed out after 1 s" Nothing)
  it "cancels" $ do
    c <- shimCfg
    (tok, trip) <- newCancel
    _ <- forkIO (threadDelay 200000 >> trip)
    r <- runClaude tok c Ask [] Nothing "/tmp" "SHIM:SLEEP"
    r `shouldBe` Left (RunFailure Cancelled "cancelled" Nothing)
  it "keeps session, turns, tokens, models and denials from the runner's JSON" $ do
    c <- shimCfg
    Right rr <- runClaude neverCancel c Ask [] Nothing "/tmp" "QUESTION: anything"
    rrTurns rr `shouldBe` Just 1
    rrTokens rr `shouldBe` Just (Tokens 2 40 1000 5000)
    fmap muCost (lookup "claude-sonnet-5-5" (rrModels rr)) `shouldBe` Just (Just 0.0123)
    fmap muTokens (lookup "claude-sonnet-5-5" (rrModels rr)) `shouldBe` Just (Tokens 2 40 1000 5000)
    rrDenials rr `shouldBe` 0
    rrDenied rr `shouldBe` []
  it "keeps each denial's tool and command" $ do
    c <- shimCfg
    Right rr <- runClaude neverCancel c Ask [] Nothing "/tmp" "QUESTION: anything SHIM:DENY"
    (rrDenials rr, rrDenied rr) `shouldBe` (2, replicate 2 (Denial "Bash" "python -m pytest t.py"))
    fmap (T.isPrefixOf "shim-") (rrSession rr) `shouldBe` Just True
  it "an is_error result is an Error failure with the runner's sentence" $ do
    c <- shimCfg
    r <- runClaude neverCancel c Ask [] Nothing "/tmp" "SHIM:ERRORJSON"
    either (\f -> (rfStatus f, rfWhy f)) (const (Ok, "")) r `shouldBe` (Error, "the runner reported an error")
  it "an is_error result keeps the runner's session; a failed exit has none" $ do
    c <- shimCfg
    answered <- runClaude neverCancel c Ask [] Nothing "/tmp" "SHIM:ERRORJSON"
    either (fmap (T.isPrefixOf "shim-") . rfSession) (const Nothing) answered `shouldBe` Just True
    exited <- runClaude neverCancel c Ask [] Nothing "/tmp" "SHIM:FAIL"
    either rfSession (const (Just "answered")) exited `shouldBe` Nothing
  it "a non-zero exit is an Error failure" $ do
    c <- shimCfg
    either (Just . rfStatus) (const Nothing) <$> runClaude neverCancel c Ask [] Nothing "/tmp" "SHIM:FAIL" >>= (`shouldBe` Just Error)
  it "a timeout is a Timeout failure" $ do
    c <- shimCfg
    either (Just . rfStatus) (const Nothing) <$> runClaude neverCancel c {pcTimeout = 1} Ask [] Nothing "/tmp" "SHIM:SLEEP" >>= (`shouldBe` Just Timeout)
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
    it "do makes no also child of the step's own anchor (nor a test child's own)" $ do
      let anchored = parsePlan "## S\n- [ ] the step @src/a.py:Foo.bar\n  - test: it works @tests/t.py:test_it\n"
          own = parseAnswer Do anchored (anchored !! 1) "DONE @src/a.py:Foo.bar\nDONE @src/c.py:Baz\n"
          test = parseAnswer Do anchored (anchored !! 2) "DONE @tests/t.py:test_it\nDONE @src/a.py:Foo.bar\n"
      map ghText (concatMap blLines (fst own)) `shouldBe` ["  - also @src/c.py:Baz"]
      snd own `shouldBe` 0
      map ghText (concatMap blLines (fst test)) `shouldBe` []
      snd test `shouldBe` 0
      (doAnswered "DONE @tests/t.py:test_it\n", doAnswered "BLOCKED no port", doAnswered "Sure, done.") `shouldBe` (True, True, False)
    it "do takes a DONE of a file with no unit (the real endings of three nightvote dos)" $ do
      -- hand-entry-fixes rows 37, 40 and 43 changed only static/css/desk.css and ended so
      let real = ["DONE @static/css/desk.css:.rd-box", "DONE @static/css/desk.css:.rd-box small", "DONE @static/css/desk.css:.rd-box small[hidden]"]
      map doAnswered real `shouldBe` [True, True, True]
      map (parseAnswer Do ls step) real `shouldBe` replicate 3 ([], 0)
      doAnswered "DONE @static/css/desk.css" `shouldBe` True
      let (bs, dropped) = parseAnswer Do ls step "DONE @static/css/desk.css\nDONE @src/c.py:Baz\n"
      (map ghText (concatMap blLines bs), dropped) `shouldBe` (["- [ ] the step   @src/c.py:Baz"], 0)
      (doAnswered "DONE @", doAnswered "DONE @:Foo", doAnswered "DONE the css") `shouldBe` (False, False, False)
    it "never fails on odd input" $ do
      parseAnswer Ask ls q "" `shouldBe` ([], 0)
      fst (parseAnswer Validate ls heading "AFTER:") `shouldBe` []
      snd (parseAnswer Validate ls heading "AFTER:") `shouldBe` 1
      parseAnswer Do ls step "DONE\r\nDONE nothing\r\n" `shouldBe` ([], 2)
      length (fst (parseAnswer Ask ls q "- a @x.py:A\r\n")) `shouldBe` 1
      parseAnswer Ask ls heading "- a @x.py:A" `shouldBe` ([], 0)
    it "the anchor rule takes the project's languages' unit words" $ do
      py <- shippedLanguage "python" []
      hs <- shippedLanguage "haskell" []
      rs <- shippedLanguage "rust" []
      projectUnitWords [py] `shouldBe` pyWords
      anchorRule pyWords `shouldSatisfy` T.isInfixOf "<Unit> is a top-level class or function name (Name) or a method (Class.method) defined in that file."
      projectUnitWords [hs] `shouldSatisfy` T.isInfixOf "a name bound in its where (function.name)"
      projectUnitWords [py, rs] `shouldBe` defaultUnitWords
      projectUnitWords [] `shouldBe` defaultUnitWords
      errandPrompt Ask (projectUnitWords [hs]) Nothing plan ls q [] `shouldSatisfy` T.isInfixOf "a name bound in its where"
    it "the prompt carries the plan, the units and the marker" $ do
      let p = errandPrompt Ask pyWords Nothing plan ls q [Grounded (Anchor "src/a.py" "Foo" 0 0) "class Foo:" "class Foo:\n    pass"]
      p `shouldSatisfy` T.isInfixOf "<doc>\n"
      p `shouldSatisfy` T.isInfixOf "QUESTION: where does the day page decide"
      p `shouldSatisfy` T.isInfixOf "@src/a.py:Foo\nclass Foo:"
      p `shouldSatisfy` T.isInfixOf "relative to the project root"
    it "validate and do prompts carry the section, step and its test" $ do
      errandPrompt Validate pyWords Nothing plan ls heading [] `shouldSatisfy` T.isInfixOf "SECTION: Day page\n## Day page"
      let d = errandPrompt Do pyWords Nothing plan ls step []
      d `shouldSatisfy` T.isInfixOf "STEP: the step\n  - test: it works @tests/t.py:test_it"
      errandPrompt Do pyWords Nothing plan ls (ls !! 4) [] `shouldSatisfy` T.isInfixOf "TEST: it works @tests/t.py:test_it"
    it "a do's prompt for a step with an excerpt leaves the fence out, its other children in" $ do
      let t = T.unlines ["## S", "- [ ] the step @src/a.py:Foo", "  ```python @src/a.py:Foo", "  class Foo:", "      SECRET = 1", "  ```", "  - test: it works @tests/t.py:test_it"]
          pls = parsePlan t
          d = errandPrompt Do pyWords Nothing t pls (pls !! 1) []
      d `shouldSatisfy` (not . T.isInfixOf "SECRET")
      d `shouldSatisfy` (not . T.isInfixOf "```python @src/a.py:Foo")
      d `shouldSatisfy` T.isInfixOf "STEP: the step @src/a.py:Foo\n  - test: it works @tests/t.py:test_it"
    it "a do's test tool is the command as the prompt prints it, up to the tests slot; an absolute program in the project by its relative path too" $ do
      doTestTools "/p" ["env", "UV_PROJECT_ENVIRONMENT={= cache =}/venv", "uv", "run", "--quiet", "--frozen", "pytest", "-q", "{= tests =}"]
        `shouldBe` ["Bash(uv run --quiet --frozen pytest -q*)"]
      doTestTools "/w/night" ["/w/night/.venv/bin/pytest", "-q", "{= tests =}", "-x"]
        `shouldBe` ["Bash(/w/night/.venv/bin/pytest -q*)", "Bash(.venv/bin/pytest -q*)"]
      doTestTools "/w/night" ["/usr/bin/pytest", "{= tests =}"] `shouldBe` ["Bash(/usr/bin/pytest*)"]
      doTestTools "/p" [] `shouldBe` []
    it "a word holding {= report =} is no part of what a do types" $ do
      let cmd = ["pytest", "-q", "{= tests =}", "-o", "junit_family=xunit1", "--junitxml={= report =}"]
      doTestTools "/p" cmd `shouldBe` ["Bash(pytest -q*)"]
      doTestCommand cmd `shouldBe` Just "pytest -q <test file, or file::test> -o junit_family=xunit1"
    it "a do's prompt says its tools up front: files only by Edit and Write, the one command, the engine runs the tests" $ do
      let d = errandPrompt Do pyWords (Just "pytest -q <test file, or file::test>") plan ls step []
          before x = T.length (fst (T.breakOn x d))
      d `shouldSatisfy` T.isInfixOf "Edit and Write"
      d `shouldSatisfy` T.isInfixOf "heredoc"
      d `shouldSatisfy` T.isInfixOf "`sed -i`"
      d `shouldSatisfy` T.isInfixOf "`python -`"
      d `shouldSatisfy` T.isInfixOf "exactly `pytest -q <test file, or file::test>`"
      before "Edit and Write" < before "Implement it here" `shouldBe` True
      errandPrompt Do pyWords Nothing plan ls step [] `shouldSatisfy` T.isInfixOf "You may not run commands"
    it "a do's prompt names the test command exactly, with no pipe or cd, and says the engine runs the tests" $ do
      let cmd = ["env", "UV_PROJECT_ENVIRONMENT={= cache =}/venv-{= context =}", "uv", "run", "--quiet", "--frozen", "pytest", "-q", "{= tests =}"]
      doTestCommand cmd `shouldBe` Just "uv run --quiet --frozen pytest -q <test file, or file::test>"
      doTestCommand [] `shouldBe` Nothing
      let d = errandPrompt Do pyWords (doTestCommand cmd) plan ls step []
      d `shouldSatisfy` T.isInfixOf "uv run --quiet --frozen pytest -q <test file, or file::test>"
      d `shouldSatisfy` T.isInfixOf "no pipe"
      d `shouldSatisfy` T.isInfixOf "no `cd`"
      d `shouldSatisfy` T.isInfixOf "never a reason for BLOCKED"
      errandPrompt Do pyWords Nothing plan ls step [] `shouldSatisfy` T.isInfixOf "never a reason for BLOCKED"
      errandPrompt Do pyWords Nothing plan ls step [] `shouldSatisfy` T.isInfixOf "one line per file you changed that holds no such unit (a stylesheet, a template, a data file), exactly\nDONE @<path>\n"
      errandPrompt Do pyWords Nothing plan ls step [] `shouldSatisfy` (not . T.isInfixOf "run the relevant tests")
    it "cuts a long body at 2000 characters and limits grounding to 20 units" $ do
      T.length (cutBody (T.replicate 3000 "x")) `shouldBe` 2002
      T.takeEnd 1 (cutBody (T.replicate 3000 "x")) `shouldBe` "…"
      cutBody "short" `shouldBe` "short"
      groundingLimit `shouldBe` 20

pyWords :: T.Text
pyWords = "a top-level class or function name (Name) or a method (Class.method)"
