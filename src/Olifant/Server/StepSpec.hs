{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.Server.StepSpec (spec) where

import Control.Arrow ((&&&))
import Data.Text qualified as T
import Olifant.Language (Failure (..), TestRun (..))
import Olifant.Server.ConjureStep (sessionDiff)
import Olifant.Server.Step
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import Test.Hspec

situation :: Situation
situation =
  Situation
    { siLanguage = "Python"
    , siTestNaming = "tests are files named <module>__test.py beside their module"
    , siFile = "src/x.py"
    , siUnit = "handle"
    , siLine = 6
    , siSpan = (5, 7)
    , siModule = "def handle():\n    pass\n"
    , siTestFile = "src/x__test.py"
    , siTestText = Nothing
    , siTestsBefore = "none (no test file)."
    , siRelated = []
    , siSession = ""
    }

spec :: Spec
spec = describe "Olifant.Server.Step (a conjure is one step)" $ do
  describe "readStep" $ do
    let reply = "<goal>fill the body</goal>\n<file path=\"a/b.py\">\nx = 1\n</file>"
    it "reads the goal and each file block" $
      readStep reply `shouldBe` StepReply (Just "fill the body") [("a/b.py", "x = 1\n")]
    it "reads the same blocks through a markdown fence and prose around them" $
      readStep ("Here is the step.\n```\n" <> reply <> "\n```\nDone.") `shouldBe` StepReply (Just "fill the body") [("a/b.py", "x = 1\n")]
    it "a fence inside a file block is not the file's" $
      srFiles (readStep "<goal>g</goal>\n<file path=\"a.py\">\n```python\nx = 1\n```\n</file>") `shouldBe` [("a.py", "x = 1\n")]
    it "nothing to do: the goal nothing and no files" $
      (srGoal &&& srFiles) (readStep "<goal>nothing</goal>") `shouldBe` (Just "nothing", [])
  describe "keptPath" $ do
    it "keeps a relative path inside the project" $ keptPath "/p" "src/a.py" `shouldBe` Just "src/a.py"
    it "keeps an absolute path inside the project, relative" $ keptPath "/p" "/p/src/a.py" `shouldBe` Just "src/a.py"
    it "drops escapes, absolute paths outside, .git and .olifant" $
      map (keptPath "/p") ["../x.py", "src/../../x.py", "/etc/x", ".git/config", ".olifant/state/x", "/p/.git/HEAD"] `shouldBe` replicate 6 Nothing
  describe "the prompt" $ do
    it "the system calls the body work in progress and never asks for the least code" $ do
      stepSystem `shouldSatisfy` T.isInfixOf "not a rejected attempt"
      stepSystem `shouldNotSatisfy` T.isInfixOf "least code"
      stepSystem `shouldNotSatisfy` T.isInfixOf "Python"
    it "the situation names the language, unit, line and where tests would go when there is no test file" $ do
      let t = situationText situation
      mapM_ (\k -> t `shouldSatisfy` T.isInfixOf k) ["Python", "<module>__test.py", "`handle`", "line 6", "There is no test file for this module yet (its tests would go in src/x__test.py)", "Tests before the step: none"]
    it "the test file's text when there is one" $
      situationText situation {siTestText = Just "def test_x(): pass\n"} `shouldSatisfy` T.isInfixOf "def test_x(): pass"
    it "the session block only when there is one" $ do
      situationText situation `shouldNotSatisfy` T.isInfixOf "What the person has been doing"
      situationText situation {siSession = "diff --git a/x b/x"} `shouldSatisfy` T.isInfixOf "What the person has been doing (uncommitted changes):\ndiff --git a/x b/x"
    it "related files" $
      situationText situation {siRelated = [("src/y.py", "Y = 1\n")]} `shouldSatisfy` T.isInfixOf "Related file src/y.py:"
  describe "testDelta" $ do
    it "newly passing, newly failing (passed before) and still red" $
      testDelta (Just (TestRun ["t2", "t3"] [("t1", f), ("t4", f)] Nothing)) (Just (TestRun ["t1", "t3"] [("t2", f), ("t4", f)] Nothing))
        `shouldBe` TestDelta ["t1"] ["t2"] ["t4"] False
    it "tests that could not run before and fail after are still red, not a regression" $
      testDelta (Just (TestRun [] [] (Just "SyntaxError"))) (Just (TestRun [] [("t1", f)] Nothing)) `shouldBe` TestDelta [] [] ["t1"] False
    it "a new test that fails is still red, not newly failing" $
      testDelta (Just (TestRun ["t1"] [] Nothing)) (Just (TestRun ["t1"] [("t2", f)] Nothing)) `shouldBe` TestDelta [] [] ["t2"] False
    it "a test that passed before and fails after is newly failing" $
      tdFailing (testDelta (Just (TestRun ["t1"] [] Nothing)) (Just (TestRun [] [("t1", f)] Nothing))) `shouldBe` ["t1"]
    it "no after-run: no tests" $
      testDelta Nothing Nothing `shouldBe` TestDelta [] [] [] True
    it "a new test file: every passing test is newly passing" $
      testDelta Nothing (Just (TestRun ["t1"] [] Nothing)) `shouldBe` TestDelta ["t1"] [] [] False
  describe "the step log line" $
    it "writer, goal, files, tests and the choice" $
      stepLogLine "claude-haiku-5-5" "fill the body" 2 (TestDelta ["a", "b", "c"] [] [] False) "accepted"
        `shouldBe` "conjure step claude-haiku-5-5 · fill the body · 2 files · +3 tests · accepted"
  describe "what the person has been doing" $ do
    let diffOf p n = (p, T.unlines (("diff --git a/" <> T.pack p <> " b/" <> T.pack p) : ["+line " <> T.pack (show i) | i <- [1 .. n :: Int]]))
    it "keeps the language's files and drops the book, .olifant and lockfiles" $ do
      let t = sessionFilter [".py"] 300 [diffOf "src/a.py" 2, diffOf "poetry.lock" 2, diffOf "book/x.md" 2, diffOf ".olifant/x.py" 2, diffOf "uv.lock" 1]
      t `shouldSatisfy` T.isInfixOf "src/a.py"
      mapM_ (\k -> t `shouldNotSatisfy` T.isInfixOf k) ["poetry.lock", "book/x.md", ".olifant/x.py", "uv.lock"]
    it "cuts a long diff at the cap and says how much is left out" $ do
      let t = sessionFilter [".py"] 300 [diffOf "src/a.py" 999]
      length (T.lines t) `shouldBe` 301
      last (T.lines t) `shouldBe` "… (700 more lines)"
    it "new files: the language's only, without the book or .olifant, at most 20 named" $ do
      untrackedLine [".py"] ["src/n.py", "book/x.py", ".olifant/y.py", "notes.md"] `shouldBe` "New files, not yet in git: src/n.py\n"
      untrackedLine [".py"] [] `shouldBe` ""
      untrackedLine [".py"] ["v/f" <> show i <> ".py" | i <- [1 .. 500 :: Int]] `shouldSatisfy` T.isSuffixOf "… (480 more)\n"
    it "drops binary diffs" $
      sessionFilter [".py"] 300 [("src/a.py", "diff --git a/src/a.py b/src/a.py\nBinary files a/src/a.py and b/src/a.py differ\n")] `shouldBe` ""
    it "not a git repository: nothing" $ withSystemTempDirectory "olifant-session" $ \dir ->
      sessionDiff dir [".py"] `shouldReturn` ""
    it "a repository with an edited file: its diff" $ withSystemTempDirectory "olifant-session" $ \dir -> do
      createDirectoryIfMissing True (dir </> "src")
      writeFile (dir </> "src/a.py") "x = 1\n"
      let git args = callProcess "git" (["-C", dir, "-c", "user.email=t@t", "-c", "user.name=t"] ++ args)
      git ["init", "-q"] >> git ["add", "."] >> git ["commit", "-q", "-m", "one"]
      writeFile (dir </> "src/a.py") "x = 2\n"
      writeFile (dir </> "src/new.py") "y = 1\n"
      t <- sessionDiff dir [".py"]
      t `shouldSatisfy` T.isInfixOf "+x = 2"
      t `shouldSatisfy` T.isInfixOf "src/new.py"
  where
    f = Failure Nothing "x" ""
