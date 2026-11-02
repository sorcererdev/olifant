{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.Language.TestFormatsSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Language.Test (Failure (..), TestRun (..), ownedBy)
import Olifant.Language.TestFormats
import System.Exit (ExitCode (..))
import Test.Hspec

hspecLines :: LinesFormat
hspecLines =
  LinesFormat
    { lfPass = "^[[:space:]]*(.*[^[:space:]]) \\[✔\\]$"
    , lfFail = "^[[:space:]]*(.*[^[:space:]]) \\[✘\\]$"
    , lfNest = True
    , lfFailure = Just "^  ([^[:space:]].*):([0-9]+):[0-9]+: *$"
    , lfEnd = Just "^  To rerun use|^Randomized with seed|^Finished in"
    , lfDrop = Just "^  [0-9]+\\) "
    , lfSplit = Nothing
    }

files :: [(FilePath, Maybe Text)]
files = [("src/Olifant/PlansSpec.hs", Just "Olifant.Plans")]

spec :: Spec
spec = do
  describe "lines (hspec's checks)" $ do
    it "reads hspec's checks: ids under the file, the failure's line and message" $ do
      out <- TIO.readFile "src/Olifant/Language/testdata/hspec/checks-one-failure.txt"
      let run = fromReported files (ExitFailure 1) out (parseLines hspecLines out)
      trPassed run `shouldBe` ["src/Olifant/PlansSpec.hs::parsePlan::reads a step", "src/Olifant/PlansSpec.hs::with spaces, commas/slash::x/y, z"]
      map fst (trFailed run) `shouldBe` ["src/Olifant/PlansSpec.hs::parsePlan::reads a test"]
      let [(_, f)] = trFailed run
      fLine f `shouldBe` Just 6
      fMessage f `shouldBe` "expected: 2\n but got: 1"
      trBroken run `shouldBe` Nothing
    it "a spec that does not compile is broken with GHC's words" $ do
      out <- TIO.readFile "src/Olifant/Language/testdata/hspec/checks-compile-error.txt"
      let run = fromReported files (ExitFailure 1) out (parseLines hspecLines out)
      trBroken run `shouldSatisfy` maybe False ("Variable not in scope: nope" `T.isInfixOf`)
      trPassed run `shouldBe` []
    it "a clean exit with no tests is broken: no tests ran" $
      trBroken (fromReported files ExitSuccess "" []) `shouldBe` Just "no tests ran"
    it "more failures than blocks: the rest fail with no line" $ do
      let out = "G\n  a [✘]\n  b [✘]\n\nFailures:\n\n  src/Olifant/PlansSpec.hs:3:1: \n  1) G a\n       boom\n\n  To rerun use: x\n"
          rs = parseLines hspecLines out
      map rpFailure rs `shouldBe` [Just (Failure (Just 3) "boom" "1) G a\n     boom"), Just (Failure Nothing "failed" "")]
    it "flat lines: a test is named by its line alone" $ do
      let flat = LinesFormat {lfPass = "^test (.*) \\.\\.\\. ok$", lfFail = "^test (.*) \\.\\.\\. FAILED$", lfNest = False, lfFailure = Nothing, lfEnd = Nothing, lfDrop = Nothing, lfSplit = Nothing}
      map rpSegments (parseLines flat "running 2 tests\ntest a::b ... ok\n  test c ... FAILED\n") `shouldBe` [["a::b"]]
  describe "attribution" $ do
    it "attributes by group across several files, else the only file, else none" $ do
      let two = [("a/XSpec.hs", Just "X"), ("b/YSpec.hs", Just "Y")]
      attribute two (Reported Nothing ["Y", "g", "t"] Nothing) `shouldBe` "b/YSpec.hs::g::t"
      attribute [("f", Nothing)] (Reported Nothing ["t"] Nothing) `shouldBe` "f::t"
      attribute [("f", Nothing), ("g", Nothing)] (Reported Nothing ["t"] Nothing) `shouldBe` "t"
    it "a report's path names the file, made relative and normalised" $ do
      let two = [("a/XSpec.hs", Just "X"), ("b/YSpec.hs", Just "Y")]
      attribute two (Reported (Just "./b/YSpec.hs") ["Y", "t"] Nothing) `shouldBe` "b/YSpec.hs::t"
      attribute two (Reported (Just "/tmp/olifant-test-1/b/YSpec.hs") ["t"] Nothing) `shouldBe` "b/YSpec.hs::t"
    it "a name with :: or a slash stays one test" $ do
      attribute files (Reported Nothing ["Olifant.Plans", "a::b/c, d"] Nothing) `shouldBe` "src/Olifant/PlansSpec.hs::a::b/c, d"
    it "ownedBy: the test, its parameters, a group's tests; not a sibling with a longer name" $ do
      ownedBy "f::g" "f::g::t" `shouldBe` True
      ownedBy "f::t" "f::t[1]" `shouldBe` True
      ownedBy "f::t" "f::t" `shouldBe` True
      ownedBy "f::t" "f::tt" `shouldBe` False
  describe "tap" $ do
    it "ok passes, not ok fails with its YAML block, a SKIP is neither" $ do
      out <- TIO.readFile "src/Olifant/Language/testdata/tap/basic.tap"
      let Right rs = parseTap out
          run = fromReported [("t/plans.t", Nothing)] (ExitFailure 1) out rs
      trPassed run `shouldBe` ["t/plans.t::parses a step", "t/plans.t::reads, a comma"]
      map fst (trFailed run) `shouldBe` ["t/plans.t::parses a test"]
      let [(_, f)] = trFailed run
      fMessage f `shouldBe` "message: expected 2, got 1\nseverity: fail"
    it "Bail out! is no run, with its reason" $
      parseTap "1..3\nok 1 - a\nBail out! database is down\n" `shouldBe` Left "database is down"
  describe "junit-xml" $ do
    it "a testcase passes, fails with its message, text and line, or is skipped" $ do
      out <- TIO.readFile "src/Olifant/Language/testdata/junit/basic.xml"
      let Right rs = parseJunit noJunitFormat out
          run = fromReported [("src/plans.test.ts", Nothing)] (ExitFailure 1) out rs
      trPassed run `shouldBe` ["src/plans.test.ts::reads a step"]
      let [(i, f)] = trFailed run
      i `shouldBe` "src/plans.test.ts::reads a test"
      (fLine f, fMessage f) `shouldBe` (Just 14, "expected 2, got 1")
      fOutput f `shouldSatisfy` T.isInfixOf "at src/plans.test.ts:14:5"
    let pytestJ = JunitFormat (Just ".") Nothing
        pyFiles = [("tests/test_x.py", Nothing)]
        pyReport = TIO.readFile "src/Olifant/Language/testdata/junit/pytest-xunit1.xml"
        inline body = parseJunit (JunitFormat (Just "/") Nothing) ("<testsuites><testsuite>" <> body <> "</testsuite></testsuites>")
    it "pytest's xunit1 report: ids are pytest's own, from file, classname and name" $ do
      out <- pyReport
      let Right rs = parseJunit pytestJ out
          run = fromReported pyFiles (ExitFailure 1) out rs
      trPassed run `shouldBe` ["tests/test_x.py::test_ok", "tests/test_x.py::TestGroup::test_param[1]"]
      map fst (trFailed run) `shouldBe` ["tests/test_x.py::test_eq", "tests/test_x.py::TestGroup::test_inst", "tests/test_x.py::TestGroup::test_param[2.5]", "tests/test_x.py::test_boom"]
    it "a failure's line is the last FILE:N in its text for the test's file, not pytest's 0-based def line" $ do
      out <- pyReport
      let Right rs = parseJunit pytestJ out
          run = fromReported pyFiles (ExitFailure 1) out rs
          lineOf i = lookup i [(k, fLine f) | (k, f) <- trFailed run]
      -- (the fixture's assert lines; pytest's own `line` attributes are 7 and 16)
      lineOf "tests/test_x.py::test_eq" `shouldBe` Just (Just 9)
      lineOf "tests/test_x.py::TestGroup::test_inst" `shouldBe` Just (Just 18)
      let Right [r] = inline "<testcase classname=\"a\" name=\"t\" file=\"a.hs\" line=\"4\"><failure message=\"m\">no frame here</failure></testcase>"
      fmap fLine (rpFailure r) `shouldBe` Just (Just 4)
    it "the message is the attribute; a one-line message its text lacks is followed by the text" $ do
      out <- pyReport
      let Right rs = parseJunit pytestJ out
          run = fromReported pyFiles (ExitFailure 1) out rs
      fmap fMessage (lookup "tests/test_x.py::test_eq" (trFailed run)) `shouldBe` Just "assert 1 == 2\n +  where 1 = f()"
      let Right [r] = inline "<testcase classname=\"a\" name=\"t\" file=\"a.hs\"><failure message=\"Received value did not match expected\">Expected: 2\n but got: 1</failure></testcase>"
      fmap fMessage (rpFailure r) `shouldBe` Just "Received value did not match expected\nExpected: 2\n but got: 1"
      basic <- TIO.readFile "src/Olifant/Language/testdata/junit/basic.xml"
      let Right brs = parseJunit noJunitFormat basic
      fmap (fmap fMessage . rpFailure) brs `shouldBe` [Nothing, Just "expected 2, got 1"]
    it "hspec-junit-formatter's report: ids as hspec's lines gave them" $ do
      out <- TIO.readFile "src/Olifant/Language/testdata/junit/hspec-junit.xml"
      let Right rs = parseJunit (JunitFormat (Just "/") Nothing) out
          run = fromReported files (ExitFailure 1) out rs
      trPassed run `shouldBe` ["src/Olifant/PlansSpec.hs::parsePlan::reads a step"]
      map fst (trFailed run) `shouldBe` ["src/Olifant/PlansSpec.hs::parsePlan::reads a test"]
      let [(_, f)] = trFailed run
      fLine f `shouldBe` Just 6
      fMessage f `shouldBe` "Received value did not match expected\nExpected: 2\n but got: 1"
    it "nextest's report: the binary is no group, the name is split" $ do
      out <- TIO.readFile "src/Olifant/Language/testdata/junit/nextest.xml"
      let Right rs = parseJunit (JunitFormat Nothing (Just "::")) out
          run = fromReported [("src/rates.rs", Just "rates")] (ExitFailure 100) out rs
      trPassed run `shouldBe` ["src/rates.rs::tests::adds"]
      map fst (trFailed run) `shouldBe` ["src/rates.rs::tests::fails"]
      fmap fLine (lookup "src/rates.rs::tests::fails" (trFailed run)) `shouldBe` Just Nothing
    it "a testcase with no classname and an error is the run's breakage" $ do
      out <- TIO.readFile "src/Olifant/Language/testdata/junit/pytest-collection-error.xml"
      parseJunit pytestJ out `shouldSatisfy` either (T.isInfixOf "No module named 'nope'") (const False)
    it "split splits the name; without join the classname is left out" $ do
      let Right rs = parseJunit (JunitFormat Nothing (Just "::")) "<testsuite><testcase classname=\"money\" name=\"rates::tests::adds\"/></testsuite>"
      map (attribute [("src/rates.rs", Just "rates")]) rs `shouldBe` ["src/rates.rs::tests::adds"]
    it "a testsuite's file stands for its testcases'" $ do
      let Right rs = parseJunit (JunitFormat (Just "/") Nothing) "<testsuites><testsuite file=\"src/Olifant/PlansSpec.hs\"><testcase classname=\"Olifant.Plans/parsePlan\" name=\"reads a step\"/></testsuite></testsuites>"
      map (attribute [("src/Olifant/PlansSpec.hs", Just "Olifant.Plans")]) rs `shouldBe` ["src/Olifant/PlansSpec.hs::parsePlan::reads a step"]
    it "a report with no testsuite is no run" $
      either (const True) (const False) (parseJunit noJunitFormat "error: cannot open report.xml") `shouldBe` True
  describe "olifant-json" $ do
    it "reads passed, failed and broken" $
      parseTestJson "{\"passed\":[\"a.hs::t\"],\"failed\":[{\"id\":\"a.hs::u\",\"line\":3,\"message\":\"m\",\"output\":\"o\"}],\"broken\":null}"
        `shouldBe` Right (TestRun ["a.hs::t"] [("a.hs::u", Failure (Just 3) "m" "o")] Nothing)
    it "a broken run says why; absent keys are empty" $
      parseTestJson "{\"broken\":\"no database\"}" `shouldBe` Right (TestRun [] [] (Just "no database"))
    it "bad input names the format" $
      either (T.isInfixOf "olifant-json") (const False) (parseTestJson "[1]") `shouldBe` True
