{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.LanguageSpec (spec) where

import Data.Aeson (encode, object, (.=))
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.ByteString qualified as BS
import Data.List (nub)
import Data.Maybe (isJust, listToMaybe)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative)
import Data.Text.IO qualified as TIO
import Olifant.Language.Elm (parseElmReport, qualifyPattern, visibleAt)
import Olifant.Anchor (Miss (..), RenderedAnchor (..), Spacing (..), locIndent, locLine, locOpen, locSpacing, insertLines, presentAt, resolveLayers)
import Olifant.Language
import Olifant.Language.Presets (shippedPresets)
import Olifant.Toml (mergeTables, parseToml)
import Olifant.Language.AstGrep (astGrepSyntax)
import Olifant.Server (Unit (..), unitAt)
import System.Directory (doesDirectoryExist, findExecutable, getCurrentDirectory, getTemporaryDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, (</>))
import Olifant.Template (Value (..))
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (filterM, forM, forM_)
import System.IO.Temp (withSystemTempDirectory)
import Data.Text.Encoding qualified as TE
import Olifant.Language.Ghc (parseGhcReport, typeOf)
import Olifant.Language.Header (headerEnd)
import Olifant.Language.Pyright (parsePyrightReport, pyrightSupport)
import Olifant.Language.Rustc (parseRustcReport, rustFunctionAt, rustcSupport)
import Olifant.Output (anchorsHuman, anchorsJson)
import Olifant.Process (neverCancel)
import Data.IORef (newIORef, readIORef, writeIORef)
import Test.Hspec

cb :: Text
cb = "src/bistro/booking/commands/cancel_booking__test.py"

elmToml :: Text
elmToml =
  T.unlines
    [ "files = [\"src/**/*.elm\"]"
    , "comment = \"--\""
    , "grammar = { source = \"grammar/elm\" }"
    , "placeholder = 'Debug.todo \"{= message =}\"'"
    , "branch = \"{= pattern =} ->\\n    {= body =}\""
    , "[check]"
    , "command = [\"elm\", \"make\", \"{= files =}\", \"--report=json\", \"--output=/dev/null\"]"
    , "context = [\"elm.json\"]"
    , "format = \"elm-json\""
    , "[targets.case_in]"
    , "phrase = \"case in FUNCTION\""
    , "container = { kind = \"case_of_expr\", inside = { kind = \"value_declaration\", regex = \"^{= FUNCTION =}\\\\b\", stopBy = \"end\" } }"
    , "items = { kind = \"case_of_branch\" }"
    , "example = \"end of case in update\""
    ]

spec :: Spec
spec = do
  describe "presets" $ do
    let py = parseLanguage "python" "/pack" . T.unlines
    it "(a) [test] preset = \"pytest\" fills command, context, format and root" $ do
      let Right lang = py ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\"", "[test]", "preset = \"pytest\"", "tests_for = \"{= dir =}/{= stem =}__test.py\""]
          Just t = langTest lang
      testFormat t `shouldBe` "pytest"
      testRoot t `shouldBe` ["pyproject.toml"]
      testsFor t `shouldBe` "{= dir =}/{= stem =}__test.py"
      last (testCommand t) `shouldBe` "{= tests =}"
    it "(b) the table's own keys win over the preset's" $ do
      let Right lang = py ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\"", "[test]", "preset = \"pytest\"", "tests_for = \"tests/test_{= stem =}.py\"", "root = [\"setup.cfg\"]"]
      fmap testRoot (langTest lang) `shouldBe` Just ["setup.cfg"]
    it "(c) an unknown preset names the ones that exist" $
      py ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\"", "[test]", "preset = \"jest\"", "tests_for = \"x\""]
        `shouldSatisfy` either (\e -> "pytest" `T.isInfixOf` e && "jest" `T.isInfixOf` e) (const False)
    it "(d) [test] needs tests_for, which the pytest preset gives" $ do
      py ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\"", "[test]", "command = [\"pytest\"]", "format = \"pytest\""] `shouldSatisfy` either ("tests_for" `T.isInfixOf`) (const False)
      let Right lang = py ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\"", "[test]", "preset = \"pytest\""]
      fmap testsFor (langTest lang) `shouldBe` Just "{= dir =}/test_{= stem =}.py"
    it "(e) [check] preset = \"pyright\" is the old table" $ do
      let Right lang = py ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\"", "[check]", "preset = \"pyright\""]
      fmap checkFormat (langCheck lang) `shouldBe` Just "pyright-json"
      fmap checkRoot (langCheck lang) `shouldBe` Just ["pyproject.toml"]
    let hs = parseLanguage "haskell" "/pack" . T.unlines . (["files = [\"**/*.hs\"]", "comment = \"--\"", "grammar = \"haskell\""] ++)
    it "(f) [test] preset = \"hspec\" alone: lines, exclusive, 900 s, tests_for, select and unit" $ do
      let Right lang = hs ["[test]", "preset = \"hspec\""]
          Just t = langTest lang
      (testFormat t, testExclusive t, testTimeout t, testsFor t) `shouldBe` ("lines", True, Just 900, "{= dir =}/{= stem =}Spec.hs")
      fmap lfNest (testLines t) `shouldBe` Just True
      testSelect t `shouldBe` Just (Select ["--test-option=--match=/{= group =}/{= name =}/"] ["--test-option=--match=/{= group =}/"] "/")
      testFileGroup t `shouldBe` Just "^module[[:space:]]+([[:alnum:]_.]+)Spec"
      testUnitRules t `shouldBe` [UnitRule "(^|[[:space:]$])(describe|context|it|specify|prop)[[:space:]]+\"(([^\"\\\\]|\\\\.)*)\"" Nothing (Just 3)]
      take 3 (testCommand t) `shouldBe` ["cabal", "test", "-v0"]
      last (testCommand t) `shouldBe` "{= tests =}"
    it "(g) a pack's own tests_for wins over hspec's" $
      fmap (fmap testsFor . langTest) (hs ["[test]", "preset = \"hspec\"", "tests_for = \"test/{= stem =}Spec.hs\""]) `shouldBe` Right (Just "test/{= stem =}Spec.hs")
    it "(h) [check] preset = \"cabal-repl\" alone: ghc-json, exclusive, a builddir in the cache" $ do
      let Right lang = hs ["[check]", "preset = \"cabal-repl\""]
          Just c = langCheck lang
      (checkFormat c, checkExclusive c) `shouldBe` ("ghc-json", True)
      checkCommand c `shouldSatisfy` elem "--builddir={= cache =}/dist-repl-{= context =}"
    it "(i) an unknown preset names the new ones" $ do
      hs ["[check]", "preset = \"stack\""] `shouldSatisfy` either ("cabal-repl" `T.isInfixOf`) (const False)
      hs ["[test]", "preset = \"tasty\"", "tests_for = \"x\""] `shouldSatisfy` either ("hspec" `T.isInfixOf`) (const False)
  describe "providers: a slot takes { provider = [...] } (4b m1)" $ do
    let lang = parseLanguage "python" "/pack" . T.unlines . (["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\""] ++)
        runner = ["[test]", "command = [\"t\"]", "format = \"lines\"", "tests_for = \"x\"", "[test.lines]", "pass = 'ok'", "fail = 'no'"]
    it "a slot takes `{ provider = [...] }`: units, test units, selection and anchors" $ do
      let Right l = lang (["anchors = { provider = [\"sh\", \"tools/anchor.sh\"] }", "[unit]", "kinds = { provider = [\"sh\", \"tools/units.sh\"] }", "[test]", "command = [\"t\"]", "format = \"lines\"", "tests_for = \"x\"", "unit = { provider = [\"sh\", \"tools/tests.sh\"] }", "select = { provider = [\"sh\", \"tools/select.sh\"] }", "[test.lines]", "pass = 'ok'", "fail = 'no'"])
      langUnitProvider l `shouldBe` Just (Provider ["sh", "tools/units.sh"] Nothing)
      langUnitKinds l `shouldBe` []
      langAnchorProvider l `shouldBe` Just (Provider ["sh", "tools/anchor.sh"] Nothing)
      (langTest l >>= testUnitProvider) `shouldBe` Just (Provider ["sh", "tools/tests.sh"] Nothing)
      fmap testUnitRules (langTest l) `shouldBe` Just []
      (langTest l >>= testSelectProvider) `shouldBe` Just (Provider ["sh", "tools/select.sh"] Nothing)
      (langTest l >>= testSelect) `shouldBe` Nothing
      (hasUnits l, hasTestUnits l) `shouldBe` (True, True)
    it "without providers, none; hasUnits and hasTestUnits follow the rules" $ do
      let Right l = lang runner
      (langUnitProvider l, langAnchorProvider l, langTest l >>= testUnitProvider, langTest l >>= testSelectProvider) `shouldBe` (Nothing, Nothing, Nothing, Nothing)
      -- 4b m2: no unit rule, but python's grammar is linked: its queries give units
      (hasUnitRule l, hasUnits l, hasTestUnits l) `shouldBe` (False, True, False)
    it "an empty provider, or a provider with another key beside it, is an error naming the key" $ do
      lang ["[unit]", "kinds = { provider = [] }"] `shouldSatisfy` either ("unit.kinds" `T.isInfixOf`) (const False)
      lang ["[unit]", "kinds = { provider = [\"x\"], kinds = [] }"] `shouldSatisfy` either ("unit.kinds" `T.isInfixOf`) (const False)
      lang ["anchors = { provider = [] }"] `shouldSatisfy` either ("anchors" `T.isInfixOf`) (const False)
      lang (runner ++ ["[test.select]", "provider = [\"x\"]", "test = \"a\"", "file = \"b\""]) `shouldSatisfy` either ("test.select" `T.isInfixOf`) (const False)
  describe "[conjure]: a pack's architecture" $ do
    let lang = parseLanguage "python" "/pack" . T.unlines . (["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\""] ++)
    it "parses its service, port, fake and constructor; absent, there is none" $ do
      let Right l = lang ["[conjure]", "service = \"src/{= app =}/{= service =}\"", "[conjure.port]", "files = [\"ports/*.py\"]", "start = '^class ([a-z]+)'", "method = '^def '", "[conjure.fake]", "bound = '^ +([a-z]+) = ([A-Za-z]+)\\('", "prefixes = [\"Fake\"]"]
      fmap cjService (langConjure l) `shouldBe` Just (Just "src/{= app =}/{= service =}")
      (langConjure l >>= cjPort) `shouldBe` Just (PortShape ["ports/*.py"] "^class ([a-z]+)" "^def ")
      (langConjure l >>= cjFake) `shouldBe` Just (FakeShape "^ +([a-z]+) = ([A-Za-z]+)\\(" [] ["Fake"] Nothing Nothing)
      (langConjure l >>= cjConstructor) `shouldBe` Nothing
      fmap langConjure (lang []) `shouldBe` Right Nothing
    it "an unknown key, a port without its start, or a bad expression is an error" $ do
      either (const True) (const False) (lang ["[conjure]", "ports = 1"]) `shouldBe` True
      either (const True) (const False) (lang ["[conjure.port]", "method = '^def '"]) `shouldBe` True
      either (const True) (const False) (lang ["[conjure.port]", "start = '('", "method = '^def '"]) `shouldBe` True
    it "parses the port's add and adapters and the advice; adapters without add is an error" $ do
      let Right l = lang ["[conjure.port]", "start = '^class ([a-z]+)'", "method = '^def '", "add = \"m\"", "adapters = \"a\"", "[[conjure.advice]]", "when = '^def h\\('", "say = \"use h\""]
      (langConjure l >>= cjAddMethod) `shouldBe` Just (AddMethod "m" (Just "a"))
      fmap cjAdvice (langConjure l) `shouldBe` Just [Advice "^def h\\(" "use h"]
      (lang ["[conjure.port]", "start = '^class ([a-z]+)'", "method = '^def '"] >>= maybe (Left "none") Right . langConjure) `shouldSatisfy` either (const False) (\c -> cjAddMethod c == Nothing && null (cjAdvice c))
      either (const True) (const False) (lang ["[conjure.port]", "start = '^class ([a-z]+)'", "method = '^def '", "adapters = \"a\""]) `shouldBe` True
  describe "[types] and undefined names: checked as they load" $ do
    let lang = parseLanguage "python" "/pack" . T.unlines . (["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\""] ++)
        bad = either (const True) (const False)
    it "a [types] template's variables and a [types] expression are checked" $ do
      bad (lang ["[types]", "param = \"{= nme =}: {= type =}\""]) `shouldBe` True
      bad (lang ["[types]", "optional = \"{= typ =} | None\""]) `shouldBe` True
      bad (lang ["[types]", "method = \"{= name =}({= args =})\""]) `shouldBe` True
      bad (lang ["[types]", "annotation = '{= name =}:[[:space:]]*(['"]) `shouldBe` True
      bad (lang ["[types]", "record = '^class {= nam =}'"]) `shouldBe` True
      bad (lang ["[types]", "checks_none = ['{= name =} is (']"]) `shouldBe` True
      bad (lang ["[types]", "param = \"{= name =}: {= type =}\"", "optional = \"{= type =} | None\"", "method = \"{= name =}(self{= params =}) -> {= returns =}\"", "annotation = '{= name =}:[[:space:]]*([^,)=]+)'", "record = '^class {= name =}[(:]'", "checks_none = ['{= name =} is None']"]) `shouldBe` False
    it "an undefined-name expression with no group is an error (the name is its last group)" $ do
      bad (lang ["[check]", "command = [\"c\"]", "format = \"pyright-json\"", "undefined = ['is not defined']"]) `shouldBe` True
      bad (lang ["[check]", "command = [\"c\"]", "format = \"pyright-json\"", "undefined = [{ pattern = 'is not defined' }]"]) `shouldBe` True
      bad (lang ["[check]", "command = [\"c\"]", "format = \"pyright-json\"", "undefined = ['\"(.*)\" is not defined']"]) `shouldBe` False
  describe "Olifant.Language.Test" $ do
    it "(a) parsePytest: one failed, four passed, the failing line" $ do
      out <- TIO.readFile "src/Olifant/Language/testdata/pytest/one-failed.txt"
      let run = parsePytest (ExitFailure 1) out
      length (trPassed run) `shouldBe` 4
      map fst (trFailed run) `shouldBe` ["src/bistro/booking/commands/close_table__test.py::test_close_table_succeeds"]
      fmap (fLine . snd) (listToMaybe (trFailed run)) `shouldBe` Just (Just 14)
      fmap (T.take 19 . fMessage . snd) (listToMaybe (trFailed run)) `shouldBe` Just "NotImplementedError"
      trBroken run `shouldBe` Nothing
    it "(b) all passed" $ do
      out <- TIO.readFile "src/Olifant/Language/testdata/pytest/all-passed.txt"
      parsePytest ExitSuccess out `shouldSatisfy` \r -> null (trFailed r) && length (trPassed r) == 4 && trBroken r == Nothing
    it "(c) testsForFile: the module's test file" $
      testsForFile (TestDef [] [] "pytest" [] "{= dir =}/{= stem =}__test.py" Nothing [] [] Nothing Nothing False Nothing [] Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing) "src/bistro/booking/commands/close_table.py"
        `shouldBe` "src/bistro/booking/commands/close_table__test.py"
    it "(d) a run that could not start is broken, not red" $ do
      out <- TIO.readFile "src/Olifant/Language/testdata/pytest/broken.txt"
      let run = parsePytest (ExitFailure 2) out
      trFailed run `shouldBe` []
      trBroken run `shouldSatisfy` maybe False ("ImportError" `T.isInfixOf`)
    it "(d2) a failing method of a test class keeps its line and message" $ do
      let out = T.unlines
            [ "=================================== FAILURES ==================================="
            , "______________________________ TestX.test_y ______________________________"
            , "pkg/a__test.py:7: in test_y"
            , "    assert add(1, 2) == 4"
            , "E   assert 3 == 4"
            , "==================================== PASSES ===================================="
            , "=========================== short test summary info ============================"
            , "FAILED pkg/a__test.py::TestX::test_y"
            , "1 failed in 0.01s" ]
          run = parsePytest (ExitFailure 1) out
      fmap (fLine . snd) (listToMaybe (trFailed run)) `shouldBe` Just (Just 7)
      fmap (fMessage . snd) (listToMaybe (trFailed run)) `shouldBe` Just "assert 3 == 4"
    it "(f) the whole E block is the message; the one-line form names the assertion" $ do
      r2 <- parsePytest (ExitFailure 1) <$> TIO.readFile "src/Olifant/Language/testdata/pytest/cancel-booking-round2.txt"
      let Just f = lookup (cb <> "::test_cancel_booking_succeeds") (trFailed r2)
      fMessage f `shouldBe` "AssertionError: assert NotFound(reason='Booking not found') == Ok(value=None)\n+  where Ok(value=None) = Ok(None)"
      failureLineIn (shippedLanguageNow "python") f `shouldBe` "assert NotFound(reason='Booking not found') == Ok(value=None)"
      assertionLineIn (shippedLanguageNow "python") "test_cancel_booking_succeeds" f `shouldBe` Just "assert result == Ok(None)"
      gotExpectedIn (shippedLanguageNow "python") f `shouldBe` Just ("NotFound(reason='Booking not found')", "Ok(value=None)")
    it "(g) assert isinstance: the where line is the one-line form; got is the value, expected the type" $ do
      r1 <- parsePytest (ExitFailure 1) <$> TIO.readFile "src/Olifant/Language/testdata/pytest/cancel-booking-round1.txt"
      let Just f = lookup (cb <> "::test_cancel_booking_not_found") (trFailed r1)
      failureLineIn (shippedLanguageNow "python") f `shouldBe` "False = isinstance(Ok(value=None), NotFound)"
      assertionLineIn (shippedLanguageNow "python") "test_cancel_booking_not_found" f `shouldBe` Just "assert isinstance(result, NotFound)"
      gotExpectedIn (shippedLanguageNow "python") f `shouldBe` Just ("Ok(value=None)", "NotFound")
      fLine f `shouldBe` Just 26
      trPassed r1 `shouldBe` [cb <> "::test_cancel_booking_succeeds"]
    it "(h) a raised error before the assertion: the error is the message, no got/expected" $ do
      let out = T.unlines
            [ "=================================== FAILURES ==================================="
            , "____________________________________ test_x ____________________________________"
            , "src/x__test.py:5: in test_x"
            , "    result = f()"
            , "src/x.py:3: in f"
            , "    raise TypeError(\"bad\")"
            , "E   TypeError: bad"
            , "=========================== short test summary info ============================"
            , "FAILED src/x__test.py::test_x"
            , "1 failed in 0.01s" ]
          Just f = lookup "src/x__test.py::test_x" (trFailed (parsePytest (ExitFailure 1) out))
      fMessage f `shouldBe` "TypeError: bad"
      failureLineIn (shippedLanguageNow "python") f `shouldBe` "TypeError: bad"
      assertionLineIn (shippedLanguageNow "python") "test_x" f `shouldBe` Just "result = f()"
      gotExpectedIn (shippedLanguageNow "python") f `shouldBe` Nothing
    it "(i) no section, a -rA tail: the tail is the message" $ do
      let out = T.unlines ["FAILED pkg/a__test.py::test_y - assert 3 == 4", "1 failed in 0.01s"]
          Just f = lookup "pkg/a__test.py::test_y" (trFailed (parsePytest (ExitFailure 1) out))
      fMessage f `shouldBe` "assert 3 == 4"
      failureLineIn (shippedLanguageNow "python") f `shouldBe` "assert 3 == 4"
    it "(j) the scripted runner carries output" $ do
      Right run <- runTests (scriptedRunner (M.fromList [("t.py::test_a", object ["line" .= (2 :: Int), "message" .= ("m" :: Text), "output" .= ("S" :: Text)])])) undefined ["t.py::test_a"] M.empty
      map (fOutput . snd) (trFailed run) `shouldBe` ["S"]
    it "(e) processRunner on the fixture: a red test is red at its assert (uv)" $ do
      strict <- (== Just "1") <$> lookupEnv "OLIFANT_REQUIRE_TOOLS"
      findExecutable "uv" >>= \case
        Nothing -> if strict then expectationFailure "required tool missing: uv" else pendingWith "uv is not installed"
        Just _ -> do
          fixture <- readFixtureTree "packs/python-htmx/fixtures/app"
          toml <- TIO.readFile "packs/python-htmx/languages/python.toml"
          let Right lang = parseOverShipped "python" "packs/python-htmx" toml
              red = "src/bistro/booking/commands/red__test.py"
              files = M.insert red "def test_red() -> None:\n    assert 1 == 2\n" fixture
          run <- runTestsIn (processRunner neverCancel 120) lang [T.pack red] files
          fmap (map fst . trFailed) run `shouldBe` Right [T.pack red <> "::test_red"]
          fmap (map (fLine . snd) . trFailed) run `shouldBe` Right [Just 2]
  describe "[test] formats and selection" $ do
    let hsToml extra = T.unlines $
          [ "files = [\"**/*.hs\"]", "comment = \"--\"", "grammar = \"haskell\""
          , "[test]"
          , "format = \"lines\""
          , "tests_for = \"{= dir =}/{= stem =}Spec.hs\""
          , "file_group = '^module[[:space:]]+([[:alnum:]_.]+)Spec'"
          , "unit = ['(^|[[:space:]$])(describe|context|it|specify|prop)[[:space:]]+\"([^\"]*)\"']"
          ] ++ extra ++
          [ "[test.select]"
          , "test = \"--test-option=--match=/{= group =}/{= name =}/\""
          , "file = \"--test-option=--match=/{= group =}/\""
          , "[test.lines]"
          , "pass = '^[[:space:]]*(.*[^[:space:]]) \\[✔\\]$'"
          , "fail = '^[[:space:]]*(.*[^[:space:]]) \\[✘\\]$'"
          , "nest = true"
          , "failure = '^  ([^[:space:]].*):([0-9]+):[0-9]+: *$'"
          , "end = '^  To rerun use|^Randomized with seed|^Finished in'"
          , "drop = '^  [0-9]+\\) '"
          ]
        fake expect = "command = [\"sh\", \"-c\", 'case \"$1\" in \"" <> expect <> "\") cat out.txt;; *) echo \"bad args: $*\"; exit 2;; esac', \"sh\", \"{= tests =}\"]"
        specFile = "src/Olifant/PlansSpec.hs"
        -- the shipped rust under a pack's [test] that keeps cargo-test and runs a fake
        rustOver ls = either (error . T.unpack) id (parseOverShipped "rust" "/r" (T.unlines (["[test]", "preset = \"cargo-test\""] ++ ls)))
    it "parses every [test] key" $ do
      let Right lang = parseLanguage "haskell" "/h" (hsToml ["command = [\"t\", \"{= tests =}\"]", "exclusive = true", "timeout = 900"])
          Just td = langTest lang
      (testFormat td, testExclusive td, testTimeout td, testConvert td) `shouldBe` ("lines", True, Just 900, [])
      testFileGroup td `shouldBe` Just "^module[[:space:]]+([[:alnum:]_.]+)Spec"
      length (testUnitRules td) `shouldBe` 1
      testSelect td `shouldBe` Just (Select ["--test-option=--match=/{= group =}/{= name =}/"] ["--test-option=--match=/{= group =}/"] "/")
      fmap lfNest (testLines td) `shouldBe` Just True
    it "refuses lines without [test.lines], and {= group =} without file_group" $ do
      let base = "files = [\"**/*.hs\"]\ncomment = \"--\"\ngrammar = \"haskell\"\n[test]\ncommand = [\"t\"]\ntests_for = \"x\"\n"
      either (T.isInfixOf "[test.lines]") (const False) (parseLanguage "h" "/h" (base <> "format = \"lines\"\n")) `shouldBe` True
      either (T.isInfixOf "file_group") (const False) (parseLanguage "h" "/h" (base <> "format = \"tap\"\n[test.select]\ntest = \"{= group =}\"\nfile = \"x\"\n")) `shouldBe` True
      fmap (fmap testFormat . langTest) (parseLanguage "h" "/h" (base <> "format = \"junit-xml\"\n")) `shouldBe` Right (Just "junit-xml")
    it "refuses a message key that is no expression, naming it" $ do
      let base = "files = [\"**/*.hs\"]\ncomment = \"--\"\ngrammar = \"haskell\"\n"
      either (T.isInfixOf "[check] undefined") (const False) (parseLanguage "h" "/h" (base <> "[check]\ncommand = [\"c\"]\nformat = \"ghc-json\"\nundefined = ['(']\n")) `shouldBe` True
      either (T.isInfixOf "[check] missing_member") (const False) (parseLanguage "h" "/h" (base <> "[check]\ncommand = [\"c\"]\nformat = \"ghc-json\"\nmissing_member = { pattern = 'a(b)', member = 1, type = 3 }\n")) `shouldBe` True
      either (T.isInfixOf "[test.failure] got_expected") (const False) (parseLanguage "h" "/h" (base <> "[test]\ncommand = [\"t\"]\ntests_for = \"x\"\nformat = \"tap\"\n[test.failure]\ngot_expected = [{ pattern = '(', got = 1, expected = 2 }]\n")) `shouldBe` True
    it "selects one test by its group and name, and reads hspec's lines" $ do
      out <- TIO.readFile "src/Olifant/Language/testdata/hspec/checks-one-failure.txt"
      let Right lang = parseLanguage "haskell" "/h" (hsToml [fake "--test-option=--match=/Olifant.Plans/parsePlan/reads a test/"])
          files = M.fromList [(specFile, "module Olifant.PlansSpec (spec) where\n"), ("out.txt", out)]
      Right run <- runTests (processRunner neverCancel 60) lang [T.pack specFile <> "::parsePlan::reads a test"] files
      map fst (trFailed run) `shouldBe` [T.pack specFile <> "::parsePlan::reads a test"]
      map (fLine . snd) (trFailed run) `shouldBe` [Just 6]
    it "cargo-test: one test by --exact and its module path, read from libtest's lines" $ do
      out <- TIO.readFile "src/Olifant/Language/testdata/cargo-test/all-passed.txt"
      let rust = rustOver ["command = [\"sh\", \"-c\", 'case \"$1 $2\" in \"--exact rates::tests::adds\") cat out.txt;; *) echo \"bad args: $*\"; exit 2;; esac', \"sh\", \"{= tests =}\"]"]
      Right run <- runTests (processRunner neverCancel 60) rust ["src/rates.rs::tests::adds"] (M.fromList [("src/rates.rs", ""), ("out.txt", out)])
      run `shouldBe` TestRun ["src/rates.rs::tests::adds"] [] Nothing
    it "cargo-test: a file's tests by its module; a failure's line and message from its panic" $ do
      out <- TIO.readFile "src/Olifant/Language/testdata/cargo-test/one-failed.txt"
      let rust = rustOver ["command = [\"sh\", \"-c\", 'case \"$1\" in \"rates::\") cat out.txt; exit 101;; *) echo \"bad args: $*\"; exit 2;; esac', \"sh\", \"{= tests =}\"]"]
      Right run <- runTests (processRunner neverCancel 60) rust ["src/rates.rs"] (M.fromList [("src/rates.rs", ""), ("out.txt", out)])
      trPassed run `shouldBe` ["src/rates.rs::tests::adds", "src/rates.rs::tests::it_works"]
      [(i, fLine f, failureLineIn (shippedLanguageNow "rust") f) | (i, f) <- trFailed run] `shouldBe` [("src/rates.rs::tests::fails", Just 16, "assertion `left == right` failed")]
    it "cargo-test: a crate root's test (src/lib.rs) has no module before it" $ do
      let rust = rustOver ["command = [\"sh\", \"-c\", 'case \"$1 $2\" in \"--exact tests::it_works\") echo \"test tests::it_works ... ok\";; *) echo \"bad args: $*\"; exit 2;; esac', \"sh\", \"{= tests =}\"]"]
      Right run <- runTests (processRunner neverCancel 60) rust ["src/lib.rs::tests::it_works"] (M.fromList [("src/lib.rs", "")])
      trPassed run `shouldBe` ["src/lib.rs::tests::it_works"]
    it "selects a whole file by its group" $ do
      out <- TIO.readFile "src/Olifant/Language/testdata/hspec/checks-one-failure.txt"
      let Right lang = parseLanguage "haskell" "/h" (hsToml [fake "--test-option=--match=/Olifant.Plans/"])
          files = M.fromList [(specFile, "module Olifant.PlansSpec (spec) where\n"), ("out.txt", out)]
      Right run <- runTests (processRunner neverCancel 60) lang [T.pack specFile] files
      length (trPassed run) `shouldBe` 2
    it "a wrong argument is a broken run saying what the command said" $ do
      let Right lang = parseLanguage "haskell" "/h" (hsToml [fake "nope"])
          files = M.fromList [(specFile, "module Olifant.PlansSpec (spec) where\n"), ("out.txt", "")]
      Right run <- runTests (processRunner neverCancel 60) lang [T.pack specFile] files
      trBroken run `shouldSatisfy` maybe False ("bad args" `T.isInfixOf`)
    it "a selection provider's argv runs in place of {= tests =}; a failing one is the run's Left" $
      withSystemTempDirectory "olifant-select" $ \out -> do
        here <- getCurrentDirectory
        let selLang script = let Right l = parseLanguage "x" "/x" (T.unlines
                                    [ "files = [\"**/*.x\"]", "comment = \"#\"", "grammar = \"x\""
                                    , "[test]", "command = [\"sh\", \"-c\", \"printf '%s\\\\n' \\\"$@\\\" > " <> T.pack (out </> "argv") <> "; echo '{}'\", \"x\", \"{= tests =}\"]"
                                    , "tests_for = \"{= stem =}_t.x\"", "format = \"olifant-json\""
                                    , "select = { provider = [\"sh\", \"" <> T.pack (here </> "src/Olifant/testdata/providers" </> script) <> "\"] }"
                                    ]) in l
        Right _ <- runTests (processRunner neverCancel 60) (selLang "selects.sh") ["a_t.x::t1"] (M.fromList [("a_t.x", "")])
        lines <$> readFile (out </> "argv") `shouldReturn` ["--only", "picked"]
        runTests (processRunner neverCancel 60) (selLang "fails.sh") ["a_t.x::t1"] (M.fromList [("a_t.x", "")]) `shouldReturn` Left "the test selection provider: no grammar"
    it "the [test] timeout wins over the caller's" $ do
      let Right lang = parseLanguage "haskell" "/h" (hsToml ["command = [\"sleep\", \"5\"]", "timeout = 1"])
      Right run <- runTests (processRunner neverCancel 60) lang [T.pack specFile] (M.fromList [(specFile, "module Olifant.PlansSpec (spec) where\n")])
      trBroken run `shouldBe` Just "the tests timed out (1 s)"
    it "olifant-json through convert: ids without :: are attributed to the requested file" $ do
      let Right lang = parseLanguage "x" "/x" (T.unlines
            [ "files = [\"**/*.x\"]", "comment = \"#\"", "grammar = \"x\""
            , "[test]", "command = [\"sh\", \"-c\", \"echo raw\"]", "tests_for = \"{= stem =}_t.x\""
            , "convert = [\"sh\", \"-c\", \"cat >/dev/null; echo '{\\\"passed\\\":[\\\"t1\\\"],\\\"failed\\\":[{\\\"id\\\":\\\"a_t.x::t2\\\",\\\"message\\\":\\\"m\\\"}]}'\"]"
            ])
      Right run <- runTests (processRunner neverCancel 60) lang ["a_t.x"] (M.fromList [("a_t.x", "")])
      (trPassed run, map fst (trFailed run)) `shouldBe` (["a_t.x::t1"], ["a_t.x::t2"])
    it "olifant-json: no results and a non-zero exit is broken with the output; with exit 0, no tests ran; a report's own broken wins" $ do
      let jsonLang cmd = let Right l = parseLanguage "x" "/x" (T.unlines
                                [ "files = [\"**/*.x\"]", "comment = \"#\"", "grammar = \"x\""
                                , "[test]", "command = [\"sh\", \"-c\", \"" <> cmd <> "\"]", "tests_for = \"{= stem =}_t.x\"", "format = \"olifant-json\""
                                ]) in l
          run cmd = runTests (processRunner neverCancel 60) (jsonLang cmd) ["a_t.x"] (M.fromList [("a_t.x", "")])
      Right crashed <- run "echo '{}'; echo runner crashed >&2; exit 1"
      trBroken crashed `shouldSatisfy` maybe False ("runner crashed" `T.isInfixOf`)
      Right empty <- run "echo '{}'"
      trBroken empty `shouldBe` Just "no tests ran"
      Right own <- run "echo '{\\\"broken\\\":\\\"mine\\\"}'; exit 1"
      trBroken own `shouldBe` Just "mine"
    it "olifant-json through convert: an empty converted report follows the same rules" $ do
      let Right lang = parseLanguage "x" "/x" (T.unlines
            [ "files = [\"**/*.x\"]", "comment = \"#\"", "grammar = \"x\""
            , "[test]", "command = [\"sh\", \"-c\", \"echo boom >&2; exit 1\"]", "tests_for = \"{= stem =}_t.x\""
            , "convert = [\"sh\", \"-c\", \"cat >/dev/null; echo '{}'\"]"
            ])
      Right r <- runTests (processRunner neverCancel 60) lang ["a_t.x"] (M.fromList [("a_t.x", "")])
      trBroken r `shouldSatisfy` maybe False (/= "no tests ran")
    it "the files a test run copies include the [test] context and files globs" $ do
      let Right lang = parseLanguage "x" "/x" (T.unlines
            [ "files = [\"**/*.x\"]", "comment = \"#\"", "grammar = \"x\""
            , "[check]", "command = [\"c\"]", "format = \"ghc-json\"", "context = [\"chk.cfg\"]"
            , "[test]", "command = [\"t\"]", "format = \"pytest\"", "tests_for = \"{= stem =}_t.x\"", "context = [\"lock.cfg\"]", "files = [\"fixtures/**\"]"
            ])
          paths = ["a.x", "chk.cfg", "lock.cfg", "fixtures/deep/f.txt", "other.txt"]
      snd (checkScope lang paths ["a.x"]) `shouldBe` ["a.x", "chk.cfg", "lock.cfg", "fixtures/deep/f.txt"]
    it "[test] files is read as its own list" $ do
      let Right lang = parseLanguage "x" "/x" (T.unlines
            [ "files = [\"**/*.x\"]", "comment = \"#\"", "grammar = \"x\""
            , "[test]", "command = [\"t\"]", "format = \"pytest\"", "tests_for = \"{= stem =}_t.x\"", "context = [\"lock.cfg\"]", "files = [\"fixtures/**\"]"
            ])
      fmap testFiles (langTest lang) `shouldBe` Just ["fixtures/**"]
    it "a group id owns its tests (keptVerdict)" $ do
      keptVerdict "f::g" [] (TestRun ["f::g::t"] [] Nothing) `shouldBe` Right ()
      keptVerdict "f::g" [] (TestRun [] [("f::g::t", Failure Nothing "x" "")] Nothing) `shouldBe` Left (Nothing, Failure Nothing "x" "")
  describe "the configuration budget (spec §3)" $ do
    it "(f) no pack TOML names Olifant's cache" $ do
      let walk d = do
            entries <- listDirectory d
            concat <$> mapM (\e -> let p = d </> e in doesDirectoryExist p >>= \isDir -> if isDir then walk p else pure [p | takeExtension p == ".toml"]) entries
      files <- walk "packs"
      bad <- filterM (fmap ("{= cache =}" `T.isInfixOf`) . TIO.readFile) files
      bad `shouldBe` []
    it "(g) python.toml is no longer than before [test] (123 lines)" $ do
      src <- TIO.readFile "packs/python-htmx/languages/python.toml"
      length (T.lines src) `shouldSatisfy` (<= 123)
  describe "parseLanguage" $ do
    it "reads an adapter, resolving the grammar source against the pack" $ do
      let Right lang = parseLanguage "elm" "/packs/elm" elmToml
      langName lang `shouldBe` "elm"
      langGrammar lang `shouldBe` GrammarSource "/packs/elm/languages/grammar/elm"
      fmap checkFormat (langCheck lang) `shouldBe` Just "elm-json"
      map targetPhrase (langTargets lang) `shouldBe` ["case in FUNCTION"]
    it "accepts a built-in grammar name" $
      fmap langGrammar (parseLanguage "rust" "/p" "files = [\"**/*.rs\"]\ncomment = \"//\"\ngrammar = \"rust\"\n") `shouldBe` Right (BuiltInGrammar "rust")
    it "rejects unknown keys" $
      parseLanguage "x" "/p" "files = []\ncomment = \"#\"\ngrammar = \"x\"\ncolour = 1\n" `shouldSatisfy` either ("colour" `T.isInfixOf`) (const False)
    it "refuses the retired `qualify` key, saying what replaced it" $
      parseLanguage "x" "/p" "files = [\"**/*.x\"]\ncomment = \"--\"\ngrammar = \"x\"\nqualify = \"elm\"\n"
        `shouldSatisfy` either ("the [check] format decides" `T.isInfixOf`) (const False)
    it "refuses a check format with no compiler support, listing the known ones" $
      parseLanguage "x" "/p" "files = [\"**/*.x\"]\ncomment = \"--\"\ngrammar = \"x\"\n[check]\ncommand = [\"x\"]\nformat = \"cobol-json\"\n"
        `shouldSatisfy` either (\e -> "unknown format `cobol-json`" `T.isInfixOf` e && "elm-json" `T.isInfixOf` e) (const False)
    it "renders a target's rule with the phrase's parameters" $ do
      let Right lang = parseLanguage "elm" "/p" elmToml
          [t] = langTargets lang
          Just binds = matchShape t ["case", "in", "update"]
      renderRule (M.fromList [(k, VText v) | (k, v) <- binds]) (targetContainer t)
        `shouldBe` Right (object ["kind" .= ("case_of_expr" :: Text), "inside" .= object ["kind" .= ("value_declaration" :: Text), "regex" .= ("^update\\b" :: Text), "stopBy" .= ("end" :: Text)]])
  describe "hole kinds (spec §6.3)" $ do
    it "reads [holes.KIND] with a placeholder and a wrap" $ do
      let Right l = parseLanguage "r" "/p" (T.unlines ["files = [\"**/*.rs\"]", "comment = \"//\"", "grammar = \"rust\"", "[holes.expr]", "placeholder = 'todo!(\"olifant: {= hole =}\")'", "wrap = \"fn __olifant() { let _ = {= fill =}; }\""])
      map fst (langHoles l) `shouldBe` ["expr"]
    it "refuses a placeholder that does not name its hole (review I2)" $
      parseLanguage "r" "/p" (T.unlines ["files = [\"**/*.rs\"]", "comment = \"//\"", "grammar = \"rust\"", "[holes.type]", "placeholder = '()'", "wrap = \"type T = {= fill =};\""])
        `shouldSatisfy` either ("{= hole =}" `T.isInfixOf`) (const False)
    it "refuses a wrap without {= fill =}" $
      parseLanguage "r" "/p" (T.unlines ["files = [\"**/*.rs\"]", "comment = \"//\"", "grammar = \"rust\"", "[holes.expr]", "placeholder = 'x({= hole =})'", "wrap = \"fn f() {}\""])
        `shouldSatisfy` either ("{= fill =}" `T.isInfixOf`) (const False)
  describe "matchShape" $ do
    it "binds parameters and requires literal words" $ do
      let Right lang = parseLanguage "elm" "/p" elmToml
          [t] = langTargets lang
      matchShape t ["case", "in", "view"] `shouldBe` Just [("FUNCTION", "view")]
      matchShape t ["case", "of", "view"] `shouldBe` Nothing
      matchShape t ["case", "in"] `shouldBe` Nothing
  describe "languageFor" $
    it "picks the first adapter whose globs match" $ do
      let Right elm = parseLanguage "elm" "/p" elmToml
      fmap langName (languageFor [elm] "src/Main.elm") `shouldBe` Just "elm"
      fmap langName (languageFor [elm] "src/lib.rs") `shouldBe` Nothing
  describe "ports without tools" $ do
    it "noSyntax reports ast-grep as missing, with the install command" $ do
      let Right lang = parseLanguage "elm" "/p" elmToml
      r <- querySyntax noSyntax lang (object []) "x"
      either (const True) (const False) r `shouldBe` True
      either describeSyntaxFailure (const "") r `shouldSatisfy` ("brew install ast-grep" `T.isInfixOf`)
    it "noChecker reports that no check is available" $ do
      let Right lang = parseLanguage "elm" "/p" elmToml
      r <- runCheck noChecker lang M.empty
      either describeCheckFailure (const "") r `shouldSatisfy` ("no checker" `T.isInfixOf`)
  describe "the Elm adapter (ast-grep, fixture Main.elm)" $ do
    let at syn elm file p = fmap (fmap locLine) (resolveLayers syn (Just elm) Nothing [PhraseAnchor p] file)
        lineOf file needle = length (takeWhile (not . (needle `T.isInfixOf`)) (T.lines file))
    it "a grammar built by several queries at once, on a cold cache, serves them all (parallel tests, the server's threads)" $ withAstGrep $ \_ elm file ->
      forM_ [1 .. 3 :: Int] $ \_ -> withSystemTempDirectory "olifant-cold-cache" $ \cold -> do
        let syn = astGrepSyntax cold
        answers <- mapConcurrently (const (at syn elm file "end of type Page")) [1 .. 8 :: Int]
        answers `shouldBe` replicate 8 (Right (lineOf file "| SamplePage SamplePage.Model" + 1))
    it "end of type Page goes after the last variant" $ withAstGrep $ \syn elm file ->
      at syn elm file "end of type Page" `shouldReturn` Right (lineOf file "| SamplePage SamplePage.Model" + 1)
    it "end of case in update skips the catch-all and the nested case" $ withAstGrep $ \syn elm file ->
      at syn elm file "end of case in update" `shouldReturn` Right (lineOf file "SamplePage.update subMsg page" + 1)
    it "end of case in view goes after the last branch" $ withAstGrep $ \syn elm file ->
      at syn elm file "end of case in view" `shouldReturn` Right (lineOf file "viewPage GotSamplePageMsg (SamplePage.view page)" + 1)
    it "end of imports goes after the last import" $ withAstGrep $ \syn elm file ->
      at syn elm file "end of imports" `shouldReturn` Right (lineOf file "import Url exposing (Url)" + 1)
    it "end of list in parser goes after the last element (Route.elm)" $ withAstGrepOn "src/Route.elm" $ \syn elm file ->
      at syn elm file "end of list in parser" `shouldReturn` Right (lineOf file ", Parser.map SamplePage (s \"sample-page\")" + 1)
    it "end of type Route goes after the last variant (Route.elm)" $ withAstGrepOn "src/Route.elm" $ \syn elm file ->
      at syn elm file "end of type Route" `shouldReturn` Right (lineOf file "| SamplePage" + 1)
    it "end of case skips a case nested in the catch-all branch (review)" $ withAstGrep $ \syn elm _ -> do
      let src = T.unlines ["module A exposing (..)", "", "update msg model =", "    case msg of", "        Foo ->", "            model", "", "        _ ->", "            case model of", "                X ->", "                    1", "", "                Y ->", "                    2"]
      at syn elm src "end of case in update" `shouldReturn` Right 6
    it "a rule ast-grep rejects on a large file is a failure, not a crash (review)" $ withAstGrep $ \syn elm _ -> do
      let big = T.replicate 20000 "x = 1\n\n"
      r <- querySyntax syn elm (object ["kind" .= ("no_such_kind" :: Text)]) big
      either (const True) (const False) r `shouldBe` True
    it "(pack-opens c) a one-line list in a language with no separator is still refused" $ withAstGrep $ \syn elm _ -> do
      let src = T.unlines ["module A exposing (..)", "", "parser =", "    oneOf [ a, b ]"]
      r <- resolveLayers syn (Just elm) Nothing [PhraseAnchor "end of list in parser"] src
      either (any (T.isInfixOf "shares its line" . missReason)) (const False) r `shouldBe` True
    it "an unknown function is a miss, not a crash" $ withAstGrep $ \syn elm file ->
      fmap (either (const "miss") (const "hit")) (at syn elm file "end of case in nope") `shouldReturn` ("miss" :: Text)

  describe "the Python adapter (python-htmx)" $ do
    let at syn lang file p = fmap (fmap (\l -> (locLine l, locIndent l))) (resolveLayers syn (Just lang) Nothing [PhraseAnchor p] file)
        lineOf needle = length (takeWhile (not . (needle `T.isInfixOf`)) (T.lines pythonModule))
    it "[unit] kinds: the adapter's definition kinds, none when it has no [unit]" $ do
      py <- either (error . T.unpack) id . parseOverShipped "python" "packs/python-htmx" <$> TIO.readFile "packs/python-htmx/languages/python.toml"
      -- 4b m2: none, its units come from the grammar's queries
      langUnitKinds py `shouldBe` []
      fmap langUnitKinds (parseLanguage "python" "/p" "files = [\"**/*.py\"]\ncomment = \"#\"\ngrammar = \"python\"\n") `shouldBe` Right []
    it "[check] convert: format absent or olifant-json; [check.errorformat] needs a pattern" $ do
      let base = "files = [\"**/*.x\"]\ncomment = \"#\"\ngrammar = \"x\"\n[check]\ncommand = [\"t\"]\n"
      fmap (fmap (\c -> (checkFormat c, checkConvert c)) . langCheck) (parseLanguage "x" "/x" (base <> "convert = [\"c\"]\n")) `shouldBe` Right (Just ("olifant-json", ["c"]))
      either (\e -> all (`T.isInfixOf` e) ["convert", "format"]) (const False) (parseLanguage "x" "/x" (base <> "convert = [\"c\"]\nformat = \"ghc-json\"\n")) `shouldBe` True
      either (T.isInfixOf "pattern") (const False) (parseLanguage "x" "/x" (base <> "format = \"errorformat\"\n[check.errorformat]\nfile = 1\nline = 2\nmessage = 3\n")) `shouldBe` True
      either (T.isInfixOf "errorformat") (const False) (parseLanguage "x" "/x" (base <> "format = \"errorformat\"\n")) `shouldBe` True
      fmap (fmap checkErrorFormat . langCheck) (parseLanguage "x" "/x" (base <> "format = \"errorformat\"\n[check.errorformat]\npattern = '^(.*):([0-9]+): (.*)$'\nfile = 1\nline = 2\nmessage = 3\n"))
        `shouldBe` Right (Just (Just (ErrorFormat "^(.*):([0-9]+): (.*)$" 1 2 3 Nothing Nothing)))
    it "[unit] name is a list of valid expressions and join a bool" $ do
      let base = "files = [\"**/*.x\"]\ncomment = \"#\"\ngrammar = \"x\"\n[unit]\nkinds = [\"f\"]\n"
      fmap (\l -> (langUnitNames l, langUnitJoin l)) (parseLanguage "x" "/x" (base <> "name = ['^(a)']\njoin = true\n")) `shouldBe` Right (["^(a)"], True)
      fmap (\l -> (langUnitNames l, langUnitJoin l)) (parseLanguage "x" "/x" base) `shouldBe` Right (["^[[:space:]]*([[:alpha:]_][[:alnum:]_']*)"], False)
      either (T.isInfixOf "[unit] name") (const False) (parseLanguage "x" "/x" (base <> "name = ['(']\n")) `shouldBe` True
      either (const True) (const False) (parseLanguage "x" "/x" (base <> "join = \"yes\"\n")) `shouldBe` True
    it "the unit at a line, by the adapter's [unit] kinds (real ast-grep)" $ withPython $ \syn py _ -> do
      let src = T.unlines ["import functools", "", "", "class Table:", "    @functools.cache", "    def seats(self) -> int:", "        return 4", "", "    def free(self) -> bool:", "        return True"]
      fmap unitName <$> unitAt syn py Nothing src 7 `shouldReturn` Right "seats"
      fmap unitDef <$> unitAt syn py Nothing src 5 `shouldReturn` Right 6
      fmap unitName <$> unitAt syn py Nothing src 8 `shouldReturn` Right "Table"
      either id (const "") <$> unitAt syn py Nothing src 2 `shouldReturn` "no function or class at line 2"
    it "(b) loads: hole kinds, a four-space indent, pyright-json" $ withPython $ \_ lang _ -> do
      map fst (langHoles lang) `shouldMatchList` ["expr", "stmt", "block", "ident", "text"]
      langIndent lang `shouldBe` "    "
      fmap checkFormat (langCheck lang) `shouldBe` Just "pyright-json"
    it "(c) end of union NAME goes after its last member, at its indentation" $ withPython $ \syn lang file ->
      at syn lang file "end of union CreateBookingFailure" `shouldReturn` Right (lineOf "| PartyTooLarge" + 1, "    ")
    it "(c) end of class NAME goes after the class's last statement" $ withPython $ \syn lang file ->
      at syn lang file "end of class BookingService" `shouldReturn` Right (lineOf "return 1" + 1, "    ")
    it "(c) end of function NAME goes after its last statement" $ withPython $ \syn lang file ->
      at syn lang file "end of function status" `shouldReturn` Right (lineOf "return 422" + 1, "    ")
    it "(c) end of match on NAME goes after the last case, whole" $ withPython $ \syn lang file ->
      at syn lang file "end of match on f" `shouldReturn` Right (lineOf "return 422" + 1, "        ")
    it "(c) end of imports goes after the last import" $ withPython $ \syn lang file ->
      at syn lang file "end of imports" `shouldReturn` Right (lineOf "from dataclasses import dataclass" + 1, "")
    it "(c) end of call NAME in FUNCTION goes after its last argument (python-pack: wire)" $ withPython $ \syn lang file ->
      at syn lang file "end of call CreateBooking in create" `shouldReturn` Right (lineOf "bookings=booking_repository," + 1, "        ")
    it "(c) end of imports adds no blank line: one between import groups is grouping, not spacing" $ withPython $ \syn lang _ -> do
      let grouped = T.unlines ["from pathlib import Path", "", "from app.x import y", "", "", "def f() -> None:", "    pass"]
      fmap (fmap locSpacing) (resolveLayers syn (Just lang) Nothing [PhraseAnchor "end of imports"] grouped) `shouldReturn` Right NoSpacing
    it "(c) end of imports in a file with none goes after its docstring, a blank line before" $ withPython $ \syn lang _ -> do
      let bare = T.unlines ["\"\"\"Clock in memory.\"\"\"", "", "", "class InMemoryClock:", "    pass"]
      fmap (fmap (\l -> (locLine l, locSpacing l))) (resolveLayers syn (Just lang) Nothing [PhraseAnchor "end of imports"] bare) `shouldReturn` Right (1, BlankBefore)
    describe "one-line containers (pack-opens: inline if it fits, else one per line)" $ do
      let grown syn lang src phrase text = do
            Right loc <- resolveLayers syn (Just lang) Nothing [PhraseAnchor phrase] src
            pure (snd (insertLines loc text src), presentAt loc text src)
          long = "tables_with_a_rather_long_name: TableRepositoryWithAnEvenLongerName"
      it "(a) inline when it fits the line length" $ withPython $ \syn lang _ ->
        fmap fst (grown syn lang "class A:\n    def __init__(self) -> None:\n        pass\n" "end of parameters in __init__" "bookings: BookingRepository")
          `shouldReturn` "class A:\n    def __init__(self, bookings: BookingRepository) -> None:\n        pass\n"
      it "(b) one item per line when it does not" $ withPython $ \syn lang _ ->
        fmap fst (grown syn lang "class A:\n    def __init__(self, bookings: BookingRepository) -> None:\n        pass\n" "end of parameters in __init__" long)
          `shouldReturn` T.unlines ["class A:", "    def __init__(", "        self,", "        bookings: BookingRepository,", "        " <> long <> ",", "    ) -> None:", "        pass"]
      it "(d) nested calls stay whole; a trailing comment stays on the closing line (Review Focus 2)" $ withPython $ \syn lang _ ->
        fmap fst (grown syn lang "x = foo(a, bar(b, c))  # note\n" "end of call foo" long)
          `shouldReturn` T.unlines ["x = foo(", "    a,", "    bar(b, c),", "    " <> long <> ",", ")  # note"]
      it "(e) a separator already at the end is not doubled (Review Focus 3)" $ withPython $ \syn lang _ ->
        fmap fst (grown syn lang "y = f(a,)\n" "end of call f" "b") `shouldReturn` "y = f(a, b)\n"
      it "(g) review C1: no argument vanishes: every argument is an item on the generic rule (a lambda, an operator)" $ withPython $ \syn lang _ ->
        fmap fst (grown syn lang "x = foo(a + b, lambda q: q, c)  # note\n" "end of call foo" "d")
          `shouldReturn` "x = foo(a + b, lambda q: q, c, d)  # note\n"
      it "(h) review C1: a keyword-only marker is not an item: refused, not dropped" $ withPython $ \syn lang _ -> do
        r <- resolveLayers syn (Just lang) Nothing [PhraseAnchor "end of parameters in f"] "def f(self, *, x: int) -> None:\n    pass\n"
        either (any (T.isInfixOf "shares its line" . missReason)) (const False) r `shouldBe` True
      it "(i) review C1: a one-line union is not a comma list: refused" $ withPython $ \syn lang _ -> do
        r <- resolveLayers syn (Just lang) Nothing [PhraseAnchor "end of union F"] "type F = (NotFound | Late)\n"
        either (any (T.isInfixOf "shares its line" . missReason)) (const False) r `shouldBe` True
      it "(f) already there: present" $ withPython $ \syn lang _ ->
        fmap snd (grown syn lang "y = f(a, b)\n" "end of call f" "b") `shouldReturn` True
    it "(pack-opens) headerEnd: prefixed docstrings and leading blank lines count" $ do
      headerEnd ["r\"\"\"Doc.\"\"\"", "", "x = 1"] `shouldBe` 1
      headerEnd ["", "", "\"\"\"Doc.\"\"\"", "x = 1"] `shouldBe` 3
      headerEnd ["b'''a", "b'''", "x"] `shouldBe` 2
      headerEnd ["x = 1"] `shouldBe` 0
    it "(pack-opens) end of imports in a file with no header: line 0, no blank line" $ withPython $ \syn lang _ ->
      fmap (fmap (\l -> (locLine l, locSpacing l))) (resolveLayers syn (Just lang) Nothing [PhraseAnchor "end of imports"] "class X:\n    pass\n") `shouldReturn` Right (0, NoSpacing)
    it "(c) end of parameters in FUNCTION goes after its last parameter (python-pack: web-command)" $ withPython $ \syn lang file ->
      at syn lang file "end of parameters in floor_routes" `shouldReturn` Right (lineOf "find_free_tables: FindFreeTables," + 1, "    ")
    it "(c) end of call NAME (anywhere in the file) goes after its last argument" $ withPython $ \syn lang file ->
      at syn lang file "end of call CreateBooking" `shouldReturn` Right (lineOf "bookings=booking_repository," + 1, "        ")
    it "(c) end of call NAME in FUNCTION opens an empty argument list" $ withPython $ \syn lang file ->
      fmap (fmap locOpen) (resolveLayers syn (Just lang) Nothing [PhraseAnchor "end of call CancelBooking in create"] file) >>= (`shouldSatisfy` either (const False) isJust)
    it "Python keeps only `union NAME` and `match on NAME`" $ withPython $ \_ lang _ ->
      map targetPhrase (langTargets lang) `shouldBe` ["match on NAME", "union NAME"]
    it "(Review Focus) end of class NAME with a docstring and a decorated method goes after the last method, at its column" $ withPython $ \syn lang _ -> do
      let src = T.unlines ["class Booking:", "    \"\"\"A booking.\"\"\"", "", "    def a(self) -> int:", "        return 1", "", "    @property", "    def b(self) -> int:", "        return 2", "", "", "x = 1"]
      at syn lang src "end of class Booking" `shouldReturn` Right (9, "    ")
      fmap (fmap locSpacing) (resolveLayers syn (Just lang) Nothing [PhraseAnchor "end of class Booking"] src) `shouldReturn` Right BlankBefore

  describe "rewriting (ast-grep, a rule with a fix)" $
    it "gives each match its replacement and its byte range" $ withPython $ \syn lang _ ->
      rewriteSyntax syn lang (object ["pattern" .= ("$A > $B" :: Text)]) "$A >= $B" "if é > b:\n    pass\n"
        `shouldReturn` Right [Rewrite (0, 3) (0, 8) "é > b" "é >= b" (3, 9)]
  describe "the Rust adapter (ast-grep, fixture service.rs)" $ do
    let at syn lang file p = fmap (fmap locLine) (resolveLayers syn (Just lang) Nothing [PhraseAnchor p] file)
        lineOf file needle = length (takeWhile (not . (needle `T.isInfixOf`)) (T.lines file))
    it "end of function NAME goes after the last equation (haskell adapter)" $ withHaskell $ \syn lang _ ->
      fmap (fmap (\l -> (locLine l, locIndent l))) (resolveLayers syn (Just lang) Nothing [PhraseAnchor "end of function f"] "module A where\n\nf :: Int -> Int\nf 0 = 1\nf n = n\n\ng :: Int\ng = 2\n")
        `shouldReturn` Right (5, "")
    it "end of data goes after the last constructor (haskell adapter)" $ withHaskell $ \syn lang file ->
      fmap (fmap (\l -> (locLine l, locIndent l))) (resolveLayers syn (Just lang) Nothing [PhraseAnchor "end of data SampleError"] file)
        `shouldReturn` Right (6, "  ")
    it "end of parameters in new goes after the last parameter" $ withRustFile "billing/billing-service/src/service.rs" $ \syn lang file ->
      at syn lang file "end of parameters in new" `shouldReturn` Right (lineOf file "invoice_repository: Arc<dyn InvoiceRepository>," + 1)
    it "end of literal VoidInvoice in new opens the empty literal" $ withRustFile "billing/billing-service/src/service.rs" $ \syn lang file ->
      fmap (fmap locOpen) (resolveLayers syn (Just lang) Nothing [PhraseAnchor "end of literal VoidInvoice in new"] file) >>= (`shouldSatisfy` either (const False) isJust)
    it "end of literal RefundInvoice in new goes after its last field" $ withRustFile "billing/billing-service/src/service.rs" $ \syn lang file ->
      at syn lang file "end of literal RefundInvoice in new" `shouldReturn` Right (lineOf file "invoice_repository: invoice_repository.clone()," + 1)
    it "end of call … BillingService::new in main goes after the last argument" $ withRustFile "main/src/main.rs" $ \syn lang file ->
      at syn lang file "end of call billing_service::service::BillingService::new in main" `shouldReturn` Right (lineOf file "invoice_repository.clone()," + 1)
    it "end of call … NotificationsService::new in main opens the empty argument list" $ withRustFile "main/src/main.rs" $ \syn lang file ->
      fmap (fmap locOpen) (resolveLayers syn (Just lang) Nothing [PhraseAnchor "end of call notifications_service::service::NotificationsService::new in main"] file) >>= (`shouldSatisfy` either (const False) isJust)
    it "anchors lines up every target's example, however long the phrase" $ do
      src <- TIO.readFile "packs/rust-htmx/languages/rust.toml"
      let Right lang = parseOverShipped "rust" "packs/rust-htmx" src
          rows = [l | l <- T.lines (anchorsHuman lang), "e.g. " `T.isInfixOf` l]
      rows `shouldSatisfy` all (\l -> "  e.g. " `T.isInfixOf` l)
      length (nub [T.length (fst (T.breakOn "e.g. " l)) | l <- rows]) `shouldBe` 1
      anchorsHuman lang `shouldSatisfy` T.isInfixOf "start of|end of file"
      anchorsHuman lang `shouldSatisfy` T.isInfixOf "from queries (tree-sitter-rust 0.24.2):"
      rows `shouldSatisfy` any ("end of struct BillingService" `T.isInfixOf`)
    it "anchors --json lists the generic phrases with origin queries, a declared target with origin targets" $ do
      src <- TIO.readFile "packs/rust-htmx/languages/rust.toml"
      let Right lang = parseOverShipped "rust" "packs/rust-htmx" src
          json = TE.decodeUtf8 (BS.toStrict (encode (anchorsJson lang)))
      json `shouldSatisfy` T.isInfixOf "{\"example\":\"end of function main\",\"origin\":\"queries\",\"phrase\":\"function NAME\"}"
      json `shouldSatisfy` T.isInfixOf "{\"example\":\"end of crate imports\",\"origin\":\"targets\",\"phrase\":\"crate imports\"}"
    it "end of struct goes after the last field" $ withRust $ \syn lang file ->
      at syn lang file "end of struct BillingService" `shouldReturn` Right (lineOf file "pub cancel_invoice: CancelInvoice," + 1)
    it "end of impl goes after the last method" $ withRust $ \syn lang file ->
      at syn lang file "end of impl BillingService" `shouldReturn` Right (lineOf file "self.cancel_invoice.cancel_invoice(input).await" + 2)
    it "end of literal in new goes after the last initializer" $ withRust $ \syn lang file ->
      at syn lang file "end of literal in new" `shouldReturn` Right (lineOf file "cancel_invoice: CancelInvoice {}," + 1)
    it "end of crate imports goes after the last imported item" $ withRust $ \syn lang file ->
      at syn lang file "end of crate imports" `shouldReturn` Right (lineOf file "cancel_invoice::{CancelInvoice" + 1)
    it "Rust keeps only the exotic targets" $ withRust $ \_ lang _ ->
      map targetPhrase (langTargets lang) `shouldBe` ["crate imports", "literal in FUNCTION", "literal NAME in FUNCTION"]
    it "`end of uses` suggests imports" $ withRust $ \syn lang file ->
      fmap (either (map missReason) (const [])) (resolveLayers syn (Just lang) Nothing [PhraseAnchor "end of uses"] file)
        >>= (`shouldSatisfy` any ("did you mean `end of imports`" `T.isInfixOf`))
    it "end of imports goes after the last top-level use, a grouped `use crate::{…}` whole" $ withRust $ \syn lang file ->
      at syn lang file "end of imports" `shouldReturn` Right (lineOf file "};" + 1)
  describe "compiler support" $ do
    it "maps the elm-json format to Elm's support" $
      fmap supportFormat (supportFor "elm-json") `shouldBe` Just "elm-json"
    it "has no support for an unknown format" $
      fmap supportFormat (supportFor "cobol-json") `shouldBe` Nothing
    it "keeps the language-neutral modules free of Elm" $
      forM_ ["src/Olifant/Generation/Complete.hs", "src/Olifant/Language/Check.hs"] $ \f -> do
        src <- TIO.readFile f
        (f, "Olifant.Language.Elm" `T.isInfixOf` src) `shouldBe` (f, False)
  describe "rustc-json" $ do
    it "reads errors as broken files, relative to the check directory" $
      fmap reportBroken (supportParse rustcSupport "{\"reason\":\"compiler-message\",\"message\":{\"level\":\"error\",\"code\":{\"code\":\"E0063\"},\"message\":\"missing field `email_sender` in initializer of `CancelInvoice`\",\"spans\":[{\"file_name\":\"billing/billing-service/src/service.rs\",\"line_start\":26,\"line_end\":26,\"is_primary\":true}],\"children\":[]}}\n{\"reason\":\"build-finished\",\"success\":false}\n")
        `shouldBe` Right [("billing/billing-service/src/service.rs", "missing field `email_sender` in initializer of `CancelInvoice`")]
    it "is a known format" $ fmap supportFormat (supportFor "rustc-json") `shouldBe` Just "rustc-json"
  describe "pyright-json (Python, recorded from pyright)" $ do
    it "reads the undefined name from a message (auto-import)" $ do
      supportUndefined pyrightSupport "\"date\" is not defined" `shouldBe` Just "date"
      supportUndefined pyrightSupport "Type of \"date\" is unknown" `shouldBe` Nothing
    let sample name = BS.readFile ("src/Olifant/Language/testdata/pyright/" <> name <> "/report.json")
        sites r = [(missingFile m, fst (missingCase m), missingPatterns m) | m <- reportMissing r]
    it "is a known format, read from stdout" $ do
      fmap supportFormat (supportFor "pyright-json") `shouldBe` Just "pyright-json"
      fmap supportReadsStdout (supportFor "pyright-json") `shouldBe` Just True
    it "(a) a non-exhaustive match is a site at its `match` line, the class as a pattern" $ do
      Right r <- parsePyrightReport <$> sample "union"
      sites r `shouldBe` [("src/failures.py", 17, ["PartyTooLarge()"])]
    it "(a) the function's \"must return on all code paths\" is the missing case's doing, not a broken file" $ do
      Right r <- parsePyrightReport <$> sample "union"
      reportBroken r `shouldBe` []
    it "(b) an enum member is written as Enum.MEMBER" $ do
      Right r <- parsePyrightReport <$> sample "enum"
      sites r `shouldBe` [("src/colors.py", 9, ["Color.BLUE"])]
    it "(c) any other error is a broken file" $ do
      Right r <- parsePyrightReport <$> sample "broken"
      (sites r, map fst (reportBroken r)) `shouldBe` ([], ["src/broken.py"])
    it "(d) a pattern is written as the site spells the others: qualified when they are (Review Focus 1)" $ do
      site <- TIO.readFile "src/Olifant/Language/testdata/pyright/qualified/src/web.py"
      supportPattern pyrightSupport (missing "src/web.py" (4, 6) []) site "PartyTooLarge" "PartyTooLarge()" `shouldBe` "fx.PartyTooLarge()"
      supportPattern pyrightSupport (missing "a.py" (0, 1) []) "match f:\n    case SlotTaken():\n" "PartyTooLarge" "PartyTooLarge()" `shouldBe` "PartyTooLarge()"
    it "(python review I2) the qualifier is the site's own match's, not the first in the file" $ do
      let file = T.unlines
            [ "def is_ok(r):", "    match r:", "        case result.Ok():", "            return True", ""
            , "def status(f):", "    match f:", "        case create_booking.SlotTaken():", "            return 409" ]
      supportPattern pyrightSupport (missing "a.py" (6, 8) []) file "Cancelled" "Cancelled()" `shouldBe` "create_booking.Cancelled()"
    it "(e) a clean run is an empty report" $
      (parsePyrightReport <$> sample "clean") `shouldReturn` Right (CheckReport [] [] [])
    it "(f) two matches lacking the same case are two sites" $ do
      Right r <- parsePyrightReport <$> sample "two-matches"
      sites r `shouldBe` [("src/failures.py", 17, ["PartyTooLarge()"]), ("src/failures.py", 23, ["PartyTooLarge()"])]
      reportBroken r `shouldBe` []
    it "(g) output that is not pyright's JSON is an error" $
      parsePyrightReport "Traceback: oops" `shouldSatisfy` either (const True) (const False)
    it "names the enclosing function: the nearest def at or above the line" $
      supportFunction pyrightSupport "def a():\n    pass\n\ndef status(f):\n    match f:\n" 4 `shouldBe` "status"
  describe "the program a check runs" $ do
    it "skips `env` and its VAR=value arguments" $ do
      checkProgram ["env", "UV_PROJECT_ENVIRONMENT=/c/venv", "uv", "run", "pyright"] `shouldBe` Just "uv"
      checkProgram ["cargo", "check"] `shouldBe` Just "cargo"
      checkProgram ["env", "A=1"] `shouldBe` Nothing
    it "(Review Focus 3) a missing program behind `env` is a missing tool, not a failed check" $ do
      let Right lang = parseLanguage "python" "/p" (T.unlines ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\"", "[check]", "command = [\"env\", \"X=1\", \"olifant-no-such-tool\"]", "format = \"pyright-json\""])
      r <- runCheck processChecker lang (M.fromList [("a.py", "x = 1\n")])
      case r of
        Left (CheckToolMissing why) -> why `shouldSatisfy` T.isInfixOf "olifant-no-such-tool"
        other -> expectationFailure (show other)
  describe "package-scoped checks ([check] root)" $ do
    let lang root extra = either (error . T.unpack) id . parseLanguage "python" "/p" . T.unlines $
          ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\"", "[check]", "command = [\"uv\"]", "context = [\"pyproject.toml\", \"uv.lock\"]", "format = \"pyright-json\""]
            ++ ["root = [" <> T.intercalate ", " ["\"" <> r <> "\"" | r <- root] <> "]" | not (null root)] ++ extra
        monorepo = ["pyproject.toml", "tools/x.py", "apps/a/pyproject.toml", "apps/a/uv.lock", "apps/a/src/x.py", "apps/a/src/x__test.py", "apps/b/pyproject.toml", "apps/b/src/y.py"]
    it "checks the judged file's package: the nearest folder with a root marker, its files only" $
      checkScope (lang ["pyproject.toml"] []) monorepo ["apps/a/src/x.py"]
        `shouldBe` ("apps/a", ["apps/a/pyproject.toml", "apps/a/uv.lock", "apps/a/src/x.py", "apps/a/src/x__test.py"])
    it "checks the whole project when the language names no root (as before)" $
      checkScope (lang [] []) monorepo ["apps/a/src/x.py"]
        `shouldBe` ("", ["pyproject.toml", "tools/x.py", "apps/a/src/x.py", "apps/a/src/x__test.py", "apps/b/src/y.py"])
    it "checks the project when judged files sit in different packages" $
      fst (checkScope (lang ["pyproject.toml"] []) monorepo ["apps/a/src/x.py", "apps/b/src/y.py"]) `shouldBe` ""
    it "takes the outermost marker when asked (a Cargo workspace)" $ do
      let Right rust = parseLanguage "rust" "/p" (T.unlines ["files = [\"**/*.rs\"]", "comment = \"//\"", "grammar = \"rust\"", "[check]", "command = [\"cargo\"]", "context = [\"**/Cargo.toml\"]", "root = [\"Cargo.toml\"]", "root_pick = \"outermost\"", "format = \"rustc-json\""])
      fst (checkScope rust ["Cargo.toml", "crates/a/Cargo.toml", "crates/a/src/lib.rs"] ["crates/a/src/lib.rs"]) `shouldBe` ""
      fst (checkScope rust ["svc/Cargo.toml", "svc/crates/a/Cargo.toml", "svc/crates/a/src/lib.rs"] ["svc/crates/a/src/lib.rs"]) `shouldBe` "svc"
    it "reads `exclusive`: a check whose tool keeps a build cache (cargo) gets a cache no other check uses meanwhile" $ do
      let parsed extra = parseLanguage "x" "/p" (T.unlines (["files = [\"**/*.x\"]", "comment = \"//\"", "grammar = \"rust\"", "[check]", "command = [\"tool\"]", "format = \"rustc-json\""] ++ extra))
      fmap (fmap checkExclusive . langCheck) (parsed ["exclusive = true"]) `shouldBe` Right (Just True)
      fmap (fmap checkExclusive . langCheck) (parsed []) `shouldBe` Right (Just False)
      rust <- TIO.readFile "packs/rust-htmx/languages/rust.toml"
      fmap (fmap checkExclusive . langCheck) (parseOverShipped "rust" "packs/rust-htmx" rust) `shouldBe` Right (Just True)
    it "runs exclusive checks of a workspace in parallel, never two in one cache at once" $
      withSystemTempDirectory "olifant-exclusive" $ \dir -> do
        let log' = dir </> "log"
            command = "[\"sh\", \"-c\", \"echo start {= cache =} >> " <> T.pack log' <> "; sleep 0.3; echo end {= cache =} >> " <> T.pack log' <> "\"]"
            Right x = parseLanguage "x" "/p" (T.unlines ["files = [\"**/*.x\"]", "comment = \"//\"", "grammar = \"rust\"", "[check]", "command = " <> command, "format = \"rustc-json\"", "exclusive = true"])
        _ <- mapConcurrently (const (runCheck processChecker x (M.fromList [("a.x", "x")]))) [1 .. 6 :: Int]
        events <- map words . lines <$> readFile log'
        -- in each cache, a run ends before the next starts
        let byCache = M.fromListWith (flip (++)) [(c, [e]) | [e, c] <- events]
        M.elems byCache `shouldSatisfy` all (\es -> es == concat (replicate (length es `div` 2) ["start", "end"]))
        -- and several caches were in use at once
        M.size byCache `shouldSatisfy` (> 1)
        length events `shouldBe` 12
    it "reads `concurrent = false` (a compiler that cannot run twice at once: Elm), true by default" $ do
      let parsed extra = parseLanguage "x" "/p" (T.unlines (["files = [\"**/*.x\"]", "comment = \"//\"", "grammar = \"rust\"", "[check]", "command = [\"tool\"]", "format = \"rustc-json\""] ++ extra))
      fmap (fmap checkConcurrent . langCheck) (parsed ["concurrent = false"]) `shouldBe` Right (Just False)
      fmap (fmap checkConcurrent . langCheck) (parsed []) `shouldBe` Right (Just True)
      elm <- TIO.readFile "packs/elm/languages/elm.toml"
      fmap (fmap checkConcurrent . langCheck) (parseOverShipped "elm" "packs/elm" elm) `shouldBe` Right (Just False)
    it "runs a non-concurrent check one at a time: two at once never overlap" $
      withSystemTempDirectory "olifant-one-at-a-time" $ \dir -> do
        let log' = dir </> "log"
            command = "[\"sh\", \"-c\", \"echo start >> " <> T.pack log' <> "; sleep 0.3; echo end >> " <> T.pack log' <> "\"]"
            Right x = parseLanguage "x" "/p" (T.unlines ["files = [\"**/*.x\"]", "comment = \"//\"", "grammar = \"rust\"", "[check]", "command = " <> command, "format = \"rustc-json\"", "concurrent = false"])
        _ <- mapConcurrently (const (runCheck processChecker x (M.fromList [("a.x", "x")]))) [1, 2 :: Int]
        lines <$> readFile log' `shouldReturn` ["start", "end", "start", "end"]
    it "runs the checker inside the package and reports project paths" $ do
      seen <- newIORef []
      let checker = Checker $ \_ files -> do
            writeIORef seen (M.keys files)
            pure (Right (CheckReport [MissingPatterns "src/x.py" (3, 5) ["B()"] Nothing] [("src/x.py", "bad")] []))
      r <- runCheckIn checker (lang ["pyproject.toml"] []) "apps/a" (M.fromList [("apps/a/pyproject.toml", ""), ("apps/a/src/x.py", "x")])
      readIORef seen `shouldReturn` ["pyproject.toml", "src/x.py"]
      fmap reportBroken r `shouldBe` Right [("apps/a/src/x.py", "bad")]
      fmap (map missingFile . reportMissing) r `shouldBe` Right ["apps/a/src/x.py"]
    it "runs a verify command inside the package, its project paths made the package's" $ do
      seen <- newIORef ([], [])
      let verifier = Verifier $ \_ argv files -> writeIORef seen (argv, M.keys files) >> pure (Right (True, ""))
      _ <- runVerifyIn verifier (lang ["pyproject.toml"] []) "apps/a" ["uv", "run", "pytest", "apps/a/src/x__test.py"] (M.fromList [("apps/a/src/x__test.py", "")])
      readIORef seen `shouldReturn` (["uv", "run", "pytest", "src/x__test.py"], ["src/x__test.py"])
  describe "contextKey: one check environment per project and lockfile" $ do
    let Right py = parseLanguage "python" "/p" (T.unlines ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\"", "[check]", "command = [\"uv\"]", "context = [\"pyproject.toml\", \"uv.lock\"]", "format = \"pyright-json\""])
        Just check = langCheck py
        project lock src = M.fromList [("pyproject.toml", "[project]\nname = \"bistro\"\n"), ("uv.lock", lock), ("src/a.py", src)]
    it "is the same for the same project, whatever its sources say" $
      contextKey check (project "v1" "x = 1\n") `shouldBe` contextKey check (project "v1" "x = 2\n")
    it "changes with the lockfile (or the project file)" $
      contextKey check (project "v1" "x = 1\n") `shouldNotBe` contextKey check (project "v2" "x = 1\n")
  describe "compileClean with pyright (a site but no broken file)" $
    it "says which cases are missing, not an empty reason" $ do
      let report = "{\"generalDiagnostics\": [{\"file\": \"a.py\", \"severity\": \"error\", \"rule\": \"reportMatchNotExhaustive\", \"message\": \"Cases within match statement do not exhaustively handle all values\\n  Unhandled type: \\\"Gone\\\"\", \"range\": {\"start\": {\"line\": 3, \"character\": 4}, \"end\": {\"line\": 3, \"character\": 9}}}]}"
          Right lang = parseLanguage "python" "/p" (T.unlines ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\"", "[check]", "command = [\"sh\", \"-c\", " <> T.pack (show ("printf '%s' '" <> T.unpack report <> "'; exit 1")) <> "]", "format = \"pyright-json\""])
      r <- compileClean lang (M.fromList [("a.py", "x = 1\n")])
      r `shouldSatisfy` either ("Gone()" `T.isInfixOf`) (const False)
  describe "a check whose report cannot be read (python review I4)" $
    it "says what the tool printed on stderr (uv offline, no lockfile)" $ do
      let Right lang = parseLanguage "python" "/p" (T.unlines ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\"", "[check]", "command = [\"sh\", \"-c\", \"echo 'error: No `uv.lock` found; run uv lock' >&2; exit 2\"]", "format = \"pyright-json\""])
      r <- runCheck processChecker lang (M.fromList [("a.py", "x = 1\n")])
      case r of
        Left f -> describeCheckFailure f `shouldSatisfy` T.isInfixOf "No `uv.lock` found"
        other -> expectationFailure (show other)
  describe "a match's extent from its indentation (Python)" $ do
    it "(a) the last line of the block under a line: blank lines don't extend it, a line back at its indentation ends it" $
      indentBlockEnd ["def f(x):", "    match x:", "        case A():", "            return 1", "", "    return 2"] 1 `shouldBe` 3
    it "a column-0 line inside a triple-quoted string does not end the block" $
      indentBlockEnd ["    match x:", "        case A():", "            return \"\"\"one", "at column 0", "            \"\"\"", "    y = 1"] 0 `shouldBe` 4
    it "(b) a last arm whose body spans lines (nested if, a multi-line call) ends at its last line (Review Focus 2)" $ do
      src <- TIO.readFile "src/Olifant/Language/testdata/pyright/two-matches/src/failures.py"
      indentBlockEnd (T.splitOn "\n" src) 23 `shouldBe` 30
      indentBlockEnd (T.splitOn "\n" src) 17 `shouldBe` 19
    it "(c) through the checker: each pyright site spans its match to the last arm (real pyright via uvx)" $
      findExecutable "uvx" >>= \case
        Nothing -> lookupEnv "OLIFANT_REQUIRE_TOOLS" >>= \case
          Just "1" -> expectationFailure "required tool missing: uv (brew install uv)"
          _ -> pendingWith "uv is not installed (brew install uv)"
        Just _ -> do
          let Right py = parseLanguage "python" "/p" (T.unlines ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\"", "[check]", "command = [\"uvx\", \"--quiet\", \"pyright==1.1.414\", \"--outputjson\"]", "context = [\"pyproject.toml\"]", "format = \"pyright-json\""])
          files <- traverse (\f -> (f,) <$> TIO.readFile ("src/Olifant/Language/testdata/pyright/two-matches/" <> f)) ["pyproject.toml", "src/failures.py"]
          r <- runCheck processChecker py (M.fromList files)
          fmap (map (\m -> (missingFile m, missingCase m)) . reportMissing) r `shouldBe` Right [("src/failures.py", (17, 19)), ("src/failures.py", (23, 30))]
  describe "rustc-json sites (spec §5.6, fixtures captured from cargo 1.99)" $ do
    report <- runIO (BS.readFile "src/Olifant/Language/testdata/rustc-e0004.jsonl")
    let Right r = parseRustcReport report
        site line = [m | m <- reportMissing r, fst (missingCase m) == line - 1]
    it "reads each E0004 as a site from the scrutinee to the last arm, with patterns and type" $ do
      map (\m -> (missingFile m, missingCase m, missingPatterns m, missingType m)) (site 11)
        `shouldBe` [("src/lib.rs", (10, 14), ["&Failure::D"], Just "&Failure")]
      map missingPatterns (site 4) `shouldBe` [["&Failure::Locked { .. }", "&Failure::A", "&Failure::B"]]
      map missingCase (site 20) `shouldBe` [(19, 25)]
    it "places both nested sites (Review Focus 4)" $
      map missingCase (site 31 ++ site 32) `shouldBe` [(30, 35), (31, 33)]
    it "reports a refutable let (E0005) as a site it cannot extend" $
      map snd (reportUnplaced r) `shouldBe` ["a `let` binding"]
    it "has no broken files: E0004/E0005 are sites, not breakage" $
      reportBroken r `shouldBe` []
    it "writes patterns without the reference (match ergonomics)" $
      supportPattern rustcSupport (missing "a.rs" (0, 0) []) "" "Failure::D" "&Failure::D" `shouldBe` "Failure::D"
    it "sees Type::Variant (and Type.Variant) where that type is matched" $ do
      let [d] = site 11
      supportVisible rustcSupport d "" "Failure::D" `shouldBe` True
      supportVisible rustcSupport d "" "Other::D" `shouldBe` False
      supportVisible rustcSupport d "" "Failure.D" `shouldBe` True
    it "names the enclosing fn" $
      rustFunctionAt "pub fn one(f: &Failure) -> u16 {\n    match f {\n" 1 `shouldBe` "one"
    it "appends a comma to the last arm before a new one (rust.toml's arm_separator)" $ do
      rust <- shippedLanguage "rust" []
      langArmSeparator rust `shouldBe` ","
      langLoneArmSpaced rust `shouldBe` False
    it "module_of: a path's module, by the language's [imports] module_of" $ do
      py <- shippedLanguage "python" []
      hs <- shippedLanguage "haskell" []
      rust <- shippedLanguage "rust" []
      let mo l = maybe (error "no module_of") moduleOf (langImportsDef l >>= imModuleOf)
      mo py "src/bistro/booking/commands/close_table.py" `shouldBe` "bistro.booking.commands.close_table"
      mo py "pkg/__init__.py" `shouldBe` "pkg"
      mo hs "src/Olifant/Plans.hs" `shouldBe` "Olifant.Plans"
      mo rust "crates/money/src/lib.rs" `shouldBe` ""
      mo rust "crates/money/src/money/rates.rs" `shouldBe` "money::rates"
      originOf hs "imports.write" `shouldBe` Just "shipped"
    it "the shipped python language finds its tests by pytest's rules" $ do
      py <- shippedLanguage "python" []
      fmap (null . testUnitRules) (langTest py) `shouldBe` Just False
      originOf py "test.unit" `shouldBe` Just "toolchain pytest"
      originOf py "test.above" `shouldBe` Just "toolchain pytest"
    it "a lone arm's new neighbour goes after a blank line in Haskell and Elm, not in Python" $ do
      hs <- shippedLanguage "haskell" []
      elm <- shippedLanguage "elm" [("pack elm", "packs/elm")]
      py <- shippedLanguage "python" []
      map langLoneArmSpaced [hs, elm, py] `shouldBe` [True, True, False]
      map langArmSeparator [hs, elm, py] `shouldBe` ["", "", ""]
  describe "check commands" $
    it "expands {= cache =} to an existing per-language cache directory" $ do
      let Right lang = parseLanguage "cachey" "/p" (T.unlines ["files = [\"**/*.x\"]", "comment = \"--\"", "grammar = \"x\"", "[check]", "command = [\"sh\", \"-c\", \"test -d \\\"$0\\\"\", \"{= cache =}\"]", "format = \"rustc-json\""])
      compileClean lang (M.fromList [("a.x", "")]) `shouldReturn` Right ()
  describe "GHC support (fixtures captured from GHC 9.10.3)" $ do
    let real = BS.readFile "src/Olifant/Language/testdata/ghc-62161.jsonl"
    it "reads one pattern per line, the full type, and only case alternatives as sites (review C1, C2, I1)" $ do
      Right r <- parseGhcReport <$> real
      [(missingPatterns m, missingType m) | m <- reportMissing r]
        `shouldBe` [ (["SampleBroken _ _", "SampleLocked _"], Just "SampleError")
                   , (["((SampleLocked _), _)"], Just "(SampleError, Bool)")
                   , (["Just (SampleLocked _)"], Just "Maybe SampleError")
                   ]
      map (fst . missingCase) (reportMissing r) `shouldBe` [8, 12, 17]
    it "puts equations, lambdas and pattern bindings in reportUnplaced, described (review I1)" $ do
      Right r <- parseGhcReport <$> real
      [(missingPatterns m, d) | (m, d) <- reportUnplaced r]
        `shouldBe` [ (["SampleLocked _"], "an equation for `label`")
                   , (["SampleBroken _ _", "SampleLocked _"], "a lambda abstraction")
                   , (["SampleBroken _ _", "SampleLocked _"], "a pattern binding")
                   ]
    it "reads a missing-pattern site even when -Werror makes it an error (review I2)" $ do
      Right r <- parseGhcReport <$> BS.readFile "src/Olifant/Language/testdata/ghc-62161-werror.jsonl"
      (map missingPatterns (reportMissing r), reportBroken r) `shouldBe` ([["SampleBroken _ _", "SampleLocked _"]], [])
    it "drops GHC's truncation marker" $
      fmap (map missingPatterns . reportMissing) (parseGhcReport (TE.encodeUtf8 ("{\"span\":{\"file\":\"src/A.hs\",\"start\":{\"line\":2,\"column\":1},\"end\":{\"line\":4,\"column\":1}},\"severity\":\"Warning\",\"code\":62161,\"message\":[\"Pattern match(es) are non-exhaustive\\nIn a case alternative:\\n    Patterns of type `T' not matched:\\n        A\\n        B\\n        ...\"]}")))
        `shouldBe` Right [["A", "B"]]
    it "reports errors as broken files and ignores other warnings" $ do
      let err = "{\"span\":{\"file\":\"src/B.hs\",\"start\":{\"line\":1,\"column\":1},\"end\":{\"line\":1,\"column\":2}},\"severity\":\"Error\",\"code\":88464,\"message\":[\"Variable not in scope: x\"]}"
          other = "{\"span\":{\"file\":\"src/C.hs\",\"start\":{\"line\":1,\"column\":1},\"end\":{\"line\":1,\"column\":2}},\"severity\":\"Warning\",\"code\":40910,\"message\":[\"Defined but not used\"]}"
      parseGhcReport (TE.encodeUtf8 (err <> "\n" <> other)) `shouldBe` Right (CheckReport [] [("src/B.hs", "Variable not in scope: x")] [])
    it "reads the type with ASCII or unicode quotes (Review Focus 3)" $ do
      typeOf "Patterns of type \8216CatalogError\8217 not matched: PackLocked _" `shouldBe` Just "CatalogError"
      typeOf "Patterns of type `Olifant.Catalog.CatalogError' not matched: PackLocked _" `shouldBe` Just "Olifant.Catalog.CatalogError"
    it "is visible where the type appears anywhere in GHC's type: tuples, Maybe, qualified (review C2)" $ do
      let Just g = supportFor "ghc-json"
          at ty = MissingPatterns "src/A.hs" (4, 8) ["x"] (Just ty)
      supportVisible g (at "(SampleError, Bool)") "" "SampleError.SampleLocked" `shouldBe` True
      supportVisible g (at "Maybe SampleError") "" "SampleError.SampleLocked" `shouldBe` True
      supportVisible g (at "Olifant.Catalog.CatalogError") "" "CatalogError.PackLocked" `shouldBe` True
      supportVisible g (at "SampleErrorish") "" "SampleError.SampleLocked" `shouldBe` False
      supportVisible g (at "RegionError") "" "SampleError.SampleLocked" `shouldBe` False
      supportVisible g (at "RegionError") "" "SampleLocked" `shouldBe` True
      supportPattern g (missing "a.hs" (0, 0) []) "" "CatalogError.PackLocked" "PackLocked _" `shouldBe` "PackLocked _"
    it "asks GHC not to truncate pattern lists (review M2)" $ do
      src <- TIO.readFile ".olifant/languages/haskell.toml"
      -- (through the cabal-repl preset)
      let Right hs = parseOverShipped "haskell" ".olifant" src
      fmap checkCommand (langCheck hs) `shouldSatisfy` maybe False (any ("-fmax-uncovered-patterns=" `T.isInfixOf`))
  describe "layers (shipped, packs, project; merged by key)" $ do
    let layer origin dir ls = either (error . T.unpack) (Layer origin dir) (parseToml (T.unlines ls))
        base = ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\""]
        Right two = languageFromLayers shippedPresets "python" [layer "shipped" Nothing (base ++ ["indent = \"  \"", "keywords = [\"a\", \"b\"]", "[unit]", "kinds = [\"a\"]"]), layer "pack p" (Just "/p") ["[unit]", "join = true"]]
    it "mergeTables: the right wins; tables merge, lists and scalars are replaced" $ do
      let Right a = parseToml (T.unlines ["x = 1", "l = [1, 2]", "[t]", "a = 1", "b = 1"])
          Right b = parseToml (T.unlines ["l = [3]", "[t]", "b = 2"])
          Right ab = parseToml (T.unlines ["x = 1", "l = [3]", "[t]", "a = 1", "b = 2"])
      mergeTables a b `shouldBe` ab
    it "a later layer's key wins and the rest is kept" $ do
      langIndent two `shouldBe` "  "
      langUnitKinds two `shouldBe` ["a"]
      langUnitJoin two `shouldBe` True
      originOf two "unit.join" `shouldBe` Just "pack p"
      originOf two "unit.kinds" `shouldBe` Just "shipped"
      originOf two "indent" `shouldBe` Just "shipped"
      langLayers two `shouldBe` ["shipped", "pack p"]
    it "lists replace" $ do
      let Right l = languageFromLayers shippedPresets "python" [layer "shipped" Nothing (base ++ ["keywords = [\"a\", \"b\"]"]), layer "project" (Just "/q/.olifant") ["keywords = [\"c\"]"]]
      langKeywords l `shouldBe` ["c"]
      originOf l "keywords" `shouldBe` Just "project"
    it "a preset fills after the merge: a later layer's preset swaps it, earlier explicit keys stay (Review Focus 2)" $ do
      let Right l = languageFromLayers shippedPresets "python" [layer "pack p" (Just "/p") (base ++ ["[test]", "preset = \"pytest\"", "tests_for = \"X\""]), layer "project" (Just "/q/.olifant") ["[test]", "preset = \"hspec\""]]
      fmap testFormat (langTest l) `shouldBe` Just "lines"
      fmap testsFor (langTest l) `shouldBe` Just "X"
      originOf l "test.format" `shouldBe` Just "toolchain hspec"
      originOf l "test.tests_for" `shouldBe` Just "pack p"
      originOf l "test.preset" `shouldBe` Just "project"
    it "a layer inherits what it does not set (Review Focus 1)" $ do
      let Right l = languageFromLayers shippedPresets "python" [layer "shipped" Nothing (base ++ ["[test]", "preset = \"pytest\""]), layer "pack p" (Just "/p") ["indent = \"    \""]]
      fmap testFormat (langTest l) `shouldBe` Just "pytest"
      fmap testsFor (langTest l) `shouldBe` Just "{= dir =}/test_{= stem =}.py"
      originOf l "test" `shouldBe` Just "shipped"
      originOf l "test.tests_for" `shouldBe` Just "toolchain pytest"
    it "a layer that sets command or convert and no preset drops the inherited preset" $ do
      let Right l = languageFromLayers shippedPresets "haskell" [layer "shipped" Nothing (base ++ ["[test]", "preset = \"hspec\""]), layer "pack p" (Just "/p") ["[test]", "command = [\"my-runner\"]", "format = \"junit-xml\"", "tests_for = \"x\""]]
          Just td = langTest l
      testCommand td `shouldBe` ["my-runner"]
      testFormat td `shouldBe` "junit-xml"
      testSelect td `shouldBe` Nothing
    it "a layer that sets neither command nor convert keeps the inherited preset" $ do
      let Right l = languageFromLayers shippedPresets "haskell" [layer "shipped" Nothing (base ++ ["[test]", "preset = \"hspec\""]), layer "pack p" (Just "/p") ["[test]", "tests_for = \"X\""]]
      fmap testFormat (langTest l) `shouldBe` Just "lines"
      fmap testsFor (langTest l) `shouldBe` Just "X"
    it "the grammar follows the layer that set it (Review Focus 3)" $ do
      let Right l = languageFromLayers shippedPresets "elm" [layer "pack elm" (Just "packs/elm") ["files = [\"**/*.elm\"]", "comment = \"--\"", "grammar = { source = \"grammar/elm\" }"], layer "project" (Just ".olifant") ["indent = \"    \""]]
      langGrammar l `shouldBe` GrammarSource "packs/elm/languages/grammar/elm"
    it "a shipped grammar source is an error" $
      languageFromLayers shippedPresets "x" [layer "shipped" Nothing ["files = [\"*.x\"]", "comment = \"#\"", "grammar = { source = \"g\" }"]]
        `shouldSatisfy` either ("cannot be shipped" `T.isInfixOf`) (const False)
    it "parseLanguage is one pack layer" $ do
      let Right l = parseLanguage "python" "packs/python-htmx" (T.unlines base)
      langLayers l `shouldBe` ["pack python-htmx"]
  describe "[[mutate]] (mutation operators)" $ do
    let x extra = parseLanguage "x" "/x" (T.unlines (["files = [\"**/*.x\"]", "comment = \"#\"", "grammar = \"python\""] ++ extra))
    it "reads a pattern with a fix, and a rule with the increment transform" $ do
      let Right l = x ["[[mutate]]", "name = \"boundary\"", "pattern = \"$A < $B\"", "fix = \"$A <= $B\"", "[[mutate]]", "name = \"number\"", "rule = { kind = \"integer\" }", "transform = \"increment\""]
      map mutName (langMutate l) `shouldBe` ["boundary", "number"]
      map (either (const Nothing) Just . mutFix) (langMutate l) `shouldBe` [Just "$A <= $B", Nothing]
    it "one of pattern or rule" $ do
      x ["[[mutate]]", "name = \"a\"", "pattern = \"p\"", "rule = { kind = \"k\" }", "fix = \"f\""] `shouldSatisfy` either ("one of `pattern` or `rule`" `T.isInfixOf`) (const False)
      x ["[[mutate]]", "name = \"a\"", "fix = \"f\""] `shouldSatisfy` either ("one of `pattern` or `rule`" `T.isInfixOf`) (const False)
    it "one of fix or transform" $
      x ["[[mutate]]", "name = \"a\"", "pattern = \"p\""] `shouldSatisfy` either ("one of `fix` or `transform`" `T.isInfixOf`) (const False)
    it "the only transform is increment" $
      x ["[[mutate]]", "name = \"a\"", "pattern = \"p\"", "transform = \"double\""] `shouldSatisfy` either ("\"increment\"" `T.isInfixOf`) (const False)
  describe "parseElmReport" $ do
    it "reads each MISSING PATTERNS problem: file, 0-based case lines, patterns" $ do
      report <- BS.readFile "src/Olifant/Language/testdata/elm-missing-patterns.json"
      parseElmReport report
        `shouldBe` Right
          ( CheckReport
              [ missing "/tmp/elmcheck/src/Page/SamplePage.elm" (30, 32) ["SetName _"]
              , missing "/tmp/elmcheck/src/Route.elm" (29, 34) ["Settings"]
              ]
              []
              []
          )
    it "is empty for a clean compile" $ parseElmReport "" `shouldBe` Right (CheckReport [] [] [])
    it "lists files with other problems as broken (review)" $
      parseElmReport "{\"type\":\"compile-errors\",\"errors\":[{\"path\":\"/x/src/A.elm\",\"name\":\"A\",\"problems\":[{\"title\":\"NAMING ERROR\",\"region\":{\"start\":{\"line\":1,\"column\":0},\"end\":{\"line\":1,\"column\":3}},\"message\":[\"x\"]}]}]}"
        `shouldBe` Right (CheckReport [] [("/x/src/A.elm", "NAMING ERROR")] [])
    it "reports a non-compile error as a failure" $
      parseElmReport "{\"type\":\"error\",\"path\":\"elm.json\",\"title\":\"BAD JSON\",\"message\":[\"oops\"]}" `shouldSatisfy` either ("BAD JSON" `T.isInfixOf`) (const False)
  describe "qualifyPattern" $ do
    let mainElm = "module Main exposing (main)\nimport Route exposing (Route)\nimport Page.Home as Home\n"
    it "qualifies a constructor from another module" $
      qualifyPattern mainElm "Route.Settings" "Just Settings" `shouldBe` "Just Route.Settings"
    it "leaves it bare in its own module or when exposed" $ do
      qualifyPattern "module Route exposing (..)\n" "Route.Settings" "Settings" `shouldBe` "Settings"
      qualifyPattern "module Main exposing (main)\nimport Route exposing (..)\n" "Route.Settings" "Just Settings" `shouldBe` "Just Settings"
    it "uses an import alias" $
      qualifyPattern "module Main exposing (main)\nimport Page.Home as Home\n" "Page.Home.Reset" "Reset" `shouldBe` "Home.Reset"
    it "leaves unqualified constructors and other words alone" $
      qualifyPattern mainElm "SetName" "SetName _" `shouldBe` "SetName _"
  describe "visibleAt" $
    it "sees a qualified constructor only in its module or a module importing it" $ do
      let route = "module Route exposing (..)\nimport Url\n"
          mainElm = "module Main exposing (main)\nimport Route exposing (Route)\n"
      visibleAt route "Route.Settings" `shouldBe` True
      visibleAt route "Main.Settings" `shouldBe` False
      visibleAt mainElm "Route.Settings" `shouldBe` True
      visibleAt mainElm "Main.Settings" `shouldBe` True
      visibleAt route "Settings" `shouldBe` True
  describe "declarationAt" $
    it "finds the top-level declaration above a line" $
      declarationAt "toPath : Route -> String\ntoPath route =\n    case route of\n        Home ->\n" 2 `shouldBe` "toPath"
  describe "processChecker (real elm)" $ do
    it "reports the missing branch in a page's update" $
      findExecutable "elm" >>= \case
        Nothing -> pendingWith "elm is not installed"
        Just _ -> do
          src <- TIO.readFile "packs/elm/languages/elm.toml"
          let Right elm = parseOverShipped "elm" "packs/elm" src
          files <- traverse (\p -> (p,) <$> TIO.readFile ("packs/elm/fixtures/app" </> p)) ["elm.json", "src/Main.elm", "src/Route.elm", "src/Page/Home.elm", "src/Page/SamplePage.elm"]
          let withReset = [(p, if p == "src/Page/SamplePage.elm" then T.replace "    = NoOp\n" "    = NoOp\n    | Reset\n" t else t) | (p, t) <- files]
          r <- runCheck processChecker elm (M.fromList withReset)
          fmap (map (\m -> (missingFile m, missingPatterns m)) . reportMissing) r `shouldBe` Right [("src/Page/SamplePage.elm", ["Reset"])]
    it "reports a file that does not compile as broken, relative to the project (review)" $
      findExecutable "elm" >>= \case
        Nothing -> pendingWith "elm is not installed"
        Just _ -> do
          src <- TIO.readFile "packs/elm/languages/elm.toml"
          let Right elm = parseOverShipped "elm" "packs/elm" src
          files <- traverse (\p -> (p,) <$> TIO.readFile ("packs/elm/fixtures/app" </> p)) ["elm.json", "src/Main.elm", "src/Route.elm", "src/Page/Home.elm", "src/Page/SamplePage.elm"]
          let typo = [(p, if p == "src/Page/Home.elm" then T.replace "Cmd.none" "Cmd.nonee" t else t) | (p, t) <- files]
          r <- runCheck processChecker elm (M.fromList typo)
          fmap (nub . map fst . reportBroken) r `shouldBe` Right ["src/Page/Home.elm"]

-- | Run with the real ast-grep and the pack's Elm adapter, or mark pending.
-- | The rust-htmx pack's adapter on the fixture's service.rs, or pending.
withRust :: (Syntax IO -> Language -> Text -> Expectation) -> Expectation
withRust k =
  findExecutable "ast-grep" >>= \case
    Nothing -> pendingWith "ast-grep is not installed (brew install ast-grep)"
    Just _ -> do
      src <- TIO.readFile "packs/rust-htmx/languages/rust.toml"
      tmp <- getTemporaryDirectory
      let Right lang = parseOverShipped "rust" "packs/rust-htmx" src
      file <- TIO.readFile "packs/rust-htmx/fixtures/app/billing/billing-service/src/service.rs"
      k (astGrepSyntax (tmp </> "olifant-test-cache")) lang file

-- | The olifant-dev pack's Haskell adapter on the fixture's SampleErrors.hs, or pending.
withHaskell :: (Syntax IO -> Language -> Text -> Expectation) -> Expectation
withHaskell k =
  findExecutable "ast-grep" >>= \case
    Nothing -> pendingWith "ast-grep is not installed (brew install ast-grep)"
    Just _ -> do
      src <- TIO.readFile ".olifant/languages/haskell.toml"
      tmp <- getTemporaryDirectory
      let Right lang = parseOverShipped "haskell" ".olifant" src
      file <- TIO.readFile "packs/olifant-dev/fixtures/app/src/Olifant/SampleErrors.hs"
      k (astGrepSyntax (tmp </> "olifant-test-cache")) lang file

-- | The rust-htmx pack's adapter on one of its fixture's files, or pending.
-- | A module with each Python target in it.
pythonModule :: Text
pythonModule =
  T.unlines
    [ "import fx"
    , "from dataclasses import dataclass"
    , ""
    , ""
    , "@dataclass(frozen=True)"
    , "class SlotTaken:"
    , "    reason: str"
    , ""
    , ""
    , "type CreateBookingFailure = ("
    , "    SlotTaken"
    , "    | PartyTooLarge"
    , ")"
    , ""
    , ""
    , "class BookingService:"
    , "    def __init__(self) -> None:"
    , "        self.x = 1"
    , ""
    , "    def run(self) -> int:"
    , "        return 1"
    , ""
    , ""
    , "def floor_routes("
    , "    find_free_tables: FindFreeTables,"
    , ") -> int:"
    , "    return 1"
    , ""
    , ""
    , "def create(db_path: str) -> int:"
    , "    create_booking = CreateBooking("
    , "        bookings=booking_repository,"
    , "    )"
    , "    cancel_booking = CancelBooking()"
    , "    return 1"
    , ""
    , ""
    , "def status(f: CreateBookingFailure) -> int:"
    , "    match f:"
    , "        case SlotTaken():"
    , "            return 409"
    , "        case PartyTooLarge():"
    , "            return 422"
    ]

-- | The python-htmx pack's adapter on 'pythonModule', or pending.
withPython :: (Syntax IO -> Language -> Text -> Expectation) -> Expectation
withPython k =
  findExecutable "ast-grep" >>= \case
    Nothing -> pendingWith "ast-grep is not installed (brew install ast-grep)"
    Just _ -> do
      src <- TIO.readFile "packs/python-htmx/languages/python.toml"
      tmp <- getTemporaryDirectory
      let Right lang = parseOverShipped "python" "packs/python-htmx" src
      k (astGrepSyntax (tmp </> "olifant-test-cache")) lang pythonModule

withRustFile :: FilePath -> (Syntax IO -> Language -> Text -> Expectation) -> Expectation
withRustFile rel k =
  findExecutable "ast-grep" >>= \case
    Nothing -> pendingWith "ast-grep is not installed (brew install ast-grep)"
    Just _ -> do
      src <- TIO.readFile "packs/rust-htmx/languages/rust.toml"
      tmp <- getTemporaryDirectory
      let Right lang = parseOverShipped "rust" "packs/rust-htmx" src
      file <- TIO.readFile ("packs/rust-htmx/fixtures/app" </> rel)
      k (astGrepSyntax (tmp </> "olifant-test-cache")) lang file

withAstGrep :: (Syntax IO -> Language -> Text -> Expectation) -> Expectation
withAstGrep = withAstGrepOn "src/Main.elm"

withAstGrepOn :: FilePath -> (Syntax IO -> Language -> Text -> Expectation) -> Expectation
withAstGrepOn fixtureFile k =
  findExecutable "ast-grep" >>= \case
    Nothing -> pendingWith "ast-grep is not installed (brew install ast-grep)"
    Just _ -> do
      src <- TIO.readFile "packs/elm/languages/elm.toml"
      tmp <- getTemporaryDirectory
      let Right elm = parseOverShipped "elm" "packs/elm" src
      file <- TIO.readFile ("packs/elm/fixtures/app" </> fixtureFile)
      k (astGrepSyntax (tmp </> "olifant-test-cache")) elm file

readFixtureTree :: FilePath -> IO (M.Map FilePath Text)
readFixtureTree root = M.fromList <$> go root
  where
    go d = do
      names <- listDirectory d
      fmap concat . forM names $ \n -> do
        let p = d </> n
        isDir <- doesDirectoryExist p
        if isDir then go p else (\t -> [(makeRelative root p, t)]) <$> TIO.readFile p
