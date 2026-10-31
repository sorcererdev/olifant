module Olifant.Language.TestUnitsSpec (spec) where

import Control.Monad (filterM, forM_)
import Data.List (isSuffixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Language (Language (..), Provider (..), TestDef (..), UnitRule (..), shippedLanguage)
import System.Directory (getCurrentDirectory)
import Olifant.Language.TestUnits
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Olifant.Server.Tests (testAtIn, testLineIn)
import Olifant.Server.Stub (testWordsIn)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec
import Control.Exception (evaluate)
import System.Timeout (timeout)
import System.IO.Temp (withSystemTempDirectory)

-- a keyword at the line's start or after a space or `$` (`spec = describe "…" $ do`); the last group is the name
hspecUnit :: [UnitRule]
hspecUnit = [UnitRule "(^|[[:space:]$])(describe|context|it|specify|prop)[[:space:]]+\"([^\"]*)\"" Nothing Nothing]

specText :: Text
specText =
  T.unlines
    [ "module Olifant.PlansSpec (spec) where"
    , "spec :: Spec"
    , "spec = describe \"Olifant.Plans\" $ do"
    , "  it \"(a) reads a step\" $"
    , "    1 `shouldBe` 1"
    , "  describe \"parsePlan\" $ do"
    , "    it \"x/y, z\" $ True `shouldBe` True"
    ]

rustRules :: [UnitRule]
rustRules = [UnitRule "^[[:space:]]*mod[[:space:]]+(tests)" Nothing Nothing, UnitRule "^[[:space:]]*fn[[:space:]]+([[:alnum:]_]+)" (Just "#\\[test\\]") Nothing]

rustText :: Text
rustText =
  T.unlines
    [ "pub fn add(a: i32, b: i32) -> i32 {"
    , "    a + b"
    , "}"
    , ""
    , "#[cfg(test)]"
    , "mod tests {"
    , "    use super::*;"
    , ""
    , "    fn helper() -> i32 {"
    , "        1"
    , "    }"
    , ""
    , "    #[test]"
    , "    fn adds() {"
    , "        assert_eq!(add(1, helper()), 2);"
    , "    }"
    , ""
    , "    #[test]"
    , "    #[should_panic]"
    , "    fn panics() {"
    , "        panic!(\"x\");"
    , "    }"
    , "}"
    ]

pyFiles :: FilePath -> IO [FilePath]
pyFiles dir = do
  entries <- map (dir </>) <$> listDirectory dir
  dirs <- filterM doesDirectoryExist entries
  nested <- concat <$> mapM pyFiles [d | d <- dirs, not (any (`isSuffixOf` d) ["/.venv", "/__pycache__", "/node_modules"])]
  pure ([f | f <- entries, f `notElem` dirs, ".py" `isSuffixOf` f] ++ nested)

spec :: Spec
spec = do
  describe "testUnitsOf: the one door to test units (4b m1)" $ do
    it "only testUnitsOf reads test units: testUnitsIn appears in no other module" $ do
      files <- haskellSources "src"
      found <- concat <$> mapM (\f -> (\t -> [(f, i) | (i, l) <- zip [1 :: Int ..] (T.lines t), "testUnitsIn" `T.isInfixOf` l]) <$> TIO.readFile f) [f | f <- files, not ("Spec.hs" `isSuffixOf` f), f /= "src/Olifant/Language/TestUnits.hs"]
      found `shouldBe` []
    it "a test-units provider gives paths by containment" $ do
      lang <- providing "test-units.sh"
      testUnitsOf lang Nothing (T.unlines (replicate 9 "x")) `shouldBe` [TestUnit ["describe"] (1, 9), TestUnit ["describe", "it"] (2, 4)]
      testCaseAtIn lang Nothing (T.unlines (replicate 9 "x")) 3 `shouldBe` Just "describe::it"
    it "the request names the file: the same text at two paths is asked twice (review I4)" $
      withSystemTempDirectory "olifant-file-units" $ \dir -> do
        lang <- providingWith "file-unit.sh" [T.pack (dir </> "runs")]
        let t = "def test_x():\n    pass\n"
        testUnitsOf lang (Just "a__test.py") t `shouldBe` [TestUnit ["a__test.py"] (1, 1)]
        testUnitsOf lang (Just "a__test.py") t `shouldBe` [TestUnit ["a__test.py"] (1, 1)]
        testUnitsOf lang (Just "b__test.py") t `shouldBe` [TestUnit ["b__test.py"] (1, 1)]
        testUnitsOf lang Nothing t `shouldBe` [TestUnit ["none"] (1, 1)]
        length . lines <$> readFile (dir </> "runs") `shouldReturn` 3
    it "a failing provider: no test units, the reason in testUnitsReport" $ do
      lang <- providing "fails.sh"
      testUnitsOf lang Nothing "a\nb\n" `shouldBe` []
      testUnitsReport lang Nothing "a\nb\n" `shouldBe` Left "no grammar"
  it "names the test at a line by its describe path" $ do
    testAtBy hspecUnit Nothing specText 5 `shouldBe` Just "Olifant.Plans::(a) reads a step"
    testAtBy hspecUnit Nothing specText 7 `shouldBe` Just "Olifant.Plans::parsePlan::x/y, z"
    testAtBy hspecUnit Nothing specText 1 `shouldBe` Nothing
  it "finds a describe by the anchor's dotted name" $ do
    testLineBy hspecUnit Nothing specText "Olifant.Plans" `shouldBe` Just 3
    testLineBy hspecUnit Nothing specText "Olifant.Plans.parsePlan" `shouldBe` Just 6
    testLineBy hspecUnit Nothing specText "nope" `shouldBe` Nothing
  it "units: paths and 1-based inclusive lines" $
    map (\u -> (tuPath u, tuLines u)) (testUnitsIn hspecUnit Nothing specText)
      `shouldBe` [(["Olifant.Plans"], (3, 7)), (["Olifant.Plans", "(a) reads a step"], (4, 5)), (["Olifant.Plans", "parsePlan"], (6, 7)), (["Olifant.Plans", "parsePlan", "x/y, z"], (7, 7))]
  describe "after and above" $ do
    it "after gates a unit" $
      map tuPath (testUnitsIn rustRules Nothing rustText) `shouldBe` [["tests"], ["tests", "adds"]]
    it "after looks past above lines" $ do
      map tuPath (testUnitsIn rustRules (Just "^[[:space:]]*#\\[") rustText) `shouldBe` [["tests"], ["tests", "adds"], ["tests", "panics"]]
      testAtBy rustRules (Just "^[[:space:]]*#\\[") rustText 18 `shouldBe` Just "tests::panics"
      testAtBy rustRules (Just "^[[:space:]]*#\\[") rustText 13 `shouldBe` Just "tests::adds"
      testAtBy rustRules (Just "^[[:space:]]*#\\[") rustText 10 `shouldBe` Just "tests"
    it "above belongs to the unit below" $ do
      let py = T.unlines ["import pytest", "", "", "@pytest.mark.x", "def test_a() -> None:", "    assert 1"]
          rules = [UnitRule "^[[:space:]]*def[[:space:]]+(test[[:alnum:]_]*)" Nothing Nothing]
      testAtBy rules (Just "^[[:space:]]*@") py 4 `shouldBe` Just "test_a"
      testAtBy rules Nothing py 4 `shouldBe` Nothing
      testLineBy rules (Just "^[[:space:]]*@") py "test_a" `shouldBe` Just 4
  describe "anchor names" $ do
    it "squashes each run outside [A-Za-z0-9_.] to one _" $ do
      anchorName ["Olifant.Plans", "parses \"x\" twice"] `shouldBe` "Olifant.Plans.parses_x_twice"
      anchorName ["Olifant.Plans", "a.b"] `shouldBe` "Olifant.Plans.a.b"
      anchorName ["TestX", "test_y"] `shouldBe` "TestX.test_y"
    it "testLineBy finds an it by its anchor name" $ do
      let t = T.unlines ["spec = describe \"Olifant.Plans\" $ do", "  it \"parses \\\"x\\\" twice\" $", "    pure ()"]
      hs <- shippedLanguage "haskell" []
      testLineIn hs Nothing t "Olifant.Plans.parses_x_twice" `shouldBe` Just 2
      testAtIn hs Nothing t 3 `shouldBe` Just "Olifant.Plans::parses \"x\" twice"
    it "a name repeated in two describes: the first" $ do
      let t = T.unlines ["spec = describe \"M\" $ do", "  describe \"a\" $", "    it \"x\" $ pure ()", "  describe \"a\" $", "    it \"x\" $ pure ()"]
      testLineBy hspecUnit Nothing t "M.a.x" `shouldBe` Just 3
  describe "pytest rules equal the built-in shape (testAt)" $ do
    it "on every line of every .py of the python-htmx fixtures and the pytest testdata" $ do
      py <- shippedLanguage "python" []
      files <- concat <$> mapM pyFiles ["packs/python-htmx/fixtures", "src/Olifant/Language/testdata/pytest"]
      length files `shouldSatisfy` (> 10)
      forM_ files $ \f -> do
        text <- TIO.readFile f
        let diffs = [(f, i, testCaseAtIn py Nothing text i, testAt text i) | i <- [0 .. length (T.lines text) + 1], testCaseAtIn py Nothing text i /= testAt text i]
        diffs `shouldBe` []
    it "a decorated method in a test class, its decorator, a nested def, an async test" $ do
      py <- shippedLanguage "python" []
      let t =
            T.unlines
              [ "import pytest"                         -- 1
              , ""                                      -- 2
              , ""                                      -- 3
              , "class TestSeats:"                      -- 4
              , "    @pytest.mark.slow"                 -- 5
              , "    def test_two(self) -> None:"       -- 6
              , "        def helper() -> int:"          -- 7
              , "            return 2"                  -- 8
              , ""                                      -- 9
              , "        assert helper() == 2"          -- 10
              , ""                                      -- 11
              , ""                                      -- 12
              , "async def test_later() -> None:"       -- 13
              , "    assert True"                       -- 14
              ]
      forM_ [5 .. 14] $ \i -> (i, testAtIn py Nothing t i) `shouldBe` (i, testAt t i)
      map (testAtIn py Nothing t) [5, 8, 13] `shouldBe` [Just "TestSeats::test_two", Just "TestSeats::test_two", Just "test_later"]
  describe "conjure's test lookups (by the language's rules)" $ do
    let pyFile =
          T.unlines
            [ "from shop.close_table import CloseTable"   -- 1
            , ""                                          -- 2
            , ""                                          -- 3
            , "def make_table() -> CloseTable:"           -- 4
            , "    return CloseTable()"                   -- 5
            , ""                                          -- 6
            , ""                                          -- 7
            , "@pytest.mark.parametrize(\"n\", [1, 2])"   -- 8
            , "def test_closes(n: int) -> None:"          -- 9
            , "    table = make_table()"                  -- 10
            , ""                                          -- 11
            , "    assert table.handle(n) == 1"           -- 12
            , ""                                          -- 13
            , ""                                          -- 14
            , "class TestSeats:"                          -- 15
            , "    def test_two(self) -> None:"           -- 16
            , "        assert make_table().handle(2) == 0" -- 17
            ]
        hsFile =
          T.unlines
            [ "spec :: Spec"
            , "spec = describe \"Olifant.Plans\" $ do"
            , "  it \"parses an empty plan\" $ do"
            , "    let ps = parsePlan \"\""
            , "    length ps `shouldBe` 0"
            , ""
            ]
    it "a test case at a line: a test, never a group (a class or describe line is none)" $ do
      py <- shippedLanguage "python" []
      hs <- shippedLanguage "haskell" []
      map (testCaseAtIn py Nothing pyFile) [1, 5, 8, 9, 10, 11, 15, 17, 99]
        `shouldBe` [Nothing, Nothing, Just "test_closes", Just "test_closes", Just "test_closes", Just "test_closes", Nothing, Just "TestSeats::test_two", Nothing]
      map (testCaseAtIn hs Nothing hsFile) [2, 3, 5] `shouldBe` [Nothing, Just "Olifant.Plans::parses an empty plan", Just "Olifant.Plans::parses an empty plan"]
    it "a test case's first line by its name (decorators included)" $ do
      py <- shippedLanguage "python" []
      map (testCaseLineIn py Nothing pyFile) ["test_closes", "TestSeats::test_two", "make_table"] `shouldBe` [Just 8, Just 16, Nothing]
    it "a test id's case keeps its class; a long file's last test is found at once" $ do
      py <- shippedLanguage "python" []
      map caseOfId ["src/a__test.py::TestSeats::test_two", "src/a__test.py::test_closes"] `shouldBe` ["TestSeats::test_two", "test_closes"]
      testCaseLineIn py Nothing pyFile (caseOfId "src/a__test.py::TestSeats::test_two") `shouldBe` Just 16
      let long = T.unlines (concat [["def test_" <> T.pack (show i) <> "() -> None:", "    x = 1", "    assert x == 1", "", ""] | i <- [1 .. 400 :: Int]])
      found <- timeout 3000000 (evaluate (testCaseLineIn py Nothing long "test_400"))
      found `shouldBe` Just (Just 1996)
    it "a test's source by its short name: from its own line (no decorators), trailing blanks dropped" $ do
      py <- shippedLanguage "python" []
      hs <- shippedLanguage "haskell" []
      testSourceIn py Nothing "test_closes" pyFile `shouldBe` Just "def test_closes(n: int) -> None:\n    table = make_table()\n\n    assert table.handle(n) == 1"
      testSourceIn py Nothing "test_two" pyFile `shouldBe` Just "    def test_two(self) -> None:\n        assert make_table().handle(2) == 0"
      testSourceIn hs Nothing "parses an empty plan" hsFile `shouldBe` Just "  it \"parses an empty plan\" $ do\n    let ps = parsePlan \"\"\n    length ps `shouldBe` 0"
      testSourceIn py Nothing "test_nothing" pyFile `shouldBe` Nothing
    it "a source's last assertion, by [test] assertion" $ do
      py <- shippedLanguage "python" []
      hs <- shippedLanguage "haskell" []
      lastAssertionIn py pyFile `shouldBe` Just "assert make_table().handle(2) == 0"
      lastAssertionIn hs hsFile `shouldBe` Just "length ps `shouldBe` 0"
    it "a test name's words, past the [test] name template's prefix" $ do
      py <- shippedLanguage "python" []
      hs <- shippedLanguage "haskell" []
      testWordsIn py "test_cancel_booking" `shouldBe` "cancel booking"
      testWordsIn py "cancel_booking" `shouldBe` "cancel booking"
      testWordsIn hs "parses an empty plan" `shouldBe` "parses an empty plan"

-- | The built-in pytest shape conjure read before the rules (milestone 3's
-- golden): the test enclosing LINE, `TestX::test_y` for a method of a test
-- class, a decorator line its def's.
testAt :: Text -> Int -> Maybe Text
testAt text line
  | line < 1 || line > length ls = Nothing
  | otherwise = listToMaybe [qualified d dl | (d, dl) <- candidates, encloses d dl]
  where
    ls = zip [1 :: Int ..] (T.lines text)
    indentOf = T.length . T.takeWhile (== ' ')
    blank = T.null . T.strip
    target = case drop (line - 1) ls of
      (_, l) : rest | "@" `T.isPrefixOf` T.strip l -> maybe line fst (listToMaybe [x | x@(_, l') <- rest, isJust (defName l')])
      _ -> line
    candidates = [(i, l) | (i, l) <- reverse (take target ls), Just n <- [defName l], "test" `T.isPrefixOf` n]
    encloses d dl = all (\(_, l) -> blank l || indentOf l > indentOf dl) [x | x@(i, _) <- ls, i > d, i <= target]
    qualified d dl = maybe name (\c -> c <> "::" <> name) owner
      where
        name = fromMaybe "" (defName dl)
        owner = do
          guard (indentOf dl > 0)
          (_, l) <- listToMaybe [x | x@(i, l) <- reverse ls, i < d, not (blank l), indentOf l < indentOf dl]
          className l
    className l = do
      rest <- T.stripPrefix "class " (T.strip l)
      let n = T.takeWhile (\c -> isAlphaNum c || c == '_') rest
      if T.null n then Nothing else Just n
    defName l = do
      r <- T.stripPrefix "def " (T.strip l) <|> T.stripPrefix "async def " (T.strip l)
      let n = T.takeWhile (\c -> isAlphaNum c || c == '_') r
      if T.null n || not ("(" `T.isPrefixOf` T.drop (T.length n) r) then Nothing else Just n

-- | The shipped python with its test units answered by a script.
providing :: FilePath -> IO Language
providing script = providingWith script []

-- | 'providing', the script given ARGS.
providingWith :: FilePath -> [T.Text] -> IO Language
providingWith script args = do
  here <- getCurrentDirectory
  py <- shippedLanguage "python" []
  let p = Provider (["sh", T.pack ("testdata/providers" </> script)] ++ args) (Just (here </> "src/Olifant"))
  pure py {langTest = (\td -> td {testUnitProvider = Just p}) <$> langTest py}

-- | Every Haskell file under a folder.
haskellSources :: FilePath -> IO [FilePath]
haskellSources dir = do
  entries <- map (dir </>) <$> listDirectory dir
  dirs <- filterM doesDirectoryExist entries
  nested <- concat <$> mapM haskellSources dirs
  pure ([f | f <- entries, f `notElem` dirs, ".hs" `isSuffixOf` f] ++ nested)
