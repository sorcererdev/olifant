module Olifant.Language.TestUnitsSpec (spec) where

import Control.Monad (filterM, forM_)
import Data.List (isSuffixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Language (UnitRule (..), shippedLanguage)
import Olifant.Language.TestUnits
import Olifant.Server.Tests (testAt, testAtIn, testLineIn)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec

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
      testLineIn hs t "Olifant.Plans.parses_x_twice" `shouldBe` Just 2
      testAtIn hs t 3 `shouldBe` Just "Olifant.Plans::parses \"x\" twice"
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
        let diffs = [(f, i, testAtIn py text i, testAt text i) | i <- [0 .. length (T.lines text) + 1], testAtIn py text i /= testAt text i]
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
      forM_ [5 .. 14] $ \i -> (i, testAtIn py t i) `shouldBe` (i, testAt t i)
      map (testAtIn py t) [5, 8, 13] `shouldBe` [Just "TestSeats::test_two", Just "TestSeats::test_two", Just "test_later"]
