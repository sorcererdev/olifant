module Olifant.Language.TestUnitsSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language.TestUnits
import Test.Hspec

-- a keyword at the line's start or after a space or `$` (`spec = describe "…" $ do`); the last group is the name
hspecUnit :: [Text]
hspecUnit = ["(^|[[:space:]$])(describe|context|it|specify|prop)[[:space:]]+\"([^\"]*)\""]

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

spec :: Spec
spec = do
  it "names the test at a line by its describe path" $ do
    testAtBy hspecUnit specText 5 `shouldBe` Just "Olifant.Plans::(a) reads a step"
    testAtBy hspecUnit specText 7 `shouldBe` Just "Olifant.Plans::parsePlan::x/y, z"
    testAtBy hspecUnit specText 1 `shouldBe` Nothing
  it "finds a describe by the anchor's dotted name" $ do
    testLineBy hspecUnit specText "Olifant.Plans" `shouldBe` Just 3
    testLineBy hspecUnit specText "Olifant.Plans.parsePlan" `shouldBe` Just 6
    testLineBy hspecUnit specText "nope" `shouldBe` Nothing
  it "units: paths and 1-based inclusive lines" $
    map (\u -> (tuPath u, tuLines u)) (testUnitsIn hspecUnit specText)
      `shouldBe` [(["Olifant.Plans"], (3, 7)), (["Olifant.Plans", "(a) reads a step"], (4, 5)), (["Olifant.Plans", "parsePlan"], (6, 7)), (["Olifant.Plans", "parsePlan", "x/y, z"], (7, 7))]
