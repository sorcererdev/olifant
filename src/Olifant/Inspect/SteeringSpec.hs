module Olifant.Inspect.SteeringSpec (spec) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Inspect.Steering
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Inspect.Steering" $ do
  it "reads the 1a file" $ do
    st <- steering <$> TIO.readFile "src/Olifant/Inspect/testdata/bistro-inspect.md"
    length (stRules st) `shouldBe` 13
    fst <$> take 1 (stRules st) `shouldBe` ["Commands and queries go through ports (must)"]
    stNitpick st `shouldBe` Nothing
    fmap (T.take 33) (ruleText st "Commands and queries go through ports (must)") `shouldBe` Just "A command or query reaches stored"
  it "no Rules section: no rules, and the whole text kept" $ do
    let src = "# Notes\n\nKeep it simple.\n"
        st = steering src
    stRules st `shouldBe` []
    stText st `shouldBe` src
    rulesSection st `shouldBe` ""
  it "duplicate rule: the first wins" $
    ruleText (steering "## Rules\n### A\none\n### A\ntwo\n") "A" `shouldBe` Just "one"
  it "nested headings are rules up to the next ## section" $ do
    let st = steering "## Rules\n### A\none\n#### B\ntwo\n## After\nthree\n"
    map fst (stRules st) `shouldBe` ["A", "B"]
    ruleText st "B" `shouldBe` Just "two"
    rulesSection st `shouldBe` "## Rules\n### A\none\n#### B\ntwo"
  it "nitpick sections" $ do
    let st = steering "## A nitpick is\nonly typos\n\n## Worth saying is\nanything that breaks\n"
    stNitpick st `shouldBe` Just "only typos"
    stWorth st `shouldBe` Just "anything that breaks"
  it "noSteering is empty" $
    (stText noSteering, stRules noSteering, steeringPath) `shouldBe` ("", [], ".olifant/inspect.md")
