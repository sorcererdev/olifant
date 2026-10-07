module Olifant.MigrationSpec (spec) where

import Data.Text qualified as T
import Olifant.Migration
import Olifant.Newness (NewLines (..))
import Data.Set qualified as S
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Migration" $ do
  let registry = T.unlines ["SWITCHES: dict[str, bool] = {", "    \"close-table\": True,", "    \"seat-party\": False,", "}", "", "def on(name: str) -> bool:", "    return SWITCHES.get(name, False)"]
  it "(a) reads the registry: each switch and whether it is on" $
    readRegistry registry `shouldBe` [Switch "close-table" True, Switch "seat-party" False]
  it "(b) finds switch markers in code" $
    switchMarkers "def f():\n    # olifant:switch close-table → booking/commands/close_table\n    pass\n" `shouldBe` ["close-table"]
  it "(c) the PR line: extracted since the base, legacy added, functions, switches" $ do
    let r = MigrationReport 211 (Just 212) [Switch "close-table" True, Switch "seat-party" False] (Just ["seat-party"]) [] []
    reportLine r `shouldBe` "+1 extracted (close-table), 0 legacy functions added, legacy 212 → 211 functions; switches: 1 on, 1 off"
  it "(d) without a base, it says so" $ do
    let r = MigrationReport 211 Nothing [Switch "a" True] Nothing [] []
    reportLine r `shouldBe` "legacy 211 functions (no base); switches: 1 on, 0 off"
  it "(f) a big change: a function not wholly new with at least N new lines (decision 1)" $ do
    let fn = ((1, 1), (30, 1))
    bigChanges 15 (NewIn (S.fromList [5 .. 24])) [fn] `shouldBe` [(fn, 20)]
    bigChanges 15 (NewIn (S.fromList [5 .. 14])) [fn] `shouldBe` []
    bigChanges 15 (NewIn (S.fromList [1 .. 30])) [fn] `shouldBe` []
    bigChanges 15 (NoBase "x") [fn] `shouldBe` []
  it "(e) legacy added is named; a marker the registry lacks is listed" $ do
    let r = MigrationReport 213 (Just 212) [] (Just []) ["waitlist"] ["close-table"]
    reportLine r `shouldBe` "+0 extracted, 1 legacy function added (waitlist), legacy 212 → 213 functions; switches: 0 on, 0 off"
    unregistered ["close-table", "x"] [Switch "x" False] `shouldBe` ["close-table"]
