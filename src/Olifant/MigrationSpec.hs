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
  it "(a2) reads a registry of lines too (SWITCHES[\"name\"] = True)" $
    readRegistry "SWITCHES: dict[str, bool] = {}\nSWITCHES[\"close-table\"] = True\nSWITCHES[\"seat-party\"] = False\n" `shouldBe` [Switch "close-table" True, Switch "seat-party" False]
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
    bigChanges 15 (NewIn (S.fromList [5 .. 24]) S.empty) [fn] `shouldBe` [(fn, 20)]
    bigChanges 15 (NewIn (S.fromList [5 .. 14]) S.empty) [fn] `shouldBe` []
    bigChanges 15 (NewIn (S.fromList [1 .. 30]) S.empty) [fn] `shouldBe` []
    bigChanges 15 (NoBase "x") [fn] `shouldBe` []
  it "(g) reads or writes: SQL that writes, a commit or a save is a command; reads alone, a query; else unsure" $ do
    readsOrWrites "def f(t):\n    c.execute(\"UPDATE tables SET open = 0 WHERE id = ?\", (t,))\n    c.commit()\n" `shouldBe` Just "command"
    readsOrWrites "def f(d):\n    return db.rows(\"SELECT * FROM bookings WHERE day = ?\", d)\n" `shouldBe` Just "query"
    readsOrWrites "def f(x):\n    return x + 1\n" `shouldBe` Nothing
    readsOrWrites "def f(x):\n    global CURRENT\n    CURRENT = x\n" `shouldBe` Just "command"
  it "(a3) a trailing comment on a registry line is not part of its value (review minor #12)" $
    readRegistry "SWITCHES: dict[str, bool] = {}\nSWITCHES[\"close-table\"] = True  # trusted since 10-07\n\"seat-party\": True,  # inline\n" `shouldBe` [Switch "close-table" True]
  it "(g2) reads or writes: comments are not code; a set's add is not a write (review minor #13)" $ do
    readsOrWrites "def f(x):\n    # update the cache later\n    return x + 1\n" `shouldBe` Nothing
    readsOrWrites "def f(xs):\n    seen = set()\n    seen.add(xs)\n    return seen\n" `shouldBe` Nothing
    readsOrWrites "def f(d):\n    # select the rows\n    db.execute(\"DELETE FROM t WHERE d = ?\", d)\n" `shouldBe` Just "command"
  it "(g3) a function's text is found for an async def too (review minor #13)" $
    functionText "async def fetch(x):\n    return x\n\ndef other():\n    pass\n" "fetch" `shouldBe` Just "async def fetch(x):\n    return x\n\n"
  it "(e) legacy added is named; a marker the registry lacks is listed" $ do
    let r = MigrationReport 213 (Just 212) [] (Just []) ["waitlist"] ["close-table"]
    reportLine r `shouldBe` "+0 extracted, 1 legacy function added (waitlist), legacy 212 → 213 functions; switches: 0 on, 0 off"
    unregistered ["close-table", "x"] [Switch "x" False] `shouldBe` ["close-table"]
