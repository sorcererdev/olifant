{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Olifant.Server.JournalSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian, utc)
import Olifant.LspSpec (plainPorts)
import Olifant.Plans (Kind (..), Line (..), TestState (..), parsePlan)
import Olifant.Server.Journal
import Olifant.Server.LoopTypes (Fact (..))
import Olifant.Server.ReviewReadings (applyFactsText)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

at :: Int -> Int -> Int -> UTCTime
at d h m = UTCTime (fromGregorian 2026 10 d) (fromIntegral (h * 3600 + m * 60))

plan :: Text
plan = T.intercalate "\n"
  [ "# T"
  , ""
  , "- [ ] tidy the helper @a.py:helper"
  , "  - test: helper works @test_a.py:test_helper ○"
  , "- [ ] count seats @a.py:count"
  , ""
  ]

ev :: Text -> Text -> Int -> Text -> Event
ev key step row words' = Event {evKey = key, evStep = step, evRow = row, evAt = at 25 21 31, evWords = words', evErrand = Nothing}

landed :: Text -> [Entry] -> Text
landed text es = let (facts, _, _) = pendingFacts (parsePlan text) es in applyFactsText facts text

spec :: Spec
spec = describe "Olifant.Server.Journal (the plan's record of what happened to each step)" $ do
  it "writes a dated line in words, a one-digit day without padding" $ do
    journalLine utc (ev "k" "s" 0 "do (sonnet) · cancelled") `shouldBe` "- 25 Oct 21:31 · do (sonnet) · cancelled"
    journalLine utc (ev "k" "s" 0 "x") {evAt = at 5 9 4} `shouldBe` "- 5 Oct 09:04 · x"
  it "parses as a journal line, whose @ words are not an anchor" $ do
    let [l] = parsePlan "  - 25 Oct 21:31 · do (sonnet) · error: no @a.py:helper here"
    lnKind l `shouldBe` Journal
    lnAnchor l `shouldBe` Nothing
    map lnKind (parsePlan "  - 25 Oct · not a time\n  - 25 Oct 21:31 no dot") `shouldBe` [Claim, Claim]
  it "records an event once: a second with a seen key is dropped" $ withSystemTempDirectory "journal" $ \root -> do
    let ports = plainPorts
    new1 <- record ports utc root "plans/t.md" [ev "do|1" "- [ ] tidy the helper @a.py:helper" 2 "do (sonnet) · cancelled"]
    new2 <- record ports utc root "plans/t.md" [ev "do|1" "- [ ] tidy the helper @a.py:helper" 2 "do (sonnet) · cancelled", ev "do|2" "- [ ] count seats @a.py:count" 4 "do (haiku) · timed out"]
    map (evKey . enEvent) new1 `shouldBe` ["do|1"]
    map (evKey . enEvent) new2 `shouldBe` ["do|2"]
    map (evKey . enEvent) <$> pendingEntries ports root "plans/t.md" `shouldReturn` ["do|1", "do|2"]
  it "lands pending lines after the step's last child, in order, and never again once landed" $ withSystemTempDirectory "journal" $ \root -> do
    let ports = plainPorts
    _ <- record ports utc root "plans/t.md"
      [ ev "a" "- [ ] tidy the helper @a.py:helper" 2 "first", ev "b" "- [ ] tidy the helper @a.py:helper" 2 "second"
      , ev "c" "- [ ] count seats @a.py:count" 4 "third" ]
    es <- pendingEntries ports root "plans/t.md"
    let (facts, keys, lost) = pendingFacts (parsePlan plan) es
    facts `shouldBe` [Insert 3 ["  - 25 Oct 21:31 · first", "  - 25 Oct 21:31 · second"], Insert 4 ["  - 25 Oct 21:31 · third"]]
    (keys, lost) `shouldBe` (["a", "b", "c"], [])
    markLanded ports root "plans/t.md" keys
    pendingEntries ports root "plans/t.md" `shouldReturn` []
    -- the user deletes a landed line; the same event again writes nothing
    record ports utc root "plans/t.md" [ev "a" "- [ ] tidy the helper @a.py:helper" 2 "first"] `shouldReturn` []
  it "finds the step by its words when the plan moved it, and loses a line whose step is gone" $ do
    let moved = "# T\n\nnew words\n\n" <> T.drop 5 plan
        es = [Entry (ev "a" "- [x] tidy the helper @a.py:helper ✓ abc1234" 2 "w") Pending "- 25 Oct 21:31 · w", Entry (ev "z" "- [ ] gone step @a.py:zz" 9 "w") Pending "- 25 Oct 21:31 · w"]
        (facts, keys, lost) = pendingFacts (parsePlan moved) es
    facts `shouldBe` [Insert 5 ["  - 25 Oct 21:31 · w"]]
    (keys, lost) `shouldBe` (["a"], ["z"])
    landed plan [head es] `shouldBe` T.intercalate "\n" (take 4 (T.splitOn "\n" plan) ++ ["  - 25 Oct 21:31 · w"] ++ drop 4 (T.splitOn "\n" plan))
  it "lands under an unanchored step that keep has since anchored" $ do
    let anchored = T.replace "- [ ] count seats @a.py:count" "- [ ] count seats   @a.py:count" plan
        es = [Entry (ev "a" "- [ ] count seats" 4 "w") Pending "- 25 Oct 21:31 · w"]
        (facts, _, lost) = pendingFacts (parsePlan anchored) es
    (facts, lost) `shouldBe` ([Insert 4 ["  - 25 Oct 21:31 · w"]], [])
  it "merges a journal insert into another fact's insert at the same row" $
    withJournal [Replace 2 "x", Insert 3 ["  - ✗ helper red again"]] [Insert 3 ["  - j"], Insert 4 ["  - k"]]
      `shouldBe` [Replace 2 "x", Insert 3 ["  - ✗ helper red again", "  - j"], Insert 4 ["  - k"]]
  it "journals tests once per fingerprint: a tick, a first red, an untick" $ do
    let ls = parsePlan plan
        green = [(("test_a.py", "test_helper"), TestGreen)]
        red = [(("test_a.py", "test_helper"), TestRed)]
        done = parsePlan (T.replace "- [ ] tidy" "- [x] tidy" plan)
    map (\e -> (evKey e, evWords e)) (testEvents ls 2 [Replace 2 "- [x] tidy the helper @a.py:helper ✓ abc1234"] green (Just "abc1234") (at 25 21 31))
      `shouldBe` [("tests|tidy the helper|abc1234|green", "ticked: tests green")]
    map (\e -> (evKey e, evWords e)) (testEvents ls 2 [] red (Just "abc1234") (at 25 21 31))
      `shouldBe` [("tests|tidy the helper|abc1234|red", "tests red: test_helper")]
    map (\e -> (evKey e, evWords e)) (testEvents done 2 [Replace 2 "- [ ] tidy the helper @a.py:helper"] red (Just "abc1234") (at 25 21 31))
      `shouldBe` [("tests|tidy the helper|abc1234|red", "unticked: tests red again")]
    map (\e -> (evKey e, evWords e)) (testEvents done 2 [] green (Just "abc1234") (at 25 21 31))
      `shouldBe` [("tests|tidy the helper|abc1234|green", "tests green")]
  it "says how a do ended, cut to 80 characters" $ do
    let words' end = evWords (doEvent "s1" "- [ ] tidy the helper @a.py:helper" 2 (at 25 21 31) False "sonnet" end)
    words' (DoAnswered "tidied the helper") `shouldBe` "do (sonnet) · answered: tidied the helper"
    words' (DoBlocked "tests could not run") `shouldBe` "do (sonnet) · blocked: tests could not run"
    map words' [DoEmpty, DoError "boom", DoTimeout, DoCancelled]
      `shouldBe` ["do (sonnet) · answered nothing", "do (sonnet) · error: boom", "do (sonnet) · timed out", "do (sonnet) · cancelled"]
    evWords (doEvent "s1" "s" 2 (at 25 21 31) True "sonnet" DoCancelled) `shouldBe` "do again (sonnet) · cancelled"
    T.length (words' (DoError (T.replicate 200 "x"))) `shouldBe` 80
    words' (DoError "two\nlines") `shouldBe` "do (sonnet) · error: two lines"
    evErrand (doEvent "s1" "s" 2 (at 25 21 31) False "sonnet" DoCancelled) `shouldBe` Just "s1"
