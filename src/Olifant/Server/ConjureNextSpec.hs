module Olifant.Server.ConjureNextSpec (spec) where

import Data.Map.Strict qualified as M
import Olifant.Decide
import Olifant.Generation.Steer
import Olifant.Language
import Olifant.Model (Escalate (..), Rung (..))
import Olifant.Server.ConjureNext
import Test.Hspec

spec :: Spec
spec = describe "conjure-next (Olifant.Server.ConjureNext)" $ do
  let run = TestRun ["t.py::test_b"] [("t.py::test_a", Failure (Just 6) "assert 0 == 1\nmore" "")] Nothing
      hole = RoundHole "body" "the tests failed:" [("return 0", "w1")] (Just run)
      state = RoundState 1 "qwen" [hole]
      view = roundView Nothing state
      viewB = roundView Nothing (RoundState 1 "qwen" [hole {rhRun = Nothing, rhWhy = "does not parse as block"}])
      nextRung e = Just (Rung "claude" e)
      standing e = Standing 3 50 (nextRung e) Nothing
      keys = ["round", "rung", "attempt", "failing", "passing", "check", "repeat", "rounds_left", "seconds_left", "hint"]
  it "(a) roundView lists failures, counts passes and spots a repeat" $ do
    rvFailing view `shouldBe` [("t.py::test_a", "assert 0 == 1")]
    rvPassing view `shouldBe` 1
    rvCheck view `shouldBe` Nothing
    rvRepeat view `shouldBe` False
    let again = roundView (Just view) state
    rvRepeat again `shouldBe` True
    rvPassingDelta again `shouldBe` Just 0
  it "(b) a round with no verdict carries the check" $ do
    rvCheck viewB `shouldBe` Just "does not parse as block"
    rvFailing viewB `shouldBe` []
    let broken = roundView Nothing (RoundState 1 "qwen" [hole {rhRun = Just (TestRun [] [] (Just "ImportError: x"))}])
    rvCheck broken `shouldBe` Just "ImportError: x"
  it "(c) attemptDiff drops the common lines" $ do
    attemptDiff (Just "a\nb\nc") "a\nx\nc" `shouldBe` "@@ line 2\n- b\n+ x"
    attemptDiff Nothing "a\nb" `shouldBe` "+ a\n+ b"
    attemptDiff (Just "a") "a" `shouldBe` "(the same as the previous attempt)"
  it "(d) undefinedNames reads python and pyright" $ do
    undefinedNames "E   NameError: name 'date' is not defined" `shouldBe` ["date"]
    undefinedNames "src/a.py: \"Booking\" is not defined; src/a.py: \"Booking\" is not defined" `shouldBe` ["Booking"]
    undefinedNames "assert 0 == 1" `shouldBe` []
  it "(e) the question offers again, climb and ask, with ten context keys" $ do
    let q = conjureNextQuestion (standing EscalateAsk) view
    qKind q `shouldBe` "conjure-next"
    map fst (qOptions' q) `shouldBe` ["again", "climb", "ask"]
    map fst (qContext q) `shouldBe` keys
    lookup "hint" (qContext q) `shouldBe` Just "none"
    lookup "repeat" (qContext q) `shouldBe` Just "no"
    lookup "repeat" (qContext (conjureNextQuestion (standing EscalateAsk) (roundView (Just view) state))) `shouldBe` Just "yes"
    map fst (qOptions' (conjureNextQuestion (standing EscalateNever) view)) `shouldBe` ["again", "ask"]
    map fst (qOptions' (conjureNextQuestion (Standing 3 50 Nothing Nothing) view)) `shouldBe` ["again", "ask"]
  it "(f) spent pauses when rounds or seconds run out" $ do
    spent (Standing 0 50 Nothing Nothing) `shouldBe` Just (NextPause "budget spent" [OfferContinue, OfferStop])
    spent (Standing 3 (-1) (Just (Rung "claude" EscalateNever)) Nothing)
      `shouldBe` Just (NextPause "budget spent" [OfferContinue, OfferClimb "claude", OfferStop])
    spent (Standing 3 50 Nothing Nothing) `shouldBe` Nothing
  it "(g) nextFrom follows a confident answer and otherwise goes again" $ do
    let st = standing EscalateAsk
        offers = [OfferContinue, OfferClimb "claude", OfferStop]
    nextFrom st view (Chosen "again" [] (Confidence 0.9)) `shouldBe` NextAgain
    nextFrom st view (Chosen "climb" [] (Confidence 0.9)) `shouldBe` NextPause "Jev would climb to claude" offers
    nextFrom (standing EscalateAuto) view (Chosen "climb" [] (Confidence 0.9)) `shouldBe` NextClimb "claude"
    nextFrom st (roundView (Just view) state) (Chosen "ask" [] (Confidence 0.9)) `shouldBe` NextPause "the same failure twice" offers
    nextFrom st view (Chosen "ask" [] (Confidence 0.9)) `shouldBe` NextPause "Jev asks you to look" offers
    nextFrom st view (Chosen "climb" [] (Confidence 0.5)) `shouldBe` NextAgain
    nextFrom st view (Chosen "ask" [] Unknown) `shouldBe` NextAgain
    nextFrom st view Unsure `shouldBe` NextAgain
  it "(h) roundSummary and offerText" $ do
    roundSummary view `shouldBe` "red: test_a \183 assert 0 == 1"
    roundSummary viewB `shouldBe` "red: does not parse as block"
    offerText (OfferClimb "claude") `shouldBe` "climb:claude"
  it "(i) withHint adds the person's words to each hole's last attempt" $
    withHint "use the repository" (Seed 2 (M.singleton "body" [("f1", "w1"), ("f2", "w2")]))
      `shouldBe` Seed 2 (M.singleton "body" [("f1", "w1"), ("f2", "w2\nThe person says: use the repository")])
  where
    qOptions' q = case qShape q of
      Choice os -> os
      _ -> []
