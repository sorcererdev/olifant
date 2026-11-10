{-# LANGUAGE OverloadedStrings #-}
module Olifant.Server.TriageSpec (spec) where

import qualified Data.Text as T
import Olifant.Decide (Answer (..), Confidence (..), Question (..), Shape (..))
import Olifant.Plans (parsePlan)
import Olifant.Plans.Sentences
import Olifant.Server.Triage
import Test.Hspec

one :: T.Text -> Sentence
one t = case sentences (parsePlan t) of
  (s : _) -> s
  [] -> error "no sentence"

spec :: Spec
spec = describe "Olifant.Server.Triage" $ do
  let t k p a = Triage k p a "jev"
  it "routes by kind and the two thresholds" $ do
    route (t "claim" 0.85 0.9) `shouldBe` Verify
    route (t "claim" 0.85 0.5) `shouldBe` Aside
    route (t "claim" 0.7 0.9) `shouldBe` Aside
    route (t "step" 0.9 0.9) `shouldBe` Progress
    route (t "question" 0.99 0) `shouldBe` Asked
    route (t "prose" 0.99 0.9) `shouldBe` Aside
    (kindThreshold, aboutThreshold) `shouldBe` (0.80, 0.70)

  it "knows steps and questions by their grammar" $ do
    byGrammar (one "- [ ] Add a field") `shouldBe` Just (Triage "step" 1 1 "grammar")
    byGrammar (one "- ? Why is it slow?") `shouldBe` Just (Triage "question" 1 0 "grammar")
    byGrammar (one "The loader reads it.") `shouldBe` Nothing
    route <$> byGrammar (one "- [x] Done it") `shouldBe` Just Progress

  it "without Jev, checks bullets and leaves paragraphs aside" $ do
    route (noJev (one "- The collector retries.")) `shouldBe` Verify
    route (noJev (one "The collector retries.")) `shouldBe` Aside
    triage Nothing (one "- The collector retries.") `shouldReturn` Triage "claim" 1 1 "grammar"

  it "asks kind and about-code over one shared state, with no unsure" $ do
    let ss = sentences (parsePlan (T.unlines ["## Retries", "- ? Do we retry?", "  - The collector retries."]))
        [k, a] = triageQuestions (ss !! 2)
    map qKind [k, a] `shouldBe` ["sentence-kind", "about-code"]
    qContext k `shouldBe` qContext a
    map fst (qContext k) `shouldBe` ["sentence", "line", "heading", "lead"]
    lookup "lead" (qContext k) `shouldBe` Just "Do we retry?"
    lookup "sentence" (qContext k) `shouldBe` Just "The collector retries."
    qUnsure k `shouldBe` False
    case qShape k of
      Choice os -> map fst os `shouldBe` ["claim", "question", "step", "prose"]
      _ -> expectationFailure "kind is a choice"
    case qShape a of
      Noul (Just _) -> pure ()
      _ -> expectationFailure "about-code is a noul with criteria"
    map (map fst . qContext) (take 1 (triageQuestions (one "Alone."))) `shouldBe` [["sentence", "line", "heading"]]

  it "reads Jev's answers into a triage" $ do
    fromAnswers [(Chosen "claim" [] (Confidence 0.6), [("claim", 0.86), ("step", 0.1)]), (Yes (Confidence 0.9), [])]
      `shouldBe` Triage "claim" 0.86 0.9 "jev"
    fromAnswers [(Chosen "prose" [] (Confidence 0.7), []), (No (Confidence 0.8), [])]
      `shouldBe` Triage "prose" 0.7 (1 - 0.8) "jev"
    trKind (fromAnswers [(Unsure, []), (Unsure, [])]) `shouldBe` "prose"

  describe "joins" $ do
    it "asks whether the second line continues the first, as a noul with criteria and no unsure" $ do
      let q = joinQuestion "We would have to" "redeploy everything."
      qKind q `shouldBe` "line-continues"
      qContext q `shouldBe` [("first line", "We would have to"), ("second line", "redeploy everything.")]
      qUnsure q `shouldBe` False
      case qShape q of
        Noul (Just _) -> pure ()
        _ -> expectationFailure "a noul with criteria"
      joinThreshold `shouldBe` 0.70

    it "joins at p >= 0.70 and not below" $ do
      joinsAnswer (Yes (Confidence 0.7), []) `shouldBe` True
      joinsAnswer (Yes (Confidence 0.69), []) `shouldBe` False
      joinsAnswer (No (Confidence 0.9), []) `shouldBe` False
      joinsAnswer (Unsure, []) `shouldBe` False

    it "with no Jev, decides nothing" $
      decideJoins Nothing [(0, "a", "b")] `shouldReturn` []
