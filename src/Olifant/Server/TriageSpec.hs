{-# LANGUAGE OverloadedStrings #-}
module Olifant.Server.TriageSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Text as T
import Data.IORef (atomicModifyIORef', modifyIORef, newIORef, readIORef)
import Olifant.Decide (Answer (..), Confidence (..), Decider (..), Question (..), Shape (..))
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
  let t k p a = Triage k p a "jev" (Just 0) (Just 0)
      q w a pub = Triage "question" 0.9 a "jev" (Just w) (Just pub)
  it "routes by kind and the two thresholds" $ do
    route False (t "claim" 0.85 0.9) `shouldBe` Verify
    route False (t "claim" 0.85 0.5) `shouldBe` Aside
    route False (t "claim" 0.7 0.9) `shouldBe` Aside
    route False (t "step" 0.9 0.9) `shouldBe` Progress
    route False (t "question" 0.99 0) `shouldBe` Asked
    route False (t "prose" 0.99 0.9) `shouldBe` Aside
    (kindThreshold, aboutThreshold) `shouldBe` (0.80, 0.70)

  it "routes after 2b: one example per row, web on and off" $ do
    -- a claim the code settles is checked, web or not
    route False (t "claim" 0.9 0.9) `shouldBe` Verify
    route True (t "claim" 0.9 0.9) `shouldBe` Verify
    -- a claim only public sources settle: checked with web, aside without
    let public = (t "claim" 0.9 0.3) {trAboutPublic = Just 0.9}
    route True public `shouldBe` Verify
    route False public `shouldBe` Aside
    -- a question that wants an answer the code settles
    route False (q 0.9 0.9 0) `shouldBe` Answer
    -- a question only public sources settle: answered with web, asked without
    route True (q 0.9 0.3 0.9) `shouldBe` Answer
    route False (q 0.9 0.3 0.9) `shouldBe` Asked
    -- a question nobody wants answered
    route True (q 0.3 0.9 0.9) `shouldBe` Asked
    -- a question never asked wants-answer (a 2a entry) waits for its re-triage
    route True (q 0.9 0.9 0) {trWantsAnswer = Nothing} `shouldBe` Asked
    route False (t "step" 0.9 0.9) `shouldBe` Progress
    -- a step is about this code: public sources never make it one to check
    route True ((t "step" 0.9 0.3) {trAboutPublic = Just 0.9}) `shouldBe` Aside
    routeName Answer `shouldBe` "answer"

  it "a question needs less about-code than a claim or a step (user ruling 2026-11-14)" $ do
    questionAboutThreshold `shouldBe` 0.50
    -- R1 ("Do we have raw vote totals from previous years stored?"), about-code 0.62
    route False (q 0.9 0.62 0) `shouldBe` Answer
    route False (q 0.9 0.5 0) `shouldBe` Answer
    route False (q 0.9 0.49 0) `shouldBe` Asked
    -- claims and steps keep 0.70
    route False (t "claim" 0.9 0.62) `shouldBe` Aside
    route False (t "step" 0.9 0.62) `shouldBe` Aside

  it "settleable and useWeb: the code, or web with about-public" $ do
    let public = (t "claim" 0.9 0.3) {trAboutPublic = Just 0.9}
    (settleable False public, settleable True public, useWeb False public, useWeb True public) `shouldBe` (False, True, False, True)
    (settleable False (t "claim" 0.9 0.9), useWeb True (t "claim" 0.9 0.9)) `shouldBe` (True, False)
    useWeb True (t "claim" 0.9 0.9) {trAboutPublic = Nothing} `shouldBe` False

  it "a sentence the code and public sources both settle takes the code check (user ruling 2026-11-14)" $ do
    let both = (t "claim" 0.9 0.9) {trAboutPublic = Just 0.9}
    useWeb True both `shouldBe` False
    route True both `shouldBe` Verify
    useWeb True ((t "claim" 0.9 0.6) {trAboutPublic = Just 0.9}) `shouldBe` True
    -- a question is the code's at its own threshold
    useWeb True (q 0.9 0.55 0.9) `shouldBe` False
    useWeb True (q 0.9 0.45 0.9) `shouldBe` True

  it "a claim covers what a named library or service does (user ruling 2026-11-14)" $ do
    let Just claimText = lookup "claim" kindOptions
        Just proseText = lookup "prose" kindOptions
    claimText `shouldSatisfy` T.isInfixOf "what a named library, framework or service does or offers"
    claimText `shouldSatisfy` (not . T.isInfixOf "outside services")
    proseText `shouldSatisfy` T.isInfixOf "Does not cover: statements about how the code is now or what a named library or service does"

  it "knows steps and questions by their grammar" $ do
    byGrammar (one "- [ ] Add a field") `shouldBe` Just (Triage "step" 1 1 "grammar" Nothing Nothing)
    byGrammar (one "- ? Why is it slow?") `shouldBe` Just (Triage "question" 1 1 "grammar" (Just 1) Nothing)
    byGrammar (one "The loader reads it.") `shouldBe` Nothing
    route False <$> byGrammar (one "- [x] Done it") `shouldBe` Just Progress
    -- with no Jev, a `- ? ` line is answered from the code (the degraded mode)
    route False <$> byGrammar (one "- ? Why is it slow?") `shouldBe` Just Answer

  it "without Jev, checks bullets and leaves paragraphs aside" $ do
    route False (noJev (one "- The collector retries.")) `shouldBe` Verify
    route False (noJev (one "The collector retries.")) `shouldBe` Aside
    triage False Nothing (one "- The collector retries.") `shouldReturn` Triage "claim" 1 1 "grammar" Nothing Nothing

  it "asks kind, about-code and wants-answer over one shared state, and about-public with web" $ do
    let ss = sentences (parsePlan (T.unlines ["## Retries", "- ? Do we retry?", "  - The collector retries."]))
        [k, a, w] = triageQuestions False (ss !! 2)
        withWeb = triageQuestions True (ss !! 2)
    map qKind [k, a, w] `shouldBe` ["sentence-kind", "about-code", "wants-answer"]
    map qKind withWeb `shouldBe` ["sentence-kind", "about-code", "wants-answer", "about-public"]
    all ((== qContext k) . qContext) withWeb `shouldBe` True
    map fst (qContext k) `shouldBe` ["sentence", "line", "heading", "lead"]
    lookup "lead" (qContext k) `shouldBe` Just "Do we retry?"
    lookup "sentence" (qContext k) `shouldBe` Just "The collector retries."
    qUnsure k `shouldBe` False
    map qUnsure (drop 2 withWeb) `shouldBe` [False, False]
    case qShape k of
      Choice os -> map fst os `shouldBe` ["claim", "question", "step", "prose"]
      _ -> expectationFailure "kind is a choice"
    forM_ (drop 1 withWeb) $ \x -> case qShape x of
      Noul (Just _) -> pure ()
      _ -> expectationFailure (T.unpack (qKind x) <> " is a noul with criteria")
    map (map fst . qContext) (take 1 (triageQuestions False (one "Alone."))) `shouldBe` [["sentence", "line", "heading"]]
    -- the sentence-kind and about-code texts are 2a's (the recorded keys hold)
    take 2 (triageQuestions True (one "Alone.")) `shouldBe` take 2 (triageQuestions False (one "Alone."))
    map qKind (gradeQuestions False (one "- ? Why?")) `shouldBe` ["about-code"]
    map qKind (gradeQuestions True (one "- ? Why?")) `shouldBe` ["about-code", "about-public"]

  it "reads Jev's answers into a triage, by question kind" $ do
    fromAnswers [("sentence-kind", (Chosen "claim" [] (Confidence 0.6), [("claim", 0.86), ("step", 0.1)])), ("about-code", (Yes (Confidence 0.9), []))]
      `shouldBe` Triage "claim" 0.86 0.9 "jev" Nothing Nothing
    fromAnswers [("sentence-kind", (Chosen "prose" [] (Confidence 0.7), [])), ("about-code", (No (Confidence 0.8), []))]
      `shouldBe` Triage "prose" 0.7 (1 - 0.8) "jev" Nothing Nothing
    -- by kind, not by position
    fromAnswers [("about-public", (Yes (Confidence 0.8), [])), ("wants-answer", (No (Confidence 0.9), [])), ("about-code", (Yes (Confidence 0.6), [])), ("sentence-kind", (Chosen "question" [] (Confidence 0.9), []))]
      `shouldBe` Triage "question" 0.9 0.6 "jev" (Just (1 - 0.9)) (Just 0.8)
    trKind (fromAnswers [("sentence-kind", (Unsure, [])), ("about-code", (Unsure, []))]) `shouldBe` "prose"
    fromAnswers [] `shouldBe` Triage "prose" 0 0 "jev" Nothing Nothing

  it "re-triages an entry missing a Noul it needs" $ do
    let twoA = Triage "claim" 0.9 0.9 "jev" Nothing Nothing
        full = Triage "claim" 0.9 0.9 "jev" (Just 0.1) (Just 0.2)
    needsRetriage False twoA `shouldBe` True
    needsRetriage True full `shouldBe` False
    needsRetriage False full `shouldBe` False
    -- with web off, a triage lacking only about-public needs nothing
    needsRetriage False full {trAboutPublic = Nothing} `shouldBe` False
    needsRetriage True full {trAboutPublic = Nothing} `shouldBe` True
    -- a 2a `- ? ` line (grammar, about-code 0) is re-triaged; a step never
    needsRetriage False (Triage "question" 1 0 "grammar" Nothing Nothing) `shouldBe` True
    needsRetriage True (Triage "step" 1 1 "grammar" Nothing Nothing) `shouldBe` False

  it "re-triage asks only the missing questions and keeps the kind" $ do
    asked <- newIORef []
    let d = Decider
          { ask = \_ -> pure Unsure, askAll = \_ -> pure []
          , weigh = \_ -> pure (Unsure, [])
          , weighAll = \qs -> do
              modifyIORef asked (++ [map qKind qs])
              pure [(if qKind x == "about-code" then Yes (Confidence 0.8) else Yes (Confidence 0.9), []) | x <- qs] }
        prose = one "The collector retries."
        mark = one "- ? Do we retry?"
    r1 <- retriage False (Just d) prose (Triage "claim" 0.85 0.75 "jev" Nothing Nothing)
    r1 `shouldBe` Triage "claim" 0.85 0.75 "jev" (Just 0.9) Nothing
    r2 <- retriage True (Just d) prose r1
    r2 `shouldBe` Triage "claim" 0.85 0.75 "jev" (Just 0.9) (Just 0.9)
    r3 <- retriage True (Just d) prose r2
    r3 `shouldBe` r2
    r4 <- retriage False (Just d) mark (Triage "question" 1 0 "grammar" Nothing Nothing)
    r4 `shouldBe` Triage "question" 1 0.8 "grammar" (Just 1) Nothing
    readIORef asked `shouldReturn` [["wants-answer"], ["about-public"], ["about-code"]]
    -- no decider: a Jev entry is unchanged; a `- ? ` line takes the no-Jev triage
    retriage False Nothing prose (Triage "claim" 0.85 0.75 "jev" Nothing Nothing) `shouldReturn` Triage "claim" 0.85 0.75 "jev" Nothing Nothing
    retriage False Nothing mark (Triage "question" 1 0 "grammar" Nothing Nothing) `shouldReturn` Triage "question" 1 1 "grammar" (Just 1) Nothing

  it "a `- ? ` line asks Jev about-code (and about-public with web), not its kind" $ do
    asked <- newIORef []
    let d = Decider
          { ask = \_ -> pure Unsure, askAll = \_ -> pure []
          , weigh = \_ -> pure (Unsure, [])
          , weighAll = \qs -> do
              modifyIORef asked (++ [map qKind qs])
              pure [(No (Confidence 0.9), []) | _ <- qs] }
    r <- triage True (Just d) (one "- ? Who should review this?")
    r `shouldBe` Triage "question" 1 (1 - 0.9) "grammar" (Just 1) (Just (1 - 0.9))
    route True r `shouldBe` Asked
    readIORef asked `shouldReturn` [["about-code", "about-public"]]
    triage True Nothing (one "- ? Who should review this?") `shouldReturn` Triage "question" 1 1 "grammar" (Just 1) Nothing

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
      decideJoins Nothing (pure True) [(0, "a", "b")] `shouldReturn` []

    it "asks each pair in its own request (Jev's batch is one state), keeps no unsure answer, and stops when told" $ do
      calls <- newIORef (0 :: Int)
      let fake = Decider undefined undefined undefined $ \qs -> do
            modifyIORef calls (+ 1)
            pure [if lookup "first line" (qContext q) == Just "c" then (Unsure, []) else (Yes (Confidence 0.9), []) | q <- qs]
      decideJoins (Just fake) (pure True) [(0, "a", "b"), (1, "b", "x"), (2, "c", "d")]
        `shouldReturn` [("a ⏎ b", True), ("b ⏎ x", True)]
      readIORef calls `shouldReturn` 3
      left <- newIORef (1 :: Int)
      decideJoins (Just fake) (atomicModifyIORef' left (\n -> (n - 1, n > 0))) [(0, "a", "b"), (1, "b", "x")]
        `shouldReturn` [("a ⏎ b", True)]
