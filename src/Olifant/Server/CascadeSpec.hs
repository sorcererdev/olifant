{-# LANGUAGE OverloadedStrings #-}
module Olifant.Server.CascadeSpec (spec) where

import Data.Text qualified as T
import Olifant.App (Project (..))
import Olifant.Catalog (Catalog (..), PlansConfig (..), defaultPlans)
import Olifant.Decide (Answer (..), Confidence (..), Question (..), Shape (..))
import Olifant.Plans (parsePlan)
import Olifant.Plans.Sentences (Sentence (..), sentences)
import Olifant.Server.Cascade
import Olifant.Server.Checker
import Olifant.Server.CheckerSpec (checkerServer)
import Olifant.Server.State (ServerConfig (..), ServerState (..))
import Test.Hspec

sentence :: Sentence
sentence = case sentences (parsePlan "Foo.bar returns one.\n") of
  s : _ -> s
  [] -> error "no sentence"

confirmed :: Judgement -> T.Text -> [Citation] -> [Source] -> Confirmed
confirmed j why ev src = Confirmed j why ev Nothing Nothing [] Nothing Nothing src

spec :: Spec
spec = describe "Olifant.Server.Cascade" $ do
  it "is the default check_model (sonnet is an option); haiku first, sonnet on doubt, at 0.60" $ do
    isCascade defaultPlans `shouldBe` True
    isCascade defaultPlans {pcCheckModel = "sonnet"} `shouldBe` False
    (cascadeFirst, cascadeStrong, cascadeThreshold) `shouldBe` ("haiku", "sonnet", 0.60)

  it "escalates a failed citation, a cannot_tell, or any guard under the threshold" $ do
    let ok = [("why-fits", 0.9), ("evidence-fits", 0.9), ("verdict-follows", 0.9)]
        right = confirmed JRight "it returns 1" [] []
    escalation 0.6 right ok `shouldBe` Nothing
    escalation 0.6 right [("why-fits", 0.9), ("evidence-fits", 0.55), ("verdict-follows", 0.59)] `shouldBe` Just "evidence-fits+verdict-follows"
    escalation 0.6 right [("why-fits", 0.6), ("evidence-fits", 0.6), ("verdict-follows", 0.6)] `shouldBe` Nothing
    escalation 0.6 (confirmed JCannotTell "cited code not found" [] []) ok `shouldBe` Just "citation"
    escalation 0.6 (confirmed JCannotTell "the code does not say" [] []) ok `shouldBe` Just "cannot_tell"

  it "reads each guard's yes; an unsure or missing guard is 0" $ do
    guardScores [Yes (Confidence 0.8), No (Confidence 0.7), Unsure] `shouldBe` [("why-fits", 0.8), ("evidence-fits", 0.30000000000000004), ("verdict-follows", 0)]
    map snd (guardScores []) `shouldBe` [0, 0, 0]

  it "asks three Nouls in one state: the sentence, the check, the verdict's meaning, the reason, the evidence" $ do
    let c = (confirmed JAnswered "the function returns it" [] []) {cfAnswer = Just "One."}
        qs = guardQuestions (CheckAsk CheckAnswer False []) sentence c "src/a.py:Foo.bar\n..."
    map qKind qs `shouldBe` ["why-fits", "evidence-fits", "verdict-follows"]
    map qUnsure qs `shouldBe` [False, False, False]
    [() | Question {qShape = Noul (Just _)} <- qs] `shouldBe` [(), (), ()]
    let ctx = concatMap qContext (take 1 qs)
    all ((== ctx) . qContext) qs `shouldBe` True
    lookup "sentence" ctx `shouldBe` Just "Foo.bar returns one."
    lookup "check" ctx `shouldBe` Just "a question answered from the project's code"
    fmap (T.isPrefixOf "answered:") (lookup "verdict" ctx) `shouldBe` Just True
    lookup "why" ctx `shouldBe` Just "Answer: One. Reason: the function returns it"
    lookup "evidence" ctx `shouldBe` Just "src/a.py:Foo.bar\n..."

  it "a web check's evidence-fits asks about the pages, not code" $ do
    let c = confirmed JRight "the docs say so" [] [Source "https://docs.example.org/5.1/" "Release notes"]
        ask = CheckAsk CheckClaim True []
        second qs = case qs of
          _ : q : _ -> q
          _ -> error "no evidence-fits"
        ev = second (guardQuestions ask sentence c "Release notes <https://docs.example.org/5.1/>")
        evCode = second (guardQuestions (CheckAsk CheckClaim False []) sentence c "")
    qInstructions ev `shouldSatisfy` T.isInfixOf "pages it cites"
    qInstructions ev `shouldSatisfy` T.isInfixOf "version"
    qInstructions evCode `shouldSatisfy` T.isInfixOf "the code it cites"
    lookup "evidence" (qContext evCode) `shouldBe` Just "(nothing cited)"
    lookup "check" (qContext ev) `shouldBe` Just "a statement checked against public web sources (the checker saw no code)"

  it "shows a web check its pages and a code check the lines around each cited line" $ do
    (s, proj) <- checkerServer
    let src = [Source "https://docs.example.org/5.1/" "Release notes"]
    web <- guardEvidence s proj (CheckAsk CheckClaim True []) (confirmed JRight "w" [] src)
    web `shouldBe` "Release notes <https://docs.example.org/5.1/>"
    code <- guardEvidence s proj (CheckAsk CheckClaim False []) (confirmed JRight "w" [Citation "src/a.py" Nothing (Just 2), Citation "src/nope.py" Nothing Nothing] [])
    code `shouldBe` "src/a.py line 2\nclass Foo:\n    def bar(self):\n        return 1\n"

  it "runs sonnet alone with no decide model to guard haiku: the model a check runs on and is priced as" $ do
    (s, proj) <- checkerServer
    let jev = proj {prCatalog = (prCatalog proj) {catalogDecideModel = Just "jev"}}
        recorded = s {ssConfig = (ssConfig s) {scDecideAnswers = Just "/r1/answers.json"}}
    checkModelIn s proj defaultPlans `shouldBe` "sonnet"
    checkModelIn recorded jev defaultPlans `shouldBe` "cascade"
    checkModelIn recorded jev defaultPlans {pcCheckModel = "haiku"} `shouldBe` "haiku"
    checkModelIn s proj defaultPlans {pcCheckModel = "opus"} `shouldBe` "opus"

  it "keeps haiku's citation failure when the code it cites is in a language olifant cannot read (sonnet would end there too)" $ do
    (s, proj) <- checkerServer
    let failed cs = confirmed JCannotTell "cited code not found" cs []
        js = Citation "static/app.js" (Just "askAboutUnsaved") (Just 1)
    -- a JavaScript unit (no language), alone or beside a citation that confirms
    citesOnlyUnreadable s proj defaultPlans (failed [js]) `shouldReturn` True
    citesOnlyUnreadable s proj defaultPlans (failed [js, Citation "src/a.py" (Just "Foo") Nothing]) `shouldReturn` True
    -- haiku's own miss: a Python unit that is not there, a file that is not there, beside or alone
    citesOnlyUnreadable s proj defaultPlans (failed [js, Citation "src/a.py" (Just "Nope") Nothing]) `shouldReturn` False
    citesOnlyUnreadable s proj defaultPlans (failed [Citation "static/gone.js" (Just "f") Nothing]) `shouldReturn` False
    citesOnlyUnreadable s proj defaultPlans (failed [Citation "src/a.py" (Just "Nope") Nothing]) `shouldReturn` False
    -- not a citation failure
    citesOnlyUnreadable s proj defaultPlans (confirmed JCannotTell "the code does not say" [js] []) `shouldReturn` False

  it "shows evidence-fits the lines around a cited line deep in a unit, not the unit's header" $ do
    (s, proj) <- checkerServer
    code <- guardEvidence s proj (CheckAsk CheckClaim False []) (confirmed JRight "w" [Citation "src/models.py" (Just "Comparison") (Just 25)] [])
    T.lines code `shouldSatisfy` (\ls -> take 1 ls == ["src/models.py:Comparison line 25"])
    code `shouldSatisfy` T.isInfixOf "field_25 ="
    code `shouldSatisfy` T.isInfixOf "field_21 ="
    code `shouldSatisfy` (not . T.isInfixOf "class Comparison:")
    -- the label, then lines 21 to the file's end (30)
    length (T.lines code) `shouldBe` 11
