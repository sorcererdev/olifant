module Olifant.DecideSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import Data.IORef (modifyIORef, newIORef, readIORef)
import Olifant.Decide
import Olifant.Model (Backend (..))
import Olifant.Process (neverCancel)
import Olifant.Process (newCancel)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Decide" $ do
  let commandOrQuery = choice "command-or-query" "Is it a command or a query" [("command", "changes state"), ("query", "reads state")] [("function", "def f(): ...")]
      withField = (choice "service" "Which service" [("booking", ""), ("new service", "")] []) {qFields = [("name", "ident")]}
      equivalence = noul "k" "Same behaviour" []
      matters = score "k" "How much it matters" ["cosmetic", "logic", "boundary"] []
  describe "validated" $ do
    it "(a) keeps an answer that is one of the options, with fields of their types" $ do
      validated commandOrQuery (Chosen "command" [] (Confidence 0.9)) `shouldBe` Chosen "command" [] (Confidence 0.9)
      validated withField (Chosen "new service" [("name", "kitchen")] Unknown) `shouldBe` Chosen "new service" [("name", "kitchen")] Unknown
    it "(b) anything else is unsure: not an option, a field of the wrong type, an unknown field" $ do
      validated commandOrQuery (Chosen "maybe" [] Unknown) `shouldBe` Unsure
      validated withField (Chosen "booking" [("name", "3x")] Unknown) `shouldBe` Unsure
      validated withField (Chosen "booking" [("other", "x")] Unknown) `shouldBe` Unsure
  describe "a chat reply (decodeAnswer)" $ do
    it "(b2) drops fields the question did not ask for, as a chat model adds a rationale" $ do
      decodeAnswer commandOrQuery "{\"choice\": \"command\", \"confidence\": 0.8, \"fields\": {\"rationale\": \"it writes\"}}" `shouldBe` Chosen "command" [] (Confidence 0.8)
      decodeAnswer withField "{\"choice\": \"new service\", \"fields\": {\"name\": \"kitchen\", \"why\": \"x\"}}" `shouldBe` Chosen "new service" [("name", "kitchen")] Unknown
      decodeAnswer withField "{\"choice\": \"new service\", \"fields\": {\"name\": \"3x\"}}" `shouldBe` Unsure
    it "(b3) a question with no fields asks for none" $ do
      decisionPrompt commandOrQuery `shouldSatisfy` (not . T.isInfixOf "fields")
      decisionPrompt withField `shouldSatisfy` T.isInfixOf "\"fields\": {"
  describe "recordedDecider (tests)" $ do
    it "(c) answers by the question's kind; an invalid answer is unsure" $
      withSystemTempDirectory "olifant-decide" $ \dir -> do
        let file = dir </> "answers.json"
        writeFile file "{\"answers\": {\"command-or-query\": \"query\", \"service\": \"elsewhere\"}}"
        (token, _) <- newCancel
        ask (recordedDecider file token) commandOrQuery `shouldReturn` Chosen "query" [] Unknown
        ask (recordedDecider file token) withField `shouldReturn` Unsure
        ask (recordedDecider file token) (choice "other" "" [("a", "")] []) `shouldReturn` Unsure
    it "(c) \"wait\" never answers until the token trips, then is unsure (Review Focus 5)" $
      withSystemTempDirectory "olifant-decide" $ \dir -> do
        let file = dir </> "answers.json"
        writeFile file "wait"
        (token, trip) <- newCancel
        _ <- forkIO (threadDelay 200000 >> trip)
        timeout 5000000 (ask (recordedDecider file token) commandOrQuery) `shouldReturn` Just Unsure
  describe "shapes and confidence" $ do
    it "(a2) a Noul takes a yes or a no, not a choice; a Score takes a level in range" $ do
      validated equivalence (Yes Unknown) `shouldBe` Yes Unknown
      validated commandOrQuery (Yes Unknown) `shouldBe` Unsure
      validated matters (Level 1 1 Unknown) `shouldBe` Level 1 1 Unknown
      validated matters (Level 3 3 Unknown) `shouldBe` Unsure
      validated matters (Level (-1) 0 Unknown) `shouldBe` Unsure
    it "(b) a Noul reads true, and an object may carry a confidence" $ do
      fromValue (Bool True) `shouldBe` Yes Unknown
      fromValue (Bool False) `shouldBe` No Unknown
      decodeAnswer equivalence "{\"choice\": true, \"confidence\": 0.8}" `shouldBe` Yes (Confidence 0.8)
    it "(c) a Score of three levels: 2 is level 2, 5 is unsure" $ do
      decodeAnswer matters "{\"choice\": 2}" `shouldBe` Level 2 2 Unknown
      decodeAnswer matters "{\"choice\": 5}" `shouldBe` Unsure
      validated matters (fromValue (Number 2)) `shouldBe` Level 2 2 Unknown
      validated matters (fromValue (Number 5)) `shouldBe` Unsure
    it "(d) a Noul's prompt offers yes | no | unsure; a Score's lists its numbered levels" $ do
      decisionPrompt equivalence `shouldSatisfy` T.isInfixOf "yes | no | unsure"
      decisionPrompt matters `shouldSatisfy` T.isInfixOf "2: boundary"
    it "(e) askAll of a decider built from ask calls it once per question, in order" $ do
      seen <- newIORef []
      let d = decider (\q -> modifyIORef seen (qKind q :) >> pure Unsure)
      askAll d [choice "one" "" [] [], noul "two" "" []] `shouldReturn` [Unsure, Unsure]
      readIORef seen `shouldReturn` ["two", "one"]
    it "(f) confident is true only for a known confidence at or over the gate" $ do
      confident 0.9 (Chosen "x" [] Unknown) `shouldBe` False
      confident 0.9 (Chosen "x" [] (Confidence 0.95)) `shouldBe` True
      confident 0.9 Unsure `shouldBe` False
    it "(g) recordedDecider reads a bare true for a Noul and a choice with a confidence for a Score" $
      withSystemTempDirectory "olifant-decide" $ \dir -> do
        let file = dir </> "answers.json"
        writeFile file "{\"answers\": {\"k\": true}}"
        (token, _) <- newCancel
        ask (recordedDecider file token) equivalence `shouldReturn` Yes Unknown
        writeFile file "{\"answers\": {\"k\": {\"choice\": 1, \"confidence\": 0.7}}}"
        ask (recordedDecider file token) matters `shouldReturn` Level 1 1 (Confidence 0.7)
  describe "jev" $ do
    let which = choice "which" "Which is being written" [("command", "changes state"), ("query", "reads state")] [("code", "def f(): ...")]
        done = noul "done" "Is it finished" [("code", "def f(): ...")]
        field k = \case
          Object o -> KM.lookup k o
          _ -> Nothing
    it "(b) jevBody shares the context as one state and asks each question by its kind; a choice may be unsure" $ do
      let body = jevBody [which, done]
      (field "state" body >>= field "code") `shouldBe` Just (String "def f(): ...")
      (KM.size <$> (field "questions" body >>= \case Object o -> Just o; _ -> Nothing)) `shouldBe` Just 2
      (field "questions" body >>= field "which" >>= field "criteria" >>= field "unsure") `shouldBe` Just (String "none of these fits")
      (field "questions" body >>= field "done" >>= field "type") `shouldBe` Just (String "noul")
    it "(c) jevAnswers reads a choice with its confidence and a noul's probability" $
      jevAnswers [which, done] "{\"answers\": {\"which\": {\"type\":\"choice\",\"choice\":\"command\",\"probabilities\":{\"command\":0.84,\"query\":0.1,\"unsure\":0.06},\"confidence\":0.84}, \"done\": {\"type\":\"noul\",\"noul\":0.93}}}"
        `shouldBe` [Chosen "command" [] (Confidence 0.84), Yes (Confidence 0.93)]
    it "(c2) jevWeighed keeps a choice's distribution, highest first" $ do
      let next = choice "conjure-next" "" [("again", ""), ("ask", ""), ("port", ""), ("test", "")] []
      jevWeighed [next] "{\"answers\":{\"conjure-next\":{\"type\":\"choice\",\"choice\":\"ask\",\"confidence\":0.49,\"probabilities\":{\"again\":0.29,\"ask\":0.59,\"port\":0.02,\"test\":0.07,\"unsure\":0.03}}}}"
        `shouldBe` [(Chosen "ask" [] (Confidence 0.49), [("ask", 0.59), ("again", 0.29), ("test", 0.07), ("unsure", 0.03), ("port", 0.02)])]
    it "(d) a choice not among the options makes the whole batch unsure" $
      jevAnswers [which, done] "{\"answers\": {\"which\": {\"type\":\"choice\",\"choice\":\"event\",\"confidence\":0.9}, \"done\": {\"type\":\"noul\",\"noul\":0.93}}}"
        `shouldBe` [Unsure, Unsure]
    it "(e) a reply that is not JSON is unsure for every question" $
      jevAnswers [which, done] "<html>bad gateway</html>" `shouldBe` [Unsure, Unsure]
    it "(f) a score is the most probable level" $
      jevAnswers [matters] "{\"answers\": {\"k\": {\"type\":\"score\",\"score\":1.2,\"probabilities\":{\"0\":0.1,\"1\":0.6,\"2\":0.3},\"confidence\":0.6}}}"
        `shouldSatisfy` \case [Level 1 _ _] -> True; _ -> False
    it "(g) jevDecider without its key is unsure for the batch and logs nothing (no request)" $ do
      logged <- newIORef []
      let d = jevDecider neverCancel 5 (Jev "jev-latest" "OLIFANT_SPEC_NO_KEY") (\l -> modifyIORef logged (l :))
      askAll d [which, done] `shouldReturn` [Unsure, Unsure]
      ask d which `shouldReturn` Unsure
      readIORef logged `shouldReturn` []
  it "noDecider is always unsure" $
    ask noDecider commandOrQuery `shouldReturn` Unsure
