module Olifant.DecideSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Olifant.Decide
import Olifant.Process (newCancel)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Decide" $ do
  let commandOrQuery = Question "command-or-query" ["command", "query"] [] [("function", "def f(): ...")]
      withField = Question "service" ["booking", "new service"] [("name", "ident")] []
  describe "validated" $ do
    it "(a) keeps an answer that is one of the options, with fields of their types" $ do
      validated commandOrQuery (Chosen "command" []) `shouldBe` Chosen "command" []
      validated withField (Chosen "new service" [("name", "kitchen")]) `shouldBe` Chosen "new service" [("name", "kitchen")]
    it "(b) anything else is unsure: not an option, a field of the wrong type, an unknown field" $ do
      validated commandOrQuery (Chosen "maybe" []) `shouldBe` Unsure
      validated withField (Chosen "booking" [("name", "3x")]) `shouldBe` Unsure
      validated withField (Chosen "booking" [("other", "x")]) `shouldBe` Unsure
  describe "recordedDecider (tests)" $ do
    it "(c) answers by the question's kind; an invalid answer is unsure" $
      withSystemTempDirectory "olifant-decide" $ \dir -> do
        let file = dir </> "answers.json"
        writeFile file "{\"answers\": {\"command-or-query\": \"query\", \"service\": \"elsewhere\"}}"
        (token, _) <- newCancel
        ask (recordedDecider file token) commandOrQuery `shouldReturn` Chosen "query" []
        ask (recordedDecider file token) withField `shouldReturn` Unsure
        ask (recordedDecider file token) (Question "other" ["a"] [] []) `shouldReturn` Unsure
    it "(c) \"wait\" never answers until the token trips, then is unsure (Review Focus 5)" $
      withSystemTempDirectory "olifant-decide" $ \dir -> do
        let file = dir </> "answers.json"
        writeFile file "wait"
        (token, trip) <- newCancel
        _ <- forkIO (threadDelay 200000 >> trip)
        timeout 5000000 (ask (recordedDecider file token) commandOrQuery) `shouldReturn` Just Unsure
  it "noDecider is always unsure" $
    ask noDecider commandOrQuery `shouldReturn` Unsure
