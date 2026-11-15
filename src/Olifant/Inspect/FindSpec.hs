module Olifant.Inspect.FindSpec (spec) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Server.Errand (RunResult (..))
import Olifant.Inspect.Find
import Olifant.Inspect.Store (Status (..))
import Olifant.Inspect.StoreSpec (finding)
import Olifant.Inspect.Store (Place (..), InspectFinding (..))
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

ask0 :: FindAsk
ask0 =
  FindAsk
    [ FindUnit "src/a.py" "A.f" (3, 5) (Just "class A:") "def f(self, x):\n    return x\n    # end"
    , FindUnit "src/b.py" "g" (1, 2) Nothing "def g():\n    pass"
    ]
    "--- a/src/a.py\n+++ b/src/a.py\n@@ -3 +3 @@\n-x\n+y\n"
    "## Rules\n### A\nno\n"
    ["rule r at src/a.py:4"]
    [(finding "abc123" "bug" Open 0.9) {vfStatement = "It returns x unchanged.", vfPlace = Place "src/a.py" (Just "A.f") (3, 3) Nothing Nothing}]

unit :: Int -> FindUnit
unit n = FindUnit "a.py" (T.pack ("u" <> show n)) (1, n) Nothing ""

spec :: Spec
spec = describe "Olifant.Inspect.Find" $ do
  it "the prompt is the 1a prompt, word for word" $ do
    golden <- TIO.readFile "src/Olifant/Inspect/testdata/find-prompt.golden"
    findPrompt ask0 `shouldBe` golden
  it "parses findings after prose, and drops one with no statement" $ do
    let reply = "I looked.\n\n{\"findings\": [{\"kind\": \"bug\", \"file\": \"src/a.py\", \"lines\": [3, 4], \"statement\": \"Wrong.\", \"rule\": \"\", \"evidence\": [{\"file\": \"src/b.py\", \"lines\": [1, 2]}]}, {\"kind\": \"bug\", \"file\": \"src/a.py\", \"lines\": [3, 4]}], \"open\": [{\"id\": \"abc123\", \"holds\": \"no longer holds\", \"why\": \"fixed\"}, {\"id\": \"x\", \"holds\": \"maybe\"}]}"
    parseFind reply `shouldBe` Right (FindReply [RawFinding "bug" "src/a.py" (3, 4) "Wrong." Nothing [("src/b.py", (1, 2))]] [("abc123", False, "fixed")])
  it "a reply with no JSON is Left; an empty answer is Right" $ do
    parseFind "Nothing to say." `shouldSatisfy` either (const True) (const False)
    parseFind "{\"findings\": [], \"open\": []}" `shouldBe` Right (FindReply [] [])
  it "drops an unknown kind and keeps a named rule" $
    fmap (map rfRule . frFindings) (parseFind "{\"findings\": [{\"kind\": \"style\", \"file\": \"a\", \"lines\": [1, 1], \"statement\": \"s\"}, {\"kind\": \"convention\", \"file\": \"a\", \"lines\": [1, 1], \"statement\": \"s\", \"rule\": \" A \"}]}") `shouldBe` Right [Just "A"]
  it "packs units up to 600 lines a call" $ do
    map (map fuName) (packUnits [unit 300, unit 300, unit 300]) `shouldBe` [["u300", "u300"], ["u300"]]
    map length (packUnits [unit 900, unit 10]) `shouldBe` [1, 1]
  it "the key is the sorted file:name pairs" $
    findKey ask0 `shouldBe` "src/a.py:A.f,src/b.py:g"
  it "the recorded finder replies and logs the key" $ withSystemTempDirectory "inspect" $ \dir -> do
    let file = dir </> "finds.json"
    TIO.writeFile file "{\"replies\": {\"src/a.py:A.f,src/b.py:g\": {\"text\": \"{}\", \"cost\": 0.03}}}"
    r <- recordedFinder file ask0
    fmap (\x -> (rrText x, rrCost x)) r `shouldBe` Right ("{}", Just 0.03)
    TIO.readFile (file <> ".asked") >>= (`shouldBe` "src/a.py:A.f,src/b.py:g\n")
    doesFileExist (file <> ".asked") >>= (`shouldBe` True)
