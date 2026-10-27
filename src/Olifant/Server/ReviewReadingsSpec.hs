{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-x-partial #-}
module Olifant.Server.ReviewReadingsSpec (spec) where

import Data.Aeson (Value (..), toJSON)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.LspSpec (plainPorts)
import Olifant.Plans (parsePlan)
import Olifant.Server.Errand (Block (..), Ghost (..))
import Olifant.Server.Journal (Event (..))
import Olifant.Server.LoopTypes
import Olifant.Server.Review (Review (..), loadReviews, saveReview)
import Olifant.Server.ReviewReadings
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Server.ReviewReadings (a reconcile's readings as a review)" $ do
  it "makes one entry per reading, each kind with its offers and first sentence" $ do
    let Just rr = readingsReview [] "plans/t.md" answer
        view = readingsPayload rr plan (const Null)
    map readingKey (rrReadings rr) `shouldBe` ["done|a.py|helper|2222222|tidy the helper", "reopened|a.py|count|3333333|count seats", "changed|a.py|helper|2222222|tidy the helper", "gone|a.py|spare||keep spare", "unplaced|a.py|extra|4444444", "unplaced|a.py|other|5555555"]
    map (\e -> (str "kind" e, str "summary" e)) (entries view)
      `shouldBe` [ ("done", "Step 1 looks done"), ("reopened", "Step 2 looks open again")
                 , ("changed", "Step 1 looks wrong: helper no longer returns one"), ("gone", "Step 3's unit is gone: spare")
                 , ("unplaced", "Not in any step: `extra` in a.py"), ("unplaced", "Not in any step: `other` in a.py") ]
    map (map (str "key") . list "offers") (entries view)
      `shouldBe` [["a", "r"], ["a", "r"], ["a", "r"], ["a", "r"], ["a", "1", "2", "3", "n", "i"], ["a", "1", "2", "3", "n", "i"]]
    -- `a` names where it goes: the step Jev picked, else a new step
    map (str "label" . head . list "offers") (drop 4 (entries view)) `shouldBe` ["place it under Step 4", "add it as a new step"]
  it "is nothing when there are no readings, or every one is answered" $ do
    readingsReview [] "plans/t.md" answer {raBlocks = []} `shouldBe` Nothing
    readingsReview (map readingKey (rrReadings full)) "plans/t.md" answer `shouldBe` Nothing
    map readingKey . rrReadings <$> readingsReview ["done|a.py|helper|2222222|tidy the helper"] "plans/t.md" answer `shouldBe` Just (drop 1 (map readingKey (rrReadings full)))
  it "derives each choice's plan edit from the plan now" $ do
    let [done, reopened, changed, gone, also, new] = rrReadings full
        ls = parsePlan plan
    readingFacts ls done Apply `shouldBe` Right [Replace 2 "- [x] tidy the helper @a.py:helper ✓ 2222222"]
    readingFacts ls reopened Apply `shouldBe` Right [Replace 3 "- [ ] count seats @a.py:count"]
    readingFacts ls changed Apply `shouldBe` Right [Insert 2 ["  - ✗ changed: helper no longer returns one @a.py:helper ✓ 2222222"]]
    readingFacts ls gone Apply `shouldBe` Right [Insert 4 ["  - ✗ gone: @a.py:spare no longer exists"]]
    readingFacts ls done Leave `shouldBe` Right []
    readingFacts ls also Apply `shouldBe` Right [Insert 5 ["  - also @a.py:extra ✓ 4444444"]]
    readingFacts ls also (Place 2) `shouldBe` Right [Insert 2 ["  - also @a.py:extra ✓ 4444444"]]
    readingFacts ls also New `shouldBe` Right [Insert 5 ["- [ ] def extra() @a.py:extra"]]
    readingFacts ls also Ignore `shouldBe` Right []
    readingFacts ls new Apply `shouldBe` Right [Insert 5 ["- [ ] def other() @a.py:other"]]
    readingFacts ls new (Place 3) `shouldSatisfy` isLeft -- a done step
    readingFacts ls done Ignore `shouldSatisfy` isLeft -- not an offer of a step's reading
  it "journals an answer under its step, in words; new and ignore under none" $ do
    let [done, _, changed, gone, also, _] = rrReadings full
        [stepLine] = take 1 (drop 2 (parsePlan plan))
        at0 = UTCTime (fromGregorian 2026 10 26) 0
        words' r c = evWords <$> readingEvent (readingKey r) r c stepLine at0
    words' done Apply `shouldBe` Just "ticked (review: looked done)"
    words' done Leave `shouldBe` Just "left open (review: looked done)"
    words' changed Apply `shouldBe` Just "marked changed (review)"
    words' gone Apply `shouldBe` Just "marked gone (review)"
    words' also (Place 2) `shouldBe` Just "placed extra here (review)"
    words' also New `shouldBe` Nothing
    words' also Ignore `shouldBe` Nothing
  it "finds its step again when the plan moved it, and refuses when the step is gone" $ do
    let [done, _, changed, _, _, new] = rrReadings full
        moved = parsePlan ("# T\n\nsome words first\n" <> T.drop 5 plan)
    readingFacts moved done Apply `shouldBe` Right [Replace 3 "- [x] tidy the helper @a.py:helper ✓ 2222222"]
    readingFacts moved changed Apply `shouldBe` Right [Insert 3 ["  - ✗ changed: helper no longer returns one @a.py:helper ✓ 2222222"]]
    readingFacts moved new Apply `shouldBe` Right [Insert 6 ["- [ ] def other() @a.py:other"]]
    readingFacts (parsePlan (T.replace "- [ ] tidy the helper @a.py:helper\n" "" plan)) done Apply `shouldSatisfy` isLeft
  it "carries the plan edit as a diff of the plan over the unit's diff" $ do
    let view = readingsPayload full plan (toJSON . length)
        landing = obj "landing" (head (entries view))
    lookupKey "edit" landing `shouldBe` Just (Number 1)
    str "diff" landing `shouldSatisfy` T.isInfixOf "-- [ ] tidy the helper @a.py:helper\n+- [x] tidy the helper @a.py:helper ✓ 2222222"
    str "diff" landing `shouldSatisfy` T.isInfixOf "-    return 1\n+    return 2"
  it "is replaced by each reconcile; an answered reading is not offered again; none left, the file goes" $
    withSystemTempDirectory "olifant-readings" $ \root -> do
      Just r1 <- replaceReadings plainPorts root "plans/t.md" answer
      length (rrReadings r1) `shouldBe` 6
      doesFileExist (root </> ".olifant/state/reviews/plans/t/reconcile.json") `shouldReturn` True
      let Just r2 = answerReading r1 "done|a.py|helper|2222222|tidy the helper"
      saveReadings plainPorts root r2
      Just r3 <- replaceReadings plainPorts root "plans/t.md" answer {raId = "second"}
      map readingKey (rrReadings r3) `shouldBe` drop 1 (map readingKey (rrReadings full))
      rrErrand r3 `shouldBe` "second"
      loadReadings plainPorts root "plans/t.md" `shouldReturn` Just r3
      -- the do reviews in the same folder do not read it as one of theirs
      saveReview plainPorts root aDo
      map rvId <$> loadReviews plainPorts root "plans/t.md" `shouldReturn` ["d1"]
      replaceReadings plainPorts root "plans/t.md" answer {raBlocks = []} `shouldReturn` Nothing
      doesFileExist (root </> ".olifant/state/reviews/plans/t/reconcile.json") `shouldReturn` False
  it "keeps two steps' readings of one unit apart: each its own id, each its own edit" $ do
    let twice = "# T\n\n- [ ] tidy the helper @a.py:helper\n- [ ] test the helper @a.py:helper\n"
        b r t = ReconcileBlock (Block r (Just t) [Ghost (T.replace "- [ ]" "- [x]" t <> " ✓ 2222222") Nothing]) True (Just ("a.py", "helper", "2222222")) Done Nothing Nothing
        Just rr = readingsReview [] "plans/t.md" answer {raBlocks = [b 2 "- [ ] tidy the helper @a.py:helper", b 3 "- [ ] test the helper @a.py:helper"]}
        [one, two] = rrReadings rr
    readingKey one `shouldNotBe` readingKey two
    readingFacts (parsePlan twice) two Apply `shouldBe` Right [Replace 3 "- [x] test the helper @a.py:helper ✓ 2222222"]
    map readingKey . rrReadings <$> answerReading rr (readingKey one) `shouldBe` Just [readingKey two]
  it "answering takes the reading out, and the last one ends the review" $ do
    let Just one = readingsReview (drop 1 (map readingKey (rrReadings full))) "plans/t.md" answer
    answerReading one "done|a.py|helper|2222222|tidy the helper" `shouldBe` Nothing
    answerReading full "nope" `shouldBe` Just full
  where
    full = let Just r = readingsReview [] "plans/t.md" answer in r
    isLeft = either (const True) (const False)

plan :: Text
plan =
  T.unlines
    [ "# T", ""
    , "- [ ] tidy the helper @a.py:helper"
    , "- [x] count seats @a.py:count ✓ 1111111"
    , "- [ ] keep spare @a.py:spare"
    , "- [ ] later" ]

answer :: ReconcileAnswer
answer = ReconcileAnswer "r1" (Just "base") [] blocks 6 0 1.0 ["jev"] (Just 3)
  where
    step r = Block r (Just (T.lines plan !! r))
    g t = Ghost t Nothing
    cu name = ChangedUnit "a.py" name (Just "x") (Changed "y") ("def " <> name <> "():") (Just ("def " <> name <> "():\n    return 2")) (Just ("def " <> name <> "():\n    return 1"))
    newAt t = Block 5 (Just "- [ ] later") [g t]
    blocks =
      [ ReconcileBlock (step 2 [g "- [x] tidy the helper @a.py:helper ✓ 2222222"]) True (Just ("a.py", "helper", "2222222")) Done Nothing (Just (cu "helper"))
      , ReconcileBlock (step 3 [g "- [ ] count seats @a.py:count"]) True (Just ("a.py", "count", "3333333")) Reopened Nothing (Just (cu "count"))
      , ReconcileBlock (step 2 [g "  - ✗ changed: helper no longer returns one @a.py:helper"]) False (Just ("a.py", "helper", "2222222")) Diverged Nothing (Just (cu "helper"))
      , ReconcileBlock (step 4 [g "  - ✗ gone: @a.py:spare no longer exists"]) False (Just ("a.py", "spare", "")) Gone Nothing (Just (cu "spare"))
      , ReconcileBlock (step 5 [g "  - also @a.py:extra"]) False (Just ("a.py", "extra", "4444444")) Also (Just (newAt "- [ ] def extra() @a.py:extra")) (Just (cu "extra"))
      , ReconcileBlock (newAt "- [ ] def other() @a.py:other") False (Just ("a.py", "other", "5555555")) NewStep (Just (newAt "- [ ] def other() @a.py:other")) (Just (cu "other"))
      ]

aDo :: Review
aDo = Review "d1" "e1" "plans/t.md" 2 "- [ ] tidy the helper @a.py:helper" (UTCTime (fromGregorian 2026 10 26) 0) "m" 1 Nothing "answered" Nothing [] [] [] [] [] [] [] Nothing "" "" Nothing Nothing [] Nothing

entries :: Value -> [Value]
entries = list "entries"

list :: Text -> Value -> [Value]
list k v = case lookupKey k v of
  Just (Array xs) -> toList xs
  _ -> []

obj :: Text -> Value -> Value
obj k v = maybe Null id (lookupKey k v)

str :: Text -> Value -> Text
str k v = case lookupKey k v of
  Just (String t) -> t
  _ -> ""

lookupKey :: Text -> Value -> Maybe Value
lookupKey k = \case
  Object o -> KM.lookup (K.fromText k) o
  _ -> Nothing
