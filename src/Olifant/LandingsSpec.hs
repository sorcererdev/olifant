module Olifant.LandingsSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Olifant.Landings
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Landings" $ do
  it "parses git log lines oldest first and labels squash, merge and plain commits" $ do
    let out =
          T.unlines
            [ "5ac7f05aaaa\t1793478356\tAdd toggle (#133)"
            , "deadbeefbbbb\t1793470000\tMerge pull request #7 from x/y"
            , "0123abccccc\t1793460000\tplain"
            ]
        ls = parseLandings "plans/p.md" out
    map ldLabel ls `shouldBe` ["0123abc", "#7", "#133"]
    ldAt (last ls) `shouldBe` UTCTime (fromGregorian 2026 10 31) (secondsToDiffTime (20 * 3600 + 25 * 60 + 56))
    map ldPlan ls `shouldBe` replicate 3 "plans/p.md"
    map ldCommit ls `shouldBe` ["0123abccccc", "deadbeefbbbb", "5ac7f05aaaa"]
  it "skips a malformed line" $ do
    map ldLabel (parseLandings "p" "abc\t12\nabc\tnotanumber\tsubject\nabcdef0123\t1793460000\tok\n") `shouldBe` ["abcdef0"]
  it "attributes a time to the first landing at or after it" $ do
    let t h m = UTCTime (fromGregorian 2026 10 25) (secondsToDiffTime (h * 3600 + m * 60))
        l h = Landing "h" "#1" "s" (t h 0) "p"
        ls = [l 10, l 12]
        pick x = ldAt <$> landingFor ls x
    pick (t 9 59) `shouldBe` Just (t 10 0)
    pick (t 10 0) `shouldBe` Just (t 10 0)
    pick (t 10 1) `shouldBe` Just (t 12 0)
    pick (t 12 1) `shouldBe` Nothing
  it "shortSubject drops the PR suffix and cuts with an ellipsis" $ do
    shortSubject 50 "Add toggle (#133)" `shouldBe` "Add toggle"
    let long = T.replicate 60 "x" :: Text
    shortSubject 50 long `shouldBe` T.replicate 49 "x" <> "…"
