module Olifant.AnchorSpec (spec) where

import Data.Text qualified as T
import Hedgehog (Gen, forAll, tripping)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Olifant.Anchor
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = phraseSpec

phraseSpec :: Spec
phraseSpec = describe "parsePhrase" $ do
  it "reads a language target" $
    parsePhrase "end of case in update" `shouldBe` Right (Phrase EndOf (NamedTarget ["case", "in", "update"]))
  it "reads a marker, keeping its text exactly" $
    parsePhrase "before marker -- olifant:pages" `shouldBe` Right (Phrase Before (MarkerTarget "-- olifant:pages"))
  it "reads line targets, keeping the regex exactly" $ do
    parsePhrase "after last line matching ^import Page\\." `shouldBe` Right (Phrase After (LineTarget LastLine "^import Page\\."))
    parsePhrase "before first line matching ^ +Olifant" `shouldBe` Right (Phrase Before (LineTarget FirstLine "^ +Olifant"))
  it "ignores surrounding whitespace" $
    parsePhrase "  start of type Msg " `shouldBe` Right (Phrase StartOf (NamedTarget ["type", "Msg"]))
  it "rejects a missing position, suggesting the closest one" $
    parsePhrase "ending of type Msg" `shouldSatisfy` either ("did you mean `end of`" `T.isInfixOf`) (const False)
  it "lists the valid forms when nothing is close" $
    parsePhrase "somewhere" `shouldSatisfy` either ("start of, end of, before or after" `T.isInfixOf`) (const False)
  it "rejects an empty target" $ do
    parsePhrase "end of" `shouldSatisfy` either ("needs a target" `T.isInfixOf`) (const False)
    parsePhrase "before marker " `shouldSatisfy` either ("marker" `T.isInfixOf`) (const False)
  it "round-trips through showPhrase" $ hedgehog $ do
    p <- forAll genPhrase
    tripping p showPhrase parsePhrase

genPhrase :: Gen Phrase
genPhrase = Phrase <$> Gen.enumBounded <*> Gen.choice [marker, line, named]
  where
    word = Gen.text (Range.linear 1 8) Gen.alphaNum
    marker = MarkerTarget . T.unwords <$> Gen.list (Range.linear 1 3) word
    line = LineTarget <$> Gen.element [FirstLine, LastLine] <*> (("^" <>) <$> word)
    named = NamedTarget <$> Gen.filter (\ws -> take 1 ws `notElem` [["marker"], ["first"], ["last"]]) (Gen.list (Range.linear 1 3) word)
