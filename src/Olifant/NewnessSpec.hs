module Olifant.NewnessSpec (spec) where

import Data.Set qualified as S
import Data.Text qualified as T
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Olifant.Newness
import Test.Hspec
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

spec :: Spec
spec = describe "Olifant.Newness" $ do
  let base = T.unlines ["def a():", "    return 1", "", "def b(x):", "    return x"]
  it "(a) nothing changed: no line is new" $
    newLinesOf base base `shouldBe` S.empty
  it "(b) a function added at the end: its lines are new" $
    newLinesOf base (base <> T.unlines ["", "def c():", "    return 3"]) `shouldBe` S.fromList [6, 7, 8]
  it "(c) a line edited inside a function: only that line" $
    newLinesOf base (T.replace "return 1" "return 2" base) `shouldBe` S.fromList [2]
  it "(d) a def line changed (a parameter added): the def is not wholly new (Review Focus 1)" $ do
    let now = T.replace "def b(x):" "def b(x, y):" base
    wholeNew (NewIn (newLinesOf base now)) ((4, 1), (5, 16)) `shouldBe` False
  it "(e) a function moved within the file: none of its body is new" $ do
    let moved = T.unlines ["def b(x):", "    return x", "", "def a():", "    return 1"]
    newLinesOf base moved `shouldSatisfy` (\s -> S.size s <= 2 && not (any (`S.member` s) [2, 5]))
  it "(f) AllNew is wholly new; NoBase never is; counts follow" $ do
    wholeNew AllNew ((1, 1), (3, 1)) `shouldBe` True
    wholeNew (NoBase "no main") ((1, 1), (3, 1)) `shouldBe` False
    newCount (NewIn (S.fromList [2, 3, 9])) ((1, 1), (5, 1)) `shouldBe` 2
  it "(g) a property: appending lines makes exactly those lines new" $ hedgehog $ do
    xs <- forAll (Gen.list (Range.linear 0 30) (Gen.text (Range.linear 0 8) Gen.alphaNum))
    ys <- forAll (Gen.list (Range.linear 1 10) (Gen.text (Range.linear 1 8) Gen.alphaNum))
    let b = T.unlines xs
        n = T.unlines (xs ++ map ("zz" <>) ys)
    newLinesOf b n === S.fromList [length xs + 1 .. length xs + length ys]
