module Olifant.NewnessSpec (spec) where

import Data.Set qualified as S
import Data.Text qualified as T
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Olifant.Newness
import Test.Hspec
import System.Timeout (timeout)
import Control.Exception (evaluate)
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
    wholeNew (NewIn (newLinesOf base now) S.empty) ((4, 1), (5, 16)) `shouldBe` False
  it "(e) a function moved within the file: none of its body is new" $ do
    let moved = T.unlines ["def b(x):", "    return x", "", "def a():", "    return 1"]
    newLinesOf base moved `shouldSatisfy` (\s -> S.size s <= 2 && not (any (`S.member` s) [2, 5]))
  it "(f) AllNew is wholly new; NoBase never is; counts follow" $ do
    wholeNew AllNew ((1, 1), (3, 1)) `shouldBe` True
    wholeNew (NoBase "no main") ((1, 1), (3, 1)) `shouldBe` False
    newCount (NewIn (S.fromList [2, 3, 9]) S.empty) ((1, 1), (5, 1)) `shouldBe` 2
  it "(g) a property: appending lines makes exactly those lines new" $ hedgehog $ do
    xs <- forAll (Gen.list (Range.linear 0 30) (Gen.text (Range.linear 0 8) Gen.alphaNum))
    ys <- forAll (Gen.list (Range.linear 1 10) (Gen.text (Range.linear 1 8) Gen.alphaNum))
    let b = T.unlines xs
        n = T.unlines (xs ++ map ("zz" <>) ys)
    newLinesOf b n === S.fromList [length xs + 1 .. length xs + length ys]
  it "(h) a 5,000-line file changed at both ends: answered at once, in little memory (review #1)" $ do
    let big = T.unlines ["x_" <> T.pack (show i) <> " = " <> T.pack (show i) | i <- [1 .. 5000 :: Int]]
        now = "import os\n" <> big <> "def added():\n    return os.getcwd()\n"
    answered <- timeout 300000 (evaluate (newLinesOf big now))
    answered `shouldBe` Just (S.fromList [1, 5002, 5003])
  it "(i) a property: lines inserted anywhere are exactly the new ones" $ hedgehog $ do
    n <- forAll (Gen.int (Range.linear 0 60))
    let xs = ["line " <> T.pack (show i) | i <- [1 .. n]]
    at <- forAll (Gen.list (Range.linear 1 6) (Gen.int (Range.linear 0 n)))
    -- each insertion point gets a line of its own (distinct texts)
    let inserted = [(p, "new " <> T.pack (show k)) | (k, p) <- zip [1 :: Int ..] at]
        build i = [t | (p, t) <- inserted, p == i] ++ [x | x <- take 1 (drop i xs), i < n]
        now = concatMap build [0 .. n]
        expected = S.fromList [j | (j, t) <- zip [1 ..] now, "new " `T.isPrefixOf` t]
    newLinesOf (T.unlines xs) (T.unlines now) === expected
  it "(j) a property: Myers' pairs are a longest common subsequence (against a plain table)" $ hedgehog $ do
    let gen = Gen.list (Range.linear 0 25) (Gen.element ["a", "b", "c", ""])
    xs <- forAll gen
    ys <- forAll gen
    let ps = lcsPairs xs ys
        ordered = and (zipWith (\(i, j) (i', j') -> i < i' && j < j') ps (drop 1 ps))
        equal = and [xs !! (i - 1) == ys !! (j - 1) | (i, j) <- ps]
    (ordered, equal, length ps) === (True, True, lcsLength xs ys)

-- | The LCS length by the textbook table (the oracle for lcsPairs).
lcsLength :: [T.Text] -> [T.Text] -> Int
lcsLength xs ys = last (foldl row (replicate (length ys + 1) 0) xs)
  where
    row prev x = scanl1 max (0 : zipWith3 (\y d u -> if x == y then d + 1 else u) ys prev (drop 1 prev)) `fixLeft` prev
    fixLeft cur _ = go 0 (zip cur [0 :: Int ..])
      where go _ [] = []
            go left ((c, _) : rest) = let v = max c left in v : go v rest

