module Olifant.HolesSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Holes
import System.Timeout (timeout)
import Test.Hspec

hole :: Text
hole = "\10214olifant-hole val|expr|a value\10215"

spec :: Spec
spec = describe "matchesWithHoles (spec §6.4)" $ do
  it "matches the whole file, not a part of it (review C1)" $ do
    matchesWithHoles ("a\n" <> hole <> "\nb\n") "zz\na\n1\nb\n" `shouldBe` False
    matchesWithHoles ("x = " <> hole <> ";") "x = 1;\ny = 2" `shouldBe` False
    matchesWithHoles ("x = " <> hole <> ";") "fn f() {}\nx = 1;" `shouldBe` False
    matchesWithHoles ("a\n" <> hole <> "\nb\n") "a\n42\nb\n" `shouldBe` True
    matchesWithHoles ("(" <> hole <> ")") "(todo!(\"x\"))" `shouldBe` True
  it "is fast on a large file (review C1)" $ do
    let big = T.unlines [T.pack ("line " <> show i <> " (x.*) [y]") | i <- [1 :: Int .. 2000]]
    r <- timeout 2000000 (pure $! matchesWithHoles (big <> hole <> "\n" <> big) (big <> "42\n" <> big))
    r `shouldBe` Just True
