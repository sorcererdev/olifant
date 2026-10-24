module Olifant.Server.EditsSpec (spec) where

import Olifant.Server.Edits (unifiedDiff)
import Test.Hspec

spec :: Spec
spec = describe "unifiedDiff (Olifant.Server.Edits)" $ do
  it "unifiedDiff gives hunks with three lines of context and no diff for equal texts" $ do
    unifiedDiff "t.py" "a\nb\nc\nd\ne\nf\ng\n" "a\nb\nc\nX\ne\nf\ng\n"
      `shouldBe` "--- a/t.py\n+++ b/t.py\n@@ -1,7 +1,7 @@\n a\n b\n c\n-d\n+X\n e\n f\n g\n"
    unifiedDiff "t.py" "a\n" "a\n" `shouldBe` ""
  it "unifiedDiff splits distant changes into hunks and counts an empty side as 0" $ do
    let old = "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n"
        new = "x\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\ny\n"
    unifiedDiff "t.py" old new
      `shouldBe` "--- a/t.py\n+++ b/t.py\n@@ -1,4 +1,4 @@\n-1\n+x\n 2\n 3\n 4\n@@ -9,4 +9,4 @@\n 9\n 10\n 11\n-12\n+y\n"
    unifiedDiff "t.py" "" "a\n" `shouldBe` "--- a/t.py\n+++ b/t.py\n@@ -0,0 +1,1 @@\n+a\n"
