module Olifant.GlobSpec (spec) where

import Olifant.Glob
import Test.Hspec

spec :: Spec
spec = do
  describe "matchesGlob" $
    it "matches ** across directories and * within a segment" $ do
      matchesGlob "**/*.elm" "src/Page/Home.elm" `shouldBe` True
      matchesGlob "**/*.elm" "Main.elm" `shouldBe` True
      matchesGlob "src/**/*.elm" "tests/Main.elm" `shouldBe` False
      matchesGlob "elm.json" "elm.json" `shouldBe` True
      matchesGlob "*.rs" "src/lib.rs" `shouldBe` False
  describe "globPrefix" $
    it "is the literal directory part before the first wildcard" $ do
      globPrefix "src/**/*.hs" `shouldBe` "src"
      globPrefix "src/Page/*.elm" `shouldBe` "src/Page"
      globPrefix "*/*-service" `shouldBe` ""
      globPrefix "olifant.cabal" `shouldBe` ""
