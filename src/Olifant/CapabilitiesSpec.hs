{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.CapabilitiesSpec (spec) where

import Data.Either (isRight)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Capabilities
import Olifant.Language (Language, parseLanguage, parseOverShipped)
import Test.Hspec

packLanguage :: T.Text -> FilePath -> IO Language
packLanguage name pack = either (error . T.unpack) id . parseOverShipped name pack <$> TIO.readFile (pack <> "/languages/" <> T.unpack name <> ".toml")

spec :: Spec
spec = do
  it "olifant's own haskell.toml: the six capabilities of milestone 1 present; no mutation" $ do
    hs <- packLanguage "haskell" ".olifant"
    map capName (capabilities hs) `shouldBe` ["units", "diagnostics", "missing cases", "tests", "test units", "test selection", "mutation"]
    map (isRight . capHow) (capabilities hs) `shouldBe` replicate 6 True ++ [False]
    lookup "mutation" [(capName c, capHow c) | c <- capabilities hs] `shouldBe` Just (Left "[[mutate]]")
    lookup "units" [(capName c, capHow c) | c <- capabilities hs] `shouldBe` Just (Right "[unit] kinds (ast-grep), named by [unit] name, joined")
    lookup "tests" [(capName c, capHow c) | c <- capabilities hs] `shouldBe` Just (Right "[test] format lines, tests_for {= dir =}/{= stem =}Spec.hs")
  it "python.toml: present, its test units and selection built in" $ do
    py <- packLanguage "python" "packs/python-htmx"
    map capHow (capabilities py)
      `shouldBe` [ Right "[unit] kinds (ast-grep), named by [unit] name"
                 , Right "[check] format pyright-json"
                 , Right "[check] format pyright-json"
                 , Right "[test] format pytest, tests_for {= dir =}/{= stem =}__test.py"
                 , Right "built in (pytest shape)"
                 , Right "pytest ids"
                 , Right "[[mutate]] (17 operators)"
                 ]
  it "a bare language: seven absences, each naming its key" $ do
    let Right bare = parseLanguage "x" "/x" "files = [\"**/*.x\"]\ncomment = \"#\"\ngrammar = \"x\"\n"
    map capHow (capabilities bare)
      `shouldBe` [ Left "[unit] kinds"
                 , Left "[check]"
                 , Left "a [check] format that reads missing cases (ghc-json, pyright-json, rustc-json, elm-json, olifant-json)"
                 , Left "[test]"
                 , Left "[test] unit"
                 , Left "[test] select (whole files run)"
                 , Left "[[mutate]]"
                 ]
  it "a check through convert says so" $ do
    let Right l = parseLanguage "x" "/x" "files = [\"**/*.x\"]\ncomment = \"#\"\ngrammar = \"x\"\n[check]\ncommand = [\"t\"]\nconvert = [\"c\"]\n"
    take 2 (map capHow (capabilities l)) `shouldBe` [Left "[unit] kinds", Right "[check] format olifant-json via convert"]
