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
  it "olifant's own haskell.toml: the six capabilities of milestone 1 and the stub present; no mutation" $ do
    hs <- packLanguage "haskell" ".olifant"
    map capName (capabilities hs) `shouldBe` ["units", "diagnostics", "missing cases", "tests", "test units", "anchors", "holes", "test selection", "mutation", "stub", "conjure shapes", "messages", "test messages", "stub bodies", "signature"]
    map (isRight . capHow) (capabilities hs) `shouldBe` replicate 8 True ++ [False, True, False, True, False, True, False]
    lookup "messages" [(capName c, capHow c) | c <- capabilities hs] `shouldBe` Just (Right "[check] undefined")
    lookup "stub" [(capName c, capHow c) | c <- capabilities hs] `shouldBe` Just (Right "[test] stub at end of do in spec")
    lookup "mutation" [(capName c, capFix c) | c <- capabilities hs] `shouldBe` Just Nothing
    lookup "mutation" [(capName c, capHow c) | c <- capabilities hs] `shouldBe` Just (Left "[[mutate]]")
    lookup "units" [(capName c, capHow c) | c <- capabilities hs] `shouldBe` Just (Right "queries (tree-sitter-haskell @c30d812): each function and class definition")
    lookup "tests" [(capName c, capHow c) | c <- capabilities hs] `shouldBe` Just (Right "[test] format lines, tests_for {= dir =}/{= stem =}Spec.hs")
  it "python.toml: present, its test units pytest.toml's rules, its selection pytest.toml's ids as they are" $ do
    py <- packLanguage "python" "packs/python-htmx"
    map capHow (capabilities py)
      `shouldBe` [ Right "queries (tree-sitter-python @293fdc0): each function and class definition"
                 , Right "[check] format pyright-json"
                 , Right "[check] format pyright-json"
                 , Right "[test] format junit-xml, tests_for {= dir =}/{= stem =}__test.py"
                 , Right "[test] unit, above"
                 , Right "[targets] 2 targets, generic phrases (queries)"
                 , Right "[holes] block, expr, ident, stmt, text"
                 , Right "[test] select"
                 , Right "[[mutate]] (17 operators)"
                 , Right "[test] stub"
                 , Right "[conjure] port, port.add, advice"
                 , Right "[check] undefined, missing_member"
                 , Right "[test] undefined, failure"
                 , Right "stub_lines (4)"
                 , Right "asked of the model, read by port-method's line"
                 ]
  it "a bare language: fifteen absences, each naming its key" $ do
    let Right bare = parseLanguage "x" "/x" "files = [\"**/*.x\"]\ncomment = \"#\"\ngrammar = \"x\"\n"
    map capHow (capabilities bare)
      `shouldBe` [ Left "[unit] kinds"
                 , Left "[check]"
                 , Left "a [check] format that reads missing cases (ghc-json, pyright-json, rustc-json, elm-json, olifant-json)"
                 , Left "[test]"
                 , Left "[test] unit"
                 , Left "[targets.*]"
                 , Left "[holes.*]"
                 , Left "[test] select (whole files run)"
                 , Left "[[mutate]]"
                 , Left "[test] stub"
                 , Left "[conjure] (conjure works at the floor: the test and its fill, no ports)"
                 , Left "[check] undefined, missing_member (no undefined names read from the check)"
                 , Left "[test] undefined, [test.failure] (the panel shows the test's last assertion)"
                 , Left "stub_lines (a body is a stub only when it is empty, a comment or a hole)"
                 , Left "[conjure.port] add (a method a port lacks is yours to add)"
                 ]
  it "a units provider is a capability, and test units and selection by program" $ do
    let Right l = parseLanguage "x" "/x" "files = [\"**/*.x\"]\ncomment = \"#\"\ngrammar = \"x\"\n[unit]\nkinds = { provider = [\"sh\", \"tools/units.sh\"] }\n[test]\ncommand = [\"t\"]\nformat = \"tap\"\ntests_for = \"x\"\nunit = { provider = [\"tests\"] }\nselect = { provider = [\"pick\"] }\n"
    lookup "units" [(capName c, capHow c) | c <- capabilities l] `shouldBe` Just (Right "[unit] provider sh tools/units.sh")
    lookup "test units" [(capName c, capHow c) | c <- capabilities l] `shouldBe` Just (Right "[test] unit provider tests")
    lookup "test selection" [(capName c, capHow c) | c <- capabilities l] `shouldBe` Just (Right "[test] select provider pick")
  it "a check through convert says so" $ do
    let Right l = parseLanguage "x" "/x" "files = [\"**/*.x\"]\ncomment = \"#\"\ngrammar = \"x\"\n[check]\ncommand = [\"t\"]\nconvert = [\"c\"]\n"
    take 2 (map capHow (capabilities l)) `shouldBe` [Left "[unit] kinds", Right "[check] format olifant-json via convert"]
