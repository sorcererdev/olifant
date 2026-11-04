{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.LanguageInitSpec (spec) where

import Data.Text qualified as T
import Olifant.Fill (FillHole (..), FillRequest (..), Stance (..))
import Olifant.LanguageInit
import Test.Hspec

survey :: Survey
survey = Survey "https://example.invalid/tree-sitter-x" Nothing Nothing ["**/*.go"] ["**/*_test.go"] (Just "go.mod") ["go"]

spec :: Spec
spec = describe "Olifant.LanguageInit" $ do
  describe "parseSurvey" $ do
    it "reads the keys" $
      parseSurvey (T.unlines ["grammar = \"https://example.invalid/tree-sitter-x\"", "files = [\"**/*.go\"]", "tests = [\"**/*_test.go\"]", "manifest = \"go.mod\"", "runners = [\"go\"]"])
        `shouldBe` Right survey
    it "reads path and name, the loader's monorepo keys" $
      fmap (\s -> (svPath s, svName s)) (parseSurvey "grammar = \"u\"\npath = \"tsx\"\nname = \"tsx\"\nfiles = [\"*.tsx\"]\n")
        `shouldBe` Right (Just "tsx", Just "tsx")
    it "refuses an empty grammar and a missing files" $ do
      parseSurvey "grammar = \"\"\nfiles = [\"*.go\"]\n" `shouldSatisfy` either ("grammar" `T.isInfixOf`) (const False)
      parseSurvey "grammar = \"u\"\n" `shouldSatisfy` either ("files" `T.isInfixOf`) (const False)
    it "strips a markdown fence around the TOML, and refuses other text" $ do
      fmap svGrammar (parseSurvey "```toml\ngrammar = \"u\"\nfiles = [\"*.go\"]\n```\n") `shouldBe` Right "u"
      parseSurvey "Here it is:\ngrammar = \"u\"\nfiles = [\"*.go\"]\n" `shouldSatisfy` either (const True) (const False)
  it "sampleFor: shallow sources and tests first, and the manifest" $
    sampleFor survey ["cmd/main.go", "internal/rates/rates.go", "internal/rates/rates_test.go", "internal/x/y/z.go", "x_test.go", "go.mod"]
      `shouldBe` (["cmd/main.go", "internal/rates/rates.go"], ["x_test.go", "internal/rates/rates_test.go"], Just "go.mod")
  it "sampleFor: no manifest when the project has none" $ do
    let (_, _, m) = sampleFor survey ["a.go"]
    m `shouldBe` Nothing
  it "cutTo keeps the first lines" $
    cutTo 2 "a\nb\nc\n" `shouldBe` "a\nb\n"
  it "surveyRequest: one hole `survey`, the file stance, the paths in its inputs" $ do
    let req = surveyRequest "go" ["go.mod", "cmd/main.go"]
    map fhName (frHoles req) `shouldBe` ["survey"]
    frStance req `shouldBe` FileStance
    lookup "language" (frInputs req) `shouldBe` Just "go"
    lookup "tracked files" (frInputs req) `shouldSatisfy` maybe False ("cmd/main.go" `T.isInfixOf`)
  it "surveyRequest lists at most 200 paths" $ do
    let req = surveyRequest "go" [show n <> ".go" | n <- [1 :: Int .. 300]]
        Just paths = lookup "tracked files" (frInputs req)
    length (T.lines paths) `shouldBe` 200
