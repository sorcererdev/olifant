{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.Language.GenericSpec (spec) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Olifant.Language
import Olifant.Language.Generic
import Test.Hspec

gcc :: ErrorFormat
gcc = ErrorFormat {efPattern = "^([^:]+):([0-9]+):[0-9]+: (error|warning): (.*)$", efFile = 1, efLine = 2, efMessage = 4, efSeverity = Just 3, efError = Just "^error$"}

isLeftWith :: T.Text -> Either T.Text a -> Bool
isLeftWith word = either (word `T.isInfixOf`) (const False)

spec :: Spec
spec = do
  describe "sarif" $ do
    it "an error result is a broken file at its line, named by rule and message; a warning is ignored" $ do
      raw <- BS.readFile "src/Olifant/Language/testdata/sarif/two-results.json"
      supportParse sarifSupport raw `shouldBe` Right (Report [] [("src/A.hs", "X1: no such name")] [] [(("src/A.hs", "X1: no such name"), 3)])
    it "a run with no results is clean" $
      supportParse sarifSupport "{\"version\":\"2.1.0\",\"runs\":[{\"results\":[]}]}" `shouldBe` Right (Report [] [] [] [])
    it "bad input names the format" $
      supportParse sarifSupport "not json" `shouldSatisfy` isLeftWith "sarif"
  describe "errorformat" $ do
    it "a line matching the pattern whose severity passes `error` is a broken file at that line" $ do
      raw <- BS.readFile "src/Olifant/Language/testdata/errorformat/gcc-style.txt"
      supportParse (errorFormatSupport gcc) raw `shouldBe` Right (Report [] [("src/a.c", "boom")] [] [(("src/a.c", "boom"), 3)])
    it "without `error`, every match counts" $ do
      raw <- BS.readFile "src/Olifant/Language/testdata/errorformat/gcc-style.txt"
      fmap reportBroken (supportParse (errorFormatSupport gcc {efError = Nothing}) raw) `shouldBe` Right [("src/a.c", "boom"), ("src/a.c", "meh")]
    it "bad input names the format" $
      supportParse (errorFormatSupport gcc {efLine = 3}) "src/a.c:3:5: error: boom\n" `shouldSatisfy` isLeftWith "errorformat"
  describe "olifant-json" $ do
    it "reads broken files and missing cases" $
      supportParse olifantJsonSupport "{\"broken\":[{\"file\":\"a.hs\",\"message\":\"m\",\"line\":4},{\"file\":\"b.hs\",\"message\":\"n\"}],\"missing\":[{\"file\":\"c.hs\",\"case\":[2,5],\"patterns\":[\"Nothing\"],\"type\":\"Maybe Int\"}]}"
        `shouldBe` Right (Report [MissingPatterns "c.hs" (2, 5) ["Nothing"] (Just "Maybe Int")] [("a.hs", "m"), ("b.hs", "n")] [] [(("a.hs", "m"), 4)])
    it "{} is a clean report" $
      supportParse olifantJsonSupport "{}" `shouldBe` Right (Report [] [] [] [])
    it "bad input names the format" $
      supportParse olifantJsonSupport "{\"broken\": 3}" `shouldSatisfy` isLeftWith "olifant-json"
  describe "formats by [check]" $ do
    it "knows the generic formats, and which formats read missing cases" $ do
      mapM_ (\f -> f `elem` knownFormats `shouldBe` True) ["sarif", "errorformat", "olifant-json"]
      map supportsMissing ["ghc-json", "olifant-json", "sarif", "errorformat"] `shouldBe` [True, True, False, False]
  describe "convert" $ do
    it "a [check] with convert parses what the convert command prints" $ do
      let Right lang = parseLanguage "x" "/x" (T.unlines
            [ "files = [\"**/*.hs\"]", "comment = \"--\"", "grammar = { git = \"https://github.com/tree-sitter/tree-sitter-haskell\", rev = \"c30d812bc90827f1a54106a25bc9a6307f5cdcec\" }"
            , "[check]", "command = [\"sh\", \"-c\", \"echo raw\"]"
            , "convert = [\"sh\", \"-c\", \"cat >/dev/null; echo '{\\\"broken\\\":[{\\\"file\\\":\\\"a.hs\\\",\\\"message\\\":\\\"m\\\"}]}'\"]"
            ])
      r <- runCheck processChecker lang (M.fromList [("a.hs", "x = 1\n")])
      fmap reportBroken r `shouldBe` Right [("a.hs", "m")]
    it "the convert command sees the check's exit and output on stdin" $ do
      let Right lang = parseLanguage "x" "/x" (T.unlines
            [ "files = [\"**/*.hs\"]", "comment = \"--\"", "grammar = { git = \"https://github.com/tree-sitter/tree-sitter-haskell\", rev = \"c30d812bc90827f1a54106a25bc9a6307f5cdcec\" }"
            , "[check]", "command = [\"sh\", \"-c\", \"echo said; exit 3\"]"
            , "convert = [\"sh\", \"-c\", \"in=$(cat); echo \\\"$in\\\" | grep -q '\\\"exit\\\":3' && echo \\\"$in\\\" | grep -q said && echo '{\\\"broken\\\":[{\\\"file\\\":\\\"a.hs\\\",\\\"message\\\":\\\"seen\\\"}]}' || { echo nope >&2; exit 1; }\"]"
            ])
      r <- runCheck processChecker lang (M.fromList [("a.hs", "x = 1\n")])
      fmap reportBroken r `shouldBe` Right [("a.hs", "seen")]
    it "convert's stdin has v 1" $ do
      let Right lang = parseLanguage "x" "/x" (T.unlines
            [ "files = [\"**/*.hs\"]", "comment = \"--\"", "grammar = { git = \"https://github.com/tree-sitter/tree-sitter-haskell\", rev = \"c30d812bc90827f1a54106a25bc9a6307f5cdcec\" }"
            , "[check]", "command = [\"sh\", \"-c\", \"echo said\"]"
            , "convert = [\"sh\", \"-c\", \"in=$(cat); echo \\\"$in\\\" | grep -q '\\\"v\\\":1' && echo '{\\\"broken\\\":[{\\\"file\\\":\\\"a.hs\\\",\\\"message\\\":\\\"v1\\\"}]}' || { echo nope >&2; exit 1; }\"]"
            ])
      r <- runCheck processChecker lang (M.fromList [("a.hs", "x = 1\n")])
      fmap reportBroken r `shouldBe` Right [("a.hs", "v1")]
    it "a failing tool whose converted report is empty is a failed check with the tool's stderr" $ do
      let Right lang = parseLanguage "x" "/x" (T.unlines
            [ "files = [\"**/*.hs\"]", "comment = \"--\"", "grammar = { git = \"https://github.com/tree-sitter/tree-sitter-haskell\", rev = \"c30d812bc90827f1a54106a25bc9a6307f5cdcec\" }"
            , "[check]", "command = [\"sh\", \"-c\", \"echo Failed to spawn >&2; exit 2\"]"
            , "convert = [\"sh\", \"-c\", \"cat >/dev/null; echo '{}'\"]"
            ])
      r <- runCheck processChecker lang (M.fromList [("a.hs", "x = 1\n")])
      either (\f -> all (`T.isInfixOf` describeCheckFailure f) ["without a report", "Failed to spawn"]) (const False) r `shouldBe` True
    it "a convert command that fails is a failed check with what it said" $ do
      let Right lang = parseLanguage "x" "/x" (T.unlines
            [ "files = [\"**/*.hs\"]", "comment = \"--\"", "grammar = { git = \"https://github.com/tree-sitter/tree-sitter-haskell\", rev = \"c30d812bc90827f1a54106a25bc9a6307f5cdcec\" }"
            , "[check]", "command = [\"sh\", \"-c\", \"echo raw\"]"
            , "convert = [\"sh\", \"-c\", \"echo cannot read it >&2; exit 1\"]"
            ])
      r <- runCheck processChecker lang (M.fromList [("a.hs", "x = 1\n")])
      either (T.isInfixOf "cannot read it" . describeCheckFailure) (const False) r `shouldBe` True
