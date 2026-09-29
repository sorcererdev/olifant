{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.LanguageSpec (spec) where

import Data.Aeson (object, (.=))
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language
import Olifant.Template (Value (..))
import Test.Hspec

elmToml :: Text
elmToml =
  T.unlines
    [ "files = [\"src/**/*.elm\"]"
    , "comment = \"--\""
    , "grammar = { source = \"grammar/elm\" }"
    , "placeholder = 'Debug.todo \"{= message =}\"'"
    , "branch = \"{= pattern =} ->\\n    {= body =}\""
    , "qualify = \"elm\""
    , "[check]"
    , "command = [\"elm\", \"make\", \"{= files =}\", \"--report=json\", \"--output=/dev/null\"]"
    , "context = [\"elm.json\"]"
    , "format = \"elm-json\""
    , "[targets.case_in]"
    , "phrase = \"case in FUNCTION\""
    , "container = { kind = \"case_of_expr\", inside = { kind = \"value_declaration\", regex = \"^{= FUNCTION =}\\\\b\", stopBy = \"end\" } }"
    , "items = { kind = \"case_of_branch\" }"
    , "example = \"end of case in update\""
    ]

spec :: Spec
spec = do
  describe "parseLanguage" $ do
    it "reads an adapter, resolving the grammar source against the pack" $ do
      let Right lang = parseLanguage "elm" "/packs/elm" elmToml
      langName lang `shouldBe` "elm"
      langGrammar lang `shouldBe` GrammarSource "/packs/elm/languages/grammar/elm"
      fmap checkFormat (langCheck lang) `shouldBe` Just "elm-json"
      map targetPhrase (langTargets lang) `shouldBe` ["case in FUNCTION"]
    it "accepts a built-in grammar name" $
      fmap langGrammar (parseLanguage "rust" "/p" "files = [\"**/*.rs\"]\ncomment = \"//\"\ngrammar = \"rust\"\n") `shouldBe` Right (BuiltInGrammar "rust")
    it "rejects unknown keys" $
      parseLanguage "x" "/p" "files = []\ncomment = \"#\"\ngrammar = \"x\"\ncolour = 1\n" `shouldSatisfy` either ("colour" `T.isInfixOf`) (const False)
    it "renders a target's rule with the phrase's parameters" $ do
      let Right lang = parseLanguage "elm" "/p" elmToml
          [t] = langTargets lang
          Just binds = matchShape t ["case", "in", "update"]
      renderRule (M.fromList [(k, VText v) | (k, v) <- binds]) (targetContainer t)
        `shouldBe` Right (object ["kind" .= ("case_of_expr" :: Text), "inside" .= object ["kind" .= ("value_declaration" :: Text), "regex" .= ("^update\\b" :: Text), "stopBy" .= ("end" :: Text)]])
  describe "matchShape" $ do
    it "binds parameters and requires literal words" $ do
      let Right lang = parseLanguage "elm" "/p" elmToml
          [t] = langTargets lang
      matchShape t ["case", "in", "view"] `shouldBe` Just [("FUNCTION", "view")]
      matchShape t ["case", "of", "view"] `shouldBe` Nothing
      matchShape t ["case", "in"] `shouldBe` Nothing
  describe "matchesGlob" $ do
    it "matches ** across directories and * within a segment" $ do
      matchesGlob "**/*.elm" "src/Page/Home.elm" `shouldBe` True
      matchesGlob "**/*.elm" "Main.elm" `shouldBe` True
      matchesGlob "src/**/*.elm" "tests/Main.elm" `shouldBe` False
      matchesGlob "elm.json" "elm.json" `shouldBe` True
      matchesGlob "*.rs" "src/lib.rs" `shouldBe` False
  describe "languageFor" $
    it "picks the first adapter whose globs match" $ do
      let Right elm = parseLanguage "elm" "/p" elmToml
      fmap langName (languageFor [elm] "src/Main.elm") `shouldBe` Just "elm"
      fmap langName (languageFor [elm] "src/lib.rs") `shouldBe` Nothing
  describe "ports without tools" $ do
    it "noSyntax reports ast-grep as missing, with the install command" $ do
      let Right lang = parseLanguage "elm" "/p" elmToml
      r <- querySyntax noSyntax lang (object []) "x"
      either (const True) (const False) r `shouldBe` True
      either describeSyntaxFailure (const "") r `shouldSatisfy` ("brew install ast-grep" `T.isInfixOf`)
    it "noChecker reports that no check is available" $ do
      let Right lang = parseLanguage "elm" "/p" elmToml
      r <- runCheck noChecker lang M.empty
      either describeCheckFailure (const "") r `shouldSatisfy` ("no checker" `T.isInfixOf`)
