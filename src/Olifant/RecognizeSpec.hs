module Olifant.RecognizeSpec (spec) where

import Data.Set qualified as S
import Data.Text.IO qualified as TIO
import Olifant.Language (Language (..), parseLanguage)
import Olifant.Recognize
import Olifant.Template (parseTemplate)
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Recognize" $
  describe "shapes" $ do
    let kw = Keywords (S.fromList ["class", "def", "self", "return"]) "#"
    it "(a) keeps keywords and punctuation, and names become their case shape" $
      shapes kw "class CloseTable:\n    def run(self, table_id):"
        `shouldBe` ["class", "Pascal2", ":", "NL", "def", "lower1", "(", "self", ",", "snake2", ")", ":"]
    it "(b) names, case and string contents do not change shapes" $
      shapes kw "def a(x): return \"hi\"" `shouldBe` shapes kw "def b(y): return 'yo'"
    it "(c) a triple-quoted docstring is one str; a comment vanishes" $
      shapes kw "\"\"\"A docstring\nover lines.\"\"\"\nx = 1  # note\n" `shouldBe` ["str", "NL", "lower1", "=", "num", "NL"]
    it "(c2) only the language's comments: a Rust attribute stays" $
      shapes (Keywords (S.fromList ["derive"]) "//") "#[derive(Debug)] // why\nstruct X;"
        `shouldBe` ["#", "[", "derive", "(", "Pascal1", ")", "]", "NL", "lower1", "Pascal1", ";"]
    it "(d) the keywords of a pack's templates are their literal words, not their variables" $ do
      src <- TIO.readFile "packs/python-htmx/generators/command/templates/command.py.dt"
      t <- either (fail . show) pure (parseTemplate src)
      let Keywords ws _ = keywordsFrom "#" [t] []
      mapM_ (\w -> (w, w `S.member` ws) `shouldBe` (w, True)) ["dataclass", "frozen", "handle", "NotImplementedError"]
      S.member "Command" ws `shouldBe` False
    it "(e) a language's keywords come from its adapter (`keywords`), none when absent" $ do
      fmap langKeywords (parseLanguage "x" "/p" "files = [\"*.x\"]\ncomment = \"#\"\ngrammar = \"python\"\nkeywords = [\"def\", \"class\"]\n")
        `shouldBe` Right ["def", "class"]
      fmap langKeywords (parseLanguage "x" "/p" "files = [\"*.x\"]\ncomment = \"#\"\ngrammar = \"python\"\n") `shouldBe` Right []
    it "makes overlapping windows of three, and knows which carry a name" $ do
      shingles ["a", "b", "c", "d"] `shouldBe` S.fromList [("a", "b", "c"), ("b", "c", "d")]
      nameShingle ("def", "snake2", "(") `shouldBe` True
      nameShingle ("def", "(", ")") `shouldBe` False
