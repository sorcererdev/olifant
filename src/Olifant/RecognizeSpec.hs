module Olifant.RecognizeSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Language (Language (..), parseLanguage)
import Olifant.Recognize
import Olifant.Template (parseTemplate)
import Test.Hspec
import Test.Hspec.Hedgehog (forAll, hedgehog)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

spec :: Spec
spec = describe "Olifant.Recognize" $ do
  weightsSpec
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

-- | Rarity weights and weighted containment (Task 2).
weightsSpec :: Spec
weightsSpec = describe "weights" $ do
  let a = ("a", "b", "c")
      b = ("b", "c", "d")
      c = ("x", "y", "z")
      everywhere = ("self", ".", "lower1")
      files = [S.fromList [everywhere, a], S.fromList [everywhere], S.fromList [everywhere], S.fromList [everywhere], S.fromList [everywhere, c]]
      w = weightsOf files
  it "(a) weighs a pattern in every file less than one in a single file" $
    weightOf w everywhere < weightOf w a `shouldBe` True
  it "(b) removes boilerplate (in more than 30% of the files) before containment" $ do
    S.member everywhere (wBoilerplate w) `shouldBe` True
    containment w (S.fromList [everywhere, a]) (S.fromList [a]) `shouldBe` 1
  it "(c) is 1 for itself, 0 for nothing shared, and always between" $ do
    containment w (S.fromList [a, b]) (S.fromList [a, b]) `shouldBe` 1
    containment w (S.fromList [a, b]) (S.fromList [c]) `shouldBe` 0
  it "(c) property: containment stays between 0 and 1" $
    hedgehog $ do
      xs <- forAll (Gen.set (Range.linear 0 8) (Gen.element [a, b, c, everywhere, ("p", "q", "r")]))
      ys <- forAll (Gen.set (Range.linear 0 8) (Gen.element [a, b, c, everywhere, ("p", "q", "r")]))
      let v = containment w xs ys
      H.assert (v >= 0 && v <= 1)
  it "(d) a change of generic patterns scores lower weighted than unweighted against a large reference (the spike's finding)" $ do
    let generic = [("g" <> T.pack (show i), "h", "k") | i <- [1 .. 6 :: Int]]
        corpus = [S.fromList (generic <> [("u" <> T.pack (show i), "v", "w")]) | i <- [1 .. 10 :: Int]] <> [S.fromList [a, b]]
        w' = (weightsOf corpus) {wBoilerplate = S.empty}
        change = S.fromList (take 3 generic <> [a, c])
        big = S.fromList (generic <> [("q", "q", "q")])
        flat = fromIntegral (S.size (S.intersection change big)) / fromIntegral (S.size change) :: Double
    containment w' change big < flat `shouldBe` True
  it "has no boilerplate in a project of fewer than 4 files" $
    wBoilerplate (weightsOf (take 3 files)) `shouldBe` S.empty
  where
    weightOf w s = M.findWithDefault (wDefault w) s (wIdf w)
