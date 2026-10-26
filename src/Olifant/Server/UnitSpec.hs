module Olifant.Server.UnitSpec (spec) where

import Data.Text qualified as T
import Olifant.Language (Language (..), Match (..), Syntax (..), parseLanguage, querying)
import Olifant.Server.Unit
import Test.Hspec

python :: Language
python = l {langUnitKinds = ["function_definition", "class_definition"]}
  where
    Right l = parseLanguage "python" "/p" (T.unlines ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\""])

haskell :: Language
haskell = l {langUnitKinds = ["function", "data_type"]}
  where
    Right l = parseLanguage "haskell" "/h" (T.unlines ["files = [\"**/*.hs\"]", "comment = \"--\"", "grammar = \"haskell\""])

-- | A syntax whose query finds the given 0-based (startLine, endLine) spans.
spans :: [(Int, Int)] -> Syntax IO
spans sps = querying (\_ _ _ -> pure (Right [Match (a, 0) (z, 0) "" (0, 0) | (a, z) <- sps]))

spec :: Spec
spec = describe "Olifant.Server.Unit (units by name)" $ do
  it "names and qualifies python units" $ do
    r <- unitsOf (spans [(0, 5), (1, 2), (3, 4)]) python "class Foo:\n    def bar(self):\n        pass\n    def baz(self):\n        pass\n"
    fmap (map namedName) r `shouldBe` Right ["Foo", "Foo.bar", "Foo.baz"]
  it "a decorated def and its function_definition are one unit, def line after the decorator" $ do
    r <- unitsOf (spans [(0, 2), (1, 2)]) python "@cache\ndef f():\n    pass\n"
    r `shouldBe` Right [Named "f" (1, 3) 2]
  it "finds a method by its qualified name and a unique bare name" $ do
    let ns = [Named "Foo" (1, 6) 1, Named "Foo.bar" (2, 3) 2]
    fmap namedName (unitNamed "Foo.bar" ns) `shouldBe` Just "Foo.bar"
    fmap namedName (unitNamed "bar" ns) `shouldBe` Just "Foo.bar"
    unitNamed "nope" ns `shouldBe` Nothing
  it "a repeated name gives the earliest; an ambiguous bare name none" $ do
    fmap namedLines (unitNamed "f" [Named "f" (5, 6) 5, Named "f" (1, 2) 1]) `shouldBe` Just (1, 2)
    unitNamed "m" [Named "A.m" (2, 3) 2, Named "B.m" (5, 6) 5] `shouldBe` Nothing
  it "names haskell units" $ do
    unitNameOf haskell "data Catalog = Catalog" `shouldBe` Just "Catalog"
    unitNameOf haskell "instance Show Foo where" `shouldBe` Just "Show Foo"
    unitNameOf haskell "conjureIn top =" `shouldBe` Just "conjureIn"
  it "fingerprints ignore indentation and blank lines, and change with the body" $ do
    fingerprint "def f():\n    return 1\n\n" (1, 3) `shouldBe` fingerprint "def f():\n  return 1\n" (1, 2)
    fingerprint "def f():\n    return 1\n" (1, 2) `shouldNotBe` fingerprint "def f():\n    return 2\n" (1, 2)
    T.length (fingerprint "x" (1, 1)) `shouldBe` 7
  it "fingerprints ignore a formatter's reflow: a call split over lines, a magic trailing comma, leading commas" $ do
    let one = "def t():\n    r = f(A(x=1, y=2))\n    assert r == Ok(B(id=\"b\", n=2))\n"
        split = "def t():\n    r = f(\n        A(x=1, y=2)\n    )\n    assert r == Ok(\n        B(id=\"b\", n=2)\n    )\n"
        exploded = "def t():\n    r = f(\n        A(\n            x=1,\n            y=2,\n        )\n    )\n    assert r == Ok(B(id=\"b\", n=2))\n"
        lines' t = (1, length (T.lines t))
    fingerprint split (lines' split) `shouldBe` fingerprint one (lines' one)
    fingerprint exploded (lines' exploded) `shouldBe` fingerprint one (lines' one)
    fingerprint "xs = [ a\n     , b\n     ]\n" (1, 3) `shouldBe` fingerprint "xs = [a, b]\n" (1, 1)
    -- a body with no reflow keeps the fingerprint it had
    fingerprint "def f():\n    return 1\n" (1, 2) `shouldBe` "aee3e55"
    fingerprint one (lines' one) `shouldNotBe` fingerprint (T.replace "y=2" "y=3" one) (lines' one)
