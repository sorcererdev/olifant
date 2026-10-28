{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.Server.UnitSpec (spec) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Language (Language (..), Match (..), Syntax (..), parseLanguage, querying)
import Olifant.Server.Unit
import Test.Hspec

-- | A pack's real language file.
packLanguage :: T.Text -> FilePath -> IO Language
packLanguage name pack = either (error . T.unpack) id . parseLanguage name pack <$> TIO.readFile (pack <> "/languages/" <> T.unpack name <> ".toml")

-- | A syntax whose query finds the given 0-based (startLine, endLine) spans.
spans :: [(Int, Int)] -> Syntax IO
spans sps = querying (\_ _ _ -> pure (Right [Match (a, 0) (z, 0) "" (0, 0) | (a, z) <- sps]))

spec :: Spec
spec = do
 python <- runIO (packLanguage "python" "packs/python-htmx")
 haskell <- runIO (packLanguage "haskell" "packs/olifant-dev")
 rust <- runIO (packLanguage "rust" "packs/rust-htmx")
 elm <- runIO (packLanguage "elm" "packs/elm")
 describe "Olifant.Server.Unit (units by name)" $ do
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
    unitNameIn haskell "data Catalog = Catalog" `shouldBe` Just "Catalog"
    unitNameIn haskell "instance Show Foo where" `shouldBe` Just "Show Foo"
    unitNameIn haskell "conjureIn top =" `shouldBe` Just "conjureIn"
  it "joins a haskell signature and its equations into one unit" $ do
    -- signature (0,0), its type's function node (0,0), two equations (1,1), (2,2)
    r <- unitsOf (spans [(0, 0), (0, 0), (1, 1), (2, 2)]) haskell "go :: Int -> Int\ngo 0 = 1\ngo n = n\n"
    r `shouldBe` Right [Named "go" (1, 3) 1]
  it "names haskell operators and multi-line signatures" $ do
    r <- unitsOf (spans [(0, 0), (1, 1)]) haskell "(<+>) :: A -> A -> A\na <+> b = b\n"
    fmap (map namedName) r `shouldBe` Right ["(<+>)", "a"] -- an infix equation is named by its first word, as today; not joined
    r2 <- unitsOf (spans [(0, 2), (3, 3)]) haskell "run\n  :: Int\n  -> Int\nrun = id\n"
    r2 `shouldBe` Right [Named "run" (1, 4) 1]
  it "joins across blank lines, not across other code" $ do
    r <- unitsOf (spans [(0, 0), (2, 2), (3, 3), (4, 4)]) haskell "f :: Int\n\nf = 1\ng = 2\nf' = 3\n"
    fmap (map namedLines) r `shouldBe` Right [(1, 3), (4, 4), (5, 5)]
  it "names rust items by the item, and impl blocks by their type" $ do
    unitNameIn rust "pub(crate) async fn load(x: u8) {" `shouldBe` Just "load"
    unitNameIn rust "impl<T> Display for Wrapper<T> {" `shouldBe` Just "Wrapper"
    unitNameIn rust "impl Booking {" `shouldBe` Just "Booking"
  it "names an elm type alias by its name" $
    unitNameIn elm "type alias Model =" `shouldBe` Just "Model"
  it "names python defs, async defs and classes; other lines have no name" $ do
    unitNameIn python "    async def load(self):" `shouldBe` Just "load"
    unitNameIn python "class Foo(Base):" `shouldBe` Just "Foo"
    unitNameIn python "@cache" `shouldBe` Nothing
    unitNameIn python "def crème(x):" `shouldBe` Just "crème"
    unitNameIn python "def first[T](xs: list[T]) -> T:" `shouldBe` Just "first"
  it "without [unit] name, a unit is named by its first identifier" $ do
    let Right bare = parseLanguage "x" "/x" (T.unlines ["files = [\"**/*.x\"]", "comment = \"#\"", "grammar = \"x\"", "[unit]", "kinds = [\"f\"]"])
    unitNameIn bare "foo x = 1" `shouldBe` Just "foo"
    unitNameIn bare "  " `shouldBe` Nothing
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
