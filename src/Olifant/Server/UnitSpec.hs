{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.Server.UnitSpec (spec) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Language (Language (..), Match (..), Provider (..), Syntax (..), parseLanguage, parseOverShipped, querying)
import Olifant.Server.Unit
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec

-- | A pack's real language file.
packLanguage :: T.Text -> FilePath -> IO Language
packLanguage name pack = either (error . T.unpack) id . parseOverShipped name pack <$> TIO.readFile (pack <> "/languages/" <> T.unpack name <> ".toml")

-- | A syntax whose query finds the given 0-based (startLine, endLine) spans.
spans :: [(Int, Int)] -> Syntax IO
spans sps = querying (\_ _ _ -> pure (Right [Match (a, 0) (z, 0) "" (0, 0) | (a, z) <- sps]))

spec :: Spec
spec = do
 python <- runIO (packLanguage "python" "packs/python-htmx")
 haskell <- runIO (packLanguage "haskell" ".olifant")
 rust <- runIO (packLanguage "rust" "packs/rust-htmx")
 elm <- runIO (packLanguage "elm" "packs/elm")
 describe "the unit at a line, by the language (unitAt)" $ do
  it "names a Haskell function by [unit] name; its body after the line that opens a block" $ do
    r <- unitAt (spans [(0, 2)]) haskell Nothing "foo :: Int\nfoo =\n  1\n" 3
    fmap (\u -> (unitName u, unitDef u, unitBody u)) r `shouldBe` Right ("foo", 1, Just (3, 3))
  it "names a Rust fn; its body after its brace" $ do
    r <- unitAt (spans [(0, 2)]) rust Nothing "fn add(a: i32) -> i32 {\n    a\n}\n" 2
    fmap (\u -> (unitName u, unitBody u)) r `shouldBe` Right ("add", Just (2, 3))
 describe "a units provider (4b m1)" $ do
  here <- runIO getCurrentDirectory
  let provided script = python {langUnitProvider = Just (Provider ["sh", T.pack ("testdata/providers" </> script)] (Just (here </> "src/Olifant")))}
      -- the rule's query must not run when a provider answers
      noQuery = querying (\_ _ _ -> error "the [unit] kinds query ran")
      text = "import x\n\ndef add(a, b):\n    return a + b\n\n"
  it "a units provider names the units" $ do
    unitsOf noQuery (provided "units.sh") Nothing text `shouldReturn` Right [Named "add" (3, 5) 3]
    r <- unitAt noQuery (provided "units.sh") Nothing text 4
    fmap (\u -> (unitName u, unitDef u, unitLines u, unitBody u)) r `shouldBe` Right ("add", 3, (3, 5), Just (4, 5))
    fmap unitName <$> unitAt noQuery (provided "units.sh") Nothing text 1 `shouldReturn` Left "no function or class at line 1"
  it "a failing units provider is a Left with its reason" $ do
    unitsOf noQuery (provided "fails.sh") Nothing text `shouldReturn` Left "no grammar"
    fmap unitName <$> unitAt noQuery (provided "fails.sh") Nothing text 4 `shouldReturn` Left "no grammar"
 describe "the test at a line, else the unit (testOrUnitAt)" $ do
  it "a hspec it: the it, its body its own lines (not the spec's)" $ do
    let text = "spec :: Spec\nspec = describe \"M\" $ do\n  it \"a\" $ do\n    1 `shouldBe` 1\n  it \"b\" $ do\n    2 `shouldBe` 2\n"
    r <- testOrUnitAt (spans [(1, 5)]) haskell Nothing text 5
    fmap (\u -> (unitName u, unitBody u)) r `shouldBe` Right ("b", Just (6, 6))
  it "no test there: the [unit]" $ do
    r <- testOrUnitAt (spans [(0, 2)]) haskell Nothing "foo :: Int\nfoo =\n  1\n" 3
    fmap (\u -> (unitName u, unitBody u)) r `shouldBe` Right ("foo", Just (3, 3))
 describe "a body that is still a stub (isStubIn)" $ do
  it "a hspec stub is a stub, a shouldBe is not" $ do
    isStubIn haskell ["  expectationFailure \"write this test\""] `shouldBe` True
    isStubIn haskell ["  1 `shouldBe` 1"] `shouldBe` False
  it "a Rust todo!() body is a stub; Python's ..., pass, assert ..., a comment and a hole are" $ do
    isStubIn rust ["    todo!(\"write this test\");"] `shouldBe` True
    isStubIn python ["    ...", "    pass", "    assert ...", "    # kills: x", "    raise NotImplementedError(\"olifant: h\")"] `shouldBe` True
    isStubIn python ["    assert x == 1"] `shouldBe` False
 describe "Olifant.Server.Unit (units by name)" $ do
  it "units come from queries when [unit] kinds is absent" $ do
    let noQuery = querying (\_ _ _ -> error "the [unit] kinds query ran")
    map langUnitKinds [python, haskell, rust] `shouldBe` [[], [], []]
    unitsOf noQuery python Nothing "class Foo:\n    def bar(self):\n        pass\n\n\ndef f():\n    pass\n"
      `shouldReturn` Right [Named "Foo" (1, 3) 1, Named "Foo.bar" (2, 3) 2, Named "f" (6, 7) 6]
    unitsOf noQuery rust Nothing "struct A;\n\nimpl A {\n    fn new() -> A {\n        A\n    }\n}\n"
      `shouldReturn` Right [Named "A" (1, 1) 1, Named "A" (3, 7) 3, Named "A.new" (4, 6) 4]
  it "a Rust trait impl's methods are qualified by its type, as on main (review I3)" $ do
    let noQuery = querying (\_ _ _ -> error "the [unit] kinds query ran")
    Right ns <- unitsOf noQuery rust Nothing "trait T { fn f(&self); }\nstruct A;\nstruct B;\nimpl T for A {\n    fn f(&self) {}\n}\nimpl T for B {\n    fn f(&self) {}\n}\n"
    map namedName ns `shouldSatisfy` (\names -> "A.f" `elem` names && "B.f" `elem` names && "f" `notElem` names)
  it "Haskell instances are named by their head, so no name repeats (review I3)" $ do
    let noQuery = querying (\_ _ _ -> error "the [unit] kinds query ran")
    Right ns <- unitsOf noQuery haskell Nothing "class C a where\n  m :: a -> Int\n\ninstance C Int where\n  m x = x\n\ninstance C Bool where\n  m _ = 1\n"
    map namedName ns `shouldBe` ["C", "C.m", "C Int", "C Int.m", "C Bool", "C Bool.m"]
  it "names and qualifies python units" $ do
    r <- unitsOf (spans [(0, 5), (1, 2), (3, 4)]) python Nothing "class Foo:\n    def bar(self):\n        pass\n    def baz(self):\n        pass\n"
    fmap (map namedName) r `shouldBe` Right ["Foo", "Foo.bar", "Foo.baz"]
  it "a decorated def and its function_definition are one unit, def line after the decorator" $ do
    r <- unitsOf (spans [(0, 2), (1, 2)]) python Nothing "@cache\ndef f():\n    pass\n"
    r `shouldBe` Right [Named "f" (1, 3) 2]
  it "finds a method by its qualified name and a unique bare name" $ do
    let ns = [Named "Foo" (1, 6) 1, Named "Foo.bar" (2, 3) 2]
    fmap namedName (unitNamed "Foo.bar" ns) `shouldBe` Just "Foo.bar"
    fmap namedName (unitNamed "bar" ns) `shouldBe` Just "Foo.bar"
    unitNamed "nope" ns `shouldBe` Nothing
  it "a repeated name is ambiguous, said so; an ambiguous bare name too (review I3)" $ do
    unitLookup "f" [Named "f" (5, 6) 5, Named "f" (1, 2) 1] `shouldBe` Left "`f` names 2 units (lines 1, 5)"
    unitNamed "f" [Named "f" (5, 6) 5, Named "f" (1, 2) 1] `shouldBe` Nothing
    unitLookup "m" [Named "A.m" (2, 3) 2, Named "B.m" (5, 6) 5] `shouldBe` Left "`m` names 2 units (A.m, B.m)"
    unitLookup "nope" [Named "A.m" (2, 3) 2] `shouldBe` Left "no unit `nope`"
  it "a Rust struct and its inherent impl sharing a name: the anchor is the struct, as main (coordinator ruling)" $ do
    let noQuery = querying (\_ _ _ -> error "the [unit] kinds query ran")
        text = "struct A;\n\nimpl A {\n    fn new() -> A {\n        A\n    }\n}\n\nfn f() {}\nstruct B;\nfn f() {}\n"
    Right ks <- unitsWithKinds noQuery rust Nothing text
    fmap namedLines (unitLookupIn "A" ks) `shouldBe` Right (1, 1)
    fmap namedLines (unitLookupIn "A.new" ks) `shouldBe` Right (4, 6)
    either (T.isInfixOf "names 2 units") (const False) (unitLookupIn "f" ks) `shouldBe` True
  it "names haskell units" $ do
    unitNameIn haskell "data Catalog = Catalog" `shouldBe` Just "Catalog"
    unitNameIn haskell "instance Show Foo where" `shouldBe` Just "Show Foo"
    unitNameIn haskell "conjureIn top =" `shouldBe` Just "conjureIn"
  it "joins a haskell signature and its equations into one unit" $ do
    -- signature (0,0), its type's function node (0,0), two equations (1,1), (2,2)
    r <- unitsOf (spans [(0, 0), (0, 0), (1, 1), (2, 2)]) haskell Nothing "go :: Int -> Int\ngo 0 = 1\ngo n = n\n"
    r `shouldBe` Right [Named "go" (1, 3) 1]
  it "names haskell operators and multi-line signatures" $ do
    r <- unitsOf (spans [(0, 0), (1, 1)]) haskell Nothing "(<+>) :: A -> A -> A\na <+> b = b\n"
    -- 4b m2: from captures, an infix equation is its operator's, joined to its signature
    r `shouldBe` Right [Named "(<+>)" (1, 2) 1]
    r2 <- unitsOf (spans [(0, 2), (3, 3)]) haskell Nothing "run\n  :: Int\n  -> Int\nrun = id\n"
    r2 `shouldBe` Right [Named "run" (1, 4) 1]
  it "joins across blank lines, not across other code" $ do
    r <- unitsOf (spans [(0, 0), (2, 2), (3, 3), (4, 4)]) haskell Nothing "f :: Int\n\nf = 1\ng = 2\nf' = 3\n"
    fmap (map namedLines) r `shouldBe` Right [(1, 3), (4, 4), (5, 5)]
  it "names rust items by the item, and impl blocks by their type" $ do
    unitNameIn rust "pub(crate) async fn load(x: u8) {" `shouldBe` Just "load"
    unitNameIn rust "impl<T> Display for Wrapper<T> {" `shouldBe` Just "Wrapper"
    unitNameIn rust "impl Booking {" `shouldBe` Just "Booking"
    unitNameIn rust "impl<I: Iterator<Item = u8>> Foo for Bar {" `shouldBe` Just "Bar"
    unitNameIn rust "impl<T: Into<String>> Wrapper<T> {" `shouldBe` Just "Wrapper"
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
