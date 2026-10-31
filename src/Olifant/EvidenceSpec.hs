{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.EvidenceSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Evidence
import Olifant.Language
import Olifant.Language.AstGrep (astGrepSyntax)
import System.Directory (findExecutable, getTemporaryDirectory)
import System.FilePath ((</>))
import Test.Hspec

-- | A pack's real language file, over the shipped one.
packLanguage :: Text -> FilePath -> IO Language
packLanguage name pack = either (error . T.unpack) id . parseOverShipped name pack <$> TIO.readFile (pack <> "/languages/" <> T.unpack name <> ".toml")

-- | Ports over in-memory files with the real ast-grep, or pending without it.
withPorts :: [(FilePath, Text)] -> Maybe (Language -> [TestId] -> IO (Either Text TestRun)) -> (EvidencePorts -> Expectation) -> Expectation
withPorts files run k =
  findExecutable "ast-grep" >>= \case
    Nothing -> pendingWith "ast-grep is not installed (brew install ast-grep)"
    Just _ -> do
      tmp <- getTemporaryDirectory
      k (EvidencePorts (pure (map fst files)) (pure . (`lookup` files)) (astGrepSyntax (tmp </> "olifant-test-cache")) run)

source, test :: Text
source = "def add(a: int, b: int) -> int:\n    return a + b\n"
test = "from a import add\n\n\ndef test_add() -> None:\n    assert add(1, 2) == 3\n"

isProven, isDisproven :: Proof -> Bool
isProven = \case Proven _ -> True; _ -> False
isDisproven = \case Disproven _ -> True; _ -> False

proofText :: Proof -> Text
proofText = \case Proven t -> t; Unproven t -> t; Disproven t -> t

spec :: Spec
spec = describe "Olifant.Evidence" $ do
  py <- runIO (packLanguage "python" "packs/python-htmx")
  rust <- runIO (packLanguage "rust" "packs/rust-htmx")
  let pair = [("src/a.py", source), ("src/a__test.py", test)]
  it "python: units, test units, holes proven on a two-file sample" $
    withPorts pair Nothing $ \ports -> do
      ev <- evidenceFor ports py
      evUnits ev `shouldBe` Proven "2 found in 2 files (e.g. src/a.py:1 add)"
      evTestUnits ev `shouldBe` Proven "1 found in 1 test files (e.g. src/a__test.py:4 test_add)"
      evHoles ev `shouldSatisfy` isProven
      proofText (evHoles ev) `shouldSatisfy` T.isSuffixOf "wrappers parse with their placeholders"
  it "a sample is the first 40 files in path order; no test file is not seen, a warning, not a failure (Review Focus 4)" $ do
    let many = [("src/m" <> (if i < 10 then "0" else "") <> show i <> ".py", "def f" <> T.pack (show i) <> "() -> None:\n    pass\n") | i <- [0 :: Int .. 99]]
    sampleOf py (reverse (map fst many) ++ ["README.md"]) `shouldBe` take 40 (map fst many)
    withPorts many Nothing $ \ports -> do
      ev <- evidenceFor ports py
      evTestUnits ev `shouldBe` Unproven "not seen (no test file in the 40 sampled)"
      evUnits ev `shouldBe` Proven "40 found in 40 files (e.g. src/m00.py:1 f0)"
  it "a test-units rule that matches nothing shows the line it missed" $ do
    let never = py {langTest = (\td -> td {testUnitRules = [UnitRule "^NEVER" Nothing Nothing]}) <$> langTest py}
    withPorts pair Nothing $ \ports -> do
      ev <- evidenceFor ports never
      evTestUnits ev `shouldBe` Disproven "0 found; src/a__test.py:4 is `def test_add() -> None:`"
  it "a stmt wrapper that does not parse is Disproven naming [holes.stmt]" $ do
    let broken = py {langHoles = [(k, if k == "stmt" then h {kindWrap = "def (:\n{= fill|indent =}\n"} else h) | (k, h) <- langHoles py]}
    withPorts pair Nothing $ \ports -> do
      ev <- evidenceFor ports broken
      evHoles ev `shouldSatisfy` isDisproven
      proofText (evHoles ev) `shouldSatisfy` T.isPrefixOf "[holes.stmt]: def (: does not parse"
  it "rust anchors: end of struct proven by a sample unit; literal NAME in FUNCTION not tried" $ do
    let dir = "packs/rust-htmx/fixtures/app/billing/billing-service/src/"
    files <- mapM (\f -> (f,) <$> TIO.readFile (dir <> f)) ["service.rs", "models.rs", "refund_invoice.rs"]
    withPorts [("src/" <> f, t) | (f, t) <- files] Nothing $ \ports -> do
      ev <- evidenceFor ports rust
      evAnchors ev `shouldSatisfy` isProven
      proofText (evAnchors ev) `shouldSatisfy` T.isInfixOf "end of function "
      proofText (evAnchors ev) `shouldSatisfy` T.isInfixOf "not tried: "
      proofText (evAnchors ev) `shouldSatisfy` T.isInfixOf "literal NAME in FUNCTION"
  it "rust anchors: generic phrases proven on the sample" $ do
    let a = T.unlines ["use std::fmt;", "", "pub struct X {", "    a: u8,", "}", "", "fn f() {", "    let _ = 1;", "}"]
    withPorts [("src/a.rs", a)] Nothing $ \ports -> do
      ev <- evidenceFor ports rust
      evAnchors ev `shouldSatisfy` isProven
      T.splitOn " · " (fst (T.breakOn "; " (proofText (evAnchors ev))))
        `shouldBe` ["end of function f (src/a.rs:9)", "end of class X (src/a.rs:5)", "end of imports (src/a.rs:2)"]
  it "a linked grammar with no [targets.*] is proven by the generic phrases alone" $ do
    let a = T.unlines ["use std::fmt;", "", "fn f() {", "    let _ = 1;", "}"]
    withPorts [("src/a.rs", a)] Nothing $ \ports -> do
      ev <- evidenceFor ports rust {langTargets = []}
      evAnchors ev `shouldBe` Proven "end of function f (src/a.rs:5) · end of imports (src/a.rs:2); ✗ class NAME: no unit in the sample to try"
  it "tests: Unproven without a runner; with a fake runner, ran …, 3 results parsed" $ do
    withPorts pair Nothing $ \ports -> do
      ev <- evidenceFor ports py
      evTests ev `shouldBe` Unproven "not run (olifant doctor --run)"
    let fake _ ids = pure (Right (TestRun [i <> "::a" | i <- ids] [(i <> "::b", Failure Nothing "x" "") | i <- ids] Nothing))
        threeResults _ ids = pure (Right (TestRun [i <> "::" <> n | i <- ids, n <- ["a", "b", "c"]] [] Nothing))
    withPorts pair (Just threeResults) $ \ports -> do
      ev <- evidenceFor ports py
      evTests ev `shouldBe` Proven "ran uv on src/a__test.py, 3 results parsed (pytest)"
    withPorts pair (Just fake) $ \ports -> do
      ev <- evidenceFor ports py
      evTests ev `shouldBe` Proven "ran uv on src/a__test.py, 2 results parsed (pytest)"
    withPorts pair (Just (\_ _ -> pure (Right (TestRun [] [] (Just "no tests ran"))))) $ \ports -> do
      ev <- evidenceFor ports py
      evTests ev `shouldBe` Disproven "src/a__test.py: no tests ran"
