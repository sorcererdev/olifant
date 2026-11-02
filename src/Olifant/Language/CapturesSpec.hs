module Olifant.Language.CapturesSpec (spec) where

import Data.Functor.Identity (Identity (..))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Anchor (Miss (..), RenderedAnchor (..), Spacing (..), locLine, locIndent, locOpen, locSpacing, locateContainers, resolveLayers)
import Olifant.Language
import Olifant.Language.Captures (anchorFromCaptures)
import Olifant.Language.TreeSitter (capturesOf, queriesFor)
import Test.Hspec

-- | Rust with no declared targets, so every phrase is the generic rule's,
-- over a pure syntax (tree-sitter in process; ast-grep never runs).
rust :: Language
rust = (shippedLanguageNow "rust") {langTargets = []}

syn :: Syntax Identity
syn = (querying (\_ _ _ -> Identity (Right []))) {anchorSyntax = treeSitterAnchors}

site :: Text -> Text -> Either [Text] (Int, Text)
site file p = either (Left . map missReason) (\l -> Right (locLine l, locIndent l)) (runIdentity (resolveLayers syn (Just rust) Nothing [PhraseAnchor p] file))

containers :: Text -> [Text] -> Maybe (Either Text [(Match, [Match])])
containers file ws = either (error . T.unpack) (\(root, caps) -> anchorFromCaptures False root caps ws) (capturesOf "rust" (queriesFor "rust") file)

lineOf :: Text -> Text -> Int
lineOf file needle = case [i | (i, l) <- zip [0 ..] (T.lines file), needle `T.isInfixOf` l] of
  i : _ -> i
  [] -> error ("no line holds " <> T.unpack needle)

tests :: Text
tests = T.unlines ["use std::sync::Arc;", "use crate::a::B;", "", "pub struct Empty {}", "", "pub enum E {", "    One,", "    // Two,", "}", "", "pub fn run(x: i64) -> i64 {", "    let y = x;", "    y", "}", "", "#[cfg(test)]", "mod tests {", "    use super::*;", "", "    #[test]", "    fn it_runs() {", "        assert_eq!(run(1), 1);", "    }", "}"]

service :: IO Text
service = TIO.readFile "packs/rust-htmx/fixtures/app/billing/billing-service/src/service.rs"

spec :: Spec
spec = describe "the generic anchor rule (captures)" $ do
  it "a name given as an expression matches whole, as a declared target's `^NAME$` did (python-htmx's `parameters in [a-z_]+_routes`)" $ do
    let file = T.unlines ["fn bookings_routes(", "    a: u8,", ")", "{", "}", "", "fn other(b: u8) {}"]
    fmap (fmap length) (containers file ["parameters", "in", "[a-z_]+_routes"]) `shouldBe` Just (Right 1)
    fmap (fmap length) (containers file ["parameters", "in", "[a-z]+"]) `shouldBe` Just (Right 1)
  it "end of mod tests goes after the test function, at its column" $
    site tests "end of mod tests" `shouldBe` Right (23, "    ")
  it "end of enum ignores a trailing comment" $
    site tests "end of enum E" `shouldBe` Right (7, "    ")
  it "end of function goes after the tail expression" $
    site tests "end of function run" `shouldBe` Right (13, "    ")
  it "end of imports goes after the last top-level use" $
    site tests "end of imports" `shouldBe` Right (2, "")
  it "end of struct opens an empty struct" $
    fmap locOpen (runIdentity (resolveLayers syn (Just rust) Nothing [PhraseAnchor "end of struct Empty"] tests)) `shouldSatisfy` either (const False) isJust
  it "end of class is ambiguous where a struct and its impl share a name" $ do
    file <- service
    site file "end of class BillingService" `shouldSatisfy` either (any ("matches 2 places" `T.isInfixOf`)) (const False)
  it "end of enum on the fixture's failure enum" $ do
    file <- TIO.readFile "packs/rust-htmx/fixtures/app/billing/billing-service/src/refund_invoice.rs"
    fmap fst (site file "end of enum RefundInvoiceFailure") `shouldBe` Right (lineOf file "Unknown(String)," + 1)
  it "class names both the struct and the impl; struct and impl one each" $ do
    file <- service
    fmap length <$> containers file ["class", "BillingService"] `shouldBe` Just (Right 2)
    fmap (either Left oneContainer) (containers file ["class", "BillingService"]) `shouldSatisfy` maybe False (either ("matches 2 places" `T.isInfixOf`) (const False))
    fmap length <$> containers file ["struct", "BillingService"] `shouldBe` Just (Right 1)
    fmap length <$> containers file ["impl", "BillingService"] `shouldBe` Just (Right 1)
  it "a name two definitions share: locateContainers returns both" $ do
    file <- service
    runIdentity (locateContainers syn rust ["class", "BillingService"] file)
      `shouldBe` Right [(10, 14), (16, 40)]
  it "kind words narrow by node type: mod tests finds the mod_item, not a struct tests" $ do
    let file = T.unlines ["struct tests {", "    a: u8,", "}", "", "mod tests {", "    fn f() {}", "}"]
    site file "end of mod tests" `shouldBe` Right (6, "    ")
    site file "end of struct tests" `shouldBe` Right (2, "    ")
  describe "bodies of more than 64 children (final review, Critical 1)" $ do
    it "an impl of 20 documented methods: end of impl goes after the last one" $ do
      let method i = ["    /// Method " <> T.pack (show i) <> ".", "    ///", "    /// It does nothing.", "    pub fn m" <> T.pack (show i) <> "(&self) {}", ""]
          file = T.unlines (["struct D;", "", "impl D {"] ++ concatMap method [1 .. 20 :: Int] ++ ["}"])
      site file "end of impl D" `shouldBe` Right (lineOf file "pub fn m20(" + 1, "    ")
    it "a fn with 80 statements: end of function goes after the last one" $ do
      let file = T.unlines (["fn big() {"] ++ ["    let x" <> T.pack (show i) <> " = " <> T.pack (show i) <> ";" | i <- [1 .. 80 :: Int]] ++ ["}"])
      site file "end of function big" `shouldBe` Right (81, "    ")
    it "a struct with 40 fields: end of struct goes after the last one" $ do
      let file = T.unlines (["struct Big {"] ++ ["    f" <> T.pack (show i) <> ": u8," | i <- [1 .. 40 :: Int]] ++ ["}"])
      site file "end of struct Big" `shouldBe` Right (41, "    ")
  it "attributed methods keep their blank line: an attribute goes with the item it decorates (final review, Important 2)" $ do
    let file = T.unlines ["struct S;", "", "impl S {", "    #[must_use]", "    fn a(&self) -> u8 {", "        1", "    }", "", "    #[must_use]", "    fn b(&self) -> u8 {", "        2", "    }", "}"]
        spacing p = fmap locSpacing (runIdentity (resolveLayers syn (Just rust) Nothing [PhraseAnchor p] file))
    spacing "end of impl S" `shouldBe` Right BlankBefore
    site file "end of impl S" `shouldBe` Right (12, "    ")
    -- start of goes before the first item's attribute, not between it and its fn
    site file "start of impl S" `shouldBe` Right (3, "    ")
  it "a phrase that is not generic is not the rule's" $
    containers tests ["literal", "in", "new"] `shouldBe` Nothing
