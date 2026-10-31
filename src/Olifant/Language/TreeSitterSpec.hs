module Olifant.Language.TreeSitterSpec (spec) where

import Data.Either (isLeft)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Language.Captures (Cap (..), PNode (..))
import Olifant.Language.TreeSitter (capturesOf, linkedGrammars, queriesFor)
import Test.Hspec

rust :: Text -> [Cap]
rust src = either (error . T.unpack) snd (capturesOf "rust" (queriesFor "rust") src)

-- | The definitions of a text: (capture, @name of the same match).
defs :: [Cap] -> [(Text, Text)]
defs caps =
  [ (capName c, pText (capNode n))
  | c <- caps
  , "definition." `T.isPrefixOf` capName c
  , Just n <- [M.lookup (capMatch c) names]
  ]
  where
    names = M.fromList [(capMatch n, n) | n <- caps, capName n == "name"]

spec :: Spec
spec = describe "tree-sitter captures" $ do
  it "links the rust grammar" $
    linkedGrammars `shouldBe` ["rust"]
  it "a Rust file's definitions are captured" $ do
    src <- TIO.readFile "packs/rust-htmx/fixtures/app/billing/billing-service/src/service.rs"
    let ds = defs (rust src)
    ds `shouldContain` [("definition.class", "BillingService")]
    ds `shouldContain` [("definition.impl", "BillingService")]
  it "a trait impl is not an impl definition" $ do
    let ds = defs (rust "struct InMemoryRepo;\ntrait Repo {}\nimpl Repo for InMemoryRepo {\n    fn f(&self) {}\n}\n")
    [n | ("definition.impl", n) <- ds] `shouldBe` []
  it "a generic impl is named by its type" $
    [n | ("definition.impl", n) <- defs (rust "struct Store<T>(T);\nimpl<T> Store<T> {\n    fn get(&self) {}\n}\n")] `shouldBe` ["Store"]
  it "columns count characters, after non-ASCII text (line 1, character column 16)" $ do
    let fieldA caps = [pStart (capNode c) | c <- caps, pText (capNode c) == "a: u8"]
    fieldA (rust "// café\nstruct größe { a: u8 }\n") `shouldSatisfy` (\ps -> not (null ps) && all (== (1, 15)) ps)
  it "CRLF endings give the same lines and columns" $ do
    let lf = "// café\nstruct größe { a: u8 }\nfn f() {}\n"
        crlf = T.replace "\n" "\r\n" lf
        at caps = [(capName c, pStart (capNode c)) | c <- caps]
    at (rust crlf) `shouldBe` at (rust lf)
  it "a broken file still captures" $
    [n | (_, n) <- defs (rust "struct A { a: u8 }\nfn f( {\n")] `shouldContain` ["A"]
  it "an unlinked grammar is a Left naming it" $
    either (T.isInfixOf "go") (const False) (capturesOf "go" [] "package main\n") `shouldBe` True
  it "a query that does not compile is a Left naming the file and byte" $ do
    let r = capturesOf "rust" [("bad.scm", "(no_such_node) @x")] "fn f() {}\n"
    r `shouldSatisfy` isLeft
    either (\e -> "bad.scm" `T.isInfixOf` e && "byte" `T.isInfixOf` e) (const False) r `shouldBe` True
