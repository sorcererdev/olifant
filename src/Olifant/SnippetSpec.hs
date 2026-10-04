module Olifant.SnippetSpec (spec) where

import Data.Text (Text)
import Olifant.Snippet
import Test.Hspec

-- | A hole marker as rendering writes it (see "Olifant.Holes").
hole :: Text -> Text -> Text -> Text
hole name kind instruction = "\10214olifant-hole " <> name <> "|" <> kind <> "|" <> instruction <> "\10215"

spec :: Spec
spec = describe "Olifant.Snippet (spec §4.9)" $ do
  it "(b) yours holes: ⟨name⟩ in the ghost, numbered tab stops in the body" $ do
    let marked = "#[error(\"" <> hole "message" "text@yours" "what went wrong" <> "\")]\n" <> hole "Variant" "ident@yours" "the variant" <> ","
        s = snippetOf "src/a.rs" 3 marked (const Nothing)
    snGhost s `shouldBe` ["#[error(\"⟨message⟩\")]", "⟨Variant⟩,"]
    snBody s `shouldBe` "#[error(\"${1:message}\")]\n${2:Variant},$0"
    (snFile s, snLine s) `shouldBe` ("src/a.rs", 3)
  it "(c) a derived hole is pre-filled; a model hole shows its fill, or ⟨name⟩ unfilled" $ do
    let marked = hole "body" "block@derived" "pending" <> " " <> hole "status" "expr" "the status" <> " " <> hole "other" "expr" "x"
        s = snippetOf "a.hs" 1 marked (\n -> if n == "status" then Just "409" else Nothing)
    snGhost s `shouldBe` ["pending 409 ⟨other⟩"]
    snBody s `shouldBe` "${1:pending} ${2:409} ${3:other}$0"
  it "(d) escapes $, } and \\ in literal text and in tab stops' text (Review Focus 2)" $ do
    lspEscape "it \"x\" $ do" `shouldBe` "it \"x\" \\$ do"
    lspEscape "format!(\"{}\")" `shouldBe` "format!(\"{\\}\")"
    lspEscape "a\\nb" `shouldBe` "a\\\\nb"
    snBody (snippetOf "a.hs" 1 ("it \"" <> hole "b" "text@yours" "x" <> "\" $ do\n  " <> hole "body" "block@derived" "pure $ ()") (const Nothing))
      `shouldBe` "it \"${1:b}\" \\$ do\n  ${2:pure \\$ ()}$0"
  it "reads a hole's source from its marker kind" $ do
    holeSource "text@yours" `shouldBe` ("text", "yours")
    holeSource "expr" `shouldBe` ("expr", "model")
