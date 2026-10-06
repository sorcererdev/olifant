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
  it "places a multi-line fill as the file gets it: continuation lines indented like the marker's line (pack-opens)" $ do
    let s = snippetOf "a.py" 32 ("        " <> hole "body" "block" "the body") (\n -> if n == "body" then Just "        x = 1\n        if x:\n            y = 2" else Nothing)
    snGhost s `shouldBe` ["        x = 1", "        if x:", "            y = 2"]
    snBody s `shouldBe` "        ${1:x = 1\n        if x:\n            y = 2}$0"
  it "reads a hole's source from its marker kind" $ do
    holeSource "text@yours" `shouldBe` ("text", "yours")
    holeSource "expr" `shouldBe` ("expr", "model")
