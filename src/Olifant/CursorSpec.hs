{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.CursorSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Olifant.Cursor
import Olifant.Generator (EditWith (..), Generator, GeneratorId (..), GeneratorWith (..), SourceRef (..), parseGenerator)
import Olifant.Template (Value (..), render)
import Olifant.Template.Syntax (Template (..))
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Cursor (snippets at the cursor, spec §5.3)" $ do
  describe "cursorIndent" $ do
    it "(c) an empty line takes the block's indentation: the line above's, one unit deeper when it opens a block (Review Focus 1)" $ do
      cursorIndent "    " ["enum E {", "    A,", ""] 3 `shouldBe` "    "
      cursorIndent "    " ["enum E {", ""] 2 `shouldBe` "    "
      cursorIndent "  " ["describe \"x\" $ do", ""] 2 `shouldBe` "  "
      cursorIndent "    " ["    match x {", "", ""] 3 `shouldBe` "        "
      cursorIndent "    " [""] 1 `shouldBe` ""
    it "a line with text, or only whitespace, keeps its own" $ do
      cursorIndent "    " ["fn f() {", "        it \""] 2 `shouldBe` "        "
      cursorIndent "    " ["fn f() {", "  "] 2 `shouldBe` "  "
  describe "indentTemplate" $
    it "indents every non-empty line of what the template renders" $ do
      let Right g = load "text = \"a {= x =}\\nb\\n\\nc\""
          [Cursor t] = genEdits g
      render (M.fromList [("x", VText "1")]) (indentTemplate "  " t) `shouldBe` Right "  a 1\n  b\n\n  c"
  describe "atCursor" $
    it "makes the cursor edit a replacement of the cursor's line, which must still read as it does" $ do
      let Right g = load "text = \"x\""
          placed = atCursor "    " "src/a.rs" 2 ["enum E {", "", "}"] g
      case genEdits placed of
        [Replace target range old _] -> do
          templateSource target `shouldBe` "src/a.rs"
          range `shouldBe` (2, 2)
          old `shouldBe` ""
        other -> expectationFailure (show other)
  where
    load :: T.Text -> Either T.Text Generator
    load edit = parseGenerator (GeneratorId "p/s") (T.unlines ["description = \"d\"", "language = \"x\"", "[[edits]]", "cursor = true", edit]) >>= traverse inline
    inline = \case
      InlineText t -> Right t
      other -> Left (T.pack (show other))
