{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.AnchorSpec (spec) where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Functor.Identity (Identity (..))
import Data.List (isInfixOf)
import Data.Map.Strict qualified as M
import Data.Text (Text)

import Data.Text qualified as T
import Hedgehog (Gen, forAll, tripping, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Olifant.Anchor
import Olifant.Language
import Olifant.Query (AstQuery (..), AstSelect (..), QueryFailure (..))
import Olifant.Template (Value (..), parseTemplate)
import Olifant.Provider (runProvider)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = phraseSpec >> resolveSpec

phraseSpec :: Spec
phraseSpec = describe "parsePhrase" $ do
  it "reads a language target" $
    parsePhrase "end of case in update" `shouldBe` Right (Phrase EndOf (NamedTarget ["case", "in", "update"]))
  it "reads a marker, keeping its text exactly" $
    parsePhrase "before marker -- olifant:pages" `shouldBe` Right (Phrase Before (MarkerTarget "-- olifant:pages"))
  it "reads line targets, keeping the regex exactly" $ do
    parsePhrase "after last line matching ^import Page\\." `shouldBe` Right (Phrase After (LineTarget LastLine "^import Page\\."))
    parsePhrase "before first line matching ^ +Olifant" `shouldBe` Right (Phrase Before (LineTarget FirstLine "^ +Olifant"))
  it "ignores surrounding whitespace" $
    parsePhrase "  start of type Msg " `shouldBe` Right (Phrase StartOf (NamedTarget ["type", "Msg"]))
  it "rejects a missing position, suggesting the closest one" $
    parsePhrase "ending of type Msg" `shouldSatisfy` either ("did you mean `end of`" `T.isInfixOf`) (const False)
  it "lists the valid forms when nothing is close" $
    parsePhrase "somewhere" `shouldSatisfy` either ("start of, end of, before or after" `T.isInfixOf`) (const False)
  it "rejects an empty target" $ do
    parsePhrase "end of" `shouldSatisfy` either ("needs a target" `T.isInfixOf`) (const False)
    parsePhrase "before marker " `shouldSatisfy` either ("marker" `T.isInfixOf`) (const False)
  it "round-trips through showPhrase" $ hedgehog $ do
    p <- forAll genPhrase
    tripping p showPhrase parsePhrase

genPhrase :: Gen Phrase
genPhrase = Phrase <$> Gen.enumBounded <*> Gen.choice [marker, line, named]
  where
    word = Gen.text (Range.linear 1 8) Gen.alphaNum
    marker = MarkerTarget . T.unwords <$> Gen.list (Range.linear 1 3) word
    line = LineTarget <$> Gen.element [FirstLine, LastLine] <*> (("^" <>) <$> word)
    named = NamedTarget <$> Gen.filter (\ws -> take 1 ws `notElem` [["marker"], ["first"], ["last"]]) (Gen.list (Range.linear 1 3) word)

-- | A fake syntax port: the container rule matches lines 2-8; the items rule
-- (recognised by its "all") returns two branches plus one nested in the first.
fakeSyntax :: Syntax Identity
fakeSyntax = querying $ \_ rule _ -> Identity . Right $
  let json = BL.unpack (encode rule)
   in if "\"all\"" `isInfixOf` json
        then [Match (3, 8) (5, 20) "A -> …" (0, 0), Match (4, 12) (4, 20) "nested" (0, 0), Match (7, 8) (8, 20) "B -> …" (0, 0)]
        else [Match (2, 4) (8, 20) "case x of …" (0, 0)]

caseTarget :: Language
caseTarget = lang
  where
    Right lang =
      parseLanguage "elm" "/p" $
        T.unlines
          [ "files = [\"**/*.elm\"]", "comment = \"--\"", "grammar = \"elm\""
          , "[targets.case_in]", "phrase = \"case in FUNCTION\""
          , "container = { kind = \"case_of_expr\" }", "items = { kind = \"case_of_branch\" }"
          , "example = \"end of case in update\""
          ]

source :: Text
source = T.unlines ["update msg =", "  -- lead", "    case x of", "        A ->", "            1", "", "", "        B ->", "            2", "tail"]

-- | 'fakeSyntax' in IO, running providers for real.
providingSyntax :: Syntax IO
providingSyntax = (querying (\l r t -> pure (runIdentity (querySyntax fakeSyntax l r t)))) {runSlotProvider = runProvider}

-- | 'caseTarget' with its anchors answered by a script.
withAnchors :: FilePath -> IO Language
withAnchors script = do
  here <- getCurrentDirectory
  pure caseTarget {langAnchorProvider = Just (Provider ["sh", T.pack ("testdata/providers" </> script)] (Just (here </> "src/Olifant")))}

-- | Rust with its declared targets, and with none (every phrase generic).
rustDeclared, rustGeneric :: Language
rustGeneric = (shippedLanguageNow "rust") {langTargets = []}
rustDeclared = rustGeneric {langTargets = langTargets structLang}
  where
    Right structLang =
      parseLanguage "rust" "/p" $
        T.unlines
          [ "files = [\"**/*.rs\"]", "comment = \"//\"", "grammar = \"rust\""
          , "[targets.struct]", "phrase = \"struct NAME\""
          , "container = { kind = \"field_declaration_list\" }", "items = { kind = \"field_declaration\" }"
          , "example = \"end of struct S\""
          ]

-- | A pure syntax: no ast-grep, the generic rule in process.
tsSyntax :: Syntax Identity
tsSyntax = (querying (\_ _ _ -> Identity (Right []))) {anchorSyntax = treeSitterAnchors}

-- | Elm through a pack's vendored grammar (not linked in), with a declared
-- `type NAME`.
elmSource :: Language
elmSource = caseTarget {langGrammar = GrammarSource "/p/languages/grammar/elm", langTargets = langTargets caseTarget ++ langTargets typeLang}
  where
    Right typeLang =
      parseLanguage "elm" "/p" $
        T.unlines
          [ "files = [\"**/*.elm\"]", "comment = \"--\"", "grammar = \"elm\""
          , "[targets.type]", "phrase = \"type NAME\""
          , "container = { kind = \"type_declaration\" }", "items = { kind = \"union_variant\" }"
          , "example = \"end of type Msg\""
          ]

genericSpec :: Spec
genericSpec = describe "the generic rule (4b m2)" $ do
  let at lang p file = runIdentity (resolveLayers tsSyntax (Just lang) Nothing [PhraseAnchor p] file)
  it "a half-typed file: a definition that still parses resolves; the rest is a miss naming the phrase" $ do
    let file = T.unlines ["struct A {", "    a: u8,", "}", "", "fn f( {", "    let x = "]
    fmap locLine (at rustGeneric "end of struct A" file) `shouldBe` Right 2
    either (map missAnchor) (const []) (at rustGeneric "end of function g" file) `shouldBe` ["end of function g"]
  it "non-ASCII before the anchor: placed at the field's column" $ do
    let file = T.unlines ["// café", "struct größe {", "    a: u8,", "}"]
    fmap (\l -> (locLine l, locIndent l)) (at rustGeneric "end of struct größe" file) `shouldBe` Right (3, "    ")
    -- a one-line struct grows at the same (character) columns as an ASCII one
    let grown name = let one = T.unlines ["// cafe", "struct " <> name <> " { a: u8 }"]
                         Right loc = at rustGeneric ("end of struct " <> name) (if name == "grosse" then one else T.replace "cafe" "café" one)
                      in T.lines (snd (insertLines loc "b: u8" (if name == "grosse" then one else T.replace "cafe" "café" one))) !! 1
    grown "größe" `shouldBe` T.replace "grosse" "größe" (grown "grosse")
    grown "größe" `shouldSatisfy` ("a: u8, b: u8" `T.isInfixOf`)
  it "CRLF text places as LF does" $ do
    let file = T.unlines ["// café", "struct größe {", "    a: u8,", "}"]
    fmap locLine (at rustGeneric "end of struct größe" (T.replace "\n" "\r\n" file)) `shouldBe` Right 3
  it "a grammar not linked in: declared targets as today; a generic phrase is an unknown target" $ do
    fmap locLine (runIdentity (resolveLayers fakeSyntax (Just elmSource) Nothing [PhraseAnchor "end of case in update"] source)) `shouldBe` Right 9
    let Left [m] = runIdentity (resolveLayers fakeSyntax {anchorSyntax = treeSitterAnchors} (Just elmSource) Nothing [PhraseAnchor "end of function f"] source)
    missReason m `shouldSatisfy` ("unknown target `function f`" `T.isInfixOf`)
    missReason m `shouldNotSatisfy` ("class NAME" `T.isInfixOf`)
    let Left [typo] = runIdentity (resolveLayers fakeSyntax {anchorSyntax = treeSitterAnchors} (Just elmSource) Nothing [PhraseAnchor "end of typ Msg"] source)
    missReason typo `shouldSatisfy` ("did you mean `end of type Msg`" `T.isInfixOf`)
  it "a declared target wins over the generic rule" $ do
    -- fakeSyntax's container is lines 2-8 and its last item ends on line 8
    let file = T.unlines ["struct S {", "    a: u8,", "}", "", "", "", "", "", "", ""]
    fmap locLine (runIdentity (resolveLayers fakeSyntax {anchorSyntax = treeSitterAnchors} (Just rustDeclared) Nothing [PhraseAnchor "end of struct S"] file)) `shouldBe` Right 9
    fmap locLine (at rustGeneric "end of struct S" file) `shouldBe` Right 2
  it "a generic phrase with no definition is a miss, and the provider is not asked" $ do
    here <- getCurrentDirectory
    let lang = rustGeneric {langAnchorProvider = Just (Provider ["sh", T.pack ("testdata/providers" </> "anchor.sh")] (Just (here </> "src/Olifant")))}
        syntax = providingSyntax {anchorSyntax = treeSitterAnchors}
        file = T.unlines ["struct A {", "    a: u8,", "}", "", ""]
    resolveLayers syntax (Just lang) Nothing [PhraseAnchor "end of struct Nope"] file
      `shouldReturn` Left [Miss "end of struct Nope" "no `struct Nope` in this file" False]
    -- a kind word that is no node type is not the engine's: the provider answers
    fmap locLine <$> resolveLayers syntax (Just lang) Nothing [PhraseAnchor "end of the imports"] file `shouldReturn` Right 2

resolveSpec :: Spec
resolveSpec = do
  genericSpec
  describe "an anchors provider (4b m1)" $ do
    it "a phrase no target knows goes to the anchors provider" $ do
      lang <- withAnchors "anchor.sh"
      Right loc <- resolveLayers providingSyntax (Just lang) Nothing [PhraseAnchor "end of the imports"] source
      (locLine loc, locIndent loc, locSpacing loc) `shouldBe` (2, "    ", NoSpacing)
      let (_, new) = insertLines loc "snippet" source
      take 4 (T.lines new) `shouldBe` ["update msg =", "  -- lead", "    snippet", "    case x of"]
    it "a declared target is never sent to the provider" $ do
      lang <- withAnchors "fails.sh"
      fmap locLine <$> resolveLayers providingSyntax (Just lang) Nothing [PhraseAnchor "end of case in update"] source `shouldReturn` Right 9
    it "a provider that fails is a miss with its reason, not a missing tool" $ do
      lang <- withAnchors "fails.sh"
      resolveLayers providingSyntax (Just lang) Nothing [PhraseAnchor "end of the imports"] source
        `shouldReturn` Left [Miss "end of the imports" "the anchors provider: no grammar" False]
    it "without a runner (noSyntax, a plain querying fake) the provider is a miss" $ do
      lang <- withAnchors "anchor.sh"
      resolveLayers (querying (\_ _ _ -> pure (Right []))) (Just lang) Nothing [PhraseAnchor "end of the imports"] source
        `shouldReturn` Left [Miss "end of the imports" "the anchors provider: no provider runner" False]
  describe "resolveLayers" $ do
    let resolve anchors = runIdentity (resolveLayers fakeSyntax (Just caseTarget) Nothing anchors source)
    it "puts `end of` after the last top-level item, at its indentation" $
      fmap (\l -> (locLine l, locIndent l, locUsed l)) (resolve [PhraseAnchor "end of case in update"]) `shouldBe` Right (9, "        ", "end of case in update")
    it "puts `start of` before the first item" $
      fmap locLine (resolve [PhraseAnchor "start of case in update"]) `shouldBe` Right 3
    it "puts `after` after the whole container, at the container's indentation" $
      fmap (\l -> (locLine l, locIndent l)) (resolve [PhraseAnchor "after case in update"]) `shouldBe` Right (9, "    ")
    it "resolves line targets with before/after" $ do
      fmap locLine (resolve [PhraseAnchor "after last line matching ^ +[AB] ->"]) `shouldBe` Right 8
      fmap locLine (resolve [PhraseAnchor "before first line matching ^tail"]) `shouldBe` Right 9
    it "rejects start of/end of on a line target" $
      resolve [PhraseAnchor "end of last line matching x"] `shouldSatisfy` either (any (("before" `T.isInfixOf`) . missReason)) (const False)
    it "falls through layers and reports which one resolved" $
      fmap locUsed (resolve [PhraseAnchor "end of type Nope", PhraseAnchor "after first line matching ^tail"]) `shouldBe` Right "after first line matching ^tail"
    it "reports every layer's miss when nothing resolves" $
      resolve [PhraseAnchor "end of type Nope", PhraseAnchor "before marker -- none"] `shouldSatisfy` either ((== 2) . length) (const False)
    it "suggests a known target for an unknown one" $
      resolve [PhraseAnchor "end of cases in update"] `shouldSatisfy` either (any (("did you mean `end of case in update`" `T.isInfixOf`) . missReason)) (const False)
    it "misses (flagged as a tool problem) when the syntax tool is missing" $
      runIdentity (resolveLayers noSyntax (Just caseTarget) Nothing [PhraseAnchor "end of case in update"] source) `shouldSatisfy` either (all missTool) (const False)
    it "resolves a marker without any tools" $
      fmap locLine (runIdentity (resolveLayers noSyntax Nothing Nothing [PhraseAnchor "before marker -- lead"] source)) `shouldBe` Right 1
    it "needs an adapter for a named target" $
      runIdentity (resolveLayers noSyntax Nothing Nothing [PhraseAnchor "end of type Msg"] source) `shouldSatisfy` either (any (("no language adapter" `T.isInfixOf`) . missReason)) (const False)
  describe "resolveLayers (items sharing a line with their container)" $ do
    -- `[ a, b ]` on one line: the container is line 1, cols 4-12; items a and b.
    let inline = querying $ \_ rule _ -> Identity . Right $
          if "\"all\"" `isInfixOf` BL.unpack (encode rule)
            then [Match (1, 6) (1, 7) "a" (0, 0), Match (1, 9) (1, 10) "b" (0, 0)]
            else [Match (1, 4) (1, 12) "[ a, b ]" (0, 0)]
        src = "x =\n    [ a, b ]\ny = 2\n"
        resolve p = runIdentity (resolveLayers inline (Just caseTarget) Nothing [PhraseAnchor p] src)
    it "misses `end of` when the last item does not end its line (review)" $
      resolve "end of case in x" `shouldSatisfy` either (any (("whole lines" `T.isInfixOf`) . missReason)) (const False)
    it "misses `start of` when the first item does not start its line (review)" $
      resolve "start of case in x" `shouldSatisfy` either (any (("whole lines" `T.isInfixOf`) . missReason)) (const False)
    it "still places `after` a container that ends its line" $
      fmap locLine (resolve "after case in x") `shouldBe` Right 2
  describe "astQueries" $ do
    let files = M.fromList [("src/A.elm", source), ("src/B.elm", "module B\n")]
        reader p = Identity (M.lookup p files)
    it "gives a record per item: text, path, line and each fields" $
      fmap (map (\r -> (M.lookup "path" r, M.lookup "line" r, M.lookup "head" r))) (runIdentity (astQueries fakeSyntax [caseTarget] (AstQuery "src/A.elm" (ItemsOf "case in update") [("head", "^([A-Z]+)")]) (M.keys files) reader))
        `shouldBe` Right [(Just (VText "src/A.elm"), Just (VInt 4), Just (VText "A")), (Just (VText "src/A.elm"), Just (VInt 8), Just (VText "B"))]
    it "gives no records for a file without the target (Review Focus 1)" $
      fmap length (runIdentity (astQueries (querying (\_ _ _ -> Identity (Right []))) [caseTarget] (AstQuery "src/B.elm" (ItemsOf "case in update") []) (M.keys files) reader))
        `shouldBe` Right 0
    it "is an error when the target appears twice in a file" $
      runIdentity (astQueries (querying (\_ _ _ -> Identity (Right [Match (0, 0) (1, 0) "a" (0, 0), Match (2, 0) (3, 0) "b" (0, 0)]))) [caseTarget] (AstQuery "src/A.elm" (ItemsOf "case in update") []) (M.keys files) reader)
        `shouldSatisfy` either (("src/A.elm" `T.isInfixOf`) . failureWhy) (const False)
    it "rejects an unknown target kind even when the glob matches no file" $
      runIdentity (astQueries fakeSyntax [caseTarget] (AstQuery "nowhere/*.elm" (ItemsOf "typo in update") []) (M.keys files) reader)
        `shouldSatisfy` either (\e -> "unknown target `typo in update`" `T.isPrefixOf` failureWhy e) (const False)
    it "skips files without a language adapter when the glob has a wildcard, but not a named file" $ do
      let files' = M.insert "src/data.json" "{}" files
          reader' p = Identity (M.lookup p files')
      fmap length (runIdentity (astQueries (querying (\_ _ _ -> Identity (Right []))) [caseTarget] (AstQuery "src/*" (ItemsOf "case in update") []) (M.keys files') reader'))
        `shouldBe` Right 0
      runIdentity (astQueries fakeSyntax [caseTarget] (AstQuery "src/data.json" (ItemsOf "case in update") []) (M.keys files') reader')
        `shouldSatisfy` either (("no language adapter for src/data.json" `T.isInfixOf`) . failureWhy) (const False)
    it "names the query when a raw rule's template fails" $ do
      let Right tpl = parseTemplate "{= nope =}"
      runIdentity (astQueries fakeSyntax [caseTarget] (AstQuery "src/A.elm" (RawRule (RObject [("regex", RText tpl)])) []) (M.keys files) reader)
        `shouldSatisfy` either (("ast src/A.elm (rule)" `T.isInfixOf`) . failureWhy) (const False)
    it "drops the CR of a CRLF file from text and each fields" $
      fmap (map (\r -> (M.lookup "text" r, M.lookup "rhs" r))) (runIdentity (astQueries (querying (\_ _ _ -> Identity (Right [Match (0, 0) (1, 5) "A ->\r\n    a" (0, 0)]))) [caseTarget] (AstQuery "src/A.elm" (RawRule (RObject [("kind", RLit "x")])) [("rhs", "->(.*)")]) (M.keys files) reader))
        `shouldBe` Right [(Just (VText "A ->\n    a"), Just (VText ""))]
  describe "spacing copied from neighbouring items" $ do
    it "end of puts a blank line before the text when the last two items are separated by one" $
      fmap locSpacing (runIdentity (resolveLayers fakeSyntax (Just caseTarget) Nothing [PhraseAnchor "end of case in update"] source)) `shouldBe` Right BlankBefore
    it "start of puts a blank line after the text when the first two items are separated by one" $
      fmap locSpacing (runIdentity (resolveLayers fakeSyntax (Just caseTarget) Nothing [PhraseAnchor "start of case in update"] source)) `shouldBe` Right BlankAfter
    it "copies nothing from a one-item container (Review Focus 4)" $ do
      let one = querying $ \_ rule _ -> Identity . Right $
            if "\"all\"" `isInfixOf` BL.unpack (encode rule) then [Match (3, 8) (4, 20) "A -> …" (0, 0)] else [Match (2, 4) (4, 20) "case" (0, 0)]
      fmap locSpacing (runIdentity (resolveLayers one (Just caseTarget) Nothing [PhraseAnchor "end of case in update"] source)) `shouldBe` Right NoSpacing
    it "insertLines adds the blank line unless the text already has it" $ do
      insertLines (Located 1 "" "" "x" BlankBefore Nothing "") "b" "a\nc\n" `shouldBe` (2, "a\n\nb\nc\n")
      insertLines (Located 1 "" "" "x" BlankBefore Nothing "") "\nb" "a\nc\n" `shouldBe` (2, "a\n\nb\nc\n")
  describe "opening an empty container (spec §7.1)" $ do
    let emptyAt c = querying $ \_ rule _ -> Identity . Right $
          if "\"all\"" `isInfixOf` BL.unpack (encode rule) then [] else [c]
        resolveIn syn phrase src = runIdentity (resolveLayers syn (Just caseTarget) Nothing [PhraseAnchor phrase] src)
        apply c phrase snippet src = fmap (\l -> snd (insertLines l snippet src)) (resolveIn (emptyAt c) phrase src)
    it "opens a one-line container that shares its line (Review Focus 1)" $
      apply (Match (0, 10) (0, 12) "()" (0, 0)) "end of case in update" "x: X," "pub fn new() -> Self {\n"
        `shouldBe` Right "pub fn new(\n    x: X,\n) -> Self {\n"
    it "inserts into a container spread over lines, at its comment's indentation (Review Focus 2)" $
      apply (Match (0, 14) (2, 5) "{\n        -- deps\n    }" (0, 0)) "end of case in update" "y: Y," "    Inv { a } {\n        -- deps\n    }\n"
        `shouldBe` Right "    Inv { a } {\n        -- deps\n        y: Y,\n    }\n"
    it "uses the container's indentation plus one level when nothing is inside" $
      apply (Match (0, 6) (1, 5) "{\n    }" (0, 0)) "start of case in update" "z: Z," "    S {\n    }\n"
        `shouldBe` Right "    S {\n        z: Z,\n    }\n"
    it "opens only bracketed containers: a whole-file container with no items is still refused" $
      apply (Match (0, 0) (2, 9) "use a;\n\nfn b() {}" (0, 0)) "end of case in update" "mod c;" "use a;\n\nfn b() {}\n"
        `shouldSatisfy` either (const True) (const False)
    it "refuses a container whose content is not one of its items, rather than delete it" $ do
      let refused r = either (\m -> "not one of the target's items" `T.isInfixOf` describeMisses m) (const False) r
      apply (Match (0, 24) (0, 30) "(true)" (0, 0)) "end of case in update" "x.clone()," "    let n = Svc::new(true);\n" `shouldSatisfy` refused
      apply (Match (0, 12) (2, 5) "{\n        ..Default::default()\n    }" (0, 0)) "end of case in update" "y: Y," "    let v = V {\n        ..Default::default()\n    };\n" `shouldSatisfy` refused
    it "drops a leading separator from the text when it opens a container: the text is the first item" $
      apply (Match (0, 10) (0, 12) "[]" (0, 0)) "end of case in update" ", Parser.map A" "    oneOf []\n"
        `shouldBe` Right "    oneOf [\n        Parser.map A\n    ]\n"
    it "does not count a commented-out copy of the snippet as present" $ do
      let src = "S {\n    -- x: X,\n    y: Y,\n}\n"
          withItem = querying $ \_ rule _ -> Identity . Right $
            if "\"all\"" `isInfixOf` BL.unpack (encode rule) then [Match (2, 4) (2, 9) "y: Y," (0, 0)] else [Match (0, 2) (3, 1) "{\n    -- x: X,\n    y: Y,\n}" (0, 0)]
      fmap (\l -> presentAt l "x: X," src) (resolveIn withItem "end of case in update" src) `shouldBe` Right False
    it "still finds text that carries its own comment lines" $ do
      let src = "S {\n    y: Y,\n    -- note\n    x: X,\n}\n"
          withItem = querying $ \_ rule _ -> Identity . Right $
            if "\"all\"" `isInfixOf` BL.unpack (encode rule) then [Match (1, 4) (1, 9) "y: Y," (0, 0), Match (3, 4) (3, 9) "x: X," (0, 0)] else [Match (0, 2) (4, 1) "{\n    y: Y,\n    -- note\n    x: X,\n}" (0, 0)]
      fmap (\l -> presentAt l "-- note\nx: X," src) (resolveIn withItem "end of case in update" src) `shouldBe` Right True
    it "finds the snippet already there once the container has it (Review Focus 3)" $ do
      let opened = "pub fn new(\n    x: X,\n) -> Self {\n"
          withItem = querying $ \_ rule _ -> Identity . Right $
            if "\"all\"" `isInfixOf` BL.unpack (encode rule) then [Match (1, 4) (1, 9) "x: X," (0, 0)] else [Match (0, 10) (2, 1) "(\n    x: X,\n)" (0, 0)]
      fmap (\l -> presentAt l "x: X," opened) (resolveIn withItem "end of case in update" opened) `shouldBe` Right True
  describe "start of file / end of file (any file, no adapter needed)" $ do
    let at p c = fmap locLine (runIdentity (resolveLayers fakeSyntax Nothing Nothing [PhraseAnchor p] c))
    it "end of file is after the last line; start of file is line 0" $ do
      at "end of file" "a\nb\n" `shouldBe` Right 2
      at "end of file" "a\nb" `shouldBe` Right 2
      at "start of file" "a\nb\n" `shouldBe` Right 0
    it "works on an empty file" $ do
      at "end of file" "" `shouldBe` Right 0
      fmap (\l -> snd (insertLines l "pub mod x;" "")) (runIdentity (resolveLayers fakeSyntax Nothing Nothing [PhraseAnchor "end of file"] "")) `shouldBe` Right "pub mod x;\n"
    it "counts the whole file for 'already present', so re-runs are no-ops" $
      fmap (\l -> presentAt l "pub mod x;" "pub mod x;\npub mod y;\n") (runIdentity (resolveLayers fakeSyntax Nothing Nothing [PhraseAnchor "end of file"] "pub mod x;\npub mod y;\n")) `shouldBe` Right True
  describe "insertLines" $ do
    it "indents non-blank lines and reports the 1-based line" $
      insertLines (Located 1 "  " "" "x" NoSpacing Nothing "") "a\n\nb\n" "one\ntwo\n" `shouldBe` (2, "one\n  a\n\n  b\ntwo\n")
    it "removes the text's common indentation first, so text written at its absolute column is not doubled" $
      insertLines (Located 1 "  " "" "x" NoSpacing Nothing "") "\n    a ->\n        b" "one\ntwo\n" `shouldBe` (2, "one\n\n  a ->\n      b\ntwo\n")
    it "keeps tab indentation relative" $
      insertLines (Located 1 "\t" "" "x" NoSpacing Nothing "") "\t\ta\n\t\t\tb" "one\n" `shouldBe` (2, "one\n\ta\n\t\tb\n")
  describe "insertedText" $ do
    it "does not depend on how far the whole text is indented" $ hedgehog $ do
      ls <- forAll (Gen.list (Range.linear 1 6) (Gen.text (Range.linear 0 10) (Gen.element ("ab (){}=-> " :: String))))
      k <- forAll (Gen.int (Range.linear 0 12))
      let t = T.intercalate "\n" ls
          shifted = T.intercalate "\n" [if T.null (T.strip l) then l else T.replicate k " " <> l | l <- ls]
      insertedText NoSpacing shifted === insertedText NoSpacing t
    it "keeps every line, and leaves the least indented one at column 0" $ hedgehog $ do
      t <- forAll (Gen.text (Range.linear 0 40) (Gen.element ("ab \n\t{}" :: String)))
      let out = T.splitOn "\n" (insertedText NoSpacing t)
          body = maybe t id (T.stripSuffix "\n" t)
          lead l = T.length (T.takeWhile (`elem` (" \t" :: String)) l)
      length out === length (T.splitOn "\n" body)
      case [lead l | l <- out, not (T.null (T.strip l))] of
        [] -> pure ()
        ns -> minimum ns === 0
    it "is the text as written, dedented, with the blank line spacing adds (what the plan shows)" $ do
      insertedText BlankBefore "    a\n      b" `shouldBe` "\na\n  b"
      insertedText BlankAfter "a" `shouldBe` "a\n"
      insertedText NoSpacing "a\n" `shouldBe` "a"
    it "start of puts the blank line after (copied from the first two items)" $
      insertedText BlankAfter "\nx" `shouldBe` "\nx\n"
