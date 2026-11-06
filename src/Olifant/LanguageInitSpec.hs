{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.LanguageInitSpec (spec) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Map.Strict qualified as M
import Olifant.Fill (replayFiller)
import Olifant.Language (Grammar (..), GrammarSource (..))
import Olifant.Language.Grammar (Loaded (..), loadGrammar)
import Olifant.Language.GrammarSpec (fixtureRepo)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)
import Olifant.Language.TreeSitter (queriesFor)
import Olifant.Library (shippedLanguages, shippedToolchains)
import Olifant.Doctor (Check (..), Status (..))
import Olifant.Fill (FillHole (..), FillRequest (..), Stance (..))
import Olifant.LanguageInit
import Test.Hspec

survey :: Survey
survey = Survey "https://example.invalid/tree-sitter-x" Nothing Nothing ["**/*.go"] ["**/*_test.go"] (Just "go.mod") ["go"]

spec :: Spec
spec = do
  describe "parseSurvey" $ do
    it "reads the keys" $
      parseSurvey (T.unlines ["grammar = \"https://example.invalid/tree-sitter-x\"", "files = [\"**/*.go\"]", "tests = [\"**/*_test.go\"]", "manifest = \"go.mod\"", "runners = [\"go\"]"])
        `shouldBe` Right survey
    it "reads path and name, the loader's monorepo keys" $
      fmap (\s -> (svPath s, svName s)) (parseSurvey "grammar = \"u\"\npath = \"tsx\"\nname = \"tsx\"\nfiles = [\"*.tsx\"]\n")
        `shouldBe` Right (Just "tsx", Just "tsx")
    it "refuses an empty grammar and a missing files" $ do
      parseSurvey "grammar = \"\"\nfiles = [\"*.go\"]\n" `shouldSatisfy` either ("grammar" `T.isInfixOf`) (const False)
      parseSurvey "grammar = \"u\"\n" `shouldSatisfy` either ("files" `T.isInfixOf`) (const False)
    it "strips a markdown fence around the TOML, and refuses other text" $ do
      fmap svGrammar (parseSurvey "```toml\ngrammar = \"u\"\nfiles = [\"*.go\"]\n```\n") `shouldBe` Right "u"
      parseSurvey "Here it is:\ngrammar = \"u\"\nfiles = [\"*.go\"]\n" `shouldSatisfy` either (const True) (const False)
  it "sampleFor: shallow sources and tests first, and the manifest" $
    sampleFor survey ["cmd/main.go", "internal/rates/rates.go", "internal/rates/rates_test.go", "internal/x/y/z.go", "x_test.go", "go.mod"]
      `shouldBe` (["cmd/main.go", "internal/rates/rates.go"], ["x_test.go", "internal/rates/rates_test.go"], Just "go.mod")
  it "sampleFor: no manifest when the project has none" $ do
    let (_, _, m) = sampleFor survey ["a.go"]
    m `shouldBe` Nothing
  it "cutTo keeps the first lines" $
    cutTo 2 "a\nb\nc\n" `shouldBe` "a\nb\n"
  it "surveyRequest: one hole `survey`, the file stance, the paths in its inputs" $ do
    let req = surveyRequest "go" ["go.mod", "cmd/main.go"]
    map fhName (frHoles req) `shouldBe` ["survey"]
    frStance req `shouldBe` FileStance
    lookup "language" (frInputs req) `shouldBe` Just "go"
    lookup "tracked files" (frInputs req) `shouldSatisfy` maybe False ("cmd/main.go" `T.isInfixOf`)
  it "surveyRequest lists at most 200 paths" $ do
    let req = surveyRequest "go" [show n <> ".go" | n <- [1 :: Int .. 300]]
        Just paths = lookup "tracked files" (frInputs req)
    length (T.lines paths) `shouldBe` 200
  describe "the errand" $ do
    let upstream = Upstream "https://example.invalid/tree-sitter-x @0123456" [("tags.scm", "(function_declaration name: (identifier) @name) @definition.function\n")] (Just ("LICENSE", "The MIT License (MIT)\n\nCopyright\n"))
    it "providersSection is user 09's providers section, alone" $ do
      providersSection `shouldSatisfy` T.isPrefixOf "### Providers"
      providersSection `shouldNotSatisfy` T.isInfixOf "### Hole kinds"
    it "captureNamesIn lists a query's capture names, in order, once each" $ do
      tags <- TIO.readFile "library/queries/rust/tags.scm"
      captureNamesIn tags `shouldSatisfy` (\cs -> "definition.function" `elem` cs && "name" `elem` cs)
      captureNamesIn "(a) @x (b) @y.z (c) @x" `shouldBe` ["x", "y.z"]
    it "queriesToWrite: a grammar with shipped queries needs none" $
      queriesToWrite "elm" (queriesFor "elm") upstream `shouldBe` ([], [])
    it "queriesToWrite: upstream tags.scm is copied with its header; the rest are holes" $ do
      let (copied, holes) = queriesToWrite "x" [] upstream
      map fst copied `shouldBe` ["tags.scm"]
      concatMap (take 1 . T.lines . snd) copied `shouldBe` ["; from https://example.invalid/tree-sitter-x @0123456 (LICENSE: The MIT License (MIT))"]
      holes `shouldBe` ["imports", "textobjects"]
    it "withoutDirectives drops each #…! form, its line too when nothing else is on it, and names them" $ do
      let goTags = T.unlines ["(", "  (comment)* @doc", "  .", "  (function_declaration", "    name: (identifier) @name) @definition.function", "  (#strip! @doc \"^//\\\\s*\")", "  (#set-adjacent! @doc @definition.function)", ")"]
      withoutDirectives goTags `shouldBe` (T.unlines ["(", "  (comment)* @doc", "  .", "  (function_declaration", "    name: (identifier) @name) @definition.function", ")"], ["#strip!", "#set-adjacent!"])
    it "withoutDirectives keeps predicates, comments and a directive's look-alikes in strings" $ do
      let q = T.unlines ["; (#strip! in a comment)", "((identifier) @name (#eq? @name \"(#set! x)\"))"]
      withoutDirectives q `shouldBe` (q, [])
    it "withoutDirectives reads a directive's string with a paren and a quote in it" $
      withoutDirectives "((identifier) @name (#set! k \"a)\\\"b\"))\n" `shouldBe` ("((identifier) @name )\n", ["#set!"])
    it "queriesToWrite: an upstream file's directives are removed, and its header says so" $ do
      let up = upstream {upQueries = [("tags.scm", "((identifier) @name (#strip! @name \"x\")) @definition.function\n")]}
          (copied, _) = queriesToWrite "x" [] up
      concatMap (T.lines . snd) copied `shouldBe` ["; from https://example.invalid/tree-sitter-x @0123456 (LICENSE: The MIT License (MIT))", "; directives removed: #strip!", "((identifier) @name ) @definition.function"]
      sourcesFile "x" "u @1" copied [] `shouldSatisfy` T.isInfixOf "tags.scm  copied from u @1 queries/tags.scm, directives removed (#strip!)"
    it "queriesToWrite: with no upstream queries, all three are holes" $
      snd (queriesToWrite "x" [] upstream {upQueries = []}) `shouldBe` ["imports", "tags", "textobjects"]
    it "errandRequest: the files as holes, the exemplar pair and the protocol" $ do
      let req = errandRequest "go" "tree-sitter-go @2346a3a" ["function_declaration"] ["definition.function"] ["go: /usr/bin/go"] [("cmd/main.go", "package main\n")] ["imports", "textobjects"]
      map fhName (frHoles req) `shouldBe` ["language", "toolchain", "imports", "textobjects"]
      frStance req `shouldBe` FileStance
      lookup "exemplar: rust.toml" (frContext req) `shouldBe` lookup "rust" shippedLanguages
      lookup "exemplar: cargo-test.toml" (frContext req) `shouldBe` lookup "cargo-test" shippedToolchains
      lookup "cmd/main.go" (frContext req) `shouldBe` Just "package main\n"
      lookup "protocol" (frInputs req) `shouldBe` Just providersSection
      lookup "node kinds" (frInputs req) `shouldBe` Just "function_declaration"
      lookup "captures olifant reads" (frInputs req) `shouldSatisfy` maybe False ("function.outer" `T.isInfixOf`)
    it "errandRequest: the language hole leaves [check] to the user" $ do
      let req = errandRequest "go" "g @1" [] [] [] [] []
      map fhInstruction (take 1 (frHoles req)) `shouldSatisfy` any ("Leave out [check]" `T.isInfixOf`)
    it "errandRequest: the closed sets, the template filters and the query predicates" $ do
      let req = errandRequest "go" "g @1" [] [] [] [] []
      lookup "template filters" (frInputs req) `shouldSatisfy` maybe False (\t -> all (`T.isInfixOf` t) ["escaped", "python_string", "only these"])
      lookup "query predicates" (frInputs req) `shouldSatisfy` maybe False (\t -> all (`T.isInfixOf` t) ["#match?", "#any-of?", "no directives"])
    it "sourcesFile says which file was copied and which the model wrote" $ do
      let (copied, holes) = queriesToWrite "x" [] upstream
          t = sourcesFile "x" "https://example.invalid/tree-sitter-x @0123456" copied holes
      t `shouldSatisfy` T.isInfixOf "tags.scm"
      t `shouldSatisfy` T.isInfixOf "imports.scm"
      t `shouldSatisfy` T.isInfixOf "written by the model"
  describe "failingRows" $ do
    let regenerate = Just "regenerate the key or supply a provider (see chapter 9, Providers)"
        rows =
          [ Check "go: units" Ok "3 found" Nothing
          , Check "go: test units" Warn "0 found" regenerate
          , Check "go: sibling tests" Warn "no [unit] annotation: only a declared [conjure.fake] and [conjure.constructor] are read" Nothing
          , Check "go: conjure shapes" Warn "absent" (Just "add [conjure] to .olifant/languages/go.toml")
          , Check "go: signature" Warn "absent" (Just "add [conjure.port] add")
          , Check "go: grammar" Bad "fetch failed" Nothing
          , Check "go: queries" Warn "no queries for the go grammar" Nothing
          , Check "test runner gotestsum (go)" Bad "`gotestsum` is not on this PATH" Nothing
          , Check "rust: test units" Warn "0 found" regenerate
          , Check "model" Bad "no default" Nothing
          ]
    it "keeps the language's errors and disproven rows, and a queries or grammar warn" $
      map checkName (failingRows "go" [] rows) `shouldBe` ["go: test units", "go: grammar", "go: queries", "test runner gotestsum (go)"]
    it "languageRows are the language's rows, all of them" $
      map checkName (languageRows "go" [] rows) `shouldBe` ["go: units", "go: test units", "go: sibling tests", "go: conjure shapes", "go: signature", "go: grammar", "go: queries", "test runner gotestsum (go)"]
    it "a load error naming a file init wrote is the language's, and failing" $ do
      let refused = Check "language /p/.olifant/queries/go/tags.scm" Bad "`tags.scm`: unknown predicate `#strip!`" Nothing
          other = Check "language /p/.olifant/queries/rust/tags.scm" Bad "unknown predicate" Nothing
          written = [".olifant/queries/go/tags.scm", ".olifant/languages/go.toml"]
      map checkName (failingRows "go" written [refused, other]) `shouldBe` [checkName refused]
      map checkName (languageRows "go" written [refused, other]) `shouldBe` [checkName refused]
  describe "repairRequest" $ do
    let req = errandRequest "go" "g @1" [] [] [] [] ["imports", "textobjects"]
        written = [(".olifant/languages/go.toml", "files = []\n")]
    it "asks again only the holes the failing rows point at, renamed _repair, with the files and rows" $ do
      let r = repairRequest req written [Check "go: test units" Warn "0 found" Nothing, Check "go: queries" Bad "textobjects.scm: bad node" Nothing]
      map fhName (frHoles r) `shouldBe` ["language_repair", "textobjects_repair"]
      lookup ".olifant/languages/go.toml" (frContext r) `shouldBe` Just "files = []\n"
      lookup "failing rows" (frContext r) `shouldSatisfy` maybe False ("go: test units — 0 found" `T.isInfixOf`)
    it "a load error naming a query file points at that query's hole" $
      map fhName (frHoles (repairRequest req written [Check "language /p/.olifant/queries/go/imports.scm" Bad "bad node" Nothing])) `shouldBe` ["imports_repair"]
    it "a repair hole's marker carries its new name, so the model answers under it" $
      map fhContext (frHoles (repairRequest req written [Check "go: test units" Warn "0 found" Nothing])) `shouldBe` ["\10214language_repair\10215"]
    it "a test runner row points at the toolchain" $
      map fhName (frHoles (repairRequest req written [Check "test runner gotestsum (go)" Bad "absent" Nothing])) `shouldBe` ["toolchain_repair"]
  describe "the errand's queries (errandCore, no shipped set)" $ do
    let elmGrammar name = do
          tmp <- getTemporaryDirectory
          let repo = tmp </> "olifant-test-cache" </> ("language-init-queries-" <> name)
          _ <- fixtureRepo repo "."
          sha <- T.strip . T.pack <$> readProcess "git" ["-C", repo, "rev-parse", "HEAD"] ""
          let g = Grammar "elm" (Just (SourceGit ("file://" <> T.pack repo) sha Nothing))
          Right ld <- loadGrammar g
          up <- withSystemTempDirectory "olifant-upstream" $ \d -> do
            createDirectoryIfMissing True (d </> "queries")
            TIO.readFile "library/queries/elm/tags.scm" >>= TIO.writeFile (d </> "queries" </> "tags.scm")
            TIO.writeFile (d </> "LICENSE") "MIT License\n"
            readUpstream "file://x @abc1234" (Just d) Nothing
          pure (g, ld, up)
        fixtures = do
          let dir = "src/Olifant/testdata/init-language"
          language <- TIO.readFile (dir </> "language.toml")
          toolchain <- TIO.readFile (dir </> "toolchain.toml")
          textobjects <- TIO.readFile "library/queries/elm/textobjects.scm"
          imports <- TIO.readFile "library/queries/elm/imports.scm"
          pure (M.fromList [("language", language), ("toolchain", toolchain), ("textobjects", textobjects), ("imports", imports)])
        input g ld up = ErrandInput "elmish" g (ldNodeTypes ld) up (const []) [] [] "/w"
    it "(m) upstream tags.scm is copied with its header" $ do
      (g, ld, up) <- elmGrammar "m"
      fills <- fixtures
      Right (_, a) <- errandCore (replayFiller fills) (input g ld up)
      anFailing a `shouldBe` []
      lookup ".olifant/queries/elm/tags.scm" (anFiles a) `shouldSatisfy` maybe False (T.isPrefixOf "; from file://x @abc1234 (LICENSE: MIT License)\n")
    it "(n) the model's textobjects and imports are written, and SOURCES says where each came from" $ do
      (g, ld, up) <- elmGrammar "n"
      fills <- fixtures
      Right (req, a) <- errandCore (replayFiller fills) (input g ld up)
      map fhName (frHoles req) `shouldBe` ["language", "toolchain", "imports", "textobjects"]
      lookup ".olifant/queries/elm/textobjects.scm" (anFiles a) `shouldBe` M.lookup "textobjects" fills
      lookup ".olifant/queries/elm/imports.scm" (anFiles a) `shouldBe` M.lookup "imports" fills
      let Just sources = lookup ".olifant/queries/elm/SOURCES" (anFiles a)
      sources `shouldSatisfy` T.isInfixOf "tags.scm  copied from file://x @abc1234"
      sources `shouldSatisfy` T.isInfixOf "imports.scm  written by the model"
      sources `shouldSatisfy` T.isInfixOf "textobjects.scm  written by the model"
    it "(o) a query that does not compile is a failing row, and textobjects_repair replaces it" $ do
      (g, ld, up) <- elmGrammar "o"
      fills0 <- fixtures
      let fills = M.insert "textobjects" "(nonexistent_node) @x\n" fills0
      Right (req, a) <- errandCore (replayFiller fills) (input g ld up)
      map (\c -> (checkName c, T.take 16 (checkDetail c))) (anFailing a) `shouldBe` [("elmish: queries", "textobjects.scm:")]
      let rreq = repairRequest req (anFiles a) (anFailing a)
      map fhName (frHoles rreq) `shouldBe` ["textobjects_repair"]
      let repaired = checkAnswers (input g ld up) (anFiles a) ["textobjects"] "_repair" (M.insert "textobjects_repair" (fills0 M.! "textobjects") fills)
      anFailing repaired `shouldBe` []
      map fst (anFiles repaired) `shouldBe` [".olifant/queries/elm/textobjects.scm"]
