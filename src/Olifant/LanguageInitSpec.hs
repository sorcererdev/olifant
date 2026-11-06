{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.LanguageInitSpec (spec, formAnswers) where

import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Map.Strict qualified as M
import Olifant.Fill (parseFills, replayFiller)
import Olifant.Language (Grammar (..), GrammarSource (..))
import Olifant.Language.Grammar (GrammarSettings (..), Loaded (..), defaultSettings, loadGrammar)
import Olifant.Language.GrammarSpec (fixtureRepo)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)
import Olifant.Language.TreeSitter (queriesFor)
import Olifant.Library (providersSection, queryExemplar)
import Olifant.Doctor (Check (..), Status (..))
import Olifant.Fill (FillHole (..), FillRequest (..), Stance (..))
import Olifant.LanguageInit
import Test.Hspec

survey :: Survey
survey = Survey "https://example.invalid/tree-sitter-x" Nothing Nothing ["**/*.go"] ["**/*_test.go"] (Just "go.mod") [Runner "go" Nothing]

-- | The elmish form's answers (testdata/init-language/form.toml), every
-- other field of both forms answered none.
formAnswers :: IO [(T.Text, T.Text)]
formAnswers = do
  Right fills <- parseFills <$> TIO.readFile "src/Olifant/testdata/init-language/form.toml"
  pure [(fdName f, M.findWithDefault "none" (fdName f) fills) | f <- languageForm "elmish" ++ toolchainForm "elmish"]

upstream :: Upstream
upstream = Upstream "https://example.invalid/tree-sitter-x @0123456" [("tags.scm", "(function_declaration name: (identifier) @name) @definition.function\n")] (Just ("LICENSE", "The MIT License (MIT)\n\nCopyright\n")) Nothing Nothing

-- | An errand's input over no loaded grammar (the form needs none).
bareInput :: Upstream -> [FilePath] -> ErrandInput
bareInput up tracked = ErrandInput "go" (Grammar "x" Nothing) ["function_declaration"] up (const []) ["go: /usr/bin/go"] [("cmd/main.go", "package main\n")] "/w" survey tracked

spec :: Spec
spec = do
  describe "the form" $ do
    let field n = case [f | f <- languageForm "x" ++ toolchainForm "x", fdName f == n] of
          f : _ -> f
          [] -> error ("no field " <> T.unpack n)
    it "hole names are underscored, one per key, and each form names its required keys" $ do
      map fdName (languageForm "x") `shouldSatisfy` all (not . T.isInfixOf ".")
      [fdName f | f <- languageForm "x", fdRequired f] `shouldBe` ["comment"]
      [fdName f | f <- toolchainForm "x", fdRequired f] `shouldBe` ["command", "root"]
      fdPath (field "test_unit") `shouldBe` ["test", "unit"]
      fdPath (field "holes_expr_wrap") `shouldBe` ["holes", "expr", "wrap"]
    it "assemble writes the engine's keys first, each field under its table, and leaves none out" $ do
      let t = assemble (languageForm "x") [(["files"], "[\"**/*.x\"]"), (["test", "preset"], "\"x-test\"")] (M.fromList [("comment", "\"#\""), ("test_unit", "['^def (x)']"), ("test_above", "none"), ("keywords", "none")])
      T.lines t `shouldBe` ["files = [\"**/*.x\"]", "comment = \"#\"", "", "[test]", "preset = \"x-test\"", "unit = ['^def (x)']"]
    it "assemble leaves out a table whose needed keys are not all answered" $ do
      let t = assemble (languageForm "x") [] (M.fromList [("comment", "\"#\""), ("holes_expr_wrap", "\"{= fill =}\"")])
      t `shouldNotSatisfy` T.isInfixOf "holes"
    it "valueOf: none is no value; a bare text of a string kind is quoted; two keys in one value are refused" $ do
      valueOf (field "comment") "none" `shouldBe` Right Nothing
      valueOf (field "comment") "\"//\"" `shouldBe` Right (Just "\"//\"")
      valueOf (field "comment") "--" `shouldBe` Right (Just "'--'")
      valueOf (field "test_above") "\"^\\s*@\"" `shouldBe` Right (Just "\"^[[:space:]]*@\"")
      valueOf (field "command") "go test ./..." `shouldBe` Right (Just "[\"go\", \"test\", \"./...\"]")
      valueOf (field "branch") "{= pattern =}: {= body =}" `shouldBe` Right (Just "'{= pattern =}: {= body =}'")
      valueOf (field "module_of") "{ \"strip\": [\"^src/\"], \"separator\": \"/\" }" `shouldBe` Right (Just "{separator = \"/\", strip = [\"^src/\"]}")
      valueOf (field "imports_resolve") "[{ \"pattern\": \"^import (x)\", \"module\": 1 }]" `shouldBe` Right (Just "[{module = 1, pattern = \"^import (x)\"}]")
      valueOf (field "comment") "\"#\"\n[check]\npreset = \"x\"" `shouldSatisfy` either (T.isInfixOf "one value") (const False)
    it "valueOf normalises a key's own variables, {{x}} and {x}, and refuses another" $ do
      valueOf (field "tests_for") "\"{{dir}}/{{stem}}_t.go\"" `shouldBe` Right (Just "\"{= dir =}/{= stem =}_t.go\"")
      valueOf (field "tests_for") "\"{dir}/{stem}_t.go\"" `shouldBe` Right (Just "\"{= dir =}/{= stem =}_t.go\"")
      valueOf (field "tests_for") "\"{= foo =}/x\"" `shouldSatisfy` either (T.isInfixOf "{= foo =}") (const False)
      normaliseTemplates ["dir", "stem"] "{{dir}}/{{stem}}_t.go" `shouldBe` Right "{= dir =}/{= stem =}_t.go"
      normaliseTemplates ["dir", "stem"] "{= foo =}" `shouldSatisfy` either (const True) (const False)
      normaliseTemplates [] "^a{2}$" `shouldBe` Right "^a{2}$"
    it "normaliseRegex writes Perl classes and lazy repeats the POSIX way, inside brackets too" $ do
      normaliseRegex "^\\s*func\\s+(\\w+)\\(.*?\\)" `shouldBe` "^[[:space:]]*func[[:space:]]+([[:alnum:]_]+)\\(.*\\)"
      normaliseRegex "[^\\s]\\d+?[\\w.]" `shouldBe` "[^[:space:]][0-9]+[[:alnum:]_.]"
      normaliseRegex "^[[:space:]]*a\\.b" `shouldBe` "^[[:space:]]*a\\.b"
    it "valueOf normalises a regex field's strings, in arrays and tables too" $ do
      valueOf (field "test_unit") "[\"^func\\\\s+(Test\\\\w+)\"]" `shouldBe` Right (Just "[\"^func[[:space:]]+(Test[[:alnum:]_]+)\"]")
      valueOf (field "test_above") "'^\\s*@'" `shouldBe` Right (Just "\"^[[:space:]]*@\"")
    it "errorField maps a reader's key to its field" $ do
      fmap fdName (errorField (languageForm "x") "`test`: `above`: expected a string, got an array") `shouldBe` Just "test_above"
      fmap fdName (errorField (languageForm "x") "`[test] unit`: not a valid expression") `shouldBe` Just "test_unit"
      fmap fdName (errorField (toolchainForm "x") "`[test.lines] pass`: bad") `shouldBe` Just "lines_pass"
      fmap fdName (errorField (languageForm "x") "`comment`: expected a string") `shouldBe` Just "comment"
      fmap fdName (errorField (languageForm "x") "something else") `shouldBe` Nothing
      map fdName (errorFields (languageForm "x" ++ toolchainForm "x") "[test.select] uses {= group =}, which needs `file_group` (or [imports] module_of)") `shouldBe` ["select_test", "module_of"]
    it "derivedTestsFor reads tests_for from a source and a test beside it" $ do
      let sv (_ :: String) fs ts = Survey "g" Nothing Nothing fs ts Nothing []
      derivedTestsFor (sv "go" ["**/*.go"] ["**/*_test.go"]) ["a/x.go", "a/x_test.go"] `shouldBe` Just "{= dir =}/{= stem =}_test.go"
      derivedTestsFor (sv "py" ["**/*.py"] ["**/test_*.py"]) ["a/x.py", "a/test_x.py"] `shouldBe` Just "{= dir =}/test_{= stem =}.py"
      derivedTestsFor (sv "hs" ["src/**/*.hs", "test/**/*.hs"] ["test/**/*Spec.hs"]) ["src/A.hs", "test/ASpec.hs"] `shouldBe` Nothing
  describe "parseSurvey" $ do
    it "reads the keys" $
      parseSurvey (T.unlines ["grammar = \"https://example.invalid/tree-sitter-x\"", "files = [\"**/*.go\"]", "tests = [\"**/*_test.go\"]", "manifest = \"go.mod\"", "runners = [\"go\"]"])
        `shouldBe` Right survey
    it "an empty path or name is none; a name written as the C symbol is what follows tree_sitter_" $ do
      let Right sv = parseSurvey "grammar = \"https://x/g\"\npath = \"\"\nname = \"\"\nfiles = [\"**/*.g\"]\n"
          Right sv2 = parseSurvey "grammar = \"https://x/g\"\nname = \"tree_sitter_g\"\nfiles = [\"**/*.g\"]\n"
      (svPath sv, svName sv, svName sv2) `shouldBe` (Nothing, Nothing, Just "g")
    it "reads path and name, the loader's monorepo keys" $
      fmap (\s -> (svPath s, svName s)) (parseSurvey "grammar = \"u\"\npath = \"tsx\"\nname = \"tsx\"\nfiles = [\"*.tsx\"]\n")
        `shouldBe` Right (Just "tsx", Just "tsx")
    it "reads a runner with its JUnit argument, and a bare program" $
      fmap svRunners (parseSurvey "grammar = \"u\"\nfiles = [\"*.go\"]\nrunners = [\"go\", { program = \"gotestsum\", junit = \"--junitfile {= report =}\" }]\n")
        `shouldBe` Right [Runner "go" Nothing, Runner "gotestsum" (Just "--junitfile {= report =}")]
    it "the survey asks for a runner that writes JUnit XML and its argument" $
      map fhInstruction (frHoles (surveyRequest "go" [])) `shouldSatisfy` any (\i -> "JUnit XML" `T.isInfixOf` i && "junit =" `T.isInfixOf` i)
    it "runnerLine: where the program is, and its JUnit argument" $ do
      runnerLine (Runner "gotestsum" (Just "--junitfile {= report =}")) (Just "/opt/bin/gotestsum") `shouldBe` "gotestsum: /opt/bin/gotestsum (JUnit: --junitfile {= report =})"
      runnerLine (Runner "go" Nothing) Nothing `shouldBe` "go: not on PATH"
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
    it "providersSection is user 09's providers section, alone" $ do
      providersSection `shouldSatisfy` T.isPrefixOf "### Providers"
      providersSection `shouldNotSatisfy` T.isInfixOf "### Hole kinds"
    it "captureNamesIn lists a query's capture names, in order, once each" $ do
      tags <- TIO.readFile "library/queries/rust/tags.scm"
      captureNamesIn tags `shouldSatisfy` (\cs -> "definition.function" `elem` cs && "name" `elem` cs)
      captureNamesIn "(a) @x (b) @y.z (c) @x" `shouldBe` ["x", "y.z"]
    it "queriesToWrite: a grammar with shipped queries needs none" $
      queriesToWrite "elm" (queriesFor "elm") upstream `shouldBe` ([], [])
    it "queriesToWrite: nvim's textobjects are copied when found, else a hole" $ do
      let (copied, holes) = queriesToWrite "x" [] upstream {upNvim = Just ("nvim @1 queries/x/textobjects.scm", "(a) @function.outer\n")}
      map fst copied `shouldBe` ["tags.scm", "textobjects.scm"]
      holes `shouldBe` ["imports"]
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
    it "formRequest: one hole per field, the value stance, the sample, no protocol" $ do
      let req = formRequest (bareInput upstream ["cmd/main.go"])
          names = map fhName (frHoles req)
      take 2 names `shouldBe` ["comment", "keywords"]
      ["tests_for", "test_unit", "command", "root", "imports", "textobjects"] `shouldSatisfy` all (`elem` names)
      frStance req `shouldBe` ValueStance
      lookup "cmd/main.go" (frContext req) `shouldBe` Just "package main\n"
      lookup "protocol" (frInputs req) `shouldBe` Nothing
      lookup "source globs" (frInputs req) `shouldBe` Just "**/*.go"
      lookup "node kinds" (frInputs req) `shouldBe` Just "function_declaration"
      [fhContext h | h <- frHoles req, fhName h == "test_unit"] `shouldBe` ["[test]\nunit = \10214test_unit\10215"]
      [fhFile h | h <- frHoles req, fhName h == "command"] `shouldBe` [".olifant/toolchains/go-test.toml"]
    it "formRequest: a textobjects hole has the exemplar query; none without it" $ do
      let withHole = formRequest (bareInput upstream [])
          without = formRequest (bareInput upstream {upNvim = Just ("textobjects.scm", "(x) @function.outer\n")} [])
      lookup "exemplar query (another grammar's): textobjects.scm" (frContext withHole) `shouldBe` Just (snd queryExemplar)
      map fst (frContext without) `shouldNotSatisfy` any (isPrefixOf "exemplar")
    it "formRequest: a tests_for the sample's names show is not asked" $ do
      map fhName (frHoles (formRequest (bareInput upstream ["a/x.go", "a/x_test.go"]))) `shouldNotSatisfy` elem "tests_for"
      map fhName (frHoles (formRequest (bareInput upstream ["a/x.go"]))) `shouldSatisfy` elem "tests_for"
    it "formRequest: the closed sets, the template filters and the query predicates" $ do
      let req = formRequest (bareInput upstream [])
      lookup "template filters" (frInputs req) `shouldSatisfy` maybe False (\t -> all (`T.isInfixOf` t) ["escaped", "python_string", "only these"])
      lookup "query predicates" (frInputs req) `shouldSatisfy` maybe False (\t -> all (`T.isInfixOf` t) ["#match?", "#any-of?", "no directives"])
    it "sourcesFile says which file was copied and which the model wrote" $ do
      let (copied, holes) = queriesToWrite "x" [] upstream
          t = sourcesFile "x" "https://example.invalid/tree-sitter-x @0123456" copied holes
      t `shouldSatisfy` T.isInfixOf "tags.scm"
      t `shouldSatisfy` T.isInfixOf "imports.scm"
      t `shouldSatisfy` T.isInfixOf "written by the model"
  describe "nvimTextobjects" $ do
    let nvimRepo name = do
          tmp <- getTemporaryDirectory
          let repo = tmp </> "olifant-test-cache" </> ("nvim-fixture-" <> name)
              git args = readProcess "git" (["-C", repo, "-c", "user.name=t", "-c", "user.email=t@t", "-c", "commit.gpgsign=false"] ++ args) ""
          removePathForcibly repo
          removePathForcibly (tmp </> "olifant-test-cache" </> ("nvim-cache-" <> name))
          createDirectoryIfMissing True (repo </> "queries" </> "elm")
          TIO.writeFile (repo </> "queries" </> "elm" </> "textobjects.scm") "(function_declaration) @function.outer\n"
          TIO.writeFile (repo </> "LICENSE") "Apache License\n"
          _ <- readProcess "git" ["init", "--quiet", repo] ""
          _ <- git ["add", "-A"]
          _ <- git ["commit", "--quiet", "-m", "queries"]
          sha <- T.strip . T.pack <$> git ["rev-parse", "HEAD"]
          let s = (defaultSettings (tmp </> "olifant-test-cache" </> ("nvim-cache-" <> name)) []) {gsTextobjects = ("file://" <> T.pack repo, sha)}
          pure (s, sha)
    it "fetches olifant's pinned repository and reads the grammar's file and the licence" $ do
      (s, sha) <- nvimRepo "a"
      Right (found, licence) <- nvimTextobjects s "elm"
      found `shouldSatisfy` maybe False (\(from, t) -> from == "nvim-fixture-a @" <> T.take 7 sha <> " queries/elm/textobjects.scm" && t == "(function_declaration) @function.outer\n")
      licence `shouldBe` Just ("LICENSE", "Apache License\n")
    it "a grammar nvim has no file for is none; an unreachable repository says why" $ do
      (s, _) <- nvimRepo "b"
      Right (found, _) <- nvimTextobjects s "nonesuch"
      found `shouldBe` Nothing
      Left why <- nvimTextobjects s {gsTextobjects = ("file:///nonexistent-nvim", "0123456789012345678901234567890123456789")} "elm"
      why `shouldSatisfy` T.isInfixOf "nonexistent-nvim"
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
  describe "a shipped language's layer" $ do
    it "layerKeysOk: the asked keys, an inline table's own keys and the tables on the way; any other key refused, named" $ do
      let allowed = [["test", "unit"], ["imports", "module_of"]]
      layerKeysOk allowed "[test]\nunit = ['^x']\n[imports]\nmodule_of = { strip = ['a'], separator = '.' }\n" `shouldBe` Right ()
      layerKeysOk allowed "files = ['**/*.x']\n[test]\nunit = ['^x']\n" `shouldBe` Left "sets `files`, which no failing row needs"
      layerKeysOk allowed "[test]\nunit = ['^x']\nabove = 'y'\n" `shouldBe` Left "sets `test.above`, which no failing row needs"
    it "shippedFailing: a tests run with results, none passing, is the project's tests, not a key's; a broken run is sent" $ do
      let row d = Check "rust: tests" Warn d (Just "regenerate the key or supply a provider (see chapter 9, Providers)")
          failed = row "ran cargo test in the package root (.); none of 2 passed; the first failure, x: boom"
          broken = row "ran cargo test in the package root (.): error: no network"
      shippedFailing "rust" [] [failed, broken] `shouldBe` [broken]

  describe "repairRequest" $ do
    let req = formRequest (bareInput upstream ["a/x.go"])
        written = [(".olifant/languages/go.toml", "files = []\n")]
        holes rows = map fhName (frHoles (repairRequest req written rows))
    it "asks again only the fields the failing rows point at, renamed _repair, with the files, rows and protocol" $ do
      let r = repairRequest req written [Check "go: test units" Warn "0 found" Nothing, Check "go: queries" Bad "textobjects.scm: bad node" Nothing]
      map fhName (frHoles r) `shouldBe` ["tests_for_repair", "test_unit_repair", "textobjects_repair"]
      lookup ".olifant/languages/go.toml" (frContext r) `shouldBe` Just "files = []\n"
      lookup "failing rows" (frContext r) `shouldSatisfy` maybe False ("go: test units — 0 found" `T.isInfixOf`)
      lookup "protocol" (frInputs r) `shouldBe` Just providersSection
    it "a row named after a field asks that field; a tests row the command and its kin" $ do
      holes [Check "go: test_above" Bad "does not load" Nothing] `shouldBe` ["test_above_repair"]
      holes [Check "go: tests" Bad "broken" Nothing] `shouldSatisfy` (\hs -> all (`elem` hs) ["tests_for_repair", "command_repair", "root_repair", "lines_pass_repair"])
      holes [Check "go: holes" Warn "[holes.expr]: x does not parse" Nothing] `shouldSatisfy` (\hs -> not (null hs) && all ("holes_" `T.isPrefixOf`) hs)
      holes [Check "go: anchors" Warn "x" Nothing] `shouldBe` ["textobjects_repair"]
    it "a load error naming a query file points at that query's hole" $
      holes [Check "language /p/.olifant/queries/go/imports.scm" Bad "bad node" Nothing] `shouldBe` ["imports_repair"]
    it "a repair hole's marker carries its new name, so the model answers under it" $
      map fhContext (frHoles (repairRequest req written [Check "go: comment" Bad "x" Nothing])) `shouldBe` ["comment = \10214comment_repair\10215"]
    it "a test runner row points at the command; a whole-file row at the file's fields" $ do
      holes [Check "test runner gotestsum (go)" Bad "absent" Nothing] `shouldBe` ["command_repair"]
      holes [Check "go: toolchain" Bad "does not load" Nothing] `shouldSatisfy` (\hs -> "command_repair" `elem` hs && "comment_repair" `notElem` hs)
    it "regressedFields: the repaired fields behind a row that passed before the repair and fails after it" $ do
      let before = [Check "go: tests" Ok "ran" Nothing, Check "go: test units" Warn "0 found" Nothing, Check "go: units" Ok "3" Nothing]
          after = [Check "go: tests" Warn "none passed" Nothing, Check "go: test units" Ok "2 found" Nothing, Check "go: units" Ok "3" Nothing]
      regressedFields before after ["command", "test_unit", "comment"] `shouldBe` ["command"]
      regressedFields before before ["command"] `shouldBe` []
    it "repairAnswers keeps the asked holes' answers under their fields' names" $ do
      let r = repairRequest req written [Check "go: comment" Bad "x" Nothing]
      repairAnswers r (M.fromList [("comment_repair", "x"), ("comment", "y"), ("keywords_repair", "z")]) `shouldBe` M.fromList [("comment", "x")]
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
          answers <- formAnswers
          textobjects <- TIO.readFile "library/queries/elm/textobjects.scm"
          imports <- TIO.readFile "library/queries/elm/imports.scm"
          pure (M.fromList (answers ++ [("textobjects", textobjects), ("imports", imports)]))
        elmSurvey = Survey "x" Nothing (Just "elm") ["**/*.elm"] ["tests/**/*.elm"] (Just "elm.json") []
        input g ld up = inputOver g ld up []
        inputOver g ld up tracked = ErrandInput "elmish" g (ldNodeTypes ld) up (const []) [] [] "/w" elmSurvey tracked
        rowsOf a = [(checkName c, checkDetail c) | c <- anFailing a]
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
      drop (length (frHoles req) - 2) (map fhName (frHoles req)) `shouldBe` ["imports", "textobjects"]
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
      let repaired = checkAnswers (input g ld up) (M.union (repairAnswers rreq (M.fromList [("textobjects_repair", fills0 M.! "textobjects")])) (anAnswers a))
      anFailing repaired `shouldBe` []
      lookup ".olifant/queries/elm/textobjects.scm" (anFiles repaired) `shouldBe` M.lookup "textobjects" fills0
    it "(o2) the form assembles both files: the engine's keys, the answered ones, junit or lines by the command" $ do
      (g, ld, up) <- elmGrammar "o2"
      fills <- fixtures
      Right (_, a) <- errandCore (replayFiller fills) (input g ld up)
      let Just language = lookup ".olifant/languages/elmish.toml" (anFiles a)
          Just toolchain = lookup ".olifant/toolchains/elmish-test.toml" (anFiles a)
      take 3 (T.lines language) `shouldBe` ["# written by olifant init --language", pinText g, "files = [\"**/*.elm\"]"]
      language `shouldSatisfy` T.isInfixOf "[test]\npreset = \"elmish-test\"\ntests_for = \"tests/{= stem =}Test.elm\""
      language `shouldNotSatisfy` T.isInfixOf "keywords"
      toolchain `shouldSatisfy` T.isInfixOf "format = \"lines\""
      toolchain `shouldSatisfy` T.isInfixOf "[test.lines]\npass = "
      let junit = checkAnswers (input g ld up) (M.insert "command" "['elm-test', '--report', '{= report =}', '{= tests =}']" (anAnswers a))
      rowsOf junit `shouldBe` []
      lookup ".olifant/toolchains/elmish-test.toml" (anFiles junit) `shouldSatisfy` maybe False (\t -> "format = \"junit-xml\"" `T.isInfixOf` t && not ("[test.lines]" `T.isInfixOf` t))
    it "(o3) a wrong value is a row naming its field: smuggled keys, a required none, a reader's error; a command with no {= tests =} a warn" $ do
      (g, ld, up) <- elmGrammar "o3"
      fills <- fixtures
      Right (_, a) <- errandCore (replayFiller fills) (input g ld up)
      let with k v = map fst (rowsOf (checkAnswers (input g ld up) (M.insert k v (anAnswers a))))
      with "comment" "\"--\"\n[check]\npreset = \"x\"" `shouldBe` ["elmish: comment"]
      with "comment" "none" `shouldBe` ["elmish: comment"]
      with "test_above" "[1, 2]" `shouldBe` ["elmish: test_above"]
      with "command" "[\"elm-test\"]" `shouldBe` []
      let noTests = checkAnswers (input g ld up) (M.insert "command" "[\"elm-test\"]" (anAnswers a))
      map (\c -> (checkName c, checkStatus c)) (anWarnings noTests) `shouldBe` [("elmish: command", Warn)]
      isJust (lookup ".olifant/toolchains/elmish-test.toml" (anFiles noTests)) `shouldBe` True
      map checkName (anWarnings (checkAnswers (input g ld up) (M.insert "command" "['elm-test', '-run', 'Test{= tests =}']" (anAnswers a)))) `shouldBe` ["elmish: command"]
      with "command" "['elm-test', '{= tests =}']" `shouldBe` []
      with "lines_fail" "none" `shouldBe` ["elmish: command"]
    it "(o3b) leaveOut: a key still wrong after the repair is left out with a warn row; a required one stays failing" $ do
      (g, ld, up) <- elmGrammar "o3b"
      fills <- fixtures
      Right (_, a) <- errandCore (replayFiller fills) (input g ld up)
      let wrong = checkAnswers (input g ld up) (M.insert "test_above" "[1, 2]" (anAnswers a))
          left = leaveOut (input g ld up) wrong
      map checkName (anFailing left) `shouldBe` []
      map (\c -> (checkName c, checkStatus c)) (anWarnings left) `shouldBe` [("elmish: test_above", Warn)]
      lookup ".olifant/languages/elmish.toml" (anFiles left) `shouldSatisfy` maybe False (not . T.isInfixOf "above")
      map checkName (anFailing (leaveOut (input g ld up) (checkAnswers (input g ld up) (M.insert "comment" "none" (anAnswers a))))) `shouldBe` ["elmish: comment"]
    it "(o4) tests_for mapping a sampled source to itself, or to no test file, is a row with the mapping" $ do
      (g, ld, up) <- elmGrammar "o4"
      fills <- fixtures
      let tracked = ["src/Route.elm", "tests/RouteTest.elm"]
          answered v = rowsOf (checkAnswers (inputOver g ld up tracked) (M.insert "tests_for" v fills))
      answered "\"{= dir =}/{= stem =}.elm\"" `shouldBe` [("elmish: tests_for", "maps src/Route.elm to src/Route.elm (itself)")]
      answered "\"{{dir}}/{{stem}}Test.elm\"" `shouldBe` [("elmish: tests_for", "maps src/Route.elm to src/RouteTest.elm (not a test file by the globs tests/**/*.elm)")]
      answered "\"tests/{= stem =}Test.elm\"" `shouldBe` []
    it "(o5) nvim's textobjects are copied with their header and licence, and SOURCES names them" $ do
      (g, ld, up0) <- elmGrammar "o5"
      fills <- fixtures
      let up = up0 {upNvim = Just ("nvim-treesitter-textobjects @5c7b026 queries/elm/textobjects.scm", "((function_declaration) @function.outer (#set! x y))\n"), upNvimLicence = Just ("LICENSE", "Apache License\n")}
      Right (req, a) <- errandCore (replayFiller fills) (input g ld up)
      map fhName (frHoles req) `shouldNotSatisfy` elem "textobjects"
      lookup ".olifant/queries/elm/textobjects.scm" (anFiles a) `shouldBe` Just "; from nvim-treesitter-textobjects @5c7b026 queries/elm/textobjects.scm (LICENSE: Apache License)\n; directives removed: #set!\n((function_declaration) @function.outer )\n"
      lookup ".olifant/queries/elm/LICENSE-nvim-treesitter" (anFiles a) `shouldBe` Just "Apache License\n"
      lookup ".olifant/queries/elm/SOURCES" (anFiles a) `shouldSatisfy` maybe False (T.isInfixOf "textobjects.scm  copied from nvim-treesitter-textobjects @5c7b026 queries/elm/textobjects.scm, directives removed (#set!)")
