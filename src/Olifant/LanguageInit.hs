{-# LANGUAGE MultiWayIf #-}


-- | @olifant init --language NAME@, the pure parts (the IO edge is
-- 'Olifant.Cli.runInitLanguage'): the model's survey of the project, the
-- sample of its files, the errand's inputs and holes, which query files to
-- write, the answers' checks, and the failing rows of doctor.
--
-- The model writes text; olifant checks it and decides where it goes. The
-- grammar's repository is the model's proposal, but its revision is olifant's
-- ('Olifant.Language.Grammar.remoteHead').
module Olifant.LanguageInit
  ( Survey (..)
  , Runner (..)
  , runnerLine
  , parseSurvey
  , surveyRequest
  , sampleFor
  , cutTo
  , unfence
  , providersSection
  , captureNamesIn
  , Upstream (..)
  , queriesToWrite
  , withoutDirectives
  , formRequest
  , errandQueries
  , Field (..)
  , languageForm
  , toolchainForm
  , tableNeeds
  , assemble
  , isNone
  , valueOf
  , normaliseTemplates
  , normaliseRegex
  , errorField
  , derivedTestsFor
  , languageFile
  , toolchainFile
  , toolchainName
  , sourcesFile
  , pinText
  , initHeader
  , ErrandInput (..)
  , Answered (..)
  , checkAnswers
  , errandCore
  , shippedFor
  , failingRows
  , languageRows
  , repairRequest
  , repairHoles
  , repairAnswers
  , regressedFields
  , leaveOut
  , leaveOutWith
  , errorFields
  , LayerInput (..)
  , layerForm
  , layerAsk
  , layerRequest
  , checkLayer
  , layerKeysOk
  , presetFile
  , shippedFailing
  , readUpstream
  , nvimTextobjects
  ) where

import Control.Monad (filterM)
import Data.ByteString qualified as BS
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import Olifant.Language.Grammar (GrammarSettings (..), failureDetail, fetch)
import Data.Bifunctor (bimap, first)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as AKM
import Data.Char (isAlphaNum, isSpace)
import Data.Foldable (toList)
import Data.Scientific (floatingOrInteger)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.List (isPrefixOf, nub, nubBy, sort, sortOn)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Doctor (Check (..), Status (..), initHeader)
import Olifant.Fill (FillHole (..), FillRequest (..), Filler (..), Stance (..))
import Olifant.Language (Grammar (..), GrammarSource (..), Language (..), Layer (..), languageFromLayers, queriesFor, queryError)
import Olifant.Language.Presets (overPresets, presetsFrom, shippedPresets)
import Olifant.Glob (matchesGlob)
import Olifant.Language.TreeSitter (knownPredicates, readCaptures)
import Olifant.Library (providersSection, queryExemplar)
import Olifant.Template.Parse (knownFilters)
import Olifant.Toml qualified as Toml
import System.FilePath (normalise, splitDirectories, takeBaseName, takeDirectory, takeExtension, (</>))

-- | The survey's answer: the grammar's repository (and, in a monorepo, the
-- folder and the name the loader reads), the globs of source and test files,
-- the manifest, and the test runners the project could use.
data Survey = Survey
  { svGrammar :: Text
  , svPath :: Maybe FilePath
  , svName :: Maybe Text
  , svFiles :: [Text]
  , svTests :: [Text]
  , svManifest :: Maybe FilePath
  , svRunners :: [Runner]
  }
  deriving stock (Eq, Show)

-- | A test runner the survey names: its program, and the argument that has
-- it write JUnit XML to @{= report =}@ when the model knows one.
data Runner = Runner {rnProgram :: Text, rnJunit :: Maybe Text}
  deriving stock (Eq, Show)

-- | A runner as @tools on PATH@ shows it: where its program is (or not on
-- PATH) and its JUnit argument.
runnerLine :: Runner -> Maybe FilePath -> Text
runnerLine r found = rnProgram r <> ": " <> maybe "not on PATH" T.pack found <> maybe "" (\j -> " (JUnit: " <> j <> ")") (rnJunit r)

-- | The survey's TOML (inside one markdown fence at most): @grammar@ and
-- @files@ are required.
parseSurvey :: Text -> Either Text Survey
parseSurvey reply = do
  top <- either (\e -> Left ("the survey is not TOML: " <> e)) Right (Toml.parseToml (unfence reply))
  grammar <- Toml.required "grammar" Toml.text top
  if T.null (T.strip grammar) then Left "the survey's `grammar` is empty" else pure ()
  files <- Toml.required "files" (Toml.list Toml.text) top
  if null files then Left "the survey's `files` is empty" else pure ()
  path <- Toml.optional "path" Toml.text top
  name <- Toml.optional "name" Toml.text top
  tests <- fromMaybe [] <$> Toml.optional "tests" (Toml.list Toml.text) top
  manifest <- Toml.optional "manifest" Toml.text top
  runners <- fromMaybe [] <$> Toml.optional "runners" (Toml.list runner) top
  pure (Survey (T.strip grammar) (T.unpack <$> path) name files tests (T.unpack <$> manifest) runners)
  where
    -- a bare program, or { program, junit }
    runner = \case
      Toml.TText p -> Right (Runner p Nothing)
      Toml.TTable t -> Runner <$> Toml.required "program" Toml.text t <*> Toml.optional "junit" Toml.text t
      other -> Left ("`runners`: expected a program or { program, junit }, got " <> Toml.describeTree other)

-- | A reply's text with one leading and one trailing markdown fence line
-- (@```toml@, @```@) dropped; anything else is left as it is.
unfence :: Text -> Text
unfence reply =
  case T.lines (T.strip reply) of
    first : rest@(_ : _)
      | "```" `T.isPrefixOf` T.strip first
      , T.strip (last rest) == "```" ->
          T.unlines (init rest)
    _ -> reply

-- | The survey: one hole, @survey@, whose answer is TOML naming the grammar
-- and the project's files. Its inputs are the language, the top-level
-- listing and the first 200 tracked paths.
surveyRequest :: Text -> [FilePath] -> FillRequest
surveyRequest language tracked =
  FillRequest
    [FillHole "survey" "toml" instruction "survey.toml" "\10214survey\10215" [] False Nothing]
    [ ("language", language)
    , ("top level", T.intercalate ", " (nub [T.pack (head' p) | p <- tracked]))
    , ("tracked files", T.intercalate "\n" (map T.pack (take 200 tracked)))
    ]
    []
    FileStance
  where
    head' p = case splitDirectories p of
      [f] -> f
      d : _ -> d <> "/"
      [] -> p
    instruction =
      T.unlines
        [ "TOML with these keys, about the " <> language <> " code in this project:"
        , "grammar = \"URL\" (the git repository of the tree-sitter grammar for " <> language <> ", https)"
        , "path = \"DIR\" (only when the grammar is one folder of a repository holding several)"
        , "name = \"NAME\" (only when the grammar's C symbol is not tree_sitter_<language>)"
        , "files = [globs] (the source files; ** spans folders)"
        , "tests = [globs] (the test files)"
        , "manifest = \"FILE\" (the project's manifest, if it has one)"
        , "runners = [{ program = \"RUNNER\" }, { program = \"WRAPPER\", junit = \"OPTION {= report =}\" }] (the test runners this project could use, by command name: RUNNER the language's own; WRAPPER a separate program that runs those tests and writes a JUnit XML file, OPTION its flag naming the file)"
        ]

-- | The sample the errand shows the model: the first two source files (not
-- tests) and the first two tests, shallowest folder first, then by path;
-- and the manifest when the project has it.
sampleFor :: Survey -> [FilePath] -> ([FilePath], [FilePath], Maybe FilePath)
sampleFor sv tracked = (take 2 sources, take 2 tests, manifest)
  where
    ordered = sortOn (\p -> (length (splitDirectories p), p)) tracked
    isTest p = any (`matchesGlob` p) (svTests sv)
    sources = [p | p <- ordered, any (`matchesGlob` p) (svFiles sv), not (isTest p)]
    tests = filter isTest ordered
    manifest = case svManifest sv of
      Just m | m `elem` tracked -> Just m
      _ -> Nothing

-- | The first N lines.
cutTo :: Int -> Text -> Text
cutTo n = T.unlines . take n . T.lines

-- | The capture names a query uses (@\@definition.function@ gives
-- @definition.function@), in order, each once.
captureNamesIn :: Text -> [Text]
captureNamesIn q = nub [T.takeWhile capChar rest | chunk <- drop 1 (T.splitOn "@" (stripComments q)), let rest = chunk, not (T.null (T.takeWhile capChar rest))]
  where
    capChar c = isAlphaNum c || c `elem` ("._-" :: String)
    stripComments = T.unlines . map (fst . T.breakOn ";") . T.lines

-- | What the grammar's checkout holds for the errand: its label (URL @SHA7),
-- its @queries/*.scm@, and its licence (file name, text); and the grammar's
-- @textobjects.scm@ from nvim-treesitter-textobjects (where it is from, as
-- @<repository> \@SHA7 queries/<g>/textobjects.scm@, and its text) with that
-- repository's licence ('Olifant.Library.textobjectsSource').
data Upstream = Upstream
  { upLabel :: Text
  , upQueries :: [(FilePath, Text)]
  , upLicence :: Maybe (FilePath, Text)
  , upNvim :: Maybe (Text, Text)
  , upNvimLicence :: Maybe (FilePath, Text)
  }
  deriving stock (Eq, Show)

-- | The query files a grammar needs, given its shipped set: none when olifant
-- ships some; else upstream's @tags.scm@ and nvim's @textobjects.scm@ copied
-- (a header line naming where each is from and its licence, and one naming
-- the directives removed) or a hole each, and an @imports@ hole, sorted.
queriesToWrite :: Text -> [(FilePath, Text)] -> Upstream -> ([(FilePath, Text)], [Text])
queriesToWrite _ shipped up
  | not (null shipped) = ([], [])
  | otherwise =
      ( [("tags.scm", copy (upLabel up) (upLicence up) t) | Just t <- [lookup "tags.scm" (upQueries up)]]
          ++ [("textobjects.scm", copy from (upNvimLicence up) t) | Just (from, t) <- [upNvim up]]
      , sort (["imports"] ++ ["tags" | Nothing <- [lookup "tags.scm" (upQueries up)]] ++ ["textobjects" | Nothing <- [upNvim up]])
      )
  where
    copy from lic text =
      let (kept, removed) = withoutDirectives text
          note = ["; directives removed: " <> T.unwords removed | not (null removed)]
       in T.unlines (("; from " <> from <> maybe "" licence lic) : note) <> kept
    licence (f, t) = " (" <> T.pack f <> case filter (not . T.null) (map T.strip (T.lines t)) of
      l : _ -> ": " <> l <> ")"
      [] -> ")"

-- | A query file without its directives (@(#strip! …)@, @(#set-adjacent! …)@:
-- instructions to the program that shows a match, which olifant does not
-- evaluate, so its loader refuses them), a line that held only directives
-- dropped, and the directives' names in order, once each. Upstream
-- @tags.scm@ files use them on @\@doc@, a capture olifant never reads.
-- Predicates (@#eq?@), comments and strings are kept as they are.
withoutDirectives :: Text -> (Text, [Text])
withoutDirectives src = (T.unlines kept, nub names)
  where
    (out, names) = go (T.unpack src)
    -- a directive's newlines are kept, so the lines still pair up
    kept = [n | (o, n) <- zip (T.lines src) (T.lines (T.pack out)), not (T.null (T.strip n)) || T.null (T.strip o)]
    go = \case
      '(' : rest
        | Just (name, after) <- directiveHead rest ->
            let (body, rest') = closing (1 :: Int) after
             in bimap (filter (== '\n') body ++) (T.pack ('#' : name) :) (go rest')
      '"' : rest -> let (str, rest') = stringBody rest in first (('"' : str) ++) (go rest')
      ';' : rest -> let (c, rest') = break (== '\n') rest in first ((';' : c) ++) (go rest')
      c : rest -> first (c :) (go rest)
      [] -> ([], [])
    directiveHead rest = case dropWhile isSpace rest of
      '#' : r | (n@(_ : _), a) <- span nameChar r, last n == '!' -> Just (n, a)
      _ -> Nothing
    nameChar c = isAlphaNum c || c `elem` ("-_?!." :: String)
    -- the text up to and including the paren that closes depth D
    closing d = \case
      ')' : s | d == 1 -> (")", s)
      ')' : s -> first (')' :) (closing (d - 1) s)
      '(' : s -> first ('(' :) (closing (d + 1) s)
      '"' : s -> let (str, s') = stringBody s in first (('"' : str) ++) (closing d s')
      c : s -> first (c :) (closing d s)
      [] -> ([], [])
    -- a string's text after its opening quote, through its closing one
    stringBody = \case
      '\\' : c : s -> first (\t -> '\\' : c : t) (stringBody s)
      '"' : s -> ("\"", s)
      c : s -> first (c :) (stringBody s)
      [] -> ([], [])

-- | The toolchain preset a new language's tests run with: @<language>-test@.
toolchainName :: Text -> Text
toolchainName l = l <> "-test"

-- | Where the errand's files go, under the project.
languageFile, toolchainFile :: Text -> FilePath
languageFile l = ".olifant/languages/" <> T.unpack l <> ".toml"
toolchainFile l = ".olifant/toolchains/" <> T.unpack (toolchainName l) <> ".toml"

-- | One key of a file the form asks: the hole's name (underscored: a fills
-- file reads a dotted key as a table), its TOML path, its kind and one-line
-- instruction as the model sees them, whether `none` is a failing row, the
-- doctor rows (by suffix) that prove it, and the template variables its
-- value may use.
data Field = Field
  { fdName :: Text
  , fdPath :: [Text]
  , fdKind :: Text
  , fdInstruction :: Text
  , fdRequired :: Bool
  , fdRows :: [Text]
  , fdVars :: [Text]
  }
  deriving stock (Eq, Show)

-- | The language file's keys the model answers (user 09, "The keys"). The
-- engine writes the rest: the header, the pinned @grammar@, @files@ (the
-- survey's globs), @[test] preset@, the ident and text holes' placeholder,
-- and @tests_for@ when 'derivedTestsFor' reads it.
languageForm :: Text -> [Field]
languageForm l =
  [ f "comment" ["comment"] "string" ("the text that starts a line comment in " <> l) True [] []
  , f "keywords" ["keywords"] "array of strings" (l <> "'s reserved words, all of them") False [] []
  , f "ignore" ["ignore"] "array of strings" ("folder names a project listing never enters: " <> l <> "'s build output and downloaded dependencies ([] when none)") False [] []
  , f "unit_words" ["unit_words"] "string" ("what a unit is in " <> l <> ", as a prompt says it: \"a top-level function or type name (Name), or a method (Type.Method)\" in " <> l <> "'s own words") False [] []
  , f "placeholder" ["placeholder"] "code over {= message =}" ("an expression that compiles in " <> l <> " and fails when run, its text {= message =}: " <> l <> "'s own not-implemented idiom (a string filter only from the template filters given)") False [] ["message"]
  , f "branch" ["branch"] "template over {= pattern =}, {= body =}" ("one arm of " <> l <> "'s multi-way branch (a case of a switch or a match), on one line: {= pattern =} the case's value, {= body =} its statement") False [] ["pattern", "body"]
  , f "separator" ["separator"] "string" ("what follows each item in a bracketed list in " <> l <> " (none when nothing does)") False [] []
  , f "line_length" ["line_length"] "integer" ("the line width " <> l <> "'s usual formatter keeps to (none when it has none)") False [] []
  , f "opens_block" ["opens_block"] "regex" "a POSIX extended regex matching a line that opens a block (how such a line ends)" False [] []
  , f "stub_lines" ["stub_lines"] "array of regexes" "POSIX extended regexes, each starting with ^, for a stripped line of a body that is still a stub (the placeholder's line)" False [] []
  , f "empty_statement" ["empty_statement"] "string" ("a statement that does nothing, when " <> l <> " needs one in an empty body (none when an empty body is fine)") False [] []
  , f "tests_for" ["test", "tests_for"] "template over {= dir =}, {= stem =}" "the test file of a source file, as a path over {= dir =} (the source's folder) and {= stem =} (its name less extension), read from the sample's test file names; never the source file itself" False ["test units", "tests"] ["dir", "stem"]
  , f "test_unit" ["test", "unit"] "array of regexes" "POSIX extended regexes (no \\s, \\d, \\w or lazy +?: use [[:space:]], [0-9], [[:alnum:]_]), each matching the line that starts one test in the sample's test files, the test's whole name its last group" False ["test units"] []
  , f "test_assertion" ["test", "assertion"] "array of regexes" "POSIX extended regexes for the line where a test's first check (its assertion) starts" False [] []
  , f "test_above" ["test", "above"] "regex" "a POSIX extended regex for a line above a test that belongs to it (an attribute or a decorator); none when tests have none" False [] []
  , f "unit_name" ["unit", "name"] "array of regexes" "POSIX extended regexes, each matching the line that defines a unit (a function, a type, a method), the unit's name its last group" False ["units"] []
  , f "unit_annotation" ["unit", "annotation"] "regex over {= name =}" "a POSIX extended regex for a name's declared type (a parameter's or a field's): {= name =} where the name goes, the type its last group" False ["sibling tests"] ["name"]
  , f "module_of" ["imports", "module_of"] "inline table { strip = [regexes], drop = [strings], separator = string }" "how a file's path becomes the module name an import names: strip removes a prefix of the path, drop the last parts that name their folder, separator joins the parts; none when imports name folders or files" False [] []
  , f "imports_write" ["imports", "write"] "template over {= module =}" "one import line that imports the module {= module =}" False [] ["module", "owner"]
  , f "imports_paths" ["imports", "paths"] "array of templates over {= path =}, {= base =}" "where an imported module's file is: {= path =} the module's parts joined by /, {= base =} the path up to the project's source folder" False [] ["path", "base"]
  , f "imports_resolve" ["imports", "resolve"] "array of inline tables { pattern = regex, module = group, names = group }" "for each form of import line: a POSIX extended regex matching it, and the group numbers of the module and of the names imported" False [] []
  ]
    ++ concat [[hole k "placeholder" (kindText k), hole k "wrap" (kindText k)] | k <- ["expr", "stmt", "block", "type"]]
    ++ [hole k "wrap" (kindText k) | k <- ["ident", "text"]]
  where
    f n p k i r rows vars = Field n p k i r rows vars
    hole :: Text -> Text -> Text -> Field
    hole k "placeholder" what =
      Field ("holes_" <> k <> "_placeholder") ["holes", k, "placeholder"] "code over {= hole =}, {= instruction =}" ("what an unfilled hole leaves where " <> what <> " goes: code that compiles in " <> l <> " there and fails when run, {= hole =} and {= instruction =} in its message") False ["holes"] ["hole", "instruction"]
    hole k _ what =
      Field ("holes_" <> k <> "_wrap") ["holes", k, "wrap"] "code over {= fill =}" ("the least " <> l <> " code around " <> what <> " that parses as a whole file: {= fill =} where it goes") False ["holes"] ["fill"]
    kindText = \case
      "expr" -> "an expression"
      "stmt" -> "a statement"
      "block" -> "a function body's statements"
      "type" -> "a type"
      "ident" -> "a new name (a declaration's)"
      _ -> "text inside a string literal"

-- | The test preset's keys the model answers. The engine writes
-- @tests_for@ (the language's) and @format@ (@junit-xml@ when the command
-- names @{= report =}@, else @lines@ when @[test.lines]@ is answered).
toolchainForm :: Text -> [Field]
toolchainForm l =
  [ Field "command" ["test", "command"] "argv (an array of strings)" ("the command that runs " <> l <> "'s tests, from the package root (the folder holding a root file) in a copy of the project: {= tests =} one element where the tests to run are named (olifant puts the selection's arguments there: select_file, select_test); {= report =} the path of the JUnit XML file the runner writes. Use a runner on PATH, the one that writes JUnit XML when there is one") True ["tests", "test runner"] ["tests", "report", "root", "cache", "context"]
  , Field "root" ["test", "root"] "array of strings" "the file names that mark a package root (the manifest's name)" True ["tests"] []
  , Field "install" ["test", "install"] "string" "one line: how to install the test runner" False [] []
  , Field "name" ["test", "name"] "template over {= words =}" "a new test's name from its words: the prefix tests need and {= words|CASE =}, CASE a case filter from the template filters" False [] ["words"]
  , Field "select_test" ["test", "select", "test"] "argv over {= name =}, {= id =}" "the arguments olifant puts in {= tests =}'s place to run one test: {= name =} the test's name, {= id =} its file's path from the package root and its name (file::name; {= id|dir =} the file's folder), {= group =} its file's group (only with module_of)" False ["tests"] ["name", "group", "id"]
  , Field "select_file" ["test", "select", "file"] "argv over {= id =}" "the arguments olifant puts in {= tests =}'s place to run one test file's tests: {= id =} the file's path from the package root ({= id|dir =} its folder), {= group =} its group (only with module_of)" False ["tests"] ["name", "group", "id"]
  , Field "select_join" ["test", "select", "join"] "string" "what joins a test's name to the names it is nested in" False ["tests"] []
  , Field "junit_join" ["test", "junit", "join"] "string" "in the JUnit report, what joins a testcase's classname parts" False ["tests"] []
  , Field "junit_split" ["test", "junit", "split"] "string" "in the JUnit report, what splits a testcase's name into its nesting" False ["tests"] []
  , Field "stub_test" ["test", "stub", "test"] "code over {= name =}, {= indent =}, {= note =}" "a new, failing test named {= name =}: {= indent =} one level of indentation, {= note =} a comment line olifant writes" False [] ["name", "unit", "module", "indent", "note"]
  , Field "stub_file" ["test", "stub", "file"] "code over {= import =}, {= module =}" "the start of a new test file, before its first test: {= import =} the unit's import, {= module =} its module" False [] ["import", "name", "unit", "module", "indent", "note"]
  , Field "stub_gap" ["test", "stub", "gap"] "integer" "blank lines between two tests" False [] []
  , Field "lines_pass" ["test", "lines", "pass"] "regex" "with no JUnit report: a POSIX extended regex for the console line of a passing test, its name the first group" False ["tests"] []
  , Field "lines_fail" ["test", "lines", "fail"] "regex" "with no JUnit report: a POSIX extended regex for the console line of a failing test, its name the first group" False ["tests"] []
  , Field "lines_failure" ["test", "lines", "failure"] "regex" "with no JUnit report: a POSIX extended regex for a failure's file:line, file and line its groups" False ["tests"] []
  ]

-- | The tables whose keys are read together: one is left out of the file
-- unless every key it needs has a value (a hole kind's placeholder and
-- wrap; a selection's test and file; a console format's pass and fail).
tableNeeds :: [([Text], [Text])]
tableNeeds =
  [(["holes", k], ["placeholder", "wrap"]) | k <- ["expr", "stmt", "block", "type", "ident", "text"]]
    ++ [(["test", "select"], ["test", "file"]), (["test", "lines"], ["pass", "fail"]), (["test", "stub"], ["test"])]

-- | A file from the engine's keys (path, TOML value) and the fields' answers
-- (by hole name, each a TOML value; @none@ and absent ones left out): the
-- top-level keys first, the engine's before the fields', then each table
-- in the order its first key appears. A table 'tableNeeds' names is
-- written only with every key it needs.
assemble :: [Field] -> [([Text], Text)] -> Map Text Text -> Text
assemble fields engine answers = T.intercalate "\n" (map section tables)
  where
    answered = [(fdPath fd, v) | fd <- fields, Just v <- [M.lookup (fdName fd) answers], not (isNone v)]
    entries = filter complete (engine ++ answered)
    complete (p, _) = case [needs | (tbl, needs) <- tableNeeds, tbl == init' p] of
      needs : _ -> all (\k -> any ((== init' p ++ [k]) . fst) (engine ++ answered)) needs
      [] -> True
    init' p = take (length p - 1) p
    tables = nub ([] : map (init' . fst) entries)
    section tbl =
      T.unlines $
        ["[" <> T.intercalate "." tbl <> "]" | not (null tbl)]
          ++ [last p <> " = " <> v | (p, v) <- entries, init' p == tbl]

-- | @none@, the model's word for a key it cannot answer (quoted or not).
isNone :: Text -> Bool
isNone v = T.toLower (T.strip v) `elem` ["none", "\"none\"", "'none'"]

-- | A model's answer for FIELD as one TOML value: @Nothing@ for @none@; a
-- key's own variables written @{{x}}@ or @{x}@ made @{= x =}@
-- ('normaliseTemplates'); a bare text of a one-string kind quoted (and a
-- double-quoted one whose escapes TOML refuses, a regex's @\\s@, re-quoted
-- as a literal), a bare argv split into words. Refused: a value that is
-- not one TOML value (a newline and a table header smuggled in) and a
-- variable that is not the key's.
valueOf :: Field -> Text -> Either Text (Maybe Text)
valueOf fd raw
  | isNone stripped = Right Nothing
  | otherwise = do
      v <- normaliseTemplates (fdVars fd) stripped
      Just . posix <$> case single v of
        Right () -> Right v
        Left (True, why) -> Left why
        Left (_, why)
          | Just t <- jsonAsToml v, Right () <- single t -> Right t
          | oneString, Just inner <- T.stripPrefix "\"" v >>= T.stripSuffix "\"", not ("\n" `T.isInfixOf` inner), Right () <- single (quoted inner) -> Right (quoted inner)
          | oneString, "{=" `T.isPrefixOf` v || not (any (`T.isPrefixOf` v) ["[", "{", "\"", "'"]), Right () <- single (quoted v) -> Right (quoted v)
          | "argv" `T.isPrefixOf` fdKind fd, not ("\n" `T.isInfixOf` v), not (any (`T.isPrefixOf` v) ["[", "\"", "'"]) -> Right ("[" <> T.intercalate ", " [basic w | w <- T.words v] <> "]")
          | otherwise -> Left why
  where
    stripped = T.strip (unfence raw)
    oneString = not (any (`T.isPrefixOf` fdKind fd) ["array", "argv", "inline", "integer"])
    -- a regex field's strings the POSIX way, re-rendered only when that changed one
    posix v
      | "regex" `T.isInfixOf` fdKind fd
      , Right top <- Toml.parseToml ("v = " <> v <> "\n")
      , Just tree <- M.lookup "v" top
      , let tree' = mapStrings normaliseRegex tree
      , tree' /= tree =
          renderTree tree'
      | otherwise = v
    -- Left (more than one key?, why)
    single v = case Toml.parseToml ("v = " <> v <> "\n") of
      Left e -> Left (False, "`" <> fdName fd <> "` is not a TOML value: " <> e)
      Right top
        | M.keys top == ["v"] -> Right ()
        | otherwise -> Left (True, "`" <> fdName fd <> "` is more than one value: the answer is one value, the right side of `" <> last (fdPath fd) <> " =`")
    quoted t
      | not ("'" `T.isInfixOf` t), not ("\n" `T.isInfixOf` t) = "'" <> t <> "'"
      | not ("'''" `T.isInfixOf` t) = "'''\n" <> t <> "'''"
      | otherwise = basic t
    basic t = "\"" <> T.concatMap (\case '"' -> "\\\""; '\\' -> "\\\\"; '\n' -> "\\n"; c -> T.singleton c) t <> "\""

-- | A regex as POSIX extended expressions read it: Perl's classes (@\\s@,
-- @\\d@, @\\w@ and their negations) as bracket classes, inside a bracket
-- expression too, and a lazy repeat (@*?@, @+?@, @??@) as the plain one. A
-- model writes them however it is told; init normalises, the loader never does.
normaliseRegex :: Text -> Text
normaliseRegex = T.pack . outside . T.unpack
  where
    outside = \case
      '\\' : c : r | Just cls <- lookup c classes -> cls ++ outside r
      '\\' : c : r -> '\\' : c : outside r
      q : '?' : r | q `elem` ("*+?" :: String) -> q : outside r
      '[' : r -> '[' : bracketStart r
      c : r -> c : outside r
      [] -> []
    bracketStart = \case
      '^' : ']' : r -> '^' : ']' : inside r
      '^' : r -> '^' : inside r
      ']' : r -> ']' : inside r
      r -> inside r
    inside = \case
      '[' : ':' : r -> let (cls, rest) = break (== ']') r in '[' : ':' : cls ++ take 1 rest ++ inside (drop 1 rest)
      '\\' : c : r | Just cls <- lookup c inBracket -> cls ++ inside r
      ']' : r -> ']' : outside r
      c : r -> c : inside r
      [] -> []
    classes = [('s', "[[:space:]]"), ('S', "[^[:space:]]"), ('d', "[0-9]"), ('D', "[^0-9]"), ('w', "[[:alnum:]_]"), ('W', "[^[:alnum:]_]")]
    inBracket = [('s', "[:space:]"), ('d', "0-9"), ('w', "[:alnum:]_")]

-- | Every string in a TOML value through F.
mapStrings :: (Text -> Text) -> Toml.Tree -> Toml.Tree
mapStrings f = \case
  Toml.TText t -> Toml.TText (f t)
  Toml.TList xs -> Toml.TList (map (mapStrings f) xs)
  Toml.TTable t -> Toml.TTable (M.map (mapStrings f) t)
  other -> other

-- | A TOML value as text: strings in double quotes, arrays and inline tables.
renderTree :: Toml.Tree -> Text
renderTree = \case
  Toml.TText t -> "\"" <> T.concatMap (\case '"' -> "\\\""; '\\' -> "\\\\"; '\n' -> "\\n"; c -> T.singleton c) t <> "\""
  Toml.TInt n -> T.pack (show n)
  Toml.TDouble d -> T.pack (show d)
  Toml.TBool b -> if b then "true" else "false"
  Toml.TList xs -> "[" <> T.intercalate ", " (map renderTree xs) <> "]"
  Toml.TTable t -> "{" <> T.intercalate ", " [k <> " = " <> renderTree v | (k, v) <- M.toList t] <> "}"
  Toml.TOther o -> o

-- | A JSON array or object (a model writes @{ "strip": [...] }@ for an inline
-- table) as the TOML value it means; anything else none.
jsonAsToml :: Text -> Maybe Text
jsonAsToml t = case Aeson.decodeStrict (TE.encodeUtf8 t) of
  Just v@(Aeson.Object _) -> go v
  Just v@(Aeson.Array _) -> go v
  _ -> Nothing
  where
    go = \case
      Aeson.Object o -> (\kvs -> "{" <> T.intercalate ", " kvs <> "}") <$> mapM (\(k, v) -> ((AK.toText k <> " = ") <>) <$> go v) (sortOn fst (AKM.toList o))
      Aeson.Array a -> (\vs -> "[" <> T.intercalate ", " vs <> "]") <$> mapM go (toList a)
      Aeson.String x -> Just ("\"" <> T.concatMap (\case '"' -> "\\\""; '\\' -> "\\\\"; '\n' -> "\\n"; c -> T.singleton c) x <> "\"")
      Aeson.Number n -> Just (either (T.pack . show) (T.pack . show) (floatingOrInteger n :: Either Double Integer))
      Aeson.Bool b -> Just (if b then "true" else "false")
      Aeson.Null -> Nothing

-- | A model's text with VARS (a key's own template variables) written
-- @{{x}}@, @{{ x }}@ or @{x}@ made @{= x =}@ (a model writes those however it
-- is told; init normalises, the loader never does). Refused: a @{= v =}@ whose
-- variable is not one of VARS (@if@, @else@ and @end@ are the template's own).
normaliseTemplates :: [Text] -> Text -> Either Text Text
normaliseTemplates vars src =
  case [v | v <- variablesIn out, v `notElem` vars] of
    [] -> Right out
    bad : _ -> Left ("{= " <> bad <> " =} is not one of this key's variables" <> if null vars then " (it has none)" else ": " <> T.intercalate ", " ["{= " <> v <> " =}" | v <- vars])
  where
    -- the double-braced spellings first: {x} is inside {{x}}
    out = foldl (\t v -> foldl (\t' (a, b) -> T.replace a b t') t [("{{" <> v <> "}}", new v), ("{{ " <> v <> " }}", new v), ("{" <> v <> "}", new v)]) src vars
    new v = "{= " <> v <> " =}"
    variablesIn t =
      [ name
      | chunk <- drop 1 (T.splitOn "{=" t)
      , let inside = T.strip (fst (T.breakOn "=}" chunk))
      , let ws = T.words (T.takeWhile (/= '|') inside)
      , name <- case ws of
          ["end"] -> []
          ["else"] -> []
          "if" : n : _ -> [n]
          n : _ -> [n]
          [] -> []
      ]

-- | A key a reader's error names, as the field that writes it: the
-- backquoted names in the message (@`test`: `above`: …@, @`[test.lines]
-- pass`@) as one path, matched against each field's path; the longest
-- path all of whose parts the message names wins.
errorField :: [Field] -> Text -> Maybe Field
errorField fields msg = case errorFields fields msg of
  fd : _ -> Just fd
  [] -> Nothing

-- | Every field a reader's error names: the backquoted names and the
-- @[table] key@ mentions in it, matched against the fields' paths (all of
-- a path's parts named), longest first; else the one field whose key alone
-- is named.
errorFields :: [Field] -> Text -> [Field]
errorFields fields msg = case sortOn (negate . length . fdPath) [fd | fd <- fields, last (fdPath fd) `elem` named, all (`elem` named) (fdPath fd)] of
  fds@(_ : _) -> fds
  [] -> case [fd | fd <- fields, last (fdPath fd) `elem` named] of
    [fd] -> [fd]
    _ -> []
  where
    quotedParts = [q | (i, q) <- zip [0 :: Int ..] (T.splitOn "`" msg), odd i]
    -- `[imports] module_of` outside backquotes: the table and the word after it
    tableRefs = [tbl <> " " <> T.takeWhile keyChar (T.stripStart after) | chunk <- drop 1 (T.splitOn "[" msg), let (tbl, rest) = T.breakOn "]" chunk, not (T.null rest), T.all (\c -> keyChar c || c == '.') tbl, let after = T.drop 1 rest]
    keyChar c = isAlphaNum c || c == '_'
    named = concatMap (T.words . T.map (\c -> if c `elem` ("[].=" :: String) then ' ' else c)) (quotedParts ++ tableRefs)

-- | @tests_for@ read off the project's names: for a sample source @d/s.e@
-- (by 'sampleFor''s order) and a tracked test file @d/t.e@ in the same folder
-- whose stem holds @s@, @{= dir =}/@ and @t@ with @s@ made @{= stem =}@, and
-- @.e@; the first pair found. None when no test sits beside a source (a
-- template over @{= dir =}@ cannot say @src/A@ → @test/ASpec@).
derivedTestsFor :: Survey -> [FilePath] -> Maybe Text
derivedTestsFor sv tracked =
  case [ "{= dir =}/" <> before <> "{= stem =}" <> after <> T.pack (takeExtension t)
       | s <- sources
       , t <- tests
       , takeDirectory s == takeDirectory t
       , takeExtension s == takeExtension t
       , let ss = T.pack (takeBaseName s)
             ts = T.pack (takeBaseName t)
       , ss /= ts
       , not (T.null ss)
       , ss `T.isInfixOf` ts
       , let (before, rest) = T.breakOn ss ts
             after = T.drop (T.length ss) rest
       ] of
    d : _ -> Just d
    [] -> Nothing
  where
    ordered = sortOn (\p -> (length (splitDirectories p), p)) tracked
    isTest p = any (`matchesGlob` p) (svTests sv)
    sources = [p | p <- ordered, any (`matchesGlob` p) (svFiles sv), not (isTest p)]
    tests = filter isTest ordered

-- | Round 1 of the errand, a form: one hole per key of the language file and
-- of its test preset ('languageForm', 'toolchainForm'; a derived
-- @tests_for@ is not asked), each answered with its value, and one per
-- query file still to write. Its inputs are the facts: the language, the
-- grammar, the survey's globs, the test runners on PATH, the template
-- filters and query predicates olifant knows (closed sets: a model invents
-- @go_string@ or @#strip!@ without them), and, for the query holes, the
-- grammar's node kinds and the captures upstream and olifant use. Its
-- context is the sample's files, and for a textobjects hole the one query
-- exemplar ('queryExemplar': the shape is what to copy there).
formRequest :: ErrandInput -> FillRequest
formRequest ei =
  FillRequest
    ( [fieldHole (languageFile l) fd | fd <- languageForm l, not (derived && fdName fd == "tests_for")]
        ++ [fieldHole (toolchainFile l) fd | fd <- toolchainForm l]
        ++ map queryHole queryHoles
    )
    ( [ ("language", l)
      , ("grammar", upLabel up)
      , ("source globs", T.intercalate ", " (svFiles sv))
      , ("test globs", T.intercalate ", " (svTests sv))
      , ("tools on PATH", T.intercalate "\n" (eiTools ei))
      , ("template filters", "only these, after a bar ({= message|escaped =}): " <> T.intercalate ", " knownFilters)
      ]
        ++ ( if null queryHoles
               then []
               else
                 [ ("node kinds", T.intercalate ", " (eiKinds ei))
                 , ("captures", T.intercalate ", " (nub (concatMap (captureNamesIn . snd) (upQueries up))))
                 , ("captures olifant reads", T.intercalate ", " (readCaptures ++ ["definition.<kind>"]))
                 , ("query predicates", "only these; no directives (#set!, #strip!, …): " <> T.intercalate ", " (map ("#" <>) knownPredicates))
                 ]
           )
    )
    (eiSample ei ++ [("exemplar query (another grammar's): textobjects.scm", snd queryExemplar) | "textobjects" `elem` queryHoles])
    ValueStance
  where
    l = eiLanguage ei
    sv = eiSurvey ei
    up = eiUpstream ei
    derived = isJust (derivedTestsFor sv (eiTracked ei))
    queryHoles = snd (errandQueries ei)
    fieldHole file fd =
      FillHole (fdName fd) (fdKind fd) (fdInstruction fd) file (keyContext fd) [] False Nothing
    queryHole h = FillHole h "file" (queryInstruction h) (".olifant/queries/" <> T.unpack h <> ".scm") ("\10214" <> h <> "\10215") [] False Nothing
    queryInstruction = \case
      "tags" -> "tree-sitter queries (tags.scm) over the node kinds given: each definition @definition.<kind> (function, method, class, …) with its name @name."
      "imports" -> "tree-sitter queries (imports.scm) over the node kinds given: each import declaration @import."
      "textobjects" -> "tree-sitter queries (textobjects.scm) over the node kinds given, in the exemplar's shape: @function.outer, @function.inner (the body's contents), @class.outer, @class.inner, @parameter.inner and @call.outer."
      h -> "tree-sitter queries (" <> h <> ".scm) over the node kinds given."

-- | A field's hole as the model sees it: @key = ⟦name⟧@ under its table.
keyContext :: Field -> Text
keyContext fd = T.intercalate "\n" (["[" <> T.intercalate "." (init (fdPath fd)) <> "]" | length (fdPath fd) > 1] ++ [last (fdPath fd) <> " = \10214" <> fdName fd <> "\10215"])

-- | The query files the errand writes: those copied from upstream (path
-- under the grammar's query folder, text) and the holes asked.
errandQueries :: ErrandInput -> ([(FilePath, Text)], [Text])
errandQueries ei = queriesToWrite (grName (eiGrammar ei)) (eiShipped ei (grName (eiGrammar ei))) (eiUpstream ei)

-- | The @SOURCES@ file beside a project's generated queries: each file and
-- where it came from (copied from the grammar's repository, or written by
-- the model), as @library/queries/<g>/SOURCES@ does.
sourcesFile :: Text -> Text -> [(FilePath, Text)] -> [Text] -> Text
sourcesFile g grammar copied holes =
  T.unlines $
    ["# Tree-sitter queries for the " <> g <> " grammar (" <> grammar <> "), written by olifant init --language."]
      ++ sort ([T.pack f <> "  copied from " <> origin f t <> removed t | (f, t) <- copied] ++ [h <> ".scm  written by the model, compiled against the grammar" | h <- holes])

  where
    -- a file copied from elsewhere than the grammar's repository names its
    -- path in its header (nvim's textobjects)
    origin f t = case T.lines t of
      l : _ | Just from <- T.stripPrefix "; from " l, let o = fst (T.breakOn " (" from), " queries/" `T.isInfixOf` o -> o
      _ -> grammar <> " queries/" <> T.pack f
    -- the copied file's own header line says which directives went
    removed t = case [r | l <- T.lines t, Just r <- [T.stripPrefix "; directives removed: " l]] of
      r : _ -> ", directives removed (" <> T.intercalate ", " (T.words r) <> ")"
      [] -> ""


-- | The pinned grammar as a language file's @grammar@ line: olifant's URL
-- and revision, never the model's text.
pinText :: Grammar -> Text
pinText g = case grSource g of
  Just (SourceGit url rev path) ->
    "grammar = { git = " <> quoted url <> ", rev = " <> quoted rev <> maybe "" (\p -> ", path = " <> quoted (T.pack p)) path <> ", name = " <> quoted (grName g) <> " }"
  _ -> "grammar = " <> quoted (grName g)
  where
    quoted t = "\"" <> T.concatMap (\c -> if c `elem` ['"', '\\'] then T.pack ['\\', c] else T.singleton c) t <> "\""

-- | What the errand needs once the grammar is loaded: the language, its
-- pinned grammar, the grammar's node kinds, the checkout's queries and
-- licence (and nvim's textobjects, 'Upstream'), the shipped query set by
-- grammar name (production: 'shippedFor'), the test runners found on PATH,
-- the sample's files, the project folder (a layer's own folder), the
-- survey and the project's tracked paths (what 'derivedTestsFor' and the
-- @tests_for@ check read).
data ErrandInput = ErrandInput
  { eiLanguage :: Text
  , eiGrammar :: Grammar
  , eiKinds :: [Text]
  , eiUpstream :: Upstream
  , eiShipped :: Text -> [(FilePath, Text)]
  , eiTools :: [Text]
  , eiSample :: [(FilePath, Text)]
  , eiProjectDir :: FilePath
  , eiSurvey :: Survey
  , eiTracked :: [FilePath]
  }

-- | The query set olifant ships for a grammar.
shippedFor :: Text -> [(FilePath, Text)]
shippedFor = queriesFor

-- | A round's checked answers: the files to write (path under the project,
-- text), the rows that failed (a key the model answered wrong, a file that
-- does not load, a query that does not compile), the answers they were
-- made from (a repair's are put over them), and the warn rows that do not
-- stop the files (a key left out, a command that names no @{= tests =}@):
-- the repair is asked about them too.
data Answered = Answered {anFiles :: [(FilePath, Text)], anFailing :: [Check], anAnswers :: Map Text Text, anWarnings :: [Check]}
  deriving stock (Eq, Show)

-- | A key that fails and is not required left out (answered none) and the
-- files checked again, until no such key fails; each left out is a warn row
-- with why. A required key, a whole file's row and a query keep failing.
-- Before the repair it lets doctor read the first round's files; after it,
-- what still fails is left out of the files written.
leaveOut :: ErrandInput -> Answered -> Answered
leaveOut ei = leaveOutWith (`M.member` optionalField) (checkAnswers ei)
  where
    optionalField = M.fromList [(fdName fd, ()) | fd <- languageForm "" ++ toolchainForm "", not (fdRequired fd)]

-- | 'leaveOut' with the fields that may be left out (OPTIONAL) and the
-- check that reads the answers again (CHECK): a shipped language's layer
-- may leave out any key, the shipped file still has it.
leaveOutWith :: (Text -> Bool) -> (Map Text Text -> Answered) -> Answered -> Answered
leaveOutWith optional check = go (length (languageForm "" ++ toolchainForm ""))
  where
    go :: Int -> Answered -> Answered
    go n a = case [(f, c) | c <- anFailing a, let f = T.drop 2 (snd (T.breakOn ": " (checkName c))), optional f, isField f, M.lookup f (anAnswers a) /= Just "none"] of
      dropped@(_ : _) | n > 0 ->
        let again = check (foldr (\(f, _) -> M.insert f "none") (anAnswers a) dropped)
            warned = [c {checkStatus = Warn, checkDetail = "left out: " <> checkDetail c} | (_, c) <- dropped]
         in go (n - 1) again {anWarnings = anWarnings a ++ warned}
      _ -> a
    isField f = f `elem` map fdName (languageForm "" ++ toolchainForm "")

-- | The answers (by hole name) checked before anything is written. Each
-- field's value is read by 'valueOf' (a value that is not one TOML value,
-- or uses another key's variables, is a row naming its field; a required
-- one answered @none@ too); the files are assembled ('assemble') over the
-- engine's keys; the facts the engine can check are checked (the command's
-- @{= tests =}@, @tests_for@ on a sampled source); the toolchain must read
-- as a preset and the language load over it as a project layer, a reader's
-- error naming its field ('errorField', else the whole file's row); each
-- query must compile against the grammar. Rows are named @<l>: <field>@.
checkAnswers :: ErrandInput -> Map Text Text -> Answered
checkAnswers ei answers =
  Answered
    ([(languageFile l, languageText) | null languageErrors] ++ [(toolchainFile l, toolchainText) | null toolchainErrors] ++ [f | Right f <- queries])
    -- one row per field: a value's own row before the load error it causes
    (nubBy (\a b -> checkName a == checkName b) (valueRows ++ factRows ++ toolchainErrors ++ languageErrors) ++ [c | Left c <- queries])
    answers
    noTests
  where
    l = eiLanguage ei
    g = eiGrammar ei
    sv = eiSurvey ei
    derived = derivedTestsFor sv (eiTracked ei)
    langFields = [fd | fd <- languageForm l, not (isJust derived && fdName fd == "tests_for")]
    toolFields = toolchainForm l
    row name detail = Check (l <> ": " <> name) Bad detail (Just "regenerate the key or supply a provider (see chapter 9, Providers)")
    read' fd = (fd, maybe (Right Nothing) (valueOf fd) (M.lookup (fdName fd) answers))
    readings = map read' (langFields ++ toolFields)
    valueRows =
      [row (fdName fd) why | (fd, Left why) <- readings]
        ++ [row (fdName fd) ("`" <> fdName fd <> "` is required; the model answered none") | (fd, Right Nothing) <- readings, fdRequired fd]
    values = M.fromList [(fdName fd, v) | (fd, Right (Just v)) <- readings]
    testsFor = maybe (M.lookup "tests_for" values) (Just . quote) derived
    command = M.lookup "command" values
    junit = maybe False ("{= report =}" `T.isInfixOf`) command
    linesAnswered = all (`M.member` values) ["lines_pass", "lines_fail"]
    format
      | junit = Just "junit-xml"
      | linesAnswered = Just "lines"
      | otherwise = Nothing
    toolValues = if junit then M.filterWithKey (\k _ -> not ("lines_" `T.isPrefixOf` k)) values else values
    engineLanguage =
      [(["grammar"], T.drop (T.length "grammar = ") (pinText g)), (["files"], array (svFiles sv)), (["test", "preset"], quote (toolchainName l))]
        ++ [(["test", "tests_for"], quote d <> "  # from the sample's names") | Just d <- [derived]]
        ++ [(["holes", k, "placeholder"], "\"{= hole =}\"") | k <- ["ident", "text"]]
    engineToolchain = [(["test", "tests_for"], t) | Just t <- [testsFor]] ++ [(["test", "format"], quote f) | Just f <- [format]]
    languageText = T.unlines [initHeader] <> assemble langFields engineLanguage values
    toolchainText = T.unlines [initHeader] <> assemble toolFields engineToolchain toolValues
    -- a command naming no {= tests =} runs, every time, all the tests it names itself
    noTests =
      [ (row "command" "the command has no element that is exactly {= tests =}, so every run runs all the tests it names: keep its program, and put {= tests =}, alone, where it names the tests to run (olifant puts the selection's arguments there, select_file and select_test)") {checkStatus = Warn}
      | Just c <- [command], not (testsElement c)
      ]
    -- the run puts the selection only in an element that is exactly {= tests =}
    testsElement c = case Toml.parseToml ("v = " <> c <> "\n") >>= Toml.required "v" (Toml.list Toml.text) of
      Right argv -> "{= tests =}" `elem` map T.strip argv
      Left _ -> "{= tests =}" `T.isInfixOf` c
    factRows =
      [ row "command" "`format`: the command names no {= report =} and no [test.lines] pass and fail were answered: a runner that writes JUnit XML to {= report =}, or the console's lines"
           | isJust command, isNothing format
           ]
        ++ [r | Nothing <- [derived], Just t <- [testsFor], r <- testsForRows t]
    -- tests_for on the first sampled source: another file, and a test file by the survey's globs
    testsForRows t = case sampleFor sv (eiTracked ei) of
      (src : _, _, _)
        | Right raw <- Toml.parseToml ("v = " <> t <> "\n") >>= Toml.required "v" Toml.text ->
            let mapped = normalise (T.unpack (T.replace "{= stem =}" (T.pack (takeBaseName src)) (T.replace "{= dir =}" (T.pack (takeDirectory src)) raw)))
                beside = case [p | p <- eiTracked ei, any (`matchesGlob` p) (svTests sv), takeDirectory p == takeDirectory src] of
                  p : _ -> "; the sample's test file beside it is " <> T.pack p
                  [] -> ""
                says why = [row "tests_for" ("maps " <> T.pack src <> " to " <> T.pack mapped <> why <> beside)]
             in if
                  | mapped == normalise src -> says " (itself)"
                  | not (null (svTests sv)), not (any (`matchesGlob` mapped) (svTests sv)) -> says (" (not a test file by the globs " <> T.intercalate ", " (svTests sv) <> ")")
                  | otherwise -> []
      _ -> []
    presets = case presetsFrom "project" [(toolchainName l, toolchainText)] of
      Right p -> overPresets shippedPresets p
      Left _ -> shippedPresets
    loadError file why = case errorFields (langFields ++ toolFields) why of
      fds@(_ : _) -> [row (fdName fd) ("does not load: " <> why) | fd <- fds]
      -- the engine writes `format` by the command
      [] | "`format`" `T.isInfixOf` why -> [row "command" ("does not load: " <> why)]
      [] -> [row file ("does not load: " <> why)]
    toolchainErrors = case presetsFrom "project" [(toolchainName l, toolchainText)] of
      Left why -> loadError "toolchain" why
      Right _ -> []
    languageErrors = case Toml.parseToml languageText >>= \top -> languageFromLayers presets l [Layer "project" (Just (eiProjectDir ei)) top] of
      Left why -> loadError "language" why
      Right lang
        | langGrammar lang /= g -> [row "language" "does not load: its grammar is not the pinned one"]
        | otherwise -> []
    queries =
      [ case M.lookup h answers of
          Nothing -> Left (Check (l <> ": " <> h) Bad ("the model gave no `" <> h <> "`") Nothing)
          Just t ->
            let file = T.unpack h <> ".scm"
                text = unfence t
             in case queryError g [(file, text)] of
                  Just why -> Left (Check (l <> ": queries") Bad (T.pack file <> ": " <> why) Nothing)
                  Nothing -> Right (".olifant" </> "queries" </> T.unpack (grName g) </> file, text)
      | h <- snd (errandQueries ei)
      ]
    quote t = "\"" <> T.concatMap (\case '"' -> "\\\""; '\\' -> "\\\\"; c -> T.singleton c) t <> "\""
    array ts = "[" <> T.intercalate ", " (map quote ts) <> "]"

-- | The errand after the gate: the query files to write (copied or asked),
-- the form ('formRequest'), and the answers checked. The request comes back
-- for a repair round.
errandCore :: Monad m => Filler m -> ErrandInput -> m (Either Text (FillRequest, Answered))
errandCore filler ei = do
  let g = eiGrammar ei
      up = eiUpstream ei
      (copied, queryHoles) = errandQueries ei
      req = formRequest ei
      dir = ".olifant" </> "queries" </> T.unpack (grName g)
      -- nvim's licence beside its file (Apache-2.0 asks for the notice)
      licences = [(dir </> "LICENSE-nvim-treesitter", t) | Just (_, t) <- [upNvimLicence up], isJust (lookup "textobjects.scm" copied)]
      queryFiles =
        [(dir </> f, t) | (f, t) <- copied]
          ++ licences
          ++ [(dir </> "SOURCES", sourcesFile (grName g) (upLabel up) copied queryHoles) | not (null copied && null queryHoles)]
  runFill filler req >>= \case
    Left why -> pure (Left why)
    Right answers -> do
      let checked = checkAnswers ei answers
      pure (Right (req, checked {anFiles = anFiles checked ++ queryFiles}))

-- | Doctor's rows about a language: its capability, grammar, queries and
-- sibling rows (@<l>: …@), its tool rows (@… (<l>)@), its own
-- @language <l>@ line, and a load error naming one of the files WRITTEN (a
-- query file the loader refused is named by its path, not the language).
languageRows :: Text -> [FilePath] -> [Check] -> [Check]
languageRows l written = filter ours
  where
    ours c =
      let n = checkName c
       in (l <> ": ") `T.isPrefixOf` n
            || (" (" <> l <> ")") `T.isSuffixOf` n
            || n == "language " <> l
            || ("languages/" <> l <> ".toml") `T.isInfixOf` n
            || any (\p -> ("/" <> T.pack p) `T.isSuffixOf` n || T.pack p == n) [p | p <- written, "language " `T.isPrefixOf` n]

-- | The rows a repair round answers: the language's errors, its evidence
-- that disproved a key (the @regenerate the key@ fix), and a queries or
-- grammar warn. Not an unproven sibling row, an absent optional capability
-- (conjure shapes, signature, mutation, stub: a new language starts at the
-- floor), nor another language's rows.
failingRows :: Text -> [FilePath] -> [Check] -> [Check]
failingRows l written = filter failing . languageRows l written
  where
    failing c =
      checkStatus c == Bad
        || (checkStatus c == Warn && maybe False ("regenerate the key" `T.isInfixOf`) (checkFix c))
        || (checkStatus c == Warn && checkName c `elem` [l <> ": queries", l <> ": grammar"])

-- | The holes of REQ the rows point at, by the fields' 'fdRows': a row named
-- after a field (an init-time row) that field; @test units@, @tests@,
-- @holes@, @units@, @sibling tests@ the fields they prove; a test runner
-- row the command; @anchors@ the textobjects hole and @units@ the tags hole
-- when asked; a queries row its file's hole (or every query hole); a row
-- of the whole language or toolchain file (a load error no field names)
-- that file's every field; a grammar row none.
repairHoles :: FillRequest -> [Check] -> [Text]
repairHoles req rows = [h | h <- names, h `elem` wanted]
  where
    names = map fhName (frHoles req)
    fields = languageForm "" ++ toolchainForm ""
    fieldNames = map fdName fields
    queryHoles = filter (`notElem` fieldNames) names
    inFile f = [fhName h | h <- frHoles req, fhName h `elem` fieldNames, f (fhFile h)]
    wanted = concatMap pointsAt rows
    pointsAt c =
      let n = checkName c
          suffix = T.drop 2 (snd (T.breakOn ": " n))
       in if
            | suffix `elem` names -> [suffix]
            | hs@(_ : _) <- [h | h <- queryHoles, ("/" <> h <> ".scm") `T.isSuffixOf` n] -> hs
            | suffix == "queries" -> case [h | h <- queryHoles, (h <> ".scm") `T.isPrefixOf` checkDetail c] of
                [] -> queryHoles
                hs -> hs
            | suffix == "grammar" -> []
            | suffix == "language" || ("language " `T.isPrefixOf` n && "/languages/" `T.isInfixOf` n) -> inFile ("languages/" `isInfixOf'`)
            | suffix == "toolchain" || ("language " `T.isPrefixOf` n && "/toolchains/" `T.isInfixOf` n) -> inFile ("toolchains/" `isInfixOf'`)
            | "test runner " `T.isPrefixOf` n -> ["command"]
            | suffix == "anchors" -> ["textobjects"]
            | otherwise -> ["tags" | suffix == "units"] ++ [fdName fd | fd <- fields, suffix `elem` fdRows fd]
    isInfixOf' a p = a `T.isInfixOf` T.pack p

-- | The repair round: the holes the failing rows point at, renamed
-- @<h>_repair@ (a recorded model answers each name with one text, so the
-- second round needs its own names; the marker too, or the model answers
-- under the old name), each told its value fails the rows; the files as
-- assembled and the rows (name, detail, fix) in its context, and the
-- providers' protocol in its inputs (supplying a provider is a fix).
repairRequest :: FillRequest -> [(FilePath, Text)] -> [Check] -> FillRequest
repairRequest req written rows =
  req
    { frHoles =
        [ h
            { fhName = fhName h <> "_repair"
            , fhContext = T.replace marker (renamed (fhName h)) (fhContext h)
            , fhInstruction = fhInstruction h <> "\nThe value as written (in the file below) fails the rows under \"failing rows\": write it again so they pass."
            }
        | h <- frHoles req
        , fhName h `elem` asked
        , let marker = "\10214" <> fhName h <> "\10215"
        ]
    , frInputs = frInputs req ++ [("protocol", providersSection)]
    , frContext = frContext req ++ written ++ [("failing rows", T.unlines [checkName c <> " — " <> checkDetail c <> maybe "" ("; fix: " <>) (checkFix c) | c <- rows])]
    }
  where
    asked = repairHoles req rows
    renamed h = "\10214" <> h <> "_repair\10215"

-- | The repaired fields (by name) behind a row that passed BEFORE the repair
-- and does not AFTER it (by the fields' 'fdRows'): the repair may not break
-- what passed, so these go back to the first round's values.
regressedFields :: [Check] -> [Check] -> [Text] -> [Text]
regressedFields before after repaired =
  [ fdName fd
  | fd <- languageForm "" ++ toolchainForm ""
  , fdName fd `elem` repaired
  , any (`elem` fdRows fd) broke
  ]
  where
    suffix c = T.drop 2 (snd (T.breakOn ": " (checkName c)))
    broke = [suffix a | b <- before, checkStatus b == Ok, a <- after, checkName a == checkName b, checkStatus a /= Ok]

-- | A repair's answers to the holes REQ asked, under their fields' names
-- (@<h>_repair@ less the suffix); any other dropped (a recorded model
-- answers every name it has).
repairAnswers :: FillRequest -> Map Text Text -> Map Text Text
repairAnswers req answers = M.fromList [(h, v) | (k, v) <- M.toList answers, k `elem` map fhName (frHoles req), Just h <- [T.stripSuffix "_repair" k]]

-- | The grammar checkout's @queries/*.scm@ (beside the grammar's folder in a
-- monorepo, else at the checkout's root) and its licence, read from disk,
-- under LABEL (URL @SHA7). No checkout: none.
readUpstream :: Text -> Maybe FilePath -> Maybe FilePath -> IO Upstream
readUpstream label checkout path = case checkout of
  Nothing -> pure (Upstream label [] Nothing Nothing Nothing)
  Just dir -> do
    found <- filterM doesDirectoryExist ([dir </> p </> "queries" | Just p <- [path]] ++ [dir </> "queries"])
    queries <- case found of
      q : _ -> do
        names <- sort . filter ((== ".scm") . takeExtension) <$> listDirectory q
        mapM (\n -> (n,) <$> readUtf8 (q </> n)) names
      [] -> pure []
    tops <- sort <$> listDirectory dir
    licence <- case [f | f <- tops, "LICENSE" `T.isPrefixOf` T.toUpper (T.pack f)] of
      f : _ -> Just . (f,) <$> readUtf8 (dir </> f)
      [] -> pure Nothing
    pure (Upstream label queries licence Nothing Nothing)
  where
    readUtf8 f = TE.decodeUtf8With lenientDecode <$> BS.readFile f

-- | The grammar's @textobjects.scm@ from nvim-treesitter-textobjects, at the
-- repository and commit the settings name ('gsTextobjects': olifant's, never
-- the model's): fetched once into the cache (@grammars/src/<repo>-<sha7>@,
-- 'fetch''s safety), then @queries/<g>/textobjects.scm@ read with where it is
-- from (@<repo> \@SHA7 queries/<g>/textobjects.scm@), and the licence. A
-- grammar nvim has no file for: none. A fetch that fails: why.
nvimTextobjects :: GrammarSettings -> Text -> IO (Either Text (Maybe (Text, Text), Maybe (FilePath, Text)))
nvimTextobjects s g = do
  let (url, sha) = gsTextobjects s
      repo = let n = last ("" : filter (not . T.null) (T.splitOn "/" url)) in fromMaybe n (T.stripSuffix ".git" n)
      checkout = gsCache s </> "grammars" </> "src" </> T.unpack (repo <> "-" <> T.take 7 sha)
      file = "queries" </> T.unpack g </> "textobjects.scm"
  there <- doesDirectoryExist checkout
  fetched <- if there then pure (Right ()) else fetch s url sha checkout
  case fetched of
    Left f -> pure (Left (failureDetail repo f))
    Right () -> do
      found <- doesFileExist (checkout </> file)
      text <- if found then Just <$> readUtf8 (checkout </> file) else pure Nothing
      tops <- sort <$> listDirectory checkout
      licence <- case [f | f <- tops, "LICENSE" `T.isPrefixOf` T.toUpper (T.pack f)] of
        f : _ -> Just . (f,) <$> readUtf8 (checkout </> f)
        [] -> pure Nothing
      pure (Right ((\t -> (repo <> " @" <> T.take 7 sha <> " " <> T.pack file, t)) <$> text, licence))
  where
    readUtf8 f = TE.decodeUtf8With lenientDecode <$> BS.readFile f

-- | A language olifant ships, and the project layer init writes over it for
-- what doctor finds failing: the shipped file (its text, shown the model,
-- and its layer, under which the answers must load), its test preset (name
-- and shipped text) when its @[test]@ names one, the runners found on PATH,
-- the sample, the project folder, the survey and the tracked paths, and the
-- @tests_for@ read off the names when a failing row points at it
-- ('derivedTestsFor'; the engine writes it, the model is not asked).
data LayerInput = LayerInput
  { liLanguage :: Text
  , liShipped :: Text
  , liLayer :: Layer
  , liPreset :: Maybe (Text, Text)
  , liTools :: [Text]
  , liSample :: [(FilePath, Text)]
  , liProjectDir :: FilePath
  , liSurvey :: Survey
  , liTracked :: [FilePath]
  , liDerived :: Maybe Text
  }

-- | The keys a shipped language's layer may set: the language file's and
-- its test preset's, as the form asks them for a new language.
layerForm :: Text -> [Field]
layerForm l = languageForm l ++ toolchainForm l

-- | Where a project's toolchain preset of that name lives.
presetFile :: Text -> FilePath
presetFile p = ".olifant/toolchains/" <> T.unpack p <> ".toml"

-- | The file a layer's field is written to: a preset's key to the project's
-- preset of the shipped name (@.olifant/toolchains/cargo-test.toml@) when the
-- language names a preset, else to the language's layer, under its @[test]@.
layerFileOf :: LayerInput -> Field -> FilePath
layerFileOf li fd = case liPreset li of
  Just (p, _) | presetSide fd -> presetFile p
  _ -> languageFile (liLanguage li)

presetSide :: Field -> Bool
presetSide fd = fdName fd `elem` map fdName (toolchainForm "")

-- | What the failing ROWS ask of a shipped language: the @tests_for@ read off
-- the project's names when a row points at it (so it is not asked), and the
-- fields the model answers ('repairHoles'' row-to-field mapping over the
-- whole 'layerForm').
layerAsk :: LayerInput -> [Check] -> (Maybe Text, [Text])
layerAsk li rows = (derived, [h | h <- pointed, not (isJust derived && h == "tests_for")])
  where
    pointed = repairHoles (layerRequest li [] (map fdName (layerForm (liLanguage li)))) rows
    derived
      | "tests_for" `elem` pointed = derivedTestsFor (liSurvey li) (liTracked li)
      | otherwise = Nothing

-- | The errand for a shipped language: one hole per field NAMES (the keys
-- doctor's failing ROWS need), each answered with its value or @none@ (the
-- shipped value stays); the facts as for a new language; the context the
-- sample, the shipped file (and its preset when a preset key is asked) and
-- the rows. No grammar, no query holes: olifant ships them.
layerRequest :: LayerInput -> [Check] -> [Text] -> FillRequest
layerRequest li rows names =
  FillRequest
    [ FillHole (fdName fd) (fdKind fd) (fdInstruction fd <> "\nThe shipped value (in the shipped file below) fails doctor's rows: write the value this project needs, or none to keep the shipped one.") (layerFileOf li fd) (keyContext fd) [] False Nothing
    | fd <- asked
    ]
    [ ("language", l)
    , ("source globs", T.intercalate ", " (svFiles sv))
    , ("test globs", T.intercalate ", " (svTests sv))
    , ("tools on PATH", T.intercalate "\n" (liTools li))
    , ("template filters", "only these, after a bar ({= message|escaped =}): " <> T.intercalate ", " knownFilters)
    ]
    ( liSample li
        ++ [("library/languages/" <> T.unpack l <> ".toml (shipped with olifant; the project's layer goes over it)", liShipped li)]
        ++ [("library/toolchains/" <> T.unpack p <> ".toml (shipped with olifant; the project's preset goes over it)", t) | Just (p, t) <- [liPreset li], any presetSide asked]
        ++ [("doctor's failing rows", T.unlines [checkName c <> " — " <> checkDetail c | c <- rows]) | not (null rows)]
    )
    ValueStance
  where
    l = liLanguage li
    sv = liSurvey li
    asked = [fd | fd <- layerForm l, fdName fd `elem` names]

-- | Which keys a layer's TEXT sets that it may not: every key path must be
-- one of ALLOWED, or inside one (an inline table's keys), or a table on the
-- way to one. A key the failing rows do not need is refused, naming it.
layerKeysOk :: [[Text]] -> Text -> Either Text ()
layerKeysOk allowed text = do
  top <- Toml.parseToml text
  case [p | p <- paths [] top, not (any (`isPrefixOf` p) allowed)] of
    [] -> Right ()
    p : _ -> Left ("sets `" <> T.intercalate "." p <> "`, which no failing row needs")
  where
    paths pre t =
      concat
        [ case v of
            Toml.TTable sub | p `notElem` allowed, any (p `isPrefixOf`) allowed -> paths p sub
            _ -> [p]
        | (k, v) <- M.toList t
        , let p = pre ++ [k]
        ]

-- | A shipped language's answers (by hole name; a field is read only when
-- it was asked, which is when it is in ANSWERS) checked before anything is
-- written: each value read by 'valueOf' (no field is required: the shipped
-- file has them); the layer assembled from the language's fields (and the
-- derived @tests_for@) and the preset from the preset's (and @format@, by
-- the command, when it changes), each written only when it has a key;
-- 'layerKeysOk' on each; the preset read over the shipped presets and the
-- language loaded as the shipped layer and this one, a reader's error
-- naming its field. Rows are @<l>: <field>@ (@<l>: layer@ for a key no row
-- needs).
checkLayer :: LayerInput -> Map Text Text -> Answered
checkLayer li answers =
  Answered
    ([(languageFile l, t) | Just t <- [languageText], null languageErrors] ++ [(presetFile p, t) | Just p <- [preset], Just t <- [toolchainText], null toolchainErrors])
    (nubBy (\a b -> checkName a == checkName b) (valueRows ++ toolchainErrors ++ languageErrors))
    answers
    noTests
  where
    l = liLanguage li
    preset = fst <$> liPreset li
    row name detail = Check (l <> ": " <> name) Bad detail (Just "regenerate the key or supply a provider (see chapter 9, Providers)")
    fields = [fd | fd <- layerForm l, M.member (fdName fd) answers, not (isJust (liDerived li) && fdName fd == "tests_for")]
    readings = [(fd, valueOf fd v) | fd <- fields, Just v <- [M.lookup (fdName fd) answers]]
    valueRows = [row (fdName fd) why | (fd, Left why) <- readings]
    values = M.fromList [(fdName fd, v) | (fd, Right (Just v)) <- readings]
    inPreset fd = isJust preset && presetSide fd
    langFields = filter (not . inPreset) fields
    toolFields = filter inPreset fields
    command = M.lookup "command" values
    junit = maybe False ("{= report =}" `T.isInfixOf`) command
    linesAnswered = all (`M.member` values) ["lines_pass", "lines_fail"]
    format
      | junit = [(["test", "format"], quote "junit-xml")]
      | linesAnswered = [(["test", "format"], quote "lines")]
      | otherwise = []
    engineLanguage = [(["test", "tests_for"], quote d <> "  # from the sample's names") | Just d <- [liDerived li]] ++ (if isJust preset then [] else format)
    engineToolchain = if isJust preset then format else []
    answeredIn fs = any (\fd -> M.member (fdName fd) values) fs
    languageText
      | answeredIn langFields || not (null engineLanguage) = Just (T.unlines [initHeader] <> assemble langFields engineLanguage values)
      | otherwise = Nothing
    toolchainText
      | answeredIn toolFields = Just (T.unlines [initHeader] <> assemble toolFields engineToolchain values)
      | otherwise = Nothing
    keysRow fs engine = maybe [] (\t -> either (\why -> [row "layer" why]) (const []) (layerKeysOk (map fdPath fs ++ map fst engine) t))
    presets = case (preset, toolchainText) of
      (Just p, Just t) | Right ps <- presetsFrom "project" [(p, t)] -> overPresets shippedPresets ps
      _ -> shippedPresets
    loadError file why = case errorFields fields why of
      fds@(_ : _) -> [row (fdName fd) ("does not load: " <> why) | fd <- fds]
      [] | "`format`" `T.isInfixOf` why -> [row "command" ("does not load: " <> why)]
      [] -> [row file ("does not load: " <> why)]
    toolchainErrors = case (preset, toolchainText) of
      (Just p, Just t) -> keysRow toolFields engineToolchain toolchainText ++ either (loadError "toolchain") (const []) (presetsFrom "project" [(p, t)])
      _ -> []
    languageErrors =
      keysRow langFields engineLanguage languageText
        ++ case Toml.parseToml (fromMaybe "" languageText) >>= \top -> languageFromLayers presets l [liLayer li, Layer "project" (Just (liProjectDir li)) top] of
          Left why -> loadError "language" why
          Right _ -> []
    noTests =
      [ (row "command" "the command has no element that is exactly {= tests =}, so every run runs all the tests it names: keep its program, and put {= tests =}, alone, where it names the tests to run (olifant puts the selection's arguments there, select_file and select_test)") {checkStatus = Warn}
      | Just c <- [command]
      , case Toml.parseToml ("v = " <> c <> "\n") >>= Toml.required "v" (Toml.list Toml.text) of
          Right argv -> "{= tests =}" `notElem` map T.strip argv
          Left _ -> not ("{= tests =}" `T.isInfixOf` c)
      ]
    quote t = "\"" <> T.concatMap (\case '"' -> "\\\""; '\\' -> "\\\\"; c -> T.singleton c) t <> "\""

-- | The rows a shipped language's layer answers: 'failingRows', less a
-- @tests@ row whose run had results and none passed (the project's tests
-- fail; that is not a key's to answer). A broken run, with no results, is
-- sent.
shippedFailing :: Text -> [FilePath] -> [Check] -> [Check]
shippedFailing l written = filter (not . testsFailed) . failingRows l written
  where
    testsFailed c = checkName c == l <> ": tests" && " passed; the first failure" `T.isInfixOf` checkDetail c
