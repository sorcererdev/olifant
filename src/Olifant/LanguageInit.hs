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
  , errandRequest
  , Field (..)
  , languageForm
  , toolchainForm
  , tableNeeds
  , assemble
  , isNone
  , valueOf
  , normaliseTemplates
  , errorField
  , derivedTestsFor
  , languageFile
  , toolchainFile
  , toolchainName
  , sourcesFile
  , pinText
  , withPin
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
  , readUpstream
  ) where

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Data.ByteString qualified as BS
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory (doesDirectoryExist, listDirectory)
import Data.Bifunctor (bimap, first)
import Data.Char (isAlphaNum, isSpace)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.List (nub, sort, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Doctor (Check (..), Status (..))
import Olifant.Fill (FillHole (..), FillRequest (..), Filler (..), Stance (..))
import Olifant.Language (Grammar (..), GrammarSource (..), Language (..), Layer (..), languageFromLayers, queriesFor, queryError)
import Olifant.Language.Presets (overPresets, presetsFrom, shippedPresets)
import Olifant.Glob (matchesGlob)
import Olifant.Language.TreeSitter (knownPredicates, readCaptures)
import Olifant.Library (providersSection, shippedLanguages, shippedToolchains)
import Olifant.Template.Parse (knownFilters)
import Olifant.Toml qualified as Toml
import System.FilePath (splitDirectories, takeBaseName, takeDirectory, takeExtension, (</>))

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
  , svRunners :: [Text]
  }
  deriving stock (Eq, Show)

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
  runners <- fromMaybe [] <$> Toml.optional "runners" (Toml.list Toml.text) top
  pure (Survey (T.strip grammar) (T.unpack <$> path) name files tests (T.unpack <$> manifest) runners)

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
        , "runners = [programs] (the test runners this project could use, by command name)"
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

-- | What the grammar's checkout holds for the errand: its label (URL @SHA),
-- its @queries/*.scm@, and its licence (file name, text).
data Upstream = Upstream {upLabel :: Text, upQueries :: [(FilePath, Text)], upLicence :: Maybe (FilePath, Text)}
  deriving stock (Eq, Show)

-- | The query files a grammar needs, given its shipped set: none when olifant
-- ships some; else upstream's @tags.scm@ copied (a header line naming where
-- it is from and its licence, and one naming the directives removed) or a
-- @tags@ hole, and @imports@ and @textobjects@ holes, sorted.
queriesToWrite :: Text -> [(FilePath, Text)] -> Upstream -> ([(FilePath, Text)], [Text])
queriesToWrite _ shipped up
  | not (null shipped) = ([], [])
  | otherwise = case lookup "tags.scm" (upQueries up) of
      Just tags ->
        let (kept, removed) = withoutDirectives tags
            note = ["; directives removed: " <> T.unwords removed | not (null removed)]
         in ([("tags.scm", T.unlines (header : note) <> kept)], ["imports", "textobjects"])
      Nothing -> ([], ["imports", "tags", "textobjects"])
  where
    header = "; from " <> upLabel up <> maybe "" licence (upLicence up)
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
  , f "placeholder" ["placeholder"] "code over {= message =}" ("an expression that compiles in " <> l <> " and fails when run, its text {= message =}: " <> l <> "'s own not-implemented idiom (a string filter only from the template filters given)") False ["holes"] ["message"]
  , f "branch" ["branch"] "template over {= pattern =}, {= body =}" ("one arm of " <> l <> "'s multi-way branch (a case of a switch or a match), on one line: {= pattern =} the case's value, {= body =} its statement") False [] ["pattern", "body"]
  , f "separator" ["separator"] "string" ("what follows each item in a bracketed list in " <> l <> " (none when nothing does)") False [] []
  , f "line_length" ["line_length"] "integer" ("the line width " <> l <> "'s usual formatter keeps to (none when it has none)") False [] []
  , f "opens_block" ["opens_block"] "regex" "a POSIX extended regex matching a line that opens a block (how such a line ends)" False [] []
  , f "stub_lines" ["stub_lines"] "array of regexes" "POSIX extended regexes, each starting with ^, for a stripped line of a body that is still a stub (the placeholder's line)" False [] []
  , f "empty_statement" ["empty_statement"] "string" ("a statement that does nothing, when " <> l <> " needs one in an empty body (none when an empty body is fine)") False [] []
  , f "tests_for" ["test", "tests_for"] "template over {= dir =}, {= stem =}" "the test file of a source file, as a path over {= dir =} (the source's folder) and {= stem =} (its name less extension), read from the sample's test file names; never the source file itself" False ["test units", "tests"] ["dir", "stem"]
  , f "test_unit" ["test", "unit"] "array of regexes" "POSIX extended regexes (no \\s, \\d, \\w or lazy +?: use [[:space:]], [0-9], [[:alnum:]_]), each matching the line that starts one test in the sample's test files, the test's whole name its last group" False ["test units"] []
  , f "test_assertion" ["test", "assertion"] "array of regexes" "POSIX extended regexes for the line where a test's first check (its assertion) starts" False ["test units"] []
  , f "test_above" ["test", "above"] "regex" "a POSIX extended regex for a line above a test that belongs to it (an attribute or a decorator); none when tests have none" False ["test units"] []
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
  [ Field "command" ["test", "command"] "argv (an array of strings)" ("the command that runs " <> l <> "'s tests, from the package root (the folder holding a root file) in a copy of the project: {= tests =} one element, the selected tests' arguments; {= report =} the JUnit XML file a runner that writes one is told to write; it must reach the package's tests from that folder. Use a runner on PATH, one that writes JUnit XML when there is one") True ["tests", "test runner"] ["tests", "report", "root", "cache", "context"]
  , Field "root" ["test", "root"] "array of strings" "the file names that mark a package root (the manifest's name)" True ["tests"] []
  , Field "install" ["test", "install"] "string" "one line: how to install the test runner" False [] []
  , Field "name" ["test", "name"] "template over {= words =}" "a new test's name from its words: the prefix tests need and {= words|CASE =}, CASE a case filter from the template filters" False [] ["words"]
  , Field "select_test" ["test", "select", "test"] "argv over {= name =}, {= group =}" "the arguments, in {= tests =}'s place, that run one test: {= name =} its name, {= group =} its file's group" False ["tests"] ["name", "group", "id"]
  , Field "select_file" ["test", "select", "file"] "argv over {= group =}" "the arguments, in {= tests =}'s place, that run one test file's tests: {= group =} its group, {= id =} its path" False ["tests"] ["name", "group", "id"]
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
      Just <$> case single v of
        Right () -> Right v
        Left (True, why) -> Left why
        Left (_, why)
          | oneString, Just inner <- T.stripPrefix "\"" v >>= T.stripSuffix "\"", not ("\n" `T.isInfixOf` inner), Right () <- single (quoted inner) -> Right (quoted inner)
          | oneString, not (any (`T.isPrefixOf` v) ["[", "{", "\"", "'"]), Right () <- single (quoted v) -> Right (quoted v)
          | "argv" `T.isPrefixOf` fdKind fd, not ("\n" `T.isInfixOf` v), not (any (`T.isPrefixOf` v) ["[", "\"", "'"]) -> Right ("[" <> T.intercalate ", " [basic w | w <- T.words v] <> "]")
          | otherwise -> Left why
  where
    stripped = T.strip (unfence raw)
    oneString = not (any (`T.isPrefixOf` fdKind fd) ["array", "argv", "inline", "integer"])
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
errorField fields msg = case sortOn (negate . length . fdPath) [fd | fd <- fields, last (fdPath fd) `elem` named, all (`elem` named) (fdPath fd)] of
  fd : _ -> Just fd
  [] -> case [fd | fd <- fields, last (fdPath fd) `elem` named] of
    [fd] -> Just fd
    _ -> Nothing
  where
    quotedParts = [q | (i, q) <- zip [0 :: Int ..] (T.splitOn "`" msg), odd i]
    named = concatMap (T.words . T.map (\c -> if c `elem` ("[].=" :: String) then ' ' else c)) quotedParts

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

-- | The errand: one hole per file (@language@, @toolchain@, and each query
-- file to write), the shipped Rust pair as exemplars, the sample's files,
-- and the grammar's facts (its node kinds, the captures upstream uses and
-- the ones olifant reads), the test runners found on PATH, the template
-- filters and query predicates olifant knows (closed sets: a model invents
-- @go_string@ or @#strip!@ without them) and the providers' protocol.
errandRequest :: Text -> Text -> [Text] -> [Text] -> [Text] -> [(FilePath, Text)] -> [Text] -> FillRequest
errandRequest language grammar kinds captures tools sample queryHoles =
  FillRequest
    ([fileHole "language" (languageFile language) languageInstruction, fileHole "toolchain" (toolchainFile language) toolchainInstruction] ++ map queryHole queryHoles)
    [ ("language", language)
    , ("grammar", grammar)
    , ("node kinds", T.intercalate ", " kinds)
    , ("captures", T.intercalate ", " captures)
    , ("captures olifant reads", T.intercalate ", " (readCaptures ++ ["definition.<kind>"]))
    , ("tools on PATH", T.intercalate "\n" tools)
    , ("template filters", "only these, after a bar ({= message|escaped =}): " <> T.intercalate ", " knownFilters)
    , ("query predicates", "only these; no directives (#set!, #strip!, …): " <> T.intercalate ", " (map ("#" <>) knownPredicates))
    , ("protocol", providersSection)
    ]
    ( [("exemplar: rust.toml", t) | Just t <- [lookup "rust" shippedLanguages]]
        ++ [("exemplar: cargo-test.toml", t) | Just t <- [lookup "cargo-test" shippedToolchains]]
        ++ sample
    )
    FileStance
  where
    fileHole name file instruction = FillHole name "file" instruction file ("\10214" <> name <> "\10215") [] False Nothing
    queryHole h = fileHole h (".olifant/queries/" <> T.unpack h <> ".scm") (queryInstruction h)
    languageInstruction =
      T.unlines
        [ "The language file for " <> language <> ", in the exemplar rust.toml's form: its keys, read from the sample files."
        , "Its tests: [test] preset = \"" <> toolchainName language <> "\" (the toolchain hole), tests_for, unit and assertion regexes (POSIX classes, as the exemplar)."
        , "Write grammar = { git = \"URL\", rev = \"REV\" } as given; olifant replaces it with its pin."
        , "Leave out a key the sample does not show you how to answer."
        , "Leave out [check]: the user writes the check command, not the model (the exemplar's [check] names a preset this language does not have)."
        ]
    toolchainInstruction =
      T.unlines
        [ "The test preset " <> toolchainName language <> ", in the exemplar cargo-test.toml's form: [test] command, format, root, tests_for, install."
        , "Run tests only with a tool listed on PATH; a format the protocol names (junit reads {= report =})."
        ]
    queryInstruction = \case
      "tags" -> "tree-sitter queries (tags.scm) over the node kinds given: each definition @definition.<kind> (function, method, class, …) with its name @name."
      "imports" -> "tree-sitter queries (imports.scm) over the node kinds given: each import declaration @import."
      "textobjects" -> "tree-sitter queries (textobjects.scm) over the node kinds given: @function.outer, @function.inner, @class.outer, @class.inner, @parameter.inner and @call.outer."
      h -> "tree-sitter queries (" <> h <> ".scm) over the node kinds given."

-- | The @SOURCES@ file beside a project's generated queries: each file and
-- where it came from (copied from the grammar's repository, or written by
-- the model), as @library/queries/<g>/SOURCES@ does.
sourcesFile :: Text -> Text -> [(FilePath, Text)] -> [Text] -> Text
sourcesFile g grammar copied holes =
  T.unlines $
    ["# Tree-sitter queries for the " <> g <> " grammar (" <> grammar <> "), written by olifant init --language."]
      ++ sort ([T.pack f <> "  copied from " <> grammar <> " queries/" <> T.pack f <> removed t | (f, t) <- copied] ++ [h <> ".scm  written by the model, compiled against the grammar" | h <- holes])

  where
    -- the copied file's own header line says which directives went
    removed t = case [r | l <- T.lines t, Just r <- [T.stripPrefix "; directives removed: " l]] of
      r : _ -> ", directives removed (" <> T.intercalate ", " (T.words r) <> ")"
      [] -> ""

-- | The first line of every file @init --language@ writes.
initHeader :: Text
initHeader = "# written by olifant init --language"

-- | The pinned grammar as a language file's @grammar@ line: olifant's URL
-- and revision, never the model's text.
pinText :: Grammar -> Text
pinText g = case grSource g of
  Just (SourceGit url rev path) ->
    "grammar = { git = " <> quoted url <> ", rev = " <> quoted rev <> maybe "" (\p -> ", path = " <> quoted (T.pack p)) path <> ", name = " <> quoted (grName g) <> " }"
  _ -> "grammar = " <> quoted (grName g)
  where
    quoted t = "\"" <> T.concatMap (\c -> if c `elem` ['"', '\\'] then T.pack ['\\', c] else T.singleton c) t <> "\""

-- | A language file's text with its top-level @grammar@ lines (before the
-- first table header) dropped and the pin and 'initHeader' put first.
withPin :: Text -> Text -> Text
withPin pin text =
  T.unlines ([initHeader, pin] ++ filter (not . grammarLine) top ++ rest)
  where
    (top, rest) = break isHeader (T.lines (unfence text))
    isHeader l = "[" `T.isPrefixOf` T.stripStart l
    grammarLine l = case T.stripPrefix "grammar" (T.stripStart l) of
      Just after -> "=" `T.isPrefixOf` T.stripStart after
      Nothing -> l == initHeader

-- | What the errand needs once the grammar is loaded: the language, its
-- pinned grammar and label, the grammar's node kinds, the checkout's
-- queries and licence, the shipped query set by grammar name (production:
-- 'shippedFor'), the test runners found on PATH, the sample's files, and
-- the project folder (a layer's own folder).
data ErrandInput = ErrandInput
  { eiLanguage :: Text
  , eiGrammar :: Grammar
  , eiKinds :: [Text]
  , eiUpstream :: Upstream
  , eiShipped :: Text -> [(FilePath, Text)]
  , eiTools :: [Text]
  , eiSample :: [(FilePath, Text)]
  , eiProjectDir :: FilePath
  }

-- | The query set olifant ships for a grammar.
shippedFor :: Text -> [(FilePath, Text)]
shippedFor = queriesFor

-- | A round's checked answers: the files to write (path under the project,
-- text) and the rows that failed (a file that does not load, a query that
-- does not compile, a hole with no answer).
data Answered = Answered {anFiles :: [(FilePath, Text)], anFailing :: [Check]}
  deriving stock (Eq, Show)

-- | The answers checked before anything is written (by hole name, with
-- the round's suffix: "" or "_repair"): the language must load as a project
-- layer over the toolchain answered (its @grammar@ replaced by the pin), the
-- toolchain must read as a preset, each query must compile against the
-- grammar. Only the holes asked are read; a repair round that does not ask
-- for the toolchain loads the language over the one written (WRITTEN).
checkAnswers :: ErrandInput -> [(FilePath, Text)] -> [Text] -> Text -> Map Text Text -> Answered
checkAnswers ei written asked suffix answers =
  Answered (concat [fs | Right fs <- results]) [c | Left c <- results]
  where
    l = eiLanguage ei
    g = eiGrammar ei
    answer h = M.lookup (h <> suffix) answers
    results = map one asked
    noAnswer h = Left (Check (l <> ": " <> h) Bad ("the model gave no `" <> h <> suffix <> "`") Nothing)
    doesNotLoad h why = Left (Check (l <> ": " <> h) Bad ("does not load: " <> why) (Just "regenerate the key or supply a provider (see chapter 9, Providers)"))
    toolchainText = fmap (\t -> T.unlines [initHeader] <> unfence t) (answer "toolchain")
    presets = case toolchainText <|> lookup (toolchainFile l) written of
      Just t | Right p <- presetsFrom "project" [(toolchainName l, t)] -> overPresets shippedPresets p
      _ -> shippedPresets
    one = \case
      "language" -> case answer "language" of
        Nothing -> noAnswer "language"
        Just t ->
          let text = withPin (pinText g) t
           in case Toml.parseToml text >>= \top -> languageFromLayers presets l [Layer "project" (Just (eiProjectDir ei)) top] of
                Left why -> doesNotLoad "language" why
                Right lang
                  | langGrammar lang /= g -> doesNotLoad "language" "its grammar is not the pinned one"
                  | otherwise -> Right [(languageFile l, text)]
      "toolchain" -> case toolchainText of
        Nothing -> noAnswer "toolchain"
        Just t -> case presetsFrom "project" [(toolchainName l, t)] of
          Left why -> doesNotLoad "toolchain" why
          Right _ -> Right [(toolchainFile l, t)]
      h -> case answer h of
        Nothing -> noAnswer h
        Just t ->
          let file = T.unpack h <> ".scm"
              text = unfence t
           in case queryError g [(file, text)] of
                Just why -> Left (Check (l <> ": queries") Bad (T.pack file <> ": " <> why) Nothing)
                Nothing -> Right [(".olifant" </> "queries" </> T.unpack (grName g) </> file, text)]

-- | The errand after the gate: the query files to write (copied or asked),
-- one request for the language, its toolchain and the asked queries, and
-- the answers checked. The request comes back for a repair round.
errandCore :: Monad m => Filler m -> ErrandInput -> m (Either Text (FillRequest, Answered))
errandCore filler ei = do
  let g = eiGrammar ei
      up = eiUpstream ei
      (copied, queryHoles) = queriesToWrite (grName g) (eiShipped ei (grName g)) up
      captures = nub (concatMap (captureNamesIn . snd) (upQueries up))
      req = errandRequest (eiLanguage ei) (upLabel up) (eiKinds ei) captures (eiTools ei) (eiSample ei) queryHoles
      dir = ".olifant" </> "queries" </> T.unpack (grName g)
      queryFiles =
        [(dir </> f, t) | (f, t) <- copied]
          ++ [(dir </> "SOURCES", sourcesFile (grName g) (upLabel up) copied queryHoles) | not (null copied && null queryHoles)]
  runFill filler req >>= \case
    Left why -> pure (Left why)
    Right answers -> do
      let checked = checkAnswers ei [] (map fhName (frHoles req)) "" answers
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

-- | The holes of REQ the rows point at: a row named after a hole (a file that
-- did not load), a queries row's file (or every query hole), a test runner
-- or tests row the toolchain, a grammar row none, anything else the
-- language file.
repairHoles :: FillRequest -> [Check] -> [Text]
repairHoles req rows = [h | h <- names, h `elem` wanted]
  where
    names = map fhName (frHoles req)
    queryHoles = filter (`notElem` ["language", "toolchain"]) names
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
            | "test runner " `T.isPrefixOf` n || suffix == "tests" -> ["toolchain"]
            | otherwise -> ["language"]

-- | The repair round: the holes the failing rows point at, renamed
-- @<h>_repair@ (a recorded model answers each name with one text, so the
-- second round needs its own names; the marker too, or the model answers
-- under the old name), with the files as written and the
-- rows (name, detail, fix) in its context.
repairRequest :: FillRequest -> [(FilePath, Text)] -> [Check] -> FillRequest
repairRequest req written rows =
  req
    { frHoles = [h {fhName = fhName h <> "_repair", fhContext = "\10214" <> fhName h <> "_repair\10215", fhInstruction = fhInstruction h <> "\nThe file below as written fails the rows under \"failing rows\": write it again, whole, so they pass."} | h <- frHoles req, fhName h `elem` asked]
    , frContext = frContext req ++ written ++ [("failing rows", T.unlines [checkName c <> " — " <> checkDetail c <> maybe "" ("; fix: " <>) (checkFix c) | c <- rows])]
    }
  where
    asked = repairHoles req rows

-- | The grammar checkout's @queries/*.scm@ (beside the grammar's folder in a
-- monorepo, else at the checkout's root) and its licence, read from disk,
-- under LABEL (URL @SHA7). No checkout: none.
readUpstream :: Text -> Maybe FilePath -> Maybe FilePath -> IO Upstream
readUpstream label checkout path = case checkout of
  Nothing -> pure (Upstream label [] Nothing)
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
    pure (Upstream label queries licence)
  where
    readUtf8 f = TE.decodeUtf8With lenientDecode <$> BS.readFile f
