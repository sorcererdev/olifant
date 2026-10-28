-- | Language adapters (spec §12): pack data in `languages/<name>.toml`.
module Olifant.Language.Adapter
  ( Language (..)
  , Grammar (..)
  , TargetDef (..)
  , CheckDef (..)
  , ErrorFormat (..)
  , supportOf
  , TestDef (..)
  , Select (..)
  , LinesFormat (..)
  , HoleKind (..)
  , ImportMerge (..)
  , MutateDef (..)
  , Transform (..)
  , parseLanguage
  , Layer (..)
  , languageFromLayers
  , shippedLayer
  , parseOverShipped
  , originOf
  , matchesGlob
  , languageFor
  , targetPhrase
  , matchShape
  , regexName
  , defaultUnitNames
  , defaultUnitWords
  ) where

import Control.Monad ((>=>))
import Data.Aeson (Value, object, (.=))
import Data.Bifunctor (first)
import Data.Char (isUpper)
import Data.List (find)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Glob (matchesGlob)
import Olifant.Language.Presets (Presets (..), applyPreset, shippedPresets)
import Olifant.Library (shippedLanguages)
import Olifant.Language.Rule
import Olifant.Language.Diagnostics (CompilerSupport)
import Olifant.Language.Generic (ErrorFormat (..), LinesFormat (..))
import Olifant.Language.Support (formatSupport, knownFormats, knownTestFormats)
import Olifant.Template (Template, describeTemplateError, freeVariables, parseTemplate)
import Data.Set qualified as S
import Olifant.Toml
import Text.Regex.TDFA (AllTextSubmatches (..), Regex, makeRegexM, matchM)
import System.FilePath (takeFileName, (</>))

data Language = Language
  { langName :: Text
  , langFiles :: [Text]
  , langComment :: Text
  , langGrammar :: Grammar
  , langTargets :: [TargetDef]
  , langPlaceholder :: Maybe Template
  , langBranch :: Maybe Template
  , langCheck :: Maybe CheckDef
  , langIndent :: Text
  -- ^ one level of indentation (key `indent`, default four spaces): used when
  -- an empty container is opened
  , langHoles :: [(Text, HoleKind)]
  -- ^ `[holes.KIND]` (spec §6.3): the hole kinds this language supports
  , langImports :: Maybe Text
  -- ^ `imports`: a POSIX expression matching one import line. A fill may
  -- begin with such lines; they join the file's imports
  , langSeparator :: Maybe Text
  -- ^ `separator`: what follows each item of a container (`,`), so an item
  -- can join a one-line container (inline, or one item per line)
  , langLineLength :: Int
  -- ^ `line_length` (default 88): the longest line an inline item may make
  , langImportMerge :: Maybe ImportMerge
  -- ^ `[import_merge]`: how an import of names from a module joins a line
  -- that already imports from it
  , langKeywords :: [Text]
  -- ^ `keywords`: the language's reserved words, which recognition keeps
  -- as they are (names become their case shape)
  , langTest :: Maybe TestDef
  -- ^ `[test]`: how the tests of a file are run (a preset names the plumbing)
  , langUnitKinds :: [Text]
  -- ^ `[unit]` kinds: the node kinds of a definition (a function, method or
  -- class); the innermost at the cursor is the unit
  , langUnitNames :: [Text]
  -- ^ `[unit] name`: POSIX expressions over a unit's line; the first that
  -- matches names it by its last group (the whole match when it has none)
  , langUnitJoin :: Bool
  -- ^ `[unit] join`: units of one name separated only by blank lines are one
  -- (a Haskell signature and its equations)
  , langUnitWords :: Text
  -- ^ `unit_words`: what a unit is in this language, as prompts say it
  -- ("a top-level class or function name (Name) or a method (Class.method)")
  , langArmSeparator :: Text
  -- ^ `arm_separator`: appended to the last alternative before a new one,
  -- unless it already ends with it or ends a block (`,` in Rust)
  , langLoneArmSpaced :: Bool
  -- ^ `lone_arm_spaced`: after a match's only arm, a new one goes after a
  -- blank line (Elm's and Haskell's style); with two or more, the existing
  -- spacing is copied
  , langMutate :: [MutateDef]
  -- ^ `[[mutate]]`: the mutation operators, ways to break this language's
  -- code that a test must notice
  , langLayers :: [Text]
  -- ^ where the language comes from, first to last: `shipped`, `pack NAME`,
  -- `project`
  , langOrigins :: M.Map Text Text
  -- ^ each key (`indent`, `check`, `check.format`) and the layer that set it
  -- last, or `toolchain NAME` for a [check] or [test] key its preset filled
  }
  deriving stock (Show)

-- | A mutation operator (`[[mutate]]`): its name, the ast-grep rule of its
-- sites (`pattern = P` is the rule @{pattern: P}@), and what a site becomes:
-- a `fix` (ast-grep's, with the rule's metavariables) or a 'Transform' of
-- the matched text.
data MutateDef = MutateDef {mutName :: Text, mutRule :: Value, mutFix :: Either Transform Text}
  deriving stock (Eq, Show)

-- | `transform = "increment"`: an integer literal plus one (none for one
-- that does not read as an integer).
data Transform = Increment
  deriving stock (Eq, Show)

-- | How a hole of one kind is written unfilled ('kindPlaceholder', with
-- @hole@ and @instruction@; it must compile) and the code a fill must parse
-- in ('kindWrap', containing the literal @{= fill =}@, or @{= fill|indent =}@
-- for a body: every line one level in).
data HoleKind = HoleKind {kindPlaceholder :: Template, kindWrap :: Text}
  deriving stock (Show)

-- | `[import_merge]`: 'mergePattern' matches an import line, its first group
-- the module and its second the names (comma-separated); 'mergeWrite' writes
-- a line from @module@ and @names@.
data ImportMerge = ImportMerge {mergePattern :: Text, mergeWrite :: Template}
  deriving stock (Show)

data Grammar = BuiltInGrammar Text | GrammarSource FilePath
  deriving stock (Eq, Show)

-- | 'targetSpaced' (key `spaced`, default true): copy a blank line between
-- the items next to the new one. False where a blank line means something
-- else (Python's import groups).
--
-- 'targetWhenEmpty' (key `when_empty`): with no items and no brackets to
-- open, `"after header"` puts the text after the file's leading comments
-- and docstring (Python's imports in a file with none).
--
-- 'targetSeparator' (key `separator`): this target's, over the language's
-- (`""`: its one-line containers never grow — a `|` union is not a list).
data TargetDef = TargetDef {targetShape :: [Text], targetContainer :: Rule, targetItems :: Rule, targetExample :: Text, targetSpaced :: Bool, targetWhenEmpty :: Maybe Text, targetSeparator :: Maybe Text}
  deriving stock (Show)

-- | A language's check: its command, the context files it needs (globs),
-- its report format, and the files that mark a package's root (`root`):
-- a check runs in the judged file's package, the nearest folder holding one
-- (`root_pick = "outermost"`: the outermost, a Cargo workspace).
-- @checkExclusive@: the tool keeps a build cache (cargo's target dir), so
-- each check gets a cache no other check uses while it runs (a slot).
-- @checkConcurrent@ (default true): false for a compiler that cannot run
-- twice at once (Elm), so its checks run one at a time.
-- @checkErrorFormat@: `[check.errorformat]`, for `format = "errorformat"`.
-- @checkConvert@ (key `convert`): a command that turns the check's output
-- into olifant-json, for a tool Olifant has no format for.
-- @checkInstall@ (key `install`): how to install the tool, doctor's fix.
data CheckDef = CheckDef {checkCommand :: [Text], checkContext :: [Text], checkFormat :: Text, checkRoot :: [Text], checkRootOutermost :: Bool, checkExclusive :: Bool, checkConcurrent :: Bool, checkErrorFormat :: Maybe ErrorFormat, checkConvert :: [Text], checkInstall :: Maybe Text}
  deriving stock (Eq, Show)

-- | The compiler support that reads a check's report.
supportOf :: CheckDef -> Maybe CompilerSupport
supportOf c = formatSupport (checkFormat c) (checkErrorFormat c)

-- | A language's test runner: the command (its @{= tests =}@ stands for the
-- test files), the context files and package root as in 'CheckDef', the
-- report format, and where a module's tests live (@tests_for@, a template
-- over the module's path).
--
-- @testLines@: `[test.lines]`, for `format = "lines"`. @testConvert@: a
-- command turning the output into olifant-json, as in 'CheckDef'.
-- @testUnitPatterns@ (`unit`): expressions whose line starts a test or a
-- group, its last group the name. @testFileGroup@ (`file_group`): an
-- expression over a test file's text, its last group the file's outermost
-- group (hspec's module). @testSelect@: how an id becomes an argument.
-- @testExclusive@: the runner keeps a build cache, so a run holds a slot.
-- @testTimeout@: seconds, over the caller's. @testFiles@ (`files`): globs of
-- files a run's scratch copy holds that are not part of its context key
-- (fixtures the tests read); the context's files are copied too.
data TestDef = TestDef
  { testCommand :: [Text], testContext :: [Text], testFormat :: Text, testRoot :: [Text], testsFor :: Text
  , testLines :: Maybe LinesFormat, testConvert :: [Text], testUnitPatterns :: [Text], testFileGroup :: Maybe Text
  , testSelect :: Maybe Select, testExclusive :: Bool, testTimeout :: Maybe Int
  , testFiles :: [Text]
  , testInstall :: Maybe Text
  -- ^ `install`: how to install the runner (doctor's fix when it is missing)
  }
  deriving stock (Eq, Show)

-- | `[test.select]`: argument templates over @{= group =}@ (the file's
-- `file_group`) and @{= name =}@ (the test's segments below its file,
-- joined by @/@): 'selTest' for a test id, 'selFile' for a file's.
data Select = Select {selTest :: Text, selFile :: Text}
  deriving stock (Eq, Show)

-- | One layer of a language: where it comes from (`shipped`, `pack NAME`,
-- `project`), the folder holding its `languages/` (a pack's root, or the
-- project's `.olifant`; Nothing when shipped), and its table.
data Layer = Layer {layerOrigin :: Text, layerDir :: Maybe FilePath, layerTable :: Table}
  deriving stock (Show)

-- | The shipped language of that name, as a layer (Nothing: none ships).
shippedLayer :: Text -> Maybe Layer
shippedLayer name = (\src -> Layer "shipped" Nothing (either (\e -> error (T.unpack ("library/languages/" <> name <> ".toml: " <> e))) id (parseToml src))) <$> lookup name shippedLanguages

-- | One pack's language file, over nothing, with the shipped presets.
parseLanguage :: Text -> FilePath -> Text -> Either Text Language
parseLanguage name packRoot src = do
  top <- parseToml src
  languageFromLayers shippedPresets name [Layer ("pack " <> T.pack (takeFileName packRoot)) (Just packRoot) top]

-- | One pack's language file over the shipped language of its name: what a
-- project with only that pack has.
parseOverShipped :: Text -> FilePath -> Text -> Either Text Language
parseOverShipped name packRoot src = do
  top <- parseToml src
  languageFromLayers shippedPresets name (maybe [] pure (shippedLayer name) ++ [Layer ("pack " <> T.pack (takeFileName packRoot)) (Just packRoot) top])

-- | A language from its layers, merged by key, later winning
-- ('mergeTables'); then the one preset the merged [check] or [test] names
-- fills what is left. A grammar source is found beside the layer that set
-- `grammar`.
languageFromLayers :: Presets -> Text -> [Layer] -> Either Text Language
languageFromLayers presets name layers = do
  let top = foldl mergeTables M.empty (map layerTable layers)
      grammarDir = case [layerDir l | l <- layers, M.member "grammar" (layerTable l)] of
        [] -> Nothing
        ds -> last ds
      origins = foldl (\m l -> M.union (keysOf (layerOrigin l) (layerTable l)) m) M.empty layers
      filled = M.fromList
        [ (section <> "." <> k, "toolchain " <> p)
        | section <- ["check", "test"]
        , Just (TTable t) <- [M.lookup section top]
        , Just (TText p) <- [M.lookup "preset" t]
        , let Presets ps = presets
        , Just pt <- [M.lookup (section, p) ps]
        , k <- M.keys pt
        , M.notMember k t
        ]
  lang <- languageFrom presets name grammarDir top
  pure lang {langLayers = map layerOrigin layers, langOrigins = M.union origins filled}
  where
    keysOf o t = M.fromList (concat [(k, o) : [(k <> "." <> k2, o) | TTable sub <- [v], k2 <- M.keys sub] | (k, v) <- M.toList t])

-- | Where a key (`check.format`) was set: its own origin, else its parent's.
originOf :: Language -> Text -> Maybe Text
originOf lang key = case M.lookup key (langOrigins lang) of
  Just o -> Just o
  Nothing
    | (parent, rest) <- T.breakOn "." key, not (T.null rest) -> M.lookup parent (langOrigins lang)
    | otherwise -> Nothing

languageFrom :: Presets -> Text -> Maybe FilePath -> Table -> Either Text Language
languageFrom presets name grammarDir top = do
  onlyKeysWith [("qualify", "`qualify` is retired: the [check] format decides how patterns are written")] ["files", "comment", "grammar", "targets", "placeholder", "branch", "check", "indent", "holes", "imports", "import_merge", "separator", "line_length", "keywords", "test", "unit", "mutate", "arm_separator", "lone_arm_spaced", "unit_words"] top
  files <- required "files" (list text) top
  comment <- required "comment" text top
  grammar <- required "grammar" grammarFrom top
  targets <- maybe [] M.elems <$> optional "targets" (table >=> traverse (table >=> targetFrom)) top
  placeholder <- optional "placeholder" (text >=> tmpl) top
  branch <- optional "branch" (text >=> tmpl) top
  check <- optional "check" (table >=> checkFrom) top
  indent <- fromMaybe "    " <$> optional "indent" text top
  holes <- maybe [] M.toList <$> optional "holes" (table >=> traverse (\v -> table v >>= holeKindFrom)) top
  imports <- optional "imports" (text >=> \re -> maybe (Left ("`imports` is not a valid expression: " <> re)) (\(_ :: Regex) -> Right re) (makeRegexM (T.unpack re))) top
  importMerge <- optional "import_merge" (table >=> importMergeFrom) top
  separator <- optional "separator" text top
  lineLength <- maybe 88 fromIntegral <$> optional "line_length" int top
  keywords <- fromMaybe [] <$> optional "keywords" (list text) top
  test <- optional "test" (table >=> testFrom) top
  unit <- fromMaybe M.empty <$> optional "unit" table top
  onlyKeys ["kinds", "name", "join"] unit
  unitKinds <- fromMaybe [] <$> optional "kinds" (list text) unit
  unitNames <- fromMaybe defaultUnitNames <$> optional "name" (list (text >=> validRegex "[unit] name")) unit
  unitJoin <- fromMaybe False <$> optional "join" bool unit
  mutate <- fromMaybe [] <$> optional "mutate" (list (table >=> mutateFrom)) top
  unitWords <- fromMaybe defaultUnitWords <$> optional "unit_words" text top
  armSeparator <- fromMaybe "" <$> optional "arm_separator" text top
  loneArmSpaced <- fromMaybe False <$> optional "lone_arm_spaced" bool top
  pure (Language name files comment grammar targets placeholder branch check indent holes imports separator lineLength importMerge keywords test unitKinds unitNames unitJoin unitWords armSeparator loneArmSpaced mutate [] M.empty)
  where
    tmpl = first describeTemplateError . parseTemplate
    validRegex key re = maybe (Left ("`" <> key <> "` is not a valid expression: " <> re)) (\(_ :: Regex) -> Right re) (makeRegexM (T.unpack re))
    grammarFrom = \case
      TText builtIn -> Right (BuiltInGrammar builtIn)
      TTable t -> do
        onlyKeys ["source"] t
        src <- required "source" text t
        case grammarDir of
          Just dir -> Right (GrammarSource (dir </> "languages" </> T.unpack src))
          Nothing -> Left "`grammar = { source }` cannot be shipped: a pack or the project carries the grammar"
      other -> Left ("expected a built-in grammar name or { source = \"dir\" }, got " <> describeTree other)
    mutateFrom t = do
      onlyKeys ["name", "pattern", "rule", "fix", "transform"] t
      n <- required "name" text t
      rule <- inKey n $ case (M.lookup "pattern" t, M.lookup "rule" t) of
        (Just p, Nothing) -> (\pat -> object ["pattern" .= pat]) <$> text p
        (Nothing, Just r) -> ruleFromTree r >>= first describeTemplateError . renderRule M.empty
        _ -> Left ("[[mutate]] `" <> n <> "`: one of `pattern` or `rule`")
      fix <- case (M.lookup "fix" t, M.lookup "transform" t) of
        (Just f, Nothing) -> Right <$> text f
        (Nothing, Just (TText "increment")) -> Right (Left Increment)
        (Nothing, Just other) -> Left ("[[mutate]] `" <> n <> "`: `transform`: expected \"increment\", got " <> describeTree other)
        _ -> Left ("[[mutate]] `" <> n <> "`: one of `fix` or `transform`")
      pure (MutateDef n rule fix)
    importMergeFrom t = do
      onlyKeys ["pattern", "write"] t
      re <- required "pattern" text t
      _ <- maybe (Left ("`import_merge.pattern` is not a valid expression: " <> re)) (\(r :: Regex) -> Right r) (makeRegexM (T.unpack re))
      write <- required "write" (text >=> tmpl) t
      if freeVariables write `S.isSubsetOf` S.fromList ["module", "names"] then Right () else Left "`import_merge.write` may use only {= module =} and {= names =}"
      pure (ImportMerge re write)
    holeKindFrom t = do
      onlyKeys ["placeholder", "wrap"] t
      ph <- required "placeholder" (text >=> tmpl) t
      if "hole" `S.member` freeVariables ph then Right () else Left "`placeholder` must use {= hole =} (the hole's name), so every placeholder is unique"
      wrap <- required "wrap" text t
      if any (`T.isInfixOf` wrap) ["{= fill =}", "{= fill|indent =}"]
        then Right (HoleKind ph wrap)
        else Left "`wrap` must contain {= fill =} (or {= fill|indent =}, one level in), where the fill goes"
    targetFrom t = do
      onlyKeys ["phrase", "container", "items", "example", "spaced", "when_empty", "separator"] t
      TargetDef
        <$> (T.words <$> required "phrase" text t)
        <*> required "container" ruleFromTree t
        <*> required "items" ruleFromTree t
        <*> required "example" text t
        <*> (fromMaybe True <$> optional "spaced" bool t)
        <*> optional "when_empty" (text >=> \w -> if w == "after header" then Right w else Left ("`when_empty`: expected \"after header\", got `" <> w <> "`")) t
        <*> optional "separator" text t
    testFrom t0 = do
      onlyKeys ["preset", "command", "context", "format", "root", "tests_for", "lines", "convert", "unit", "file_group", "select", "exclusive", "timeout", "files", "install"] t0
      t <- applyPreset presets "test" t0
      convert <- fromMaybe [] <$> optional "convert" (list text) t
      format <- (if null convert then required "format" text t else fromMaybe "olifant-json" <$> optional "format" text t)
      if format `elem` knownTestFormats then Right () else Left ("`format`: unknown test format `" <> format <> "`; expected one of " <> T.intercalate ", " knownTestFormats)
      if null convert || format == "olifant-json" then Right () else Left ("`convert` writes olifant-json: with `convert`, `format` must be absent or \"olifant-json\", not `" <> format <> "`")
      linesFormat <- optional "lines" (table >=> linesFrom) t
      case (format, linesFormat) of
        ("lines", Nothing) -> Left "`format = \"lines\"` needs a [test.lines] table (pass, fail)"
        ("lines", _) -> Right ()
        (_, Just _) -> Left "[test.lines] is read only with `format = \"lines\"`"
        _ -> Right ()
      fileGroup <- optional "file_group" (text >=> validRegex "[test] file_group") t
      select <- optional "select" (table >=> selectFrom) t
      case select of
        Just (Select a b) | any ("{= group =}" `T.isInfixOf`) [a, b], Nothing <- fileGroup -> Left "[test.select] uses {= group =}, which needs `file_group`"
        _ -> Right ()
      TestDef <$> required "command" (list text) t <*> (fromMaybe [] <$> optional "context" (list text) t) <*> pure format
        <*> (fromMaybe [] <$> optional "root" (list text) t) <*> required "tests_for" text t
        <*> pure linesFormat <*> pure convert
        <*> (fromMaybe [] <$> optional "unit" (list (text >=> validRegex "[test] unit")) t)
        <*> pure fileGroup <*> pure select
        <*> (fromMaybe False <$> optional "exclusive" bool t)
        <*> optional "timeout" (int >=> \n -> if n > 0 then Right (fromIntegral n) else Left "`timeout`: expected a number of seconds above 0") t
        <*> (fromMaybe [] <$> optional "files" (list text) t)
        <*> optional "install" text t
    linesFrom t = do
      onlyKeys ["pass", "fail", "nest", "failure", "end", "drop"] t
      let re k = text >=> validRegex ("[test.lines] " <> k)
      LinesFormat <$> required "pass" (re "pass") t <*> required "fail" (re "fail") t <*> (fromMaybe False <$> optional "nest" bool t)
        <*> optional "failure" (re "failure") t <*> optional "end" (re "end") t <*> optional "drop" (re "drop") t
    selectFrom t = do
      onlyKeys ["test", "file"] t
      Select <$> required "test" text t <*> required "file" text t
    checkFrom t0 = do
      onlyKeys ["preset", "command", "context", "format", "root", "root_pick", "exclusive", "concurrent", "errorformat", "convert", "install"] t0
      t <- applyPreset presets "check" t0
      convert <- fromMaybe [] <$> optional "convert" (list text) t
      format <- (if null convert then required "format" text t else fromMaybe "olifant-json" <$> optional "format" text t)
      if format `elem` knownFormats
        then Right ()
        else Left ("`format`: unknown format `" <> format <> "`; expected one of " <> T.intercalate ", " knownFormats)
      if null convert || format == "olifant-json" then Right () else Left ("`convert` writes olifant-json: with `convert`, `format` must be absent or \"olifant-json\", not `" <> format <> "`")
      errorFormat <- optional "errorformat" (table >=> errorFormatFrom) t
      case (format, errorFormat) of
        ("errorformat", Nothing) -> Left "`format = \"errorformat\"` needs a [check.errorformat] table (pattern, file, line, message)"
        ("errorformat", _) -> Right ()
        (_, Just _) -> Left "[check.errorformat] is read only with `format = \"errorformat\"`"
        _ -> Right ()
      CheckDef <$> required "command" (list text) t <*> (fromMaybe [] <$> optional "context" (list text) t) <*> pure format
        <*> (fromMaybe [] <$> optional "root" (list text) t)
        <*> (optional "root_pick" text t >>= \case
              Nothing -> Right False
              Just "nearest" -> Right False
              Just "outermost" -> Right True
              Just other -> Left ("`root_pick`: expected \"nearest\" or \"outermost\", got `" <> other <> "`"))
        <*> (fromMaybe False <$> optional "exclusive" bool t)
        <*> (fromMaybe True <$> optional "concurrent" bool t)
        <*> pure errorFormat
        <*> pure convert
        <*> optional "install" text t
    errorFormatFrom t = do
      onlyKeys ["pattern", "file", "line", "message", "severity", "error"] t
      ErrorFormat
        <$> required "pattern" (text >=> validRegex "[check.errorformat] pattern") t
        <*> (fromIntegral <$> required "file" int t)
        <*> (fromIntegral <$> required "line" int t)
        <*> (fromIntegral <$> required "message" int t)
        <*> (fmap fromIntegral <$> optional "severity" int t)
        <*> optional "error" (text >=> validRegex "[check.errorformat] error") t

-- | What a key's expression names in TEXT: its last group (the whole match
-- when it has none), stripped; Nothing when it does not match, is not a
-- valid expression, or names nothing. Partly applied, it compiles once.
regexName :: Text -> Text -> Maybe Text
regexName re = case makeRegexM (T.unpack re) :: Maybe Regex of
  Nothing -> const Nothing
  Just rx -> \txt -> do
    groups <- getAllTextSubmatches <$> (matchM rx (T.unpack txt) :: Maybe (AllTextSubmatches [] String))
    let n = T.strip (T.pack (last groups))
    if T.null n then Nothing else Just n

-- | What a unit is, for a language without `unit_words` (or a project of
-- several that differ).
defaultUnitWords :: Text
defaultUnitWords = "a top-level function or type name (Name), or a method (Type.method)"

-- | A unit's name when the language has no `[unit] name`: its line's first identifier.
defaultUnitNames :: [Text]
defaultUnitNames = ["^[[:space:]]*([[:alpha:]_][[:alnum:]_']*)"]

targetPhrase :: TargetDef -> Text
targetPhrase = T.unwords . targetShape

-- | Bind a target's parameters (words in capitals) to the phrase's words.
matchShape :: TargetDef -> [Text] -> Maybe [(Text, Text)]
matchShape t ws
  | length ws /= length (targetShape t) = Nothing
  | otherwise = concat <$> traverse bind (zip (targetShape t) ws)
  where
    bind (s, w)
      | isParam s = Just [(s, w)]
      | s == w = Just []
      | otherwise = Nothing
    isParam s = T.all (\c -> isUpper c || c == '_') s && T.any isUpper s

languageFor :: [Language] -> FilePath -> Maybe Language
languageFor langs p = find (\l -> any (`matchesGlob` p) (langFiles l)) langs
