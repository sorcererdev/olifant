-- | Language adapters (spec §12): pack data in `languages/<name>.toml`.
module Olifant.Language.Adapter
  ( Language (..)
  , Grammar (..)
  , TargetDef (..)
  , CheckDef (..)
  , ErrorFormat (..)
  , supportOf
  , TestDef (..)
  , UnitRule (..)
  , UndefinedRule (..)
  , MissingMember (..)
  , FailureDef (..)
  , GotExpected (..)
  , StubDef (..)
  , Select (..)
  , LinesFormat (..)
  , HoleKind (..)
  , ImportMerge (..)
  , ImportsDef (..)
  , ConjureDef (..)
  , TypesDef (..)
  , PortShape (..)
  , AddMethod (..)
  , Advice (..)
  , FakeShape (..)
  , CtorShape (..)
  , ModuleOf (..)
  , Resolve (..)
  , moduleOf
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
  , Provider (..)
  , withProviderRoot
  , hasUnitRule
  , hasTestUnits
  , mergeLayerTables
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
import Olifant.Provider (Provider (..))
import Olifant.Anchor.Phrase (parsePhrase)
import Olifant.Language.Presets (Presets (..), applyPreset, shippedPresets)
import Olifant.Library (shippedLanguages)
import Olifant.Language.Rule
import Olifant.Language.Diagnostics (CompilerSupport)
import Olifant.Language.Generic (ErrorFormat (..), LinesFormat (..))
import Olifant.Language.Support (formatSupport, knownFormats, knownTestFormats)
import Olifant.Template (Template, describeTemplateError, freeVariables, parseTemplate)
import Data.Set qualified as S
import Olifant.Toml
import Text.Regex.TDFA (AllTextSubmatches (..), Regex, makeRegexM, matchM, matchOnceText)
import System.FilePath (dropExtension, takeFileName, (</>))

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
  -- ^ `[imports] line` (or `imports`, a string): a POSIX expression matching
  -- one import line. A fill may begin with such lines; they join the file's
  -- imports
  , langImportsDef :: Maybe ImportsDef
  -- ^ `[imports]`: how a module imports a unit, a path's module, and which
  -- project files an import names
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
  , langOpensBlock :: Maybe Text
  -- ^ `opens_block`: an expression over a line (its comment cut) that opens
  -- a block (Python's `:`, Rust's `{`, Haskell's `do` or `=`): what a fill
  -- that repeats the file is cut back to
  , langEmptyStatement :: Maybe Text
  -- ^ `empty_statement`: a statement that does nothing (Python's `pass`),
  -- for an arrangement left with no lines; without it, the side is empty
  , langAssertion :: [Text]
  -- ^ `[test] assertion`: expressions over a line where a test's assertion
  -- starts (Python's `assert`, Rust's `assert_eq!`, hspec's `shouldBe`)
  , langStubLines :: [Text]
  -- ^ `stub_lines`: expressions over a stripped line of a body that is still
  -- a stub (Python's `...`, Rust's `todo!(`); a comment and a hole's line
  -- count in every language
  , langConjure :: Maybe ConjureDef
  -- ^ `[conjure]`: a pack's architecture as conjure reads it (ports, fakes,
  -- constructors); absent, conjure works at the floor (the test and its fill)
  , langTypes :: Maybe TypesDef
  -- ^ `[types]`: what a new method's signature is made of
  , langUnitProvider :: Maybe Provider
  -- ^ `[unit] kinds = { provider = [...] }`: a program answers the units slot
  , langAnchorProvider :: Maybe Provider
  -- ^ `anchors = { provider = [...] }`: a program places a phrase no target knows
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

-- | `[imports]`: 'imWrite' (`write`), a template over @{= module =}@ and
-- @{= owner =}@ (a unit's first dotted segment: its class for a method)
-- writing the line that imports a unit; 'imModuleOf' (`module_of`), how a
-- path becomes a module; 'imResolve' (`[[imports.resolve]]`), how an import
-- line names modules; 'imPaths' (`paths`), templates over @{= path =}@ (a
-- module, its separator written @/@) for the files a module may be;
-- 'imExample' (`example`), a fill that starts with an import line, shown to
-- the model when its fill may begin with imports: @needs@, the name the
-- import binds, and @fill@, the whole fill; 'imSpaced' (`spaced`, default
-- true): false when a blank line between imports groups them, so `end of
-- imports` joins the last import with no blank line; 'imWhenEmpty'
-- (`when_empty`, as a target's): "after header" puts a file's first import
-- after its leading comments and docstring (else a file with none is "no
-- `imports` in this file").
data ImportsDef = ImportsDef {imWrite :: Maybe Text, imModuleOf :: Maybe ModuleOf, imResolve :: [Resolve], imPaths :: [Text], imExample :: Maybe (Text, Text), imSpaced :: Bool, imWhenEmpty :: Maybe Text}
  deriving stock (Eq, Show)

-- | A pack's `[conjure]`: how conjure reads its architecture. 'cjService'
-- (`service`), a path template over @{= app =}@ and @{= service =}@ a
-- module's path starts with (binding the wire's sets); the port, fake and
-- constructor shapes, each absent when the pack declares none; 'cjAddMethod'
-- (`[conjure.port] add`, `adapters`), the generators that add a method a port
-- lacks (none: the person adds it); 'cjAdvice' (`[[conjure.advice]]`), what a
-- test file's lines add to the instruction that writes a test's body.
data ConjureDef = ConjureDef {cjService :: Maybe Text, cjPort :: Maybe PortShape, cjFake :: Maybe FakeShape, cjConstructor :: Maybe CtorShape, cjAddMethod :: Maybe AddMethod, cjAdvice :: [Advice]}
  deriving stock (Eq, Show)

-- | `[conjure.port] add` and `adapters`: 'amGenerator' (`add`), the id of a
-- snippet generator placed on a blank line after a port's last line, its holes
-- named as `[types] method` names a signature's parts (@name@, @params@,
-- @returns@); 'amAdapters' (`adapters`), the id of a generator run after it,
-- given the slots of its `[applies_at] path` over the port's file and @name@,
-- @params@, @returns@ and @signature@ (each only when it declares the input).
data AddMethod = AddMethod {amGenerator :: Text, amAdapters :: Maybe Text}
  deriving stock (Eq, Show)

-- | `[[conjure.advice]]`: 'avWhen' (`when`), an expression over a test
-- file's lines; 'avSay' (`say`), the sentence the instruction that writes a
-- test's body gains when one of them matches (the first advice to match, by
-- line, then by order).
data Advice = Advice {avWhen :: Text, avSay :: Text}
  deriving stock (Eq, Show)

-- | `[types]`: the facts a new method's signature is made of, in the language's own words.
-- 'tyString' (`string`) and 'tyInteger' (`integer`), the types of a string and a number
-- literal; 'tyNothing' (`nothing`), what a call whose result is unused returns;
-- 'tyOptional' (`optional`), a template over @{= type =}@ for a result that may be absent;
-- 'tyParam' (`param`), a template over @{= name =}@ and @{= type =}@ writing one parameter;
-- 'tyAnnotation' (`annotation`), an expression over @{= name =}@ whose last group is that
-- name's declared type; 'tyReturns' (`returns`), an expression over a method's line, group 1
-- what it returns; 'tyAssigned' (`assigned`), an expression over a line assigning a call's
-- result, its last group the variable; 'tyChecksNone' (`checks_none`), expressions over
-- @{= name =}@ testing a result for absence; 'tyRecord' (`record`), an expression over
-- @{= name =}@ starting a type with fields; 'tyMethod' (`method`), a template over @{= name =}@,
-- @{= params =}@ (each @, @ and `param`) and @{= returns =}@ writing a method's signature.
data TypesDef = TypesDef
  { tyString, tyInteger, tyNothing, tyOptional, tyParam, tyAnnotation, tyReturns, tyAssigned, tyRecord, tyMethod :: Maybe Text
  , tyChecksNone :: [Text]
  }
  deriving stock (Eq, Show)

-- | `[conjure.port]`: 'psFiles' (`files`), globs under the service where
-- ports live; 'psStart' (`start`), an expression over a line that starts a
-- port (group 1 its name), its block the lines after it that are blank or
-- deeper; 'psMethod' (`method`), an expression over a stripped block line
-- that is one of its methods.
data PortShape = PortShape {psFiles :: [Text], psStart :: Text, psMethod :: Text}
  deriving stock (Eq, Show)

-- | `[conjure.fake]`: 'fsBound' (`bound`), an expression over a test's line
-- binding a fake (group 1 the variable, group 2 the class called); a fake's
-- class is one of 'fsClasses' (`classes`) or starts with one of
-- 'fsPrefixes' (`prefixes`) and is longer (the class less it names a port);
-- 'fsKeyword' (`keyword`), a template over @{= param =}@ and @{= var =}@
-- for a fake handed to a call by name (none: positional only); 'fsStub'
-- (`stub`), an expression over @{= method =}@ for a fake's stand-in method
-- (group 1 its parameters, group 2 what it returns).
data FakeShape = FakeShape {fsBound :: Text, fsClasses :: [Text], fsPrefixes :: [Text], fsKeyword :: Maybe Text, fsStub :: Maybe Text}
  deriving stock (Eq, Show)

-- | `[conjure.constructor]`: 'ctFiles' (`files`), globs under the service
-- for the siblings whose constructors show what a port's parameter is
-- called; 'ctOwner' (`owner`), an expression over a line starting what holds
-- a constructor (group 1 its name: a class); 'ctStart' (`start`), an
-- expression its parameters follow, up to the bracket closing the one it
-- opens; 'ctParam' (`param`), an expression over one parameter (group 1 its
-- name, group 2 its type; one that does not match is left out).
data CtorShape = CtorShape {ctFiles :: [Text], ctOwner :: Text, ctStart :: Text, ctParam :: Text}
  deriving stock (Eq, Show)

-- | `module_of = { strip, drop, separator }`: strip the first of `strip`'s
-- expressions that matches the path, drop the extension, drop the trailing
-- segments `drop` lists, and join the rest with `separator`.
data ModuleOf = ModuleOf {moStrip :: [Text], moDrop :: [Text], moSeparator :: Text}
  deriving stock (Eq, Show)

-- | `[[imports.resolve]]`: an expression over an import line (stripped);
-- group `module` holds the modules it names (comma-separated, each less its
-- alias), group `names` (optional) the names imported from it, each of
-- which may be a module too.
data Resolve = Resolve {rsPattern :: Text, rsModule :: Int, rsNames :: Maybe Int}
  deriving stock (Eq, Show)

-- | A path's module by 'ModuleOf': @src/a/b.py@ is @a.b@ (Python's), and
-- @crates/m/src/lib.rs@ is the empty module (Rust's crate root).
moduleOf :: ModuleOf -> FilePath -> Text
moduleOf mo path = T.intercalate (moSeparator mo) (dropTrailing (filter (not . T.null) (T.splitOn "/" noExt)))
  where
    stripped = case [r | re <- moStrip mo, Just r <- [stripOnce re (T.pack path)]] of
      r : _ -> r
      [] -> T.pack path
    noExt = T.pack (dropExtension (T.unpack stripped))
    dropTrailing = reverse . dropWhile (`elem` moDrop mo) . reverse
    stripOnce re t = case makeRegexM (T.unpack re) :: Maybe Regex of
      Nothing -> Nothing
      Just rx -> case matchOnceText rx (T.unpack t) of
        Just (before, _, after) -> Just (T.pack before <> T.pack after)
        Nothing -> Nothing

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
-- @checkUndefined@ (`undefined`): expressions for a name the check says is
-- undefined ('UndefinedRule'). @checkMissingMember@ (`missing_member`): the
-- check's words for a member a type lacks.
data CheckDef = CheckDef {checkCommand :: [Text], checkContext :: [Text], checkFormat :: Text, checkRoot :: [Text], checkRootOutermost :: Bool, checkExclusive :: Bool, checkConcurrent :: Bool, checkErrorFormat :: Maybe ErrorFormat, checkConvert :: [Text], checkInstall :: Maybe Text, checkUndefined :: [UndefinedRule], checkMissingMember :: Maybe MissingMember}
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
-- @testUnitRules@ (`unit`): rules whose line starts a test or a group, its
-- last group the name ('UnitRule'). @testAbove@ (`above`): an expression
-- for the lines that belong to the unit below them (decorators,
-- attributes). @testFileGroup@ (`file_group`): an
-- expression over a test file's text, its last group the file's outermost
-- group (hspec's module). @testSelect@: how an id becomes an argument.
-- @testExclusive@: the runner keeps a build cache, so a run holds a slot.
-- @testTimeout@: seconds, over the caller's. @testFiles@ (`files`): globs of
-- files a run's scratch copy holds that are not part of its context key
-- (fixtures the tests read); the context's files are copied too.
data TestDef = TestDef
  { testCommand :: [Text], testContext :: [Text], testFormat :: Text, testRoot :: [Text], testsFor :: Text
  , testLines :: Maybe LinesFormat, testConvert :: [Text], testUnitRules :: [UnitRule], testFileGroup :: Maybe Text
  , testSelect :: Maybe Select, testExclusive :: Bool, testTimeout :: Maybe Int
  , testFiles :: [Text]
  , testInstall :: Maybe Text
  -- ^ `install`: how to install the runner (doctor's fix when it is missing)
  , testAbove :: Maybe Text
  , testStub :: Maybe StubDef
  -- ^ `[test.stub]`: what a new test looks like and where it goes
  , testNaming :: Maybe Text
  -- ^ `name`: a new test's name, a template over @{= words =}@
  , testUndefined :: [UndefinedRule]
  -- ^ `undefined`: expressions for a name the runner says is undefined
  , testFailure :: Maybe FailureDef
  -- ^ `[test.failure]`: how a failure's output names its assertion, what
  -- came back and what was expected
  , testUnitProvider :: Maybe Provider
  -- ^ `unit = { provider = [...] }`: a program answers the test-units slot
  , testSelectProvider :: Maybe Provider
  -- ^ `select = { provider = [...] }`: a program turns ids into arguments
  }
  deriving stock (Eq, Show)

-- | `[test.stub]`: a new test. 'sdTest' (`test`), its text, a template
-- over @{= name =}@, @{= unit =}@, @{= module =}@, @{= indent =}@ and
-- @{= note =}@ (a survivor's line, or empty); 'sdFile' (`file`), a new test
-- file's head, over the same and @{= import =}@; 'sdGap' (`gap`), the blank
-- lines before a new test in a file that has some; 'sdAt' (`at`), a target
-- phrase (`end of mod tests`) the test goes at instead of the file's end;
-- 'sdOpen' (`open`), the text appended at the file's end when `at` finds no
-- target, over the same and @{= test =}@ (the test, inside it);
-- 'sdElsewhere' (`elsewhere`), an expression for a line saying the tests
-- live in another file (Rust's `mod tests;`), its last group that file's
-- name: the stub then refuses, naming it.
data StubDef = StubDef {sdTest :: Text, sdFile :: Maybe Text, sdGap :: Int, sdAt :: Maybe Text, sdOpen :: Maybe Text, sdElsewhere :: Maybe Text}
  deriving stock (Eq, Show)

-- | One `[test] unit` entry: an expression whose line starts a test or a
-- group (its last group the name), and `after`, an expression a line above
-- it must match (looking past `above` lines): Rust's `#[test]`; `name`, the
-- group holding the name when it is not the last (a name inside a string
-- with escapes, whose own group is not the last). A string entry is a rule
-- with neither.
data UnitRule = UnitRule {urPattern :: Text, urAfter :: Maybe Text, urName :: Maybe Int}
  deriving stock (Eq, Show)

-- | One `undefined` entry of a `[check]` or `[test]`: an expression whose
-- match names an undefined name (its last group, or group `name`), and
-- `label`, how a summary calls such names (@NameError@; default
-- @undefined:@). A string entry has neither.
data UndefinedRule = UndefinedRule {udPattern :: Text, udName :: Maybe Int, udLabel :: Maybe Text}
  deriving stock (Eq, Show)

-- | `[check] missing_member`: an expression for a check's words that a type
-- lacks a member, the groups holding the member and the type.
data MissingMember = MissingMember {mmPattern :: Text, mmMember :: Int, mmType :: Int}
  deriving stock (Eq, Show)

-- | `[test.failure]`: 'fdLineAfter' (`line_after`), an expression over
-- @{= name =}@ (the test's short name) for the output line after which the
-- failing source line comes; 'fdHeadStrip' (`head_strip`), an expression
-- taken off the front of a message's first line; 'fdGotExpected'
-- (`got_expected`), expressions over a message's lines (the first less
-- `head_strip`) whose groups are what came back and what was expected;
-- 'fdBare' (`bare`), expressions for a first line that says nothing (an
-- empty one says nothing too); 'fdExplain' (`explain`), an expression for
-- a line explaining it, its last group the one-line form in its place.
data FailureDef = FailureDef {fdLineAfter :: Maybe Text, fdHeadStrip :: Maybe Text, fdGotExpected :: [GotExpected], fdBare :: [Text], fdExplain :: Maybe Text}
  deriving stock (Eq, Show)

-- | One `got_expected` entry: an expression and its two groups.
data GotExpected = GotExpected {gePattern :: Text, geGot :: Int, geExpected :: Int}
  deriving stock (Eq, Show)

-- | `[test.select]`: argument templates over @{= group =}@ (the file's
-- `file_group`, else its module by `[imports] module_of`), @{= name =}@
-- (the test's segments below its file, joined by 'selJoin', key `join`,
-- default @/@) and @{= id =}@ (the whole id, @file::name@, for a runner
-- that reads it as it is): 'selTest' for a test id, 'selFile' for a file's. Each is
-- one argument (a string) or several (a list: `["--exact", "…"]`).
data Select = Select {selTest :: [Text], selFile :: [Text], selJoin :: Text}
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
-- ('mergeLayerTables'); then the one preset the merged [check] or [test] names
-- fills what is left. A grammar source is found beside the layer that set
-- `grammar`.
languageFromLayers :: Presets -> Text -> [Layer] -> Either Text Language
languageFromLayers presets name layers = do
  let top = foldl (\acc l -> mergeLayerTables (dropPresets acc (layerTable l)) (layerTable l)) M.empty layers
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
    -- a layer that writes its own command or convert, and no preset, stops
    -- inheriting the preset of the layers before it
    dropPresets acc t = foldr (\sec -> M.adjust (dropIn sec) sec) acc ["check", "test"]
      where
        dropIn sec = \case
          TTable a | Just (TTable own) <- M.lookup sec t, M.notMember "preset" own, any (`M.member` own) ["command", "convert"] -> TTable (M.delete "preset" a)
          v -> v
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
  onlyKeysWith [("qualify", "`qualify` is retired: the [check] format decides how patterns are written")] ["files", "comment", "grammar", "targets", "placeholder", "branch", "check", "indent", "holes", "imports", "import_merge", "separator", "line_length", "keywords", "test", "unit", "mutate", "arm_separator", "lone_arm_spaced", "unit_words", "opens_block", "empty_statement", "stub_lines", "conjure", "types", "anchors"] top
  files <- required "files" (list text) top
  comment <- required "comment" text top
  grammar <- required "grammar" grammarFrom top
  targets <- maybe [] M.elems <$> optional "targets" (table >=> traverse (table >=> targetFrom)) top
  placeholder <- optional "placeholder" (text >=> tmpl) top
  branch <- optional "branch" (text >=> tmpl) top
  check <- optional "check" (table >=> checkFrom) top
  indent <- fromMaybe "    " <$> optional "indent" text top
  holes <- maybe [] M.toList <$> optional "holes" (table >=> traverse (\v -> table v >>= holeKindFrom)) top
  (imports, importsDef) <- maybe (Nothing, Nothing) id <$> optional "imports" importsFrom top
  importMerge <- optional "import_merge" (table >=> importMergeFrom) top
  separator <- optional "separator" text top
  lineLength <- maybe 88 fromIntegral <$> optional "line_length" int top
  keywords <- fromMaybe [] <$> optional "keywords" (list text) top
  -- [test]'s shape keys are the language's; the rest is a runner, absent
  -- when the table holds only shapes
  testTable <- optional "test" table top
  assertion <- maybe (Right []) (\t -> fromMaybe [] <$> optional "assertion" (list (text >=> validRegex "[test] assertion")) t) testTable
  let hasModuleOf = maybe False (\im -> imModuleOf im /= Nothing) importsDef
  test <- case testTable of
    Just t | not (M.null (foldr M.delete t testShapeKeys)) -> Just <$> testFrom hasModuleOf t
    _ -> Right Nothing
  opensBlock <- optional "opens_block" (text >=> validRegex "opens_block") top
  emptyStatement <- optional "empty_statement" text top
  stubLines <- fromMaybe [] <$> optional "stub_lines" (list (text >=> validRegex "stub_lines")) top
  conjure <- optional "conjure" (table >=> conjureFrom) top
  types <- optional "types" (table >=> typesFrom) top
  unit <- fromMaybe M.empty <$> optional "unit" table top
  onlyKeys ["kinds", "name", "join"] unit
  (unitKinds, unitProvider) <- maybe ([], Nothing) id <$> optional "kinds" (ruleOrProvider "unit.kinds" (list text) []) unit
  anchorProvider <- optional "anchors" (providerFrom "anchors") top
  unitNames <- fromMaybe defaultUnitNames <$> optional "name" (list (text >=> validRegex "[unit] name")) unit
  unitJoin <- fromMaybe False <$> optional "join" bool unit
  mutate <- fromMaybe [] <$> optional "mutate" (list (table >=> mutateFrom)) top
  unitWords <- fromMaybe defaultUnitWords <$> optional "unit_words" text top
  armSeparator <- fromMaybe "" <$> optional "arm_separator" text top
  loneArmSpaced <- fromMaybe False <$> optional "lone_arm_spaced" bool top
  pure (Language name files comment grammar targets placeholder branch check indent holes imports importsDef separator lineLength importMerge keywords test unitKinds unitNames unitJoin unitWords armSeparator loneArmSpaced opensBlock emptyStatement assertion stubLines conjure types unitProvider anchorProvider mutate [] M.empty)
  where
    tmpl = first describeTemplateError . parseTemplate
    -- a slot's value: the rule, or `{ provider = [...] }` alone in its place
    ruleOrProvider key rule none = \case
      TTable t | M.member "provider" t -> (\p -> (none, Just p)) <$> providerFrom key (TTable t)
      v -> (\r -> (r, Nothing)) <$> rule v
    providerFrom key = \case
      TTable t -> do
        case filter (/= "provider") (M.keys t) of
          [] -> Right ()
          other -> Left ("`" <> key <> "`: `{ provider = [...] }` stands alone in the rule's place; it also has " <> T.intercalate ", " ["`" <> k <> "`" | k <- other])
        argv <- inKey key (required "provider" (list text) t)
        if null argv then Left ("`" <> key <> "`: `provider` names no program") else Right (Provider argv Nothing)
      other -> Left ("`" <> key <> "`: expected { provider = [...] }, got " <> describeTree other)
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
    importsFrom = \case
      TText re -> (\r -> (Just r, Nothing)) <$> validRegex "imports" re
      TTable t -> do
        onlyKeys ["line", "write", "module_of", "resolve", "paths", "example", "spaced", "when_empty"] t
        line <- optional "line" (text >=> validRegex "[imports] line") t
        write <- optional "write" (text >=> \w -> tmpl w >>= \tp -> if freeVariables tp `S.isSubsetOf` S.fromList ["module", "owner"] then Right w else Left "`[imports] write` may use only {= module =} and {= owner =}") t
        mo <- optional "module_of" (table >=> \m -> do
          onlyKeys ["strip", "drop", "separator"] m
          ModuleOf <$> (fromMaybe [] <$> optional "strip" (list (text >=> validRegex "[imports] module_of strip")) m)
            <*> (fromMaybe [] <$> optional "drop" (list text) m)
            <*> (fromMaybe "." <$> optional "separator" text m)) t
        resolve <- fromMaybe [] <$> optional "resolve" (list (table >=> \r -> do
          onlyKeys ["pattern", "module", "names"] r
          let group k = int >=> \n -> if n > 0 then Right (fromIntegral n) else Left ("`[[imports.resolve]] " <> k <> "`: expected a group number above 0")
          Resolve <$> required "pattern" (text >=> validRegex "[[imports.resolve]] pattern") r <*> required "module" (group "module") r <*> optional "names" (group "names") r)) t
        paths <- fromMaybe [] <$> optional "paths" (list (text >=> \p -> tmpl p >>= \tp -> if freeVariables tp `S.isSubsetOf` S.fromList ["path"] then Right p else Left "`[imports] paths` may use only {= path =}")) t
        example <- optional "example" (table >=> \e -> do
          onlyKeys ["needs", "fill"] e
          (,) <$> required "needs" text e <*> required "fill" text e) t
        spaced <- fromMaybe True <$> optional "spaced" bool t
        whenEmpty <- optional "when_empty" (text >=> \w -> if w == "after header" then Right w else Left "`[imports] when_empty`: expected \"after header\"") t
        pure (line, Just (ImportsDef write mo resolve paths example spaced whenEmpty))
      other -> Left ("`imports`: expected an expression or an [imports] table, got " <> describeTree other)
    typesFrom t = do
      onlyKeys ["string", "integer", "nothing", "optional", "param", "annotation", "returns", "assigned", "checks_none", "record", "method"] t
      let o k = optional k text t
          -- a template over VARS; an expression over @{= name =}@, valid once it is filled
          over k vars = optional k (text >=> templateOver ("[types] " <> k) vars) t
          named k = text >=> templateOver ("[types] " <> k) ["name"] >=> \x -> validRegex ("[types] " <> k) (T.replace "{= name =}" "x" x) >> Right x
      TypesDef <$> o "string" <*> o "integer" <*> o "nothing" <*> over "optional" ["type"] <*> over "param" ["name", "type"] <*> optional "annotation" (named "annotation") t <*> optional "returns" (text >=> validRegex "[types] returns") t <*> optional "assigned" (text >=> validRegex "[types] assigned") t <*> optional "record" (named "record") t <*> over "method" ["name", "params", "returns"]
        <*> (fromMaybe [] <$> optional "checks_none" (list (named "checks_none")) t)
    conjureFrom t = do
      onlyKeys ["service", "port", "fake", "constructor", "advice"] t
      let re k = text >=> validRegex ("[conjure] " <> k)
      port <- optional "port" (table >=> \p -> do
        onlyKeys ["files", "start", "method", "add", "adapters"] p
        shape <- PortShape <$> (fromMaybe [] <$> optional "files" (list text) p) <*> required "start" (re "port start") p <*> required "method" (re "port method") p
        add <- optional "add" text p
        adapters <- optional "adapters" text p
        case (add, adapters) of
          (Nothing, Just _) -> Left "`[conjure.port] adapters` needs `add` (the generator that adds the method)"
          _ -> Right (shape, (`AddMethod` adapters) <$> add)) t
      advice <- fromMaybe [] <$> optional "advice" (list (table >=> \a -> do
        onlyKeys ["when", "say"] a
        Advice <$> required "when" (re "advice when") a <*> required "say" text a)) t
      ConjureDef
        <$> optional "service" text t
        <*> pure (fst <$> port)
        <*> optional "fake" (table >=> \f -> do
          onlyKeys ["bound", "classes", "prefixes", "keyword", "stub"] f
          FakeShape <$> required "bound" (re "fake bound") f
            <*> (fromMaybe [] <$> optional "classes" (list text) f)
            <*> (fromMaybe [] <$> optional "prefixes" (list text) f)
            <*> optional "keyword" text f
            <*> optional "stub" text f) t
        <*> optional "constructor" (table >=> \c -> do
          onlyKeys ["files", "owner", "start", "param"] c
          CtorShape <$> (fromMaybe [] <$> optional "files" (list text) c) <*> required "owner" (re "constructor owner") c <*> required "start" (re "constructor start") c <*> required "param" (re "constructor param") c) t
        <*> pure (snd =<< port)
        <*> pure advice
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
    testFrom hasModuleOf t0 = do
      onlyKeys ["preset", "command", "context", "format", "root", "tests_for", "lines", "convert", "unit", "file_group", "select", "exclusive", "timeout", "files", "install", "above", "stub", "name", "undefined", "failure"] (foldr M.delete t0 testShapeKeys)
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
      (select, selectProvider) <- maybe (Nothing, Nothing) id <$> optional "select" (ruleOrProvider "test.select" (fmap Just . (table >=> selectFrom)) Nothing) t
      (unitRules, unitProvider) <- maybe ([], Nothing) id <$> optional "unit" (ruleOrProvider "test.unit" (list unitRuleFrom) []) t
      case select of
        Just (Select a b _) | any ("{= group =}" `T.isInfixOf`) (a ++ b), Nothing <- fileGroup, not hasModuleOf -> Left "[test.select] uses {= group =}, which needs `file_group` (or [imports] module_of)"
        _ -> Right ()
      TestDef <$> required "command" (list text) t <*> (fromMaybe [] <$> optional "context" (list text) t) <*> pure format
        <*> (fromMaybe [] <$> optional "root" (list text) t) <*> required "tests_for" text t
        <*> pure linesFormat <*> pure convert
        <*> pure unitRules
        <*> pure fileGroup <*> pure select
        <*> (fromMaybe False <$> optional "exclusive" bool t)
        <*> optional "timeout" (int >=> \n -> if n > 0 then Right (fromIntegral n) else Left "`timeout`: expected a number of seconds above 0") t
        <*> (fromMaybe [] <$> optional "files" (list text) t)
        <*> optional "install" text t
        <*> optional "above" (text >=> validRegex "[test] above") t
        <*> optional "stub" (table >=> stubFrom) t
        <*> optional "name" (text >=> templateOver "[test] name" ["words"]) t
        <*> (fromMaybe [] <$> optional "undefined" (list (undefinedFrom "[test] undefined")) t)
        <*> optional "failure" (table >=> failureFrom) t
        <*> pure unitProvider
        <*> pure selectProvider
    templateOver key vars src = do
      tp <- first (\e -> "`" <> key <> "`: " <> describeTemplateError e) (parseTemplate src)
      if freeVariables tp `S.isSubsetOf` S.fromList vars then Right src else Left ("`" <> key <> "` may use only " <> T.intercalate ", " ["{= " <> v <> " =}" | v <- vars])
    stubFrom t = do
      onlyKeys ["test", "file", "gap", "at", "open", "elsewhere"] t
      let vars = ["name", "unit", "module", "indent", "note"]
      StubDef <$> required "test" (text >=> templateOver "[test.stub] test" vars) t
        <*> optional "file" (text >=> templateOver "[test.stub] file" ("import" : vars)) t
        <*> (maybe 1 fromIntegral <$> optional "gap" (int >=> \n -> if n >= 0 then Right n else Left "`[test.stub] gap`: expected a number of blank lines, 0 or more") t)
        <*> optional "at" (text >=> \a -> first (\e -> "`[test.stub] at`: " <> e) (parsePhrase a) >> Right a) t
        <*> optional "open" (text >=> templateOver "[test.stub] open" ("test" : vars)) t
        <*> optional "elsewhere" (text >=> validRegex "[test.stub] elsewhere") t
    unitRuleFrom = \case
      TText re -> (\r -> UnitRule r Nothing Nothing) <$> validRegex "[test] unit" re
      TTable u -> do
        onlyKeys ["pattern", "after", "name"] u
        UnitRule <$> required "pattern" (text >=> validRegex "[test] unit") u <*> optional "after" (text >=> validRegex "[test] unit after") u
          <*> optional "name" (int >=> \n -> if n > 0 then Right (fromIntegral n) else Left "`[test] unit` `name`: expected a group number above 0") u
      other -> Left ("`[test] unit`: expected an expression or { pattern, after, name }, got " <> describeTree other)
    undefinedFrom key = \case
      TText re -> (\r -> UndefinedRule r Nothing Nothing) <$> (validRegex key re >>= grouped)
      TTable u -> do
        onlyKeys ["pattern", "name", "label"] u
        re <- required "pattern" (text >=> validRegex key >=> grouped) u
        UndefinedRule re <$> optional "name" (int >=> groupOf key re) u <*> optional "label" text u
      other -> Left ("`" <> key <> "`: expected an expression or { pattern, name, label }, got " <> describeTree other)
      where
        -- the name is a group (the last, without `name`)
        grouped re = if groupCount re > 0 then Right re else Left ("`" <> key <> "`: " <> re <> " has no group: the undefined name is its last group")
    groupOf key re n
      | n > 0, fromIntegral n <= groupCount re = Right (fromIntegral n)
      | otherwise = Left ("`" <> key <> "`: group " <> T.pack (show n) <> " is not a group of " <> re)
    missingMemberFrom t = do
      let key = "[check] missing_member"
      onlyKeys ["pattern", "member", "type"] t
      re <- required "pattern" (text >=> validRegex key) t
      MissingMember re <$> required "member" (int >=> groupOf key re) t <*> required "type" (int >=> groupOf key re) t
    failureFrom t = do
      onlyKeys ["line_after", "head_strip", "got_expected", "bare", "explain"] t
      FailureDef
        <$> optional "line_after" (text >=> templateOver "[test.failure] line_after" ["name"] >=> \x -> validRegex "[test.failure] line_after" (T.replace "{= name =}" "x" x) >> Right x) t
        <*> optional "head_strip" (text >=> validRegex "[test.failure] head_strip") t
        <*> (fromMaybe [] <$> optional "got_expected" (list (table >=> gotExpectedFrom)) t)
        <*> (fromMaybe [] <$> optional "bare" (list (text >=> validRegex "[test.failure] bare")) t)
        <*> optional "explain" (text >=> validRegex "[test.failure] explain") t
    gotExpectedFrom t = do
      let key = "[test.failure] got_expected"
      onlyKeys ["pattern", "got", "expected"] t
      re <- required "pattern" (text >=> validRegex key) t
      GotExpected re <$> required "got" (int >=> groupOf key re) t <*> required "expected" (int >=> groupOf key re) t
    linesFrom t = do
      onlyKeys ["pass", "fail", "nest", "failure", "end", "drop", "split"] t
      let re k = text >=> validRegex ("[test.lines] " <> k)
      LinesFormat <$> required "pass" (re "pass") t <*> required "fail" (re "fail") t <*> (fromMaybe False <$> optional "nest" bool t)
        <*> optional "failure" (re "failure") t <*> optional "end" (re "end") t <*> optional "drop" (re "drop") t
        <*> optional "split" (text >=> \x -> if T.null x then Left "`[test.lines] split`: expected a separator, not empty" else Right x) t
    selectFrom t = do
      onlyKeys ["test", "file", "join"] t
      let args = \case
            TText a -> Right [a]
            v -> list text v
      Select <$> required "test" args t <*> required "file" args t <*> (fromMaybe "/" <$> optional "join" text t)
    checkFrom t0 = do
      onlyKeys ["preset", "command", "context", "format", "root", "root_pick", "exclusive", "concurrent", "errorformat", "convert", "install", "undefined", "missing_member"] t0
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
        <*> (fromMaybe [] <$> optional "undefined" (list (undefinedFrom "[check] undefined")) t)
        <*> optional "missing_member" (table >=> missingMemberFrom) t
    errorFormatFrom t = do
      onlyKeys ["pattern", "file", "line", "message", "severity", "error"] t
      ErrorFormat
        <$> required "pattern" (text >=> validRegex "[check.errorformat] pattern") t
        <*> (fromIntegral <$> required "file" int t)
        <*> (fromIntegral <$> required "line" int t)
        <*> (fromIntegral <$> required "message" int t)
        <*> (fmap fromIntegral <$> optional "severity" int t)
        <*> optional "error" (text >=> validRegex "[check.errorformat] error") t

-- | The groups of a POSIX expression: its @(@s that are neither escaped nor
-- inside a bracket expression.
groupCount :: Text -> Int
groupCount = go False . T.unpack
  where
    go _ [] = 0
    go inB ('\\' : _ : r) = go inB r
    go False ('[' : ']' : r) = go True r
    go False ('[' : '^' : ']' : r) = go True r
    go False ('[' : r) = go True r
    go True ('[' : ':' : r) = go True (dropClass r)
    go True (']' : r) = go False r
    go False ('(' : r) = 1 + go False r
    go inB (_ : r) = go inB r
    dropClass r = case break (== ':') r of
      (_, ':' : ']' : r') -> r'
      (_, r') -> r'

-- | Language layers merged by key, the later winning ('mergeTables'), except
-- that a slot's `{ provider = [...] }` replaces the value under it whole and
-- is replaced whole: it takes the rule's place, and never merges with it.
mergeLayerTables :: Table -> Table -> Table
mergeLayerTables = M.unionWith (flip merge)
  where
    merge (TTable new) (TTable old)
      | M.member "provider" new || M.member "provider" old = TTable new
      | otherwise = TTable (mergeLayerTables old new)
    merge new _ = new

-- | Every provider of the language runs from the project root.
withProviderRoot :: FilePath -> Language -> Language
withProviderRoot root lang =
  lang
    { langUnitProvider = rooted <$> langUnitProvider lang
    , langAnchorProvider = rooted <$> langAnchorProvider lang
    , langTest = (\td -> td {testUnitProvider = rooted <$> testUnitProvider td, testSelectProvider = rooted <$> testSelectProvider td}) <$> langTest lang
    }
  where
    rooted p = p {provRoot = Just root}

-- | The language can name its units: by `[unit] kinds` or a provider.
hasUnitRule :: Language -> Bool
hasUnitRule lang = not (null (langUnitKinds lang)) || langUnitProvider lang /= Nothing

-- | The language can find its tests: by `[test] unit` rules or a provider.
hasTestUnits :: Language -> Bool
hasTestUnits lang = case langTest lang of
  Just td -> not (null (testUnitRules td)) || testUnitProvider td /= Nothing
  Nothing -> False

-- | The keys of `[test]` that are the language's shapes, not its runner's.
testShapeKeys :: [Text]
testShapeKeys = ["assertion"]

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
