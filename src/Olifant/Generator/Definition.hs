module Olifant.Generator.Definition
  ( GeneratorId (..)
  , GeneratorWith (..)
  , GeneratorDef
  , generatorName
  , replaceGenerator
  , Generator
  , Requires (..)
  , EditWith (..)
  , OnExists (..)
  , Unless (..)
  , AppliesAt (..)
  , AnchorSpec (..)
  , SourceRef (..)
  , UseStep (..)
  , ResolveFailure (..)
  , InputSpec (..)
  , InputType (..)
  , Scalar (..)
  , ChoiceValue (..)
  , Choices (..)
  , ListItem (..)
  , FieldSpec (..)
  , HoleSource (..)
  , describeHoleSource
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Anchor (Position)
import Olifant.Language (Rule)
import Olifant.Query (ChoiceValue (..), Query)
import Olifant.Template (Binding, Template)
import Olifant.Template.Syntax (Segment (..), Template (..))

newtype GeneratorId = GeneratorId {generatorIdText :: Text}
  deriving stock (Eq, Ord, Show)

-- | A generator whose edit sources are @src@: file references when loaded
-- from TOML ('GeneratorDef'), parsed templates once resolved ('Generator').
data GeneratorWith src = Generator
  { genId :: GeneratorId
  , genDescription :: Text
  , genLanguage :: Text
  , genVersion :: Int
  , genInputs :: [InputSpec]
  , genDerived :: [(Text, Template)]
  , genRequires :: Maybe Requires
  , genEdits :: [EditWith src]
  , genUses :: [UseStep]
  , genExamples :: [Text] -- ^ example commands, shown by `olifant show`
  , -- | `[query.NAME]` (spec §4.5): lists bound after derived values
    genQueries :: [(Text, Query)]
  , -- | `[applies_at]` (spec §4.6): where in the code this generator applies
    genAppliesAt :: Maybe AppliesAt
  , -- | `name`: what editors show ("Add Command Dependency"); see 'generatorName'
    genName :: Maybe Text
  , -- | `[holes.NAME]` (spec §4.9): where a hole's value comes from (model by default)
    genHoles :: [(Text, HoleSource)]
  , -- | `starter = true`: the pack's project generator (`olifant init`)
    genStarter :: Bool
  , -- | `next`: what to run after `olifant init` (rendered with the inputs)
    genNext :: [Template]
  }
  deriving stock (Show, Functor, Foldable, Traversable)

type GeneratorDef = GeneratorWith SourceRef

type Generator = GeneratorWith Template

-- | One composition step in a generator: run another generator with exactly
-- the inputs in 'useWith' (rendered in this generator's scope), optionally
-- once per item of the list named by 'useForEach' (binding @item@, @loop@).
data UseStep = UseStep
  { useGenerator :: Text
  , useForEach :: Maybe Text
  , useWith :: [(Text, Template)]
  }
  deriving stock (Show)

data Requires = Requires {requiresExists :: [Template], requiresHint :: Maybe Template}
  deriving stock (Show)

data EditWith src
  = -- | 'editUnless': when the edit is skipped (spec §5).
    Create {editTarget :: Template, editSource :: src, editOnExists :: OnExists, editUnless :: Maybe (Unless Template)}
  | Insert {editTarget :: Template, editAt :: [AnchorSpec], editSource :: src, editUnless :: Maybe (Unless Template)}
  | -- | Add a branch for 'editConstructor' to every case the compiler reports (spec §5.6).
    -- With no body, every site gets a todo branch.
    Complete {editConstructor :: Template, editWhere :: Maybe Template, editBody :: Maybe src}
  | -- | Replace whole lines (1-based, inclusive) that must still read
    -- 'editOld' — a rule's fix (spec §5.9); not written in generator.toml.
    Replace {editTarget :: Template, editLines :: (Int, Int), editOld :: Text, editSource :: src}
  | -- | A snippet (spec §5.3): the text goes at the caller's cursor (`gen --at`),
    -- replacing that line; see "Olifant.Cursor".
    Cursor {editSource :: src}
  deriving stock (Show, Functor, Foldable, Traversable)

data OnExists = ErrorIfExists | SkipIfExists | OverwriteIfExists
  deriving stock (Eq, Show)

-- | `unless = { exists = "<glob>" }`: a project file matches the glob;
-- `unless = { matches = "<regex>" }`: a line of the edit's own target matches,
-- as this plan has it so far. The second is how an edit knows its text is
-- already there by name, wherever it is (spec §5.4).
data Unless t = UnlessExists t | UnlessMatches t
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- | A generator of one replacement: lines @l1..l2@ of @path@, which must
-- read @old@, become @new@ (a template: it may hold holes) — how a rule's
-- fix removes or rewrites the code it found (spec §5.9).
replaceGenerator :: FilePath -> (Int, Int) -> Text -> Template -> Generator
replaceGenerator path range old new =
  Generator (GeneratorId "olifant/fix") "Replace the code a rule found" "" 1 [] [] Nothing [Replace (Template (T.pack path) [Lit (T.pack path)]) range old new] [] [] [] Nothing (Just "Fix") [] False []

-- | The name people see: `name`, or one made from the id
-- (`rust-htmx/command-dependency` → "Command Dependency").
generatorName :: GeneratorWith s -> Text
generatorName g = fromMaybe derived (genName g)
  where
    derived = T.unwords [T.toUpper (T.take 1 w) <> T.drop 1 w | w <- T.split (`elem` ("-_" :: String)) (snd (T.breakOnEnd "/" (generatorIdText (genId g)))), not (T.null w)]

-- | Where a generator applies (spec §4.6): a path glob whose `{slot}`s bind
-- inputs, an optional ast-grep rule the cursor must be inside, regexes over
-- that node's text for more inputs, an input that takes the file's own path,
-- the label an editor shows, and what the rule looks for, in words.
data AppliesAt = AppliesAt {apPath :: Text, apNode :: Maybe Rule, apBind :: [(Text, Text)], apFile :: Maybe Text, apLabel :: Maybe Template, apWhere :: Maybe Template, apSuggest :: Bool, apTrigger :: Maybe Text, apAfter :: Maybe Rule}
  deriving stock (Show)

-- | Where a hole's value comes from (spec §4.9): a tab stop for you, a tab
-- stop pre-filled from a template, or the model (the default).
data HoleSource = Yours | Derived Template | Model
  deriving stock (Show)

describeHoleSource :: HoleSource -> Text
describeHoleSource = \case
  Yours -> "yours"
  Derived _ -> "derived"
  Model -> "model"

-- | Where an insert goes: a phrase (rendered, then parsed) or a raw ast-grep rule.
data AnchorSpec = AtPhrase Template | AtRule Rule Position
  deriving stock (Show)

data SourceRef
  = TemplateFile FilePath
  | ExemplarFile Text [Binding]
  | -- | a fragment exemplar (spec §4.3): file ref, items_of target, matching regex, bindings
    FragmentFile Text Text Text [Binding]
  | InlineText Template
  deriving stock (Show)

data InputSpec = InputSpec
  { inputName :: Text
  , inputType :: InputType
  , inputOptional :: Bool
  , inputDefault :: Maybe Text
  , inputPrompt :: Maybe Text
  }
  deriving stock (Eq, Show)

data InputType = ScalarInput Scalar | ListInput ListItem
  deriving stock (Eq, Show)

-- | 'EnumOf': `one_of` (spec §4.1) — the query's name, which part of each
-- record is the choice, and the choices once resolved against a project.
data Scalar = IdentS | TextS | BoolS | EnumS [Text] | EnumOf Text ChoiceValue Choices
  deriving stock (Eq, Show)

-- | 'Resolved': the choices and the (input, value) context the query used;
-- 'NeedsInputs': inputs above it that have no value yet.
data Choices = Unresolved | Resolved [Text] [(Text, Text)] | NeedsInputs [Text]
  deriving stock (Eq, Show)

data ListItem = ScalarItems Scalar | RecordItems [FieldSpec]
  deriving stock (Eq, Show)

data FieldSpec = FieldSpec {fieldName :: Text, fieldType :: Scalar, fieldDefault :: Maybe Text}
  deriving stock (Eq, Show)

-- | Why a generator ref (a [[uses]] entry, a step) did not resolve: it names
-- nothing, or it names a generator whose definition is broken (a different
-- exit code, spec §11.3).
data ResolveFailure = ResolveNotFound Text | ResolveInvalid Text
  deriving stock (Eq, Show)
