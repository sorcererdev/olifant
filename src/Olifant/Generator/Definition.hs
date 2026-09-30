module Olifant.Generator.Definition
  ( GeneratorId (..)
  , GeneratorWith (..)
  , GeneratorDef
  , Generator
  , Requires (..)
  , EditWith (..)
  , OnExists (..)
  , AnchorSpec (..)
  , SourceRef (..)
  , UseStep (..)
  , InputSpec (..)
  , InputType (..)
  , Scalar (..)
  , ListItem (..)
  , FieldSpec (..)
  ) where

import Data.Text (Text)
import Olifant.Anchor (Position)
import Olifant.Language (Rule)
import Olifant.Query (Query)
import Olifant.Template (Binding, Template)

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
  = -- | 'editUnless': a glob; the edit is skipped when it matches a file (spec §5).
    Create {editTarget :: Template, editSource :: src, editOnExists :: OnExists, editUnless :: Maybe Template}
  | Insert {editTarget :: Template, editAt :: [AnchorSpec], editSource :: src, editUnless :: Maybe Template}
  | -- | Add a branch for 'editConstructor' to every case the compiler reports (spec §5.6).
    Complete {editConstructor :: Template, editWhere :: Maybe Template, editSource :: src}
  deriving stock (Show, Functor, Foldable, Traversable)

data OnExists = ErrorIfExists | SkipIfExists | OverwriteIfExists
  deriving stock (Eq, Show)

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

-- | 'EnumQ': `choices_from` — the query, the value template, and the choices
-- once resolved against a project ('Nothing' until then).
data Scalar = IdentS | TextS | BoolS | EnumS [Text] | EnumQ Query Template (Maybe [Text])
  deriving stock (Eq, Show)

data ListItem = ScalarItems Scalar | RecordItems [FieldSpec]
  deriving stock (Eq, Show)

data FieldSpec = FieldSpec {fieldName :: Text, fieldType :: Scalar, fieldDefault :: Maybe Text}
  deriving stock (Eq, Show)
