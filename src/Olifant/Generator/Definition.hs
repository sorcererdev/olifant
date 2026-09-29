module Olifant.Generator.Definition
  ( GeneratorId (..)
  , GeneratorWith (..)
  , GeneratorDef
  , Generator
  , Requires (..)
  , EditWith (..)
  , OnExists (..)
  , Anchor (..)
  , SourceRef (..)
  , UseStep (..)
  , InputSpec (..)
  , InputType (..)
  , Scalar (..)
  , ListItem (..)
  , FieldSpec (..)
  ) where

import Data.Text (Text)
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
  = Create {editTarget :: Template, editSource :: src, editOnExists :: OnExists}
  | Insert {editTarget :: Template, editAnchor :: Anchor, editSource :: src}
  deriving stock (Show, Functor, Foldable, Traversable)

data OnExists = ErrorIfExists | SkipIfExists | OverwriteIfExists
  deriving stock (Eq, Show)

newtype Anchor = Marker Text
  deriving stock (Eq, Show)

data SourceRef
  = TemplateFile FilePath
  | ExemplarFile Text [Binding]
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

data Scalar = IdentS | TextS | BoolS | EnumS [Text]
  deriving stock (Eq, Show)

data ListItem = ScalarItems Scalar | RecordItems [FieldSpec]
  deriving stock (Eq, Show)

data FieldSpec = FieldSpec {fieldName :: Text, fieldType :: Scalar, fieldDefault :: Maybe Text}
  deriving stock (Eq, Show)
