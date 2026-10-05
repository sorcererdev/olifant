-- | Language adapters (spec §12): pack data in `languages/<name>.toml`.
module Olifant.Language.Adapter
  ( Language (..)
  , Grammar (..)
  , TargetDef (..)
  , CheckDef (..)
  , HoleKind (..)
  , parseLanguage
  , matchesGlob
  , languageFor
  , targetPhrase
  , matchShape
  ) where

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Char (isUpper)
import Data.List (find)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Glob (matchesGlob)
import Olifant.Language.Rule
import Olifant.Language.Support (knownFormats)
import Olifant.Template (Template, describeTemplateError, freeVariables, parseTemplate)
import Data.Set qualified as S
import Olifant.Toml
import Text.Regex.TDFA (Regex, makeRegexM)
import System.FilePath ((</>))

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
  }
  deriving stock (Show)

-- | How a hole of one kind is written unfilled ('kindPlaceholder', with
-- @hole@ and @instruction@; it must compile) and the code a fill must parse
-- in ('kindWrap', containing the literal @{= fill =}@, or @{= fill|indent =}@
-- for a body: every line one level in).
data HoleKind = HoleKind {kindPlaceholder :: Template, kindWrap :: Text}
  deriving stock (Show)

data Grammar = BuiltInGrammar Text | GrammarSource FilePath
  deriving stock (Eq, Show)

data TargetDef = TargetDef {targetShape :: [Text], targetContainer :: Rule, targetItems :: Rule, targetExample :: Text}
  deriving stock (Show)

data CheckDef = CheckDef {checkCommand :: [Text], checkContext :: [Text], checkFormat :: Text}
  deriving stock (Eq, Show)

parseLanguage :: Text -> FilePath -> Text -> Either Text Language
parseLanguage name packRoot src = do
  top <- parseToml src
  onlyKeysWith [("qualify", "`qualify` is retired: the [check] format decides how patterns are written")] ["files", "comment", "grammar", "targets", "placeholder", "branch", "check", "indent", "holes", "imports"] top
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
  pure (Language name files comment grammar targets placeholder branch check indent holes imports)
  where
    tmpl = first describeTemplateError . parseTemplate
    grammarFrom = \case
      TText builtIn -> Right (BuiltInGrammar builtIn)
      TTable t -> onlyKeys ["source"] t >> GrammarSource . (\s -> packRoot </> "languages" </> T.unpack s) <$> required "source" text t
      other -> Left ("expected a built-in grammar name or { source = \"dir\" }, got " <> describeTree other)
    holeKindFrom t = do
      onlyKeys ["placeholder", "wrap"] t
      ph <- required "placeholder" (text >=> tmpl) t
      if "hole" `S.member` freeVariables ph then Right () else Left "`placeholder` must use {= hole =} (the hole's name), so every placeholder is unique"
      wrap <- required "wrap" text t
      if any (`T.isInfixOf` wrap) ["{= fill =}", "{= fill|indent =}"]
        then Right (HoleKind ph wrap)
        else Left "`wrap` must contain {= fill =} (or {= fill|indent =}, one level in), where the fill goes"
    targetFrom t = do
      onlyKeys ["phrase", "container", "items", "example"] t
      TargetDef
        <$> (T.words <$> required "phrase" text t)
        <*> required "container" ruleFromTree t
        <*> required "items" ruleFromTree t
        <*> required "example" text t
    checkFrom t = do
      onlyKeys ["command", "context", "format"] t
      format <- required "format" text t
      if format `elem` knownFormats
        then Right ()
        else Left ("`format`: unknown format `" <> format <> "`; expected one of " <> T.intercalate ", " knownFormats)
      CheckDef <$> required "command" (list text) t <*> (fromMaybe [] <$> optional "context" (list text) t) <*> pure format

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
