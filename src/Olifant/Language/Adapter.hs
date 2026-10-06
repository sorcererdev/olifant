-- | Language adapters (spec §12): pack data in `languages/<name>.toml`.
module Olifant.Language.Adapter
  ( Language (..)
  , Grammar (..)
  , TargetDef (..)
  , CheckDef (..)
  , HoleKind (..)
  , ImportMerge (..)
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
  , langSeparator :: Maybe Text
  -- ^ `separator`: what follows each item of a container (`,`), so an item
  -- can join a one-line container (inline, or one item per line)
  , langLineLength :: Int
  -- ^ `line_length` (default 88): the longest line an inline item may make
  , langImportMerge :: Maybe ImportMerge
  -- ^ `[import_merge]`: how an import of names from a module joins a line
  -- that already imports from it
  }
  deriving stock (Show)

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
-- checks of one workspace run one at a time.
data CheckDef = CheckDef {checkCommand :: [Text], checkContext :: [Text], checkFormat :: Text, checkRoot :: [Text], checkRootOutermost :: Bool, checkExclusive :: Bool}
  deriving stock (Eq, Show)

parseLanguage :: Text -> FilePath -> Text -> Either Text Language
parseLanguage name packRoot src = do
  top <- parseToml src
  onlyKeysWith [("qualify", "`qualify` is retired: the [check] format decides how patterns are written")] ["files", "comment", "grammar", "targets", "placeholder", "branch", "check", "indent", "holes", "imports", "import_merge", "separator", "line_length"] top
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
  pure (Language name files comment grammar targets placeholder branch check indent holes imports separator lineLength importMerge)
  where
    tmpl = first describeTemplateError . parseTemplate
    grammarFrom = \case
      TText builtIn -> Right (BuiltInGrammar builtIn)
      TTable t -> onlyKeys ["source"] t >> GrammarSource . (\s -> packRoot </> "languages" </> T.unpack s) <$> required "source" text t
      other -> Left ("expected a built-in grammar name or { source = \"dir\" }, got " <> describeTree other)
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
    checkFrom t = do
      onlyKeys ["command", "context", "format", "root", "root_pick", "exclusive"] t
      format <- required "format" text t
      if format `elem` knownFormats
        then Right ()
        else Left ("`format`: unknown format `" <> format <> "`; expected one of " <> T.intercalate ", " knownFormats)
      CheckDef <$> required "command" (list text) t <*> (fromMaybe [] <$> optional "context" (list text) t) <*> pure format
        <*> (fromMaybe [] <$> optional "root" (list text) t)
        <*> (optional "root_pick" text t >>= \case
              Nothing -> Right False
              Just "nearest" -> Right False
              Just "outermost" -> Right True
              Just other -> Left ("`root_pick`: expected \"nearest\" or \"outermost\", got `" <> other <> "`"))
        <*> (fromMaybe False <$> optional "exclusive" bool t)

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
