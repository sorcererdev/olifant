module Olifant.Generator.Load (parseGenerator) where

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Anchor (Position (..), parsePhrase)
import Olifant.Generator.Definition
import Olifant.Language (ruleFromTree)
import Olifant.Naming (parseName)
import Olifant.Query (posixRegex, queryFromTree)
import Olifant.Template (Binding (..), Template, describeTemplateError, freeVariables, parseTemplate)
import Olifant.Toml

parseGenerator :: GeneratorId -> Text -> Either Text GeneratorDef
parseGenerator gid src = do
  top <- parseToml src
  onlyKeys ["description", "language", "version", "inputs", "derive", "requires", "edits", "uses", "examples", "query"] top
  description <- required "description" text top
  language <- required "language" text top
  version <- maybe 1 fromInteger <$> optional "version" int top
  inputs <- optional "inputs" table top >>= maybe (Right []) inputsFrom
  derived <- optional "derive" table top >>= maybe (Right []) deriveFrom
  requires <- optional "requires" (table >=> requiresFrom) top
  edits <- fromMaybe [] <$> optional "edits" (list (table >=> editFrom)) top
  uses <- fromMaybe [] <$> optional "uses" (list (table >=> useFrom)) top
  examples <- fromMaybe [] <$> optional "examples" (list text) top
  queries <- maybe (Right []) (traverse (\(k, v) -> inKey ("query." <> k) ((k,) <$> queryFromTree v)) . M.toList) =<< optional "query" table top
  case [k | (k, _) <- queries, k `elem` map inputName inputs ++ map fst derived] of
    k : _ -> Left ("`query." <> k <> "`: `" <> k <> "` is already an input or derived value; name the query differently")
    [] -> Right ()
  pure (Generator gid description language version inputs derived requires edits uses examples queries)

tmpl :: Text -> Either Text Template
tmpl = first describeTemplateError . parseTemplate

inputsFrom :: Table -> Either Text [InputSpec]
inputsFrom tbl = traverse (\(k, v) -> inKey ("inputs." <> k) (table v >>= inputFrom k)) (M.toList tbl)

inputFrom :: Text -> Table -> Either Text InputSpec
inputFrom name t = do
  onlyKeys ["type", "optional", "default", "prompt", "choices", "choices_from", "of"] t
  ty <- required "type" text t
  itype <- case ty of
    "list" -> ListInput <$> required "of" listItemFrom t
    _ -> ScalarInput <$> scalarFrom ty t
  isOptional <- fromMaybe False <$> optional "optional" bool t
  def <- optional "default" scalarDefault t
  prompt <- optional "prompt" text t
  pure (InputSpec name itype isOptional def prompt)

scalarFrom :: Text -> Table -> Either Text Scalar
scalarFrom ty t = case ty of
  "ident" -> Right IdentS
  "text" -> Right TextS
  "bool" -> Right BoolS
  "enum" -> case (M.lookup "choices" t, M.lookup "choices_from" t) of
    (Just _, Nothing) -> EnumS <$> required "choices" (list text) t
    (Nothing, Just v) -> inKey "choices_from" $ do
      q <- table v
      query <- queryFromTree (TTable (M.delete "value" q))
      valueT <- maybe (tmpl "{= name =}") (text >=> tmpl) (M.lookup "value" q)
      pure (EnumQ query valueT Nothing)
    _ -> Left "an enum needs exactly one of `choices` or `choices_from`"
  other -> Left ("unknown type `" <> other <> "`; expected ident, text, bool, enum or list")

listItemFrom :: Tree -> Either Text ListItem
listItemFrom = \case
  TText ty -> ScalarItems <$> scalarFrom ty M.empty
  fields@(TList _) -> RecordItems <$> list (table >=> fieldFrom) fields
  other -> Left ("expected a type name or an array of fields, got " <> describeTree other)

fieldFrom :: Table -> Either Text FieldSpec
fieldFrom t = do
  onlyKeys ["name", "type", "choices", "default"] t
  name <- required "name" text t
  ty <- required "type" text t >>= \x -> scalarFrom x t
  def <- optional "default" scalarDefault t
  pure (FieldSpec name ty def)

scalarDefault :: Tree -> Either Text Text
scalarDefault = \case
  TText x -> Right x
  TBool b -> Right (if b then "true" else "false")
  TInt i -> Right (T.pack (show i))
  other -> Left ("expected a default value, got " <> describeTree other)

deriveFrom :: Table -> Either Text [(Text, Template)]
deriveFrom tbl = traverse (\(k, v) -> inKey ("derive." <> k) ((,) k <$> (text v >>= tmpl))) (M.toList tbl)

requiresFrom :: Table -> Either Text Requires
requiresFrom t = do
  onlyKeys ["exists", "hint"] t
  exists <- fromMaybe [] <$> optional "exists" (list (text >=> tmpl)) t
  hint <- optional "hint" (text >=> tmpl) t
  pure (Requires exists hint)

editFrom :: Table -> Either Text (EditWith SourceRef)
editFrom t = case filter (`M.member` t) ["create", "insert", "complete"] of
  ["create"] -> do
    onlyKeys ["create", "template", "exemplar", "bind", "text", "on_exists", "unless"] t
    target <- required "create" (text >=> tmpl) t
    src <- sourceFrom True t
    onExists <- fromMaybe ErrorIfExists <$> optional "on_exists" (text >=> onExistsFrom) t
    unlessG <- unlessFrom t
    pure (Create target src onExists unlessG)
  ["insert"] -> do
    onlyKeys ["insert", "at", "anchor", "template", "text", "fragment", "bind", "unless"] t
    target <- required "insert" (text >=> tmpl) t
    anchors <- case (M.lookup "at" t, M.lookup "anchor" t) of
      (Just v, Nothing) -> inKey "at" (atFrom v)
      (Nothing, Just v) -> inKey "anchor" (table v >>= markerAlias)
      _ -> Left "an insert needs exactly one of `at` (preferred) or `anchor`"
    src <- case M.lookup "fragment" t of
      Nothing -> sourceFrom False t
      Just _ | any (`M.member` t) ["text", "template"] -> Left "an insert needs exactly one of `template`, `text` or `fragment`"
      Just v -> inKey "fragment" $ do
        f <- table v
        onlyKeys ["file", "items_of", "matching"] f
        binds <- inKey "bind" (required "bind" (table >=> bindsFrom) t)
        FragmentFile <$> required "file" text f <*> required "items_of" text f <*> inKey "fragment.matching" (required "matching" text f >>= posixRegex) <*> pure binds
    unlessG <- unlessFrom t
    pure (Insert target anchors src unlessG)
  ["complete"] -> do
    onlyKeys ["complete", "where", "template", "text"] t
    ctor <- required "complete" (text >=> tmpl) t
    wher <- optional "where" (text >=> targetTemplate) t
    src <- sourceFrom False t
    pure (Complete ctor wher src)
  _ -> Left "each edit needs exactly one of `create`, `insert` or `complete`"

sourceFrom :: Bool -> Table -> Either Text SourceRef
sourceFrom allowExemplar t = case (M.lookup "template" t, M.lookup "exemplar" t, M.lookup "text" t) of
  (Nothing, Just v, Nothing) | allowExemplar -> do
    ref <- inKey "exemplar" (text v)
    binds <- required "bind" (table >=> bindsFrom) t
    pure (ExemplarFile ref binds)
  _ | M.member "bind" t -> Left "`bind` renames an exemplar's (or fragment's) names; this edit has neither"
  (Just v, Nothing, Nothing) -> TemplateFile . T.unpack <$> inKey "template" (text v)
  (Nothing, Nothing, Just v) -> InlineText <$> inKey "text" (text v >>= tmpl)
  _
    | allowExemplar -> Left "a create needs exactly one of `template`, `exemplar` or `text`"
    | otherwise -> Left "an insert needs exactly one of `template`, `text` or `fragment`"

bindsFrom :: Table -> Either Text [Binding]
bindsFrom tbl = traverse (\(k, v) -> inKey k (text v >>= fmap (Binding k) . parseName)) (M.toList tbl)

atFrom :: Tree -> Either Text [AnchorSpec]
atFrom = \case
  TList [] -> Left "`at` needs at least one anchor"
  v@(TList _) -> list one v
  v -> (: []) <$> one v
  where
    one = \case
      TText s -> AtPhrase <$> checkedTemplate "" s
      TTable tb -> do
        onlyKeys ["ast", "pos"] tb
        AtRule <$> required "ast" ruleFromTree tb <*> required "pos" (text >=> positionFrom) tb
      other -> Left ("expected a phrase or { ast = …, pos = … }, got " <> describeTree other)

-- | `where` holds a target (`case in update`), checked as the target of a phrase.
targetTemplate :: Text -> Either Text Template
targetTemplate = checkedTemplate "end of "

-- | A phrase template; one without variables is parsed now, so typos fail at load.
checkedTemplate :: Text -> Text -> Either Text Template
checkedTemplate prefix s = do
  t <- tmpl s
  if null (freeVariables t) then () <$ parsePhrase (prefix <> s) else Right ()
  pure t

positionFrom :: Text -> Either Text Position
positionFrom = \case
  "start" -> Right StartOf
  "end" -> Right EndOf
  "before" -> Right Before
  "after" -> Right After
  other -> Left ("expected start, end, before or after, got `" <> other <> "`")

-- | Step 1's `anchor = { marker = "X" }`, kept as `at = "before marker X"`.
markerAlias :: Table -> Either Text [AnchorSpec]
markerAlias t = do
  onlyKeys ["marker"] t
  m <- required "marker" text t
  (: []) . AtPhrase <$> tmpl ("before marker " <> m)

onExistsFrom :: Text -> Either Text OnExists
onExistsFrom = \case
  "error" -> Right ErrorIfExists
  "skip" -> Right SkipIfExists
  "overwrite" -> Right OverwriteIfExists
  other -> Left ("expected error, skip or overwrite, got `" <> other <> "`")

useFrom :: Table -> Either Text UseStep
useFrom t = do
  onlyKeys ["generator", "for_each", "with"] t
  ref <- required "generator" text t
  forEach <- optional "for_each" text t
  with <- fromMaybe [] <$> optional "with" (table >=> withFrom) t
  pure (UseStep ref forEach with)
  where
    withFrom tbl = traverse (\(k, v) -> inKey k ((k,) <$> (text v >>= tmpl))) (M.toList tbl)

-- | `unless = { exists = "<glob>" }` (spec §5).
unlessFrom :: Table -> Either Text (Maybe Template)
unlessFrom = optional "unless" (table >=> \u -> onlyKeys ["exists"] u >> required "exists" (text >=> tmpl) u)
