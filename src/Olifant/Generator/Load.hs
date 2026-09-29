module Olifant.Generator.Load (parseGenerator) where

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Generator.Definition
import Olifant.Naming (parseName)
import Olifant.Template (Binding (..), Template, describeTemplateError, parseTemplate)
import Olifant.Toml

parseGenerator :: GeneratorId -> Text -> Either Text GeneratorDef
parseGenerator gid src = do
  top <- parseToml src
  onlyKeys ["description", "language", "version", "inputs", "derive", "requires", "edits", "uses"] top
  description <- required "description" text top
  language <- required "language" text top
  version <- maybe 1 fromInteger <$> optional "version" int top
  inputs <- optional "inputs" table top >>= maybe (Right []) inputsFrom
  derived <- optional "derive" table top >>= maybe (Right []) deriveFrom
  requires <- optional "requires" (table >=> requiresFrom) top
  edits <- fromMaybe [] <$> optional "edits" (list (table >=> editFrom)) top
  uses <- fromMaybe [] <$> optional "uses" (list (table >=> useFrom)) top
  pure (Generator gid description language version inputs derived requires edits uses)

tmpl :: Text -> Either Text Template
tmpl = first describeTemplateError . parseTemplate

inputsFrom :: Table -> Either Text [InputSpec]
inputsFrom tbl = traverse (\(k, v) -> inKey ("inputs." <> k) (table v >>= inputFrom k)) (M.toList tbl)

inputFrom :: Text -> Table -> Either Text InputSpec
inputFrom name t = do
  onlyKeys ["type", "optional", "default", "prompt", "choices", "of"] t
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
  "enum" -> EnumS <$> required "choices" (list text) t
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
editFrom t = case (M.member "create" t, M.member "insert" t) of
  (True, False) -> do
    onlyKeys ["create", "template", "exemplar", "bind", "text", "on_exists"] t
    target <- required "create" (text >=> tmpl) t
    src <- sourceFrom True t
    onExists <- fromMaybe ErrorIfExists <$> optional "on_exists" (text >=> onExistsFrom) t
    pure (Create target src onExists)
  (False, True) -> do
    onlyKeys ["insert", "anchor", "template", "text"] t
    target <- required "insert" (text >=> tmpl) t
    anchor <- required "anchor" (table >=> anchorFrom) t
    src <- sourceFrom False t
    pure (Insert target anchor src)
  _ -> Left "each edit needs exactly one of `create` or `insert`"

sourceFrom :: Bool -> Table -> Either Text SourceRef
sourceFrom allowExemplar t = case (M.lookup "template" t, M.lookup "exemplar" t, M.lookup "text" t) of
  (Just v, Nothing, Nothing) -> TemplateFile . T.unpack <$> inKey "template" (text v)
  (Nothing, Just v, Nothing) | allowExemplar -> do
    ref <- inKey "exemplar" (text v)
    binds <- required "bind" (table >=> bindsFrom) t
    pure (ExemplarFile ref binds)
  (Nothing, Nothing, Just v) -> InlineText <$> inKey "text" (text v >>= tmpl)
  _
    | allowExemplar -> Left "a create needs exactly one of `template`, `exemplar` or `text`"
    | otherwise -> Left "an insert needs exactly one of `template` or `text`"

bindsFrom :: Table -> Either Text [Binding]
bindsFrom tbl = traverse (\(k, v) -> inKey k (text v >>= fmap (Binding k) . parseName)) (M.toList tbl)

anchorFrom :: Table -> Either Text Anchor
anchorFrom t = onlyKeys ["marker"] t >> Marker <$> required "marker" text t

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
