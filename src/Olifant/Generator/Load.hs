module Olifant.Generator.Load
  ( parseGenerator
  , topKeys
  , inputKeys
  , createKeys
  , insertKeys
  , completeKeys
  , requiresKeys
  , useKeys
  ) where

import Control.Monad (forM_, when, (>=>))
import Data.Char (isAlphaNum)
import Data.Set qualified as S
import Olifant.Generator.Inputs (inputsUsed)
import Data.Bifunctor (first)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Anchor (Position (..), parsePhrase)
import Olifant.Generator.Definition
import Olifant.Language (ruleFromTree)
import Olifant.Naming (parseName)
import Olifant.Query (Query (..), posixRegex, queryFromTree, queryVariables)
import Olifant.Template (Binding (..), Template, describeTemplateError, freeVariables, parseTemplate)
import Olifant.Toml

parseGenerator :: GeneratorId -> Text -> Either Text GeneratorDef
parseGenerator gid src = do
  top <- parseToml src
  onlyKeysWith topAdvice topKeys top
  description <- required "description" text top
  language <- required "language" text top
  version <- maybe 1 fromInteger <$> optional "version" int top
  inputs0 <- optional "inputs" (shaped "[inputs.NAME] tables, one per input" table) top >>= maybe (Right []) inputsFrom
  -- In the order the author wrote them, which `show` and prompts follow.
  let declared = keyOrder src ["inputs"]
      inputs = sortOn (\i -> length (takeWhile (/= inputName i) declared)) inputs0
  derived <- optional "derive" (shaped "a [derive] table of NAME = \"{= … =}\" lines" table) top >>= maybe (Right []) deriveFrom
  requires <- optional "requires" (table >=> requiresFrom) top
  edits <- fromMaybe [] <$> optional "edits" (shaped "[[edits]] (double brackets: an array of tables, one per edit)" (list (table >=> editFrom))) top
  -- A snippet's distant consequences are the follow-up its result raises (spec §4.9).
  when (any isCursor edits && length edits > 1) (Left "a cursor edit (a snippet) must be the generator's only edit: what it implies elsewhere is a separate generator, offered as a follow-up")
  uses <- fromMaybe [] <$> optional "uses" (list (table >=> useFrom)) top
  examples <- fromMaybe [] <$> optional "examples" (list text) top
  withValues <- maybe (Right []) (traverse (\(k, v) -> inKey ("query." <> k) ((k,) <$> queryWithValue v)) . M.toList) =<< optional "query" table top
  let queries = [(k, q) | (k, (q, _)) <- withValues]
  case [k | (k, _) <- queries, k `elem` map inputName inputs ++ map fst derived] of
    k : _ -> Left ("`query." <> k <> "`: `" <> k <> "` is already an input or derived value; name the query differently")
    [] -> Right ()
  inputs' <- traverse (fillChoice withValues) inputs
  -- A query serving an input uses only the inputs above it (spec §4.1).
  forM_ (zip [0 :: Int ..] inputs') $ \(i, spec) -> case inputType spec of
    ScalarInput (EnumOf q _ _) | Just query <- lookup q queries ->
      let above = map inputName (take i inputs')
          used = inputsUsed derived (queryVariables query)
       in case [n | n <- map inputName (drop i inputs'), n `S.member` used, n `notElem` above] of
            n : _ -> Left ("`[query." <> q <> "]` uses `" <> n <> "`, which is declared after `" <> inputName spec <> "`")
            [] -> Right ()
    _ -> Right ()
  appliesAt <- optional "applies_at" (table >=> appliesAtFrom (map inputName inputs')) top
  name <- optional "name" text top
  pure (Generator gid description language version inputs' derived requires edits uses examples queries appliesAt name)
  where
    queryWithValue v = do
      t <- table v
      q <- queryFromTree (TTable (M.delete "value" t))
      cv <- traverse (text >=> choiceValueFrom) (M.lookup "value" t)
      pure (q, fromMaybe (case q of GlobQuery {} -> FileName; _ -> FieldValue "name") cv)
    fillChoice withValues spec = case inputType spec of
      ScalarInput (EnumOf q _ c) -> case lookup q withValues of
        Nothing -> Left ("`inputs." <> inputName spec <> "`: one_of = \"" <> q <> "\" but there is no [query." <> q <> "]")
        Just (_, cv) -> Right spec {inputType = ScalarInput (EnumOf q cv c)}
      _ -> Right spec

-- | `[applies_at]` (spec §4.6). Slots and bind names must be inputs.
appliesAtFrom :: [Text] -> Table -> Either Text AppliesAt
appliesAtFrom inputNames t = do
  onlyKeys ["path", "node", "bind", "file", "label", "where", "suggest"] t
  path <- required "path" text t
  case [x | x <- slots path, x `notElem` inputNames] of
    x : _ -> Left ("`{" <> x <> "}` in applies_at.path is not an input (inputs: " <> T.intercalate ", " inputNames <> ")")
    [] -> Right ()
  node <- traverse (inKey "node" . ruleFromTree) (M.lookup "node" t)
  bind <- maybe (Right []) (traverse (\(k, v) -> inKey ("bind." <> k) (if k `elem` inputNames then (k,) <$> (text v >>= posixRegex) else Left ("`" <> k <> "` is not an input"))) . M.toList) =<< optional "bind" table t
  file <- traverse (\v -> text v >>= \k -> if k `elem` inputNames then Right k else Left ("file: `" <> k <> "` is not an input")) (M.lookup "file" t)
  label <- traverse (text >=> tmpl) (M.lookup "label" t)
  where' <- traverse (inKey "where" . (text >=> tmpl)) (M.lookup "where" t)
  -- `suggest`: editors may offer it unasked, as ghost text or a follow-up (spec §4.8)
  suggest <- fromMaybe False <$> optional "suggest" bool t
  pure (AppliesAt path node bind file label where' suggest)
  where
    slots p = case T.breakOn "{" p of
      (_, "") -> []
      (_, rest) -> let (name, after) = T.breakOn "}" (T.drop 1 rest) in name : slots (T.drop 1 after)

-- | `value` in words (spec §4.1), a `{= … =}` template, or a field name.
choiceValueFrom :: Text -> Either Text ChoiceValue
choiceValueFrom v = case v of
  "file name" -> Right FileName
  "parent folder" -> Right ParentFolder
  "path" -> Right PathValue
  _ | "{=" `T.isInfixOf` v -> ValueTemplate <$> tmpl v
    | T.all (\c -> isAlphaNum c || c == '_') v && not (T.null v) -> Right (FieldValue v)
    | otherwise -> Left ("`value`: expected \"file name\", \"parent folder\", \"path\", a field name or a {= … =} template; got `" <> v <> "`")

-- | The keys each table accepts (also the source of `olifant schema`).
topKeys, inputKeys, createKeys, insertKeys, completeKeys, cursorKeys, requiresKeys, useKeys :: [Text]
topKeys = ["name", "description", "language", "version", "inputs", "derive", "requires", "edits", "uses", "examples", "query", "applies_at"]
inputKeys = ["type", "optional", "default", "prompt", "choices", "one_of", "choices_from", "of"]
createKeys = ["create", "template", "exemplar", "bind", "text", "on_exists", "unless"]
insertKeys = ["insert", "at", "anchor", "template", "text", "fragment", "bind", "unless"]
completeKeys = ["complete", "where", "template", "text"]
cursorKeys = ["cursor", "template", "text"]
requiresKeys = ["exists", "hint"]
useKeys = ["generator", "for_each", "with"]

-- | What a misplaced key usually means (measured: the keys models invent).
topAdvice, inputAdvice, editAdvice :: [(Text, Text)]
topAdvice =
  [ (k, "a generator's id is its directory (.olifant/generators/ID/generator.toml), not a key; `name` is what editors show") | k <- ["id", "generator"] ]
    ++ [ ("edit", "edits are [[edits]] tables"), ("steps", "run other generators with [[uses]]"), ("templates", "templates live in the generator's templates/ directory; name one with `template` on an edit") ]
inputAdvice =
  [ ("description", "did you mean `prompt`?")
  , ("required", "inputs are required unless `optional = true`")
  , ("name", "the input's name is its table name: [inputs.NAME]")
  , ("derive", "derived values go in a top-level [derive] table")
  , ("values", "did you mean `choices`?")
  ]
editAdvice =
  [ (k, "the file is the value of `insert` (or `create`): insert = \"src/Main.elm\"") | k <- ["path", "file", "target"] ]
    ++ [ (k, "did you mean `text`?") | k <- ["content", "contents", "body", "code"] ]
    ++ [ (k, "say where with `at`: at = \"end of type Route\" (`olifant anchors LANGUAGE` lists the phrases)") | k <- ["position", "location", "after", "before"] ]
    ++ [ ("description", "edits take no description") ]

-- | A string for a file path, explaining the usual mistake of a table.
pathText :: Text -> Tree -> Either Text Text
pathText key = \case
  TText t -> Right t
  other -> Left ("expected the file path as a string (" <> key <> " = \"src/Main.elm\"), got " <> describeTree other <> "; the text to add goes in `text`")

-- | A decoder whose wrong-shape error also says what the right shape is.
shaped :: Text -> (Tree -> Either Text a) -> Tree -> Either Text a
shaped want f v = first (\e -> e <> "; write " <> want) (f v)

tmpl :: Text -> Either Text Template
tmpl = first describeTemplateError . parseTemplate

inputsFrom :: Table -> Either Text [InputSpec]
inputsFrom tbl = traverse (\(k, v) -> inKey ("inputs." <> k) (table v >>= inputFrom k)) (M.toList tbl)

inputFrom :: Text -> Table -> Either Text InputSpec
inputFrom name t = do
  onlyKeysWith inputAdvice inputKeys t
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
  "enum" -> case (M.lookup "choices" t, M.lookup "one_of" t, M.lookup "choices_from" t) of
    (_, _, Just _) -> Left "`choices_from` is retired: use `one_of` (one_of = \"NAME\") and a [query.NAME] table (spec §4.1)"
    (Just _, Nothing, _) -> EnumS <$> required "choices" (list text) t
    -- The value lives on the query; parseGenerator fills it in.
    (Nothing, Just _, _) -> (\q -> EnumOf q FileName Unresolved) <$> required "one_of" text t
    _ -> Left "an enum needs exactly one of `choices` or `one_of`"
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

isCursor :: EditWith s -> Bool
isCursor = \case
  Cursor {} -> True
  _ -> False

requiresFrom :: Table -> Either Text Requires
requiresFrom t = do
  onlyKeys requiresKeys t
  exists <- fromMaybe [] <$> optional "exists" (list (text >=> tmpl)) t
  hint <- optional "hint" (text >=> tmpl) t
  pure (Requires exists hint)

editFrom :: Table -> Either Text (EditWith SourceRef)
editFrom t = case filter (`M.member` t) ["create", "insert", "complete", "cursor"] of
  ["cursor"] -> do
    onlyKeysWith editAdvice cursorKeys t
    required "cursor" bool t >>= \case
      True -> Cursor <$> sourceFrom False t
      False -> Left "`cursor = true` makes the edit a snippet at the editor's cursor; leave `cursor` out otherwise"
  ["create"] -> do
    onlyKeysWith editAdvice createKeys t
    target <- required "create" (pathText "create" >=> tmpl) t
    src <- sourceFrom True t
    onExists <- fromMaybe ErrorIfExists <$> optional "on_exists" (text >=> onExistsFrom) t
    unlessG <- unlessFrom t
    pure (Create target src onExists unlessG)
  ["insert"] -> do
    onlyKeysWith editAdvice insertKeys t
    target <- required "insert" (pathText "insert" >=> tmpl) t
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
    onlyKeysWith editAdvice completeKeys t
    ctor <- required "complete" (text >=> tmpl) t
    wher <- optional "where" (text >=> targetTemplate) t
    -- No text or template: leave a todo at every site (spec §5.6).
    src <- if any (`M.member` t) ["text", "template"] then Just <$> sourceFrom False t else pure Nothing
    case (wher, src) of
      (Just _, Nothing) -> Left "`where` chooses a body for one kind of case, but this edit has no `text` or `template`; add one, or drop `where` to leave todos everywhere"
      _ -> pure (Complete ctor wher src)
  _ -> Left "each edit needs exactly one of `create`, `insert`, `complete` or `cursor`: create = \"path/{= name =}.ext\" (a new file), insert = \"src/Main.elm\" with at = \"end of …\" (text into a file), or complete = \"Module.{= Ctor =}\" (case branches the compiler asks for)"

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
  onlyKeys useKeys t
  ref <- required "generator" text t
  forEach <- optional "for_each" text t
  with <- fromMaybe [] <$> optional "with" (table >=> withFrom) t
  pure (UseStep ref forEach with)
  where
    withFrom tbl = traverse (\(k, v) -> inKey k ((k,) <$> (text v >>= tmpl))) (M.toList tbl)

-- | `unless = { exists = "<glob>" }` or `{ matches = "<regex>" }` (spec §5).
-- The regex is checked with each `{= … =}` standing in for a plain name.
unlessFrom :: Table -> Either Text (Maybe (Unless Template))
unlessFrom = optional "unless" $ table >=> \u -> do
  onlyKeys ["exists", "matches"] u
  exists <- optional "exists" text u
  matches <- optional "matches" text u
  case (exists, matches) of
    (Just g, Nothing) -> UnlessExists <$> tmpl g
    (Nothing, Just re) -> UnlessMatches <$> inKey "matches" (posixRegex (placeholdersAsNames re) >> tmpl re)
    _ -> Left "write exactly one of `exists` (a glob) or `matches` (a regular expression)"
  where
    placeholdersAsNames re = case T.breakOn "{=" re of
      (before, rest)
        | T.null rest -> re
        | otherwise -> before <> "x" <> placeholdersAsNames (T.drop 2 (snd (T.breakOn "=}" rest)))
