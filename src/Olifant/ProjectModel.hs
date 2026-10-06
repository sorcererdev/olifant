-- | The project in Olifant's shared vocabulary (olifant lsp spec §1b): its
-- services, commands, queries, ports, adapters, resources, data models,
-- tests and pages, with stable ids, and how they relate (uses, implements,
-- calls, covers). Each pack says how its conventions map onto it in
-- `model.toml`; packs may add namespaced kinds (`elm:route`).
module Olifant.ProjectModel
  ( Kind (..)
  , kindText
  , RelKind (..)
  , relKindText
  , Element (..)
  , Relation (..)
  , ProjectModel (..)
  , ModelMap
  , parseModelMap
  , buildModel
  ) where

import Control.Monad (forM, (>=>))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.List (sort, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Query (Queries, Query, QueryFailure (..), queryFromTree, runQuery)
import Olifant.Template (Template, Value (..), describeTemplateError, parseTemplate, render)
import Olifant.Toml qualified as Toml
import Text.Regex.TDFA (Regex, makeRegexM, match, matchTest)

data Kind = Service | Command | Query | Port | Adapter | Resource | DataModel | Test | Page | Custom Text
  deriving stock (Eq, Ord, Show)

vocabulary :: [(Text, Kind)]
vocabulary = [("service", Service), ("command", Command), ("query", Query), ("port", Port), ("adapter", Adapter), ("resource", Resource), ("model", DataModel), ("test", Test), ("page", Page)]

kindText :: Kind -> Text
kindText = \case
  Custom t -> t
  k -> fromMaybe "" (lookup k [(k', t) | (t, k') <- vocabulary])

-- | A kind of the vocabulary, or a pack's own, namespaced (`elm:route`).
kindFrom :: Text -> Either Text Kind
kindFrom t = case lookup t vocabulary of
  Just k -> Right k
  Nothing
    | [ns, name] <- T.splitOn ":" t, not (T.null ns), not (T.null name) -> Right (Custom t)
    | otherwise -> Left ("unknown kind `" <> t <> "`: expected one of " <> T.intercalate ", " (map fst vocabulary) <> ", or a pack's own, namespaced (`elm:route`)")

data RelKind = Uses | Implements | Calls | Covers
  deriving stock (Eq, Ord, Show)

relVocabulary :: [(Text, RelKind)]
relVocabulary = [("uses", Uses), ("implements", Implements), ("calls", Calls), ("covers", Covers)]

relKindText :: RelKind -> Text
relKindText k = fromMaybe "" (lookup k [(k', t) | (t, k') <- relVocabulary])

-- | One thing in the project. Its id is `PROJECT:` and a path-like name
-- (`bistro:booking/commands/create_booking`), the same however the files
-- are listed.
data Element = Element {elId :: Text, elKind :: Kind, elName :: Text, elPath :: FilePath, elLine :: Int}
  deriving stock (Eq, Ord, Show)

data Relation = Relation {relFrom :: Text, relKind :: RelKind, relTo :: Text}
  deriving stock (Eq, Ord, Show)

-- | The model, and why parts of it could not be read (a query that failed).
data ProjectModel = ProjectModel {pmProject :: Text, pmElements :: [Element], pmRelations :: [Relation], pmProblems :: [Text]}
  deriving stock (Eq, Show)

instance ToJSON ProjectModel where
  toJSON m =
    object
      [ "project" .= pmProject m
      , "elements" .= [object ["id" .= elId e, "kind" .= kindText (elKind e), "name" .= elName e, "path" .= elPath e, "line" .= elLine e] | e <- pmElements m]
      , "relations" .= [object ["from" .= relFrom r, "kind" .= relKindText (relKind r), "to" .= relTo r] | r <- pmRelations m]
      , "problems" .= pmProblems m
      ]

-- | A pack's `model.toml`: which query records are which elements, and how
-- they relate.
data ModelMap = ModelMap {mmElements :: [ElementDef], mmRelations :: [RelationDef]}

-- | Elements of a kind: one per record of the query, its id and name
-- templates over the record; @differ@: only records whose two renderings
-- differ (a port's adapters, not the port's own file beside them).
data ElementDef = ElementDef {edKind :: Kind, edQuery :: Query, edId :: Template, edName :: Maybe Template, edDiffer :: Maybe (Template, Template)}

-- | Relations from each element of a kind (templates over its record), or
-- from each record of a query (@from@ renders the element's id).
data Source = FromKind Kind | FromQuery Query Template

-- | What a relation points at: an element id, an element's path, or an
-- element's name among some kinds; each rendered, then rewritten.
data Target = ToId Template | ToPath Template | ToName Template [Kind]

data RelationDef = RelationDef {rdKind :: RelKind, rdSource :: Source, rdTo :: Target, rdRewrite :: [(Text, Text)]}

parseModelMap :: Text -> Either Text ModelMap
parseModelMap src = do
  top <- Toml.parseToml src
  Toml.onlyKeys ["elements", "relations"] top
  elements <- fromMaybe [] <$> Toml.optional "elements" (Toml.list (Toml.table >=> elementDef)) top
  relations <- fromMaybe [] <$> Toml.optional "relations" (Toml.list (Toml.table >=> relationDef)) top
  pure (ModelMap elements relations)
  where
    tmpl t = first describeTemplateError (parseTemplate t)
    elementDef t = do
      Toml.onlyKeys ["kind", "query", "id", "name", "differ"] t
      kind <- Toml.required "kind" (Toml.text >=> kindFrom) t
      Toml.inKey (kindText kind) $
        ElementDef kind
          <$> Toml.required "query" queryFromTree t
          <*> Toml.required "id" (Toml.text >=> tmpl) t
          <*> Toml.optional "name" (Toml.text >=> tmpl) t
          <*> Toml.optional "differ" (Toml.list (Toml.text >=> tmpl) >=> pair "differ") t
    relationDef t = do
      Toml.onlyKeys ["kind", "from", "query", "to", "to_path", "to_name", "to_kinds", "rewrite"] t
      kind <- Toml.required "kind" (Toml.text >=> \k -> maybe (Left ("unknown relation `" <> k <> "`: expected one of " <> T.intercalate ", " (map fst relVocabulary))) Right (lookup k relVocabulary)) t
      Toml.inKey (relKindText kind) $ do
        source <- Toml.optional "query" queryFromTree t >>= \case
          Nothing -> FromKind <$> Toml.required "from" (Toml.text >=> kindFrom) t
          Just q -> FromQuery q <$> Toml.required "from" (Toml.text >=> tmpl) t
        targets <- sequence [fmap c <$> Toml.optional k (Toml.text >=> tmpl) t | (k, c) <- [("to", ToId), ("to_path", ToPath), ("to_name", (`ToName` []))]]
        target <- case [x | Just x <- targets] of
          [ToName n _] -> ToName n . fromMaybe [] <$> Toml.optional "to_kinds" (Toml.list (Toml.text >=> kindFrom)) t
          [x] -> Right x
          _ -> Left "a relation needs exactly one of `to`, `to_path` or `to_name`"
        rewrite <- maybe [] id <$> Toml.optional "rewrite" (Toml.list (Toml.list Toml.text >=> rewriteRule)) t
        pure (RelationDef kind source target rewrite)
    pair _ [a, b] = Right (a, b)
    pair k _ = Left ("`" <> k <> "` needs two templates")
    rewriteRule = \case
      [re, with] | Just (_ :: Regex) <- makeRegexM (T.unpack re) -> Right (re, with)
      [re, _] -> Left ("`rewrite`: `" <> re <> "` is not a valid regular expression")
      _ -> Left "`rewrite` takes pairs: [\"regex\", \"replacement\"]"

-- | The model of a project's files by its packs' maps: every element, sorted
-- by id (the first of a duplicate id stands), and every relation whose two
-- ends are elements.
buildModel :: Monad m => Queries m -> [FilePath] -> (FilePath -> m (Maybe Text)) -> [ModelMap] -> Text -> m ProjectModel
buildModel queries paths0 readFile' maps project = do
  let paths = sort paths0
  found <- forM [d | m <- maps, d <- mmElements m] $ \d ->
    runQuery queries paths readFile' (edQuery d) <&> \case
      Left f -> ([], ["elements of kind " <> kindText (edKind d) <> ": " <> failureWhy f])
      Right records -> let (bad, good) = partitionEithers [first (problem d) (elementOf d r) | r <- records, kept d r] in (good, bad)
  let withRecords = concatMap fst found
      byId = M.fromListWith (\_ old -> old) [(elId e, (e, r)) | (e, r) <- withRecords]
      elements = map fst (M.elems byId)
      resolve d r = case rdTo d of
        ToId t -> [qualified (rewrite d x) | Right x <- [render r t]]
        ToPath t -> [elId e | Right x <- [render r t], let p = T.unpack (rewrite d x), e <- elements, elPath e == p]
        ToName t ks -> [elId e | Right x <- [render r t], let n = rewrite d x, e <- elements, null ks || elKind e `elem` ks, elName e == n]
      related d froms = [Relation from (rdKind d) to | (from, r) <- froms, M.member from byId, to <- resolve d r, M.member to byId, to /= from]
  rels <- forM [d | m <- maps, d <- mmRelations m] $ \d -> case rdSource d of
    FromKind k -> pure (related d [(elId e, r) | (e, r) <- M.elems byId, elKind e == k], [])
    FromQuery q fromT ->
      runQuery queries paths readFile' q <&> \case
        Left f -> ([], [relKindText (rdKind d) <> " relations: " <> failureWhy f])
        Right records -> (related d [(qualified i, r) | r <- records, Right i <- [render r fromT]], [])
  pure
    ProjectModel
      { pmProject = project
      , pmElements = sortOn elId elements
      , pmRelations = S.toAscList (S.fromList (concatMap fst rels))
      , pmProblems = concatMap snd found ++ concatMap snd rels
      }
  where
    qualified i = project <> ":" <> i
    elementOf d r = do
      i <- render r (edId d)
      name <- maybe (Right (lastSegment i)) (render r) (edName d)
      pure (Element (qualified i) (edKind d) name (T.unpack (textOf "path" r)) (lineOf r), r)
    problem d why = "an element of kind " <> kindText (edKind d) <> ": " <> describeTemplateError why
    kept d r = case edDiffer d of
      Nothing -> True
      Just (a, b) -> render r a /= render r b
    lastSegment = snd . T.breakOnEnd "/"
    textOf k r = case M.lookup k r of
      Just (VText t) -> t
      _ -> ""
    lineOf r = case M.lookup "line" r of
      Just (VInt n) -> fromIntegral n
      _ -> 1
    rewrite d x = foldl (\acc (re, with) -> replaceFirst re with acc) x (rdRewrite d)
    replaceFirst re with t = case makeRegexM (T.unpack re) :: Maybe Regex of
      Just rx | matchTest rx (T.unpack t) -> let (before, _, after) = match rx (T.unpack t) :: (String, String, String) in T.pack before <> with <> T.pack after
      _ -> t
