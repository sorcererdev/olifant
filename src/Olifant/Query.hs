-- | Queries (spec §4.5): facts read out of the repo as a list of records,
-- for owned regions, `choices_from` and generators. Globs are evaluated over
-- a list of paths (pure); commands and AST queries go through the 'Queries' port.
module Olifant.Query
  ( Query (..)
  , Record
  , queryFromTree
  , describeQuery
  , globRecords
  , AstQuery (..)
  , AstSelect (..)
  , renderQuery
  , eachFields
  , posixRegex
  , Queries (..)
  , QueryFailure (..)
  , ChoiceValue (..)
  , choiceValue
  , queryVariables
  , failed
  , noQueries
  , processCommand
  , runQuery
  ) where

import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.List (inits, sortOn)
import Data.Set qualified as S
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Olifant.Glob (globPrefix, matchesGlob)
import Olifant.Process (runBytes)
import Olifant.Naming (parseName)
import Olifant.Language.Rule (Rule (..), renderRule, ruleFromTree)
import Olifant.Template (Env, Template, TemplateError, Value (..), freeVariables, parseTemplate, render)
import Text.Regex.TDFA (Regex, makeRegexM, match, matchTest)
import Olifant.Toml
import Olifant.Workspace (pruned)
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, isAbsolute, joinPath, makeRelative, splitDirectories, takeBaseName, takeDirectory, takeFileName, (</>))

data Query = GlobQuery Text [Text] | CommandQuery [Text] (Maybe Text) (Maybe Text) | AstQueryQ AstQuery
  deriving stock (Eq, Show)

-- | Nodes in code (spec §4.5): in the files a glob selects, the items of an
-- adapter target, or every node a raw ast-grep rule matches; `each` adds
-- fields (field name, POSIX regex over the node's text).
data AstQuery = AstQuery {astFiles :: Text, astSelect :: AstSelect, astEach :: [(Text, Text)]}
  deriving stock (Eq, Show)

data AstSelect = ItemsOf Text | RawRule Rule
  deriving stock (Eq, Show)

type Record = Map Text Value

queryFromTree :: Tree -> Either Text Query
queryFromTree tree = do
  t <- table tree
  case filter (`M.member` t) ["glob", "command", "files"] of
    ["files"] -> do
      onlyKeys ["files", "items_of", "ast", "each"] t
      files <- required "files" text t
      sel <- case (M.lookup "items_of" t, M.lookup "ast" t) of
        (Just v, Nothing) -> ItemsOf <$> inKey "items_of" (text v)
        (Nothing, Just v) -> RawRule <$> inKey "ast" (ruleFromTree v)
        _ -> Left "an AST query needs exactly one of `items_of` or `ast`"
      each <- maybe (Right []) (\tb -> traverse field (M.toList tb)) =<< optional "each" table t
      pure (AstQueryQ (AstQuery files sel each))
    ["glob"] -> do
      onlyKeys ["glob", "exclude"] t
      GlobQuery <$> required "glob" text t <*> (maybe [] id <$> optional "exclude" (list text) t)
    ["command"] -> do
      onlyKeys ["command", "select", "sort"] t
      argv <- required "command" (list text) t
      if null argv then Left "`command` needs at least the program" else pure ()
      CommandQuery argv <$> optional "select" text t <*> optional "sort" text t
    _ -> Left "a query needs exactly one of `glob`, `command` or `files` (an AST query)"
  where
    field (name, v) = inKey ("each." <> name) ((name,) <$> (text v >>= posixRegex))

-- | A POSIX extended regular expression, checked. Perl classes (@\\s@, @\\w@,
-- @\\d@, @\\b@…) are refused: POSIX reads them as literal letters, so they
-- would compile and silently match nothing.
posixRegex :: Text -> Either Text Text
posixRegex re
  | perlClass (T.unpack re) || invalid = Left ("`" <> re <> "` is not a valid regular expression (POSIX extended: use [[:space:]] and [a-z_0-9], not \\s or \\w)")
  | otherwise = Right re
  where
    invalid = null (makeRegexM (T.unpack re) :: Maybe Regex)
    perlClass = \case
      '\\' : '\\' : rest -> perlClass rest
      '\\' : c : rest -> c `elem` ("sSwWdDbB" :: String) || perlClass rest
      _ : rest -> perlClass rest
      [] -> False

describeQuery :: Query -> Text
describeQuery = \case
  AstQueryQ (AstQuery files sel _) -> "ast " <> files <> " (" <> (case sel of ItemsOf target -> "items of " <> target; RawRule _ -> "rule") <> ")"
  GlobQuery g _ -> "glob " <> g
  CommandQuery argv _ _ -> "command " <> T.unwords argv

-- | What queries never see: the directories no listing walks, plus Olifant's own.
ignoredDirs :: [FilePath]
ignoredDirs = pruned ++ [".olifant"]

globRecords :: Text -> [Text] -> [FilePath] -> [Record]
globRecords g excludes paths =
  [ record p
  | p <- S.toAscList (S.fromList (paths ++ concatMap ancestors paths))
  , not (any (`elem` ignoredDirs) (splitDirectories p))
  , matchesGlob g p
  , not (any (`matchesGlob` p) excludes)
  ]
  where
    -- Directories are matchable too (`*/*-service`), though only files are listed.
    ancestors = map joinPath . drop 1 . reverse . drop 1 . reverse . inits . splitDirectories
    prefix = globPrefix g
    record p =
      let rel = dropExtension (if null prefix then p else makeRelative prefix p)
          stem = T.pack (takeBaseName p)
       in M.fromList
            [ ("path", VText (T.pack p))
            , ("dir", VText (T.pack (let d = takeDirectory p in if d == "." then "" else d)))
            , ("stem", VText stem)
            , ("name", either (const (VText stem)) VName (parseName stem))
            , ("rel", VText (T.pack rel))
            , ("dotted", VText (T.intercalate "." (map T.pack (splitDirectories rel))))
            ]

-- | Which part of a record an enum's choice is (spec §4.1 `value`): words over
-- the record's `path`, a field, or a template.
data ChoiceValue = FileName | ParentFolder | PathValue | FieldValue Text | ValueTemplate Template
  deriving stock (Eq, Show)

-- | A choice from one record.
choiceValue :: ChoiceValue -> Record -> Either TemplateError Text
choiceValue cv r = case cv of
  FileName -> Right (T.pack (takeBaseName path))
  ParentFolder -> Right (T.pack (takeFileName (takeDirectory path)))
  PathValue -> Right (T.pack path)
  FieldValue f -> Right (maybe "" plain (M.lookup f r))
  ValueTemplate t -> render r t
  where
    path = maybe "" (T.unpack . plain) (M.lookup "path" r)
    plain = \case
      VText t -> t
      VInt i -> T.pack (show i)
      VBool b -> if b then "true" else "false"
      _ -> ""

-- | Every variable a query's templates use (spec §4.1: a query serving an
-- input may use only the inputs above it). Unparsable text contributes none;
-- rendering reports it.
queryVariables :: Query -> S.Set Text
queryVariables = \case
  GlobQuery g ex -> vars (g : ex)
  CommandQuery argv _ _ -> vars argv
  AstQueryQ (AstQuery files sel _) -> vars (files : [t | ItemsOf t <- [sel]])
  where
    vars ts = S.unions [either (const S.empty) freeVariables (parseTemplate t) | t <- ts]

-- | Why a query failed, and whether only a tool is missing (ast-grep, a
-- command's program): such failures are pending in pack tests (spec §4.5).
data QueryFailure = QueryFailure {failureWhy :: Text, failureToolMissing :: Bool}
  deriving stock (Eq, Show)

failed :: Text -> QueryFailure
failed why = QueryFailure why False

-- | The ports that answer queries: running a command (stdout, or why it
-- failed), and evaluating an AST query over a list of paths.
data Queries m = Queries
  { queryCommand :: [Text] -> m (Either QueryFailure ByteString)
  , -- | an AST query over some paths, reading files through the given view
    -- (a plan's contents first, then disk)
    queryAst :: AstQuery -> [FilePath] -> (FilePath -> m (Maybe Text)) -> m (Either QueryFailure [Record])
  }

noQueries :: Applicative m => Queries m
noQueries =
  Queries
    { queryCommand = \argv -> pure (Left (failed ("commands are not available here (" <> T.unwords argv <> ")")))
    , queryAst = \q _ _ -> pure (Left (failed ("AST queries are not available here (" <> describeQuery (AstQueryQ q) <> ")")))
    }

processCommand :: FilePath -> [Text] -> IO (Either QueryFailure ByteString)
processCommand dir = \case
  [] -> pure (Left (failed "empty command"))
  (exe : args) ->
    locateIn dir (T.unpack exe) >>= \case
      Nothing -> pure (Left (QueryFailure ("`" <> exe <> "` is not installed") True))
      Just path ->
        runBytes path (map T.unpack args) (Just dir) "" >>= \case
          Left why -> pure (Left (failed why))
          Right (ExitSuccess, out, _) -> pure (Right out)
          Right (ExitFailure c, _, err) -> pure (Left (failed ("exited " <> T.pack (show c) <> ": " <> T.strip (TE.decodeUtf8With lenientDecode err))))

-- | A program with a `/` (`bin/list`, `./scripts/x`) is the project's own,
-- resolved against the project directory; a bare name is looked up on PATH.
locateIn :: FilePath -> FilePath -> IO (Maybe FilePath)
locateIn dir exe
  | '/' `elem` exe = do
      let path = if isAbsolute exe then exe else dir </> exe
      ok <- doesFileExist path
      pure (if ok then Just path else Nothing)
  | otherwise = findExecutable exe

runQuery :: Monad m => Queries m -> [FilePath] -> (FilePath -> m (Maybe Text)) -> Query -> m (Either QueryFailure [Record])
runQuery queries paths readFile' q = case q of
  GlobQuery g excludes -> pure (Right (globRecords g excludes paths))
  AstQueryQ ast -> queryAst queries ast paths readFile'
  CommandQuery argv select sortKey ->
    queryCommand queries argv >>= \case
      Left f -> pure (Left f {failureWhy = describeQuery q <> " failed: " <> failureWhy f})
      Right out -> pure . first failed $ case Aeson.eitherDecodeStrict out of
        Left err -> Left (describeQuery q <> " did not print JSON: " <> T.pack err)
        Right v -> do
          arr <- case (select, v) of
            (Nothing, Aeson.Array xs) -> Right (toList xs)
            (Just k, Aeson.Object o) | Just (Aeson.Array xs) <- KM.lookup (Key.fromText k) o -> Right (toList xs)
            _ -> Left (describeQuery q <> ": expected a JSON array of objects" <> maybe "" (\k -> " under `" <> k <> "`") select)
          let records = mapMaybe toRecord arr
          Right (maybe records (\k -> sortOn (fmap asText . M.lookup k) records) sortKey)
  where
    toRecord = \case
      Aeson.Object o -> Just (M.fromList [(Key.toText k, scalar x) | (k, x) <- KM.toList o])
      _ -> Nothing
    scalar = \case
      Aeson.String s -> VText s
      Aeson.Bool b -> VBool b
      Aeson.Number n -> either (VText . T.pack . show) VInt (floatingOrInteger n :: Either Double Integer)
      _ -> VNull
    asText = \case
      VText t -> t
      VInt i -> T.pack (show i)
      other -> T.pack (show other)

-- | Queries' text is template text (a generator's `[query.NAME]` can use its
-- inputs and derived values): render it.
renderQuery :: Env -> Query -> Either TemplateError Query
renderQuery env = \case
  GlobQuery g excludes -> GlobQuery <$> tmpl g <*> traverse tmpl excludes
  CommandQuery argv sel sortKey -> CommandQuery <$> traverse tmpl argv <*> pure sel <*> pure sortKey
  AstQueryQ (AstQuery files sel each) -> do
    files' <- tmpl files
    sel' <- case sel of
      ItemsOf target -> ItemsOf <$> tmpl target
      RawRule rule -> RawRule . literal <$> renderRule env rule
    pure (AstQueryQ (AstQuery files' sel' each))
  where
    tmpl t = parseTemplate t >>= render env
    literal = \case
      Aeson.String x -> RLit x
      Aeson.Number n -> RInt (either round id (floatingOrInteger n :: Either Double Integer))
      Aeson.Bool b -> RBool b
      Aeson.Array xs -> RList (map literal (toList xs))
      Aeson.Object o -> RObject [(Key.toText k, literal v) | (k, v) <- KM.toList o]
      Aeson.Null -> RLit ""

-- | `each` fields for a node's text: the first capture group of each regex
-- (the whole match if it has none; empty text if it does not match).
eachFields :: [(Text, Text)] -> Text -> Record
eachFields each =
  -- Compiled once per query, not once per record.
  let compiled = [(name, makeRegexM (T.unpack re) :: Maybe Regex) | (name, re) <- each]
   in \nodeText ->
        let str = T.unpack nodeText
            capture = \case
              Nothing -> ""
              Just rx
                | not (matchTest rx str) -> ""
                | otherwise ->
                    let (_, whole, _, groups) = match rx str :: (String, String, String, [String])
                     in T.pack (case groups of (g : _) -> g; [] -> whole)
         in M.fromList [(name, VText (capture rx)) | (name, rx) <- compiled]
