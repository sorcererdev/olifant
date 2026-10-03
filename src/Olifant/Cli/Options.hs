module Olifant.Cli.Options
  ( Options (..)
  , Format (..)
  , Command (..)
  , GenOptions (..)
  , StepSpec (..)
  , SyncMode (..)
  , parseOptions
  ) where

import Control.Monad (unless)
import Data.Aeson (FromJSON (..), Value (..), withObject, (.:), (.:?))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as Aeson
import Data.Foldable (toList)
import Data.Scientific (floatingOrInteger)
import Data.Map.Strict qualified as M
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Options.Applicative
import System.Exit (ExitCode (..))

data Format = Json | Human
  deriving stock (Eq, Show)

data Options = Options {optFormat :: Maybe Format, optCommand :: Command}
  deriving stock (Eq, Show)

-- | 'ListCmd': --detail. 'AnchorsCmd': a language's `at` targets. 'WhereCmd': file, phrase.
data Command = ListCmd Bool | ShowCmd Text [(Text, Text)] | GenCmd GenOptions | AnchorsCmd Text | WhereCmd FilePath Text | SyncCmd SyncMode | SchemaCmd | NewCmd Text (Maybe Text) | ActionsCmd FilePath (Int, Int) Bool
  deriving stock (Eq, Show)

-- | `olifant sync` (spec §5.8): refresh regions, check them (write nothing), or create one's fences.
data SyncMode = SyncApply | SyncCheck | SyncInit Text
  deriving stock (Eq, Show)

-- | One step of a `gen` run: a generator and exactly its inputs (spec §7).
data StepSpec = StepSpec {specGenerator :: Text, specSets :: [(Text, Text)]}
  deriving stock (Eq, Show)

instance FromJSON StepSpec where
  parseJSON = withObject "step" $ \o -> do
    let unknown = filter (`notElem` ["gen", "set"]) (map Key.toText (KM.keys o))
    unless (null unknown) $
      fail ("unknown key(s) " <> show unknown <> "; each step is {\"gen\": GENERATOR, \"set\": {...}}: use \"gen\" and \"set\"")
    gen <- o .: "gen"
    sets <- maybe (pure []) (traverse (\(k, v) -> (k,) <$> asText v) . M.toList) =<< (o .:? "set" :: Aeson.Parser (Maybe (M.Map Text Value)))
    pure (StepSpec gen sets)
    where
      -- JSON values become the same text a --set would carry: numbers and
      -- booleans as written, arrays joined with "," (the CLI list form).
      asText = \case
        String t -> pure t
        Number n -> pure (T.pack (either (show :: Double -> String) (show :: Integer -> String) (floatingOrInteger n)))
        Bool b -> pure (if b then "true" else "false")
        Array xs -> T.intercalate "," <$> traverse scalar (toList xs)
        other -> fail ("expected text, a number, a boolean or a list, got " <> show other)
      scalar = \case
        Array _ -> fail "nested lists are not supported"
        Object _ -> fail "objects are not supported in a list"
        v -> asText v

data GenOptions = GenOptions
  { genSteps :: [StepSpec]
  , genStepsFile :: Maybe FilePath
  , genPlanOnly :: Bool
  , genNow :: Maybe UTCTime
  , genModel :: Maybe Text
  -- ^ `--model NAME`: which `[model.backends.NAME]` fills holes
  , genNoFill :: Bool
  -- ^ `--no-fill`: leave every hole as its placeholder
  }
  deriving stock (Eq, Show)

-- | Left carries the text to print (usage or help) and the exit code.
parseOptions :: [String] -> Either (Text, ExitCode) Options
parseOptions ("gen" : rest)
  | "--help" `notElem` rest && "-h" `notElem` rest = either (\e -> Left (e <> "\n", ExitFailure 2)) Right (parseGen rest)
-- A generator name where a command belongs (`olifant elm/page …`) is a
-- common slip for agents: point at `gen` instead of optparse's generic error.
parseOptions (ref : rest)
  | '/' `elem` ref && take 1 ref /= "-" =
      Left ("unknown command `" <> T.pack ref <> "`; did you mean `olifant gen " <> T.pack (unwords (ref : rest)) <> "`?\n", ExitFailure 2)
parseOptions args = case execParserPure defaultPrefs parserInfo args of
  Success o -> Right o
  Failure f -> let (msg, code) = renderFailure f "olifant" in Left (T.pack msg <> "\n", code)
  CompletionInvoked _ -> Left ("shell completion is not supported\n", ExitFailure 2)
  where
    parserInfo = info (commands <**> helper) (fullDesc <> progDesc "Olifant: Durandal's generator engine" <> footer "Generators live in packs (listed under [packs] in .olifant/config.toml) and in the project: .olifant/generators/NAME/generator.toml is found automatically as NAME. `olifant show GEN` says where a generator is defined.")
    commands =
      hsubparser
        ( cmd "list" (ListCmd <$> switch (long "detail" <> help "Include every generator's inputs, sub-generators and examples")) "List generators"
            <> cmd "show" (ShowCmd <$> strArgument (metavar "GENERATOR") <*> many (option (eitherReader (either (Left . T.unpack) Right . parseSet)) (long "set" <> metavar "K=V" <> help "Narrow dependent choices to these values"))) "Describe a generator's inputs and edits"
            <> cmd "anchors" (AnchorsCmd <$> strArgument (metavar "LANGUAGE")) "List the `at` targets a language supports"
            <> cmd "where" (WhereCmd <$> strArgument (metavar "FILE") <*> strArgument (metavar "PHRASE")) "Show where an `at` phrase would put text in a file"
            <> cmd "sync" (SyncCmd <$> syncP) "Refresh owned regions (--check: report drift and write nothing; --init REGION: create a region's fences)"
            <> cmd "new" (NewCmd <$> strArgument (metavar "NAME") <*> optional (strOption (long "language" <> metavar "LANGUAGE" <> help "The language adapter (needed when the project has more than one)"))) "Write a skeleton project generator at .olifant/generators/NAME/generator.toml"
            <> cmd "schema" (pure SchemaCmd) "Print the generator.toml format as a JSON Schema"
            <> cmd "actions" (actionsP) "The generators that apply at a position (for editors): --at FILE:LINE:COL [--content -]"
            <> cmd "gen" (GenCmd <$> genP) "Run generators: GEN [--set K=V]... [+ GEN [--set K=V]...]... (applies by default; --plan previews; --steps FILE|- reads the steps as JSON)"
        )
    cmd name p desc = command name (info (Options <$> formatP <*> p) (progDesc desc))
    formatP = optional (flag' Json (long "json" <> help "JSON output") <|> flag' Human (long "human" <> help "Human-readable output"))
    -- Used only to render `gen --help`; real `gen` parsing is 'parseGen'.
    genP =
      (\target sets plan now stepsFile model noFill -> GenOptions [StepSpec target sets] stepsFile plan now model noFill)
        <$> strArgument (metavar "GENERATOR")
        <*> many (option (eitherReader setP) (long "set" <> metavar "KEY=VALUE" <> help "Set an input or override a derived value"))
        <*> switch (long "plan" <> help "Show what would change without writing anything")
        <*> optional (option (eitherReader timeP) (long "now" <> metavar "ISO8601" <> help "Pin the current time"))
        <*> optional (strOption (long "steps" <> metavar "FILE|-" <> help "Read the steps as JSON: [{\"gen\": ..., \"set\": {...}}]"))
        <*> optional (strOption (long "model" <> metavar "NAME" <> help "Fill holes with [model.backends.NAME] (default: [model] default)"))
        <*> switch (long "no-fill" <> help "Leave every hole as its placeholder")
    syncP = flag' SyncCheck (long "check" <> help "Report regions that drifted or are broken; write nothing") <|> (SyncInit <$> strOption (long "init" <> metavar "REGION" <> help "Create a region's fences at its init_at, then fill it")) <|> pure SyncApply
    setP s = either (Left . T.unpack) Right (parseSet s)
    actionsP =
      (\(f, pos) content -> ActionsCmd f pos content)
        <$> option (eitherReader positionP) (long "at" <> metavar "FILE:LINE:COL" <> help "1-based line and column")
        <*> (maybe False (== ("-" :: String)) <$> optional (strOption (long "content" <> metavar "-" <> help "Read the file's current text from stdin")))
    -- FILE:LINE:COL, split from the right (a path may contain colons).
    positionP s = case reverse (splitOn ':' s) of
      (c : l : rest@(_ : _)) | [(col, "")] <- reads c, [(line, "")] <- reads l -> Right (intercalate ":" (reverse rest), (line, col))
      _ -> Left "expected FILE:LINE:COL (1-based)"
    splitOn sep xs = case break (== sep) xs of
      (a, _ : b) -> a : splitOn sep b
      (a, []) -> [a]
    timeP s = maybe (Left "expected an ISO 8601 time like 2026-09-27T15:30:00Z") Right (iso8601ParseM s)

-- | `gen` arguments: global flags anywhere; steps separated by a standalone "+".
parseGen :: [String] -> Either Text Options
parseGen args0 = do
  -- `--model NAME` and `--no-fill` apply to the whole run, like --plan.
  (model, noFill, args) <- pull Nothing False [] (concatMap splitEq args0)
  Options fmt cmd <- go Nothing False Nothing Nothing [] [] args
  pure $ case cmd of
    GenCmd o -> Options fmt (GenCmd o {genModel = model, genNoFill = noFill})
    other -> Options fmt other
  where
    pull model noFill acc = \case
      [] -> Right (model, noFill, reverse acc)
      "--model" : m : xs -> pull (Just (T.pack m)) noFill acc xs
      ["--model"] -> Left "--model needs a backend name"
      "--no-fill" : xs -> pull model True acc xs
      x : xs -> pull model noFill (x : acc) xs
    -- `--set=k=v` / `--now=T` / `--steps=F` are the same as the two-word forms.
    splitEq a = case break (== '=') a of
      (opt, '=' : v) | opt `elem` ["--set", "--now", "--steps", "--model"] -> [opt, v]
      _ -> [a]
    go fmt plan now stepsFile cur done = \case
      [] -> finish fmt plan now stepsFile (reverse (reverse cur : done))
      "--json" : xs -> go (Just Json) plan now stepsFile cur done xs
      "--human" : xs -> go (Just Human) plan now stepsFile cur done xs
      "--plan" : xs -> go fmt True now stepsFile cur done xs
      "--now" : t : xs -> case iso8601ParseM t of
        Just time -> go fmt plan (Just time) stepsFile cur done xs
        Nothing -> Left "--now: expected an ISO 8601 time like 2026-09-27T15:30:00Z"
      "--steps" : f : xs -> go fmt plan now (Just f) cur done xs
      "+" : xs -> go fmt plan now stepsFile [] (reverse cur : done) xs
      x : xs -> go fmt plan now stepsFile (x : cur) done xs
    finish fmt plan now stepsFile segments = do
      steps <- case stepsFile of
        Just _
          | all null segments -> Right []
          | otherwise -> Left "use either --steps or positional generator steps, not both"
        Nothing
          | all null segments -> Left "missing GENERATOR: olifant gen GENERATOR [--set KEY=VALUE]... [+ GENERATOR ...]"
          | any null segments -> Left "empty step: each `+` must be followed by a generator"
          | otherwise -> traverse step segments
      pure (Options fmt (GenCmd (GenOptions steps stepsFile plan now Nothing False)))
    step (g : xs) = StepSpec (T.pack g) <$> sets xs
    step [] = Left "empty step"
    sets = \case
      [] -> Right []
      "--set" : kv : xs -> (:) <$> parseSet kv <*> sets xs
      x : _ -> Left ("unexpected `" <> T.pack x <> "` (expected --set KEY=VALUE)")

parseSet :: String -> Either Text (Text, Text)
parseSet s = case break (== '=') s of
  (k, '=' : v) | not (null k) -> Right (T.pack k, T.pack v)
  _ -> Left "expected KEY=VALUE"
