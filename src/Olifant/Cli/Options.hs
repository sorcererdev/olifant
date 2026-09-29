module Olifant.Cli.Options
  ( Options (..)
  , Format (..)
  , Command (..)
  , GenOptions (..)
  , StepSpec (..)
  , parseOptions
  ) where

import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.Map.Strict qualified as M
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

data Command = ListCmd | ShowCmd Text | GenCmd GenOptions
  deriving stock (Eq, Show)

-- | One step of a `gen` run: a generator and exactly its inputs (spec §7).
data StepSpec = StepSpec {specGenerator :: Text, specSets :: [(Text, Text)]}
  deriving stock (Eq, Show)

instance FromJSON StepSpec where
  parseJSON = withObject "step" $ \o -> StepSpec <$> o .: "gen" <*> (maybe [] M.toList <$> o .:? "set")

data GenOptions = GenOptions
  { genSteps :: [StepSpec]
  , genStepsFile :: Maybe FilePath
  , genPlanOnly :: Bool
  , genNow :: Maybe UTCTime
  }
  deriving stock (Eq, Show)

-- | Left carries the text to print (usage or help) and the exit code.
parseOptions :: [String] -> Either (Text, ExitCode) Options
parseOptions ("gen" : rest)
  | "--help" `notElem` rest && "-h" `notElem` rest = either (\e -> Left (e <> "\n", ExitFailure 2)) Right (parseGen rest)
parseOptions args = case execParserPure defaultPrefs parserInfo args of
  Success o -> Right o
  Failure f -> let (msg, code) = renderFailure f "olifant" in Left (T.pack msg <> "\n", code)
  CompletionInvoked _ -> Left ("shell completion is not supported\n", ExitFailure 2)
  where
    parserInfo = info (commands <**> helper) (fullDesc <> progDesc "Olifant: Durandal's generator engine")
    commands =
      hsubparser
        ( cmd "list" (pure ListCmd) "List generators"
            <> cmd "show" (ShowCmd <$> strArgument (metavar "GENERATOR")) "Describe a generator's inputs and edits"
            <> cmd "gen" (GenCmd <$> genP) "Run generators: GEN [--set K=V]... [+ GEN [--set K=V]...]... (applies by default; --plan previews; --steps FILE|- reads the steps as JSON)"
        )
    cmd name p desc = command name (info (Options <$> formatP <*> p) (progDesc desc))
    formatP = optional (flag' Json (long "json" <> help "JSON output") <|> flag' Human (long "human" <> help "Human-readable output"))
    -- Used only to render `gen --help`; real `gen` parsing is 'parseGen'.
    genP =
      (\target sets plan now stepsFile -> GenOptions [StepSpec target sets] stepsFile plan now)
        <$> strArgument (metavar "GENERATOR")
        <*> many (option (eitherReader setP) (long "set" <> metavar "KEY=VALUE" <> help "Set an input or override a derived value"))
        <*> switch (long "plan" <> help "Show what would change without writing anything")
        <*> optional (option (eitherReader timeP) (long "now" <> metavar "ISO8601" <> help "Pin the current time"))
        <*> optional (strOption (long "steps" <> metavar "FILE|-" <> help "Read the steps as JSON: [{\"gen\": ..., \"set\": {...}}]"))
    setP s = either (Left . T.unpack) Right (parseSet s)
    timeP s = maybe (Left "expected an ISO 8601 time like 2026-09-27T15:30:00Z") Right (iso8601ParseM s)

-- | `gen` arguments: global flags anywhere; steps separated by a standalone "+".
parseGen :: [String] -> Either Text Options
parseGen = go Nothing False Nothing Nothing [] []
  where
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
          | any null segments -> Left "empty step: each `+` must be followed by a generator"
          | otherwise -> traverse step segments
      pure (Options fmt (GenCmd (GenOptions steps stepsFile plan now)))
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
