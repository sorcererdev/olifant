module Olifant.Cli.Options
  ( Options (..)
  , Format (..)
  , Command (..)
  , GenOptions (..)
  , parseOptions
  ) where

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

data GenOptions = GenOptions
  { genTarget :: Text
  , genSets :: [(Text, Text)]
  , genPlanOnly :: Bool
  , genNow :: Maybe UTCTime
  }
  deriving stock (Eq, Show)

-- | Left carries the text to print (usage or help) and optparse's exit code.
parseOptions :: [String] -> Either (Text, ExitCode) Options
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
            <> cmd "gen" (GenCmd <$> genP) "Run a generator (applies by default; --plan previews)"
        )
    cmd name p desc = command name (info (Options <$> formatP <*> p) (progDesc desc))
    formatP = optional (flag' Json (long "json" <> help "JSON output") <|> flag' Human (long "human" <> help "Human-readable output"))
    genP =
      GenOptions
        <$> strArgument (metavar "GENERATOR")
        <*> many (option (eitherReader parseSet) (long "set" <> metavar "KEY=VALUE" <> help "Set an input or override a derived value"))
        <*> switch (long "plan" <> help "Show what would change without writing anything")
        <*> optional (option (eitherReader parseTime) (long "now" <> metavar "ISO8601" <> help "Pin the current time"))
    parseSet s = case break (== '=') s of
      (k, '=' : v) | not (null k) -> Right (T.pack k, T.pack v)
      _ -> Left "expected KEY=VALUE"
    parseTime s = maybe (Left "expected an ISO 8601 time like 2026-09-27T15:30:00Z") Right (iso8601ParseM s)
