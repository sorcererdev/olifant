-- | The tools a generation may use: the syntax and check ports, the language
-- adapters and owned regions from the catalog, and the command runner for
-- queries (commands, AST). `noTools` needs nothing installed.
module Olifant.Generation.Tools (Tools (..), ProgressEvent (..), Verifier (..), noVerifier, Runner (..), noRunner, noTools, basicTools) where

import Data.Text (Text)

import Olifant.Fill (ModelFiller)
import Olifant.Language
import Olifant.Query (Queries, noQueries)
import Olifant.Region (RegionDef)

data Tools m = Tools
  { toolSyntax :: Syntax m
  , toolChecker :: Checker m
  , toolLanguages :: [Language]
  , toolRegions :: [RegionDef]
  , toolQueries :: Queries m
  , -- | the model that fills holes, if any (spec §6.4)
    toolFiller :: Maybe (ModelFiller m)
  , -- | where progress goes while checking and filling (editors show it)
    toolProgress :: ProgressEvent -> m ()
  , -- | related files shown to the model with the holes (a rule fix's context)
    toolFillContext :: [(FilePath, Text)]
  , -- | runs a hole's `verify` command over the files as the plan has them
    toolVerifier :: Verifier m
  , -- | runs a hole's `verify = "tests"`: the language's test runner
    toolTester :: Runner m
  , -- | whether the plan was cancelled (the language server's superseded
    -- work): the fill phase stops between rounds
    toolCancelled :: m Bool
  }


-- | What a long-running plan is doing: a language's check; a fill request
-- (holes, attempt, model); a hole filled (attempt) or given up on (why).
data ProgressEvent = Checking Text | Filling [Text] Int Text | Filled Text Int | Unfilled Text Text | AutoImported Text Text | Verifying Text
  deriving stock (Eq, Show)

noTools :: Applicative m => Tools m
noTools = basicTools noSyntax noChecker []

-- | Syntax, checker and languages, with no regions and no commands.
basicTools :: Applicative m => Syntax m -> Checker m -> [Language] -> Tools m
basicTools syntax checker langs = Tools syntax checker langs [] noQueries Nothing (const (pure ())) [] noVerifier noRunner (pure False)
