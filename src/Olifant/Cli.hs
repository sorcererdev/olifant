-- | The `olifant` command line: parses arguments, runs catalog/generation
-- use cases, and renders JSON or human output with the spec's exit codes.
module Olifant.Cli (Runtime (..), runCli) where

import Data.Aeson (Value)
import Data.Aeson.Types (Pair, (.=))
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)
import Olifant.Catalog
import Olifant.Cli.Options
import Olifant.Cli.Output
import Olifant.Generation
import Olifant.Generator (EnvError (..), generatorIdText, describeEnvError)
import Olifant.Template (describeTemplateError)
import Olifant.Workspace (Workspace, within)
import System.Exit (ExitCode (..))

data Runtime = Runtime
  { rtRoot :: Workspace IO
  , rtProjectDir :: FilePath
  , rtNow :: IO UTCTime
  , rtStdoutIsTty :: Bool
  , rtOut :: Text -> IO ()
  , rtErr :: Text -> IO ()
  }

runCli :: Runtime -> [String] -> IO ExitCode
runCli rt args = case parseOptions args of
  Left (text, ExitSuccess) -> rtOut rt text >> pure ExitSuccess
  Left (text, _) -> rtErr rt text >> pure (ExitFailure 2)
  Right opts -> do
    let fmt = fromMaybe (if rtStdoutIsTty rt then Human else Json) (optFormat opts)
    openProjectCatalog (rtRoot rt) (rtProjectDir rt) >>= \case
      Left err -> catalogFailure rt fmt err
      Right cat -> runCommand rt fmt cat (optCommand opts)

runCommand :: Runtime -> Format -> Catalog -> Command -> IO ExitCode
runCommand rt fmt cat = \case
  ListCmd -> do
    let entries = catalogEntries cat
    emit rt fmt (listJson entries) (listHuman entries)
    pure ExitSuccess
  ShowCmd ref -> case findEntry cat ref of
    Left err -> catalogFailure rt fmt err
    Right e -> emit rt fmt (showJson e) (showHuman e) >> pure ExitSuccess
  GenCmd g -> case findEntry cat (genTarget g) of
    Left err -> catalogFailure rt fmt err
    Right entry ->
      loadGenerator (rtRoot rt) cat entry >>= \case
        Left err -> catalogFailure rt fmt err
        Right gen -> do
          now <- maybe (rtNow rt) pure (genNow g)
          let mode = if genPlanOnly g then PlanOnly else Apply
          generate (within (rtProjectDir rt) (rtRoot rt)) gen (M.fromList (genSets g)) now mode >>= \case
            Left err -> generationFailure rt fmt err
            Right plan -> do
              let applied = mode == Apply && not (hasConflicts plan)
              emit rt fmt (planJson applied plan) (planHuman applied plan)
              pure (if hasConflicts plan then ExitFailure 5 else ExitSuccess)

emit :: Runtime -> Format -> Value -> Text -> IO ()
emit rt fmt json human = rtOut rt $ case fmt of
  Json -> encodeJson json <> "\n"
  Human -> human

failWith :: Runtime -> Format -> Int -> Text -> Text -> Maybe Text -> [Pair] -> IO ExitCode
failWith rt fmt code errCode message hint extra = do
  case fmt of
    Json -> rtOut rt (encodeJson (errorJson errCode message (maybe [] (\h -> ["hint" .= h]) hint ++ extra)) <> "\n")
    Human -> rtErr rt (errorHuman message hint)
  pure (ExitFailure code)

catalogFailure :: Runtime -> Format -> CatalogError -> IO ExitCode
catalogFailure rt fmt err = case err of
  GeneratorNotFound _ available ->
    failWith rt fmt 3 "generator_not_found" msg (Just "run `olifant list` to see generators") ["available" .= map generatorIdText available]
  GeneratorAmbiguous {} -> failWith rt fmt 2 "generator_ambiguous" msg (Just "use the full pack/name") []
  SourceMissing {} -> failWith rt fmt 3 "template_not_found" msg Nothing []
  ConfigInvalid {} -> failWith rt fmt 1 "config_invalid" msg Nothing []
  GeneratorInvalid {} -> failWith rt fmt 1 "generator_invalid" msg Nothing []
  SourceInvalid {} -> failWith rt fmt 1 "template_invalid" msg Nothing []
  where
    msg = describeCatalogError err

generationFailure :: Runtime -> Format -> GenerationError -> IO ExitCode
generationFailure rt fmt = \case
  InvalidInputs e@(InputProblems ps) -> failWith rt fmt 4 "invalid_inputs" (describeEnvError e) Nothing (inputProblemFields ps)
  InvalidInputs e -> failWith rt fmt 1 "generator_invalid" (describeEnvError e) Nothing []
  TemplateFailed e -> failWith rt fmt 1 "template_failed" (describeTemplateError e) Nothing []
