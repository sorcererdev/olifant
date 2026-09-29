-- | The `olifant` command line: parses arguments, runs catalog/generation
-- use cases, and renders JSON or human output with the spec's exit codes.
module Olifant.Cli (Runtime (..), runCli) where

import Data.Aeson (Value, eitherDecodeStrict)
import Data.Aeson.Types (Pair, (.=))
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
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
  , rtUserConfigDir :: Maybe FilePath
  , rtHomeDir :: Maybe FilePath
  , rtReadStdin :: IO Text
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
    openProjectCatalog (rtRoot rt) (CatalogPaths (rtProjectDir rt) (rtUserConfigDir rt) (rtHomeDir rt)) >>= \case
      Left err -> catalogFailure rt fmt err
      Right cat -> runCommand rt fmt cat (optCommand opts)

runCommand :: Runtime -> Format -> Catalog -> Command -> IO ExitCode
runCommand rt fmt cat = \case
  ListCmd -> do
    let entries = catalogEntries cat
        broken = catalogBroken cat
    emit rt fmt (listJson entries broken) (listHuman entries broken)
    pure ExitSuccess
  ShowCmd ref -> case findEntry cat ref of
    Left err -> catalogFailure rt fmt err
    Right e -> emit rt fmt (showJson e) (showHuman e) >> pure ExitSuccess
  GenCmd g -> do
    specs <- case genStepsFile g of
      Nothing -> pure (Right (genSteps g))
      Just f -> do
        raw <- if f == "-" then rtReadStdin rt else either (const "") id . TE.decodeUtf8' <$> BS.readFile f
        pure (first (\e -> "--steps: " <> T.pack e) (eitherDecodeStrict (TE.encodeUtf8 raw)))
    case specs of
      Left msg -> rtErr rt (msg <> "\n") >> pure (ExitFailure 2)
      Right [] -> rtErr rt "no steps to run\n" >> pure (ExitFailure 2)
      Right ss -> do
        loaded <- traverse (\s -> either (pure . Left) (loadGenerator (rtRoot rt) cat) (findEntry cat (specGenerator s))) ss
        case sequence loaded of
          Left err -> catalogFailure rt fmt err
          Right gens -> do
            now <- maybe (rtNow rt) pure (genNow g)
            let mode = if genPlanOnly g then PlanOnly else Apply
                steps = zipWith (\gen s -> Step gen (M.fromList (specSets s))) gens ss
            generate (within (rtProjectDir rt) (rtRoot rt)) (resolver (rtRoot rt) cat) steps now mode >>= \case
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
  PackNotConfigured pack _ ->
    failWith rt fmt 3 "pack_not_configured" msg (Just (pack <> " = { path = \"/path/to/" <> pack <> "\" }")) ["pack" .= pack]
  GeneratorInvalid {} -> failWith rt fmt 1 "generator_invalid" msg Nothing []
  SourceInvalid {} -> failWith rt fmt 1 "template_invalid" msg Nothing []
  where
    msg = describeCatalogError err

generationFailure :: Runtime -> Format -> GenerationError -> IO ExitCode
generationFailure rt fmt = \case
  InvalidInputs path e@(InputProblems ps) -> failWith rt fmt 4 "invalid_inputs" (inStep path (describeEnvError e)) Nothing (inputProblemFields ps ++ ["step" .= showStepPath path])
  InvalidInputs path e -> failWith rt fmt 1 "generator_invalid" (inStep path (describeEnvError e)) Nothing []
  TemplateFailed path e -> failWith rt fmt 1 "template_failed" (inStep path (describeTemplateError e)) Nothing []
  UnknownGenerator path ref why -> failWith rt fmt 3 "generator_not_found" (inStep path ("`" <> ref <> "`: " <> why)) Nothing []
  CompositionCycle path -> failWith rt fmt 1 "composition_cycle" ("generators use each other in a cycle: " <> showStepPath path) Nothing []
  where
    inStep path msg = showStepPath path <> ": " <> msg
