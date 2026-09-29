-- | The `olifant` command line: parses arguments, runs catalog/generation
-- use cases, and renders JSON or human output with the spec's exit codes.
module Olifant.Cli (Runtime (..), runCli) where

import Data.Aeson (Value, eitherDecodeStrict)
import Data.Aeson.Types (Pair, (.=))
import Data.Bifunctor (first)
import Data.Map.Strict qualified as M
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Olifant.Anchor (RenderedAnchor (..), describeMisses, parsePhrase, resolveLayers)
import Olifant.Catalog
import Olifant.Cli.Options
import Olifant.Cli.Output
import Olifant.Generation
import Olifant.Generator (EnvError (..), GeneratorWith (..), UseStep (..), describeEnvError, generatorIdText)
import Olifant.Language (Checker, Language (..), Syntax, languageFor)
import Olifant.Template (describeTemplateError)
import Olifant.Workspace (Workspace (..), within)
import System.FilePath ((</>))
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
  , rtSyntax :: Syntax IO
  , rtChecker :: Checker IO
  }

runCli :: Runtime -> [String] -> IO ExitCode
runCli rt args = case parseOptions args of
  Left (text, ExitSuccess) -> rtOut rt text >> pure ExitSuccess
  Left (text, _)
    | wantsJson -> rtOut rt (encodeJson (errorJson "usage" (T.strip text) []) <> "\n") >> pure (ExitFailure 2)
    | otherwise -> rtErr rt text >> pure (ExitFailure 2)
  Right opts -> do
    let fmt = fromMaybe (if rtStdoutIsTty rt then Human else Json) (optFormat opts)
    openProjectCatalog (rtRoot rt) (CatalogPaths (rtProjectDir rt) (rtUserConfigDir rt) (rtHomeDir rt)) >>= \case
      Left err -> catalogFailure rt fmt err
      Right cat -> runCommand rt fmt cat (optCommand opts)
  where
    wantsJson = "--json" `elem` args || (not (rtStdoutIsTty rt) && "--human" `notElem` args)

runCommand :: Runtime -> Format -> Catalog -> Command -> IO ExitCode
runCommand rt fmt cat = \case
  ListCmd detail -> do
    let entries = catalogEntries cat
        broken = catalogBroken cat
    emit rt fmt (listJson detail (subsOf cat) entries broken) (listHuman detail (subsOf cat) entries broken)
    pure ExitSuccess
  ShowCmd ref -> case findEntry cat ref of
    Left err -> catalogFailure rt fmt err
    Right e -> emit rt fmt (showJson (subsOf cat e) e) (showHuman (subsOf cat e) e) >> pure ExitSuccess
  AnchorsCmd name -> case find ((== name) . langName) (catalogLanguages cat) of
    Nothing ->
      failWith rt fmt 3 "language_not_found" ("no language adapter named `" <> name <> "`") (Just "packs define languages in languages/<name>.toml") ["available" .= map langName (catalogLanguages cat)]
    Just lang -> emit rt fmt (anchorsJson lang) (anchorsHuman lang) >> pure ExitSuccess
  WhereCmd file phrase -> case parsePhrase phrase of
    Left err -> usageIn rt fmt err
    Right _ ->
      readText (rtRoot rt) (rtProjectDir rt </> file) >>= \case
        Nothing -> failWith rt fmt 3 "file_not_found" ("cannot read " <> T.pack file) Nothing []
        Just content ->
          resolveLayers (rtSyntax rt) (languageFor (catalogLanguages cat) file) [PhraseAnchor phrase] content >>= \case
            Left misses -> failWith rt fmt 5 "anchor_unresolved" (describeMisses misses) (Just "`olifant anchors <language>` lists the targets") []
            Right loc -> emit rt fmt (whereJson file phrase loc content) (whereHuman file loc content) >> pure ExitSuccess
  GenCmd g -> do
    let usage = usageIn rt fmt
    specs <- case genStepsFile g of
      Nothing -> pure (Right (Right (genSteps g)))
      Just f -> do
        raw <- if f == "-" then Just <$> rtReadStdin rt else readText (rtRoot rt) (rtProjectDir rt </> f)
        pure $ case raw of
          Nothing -> Left f
          Just text -> Right (first (\e -> "--steps: " <> T.pack e) (eitherDecodeStrict (TE.encodeUtf8 text)))
    case specs of
      Left f -> failWith rt fmt 3 "steps_not_found" ("--steps: cannot read " <> T.pack f <> " (missing, or not UTF-8 text)") Nothing []
      Right (Left msg) -> usage msg
      Right (Right []) -> usage "no steps to run"
      Right (Right ss) -> do
        let label i s = specGenerator s <> if length ss > 1 then " (step " <> T.pack (show i) <> ")" else ""
        loaded <- traverse (\(i, s) -> fmap (first (label i s,)) (either (pure . Left) (loadGenerator (rtRoot rt) cat) (findEntry cat (specGenerator s)))) (zip [1 :: Int ..] ss)
        case sequence loaded of
          Left (step, err) -> catalogFailureAt rt fmt (if length ss > 1 then Just step else Nothing) err
          Right gens -> do
            now <- maybe (rtNow rt) pure (genNow g)
            let mode = if genPlanOnly g then PlanOnly else Apply
                steps = zipWith (\gen s -> Step gen (M.fromList (specSets s))) gens ss
            generate (within (rtProjectDir rt) (rtRoot rt)) (resolver (rtRoot rt) cat) (Tools (rtSyntax rt) (rtChecker rt) (catalogLanguages cat)) steps now mode >>= \case
              Left err -> generationFailure rt fmt err
              Right plan -> do
                let applied = mode == Apply && not (hasConflicts plan)
                emit rt fmt (planJson applied plan) (planHuman applied plan)
                pure (if hasConflicts plan then ExitFailure 5 else ExitSuccess)

-- | A usage-level failure inside a command: JSON on stdout in JSON mode.
usageIn :: Runtime -> Format -> Text -> IO ExitCode
usageIn rt fmt msg = do
  case fmt of
    Json -> rtOut rt (encodeJson (errorJson "usage" msg []) <> "\n")
    Human -> rtErr rt (msg <> "\n")
  pure (ExitFailure 2)

-- | A generator's [[uses]] steps with the entries they resolve to, so `show`
-- can describe a composite completely in one call.
subsOf :: Catalog -> CatalogEntry -> [(UseStep, Maybe CatalogEntry)]
subsOf cat e = [(u, either (const Nothing) Just (resolveEntry cat (packOfEntry e) (useGenerator u))) | u <- genUses (entryDefinition e)]

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
catalogFailure rt fmt = catalogFailureAt rt fmt Nothing

-- | Like 'catalogFailure', naming the chain step it happened in (spec §7).
catalogFailureAt :: Runtime -> Format -> Maybe Text -> CatalogError -> IO ExitCode
catalogFailureAt rt fmt step err = case err of
  GeneratorNotFound _ available ->
    failWith' 3 "generator_not_found" msg (Just "run `olifant list` to see generators") ["available" .= map generatorIdText available]
  GeneratorAmbiguous {} -> failWith' 2 "generator_ambiguous" msg (Just "use the full pack/name") []
  SourceMissing {} -> failWith' 3 "template_not_found" msg Nothing []
  ConfigInvalid {} -> failWith' 1 "config_invalid" msg Nothing []
  PackNotConfigured pack _ ->
    failWith' 3 "pack_not_configured" msg (Just (pack <> " = { path = \"/path/to/" <> pack <> "\" }")) ["pack" .= pack]
  GeneratorInvalid {} -> failWith' 1 "generator_invalid" msg Nothing []
  SourceInvalid {} -> failWith' 1 "template_invalid" msg Nothing []
  where
    msg = maybe "" (<> ": ") step <> describeCatalogError err
    failWith' code c m h extra = failWith rt fmt code c m h (extra ++ maybe [] (\st -> ["step" .= st]) step)

generationFailure :: Runtime -> Format -> GenerationError -> IO ExitCode
generationFailure rt fmt = \case
  InvalidInputs path e@(InputProblems ps) -> failWith rt fmt 4 "invalid_inputs" (inStep path (describeEnvError e)) Nothing (inputProblemFields ps ++ ["step" .= showStepPath path])
  InvalidInputs path e -> failWith rt fmt 1 "generator_invalid" (inStep path (describeEnvError e)) Nothing []
  TemplateFailed path e -> failWith rt fmt 1 "template_failed" (inStep path (describeTemplateError e)) Nothing []
  UnknownGenerator path ref why -> failWith rt fmt 3 "generator_not_found" (inStep path ("`" <> ref <> "`: " <> why)) Nothing []
  InvalidGenerator path ref why -> failWith rt fmt 1 "generator_invalid" (inStep path ("`" <> ref <> "`: " <> why)) Nothing []
  CompositionCycle path -> failWith rt fmt 1 "composition_cycle" ("generators use each other in a cycle: " <> showStepPath path) Nothing []
  where
    inStep path msg = showStepPath path <> ": " <> msg
