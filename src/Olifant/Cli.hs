-- | The `olifant` command line: parses arguments, runs catalog/generation
-- use cases, and renders JSON or human output with the spec's exit codes.
module Olifant.Cli (Runtime (..), runCli) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Aeson (Value, eitherDecodeStrict, object)
import Data.Aeson.Types (Pair, (.=))
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Olifant.Anchor (Located (..), RenderedAnchor (..), astQueries, describeMisses, insertLines, parsePhrase, resolveLayers)
import Olifant.Region (RegionDef, RegionDefWith (..), RegionProblem (..), applyRegion, fenceLines, wholeHeader)
import Olifant.Catalog
import Olifant.Cli.Options
import Olifant.Cli.Output
import Olifant.Generator.Schema (generatorSchema)
import Olifant.Generation
import Olifant.Generator (EnvError (..), GeneratorId (..), GeneratorWith (..), UseStep (..), generatorIdText, needsChoices, withChoices)
import Olifant.Query (Queries (..), QueryFailure (..), processCommand)
import Olifant.Language (Checker, Language (..), Syntax, languageFor)
import Olifant.Workspace (PathState (..), Workspace (..), isSafeRelative, within)
import System.FilePath (takeBaseName, takeDirectory, (</>))
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
        cwd = rtProjectDir rt
    root <- projectRoot (rtRoot rt) (rtHomeDir rt) cwd
    let rt' = rt {rtProjectDir = root}
    openProjectCatalog (rtRoot rt') (CatalogPaths root (rtUserConfigDir rt') (rtHomeDir rt')) >>= \case
      Left err -> catalogFailure rt' fmt err
      Right cat -> runCommand rt' cwd fmt cat (optCommand opts)
  where
    wantsJson = "--json" `elem` args || (not (rtStdoutIsTty rt) && "--human" `notElem` args)

-- | The project is the nearest directory, from where Olifant runs upwards,
-- that has a `.olifant` directory; with none, where it runs. The search
-- stops below the home directory (user config is ~/.config/olifant).
projectRoot :: Monad m => Workspace m -> Maybe FilePath -> FilePath -> m FilePath
projectRoot ws home start = go start
  where
    go dir
      | Just dir == home = pure start
      | otherwise =
          inspectPath ws (dir </> ".olifant") >>= \case
            Directory -> pure dir
            _ | takeDirectory dir == dir -> pure start
              | otherwise -> go (takeDirectory dir)

-- | Where to look when a generator.toml does not load.
formatHint :: Text
formatHint = "`olifant schema` prints the generator.toml format; `olifant new NAME` writes a skeleton that loads"

-- | A generator that loads as written: one ident input, a derived PascalCase
-- name, and the three kinds of edit as commented examples to adapt.
skeletonGenerator :: Text -> Text -> Text
skeletonGenerator name lang =
  T.unlines
    [ "# Run with: olifant gen " <> name <> " --set name=example   (--plan to preview)"
    , "# Format: `olifant schema`. Phrases for `at`: `olifant anchors " <> lang <> "`; test one: `olifant where FILE \"PHRASE\"`."
    , "description = \"One line saying what this adds\""
    , "language = \"" <> lang <> "\""
    , "version = 1"
    , "examples = [\"olifant gen " <> name <> " --set name=example\"]"
    , ""
    , "[inputs.name]"
    , "type = \"ident\""
    , "prompt = \"The name, snake_case (e.g. contact_us)\""
    , ""
    , "[derive]"
    , "P = \"{= name|pascal =}\""
    , ""
    , "# Each edit is one [[edits]] table (double brackets). Uncomment and adapt."
    , "# Template values: {= name =}, {= P =}; filters after a bar: {= name|kebab =}."
    , "#"
    , "# [[edits]]                          # a new file"
    , "# create = \"src/{= P =}.txt\""
    , "# text   = \"{= P =}\\n\""
    , "#"
    , "# [[edits]]                          # text into an existing file"
    , "# insert = \"src/Main.txt\""
    , "# at     = \"end of type Route\""
    , "# text   = \"| {= P =}\""
    , "#"
    , "# [[edits]]                          # case branches the compiler asks for"
    , "# complete = \"Route.{= P =}\""
    , "# where    = \"case in changeRouteTo\""
    , "# text     = \"\"\""
    , "# {= pattern =} ->"
    , "#     ( model, Cmd.none )\"\"\""
    ]

-- | @cwd@ is where Olifant was run: file arguments (`where FILE`, `--steps
-- FILE`) are relative to it, while generators work in the project root.
runCommand :: Runtime -> FilePath -> Format -> Catalog -> Command -> IO ExitCode
runCommand rt cwd fmt cat = \case
  ListCmd detail -> do
    -- Sources and fixed targets are checked here, so a broken generator shows
    -- as INVALID before anyone runs it.
    checked <- traverse (\e -> (e,) <$> checkEntry (rtRoot rt) (rtSyntax rt) cat e) (catalogEntries cat)
    let broken =
          catalogBroken cat
            ++ [(entryId e, entryDir e </> "generator.toml", why) | (e, Just why) <- checked]
            ++ [(GeneratorId ("language " <> T.pack (takeBaseName path)), path, why) | (path, why) <- catalogBrokenLanguages cat]
        ok = [e | (e, Nothing) <- checked]
    -- --detail prints inputs, so choices_from must be live, as in `show`.
    entries <-
      if detail
        then traverse (\e -> either (const e) (\d -> e {entryDefinition = d}) <$> resolveChoicesFor rt cat M.empty (entryDefinition e)) ok
        else pure ok
    emit rt fmt (listJson detail (subsOf cat) entries broken) (listHuman detail (subsOf cat) entries broken)
    pure ExitSuccess
  SchemaCmd -> do
    emit rt fmt generatorSchema (encodeJson generatorSchema <> "\n")
    pure ExitSuccess
  NewCmd name langFlag -> do
    let path = ".olifant" </> "generators" </> T.unpack name </> "generator.toml"
        ws = within (rtProjectDir rt) (rtRoot rt)
        language = case (langFlag, map langName (catalogLanguages cat)) of
          (Just l, _) -> Right l
          (Nothing, [l]) -> Right l
          (Nothing, ls) -> Left ("pass --language (" <> (if null ls then "no language adapter is configured" else "adapters: " <> T.intercalate ", " ls) <> ")")
    exists <- fileExists ws path
    case language of
      Left why -> failWith rt fmt 2 "usage" why Nothing []
      Right _ | exists -> failWith rt fmt 1 "already_exists" (T.pack path <> " already exists") (Just ("edit it, or pick another name; `olifant show " <> name <> "` shows what it does")) []
      Right lang -> do
        writeText ws path (skeletonGenerator name lang)
        emit rt fmt (object ["created" .= path, "next" .= nextSteps lang]) ("created " <> T.pack path <> "\n" <> nextSteps lang <> "\n")
        pure ExitSuccess
    where
      nextSteps lang = "fill in description and [[edits]]; `olifant anchors " <> lang <> "` lists placement phrases, `olifant where FILE \"PHRASE\"` tests one, `olifant gen " <> name <> " --set name=example --plan` previews"
  ShowCmd ref -> case findEntry cat ref of
    Left err -> catalogFailure rt fmt err
    Right e0 ->
      checkEntry (rtRoot rt) (rtSyntax rt) cat e0 >>= \case
       Just why -> failWith rt fmt 1 "generator_invalid" (generatorIdText (entryId e0) <> ": " <> why) Nothing []
       Nothing -> resolveChoicesFor rt cat M.empty (entryDefinition e0) >>= \case
        Left why -> failWith rt fmt 1 "generator_invalid" why Nothing []
        Right d -> let e = e0 {entryDefinition = d} in emit rt fmt (showJson (subsOf cat e) e) (showHuman (subsOf cat e) e) >> pure ExitSuccess
  AnchorsCmd name -> case find ((== name) . langName) (catalogLanguages cat) of
    Nothing ->
      failWith rt fmt 3 "language_not_found" ("no language adapter named `" <> name <> "`" <> T.concat ["; " <> T.pack path <> " failed to load: " <> why | (path, why) <- catalogBrokenLanguages cat, takeBaseName path == T.unpack name]) (Just "packs define languages in languages/<name>.toml") ["available" .= map langName (catalogLanguages cat)]
    Just lang -> emit rt fmt (anchorsJson lang) (anchorsHuman lang) >> pure ExitSuccess
  WhereCmd file phrase -> case parsePhrase phrase of
    Left err -> usageIn rt fmt err
    Right _ ->
      readText (rtRoot rt) (cwd </> file) >>= \case
        Nothing -> failWith rt fmt 3 "file_not_found" ("cannot read " <> T.pack file) Nothing []
        Just content ->
          resolveLayers (rtSyntax rt) (languageFor (catalogLanguages cat) file) [PhraseAnchor phrase] content >>= \case
            Left misses -> failWith rt fmt 5 "anchor_unresolved" (describeMisses misses) (Just "`olifant anchors <language>` lists the targets") []
            Right loc -> emit rt fmt (whereJson file phrase loc content) (whereHuman file loc content) >> pure ExitSuccess
  SyncCmd mode -> do
    now <- rtNow rt
    let ws = within (rtProjectDir rt) (rtRoot rt)
        tools = projectTools rt cat
    case mode of
      SyncApply -> syncRegions rt fmt (catalogBrokenRegions cat) ws tools SkipUnset False now
      SyncCheck -> syncRegions rt fmt (catalogBrokenRegions cat) ws tools ReportUnset True now
      SyncInit name -> case find ((== name) . regionName) (catalogRegions cat) of
        Nothing -> failWith rt fmt 3 "region_not_found" ("no region named `" <> name <> "`") (Just "regions are declared in regions.toml") ["available" .= map regionName (catalogRegions cat)]
        Just def -> initRegion rt fmt ws tools def now
  GenCmd g -> do
    let usage = usageIn rt fmt
    specs <- case genStepsFile g of
      Nothing -> pure (Right (Right (genSteps g)))
      Just f -> do
        raw <- if f == "-" then Just <$> rtReadStdin rt else readText (rtRoot rt) (cwd </> f)
        pure $ case raw of
          Nothing -> Left f
          Just text -> Right (first (\e -> "--steps: " <> T.pack e) (eitherDecodeStrict (TE.encodeUtf8 text)))
    case specs of
      Left f -> failWith rt fmt 3 "steps_not_found" ("--steps: cannot read " <> T.pack f <> " (missing, or not UTF-8 text)") Nothing []
      Right (Left msg) -> usage msg
      Right (Right []) -> usage "no steps to run"
      Right (Right ss) -> do
        let label i s = specGenerator s <> if length ss > 1 then " (step " <> T.pack (show i) <> ")" else ""
        loaded <- traverse (\(i, s) -> fmap (first (label i s,)) (either (pure . Left) (loadGenerator (rtRoot rt) (rtSyntax rt) cat) (findEntry cat (specGenerator s)))) (zip [1 :: Int ..] ss)
        case sequence loaded of
          Left (step, err) -> catalogFailureAt rt fmt (if length ss > 1 then Just step else Nothing) err
          -- choices_from is resolved inside generate, step by step, so a
          -- step can choose what an earlier step of the chain creates.
          Right gens -> do
            now <- maybe (rtNow rt) pure (genNow g)
            let mode = if genPlanOnly g then PlanOnly else Apply
                steps = zipWith (\gen s -> Step gen (M.fromList (specSets s))) gens ss
            generate (within (rtProjectDir rt) (rtRoot rt)) (resolver (rtRoot rt) (rtSyntax rt) cat) (projectTools rt cat) steps now mode >>= \case
              Left err -> generationFailure rt fmt err
              Right plan0 -> do
                let plan =
                      plan0
                        { planWarnings =
                            planWarnings plan0
                              ++ ["regions are not refreshed from " <> T.pack path <> ": " <> err | (path, err) <- catalogBrokenRegions cat]
                              ++ ["language adapter " <> T.pack path <> " failed to load: " <> err | (path, err) <- catalogBrokenLanguages cat]
                        }
                    applied = mode == Apply && not (hasConflicts plan)
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
  PackMissing {} -> failWith' 1 "config_invalid" msg Nothing []
  PackNotConfigured pack _ ->
    failWith' 3 "pack_not_configured" msg (Just (pack <> " = { path = \"/path/to/" <> pack <> "\" }")) ["pack" .= pack]
  GeneratorInvalid {} -> failWith' 1 "generator_invalid" msg (Just formatHint) []
  SourceInvalid {} -> failWith' 1 "template_invalid" msg Nothing []
  where
    msg = maybe "" (<> ": ") step <> describeCatalogError err
    failWith' code c m h extra = failWith rt fmt code c m h (extra ++ maybe [] (\st -> ["step" .= st]) step)

generationFailure :: Runtime -> Format -> GenerationError -> IO ExitCode
generationFailure rt fmt err = case err of
  InvalidInputs path (InputProblems ps) -> failWith rt fmt 4 "invalid_inputs" msg Nothing (inputProblemFields ps ++ ["step" .= showStepPath path])
  InvalidInputs {} -> failWith rt fmt 1 "generator_invalid" msg Nothing []
  TemplateFailed {} -> failWith rt fmt 1 "template_failed" msg Nothing []
  UnknownGenerator {} -> failWith rt fmt 3 "generator_not_found" msg Nothing []
  InvalidGenerator {} -> failWith rt fmt 1 "generator_invalid" msg Nothing []
  QueryFailed {} -> failWith rt fmt 1 "query_failed" msg Nothing []
  CompositionCycle {} -> failWith rt fmt 1 "composition_cycle" msg Nothing []
  where
    msg = describeGenerationError err

-- | Resolve `choices_from` inputs against the project (spec §4.1); the
-- project is listed only when a generator needs it.
resolveChoicesFor :: Runtime -> Catalog -> Map Text Text -> GeneratorWith s -> IO (Either Text (GeneratorWith s))
resolveChoicesFor rt cat sets gen
  | not (needsChoices gen) = pure (Right gen)
  | otherwise = do
      paths <- listFiles (within (rtProjectDir rt) (rtRoot rt)) "."
      first failureWhy <$> withChoices (projectQueries rt cat) paths (readText (within (rtProjectDir rt) (rtRoot rt))) sets gen

-- | Refresh every region (spec §5.8). With @checkOnly@, write nothing and
-- fail (exit 5) if any region drifted, is broken, or is not set up.
syncRegions :: Runtime -> Format -> [(FilePath, Text)] -> Workspace IO -> Tools IO -> RegionMode -> Bool -> UTCTime -> IO ExitCode
syncRegions rt fmt rtBrokenRegions ws tools regionMode checkOnly now = do
  (edits, conflicts0, current) <- refreshRegions regionMode tools ws 1 M.empty
  let conflicts = [RegionBroken "" path ("cannot read " <> T.pack path <> ": " <> err) | (path, err) <- rtBrokenRegions] ++ conflicts0
  let writes = M.fromList [(p, c) | (p, Just c) <- M.toList current]
      plan = Plan (GeneratorId "sync") [] now edits conflicts [] writes
      applied = not checkOnly && not (hasConflicts plan)
  when applied $ mapM_ (uncurry (writeText ws)) (M.toList writes)
  emit rt fmt (planJson applied plan) (if checkOnly then syncCheckHuman plan else planHuman applied plan)
  pure (if hasConflicts plan || (checkOnly && not (null edits)) then ExitFailure 5 else ExitSuccess)

-- | `sync --init NAME`: put a region's empty fences at its `init_at`, then fill it.
initRegion :: Runtime -> Format -> Workspace IO -> Tools IO -> RegionDef -> UTCTime -> IO ExitCode
initRegion rt fmt ws tools def now
  | not (isSafeRelative file) = failWith rt fmt 5 "region_broken" ("region `" <> regionName def <> "`: " <> T.pack file <> " is outside the project") Nothing []
  | otherwise = case regionComment def <|> (langComment <$> lang) of
      Nothing -> failWith rt fmt 1 "region_broken" ("no comment syntax for " <> T.pack file <> " (add `comment` to the region)") Nothing []
      Just c ->
        readText ws file >>= \case
          -- A whole-file region is set up by creating its file with the header.
          Nothing
            | regionWholeFile def -> writeText ws file (wholeHeader def c <> "\n") >> fill
            | otherwise -> failWith rt fmt 3 "file_not_found" ("cannot read " <> T.pack file) Nothing []
          Just content -> case applyRegion def c [] content of
            Right _ -> emit rt fmt (object ["region" .= regionName def, "status" .= ("already set up" :: Text)]) ("region " <> regionName def <> " is already set up in " <> T.pack file <> "\n") >> pure ExitSuccess
            Left (FenceBroken why) -> failWith rt fmt 5 "region_broken" ("region `" <> regionName def <> "` in " <> T.pack file <> " is broken: " <> why) Nothing []
            Left FenceMissing -> case regionInitAt def of
              Nothing -> failWith rt fmt 2 "usage" ("region `" <> regionName def <> "` has no `init_at`, so Olifant cannot tell where its fences go") Nothing []
              Just phrase ->
                resolveLayers (toolSyntax tools) lang [PhraseAnchor phrase] content >>= \case
                  Left misses -> failWith rt fmt 5 "anchor_unresolved" (describeMisses misses) Nothing []
                  Right loc -> do
                    let (open, close) = fenceLines def c
                        placed = maybe loc (\i -> loc {locIndent = i}) (regionInitIndent def)
                    writeText ws file (snd (insertLines placed (open <> "\n" <> close) content))
                    fill
  where
    file = regionFile def
    lang = languageFor (toolLanguages tools) file
    fill = syncRegions rt fmt [] ws tools {toolRegions = [def]} SkipUnset False now

-- | The tools a command in this project uses: the runtime's syntax and
-- checker, the catalog's languages and regions, and the project's queries.
projectTools :: Runtime -> Catalog -> Tools IO
projectTools rt cat = (basicTools (rtSyntax rt) (rtChecker rt) (catalogLanguages cat)) {toolRegions = catalogRegions cat, toolQueries = projectQueries rt cat}

-- | Queries run in the project: commands in its root, AST queries over its
-- files with the catalog's language adapters.
projectQueries :: Runtime -> Catalog -> Queries IO
projectQueries rt cat =
  Queries
    { queryCommand = processCommand (rtProjectDir rt)
    , queryAst = astQueries (rtSyntax rt) (catalogLanguages cat)
    }
