-- | Running a pack's generator test cases against its fixture app. This is
-- the seed of `olifant test` (spec §9, step 4).
module Olifant.GeneratorTesting
  ( PackCase (..)
  , CaseResult (..)
  , discoverCases
  , runCase
  , parseCaseInputs
  , parseCaseCursor
  , atCaseCursor
  , preparePack
  ) where

import Control.Monad (forM, forM_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Olifant.Catalog
import Olifant.Generation
import Olifant.Cursor (atCursor, isSnippet)
import Olifant.Uncovered (uncoveredQuery)
import Olifant.Generator (Generator)
import Olifant.Language.Adapter (Language (..), languageFor)
import Olifant.Fill (FillHole (..), FillRequest (..), Filler (..), ModelFiller (..))
import Data.List (nub)
import Olifant.Anchor (astQueries)
import Olifant.Query (Queries (..), processCommand)
import Olifant.Language (Checker, Syntax)
import Olifant.Toml qualified as Toml
import Olifant.Workspace (Workspace (..), memoryWorkspace)
import System.FilePath (splitDirectories, (</>))

data PackCase = PackCase
  { caseName :: Text
  , casePack :: Text
  , casePackRoot :: FilePath
  , caseGenerator :: Text
  , caseDir :: FilePath
  }
  deriving stock (Eq, Show)

-- | 'CaseSkipped': a tool the case needs is not installed.
data CaseResult = CasePassed | CaseFailed [Text] | CaseSkipped Text
  deriving stock (Eq, Show)

discoverCases :: Monad m => Workspace m -> FilePath -> m [PackCase]
discoverCases root packsDir = do
  files <- listFiles root packsDir
  pure
    [ PackCase (T.pack (pack <> "/" <> gen <> "/" <> c)) (T.pack pack) (packsDir </> pack) (T.pack gen) (packsDir </> pack </> "generators" </> gen </> "tests" </> c)
    | f <- files
    , [pack, "generators", gen, "tests", c, "inputs.toml"] <- [splitDirectories f]
    ]

runCase :: Workspace IO -> Syntax IO -> Checker IO -> Bool -> PackCase -> IO CaseResult
runCase root syntax checker bless pc = do
  caseInputs <- readText root (caseDir pc </> "inputs.toml")
  case caseInputs >>= either (const Nothing) Just . parseCase of
    Nothing -> pure (CaseFailed ["cannot read " <> T.pack (caseDir pc </> "inputs.toml") <> " (needs `now` and an [inputs] table of strings)"])
    Just (now, sets, at) -> do
        -- A case's `given/` files replace or add to the fixture, for this
        -- case only: the state a re-run or a hand edit leaves.
        givenPaths <- listFiles root (caseDir pc </> "given")
        given <- forM givenPaths $ \p -> (p,) . fromMaybe "" <$> readText root (caseDir pc </> "given" </> p)
        -- `fills.toml` (spec §9): the recorded fills replay as the model,
        -- so tests never call one.
        recorded <- readText root (caseDir pc </> "fills.toml")
        let replay = recorded >>= either (const Nothing) Just . parseFills
        preparePack root syntax checker (casePackRoot pc) (casePack pc) (caseGenerator pc) given >>= \case
          Left why -> pure (CaseFailed [why])
          Right (ws, tools0, resolve, gen0) -> do
                   -- A snippet goes at the case's cursor; it is not re-run (its
                   -- line is the expansion now, so a second run is another snippet).
                   (tools, gen) <- atCaseCursor ws checker at (tools0 {toolFiller = (\fills -> ModelFiller "recorded" 0 (replayFiller fills)) <$> replay}) gen0
                   firstRun <- generate ws resolve tools [Step gen sets] now Apply
                   secondRun <- if isSnippet gen0 then pure (fmap (\p -> p {planWrites = M.empty}) firstRun) else generate ws resolve tools [Step gen sets] now Apply
                   case (firstRun, secondRun) of
                     (Left err, _)
                       | generationToolMissing err -> pure (CaseSkipped (describeGenerationError err))
                       | otherwise -> pure (CaseFailed [describeGenerationError err])
                     (Right plan, _)
                       | hasConflicts plan, all conflictToolMissing (planConflicts plan) -> pure (CaseSkipped (T.intercalate "; " (map describeConflict (planConflicts plan))))
                       | hasConflicts plan -> pure (CaseFailed (map describeConflict (planConflicts plan)))
                     (Right plan, _)
                       | Just _ <- replay, unfilled@(_ : _) <- [why | PlannedHole {phStatus = HoleUnfilled why} <- planHoles plan] -> pure (CaseFailed (nub unfilled))
                     (Right plan, again) -> do
                       snapshotProblems <- checkSnapshot (planWrites plan)
                       let rerunProblems = case again of
                             Right p | not (M.null (planWrites p)) -> ["re-running changed: " <> T.intercalate ", " (map T.pack (M.keys (planWrites p)))]
                             Left err -> ["re-running failed: " <> describeGenerationError err]
                             _ -> []
                       pure (if null (snapshotProblems ++ rerunProblems) then CasePassed else CaseFailed (snapshotProblems ++ rerunProblems))
  where
    snapshotDir = caseDir pc </> "snapshot"
    checkSnapshot :: Map FilePath Text -> IO [Text]
    checkSnapshot written
      | bless = do
          stale <- filter (`M.notMember` written) <$> listFiles root snapshotDir
          forM_ stale $ \p -> removeFile root (snapshotDir </> p)
          forM_ (M.toList written) $ \(p, c) -> writeText root (snapshotDir </> p) c
          pure []
      | otherwise = do
          paths <- listFiles root snapshotDir
          expected <- M.fromList <$> forM paths (\p -> (p,) . fromMaybe "" <$> readText root (snapshotDir </> p))
          pure $
            [T.pack p <> ": generated but not in snapshot" | p <- M.keys (written `M.difference` expected)]
              ++ [T.pack p <> ": in snapshot but not generated" | p <- M.keys (expected `M.difference` written)]
              ++ [T.pack p <> ": differs\n" <> firstDifference e w | (p, (e, w)) <- M.toList (M.intersectionWith (,) expected written), e /= w]
    firstDifference e w =
      let width = max (length (T.lines e)) (length (T.lines w))
       in case [(n, a, b) | (n, a, b) <- zip3 [1 .. width] (T.lines e <> repeat "<eof>") (T.lines w <> repeat "<eof>"), a /= b] of
            ((n, a, b) : _) -> "  line " <> T.pack (show n) <> "\n  expected: " <> a <> "\n  actual:   " <> b
            [] -> "  (trailing whitespace differs)"

-- | A pack's generator ready to run against its fixture (in memory), with
-- @overlay@ files laid over it: the workspace, tools, resolver and generator.
-- Commands run in the fixture on disk, like a project's commands run in the
-- project; `one_of` (here and in [[uses]] generators) is resolved by
-- 'generate' against the fixture and the plan so far.
preparePack :: Workspace IO -> Syntax IO -> Checker IO -> FilePath -> Text -> Text -> [(FilePath, Text)] -> IO (Either Text (Workspace IO, Tools IO, Resolve IO, Generator))
preparePack root syntax checker packRoot pack generator overlay =
  openPackCatalog root [(pack, packRoot)] >>= \case
    Left err -> pure (Left (describeCatalogError err))
    Right cat -> case findEntry cat (pack <> "/" <> generator) of
      Left err -> pure (Left (describeCatalogError err))
      Right entry ->
        loadGenerator root syntax cat entry >>= \case
          Left err -> pure (Left (describeCatalogError err))
          Right gen -> do
            let fixtureDir = packRoot </> "fixtures" </> "app"
            fixturePaths <- listFiles root fixtureDir
            fixture <- forM fixturePaths $ \p -> (p,) . fromMaybe "" <$> readText root (fixtureDir </> p)
            (ws, _) <- memoryWorkspace (M.toList (M.fromList overlay `M.union` M.fromList fixture))
            let queries = Queries {queryCommand = processCommand fixtureDir, queryAst = astQueries syntax (catalogLanguages cat), queryUncovered = \_ -> pure (Right [])}
                tools = (basicTools syntax checker (catalogLanguages cat)) {toolRegions = catalogRegions cat, toolQueries = queries}
            pure (Right (ws, tools, resolver root syntax cat, gen))

-- | A case's cursor (`at`): the snippet placed there, and `uncovered`
-- queries asking the compiler about the match there (spec §4.9).
atCaseCursor :: Workspace IO -> Checker IO -> Maybe (FilePath, Int) -> Tools IO -> Generator -> IO (Tools IO, Generator)
atCaseCursor ws checker at tools gen = case at of
  Nothing -> pure (tools, gen)
  Just (file, line) -> do
    text <- fromMaybe "" <$> readText ws file
    let unit = maybe "    " langIndent (languageFor (toolLanguages tools) file)
        tools' = tools {toolQueries = (toolQueries tools) {queryUncovered = uncoveredQuery checker (toolLanguages tools) ws file line}}
    pure (tools', if isSnippet gen then atCursor unit file line (T.splitOn "\n" text) gen else gen)

-- | A case's `at = "FILE:LINE:COL"`, if it has one.
parseCaseCursor :: Text -> Either Text (Maybe (FilePath, Int))
parseCaseCursor src = (\(_, _, at) -> at) <$> parseCase src

-- | `[fills]` in a case's `fills.toml`: hole name = recorded code.
parseFills :: Text -> Either Text (Map Text Text)
parseFills src = do
  top <- Toml.parseToml src
  fills <- fromMaybe M.empty <$> Toml.optional "fills" Toml.table top
  traverse Toml.text fills

-- | Recorded fills as a model: every hole asked for must have one.
replayFiller :: Monad m => Map Text Text -> Filler m
replayFiller fills = Filler $ \req -> pure $ case [fhName h | h <- frHoles req, M.notMember (fhName h) fills] of
  [] -> Right fills
  missing -> Left (T.intercalate "; " ["no recorded fill for `" <> n <> "` in fills.toml" | n <- missing])

-- | A case's `inputs.toml`: `now` and the `[inputs]` values.
parseCaseInputs :: Text -> Either Text (UTCTime, Map Text Text)
parseCaseInputs src = (\(now, sets, _) -> (now, sets)) <$> parseCase src

-- | `now`, the `[inputs]`, and `at = "FILE:LINE:COL"`: where a snippet's
-- cursor is (spec §5.3).
parseCase :: Text -> Either Text (UTCTime, Map Text Text, Maybe (FilePath, Int))
parseCase src = do
  top <- Toml.parseToml src
  nowText <- Toml.required "now" Toml.text top
  now <- maybe (Left "`now`: expected ISO 8601") Right (iso8601ParseM (T.unpack nowText))
  inputs <- fromMaybe M.empty <$> Toml.optional "inputs" Toml.table top
  sets <- traverse Toml.text inputs
  at <- Toml.optional "at" Toml.text top >>= traverse cursor
  pure (now, sets, at)
  where
    cursor t = case reverse (T.splitOn ":" t) of
      (_ : l : rest@(_ : _)) | [(line, "")] <- reads (T.unpack l) -> Right (T.unpack (T.intercalate ":" (reverse rest)), line)
      _ -> Left "`at`: expected FILE:LINE:COL"
