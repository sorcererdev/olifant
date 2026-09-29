-- | Running a pack's generator test cases against its fixture app. This is
-- the seed of `olifant test` (spec §9, step 4).
module Olifant.GeneratorTesting
  ( PackCase (..)
  , CaseResult (..)
  , discoverCases
  , runCase
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
    Just (now, sets) ->
      openPackCatalog root [(casePack pc, casePackRoot pc)] >>= \case
        Left err -> pure (CaseFailed [describeCatalogError err])
        Right cat -> case findEntry cat (casePack pc <> "/" <> caseGenerator pc) of
          Left err -> pure (CaseFailed [describeCatalogError err])
          Right entry ->
            loadGenerator root cat entry >>= \case
              Left err -> pure (CaseFailed [describeCatalogError err])
              Right gen -> do
                let fixtureDir = casePackRoot pc </> "fixtures" </> "app"
                fixturePaths <- listFiles root fixtureDir
                fixture <- forM fixturePaths $ \p -> (p,) . fromMaybe "" <$> readText root (fixtureDir </> p)
                (ws, _) <- memoryWorkspace fixture
                let tools = Tools syntax checker (catalogLanguages cat)
                firstRun <- generate ws (resolver root cat) tools [Step gen sets] now Apply
                secondRun <- generate ws (resolver root cat) tools [Step gen sets] now Apply
                case (firstRun, secondRun) of
                  (Left err, _) -> pure (CaseFailed [T.pack (show err)])
                  (Right plan, _)
                    | hasConflicts plan, all conflictToolMissing (planConflicts plan) -> pure (CaseSkipped (T.intercalate "; " (map describeConflict (planConflicts plan))))
                    | hasConflicts plan -> pure (CaseFailed (map describeConflict (planConflicts plan)))
                  (Right plan, again) -> do
                    snapshotProblems <- checkSnapshot (planWrites plan)
                    let rerunProblems = case again of
                          Right p | not (M.null (planWrites p)) -> ["re-running changed: " <> T.intercalate ", " (map T.pack (M.keys (planWrites p)))]
                          Left err -> ["re-running failed: " <> T.pack (show err)]
                          _ -> []
                    pure (if null (snapshotProblems ++ rerunProblems) then CasePassed else CaseFailed (snapshotProblems ++ rerunProblems))
  where
    snapshotDir = caseDir pc </> "snapshot"
    checkSnapshot :: Map FilePath Text -> IO [Text]
    checkSnapshot written
      | bless = do
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

parseCase :: Text -> Either Text (UTCTime, Map Text Text)
parseCase src = do
  top <- Toml.parseToml src
  nowText <- Toml.required "now" Toml.text top
  now <- maybe (Left "`now`: expected ISO 8601") Right (iso8601ParseM (T.unpack nowText))
  inputs <- fromMaybe M.empty <$> Toml.optional "inputs" Toml.table top
  sets <- traverse Toml.text inputs
  pure (now, sets)
