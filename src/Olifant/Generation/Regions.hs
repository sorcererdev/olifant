-- | The owned-region phase (spec §5.8): after a plan's edits, rewrite every
-- region from its query, against the files as the plan leaves them.
module Olifant.Generation.Regions (RegionMode (..), refreshRegions, regionInstances, unsetRegions, absorbDuplicates) where

import Control.Monad (foldM)
import Data.List (sortOn, (\\))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Generation.Plan
import Olifant.Generation.Tools (Tools (..))
import Olifant.Language (Language (..), languageFor)
import Olifant.Glob (matchesGlob)
import Olifant.Query (AstQuery (..), Query (..), QueryFailure (..), Record, runQuery)
import System.FilePath (takeDirectory)
import Olifant.Region
import Olifant.Template (describeTemplateError)
import Olifant.Workspace (Workspace (..), isSafeRelative)

-- | 'SkipUnset' (`gen`): a region whose file or fences are absent is not set
-- up in this project, so it is skipped. 'ReportUnset' (`sync --check`)
-- reports it.
data RegionMode = SkipUnset | ReportUnset
  deriving stock (Eq, Show)

type Cache = [(Query, Either Text [Record])]

refreshRegions :: Monad m => RegionMode -> Tools m -> Workspace m -> Int -> Map FilePath (Maybe Text) -> m ([PlannedEdit], [Conflict], Map FilePath (Maybe Text))
refreshRegions mode tools ws firstNumber current0
  | null (toolRegions tools) = pure ([], [], current0)
  | otherwise = do
      disk <- listFiles ws "."
      -- A set, not `nub`: projects can hold hundreds of thousands of files.
      let universe = S.toList (S.fromList (disk ++ [p | (p, Just _) <- M.toList current0]))
      (edits, conflicts, current, _) <- foldM (step universe) ([], [], current0, []) (sortOn (\d -> (regionName d, regionFile d)) (concatMap (regionInstances universe) (toolRegions tools)))
      pure (reverse edits, reverse conflicts, current)
  where
    step universe (edits, conflicts, current, cache) def = do
      let file = regionFile def
          name = regionName def
          conflict why = pure (edits, RegionBroken name file why : conflicts, current, cache)
          unset why = if mode == ReportUnset then conflict why else pure (edits, conflicts, current, cache)
      old <- if isSafeRelative file then maybe (readText ws file) pure (M.lookup file current) else pure Nothing
      case (commentFor file def, old) of
        _ | not (isSafeRelative file) -> conflict ("region `" <> name <> "`: " <> T.pack file <> " is outside the project")
        (Nothing, _) -> conflict ("region `" <> name <> "`: no comment syntax for " <> T.pack file <> " (add `comment` to the region)")
        (_, Nothing) -> unset ("region `" <> name <> "`: " <> T.pack file <> " does not exist")
        (Just c, _) -> do
          (found, cache') <- cachedQuery universe (\p -> maybe (readText ws p) pure (M.lookup p current)) cache (regionQuery def)
          case found of
            Left why -> conflict ("region `" <> name <> "`: " <> why)
            Right records -> case renderRegion def records of
              Left err -> conflict ("region `" <> name <> "`: " <> describeTemplateError err)
              Right body -> case applyRegion def c body (fromMaybe "" old) of
                Left FenceMissing -> unset (describeRegionProblem def FenceMissing)
                Left problem -> conflict (describeRegionProblem def problem)
                Right new
                  | Just new == old -> pure (edits, conflicts, current, cache')
                  | otherwise ->
                      let before = maybe [] (oldBody def c) old
                          action = WillRefresh name (length (body \\ before)) (length (before \\ body)) new
                          edit = PlannedEdit ["region " <> name] (firstNumber + length edits) file action Nothing Nothing
                       in pure (edit : edits, conflicts, M.insert file (Just new) current, cache')
    commentFor file def = case regionComment def of
      Just c -> Just c
      Nothing -> langComment <$> languageFor (toolLanguages tools) file
    -- A command query runs once per refresh, however many regions use it.
    -- Other queries read files through the view, which earlier regions may
    -- have just rewritten, so they run each time (and are cheap enough to).
    cachedQuery universe view cache q = case (q, lookup q (cache :: Cache)) of
      (CommandQuery {}, Just r) -> pure (r, cache)
      (CommandQuery {}, Nothing) -> do
        r <- first failureWhy <$> runQuery (toolQueries tools) universe view q
        pure (r, (q, r) : cache)
      _ -> (,cache) . first failureWhy <$> runQuery (toolQueries tools) universe view q

-- | A region whose `file` is a pattern is a family: one region per existing
-- file it matches, its query's `{= dir =}` and `{= file =}` being that
-- file's directory and path (spec §5.9).
regionInstances :: [FilePath] -> RegionDef -> [RegionDef]
regionInstances universe def
  | any (`elem` ("*?[" :: String)) (regionFile def) =
      [ def {regionFile = f, regionQuery = forFile f (regionQuery def)}
      | f <- universe
      , matchesGlob (T.pack (regionFile def)) f
      ]
  | otherwise = [def]
  where
    forFile f = \case
      GlobQuery g ex -> GlobQuery (sub f g) (map (sub f) ex)
      AstQueryQ q -> AstQueryQ q {astFiles = sub f (astFiles q)}
      q -> q
    sub f = T.replace "{= dir =}" (T.pack (takeDirectory f)) . T.replace "{= file =}" (T.pack f)

-- | Region files that exist but have no fences for their region (not set
-- up): `gen` and `sync` skip them, so they say so (review I5).
unsetRegions :: Monad m => Tools m -> Workspace m -> m [(FilePath, Text)]
unsetRegions tools ws = do
  universe <- listFiles ws "."
  fmap concat . traverse (\d -> check d) $ concatMap (regionInstances universe) (toolRegions tools)
  where
    check def
      | regionWholeFile def = pure []
      | otherwise =
          readText ws (regionFile def) >>= \case
            Just content | not (("olifant:region " <> regionName def) `T.isInfixOf` content) -> pure [(regionFile def, regionName def)]
            _ -> pure []

-- | After `sync --init` puts fences into a hand-written file, the lines the
-- region now owns would appear twice (inside and below): drop the copies
-- outside the fences (review I4).
absorbDuplicates :: Text -> Text
absorbDuplicates content =
  let ls = T.lines content
      isOpen l = "olifant:region " `T.isInfixOf` l
      isClose l = "olifant:end " `T.isInfixOf` l
      inside = go False ls
        where
          go _ [] = []
          go False (l : rest) = go (isOpen l) rest
          go True (l : rest)
            | isClose l = go False rest
            | otherwise = T.strip l : go True rest
      owned = S.fromList [l | l <- inside, not (T.null l)]
      keep = go False ls
        where
          go _ [] = []
          go open (l : rest)
            | isOpen l = l : go True rest
            | isClose l = l : go False rest
            | open = l : go True rest
            | T.strip l `S.member` owned = go False rest
            | otherwise = l : go False rest
   in T.unlines keep

-- | The lines currently inside a region (stripped), for counting what changed.
oldBody :: RegionDef -> Text -> Text -> [Text]
oldBody def c content
  | regionWholeFile def = drop 1 (T.lines content)
  | otherwise =
      let ls = map T.strip (T.lines (T.replace "\r\n" "\n" content))
          open = c <> " olifant:region " <> regionName def
          isOpen l = l == open || (open <> " ") `T.isPrefixOf` l
          isClose l = l == c <> " olifant:end " <> regionName def
       in takeWhile (not . isClose) (drop 1 (dropWhile (not . isOpen) ls))
