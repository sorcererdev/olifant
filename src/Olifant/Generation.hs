-- | Running generators: expand composed steps (spec §7), render their edits,
-- plan them against the workspace as one atomic plan, and (unless plan-only
-- or conflicted) write them.
module Olifant.Generation
  ( Mode (..)
  , GenerationError (..)
  , describeGenerationError
  , Step (..)
  , Resolve
  , ResolveFailure (..)
  , noResolve
  , StepPath
  , showStepPath
  , generate
  , RegionMode (..)
  , refreshRegions
  , module Olifant.Generation.Plan
  , module Olifant.Generation.Tools
  ) where

import Control.Monad (filterM, foldM, forM_, when)
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Olifant.Generation.Complete (completeEdits)
import Olifant.Generation.Regions (RegionMode (..), refreshRegions)
import Olifant.Generation.Compose
import Olifant.Generation.Plan
import Olifant.Anchor (resolveLayers)
import Olifant.Generation.Planner (decideItems)
import Olifant.Generation.Tools
import Olifant.Generation.Render (RenderedEdit (..), renderedPath, renderedUnless)
import Olifant.Generator (EnvError, GeneratorId (..), GeneratorWith (..), describeEnvError)
import Olifant.Language (languageFor)
import Olifant.Query (renderQuery, runQuery)
import Olifant.Template (TemplateError, Value (..), describeTemplateError)
import Olifant.Workspace (PathState (..), Workspace (..), isSafeRelative)
import System.FilePath (takeDirectory)

data Mode = PlanOnly | Apply
  deriving stock (Eq, Show)

data GenerationError
  = InvalidInputs StepPath EnvError
  | TemplateFailed StepPath TemplateError
  | UnknownGenerator StepPath Text Text
  | InvalidGenerator StepPath Text Text
  | CompositionCycle StepPath
  | QueryFailed StepPath Text
  deriving stock (Eq, Show)

-- | The message for a 'GenerationError', naming the step it happened in.
describeGenerationError :: GenerationError -> Text
describeGenerationError = \case
  InvalidInputs path e -> inStep path (describeEnvError e)
  TemplateFailed path e -> inStep path (describeTemplateError e)
  UnknownGenerator path ref why -> inStep path ("`" <> ref <> "`: " <> why)
  InvalidGenerator path ref why -> inStep path ("`" <> ref <> "`: " <> why)
  QueryFailed path why -> inStep path why
  CompositionCycle path -> "generators use each other in a cycle: " <> showStepPath path
  where
    inStep path msg = showStepPath path <> ": " <> msg

generate :: Monad m => Workspace m -> Resolve m -> Tools m -> [Step] -> UTCTime -> Mode -> m (Either GenerationError Plan)
generate ws resolve tools steps now mode =
  expandSteps resolve bindQueries now steps >>= \case
    Left err -> pure (Left (fromComposition err))
    Right items -> do
      let targets = nub [p | PlanEdit _ e <- items, Just p <- [renderedPath e]]
          required = nub [p | PlanRequire _ ps _ <- items, p <- ps]
      inspected <- traverse (\p -> (p,) <$> inspectTarget p) (nub (targets ++ required))
      onDisk <- S.fromList <$> filterM (\p -> if isSafeRelative p then (/= Absent) <$> inspectPath ws p else pure False) required
      let originals = M.fromList [(p, t) | (p, Right t) <- inspected]
          blocked = M.fromList [(p, why) | (p, Left why) <- inspected, p `elem` targets]
          ids = [generatorIdText (genId (stepGenerator s)) | s <- steps]
      -- `unless` globs need the project's paths; list them only when used.
      diskPaths <- if any (isJust . renderedUnless) [e | PlanEdit _ e <- items] then listFiles ws "." else pure []
      let completes = [(p, c) | PlanEdit p (RenderedComplete c) <- items]
          -- Both phases (spec §5.6): positional edits, then compiler-found branches.
          phases originals' = do
            (positional, pc, afterPositional, next) <- decideItems locate diskPaths items originals' blocked onDisk
            (compiled, cc, ws', afterComplete) <- completeEdits tools ws completes next afterPositional
            (refreshed, rc, current') <- refreshRegions SkipUnset tools ws (next + length compiled) afterComplete
            pure (positional ++ compiled ++ refreshed, pc ++ cc ++ rc, ws', current')
      firstPass <- phases originals
      (edits, conflicts, warnings, current) <- case [p | TargetExists _ p <- (\(_, c, _, _) -> c) firstPass] of
        [] -> pure firstPass
        -- A file this plan creates and then edits no longer matches its
        -- template on a re-run, so its create reports `target_exists`. If
        -- planning as if it were absent reproduces it exactly, the run is a
        -- no-op for it, not a conflict (spec §5.4).
        existing -> do
          (edits2, conflicts2, warnings2, current2) <- phases (foldr (\p -> M.insert p Nothing) originals existing)
          let unchanged p = M.lookup p current2 == M.lookup p originals
          pure $
            if null conflicts2 && all unchanged existing
              then ([if editPath e `elem` existing then e {editAction = Skipped UnchangedContent, editSite = Nothing} else e | e <- edits2], conflicts2, warnings2, current2)
              else firstPass
      let writes = M.fromList [(p, c) | (p, Just c) <- M.toList current, M.lookup p originals /= Just (Just c)]
          plan = Plan (GeneratorId (T.intercalate " + " ids)) (zip ids (map stepSets steps)) now edits conflicts warnings writes
      when (mode == Apply && not (hasConflicts plan)) $
        forM_ (M.toList writes) (uncurry (writeText ws))
      pure (Right plan)
  where
    -- `[query.NAME]`: render each query with the generator's inputs and
    -- derived values (never another query's records, so their order does not
    -- matter), run it over the project as it is on disk, and bind the records
    -- as a list.
    bindQueries env queries = do
      paths <- listFiles ws "."
      let one acc (name, q) = case acc of
            Left why -> pure (Left why)
            Right e -> case renderQuery env q of
              Left err -> pure (Left ("query `" <> name <> "`: " <> describeTemplateError err))
              Right q' -> fmap (\rs -> M.insert name (VList (map VRecord rs)) e) . first (\why -> "query `" <> name <> "`: " <> why) <$> runQuery (toolQueries tools) paths (readText ws) q'
      foldM one (Right env) queries
    locate p anchors content = resolveLayers (toolSyntax tools) (languageFor (toolLanguages tools) p) anchors content
    fromComposition = \case
      StepInputs p e -> InvalidInputs p e
      StepTemplate p e -> TemplateFailed p e
      StepUnknown p ref why -> UnknownGenerator p ref why
      StepInvalid p ref why -> InvalidGenerator p ref why
      StepCycle p -> CompositionCycle p
      StepQuery p why -> QueryFailed p why
    -- Right: current text (or Nothing if absent). Left: why it cannot be written.
    -- Unsafe paths are never touched; the planner reports them.
    inspectTarget p
      | not (isSafeRelative p) = pure (Right Nothing)
      | otherwise = do
          parentFiles <- filterM (fmap isFile . inspectPath ws) (parents p)
          case parentFiles of
            (parent : _) -> pure (Left ("`" <> T.pack parent <> "` is a file, not a directory"))
            [] ->
              inspectPath ws p <&> \case
                Absent -> Right Nothing
                Readable t -> Right (Just t)
                Unreadable -> Left "it exists but is not UTF-8 text, so Olifant will not touch it"
                Directory -> Left "it is a directory"
    isFile = \case
      Readable _ -> True
      Unreadable -> True
      _ -> False
    parents = takeWhile (/= ".") . drop 1 . iterate takeDirectory
