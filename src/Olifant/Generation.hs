-- | Running generators: expand composed steps (spec §7), render their edits,
-- plan them against the workspace as one atomic plan, and (unless plan-only
-- or conflicted) write them.
module Olifant.Generation
  ( Mode (..)
  , GenerationError (..)
  , Step (..)
  , Resolve
  , noResolve
  , StepPath
  , showStepPath
  , generate
  , module Olifant.Generation.Plan
  ) where

import Control.Monad (filterM, forM_, when)
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Olifant.Generation.Compose
import Olifant.Generation.Plan
import Olifant.Generation.Planner (decideItems)
import Olifant.Generation.Render (renderedPath)
import Olifant.Generator (EnvError, GeneratorId (..), GeneratorWith (..))
import Olifant.Template (TemplateError)
import Olifant.Workspace (PathState (..), Workspace (..), isSafeRelative)
import System.FilePath (takeDirectory)

data Mode = PlanOnly | Apply
  deriving stock (Eq, Show)

data GenerationError
  = InvalidInputs StepPath EnvError
  | TemplateFailed StepPath TemplateError
  | UnknownGenerator StepPath Text Text
  | CompositionCycle StepPath
  deriving stock (Eq, Show)

generate :: Monad m => Workspace m -> Resolve m -> [Step] -> UTCTime -> Mode -> m (Either GenerationError Plan)
generate ws resolve steps now mode =
  expandSteps resolve now steps >>= \case
    Left err -> pure (Left (fromComposition err))
    Right items -> do
      let targets = nub [renderedPath e | PlanEdit _ e <- items]
          required = nub [p | PlanRequire _ ps _ <- items, p <- ps]
      inspected <- traverse (\p -> (p,) <$> inspectTarget p) (nub (targets ++ required))
      let originals = M.fromList [(p, t) | (p, Right t) <- inspected]
          blocked = M.fromList [(p, why) | (p, Left why) <- inspected, p `elem` targets]
          (edits, conflicts, writes) = decideItems items originals blocked
          ids = [generatorIdText (genId (stepGenerator s)) | s <- steps]
          plan = Plan (GeneratorId (T.intercalate " + " ids)) (zip ids (map stepSets steps)) now edits conflicts writes
      when (mode == Apply && not (hasConflicts plan)) $
        forM_ (M.toList writes) (uncurry (writeText ws))
      pure (Right plan)
  where
    fromComposition = \case
      StepInputs p e -> InvalidInputs p e
      StepTemplate p e -> TemplateFailed p e
      StepUnknown p ref why -> UnknownGenerator p ref why
      StepCycle p -> CompositionCycle p
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
