-- | Running a generator: build the environment, render edits, plan them
-- against the workspace, and (unless plan-only or conflicted) write them.
module Olifant.Generation
  ( Mode (..)
  , GenerationError (..)
  , generate
  , module Olifant.Generation.Plan
  ) where

import Control.Monad (filterM, forM_, when)
import Data.Bifunctor (first)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Olifant.Generation.Plan
import Olifant.Generation.Planner
import Olifant.Generator (EnvError, Generator, GeneratorWith (..), Requires (..), buildEnv)
import Olifant.Template (TemplateError, render)
import Olifant.Workspace (Workspace (..), isSafeRelative, normalisePath)

data Mode = PlanOnly | Apply
  deriving stock (Eq, Show)

data GenerationError = InvalidInputs EnvError | TemplateFailed TemplateError
  deriving stock (Eq, Show)

generate :: Monad m => Workspace m -> Generator -> Map Text Text -> UTCTime -> Mode -> m (Either GenerationError Plan)
generate ws gen sets now mode = case prepare of
  Left err -> pure (Left err)
  Right (rendered, requiredFiles, hint) -> do
    missing <- filterM (\p -> if isSafeRelative p then not <$> fileExists ws p else pure True) requiredFiles
    originals <- M.fromList <$> traverse (\p -> (p,) <$> readIfSafe p) (nub (map renderedPath rendered))
    let (edits, conflicts, writes) = decideEdits rendered originals
        requirementConflicts = [RequirementUnmet missing hint | not (null missing)]
        plan = Plan (genId gen) sets now edits (requirementConflicts ++ conflicts) writes
    when (mode == Apply && not (hasConflicts plan)) $
      forM_ (M.toList writes) (uncurry (writeText ws))
    pure (Right plan)
  where
    prepare = do
      env <- first InvalidInputs (buildEnv gen sets now)
      rendered <- first TemplateFailed (renderEdits env (genEdits gen))
      let requires = genRequires gen
      requiredFiles <- first TemplateFailed (traverse (fmap (normalisePath . T.unpack) . render env) (maybe [] requiresExists requires))
      hint <- first TemplateFailed (traverse (render env) (requires >>= requiresHint))
      pure (rendered, requiredFiles, hint)
    readIfSafe p = if isSafeRelative p then readText ws p else pure Nothing
