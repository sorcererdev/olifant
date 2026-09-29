-- | Expanding composed steps into plan items (spec §7): each generator's own
-- requirements and edits, then its [[uses]] in order (for_each in place).
module Olifant.Generation.Compose
  ( StepPath
  , Step (..)
  , Resolve
  , noResolve
  , CompositionError (..)
  , PlanItem (..)
  , expandSteps
  , showStepPath
  ) where

import Control.Monad (when, zipWithM)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE)
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Olifant.Generation.Render (RenderedEdit, renderEdits)
import Olifant.Generator
import Olifant.Template (Env, TemplateError (..), Value (..), render)
import Olifant.Workspace (normalisePath)

type StepPath = [Text]

data Step = Step {stepGenerator :: Generator, stepSets :: Map Text Text}

-- | Load a generator named in a [[uses]] step. Arguments: the pack of the
-- generator doing the using (bare names resolve there first), and the ref.
type Resolve m = Maybe Text -> Text -> m (Either Text Generator)

noResolve :: Applicative m => Resolve m
noResolve _ ref = pure (Left ("cannot resolve `" <> ref <> "`: no generator catalog available"))

data CompositionError
  = StepInputs StepPath EnvError
  | StepTemplate StepPath TemplateError
  | StepUnknown StepPath Text Text
  | StepCycle StepPath
  deriving stock (Eq, Show)

data PlanItem
  = PlanRequire StepPath [FilePath] (Maybe Text)
  | PlanEdit StepPath RenderedEdit

showStepPath :: StepPath -> Text
showStepPath = T.intercalate " › "

expandSteps :: Monad m => Resolve m -> UTCTime -> [Step] -> m (Either CompositionError [PlanItem])
expandSteps resolve now steps = runExceptT (concat <$> zipWithM top [1 :: Int ..] steps)
  where
    top i (Step gen sets) = expand resolve now [] [label gen i] gen sets
    label gen i
      | length steps > 1 = idText gen <> " (step " <> T.pack (show i) <> ")"
      | otherwise = idText gen

expand :: Monad m => Resolve m -> UTCTime -> [GeneratorId] -> StepPath -> Generator -> Map Text Text -> ExceptT CompositionError m [PlanItem]
expand resolve now ancestors path gen sets = do
  when (genId gen `elem` ancestors) $ throwE (StepCycle path)
  env <- except (first (StepInputs path) (buildEnv gen sets now))
  rendered <- except (first (StepTemplate path) (renderEdits env (genEdits gen)))
  (required, hint) <- except (first (StepTemplate path) (renderRequires env (genRequires gen)))
  nested <- concat <$> traverse (expandUse env) (genUses gen)
  pure ([PlanRequire path required hint | not (null required)] ++ map (PlanEdit path) rendered ++ nested)
  where
    expandUse env u = do
      childSets <- except (first (StepTemplate path) (renderUse env u))
      child <- ExceptT (first (StepUnknown path (useGenerator u)) <$> resolve (packOf (genId gen)) (useGenerator u))
      concat
        <$> traverse
          (\(i, s) -> expand resolve now (genId gen : ancestors) (path ++ [idText child <> maybe "" (\n -> "[" <> T.pack (show n) <> "]") i]) child s)
          childSets

renderRequires :: Env -> Maybe Requires -> Either TemplateError ([FilePath], Maybe Text)
renderRequires _ Nothing = Right ([], Nothing)
renderRequires env (Just r) =
  (,) <$> traverse (fmap (normalisePath . T.unpack) . render env) (requiresExists r) <*> traverse (render env) (requiresHint r)

-- | The input sets for a [[uses]] step: one, or one per for_each item.
renderUse :: Env -> UseStep -> Either TemplateError [(Maybe Int, Map Text Text)]
renderUse env u = case useForEach u of
  Nothing -> (\s -> [(Nothing, s)]) <$> renderWith env
  Just listName -> case M.lookup listName env of
    Nothing -> Left (UndefinedVariable listName)
    Just VNull -> Right []
    Just (VList items) ->
      let n = length items
          loopVars i = VRecord (M.fromList [("first", VBool (i == 1)), ("last", VBool (i == n)), ("index", VInt (fromIntegral i))])
       in traverse (\(i, item) -> (Just i,) <$> renderWith (M.insert "item" item (M.insert "loop" (loopVars i) env))) (zip [1 ..] items)
    Just _ -> Left (TypeMismatch ("`for_each` needs a list: `" <> listName <> "`"))
  where
    renderWith e = M.fromList <$> traverse (\(k, t) -> (k,) <$> render e t) (useWith u)

idText :: Generator -> Text
idText = generatorIdText . genId

packOf :: GeneratorId -> Maybe Text
packOf (GeneratorId t) = case T.breakOn "/" t of
  (p, rest) | not (T.null rest) -> Just p
  _ -> Nothing
