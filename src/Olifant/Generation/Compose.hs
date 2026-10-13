-- | Expanding composed steps into plan items (spec §7): each generator's own
-- requirements and edits, then its [[uses]] in order (for_each in place).
module Olifant.Generation.Compose
  ( StepPath
  , Step (..)
  , Resolve
  , ResolveFailure (..)
  , noResolve
  , CompositionError (..)
  , PlanItem (..)
  , expandSteps
  , BindQueries
  , Choose
  , showStepPath
  ) where

import Control.Monad (foldM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE)
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Olifant.Generation.Render (RenderedEdit (..), renderEdits)
import Olifant.Query (Query, QueryFailure)
import Olifant.Generator
import Olifant.Template (Env, TemplateError (..), Value (..), render)
import Olifant.Workspace (normalisePath)

type StepPath = [Text]

data Step = Step {stepGenerator :: Generator, stepSets :: Map Text Text}

-- | Load a generator named in a [[uses]] step. Arguments: the pack of the
-- generator doing the using (bare names resolve there first), and the ref.
type Resolve m = Maybe Text -> Text -> m (Either ResolveFailure Generator)

noResolve :: Applicative m => Resolve m
noResolve _ ref = pure (Left (ResolveNotFound ("cannot resolve `" <> ref <> "`: no generator catalog available")))

data CompositionError
  = StepInputs StepPath EnvError
  | StepTemplate StepPath TemplateError
  | StepUnknown StepPath Text Text
  | StepInvalid StepPath Text Text
  | StepCycle StepPath
  | StepQuery StepPath QueryFailure
  deriving stock (Eq, Show)

-- | Bind a generator's `[query.NAME]` lists into its environment (spec §4.5).
type BindQueries m = Env -> [(Text, Query)] -> m (Either QueryFailure Env)

-- | Resolve a generator's `choices_from` enums (spec §4.1), given the files
-- the run has created so far (path, planned contents): a later step can
-- choose what an earlier step creates.
type Choose m = [(FilePath, Text)] -> Map Text Text -> Generator -> m (Either QueryFailure Generator)

data PlanItem
  = PlanRequire StepPath [FilePath] (Maybe Text)
  | -- | a `[requires] holds` that rendered empty: the hint
    PlanRefuse StepPath Text
  | -- | a hole's `verify` command, rendered (the hole's name, argv)
    PlanVerify Text (Verify [Text])
  | PlanEdit StepPath RenderedEdit
  | -- | globs of files the model sees when filling (a generator's `context`)
    PlanContext [Text]

showStepPath :: StepPath -> Text
showStepPath = T.intercalate " › "

expandSteps :: Monad m => Resolve m -> BindQueries m -> Choose m -> UTCTime -> [Step] -> m (Either CompositionError [PlanItem])
expandSteps resolve bindQ choose now steps = runExceptT (foldM top [] (zip [1 :: Int ..] steps))
  where
    -- Steps in order, each seeing the files the earlier ones create.
    top acc (i, Step gen sets) = (acc ++) <$> expand resolve bindQ choose now (created acc) [] [label gen i] gen sets
    label gen i
      | length steps > 1 = idText gen <> " (step " <> T.pack (show i) <> ")"
      | otherwise = idText gen

-- | The files a run's items create, with their planned contents.
created :: [PlanItem] -> [(FilePath, Text)]
created items = [(p, c) | PlanEdit _ (RenderedCreate p c _ _) <- items]

expand :: Monad m => Resolve m -> BindQueries m -> Choose m -> UTCTime -> [(FilePath, Text)] -> [GeneratorId] -> StepPath -> Generator -> Map Text Text -> ExceptT CompositionError m [PlanItem]
expand resolve bindQ choose now prior ancestors path gen0 sets = do
  when (genId gen0 `elem` ancestors) $ throwE (StepCycle path)
  checkCycles resolve (genId gen0 : ancestors) path gen0
  gen <- ExceptT (first (StepQuery path) <$> choose prior sets gen0)
  env0 <- except (first (StepInputs path) (buildEnv gen sets now))
  env <- if null (genQueries gen) then pure env0 else ExceptT (first (StepQuery path) <$> bindQ env0 (genQueries gen))
  rendered <- except (first (StepTemplate path) (renderEdits (genLanguage gen) env (genEdits gen)))
  (required, hint) <- except (first (StepTemplate path) (renderRequires env (genRequires gen)))
  holds <- except (first (StepTemplate path) (traverse (render env) (requiresHolds =<< genRequires gen)))
  context <- except (first (StepTemplate path) (traverse (render env) (genContext gen)))
  -- {= cache =} and {= context =} are the verifier's to expand (as a check's)
  let verifyEnv = M.union env (M.fromList [("cache", VText "{= cache =}"), ("context", VText "{= context =}")])
  verifies <- except (first (StepTemplate path) (traverse (\(n, v) -> (n,) <$> traverse (traverse (render verifyEnv)) v) (genVerify gen)))
  let own = map (PlanEdit path) rendered
  -- Each use sees what the run, this generator and earlier uses create.
  nested <- foldM (\acc (k, u) -> (acc ++) <$> expandUse env (prior ++ created (own ++ acc)) k u) [] (zip [1 :: Int ..] (genUses gen))
  pure ([PlanRefuse path (fromMaybe "a requirement does not hold" hint) | Just h <- [holds], T.null (T.strip h)] ++ [PlanRequire path required hint | not (null required)] ++ [PlanContext context | not (null context)] ++ [PlanVerify n argv | (n, argv) <- verifies] ++ own ++ nested)
  where
    repeated u = length (filter ((== useGenerator u) . useGenerator) (genUses gen0)) > 1
    expandUse env seen k u = do
      childSets <- except (first (StepTemplate path) (renderUse env u))
      child <- ExceptT (first (failure (useGenerator u)) <$> resolve (packOf (genId gen0)) (useGenerator u))
      foldM
        (\acc (i, s) -> (acc ++) <$> expand resolve bindQ choose now (seen ++ created acc) (genId gen0 : ancestors) (path ++ [label child k u i]) child s)
        []
        childSets

    label child k u i =
      idText child
        <> (if repeated u then " (use " <> T.pack (show k) <> ")" else "")
        <> maybe "" (\n -> "[" <> T.pack (show n) <> "]") i
    failure ref = \case
      ResolveNotFound why -> StepUnknown path ref why
      ResolveInvalid why -> StepInvalid path ref why

-- | Walk the [[uses]] graph without rendering, so a cycle is reported even
-- when a for_each list happens to be empty (spec §7: before anything is planned).
checkCycles :: Monad m => Resolve m -> [GeneratorId] -> StepPath -> Generator -> ExceptT CompositionError m ()
checkCycles resolve seen path gen =
  mapM_ visit (genUses gen)
  where
    visit u = do
      found <- lift (resolve (packOf (genId gen)) (useGenerator u))
      case found of
        Left _ -> pure () -- reported with its step when (if) the use is expanded
        Right child
          | genId child `elem` seen -> throwE (StepCycle (path ++ [idText child]))
          | otherwise -> checkCycles resolve (genId child : seen) (path ++ [idText child]) child

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
