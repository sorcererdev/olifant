-- | `one_of` (spec §4.1): resolve enum inputs whose choices come from a
-- query over the project, before inputs are parsed.
module Olifant.Generator.Choices (withChoices, needsChoices, ChoiceView (..), choiceViews, maxGroups) where

import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.List (nub, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Generator.Definition
import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Time (UTCTime)
import Olifant.Generator.Inputs (envBefore, inputsUsed, showContext)
import Olifant.Query (Queries, QueryFailure (..), choiceValue, failed, queryVariables, renderQuery, runQuery)
import Olifant.Template (describeTemplateError)

-- | Resolve every `one_of` top to bottom (spec §4.1): each input's query is
-- rendered with the inputs above it (from @sets@) and the derived values
-- built from them, and run over @paths@; choices are sorted, deduplicated,
-- empty values dropped, and remember the context they were found in. An
-- input whose query needs an input above it that has no value waits for it.
withChoices :: Monad m => Queries m -> [FilePath] -> (FilePath -> m (Maybe Text)) -> Map Text Text -> UTCTime -> GeneratorWith s -> m (Either QueryFailure (GeneratorWith s))
withChoices queries paths readFile' sets now gen0 = foldM step (Right gen0) (map inputName (genInputs gen0))
  where
    step (Left f) _ = pure (Left f)
    step (Right gen) name = case [s | s <- genInputs gen, inputName s == name] of
      [InputSpec {inputType = ScalarInput (EnumOf q cv Unresolved)}] -> case lookup q (genQueries gen) of
        Nothing -> pure (Left (failed ("one_of = \"" <> q <> "\": no such query")))
        Just query -> do
          let env = envBefore gen sets now name
              used = inputsUsed (genDerived gen) (queryVariables query)
              overridden = [k | (k, _) <- genDerived gen, k `M.member` sets]
              needed = [n | n <- map inputName (genInputs gen) ++ overridden, n `S.member` used]
              waiting = [n | n <- needed, n `M.notMember` env]
              context = [(n, raw) | n <- needed, Just raw <- [M.lookup n sets]]
              set c = gen {genInputs = [if inputName s == name then s {inputType = ScalarInput (EnumOf q cv c)} else s | s <- genInputs gen]}
          if not (null waiting)
            then pure (Right (set (NeedsInputs waiting)))
            else case renderQuery env query of
              Left err -> pure (Left (failed ("`" <> name <> "`: query `" <> q <> "`: " <> describeTemplateError err)))
              Right q' ->
                runQuery queries paths readFile' q' <&> \case
                  Left f -> Left f {failureWhy = "`" <> name <> "`: query " <> q <> (if null context then "" else " (for " <> showContext context <> ")") <> " failed: " <> failureWhy f}
                  Right records -> do
                    values <- first (failed . describeTemplateError) (traverse (choiceValue cv) records)
                    pure (set (Resolved (sort (nub (filter (not . T.null) values))) context))
      _ -> pure (Right gen)

-- | Does any input still need its `one_of` resolved?
needsChoices :: GeneratorWith s -> Bool
needsChoices gen = not (null [() | InputSpec {inputType = ScalarInput (EnumOf _ _ Unresolved)} <- genInputs gen])

-- | What `show` prints for an enum (spec §4.1): its choices; one group per
-- combination of the inputs it depends on; or only what it depends on.
-- A group whose query failed carries why (`show` must not print it as empty).
data ChoiceView = Flat [Text] | Grouped [Text] [([(Text, Text)], Either Text [Text])] | DependsOn [Text]
  deriving stock (Eq, Show)

-- | Views for every enum input: flat when its choices resolve with @sets@;
-- otherwise grouped by the enums it depends on (at most 'maxGroups' groups),
-- or 'DependsOn' when one of those is not an enum or there are too many.
choiceViews :: Monad m => Queries m -> [FilePath] -> (FilePath -> m (Maybe Text)) -> Map Text Text -> UTCTime -> GeneratorWith s -> m (Either QueryFailure [(Text, ChoiceView)])
choiceViews queries paths readFile' sets now gen0 =
  withChoices queries paths readFile' sets now gen0 >>= \case
    Left f -> pure (Left f)
    Right gen -> Right . concat <$> traverse (view gen) (genInputs gen)
  where
    view gen spec = case inputType spec of
      ScalarInput (EnumS cs) -> pure [(inputName spec, Flat cs)]
      ScalarInput (EnumOf _ _ (Resolved cs _)) -> pure [(inputName spec, Flat cs)]
      ScalarInput (EnumOf _ _ (NeedsInputs ns)) -> case traverse (flatChoices gen) ns of
        Just lists | product (map length lists) <= maxGroups -> do
          let combos = sequence [[(n, c) | c <- cs] | (n, cs) <- zip ns lists]
          groups <- traverse (\combo -> (combo,) <$> choicesWith (M.union (M.fromList combo) sets) (inputName spec)) combos
          pure [(inputName spec, Grouped ns groups)]
        _ -> pure [(inputName spec, DependsOn ns)]
      _ -> pure []
    flatChoices gen n = case [inputType s | s <- genInputs gen, inputName s == n] of
      [ScalarInput (EnumS cs)] -> Just cs
      [ScalarInput (EnumOf _ _ (Resolved cs _))] -> Just cs
      _ -> Nothing
    choicesWith sets' name =
      withChoices queries paths readFile' sets' now gen0 <&> \case
        Left f -> Left (failureWhy f)
        Right g | [ScalarInput (EnumOf _ _ (Resolved cs _))] <- [inputType s | s <- genInputs g, inputName s == name] -> Right cs
        Right _ -> Right []

-- | The most groups `show` lists for one input (spec §4.1).
maxGroups :: Int
maxGroups = 30
