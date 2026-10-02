-- | `one_of` (spec §4.1): resolve enum inputs whose choices come from a
-- query over the project, before inputs are parsed.
module Olifant.Generator.Choices (withChoices, needsChoices) where

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
import Olifant.Generator.Inputs (envBefore, inputsUsed, showContext)
import Olifant.Query (Queries, QueryFailure (..), choiceValue, failed, queryVariables, renderQuery, runQuery)
import Olifant.Template (describeTemplateError)

-- | Resolve every `one_of` top to bottom (spec §4.1): each input's query is
-- rendered with the inputs above it (from @sets@) and the derived values
-- built from them, and run over @paths@; choices are sorted, deduplicated,
-- empty values dropped, and remember the context they were found in. An
-- input whose query needs an input above it that has no value waits for it.
withChoices :: Monad m => Queries m -> [FilePath] -> (FilePath -> m (Maybe Text)) -> Map Text Text -> GeneratorWith s -> m (Either QueryFailure (GeneratorWith s))
withChoices queries paths readFile' sets gen0 = foldM step (Right gen0) (map inputName (genInputs gen0))
  where
    step (Left f) _ = pure (Left f)
    step (Right gen) name = case [s | s <- genInputs gen, inputName s == name] of
      [InputSpec {inputType = ScalarInput (EnumOf q cv Unresolved)}] -> case lookup q (genQueries gen) of
        Nothing -> pure (Left (failed ("one_of = \"" <> q <> "\": no such query")))
        Just query -> do
          let env = envBefore gen sets name
              used = inputsUsed (genDerived gen) (queryVariables query)
              needed = [n | n <- map inputName (genInputs gen), n `S.member` used]
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
