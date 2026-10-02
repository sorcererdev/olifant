-- | `one_of` (spec §4.1): resolve enum inputs whose choices come from a
-- query over the project, before inputs are parsed.
module Olifant.Generator.Choices (withChoices, needsChoices) where

import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.List (nub, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Generator.Definition
import Olifant.Query (Queries, QueryFailure (..), choiceValue, failed, runQuery)
import Olifant.Template (describeTemplateError)

-- | Fill every unresolved `one_of` with the values its query finds in
-- @paths@ (sorted, deduplicated, empty values dropped).
withChoices :: Monad m => Queries m -> [FilePath] -> (FilePath -> m (Maybe Text)) -> GeneratorWith s -> m (Either QueryFailure (GeneratorWith s))
withChoices commands paths readFile' gen = do
  inputs <- traverse resolve (genInputs gen)
  pure ((\is -> gen {genInputs = is}) <$> sequence inputs)
  where
    resolve spec = case inputType spec of
      ScalarInput (EnumOf name cv Unresolved) -> case lookup name (genQueries gen) of
        Nothing -> pure (Left (failed ("one_of = \"" <> name <> "\": no such query")))
        Just q ->
          runQuery commands paths readFile' q <&> \case
            Left f -> Left f {failureWhy = "`" <> inputName spec <> "`: " <> failureWhy f}
            Right records -> do
              values <- first (failed . describeTemplateError) (traverse (choiceValue cv) records)
              pure spec {inputType = ScalarInput (EnumOf name cv (Resolved (sort (nub (filter (not . T.null) values))) []))}
      _ -> pure (Right spec)

-- | Does any input still need its `one_of` resolved?
needsChoices :: GeneratorWith s -> Bool
needsChoices gen = not (null [() | InputSpec {inputType = ScalarInput (EnumOf _ _ Unresolved)} <- genInputs gen])
