-- | AST queries (spec §4.5): records for nodes in code, selected with an
-- adapter target (`items_of`) or a raw ast-grep rule, with `each` fields.
module Olifant.Anchor.Items (astQueries) where

import Data.List (sort)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Anchor.Resolve (itemsOfTarget)
import Olifant.Language
import Olifant.Query (AstQuery (..), AstSelect (..), Record, eachFields)
import Olifant.Template (Value (..), describeTemplateError)

-- | Evaluate an AST query over the files among @paths@ that its `files` glob
-- selects. A file without the target contributes nothing; an unknown or
-- ambiguous target, or a file with no language adapter, is an error.
astQueries :: Monad m => Syntax m -> [Language] -> AstQuery -> [FilePath] -> (FilePath -> m (Maybe Text)) -> m (Either Text [Record])
astQueries syntax langs q paths readFile' = fmap concat . sequence <$> traverse one (sort [p | p <- paths, matchesGlob (astFiles q) p])
  where
    one path =
      readFile' path >>= \case
        Nothing -> pure (Left ("cannot read " <> T.pack path))
        Just content -> case languageFor langs path of
          Nothing -> pure (Left ("no language adapter for " <> T.pack path))
          Just lang -> fmap (map (record path)) <$> nodes path lang content
    nodes path lang content = case astSelect q of
      ItemsOf target ->
        itemsOfTarget syntax lang "items of" (T.words target) content >>= \case
          Left (why, _) -> pure (Left (T.pack path <> ": `" <> target <> "`: " <> why))
          Right Nothing -> pure (Right [])
          Right (Just (_, items)) -> pure (Right items)
      RawRule rule -> case renderRule M.empty rule of
        Left err -> pure (Left (describeTemplateError err))
        Right value -> either (\f -> Left (T.pack path <> ": " <> describeSyntaxFailure f)) Right <$> querySyntax syntax lang value content
    record path m =
      M.union
        (M.fromList [("text", VText (matchText m)), ("path", VText (T.pack path)), ("line", VInt (fromIntegral (fst (matchStart m) + 1)))])
        (eachFields (astEach q) (matchText m))
