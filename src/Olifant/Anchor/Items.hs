-- | AST queries (spec §4.5): records for nodes in code, selected with an
-- adapter target (`items_of`) or a raw ast-grep rule, with `each` fields.
module Olifant.Anchor.Items (astQueries) where

import Data.List (nub, sort)
import Data.Maybe (isJust)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Anchor.Resolve (itemsOfTarget)
import Olifant.Language
import Olifant.Query (AstQuery (..), AstSelect (..), Query (..), Record, describeQuery, eachFields)
import Olifant.Template (Value (..), describeTemplateError)

-- | Evaluate an AST query over the files among @paths@ that its `files` glob
-- selects. A file without the target contributes nothing. An unknown target
-- (checked once, whether or not any file matches) or an ambiguous one is an
-- error. A file with no language adapter is skipped when the glob has a
-- wildcard, and an error when the glob names it.
astQueries :: Monad m => Syntax m -> [Language] -> AstQuery -> [FilePath] -> (FilePath -> m (Maybe Text)) -> m (Either Text [Record])
astQueries syntax langs q paths readFile' = case (astSelect q, rawRule) of
  (ItemsOf target, _)
    | not (any (\l -> any (\t -> isJust (matchShape t (T.words target))) (langTargets l)) langs) ->
        pure (Left ("unknown target `" <> target <> "` in any language adapter (targets: " <> T.intercalate ", " (nub [targetPhrase t | l <- langs, t <- langTargets l]) <> ")"))
  (RawRule _, Just (Left err)) -> pure (Left (describeQuery (AstQueryQ q) <> ": " <> describeTemplateError err))
  _ -> fmap concat . sequence <$> traverse one (sort [p | p <- paths, matchesGlob (astFiles q) p])
  where
    rawRule = case astSelect q of
      RawRule rule -> Just (renderRule M.empty rule)
      ItemsOf _ -> Nothing
    wildcard = T.any (== '*') (astFiles q)
    one path =
      case languageFor langs path of
        Nothing
          | wildcard -> pure (Right [])
          | otherwise -> pure (Left ("no language adapter for " <> T.pack path))
        Just lang ->
          readFile' path >>= \case
            Nothing -> pure (Left ("cannot read " <> T.pack path))
            Just content -> fmap (map (record path)) <$> nodes path lang content
    nodes path lang content = case (astSelect q, rawRule) of
      (ItemsOf target, _) ->
        itemsOfTarget syntax lang "items of" (T.words target) content >>= \case
          Left (why, _) -> pure (Left (T.pack path <> ": `" <> target <> "`: " <> why))
          Right Nothing -> pure (Right [])
          Right (Just (_, items)) -> pure (Right items)
      (_, Just (Right value)) -> either (\f -> Left (T.pack path <> ": " <> describeSyntaxFailure f)) Right <$> querySyntax syntax lang value content
      (_, _) -> pure (Right [])
    fields = eachFields (astEach q)
    -- A CRLF file's node text keeps its CRs; records never do.
    record path m =
      let body = T.replace "\r\n" "\n" (matchText m)
       in M.union
            (M.fromList [("text", VText body), ("path", VText (T.pack path)), ("line", VInt (fromIntegral (fst (matchStart m) + 1)))])
            (fields body)
