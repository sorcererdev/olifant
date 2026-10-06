-- | Internal to Olifant.Server: the ⚡ actions at a cursor, and an action
-- planned there (a snippet placed at the cursor, as `gen --at` does) with
-- the choices made — against the documents as they are now.
module Olifant.Server.Actions (actionsFor, actionEdits) where

import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Actions (Action (..))
import Olifant.App (PlanOptions (..), Ports (..), Project (..), actionsAt', generatorsOf, placeAt, planOptions, planWith)
import Olifant.Catalog (describeCatalogError, findEntry, loadGenerator)
import Olifant.Cursor (isSnippet)
import Olifant.Generation (Plan (..), Step (..), describeConflict, describeGenerationError)
import Olifant.Snippet (Snippet)
import Olifant.Server.Edits (FileEdit, editsOfPlan)
import Olifant.Server.State (ServerState (..), projectFor)
import System.FilePath (makeRelative)

-- | The actions at a position (1-based line, 1-based byte column) of a
-- file, each with whether it is a snippet; none in no project.
actionsFor :: FilePath -> (Int, Int) -> ServerState -> IO (ServerState, [(Action, Bool)])
actionsFor path pos s =
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', [])
    (s', Just (root, project)) -> do
      acts <- actionsAt' (ssPorts s') project (makeRelative root path) pos
      let snippets = M.fromList [(k, isSnippet g) | (k, g) <- generatorsOf project]
      pure (s', [(a, M.findWithDefault False (actGenerator a) snippets) | a <- acts])

-- | A generator planned at a cursor (1-based line) with its inputs set: the
-- edits, and the snippet it placed at the cursor, if it is one.
actionEdits :: FilePath -> Int -> Text -> [(Text, Text)] -> ServerState -> IO (ServerState, Either Text ([FileEdit], Maybe Snippet))
actionEdits path line generator sets s =
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', Left (T.pack path <> " is in no Olifant project"))
    (s', Just (root, project)) -> fmap (s',) $ do
      let ports = ssPorts s'
          rel = makeRelative root path
      case findEntry (prCatalog project) generator of
        Left err -> pure (Left (describeCatalogError err))
        Right entry ->
          loadGenerator (pFiles ports) (pSyntax ports) (prCatalog project) entry >>= \case
            Left err -> pure (Left (describeCatalogError err))
            Right gen0 ->
              placeAt ports project (rel, line) [gen0] >>= \case
                Left why -> pure (Left why)
                Right gens ->
                  planWith ports project planOptions {poAt = Just (rel, line)} [Step g (M.fromList sets) | g <- gens] >>= \case
                    Left err -> pure (Left (describeGenerationError err))
                    Right plan
                      | not (null (planConflicts plan)) -> pure (Left (T.intercalate "; " (map describeConflict (planConflicts plan))))
                      | otherwise -> pure (Right (editsOfPlan s' root plan, planSnippet plan))
