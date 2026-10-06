-- | Internal to Olifant.Server: a file's rule findings (with their fixes'
-- open choices), and a fix planned as edits — always against the
-- documents as they are now.
module Olifant.Server.Fixes (findingsFor, fixEdits) where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.App (FixFailure (..), PlanOptions (..), findings, fixSteps, planOptions, planWith)
import Olifant.Catalog (describeCatalogError)
import Olifant.Generation (Plan (..), describeConflict, describeGenerationError)
import Olifant.Rules (Finding)
import Olifant.Server.Edits (FileEdit, editsOfPlan)
import Olifant.Server.State (ServerState (..), projectFor)
import System.FilePath (makeRelative)

-- | The rules' findings in a file (project-relative paths in them); none
-- for a file in no project.
findingsFor :: FilePath -> ServerState -> IO (ServerState, [Finding])
findingsFor path s =
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', [])
    (s', Just (root, project)) -> (s',) . fst <$> findings (ssPorts s') project [makeRelative root path]

-- | The edits of a rule's fix (the @n@th, 1-based) at a line of a file,
-- with the choices made: planned now, so a finding that moved or went
-- away says so instead of editing stale text.
fixEdits :: FilePath -> Text -> Int -> Int -> [(Text, Text)] -> ServerState -> IO (ServerState, Either Text [FileEdit])
fixEdits path rule line n sets s =
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', Left (T.pack path <> " is in no Olifant project"))
    (s', Just (root, project)) ->
      fmap (s',) $
        fixSteps (ssPorts s') project rule (makeRelative root path) line n sets >>= \case
          Left failure -> pure (Left (describeFix failure))
          Right (context, steps) ->
            first describeGenerationError <$> planWith (ssPorts s') project planOptions {poContext = context} steps >>= \case
              Left why -> pure (Left why)
              Right plan
                | not (null (planConflicts plan)) -> pure (Left (T.intercalate "; " (map describeConflict (planConflicts plan))))
                | otherwise -> pure (Right (editsOfPlan s' root plan))
  where
    describeFix = \case
      FixNotFound why -> why <> " (the code changed since: ask for the actions again)"
      FixUsage why -> why
      FixCatalog err -> describeCatalogError err
      FixFailed why -> why
