-- | Internal to Olifant.Server: the snippets a cursor may complete, each
-- placed there (its body with tab stops, unfilled: the model fills on
-- resolve), and the one anticipated there, offered unasked as a ghost.
module Olifant.Server.Completion (completionsFor, ghostFor) where

import Control.Monad (forM)
import Data.Map.Strict qualified as M
import Olifant.Actions (Action (..), Completion (..))
import Olifant.App (actionsAt', completeAt, generatorsOf)
import Olifant.Cursor (isSnippet)
import Olifant.Generator (AppliesAt (..), GeneratorWith (..))
import Olifant.Server.Actions (actionEdits)
import Olifant.Server.State (ServerState (..), projectFor)
import Olifant.Snippet (Snippet (..))
import System.FilePath (makeRelative)

-- | The snippets at a position (1-based line, 1-based byte column), each
-- with its placed body; nothing in no project.
completionsFor :: FilePath -> (Int, Int) -> ServerState -> IO (ServerState, [(Completion, Snippet)])
completionsFor path pos s =
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', [])
    (s', Just (root, project)) -> do
      items <- completeAt (ssPorts s') project (makeRelative root path) pos
      placed <- forM items $ \c -> snd <$> actionEdits False path (fst pos) (cpGenerator c) (cpSets c) s'
      pure (s', [(c, sn) | (c, Right (_, Just sn)) <- zip items placed])

-- | The snippet anticipated at a position: the first suggested snippet
-- that applies there (its missing inputs set to their first choice),
-- placed on the cursor's line and short enough to show (8 lines at most).
ghostFor :: FilePath -> (Int, Int) -> ServerState -> IO (ServerState, Maybe Snippet)
ghostFor path pos@(line, _) s =
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', Nothing)
    (s', Just (root, project)) -> do
      acts <- actionsAt' (ssPorts s') project (makeRelative root path) pos
      let suggested = M.fromList [(k, isSnippet g && maybe False apSuggest (genAppliesAt g)) | (k, g) <- generatorsOf project]
          candidates =
            [ (actGenerator a, actSets a ++ chosen)
            | a <- acts
            , M.findWithDefault False (actGenerator a) suggested
            , let firsts = [(k, c) | k <- actMissing a, Just (c : _) <- [lookup k (actChoices a)]]
            , length firsts == length (actMissing a)
            , let chosen = firsts
            ]
          try [] = pure Nothing
          try ((generator, sets) : rest) =
            snd <$> actionEdits False path line generator sets s' >>= \case
              Right (_, Just sn) | snLine sn == line, length (snGhost sn) <= 8 -> pure (Just sn)
              _ -> try rest
      (s',) <$> try candidates
