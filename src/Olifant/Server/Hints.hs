-- | Internal to Olifant.Server: where in a file actions apply, line by line
-- (for the ⚡ hint: the first one's label, and how many more), and the
-- generators that nearly apply (and why not).
module Olifant.Server.Hints (LineHint (..), hintsFor) where

import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Olifant.Actions (Action (..), FileAction (..), Nearby, Range (..))
import Olifant.App (actionsIn, generatorsOf)
import Olifant.Cursor (isSnippet)
import Olifant.Server.State (ServerState (..), memo, projectFor)
import System.FilePath (makeRelative)

-- | A line's ⚡ hint: its line (1-based), the actions there (snippets
-- last: they show as ghosts, not hints) and whether each is a snippet.
data LineHint = LineHint {lhLine :: Int, lhActions :: [(Action, Bool)]}
  deriving stock (Eq, Show)

-- | Every line of a file with actions (whole-file actions are never
-- hinted), and the generators that nearly apply; nothing in no project.
hintsFor :: FilePath -> ServerState -> IO (ServerState, ([LineHint], [Nearby]))
hintsFor path = memo "hints" path "" $ \s ->
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', ([], []))
    (s', Just (root, project)) -> do
      (acts, near) <- actionsIn (ssPorts s') project (makeRelative root path)
      let snippets = M.fromList [(k, isSnippet g) | (k, g) <- generatorsOf project]
          snippet a = M.findWithDefault False (actGenerator a) snippets
          byLine = M.fromListWith (flip (++)) [(fst (rangeStart r), [(a, snippet a)]) | FileAction a (Just r) <- acts]
      pure (s', ([LineHint l (sortOn snd as) | (l, as) <- M.toList byLine], near))
