-- | Internal to Olifant.Server: owned regions kept as files are saved (spec
-- §5.8) — every region of the project refreshed against the documents as
-- the editor has them, as edits.
module Olifant.Server.Regions (regionEdits) where

import Control.Monad (forM)
import Data.Map.Strict qualified as M
import Olifant.App (Project (..), projectTools)
import Olifant.Generation.Regions (RegionMode (..), refreshRegions)
import Olifant.Server.Edits (FileEdit (..), smallestChange)
import Olifant.Server.State (ServerState (..), projectFor)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath ((</>))

-- | The edits that bring the regions of a file's project up to date; none
-- in no project, or when every region is current (regions not set up yet
-- are left for `sync --init`).
regionEdits :: FilePath -> ServerState -> IO (ServerState, [FileEdit])
regionEdits path s =
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', [])
    (s', Just (root, project)) -> do
      (_, _, current) <- refreshRegions SkipUnset (projectTools (ssPorts s') project) (prWorkspace project) 1 M.empty
      changed <- forM (M.toList current) $ \(rel, new) -> (rel,,new) <$> readText (prWorkspace project) rel
      pure
        ( s'
        , [ FileEdit abs' (fst <$> M.lookup abs' (ssDocs s')) False range text
          | (rel, Just old, Just new) <- changed
          , old /= new
          , let abs' = normalisePath (root </> rel)
                (range, text) = smallestChange old new
          ]
        )
