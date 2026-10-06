-- | Internal to Olifant.Server: the project model of a document's project
-- (spec §1b), the seam a living-system view composes later.
module Olifant.Server.Model (modelFor) where

import Olifant.App (projectModel)
import Olifant.ProjectModel (ProjectModel)
import Olifant.Server.State

-- | The model of the project a file is in; Nothing in no project.
modelFor :: FilePath -> ServerState -> IO (ServerState, Maybe ProjectModel)
modelFor path s =
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', Nothing)
    (s', Just (_, project)) -> (s',) . Just <$> projectModel (ssPorts s') project
