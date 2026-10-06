-- | Internal to Olifant.Server: what the server holds. Open documents (the
-- editor's unsaved text, by absolute path) and the projects they belong to,
-- each loaded the first time one of its files is asked about; a project
-- whose configuration breaks keeps its last good catalog.
module Olifant.Server.State
  ( ServerState (..)
  , ServerConfig (..)
  , defaultServerConfig
  , ProjectSlot (..)
  , newServerState
  , openDoc
  , changeDoc
  , closeDoc
  , projectFor
  , workspaceOf
  , catalogChanged
  , slotOf
  ) where

import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Olifant.App (Ports (..), Project (..), nearestProject, openProjectWith)
import Olifant.Catalog (PackCache, describeCatalogError, newPackCache)
import Olifant.Workspace (Workspace, normalisePath, overlayMany, within)
import System.FilePath (takeDirectory)

-- | The client's settings (`initializationOptions`): how long slow work
-- waits after typing stops, a model to use instead of the projects', and
-- (tests) a file of recorded fills that stands in for the model.
data ServerConfig = ServerConfig {scDebounceMs :: Int, scModel :: Maybe Text, scFills :: Maybe FilePath}

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig 300 Nothing Nothing

-- | A project as loaded: its catalog, or why it no longer loads (with the
-- last good one, still served).
data ProjectSlot = Loaded Project | Broken Text (Maybe Project)

data ServerState = ServerState
  { ssPorts :: Ports
  , ssDocs :: Map FilePath (Int, Text)
  , ssProjects :: Map FilePath ProjectSlot
  , ssPackCache :: PackCache
  , ssConfig :: ServerConfig
  }

newServerState :: Ports -> IO ServerState
newServerState ports = (\cache -> ServerState ports M.empty M.empty cache defaultServerConfig) <$> newPackCache

-- | A document opened in the editor, at a version, with its text.
openDoc :: FilePath -> Int -> Text -> ServerState -> ServerState
openDoc path version text s = s {ssDocs = M.insert (normalisePath path) (version, text) (ssDocs s)}

changeDoc :: FilePath -> Int -> Text -> ServerState -> ServerState
changeDoc = openDoc

-- | Closed: the file is read from disk again.
closeDoc :: FilePath -> ServerState -> ServerState
closeDoc path s = s {ssDocs = M.delete (normalisePath path) (ssDocs s)}

slotOf :: FilePath -> ServerState -> Maybe ProjectSlot
slotOf root = M.lookup root . ssProjects

-- | A project's files as the editor has them: its open documents over disk,
-- relative to its root.
workspaceOf :: ServerState -> FilePath -> Workspace IO
workspaceOf s root =
  overlayMany
    (M.fromList [(rel, text) | (path, (_, text)) <- M.toList (ssDocs s), Just rel <- [under path]])
    (within root (pFiles (ssPorts s)))
  where
    prefix = if root == "/" then "/" else root <> "/"
    under path = if prefix `isPrefixOf` path then Just (drop (length prefix) path) else Nothing

-- | The project a file belongs to (the nearest with `.olifant/`), loaded if
-- it was not, seeing the open documents; nothing for a file in no project,
-- or in one that has never loaded.
projectFor :: FilePath -> ServerState -> IO (ServerState, Maybe (FilePath, Project))
projectFor file s =
  nearestProject (pFiles (ssPorts s)) (pHomeDir (ssPorts s)) (takeDirectory (normalisePath file)) >>= \case
    Nothing -> pure (s, Nothing)
    Just root -> do
      s' <- case slotOf root s of
        Just _ -> pure s
        Nothing -> load root Nothing s
      pure (s', (\p -> (root, p {prWorkspace = workspaceOf s' root})) <$> (usable =<< slotOf root s'))

-- | A project's configuration or packs changed: load it again, keeping the
-- last good catalog when it no longer loads. A project not yet loaded is
-- left to load when asked about.
catalogChanged :: FilePath -> ServerState -> IO ServerState
catalogChanged root s = case slotOf root s of
  Nothing -> pure s
  Just slot -> load root (usable slot) s

load :: FilePath -> Maybe Project -> ServerState -> IO ServerState
load root lastGood s =
  openProjectWith (ssPackCache s) (ssPorts s) root <&> \result ->
    s {ssProjects = M.insert root (either (\err -> Broken (describeCatalogError err) lastGood) Loaded result) (ssProjects s)}

usable :: ProjectSlot -> Maybe Project
usable = \case
  Loaded p -> Just p
  Broken _ p -> p
