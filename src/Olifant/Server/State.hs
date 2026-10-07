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
  , brokenFor
  , workspaceOf
  , catalogChanged
  , filesChanged
  , slotOf
  , fileSaved
  , memo
  ) where

import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Olifant.App (Ports (..), Project (..), nearestProject, openProjectWith)
import Olifant.Catalog (PackCache, catalogPackRoots, describeCatalogError, newPackCache)
import Olifant.Process (Cancel, neverCancel)
import Olifant.Generator (Generator)
import Olifant.Recognize (Index, Shingle)
import Olifant.Workspace (Workspace (..), normalisePath, overlayMany, within)
import System.FilePath (takeDirectory, (</>))

-- | The client's settings (`initializationOptions`): how long slow work
-- waits after typing stops, a model to use instead of the projects', and
-- (tests) a file of recorded fills that stands in for the model.
-- | @scRecognizeAnswers@: a file holding a model's recorded answer, which
-- recognition uses instead of the model (tests, as fills use @scFills@).
data ServerConfig = ServerConfig {scDebounceMs :: Int, scModel :: Maybe Text, scFills :: Maybe FilePath, scRecognizeAnswers :: Maybe FilePath}

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig 300 Nothing Nothing Nothing

-- | A project as loaded: its catalog, or why it no longer loads (with the
-- last good one, still served).
data ProjectSlot = Loaded Project | Broken Text (Maybe Project)

data ServerState = ServerState
  { ssPorts :: Ports
  , ssDocs :: Map FilePath (Int, Text)
  , ssProjects :: Map FilePath ProjectSlot
  , ssPackCache :: PackCache
  , ssConfig :: ServerConfig
  , -- | how many times each project was loaded (debugging, tests)
    ssLoads :: Map FilePath Int
  , -- | bumped by every change that can change an answer (an edit, a file
    -- or a project's conventions changing): answers are kept per generation
    ssGeneration :: Int
  , -- | bumped when files may have appeared or gone (saves, the client's
    -- file watching): each project's file listing is kept per stamp
    ssFilesStamp :: Int
  , ssListings :: Map FilePath (Int, [FilePath])
  , -- | answers by (request, file, where), for one generation
    ssMemo :: Map (Text, FilePath, Text) (Int, Dynamic)
  , -- | the token of the work this state was handed to (a job, a request):
    -- its plans' checks, verifies and model calls stop when it trips
    ssCancel :: Cancel
  , -- | each project's recognition index per language, for one file stamp
    ssRecognition :: Map (FilePath, Text) (Int, Index, [Generator])
  , -- | per project root: each generator's dismissals in a row (kept in
    -- .olifant/state/recognition.json)
    ssQuiet :: Map FilePath (Map Text Int)
  , -- | per file: its shape when its recognition was dismissed
    ssDismissed :: Map FilePath (Set Shingle)
  }

newServerState :: Ports -> IO ServerState
newServerState ports = (\cache -> ServerState ports M.empty M.empty cache defaultServerConfig M.empty 0 0 M.empty M.empty neverCancel M.empty M.empty M.empty) <$> newPackCache

-- | A document opened in the editor, at a version, with its text.
openDoc :: FilePath -> Int -> Text -> ServerState -> ServerState
openDoc path version text s =
  (changeDoc path version text s)
    { -- a document not on disk yet is a file the listings lack
      ssFilesStamp = if known then ssFilesStamp s else ssFilesStamp s + 1
    }
  where
    known = or [rel `elem` ps | (root, (_, ps)) <- M.toList (ssListings s), let prefix = root <> "/", prefix `isPrefixOf` path, let rel = drop (length prefix) path]

changeDoc :: FilePath -> Int -> Text -> ServerState -> ServerState
changeDoc path version text s = s {ssDocs = M.insert (normalisePath path) (version, text) (ssDocs s), ssGeneration = ssGeneration s + 1}

-- | Closed: the file is read from disk again.
closeDoc :: FilePath -> ServerState -> ServerState
closeDoc path s = s {ssDocs = M.delete (normalisePath path) (ssDocs s), ssGeneration = ssGeneration s + 1}

-- | Saved: the file is on disk now (perhaps for the first time).
fileSaved :: ServerState -> ServerState
fileSaved s = s {ssFilesStamp = ssFilesStamp s + 1, ssGeneration = ssGeneration s + 1}

-- | An answer computed once per generation: kept by (request, file, where)
-- until anything changes.
memo :: Typeable v => Text -> FilePath -> Text -> (ServerState -> IO (ServerState, v)) -> ServerState -> IO (ServerState, v)
memo request path at work s = case M.lookup key (ssMemo s) of
  Just (generation, answer) | generation == ssGeneration s, Just v <- fromDynamic answer -> pure (s, v)
  _ -> do
    (s', v) <- work s
    let current = M.filter ((== ssGeneration s') . fst) (ssMemo s')
    pure (s' {ssMemo = M.insert key (ssGeneration s', toDyn v) current}, v)
  where
    key = (request, path, at)

slotOf :: FilePath -> ServerState -> Maybe ProjectSlot
slotOf root = M.lookup root . ssProjects

-- | A project's files as the editor has them: its open documents over disk,
-- relative to its root.
workspaceOf :: ServerState -> FilePath -> Workspace IO
workspaceOf s root =
  overlayMany
    (M.fromList [(rel, text) | (path, (_, text)) <- M.toList (ssDocs s), Just rel <- [under path]])
    listed
  where
    disk = within root (pFiles (ssPorts s))
    -- the project's listing, kept while no file may have come or gone
    listed = case M.lookup root (ssListings s) of
      Just (stamp, paths) | stamp == ssFilesStamp s -> disk {listFiles = \dir -> pure (inside dir paths)}
      _ -> disk
    inside dir paths = case normalisePath dir of
      "." -> paths
      d -> [drop (length d + 1) p | p <- paths, (d <> "/") `isPrefixOf` p]
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
      s0 <- case slotOf root s of
        Just _ -> pure s
        Nothing -> load root Nothing s
      s' <- case M.lookup root (ssListings s0) of
        Just (stamp, _) | stamp == ssFilesStamp s0 -> pure s0
        _ -> (\paths -> s0 {ssListings = M.insert root (ssFilesStamp s0, paths) (ssListings s0)}) <$> listFiles (within root (pFiles (ssPorts s0))) "."
      pure (s', (\p -> (root, p {prWorkspace = workspaceOf s' root})) <$> (usable =<< slotOf root s'))

-- | Why the project a file belongs to does not load (loading it if it was
-- not), when it does not; the last good catalog, if any, is still served.
brokenFor :: FilePath -> ServerState -> IO (ServerState, Maybe Text)
brokenFor file s =
  nearestProject (pFiles (ssPorts s)) (pHomeDir (ssPorts s)) (takeDirectory (normalisePath file)) >>= \case
    Nothing -> pure (s, Nothing)
    Just root -> do
      s' <- maybe (load root Nothing s) (const (pure s)) (slotOf root s)
      pure (s', case slotOf root s' of Just (Broken why _) -> Just why; _ -> Nothing)

-- | A project's configuration or packs changed: load it again, keeping the
-- last good catalog when it no longer loads. A project not yet loaded is
-- left to load when asked about.
catalogChanged :: FilePath -> ServerState -> IO ServerState
catalogChanged root s = case slotOf root s of
  Nothing -> pure s
  Just slot -> (\s' -> s' {ssGeneration = ssGeneration s' + 1}) <$> load root (usable slot) s

-- | Files changed on disk (the client's file watching): every loaded
-- project whose `.olifant/` or one of whose packs holds one is loaded
-- again; the others are left as they are.
filesChanged :: [FilePath] -> ServerState -> IO ServerState
filesChanged paths s0 = foldr (\root next -> next >>= catalogChanged root) (pure s) affected
  where
    s = s0 {ssFilesStamp = ssFilesStamp s0 + 1, ssGeneration = ssGeneration s0 + 1}
    affected = [root | (root, slot) <- M.toList (ssProjects s), any (touches root slot) paths]
    touches root slot path =
      (root </> ".olifant") `isAncestorOf` path || any (`isAncestorOf` path) (maybe [] (catalogPackRoots . prCatalog) (usable slot))
    isAncestorOf dir path = (normalisePath dir <> "/") `isPrefixOf` normalisePath path

load :: FilePath -> Maybe Project -> ServerState -> IO ServerState
load root lastGood s =
  openProjectWith (ssPackCache s) (ssPorts s) root <&> \result ->
    s { ssProjects = M.insert root (either (\err -> Broken (describeCatalogError err) lastGood) Loaded result) (ssProjects s)
      , ssLoads = M.insertWith (+) root 1 (ssLoads s)
      }

usable :: ProjectSlot -> Maybe Project
usable = \case
  Loaded p -> Just p
  Broken _ p -> p
