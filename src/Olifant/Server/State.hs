-- | Internal to Olifant.Server: what the server holds. Open documents (the
-- editor's unsaved text, by absolute path) and the projects they belong to,
-- each loaded the first time one of its files is asked about; a project
-- whose configuration breaks keeps its last good catalog.
module Olifant.Server.State
  ( ServerState (..)
  , appendStepLine
  , cachedBase
  , deciderFor
  , deciderUnder
  , decision
  , decisionsNow
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
import Data.Set qualified as S
import Olifant.Decide (Answer, Decider (..), Question, curlDecider, jevDecider, recordedDecider)
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, finally, try)
import Control.Monad (void, when)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime)
import Olifant.App (Ports (..), Project (..), nearestProject, openProjectWith)
import Olifant.Fill (ModelFiller)
import Olifant.Catalog (Catalog (..), PackCache, catalogPackRoots, describeCatalogError, newPackCache)
import Olifant.Model (Backend (..), ModelConfig (..))
import Olifant.Process (Cancel, neverCancel)
import Olifant.Server.ConjureNext (PausedConjure)
import Olifant.Server.LoopTypes (ReconcileAnswer)
import Olifant.Generator (Generator)
import Olifant.Recognize (Index, Shingle)
import Olifant.Workspace (Workspace (..), normalisePath, overlayMany, within)
import System.FilePath (takeDirectory, (</>))

-- | The client's settings (`initializationOptions`): how long slow work
-- waits after typing stops, a model to use instead of the projects', and
-- (tests) a file of recorded fills that stands in for the model.
-- | @scDecideAnswers@: a file of recorded decisions, which the decider
-- (recognition's too) answers from instead of the model (tests, as fills
-- use @scFills@).
-- @scTestScript@: a file of scripted test verdicts, which the server's
-- test runner answers from instead of running anything (tests).
-- @scKeys@: the editor's keys by name (`plan_review`: "<leader>mpr"), which
-- the lenses that offer them name.
data ServerConfig = ServerConfig {scDebounceMs :: Int, scModel :: Maybe Text, scFills :: Maybe FilePath, scDecideAnswers :: Maybe FilePath, scTestScript :: Maybe FilePath, scKeys :: [(Text, Text)]}

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig 300 Nothing Nothing Nothing Nothing []

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
  , -- | per project root: the files it had when first listed (recognition
    -- is for files new since)
    ssEstablished :: Map FilePath (Set FilePath)
  , -- | base texts (root, ref, path), each with the files stamp it was read at:
    -- kept until files change (a save, a pull, a rebase)
    ssBaseCache :: IORef (Map (FilePath, Text, FilePath) ((Int, Text), Either Text (Text, Maybe Text)))
  , -- | typed decisions made (by file and what they were about) and those
    -- being asked, in one place (one atomic step tells known from asked);
    -- and what to do when one is made (the editor pulls diagnostics again)
    ssDecisions :: IORef (Map (FilePath, Text) (Text, Answer), Set (FilePath, Text))
  , ssOnDecided :: IORef (IO ())
  , -- | a document as the editor has it now (its version and text), not as
    -- this state saw it (the adapter's live documents): long work (a conjure)
    -- asks it before its result lands. None: this state's documents are the truth
    ssLiveDoc :: Maybe (FilePath -> IO (Maybe (Int, Text)))
  , -- | a filler that answers instead of any model (tests)
    ssTestFiller :: Maybe (ModelFiller IO)
  , -- | paused conjures by token (`conjure-N`), and the last N given: an
    -- IORef, so a pause outlives the snapshot of the request that made it
    ssPaused :: IORef (Int, Map Text PausedConjure)
  , -- | per plan: the last reconcile's situation (its key) and its answer
    ssReconciled :: Map FilePath (Text, ReconcileAnswer)
  , -- | per (language, path): the units found in a text, kept while the file's
    -- text is the same (anchor completion asks for many files per keystroke)
    ssUnitCache :: IORef (Map (Text, FilePath) (Text, Dynamic))
  }

newServerState :: Ports -> IO ServerState
newServerState ports = do
  cache <- newPackCache
  bases <- newIORef M.empty
  decisions <- newIORef (M.empty, S.empty)
  onDecided <- newIORef (pure ())
  paused <- newIORef (0, M.empty)
  units <- newIORef M.empty
  pure (ServerState ports M.empty M.empty cache defaultServerConfig M.empty 0 0 M.empty M.empty neverCancel M.empty M.empty M.empty M.empty bases decisions onDecided Nothing Nothing paused M.empty units)

-- | A file's base text, from the cache while no file changed since.
cachedBase :: ServerState -> FilePath -> Text -> FilePath -> IO (Either Text (Text, Maybe Text))
cachedBase s root ref path = do
  -- good while the files are as they were and git's state is too (a
  -- checkout or pull with no file event, as on Linux Neovim, moves the base)
  git <- pBaseStamp (ssPorts s) root ref
  known <- M.lookup (root, ref, path) <$> readIORef (ssBaseCache s)
  case known of
    Just ((stamp, gitWas), answer) | stamp == ssFilesStamp s, gitWas == git -> pure answer
    _ -> do
      answer <- pBaseText (ssPorts s) root ref path
      modifyIORef' (ssBaseCache s) (M.insert (root, ref, path) ((ssFilesStamp s, git), answer))
      pure answer

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
        _ -> do
          paths <- listFiles (within root (pFiles (ssPorts s0))) "."
          -- what the project already had: under git, the files it tracks (a
          -- file written before the server started is still new); else what
          -- was there when first listed
          established <- case M.lookup root (ssEstablished s0) of
            Just known -> pure known
            Nothing -> fromMaybe (S.fromList paths) <$> pTracked (ssPorts s0) root
          pure s0 {ssListings = M.insert root (ssFilesStamp s0, paths) (ssListings s0), ssEstablished = M.insert root established (ssEstablished s0)}
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
      -- the commands' step log (`.olifant/state/`) is no configuration
      ((root </> ".olifant") `isAncestorOf` path && not ((root </> ".olifant/state") `isAncestorOf` path)) || any (`isAncestorOf` path) (maybe [] (catalogPackRoots . prCatalog) (usable slot))
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

-- | The typed decider a project asks, if it names a model ([decide] model,
-- or [recognize] model): the configured backend (a Jev backend batches
-- each askAll in one call and notes each call in the project's step log),
-- or (tests) recorded answers. Under no token: a decision outlives the
-- request that asked it (it is answered in the background); the model's
-- timeout bounds it.
deciderFor :: ServerState -> Project -> Maybe (Decider IO)
deciderFor = deciderUnder neverCancel

-- | The decider under a token: recognition's unsure band asks under the
-- work's token, so typing on cancels it.
deciderUnder :: Cancel -> ServerState -> Project -> Maybe (Decider IO)
deciderUnder token s project = do
  name <- catalogDecideModel (prCatalog project)
  case scDecideAnswers (ssConfig s) of
    Just recorded -> Just (recordedDecider recorded token)
    Nothing -> do
      let models = catalogModel (prCatalog project)
      backend <- lookup name (mcBackends models)
      Just $ case backend of
        Jev {} -> jevDecider token (mcTimeout models) backend (appendStepLine (ssPorts s) (prRoot project) . T.splitOn "\t")
        _ -> curlDecider token (mcTimeout models) backend

-- | One line of the step log (`.olifant/state/steps.log`, ignored by git):
-- the time, then FIELDS, tab-separated, each on one line. A log that
-- cannot be written never stops a move.
appendStepLine :: Ports -> FilePath -> [Text] -> IO ()
appendStepLine ports root fields = do
  now <- pNow ports
  let path = root </> ".olifant/state/steps.log"
      entry = T.intercalate "\t" (T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now) : map (T.unwords . T.words) fields)
  void (try (readText (pFiles ports) path >>= \old -> writeText (pFiles ports) path (fromMaybe "" old <> entry <> "\n")) :: IO (Either SomeException ()))

-- | The decisions made, and those being asked (tests).
decisionsNow :: ServerState -> IO (Map (FilePath, Text) (Text, Answer), Set (FilePath, Text))
decisionsNow s = readIORef (ssDecisions s)

-- | A decision about one thing (a file's function, by question kind and
-- name: KEY): its latest answer, if any. Asked in the background at most
-- once at a time, and again (after that one lands) when the thing's text
-- has changed since its answer (FP): typing in a function asks once per
-- model round trip, not once per keystroke. ON_DONE runs with each answer.
-- What is known and whether to ask are one atomic step: callers at once
-- see the answer, or that it is being asked, and only one asks.
decision :: ServerState -> Decider IO -> (FilePath, Text) -> Text -> Question -> IO () -> IO (Maybe Answer)
decision s decider key fp q onDone = do
  (known, fresh) <- atomicModifyIORef' (ssDecisions s) $ \(answers, asking) ->
    let known = M.lookup key answers
        fresh = fmap fst known /= Just fp && S.notMember key asking
     in ((answers, if fresh then S.insert key asking else asking), (known, fresh))
  when fresh . void . forkIO $
    -- a failure is no answer: nothing kept, asked again on a later pull
    ( try (ask decider q) >>= \case
        Left (_ :: SomeException) -> pure ()
        Right answer -> do
          -- answers land at once from many threads (one question per survivor)
          atomicModifyIORef' (ssDecisions s) (\(answers, asking) -> ((M.insert key (fp, answer) answers, asking), ()))
          onDone
    )
      `finally` atomicModifyIORef' (ssDecisions s) (\(answers, asking) -> ((answers, S.delete key asking), ()))
  pure (snd <$> known)

