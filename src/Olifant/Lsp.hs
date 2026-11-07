{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

-- | `olifant lsp` (olifant lsp spec §2): the language server, speaking LSP
-- over stdio. This module only maps LSP messages onto "Olifant.Server",
-- which holds the projects, the open documents and the jobs.
module Olifant.Lsp (runLsp, runLspWithHandles, serverDefinition) where

import Data.Char (isSpace)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar, tryPutMVar, tryTakeMVar)
import Control.Exception (SomeAsyncException, SomeException, catch, displayException, finally, fromException, throwIO, try)
import Olifant.Language.Grammar (GrammarUnavailable (..))
import Olifant.Process (Cancel, cancelled, neverCancel, newCancel)
import Olifant.Recognize qualified as Recognize
import Olifant.Decide qualified as De
import Olifant.Generator (GeneratorId (..))
import Control.Lens ((%~), (&), (.~), (?~), (^.))
import Control.Applicative ((<|>))
import Control.Monad (forM_, guard, join, unless, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (Bool, Number, Object, String), object, toJSON, (.=))
import Colog.Core qualified as L
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text.Encoding qualified as TE
import Data.Functor ((<&>))
import System.IO (Handle, BufferMode (NoBuffering), hFlush, hPutStrLn, hSetBinaryMode, hSetBuffering, stderr, stdin, stdout)
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Olifant.Plans (Anchor (..), Kind (..), Line (..), Verdict (..), anchorText, childrenOf, excerptInfo, excerptSpans, parsePlan, stepDone, testState, withVerdict)
import Olifant.Server.Unit (Named (..))
import Olifant.Server.Claims (checkedKeys, claimKey, claimsOf, passWork)
import Olifant.Server.Claims qualified as Claims
import Olifant.Server.Loop (PassAnswer (..), claimPass)
import Data.Text qualified as T
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, isInfixOf, nub)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Proxy (Proxy (..))
import Olifant.Language (scriptFileRunner)
import Data.Scientific (toBoundedInteger)
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server hiding (ServerConfig (..))
import Language.LSP.Server qualified as Lsp
import Language.LSP.VFS (virtualFileText, virtualFileVersion)
import Olifant.App (Ports (..))
import Olifant.Server.Excerpt (Excerpt (..), excerptFacts, excerptsOf, renderFence, unitExcerpt, unitHover)
import Olifant.Server.LoopTests (checkbox)
import Olifant.Server.Loop (ReconcileAnswer (..), StepStub (..), StepTest (..), baseline, factJson, reconcile, reconcileJson, stepStub, stepTest, stepTestFacts)
import Olifant.Server.ReviewReadings (Choice (..), Reading (..), ReadingsReview (..), answerReading, applyFactsText, choiceOf, readingEvent, dropReadings, entryKind, loadReadings, outcomeOf, readingFacts, readingKey, readingLens, readingRow, readingsId, readingsPayload, replaceReadings, saveReadings)
import Olifant.Server.Telemetry (OutcomeRecord (..), Record (..), appendRecord)
import Olifant.Server.Review (Change (..), FileChange (..), Review (..), allowInConfig, allowPattern, archiveErrand, changeSince, dropReview, errandPayload, keepLines, keepTicked, loadArchive, loadArchiveIndex, loadArchived, loadReview, loadReviews, locateStep, revertChange, pastDosAt, reopenable, reviewPayload, sameStep, saveReview, testsOfFacts)
import Olifant.Server.Journal (Entry (..), Event (..), keepEvent, movedEvent, withJournal)
import Olifant.Server.Journal qualified as Journal
import Olifant.Transcripts (slugOf)
import Data.Time (getCurrentTimeZone)
import System.Directory (getHomeDirectory)
import Olifant.Server.LoopTypes (Fact (..))
import Olifant.Server.Plans (Reopen (..), claimHover, planTokensWith)
import System.FilePath (makeRelative, takeFileName, (</>))
import Olifant.Generation (Plan (..), ProgressEvent)
import Olifant.Output (doctorJson, fileActionsJson, listJson, planJson, showJson)
import Olifant.App (Project (..), ShowFailure (..), doctorChecks, inMenu, listGenerators, migrationFor, showGenerator, subsOf)
import Olifant.Catalog (Catalog (..), PlansConfig (..), catalogPackRoots, describeCatalogError)
import Olifant.Actions (Action (..), Completion (..), Nearby (..))
import Olifant.Rules (Finding (..), FixOffer (..))
import Olifant.Snippet (Snippet (..))
import Olifant.Rules qualified as R
import Olifant.Workspace (Workspace (..), normalisePath)
import Olifant.Mutate (Mutant (..))
import Olifant.Server
import Olifant.Migration (MigrationReport, migrationJson)
import Data.Foldable qualified as F
import Olifant.Preview (Field (..), Source (..))
import Control.Concurrent.Async qualified as Async

-- | The server's state behind a lock, its background jobs, the plans of
-- the suggestions it has offered (by id, for `olifant/plan`), and whether
-- the test-only `olifant/debugState` answers.
data Env = Env
  { envPorts :: Ports
  , envState :: MVar ServerState
  , envJobs :: Jobs
  , envOffered :: IORef (Int, M.Map Int Offered)
  , envDebug :: IORef Bool
  , -- | the globs the client already watches for us
    envWatched :: IORef (S.Set T.Text)
  , -- | the requests running in the background, by id: how to cancel each
    envRunning :: IORef (M.Map T.Text (IO ()))
  , -- | the other files' region edits a save's willSaveWaitUntil worked
    -- out, for its didSave to send (one refresh per save)
    envSaving :: IORef (M.Map FilePath [FileEdit])
  , -- | the token of the work this Env was handed to (a job, a request);
    -- the work's snapshots carry it (`ssCancel`)
    envCancel :: Cancel
  , -- | held while a plan's `do` runs: one edits the tree at a time
    envDoing :: MVar ()
  , -- | each plan's anchor fingerprints as the diagnostics pass last read
    -- them: what its semantic tokens mark stale
    envPlanPrints :: IORef (M.Map FilePath AnchorPrints)
  , -- | the grammar stops already shown in a message: once per text per session
    envStops :: IORef (S.Set T.Text)
  , -- | each plan's running claim pass (its number and how to cancel it),
    -- and the last number given
    envPasses :: IORef (Int, M.Map FilePath (Int, IO ()))
  }

-- | A suggestion's plan, for the document version it was planned against.
data Offered = Offered {ofPath :: FilePath, ofVersion :: Int, ofSuggestion :: Suggestion}

-- | The server over stdin and stdout (what `olifant lsp` runs).
runLsp :: Ports -> IO Int
runLsp ports = runLspWithHandles ports stdin stdout

-- | The server over two handles, from the client and to it.
runLspWithHandles :: Ports -> Handle -> Handle -> IO Int
runLspWithHandles ports hin hout = do
  mapM_ (\h -> hSetBuffering h NoBuffering >> hSetBinaryMode h True) [hin, hout]
  definition <- serverDefinition ports
  runServerWithConfig
    Lsp.ServerConfig
      { Lsp.ioLogger = problems
      , Lsp.lspLogger = L.hoistLogAction liftIO problems
      , Lsp.inwards = BS.hGetSome hin 65536
      , Lsp.outwards = \out -> BSL.hPut hout out >> hFlush hout
      , Lsp.prepareOutwards = prependHeader . advertise
      , Lsp.parseInwards = parseHeaders
      }
    definition
  where
    -- errors to stderr (never stdout: that is the protocol); a client
    -- without settings for us is not one (the library always asks)
    problems = L.LogAction $ \msg ->
      let text = show (L.getMsg msg)
       in if L.getSeverity msg == L.Error && not ("BadConfigurationResponse" `isInfixOf` text) then hPutStrLn stderr ("olifant lsp: " <> text) else pure ()

-- | The initialize response, with what the protocol library cannot yet
-- say: inline completion (LSP 3.18), which the server answers.
advertise :: BSL.ByteString -> BSL.ByteString
advertise out
  | not ("\"capabilities\"" `BS.isInfixOf` BSL.toStrict out) = out
  | otherwise = case Aeson.decode out of
      Just (Object o)
        | Just (Object result) <- KM.lookup "result" o
        , Just (Object caps) <- KM.lookup "capabilities" result ->
            -- inline completion (lsp-types predates it); semantic tokens with
            -- the plan legend (only plans have tokens: full, no range, no delta)
            let legend = object ["tokenTypes" .= planTokenTypes, "tokenModifiers" .= planTokenModifiers]
                caps' = KM.insert "semanticTokensProvider" (object ["legend" .= legend, "full" .= True]) (KM.insert "inlineCompletionProvider" (Bool True) caps)
             in Aeson.encode (Object (KM.insert "result" (Object (KM.insert "capabilities" (Object caps') result)) o))
      _ -> out

serverDefinition :: Ports -> IO (ServerDefinition ())
serverDefinition ports = do
  state <- newServerState ports >>= newMVar
  -- a conjure asks the document's version as the editor has it now, not as
  -- its snapshot saw it, before its edits land
  modifyMVar_ state (\s -> pure s {ssLiveDoc = Just (\path -> M.lookup path . ssDocs <$> readMVar state)})
  env <- Env ports state <$> newJobs <*> newIORef (0, M.empty) <*> newIORef False <*> newIORef S.empty <*> newIORef M.empty <*> newIORef M.empty <*> pure neverCancel <*> newMVar () <*> newIORef M.empty <*> newIORef S.empty <*> newIORef (0, M.empty)
  pure
    ServerDefinition
      { parseConfig = \_ _ -> Right ()
      , onConfigChange = const (pure ())
      , defaultConfig = ()
      , configSection = "olifant"
      , doInitialize = \lspEnv req -> configure env (req ^. L.params . L.initializationOptions) >> pure (Right lspEnv)
      , staticHandlers = \_ -> guarded env (handlers env)
      , interpretHandler = \lspEnv -> Iso (runLspT lspEnv) liftIO
      , options = defaultOptions {optTextDocumentSync = Just sync, optServerInfo = Just (ServerInfo "olifant" Nothing), optCompletionTriggerCharacters = Just ['@', ':'], optExecuteCommandCommands = Just ["olifant.doctor"]}
      }
  where
    -- open/close and incremental changes; saves are noticed (regions)
    sync = TextDocumentSyncOptions (Just True) (Just TextDocumentSyncKind_Incremental) (Just False) (Just True) (Just (InR (SaveOptions (Just False))))

-- | `initializationOptions`: `debounce_ms`, `model`, `keys` (the editor's
-- keys by name, which lenses name), and `debug` (tests).
configure :: Env -> Maybe Value -> IO ()
configure env = \case
  Just (Object o) -> do
    writeIORef (envDebug env) (KM.lookup "debug" o == Just (Bool True))
    modifyMVar_ (envState env) $ \s ->
      pure
        s
          { ssConfig =
              (ssConfig s)
                { scDebounceMs = maybe (scDebounceMs (ssConfig s)) id (KM.lookup "debounce_ms" o >>= \case Number n -> toBoundedInteger n; _ -> Nothing)
                , scModel = case KM.lookup "model" o of Just (String m) -> Just m; _ -> scModel (ssConfig s)
                , scFills = case KM.lookup "fills" o of Just (String f) -> Just (T.unpack f); _ -> scFills (ssConfig s)
                , -- recognize_answers: an alias of decide_answers (one answers file)
                  scDecideAnswers = case (KM.lookup "decide_answers" o, KM.lookup "recognize_answers" o) of
                    (Just (String f), _) -> Just (T.unpack f)
                    (_, Just (String f)) -> Just (T.unpack f)
                    _ -> scDecideAnswers (ssConfig s)
                , scTestScript = script s
                , -- chat_answers: a file of recorded answers the chat rungs give (tests)
                  scChatAnswers = case KM.lookup "chat_answers" o of
                    Just (String f) -> Just (T.unpack f)
                    _ -> scChatAnswers (ssConfig s)
                , scKeys = case KM.lookup "keys" o of
                    Just (Object ks) -> [(Key.toText k, v) | (k, String v) <- KM.toList ks]
                    _ -> scKeys (ssConfig s)
                }
          , -- (tests) scripted verdicts stand in for the test runner
            ssPorts = maybe (ssPorts s) (\f -> (ssPorts s) {pTester = const (scriptFileRunner f), pTesterWithin = \_ _ -> scriptFileRunner f}) (script s)
          }
      where
        script s = case KM.lookup "test_script" o of Just (String f) -> Just (T.unpack f); _ -> scTestScript (ssConfig s)
  _ -> pure ()

-- | Every handler, kept from killing the server: lsp runs them on its
-- message loop without catching, so an IO exception (a file deleted while
-- read, an unreadable folder) would end the process (review I4). A
-- request answers an internal error instead; a notification is logged. A
-- grammar that cannot load ('GrammarUnavailable') answers its stop text as
-- a failed request, and is shown once ('grammarStopped').
guarded :: Env -> Handlers (LspM ()) -> Handlers (LspM ())
guarded env = mapHandlers onRequest onNotification
  where
    onRequest h req respond =
      rescue (h req respond) $ \case
        Left stop -> grammarStopped env stop >> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) stop Nothing))
        Right why -> respond (Left (TResponseError (InR ErrorCodes_InternalError) why Nothing))
    onNotification h msg = rescue (h msg) (\why -> liftIO (hPutStrLn stderr ("olifant lsp: " <> T.unpack (either id id why))))
    rescue :: LspM () () -> (Either T.Text T.Text -> LspM () ()) -> LspM () ()
    rescue work recover = do
      lspEnv <- getLspEnv
      liftIO $
        runLspT lspEnv work `catch` \(e :: SomeException) -> case (fromException e :: Maybe SomeAsyncException, fromException e) of
          (Just _, _) -> throwIO e
          (_, Just (GrammarUnavailable t)) -> runLspT lspEnv (recover (Left t))
          _ -> runLspT lspEnv (recover (Right (T.pack (displayException e))))

-- | A grammar stop shown to the person: a `window/showMessage` (Error) the
-- first time this session meets its text.
grammarStopped :: Env -> T.Text -> LspM () ()
grammarStopped env stop = do
  new <- liftIO (atomicModifyIORef' (envStops env) (\seen -> (S.insert stop seen, not (S.member stop seen))))
  when new (sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Error ("olifant: " <> stop)))

handlers :: Env -> Handlers (LspM ())
handlers env =
  mconcat
    [ -- what the editor did with a recognition (earned quietness)
      notificationHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/recognitionFeedback")) $ \msg ->
        case msg ^. L.params of
          Object o
            | Just (Object doc) <- KM.lookup "textDocument" o
            , Just (String uri) <- KM.lookup "uri" doc
            , Just path <- uriToFilePath (Uri uri)
            , Just (String generator) <- KM.lookup "generator" o
            , Just (String outcome) <- KM.lookup "outcome" o
            , outcome `elem` ["accepted", "dismissed", "reset"] ->
                liftIO (modifyMVar_ (envState env) (recognitionFeedback path generator outcome))
          _ -> pure ()
    , -- the client gave up on a request: stop its work
      notificationHandler SMethod_CancelRequest $ \msg -> do
        let key = case msg ^. L.params . L.id of InL n -> T.pack (show n); InR t -> t
        liftIO (readIORef (envRunning env)) >>= maybe (pure ()) liftIO . M.lookup key
    , -- watch every project's `.olifant/`; packs are watched as projects load
      notificationHandler SMethod_Initialized $ \_ -> do
        watch env [GlobPattern (InL (Pattern "**/.olifant/**"))]
        -- a typed decision made in the background: the editor pulls diagnostics again
        lspEnv <- getLspEnv
        st <- liftIO (readMVar (envState env))
        liftIO (writeIORef (ssOnDecided st) (runLspT lspEnv (void (sendRequest SMethod_WorkspaceDiagnosticRefresh Nothing (\_ -> pure ())))))
    , notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
        let item = msg ^. L.params . L.textDocument
        atPath (item ^. L.uri) $ \path -> do
          update env (openDoc path (fromIntegral (item ^. L.version)) (item ^. L.text))
          -- its project loads now; watch its files (config, generators, the
          -- code agents and the CLI change on disk) and its packs, each
          -- relative to its own folder: an editor scopes a plain glob to the
          -- workspace folders it had when it registered (review I3, I8)
          found <- liftIO (withSnapshot env (projectFor path))
          watch env [everything r | Just (root, p) <- [found], r <- root : catalogPackRoots (prCatalog p)]
          -- a plan's journal lines left pending while it was closed land now
          -- (only when there are any: the edit moves the plan's version)
          case found of
            Just (_, p) | isPlanPath p path -> flushJournal env path >> startPass env path
            _ -> pure ()
    , notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
        let uri = msg ^. L.params . L.textDocument . L.uri
        getVirtualFile (toNormalizedUri uri) >>= \case
          Nothing -> pure ()
          Just vf -> atPath uri $ \path -> do
            let version = fromIntegral (virtualFileVersion vf)
            update env (changeDoc path version (virtualFileText vf))
            -- the plan's claim pass stops: the user is typing in it
            liftIO (stopPass env path)
            -- what follows from this edit, once typing settles
            case lastEditedLine (msg ^. L.params . L.contentChanges) of
              Nothing -> pure ()
              Just line -> do
                lspEnv <- getLspEnv
                liftIO (suggestLater env lspEnv path version line)
    , -- the editor saw a plan go quiet (out of insert mode, a second since the last change)
      notificationHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/plans/idle")) $ \msg ->
        case documentPath (msg ^. L.params) of
          Just path -> startPass env path
          Nothing -> pure ()
    , notificationHandler SMethod_TextDocumentDidClose $ \msg ->
        atPath (msg ^. L.params . L.textDocument . L.uri) (update env . closeDoc)
    , -- regions follow a save: the other files' through applyEdit (the saved
      -- file's came with willSaveWaitUntil)
      notificationHandler SMethod_TextDocumentDidSave $ \msg ->
        atPath (msg ^. L.params . L.textDocument . L.uri) $ \path -> do
          update env fileSaved
          startPass env path
          worked <- liftIO (atomicModifyIORef' (envSaving env) (\m -> (M.delete path m, M.lookup path m)))
          lspEnv <- getLspEnv
          -- what willSaveWaitUntil worked out, else (a client without it)
          -- the refresh now, off the message loop
          void . liftIO . forkIO . runLspT lspEnv $ do
            others <- maybe (liftIO (filter ((/= path) . fePath) <$> withSnapshot env (regionEdits path))) pure worked
            unless (null others) $
              void (sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams (Just "Olifant: regions") (workspaceEdit others)) (\_ -> pure ()))
    , requestHandler SMethod_TextDocumentWillSaveWaitUntil $ \req respond -> do
        edits <- case uriToFilePath (req ^. L.params . L.textDocument . L.uri) of
          Nothing -> pure []
          Just path -> do
            all' <- liftIO (withSnapshot env (regionEdits path))
            liftIO (atomicModifyIORef' (envSaving env) (\m -> (M.insert path (filter ((/= path) . fePath) all') m, ())))
            pure (filter ((== path) . fePath) all')
        respond (Right (InL [TextEdit (toRange (feRange e)) (feText e) | e <- edits]))
    , -- the generator picker: every generator with its inputs, and one
      -- generator's choices given what is set so far
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/generators")) $ \req respond ->
        liftIO (projectOfRequest env (req ^. L.params)) >>= \case
          Nothing -> respond (Right (listJson True (const []) (const M.empty) [] []))
          Just project -> do
            (entries, broken, views) <- liftIO (listGenerators (envPorts env) project True)
            -- with a position: which apply at the cursor (the picker ranks them first)
            here <- case (documentPath (req ^. L.params), positionOf (req ^. L.params)) of
              (Just path, Just (line, col)) -> S.fromList . map (actGenerator . fst) <$> liftIO (withSnapshot env (menuActionsFor path (line + 1, col + 1)))
              _ -> pure S.empty
            -- `menu = false`: callable, but not offered here
            respond (Right (withHere here (listJson True (subsOf (prCatalog project)) views (filter inMenu entries) broken)))
    , requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/choices")) $ \req respond -> do
        let params = req ^. L.params
            field k = case params of Object o -> KM.lookup k o; _ -> Nothing
            sets = case field "sets" of Just (Object m) -> M.fromList [(Key.toText k, v) | (k, String v) <- KM.toList m]; _ -> M.empty
        liftIO (projectOfRequest env params) >>= \case
          Nothing -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) "the document is in no Olifant project" Nothing))
          Just project -> case field "generator" of
            Just (String ref) ->
              liftIO (showGenerator (envPorts env) project ref sets) >>= \case
                Left (ShowCatalog err) -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) (describeCatalogError err) Nothing))
                Left (ShowInvalid why) -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
                Right (e, vs) -> respond (Right (showJson (M.fromList vs) (subsOf (prCatalog project) e) e))
            _ -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/choices needs a generator" Nothing))
    , requestHandler SMethod_WorkspaceExecuteCommand $ \req respond -> case req ^. L.params . L.command of
        "olifant.doctor" -> do
          let uri = case req ^. L.params . L.arguments of
                Just (String u : _) -> Just u
                _ -> Nothing
          found <- liftIO (maybe (pure Nothing) (\u -> projectOfRequest env (object ["textDocument" .= object ["uri" .= u]])) uri)
          case found of
            Nothing -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) "olifant.doctor needs a document of an Olifant project" Nothing))
            -- never `--run`: the editor's doctor runs no tests. Its evidence
            -- runs tools on a sample of the project, so it answers from the
            -- background; a cancel stops it, its processes killed
            Just project -> inBackground env req respond $ \job ->
              liftIO (Async.race (untilCancelled (envCancel job)) (doctorChecks (envPorts env) project False)) >>= \case
                Left () -> respond (Left (TResponseError (InL LSPErrorCodes_RequestCancelled) "cancelled" Nothing))
                Right checks -> respond (Right (InL (doctorJson checks)))
        other -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) ("no command " <> other) Nothing))
    , notificationHandler SMethod_WorkspaceDidChangeWorkspaceFolders (\_ -> pure ())
    , notificationHandler SMethod_WorkspaceDidChangeConfiguration (\_ -> pure ())
    , -- rule findings, pulled (spec §2)
      requestHandler SMethod_TextDocumentDiagnostic $ \req respond -> do
        (items, printsChanged) <- case uriToFilePath (req ^. L.params . L.textDocument . L.uri) of
          Nothing -> pure ([], False)
          Just path -> liftIO $ do
            broken <- withSnapshot env (brokenFor path)
            -- a project that does not load says why, on top of every file (review I6)
            let why w = Diagnostic (Range (Position 0 0) (Position 0 0)) (Just DiagnosticSeverity_Error) (Just (InR "config")) Nothing (Just "olifant") ("this project's Olifant configuration does not load: " <> w) Nothing Nothing Nothing
            before <- M.lookup path <$> readIORef (envPlanPrints env)
            found <- map fst <$> diagnosticsOf env path
            after <- M.lookup path <$> readIORef (envPlanPrints env)
            pure (maybe [] (pure . why) broken ++ found, before /= after)
        respond (Right (DocumentDiagnosticReport (InL (RelatedFullDocumentDiagnosticReport AString Nothing items Nothing))))
        -- a changed unit turns a plan's ✓ stale without another edit
        when printsChanged semanticTokensChanged
    , -- the fixes of the findings on the lines asked about, resolved lazily
      requestHandler SMethod_TextDocumentCodeAction $ \req respond -> do
        let Range (Position from _) (Position to _) = req ^. L.params . L.range
        actions <- case uriToFilePath (req ^. L.params . L.textDocument . L.uri) of
          Nothing -> pure []
          Just path -> liftIO (concat <$> sequence [fixActions env path (fromIntegral from + 1) (fromIntegral to + 1), holeActions env path (fromIntegral from + 1), cursorActions env path (req ^. L.params . L.range . L.start), newStepAction env path (req ^. L.params . L.range . L.start), excerptAction env path (req ^. L.params . L.context . L.only) (req ^. L.params . L.range . L.start), toggleAction env path (req ^. L.params . L.context . L.only) (req ^. L.params . L.range . L.start), refingerprintAction env path (req ^. L.params . L.range . L.start)])
        respond (Right (InL (map InR actions)))
    , -- planned on its own thread: a model fill or a test run takes seconds,
      -- and the server reads on meanwhile (other requests, progress)
      requestHandler SMethod_CodeActionResolve $ \req respond -> inBackground env req respond $ \job -> do
        let action = req ^. L.params
        let failed why = respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
        case (fixData =<< (action ^. L.data_), actionData =<< (action ^. L.data_)) of
          (Just (path, rule, line, n, sets), _) ->
            planProgress (action ^. L.title) (\progress -> withPlanned job (fixPlan True progress path rule line n sets)) >>= \case
              Left why -> failed why
              Right planned -> respond (Right (resolved action planned))
          (_, Just (path, line, generator, sets)) ->
            planProgress (action ^. L.title) (\progress -> withPlanned job (actionPlanWith progress True path line generator sets)) >>= \case
              Left why -> failed why
              -- a snippet's body (tab stops) rides along for editors that expand it
              Right planned@(_, (_, plan)) -> respond (Right (resolved action planned & L.data_ %~ fmap (withSnippet (planSnippet plan))))
          _ | Just (path, line, version) <- excerptData =<< (action ^. L.data_) ->
                liftIO (excerptEdit env path line version) >>= either failed (\e -> respond (Right (action & L.edit ?~ e)))
          _ | Just (path, line, version) <- toggleData =<< (action ^. L.data_) ->
                liftIO (toggleEdit env path line version) >>= either failed (\e -> respond (Right (action & L.edit ?~ e)))
          _ -> case holeData =<< (action ^. L.data_) of
            Just (path, line) ->
              planProgress "Fill with the model" (\progress -> withPlanned job (fillHolePlan progress path line)) >>= \case
                Left why -> failed why
                Right planned -> respond (Right (resolved action planned))
            Nothing -> respond (Right action)
    , -- ⚡ at the end of each line with actions; the hint opens the menu there
      requestHandler SMethod_TextDocumentInlayHint $ \req respond -> do
        let Range (Position from _) (Position to _) = req ^. L.params . L.range
            uri = req ^. L.params . L.textDocument . L.uri
        hints <- case uriToFilePath uri of
          Nothing -> pure []
          Just path -> liftIO $ do
            (lines', _) <- withSnapshot env (hintsFor path)
            text <- documentText env path
            let ls = T.splitOn "\n" text
            pure
              [ InlayHint (Position (fromIntegral l0) (fromIntegral (utf16 (lineAt ls l0)))) (InR [InlayHintLabelPart label Nothing Nothing (Just (Command label "olifant.showActions" (Just [toJSON uri, toJSON (Position (fromIntegral l0) 0)])))]) Nothing Nothing Nothing (Just True) Nothing Nothing
              | LineHint line acts <- lines'
              , let l0 = line - 1
              , fromIntegral from <= l0 && l0 <= fromIntegral to
              , let plain = [a | (a, False) <- acts]
              , first : rest <- [plain]
              , let label = "⚡ " <> actLabel first <> (if null rest then "" else " +" <> T.pack (show (length rest)))
              ]
        respond (Right (InL hints))
    , -- the generators that nearly apply in this file, and why not; in a
      -- plan, the code of the unit the anchor under the cursor names
      requestHandler SMethod_TextDocumentHover $ \req respond -> do
        let Position l c = req ^. L.params . L.position
        hover <- case uriToFilePath (req ^. L.params . L.textDocument . L.uri) of
          Nothing -> pure Nothing
          Just path -> liftIO $ withSnapshot env (projectFor path) >>= \case
            Just (_, project) | isPlanPath project path -> planHover env project path (Position l c)
            _ -> do
              (lines', near) <- withSnapshot env (hintsFor path)
              let here = concat [map fst acts | LineHint line acts <- lines', line == fromIntegral l + 1]
                  text = T.intercalate "\n\n" (["⚡ " <> actLabel a <> " (" <> actGenerator a <> ")" | a <- here] ++ ["nearby: " <> nbGenerator n <> " — " <> nbWhy n | n <- near])
              pure (if T.null text then Nothing else Just (Hover (InL (MarkupContent MarkupKind_Markdown text)) Nothing))
        respond (Right (maybe (InR Null) InL hover))
    , -- snippets, each with its body placed at the cursor (tab stops)
      requestHandler SMethod_TextDocumentCompletion $ \req respond -> inBackground env req respond $ \job -> do
        let typedChar = (req ^. L.params . L.context) >>= \c -> if c ^. L.triggerKind == CompletionTriggerKind_TriggerCharacter then Just () else Nothing
        (incomplete, items) <- case uriToFilePath (req ^. L.params . L.textDocument . L.uri) of
          Nothing -> pure (False, [])
          Just path -> liftIO $
            withSnapshot job (projectFor path) >>= \case
              -- a plan completes its anchors (units and files after `@`, units
              -- after `@path:`) and offers a new step on a blank line; the
              -- list is bounded, so the client asks again as more is typed
              Just (_, project) | isPlanPath project path -> (True,) <$> anchorCompletions job project path (req ^. L.params . L.position)
              -- `@` and `:` trigger completion everywhere, but only plans use them
              _ | Just () <- typedChar -> pure (False, [])
              _ -> (False,) <$> snippetItems job path (req ^. L.params . L.position)
        respond (Right (if incomplete then InR (InL (CompletionList True Nothing items)) else InL items))
    , -- the chosen snippet, planned again with the model filling its holes
      requestHandler SMethod_CompletionItemResolve $ \req respond -> inBackground env req respond $ \job -> do
        let item = req ^. L.params
        case actionData =<< (item ^. L.data_) of
          Nothing -> respond (Right item)
          Just (path, line, generator, sets) ->
            liftIO (withSnapshot job (actionEdits True path line generator sets)) >>= \case
              Right (_, Just sn) -> respond (Right (item & L.textEdit %~ fmap (\case InL e -> InL (e & L.newText .~ snBody sn); InR e -> InR (e & L.newText .~ snBody sn))))
              _ -> respond (Right item)
    , -- the snippet anticipated here, unasked (LSP 3.18 inline completion)
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "textDocument/inlineCompletion")) $ \req respond -> inBackground env req respond $ \job -> do
        let params = req ^. L.params
            found = do
              Object o <- pure params
              Object doc <- KM.lookup "textDocument" o
              String uri <- KM.lookup "uri" doc
              Object pos <- KM.lookup "position" o
              Number l <- KM.lookup "line" pos
              Number c <- KM.lookup "character" pos
              (,) <$> uriToFilePath (Uri uri) <*> (Position <$> toBoundedInteger l <*> toBoundedInteger c)
        items <- case found of
          Nothing -> pure []
          Just (path, position) -> liftIO (ghostItems job path position)
        respond (Right (object ["items" .= items]))
    , -- every action in a file, where each applies (durandal's hints read
      -- this; other editors get inlay hints)
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/fileActions")) $ \req respond -> do
        let path = case req ^. L.params of
              Object o | Just (Object doc) <- KM.lookup "textDocument" o, Just (String uri) <- KM.lookup "uri" doc -> uriToFilePath (Uri uri)
              _ -> Nothing
        found <- maybe (pure Nothing) (liftIO . withSnapshot env . fileActionsFor) path
        respond (Right (maybe (fileActionsJson [] [] []) (\(gens, acts, near) -> fileActionsJson gens acts near) found))
    , -- what follows at a line (the editor knows the cursor; pushes guess it
      -- from the last edit), offered as pushed ones are, its plan with it
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/suggest")) $ \req respond -> inBackground env req respond $ \job -> do
        let params = req ^. L.params
            found = do
              Object o <- pure params
              Object doc <- KM.lookup "textDocument" o
              String uri <- KM.lookup "uri" doc
              Number l <- KM.lookup "line" o
              (,) <$> uriToFilePath (Uri uri) <*> toBoundedInteger l
        case found of
          Nothing -> respond (Right Aeson.Null)
          Just (path, line) -> do
            liftIO (withSnapshotState job (suggestOrRecognize path line)) >>= \case
              (at, Just sg0) | Just v <- fst <$> M.lookup path (ssDocs at) -> do
                -- a hint the model has upgraded for this version: the offer
                upgraded <- liftIO (upgradedFor job path v sg0)
                let sg = maybe sg0 snd upgraded
                n <- maybe (liftIO (offer job path v sg)) (pure . fst) upgraded
                respond (Right (object (suggestionFields path v n sg ++ ["plan" .= planJson False (sgPlan sg)] ++ ["edit" .= workspaceEdit (editsOfPlan at (sgRoot sg) (sgPlan sg))])))
              _ -> respond (Right Aeson.Null)
    , -- an offered suggestion's plan, while its document is as it was
      -- (`fill`: planned again with the model, as accepting it does)
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/plan")) $ \req respond -> inBackground env req respond $ \job -> do
        let wanted = case req ^. L.params of
              Object o | Just (Number n) <- KM.lookup "planId" o -> toBoundedInteger n
              _ -> Nothing
            fill = case req ^. L.params of
              Object o -> KM.lookup "fill" o == Just (Bool True)
              _ -> False
        offered <- liftIO ((\(_, m) -> (`M.lookup` m) =<< wanted) <$> readIORef (envOffered job))
        s <- liftIO (readMVar (envState job))
        -- or a generator from the inputs chosen (the picker), for a document's project
        let chosen = case req ^. L.params of
              Object o
                | Just (String generator) <- KM.lookup "generator" o
                , Just (Object doc) <- KM.lookup "textDocument" o
                , Just (String uri) <- KM.lookup "uri" doc
                , Just path <- uriToFilePath (Uri uri) ->
                    Just (path, generator, setsIn o, KM.lookup "recognize" o == Just (Bool True))
              _ -> Nothing
        -- or the picker's steps (a `+ New` step first, then the picked generator)
        let picked = case req ^. L.params of
              Object o | Just (Aeson.Array _) <- KM.lookup "steps" o, Just path <- documentPath (req ^. L.params) -> Just (path, pickSteps o)
              _ -> Nothing
        case (picked, chosen, offered) of
          (Just (path, steps), _, _) ->
            planProgress "Generating" (\progress -> withPlanned job (stepsPlanWith progress True path (fromMaybe (0, 0) (positionOf (req ^. L.params))) steps)) >>= \case
              Left why -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
              Right (at, (root, plan)) -> respond (Right (object ["plan" .= planJson False plan, "edit" .= workspaceEdit (editsOfPlan at root plan)]))
          (_, Just (path, generator, sets, recognising), _) ->
            -- a recognition's plan (the file converted, or its companions), or the picker's
            let planned progress = if recognising then recognitionPlan True progress path generator sets else actionPlanWith progress True path 1 generator sets
             in planProgress generator (\progress -> withPlanned job (planned progress)) >>= \case
                  Left why -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
                  Right (at, (root, plan)) -> respond (Right (object ["plan" .= planJson False plan, "edit" .= workspaceEdit (editsOfPlan at root plan)]))
          (_, _, Nothing) -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) "no such plan (offered plans last until their document changes)" Nothing))
          (_, _, Just o)
            | fmap fst (M.lookup (ofPath o) (ssDocs s)) /= Just (ofVersion o) -> respond (Left (TResponseError (InL LSPErrorCodes_ContentModified) "the document changed since this was suggested" Nothing))
            | not fill ->
                let sg = ofSuggestion o
                 in respond (Right (object ["plan" .= planJson False (sgPlan sg), "edit" .= workspaceEdit (editsOfPlan s (sgRoot sg) (sgPlan sg))]))
            | otherwise -> do
                let sg = ofSuggestion o
                let replanned progress = case sgRecognition sg of
                      Just _ -> recognitionPlan True progress (ofPath o) (sgGenerator sg) (sgSets sg)
                      Nothing -> actionPlanWith progress True (ofPath o) (sgAt sg) (sgGenerator sg) (sgSets sg)
                planProgress (sgLabel sg) (\progress -> withPlanned job (replanned progress)) >>= \case
                  Left why -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
                  Right (at, (root, plan)) -> respond (Right (object ["plan" .= planJson False plan, "edit" .= workspaceEdit (editsOfPlan at root plan)]))
    , -- the generate picker: the form and what it would write; a newer
      -- request for the document supersedes an older one
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/preview")) $ \req respond -> inBackground env req respond $ \job ->
        case (documentPath (req ^. L.params), req ^. L.params) of
          (Just path, Object o) -> do
            let at = fromMaybe (0, 0) (positionOf (req ^. L.params))
                steps = pickSteps o
            done <- liftIO (submit (envJobs job) ("preview " <> T.pack path) 0 (\token -> withSnapshot job {envCancel = token} (previewFor path at steps)) >>= Async.wait)
            case done of
              Nothing -> respond (Left (TResponseError (InL LSPErrorCodes_RequestCancelled) "superseded by a newer preview" Nothing))
              Just pv -> respond (Right (previewJson steps pv))
          _ -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/preview needs a textDocument and steps" Nothing))
    , -- the project of a document in the shared vocabulary (spec §1b)
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/model")) $ \req respond -> inBackground env req respond $ \job -> do
        let path = case req ^. L.params of
              Object o | Just (Object doc) <- KM.lookup "textDocument" o, Just (String uri) <- KM.lookup "uri" doc -> uriToFilePath (Uri uri)
              _ -> Nothing
        model <- maybe (pure Nothing) (liftIO . withSnapshot job . modelFor) path
        -- with the migration report, when the project is migrating
        mig <- case path of
          Nothing -> pure Nothing
          Just p -> liftIO (withSnapshot job (\s -> projectFor p s >>= \case
            (s', Just (_, project)) -> (s',) <$> migrationFor (ssPorts s') project
            (s', Nothing) -> pure (s', Nothing)))
        respond (Right (maybe Aeson.Null (\m -> withMigration mig (toJSON m)) model))
    , -- the holes left in the project of a document: where, which, what for
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/holes")) $ \req respond -> inBackground env req respond $ \job -> do
        let path = case req ^. L.params of
              Object o | Just (Object doc) <- KM.lookup "textDocument" o, Just (String uri) <- KM.lookup "uri" doc -> uriToFilePath (Uri uri)
              _ -> Nothing
        holes <- maybe (pure []) (liftIO . withSnapshot job . holesFor) path
        respond (Right (object ["holes" .= [object ["path" .= hoPath h, "line" .= hoLine h, "name" .= hoName h, "kind" .= hoKind h, "instruction" .= hoInstruction h] | h <- holes]]))
    , -- a file's tests (the module's test file, or this test file), run
      -- whole over the editor's text, under progress
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/tests/run")) $ \req respond -> inBackground env req respond $ \job ->
        case documentPath (req ^. L.params) of
          Nothing -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/tests/run needs a textDocument" Nothing))
          Just path -> do
            let scope = case req ^. L.params of
                  Object o | Just (String "all") <- KM.lookup "scope" o -> ScopeAll
                  Object o | Just (String "file") <- KM.lookup "scope" o -> ScopeFile
                  params | Just (line, _) <- positionOf params -> ScopeAt (line + 1)
                  _ -> ScopeFile
            ran <- planProgress "testing" (\_ -> withSnapshot job (testsRun scope path))
            -- a run the editor cancelled is no result (its runner was stopped)
            stopped <- liftIO (cancelled (envCancel job))
            case ran of
              _ | stopped -> respond (Left (TResponseError (InL LSPErrorCodes_RequestCancelled) "cancelled" Nothing))
              Left why -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
              Right answer -> respond (Right (testsJson answer))
    , -- where a plan's anchor on a line points: its unit's file and line
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/plans/anchor")) $ \req respond ->
        case (documentPath (req ^. L.params), lineOf (req ^. L.params)) of
          (Just path, Just line) -> do
            text <- liftIO (documentText env path)
            case [a | l <- parsePlan text, lnRow l == line, Just a <- [lnAnchor l]] of
              [] -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) "no anchor on this line" Nothing))
              a : _ ->
                liftIO (withSnapshot env (\s -> projectFor path s >>= \case
                  (s', Just (_, project)) -> (s',) . Just <$> resolveAnchor s' project a
                  (s', Nothing) -> pure (s', Nothing))) >>= \case
                  Nothing -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) "this plan is in no Olifant project" Nothing))
                  Just r -> respond (Right (object
                    [ "file" .= (rsAbs r <$ rsText r)
                    , "line" .= maybe 1 namedDef (rsUnit r)
                    , "unit" .= maybe (anUnit a) namedName (rsUnit r)
                    , "found" .= isJust (rsUnit r)
                    , "fp" .= (rsFingerprint r <* rsUnit r)
                    ]))
          _ -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/plans/anchor needs a textDocument and a line" Nothing))
    , -- gd on a plan's anchor: its unit's def line; quiet anywhere else
      requestHandler SMethod_TextDocumentDefinition $ \req respond -> do
        let Position l c = req ^. L.params . L.position
        found <- case uriToFilePath (req ^. L.params . L.textDocument . L.uri) of
          Nothing -> pure Nothing
          Just path -> liftIO $ withSnapshot env (projectFor path) >>= \case
            Just (_, project) | isPlanPath project path -> do
              text <- documentText env path
              let col = T.length (takeUtf16 (fromIntegral c) (lineAt (T.splitOn "\n" text) (fromIntegral l)))
              case [a | ln <- parsePlan text, lnRow ln == fromIntegral l, Just a <- [lnAnchor ln], anStart a <= col, col <= anEnd a] of
                a : _ -> withSnapshot env (\s -> (s,) <$> resolveAnchor s project a) <&> \r -> case (rsUnit r, rsText r) of
                  (Just u, Just _) -> Just (Location (filePathToUri (rsAbs r)) (Range (Position (fromIntegral (namedDef u) - 1) 0) (Position (fromIntegral (namedDef u) - 1) 0)))
                  _ -> Nothing
                [] -> pure Nothing
            _ -> pure Nothing
        respond (Right (maybe (InR (InR Null)) (InL . Definition . InL) found))
    , -- a plan's anchored lines, by section, each with where it points
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/plans/files")) $ \req respond ->
        case documentPath (req ^. L.params) of
          Nothing -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/plans/files needs a textDocument" Nothing))
          Just path -> do
            text <- liftIO (documentText env path)
            items <- liftIO (withSnapshot env (\s -> projectFor path s >>= \case
              (s', Just (_, project)) -> (s',) <$> planFiles s' project text
              (s', Nothing) -> pure (s', [])))
            respond (Right (object ["items" .= [object ["section" .= fiSection i, "row" .= fiRow i, "text" .= fiText i, "file" .= fiAbs i, "line" .= fiLine i] | i <- items]]))
    , -- a plan's line asked, validated or done by the runner (claude -p):
      -- ghost blocks to insert, nothing written to the plan
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/plans/ask")) $ \req respond -> errandRequest env Ask "asking" req (req ^. L.params) respond
    , requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/plans/validate")) $ \req respond -> errandRequest env Validate "validating" req (req ^. L.params) respond
    , requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/plans/do")) $ \req respond -> errandRequest env Do "doing" req (req ^. L.params) respond
    , -- a past do, by id or by the journal line at a row (null: none)
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/plans/errand")) $ \req respond ->
        case documentPath (req ^. L.params) of
          Nothing -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/plans/errand needs a textDocument and an id or a row" Nothing))
          Just path -> liftIO (errandView env path (req ^. L.params)) >>= \case
            Left why -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
            Right v -> respond (Right v)
    , -- a plan's past dos, newest first: a step's (at `row`), or all
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/plans/errands")) $ \req respond ->
        case documentPath (req ^. L.params) of
          Nothing -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/plans/errands needs a textDocument" Nothing))
          Just path ->
            liftIO (planContext env path) >>= \case
              Nothing -> respond (Right (object ["errands" .= ([] :: [Value])]))
              Just (p, rel, text) -> do
                all' <- liftIO (loadArchiveIndex (envPorts env) (prRoot p) rel)
                let ofStep = maybe all' (pastDosAt (parsePlan text) all') (lineParam "row" (req ^. L.params))
                    words' r = fromMaybe (rvStatus r) (listToMaybe [T.strip w | l <- rvSaid r, Just w <- [T.stripPrefix "DONE" l <|> T.stripPrefix "BLOCKED" l]])
                respond (Right (object ["errands" .= [object ["id" .= rvId r, "at" .= rvAt r, "status" .= rvStatus r, "choice" .= rvChoice r, "words" .= words' r, "model" .= rvModel r] | r <- ofStep]]))
    , -- a plan's open reviews (a `do` not yet answered), each at its step now
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/plans/reviews")) $ \req respond ->
        case documentPath (req ^. L.params) of
          Nothing -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/plans/reviews needs a textDocument" Nothing))
          Just path ->
            liftIO (planContext env path) >>= \case
              Nothing -> respond (Right (object ["reviews" .= ([] :: [Value])]))
              Just (p, rel, text) -> do
                rs <- liftIO (loadReviews (envPorts env) (prRoot p) rel)
                readings <- liftIO (loadReadings (envPorts env) (prRoot p) rel)
                let ls = parsePlan text
                respond (Right (object ["reviews" .=
                  ( [object ["id" .= rvId r, "kind" .= ("do" :: T.Text), "row" .= fmap lnRow (locateStep ls r), "step" .= T.strip (rvStep r), "status" .= rvStatus r] | r <- rs]
                      ++ [ object ["id" .= readingsId, "kind" .= ("reconcile" :: T.Text), "entries" .= [object ["id" .= readingKey r, "kind" .= entryKind (rdKind r), "row" .= readingRow ls r] | r <- rrReadings rr]]
                         | Just rr <- [readings] ] )]))
    , -- one review, as the window draws it (the change re-derived from its reference now)
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/plans/review")) $ \req respond ->
        case (documentPath (req ^. L.params), reviewIdOf (req ^. L.params)) of
          (Just path, Just rid) ->
            liftIO (planContext env path) >>= \case
              Nothing -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) "this plan is in no Olifant project" Nothing))
              Just (p, rel, text) | rid == readingsId ->
                liftIO (loadReadings (envPorts env) (prRoot p) rel) >>= \case
                  Nothing -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) ("no open review " <> rid) Nothing))
                  Just rr -> liftIO (versionOf env path) >>= \v -> respond (Right (readingsView path v text rr))
              Just (p, rel, text) ->
                liftIO (loadReview (envPorts env) (prRoot p) rel rid) >>= \case
                  Nothing -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) ("no open review " <> rid) Nothing))
                  Just r -> liftIO (reviewView p text r) >>= respond . Right
          _ -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/plans/review needs a textDocument and an id" Nothing))
    , -- a review answered: keep, revert, test (its step's tests now) or allow (its denials)
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/plans/review/choose")) $ \req respond -> inBackground env req respond $ \job ->
        case (documentPath (req ^. L.params), reviewIdOf (req ^. L.params), choiceParam (req ^. L.params)) of
          (Just path, Just rid, Just choice) | rid == readingsId -> do
            answered <- liftIO (chooseReading env path (textParam "entry" (req ^. L.params)) choice (lineParam "step" (req ^. L.params)))
            case answered of
              Left why -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
              Right v -> respond (Right v) >> lensesChanged
          (Just path, Just rid, Just choice) | choice `elem` ["keep", "revert", "reopen", "test", "allow"] -> do
            answered <- planProgress (if choice == "test" then "testing" else "reviewing") (\_ -> chooseReview job path rid choice)
            case answered of
              Left why -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
              Right v -> respond (Right v) >> lensesChanged
          _ -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/plans/review/choose needs a textDocument, an id and a choice (a do: keep, revert, test or allow; the reconcile: an entry and apply, leave, place with a step, new or ignore)" Nothing))
    , -- a lens on the step of each open review
      requestHandler SMethod_TextDocumentCodeLens $ \req respond -> do
        let uri = req ^. L.params . L.textDocument . L.uri
        lenses <- case uriToFilePath uri of
          Nothing -> pure []
          Just path -> liftIO (planContext env path) >>= \case
            Just (p, rel, text) | isPlanPath p path -> do
              rs <- liftIO (loadReviews (envPorts env) (prRoot p) rel)
              readings <- maybe [] rrReadings <$> liftIO (loadReadings (envPorts env) (prRoot p) rel)
              past <- liftIO (loadArchiveIndex (envPorts env) (prRoot p) rel)
              reviewKey <- liftIO (lookup "plan_review" . scKeys . ssConfig <$> readMVar (envState env))
              let ls = parsePlan text
                  keyed t = maybe t (\k -> t <> " \183 " <> k) reviewKey
                  lens row title args = CodeLens (Range (Position (fromIntegral row) 0) (Position (fromIntegral row) 0)) (Just (Command title "olifant.plans.review" (Just (map String args ++ [])))) Nothing
                  open = length rs + length readings
                  -- the title line: the plan's first heading, else its first line
                  titleRow = maybe 0 lnRow (find (\l -> case lnKind l of Heading _ -> True; _ -> False) ls)
                  first = case rs of
                    r : _ -> [rvId r, getUri uri]
                    [] -> [readingsId, getUri uri]
                  -- readings only: "N readings to review"
                  counted
                    | null rs = T.pack (show open) <> (if open == 1 then " reading" else " readings") <> " to review"
                    | otherwise = T.pack (show open) <> " to review"
              pure
                ( [lens titleRow (keyed counted) first | open > 0]
                    ++ [lens (lnRow l) (lensTitle r) [rvId r, getUri uri] | r <- rs, Just l <- [locateStep ls r]]
                    ++ [lens row t [readingsId, getUri uri, readingKey r] | r <- readings, Just row <- [readingRow ls r], Just t <- [readingLens reviewKey (listToMaybe (drop row ls)) r]]
                    ++ [ CodeLens (Range (Position (fromIntegral (lnRow l)) 0) (Position (fromIntegral (lnRow l)) 0)) (Just (Command (T.pack (show n) <> (if n == 1 then " delegation" else " delegations")) "olifant.plans.errand" (Just [String (getUri uri), Number (fromIntegral (lnRow l))]))) Nothing
                       | l <- ls, case lnKind l of Step _ -> True; _ -> False
                       , let n = length (pastDosAt ls past (lnRow l)), n > 0 ]
                    ++ excerptLenses uri ls )
            _ -> pure []
        respond (Right (InL lenses))
    , -- a plan's spans, classified (the legend is 'planTokenTypes'); any other file has none
      requestHandler SMethod_TextDocumentSemanticTokensFull $ \req respond -> do
        encoded <- case uriToFilePath (req ^. L.params . L.textDocument . L.uri) of
          Nothing -> pure []
          Just path -> liftIO $ planContext env path >>= \case
            Just (p, _, text) | isPlanPath p path -> do
              prints <- M.findWithDefault M.empty path <$> readIORef (envPlanPrints env)
              store <- readMVar (envState env) >>= \s -> claimsOf s (prRoot p) path
              pure (encodePlanTokens (planTokensWith store prints (parsePlan text)))
            _ -> pure []
        respond (Right (InL (SemanticTokens Nothing encoded)))
    , -- a plan's excerpts, ticked steps and answered questions fold
      requestHandler SMethod_TextDocumentFoldingRange $ \req respond -> do
        ranges <- case uriToFilePath (req ^. L.params . L.textDocument . L.uri) of
          Nothing -> pure []
          Just path -> liftIO $ planContext env path >>= \case
            Just (p, _, text) | isPlanPath p path -> do
              -- a belief folds its question once its check is stored
              checked <- readMVar (envState env) >>= \s -> checkedKeys <$> claimsOf s (prRoot p) path
              let ls = parsePlan text
                  region o c = FoldingRange (fromIntegral o) Nothing (fromIntegral c) Nothing (Just FoldingRangeKind_Region) Nothing
              pure ([region o c | (_, o, c) <- excerptSpans ls] ++ [region o c | (o, c) <- planFolds checked ls])
            _ -> pure []
        respond (Right (InL ranges))
    , -- a plan against the work: facts to apply, ghost blocks to place
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/plans/reconcile")) $ \req respond ->
        case documentPath (req ^. L.params) of
          Nothing -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/plans/reconcile needs a textDocument" Nothing))
          Just path -> inBackground env req respond $ \job -> do
            let forced = case req ^. L.params of Object o -> KM.lookup "forced" o == Just (Bool True); _ -> False
            answered <- planProgress "reconciling" (\_ -> withSnapshot job (\s -> reconcile (envCancel job) forced path s))
            stopped <- liftIO (cancelled (envCancel job))
            case answered of
              _ | stopped -> respond (Left (TResponseError (InL LSPErrorCodes_RequestCancelled) "cancelled" Nothing))
              Left why -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
              Right a -> do
                -- the readings replace the plan's reconcile review; the facts land by applyEdit
                open <- liftIO (planContext env path) >>= \case
                  Just (p, rel, _) -> maybe 0 (length . rrReadings) <$> liftIO (replaceReadings (envPorts env) (prRoot p) rel a)
                  Nothing -> pure 0
                landed <- sendFacts env path a
                respond (Right (withKeys ["landed" .= landed, "readings" .= open] (reconcileJson a {raFacts = [], raBlocks = []}) `without` "blocks"))
                lensesChanged
    , -- a step's test stubbed in its unit's test file: the one edit, where
      -- its name is, and its test child as a fact
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/plans/stub")) $ \req respond -> inBackground env req respond $ \job ->
        case (documentPath (req ^. L.params), lineOf (req ^. L.params)) of
          (Just path, Just line) ->
            liftIO (withSnapshot job (stepStub path line)) >>= \case
              Left why -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
              Right st ->
                let a = stAnswer st
                 in respond (Right (object ["file" .= saFile a, "line" .= saLine a, "name" .= saName a, "edits" .= workspaceEdit [saEdit a], "facts" .= [factJson (stFact st)]]))
          _ -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/plans/stub needs a textDocument and a line" Nothing))
    , -- a step's tests (or one test child's) run: the facts they leave
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/plans/test")) $ \req respond -> inBackground env req respond $ \job ->
        case (documentPath (req ^. L.params), lineOf (req ^. L.params)) of
          (Just path, Just line) -> do
            before <- liftIO (versionOf env path)
            ran <- planProgress "testing" (\_ -> withSnapshot job (stepTest path line))
            stopped <- liftIO (cancelled (envCancel job))
            case ran of
              _ | stopped -> respond (Left (TResponseError (InL LSPErrorCodes_RequestCancelled) "cancelled" Nothing))
              Left why -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
              Right t -> do
                -- the journal rides the facts only while the plan is as the
                -- tests read it (the editor drops facts for a plan edited
                -- since); else it lands by its own edit after the answer
                doc <- liftIO (M.lookup path . ssDocs <$> readMVar (envState env))
                facts <- liftIO $ case doc of
                  Just (v, text) -> do
                    journalTests env path text line (ttFacts t)
                    if before == Just v
                      then do
                        (jf, land) <- journalOn env path text
                        withJournal (ttFacts t) jf <$ land
                      else pure (ttFacts t)
                  Nothing -> pure (ttFacts t)
                respond (Right (object ["facts" .= map factJson facts, "passed" .= ttPassed t, "failed" .= ttFailed t, "seconds" .= ttSeconds t]))
                when (maybe False (\(v, _) -> before /= Just v) doc) (flushJournal env path)
          _ -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/plans/test needs a textDocument and a line" Nothing))
    , -- what the editor did with an errand's ghost block: an outcome record
      -- (anything malformed, or out of a project, is ignored; a record that
      -- cannot be written is dropped: telemetry never reaches the editor)
      notificationHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/plans/outcome")) $ \msg ->
        forM_ (outcomeParams (msg ^. L.params)) $ \(path, eid, block, outcome) -> liftIO $ do
          found <- withSnapshot env (\s -> projectFor path s >>= \(s', p) -> pure (s', (ssPorts s',) . prRoot . snd <$> p))
          forM_ found $ \(ports, root) -> do
            now <- pNow ports
            appendRecord ports root (AnOutcome (OutcomeRecord {orId = eid, orBlock = block, orAt = now, orOutcome = outcome}))
    , -- a file's other side: a module's test file (which may not exist
      -- yet), or a test file's module; it reads only config and the file list
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/tests/file")) $ \req respond ->
        case documentPath (req ^. L.params) of
          Nothing -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/tests/file needs a textDocument" Nothing))
          Just path ->
            liftIO (withSnapshot env (testsFile path)) >>= \case
              Left why -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
              Right answer -> respond (Right (testsFileJson answer))
    , -- a new test's stub for the function or class at a position, at the
      -- end of its test file: the one edit, and where the test's name is
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/stub")) $ \req respond -> inBackground env req respond $ \job ->
        case (documentPath (req ^. L.params), positionOf (req ^. L.params), survivorOf "olifant/stub" (req ^. L.params)) of
          (_, _, Left bad) -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) bad Nothing))
          (Just path, Just (line, _), Right survivor) ->
            liftIO (withSnapshot job (stubAt path (line + 1) survivor)) >>= \case
              Left why -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
              Right a -> respond (Right (object ["file" .= saFile a, "line" .= saLine a, "name" .= saName a, "edits" .= workspaceEdit [saEdit a]]))
          _ -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/stub needs a textDocument and a position" Nothing))
    , -- the survivors of the unit at a position, mutated against its test
      -- file: nothing is stored
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/mutate")) $ \req respond -> inBackground env req respond $ \job ->
        case (documentPath (req ^. L.params), positionOf (req ^. L.params)) of
          (Just path, Just (line, _)) -> do
            mutated <- planProgress "mutating" (\_ -> withSnapshot job (mutateAt path (line + 1)))
            stopped <- liftIO (cancelled (envCancel job))
            case mutated of
              _ | stopped -> respond (Left (TResponseError (InL LSPErrorCodes_RequestCancelled) "cancelled" Nothing))
              Left why -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
              Right a ->
                let flat = T.replace "\n" "\9166"
                    mutatedLine m = fromMaybe "" (lookup (muLine m) (zip [1 :: Int ..] (T.splitOn "\n" (muText m))))
                 in respond . Right $
                      object
                        [ "file" .= maFile a
                        , "unit" .= maUnit a
                        , "lines" .= object ["first" .= fst (maLines a), "last" .= snd (maLines a)]
                        , "killed" .= maKilled a
                        , "survived" .= length (maSurvivors a)
                        , "waived" .= maWaived a
                        , "survivors" .= [object ["line" .= muLine m, "operator" .= muOperator m, "from" .= flat (muFrom m), "to" .= flat (muTo m), "mutated" .= mutatedLine m, "dim" .= muDim m] | m <- maSurvivors a]
                        , "summary" .= maSummary a
                        ]
          _ -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/mutate needs a textDocument and a position" Nothing))
    , -- a survivor set aside for a reason: `olifant/mutate` leaves it out
      -- from now on (a line of the project's waived.log)
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/waive")) $ \req respond ->
        let reason = case req ^. L.params of
              Object o | Just (String r) <- KM.lookup "reason" o -> r
              _ -> ""
         in case (documentPath (req ^. L.params), survivorOf "olifant/waive" (req ^. L.params)) of
              (_, Left bad) -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) bad Nothing))
              (Just path, Right (Just survivor)) ->
                liftIO (withSnapshot env (waiveAt path reason survivor)) >>= \case
                  Left why -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
                  Right () -> respond (Right (object ["waived" .= True]))
              _ -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/waive needs a textDocument and a survivor" Nothing))
    , -- a conjure of the unit at a position (or of the selected lines): its
      -- tests run before and after; the edit and the after-run
      requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/conjure")) $ \req respond -> inBackground env req respond $ \job ->
        case (documentPath (req ^. L.params), positionOf (req ^. L.params)) of
          (Just path, Just (line, _)) -> do
            let selection = case req ^. L.params of
                  Object o
                    | Just (Object sel) <- KM.lookup "selection" o
                    , Just (Number a) <- KM.lookup "start" sel
                    , Just (Number z) <- KM.lookup "end" sel
                    , Just a' <- toBoundedInteger a
                    , Just z' <- toBoundedInteger z ->
                        Just (a' + 1, z' + 1)
                  _ -> Nothing
                invalid = respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/conjure resume needs a token and a choice (continue, climb, stop, reconjure, accept, reject or wire)" Nothing))
            case resumeOf (req ^. L.params) of
              Left _ -> invalid
              Right resumed -> do
                conjured' <- planProgress "conjuring" (\progress -> withSnapshot job (maybe (conjureAt progress path (line + 1) selection) (resumeConjure progress path) resumed))
                -- a conjure the editor cancelled is no result
                stopped <- liftIO (cancelled (envCancel job))
                case conjured' of
                  _ | stopped -> respond (Left (TResponseError (InL LSPErrorCodes_RequestCancelled) "cancelled" Nothing))
                  Left why -> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
                  Right outcome -> respond (Right (conjureJson outcome))
          _ -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "olifant/conjure needs a textDocument and a position" Nothing))
    , requestHandler (SMethod_CustomMethod (Proxy :: Proxy "olifant/debugState")) $ \req respond ->
        liftIO (readIORef (envDebug env)) >>= \case
          False -> respond (Left (TResponseError (InR ErrorCodes_MethodNotFound) "olifant/debugState is for tests (initializationOptions.debug)" Nothing))
          True -> inBackground env req respond $ \job -> liftIO (debugState job) >>= respond . Right
    ]

-- | Each open document, its version and its project (loading it).
debugState :: Env -> IO Value
debugState env = withSnapshotState env (\s0 -> do
  let docs = M.toList (ssDocs s0)
  (s, rows) <- foldr (\(path, (version, _)) acc -> do
                         (s', rs) <- acc
                         (s'', found) <- projectFor path s'
                         pure (s'', object ["path" .= path, "version" .= version, "project" .= fmap fst found] : rs))
                     (pure (s0, [])) docs
  pure (s, object ["documents" .= rows, "projects" .= M.mapKeys T.pack (ssLoads s), "broken" .= M.fromList [(T.pack root, why) | (root, Broken why _) <- M.toList (ssProjects s)], "recognitionIndexes" .= M.size (ssRecognition s), "recognitionFresh" .= M.size (M.filter (\(stamp, _, _) -> stamp == ssFilesStamp s) (ssRecognition s0)), "listingsFresh" .= M.size (M.filter ((== ssFilesStamp s) . fst) (ssListings s0))]))
    <&> snd

-- | Ask the client to watch more files (dynamic registration: the protocol
-- has no static capability for it), once per glob; a change reloads the
-- projects whose `.olifant/` or packs hold it.
watch :: Env -> [GlobPattern] -> LspM () ()
watch env globs = do
  let keyed = [(TE.decodeUtf8 (BSL.toStrict (Aeson.encode g)), g) | g <- globs]
  fresh <- liftIO (atomicModifyIORef' (envWatched env) (\seen -> let new = [kg | kg@(k, _) <- keyed, k `S.notMember` seen] in (foldr (S.insert . fst) seen new, map snd new)))
  if null fresh
    then pure ()
    else void $
      registerCapability mempty SMethod_WorkspaceDidChangeWatchedFiles (DidChangeWatchedFilesRegistrationOptions [FileSystemWatcher g Nothing | g <- fresh]) $ \msg ->
        liftIO (modifyMVar_ (envState env) (filesChanged [p | ev <- msg ^. L.params . L.changes, Just p <- [uriToFilePath (ev ^. L.uri)]]))

-- | Every file under a folder, as a pattern relative to it.
everything :: FilePath -> GlobPattern
everything dir = GlobPattern (InR (RelativePattern (InR (filePathToUri dir)) (Pattern "**/*")))

-- | The project of the document a request names (`textDocument.uri`).
-- | A picker's steps: [{generator, sets, makes?: {step, input}}].
pickSteps :: KM.KeyMap Value -> [PickStep]
pickSteps o = case KM.lookup "steps" o of
  Just (Aeson.Array ss) -> [PickStep g (setsIn st) (makesOf st) | Object st <- F.toList ss, Just (String g) <- [KM.lookup "generator" st]]
  _ -> []
  where
    makesOf st = case KM.lookup "makes" st of
      Just (Object m) | Just (Number k) <- KM.lookup "step" m, Just (String i) <- KM.lookup "input" m -> (,i) <$> toBoundedInteger k
      _ -> Nothing

-- | A preview as the picker reads it: per step its fields, the files the
-- plan writes (new, or changed with line counts), its main file's text,
-- whether it is ready, or why it cannot plan.
previewJson :: [PickStep] -> Preview -> Value
previewJson steps pv =
  object
    [ "steps" .= [object ["generator" .= psGenerator st, "fields" .= map fieldJson fs] | (st, fs) <- zip steps (pvFields pv)]
    , "files" .= [object ["path" .= p, "new" .= isNew p, "added" .= added p t, "removed" .= removed p t] | Just plan <- [pvPlan pv], (p, t) <- M.toList (planWrites plan)]
    , "main" .= (pvPlan pv >>= mainFile)
    , "ready" .= pvReady pv
    , "error" .= pvError pv
    ]
  where
    before p = pvPlan pv >>= M.lookup p . planBefore
    isNew p = before p == Just Nothing
    old p = maybe [] T.lines (before p >>= id)
    added p t = length (T.lines t) - common p t
    removed p t = length (old p) - common p t
    -- lines kept (as a multiset): what the counts leave out
    common p t = sum (M.elems (M.intersectionWith min (counts (old p)) (counts (T.lines t))))
    counts = M.fromListWith (+) . map (,1 :: Int)
    -- the main file: the first new one, else the first changed; with the
    -- first line (1-based) where it differs from before, to show the change
    mainFile plan =
      listToMaybe
        ( [main p t (1 :: Int) | (p, t) <- M.toList (planWrites plan), M.lookup p (planBefore plan) == Just Nothing]
            ++ [main p t (firstChange (maybe [] T.lines (M.lookup p (planBefore plan) >>= id)) (T.lines t)) | (p, t) <- M.toList (planWrites plan)]
        )
    main p t first = object ["path" .= p, "text" .= t, "first" .= first]
    firstChange xs ys = 1 + length (takeWhile id (zipWith (==) xs ys))
    fieldJson f =
      object
        [ "name" .= fName f
        , "value" .= fValue f
        , "source" .= fmap sourceText (fSource f)
        , "valid" .= fValid f
        , "why" .= fWhy f
        , "choices" .= fChoices f
        , "needs" .= fNeeds f
        , "new" .= fNew f
        ]
    sourceText = \case
      FromYou -> "you" :: T.Text
      FromPath -> "path"
      FromHere -> "here"
      OnlyChoice -> "only"
      FromDefault -> "default"
      FromNew -> "new"
      FromSuggested -> "suggested"

-- | A model's JSON with the migration report, if there is one.
withMigration :: Maybe MigrationReport -> Value -> Value
withMigration mig = \case
  Object o | Just m <- mig -> Object (KM.insert "migration" (migrationJson m) o)
  v -> v

-- | A request's document, as a path.
documentPath :: Value -> Maybe FilePath
documentPath = \case
  Object o | Just (Object doc) <- KM.lookup "textDocument" o, Just (String uri) <- KM.lookup "uri" doc -> uriToFilePath (Uri uri)
  _ -> Nothing

-- | An `olifant/plans/outcome`'s params: the plan, the errand's id, the
-- block's 0-based index and `accepted` or `rejected`; else Nothing.
outcomeParams :: Value -> Maybe (FilePath, T.Text, Int, T.Text)
outcomeParams params = do
  path <- documentPath params
  Object o <- pure params
  String eid <- KM.lookup "id" o
  Number n <- KM.lookup "block" o
  block <- toBoundedInteger n
  String outcome <- KM.lookup "outcome" o
  guard (not (T.null eid) && block >= 0 && outcome `elem` ["accepted", "rejected"])
  pure (path, eid, block, outcome)

-- | A conjure request's `resume` (token, choice, optional hint, arg and land;
-- an empty hint is none; `land` a boolean, false when absent; `wire` needs an
-- `arg` of the form `param: Port`): Nothing when it has none, an error when
-- it is not whole.
resumeOf :: Value -> Either T.Text (Maybe Resume)
resumeOf = \case
  Object o | Just r <- KM.lookup "resume" o -> case r of
    Object ro
      | Just (String tok) <- KM.lookup "token" ro
      , not (T.null tok)
      , Just (String c) <- KM.lookup "choice" ro
      , Just choice <- lookup c [("continue", ResumeContinue), ("climb", ResumeClimb), ("stop", ResumeStop), ("reconjure", ResumeReconjure), ("accept", ResumeAccept), ("reject", ResumeReject), ("wire", ResumeWire), ("port", ResumePort)]
      , Just arg <- case KM.lookup "arg" ro of
          Nothing -> Just Nothing
          Just Aeson.Null -> Just Nothing
          Just (String a) -> Just (Just a)
          _ -> Nothing
      , Just land <- case KM.lookup "land" ro of
          Nothing -> Just False
          Just Aeson.Null -> Just False
          Just (Bool b) -> Just b
          _ -> Nothing
      , choice /= ResumeWire || maybe False (\a -> length (filter (not . T.null . T.strip) (T.splitOn ":" a)) == 2) arg ->
          Right (Just (Resume tok choice (case KM.lookup "hint" ro of Just (String h) | not (T.null h) -> Just h; _ -> Nothing) arg land))
    _ -> Left "bad resume"
  _ -> Right Nothing

-- | What `olifant/conjure` answers: a landed conjure, or a paused one.
conjureJson :: ConjureOutcome -> Value
conjureJson = \case
  Landed a ->
    object
      [ "file" .= daFile a
      , "unit" .= daUnit a
      , "edits" .= (if null (daEdits a) then Aeson.Null else toJSON (acceptedEdit (daEdits a)))
      , "summary" .= daSummary a
      , "green" .= daGreen a
      , "tests" .= testsJson (daTests a)
      , "rounds" .= daRounds a
      , "rungs" .= daRungs a
      , "stopped" .= daStopped a
      ]
  Paused p -> object ["paused" .= pausedJson p]

-- | A request's `line` (0-based), if it has one.
lineOf :: Value -> Maybe Int
lineOf = \case
  Object o | Just (Number l) <- KM.lookup "line" o -> toBoundedInteger l
  _ -> Nothing

-- | A request's `position` (0-based line and character), if it has one.
positionOf :: Value -> Maybe (Int, Int)
positionOf = \case
  Object o
    | Just (Object p) <- KM.lookup "position" o
    , Just (Number l) <- KM.lookup "line" p
    , Just (Number c) <- KM.lookup "character" p ->
        (,) <$> toBoundedInteger l <*> toBoundedInteger c
  _ -> Nothing

-- | Each listed generator marked with whether it applies at the cursor.
withHere :: S.Set T.Text -> Value -> Value
withHere here = \case
  Object o | Just (Aeson.Array gs) <- KM.lookup "generators" o -> Object (KM.insert "generators" (Aeson.Array (fmap mark gs)) o)
  v -> v
  where
    mark = \case
      Object g | Just (String i) <- KM.lookup "id" g -> Object (KM.insert "here" (Bool (i `S.member` here)) g)
      g -> g

projectOfRequest :: Env -> Value -> IO (Maybe Project)
projectOfRequest env params = case params of
  Object o
    | Just (Object doc) <- KM.lookup "textDocument" o
    , Just (String uri) <- KM.lookup "uri" doc
    , Just path <- uriToFilePath (Uri uri) ->
        fmap snd <$> withSnapshot env (projectFor path)
  _ -> pure Nothing

toRange :: ((Int, Int), (Int, Int)) -> Range
toRange ((l1, c1), (l2, c2)) = Range (Position (fromIntegral l1) (fromIntegral c1)) (Position (fromIntegral l2) (fromIntegral c2))

-- | The line (1-based) an edit ends on, in the text after it: where to
-- look for what follows. None for a whole-document change.
lastEditedLine :: [TextDocumentContentChangeEvent] -> Maybe Int
lastEditedLine changes = case reverse changes of
  TextDocumentContentChangeEvent (InL partial) : _ ->
    let Range (Position l _) _ = partial ^. L.range
     in Just (fromIntegral l + 1 + T.count "\n" (partial ^. L.text))
  _ -> Nothing

-- | After the debounce (a newer edit of the file cancels it), plan what
-- follows from the edit and push it, unless the document has moved on;
-- then, when nothing sure was pushed, the anticipation.
suggestLater :: Env -> LanguageContextEnv () -> FilePath -> Int -> Int -> IO ()
suggestLater env lspEnv path version line = do
  debounce <- scDebounceMs . ssConfig <$> readMVar (envState env)
  void . submit (envJobs env) ("suggest " <> T.pack path) debounce $ \token -> do
    -- superseded by the next edit: its checks stop (review I5)
    let job = env {envCancel = token}
    found <- withSnapshot job (suggestOrRecognize path line)
    current <- versionOf env path
    case found of
      Just sg | current == Just version -> do
        pushSuggestion env lspEnv path version sg
        -- unsure: the deterministic answer is out; a configured model may
        -- sharpen it, for this version, if the buffer has not moved on
        upgraded <- withSnapshot job (recognitionUpgrade path sg)
        now <- versionOf env path
        case upgraded of
          Just better | now == Just version -> pushSuggestion env lspEnv path version better
          Just _ -> pure ()
          Nothing -> anticipateLater job lspEnv path version line (Just sg)
      Nothing | current == Just version -> anticipateLater job lspEnv path version line Nothing
      _ -> pure ()

-- | The anticipation, after the deterministic push (PUSHED, or none): when
-- the project asks Jev and nothing sure was pushed (none, a hint, or two
-- actions apply at the line), the one question, in the background and
-- asked once per prefix. A sure answer is pushed as an upgrade for this
-- version, unless the buffer has moved on (stale, Review Focus 1) or its
-- generator was quieted by three dismissals (quiet, Review Focus 4); each
-- outcome goes in the step log: shown, quiet, stale, unsure (under the
-- gate) or none (sure, but no suggestion comes of it).
anticipateLater :: Env -> LanguageContextEnv () -> FilePath -> Int -> Int -> Maybe Suggestion -> IO ()
anticipateLater env lspEnv path version line pushed
  | maybe False offered pushed = pure ()
  | otherwise =
      withSnapshotState env (projectFor path) >>= \case
        (s, Just (root, project)) | Just d <- deciderFor s project ->
          withSnapshot env (anticipationAsk path line) >>= \case
            Nothing -> pure ()
            Just (moment, fp, q) -> do
              let key = (path, "anticipate " <> fp)
                  -- the answer is kept: asked again, it is known at once
                  landed = decision s d key fp q (pure ()) >>= mapM_ (land env root moment)
              decision s d key fp q (landed `catch` \(_ :: SomeException) -> pure ()) >>= mapM_ (land env root moment)
        _ -> pure ()
  where
    offered sg = fmap Recognize.rcBand (sgRecognition sg) == Just Recognize.Offer
    land at root moment answer = case sureAnswer moment answer of
      Nothing -> logged root (unsure answer) "unsure"
      Just (g, c) -> sure at root moment answer g c
    -- what an answer under the gate chose, and how surely ("-" for none)
    unsure = \case
      De.Chosen g _ (De.Confidence c) -> (g, T.pack (show c))
      De.Chosen g _ De.Unknown -> (g, "-")
      _ -> ("-", "-")
    logged root (g, c) outcome = do
      ports <- ssPorts <$> readMVar (envState env)
      appendStepLine ports root ["anticipate", g, c, outcome]
    sure at root moment answer g c = do
      let note = logged root (g, T.pack (show c))
      now <- versionOf env path
      counts <- readMVar (envState env) >>= quietCounts root
      if now /= Just version
        then note "stale"
        else
          if M.findWithDefault 0 g counts >= 3
            then note "quiet"
            else
              withSnapshot at (anticipationFrom path line moment answer) >>= \case
                Nothing -> note "none"
                Just an -> do
                  still <- versionOf env path
                  if still == Just version
                    then pushSuggestion env lspEnv path version (anSuggestion an) >> note "shown"
                    else note "stale"

-- | The version of a document as the editor has it now.
versionOf :: Env -> FilePath -> IO (Maybe Int)
versionOf env path = fmap fst . M.lookup path . ssDocs <$> readMVar (envState env)

-- | A suggestion pushed (olifant/suggestion) for a version of a file, kept
-- for `olifant/plan`.
pushSuggestion :: Env -> LanguageContextEnv () -> FilePath -> Int -> Suggestion -> IO ()
pushSuggestion env lspEnv path version sg = do
  n <- offer env path version sg
  runLspT lspEnv . sendNotification (SMethod_CustomMethod (Proxy :: Proxy "olifant/suggestion")) $ object (suggestionFields path version n sg)

-- | The offer a model's yes made of a recognition hint, for this file at
-- this version (pushed by the suggest job), with its id: what a pull at
-- any line of the file answers instead of the hint.
upgradedFor :: Env -> FilePath -> Int -> Suggestion -> IO (Maybe (Int, Suggestion))
upgradedFor env path version sg = case sgRecognition sg of
  Just r | Recognize.rcBand r == Recognize.Hint -> do
    (_, m) <- readIORef (envOffered env)
    pure $
      listToMaybe
        [ (n, ofSuggestion o)
        | (n, o) <- M.toDescList m
        , ofPath o == path
        , ofVersion o == version
        , Just u <- [sgRecognition (ofSuggestion o)]
        , Recognize.rcBand u == Recognize.Offer
        ]
  _ -> pure Nothing

-- | Keep a suggestion's plan for `olifant/plan`, while its file is at
-- that version (a file's offers for older versions go): its id.
offer :: Env -> FilePath -> Int -> Suggestion -> IO Int
offer env path version sg =
  atomicModifyIORef' (envOffered env) $ \(k, m) ->
    let current = M.filter (\o -> ofPath o /= path || ofVersion o >= version) m
     in ((k + 1, M.insert (k + 1) (Offered path version sg) current), k + 1)

-- | A suggestion as editors read it (olifant/suggestion, olifant/suggest).
suggestionFields :: FilePath -> Int -> Int -> Suggestion -> [(Key.Key, Value)]
suggestionFields path version n sg =
  [ "uri" .= filePathToUri path
  , "version" .= version
  , "kind" .= sgKind sg
  , "label" .= sgLabel sg
  , "summary" .= sgSummary sg
  , "line" .= sgLine sg
  , "lines" .= sgLines sg
  , "at" .= sgAt sg
  , "planId" .= n
  , "generator" .= sgGenerator sg
  , "sets" .= M.fromList (sgSets sg)
  ]
    <> case sgRecognition sg of
      Nothing -> []
      Just r ->
        [ "band" .= (case Recognize.rcBand r of Recognize.Offer -> "offer" :: T.Text; Recognize.Hint -> "hint")
        , "or" .= fmap (\(GeneratorId g) -> g) (Recognize.rcOr r)
        , "reasons" .= Recognize.rcReasons r
        , "score" .= Recognize.rcScore r
        ]

-- | Run against the state as it is now, without holding it: what the work
-- loaded (projects) is kept; documents changed meanwhile are not undone.
withSnapshot :: Env -> (ServerState -> IO (ServerState, a)) -> IO a
withSnapshot env work = snd <$> withSnapshotState env work

-- | A plan made on a snapshot, with the state it was made on: edits are
-- stamped with that state's document versions.
withPlanned :: Env -> (ServerState -> IO (ServerState, Either e (FilePath, Plan))) -> IO (Either e (ServerState, (FilePath, Plan)))
withPlanned env work = withSnapshotState env work <&> \(at, planned) -> (at,) <$> planned

-- | 'withSnapshot', with the state the work ran on (the documents as they
-- were when it started).
withSnapshotState :: Env -> (ServerState -> IO (ServerState, a)) -> IO (ServerState, a)
withSnapshotState env work = do
  before <- (\s -> s {ssCancel = envCancel env}) <$> readMVar (envState env)
  (after, a) <- work before
  let loaded = M.filter (> 0) (M.unionWith (-) (ssLoads after) (ssLoads before))
  modifyMVar_ (envState env) $ \now -> do
    -- the reconciles it cached (one that changed wins over the live one);
    -- forced here, so no thunk holds the two snapshots
    let reconciled = M.union (M.differenceWith (\x y -> if fst x == fst y then Nothing else Just x) (ssReconciled after) (ssReconciled before)) (ssReconciled now)
    reconciled `seq`
      pure
        now
        { -- projects it loaded stand only if no file changed meanwhile: a
          -- configuration changed under a load is read again when next asked
          ssProjects = M.union (ssProjects now) (if ssFilesStamp now == ssFilesStamp before then ssProjects after else M.empty)
        , ssLoads = M.unionWith (+) (ssLoads now) loaded
        , -- what it worked out stands only if nothing changed meanwhile
          -- (a fresh one replaces one gone stale: a save leaves the old in place)
          ssListings = M.union (M.filter ((== ssFilesStamp now) . fst) (ssListings after)) (ssListings now)
        , ssMemo = M.union (ssMemo now) (M.filter ((== ssGeneration now) . fst) (ssMemo after))
        , ssEstablished = M.union (ssEstablished now) (ssEstablished after)
        , ssReconciled = reconciled
        , -- a recognition index it built, while no file changed since
          ssRecognition = M.union (M.filter (\(stamp, _, _) -> stamp == ssFilesStamp now) (ssRecognition after)) (ssRecognition now)
        }
  pure (after, a)

-- | A file's findings as diagnostics (with the finding each is); a plan's
-- stale and missing anchors too, which are no finding (they have no fix).
diagnosticsOf :: Env -> FilePath -> IO [(Diagnostic, Maybe Finding)]
diagnosticsOf env path = do
  fs <- withSnapshot env (findingsFor path)
  text <- documentText env path
  plan <- withSnapshot env (\s -> projectFor path s >>= \case
    (s', Just (_, project)) | isPlanPath project path -> planDiagnostics s' project path text >>= \(ds, prints) -> do
      atomicModifyIORef' (envPlanPrints env) (\m -> (M.insert path prints m, ()))
      pure (s', ds)
    (s', _) -> pure (s', []))
  let ls = T.splitOn "\n" text
      lineText n = fromMaybe "" (listToMaybe (drop (n - 1) ls))
      position (l, c) = let (l', c') = editorPosition (lineText l) (l, c) in Position (fromIntegral l') (fromIntegral c')
      range f = maybe (Range (Position 0 0) (Position 0 0)) (\(a, b) -> Range (position a) (position b)) (fRange f)
  pure
    ( [ (Diagnostic (range f) (Just sev) (Just (InR (fRule f))) Nothing (Just "olifant") (fMessage f) Nothing Nothing Nothing, Just f)
      | f <- fs
      , Just sev <- [severityOf (fSeverity f)]
      ]
        ++ map (,Nothing) plan
    )
  where
    severityOf = \case
      R.Error -> Just DiagnosticSeverity_Error
      R.Warn -> Just DiagnosticSeverity_Warning
      R.Hint -> Just DiagnosticSeverity_Hint
      R.Off -> Nothing

-- | Each fix of the findings on lines @from@–@to@ (1-based), as a quickfix
-- to resolve: its data says which fix, with the choices it still needs.
fixActions :: Env -> FilePath -> Int -> Int -> IO [CodeAction]
fixActions env path from to = do
  diagnostics <- diagnosticsOf env path
  version <- fmap fst . M.lookup path . ssDocs <$> readMVar (envState env)
  let here = [(d, f, l1) | (d, Just f) <- diagnostics, let (l1, l2) = maybe (1, 1) (\((a, _), (b, _)) -> (a, b)) (fRange f), l1 <= to, from <= l2]
      actions =
        [ CodeAction
            { _title = fromMaybe (foGenerator o) (foLabel o)
            , _kind = Just CodeActionKind_QuickFix
            , _diagnostics = Just [d]
            , _isPreferred = Just (i == 1)
            , _disabled = Nothing
            , _edit = Nothing
            , _command = Nothing
            , _data_ = Just (object ["kind" .= ("fix" :: T.Text), "path" .= path, "rule" .= fRule f, "generator" .= foGenerator o, "line" .= line, "index" .= i, "sets" .= M.fromList (foSets o), "missing" .= map fst (foChoices o), "choices" .= M.fromList (foChoices o), "version" .= version])
            }
        | (d, f, line) <- here
        , (i, o) <- zip [1 :: Int ..] (fFixes f)
        ]
  pure actions

-- | The ⚡ actions at a cursor, to resolve: each one's data says which
-- generator, what it sets, what it still needs (and the choices for it).
cursorActions :: Env -> FilePath -> Position -> IO [CodeAction]
cursorActions env path (Position l c) = do
  text <- documentText env path
  let lineText = fromMaybe "" (listToMaybe (drop (fromIntegral l) (T.splitOn "\n" text)))
      (line, col) = bytePosition lineText (fromIntegral l, fromIntegral c)
  acts <- withSnapshot env (menuActionsFor path (line, col))
  version <- fmap fst . M.lookup path . ssDocs <$> readMVar (envState env)
  pure
    [ CodeAction
        { _title = actLabel a
        , _kind = Just CodeActionKind_RefactorRewrite
        , _diagnostics = Nothing
        , _isPreferred = Nothing
        , _disabled = Nothing
        , _edit = Nothing
        , _command = Nothing
        , _data_ = Just (object ["kind" .= ("action" :: T.Text), "path" .= path, "generator" .= actGenerator a, "sets" .= M.fromList (actSets a), "missing" .= actMissing a, "choices" .= M.fromList (actChoices a), "snippet" .= snippet, "line" .= line, "col" .= col, "version" .= version])
        }
    | (a, snippet) <- acts
    ]

-- | A plan's anchor completions at a cursor: each replaces what was typed
-- of its path (after `@`) or its unit (after `:`).
anchorCompletions :: Env -> Project -> FilePath -> Position -> IO [CompletionItem]
anchorCompletions env project path (Position l c) = do
  text <- documentText env path
  let upTo = takeUtf16 (fromIntegral c) (lineAt (T.splitOn "\n" text) (fromIntegral l))
      typed = T.takeWhileEnd (\ch -> ch /= '@' && ch /= ':') upTo
      range = Range (Position l (c - fromIntegral (utf16 typed))) (Position l c)
  if T.all isSpace upTo
    then pure [newStepItem (Position l c)]
    else do
      items <- withSnapshot env (\s -> (s,) <$> anchorItems s project upTo)
      pure [i & L.textEdit ?~ InL (TextEdit range (fromMaybe (i ^. L.label) (i ^. L.insertText))) | i <- items]

-- | The new-step snippet, for a blank line of a plan.
newStepItem :: Position -> CompletionItem
newStepItem pos =
  (CompletionItem "- [ ] step @anchor" Nothing (Just CompletionItemKind_Snippet) Nothing (Just "new step") Nothing Nothing Nothing Nothing Nothing (Just newStepSnippet) (Just InsertTextFormat_Snippet) Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
    & L.textEdit ?~ InL (TextEdit (Range (Position (pos ^. L.line) 0) pos) newStepSnippet)

-- | "New step below" for a plan: the same line as 'newStepItem', as plain
-- text under the cursor's line.
newStepAction :: Env -> FilePath -> Position -> IO [CodeAction]
newStepAction env path (Position l _) =
  withSnapshot env (projectFor path) >>= \case
    Just (_, project) | isPlanPath project path -> do
      text <- documentText env path
      let end = Position l (fromIntegral (utf16 (lineAt (T.splitOn "\n" text) (fromIntegral l))))
          edit = WorkspaceEdit (Just (M.singleton (filePathToUri path) [TextEdit (Range end end) ("\n" <> newStepText)])) Nothing Nothing
      pure [CodeAction "New step below" (Just CodeActionKind_Source) Nothing Nothing Nothing (Just edit) Nothing Nothing]
    _ -> pure []

-- | "Show the code here" on a step whose anchor resolves and that has no
-- excerpt (no child opening one, closed or not): resolved later
-- ('excerptEdit'), so a request on every cursor move runs no git and no clip
-- (review M3); its data holds the plan's version it was offered at. On a
-- ticked step it writes the excerpt frozen. Asked for source actions (ONLY
-- naming `source`, as the editors' keys ask), elsewhere in a plan it comes
-- disabled with the reason (LSP 3.16); on a step with an excerpt its data
-- names the excerpt's opening row (an editor folds it).
excerptAction :: Env -> FilePath -> Maybe [CodeActionKind] -> Position -> IO [CodeAction]
excerptAction env path only (Position l _) =
  withSnapshot env (\s -> projectFor path s >>= \case
    (s', Just (_, project)) | isPlanPath project path -> do
      text <- documentText env path
      case excerptStep text (fromIntegral l) of
        Right (_, a) -> resolveAnchor s' project a <&> \r -> (s', [if isJust (rsUnit r) then Right () else Left ("the anchor " <> anchorText a <> " does not resolve" <> maybe "" (": " <>) (rsWhy r), Nothing)])
        Left why -> pure (s', [Left why])
    (s', _) -> pure (s', [])) >>= \found -> do
    version <- fmap fst . M.lookup path . ssDocs <$> readMVar (envState env)
    let asked = maybe False (any (`elem` [CodeActionKind_Source])) only
        showing disabled extra = CodeAction "Show the code here" (Just CodeActionKind_Source) Nothing Nothing disabled Nothing Nothing
          (Just (object (["kind" .= ("excerpt" :: T.Text), "path" .= path, "line" .= l, "version" .= version] ++ extra)))
    pure
      [ either (\(why, open) -> showing (Just (CodeActionDisabled why)) ["open" .= o | Just o <- [open]]) (const (showing Nothing [])) f
      | f <- found, either (const asked) (const True) f ]

-- | The anchored step at ROW of a plan's TEXT with no child opening an
-- excerpt (closed or not, live or frozen), and its anchor; else why not,
-- with the opening row of the step's excerpt when it has one.
excerptStep :: T.Text -> Int -> Either (T.Text, Maybe Int) (Line, Anchor)
excerptStep text row = case [ln | ln <- ls, lnRow ln == row] of
  ln : _ | Step _ <- lnKind ln -> case lnAnchor ln of
    Nothing -> Left ("no anchor on this line", Nothing)
    Just a
      | o : _ <- [o | (st, o, _) <- excerptSpans ls, st == row] -> Left ("the step shows its code already", Just o)
      | any (isJust . excerptInfo . lnText) (childrenOf ls row) -> Left ("the step's excerpt has no closing line", Nothing)
      | otherwise -> Right (ln, a)
  _ -> Left ("not on a step", Nothing)
  where
    ls = parsePlan text

-- | "Show the code here" resolved: its unit's code as a fence below the step
-- line at LINE (frozen under a ticked step), as an edit of the plan at VERSION, the one it was offered at
-- (refused when the plan has moved on since, and the editor refuses it if
-- the plan moves on before it lands, review M4). In a CRLF plan the fence
-- goes before the step's carriage return, its lines joined by CRLF.
excerptEdit :: Env -> FilePath -> Int -> Maybe Int -> IO (Either T.Text WorkspaceEdit)
excerptEdit env path l version =
  withSnapshot env (\s -> projectFor path s >>= \case
    (s', Just (_, project)) | isPlanPath project path -> do
      text <- documentText env path
      let now = fst <$> M.lookup path (ssDocs s')
      case excerptStep text l of
        _ | now /= version -> pure (s', Left "the plan changed since; ask again")
        Right (ln, a) -> do
          base <- baseline (prRoot project) (makeRelative (prRoot project) (normalisePath path))
          unitExcerpt s' project base a <&> \case
            Just (lang, body) -> (s', Right (fenceEdit ln (renderFence (lnIndent ln + 2) lang a (stepDone ln) body)))
            Nothing -> (s', Left "the step's anchor does not resolve")
        Left (why, _) -> pure (s', Left why)
    (s', _) -> pure (s', Left "not a plan"))
  where
    fenceEdit ln fence =
      let (bare, eol) = maybe (lnText ln, "\n") (,"\r\n") (T.stripSuffix "\r" (lnText ln))
          col = utf16 bare
       in workspaceEditAsking False [FileEdit path version False ((l, col), (l, col)) (T.concat [eol <> f | f <- fence])]

-- | "Mark done" on an open step, "Reopen step" on a ticked one, at its line
-- or a line under it (kind `source`): resolved later ('toggleEdit'), its
-- data the step's row and the plan's version. Asked for source actions, off
-- a step it comes disabled ("not on a step").
toggleAction :: Env -> FilePath -> Maybe [CodeActionKind] -> Position -> IO [CodeAction]
toggleAction env path only (Position l _) =
  withSnapshot env (\s -> projectFor path s <&> \(s', p) -> (s', fmap snd p)) >>= \case
    Just project | isPlanPath project path -> do
      text <- documentText env path
      version <- fmap fst . M.lookup path . ssDocs <$> readMVar (envState env)
      let asked = maybe False (elem CodeActionKind_Source) only
          toggling title disabled row = CodeAction title (Just CodeActionKind_Source) Nothing Nothing disabled Nothing Nothing
            (Just (object ["kind" .= ("toggle" :: T.Text), "path" .= path, "line" .= row, "version" .= version]))
      pure $ case stepOf (parsePlan text) (fromIntegral l) of
        Just st -> [toggling (if stepDone st then "Reopen step" else "Mark done") Nothing (lnRow st)]
        Nothing -> [toggling "Mark done" (Just (CodeActionDisabled "not on a step")) (fromIntegral l :: Int) | asked]
    _ -> pure []

-- | The step at ROW of LS, or the nearest one ROW is a line under.
stepOf :: [Line] -> Int -> Maybe Line
stepOf ls row =
  listToMaybe
    [ st
    | st <- reverse (take (row + 1) ls)
    , case lnKind st of Step _ -> True; _ -> False
    , lnRow st == row || row `elem` map lnRow (childrenOf ls (lnRow st)) ]

-- | "Mark done" or "Reopen step" resolved, as an edit of the plan at
-- VERSION (refused when it has moved on): ticked, with ✓ its unit's
-- fingerprint now when its anchor resolves (an explicit claim: no reading
-- offers it again) and its live excerpts frozen with the code now; or
-- unticked with its verdict dropped, a frozen excerpt left as it is.
toggleEdit :: Env -> FilePath -> Int -> Maybe Int -> IO (Either T.Text WorkspaceEdit)
toggleEdit env path row version =
  withSnapshot env (\s -> projectFor path s >>= \case
    (s', Just (_, project)) | isPlanPath project path -> do
      text <- documentText env path
      let now = fst <$> M.lookup path (ssDocs s')
          ls = parsePlan text
      case listToMaybe [st | st <- ls, lnRow st == row, case lnKind st of Step _ -> True; _ -> False] of
        _ | now /= version -> pure (s', Left "the plan changed since; ask again")
        Nothing -> pure (s', Left "not on a step")
        Just st | stepDone st -> pure (s', Right (factsEdit path version text [Replace row (withVerdict (checkbox "- [x]" "- [ ]" (lnText st)) NoVerdict)]))
        Just st -> (s',) . Right . factsEdit path version text <$> doneFacts s' project path text row (checkbox "- [ ]" "- [x]" (lnText st))
    (s', _) -> pure (s', Left "not a plan"))

-- | The facts that mark the step at ROW of TEXT done, TICKED its line
-- already ticked: ✓ its unit's fingerprint now when its anchor resolves (an
-- explicit claim: no reading offers it again) and its live excerpts frozen
-- with the code now. "Mark done", and `keep` on a step with no tests.
doneFacts :: ServerState -> Project -> FilePath -> T.Text -> Int -> T.Text -> IO [Fact]
doneFacts s project path text row ticked0 = do
  fp <- maybe (pure Nothing) (\a -> resolveAnchor s project a <&> \r -> rsFingerprint r <* rsUnit r) (listToMaybe (parsePlan ticked0) >>= lnAnchor)
  let ticked = maybe id (\f t -> withVerdict t (Verified f)) fp ticked0
      after = parsePlan (applyFactsText [Replace row ticked] text)
      mine = [e | e <- excerptsOf after, exStep e == row, not (exDone e)]
  base <- if null mine then pure Nothing else baseline (prRoot project) (makeRelative (prRoot project) (normalisePath path))
  codes <- mapM (\e -> (exAnchor e,) <$> unitExcerpt s project base (exAnchor e)) mine
  let freeze = [f | f <- excerptFacts after (const Nothing) (\a -> join (lookup a codes)), factRow f `elem` map exOpen mine]
  pure (Replace row ticked : freeze)
  where
    factRow = \case
      Replace r _ -> r
      ReplaceRows r _ _ -> r
      Insert r _ -> r

-- | A "Mark done" or "Reopen step" action's data: its plan, step row and version.
toggleData :: Value -> Maybe (FilePath, Int, Maybe Int)
toggleData = kindData "toggle"

-- | A "Show the code here" action's data: its plan, step line and version.
excerptData :: Value -> Maybe (FilePath, Int, Maybe Int)
excerptData = kindData "excerpt"

-- | An action's data of KIND: its plan, line and version.
kindData :: T.Text -> Value -> Maybe (FilePath, Int, Maybe Int)
kindData kind = \case
  Object o | KM.lookup "kind" o == Just (String kind) -> do
    String path <- KM.lookup "path" o
    Number line <- KM.lookup "line" o
    let version = case KM.lookup "version" o of
          Just (Number v) -> toBoundedInteger v
          _ -> Nothing
    (T.unpack path,,version) <$> toBoundedInteger line
  _ -> Nothing

-- | A hover over a plan's anchor: its unit's code; none off an anchor, or
-- when it does not resolve.
planHover :: Env -> Project -> FilePath -> Position -> IO (Maybe Hover)
planHover env project path (Position l c) = do
  text <- documentText env path
  let col = T.length (takeUtf16 (fromIntegral c) (lineAt (T.splitOn "\n" text) (fromIntegral l)))
  case [a | ln <- parsePlan text, lnRow ln == fromIntegral l, Just a <- [lnAnchor ln], anStart a <= col, col <= anEnd a] of
    a : _ -> withSnapshot env (\s -> (s,) <$> unitHover s project a) <&> fmap (\md -> Hover (InL (MarkupContent MarkupKind_Markdown md)) Nothing)
    [] -> do
      -- a bare line: what the claim pass found for it
      store <- readMVar (envState env) >>= \s -> claimsOf s (prRoot project) path
      pure $ case [ln | ln <- parsePlan text, lnRow ln == fromIntegral l] of
        ln : _ | Just md <- claimHover store ln -> Just (Hover (InL (MarkupContent MarkupKind_Markdown md)) Nothing)
        _ -> Nothing

-- | The start of a line, up to a UTF-16 column.
takeUtf16 :: Int -> T.Text -> T.Text
takeUtf16 n t = case T.uncons t of
  Just (ch, rest) | n > 0 -> T.cons ch (takeUtf16 (n - (if fromEnum ch > 0xFFFF then 2 else 1)) rest)
  _ -> ""

-- | Completion items for the snippets at a cursor: each replaces the
-- cursor's line with its body (tab stops), filtered by what the line holds.
snippetItems :: Env -> FilePath -> Position -> IO [CompletionItem]
snippetItems env path (Position l c) = do
  text <- documentText env path
  let lineText = lineAt (T.splitOn "\n" text) (fromIntegral l)
      (line, col) = bytePosition lineText (fromIntegral l, fromIntegral c)
      range = Range (Position l 0) (Position l (fromIntegral (utf16 lineText)))
  snippets <- withSnapshot env (completionsFor path (line, col))
  pure
    [ CompletionItem
        { _label = cpLabel cp
        , _labelDetails = Nothing
        , _kind = Just CompletionItemKind_Snippet
        , _tags = Nothing
        , _detail = Just (cpGenerator cp)
        , _documentation = Nothing
        , _deprecated = Nothing
        , _preselect = Nothing
        , _sortText = Nothing
        , _filterText = Just (if T.null (T.strip lineText) then cpLabel cp else lineText)
        , _insertText = Nothing
        , _insertTextFormat = Just InsertTextFormat_Snippet
        , _insertTextMode = Nothing
        , _textEdit = Just (InL (TextEdit range (snBody sn)))
        , _textEditText = Nothing
        , _additionalTextEdits = Nothing
        , _commitCharacters = Nothing
        , _command = Nothing
        , _data_ = Just (object ["kind" .= ("action" :: T.Text), "path" .= path, "generator" .= cpGenerator cp, "sets" .= M.fromList (cpSets cp), "line" .= line])
        }
    | (cp, sn) <- snippets
    ]

-- | The inline completion at a cursor: the anticipated snippet, unfilled,
-- replacing the cursor's line.
ghostItems :: Env -> FilePath -> Position -> IO [Value]
ghostItems env path (Position l c) = do
  text <- documentText env path
  let lineText = lineAt (T.splitOn "\n" text) (fromIntegral l)
      (line, col) = bytePosition lineText (fromIntegral l, fromIntegral c)
  withSnapshot env (ghostFor path (line, col)) <&> \case
    Nothing -> []
    Just (generator, sets, sn) ->
      [ object
          [ "insertText" .= object ["kind" .= ("snippet" :: T.Text), "value" .= snBody sn]
          , "range" .= Range (Position l 0) (Position l (fromIntegral (utf16 lineText)))
          , "filterText" .= lineText
          , -- beyond LSP: the ghost as the engine draws it (holes as ⟨name⟩),
            -- and what completionItem/resolve takes to fill it with the model
            "ghost" .= snGhost sn
          , "data" .= object ["kind" .= ("action" :: T.Text), "path" .= path, "generator" .= generator, "sets" .= M.fromList sets, "line" .= line]
          ]
      ]

-- | A resolved code action: its plan's edit, and the plan itself in its
-- data (the editors' plan JSON, files before and after, for a preview).
-- Its edits are for the documents as they were planned (the versions of
-- that state), never as they are when it answers (review C1).
resolved :: CodeAction -> (ServerState, (FilePath, Plan)) -> CodeAction
resolved action (at, (root, plan)) = action & L.edit ?~ workspaceEdit (editsOfPlan at root plan) & L.data_ %~ fmap withPlan
  where
    withPlan = \case
      Object o -> Object (KM.insert "plan" (planJson False plan) o)
      other -> other

-- | "Fill with the model" on a line holding a hole.
holeActions :: Env -> FilePath -> Int -> IO [CodeAction]
holeActions env path line =
  withSnapshot env (holeAt path line) <&> \case
    Nothing -> []
    Just (_, _, h) ->
      [ CodeAction
          { _title = "Fill with the model"
          , _kind = Just CodeActionKind_RefactorRewrite
          , _diagnostics = Nothing
          , _isPreferred = Nothing
          , _disabled = Nothing
          , _edit = Nothing
          , _command = Nothing
          , _data_ = Just (object ["kind" .= ("hole" :: T.Text), "path" .= path, "line" .= line, "name" .= hoName h])
          }
      ]

holeData :: Value -> Maybe (FilePath, Int)
holeData = \case
  Object o | KM.lookup "kind" o == Just (String "hole") -> do
    String path <- KM.lookup "path" o
    Number line <- KM.lookup "line" o
    (T.unpack path,) <$> toBoundedInteger line
  _ -> Nothing

-- | Returns once the token is tripped (polled every 50 ms).
untilCancelled :: Cancel -> IO ()
untilCancelled token = cancelled token >>= \stop -> if stop then pure () else threadDelay 50000 >> untilCancelled token

-- | Answer a request from a thread of its own, so slow work never holds up
-- the server; a failure there is answered as an error, never left hanging.
-- It runs under a token of its own, tripped by the client's
-- `$/cancelRequest` for it: its checks, verifies and model calls stop,
-- their processes killed (review I5).
inBackground :: Env -> TRequestMessage m -> (Either (TResponseError m) a -> LspM () ()) -> (Env -> LspM () ()) -> LspM () ()
inBackground env req respond work = do
  lspEnv <- getLspEnv
  (token, trip) <- liftIO newCancel
  let key = requestKey (req ^. L.id)
  liftIO (atomicModifyIORef' (envRunning env) (\m -> (M.insert key trip m, ())))
  void . liftIO . forkIO $
    ( runLspT lspEnv (work env {envCancel = token}) `catch` \(e :: SomeException) -> case fromException e :: Maybe Lsp.ProgressCancelledException of
        -- the client cancelled it from its progress: stopped, not failed
        Just _ -> trip >> runLspT lspEnv (respond (Left (TResponseError (InL LSPErrorCodes_RequestCancelled) "cancelled" Nothing)))
        Nothing -> case fromException e of
          -- a grammar that cannot load: its stop, not an internal error
          Just (GrammarUnavailable t) -> runLspT lspEnv (grammarStopped env t >> respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) t Nothing)))
          Nothing -> runLspT lspEnv (respond (Left (TResponseError (InR ErrorCodes_InternalError) ("olifant lsp: " <> T.pack (displayException e)) Nothing)))
    )
      `finally` atomicModifyIORef' (envRunning env) (\m -> (M.delete key m, ()))

-- | The claim pass on the plan at PATH, in the background under "checking":
-- one per plan (a new one cancels the running one; an edit cancels it too).
-- Its anchors land by applyEdit only at the version it read; then the
-- diagnostics and tokens are pulled again.
startPass :: Env -> FilePath -> LspM () ()
startPass env path =
  liftIO (planContext env path) >>= \case
    Just (p, _, text) | isPlanPath p path -> do
      lspEnv <- getLspEnv
      (token, trip) <- liftIO newCancel
      n <- liftIO $ do
        (n, old) <- atomicModifyIORef' (envPasses env) (\(i, m) -> ((i + 1, M.insert path (i + 1, trip) m), (i + 1, M.lookup path m)))
        maybe (pure ()) snd old
        -- the lines it works on show as being checked
        st <- readMVar (envState env)
        store <- claimsOf st (prRoot p) path
        let keys = S.fromList [claimKey (workLine w) | w <- passWork (parsePlan text) store]
        atomicModifyIORef' (ssChecking st) (\m -> (M.insert (normalisePath path) keys m, ()))
        pure n
      -- the client may cancel its progress: the pass stops, the margin clears
      void . liftIO . forkIO . (`catch` \(_ :: SomeException) -> trip >> runLspT lspEnv claimsChanged) . runLspT lspEnv $ do
        answered <- planProgress "checking" (\_ -> try (withSnapshotState env {envCancel = token} (claimPass token path)) :: IO (Either SomeException (ServerState, Either T.Text PassAnswer)))
        stopped <- liftIO (cancelled token)
        mine <- liftIO $ do
          mine <- atomicModifyIORef' (envPasses env) (\(i, m) -> case M.lookup path m of
            Just (j, _) | j == n -> ((i, M.delete path m), True)
            _ -> ((i, m), False))
          st <- readMVar (envState env)
          when mine (atomicModifyIORef' (ssChecking st) (\m -> (M.delete (normalisePath path) m, ())))
          pure mine
        case answered of
          Right (read', Right a) | not stopped -> do
            now <- liftIO (M.lookup path . ssDocs <$> readMVar (envState env))
            case (fst <$> M.lookup path (ssDocs read'), now) of
              (Just v, Just (v', current)) | v == v', not (null (paFacts a)) ->
                void (sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams (Just "Olifant: checked") (factsEdit path (Just v) current (paFacts a))) (\_ -> pure ()))
              _ -> pure ()
            claimsChanged
          _ -> when mine claimsChanged
    _ -> pure ()
  where
    workLine = \case
      Claims.ResolveAndCheck l -> l
      Claims.ResolveOnly l -> l
      Claims.WriteAnchor l _ -> l
    claimsChanged = do
      void (sendRequest SMethod_WorkspaceDiagnosticRefresh Nothing (\_ -> pure ()))
      semanticTokensChanged

-- | Cancel the claim pass running on the plan at PATH, if any.
stopPass :: Env -> FilePath -> IO ()
stopPass env path = readIORef (envPasses env) >>= \(_, m) -> maybe (pure ()) snd (M.lookup path m)

-- | `olifant/plans/<verb>` (params: textDocument, line): the errand in the
-- background under a progress TITLE. A `do` runs alone: a second while one
-- runs is refused without starting, and the lock is let go however the
-- first ends (answered, failed, cancelled).
errandRequest :: Env -> Verb -> T.Text -> TRequestMessage m -> Value -> (Either (TResponseError m) Value -> LspM () ()) -> LspM () ()
errandRequest env verb title req params respond = case documentPath params of
  Just path
    | verb == Do ->
        liftIO (reopenOf env path params) >>= \case
          Left why -> failed why
          Right reopen -> case (snd <$> reopen) <|> lineOf params of
            Nothing -> invalid
            Just line ->
              liftIO (tryTakeMVar (envDoing env)) >>= \case
                Nothing -> failed "a do is already running"
                Just () -> do
                  lspEnv <- getLspEnv
                  let release = void (tryPutMVar (envDoing env) ())
                  -- taken here, in the order requests arrive; let go in the work's finally
                  inBackground env req respond (\job -> liftIO (runLspT lspEnv (run job path line (fst <$> reopen)) `finally` release))
    | Just line <- lineOf params -> inBackground env req respond (\job -> run job path line Nothing)
  _ -> invalid
  where
    invalid = respond (Left (TResponseError (InR ErrorCodes_InvalidParams) ("olifant/plans/" <> name <> " needs a textDocument and a line") Nothing))
    name = case verb of Ask -> "ask"; Validate -> "validate"; Do -> "do"
    failed why = respond (Left (TResponseError (InL LSPErrorCodes_RequestFailed) why Nothing))
    run job path line reopen = do
      lspEnv <- getLspEnv
      -- a do's review is ready: the editor opens it (before the answer)
      let ready rid = runLspT lspEnv $ do
            sendNotification (SMethod_CustomMethod (Proxy :: Proxy "olifant/plans/review/ready")) (object ["id" .= rid, "kind" .= ("do" :: T.Text), "textDocument" .= object ["uri" .= filePathToUri path]])
            lensesChanged
      before <- liftIO (versionOf env path)
      answered <- planProgress title (\_ -> withSnapshot job (runErrand stepTestFacts ready reopen (envCancel job) verb path line))
      -- an errand the editor cancelled is no result (its runner was stopped)
      stopped <- liftIO (cancelled (envCancel job))
      doc <- liftIO (M.lookup path . ssDocs <$> readMVar (envState env))
      case answered of
        _ | stopped -> respond (Left (TResponseError (InL LSPErrorCodes_RequestCancelled) "cancelled" Nothing)) >> flushJournal env path
        Left why -> failed why >> flushJournal env path
        -- a do's journal lines ride its facts while the plan is as the do
        -- read it; else they land by their own edit
        Right a | verb == Do, Just (v, text) <- doc, before == Just v -> do
          (jf, land) <- liftIO (journalOn env path text)
          liftIO land
          respond (Right (errandJson name a {eaFacts = withJournal (eaFacts a) jf}))
        Right a -> respond (Right (errandJson name a)) >> when (verb == Do) (flushJournal env path)

-- | What `olifant/plans/<verb>` answers.
errandJson :: T.Text -> ErrandAnswer -> Value
errandJson verb a =
  object
    [ "id" .= eaId a
    , "verb" .= verb
    , "blocks" .= [object ["after" .= blAfterRow b, "after_text" .= blAfterText b, "lines" .= [object ["text" .= ghText g, "fp" .= fp] | (g, fp) <- zip (blLines b) fps]] | (b, fps) <- eaBlocks a]
    , "dropped" .= eaDropped a
    , "unresolved" .= eaUnresolved a
    , "seconds" .= eaSeconds a
    , "cost" .= eaCost a
    , "changed" .= eaChanged a
    , "facts" .= map factJson (eaFacts a)
    , "review" .= eaReview a
    ]

-- | The open reviews changed: the editor asks for the lenses again.
semanticTokensChanged :: LspM () ()
semanticTokensChanged = void (sendRequest SMethod_WorkspaceSemanticTokensRefresh Nothing (\_ -> pure ()))

-- | Tokens as the protocol's flat list: five integers each (line and
-- column as deltas from the token before, length, type index, modifier
-- bits), by the legend's order.
encodePlanTokens :: [PlanToken] -> [UInt]
encodePlanTokens ts = concat (zipWith one (Nothing : map Just ts) ts)
  where
    one prev t =
      let sameRow = fmap ptRow prev == Just (ptRow t)
          dRow = maybe (ptRow t) (\q -> ptRow t - ptRow q) prev
          dCol = case prev of Just q | sameRow -> ptCol t - ptCol q; _ -> ptCol t
       in map fromIntegral [dRow, dCol, ptLen t, index planTokenTypes (ptType t), sum [2 ^ i | m <- ptMods t, i <- [index planTokenModifiers m]]]
    index xs x = length (takeWhile (/= x) xs)

lensesChanged :: LspM () ()
lensesChanged = void (sendRequest SMethod_WorkspaceCodeLensRefresh Nothing (\_ -> pure ()))

-- | A review request's `id`.
reviewIdOf :: Value -> Maybe T.Text
reviewIdOf = \case
  Object o | Just (String i) <- KM.lookup "id" o, not (T.null i) -> Just i
  _ -> Nothing

-- | A request's `choice`.
choiceParam :: Value -> Maybe T.Text
choiceParam = textParam "choice"

-- | A request's text field KEY.
textParam :: Key.Key -> Value -> Maybe T.Text
textParam key = \case
  Object o | Just (String c) <- KM.lookup key o -> Just c
  _ -> Nothing

-- | A request's whole-number field KEY.
lineParam :: Key.Key -> Value -> Maybe Int
lineParam key = \case
  Object o | Just (Number l) <- KM.lookup key o -> toBoundedInteger l
  _ -> Nothing

-- | An answer object with KVS added (an answer that is no object: as it is).
withKeys :: [(Key.Key, Value)] -> Value -> Value
withKeys kvs = \case
  Object o -> Object (KM.union (KM.fromList kvs) o)
  v -> v

-- | An answer object without KEY.
without :: Value -> Key.Key -> Value
without v key = case v of
  Object o -> Object (KM.delete key o)
  _ -> v

-- | FACTS (rows of TEXT, the plan at PATH) as one edit of the plan at
-- VERSION: the client refuses it when the plan has moved on.
factsEdit :: FilePath -> Maybe Int -> T.Text -> [Fact] -> WorkspaceEdit
factsEdit path version text facts =
  let (range, t) = smallestChange text (applyFactsText facts text)
   in workspaceEditAsking False [FileEdit path version False range t]

-- | A reconcile's facts sent as `workspace/applyEdit` of the plan at the
-- version the reconcile read; how many were sent. None when the plan is
-- not open or has moved on since (the next reconcile finds them again).
sendFacts :: Env -> FilePath -> ReconcileAnswer -> LspM () Int
sendFacts env path a = do
  now <- liftIO (M.lookup path . ssDocs <$> readMVar (envState env))
  case (raVersion a, now) of
    (Just v, Just (v', text)) | v == v' -> do
      -- the reconcile's moved anchors are journaled; pending lines ride along
      at <- liftIO (pNow (envPorts env))
      let ls = parsePlan text
          moved = [ movedEvent (lnText l) r at (anchorText' a')
                  | Replace r t <- raFacts a, l <- take 1 (drop r ls), isStepLine l
                  , Just old <- [lnAnchor l], a' : _ <- [[x | Just x <- map lnAnchor (take 1 (parsePlan t))]]
                  , (anPath old, anUnit old) /= (anPath a', anUnit a') ]
      liftIO (journal env path moved)
      (jf, land) <- liftIO (journalOn env path text)
      let facts = withJournal (raFacts a) jf
      unless (null facts) . void $
        sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams (Just "Olifant: reconcile") (factsEdit path (Just v) text facts)) (\case
          Right r | r ^. L.applied -> liftIO land
          _ -> pure ())
      pure (length (raFacts a))
    _ -> pure 0
  where
    isStepLine l = case lnKind l of Step _ -> True; _ -> False
    anchorText' x = T.pack (anPath x) <> ":" <> anUnit x

-- | Record EVENTS in the journal of the plan at PATH (each once).
journal :: Env -> FilePath -> [Event] -> IO ()
journal env path evs = unless (null evs) $
  planContext env path >>= \case
    Nothing -> pure ()
    Just (p, rel, _) -> do
      zone <- getCurrentTimeZone
      void (Journal.record (envPorts env) zone (prRoot p) rel evs)

-- | The plan's pending journal lines placed on TEXT (the plan now): their
-- facts, and what marks them landed (a line whose step is gone is lost).
journalOn :: Env -> FilePath -> T.Text -> IO ([Fact], IO ())
journalOn env path text =
  planContext env path >>= \case
    Nothing -> pure ([], pure ())
    Just (p, rel, _) -> do
      let ports = envPorts env
      es <- Journal.pendingEntries ports (prRoot p) rel
      let (facts, keys, lost) = Journal.pendingFacts (parsePlan text) es
      Journal.markLost ports (prRoot p) rel lost
      pure (facts, Journal.markLanded ports (prRoot p) rel keys)

-- | The plan's pending journal lines as their own edit of the plan open
-- now (landed when the editor applied it); nothing when it is not open.
flushJournal :: Env -> FilePath -> LspM () ()
flushJournal env path = do
  doc <- liftIO (M.lookup path . ssDocs <$> readMVar (envState env))
  forM_ doc $ \(v, text) -> do
    (facts, land) <- liftIO (journalOn env path text)
    unless (null facts) . void $
      sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams (Just "Olifant: journal") (factsEdit path (Just v) text facts)) (\case
        Right r | r ^. L.applied -> liftIO land
        _ -> pure ())

-- | A test run's journal events for the step at ROW of the plan's TEXT,
-- from the FACTS it left (its test children's marks, its tick).
journalTests :: Env -> FilePath -> T.Text -> Int -> [Fact] -> IO ()
journalTests env path text row facts =
  planContext env path >>= \case
    Nothing -> pure ()
    Just (p, rel, _) -> do
      let ls = parsePlan text
      fp <- case [a | l <- ls, lnRow l == row, Just a <- [lnAnchor l]] of
        a : _ -> withSnapshot env (\s -> projectFor path s >>= \case
          (s', Just (_, p')) -> (s',) . rsFingerprint <$> resolveAnchor s' p' a
          (s', Nothing) -> pure (s', Nothing))
        [] -> pure Nothing
      now <- pNow (envPorts env)
      zone <- getCurrentTimeZone
      Journal.recordTests (envPorts env) zone (prRoot p) rel ls row facts fp now

-- | A do's `resume` (an archived errand's id) and `text`: the reopen and
-- the row of its step now; Nothing without `resume`.
reopenOf :: Env -> FilePath -> Value -> IO (Either T.Text (Maybe (Reopen, Int)))
reopenOf env path params = case textParam "resume" params of
  Nothing -> pure (Right Nothing)
  Just rid -> case T.strip <$> textParam "text" params of
    Just ask | not (T.null ask) ->
      planContext env path >>= \case
        Nothing -> pure (Left "this plan is in no Olifant project")
        Just (p, rel, text) ->
          loadArchived (envPorts env) (prRoot p) rel rid <&> \case
            Nothing -> Left ("no past do " <> rid)
            Just r -> case (rvSession r, locateStep (parsePlan text) r) of
              _ | not (reopenable (pcRunner (catalogPlans (prCatalog p)))) -> Left "this project's runner cannot resume a do's thread"
              (Nothing, _) -> Left "that do has no session to resume"
              (_, Nothing) -> Left "the step is gone"
              (Just sid, Just l) -> Right (Just (Reopen rid sid ask, lnRow l))
    _ -> pure (Left "say what to change")

-- | A past do as the window draws it: by `id`, or the errand behind the
-- journal line at `row`; Null when there is none.
errandView :: Env -> FilePath -> Value -> IO (Either T.Text Value)
errandView env path params =
  planContext env path >>= \case
    Nothing -> pure (Left "this plan is in no Olifant project")
    Just (p, rel, text) -> do
      let ports = envPorts env
          root = prRoot p
      rid <- case (reviewIdOf params, lineParam "row" params) of
        (Just i, _) -> pure (Just i)
        (_, Just row) -> Journal.entries ports root rel <&> \es ->
          evErrand . enEvent =<< Journal.entryAt es (parsePlan text) row
        _ -> pure Nothing
      found <- maybe (pure Nothing) (loadArchived ports root rel) rid
      case found of
        Nothing -> pure (Right Aeson.Null)
        Just r -> do
          home <- getHomeDirectory
          let runner = pcRunner (catalogPlans (prCatalog p))
              transcript = [home </> ".claude/projects" </> slugOf root </> T.unpack sid <> ".jsonl" | takeFileName runner == "claude", Just sid <- [rvSession r]]
          pure (Right (canReopen p (errandPayload (listToMaybe transcript) r)))

-- | The reconcile review as the window draws it, from the plan's TEXT at
-- VERSION (each entry's edit derived from it).
readingsView :: FilePath -> Maybe Int -> T.Text -> ReadingsReview -> Value
readingsView path version text rr = readingsPayload rr text (toJSON . factsEdit path version text)

-- | Answer the reading ENTRY of the plan at PATH's reconcile review with
-- CHOICE (STEP: the open step's row, for `place`): its plan edit (from the
-- plan now, at its version), the outcome recorded against the reconcile's
-- record and block, the reading answered; the review after it, or done.
chooseReading :: Env -> FilePath -> Maybe T.Text -> T.Text -> Maybe Int -> IO (Either T.Text Value)
chooseReading env path entry name step =
  planContext env path >>= \case
    Nothing -> pure (Left "this plan is in no Olifant project")
    Just (p, rel, text0) -> do
     -- the plan's text and its version read together: the edit is of that text
     (version, text) <- maybe (Nothing, text0) (\(v, t) -> (Just v, t)) . M.lookup path . ssDocs <$> readMVar (envState env)
     loadReadings ports (prRoot p) rel >>= \case
        Nothing -> pure (Left ("no open review " <> readingsId))
        Just rr -> case (find ((== entry) . Just . readingKey) (rrReadings rr), choiceOf name step) of
          (Nothing, _) -> pure (Left "that reading is no longer open; open the review again")
          (_, Nothing) -> pure (Left ("no choice " <> name))
          (Just r, Just choice) -> case readingFacts (parsePlan text) r choice of
            Left why -> pure (Left why)
            Right facts0 -> do
              let root = prRoot p
                  ls = parsePlan text
              now <- pNow ports
              appendRecord ports root (AnOutcome (OutcomeRecord {orId = rrErrand rr, orBlock = rdBlock r, orAt = now, orOutcome = outcomeOf choice}))
              -- the answer is journaled under the step it is about
              let stepRow = case choice of Place n -> Just n; _ -> readingRow ls r
              journal env path [e | Just row <- [stepRow], l <- take 1 (drop row ls), Just e <- [readingEvent (readingKey r) r choice l now]]
              (jf, land) <- journalOn env path text
              land
              let facts = withJournal facts0 jf
                  rest = answerReading rr (readingKey r)
                  after = applyFactsText facts text
              maybe (dropReadings ports root rel) (saveReadings ports root) rest
              pure (Right (object
                [ "done" .= isNothing rest
                , "edit" .= (if null facts then Nothing else Just (factsEdit path version text facts))
                , "review" .= fmap (readingsView path Nothing after) rest
                , "said" .= (case choice of Apply -> "applied" :: T.Text; Leave -> "left"; Place _ -> "placed"; New -> "added a step"; Ignore -> "ignored")
                ]))
  where
    ports = envPorts env

-- | "Re-fingerprint this claim" on a plan line that takes a fingerprint (a
-- claim, evidence, a ticked step) whose anchor resolves to a fingerprint
-- it does not carry: the line with that one ("I reviewed it").
refingerprintAction :: Env -> FilePath -> Position -> IO [CodeAction]
refingerprintAction env path (Position l _) =
  withSnapshot env (\s -> projectFor path s >>= \case
    (s', Just (_, project)) | isPlanPath project path -> do
      text <- documentText env path
      case [(ln, a) | ln <- parsePlan text, lnRow ln == fromIntegral l, lnKind ln `elem` [Claim, Evidence, Step True], Just a <- [lnAnchor ln]] of
        (ln, a) : _ -> resolveAnchor s' project a <&> \r -> (s', [(ln, fp) | Just fp <- [rsFingerprint r <* rsUnit r], lnVerdict ln /= Verified fp])
        [] -> pure (s', [])
    (s', _) -> pure (s', [])) <&> \found ->
    [ CodeAction "Re-fingerprint this claim" (Just CodeActionKind_QuickFix) Nothing Nothing Nothing (Just edit) Nothing Nothing
    | (ln, fp) <- found
    , let end = Position l (fromIntegral (utf16 (lnText ln)))
          edit = WorkspaceEdit (Just (M.singleton (filePathToUri path) [TextEdit (Range (Position l 0) end) (withVerdict (lnText ln) (Verified fp))])) Nothing Nothing ]

-- | The lens on each excerpt's opening line: "live · from <file> ·
-- read-only" (the reconcile rewrites it) or "frozen at tick"; its command
-- (`olifant.plans.excerpt`: the plan, the opening and closing rows, live,
-- the anchor) folds it.
excerptLenses :: Uri -> [Line] -> [CodeLens]
excerptLenses uri ls =
  [ CodeLens (Range (Position (fromIntegral (exOpen e)) 0) (Position (fromIntegral (exOpen e)) 0)) (Just (Command title "olifant.plans.excerpt" (Just [String (getUri uri), Number (fromIntegral (exOpen e)), Number (fromIntegral (exClose e)), Bool (not (exDone e)), String (anchorText (exAnchor e))]))) Nothing
  | e <- excerptsOf ls
  , let title = if exDone e then "frozen at tick" else "live \183 from " <> T.pack (anPath (exAnchor e)) <> " \183 read-only" ]

-- | A review's lens on its step.
lensTitle :: Review -> T.Text
lensTitle r = case rvStatus r of
  "answered" -> "do \9670 to review"
  s -> "do \10007 " <> s <> " \183 to review"

-- | A plan's project, its root-relative path, and its text now.
planContext :: Env -> FilePath -> IO (Maybe (Project, FilePath, T.Text))
planContext env path =
  withSnapshot env (projectFor path) >>= \case
    Nothing -> pure Nothing
    Just (_, p) -> documentText env path <&> \text -> Just (p, makeRelative (prRoot p) (normalisePath path), text)

-- | A review as the window draws it: its step found in the plan's TEXT, the
-- change since its reference now, as diffs and as one workspace edit (each
-- file from its text before the `do` to its text now).
reviewView :: Project -> T.Text -> Review -> IO Value
reviewView p text r = do
  let root = prRoot p
      ls = parsePlan text
      isStep l = case lnKind l of Step _ -> True; _ -> False
      step = locateStep ls r <&> \l -> (lnRow l, length [() | x <- ls, lnRow x <= lnRow l, isStep x])
  changes <- maybe (pure []) (changeSince root [rvPlan r]) (rvRef r)
  let edit c =
        let path = normalisePath (root </> fcPath c)
         in case (fcChange c, fcBefore c, fcAfter c) of
              (Added, _, Just new) -> Just (FileEdit path Nothing True ((0, 0), (0, 0)) new)
              (Modified, Just old, Just new) -> let (range, t) = smallestChange old new in Just (FileEdit path Nothing False range t)
              _ -> Nothing
  pure (canReopen p (reviewPayload r step changes (toJSON (workspaceEditAsking False (mapMaybe edit changes)))))

-- | A review's payload without its reopen offer when the project's runner
-- cannot resume a thread.
canReopen :: Project -> Value -> Value
canReopen p = \case
  Object o | not (reopenable (pcRunner (catalogPlans (prCatalog p)))), Just (Aeson.Array offers) <- KM.lookup "offers" o ->
    Object (KM.insert "offers" (toJSON (filter (\case Object x -> KM.lookup "choice" x /= Just (String "reopen"); _ -> True) (foldr (:) [] offers))) o)
  v -> v

-- | The plan's text with ROWS' lines replaced (row, text) and LINES inserted
-- after a row, as one edit of the plan at its version (Nothing: no change).
planEdit :: Env -> FilePath -> T.Text -> [(Int, T.Text)] -> Maybe (Int, [T.Text]) -> IO (Maybe WorkspaceEdit)
planEdit env path text reps insert = planFactsEdit env path text ([Replace r t | (r, t) <- reps] ++ [Insert after more | Just (after, more) <- [insert], not (null more)])

-- | 'planEdit' of FACTS0 (rows of TEXT).
planFactsEdit :: Env -> FilePath -> T.Text -> [Fact] -> IO (Maybe WorkspaceEdit)
planFactsEdit env path text facts0 = do
  version <- versionOf env path
  -- the plan's pending journal lines land with it
  (jf, land) <- journalOn env path text
  let facts = withJournal facts0 jf
  if applyFactsText facts text == text then pure Nothing else Just (factsEdit path version text facts) <$ land

-- | Answer the review RID of the plan at PATH with CHOICE: keep (its plan
-- lines land, the review goes), revert (the files restored from its
-- reference, the facts its answer landed put back, the review goes), test
-- (the step's tests run now; the review stays, with them) or allow (its
-- denials' patterns added to `[plans] do_tools`; the review stays).
chooseReview :: Env -> FilePath -> T.Text -> T.Text -> IO (Either T.Text Value)
chooseReview env path rid choice =
  planContext env path >>= \case
    Nothing -> pure (Left "this plan is in no Olifant project")
    Just (p, rel, text) ->
      loadReview ports (prRoot p) rel rid >>= \case
        Nothing -> pure (Left ("no open review " <> rid))
        Just r -> answer p rel text r
  where
    ports = envPorts env
    answer p rel text r = do
      let root = prRoot p
          ls = parsePlan text
          outcome o = do
            now <- pNow ports
            appendRecord ports root (AnOutcome (OutcomeRecord {orId = rvErrand r, orBlock = 0, orAt = now, orOutcome = o}))
            dropReview ports root r
          -- the answer journaled under its step and kept in the archive
          answered how = answeredAs how how
          -- WORDS: the journal's choice (keepEvent), HOW the archive's
          answeredAs words' how = do
            now <- pNow ports
            let step = locateStep ls r
            journal env path [keepEvent (rvId r) (rvErrand r) (maybe (rvStep r) lnText step) (maybe (rvRow r) lnRow step) now words']
            loadArchived ports root rel (rvId r) >>= mapM_ (\a -> archiveErrand ports root a {rvChoice = Just how})
          again r' = do
            saveReview ports root r'
            text' <- documentText env path
            reviewView p text' r'
      case choice of
        _ | choice `elem` ["keep", "reopen"] -> do
          -- keep ticks a step with no tests (as "Mark done" does); a reopen never
          let kept = if choice == "keep" then keepTicked ls r else (\(st, rep, more) -> (st, rep, more, False)) <$> keepLines ls r
              ticks = maybe False (\(_, _, _, t) -> t) kept
          answeredAs (if choice == "keep" then (if ticks then "kept-ticked" else "kept") else "reopened") (if choice == "keep" then "kept" else "reopened")
          edit <- case kept of
            Nothing -> planEdit env path text [] Nothing
            Just (step, Just t, more, True) -> do
              done <- withSnapshot env (\s -> (s,) <$> doneFacts s p path text (lnRow step) t)
              planFactsEdit env path text (done ++ [Insert (lnRow step) more | not (null more)])
            Just (step, rep, more, _) -> planEdit env path text [(lnRow step, t) | Just t <- [rep]] (Just (lnRow step, more))
          outcome "accepted"
          pure (Right (object (["done" .= True, "edit" .= edit, "said" .= (if choice == "keep" then "kept" else "kept; say what to change" :: T.Text)] ++ ["reopen" .= object ["id" .= rvId r] | choice == "reopen"])))
        "revert" -> case rvRef r of
          Nothing -> pure (Left "this do has no tree reference to revert to")
          Just tr -> do
            changes <- changeSince root [rel] tr
            revertChange root tr changes >>= \case
              Left why -> pure (Left why)
              Right () -> do
                let now' = T.splitOn "\n" text
                    undone = [(n, b) | (n, b, a) <- rvFacts r, listToMaybe (drop n now') == Just a]
                answered "reverted"
                edit <- planEdit env path text undone Nothing
                outcome "rejected"
                let n = length changes
                pure (Right (object ["done" .= True, "edit" .= edit, "files" .= [normalisePath (root </> fcPath c) | c <- changes], "said" .= ("reverted " <> T.pack (show n) <> (if n == 1 then " file" else " files"))]))
        "test" -> case locateStep ls r of
          Nothing -> pure (Left "the step is no longer in the plan")
          Just step ->
            withSnapshot env (stepTest path (lnRow step)) >>= \case
              Left why -> pure (Left why)
              Right t -> do
                view <- again r {rvTests = testsOfFacts [x | Replace _ x <- ttFacts t]}
                journalTests env path text (lnRow step) (ttFacts t)
                (jf, land) <- journalOn env path text
                land
                pure (Right (object ["done" .= False, "review" .= view, "facts" .= map factJson (withJournal (ttFacts t) jf), "said" .= (T.pack (show (ttPassed t)) <> " passed \183 " <> T.pack (show (ttFailed t)) <> " failed")]))
        _ -> do
          let pats = nub [allowPattern d | d <- rvDenials r, allowPattern d `notElem` rvAllowed r]
              config = root </> ".olifant/config.toml"
          if null pats
            then pure (Left "nothing denied is left to allow")
            else do
              old <- fromMaybe "" <$> readText (pFiles ports) config
              writeText (pFiles ports) config (allowInConfig pats old)
              modifyMVar_ (envState env) (filesChanged [config])
              view <- again r {rvAllowed = rvAllowed r ++ pats}
              pure (Right (object ["done" .= False, "review" .= view, "said" .= ("allowed " <> T.intercalate ", " pats <> " in .olifant/config.toml")]))

-- | A request id as `$/cancelRequest` names it.
requestKey :: LspId m -> T.Text
requestKey = \case
  IdInt n -> T.pack (show n)
  IdString t -> t

-- | Work under a progress report (checking, asking the model, testing).
planProgress :: T.Text -> ((ProgressEvent -> IO ()) -> IO a) -> LspM () a
planProgress title work =
  withIndefiniteProgress title Nothing Cancellable $ \report -> do
    lspEnv <- getLspEnv
    liftIO (work (runLspT lspEnv . report . describeProgress))

-- | An action's data: the file, the cursor's line, the generator, and the
-- inputs set (its own and the choices made).
actionData :: Value -> Maybe (FilePath, Int, T.Text, [(T.Text, T.Text)])
actionData = \case
  Object o | KM.lookup "kind" o == Just (String "action") -> do
    String path <- KM.lookup "path" o
    Number line <- KM.lookup "line" o
    String generator <- KM.lookup "generator" o
    (T.unpack path,,generator,setsIn o) <$> toBoundedInteger line
  _ -> Nothing

setsIn :: KM.KeyMap Value -> [(T.Text, T.Text)]
setsIn o = case KM.lookup "sets" o of
  Just (Object m) -> [(Key.toText k, v) | (k, String v) <- KM.toList m]
  _ -> []

-- | The snippet a plan placed at the cursor, in an action's data.
withSnippet :: Maybe Snippet -> Value -> Value
withSnippet snippet = \case
  Object o | Just sn <- snippet -> Object (KM.insert "placed" (object ["line" .= snLine sn, "body" .= snBody sn, "ghost" .= snGhost sn]) o)
  other -> other

-- | A fix's data: the file, rule, line, which fix, and the choices made.
fixData :: Value -> Maybe (FilePath, T.Text, Int, Int, [(T.Text, T.Text)])
fixData = \case
  Object o | KM.lookup "kind" o == Just (String "fix") -> do
    String path <- KM.lookup "path" o
    String rule <- KM.lookup "rule" o
    Number line <- KM.lookup "line" o
    Number n <- KM.lookup "index" o
    (,,,,) (T.unpack path) rule <$> toBoundedInteger line <*> toBoundedInteger n <*> pure (setsIn o)
  _ -> Nothing

-- | Edits as one workspace edit: new files created, then each change at
-- its document's version (the editor refuses it if the text moved on). An
-- edit of several files asks to be confirmed (spec §2).
workspaceEdit :: [FileEdit] -> WorkspaceEdit
workspaceEdit = workspaceEditAsking True

-- | Edits the person already accepted, step by step (a conjure's landing):
-- one workspace edit that asks nothing again. Annotated, Neovim asks once per
-- file and notifies once per file.
acceptedEdit :: [FileEdit] -> WorkspaceEdit
acceptedEdit = workspaceEditAsking False

workspaceEditAsking :: Bool -> [FileEdit] -> WorkspaceEdit
workspaceEditAsking asking edits = WorkspaceEdit Nothing (Just (concatMap change edits)) annotations
  where
    several = asking && length edits > 1
    plan = ChangeAnnotationIdentifier "olifant-plan"
    annotations
      | several = Just (M.singleton plan (ChangeAnnotation "Olifant plan" (Just True) (Just (T.pack (show (length edits)) <> " files"))))
      | otherwise = Nothing
    change e =
      let uri = filePathToUri (fePath e)
          ((l1, c1), (l2, c2)) = feRange e
          range = Range (Position (fromIntegral l1) (fromIntegral c1)) (Position (fromIntegral l2) (fromIntegral c2))
          text = if several then InR (AnnotatedTextEdit range (feText e) plan) else InL (TextEdit range (feText e))
          edit = TextDocumentEdit (OptionalVersionedTextDocumentIdentifier uri (maybe (InR Null) (InL . fromIntegral) (feVersion e))) [text]
       in [InR (InL (CreateFile (if several then Just plan else Nothing) AString uri Nothing)) | feCreate e] ++ [InL edit]

lineAt :: [T.Text] -> Int -> T.Text
lineAt ls n = fromMaybe "" (listToMaybe (drop n ls))

utf16 :: T.Text -> Int
utf16 = T.foldl' (\n c -> n + if fromEnum c > 0xFFFF then 2 else 1) 0

-- | A document's text: the editor's when it is open, else disk's.
documentText :: Env -> FilePath -> IO T.Text
documentText env path = do
  s <- readMVar (envState env)
  case M.lookup path (ssDocs s) of
    Just (_, text) -> pure text
    Nothing -> fromMaybe "" <$> readText (pFiles (ssPorts s)) path

update :: MonadIO m => Env -> (ServerState -> ServerState) -> m ()
update env f = liftIO (modifyMVar_ (envState env) (pure . f))

atPath :: Applicative m => Uri -> (FilePath -> m ()) -> m ()
atPath uri act = maybe (pure ()) act (uriToFilePath uri)


-- | The optional `survivor` of an `olifant/stub` request, as a mutant (its
-- text only the mutated line, at its place); Left says what is malformed.
survivorOf :: T.Text -> Value -> Either T.Text (Maybe Mutant)
survivorOf method = \case
  Object o | Just v <- KM.lookup "survivor" o -> case v of
    Object sv
      | Just (Number l) <- KM.lookup "line" sv
      , Just n <- toBoundedInteger l
      , n >= 1
      , Just (String op) <- KM.lookup "operator" sv
      , Just (String from) <- KM.lookup "from" sv
      , Just (String to) <- KM.lookup "to" sv
      , Just (String mutated) <- KM.lookup "mutated" sv ->
          Right (Just (Mutant n op from to (T.intercalate "\n" (replicate (n - 1) "" ++ [mutated])) False))
    _ -> Left (method <> ": the survivor needs line, operator, from, to and mutated")
  _ -> Right Nothing
