{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Olifant.LspSpec (spec, needsUv, withPython, plainPorts) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, void, when)
import Data.Foldable (toList)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BSL8
import System.IO (Handle, hFlush, hGetLine)
import Control.Exception (bracket, finally)
import Data.Aeson (Value (Array, Bool, Number, Object, String), object, toJSON, (.=))
import Data.Int (Int32)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Functor ((<&>))
import Control.Lens ((%~), (&), (^.))
import Language.LSP.Protocol.Lens qualified as L
import System.Exit (ExitCode (..))
import System.Process (callProcess, proc, readCreateProcessWithExitCode)
import Data.Text.IO qualified as TIO
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf, sort, sortOn)
import Data.Ord (Down (..))
import Data.Time (UTCTime (..), diffUTCTime, fromGregorian, getCurrentTime)
import Olifant.Server.Fixes qualified as Fx
import Olifant.Server.Errand (Status (..), Tokens (..))
import Olifant.Server.Review (FileChange (..), Review (..), loadArchive, loadArchived)
import Olifant.Server.Telemetry (ErrandRecord (..), OutcomeRecord (..), ReconcileRecord (..), Record (..), readRecords)
import Olifant.Server.State qualified as St
import Olifant.Decide qualified as De
import Olifant.Rules qualified as Ru
import Language.LSP.Protocol.Message (FromServerMessage, FromServerMessage' (..), LspId (..), SMethod (..), TCustomMessage (..), TNotificationMessage (..), TRequestMessage (..), TResponseMessage (..))
import Data.Text.Encoding qualified as TE
import Language.LSP.Protocol.Types
import Language.LSP.Test
import Olifant.App (Ports (..))
import Olifant.Language (CheckReport (..), Checker (..), Runner (..), Syntax (..), querying, noChecker, noRunner, noSyntax, processCheckerWith, processRunner, processVerifierCancellable)
import Olifant.Generation (HoleStatus (..), Plan (..), PlannedHole (..))
import Olifant.Server (PickStep (..), Preview (..), ServerConfig (..), ServerState (..), actionPlanWith, describeProgress, fixPlan, newServerState, previewFor, stepsPlanWith)
import Olifant.Preview (Field (..), Source (..))
import Olifant.Process (gitBaseFiles, gitBaseText, gitStateStamp)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Olifant.Language.Check (CheckFailure (..), noVerifier)
import Olifant.Process (cancelled, newCancel)
import Olifant.Lsp
import Olifant.Doctor (DoctorPorts (..))
import Olifant.Workspace (Workspace (listFiles, readText), diskWorkspace)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Directory (findExecutable, canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getTemporaryDirectory, listDirectory, removeDirectory, removeFile, renameFile)
import System.FilePath (makeRelative, normalise, takeDirectory, (</>), splitDirectories)
import Olifant.Language.AstGrep (astGrepSyntax)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), StdStream (..), createPipe, createProcess, terminateProcess)
import Olifant.Server.Unit qualified as Un
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Lsp (olifant lsp over stdio)" $ do
  it "starts, follows a document's edits, and exits cleanly when asked" $
    withProject [("src/x.py", "x = 1\n")] $ \dir -> do
      ((state, version), code) <- inProcess dir $ do
        doc <- openDoc "src/x.py" "python"
        changeDoc doc [TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument "x = 2\n"))]
        VersionedTextDocumentIdentifier _ v <- getVersionedDoc doc
        st <- debugState
        pure (st, v)
      code `shouldBe` Just 0
      let doc = object ["path" .= (dir </> "src/x.py"), "version" .= version, "project" .= dir]
      lookupKey "documents" state `shouldBe` Just (Array (pure doc))
  fixesSpec
  actionsSpec
  hintsSpec
  completionSpec
  followupsSpec
  recognitionSpec
  anticipationSpec
  generatePickerSpec
  newCodeSpec
  stranglerSpec
  holesSpec
  pickerSpec
  conventionsSpec
  scaleSpec
  concurrencySpec
  testsRunSpec
  stubSpec
  conjureSpec
  mutateSpec
  plansSpec
  plansStage1Spec
  doReviewSpec
  plansLoopSpec
  it "is the `olifant lsp` command: the built binary initialises and opens a document" $ do
    (built, out, _) <- readCreateProcessWithExitCode (proc "cabal" ["list-bin", "-v0", "olifant"]) ""
    if built /= ExitSuccess then pendingWith "cabal list-bin olifant failed" else
      withProject [("src/x.py", "x = 1\n")] $ \dir -> do
        name <- runSessionWithConfig defaultConfig (T.unpack (T.strip (T.pack out)) <> " lsp") fullLatestClientCaps dir $ do
          _ <- openDoc "src/x.py" "python"
          initializeResponse <&> \case
            TResponseMessage _ _ (Right result) -> fmap (\(ServerInfo n _) -> n) (result ^. L.serverInfo)
            _ -> Nothing
        name `shouldBe` Just "olifant"
  it "advertises only what it answers: no semantic tokens (Neovim asked, and logged an error each time)" $
    withPython [] $ \ports dir -> do
      (caps, _) <- inProcessWith ports dir $
        initializeResponse <&> \case
          TResponseMessage _ _ (Right result) -> Just (result ^. L.capabilities)
          _ -> Nothing
      fmap (^. L.semanticTokensProvider) caps `shouldBe` Just Nothing
      fmap (isJust . (^. L.definitionProvider)) caps `shouldBe` Just True
      fmap (\c -> (^. L.triggerCharacters) =<< (c ^. L.completionProvider)) caps `shouldBe` Just (Just ["@", ":"])

-- | Rule findings and their fixes (Task 9): pull diagnostics, quickfixes
-- carrying a fix's open choices, resolved to the fix's plan.
fixesSpec :: Spec
fixesSpec = describe "rule findings and their fixes" $ do
  let closeTable = "src/bistro/booking/commands/close_table.py"
      closeTableText = T.unlines ["import sqlite3", "", "", "class CloseTableInput:", "    pass", "", "", "class CloseTable:", "    def __init__(self) -> None:", "        \"\"\"Its ports.\"\"\"", "", "    def handle(self, input: CloseTableInput) -> None:", "        raise NotImplementedError(\"later\")"]
      quickfixes doc = (\as -> [a | InR a <- as, a ^. L.kind == Just CodeActionKind_QuickFix]) <$> getCodeActions doc (Range (Position 0 0) (Position 0 14))
  it "(a) pulls a file's findings as diagnostics, each fix a quickfix (the first preferred)" $
    withPython [(closeTable, closeTableText)] $ \ports dir -> do
      ((diags, fixes), _) <- inProcessWith ports dir $ do
        doc <- openDoc closeTable "python"
        ds <- pullDiagnostics doc
        fs <- quickfixes doc
        pure (ds, fs)
      [(d ^. L.code, d ^. L.source) | d <- diags] `shouldSatisfy` elem (Just (InR "commands-use-ports"), Just "olifant")
      map (^. L.title) fixes `shouldSatisfy` (\ts -> "Use an existing port" `elem` ts && any ("Add a repository" `T.isPrefixOf`) ts)
      map (^. L.isPreferred) (take 1 fixes) `shouldBe` [Just True]
  it "(b) carries a fix's open choices in its data, for the editor to ask" $
    withPython [(closeTable, closeTableText)] $ \ports dir -> do
      (fixes, _) <- inProcessWith ports dir (openDoc closeTable "python" >>= quickfixes)
      case [a ^. L.data_ | a <- fixes, a ^. L.title == "Use an existing port"] of
        Just d : _ -> do
          lookupKey "missing" d `shouldBe` Just (toJSON ["port" :: Text])
          (lookupKey "choices" d >>= lookupKey "port") `shouldSatisfy` maybe False (\case Array xs -> String "booking_repository" `elem` xs; _ -> False)
        _ -> expectationFailure "no data on the fix"
  it "(c) resolves a fix, its choices set, to the fix's edit" $
    withPython [(closeTable, closeTableText)] $ \ports dir -> do
      (texts, _) <- inProcessWith ports dir $ do
        doc <- openDoc closeTable "python"
        fixes <- quickfixes doc
        case [a | a <- fixes, a ^. L.title == "Use an existing port"] of
          a : _ -> editTexts <$> resolveCodeAction (withSet "port" "booking_repository" a)
          [] -> fail "no fix"
      T.concat texts `shouldSatisfy` T.isInfixOf "booking_repository: BookingRepository"
      T.concat texts `shouldNotSatisfy` T.isInfixOf "import sqlite3"
  it "(d) resolves against the document as it is now, never an older version (Review Focus 1)" $
    withPython [(closeTable, closeTableText)] $ \ports dir -> do
      ((versions, current), _) <- inProcessWith ports dir $ do
        doc <- openDoc closeTable "python"
        fixes <- quickfixes doc
        changeDoc doc [TextDocumentContentChangeEvent (InL (TextDocumentContentChangePartial (Range (Position 13 0) (Position 13 0)) Nothing "# later\n"))]
        VersionedTextDocumentIdentifier _ v <- getVersionedDoc doc
        case [a | a <- fixes, a ^. L.title == "Use an existing port"] of
          a : _ -> (,v) . editVersions <$> resolveCodeAction (withSet "port" "booking_repository" a)
          [] -> fail "no fix"
      versions `shouldBe` [InL current]
  it "(e) gives a file in no project no diagnostics and no actions (Review Focus 2)" $
    withPython [] $ \ports dir -> do
      createDirectoryIfMissing True (dir </> "outside")
      TIO.writeFile (dir </> "outside/notes.py") "import sqlite3\n"
      removeFile (dir </> ".olifant/config.toml") >> removeDirectory (dir </> ".olifant")
      ((ds, as), _) <- inProcessWith ports dir $ do
        doc <- openDoc "outside/notes.py" "python"
        (,) <$> pullDiagnostics doc <*> getCodeActions doc (Range (Position 0 0) (Position 0 5))
      (ds, length as) `shouldBe` ([], 0)
  it "(g) says why a project's configuration does not load, on its files and to health (review I6)" $
    withPython [] $ \ports dir -> do
      TIO.writeFile (dir </> ".olifant/config.toml") "[packs\n"
      ((ds, state), _) <- inProcessWith ports dir $ do
        doc <- openDoc "src/bistro/booking/commands/create_booking.py" "python"
        (,) <$> pullDiagnostics doc <*> debugState
      [d ^. L.message | d <- ds] `shouldSatisfy` any (T.isInfixOf "configuration does not load")
      [d ^. L.severity | d <- ds, "configuration does not load" `T.isInfixOf` (d ^. L.message)] `shouldBe` [Just DiagnosticSeverity_Error]
      (lookupKey "broken" state >>= lookupKey (T.pack dir)) `shouldSatisfy` (/= Nothing)
  it "(h) never keeps a catalog loaded in the background from a configuration that changed meanwhile (review minor)" $ do
    armed <- newIORef False
    entered <- newEmptyMVar
    gate <- newEmptyMVar
    withPython [] $ \ports dir -> do
      let disk = pFiles ports
          -- held after the load, as it lists the project's files
          held = disk {listFiles = \p -> do
                         stop <- if normalise p `elem` [normalise dir, normalise (dir </> ".")] then atomicModifyIORef' armed (False,) else pure False
                         when stop (putMVar entered () >> takeMVar gate)
                         listFiles disk p}
          command = "src/bistro/booking/commands/create_booking.py"
      _ <- forkIO (threadDelay 10000000 >> void (tryPutMVar gate ()))
      (ds, _) <- inProcessWith ports {pFiles = held} dir $ do
        liftIO (writeIORef armed True)
        -- the project's first load, in the background (no document open yet)
        _ <- sendRequest (SMethod_CustomMethod (Proxy :: Proxy "olifant/model")) (object ["textDocument" .= object ["uri" .= filePathToUri (dir </> command)]])
        liftIO (timeout 10000000 (takeMVar entered)) >>= maybe (fail "the load never listed the project") pure
        -- meanwhile the configuration breaks
        liftIO (TIO.writeFile (dir </> ".olifant/config.toml") "[packs\n")
        sendNotification SMethod_WorkspaceDidChangeWatchedFiles (DidChangeWatchedFilesParams [FileEvent (filePathToUri (dir </> ".olifant/config.toml")) FileChangeType_Changed])
        liftIO (threadDelay 200000 >> putMVar gate ())
        liftIO (threadDelay 500000)
        doc <- openDoc command "python"
        pullDiagnostics doc
      [d ^. L.message | d <- ds] `shouldSatisfy` any (T.isInfixOf "configuration does not load")
  it "(f) answers an error, and keeps serving, when a request throws (review I4)" $
    withPython [] $ \ports dir -> do
      -- a tool that throws instead of answering (a file deleted mid-read, an unreadable folder)
      let throwing = querying (\_ _ _ -> ioError (userError "the disk went away"))
      ((answer, after), code) <- inProcessWith ports {pSyntax = throwing} dir $ do
        doc <- openDoc "src/bistro/booking/commands/create_booking.py" "python"
        TResponseMessage _ _ answer <- request SMethod_TextDocumentCodeAction (CodeActionParams Nothing Nothing doc (Range (Position 0 0) (Position 0 0)) (CodeActionContext [] Nothing Nothing))
        after <- debugState
        pure (answer, after)
      either (\e -> T.isInfixOf "the disk went away" (e ^. L.message)) (const True) answer `shouldBe` True
      lookupKey "projects" after `shouldSatisfy` (/= Nothing)
      code `shouldBe` Just 0

-- | The ⚡ actions at a cursor (Task 10): refactor code actions resolved to
-- their plan at the cursor.
actionsSpec :: Spec
actionsSpec = describe "⚡ actions at the cursor" $ do
  let command = "src/bistro/booking/commands/cancel_booking.py"
      atInit = Range (Position 25 8) (Position 25 8)
      refactors doc = (\as -> [a | InR a <- as, a ^. L.kind == Just CodeActionKind_RefactorRewrite]) <$> getCodeActions doc atInit
      named t = filter ((== t) . (^. L.title))
      given = readTree "packs/python-htmx/generators/add-dependency/tests/inline/given"
  it "(a) offers the actions at the cursor, with the choices they need" $ do
    files <- given
    withPython (M.toList files) $ \ports dir -> do
      (actions, _) <- inProcessWith ports dir (openDoc command "python" >>= refactors)
      case named "Add a port" actions of
        a : _ -> do
          (a ^. L.data_ >>= lookupKey "missing") `shouldBe` Just (toJSON ["port" :: Text])
          (a ^. L.data_ >>= lookupKey "choices" >>= lookupKey "port") `shouldSatisfy` maybe False (\case Array xs -> String "booking_repository" `elem` xs; _ -> False)
        [] -> expectationFailure ("no \"Add a port\" among " <> show (map (^. L.title) actions))
  it "(b) resolves to the plan's edits in every file it changes, asking to confirm them" $ do
    files <- given
    withPython (M.toList files) $ \ports dir -> do
      (resolved, _) <- inProcessWith ports dir $ do
        doc <- openDoc command "python"
        actions <- refactors doc
        case named "Add a port" actions of
          a : _ -> resolveCodeAction (withSet "port" "booking_repository" a)
          [] -> fail "no action"
      let edited = [te ^. L.textDocument . L.uri | Just e <- [resolved ^. L.edit], Just cs <- [e ^. L.documentChanges], InL te <- cs]
      length edited `shouldSatisfy` (> 1)
      T.concat (editTexts resolved) `shouldSatisfy` T.isInfixOf "booking_repository: BookingRepository"
      (resolved ^. L.edit >>= (^. L.changeAnnotations)) `shouldSatisfy` maybe False (any (\c -> c ^. L.needsConfirmation == Just True))
  it "(b) carries the plan itself on a resolved action, as durandal previews it" $ do
    files <- given
    withPython (M.toList files) $ \ports dir -> do
      (resolved, _) <- inProcessWith ports dir $ do
        doc <- openDoc command "python"
        actions <- refactors doc
        case named "Add a port" actions of
          a : _ -> resolveCodeAction (withSet "port" "booking_repository" a)
          [] -> fail "no action"
      (resolved ^. L.data_ >>= lookupKey "plan" >>= lookupKey "files") `shouldSatisfy` maybe False (\case Array fs -> length fs > 1; _ -> False)
  it "(e) lists a file's actions, where each applies, as `actions --file` does (durandal's hints)" $ do
    files <- given
    withPython (M.toList files) $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc command "python"
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/fileActions")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)]]) >>= \case
          TResponseMessage _ _ (Right r) -> pure r
          TResponseMessage _ _ (Left err) -> fail (show err)
      let actions = case lookupKey "actions" v of
            Just (Array as) -> [a | Object a <- foldr (:) [] as]
            _ -> []
      [KM.lookup "label" a | a <- actions] `shouldSatisfy` elem (Just (String "Add a port"))
      [KM.lookup "range" a | a <- actions, KM.lookup "label" a == Just (String "Add a port")] `shouldSatisfy` all (/= Nothing)
      [KM.member "suggest" a && KM.member "snippet" a | a <- actions] `shouldSatisfy` and
  it "(c) resolves against the document as it is now (Review Focus 1)" $ do
    files <- given
    withPython (M.toList files) $ \ports dir -> do
      ((versions, current), _) <- inProcessWith ports dir $ do
        doc <- openDoc command "python"
        actions <- refactors doc
        changeDoc doc [TextDocumentContentChangeEvent (InL (TextDocumentContentChangePartial (Range (Position 0 0) (Position 0 0)) Nothing "# edited\n"))]
        VersionedTextDocumentIdentifier _ v <- getVersionedDoc doc
        case named "Add a port" actions of
          a : _ -> (,v) . editVersionsOf (filePathToUri (dir </> command)) <$> resolveCodeAction (withSet "port" "booking_repository" a)
          [] -> fail "no action"
      versions `shouldBe` [InL current]
  it "(d) sees an unsaved file in another buffer: a new port is a choice at once" $ do
    files <- given
    withPython (M.toList files) $ \ports dir -> do
      (actions, _) <- inProcessWith ports dir $ do
        _ <- createDoc "src/bistro/booking/ports/table_repository/__init__.py" "python" ""
        openDoc command "python" >>= refactors
      case named "Add a port" actions of
        a : _ -> (a ^. L.data_ >>= lookupKey "choices" >>= lookupKey "port") `shouldSatisfy` maybe False (\case Array xs -> String "table_repository" `elem` xs; _ -> False)
        [] -> expectationFailure "no action"
  it "(f) leaves out a generator with `menu = false` (the old test-case): not in the ⚡ menu, the picker, the hover's ⚡ list nor olifant/fileActions" $
    withPython [] $ \ports dir -> do
      ((actions, listed, hover, fileActions), _) <- inProcessWith ports dir $ do
        doc <- openDoc "src/bistro/booking/commands/create_booking__test.py" "python"
        as <- getCodeActions doc (Range (Position 34 0) (Position 34 0))
        v <- request (SMethod_CustomMethod (Proxy :: Proxy "olifant/generators")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "position" .= object ["line" .= (34 :: Int), "character" .= (0 :: Int)]]) >>= \case
          TResponseMessage _ _ (Right v) -> pure v
          TResponseMessage _ _ (Left e) -> fail (show e)
        hover <- getHover doc (Position 34 0)
        fileActions <- request (SMethod_CustomMethod (Proxy :: Proxy "olifant/fileActions")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)]]) >>= \case
          TResponseMessage _ _ (Right fa) -> pure fa
          TResponseMessage _ _ (Left e) -> fail (show e)
        pure ([a ^. L.title | InR a <- as], [i | Just (Array gs) <- [lookupKey "generators" v], g <- toList gs, Just (String i) <- [lookupKey "id" g]], show hover, Aeson.encode fileActions)
      actions `shouldNotContain` ["Test case"]
      hover `shouldNotSatisfy` T.isInfixOf "test-case" . T.pack
      fileActions `shouldNotSatisfy` BS.isInfixOf "test-case" . BSL.toStrict
      listed `shouldNotContain` ["python-htmx/test-case"]
      listed `shouldContain` ["python-htmx/command"]

-- | ⚡ hints and near misses (Task 11).
hintsSpec :: Spec
hintsSpec = describe "⚡ hints and near misses" $ do
  let whole = Range (Position 0 0) (Position 200 0)
  it "(a) hints a line's actions at its end, the hint opening the ⚡ menu there" $ do
    files <- readTree "packs/python-htmx/generators/add-dependency/tests/inline/given"
    withPython (M.toList files) $ \ports dir -> do
      (hints, _) <- inProcessWith ports dir $ do
        doc <- openDoc "src/bistro/booking/commands/cancel_booking.py" "python"
        request SMethod_TextDocumentInlayHint (InlayHintParams Nothing doc whole) >>= \case
          TResponseMessage _ _ (Right (InL hs)) -> pure hs
          other -> fail (show other)
      let at25 = [h | h <- hints, h ^. L.position . L.line == 25]
      case at25 of
        [h] -> case h ^. L.label of
          InR [part] -> do
            (part ^. L.value) `shouldSatisfy` T.isPrefixOf "⚡ Add a port"
            fmap (^. L.command) (part ^. L.command) `shouldBe` Just "olifant.showActions"
          other -> expectationFailure ("label: " <> show other)
        other -> expectationFailure ("hints on line 26: " <> show other)
  it "(b) says on hover which generators nearly apply, and why not" $
    withPython [] $ \ports dir -> do
      (hover, _) <- inProcessWith ports dir $ do
        doc <- openDoc "src/bistro/main.py" "python"
        getHover doc (Position 0 0)
      case hover of
        Just h | InL (MarkupContent _ text) <- h ^. L.contents -> text `shouldSatisfy` T.isInfixOf "nearby: python-htmx/match-arm — on a blank line after a match's last case"
        other -> expectationFailure ("hover: " <> show other)
  it "(c) gives a file in no project no hints and no hover (Review Focus 2)" $
    withPython [] $ \ports dir -> do
      createDirectoryIfMissing True (dir </> "outside")
      TIO.writeFile (dir </> "outside/notes.py") "x = 1\n"
      removeFile (dir </> ".olifant/config.toml") >> removeDirectory (dir </> ".olifant")
      ((hints, hover), _) <- inProcessWith ports dir $ do
        doc <- openDoc "outside/notes.py" "python"
        hs <- request SMethod_TextDocumentInlayHint (InlayHintParams Nothing doc whole) >>= \case
          TResponseMessage _ _ (Right (InL hs)) -> pure hs
          TResponseMessage _ _ (Right (InR _)) -> pure []
          other -> fail (show other)
        (hs,) <$> getHover doc (Position 0 0)
      (length hints, hover) `shouldBe` (0, Nothing)

-- | Snippets as completion items, and the one anticipated at the cursor as
-- an inline completion (Task 12).
completionSpec :: Spec
completionSpec = describe "snippets: completion and inline completion (ghosts)" $ do
  let test = "src/bistro/booking/commands/create_booking__test.py"
      afterTest = Position 34 0
      port = "src/bistro/booking/ports/booking_repository/booking_repository.py"
      -- a blank line between the Protocol's methods
      inProtocol = Position 12 0
      inline doc pos = do
        let params = object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "position" .= pos, "context" .= object ["triggerKind" .= (1 :: Int)]]
        request (SMethod_CustomMethod (Proxy :: Proxy "textDocument/inlineCompletion")) params >>= \case
          TResponseMessage _ _ (Right v) -> pure v
          TResponseMessage _ _ (Left err) -> fail (show err)
      firstInsert v = case lookupKey "items" v of
        Just (Array items) | Object item : _ <- foldr (:) [] items, Just insert <- KM.lookup "insertText" item -> Just insert
        _ -> Nothing
      firstItem v = case lookupKey "items" v of
        Just (Array items) | Object item : _ <- foldr (:) [] items -> Just (Object item)
        _ -> Nothing
  it "(a) completes a snippet that applies here, its body with tab stops" $
    withPython [] $ \ports dir -> do
      (items, _) <- inProcessWith ports dir $ do
        doc <- openDoc port "python"
        getCompletions doc inProtocol
      case [i | i <- items, i ^. L.label == "Port method"] of
        item : _ -> do
          item ^. L.insertTextFormat `shouldBe` Just InsertTextFormat_Snippet
          let body = case item ^. L.textEdit of
                Just (InL e) -> e ^. L.newText
                Just (InR e) -> e ^. L.newText
                Nothing -> maybe "" id (item ^. L.insertText)
          body `shouldSatisfy` T.isInfixOf "${1:name}"
        [] -> expectationFailure ("no Port method among " <> show (map (^. L.label) items))
  it "(a2) a typed trigger character (@ or :) in a code file completes nothing" $
    withPython [] $ \ports dir -> do
      ((typed, invoked), _) <- inProcessWith ports dir $ do
        doc <- openDoc port "python"
        let ask ctx = request SMethod_TextDocumentCompletion (CompletionParams doc inProtocol Nothing Nothing (Just ctx)) <&> \case
              TResponseMessage _ _ (Right (InL items)) -> map (^. L.label) items
              _ -> []
        (,) <$> ask (CompletionContext CompletionTriggerKind_TriggerCharacter (Just "@")) <*> ask (CompletionContext CompletionTriggerKind_Invoked Nothing)
      typed `shouldBe` []
      invoked `shouldContain` ["Port method"]
  it "(b) offers the snippet anticipated here as an inline completion: unfilled, a snippet" $
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir (openDoc port "python" >>= (`inline` inProtocol))
      case firstInsert v of
        Just (Object insert) -> do
          KM.lookup "kind" insert `shouldBe` Just (String "snippet")
          KM.lookup "value" insert `shouldSatisfy` \case Just (String body) -> "(self" `T.isInfixOf` body; _ -> False
          -- the ghost as the engine draws it, and what to resolve for the filled body
          (firstItem v >>= lookupKey "ghost") `shouldSatisfy` \case Just (Array ls) -> any (\case String l -> "def \10216name\10217(self" `T.isInfixOf` l; _ -> False) ls; _ -> False
          (firstItem v >>= lookupKey "data" >>= lookupKey "generator") `shouldBe` Just (String "python-htmx/port-method")
        other -> expectationFailure ("inline completion: " <> show v <> " / " <> show other)
  it "(b2) neither completes nor ghosts a generator with `menu = false` (the old test-case, after a test)" $
    withPython [] $ \ports dir -> do
      ((items, v), _) <- inProcessWith ports dir $ do
        doc <- openDoc test "python"
        (,) <$> getCompletions doc afterTest <*> inline doc afterTest
      map (^. L.label) items `shouldNotContain` ["Test case"]
      (firstItem v >>= lookupKey "data" >>= lookupKey "generator") `shouldNotBe` Just (String "python-htmx/test-case")
  it "(c) says it serves inline completion (LSP 3.18), though its library predates it" $
    withPython [] $ \ports dir -> do
      -- the raw response: a typed client drops what its types do not know
      (inRead, inWrite) <- createPipe
      (outRead, outWrite) <- createPipe
      _ <- forkIO (void (runLspWithHandles ports inRead outWrite))
      let body = Aeson.encode (object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "method" .= ("initialize" :: Text), "params" .= object ["processId" .= Aeson.Null, "rootUri" .= filePathToUri dir, "capabilities" .= object []]])
      BSL.hPut inWrite ("Content-Length: " <> BSL8.pack (show (BSL.length body)) <> "\r\n\r\n" <> body) >> hFlush inWrite
      response <- readMessage outRead
      (Aeson.decode response >>= lookupKey "result" >>= lookupKey "capabilities" >>= lookupKey "inlineCompletionProvider") `shouldBe` Just (Bool True)
  sequential . it "(e) answers an inline completion within 150 ms once warm (median of 20)" $
    withPython [] $ \ports dir -> do
      (times, _) <- inProcessWith ports dir $ do
        doc <- openDoc port "python"
        _ <- inline doc inProtocol
        forM [1 .. 20 :: Int] $ \_ -> do
          t0 <- liftIO getCurrentTime
          _ <- inline doc inProtocol
          t1 <- liftIO getCurrentTime
          pure (realToFrac (diffUTCTime t1 t0) * 1000 :: Double)
      let median = sort times !! 10
      liftIO (putStrLn ("      inline completion, warm: median " <> show (round median :: Int) <> " ms, max " <> show (round (maximum times) :: Int) <> " ms"))
      median `shouldSatisfy` (< 150)
  it "(d) offers nothing inline where nothing is anticipated, and nothing in no project" $
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir (openDoc test "python" >>= (`inline` Position 0 0))
      firstInsert v `shouldBe` Nothing

-- | Follow-ups (Task 13): after typing settles, the server pushes what
-- follows from the edit; its plan is fetched by id.
followupsSpec :: Spec
followupsSpec = describe "follow-ups: olifant/suggestion and olifant/plan" $ do
  let command = "src/bistro/booking/commands/create_booking.py"
      at l c = Range (Position l c) (Position l c)
      insert l c t = TextDocumentContentChangeEvent (InL (TextDocumentContentChangePartial (at l c) Nothing t))
      -- a new failure class, typed: the block, then the colon that ends its line
      typeFailure doc = do
        changeDoc doc [insert 33 0 "@dataclass(frozen=True)\nclass Cancelled\n    reason: str\n\n\n"]
        changeDoc doc [insert 34 15 ":"]
      suggestion =
        customNotification (Proxy :: Proxy "olifant/suggestion") >>= \case
          NotMess (TNotificationMessage _ _ v) -> pure (v :: Value)
  it "(a) suggests what follows from an edit once typing settles: a new failure, its arms" $
    withPythonChecked [] $ \ports dir -> do
      ((v, version), _) <- inProcessWith ports dir $ do
        doc <- openDoc command "python"
        typeFailure doc
        VersionedTextDocumentIdentifier _ current <- getVersionedDoc doc
        (,current) <$> suggestion
      lookupKey "label" v `shouldBe` Just (String "Add Cancelled to every match")
      lookupKey "kind" v `shouldBe` Just (String "followup")
      lookupKey "at" v `shouldBe` Just (toJSON (35 :: Int))
      lookupKey "version" v `shouldBe` Just (toJSON version)
      lookupKey "summary" v `shouldSatisfy` \case Just (String t) -> "+1 line here" `T.isPrefixOf` t && "failures.py" `T.isInfixOf` t; _ -> False
  it "(a2) stops a superseded suggestion's running check when typing goes on (review I5)" $ do
    entered <- newEmptyMVar
    stopped <- newEmptyMVar
    let endless token = Checker $ \_ _ -> do
          _ <- tryPutMVar entered ()
          let wait = cancelled token >>= \case
                True -> tryPutMVar stopped () >> pure (Left (CheckFailed "cancelled"))
                False -> threadDelay 20000 >> wait
          wait
    withPython [] $ \ports dir -> do
      (seen, _) <- inProcessWith ports {pChecker = endless} dir $ do
        doc <- openDoc command "python"
        typeFailure doc
        liftIO (timeout 30000000 (takeMVar entered)) >>= maybe (fail "the suggestion never checked") pure
        changeDoc doc [insert 0 0 "# more typing\n"]
        liftIO (timeout 10000000 (takeMVar stopped))
      seen `shouldBe` Just ()
  it "(b) gives the suggestion's plan, with its edit, by id" $
    withPythonChecked [] $ \ports dir -> do
      (planned, _) <- inProcessWith ports dir $ do
        doc <- openDoc command "python"
        typeFailure doc
        v <- suggestion
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/plan")) (object ["planId" .= lookupKey "planId" v]) >>= \case
          TResponseMessage _ _ (Right p) -> pure p
          TResponseMessage _ _ (Left err) -> fail (show err)
      let texts = case lookupKey "edit" planned >>= lookupKey "documentChanges" of
            Just (Array cs) -> [t | Object c <- foldr (:) [] cs, Just (Array es) <- [KM.lookup "edits" c], Object e <- foldr (:) [] es, Just (String t) <- [KM.lookup "newText" e]]
            _ -> []
      T.concat texts `shouldSatisfy` T.isInfixOf "Cancelled"
      (lookupKey "plan" planned >>= lookupKey "files") `shouldSatisfy` maybe False (\case Array fs -> not (null fs); _ -> False)
  it "(d) gives the plan again filled by the model, when asked (accepting a suggestion)" $ do
    recorded <- (</> "olifant-followup-fills.toml") <$> getTemporaryDirectory
    TIO.writeFile recorded "[fills]\n"
    withPythonChecked [] $ \ports dir -> do
      (statuses, _) <- inProcessWithConfig [("fills", String (T.pack recorded))] ports dir $ do
        doc <- openDoc command "python"
        typeFailure doc
        v <- suggestion
        let ask fill = request (SMethod_CustomMethod (Proxy :: Proxy "olifant/plan")) (object ["planId" .= lookupKey "planId" v, "fill" .= fill]) >>= \case
              TResponseMessage _ _ (Right p) -> pure p
              TResponseMessage _ _ (Left err) -> fail (show err)
            holesOf p = case lookupKey "plan" p >>= lookupKey "holes" of
              Just (Array hs) -> [h | Object h <- foldr (:) [] hs]
              _ -> []
        unfilled <- ask False
        -- the model's answer for each of its holes
        liftIO . TIO.writeFile recorded . T.unlines $ "[fills]" : ["\"" <> n <> "\" = \"return 409\"" | h <- holesOf unfilled, Just (String n) <- [KM.lookup "name" h]]
        filled <- ask True
        pure ([KM.lookup "status" h | h <- holesOf unfilled], [KM.lookup "status" h | h <- holesOf filled])
      fst statuses `shouldSatisfy` (\ss -> not (null ss) && all (/= Just (String "filled")) ss)
      snd statuses `shouldSatisfy` (\ss -> not (null ss) && all (== Just (String "filled")) ss)
  it "(e) answers what follows at a line when asked (the editor knows the cursor), with its plan" $
    withPythonChecked [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc command "python"
        -- the class typed in one change: the change ends below the class line
        changeDoc doc [insert 33 0 "@dataclass(frozen=True)\nclass Cancelled:\n    reason: str\n\n\n"]
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/suggest")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "line" .= (35 :: Int)]) >>= \case
          TResponseMessage _ _ (Right r) -> pure r
          TResponseMessage _ _ (Left err) -> fail (show err)
      lookupKey "label" v `shouldBe` Just (String "Add Cancelled to every match")
      lookupKey "planId" v `shouldSatisfy` (/= Nothing)
      (lookupKey "plan" v >>= lookupKey "files") `shouldSatisfy` maybe False (\case Array fs -> not (null fs); _ -> False)
  it "(g) never follows up with a generator with `menu = false`" $ do
    let note menu = ".olifant/generators/note/generator.toml" `withText` ("description = \"d\"\nlanguage = \"python\"\n" <> menu <> "[applies_at]\npath = \"src/**/noted.py\"\nnode = { kind = \"function_definition\" }\nsuggest = true\nlabel = \"Note\"\n[[edits]]\ncreate = \"note.txt\"\ntext = \"x\\n\"\n")
        withText p t = (p, t)
        asked menu = withPython [note menu, ("src/bistro/noted.py", "def f() -> None:\n    pass\n")] $ \ports dir -> fst <$> inProcessWith ports dir (do
          doc <- openDoc "src/bistro/noted.py" "python"
          request (SMethod_CustomMethod (Proxy :: Proxy "olifant/suggest")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "line" .= (1 :: Int)]) >>= \case
            TResponseMessage _ _ (Right r) -> pure r
            TResponseMessage _ _ (Left err) -> fail (show err))
    offered <- asked ""
    lookupKey "label" offered `shouldBe` Just (String "Note")
    hidden <- asked "menu = false\n"
    lookupKey "label" hidden `shouldNotBe` Just (String "Note")
  it "(f) keeps every plan offered for the document as it is (a push and a pull both resolve)" $
    withPythonChecked [] $ \ports dir -> do
      (planned, _) <- inProcessWith ports dir $ do
        doc <- openDoc command "python"
        typeFailure doc
        pushed <- suggestion
        _ <- request (SMethod_CustomMethod (Proxy :: Proxy "olifant/suggest")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "line" .= (35 :: Int)])
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/plan")) (object ["planId" .= lookupKey "planId" pushed]) >>= \case
          TResponseMessage _ _ (Right p) -> pure (Right p)
          TResponseMessage _ _ (Left err) -> pure (Left (show err))
      either expectationFailure (\p -> lookupKey "plan" p `shouldSatisfy` (/= Nothing)) planned
  it "(c) drops the suggestion of a version typed over before it settled" $
    withPythonChecked [] $ \ports dir -> do
      ((v, version), _) <- inProcessWith ports dir $ do
        doc <- openDoc command "python"
        typeFailure doc
        changeDoc doc [insert 34 16 "  # one more"]
        VersionedTextDocumentIdentifier _ current <- getVersionedDoc doc
        (,current) <$> suggestion
      lookupKey "version" v `shouldBe` Just (toJSON version)

-- | Progress, the holes list, and filling a hole on request (Task 14).
holesSpec :: Spec
holesSpec = describe "progress, holes and verify" $ do
  let command = "src/bistro/booking/commands/create_booking.py"
      handCase = "packs/python-htmx/generators/handle-body/tests/create-booking"
  it "(a) fills handle's body with the model until its tests pass, saying so as it goes" $ do
    given <- readTree (handCase <> "/given")
    fills <- canonicalizePath (handCase <> "/fills.toml")
    withPythonChecked (M.toList given) $ \ports dir -> do
      events <- newIORef []
      s0 <- newServerState ports {pVerifier = (`processVerifierCancellable` 120), pTester = (`processRunner` 120), pTesterWithin = flip processRunner}
      let s1 = s0 {ssConfig = (ssConfig s0) {scFills = Just fills}}
      (_, planned) <- actionPlanWith (\e -> modifyIORef' events (++ [e])) True (dir </> command) 50 "python-htmx/handle-body" [("app", "bistro"), ("service", "booking"), ("kind", "commands"), ("command", "create_booking")] s1
      seen <- readIORef events
      map describeProgress seen `shouldSatisfy` elem "testing body…"
      case planned of
        Right (_, plan) -> map phStatus (planHoles plan) `shouldSatisfy` all (\case HoleFilled _ _ -> True; _ -> False)
        Left why -> expectationFailure (T.unpack why)
  it "(a) fills a rule fix's holes with the model too, as `olifant fix` does" $ do
    recorded <- (</> "fills.toml") <$> getTemporaryDirectory
    TIO.writeFile recorded "[fills]\n\"port_call\" = \"let invoice = self.invoice_repository.get_invoice(\\\"inv-1\\\".to_string()).await;\"\n"
    withProjects [("shop", "rust-htmx", "packs/rust-htmx/fixtures/app")] $ \ports root -> do
      let file = root </> "shop/billing/billing-service/src/cancel_invoice.rs"
      original <- TIO.readFile file
      TIO.writeFile file (T.replace "        todo!(\"cancel_invoice\")" "        let invoice = invoices::table.find(\"inv-1\").first::<Invoice>(&mut conn).ok();\n        todo!(\"cancel_invoice\")" original)
      s0 <- newServerState ports
      let s1 = s0 {ssConfig = (ssConfig s0) {scFills = Just recorded}}
          line = 1 + length (takeWhile (not . T.isInfixOf "invoices::table") (T.lines (T.replace "        todo!(\"cancel_invoice\")" "        let invoice = invoices::table.find(\"inv-1\").first::<Invoice>(&mut conn).ok();\n        todo!(\"cancel_invoice\")" original)))
      (_, planned) <- fixPlan True (const (pure ())) file "commands-use-ports" line 1 [] s1
      case planned of
        Right (_, plan) -> map phStatus (planHoles plan) `shouldSatisfy` (\hs -> not (null hs) && all (\case HoleFilled _ _ -> True; _ -> False) hs)
        Left why -> expectationFailure (T.unpack why)
  it "(b) lists the project's unfilled holes: where, which, and what they are for" $
    withPython [("src/bistro/booking/commands/close_table.py", "def handle() -> None:\n    raise NotImplementedError(\"olifant: body: close the table, then say \\\"closed\\\"\")\n")] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc "src/bistro/booking/commands/close_table.py" "python"
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/holes")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)]]) >>= \case
          TResponseMessage _ _ (Right r) -> pure r
          TResponseMessage _ _ (Left err) -> fail (show err)
      let expected = object ["path" .= (dir </> "src/bistro/booking/commands/close_table.py"), "line" .= (2 :: Int), "name" .= ("body" :: Text), "kind" .= ("block" :: Text), "instruction" .= ("close the table, then say \"closed\"" :: Text)]
      lookupKey "holes" v `shouldBe` Just (Array (pure expected))
  it "(c) fills a hole on request: its code action resolves to the model's fill" $ do
    fills <- canonicalizePath (handCase <> "/fills.toml")
    withPython [("src/bistro/booking/commands/close_table.py", "def handle() -> None:\n    raise NotImplementedError(\"olifant: body: close the table\")\n")] $ \ports dir -> do
      (resolved, _) <- inProcessWithConfig [("fills", String (T.pack fills))] ports dir $ do
        doc <- openDoc "src/bistro/booking/commands/close_table.py" "python"
        actions <- getCodeActions doc (Range (Position 1 4) (Position 1 4))
        case [a | InR a <- actions, a ^. L.title == "Fill with the model"] of
          a : _ -> resolveCodeAction a
          [] -> fail ("no Fill action among " <> show [a ^. L.title | InR a <- actions])
      T.concat (editTexts resolved) `shouldSatisfy` T.isInfixOf "self.bookings.save(booking)"

  it "(e) stamps a resolved edit with the version it was planned against, though the document changed during the resolve (review C1)" $ do
    fills <- canonicalizePath (handCase <> "/fills.toml")
    entered <- newEmptyMVar
    gate <- newEmptyMVar
    -- the fill's check waits until the test has changed the document
    let gated = Checker $ \lang files -> putMVar entered () >> takeMVar gate >> runCheck noChecker lang files
    withPython [("src/bistro/booking/commands/close_table.py", "def handle() -> None:\n    raise NotImplementedError(\"olifant: body: close the table\")\n")] $ \ports dir -> do
      (versions, _) <- inProcessWithConfig [("fills", String (T.pack fills))] ports {pChecker = const gated} dir $ do
        doc <- openDoc "src/bistro/booking/commands/close_table.py" "python"
        actions <- getCodeActions doc (Range (Position 1 4) (Position 1 4))
        a <- case [a | InR a <- actions, a ^. L.title == "Fill with the model"] of
          a : _ -> pure a
          [] -> fail "no Fill action"
        rid <- sendRequest SMethod_CodeActionResolve a
        liftIO (timeout 30000000 (takeMVar entered)) >>= maybe (fail "the fill never checked") pure
        changeDoc doc [TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument "# typed meanwhile\ndef handle() -> None:\n    raise NotImplementedError(\"olifant: body: close the table\")\n"))]
        liftIO (threadDelay 300000) -- the change is in before the check goes on
        liftIO (putMVar gate () >> forkIO (let loop = putMVar gate () >> loop in loop))
        let awaitResolve = responseForId SMethod_CodeActionResolve rid <|> (anyMessage >> awaitResolve)
        TResponseMessage _ _ answer <- awaitResolve
        either (fail . show) (pure . editVersions) answer
      versions `shouldSatisfy` (\vs -> not (null vs) && all (== InL 0) vs)

  it "(f) stops a resolve's running check when the editor cancels it (review I5)" $ do
    fills <- canonicalizePath (handCase <> "/fills.toml")
    entered <- newEmptyMVar
    stopped <- newEmptyMVar
    -- a check that runs until its token trips (as cargo or pyright would)
    let endless token = Checker $ \_ _ -> do
          putMVar entered ()
          let wait = cancelled token >>= \case
                True -> putMVar stopped () >> pure (Left (CheckFailed "cancelled"))
                False -> threadDelay 20000 >> wait
          wait
    withPython [("src/bistro/booking/commands/close_table.py", "def handle() -> None:\n    raise NotImplementedError(\"olifant: body: close the table\")\n")] $ \ports dir -> do
      (seen, _) <- inProcessWithConfig [("fills", String (T.pack fills))] ports {pChecker = endless} dir $ do
        doc <- openDoc "src/bistro/booking/commands/close_table.py" "python"
        actions <- getCodeActions doc (Range (Position 1 4) (Position 1 4))
        a <- case [a | InR a <- actions, a ^. L.title == "Fill with the model"] of
          a : _ -> pure a
          [] -> fail "no Fill action"
        rid <- sendRequest SMethod_CodeActionResolve a
        liftIO (timeout 30000000 (takeMVar entered)) >>= maybe (fail "the fill never checked") pure
        sendNotification SMethod_CancelRequest (CancelParams (case rid of IdInt n -> InL n; IdString t -> InR t))
        liftIO (timeout 10000000 (takeMVar stopped))
      seen `shouldBe` Just ()

  it "(f2) answers a resolve cancelled from its progress as cancelled, not as a failure" $ do
    fills <- canonicalizePath (handCase <> "/fills.toml")
    entered <- newEmptyMVar
    let endless = Checker $ \_ _ -> tryPutMVar entered () >> let wait = threadDelay 20000 >> wait in wait
    withPython [("src/bistro/booking/commands/close_table.py", "def handle() -> None:\n    raise NotImplementedError(\"olifant: body: close the table\")\n")] $ \ports dir -> do
      (answer, _) <- inProcessWithConfig [("fills", String (T.pack fills))] ports {pChecker = const endless} dir $ do
        doc <- openDoc "src/bistro/booking/commands/close_table.py" "python"
        actions <- getCodeActions doc (Range (Position 1 4) (Position 1 4))
        a <- case [a | InR a <- actions, a ^. L.title == "Fill with the model"] of
          a : _ -> pure a
          [] -> fail "no Fill action"
        rid <- sendRequest SMethod_CodeActionResolve a
        let created :: FromServerMessage -> Maybe ProgressToken
            created = \case
              FromServerMess SMethod_WindowWorkDoneProgressCreate req -> Just (req ^. L.params . L.token)
              _ -> Nothing
            awaitCreate = satisfyMaybe created <|> (anyMessage >> awaitCreate)
        token <- awaitCreate
        liftIO (timeout 30000000 (takeMVar entered)) >>= maybe (fail "the fill never checked") pure
        sendNotification SMethod_WindowWorkDoneProgressCancel (WorkDoneProgressCancelParams token)
        let awaitResolve = responseForId SMethod_CodeActionResolve rid <|> (anyMessage >> awaitResolve)
        TResponseMessage _ _ r <- awaitResolve
        pure (either (Just . (^. L.code)) (const Nothing) r)
      answer `shouldBe` Just (InL LSPErrorCodes_RequestCancelled)

  it "(d1) names the model's ids by the project's `name`, so projects in folders of the same name do not collide (review minor)" $
    withPython [] $ \ports dir -> do
      config <- TIO.readFile (dir </> ".olifant/config.toml")
      TIO.writeFile (dir </> ".olifant/config.toml") ("name = \"bistro\"\n" <> config)
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc "src/bistro/booking/commands/create_booking.py" "python"
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/model")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)]]) >>= \case
          TResponseMessage _ _ (Right r) -> pure r
          TResponseMessage _ _ (Left err) -> fail (show err)
      lookupKey "project" v `shouldBe` Just (String "bistro")
      let ids = case lookupKey "elements" v of Just (Array es) -> [i | e <- foldr (:) [] es, Just (String i) <- [lookupKey "id" e]]; _ -> []
      ids `shouldSatisfy` (\is -> not (null is) && all ("bistro:" `T.isPrefixOf`) is)
  it "(d0) answers others while it reports its state (health) and re-lists a project (review minor)" $ do
    armed <- newIORef False
    entered <- newEmptyMVar
    gate <- newEmptyMVar
    withPython [] $ \ports dir -> do
      let disk = pFiles ports
          gated = disk {listFiles = \p -> do
                          held <- atomicModifyIORef' armed (False,)
                          when held (putMVar entered () >> takeMVar gate)
                          listFiles disk p}
      _ <- forkIO (threadDelay 5000000 >> void (tryPutMVar gate ()))
      (first, _) <- inProcessWith ports {pFiles = gated} dir $ do
        _ <- openDoc "src/bistro/booking/commands/create_booking.py" "python"
        liftIO (writeIORef armed True)
        sendNotification SMethod_WorkspaceDidChangeWatchedFiles (DidChangeWatchedFilesParams [FileEvent (filePathToUri (dir </> "src/new.py")) FileChangeType_Created])
        state <- sendRequest (SMethod_CustomMethod (Proxy :: Proxy "olifant/debugState")) Aeson.Null
        liftIO (timeout 10000000 (takeMVar entered)) >>= maybe (fail "debugState never re-listed") pure
        plan <- sendRequest (SMethod_CustomMethod (Proxy :: Proxy "olifant/plan")) (object ["planId" .= (999 :: Int)])
        let firstOf = (responseForId (SMethod_CustomMethod (Proxy :: Proxy "olifant/plan")) plan >> pure "plan")
              <|> (responseForId (SMethod_CustomMethod (Proxy :: Proxy "olifant/debugState")) state >> pure "debugState")
              <|> (anyMessage >> firstOf)
        winner <- firstOf
        liftIO (void (tryPutMVar gate ()))
        pure (winner :: Text)
      first `shouldBe` "plan"

  it "(d) says the project's model: its elements in the shared vocabulary, and how they relate (spec §1b)" $
    withProjects [("shop", "rust-htmx", "packs/rust-htmx/fixtures/app")] $ \ports root -> do
      (v, _) <- inProcessWith ports root $ do
        doc <- openDoc "shop/billing/billing-service/src/cancel_invoice.rs" "rust"
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/model")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)]]) >>= \case
          TResponseMessage _ _ (Right r) -> pure r
          TResponseMessage _ _ (Left err) -> fail (show err)
      let elements = case lookupKey "elements" v of Just (Array es) -> foldr (:) [] es; _ -> []
          kindOf i = [lookupKey "kind" e | e <- elements, fmap (T.isSuffixOf i) (textOf (lookupKey "id" e)) == Just True]
          textOf = \case Just (String t) -> Just t; _ -> Nothing
      kindOf ":billing/commands/cancel_invoice" `shouldBe` [Just (String "command")]
      lookupKey "relations" v `shouldSatisfy` (\case Just (Array rs) -> not (null rs); _ -> False)
      lookupKey "problems" v `shouldBe` Just (Array mempty)

-- | Regions kept on save, the generator picker, commands (Task 15).
pickerSpec :: Spec
pickerSpec = describe "regions on save, the generator picker, commands" $ do
  let encoded = TE.decodeUtf8 . BSL.toStrict . Aeson.encode
  it "(a) keeps regions on save: a new web resource joins resources.py's list" $
    withPython [] $ \ports dir -> do
      let routes = "src/bistro/web/floor/routes.py"
      createDirectoryIfMissing True (dir </> "src/bistro/web/floor")
      TIO.writeFile (dir </> routes) "from fastapi import APIRouter\n"
      (edit, _) <- inProcessWith ports dir $ do
        doc <- openDoc routes "python"
        sendNotification SMethod_TextDocumentDidSave (DidSaveTextDocumentParams doc Nothing)
        TRequestMessage _ _ _ params <- message SMethod_WorkspaceApplyEdit
        pure (params ^. L.edit)
      let texts = [either (^. L.newText) (^. L.newText) (toEither t) | Just cs <- [edit ^. L.documentChanges], InL te <- cs, t <- te ^. L.edits]
          toEither = \case InL x -> Left x; InR y -> Right y
      T.concat texts `shouldSatisfy` T.isInfixOf "\"floor\","
  it "(a) keeps the saved file's own regions as it saves (willSaveWaitUntil)" $
    withPython [] $ \ports dir -> do
      createDirectoryIfMissing True (dir </> "src/bistro/web/floor")
      TIO.writeFile (dir </> "src/bistro/web/floor/routes.py") "from fastapi import APIRouter\n"
      (edits, _) <- inProcessWith ports dir $ do
        doc <- openDoc "src/bistro/web/resources.py" "python"
        request SMethod_TextDocumentWillSaveWaitUntil (WillSaveTextDocumentParams doc TextDocumentSaveReason_Manual) >>= \case
          TResponseMessage _ _ (Right (InL es)) -> pure es
          other -> fail (show other)
      T.concat (map (^. L.newText) edits) `shouldSatisfy` T.isInfixOf "\"floor\","
  it "(a) refreshes regions once per save: didSave sends what willSaveWaitUntil worked out (review minor)" $ do
    reads <- newIORef (0 :: Int)
    withPython [] $ \ports dir -> do
      let routes = "src/bistro/web/floor/routes.py"
          disk = pFiles ports
          counted = disk {readText = \p -> modifyIORef' reads (+ 1) >> readText disk p}
      createDirectoryIfMissing True (dir </> "src/bistro/web/floor")
      TIO.writeFile (dir </> routes) "from fastapi import APIRouter\n"
      ((during, edit), _) <- inProcessWith ports {pFiles = counted} dir $ do
        doc <- openDoc routes "python"
        _ <- request SMethod_TextDocumentWillSaveWaitUntil (WillSaveTextDocumentParams doc TextDocumentSaveReason_Manual)
        before <- liftIO (readIORef reads)
        sendNotification SMethod_TextDocumentDidSave (DidSaveTextDocumentParams doc Nothing)
        TRequestMessage _ _ _ params <- message SMethod_WorkspaceApplyEdit
        after <- liftIO (readIORef reads)
        pure (after - before, params ^. L.edit)
      let texts = [either (^. L.newText) (^. L.newText) (toEither t) | Just cs <- [edit ^. L.documentChanges], InL te <- cs, t <- te ^. L.edits]
          toEither = \case InL x -> Left x; InR y -> Right y
      T.concat texts `shouldSatisfy` T.isInfixOf "\"floor\","
      during `shouldBe` 0
  it "(b) lists the generators, each with its inputs (the picker)" $
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc "src/bistro/main.py" "python"
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/generators")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)]]) >>= \case
          TResponseMessage _ _ (Right r) -> pure r
          TResponseMessage _ _ (Left err) -> fail (show err)
      encoded v `shouldSatisfy` T.isInfixOf "\"python-htmx/command\""
      encoded v `shouldSatisfy` T.isInfixOf "\"inputs\""
  it "(c) gives a generator's choices, given what is set so far" $
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc "src/bistro/main.py" "python"
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/choices")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "generator" .= ("python-htmx/command" :: Text), "sets" .= object ["app" .= ("bistro" :: Text)]]) >>= \case
          TResponseMessage _ _ (Right r) -> pure r
          TResponseMessage _ _ (Left err) -> fail (show err)
      encoded v `shouldSatisfy` T.isInfixOf "\"booking\""
  it "(c) plans a generator from the inputs chosen (the picker's last step), its plan and edit" $
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc "src/bistro/main.py" "python"
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/plan")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "generator" .= ("python-htmx/model" :: Text), "sets" .= object ["app" .= ("bistro" :: Text), "service" .= ("booking" :: Text), "model" .= ("guest" :: Text)]]) >>= \case
          TResponseMessage _ _ (Right r) -> pure r
          TResponseMessage _ _ (Left err) -> fail (show err)
      (lookupKey "plan" v >>= lookupKey "files") `shouldSatisfy` maybe False (\case Array fs -> not (null fs); _ -> False)
      encoded v `shouldSatisfy` T.isInfixOf "models/guest.py"
  it "(d) runs the doctor: what generators need here, and how to fix it" $
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc "src/bistro/main.py" "python"
        request SMethod_WorkspaceExecuteCommand (ExecuteCommandParams Nothing "olifant.doctor" (Just [toJSON (doc ^. L.uri)])) >>= \case
          TResponseMessage _ _ (Right (InL r)) -> pure r
          other -> fail (show other)
      (lookupKey "checks" v) `shouldSatisfy` maybe False (\case Array cs -> not (null cs); _ -> False)

-- | Many projects, many conventions, one session (Task 16).
conventionsSpec :: Spec
conventionsSpec = describe "many conventions in one session" $ do
  let generatorsOf' doc =
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/generators")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)]]) >>= \case
          TResponseMessage _ _ (Right v) -> pure [i | Just (Array gs) <- [lookupKey "generators" v], Object g <- foldr (:) [] gs, Just (String i) <- [KM.lookup "id" g]]
          TResponseMessage _ _ (Left err) -> fail (show err)
      descriptions doc =
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/generators")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)]]) >>= \case
          TResponseMessage _ _ (Right v) -> pure [d | Just (Array gs) <- [lookupKey "generators" v], Object g <- foldr (:) [] gs, Just (String d) <- [KM.lookup "description" g]]
          TResponseMessage _ _ (Left err) -> fail (show err)
  it "(a) gives each file its own project's conventions: a Python app and a Rust app side by side" $
    withProjects [("py", "python-htmx", "packs/python-htmx/fixtures/app"), ("rs", "rust-htmx", "packs/rust-htmx/fixtures/app")] $ \ports root -> do
      ((py, rs), _) <- inProcessWith ports root $ do
        a <- openDoc "py/src/bistro/main.py" "python"
        b <- openDoc "rs/billing/billing-service/src/cancel_invoice.rs" "rust"
        (,) <$> generatorsOf' a <*> generatorsOf' b
      py `shouldSatisfy` (\gs -> not (null gs) && all ("python-htmx/" `T.isPrefixOf`) gs)
      rs `shouldSatisfy` (\gs -> not (null gs) && all ("rust-htmx/" `T.isPrefixOf`) gs)
  it "(a2) watches each project's files and its packs relative to their own folders, whatever the editor's workspace folders (review I3, I8)" $
    withProjects [("py", "python-htmx", "packs/python-htmx/fixtures/app"), ("rs", "rust-htmx", "packs/rust-htmx/fixtures/app")] $ \ports root -> do
      (registered, _) <- inProcessWith ports root $ do
        _ <- openDoc "py/src/bistro/main.py" "python"
        _ <- openDoc "rs/billing/billing-service/src/cancel_invoice.rs" "rust"
        _ <- debugState
        TE.decodeUtf8 . BSL.toStrict . Aeson.encode <$> getRegisteredCapabilities
      pyPack <- canonicalizePath "packs/python-htmx"
      let relative dir = "{\"baseUri\":" <> TE.decodeUtf8 (BSL.toStrict (Aeson.encode (filePathToUri dir))) <> ",\"pattern\":\"**/*\"}"
      forM_ [root </> "py", root </> "rs", pyPack] $ \dir ->
        (dir, relative dir `T.isInfixOf` registered) `shouldBe` (dir, True)
  it "(b) keeps two copies of a pack apart: each project sees its own" $
    withPackCopies $ \ports root packB -> do
      createDirectoryIfMissing True (packB </> "generators/extra")
      TIO.writeFile (packB </> "generators/extra/generator.toml") "description = \"only in the second copy\"\nlanguage = \"python\"\n[[edits]]\ncreate = \"x.py\"\ntext = \"x = 1\\n\"\n"
      ((one, two), _) <- inProcessWith ports root $ do
        a <- openDoc "one/src/bistro/main.py" "python"
        b <- openDoc "two/src/bistro/main.py" "python"
        (,) <$> generatorsOf' a <*> generatorsOf' b
      (one, two) `shouldSatisfy` (\(xs, ys) -> "python-htmx/extra" `notElem` xs && "python-htmx/extra" `elem` ys)
  it "(c) reloads only the projects that use a pack when its files change" $
    withPackCopies $ \ports root packB -> do
      let gen = packB </> "generators/command/generator.toml"
      ((before, after, loads), _) <- inProcessWith ports root $ do
        a <- openDoc "one/src/bistro/main.py" "python"
        b <- openDoc "two/src/bistro/main.py" "python"
        before <- descriptions b
        liftIO (TIO.readFile gen >>= TIO.writeFile gen . T.replace "description = \"" "description = \"Changed: ")
        sendNotification SMethod_WorkspaceDidChangeWatchedFiles (DidChangeWatchedFilesParams [FileEvent (filePathToUri gen) FileChangeType_Changed])
        after <- descriptions b
        _ <- descriptions a
        (before, after,) . lookupKey "projects" <$> debugState
      before `shouldNotSatisfy` any ("Changed: " `T.isPrefixOf`)
      after `shouldSatisfy` any ("Changed: " `T.isPrefixOf`)
      loads `shouldBe` Just (object [Key.fromString (root </> "one") .= (1 :: Int), Key.fromString (root </> "two") .= (2 :: Int)])

-- | A plain LSP client (no olifant/* methods), and latency at scale.
scaleSpec :: Spec
scaleSpec = sequential . describe "a plain client; latency at 5,000 files" $ do
  it "(d) serves a client that knows only standard LSP: diagnostics, a quickfix, a refactor, a completion" $
    withPython [("src/bistro/booking/commands/close_table.py", "import sqlite3\n\n\ndef f() -> None:\n    pass\n")] $ \ports dir -> do
      ((ds, fixes, refactors, completions), _) <- inProcessWith ports dir $ do
        bad <- openDoc "src/bistro/booking/commands/close_table.py" "python"
        ds <- pullDiagnostics bad
        fixes <- getCodeActions bad (Range (Position 0 0) (Position 0 5))
        query <- openDoc "src/bistro/booking/queries/get_day_schedule.py" "python"
        refactors <- getCodeActions query (Range (Position 18 30) (Position 18 30))
        port <- openDoc "src/bistro/booking/ports/booking_repository/booking_repository.py" "python"
        completions <- getCompletions port (Position 12 0)
        resolved <- case [a | InR a <- refactors, a ^. L.kind == Just CodeActionKind_RefactorRewrite, maybe False (\d -> lookupKey "missing" d == Just (Array mempty)) (a ^. L.data_)] of
          a : _ -> Just <$> resolveCodeAction a
          [] -> pure Nothing
        pure (ds, [a | InR a <- fixes, a ^. L.kind == Just CodeActionKind_QuickFix], fmap (^. L.edit) resolved, completions)
      (null ds, null fixes, refactors, null completions) `shouldSatisfy` (\(a, b, c, d) -> not a && not b && maybe False (/= Nothing) c && not d)
  it "(f) recognises fast after a pause: the whole answer, planned, in the fixture (median of 20 edits)" $
    withPython [] $ \ports dir -> do
      let ownWay = "class CloseTable:\n    def __init__(self, repo):\n        self.repo = repo\n\n    def run(self, table_id):\n        table = self.repo.table(table_id)\n        self.repo.save(table)\n"
          ask doc = request (SMethod_CustomMethod (Proxy :: Proxy "olifant/suggest")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "line" .= (1 :: Int)])
          timed act = do
            t0 <- liftIO getCurrentTime
            _ <- act
            t1 <- liftIO getCurrentTime
            pure (realToFrac (diffUTCTime t1 t0) * 1000 :: Double)
      ((first, times), _) <- inProcessWith ports dir $ do
        uri <- getDocUri "src/bistro/booking/commands/close_table.py"
        sendNotification SMethod_TextDocumentDidOpen (DidOpenTextDocumentParams (TextDocumentItem uri "python" 0 ownWay))
        let doc = TextDocumentIdentifier uri
        first <- timed (ask doc)
        times <- forM [1 .. 20 :: Int] $ \v -> do
          sendNotification SMethod_TextDocumentDidChange (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier uri (fromIntegral v)) [TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument (ownWay <> "\n# " <> T.pack (show v) <> "\n")))])
          timed (ask doc)
        pure (first, times)
      let median = sort times !! 10
      liftIO (putStrLn ("      recognition, planned: first " <> show (round first :: Int) <> " ms, then median " <> show (round median :: Int) <> " ms, max " <> show (round (maximum times) :: Int) <> " ms"))
      median `shouldSatisfy` (< 50)
  it "(e) answers fast in a project of 5,000 files: the first request, then from warm" $
    withPython [] $ \ports dir -> do
      forM_ [1 .. 5000 :: Int] $ \i -> do
        let p = dir </> "src/bistro/bulk" </> ("m" <> show (i `div` 100)) </> ("mod_" <> show i <> ".py")
        createDirectoryIfMissing True (takeDirectory p)
        TIO.writeFile p ("x_" <> T.pack (show i) <> " = " <> T.pack (show i) <> "\n")
      let query = "src/bistro/booking/queries/get_day_schedule.py"
          test = "src/bistro/booking/commands/create_booking__test.py"
          timed act = do
            t0 <- liftIO getCurrentTime
            _ <- act
            t1 <- liftIO getCurrentTime
            pure (realToFrac (diffUTCTime t1 t0) * 1000 :: Double)
          median xs = sort xs !! (length xs `div` 2)
      ((firstMs, actionsMs, hintsMs, completionMs), _) <- inProcessWith ports dir $ do
        doc <- openDoc query "python"
        firstMs <- timed (getCodeActions doc (Range (Position 18 4) (Position 18 4)))
        actionsMs <- forM [1 .. 20 :: Int] (const (timed (getCodeActions doc (Range (Position 18 4) (Position 18 4)))))
        hintsMs <- forM [1 .. 20 :: Int] (const (timed (request SMethod_TextDocumentInlayHint (InlayHintParams Nothing doc (Range (Position 0 0) (Position 200 0))))))
        t <- openDoc test "python"
        completionMs <- forM [1 .. 20 :: Int] (const (timed (getCompletions t (Position 34 0))))
        -- after each edit nothing is cached: what typing costs
        editedMs <- forM [1 .. 10 :: Int] $ \i -> do
          changeDoc doc [TextDocumentContentChangeEvent (InL (TextDocumentContentChangePartial (Range (Position 0 0) (Position 0 0)) Nothing ("# edit " <> T.pack (show i) <> "\n")))]
          timed (request SMethod_TextDocumentInlayHint (InlayHintParams Nothing doc (Range (Position 0 0) (Position 200 0))))
        liftIO (putStrLn ("      inlay hints just after an edit: median " <> show (round (median editedMs) :: Int) <> " ms, max " <> show (round (maximum editedMs) :: Int) <> " ms"))
        pure (firstMs, actionsMs, hintsMs, completionMs)
      let line name xs = "      " <> name <> ": median " <> show (round (median xs) :: Int) <> " ms, max " <> show (round (maximum xs) :: Int) <> " ms"
      liftIO (mapM_ putStrLn ["      first request (loads the project): " <> show (round firstMs :: Int) <> " ms", line "code actions" actionsMs, line "inlay hints" hintsMs, line "completion" completionMs])
      firstMs `shouldSatisfy` (< 500)
      (median actionsMs, median hintsMs, median completionMs) `shouldSatisfy` (\(a, h, c) -> a < 50 && h < 50 && c < 50)

-- | Projects side by side under one root: (folder, pack, fixture app).
withProjects :: [(FilePath, Text, FilePath)] -> (Ports -> FilePath -> IO a) -> IO a
withProjects projects act = do
  tmp <- getTemporaryDirectory
  withSystemTempDirectory "olifant-lsp-many" $ \root0 -> do
    root <- canonicalizePath root0
    forM_ projects $ \(folder, pack, fixture) -> do
      packDir <- canonicalizePath ("packs" </> T.unpack pack)
      files <- readTree fixture
      forM_ (M.toList files) $ \(p, t) -> createDirectoryIfMissing True (takeDirectory (root </> folder </> p)) >> TIO.writeFile (root </> folder </> p) t
      createDirectoryIfMissing True (root </> folder </> ".olifant")
      TIO.writeFile (root </> folder </> ".olifant/config.toml") ("[packs]\n" <> pack <> " = { path = \"" <> T.pack packDir <> "\" }\n")
    act plainPorts {pSyntax = astGrepSyntax (tmp </> "olifant-test-cache")} root

-- | Two python-htmx apps, "one" and "two", each on its own copy of the
-- pack (the second copy's path given, to change it).
withPackCopies :: (Ports -> FilePath -> FilePath -> IO a) -> IO a
withPackCopies act = do
  tmp <- getTemporaryDirectory
  pack <- readTree "packs/python-htmx"
  fixture <- readTree "packs/python-htmx/fixtures/app"
  withSystemTempDirectory "olifant-lsp-copies" $ \root0 -> do
    root <- canonicalizePath root0
    forM_ ["one", "two"] $ \name -> do
      let packDir = root </> ("pack-" <> name)
      forM_ (M.toList pack) $ \(p, t) -> createDirectoryIfMissing True (takeDirectory (packDir </> p)) >> TIO.writeFile (packDir </> p) t
      forM_ (M.toList fixture) $ \(p, t) -> createDirectoryIfMissing True (takeDirectory (root </> name </> p)) >> TIO.writeFile (root </> name </> p) t
      createDirectoryIfMissing True (root </> name </> ".olifant")
      TIO.writeFile (root </> name </> ".olifant/config.toml") ("[packs]\npython-htmx = { path = \"" <> T.pack packDir <> "\" }\n")
    act plainPorts {pSyntax = astGrepSyntax (tmp </> "olifant-test-cache")} root (root </> "pack-two")

-- | Slow work runs beside the requests that come meanwhile (the server
-- reads on; progress can begin).
-- | `olifant/stub`: a new test's stub for the unit at a position.
stubSpec :: Spec
stubSpec = describe "olifant/stub" $ do
  let moduleRel = "src/bistro/booking/commands/create_booking.py"
      testRel = "src/bistro/booking/commands/create_booking__test.py"
  it "in a method: the test file's edit, its file, the name and its line; outside any unit, why" $
    withPython [] $ \ports dir -> do
      ((answer, outside), _) <- inProcessWithConfig [] ports dir $ do
        doc <- openDoc moduleRel "python"
        text <- documentContents doc
        -- the line after `def handle(`, 0-based
        let handleAt = length (takeWhile (not . ("    def handle(" `T.isPrefixOf`)) (T.lines text))
            ask line = blockMove (Proxy :: Proxy "olifant/stub") (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "position" .= object ["line" .= line, "character" .= (0 :: Int)]])
        answer <- ask (handleAt + 1)
        outside <- ask (0 :: Int)
        pure (answer, outside)
      let field k = either (const Nothing) (lookupKey k)
      field "file" answer `shouldBe` Just (String (T.pack (dir </> testRel)))
      field "name" answer `shouldBe` Just (String "test_handle")
      field "line" answer `shouldSatisfy` \case Just (Number _) -> True; _ -> False
      field "edits" answer `shouldSatisfy` \case Just (Object _) -> True; _ -> False
      outside `shouldSatisfy` either ("no function or class at line 1" `T.isInfixOf`) (const False)

-- | `olifant/conjure`: the unit at a position conjured (recorded fills, a
-- scripted runner), its tests run before and after.
conjureSpec :: Spec
conjureSpec = describe "olifant/conjure" $ do
  let moduleRel = "src/bistro/booking/commands/create_booking.py"
      -- the labels of a paused object's offers
      offerLabels :: Maybe Value -> [Text]
      offerLabels = \case
        Just (Object p) | Just (Array os) <- KM.lookup "offers" p -> [l | Object o <- toList os, Just (String l) <- [KM.lookup "label" o]]
        _ -> []
      offerDetails :: Maybe Value -> [Text]
      offerDetails = \case
        Just (Object p) | Just (Array os) <- KM.lookup "offers" p -> [l | Object o <- toList os, Just (String l) <- [KM.lookup "detail" o]]
        _ -> []
      -- a unified diff's changed lines (its headers left out)
      changedLines :: Text -> [Text]
      changedLines d = [l | l <- T.lines d, any (`T.isPrefixOf` l) ["+", "-"], not (any (`T.isPrefixOf` l) ["+++", "---"])]
      -- the (param, port) of a paused object's wires
      wiredPorts :: Maybe Value -> [(Text, Text)]
      wiredPorts = \case
        Just (Object p) | Just (Array ws) <- KM.lookup "wired" p -> [(a, b) | Object w <- toList ws, Just (String a) <- [KM.lookup "param" w], Just (String b) <- [KM.lookup "port" w]]
        _ -> []
  it "in a method: the edit, the summary and the after-run; outside any unit, why" $
    withSystemTempDirectory "olifant-conjure" $ \tmp -> do
      let script = tmp </> "tests.json"
          fills = tmp </> "fills.toml"
      BSL.writeFile script "{}"
      TIO.writeFile fills "[fills]\nbody = \"return None\"\n"
      withPython [] $ \ports dir -> do
        ((answer, outside), _) <- inProcessWithConfig [("test_script", String (T.pack script)), ("fills", String (T.pack fills))] ports dir $ do
          doc <- openDoc moduleRel "python"
          text <- documentContents doc
          -- the line after `def handle(`, 0-based
          let handleAt = length (takeWhile (not . ("    def handle(" `T.isPrefixOf`)) (T.lines text))
              ask line = blockMove (Proxy :: Proxy "olifant/conjure") (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "position" .= object ["line" .= line, "character" .= (0 :: Int)]])
          answer <- ask (handleAt + 1)
          outside <- ask (0 :: Int)
          pure (answer, outside)
        let field k = either (const Nothing) (lookupKey k)
        field "file" answer `shouldBe` Just (String (T.pack (dir </> moduleRel)))
        field "unit" answer `shouldBe` Just (String "handle")
        field "summary" answer `shouldBe` Just (String "green")
        field "green" answer `shouldBe` Just (Bool True)
        field "tests" answer `shouldSatisfy` \case Just (Object _) -> True; _ -> False
        field "edits" answer `shouldSatisfy` \case Just (Object _) -> True; _ -> False
        outside `shouldSatisfy` either ("no function or class at line 1" `T.isInfixOf`) (const False)

  it "a steered conjure pauses as a result; resume stop keeping the body lands it; a token resumes once" $
    withSystemTempDirectory "olifant-conjure-steered" $ \tmp -> do
      let script = tmp </> "tests.json"
          fills = tmp </> "fills.toml"
          answers = tmp </> "answers.json"
      BSL.writeFile script "{\"src/bistro/booking/commands/create_booking__test.py::test_books_a_free_slot\": {\"line\": 34, \"message\": \"assert 1 == 2\"}}"
      TIO.writeFile fills "[fills]\nbody = \"return None\"\n"
      TIO.writeFile answers "{\"answers\": {}}"
      withPython [] $ \ports dir -> do
        old <- TIO.readFile (dir </> ".olifant/config.toml")
        TIO.writeFile (dir </> ".olifant/config.toml") (old <> "\n[model]\ndefault = \"local\"\nrounds = 1\n[model.backends.local]\nkind = \"openai-compatible\"\nurl = \"http://127.0.0.1:9\"\nmodel = \"q\"\n[model.backends.jev]\nkind = \"jev\"\n[decide]\nmodel = \"jev\"\n")
        let config = [("test_script", String (T.pack script)), ("fills", String (T.pack fills)), ("decide_answers", String (T.pack answers))]
        ((paused, stopped, again, later), _) <- inProcessWithConfig config ports dir $ do
          doc <- openDoc moduleRel "python"
          text <- documentContents doc
          let handleAt = length (takeWhile (not . ("    def handle(" `T.isPrefixOf`)) (T.lines text))
              base line = ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "position" .= object ["line" .= (line :: Int), "character" .= (0 :: Int)]]
              ask extra = blockMove (Proxy :: Proxy "olifant/conjure") (object (base (handleAt + 1) <> extra))
              resume tok choice = ask ["resume" .= object ["token" .= (tok :: Text), "choice" .= (choice :: Text)]]
          paused <- ask []
          stopped <- ask ["resume" .= object ["token" .= ("conjure-1" :: Text), "choice" .= ("stop" :: Text), "land" .= True]]
          again <- resume "conjure-1" "stop"
          later <- resume "conjure-1" "later"
          pure (paused, stopped, again, later)
        let field k = either (const Nothing) (lookupKey k)
            inner k v = case v of Just (Object o) -> KM.lookup k o; _ -> Nothing
            pausedObj = field "paused" paused
        inner "token" pausedObj `shouldBe` Just (String "conjure-1")
        inner "why" pausedObj `shouldSatisfy` \case Just (String x) -> "1 round used (" `T.isPrefixOf` x; _ -> False
        offerLabels pausedObj `shouldBe` ["continue", "continue with a hint\8230", "fix the test: books_a_free_slot", "stop", "keep round 1"]
        inner "round" pausedObj `shouldBe` Just (Number 1)
        inner "rung" pausedObj `shouldBe` Just (String "local")
        inner "summary" pausedObj `shouldBe` Just (String "red: test_books_a_free_slot \183 assert 1 == 2")
        let lines' = inner "lines" pausedObj
        inner "first" lines' `shouldSatisfy` \case Just (Number _) -> True; _ -> False
        inner "last" lines' `shouldSatisfy` \case Just (Number _) -> True; _ -> False
        field "summary" stopped `shouldBe` Just (String "kept round 1 \183 red: books_a_free_slot")
        field "stopped" stopped `shouldBe` Just (Bool True)
        field "edits" stopped `shouldSatisfy` \case Just (Object _) -> True; _ -> False
        field "rounds" stopped `shouldBe` Just (Number 1)
        field "rungs" stopped `shouldBe` Just (toJSON ["local" :: Text])
        again `shouldSatisfy` either ("gone" `T.isInfixOf`) (const False)
        later `shouldSatisfy` either ("continue, climb, stop, reconjure, accept, reject or wire" `T.isInfixOf`) (const False)

  -- the files of the steered conjures below; FIELD a key of an answer, INNER of an object;
  -- each changed document's path (from its uri) and its edits' new texts
  let testRel = "src/bistro/booking/commands/create_booking__test.py"
      fixture p = TIO.readFile ("packs/python-htmx/fixtures/app" </> p)
      -- create_booking with no bookings port: the class lacks it
      unwired = T.replace "from bistro.booking.ports.booking_repository.booking_repository import BookingRepository\n" "" . T.replace "self, bookings: BookingRepository, ids: IdGenerator" "self, ids: IdGenerator" . T.replace "        self.bookings = bookings\n" ""
      field k = either (const Nothing) (lookupKey k)
      inner k v = case v of Just (Object o) -> KM.lookup k o; _ -> Nothing
      changes v = [(T.pack (fromMaybe "" (uriToFilePath (Uri u))), [t | Object te <- toList es, Just (String t) <- [KM.lookup "newText" te]]) | Just (Array cs) <- [inner "documentChanges" v], Object c <- toList cs, Just (Object d) <- [KM.lookup "textDocument" c], Just (String u) <- [KM.lookup "uri" d], Just (Array es) <- [KM.lookup "edits" c]]
      wroteIn rel needle v = any (\(p, ts) -> T.pack rel `T.isSuffixOf` p && any (needle `T.isInfixOf`) ts) (changes v)

  -- the cancel_booking walk (conjure-pause-context-design.md §3.5): the demo's morning
  -- cancel_booking (CancelBooking() portless, handle returning Ok(None), not_found building a
  -- Mock() it never passes in), rounds = 6, Jev answering nothing, WIRE the `[conjure] wire`;
  -- MODULE and TEST the two files; SESSION given the test script's path, ask and resume; the
  -- step log's lines (less their time) after, and the questions Jev was asked
  let cbTest = "src/bistro/booking/commands/cancel_booking__test.py"
      cbModule = "src/bistro/booking/commands/cancel_booking.py"
      notFound = T.pack cbTest <> "::test_cancel_booking_not_found"
      morningTest =
        T.unlines
          [ "from unittest.mock import Mock"
          , ""
          , "from bistro.booking.commands.cancel_booking import ("
          , "    CancelBooking,"
          , "    CancelBookingInput,"
          , "    NotFound,"
          , ")"
          , "from bistro.shared.result import Ok"
          , ""
          , ""
          , "def command() -> CancelBooking:"
          , "    return CancelBooking()"
          , ""
          , ""
          , "def test_cancel_booking_succeeds() -> None:"
          , "    cancel_booking = command()"
          , "    result = cancel_booking.handle(CancelBookingInput(id=\"1\"))"
          , "    assert result == Ok(None)"
          , ""
          , ""
          , "def test_cancel_booking_not_found() -> None:"
          , "    repository = Mock()"
          , "    repository.find_by_id = lambda id: None"
          , "    cancel_booking = command()"
          , "    result = cancel_booking.handle(CancelBookingInput(id=\"1\"))"
          , "    assert isinstance(result, NotFound)"
          , ""
            ]
      morningModule =
        T.unlines
          [ "\"\"\"Cancel booking.\"\"\""
          , ""
          , "from dataclasses import dataclass"
          , ""
          , "from bistro.shared.result import Ok"
          , ""
          , ""
          , "@dataclass(frozen=True)"
          , "class CancelBookingInput:"
          , "    id: str"
          , ""
          , ""
          , "@dataclass(frozen=True)"
          , "class NotFound:"
          , "    reason: str"
          , ""
          , ""
          , "type CancelBookingFailure = NotFound"
          , ""
          , "type CancelBookingOutput = Ok[None] | CancelBookingFailure"
          , ""
          , ""
          , "class CancelBooking:"
          , "    def __init__(self) -> None:"
          , "        \"\"\"Takes the ports it uses; main.py passes their adapters.\"\"\""
          , ""
          , "    def handle(self, input: CancelBookingInput) -> CancelBookingOutput:"
          , "        return Ok(None)"
          , ""
            ]
      wiredTest =
        T.unlines
          [ "from unittest.mock import Mock"
          , ""
          , "from bistro.booking.commands.cancel_booking import ("
          , "    CancelBooking,"
          , "    CancelBookingInput,"
          , "    NotFound,"
          , ")"
          , "from bistro.shared.result import Ok"
          , "from bistro.booking.ports.booking_repository.in_memory_booking_repository import InMemoryBookingRepository"
          , "from bistro.booking.ports.booking_repository.booking_repository import BookingRepository"
          , ""
          , ""
          , "def command("
          , "    bookings: BookingRepository | None = None,"
          , ") -> CancelBooking:"
          , "    return CancelBooking("
          , "        bookings=bookings or InMemoryBookingRepository(),"
          , "    )"
          , ""
          , ""
          , "def test_cancel_booking_succeeds() -> None:"
          , "    cancel_booking = command()"
          , "    result = cancel_booking.handle(CancelBookingInput(id=\"1\"))"
          , "    assert result == Ok(None)"
          , ""
          , ""
          , "def test_cancel_booking_not_found() -> None:"
          , "    repository = Mock()"
          , "    repository.find_by_id = lambda id: None"
          , "    cancel_booking = command()"
          , "    result = cancel_booking.handle(CancelBookingInput(id=\"1\"))"
          , "    assert isinstance(result, NotFound)"
          , ""
            ]
      -- the module wired by hand, its port's type quoted: the class seems to lack it, the wire writes nothing
      quotedModule = T.replace "bookings: BookingRepository) -> None" "bookings: \"BookingRepository\") -> None" wiredModule
      wiredModule =
        T.unlines
          [ "\"\"\"Cancel booking.\"\"\""
          , ""
          , "from dataclasses import dataclass"
          , ""
          , "from bistro.shared.result import Ok"
          , "from bistro.booking.ports.booking_repository.booking_repository import BookingRepository"
          , ""
          , ""
          , "@dataclass(frozen=True)"
          , "class CancelBookingInput:"
          , "    id: str"
          , ""
          , ""
          , "@dataclass(frozen=True)"
          , "class NotFound:"
          , "    reason: str"
          , ""
          , ""
          , "type CancelBookingFailure = NotFound"
          , ""
          , "type CancelBookingOutput = Ok[None] | CancelBookingFailure"
          , ""
          , ""
          , "class CancelBooking:"
          , "    def __init__(self, bookings: BookingRepository) -> None:"
          , "        \"\"\"Takes the ports it uses; main.py passes their adapters.\"\"\""
          , "        self.bookings = bookings"
          , ""
          , "    def handle(self, input: CancelBookingInput) -> CancelBookingOutput:"
          , "        return Ok(None)"
          , ""
            ]
      round1Section = T.unlines ["src/bistro/booking/commands/cancel_booking__test.py:26: in test_cancel_booking_not_found", "    assert isinstance(result, NotFound)", "E   assert False", "E    +  where False = isinstance(Ok(value=None), NotFound)"]
      notFoundRed = Aeson.encode (object [Key.fromText notFound .= object ["line" .= (26 :: Int), "message" .= ("assert False\n+  where False = isinstance(Ok(value=None), NotFound)" :: Text), "output" .= round1Section]])
      cancelWith :: Maybe Text -> Text -> Text -> (FilePath -> ([(Key.Key, Value)] -> Session (Either Text Value)) -> (Text -> Text -> Session (Either Text Value)) -> Session a) -> IO (a, [[Text]], [String])
      cancelWith wire moduleText testText session = (\(a, steps, asked, _) -> (a, steps, asked)) <$> cancelOnDisk wire moduleText testText session
      -- the same, with the module and the test file as they are on disk after
      cancelOnDisk :: Maybe Text -> Text -> Text -> (FilePath -> ([(Key.Key, Value)] -> Session (Either Text Value)) -> (Text -> Text -> Session (Either Text Value)) -> Session a) -> IO (a, [[Text]], [String], (Text, Text))
      cancelOnDisk = cancelOnDiskWith noChecker
      -- a check that says pyright's two errors here: a lambda's untyped parameter, and BAD
      lambdaChecker = Checker $ \_ files ->
        let found p t = [((p, m), i) | (i, l) <- zip [1 ..] (T.lines t), (needle, m) <- [("lambda id: None", "Type of parameter \"id\" is unknown"), ("BAD", "\"BAD\" is not defined")], needle `T.isInfixOf` l]
            all' = concat [found p t | (p, t) <- M.toList files]
         in pure (Right (Report [] (map fst all') [] all'))
      -- the cancel_booking conjure with that check; the session's fills file is beside its script
      -- (the test calls the helper inline, so the scan knows no edit: the model re-conjures it)
      cancelChecked = (\(a, steps, asked, _) -> (a, steps, asked)) <$$> cancelOnDiskWith lambdaChecker (Just "wire") morningModule inlineTest
      inlineTest = T.replace "    cancel_booking = command()\n    result = cancel_booking.handle(" "    result = command().handle(" morningTest
      (<$$>) f g x = f <$> g x
      fillsBeside script body = TIO.writeFile (takeDirectory script </> "fills.toml") ("[fills]\nbody = " <> T.pack (show (T.unpack body)) <> "\n")
      notFoundBody bad = T.intercalate "\n" (["repository = Mock()", "repository.find_by_id = lambda id: None"] ++ ["BAD" | bad] ++ ["cancel_booking = command(bookings=repository)", "result = cancel_booking.handle(CancelBookingInput(id=\"1\"))", "assert isinstance(result, NotFound)"])
      cancelOnDiskWith checker wire moduleText testText session =
        withSystemTempDirectory "olifant-conjure-cancel" $ \tmp -> do
          let script = tmp </> "tests.json"
              fills = tmp </> "fills.toml"
              answers = tmp </> "answers.json"
          BSL.writeFile script notFoundRed
          TIO.writeFile fills "[fills]\nbody = \"return Ok(None)\"\n"
          TIO.writeFile answers "{\"answers\": {}}"
          withPython [(cbModule, moduleText), (cbTest, testText)] $ \ports0 dir -> do
            let ports = ports0 {pChecker = const checker}
            old <- TIO.readFile (dir </> ".olifant/config.toml")
            TIO.writeFile (dir </> ".olifant/config.toml") (old <> "\n[model]\ndefault = \"local\"\nrounds = 6\n[model.backends.local]\nkind = \"openai-compatible\"\nurl = \"http://127.0.0.1:9\"\nmodel = \"q\"\n[model.backends.jev]\nkind = \"jev\"\n[decide]\nmodel = \"jev\"\n" <> maybe "" (\w -> "[conjure]\nwire = \"" <> w <> "\"\n") wire)
            let config = [("test_script", String (T.pack script)), ("fills", String (T.pack fills)), ("decide_answers", String (T.pack answers))]
            (r, _) <- inProcessWithConfig config ports dir $ do
              doc <- openDoc cbModule "python"
              text <- documentContents doc
              let handleAt = length (takeWhile (not . ("    def handle(" `T.isPrefixOf`)) (T.lines text))
                  base = ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "position" .= object ["line" .= (handleAt + 1 :: Int), "character" .= (0 :: Int)]]
                  ask extra = blockMove (Proxy :: Proxy "olifant/conjure") (object (base <> extra))
                  resume tok choice = ask ["resume" .= object ["token" .= (tok :: Text), "choice" .= (choice :: Text)]]
              session script ask resume
            let stepLog = dir </> ".olifant/state/steps.log"
            found <- doesFileExist stepLog
            lines' <- if found then T.lines <$> TIO.readFile stepLog else pure []
            asked <- doesFileExist (answers <> ".asked") >>= \case
              True -> lines <$> readFile (answers <> ".asked")
              False -> pure []
            disk <- (,) <$> TIO.readFile (dir </> cbModule) <*> TIO.readFile (dir </> cbTest)
            pure (r, [drop 1 (T.splitOn "\t" l) | l <- lines'], asked, disk)
      cancelConjure = cancelWith (Just "wire") morningModule morningTest
      -- the first pause of a conjure over MODULE and TEST, the wire configured
      cancelConjure' moduleText testText = cancelWith (Just "wire") moduleText testText (\_ ask _ -> ask [])

  it "cancel_booking: the scan wires before round 1; round 1 pauses on the unsatisfiable test, no Jev asked" $ do
    ((paused, proposed, accepted), steps, asked) <- cancelConjure $ \script ask resume -> do
      paused <- ask []
      -- the tests pass from here on: the re-conjured test is green (allowed), the function lands
      liftIO (BSL.writeFile script "{}")
      proposed <- resume "conjure-1" "reconjure"
      accepted <- resume "conjure-1" "accept"
      pure (paused, proposed, accepted)
    let kinds = map (take 2) steps
    kinds `shouldSatisfy` elem ["conjure-test", "flagged"]
    steps `shouldSatisfy` elem ["conjure-test", "flagged", notFound, "repository", "Mock"]
    steps `shouldSatisfy` elem ["conjure-wire", "wire", "param=bookings", "port=BookingRepository", "app=bistro", "service=booking", "kind=commands", "command=cancel_booking"]
    steps `shouldSatisfy` elem ["conjure-pause", "test", notFound]
    map (take 1) (takeWhile ((/= ["conjure-pause"]) . take 1) steps) `shouldSatisfy` notElem ["conjure-next"]
    asked `shouldBe` []
    inner "why" (field "paused" paused) `shouldBe` Just (String "No body can pass not_found: it builds repository = Mock() and never hands it to command(), so handle never sees it. CancelBooking takes bookings: BookingRepository since the wire; the test should pass its fake as command(bookings=\8230).")
    offerLabels (field "paused" paused) `shouldBe` ["continue", "continue with a hint\8230", "fix the test: not_found", "stop", "keep round 1"]
    wiredPorts (field "paused" paused) `shouldBe` [("bookings", "BookingRepository")]
    offerDetails (field "paused" paused) !! 2 `shouldBe` "hand repository to command(bookings=repository); you see the diff"
    offerLabels (field "paused" proposed) `shouldBe` ["accept", "reject"]
    -- the scan knows the fix: the engine makes it, no model; the proposal is that one line
    inner "diff" (inner "proposal" (field "paused" proposed)) `shouldSatisfy` \case Just (String d) -> changedLines d == ["-    cancel_booking = command()", "+    cancel_booking = command(bookings=repository)"]; _ -> False
    steps `shouldSatisfy` elem ["conjure-proposal", notFound]
    steps `shouldSatisfy` elem ["conjure-resume", "reconjure"]
    steps `shouldSatisfy` elem ["conjure-resume", "accept"]
    -- the landing edits every file the wire wrote: the module and the test file's helper
    field "edits" accepted `shouldSatisfy` wroteIn cbModule "self.bookings = bookings"
    field "edits" accepted `shouldSatisfy` wroteIn cbTest "bookings: BookingRepository | None = None"
    -- every step was accepted: the landing asks nothing again (no annotation needing confirmation)
    inner "changeAnnotations" (field "edits" accepted) `shouldBe` Nothing
    show (field "edits" accepted) `shouldNotSatisfy` isInfixOf "annotationId"
    field "green" accepted `shouldBe` Just (Bool True)
    field "summary" accepted `shouldBe` Just (String "green in round 2 \183 local \183 bookings wired \183 not_found rewritten")

  it "the cancel_booking pause carries the title, the wire's diffs and the landing diff" $ do
    (paused, _, _) <- cancelConjure $ \_ ask _ -> ask []
    let p = field "paused" paused
        title = inner "title" p
        wired = case inner "wired" p of
          Just (Array ws) | w : _ <- toList ws -> Just w
          _ -> Nothing
        files = [(f, d) | Just (Array fs) <- [inner "files" wired], Object o <- toList fs, Just (String f) <- [KM.lookup "file" o], Just (String d) <- [KM.lookup "diff" o]]
        firstOffer = case inner "offers" p of
          Just (Array os) | o : _ <- toList os -> Just o
          _ -> Nothing
    inner "of" title `shouldBe` Just (Number 6)
    inner "rung" title `shouldBe` Just (String "local")
    inner "unit" title `shouldBe` Just (String "CancelBooking.handle")
    inner "model" title `shouldBe` Just (String "q")
    inner "param" wired `shouldBe` Just (String "bookings")
    inner "port" wired `shouldBe` Just (String "BookingRepository")
    inner "test" wired `shouldBe` Just (String notFound)
    map fst files `shouldSatisfy` \fs -> T.pack cbModule `elem` fs && T.pack cbTest `elem` fs
    map snd files `shouldSatisfy` all ("--- a/" `T.isPrefixOf`)
    inner "diff" (inner "landing" p) `shouldSatisfy` \case Just (String d) -> "--- a/src/bistro/booking/commands/cancel_booking.py" `T.isPrefixOf` d; _ -> False
    inner "expires" p `shouldBe` Just (Number 600)
    inner "header" (inner "unit" p) `shouldBe` Just (String "class CancelBooking:\n    def __init__(self, bookings: BookingRepository) -> None:")
    inner "ports" (inner "unit" p) `shouldBe` Just (toJSON ["bookings: BookingRepository" :: Text])
    firstOffer `shouldBe` Just (object ["choice" .= ("continue" :: Text), "arg" .= Aeson.Null, "label" .= ("continue" :: Text), "detail" .= ("6 more rounds with local, from round 2" :: Text), "land" .= Aeson.Null, "hint" .= Aeson.Null])

  it "without a wire generator the unsatisfiable test pauses round 1, the class's ports named" $ do
    (paused, steps, asked) <- cancelWith Nothing morningModule morningTest $ \_ ask _ -> ask []
    inner "why" (field "paused" paused) `shouldBe` Just (String "No body can pass not_found: it builds repository = Mock() and never hands it to command(), so handle never sees it. CancelBooking() takes no ports.")
    steps `shouldSatisfy` all (\fs -> take 1 fs /= ["conjure-wire"])
    steps `shouldSatisfy` elem ["conjure-pause", "test", notFound]
    asked `shouldBe` []

  it "a class that takes the port already: no wire, an unsatisfiable pause (never a wire in a loop)" $ do
    (paused, steps, _) <- cancelConjure' wiredModule wiredTest
    steps `shouldSatisfy` all (\fs -> take 1 fs `notElem` [["conjure-wire"], ["conjure-wire-failed"]])
    steps `shouldSatisfy` elem ["conjure-pause", "test", notFound]
    inner "why" (field "paused" paused) `shouldSatisfy` \case Just (String x) -> "CancelBooking takes bookings: BookingRepository." `T.isSuffixOf` x; _ -> False

  it "a wire that writes nothing is logged as failed once and the round goes on to a pause" $ do
    (paused, steps, _) <- cancelConjure' quotedModule wiredTest
    length (filter ((== ["conjure-wire-failed", "wire"]) . take 2) steps) `shouldBe` 1
    steps `shouldSatisfy` all (\fs -> take 1 fs /= ["conjure-wire"])
    steps `shouldSatisfy` elem ["conjure-pause", "test", notFound]
    inner "diagnosis" (field "paused" paused) `shouldSatisfy` \case Just (Array xs) -> String "The wire failed: wire writes nothing here." `elem` toList xs; _ -> False
    inner "trigger" (field "paused" paused) `shouldBe` Just (String ("unsatisfiable test: " <> notFound <> "; wire failed: wire writes nothing here"))
    inner "wired" (field "paused" paused) `shouldBe` Just (toJSON ([] :: [Text]))

  it "a written file edited during the pause ends the resume and drops the token" $ do
    ((paused, changed, again), _, _) <- cancelConjure $ \_ ask resume -> do
      paused <- ask []
      test <- openDoc cbTest "python"
      changeDoc test [TextDocumentContentChangeEvent (InL (TextDocumentContentChangePartial (Range (Position 0 0) (Position 0 0)) Nothing "# edited\n"))]
      changed <- resume "conjure-1" "continue"
      again <- resume "conjure-1" "continue"
      pure (paused, changed, again)
    wiredPorts (field "paused" paused) `shouldBe` [("bookings", "BookingRepository")]
    changed `shouldSatisfy` either ("your text stands" `T.isInfixOf`) (const False)
    again `shouldSatisfy` either ("gone" `T.isInfixOf`) (const False)

  it "reconjure answers a proposal; reject returns the pause, accept without a proposal is refused and keeps the token" $ do
    ((refused, proposed, rejected, again, continued), steps, _) <- cancelConjure $ \script ask resume -> do
      _ <- ask []
      refused <- resume "conjure-1" "accept"
      liftIO (BSL.writeFile script "{}")
      proposed <- resume "conjure-1" "reconjure"
      rejected <- resume "conjure-1" "reject"
      again <- resume "conjure-1" "reconjure"
      continued <- resume "conjure-1" "accept"
      pure (refused, proposed, rejected, again, continued)
    let proposal v = inner "proposal" (field "paused" v)
    refused `shouldBe` Left "there is no proposal to accept"
    inner "file" (proposal proposed) `shouldBe` Just (String (T.pack cbTest))
    inner "test" (proposal proposed) `shouldBe` Just (String notFound)
    inner "diff" (proposal proposed) `shouldSatisfy` \case Just (String d) -> "--- a/" `T.isPrefixOf` d; _ -> False
    inner "token" (field "paused" rejected) `shouldBe` Just (String "conjure-1")
    offerLabels (field "paused" rejected) `shouldBe` ["continue", "continue with a hint\8230", "fix the test: not_found", "stop", "keep round 1"]
    inner "proposal" (field "paused" rejected) `shouldBe` Nothing
    offerLabels (field "paused" again) `shouldBe` ["accept", "reject"]
    continued `shouldSatisfy` either (const False) (const True)
    steps `shouldSatisfy` any (== ["conjure-proposal", notFound])
    steps `shouldSatisfy` any (== ["conjure-resume", "reject"])

  it "cancel_booking with the real checker and pytest: the steps compose (the wire, the test's fix, the port's method), round by round what is red after each" $ do
    let body = T.intercalate "\n" ["booking = self.bookings.find_by_id(input.id)", "if booking is None:", "    return NotFound(\"Booking not found\")", "return Ok(None)"]
    withSystemTempDirectory "olifant-conjure-real" $ \tmp -> do
      let fills = tmp </> "fills.toml"
          answers = tmp </> "answers.json"
      TIO.writeFile fills ("[fills]\nbody = " <> T.pack (show (T.unpack body)) <> "\n")
      TIO.writeFile answers "{\"answers\": {}}"
      withPythonChecked [(cbModule, morningModule), (cbTest, morningTest)] $ \ports0 dir -> do
        let ports = ports0 {pVerifier = (`processVerifierCancellable` 120), pTester = (`processRunner` 120), pTesterWithin = flip processRunner}
        old <- TIO.readFile (dir </> ".olifant/config.toml")
        TIO.writeFile (dir </> ".olifant/config.toml") (old <> "\n[model]\ndefault = \"local\"\nrounds = 6\n[model.backends.local]\nkind = \"openai-compatible\"\nurl = \"http://127.0.0.1:9\"\nmodel = \"q\"\n[model.backends.jev]\nkind = \"jev\"\n[decide]\nmodel = \"jev\"\n[conjure]\nwire = \"wire\"\n")
        let config = [("fills", String (T.pack fills)), ("decide_answers", String (T.pack answers))]
        ((paused, proposed, after, portProposed, afterPort), _) <- inProcessWithConfig config ports dir $ do
          doc <- openDoc cbModule "python"
          text <- documentContents doc
          let handleAt = length (takeWhile (not . ("    def handle(" `T.isPrefixOf`)) (T.lines text))
              base = ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "position" .= object ["line" .= (handleAt + 1 :: Int), "character" .= (0 :: Int)]]
              ask extra = blockMove (Proxy :: Proxy "olifant/conjure") (object (base <> extra))
              resume tok choice = ask ["resume" .= object ["token" .= (tok :: Text), "choice" .= (choice :: Text)]]
              tokenOf v = case inner "token" (field "paused" v) of Just (String t) -> t; _ -> ""
          paused <- ask []
          proposed <- resume (tokenOf paused) "reconjure"
          after <- resume (tokenOf proposed) "accept"
          portProposed <- resume (tokenOf after) "port"
          afterPort <- resume (tokenOf portProposed) "accept"
          pure (paused, proposed, after, portProposed, afterPort)
        -- round 1 is refused by the check: BookingRepository has no find_by_id. That wall pauses
        -- at once (no Jev), offering its step beside the scanned test's fix
        inner "trigger" (field "paused" paused) `shouldBe` Just (String "a port lacks a method: BookingRepository.find_by_id")
        inner "round" (field "paused" paused) `shouldBe` Just (Number 1)
        offerLabels (field "paused" paused) `shouldSatisfy` \ls -> "add find_by_id to BookingRepository" `elem` ls && "fix the test: not_found" `elem` ls
        offerDetails (field "paused" paused) `shouldSatisfy` elem "hand repository to command(bookings=repository); you see the diff"
        inner "diff" (inner "proposal" (field "paused" proposed)) `shouldSatisfy` \case Just (String d) -> changedLines d == ["-    cancel_booking = command()", "+    cancel_booking = command(bookings=repository)"]; _ -> False
        -- with not_found fixed, round 2 writes the same body: no repeat (a step came between),
        -- and the check refuses it for the same wall
        inner "round" (field "paused" after) `shouldBe` Just (Number 2)
        inner "trigger" (field "paused" after) `shouldBe` Just (String "a port lacks a method: BookingRepository.find_by_id")
        inner "summary" (field "paused" after) `shouldSatisfy` \case Just (String x) -> "Cannot access attribute \"find_by_id\" for class \"BookingRepository\"" `T.isInfixOf` x; _ -> False
        -- composable: the accepted fix is a step after round 1, in force from round 2; no fix is
        -- offered again for the test it fixed (it has not failed anew since)
        inner "steps" (field "paused" after) `shouldSatisfy` \case
          Just (Array xs) -> [(inner "kind" (Just x), inner "after" (Just x), inner "text" (Just x)) | x <- toList xs] == [(Just (String "wire"), Just (Number 0), Just (String "wire bookings: BookingRepository")), (Just (String "test"), Just (Number 1), Just (String "test fixed: not_found (command(bookings=repository))"))]
          _ -> False
        offerLabels (field "paused" after) `shouldSatisfy` notElem "fix the test: not_found"
        offerLabels (field "paused" after) `shouldSatisfy` elem "add find_by_id to BookingRepository"
        -- the failing section is round 2's real failure: the check's refusal, its line; the
        -- diagnosis says it, not the before-run's assertion
        inner "kind" (inner "failure" (field "paused" after)) `shouldBe` Just (String "check")
        inner "tool" (inner "failure" (field "paused" after)) `shouldBe` Just (String "pyright")
        inner "round" (inner "failure" (field "paused" after)) `shouldBe` Just (Number 2)
        inner "problems" (inner "failure" (field "paused" after)) `shouldSatisfy` \case
          Just (Array (toList -> x : _)) -> inner "message" (Just x) == Just (String "Cannot access attribute \"find_by_id\" for class \"BookingRepository\"") && (case inner "line" (Just x) of Just (Number _) -> True; _ -> False)
          _ -> False
        inner "why" (field "paused" after) `shouldSatisfy` \case Just (String w) -> "BookingRepository has no find_by_id: pyright refuses round 2's body before the tests run (cancel_booking.py:" `T.isPrefixOf` w; _ -> False
        -- p: the pack's port-method writes the derived signature into the Protocol, and
        -- port-method-adapters each adapter; one proposal, every file's diff
        inner "kind" (inner "proposal" (field "paused" portProposed)) `shouldBe` Just (String "port")
        inner "step" (inner "proposal" (field "paused" portProposed)) `shouldBe` Just (String "port method: BookingRepository.find_by_id(id: str) -> Booking | None")
        inner "diff" (inner "proposal" (field "paused" portProposed)) `shouldSatisfy` \case Just (String d) -> "+    def find_by_id(self, id: str) -> Booking | None: ..." `elem` T.lines d; _ -> False
        inner "files" (inner "proposal" (field "paused" portProposed)) `shouldSatisfy` \case
          Just (Array fs) -> all (`elem` [f | String f <- toList fs]) ["src/bistro/booking/ports/booking_repository/booking_repository.py", "src/bistro/booking/ports/booking_repository/in_memory_booking_repository.py"]
          _ -> False
        offerLabels (field "paused" portProposed) `shouldBe` ["accept", "reject"]
        offerDetails (field "paused" portProposed) `shouldBe` ["the method lands with the conjure; round 3 runs against it", "back to the rows; the port stands as it was"]
        inner "checker" (field "paused" afterPort) `shouldBe` Just (String "pyright")
        -- the wire entry is the wire's files alone, not the steps after it
        inner "wired" (field "paused" afterPort) `shouldSatisfy` \case
          Just (Array (toList -> w : _)) -> case inner "files" (Just w) of
            Just (Array fs) -> [f | x <- toList fs, Just (String f) <- [inner "file" (Just x)]] == map T.pack [cbModule, cbTest]
            _ -> False
          _ -> False
        -- accepted, it is a step: round 3 type-checks, and its tests ran
        inner "steps" (field "paused" afterPort) `shouldSatisfy` \case
          Just (Array xs) -> [inner "kind" (Just x) | x <- toList xs] == map (Just . String) ["wire", "test", "port"]
          _ -> False
        inner "round" (field "paused" afterPort) `shouldBe` Just (Number 3)
        inner "kind" (inner "failure" (field "paused" afterPort)) `shouldBe` Just (String "tests")
        offerLabels (field "paused" afterPort) `shouldSatisfy` notElem "add find_by_id to BookingRepository"
  it "a re-conjure is refused only for what it adds: the lambda's error the test already had does not refuse it" $ do
    (proposed, steps, _) <- cancelChecked $ \script ask resume -> do
      _ <- ask []
      liftIO (BSL.writeFile script "{}")
      liftIO (fillsBeside script (notFoundBody False))
      resume "conjure-1" "reconjure"
    offerLabels (field "paused" proposed) `shouldBe` ["accept", "reject"]
    inner "diff" (inner "proposal" (field "paused" proposed)) `shouldSatisfy` \case Just (String d) -> "+    cancel_booking = command(bookings=repository)" `elem` T.lines d; _ -> False
    steps `shouldSatisfy` elem ["conjure-proposal", notFound]

  it "a round is refused for a pyright error the test file gained; one it had at the start stands, and the green says so" $ do
    -- a re-conjure the person accepted although it was refused leaves BAD in the test file: new
    (refused, _, _) <- cancelChecked $ \script ask resume -> do
      _ <- ask []
      liftIO (BSL.writeFile script "{}")
      liftIO (fillsBeside script (notFoundBody True))
      _ <- resume "conjure-1" "reconjure"
      liftIO (fillsBeside script "return Ok(None)")
      resume "conjure-1" "accept"
    inner "kind" (inner "failure" (field "paused" refused)) `shouldBe` Just (String "check")
    field "green" refused `shouldBe` Nothing
    -- the same error there from the start is not new: the round is green, and the summary says so
    (green, _, _, _) <- cancelOnDiskWith lambdaChecker (Just "wire") morningModule morningTest $ \script ask resume -> do
      _ <- ask []
      liftIO (BSL.writeFile script "{}")
      _ <- resume "conjure-1" "reconjure"
      resume "conjure-1" "accept"
    field "green" green `shouldBe` Just (Bool True)
    field "summary" green `shouldBe` Just (String "green in round 2 \183 local \183 bookings wired \183 not_found rewritten \183 cancel_booking__test.py has 1 pyright error")

  it "a refused re-conjure leads the pause, keeps its proposal to see, and offers it again" $ do
    ((refused, again, accepted), steps, _) <- cancelChecked $ \script ask resume -> do
      _ <- ask []
      liftIO (BSL.writeFile script "{}")
      liftIO (fillsBeside script (notFoundBody True))
      refused <- resume "conjure-1" "reconjure"
      liftIO (fillsBeside script (notFoundBody False))
      again <- ask ["resume" .= object ["token" .= ("conjure-1" :: Text), "choice" .= ("reconjure" :: Text), "arg" .= notFound]]
      _ <- resume "conjure-1" "reject"
      liftIO (fillsBeside script (notFoundBody True))
      _ <- resume "conjure-1" "reconjure"
      accepted <- resume "conjure-1" "accept"
      pure (refused, again, accepted)
    let p = field "paused" refused
        after = case inner "after" (inner "proposal" p) of Just (String t) -> t; _ -> ""
        badAt = length (takeWhile (not . ("    BAD" ==)) (T.lines after)) + 1
        reason = "it adds `\"BAD\" is not defined` at line " <> T.pack (show badAt)
    inner "outcome" p `shouldBe` Just (String ("Your re-conjure of not_found was refused: " <> reason <> "."))
    inner "diagnosis" p `shouldSatisfy` \case Just (Array xs) -> not (any (\case String x -> "re-conjure" `T.isInfixOf` x; _ -> False) (toList xs)); _ -> False
    offerLabels p `shouldBe` ["continue", "continue with a hint\8230", "see the refused proposal", "fix the test: not_found, again", "stop", "keep round 1"]
    inner "refused" (inner "proposal" p) `shouldBe` Just (String reason)
    after `shouldSatisfy` T.isInfixOf "    BAD\n"
    offerLabels (field "paused" again) `shouldBe` ["accept", "reject"]
    inner "outcome" (field "paused" again) `shouldBe` Just Aeson.Null
    -- accept lands the refused proposal as the person's choice: the conjure goes on over it
    accepted `shouldSatisfy` either (const False) (const True)
    steps `shouldSatisfy` elem ["conjure-reconjure-failed", notFound]
    steps `shouldSatisfy` elem ["conjure-resume", "accept"]

  it "each round's change is in the pause; keep lands the round it names" $ do
    ((second, refused, kept), _, _) <- cancelConjure $ \script ask resume -> do
      _ <- ask []
      liftIO (fillsBeside script "return NotFound(reason=\"x\")")
      second <- resume "conjure-1" "continue"
      let tok = case inner "token" (field "paused" second) of Just (String t) -> t; _ -> ""
      refused <- ask ["resume" .= object ["token" .= tok, "choice" .= ("stop" :: Text), "land" .= True, "arg" .= ("9" :: Text)]]
      kept <- ask ["resume" .= object ["token" .= tok, "choice" .= ("stop" :: Text), "land" .= True, "arg" .= ("1" :: Text)]]
      pure (second, refused, kept)
    let p = field "paused" second
        roundAt i k = case inner "rounds" p of
          Just (Array rs) | r : _ <- drop i (toList rs) -> inner k (Just r)
          _ -> Nothing
        files i = case roundAt i "files" of
          Just (Array fs) -> [f | Object o <- toList fs, Just (String f) <- [KM.lookup "file" o]]
          _ -> []
    roundAt 1 "diff" `shouldSatisfy` \case Just (String d) -> "+        return NotFound(reason=\"x\")" `elem` T.lines d; _ -> False
    roundAt 0 "diff" `shouldSatisfy` \case Just (String d) -> not ("NotFound(reason" `T.isInfixOf` d); _ -> False
    files 0 `shouldBe` [T.pack cbModule, T.pack cbTest]
    offerLabels p `shouldSatisfy` elem "keep round 2"
    refused `shouldBe` Left "there is no round 9 body to keep"
    field "summary" kept `shouldBe` Just (String "kept round 1 \183 red: not_found \183 bookings wired")
    field "edits" kept `shouldSatisfy` wroteIn cbModule "self.bookings = bookings"
    field "edits" kept `shouldSatisfy` (not . wroteIn cbModule "NotFound(reason=\"x\")")

  let resumeWith tok extra = ["resume" .= object (["token" .= (tok :: Text)] <> extra)]
      -- not_found's fake renamed: no rule names its port, and Jev (answering nothing) is unsure
      thingTest = T.replace "repository" "thing" morningTest

  it "bare stop lands nothing, even after the wire; keep lands round 1's body and the wire" $ do
    (stopped, stopSteps, _, (diskModule, diskTest)) <- cancelOnDisk (Just "wire") morningModule morningTest $ \_ ask _ -> do
      _ <- ask []
      ask (resumeWith "conjure-1" ["choice" .= ("stop" :: Text), "land" .= False])
    field "edits" stopped `shouldBe` Just Aeson.Null
    field "stopped" stopped `shouldBe` Just (Bool True)
    field "summary" stopped `shouldBe` Just (String "nothing landed")
    field "green" stopped `shouldBe` Just (Bool False)
    -- Review Focus 4: the module and the test file on disk are the morning ones, byte for byte
    (diskModule, diskTest) `shouldBe` (morningModule, morningTest)
    stopSteps `shouldSatisfy` elem ["conjure-resume", "stop"]
    stopSteps `shouldSatisfy` any (\fs -> take 2 fs == ["conjure", "person"] && "stopped \183 nothing landed" `elem` fs)
    (kept, keepSteps, _) <- cancelConjure $ \_ ask _ -> do
      _ <- ask []
      ask (resumeWith "conjure-1" ["choice" .= ("stop" :: Text), "land" .= True])
    field "summary" kept `shouldBe` Just (String "kept round 1 \183 red: not_found \183 bookings wired")
    field "stopped" kept `shouldBe` Just (Bool True)
    field "edits" kept `shouldSatisfy` wroteIn cbModule "self.bookings = bookings"
    field "edits" kept `shouldSatisfy` wroteIn cbTest "bookings: BookingRepository | None = None"
    keepSteps `shouldSatisfy` elem ["conjure-resume", "keep", "1"]
    keepSteps `shouldSatisfy` any (\fs -> take 2 fs == ["conjure", "person"] && any ("kept round 1 \183 red: test_cancel_booking_not_found" `T.isPrefixOf`) fs)

  it "keep 0 lands the wire alone, the person's body standing; with no wire it is refused" $ do
    ((kept, steps), _, _) <- cancelConjure $ \_ ask _ -> do
      _ <- ask []
      fills <- ask (resumeWith "conjure-1" ["choice" .= ("stop" :: Text), "land" .= True, "arg" .= ("0" :: Text)])
      pure (fills, ())
    field "summary" kept `shouldSatisfy` \case Just (String x) -> "kept the wire alone \183 red: not_found" `T.isPrefixOf` x && " \183 bookings wired" `T.isSuffixOf` x; _ -> False
    field "edits" kept `shouldSatisfy` wroteIn cbModule "self.bookings = bookings"
    field "edits" kept `shouldSatisfy` wroteIn cbTest "bookings: BookingRepository | None = None"
    steps `shouldBe` ()
    (refused, keepSteps, _) <- cancelWith Nothing morningModule morningTest $ \_ ask _ -> do
      _ <- ask []
      ask (resumeWith "conjure-1" ["choice" .= ("stop" :: Text), "land" .= True, "arg" .= ("0" :: Text)])
    refused `shouldBe` Left "there is no wire to keep"
    keepSteps `shouldSatisfy` notElem ["conjure-resume", "keep", "0"]

  it "keep 0 after a test fix lands the wire's files only, as the wire left them" $ do
    (kept, _, _, (diskModule, diskTest)) <- cancelOnDisk (Just "wire") morningModule morningTest $ \_ ask resume -> do
      _ <- ask []
      _ <- resume "conjure-1" "reconjure"
      -- the fix accepted, the script still red: the conjure pauses again, the fix in force
      accepted <- resume "conjure-1" "accept"
      let tok = case inner "token" (field "paused" accepted) of Just (String t) -> t; _ -> "conjure-1"
      ask (resumeWith tok ["choice" .= ("stop" :: Text), "land" .= True, "arg" .= ("0" :: Text)])
    field "summary" kept `shouldSatisfy` \case Just (String x) -> "kept the wire alone" `T.isPrefixOf` x && not ("rewritten" `T.isInfixOf` x); _ -> False
    field "edits" kept `shouldSatisfy` wroteIn cbModule "self.bookings = bookings"
    field "edits" kept `shouldSatisfy` wroteIn cbTest "bookings: BookingRepository | None = None"
    field "edits" kept `shouldSatisfy` (not . wroteIn cbTest "command(bookings=repository)")
    (diskModule, diskTest) `shouldBe` (morningModule, morningTest)

  it "a wire resume runs the offered wire; an arg not offered is refused and keeps the token" $ do
    ((paused, refused, wired), steps, asked) <- cancelWith (Just "wire") morningModule thingTest $ \_ ask _ -> do
      paused <- ask []
      refused <- ask (resumeWith "conjure-1" ["choice" .= ("wire" :: Text), "arg" .= ("x: Nope" :: Text)])
      wired <- ask (resumeWith "conjure-1" ["choice" .= ("wire" :: Text), "arg" .= ("bookings: BookingRepository" :: Text)])
      pure (paused, refused, wired)
    let wireArgs = [a | Just (Array os) <- [inner "offers" (field "paused" paused)], Object o <- toList os, Just (String "wire") <- [KM.lookup "choice" o], Just (String a) <- [KM.lookup "arg" o]]
    steps `shouldSatisfy` elem ["conjure-pause", "port"]
    asked `shouldSatisfy` elem "conjure-port"
    wireArgs `shouldBe` ["bookings: BookingRepository", "ids: IdGenerator"]
    offerLabels (field "paused" paused) `shouldSatisfy` elem "wire bookings: BookingRepository"
    inner "why" (field "paused" paused) `shouldBe` Just (String "not_found builds thing = Mock() for a port CancelBooking lacks; it could be bookings: BookingRepository or ids: IdGenerator.")
    refused `shouldBe` Left "this pause offers no wire for x: Nope"
    let after = dropWhile (/= ["conjure-resume", "wire"]) steps
    after `shouldSatisfy` (not . null)
    map (take 2) after `shouldSatisfy` elem ["conjure-wire", "wire"]
    wiredPorts (field "paused" wired) `shouldBe` [("bookings", "BookingRepository")]

  it "a reconjure arg not offered, and land as a string, are refused" $ do
    ((refused, badLand, continued), _, _) <- cancelConjure $ \_ ask _ -> do
      _ <- ask []
      refused <- ask (resumeWith "conjure-1" ["choice" .= ("reconjure" :: Text), "arg" .= (T.pack cbTest <> "::test_nope")])
      badLand <- ask (resumeWith "conjure-1" ["choice" .= ("stop" :: Text), "land" .= ("yes" :: Text)])
      continued <- ask (resumeWith "conjure-1" ["choice" .= ("continue" :: Text)])
      pure (refused, badLand, continued)
    refused `shouldBe` Left ("this pause offers no re-conjure of " <> T.pack cbTest <> "::test_nope")
    badLand `shouldSatisfy` either ("continue, climb, stop, reconjure, accept, reject or wire" `T.isInfixOf`) (const False)
    continued `shouldSatisfy` either (const False) (const True)

  -- a test conjure of a stub test appended to the test file (CHANGED on top), its fill
  -- BODY; WIRE the `[conjure] wire`; the answer and the step log's lines
  let stubTest = "\n\ndef test_lists_the_bookings() -> None:\n    ...\n"
      unpassedBody = "repository = InMemoryBookingRepository()\\nassert repository is not None"
      testConjured :: Maybe Text -> [(FilePath, Text)] -> IO (Either Text Value, [[Text]])
      testConjured = testConjuredWith unpassedBody (const "{}")
      -- the same with the stub's recorded BODY and the test SCRIPT given the stub's body line (1-based)
      testConjuredWith :: Text -> (Int -> BSL.ByteString) -> Maybe Text -> [(FilePath, Text)] -> IO (Either Text Value, [[Text]])
      testConjuredWith body scripted wire changed =
        withSystemTempDirectory "olifant-conjure-test-wire" $ \tmp -> do
          let script = tmp </> "tests.json"
              fills = tmp </> "fills.toml"
              answers = tmp </> "answers.json"
          testText <- (<> stubTest) . T.stripEnd <$> fixture testRel
          BSL.writeFile script (scripted (length (takeWhile (not . ("def test_lists_the_bookings(" `T.isPrefixOf`)) (T.lines testText)) + 2))
          TIO.writeFile fills ("[fills]\nbody = \"" <> body <> "\"\n")
          TIO.writeFile answers "{\"answers\": {}}"
          withPython ((testRel, testText) : changed) $ \ports dir -> do
            old <- TIO.readFile (dir </> ".olifant/config.toml")
            TIO.writeFile (dir </> ".olifant/config.toml") (old <> "\n[model]\ndefault = \"local\"\nrounds = 2\n[model.backends.local]\nkind = \"openai-compatible\"\nurl = \"http://127.0.0.1:9\"\nmodel = \"q\"\n[model.backends.jev]\nkind = \"jev\"\n[decide]\nmodel = \"jev\"\n" <> maybe "" (\w -> "[conjure]\nwire = \"" <> w <> "\"\n") wire)
            let config = [("test_script", String (T.pack script)), ("fills", String (T.pack fills)), ("decide_answers", String (T.pack answers))]
            (r, _) <- inProcessWithConfig config ports dir $ do
              doc <- openDoc testRel "python"
              text <- documentContents doc
              -- the stub's body line, 0-based
              let stubAt = length (takeWhile (not . ("def test_lists_the_bookings(" `T.isPrefixOf`)) (T.lines text)) + 1
              blockMove (Proxy :: Proxy "olifant/conjure") (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "position" .= object ["line" .= stubAt, "character" .= (0 :: Int)]])
            let stepLog = dir </> ".olifant/state/steps.log"
            found <- doesFileExist stepLog
            lines' <- if found then T.lines <$> TIO.readFile stepLog else pure []
            pure (r, [drop 1 (T.splitOn "\t" l) | l <- lines'])
      lists = T.pack testRel <> "::test_lists_the_bookings"

  it "a test conjure whose fill builds a fake it never passes wires the port and lands the wire with the test" $ do
    unwiredModule <- unwired <$> fixture moduleRel
    (landed, steps) <- testConjured (Just "wire") [(moduleRel, unwiredModule)]
    let edits = field "edits" landed
    edits `shouldSatisfy` wroteIn testRel "repository = InMemoryBookingRepository()"
    edits `shouldSatisfy` wroteIn testRel "bookings: BookingRepository | None = None"
    edits `shouldSatisfy` wroteIn moduleRel "self.bookings = bookings"
    -- the recorded fill repeats: the refilled test still builds its own fake
    field "summary" landed `shouldSatisfy` \case Just (String x) -> " \183 bookings wired; the test still builds its own fake, re-conjure it" `T.isSuffixOf` x; _ -> False
    let kinds = map (take 2) steps
    steps `shouldSatisfy` elem ["conjure-test", "flagged", lists, "repository", "InMemoryBookingRepository"]
    takeWhile (/= ["conjure-wire", "wire"]) kinds `shouldSatisfy` elem ["conjure-test", "flagged"]
    kinds `shouldSatisfy` elem ["conjure-wire", "wire"]

  it "a test conjure that passes its fakes is ok, nothing wired" $ do
    unwiredModule <- unwired <$> fixture moduleRel
    (landed, steps) <- testConjuredWith "return None" (const "{}") (Just "wire") [(moduleRel, unwiredModule)]
    field "edits" landed `shouldSatisfy` wroteIn testRel "return None"
    steps `shouldSatisfy` elem ["conjure-test", "ok", "-"]
    steps `shouldSatisfy` all (\fs -> take 1 fs /= ["conjure-wire"])

  it "without a wire generator the test lands and the summary names the port" $ do
    unwiredModule <- unwired <$> fixture moduleRel
    (landed, steps) <- testConjured Nothing [(moduleRel, unwiredModule)]
    field "edits" landed `shouldSatisfy` wroteIn testRel "repository = InMemoryBookingRepository()"
    field "summary" landed `shouldSatisfy` \case Just (String x) -> " \183 needs a port the class lacks: bookings: BookingRepository" `T.isSuffixOf` x; _ -> False
    steps `shouldSatisfy` elem ["conjure-test", "flagged", lists, "repository", "InMemoryBookingRepository"]
    steps `shouldSatisfy` all (\fs -> take 1 fs /= ["conjure-wire"])

  it "a re-fill that fails lands the wire with the test as first written" $ do
    -- red at the stub's body line: at its assertion before the wire, before it once the wire moves it down
    unwiredModule <- unwired <$> fixture moduleRel
    let redAt l = Aeson.encode (object [Key.fromText lists .= object ["line" .= (l + 1), "message" .= ("assert False" :: Text)]])
    (landed, steps) <- testConjuredWith "repository = InMemoryBookingRepository()\\nassert False" redAt (Just "wire") [(moduleRel, unwiredModule)]
    let edits = field "edits" landed
    edits `shouldSatisfy` wroteIn testRel "    assert False"
    edits `shouldSatisfy` wroteIn testRel "bookings: BookingRepository | None = None"
    edits `shouldSatisfy` wroteIn moduleRel "self.bookings = bookings"
    field "summary" landed `shouldSatisfy` \case Just (String x) -> " \183 bookings wired; the test still builds its own fake, re-conjure it" `T.isSuffixOf` x; _ -> False
    map (take 2) steps `shouldSatisfy` elem ["conjure-wire", "wire"]

-- | `olifant/mutate`: the unit at a position mutated against its test
-- file (a scripted runner that kills nothing); and a survivor's stub.
mutateSpec :: Spec
mutateSpec = describe "olifant/mutate" $ do
  let moduleRel = "src/bistro/booking/commands/create_booking.py"
      testRel = "src/bistro/booking/commands/create_booking__test.py"
  it "in a method: its lines and survivors, each line inside them, 1-based; outside any unit, why" $
    withSystemTempDirectory "olifant-mutate" $ \tmp -> do
      let script = tmp </> "tests.json"
      BSL.writeFile script "{}"
      withPython [] $ \ports dir -> do
        ((answer, outside), _) <- inProcessWithConfig [("test_script", String (T.pack script))] ports dir $ do
          doc <- openDoc moduleRel "python"
          text <- documentContents doc
          let handleAt = length (takeWhile (not . ("    def handle(" `T.isPrefixOf`)) (T.lines text))
              ask line = blockMove (Proxy :: Proxy "olifant/mutate") (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "position" .= object ["line" .= line, "character" .= (0 :: Int)]])
          answer <- ask (handleAt + 1)
          outside <- ask (0 :: Int)
          pure (answer, outside)
        let field k = either (const Nothing) (lookupKey k)
            int = \case Just (Number n) -> round n :: Int; _ -> -1
            span' = case field "lines" answer of
              Just (Object o) -> (int (KM.lookup "first" o), int (KM.lookup "last" o))
              _ -> (-1, -1)
            survivors = case field "survivors" answer of Just (Aeson.Array xs) -> [o | Object o <- toList xs]; _ -> []
        field "file" answer `shouldBe` Just (String (T.pack (dir </> moduleRel)))
        field "unit" answer `shouldBe` Just (String "handle")
        survivors `shouldSatisfy` (not . null)
        int (field "survived" answer) `shouldBe` length survivors
        int (field "killed" answer) `shouldBe` 0
        field "summary" answer `shouldSatisfy` \case Just (String s) -> "killed" `T.isInfixOf` s; _ -> False
        forM_ survivors $ \o -> do
          int (KM.lookup "line" o) `shouldSatisfy` \l -> l >= fst span' && l <= snd span'
          forM_ ["operator", "from", "to", "mutated"] $ \k -> KM.lookup k o `shouldSatisfy` \case Just (String _) -> True; _ -> False
          KM.lookup "dim" o `shouldBe` Just (Bool False)
        outside `shouldSatisfy` either ("no function or class at line 1" `T.isInfixOf`) (const False)
  it "olifant/stub with a survivor: the test named from the mutant" $
    withPython [] $ \ports dir -> do
      (answer, _) <- inProcessWithConfig [] ports dir $ do
        doc <- openDoc moduleRel "python"
        text <- documentContents doc
        let handleAt = length (takeWhile (not . ("    def handle(" `T.isPrefixOf`)) (T.lines text))
            survivor = object ["line" .= (handleAt + 2), "operator" .= ("boundary" :: Text), "from" .= ("seats > 2" :: Text), "to" .= ("seats >= 2" :: Text), "mutated" .= ("        return seats >= 2" :: Text)]
        blockMove (Proxy :: Proxy "olifant/stub") (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "position" .= object ["line" .= (handleAt + 1), "character" .= (0 :: Int)], "survivor" .= survivor])
      either (const Nothing) (lookupKey "name") answer `shouldBe` Just (String "test_boundary_seats")
      either (const Nothing) (lookupKey "file") answer `shouldBe` Just (String (T.pack (dir </> testRel)))
  it "olifant/stub with a malformed survivor: invalid params" $
    withPython [] $ \ports dir -> do
      (answer, _) <- inProcessWithConfig [] ports dir $ do
        doc <- openDoc moduleRel "python"
        blockMove (Proxy :: Proxy "olifant/stub") (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "position" .= object ["line" .= (0 :: Int), "character" .= (0 :: Int)], "survivor" .= object ["line" .= ("x" :: Text)]])
      answer `shouldSatisfy` either ("survivor" `T.isInfixOf`) (const False)
  it "olifant/waive: the survivor leaves the next mutate, which counts it; an empty reason is refused" $
    withSystemTempDirectory "olifant-mutate" $ \tmp -> do
      let script = tmp </> "tests.json"
      BSL.writeFile script "{}"
      withPython [] $ \ports dir -> do
        ((before, refused, waived, after), _) <- inProcessWithConfig [("test_script", String (T.pack script))] ports dir $ do
          doc <- openDoc moduleRel "python"
          text <- documentContents doc
          let handleAt = length (takeWhile (not . ("    def handle(" `T.isPrefixOf`)) (T.lines text))
              target = object ["uri" .= (doc ^. L.uri)]
              mutate = blockMove (Proxy :: Proxy "olifant/mutate") (object ["textDocument" .= target, "position" .= object ["line" .= (handleAt + 1), "character" .= (0 :: Int)]])
              waive survivor reason = blockMove (Proxy :: Proxy "olifant/waive") (object ["textDocument" .= target, "survivor" .= survivor, "reason" .= (reason :: Text)])
          before <- mutate
          let first' = case either (const Nothing) (lookupKey "survivors") before of
                Just (Array vs) | v : _ <- toList vs -> v
                _ -> Aeson.Null
          refused <- waive first' ""
          waived <- waive first' "n only gates the return"
          after <- mutate
          pure (before, refused, waived, after)
        let count k = either (const (-1)) (\a -> case lookupKey k a of Just (Number n) -> round n :: Int; _ -> -1)
        count "survived" before `shouldSatisfy` (> 0)
        refused `shouldSatisfy` either ("needs a reason" `T.isInfixOf`) (const False)
        waived `shouldSatisfy` either (const False) (const True)
        (count "survived" after, count "waived" after) `shouldBe` (count "survived" before - 1, 1)
        either (const "") (\a -> case lookupKey "summary" a of Just (String t) -> t; _ -> "") after `shouldSatisfy` T.isSuffixOf "1 waived"
  it "olifant/waive with no survivor: invalid params" $
    withPython [] $ \ports dir -> do
      (answer, _) <- inProcessWithConfig [] ports dir $ do
        doc <- openDoc moduleRel "python"
        blockMove (Proxy :: Proxy "olifant/waive") (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "reason" .= ("x" :: Text)])
      answer `shouldSatisfy` either ("survivor" `T.isInfixOf`) (const False)

-- | `olifant/tests/run`: a file's tests, run whole, with a scripted runner.
testsRunSpec :: Spec
testsRunSpec = describe "olifant/tests/run" $ do
  let testRel = "src/bistro/booking/commands/create_booking__test.py"
      moduleRel = "src/bistro/booking/commands/create_booking.py"
      red = T.pack testRel <> "::test_a_party_larger_than_the_table_is_refused"
      run doc = blockMove (Proxy :: Proxy "olifant/tests/run") (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)]])
  it "answers the test file's run: passed, failed with line and message, broken with why" $
    withSystemTempDirectory "olifant-tests" $ \tmp -> do
      let script = tmp </> "tests.json"
      BSL.writeFile script "{}"
      withPython [] $ \ports dir -> do
        ((passing, failing, broken), _) <- inProcessWithConfig [("test_script", String (T.pack script))] ports dir $ do
          doc <- openDoc moduleRel "python"
          passing <- run doc
          liftIO (BSL.writeFile script (Aeson.encode (object [Key.fromText red .= object ["line" .= (47 :: Int), "message" .= ("assert isinstance(result, PartyTooLarge)" :: Text)]])))
          failing <- run doc
          liftIO (removeFile script)
          broken <- run doc
          pure (passing, failing, broken)
        let field k = either (const Nothing) (lookupKey k)
        field "file" passing `shouldBe` Just (String (T.pack (dir </> testRel)))
        -- each test of the file by its id, as pytest reports them
        let ids = \case Just (Aeson.Array xs) -> [t | String t <- toList xs]; _ -> []
        ids (field "passed" passing) `shouldSatisfy` \ps -> length ps == 4 && red `elem` ps && all ((T.pack testRel <> "::") `T.isPrefixOf`) ps
        field "failed" passing `shouldBe` Just (toJSON ([] :: [Value]))
        field "broken" passing `shouldBe` Just Aeson.Null
        field "seconds" passing `shouldSatisfy` \case Just (Number _) -> True; _ -> False
        field "failed" failing `shouldBe` Just (toJSON [object ["id" .= red, "file" .= (dir </> testRel), "line" .= (47 :: Int), "message" .= ("assert isinstance(result, PartyTooLarge)" :: Text), "output" .= ("" :: Text)]])
        ids (field "passed" failing) `shouldBe` filter (/= red) (ids (field "passed" passing))
        field "broken" broken `shouldSatisfy` \case Just (String why) -> "the test script does not read" `T.isPrefixOf` why; _ -> False
  it "a position in a test runs that test; scope file the whole file; scope all the project's" $
    withSystemTempDirectory "olifant-tests" $ \tmp -> do
      let script = tmp </> "tests.json"
      BSL.writeFile script "{}"
      withPython [] $ \ports dir -> do
        ((one, whole, every), _) <- inProcessWithConfig [("test_script", String (T.pack script))] ports dir $ do
          doc <- openDoc testRel "python"
          text <- documentContents doc
          -- the red test's def line, 0-based
          let defLine = length (takeWhile (not . ("def test_a_party_larger_than_the_table_is_refused" `T.isInfixOf`)) (T.lines text))
              ask extra = blockMove (Proxy :: Proxy "olifant/tests/run") (object (["textDocument" .= object ["uri" .= (doc ^. L.uri)]] ++ extra))
          one <- ask ["position" .= object ["line" .= (defLine + 1), "character" .= (0 :: Int)]]
          whole <- ask ["position" .= object ["line" .= (defLine + 1), "character" .= (0 :: Int)], "scope" .= ("file" :: Text)]
          every <- ask ["scope" .= ("all" :: Text)]
          pure (one, whole, every)
        let field k = either (const Nothing) (lookupKey k)
            ids = \case Just (Aeson.Array xs) -> [t | String t <- toList xs]; _ -> []
        ids (field "passed" one) `shouldBe` [red]
        field "scope" one `shouldBe` Just (String "test")
        field "test" one `shouldBe` Just (String red)
        length (ids (field "passed" whole)) `shouldBe` 4
        field "scope" whole `shouldBe` Just (String "file")
        field "scope" every `shouldBe` Just (String "all")
        field "file" every `shouldBe` Just (String (T.pack dir))
  it "runs under $/progress titled testing" $
    withSystemTempDirectory "olifant-tests" $ \tmp -> do
      let script = tmp </> "tests.json"
      BSL.writeFile script "{}"
      withPython [] $ \ports dir -> do
        (titles, _) <- inProcessWithConfig [("test_script", String (T.pack script))] ports dir $ do
          doc <- openDoc moduleRel "python"
          rid <- sendRequest (SMethod_CustomMethod (Proxy :: Proxy "olifant/tests/run")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)]])
          let begun :: FromServerMessage -> Maybe Text
              begun = \case
                FromServerMess SMethod_Progress n | Just (String t) <- lookupKey "title" (n ^. L.params . L.value) -> Just t
                _ -> Nothing
              await seen = (responseForId (SMethod_CustomMethod (Proxy :: Proxy "olifant/tests/run")) rid >> pure seen) <|> (satisfyMaybe begun >>= \t -> await (t : seen)) <|> (anyMessage >> await seen)
          await []
        titles `shouldBe` ["testing"]
  it "answers a run the editor cancels as cancelled, its runner stopped" $ do
    entered <- newEmptyMVar
    stopped <- newEmptyMVar
    -- a runner that runs until its token trips (as pytest killed would)
    let endless token = Runner $ \_ _ _ -> do
          _ <- tryPutMVar entered ()
          let wait = cancelled token >>= \case
                True -> putMVar stopped () >> pure (Left "cancelled")
                False -> threadDelay 20000 >> wait
          wait
    withPython [] $ \ports dir -> do
      ((answer, seen), _) <- inProcessWithConfig [] ports {pTester = endless} dir $ do
        doc <- openDoc moduleRel "python"
        rid <- sendRequest (SMethod_CustomMethod (Proxy :: Proxy "olifant/tests/run")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)]])
        liftIO (timeout 30000000 (takeMVar entered)) >>= maybe (fail "the run never began") pure
        sendNotification SMethod_CancelRequest (CancelParams (case rid of IdInt n -> InL n; IdString t -> InR t))
        seen <- liftIO (timeout 10000000 (takeMVar stopped))
        let awaitRun = responseForId (SMethod_CustomMethod (Proxy :: Proxy "olifant/tests/run")) rid <|> (anyMessage >> awaitRun)
        TResponseMessage _ _ r <- awaitRun
        pure (either (Just . (^. L.code)) (const Nothing) r, seen)
      seen `shouldBe` Just ()
      answer `shouldBe` Just (InL LSPErrorCodes_RequestCancelled)
  it "refuses a module with no test file yet, in one sentence" $
    withPython [("src/bistro/lonely.py", "def alone() -> int:\n    return 1\n")] $ \ports dir -> do
      (answer, _) <- inProcessWithConfig [] ports dir (openDoc "src/bistro/lonely.py" "python" >>= run)
      answer `shouldBe` Left "no test file for this module yet: src/bistro/lonely__test.py"
  it "olifant/tests/file answers a module's test file, and a test file's module, as JSON" $
    withPython [] $ \ports dir -> do
      ((toTest, toModule), _) <- inProcessWithConfig [] ports dir $ do
        let jump doc = blockMove (Proxy :: Proxy "olifant/tests/file") (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)]])
        toTest <- openDoc moduleRel "python" >>= jump
        toModule <- openDoc testRel "python" >>= jump
        pure (toTest, toModule)
      toTest `shouldBe` Right (object ["file" .= (dir </> testRel), "kind" .= ("test" :: Text), "exists" .= True])
      toModule `shouldBe` Right (object ["file" .= (dir </> moduleRel), "kind" .= ("module" :: Text), "exists" .= True])

-- | A plan's anchors (plans, stage 1): diagnosed over the editor's text,
-- jumped to, listed, completed.
plansSpec :: Spec
plansSpec = describe "plans: anchors in the server" $ do
  let schedule = "src/bistro/booking/queries/get_day_schedule.py"
      planText = T.unlines ["- handles a day @" <> T.pack schedule <> ":GetDaySchedule.handle", "- gone @" <> T.pack schedule <> ":Gone"]
      docOf d = "textDocument" .= object ["uri" .= (d ^. L.uri)]
  it "gd on a plan's anchor goes to its unit's def line; elsewhere it stays quiet" $
    withPython [("plans/t.md", planText), ("src/x.py", "x = 1\n")] $ \ports dir -> do
      ((on, justAfter, off, gone, code), _) <- inProcessWith ports dir $ do
        plan <- openDoc "plans/t.md" "markdown"
        py <- openDoc "src/x.py" "python"
        let at d l c = getDefinitions d (Position l c)
            width = fromIntegral (T.length ("- handles a day @" <> T.pack schedule <> ":GetDaySchedule.handle"))
        on <- at plan 0 30
        justAfter <- at plan 0 width
        off <- at plan 0 3
        gone <- at plan 1 20
        code <- at py 0 0
        pure (on, justAfter, off, gone, code)
      let spots = \case
            InL (Definition (InL (Location u r))) -> [(uriToFilePath u, r ^. L.start . L.line, r ^. L.start . L.character)]
            InL (Definition (InR ls)) -> [(uriToFilePath u, r ^. L.start . L.line, r ^. L.start . L.character) | Location u r <- ls]
            _ -> []
          expected = [(Just (dir </> schedule), 21, 0)]
      spots on `shouldBe` expected
      spots justAfter `shouldBe` expected
      spots off `shouldBe` []
      spots gone `shouldBe` []
      spots code `shouldBe` []
  it "a plan's missing anchor is a diagnostic; olifant/plans/anchor jumps; olifant/plans/files lists" $
    withPython [("plans/t.md", planText)] $ \ports dir -> do
      ((before, jump, missing, bare, files, after), _) <- inProcessWith ports dir $ do
        plan <- openDoc "plans/t.md" "markdown"
        before <- pullDiagnostics plan
        jump <- blockMove (Proxy :: Proxy "olifant/plans/anchor") (object [docOf plan, "line" .= (0 :: Int)])
        missing <- blockMove (Proxy :: Proxy "olifant/plans/anchor") (object [docOf plan, "line" .= (1 :: Int)])
        bare <- blockMove (Proxy :: Proxy "olifant/plans/anchor") (object [docOf plan, "line" .= (5 :: Int)])
        files <- blockMove (Proxy :: Proxy "olifant/plans/files") (object [docOf plan])
        -- the editor's text, not disk's: line 1 now holds a real anchor, the missing one moves to line 2
        changeDoc plan [TextDocumentContentChangeEvent (InL (TextDocumentContentChangePartial (Range (Position 1 0) (Position 1 0)) Nothing ("- table @src/bistro/booking/models/table.py:Table\n")))]
        after <- pullDiagnostics plan
        pure (before, jump, missing, bare, files, after)
      map (\d -> (d ^. L.message, d ^. L.range . L.start . L.line)) before `shouldBe` [("? not found", 1)]
      fmap (lookupKey "found") jump `shouldBe` Right (Just (Bool True))
      fmap (lookupKey "line") jump `shouldBe` Right (Just (Number 22))
      fmap (fmap (\case String f -> (T.pack schedule) `T.isSuffixOf` f; _ -> False) . lookupKey "file") jump `shouldBe` Right (Just True)
      fmap (\v -> (lookupKey "found" v, lookupKey "line" v, lookupKey "file" v)) missing `shouldBe` Right (Just (Bool False), Just (Number 1), Just (String (T.pack (dir </> schedule))))
      bare `shouldBe` Left "no anchor on this line"
      fmap (fmap (\case Array xs -> length xs; _ -> 0) . lookupKey "items") files `shouldBe` Right (Just 2)
      map (\d -> (d ^. L.message, d ^. L.range . L.start . L.line)) after `shouldBe` [("? not found", 2)]
  it "a plan completes files after @ and units after @path:" $
    withPython [("plans/t.md", "- x @src/bistro/booking/queries/get_day_schedule.\n- y @" <> T.pack schedule <> ":GetDaySchedule.h\n")] $ \ports dir -> do
      ((paths, units), _) <- inProcessWith ports dir $ do
        plan <- openDoc "plans/t.md" "markdown"
        let complete l c = request SMethod_TextDocumentCompletion (CompletionParams plan (Position l c) Nothing Nothing Nothing) <&> \case
              TResponseMessage _ _ (Right (InR (InL (CompletionList True _ items)))) -> map (^. L.label) items
              _ -> []
        (,) <$> complete 0 (fromIntegral (T.length "- x @src/bistro/booking/queries/get_day_schedule.")) <*> complete 1 (fromIntegral (T.length ("- y @" <> T.pack schedule <> ":GetDaySchedule.h")))
      paths `shouldBe` ["src/bistro/booking/queries/get_day_schedule.py", "src/bistro/booking/queries/get_day_schedule__test.py"]
      units `shouldBe` ["GetDaySchedule.handle"]
  it "a plan offers a new step on a blank line (snippet) and as a code action under the cursor's line" $
    withPython [("plans/t.md", "# P\n- [ ] one @a.py:A\n\n")] $ \ports dir -> do
      ((snips, titles, edits), _) <- inProcessWith ports dir $ do
        plan <- openDoc "plans/t.md" "markdown"
        items <- request SMethod_TextDocumentCompletion (CompletionParams plan (Position 2 0) Nothing Nothing Nothing) <&> \case
          TResponseMessage _ _ (Right (InR (InL (CompletionList _ _ is)))) -> is
          _ -> []
        acts <- getCodeActions plan (Range (Position 1 3) (Position 1 3))
        let news = [a | InR a <- acts, a ^. L.kind == Just CodeActionKind_Source]
        pure
          ( [(i ^. L.insertText, i ^. L.insertTextFormat) | i <- items]
          , map (^. L.title) news
          , [concat (M.elems cs) | a <- news, Just (WorkspaceEdit (Just cs) _ _) <- [a ^. L.edit], let _ = a]
          )
      snips `shouldBe` [(Just "- [ ] ${1:step} @${2:anchor}", Just InsertTextFormat_Snippet)]
      titles `shouldBe` ["New step below"]
      map (map (\e -> (e ^. L.range . L.start, e ^. L.newText))) edits `shouldBe` [[(Position 1 17, "\n- [ ] step @anchor")]]

-- | Plan errands (plans, stage 1, Task 7): ask, validate and do run the
-- runner (the shim here, never claude) and answer ghost blocks.
plansLoopSpec :: Spec
plansLoopSpec = describe "plans, stage 2" $ do
  let table = "src/bistro/booking/models/table.py"
      plan = "- [ ] s @" <> T.pack table <> ":label\n  - test: t @tests/test_table.py:test_label \9679\n"
      jev = "\n[decide]\nmodel = \"jev\"\n\n[model.backends.jev]\nkind = \"jev\"\n"
      docOf d = "textDocument" .= object ["uri" .= (d ^. L.uri)]
      reconcileP = Proxy :: Proxy "olifant/plans/reconcile"
      anchorP = Proxy :: Proxy "olifant/plans/anchor"
      stubP = Proxy :: Proxy "olifant/plans/stub"
      testP = Proxy :: Proxy "olifant/plans/test"
      -- the fixture, the plan and an edit of the anchored unit, committed first
      tableText = T.unlines ["def label(id: str) -> str:", "    return id"]
      withEdited = withEditedPlan plan
      withEditedPlan text config act = withPython [("plans/t.md", text), (table, tableText)] $ \ports dir -> do
        TIO.appendFile (dir </> ".olifant/config.toml") config
        let git args = callProcess "git" (["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t"] ++ args)
        git ["init", "-q", "-b", "main"]
        git ["add", "."]
        git ["commit", "-q", "-m", "the fixture"]
        old <- TIO.readFile (dir </> table)
        TIO.writeFile (dir </> table) (T.replace "return id" "return \"t-\" + id" old)
        act ports dir
      answers :: FilePath -> [(Text, Text)] -> IO [(Key.Key, Value)]
      answers dir kvs = do
        let f = dir </> "answers.json"
        TIO.writeFile f (TE.decodeUtf8 (BSL.toStrict (Aeson.encode (object ["answers" .= object [Key.fromText k .= object ["choice" .= v, "confidence" .= (0.9 :: Double)] | (k, v) <- kvs]]))))
        pure [("decide_answers", String (T.pack f))]
      arr = \case
        Just (Array xs) -> toList xs
        _ -> []
      reviewP = Proxy :: Proxy "olifant/plans/review"
      reviewsP = Proxy :: Proxy "olifant/plans/reviews"
      chooseP = Proxy :: Proxy "olifant/plans/review/choose"
      at ks v = foldl (\m k -> m >>= lookupKey k) (Just v) ks
      toE = \case InL x -> Left x; InR y -> Right y
  it "(a) reconcile lands its facts by workspace/applyEdit at the version it read; its answer carries none, and no blocks" $
    withEdited jev $ \ports dir -> do
      cfg <- answers dir [("plan-state", "in_progress")]
      ((edit, answer), _) <- inProcessWithConfig cfg ports dir $ do
        p <- openDoc "plans/t.md" "markdown"
        let method = SMethod_CustomMethod reconcileP
        rid <- sendRequest method (object [docOf p])
        let edited = message SMethod_WorkspaceApplyEdit <|> (anyMessage >> edited)
            answerOf = responseForId method rid <|> (anyMessage >> answerOf)
        TRequestMessage _ _ _ params <- edited
        TResponseMessage _ _ r <- answerOf
        pure (params ^. L.edit, either (Left . (^. L.message)) Right r)
      v <- either (fail . T.unpack) pure answer
      arr (lookupKey "facts" v) `shouldBe` []
      lookupKey "blocks" v `shouldBe` Nothing
      lookupKey "landed" v `shouldBe` Just (Number 1)
      [(te ^. L.textDocument . L.version, [either (^. L.newText) (^. L.newText) (toE t) | t <- te ^. L.edits]) | Just cs <- [edit ^. L.documentChanges], InL te <- cs]
        `shouldSatisfy` \case [(InL 0, texts)] -> any (T.isInfixOf "\9675") texts; _ -> False
  it "(a2) a second reconcile of the unchanged plan answers from the cache: one record, the same id" $
    withEdited jev $ \ports dir -> do
      cfg <- answers dir [("plan-state", "in_progress")]
      ((first, second), _) <- inProcessWithConfig cfg ports dir $ do
        p <- openDoc "plans/t.md" "markdown"
        (,) <$> blockMove reconcileP (object [docOf p]) <*> blockMove reconcileP (object [docOf p])
      v1 <- either (fail . T.unpack) pure first
      v2 <- either (fail . T.unpack) pure second
      lookupKey "id" v2 `shouldBe` lookupKey "id" v1
      (rs, _) <- readRecords . fromMaybe "" <$> readText (diskWorkspace dir) (dir </> ".olifant/state/errands.jsonl")
      length [() | AReconcile _ <- rs] `shouldBe` 1
  describe "excerpts" $ do
    let anchorOf = "@" <> T.pack table <> ":label"
        stepLine box = "- " <> box <> " s " <> anchorOf
        fenceOf body = ["  ```python " <> anchorOf] ++ map ("  " <>) body ++ ["  ```"]
        sourceActions p l = getCodeActions p (Range (Position l 3) (Position l 3)) <&> \acts -> [a | InR a <- acts, a ^. L.title == "Show the code here"]
        editsOf a = [concat (M.elems cs) | Just (WorkspaceEdit (Just cs) _ _) <- [a ^. L.edit]]
    it "(x1) hover on a step's anchor shows its unit's code; off the anchor, nothing" $
      withEditedPlan (T.unlines [stepLine "[ ]"]) "" $ \ports dir -> do
        ((on, off), _) <- inProcessWith ports dir $ do
          p <- openDoc "plans/t.md" "markdown"
          (,) <$> getHover p (Position 0 12) <*> getHover p (Position 0 2)
        case on of
          Just h | InL (MarkupContent _ text) <- h ^. L.contents ->
            text `shouldBe` T.intercalate "\n" ["`" <> T.pack table <> ":label` · lines 1–2", "", "```python", "def label(id: str) -> str:", "    return \"t-\" + id", "```"]
          other -> expectationFailure ("hover: " <> show other)
        off `shouldBe` Nothing
    it "(x2) Show the code here on an open anchored step writes the fence below it; not on a done step, nor on one that has one" $
      withEditedPlan (T.unlines ([stepLine "[ ]", stepLine "[x]", stepLine "[ ]"] ++ fenceOf ["x"])) "" $ \ports dir -> do
        ((open', done, has), _) <- inProcessWith ports dir $ do
          p <- openDoc "plans/t.md" "markdown"
          (,,) <$> sourceActions p 0 <*> sourceActions p 1 <*> sourceActions p 2
        map (map (map (\e -> (e ^. L.range . L.start, e ^. L.newText))) . editsOf) open'
          `shouldBe` [[[(Position 0 (fromIntegral (T.length (stepLine "[ ]"))), T.concat ["\n" <> l | l <- fenceOf ["def label(id: str) -> str:", "    return \"t-\" + id"]])]]]
        (length done, length has) `shouldBe` (0, 0)
    it "(x3) reconcile after the unit changed: one applyEdit with the new code; a second reconcile, none" $
      withEditedPlan (T.unlines ([stepLine "[ ]"] ++ fenceOf ["def label(id: str) -> str:", "    return id"])) jev $ \ports dir -> do
        cfg <- answers dir [("plan-state", "in_progress")]
        ((text, edits), _) <- inProcessWithConfig cfg ports dir $ do
          p <- openDoc "plans/t.md" "markdown"
          let edited = message SMethod_WorkspaceApplyEdit <|> (anyMessage >> edited)
          rid <- sendRequest (SMethod_CustomMethod reconcileP) (object [docOf p])
          _ <- edited
          let answerOf = responseForId (SMethod_CustomMethod reconcileP) rid <|> (anyMessage >> answerOf)
          _ <- answerOf
          second <- blockMove reconcileP (object [docOf p])
          (,) <$> documentContents p <*> pure (fmap (lookupKey "landed") second)
        text `shouldBe` T.unlines ([stepLine "[ ]"] ++ fenceOf ["def label(id: str) -> str:", "    return \"t-\" + id"])
        edits `shouldBe` Right (Just (Number 0))
    it "(x4) a moved unit: the excerpt follows, in the same edit as the step's anchor and its journal line" $
      withPython [("plans/t.md", T.unlines ([stepLine "[ ]"] ++ fenceOf ["def label(id: str) -> str:", "    return id"])), (table, tableText)] $ \ports dir -> do
        TIO.appendFile (dir </> ".olifant/config.toml") jev
        let git args = callProcess "git" (["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t"] ++ args)
            moved = "src/bistro/booking/models/labels.py"
        git ["init", "-q", "-b", "main"]
        git ["add", "."]
        git ["commit", "-q", "-m", "the fixture"]
        TIO.writeFile (dir </> table) ""
        TIO.writeFile (dir </> moved) tableText
        cfg <- answers dir [("plan-state", "in_progress"), ("plan-step", "none")]
        ((edit, text), _) <- inProcessWithConfig cfg ports dir $ do
          p <- openDoc "plans/t.md" "markdown"
          let edited = message SMethod_WorkspaceApplyEdit <|> (anyMessage >> edited)
          rid <- sendRequest (SMethod_CustomMethod reconcileP) (object [docOf p])
          TRequestMessage _ _ _ params <- edited
          let answerOf = responseForId (SMethod_CustomMethod reconcileP) rid <|> (anyMessage >> answerOf)
          _ <- answerOf
          (params ^. L.edit,) <$> documentContents p
        let ls = T.lines text
            new = "@" <> T.pack moved <> ":label"
            sent = T.concat [either (^. L.newText) (^. L.newText) (toE t) | Just cs <- [edit ^. L.documentChanges], InL te <- cs, t <- te ^. L.edits]
        take 2 ls `shouldBe` ["- [ ] s " <> new, "  ```python " <> new]
        drop 5 ls `shouldSatisfy` \case [j] -> "moved: now " `T.isInfixOf` j; _ -> False
        -- one edit: the anchor, the excerpt and the journal line
        sent `shouldSatisfy` \t -> ("```python " <> new) `T.isInfixOf` t && "moved: now " `T.isInfixOf` t
  it "(f) a reading is the reconcile review: listed, drawn, a lens on its step and the title; apply returns the plan edit and answers it" $
    withEdited jev $ \ports dir -> do
      cfg <- answers dir [("plan-state", "done")]
      ((listed, view, lenses, chosen, after), _) <- inProcessWithConfig cfg ports dir $ do
        p <- openDoc "plans/t.md" "markdown"
        _ <- blockMove reconcileP (object [docOf p])
        listed <- blockMove reviewsP (object [docOf p])
        view <- blockMove reviewP (object [docOf p, "id" .= ("reconcile" :: Text)])
        lenses <- getCodeLenses p
        let entry = either (const Aeson.Null) (\w -> fromMaybe Aeson.Null (at ["entries"] w >>= \case Array xs -> lookupKey "id" =<< listToMaybe (toList xs); _ -> Nothing)) view
        chosen <- blockMove chooseP (object [docOf p, "id" .= ("reconcile" :: Text), "entry" .= entry, "choice" .= ("apply" :: Text)])
        (listed, view, lenses, chosen,) <$> blockMove reviewsP (object [docOf p])
      l <- either (fail . T.unpack) pure listed
      [(lookupKey "id" x, map (\e -> (lookupKey "kind" e, lookupKey "row" e)) (arr (lookupKey "entries" x))) | x <- arr (lookupKey "reviews" l)]
        `shouldBe` [(Just (String "reconcile"), [(Just (String "done"), Just (Number 0))])]
      w <- either (fail . T.unpack) pure view
      [(lookupKey "summary" e, map (lookupKey "key") (arr (lookupKey "offers" e))) | e <- arr (at ["entries"] w)]
        `shouldBe` [(Just (String "Step 1 looks done"), map (Just . String) ["a", "r"])]
      sort [(c ^. L.range . L.start . L.line, fmap (^. L.title) (c ^. L.command)) | c <- lenses] `shouldBe` [(0, Just "1 to review"), (0, Just "looks done \183 review")]
      c <- either (fail . T.unpack) pure chosen
      lookupKey "done" c `shouldBe` Just (Bool True)
      Aeson.encode (lookupKey "edit" c) `shouldSatisfy` \e -> ("- [x] s @" <> TE.encodeUtf8 (T.pack table) <> ":label") `BS.isInfixOf` BSL.toStrict e
      fmap (arr . lookupKey "reviews") after `shouldBe` Right []
      (rs, _) <- readRecords . fromMaybe "" <$> readText (diskWorkspace dir) (dir </> ".olifant/state/errands.jsonl")
      [(rcId r == orId o, orBlock o, orOutcome o) | AReconcile r <- rs, AnOutcome o <- rs] `shouldBe` [(True, 0, "accepted")]
  it "(g) ignore on a unit in no step: the next reconcile does not offer it again" $
    withEditedPlan "# T\n\n- [ ] s\n" jev $ \ports dir -> do
      cfg <- answers dir [("plan-step", "none")]
      ((view, chosen, after), _) <- inProcessWithConfig cfg ports dir $ do
        p <- openDoc "plans/t.md" "markdown"
        _ <- blockMove reconcileP (object [docOf p])
        view <- blockMove reviewP (object [docOf p, "id" .= ("reconcile" :: Text)])
        chosen <- blockMove chooseP (object [docOf p, "id" .= ("reconcile" :: Text), "entry" .= ("unplaced|" <> T.pack table <> "|label|" :: Text), "choice" .= ("ignore" :: Text)])
        _ <- blockMove reconcileP (object [docOf p, "forced" .= True])
        (view, chosen,) <$> blockMove reviewsP (object [docOf p])
      w <- either (fail . T.unpack) pure view
      [(lookupKey "summary" e, map (lookupKey "key") (arr (lookupKey "offers" e))) | e <- arr (at ["entries"] w)]
        `shouldBe` [(Just (String "Not in any step: `label` in table.py"), map (Just . String) ["a", "1", "n", "i"])]
      -- the entry's id names the unit at its fingerprint
      [lookupKey "id" e | e <- arr (at ["entries"] w)] `shouldSatisfy` \case [Just (String k)] -> ("unplaced|" <> T.pack table <> "|label|") `T.isPrefixOf` k; _ -> False
      chosen `shouldSatisfy` either (const True) (const False) -- not the full key: no such reading
      fmap (arr . lookupKey "reviews") after `shouldSatisfy` either (const False) (not . null)
  it "(g2) ignore by the entry's id: answered, and the next reconcile offers nothing" $
    withEditedPlan "# T\n\n- [ ] s\n" jev $ \ports dir -> do
      cfg <- answers dir [("plan-step", "none")]
      ((chosen, after), _) <- inProcessWithConfig cfg ports dir $ do
        p <- openDoc "plans/t.md" "markdown"
        _ <- blockMove reconcileP (object [docOf p])
        view <- blockMove reviewP (object [docOf p, "id" .= ("reconcile" :: Text)])
        let entry = either (const Aeson.Null) (\w -> fromMaybe Aeson.Null (at ["entries"] w >>= \case Array xs -> lookupKey "id" =<< listToMaybe (toList xs); _ -> Nothing)) view
        chosen <- blockMove chooseP (object [docOf p, "id" .= ("reconcile" :: Text), "entry" .= entry, "choice" .= ("ignore" :: Text)])
        _ <- blockMove reconcileP (object [docOf p, "forced" .= True])
        (chosen,) <$> blockMove reviewsP (object [docOf p])
      c <- either (fail . T.unpack) pure chosen
      (lookupKey "done" c, lookupKey "edit" c) `shouldBe` (Just (Bool True), Just Aeson.Null)
      fmap (arr . lookupKey "reviews") after `shouldBe` Right []
  it "(h) a claim whose anchor resolves to a new fingerprint offers \"Re-fingerprint this claim\"" $
    withEditedPlan ("# T\n\n- a claim @" <> T.pack table <> ":label \10003 0000000\n") "" $ \ports dir -> do
      (acts, _) <- inProcessWith ports dir $ do
        p <- openDoc "plans/t.md" "markdown"
        getCodeActions p (Range (Position 2 3) (Position 2 3))
      let refp = [a | InR a <- acts, a ^. L.title == "Re-fingerprint this claim"]
      [T.concat (map (^. L.newText) (concat (M.elems cs))) | a <- refp, Just (WorkspaceEdit (Just cs) _ _) <- [a ^. L.edit]]
        `shouldSatisfy` \case [t] -> ("@" <> T.pack table <> ":label \10003 ") `T.isInfixOf` t && not ("0000000" `T.isInfixOf` t); _ -> False
  it "(b) cancel stops it, mid-call" $ do
    let script = "import socket,time\ns=socket.socket()\ns.bind(('127.0.0.1',0))\ns.listen(1)\nprint(s.getsockname()[1],flush=True)\nc,_=s.accept()\nprint('accepted',flush=True)\ntime.sleep(60)\n"
        hanging act = bracket (createProcess (proc "python3" ["-I", "-c", script]) {std_out = CreatePipe}) (\(_, _, _, h) -> terminateProcess h) $ \case
          (_, Just out, _, _) -> hGetLine out >>= \port -> act (T.strip (T.pack port)) (timeout 30000000 (hGetLine out))
          _ -> fail "no server"
    hanging $ \port accepted -> do
      let config = jev <> T.unlines ["[model.backends.local]", "kind = \"openai-compatible\"", "url = \"http://127.0.0.1:" <> port <> "/v1\"", "model = \"x\"", "escalate = \"auto\"", "[plans]", "ladder = [\"local\"]"]
          tidy = "# T\n\n## Tables\n\n- [ ] tidy the helper\n"
      withPython [("plans/t.md", tidy), (table, tableText)] $ \ports dir -> do
        TIO.appendFile (dir </> ".olifant/config.toml") config
        let git args = callProcess "git" (["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t"] ++ args)
        git ["init", "-q", "-b", "main"]
        git ["add", "."]
        git ["commit", "-q", "-m", "the fixture"]
        old <- TIO.readFile (dir </> table)
        TIO.writeFile (dir </> table) (T.replace "return id" "return \"t-\" + id" old)
        cfg <- answers dir [("plan-step", "none")]
        ((code, took), _) <- inProcessWithConfig cfg ports dir $ do
          p <- openDoc "plans/t.md" "markdown"
          let method = SMethod_CustomMethod reconcileP
          rid <- sendRequest method (object [docOf p])
          -- the words call has reached the server
          liftIO (accepted >>= (`shouldBe` Just "accepted"))
          start <- liftIO getCurrentTime
          sendNotification SMethod_CancelRequest (CancelParams (case rid of IdInt n -> InL n; IdString t -> InR t))
          let await = responseForId method rid <|> (anyMessage >> await)
          TResponseMessage _ _ r <- await
          end <- liftIO getCurrentTime
          pure (either (Just . (^. L.code)) (const Nothing) r, diffUTCTime end start)
        code `shouldBe` Just (InL LSPErrorCodes_RequestCancelled)
        took `shouldSatisfy` (< 5)
  it "(c) plans/anchor answers the fingerprint of the unit's lines" $
    withEdited "" $ \ports dir -> do
      (answer, _) <- inProcessWith ports dir $ do
        p <- openDoc "plans/t.md" "markdown"
        blockMove anchorP (object [docOf p, "line" .= (0 :: Int)])
      v <- either (fail . T.unpack) pure answer
      text <- TIO.readFile (dir </> table)
      -- label is two lines: its def and its return
      let Just start = lookupKey "line" v >>= \case Number n -> Just (round n :: Int); _ -> Nothing
      lookupKey "fp" v `shouldSatisfy` \case Just (String fp) -> T.length fp == 7 && T.all (`elem` ("0123456789abcdef" :: String)) fp; _ -> False
      lookupKey "fp" v `shouldBe` Just (String (Un.fingerprint text (start, start + 1)))

  it "(d) plans/stub answers the stub and its test child as a fact" $
    withEdited "" $ \ports dir -> do
      (answer, _) <- inProcessWith ports dir $ do
        p <- openDoc "plans/t.md" "markdown"
        blockMove stubP (object [docOf p, "line" .= (0 :: Int)])
      v <- either (fail . T.unpack) pure answer
      case v of
        Object o -> sort (KM.keys o) `shouldBe` ["edits", "facts", "file", "line", "name"]
        _ -> expectationFailure ("not an object: " <> show v)
      lookupKey "name" v `shouldBe` Just (String "test_s")
      lookupKey "file" v `shouldSatisfy` \case Just (String f) -> "src/bistro/booking/models/table__test.py" `T.isSuffixOf` f; _ -> False
      arr (lookupKey "facts" v) `shouldBe` [object ["after" .= (1 :: Int), "lines" .= ["  - test: s @src/bistro/booking/models/table__test.py:test_s \9675" :: Text]]]
  it "(e2) plans/test on a plan edited while its tests ran: the journal line comes by its own edit, not with the facts" $ do
    let green = "- [ ] s @" <> T.pack table <> ":Table\n  - test: imports @src/bistro/main__test.py:test_importing_main_opens_no_database \9675\n"
    withPythonChecked [("plans/t.md", green)] $ \ports dir -> do
      ((answer, text), _) <- inProcessWith ports {pTester = (`processRunner` 120)} dir $ do
        p <- openDoc "plans/t.md" "markdown"
        let method = SMethod_CustomMethod testP
        i <- sendRequest method (object [docOf p, "line" .= (0 :: Int)])
        -- the edit after the server took the plan's version (its progress has begun)
        let begun = (() <$ message SMethod_WindowWorkDoneProgressCreate) <|> (anyMessage >> begun)
        begun
        _ <- applyEdit p (TextEdit (Range (Position 2 0) (Position 2 0)) "some words\n")
        let answerOf = responseForId method i <|> (anyMessage >> answerOf)
            applied = (() <$ message SMethod_WorkspaceApplyEdit) <|> (anyMessage >> applied)
        TResponseMessage _ _ r <- answerOf
        applied
        (r,) <$> documentContents p
      v <- either (fail . show) pure answer
      [x | f <- arr (lookupKey "facts" (toJSON v)), String x <- arr (lookupKey "lines" f)] `shouldBe` []
      text `shouldSatisfy` T.isInfixOf "ticked: tests green"
  it "(e) plans/test runs a step's tests: passed, failed, the facts that tick it" $ do
    let green = "- [ ] s @" <> T.pack table <> ":Table\n  - test: imports @src/bistro/main__test.py:test_importing_main_opens_no_database \9675\n"
    withPythonChecked [("plans/t.md", green)] $ \ports dir -> do
      (answer, _) <- inProcessWith ports {pTester = (`processRunner` 120)} dir $ do
        p <- openDoc "plans/t.md" "markdown"
        blockMove testP (object [docOf p, "line" .= (0 :: Int)])
      v <- either (fail . T.unpack) pure answer
      case v of
        Object o -> sort (KM.keys o) `shouldBe` ["facts", "failed", "passed", "seconds"]
        _ -> expectationFailure ("not an object: " <> show v)
      (lookupKey "passed" v, lookupKey "failed" v) `shouldBe` (Just (Number 1), Just (Number 0))
      case arr (lookupKey "facts" v) of
        [child, step, journalled] -> do
          -- the tick's journal line rides with the facts
          [x | String x <- arr (lookupKey "lines" journalled)] `shouldSatisfy` \case [l] -> "ticked: tests green" `T.isSuffixOf` l; _ -> False
          child `shouldBe` object ["row" .= (1 :: Int), "text" .= ("  - test: imports @src/bistro/main__test.py:test_importing_main_opens_no_database \9679" :: Text)]
          (lookupKey "row" step, lookupKey "text" step) `shouldSatisfy` \case
            (Just (Number 0), Just (String t)) -> ("- [x] s @" <> T.pack table <> ":Table \10003 ") `T.isPrefixOf` t
            _ -> False
        other -> expectationFailure ("two facts, got " <> show other)

plansStage1Spec :: Spec
plansStage1Spec = describe "olifant/plans (stage 1)" $ do
  let table = "src/bistro/booking/models/table.py"
      planWith step = T.unlines ["# T", "", "## Hypothesis", "- ? where is the table", "- a claim @" <> T.pack table <> ":Table ", "", "## Plan", step]
      plan = planWith "- [ ] add a method · by hand"
      docOf d = "textDocument" .= object ["uri" .= (d ^. L.uri)]
      -- the fixture app with a plan, its runner the shim
      withPlan text act = withPython [("plans/t.md", text)] $ \ports dir -> do
        shim <- canonicalizePath "src/Olifant/Server/testdata/claude-shim.sh"
        TIO.appendFile (dir </> ".olifant/config.toml") ("\n[plans]\nrunner = \"" <> T.pack shim <> "\"\n")
        act ports dir
      askP = Proxy :: Proxy "olifant/plans/ask"
      validateP = Proxy :: Proxy "olifant/plans/validate"
      doP = Proxy :: Proxy "olifant/plans/do"
      errand method line = do
        p <- openDoc "plans/t.md" "markdown"
        blockMove method (object [docOf p, "line" .= (line :: Int)])
      arr = \case
        Just (Array xs) -> toList xs
        _ -> []
      linesOf b = arr (lookupKey "lines" b)
      -- the errand records the repo holds, and the lines that were not one
      records dir = readRecords . fromMaybe "" <$> readText (diskWorkspace dir) (dir </> ".olifant/state/errands.jsonl")
      lastStatus dir = (\l -> last (T.splitOn "\t" (last (T.lines l)))) <$> TIO.readFile (dir </> ".olifant/state/steps.log")
  it "ask answers ghost blocks with fingerprints for anchors that resolve" $
    withPlan plan $ \ports dir -> do
      (answer, _) <- inProcessWith ports dir (errand askP 3)
      v <- either (fail . T.unpack) pure answer
      lookupKey "verb" v `shouldBe` Just (String "ask")
      let blocks = arr (lookupKey "blocks" v)
          fps = map (lookupKey "fp") (concatMap linesOf blocks)
      map (lookupKey "after") blocks `shouldBe` [Just (Number 3)]
      map (\case Just (String fp) -> T.length fp; _ -> 0) fps `shouldBe` [7, 7, 0]
      last fps `shouldBe` Just Aeson.Null
      lookupKey "unresolved" v `shouldBe` Just (Number 1)
      lookupKey "cost" v `shouldBe` Just (Number 0.0123)
      logged <- T.lines <$> TIO.readFile (dir </> ".olifant/state/steps.log")
      fmap (T.drop 1 . T.dropWhile (/= '\t')) (listToMaybe (reverse logged)) `shouldSatisfy` maybe False ("plan-ask\tplans/t.md:3\t" `T.isPrefixOf`)
  it "ask on a step is refused" $
    withPlan plan $ \ports dir -> do
      (answer, _) <- inProcessWith ports dir (errand askP 7)
      either (T.isInfixOf "ask needs a question line") (const False) answer `shouldBe` True
  it "validate on the heading lands after the matching line" $
    withPlan plan $ \ports dir -> do
      (answer, _) <- inProcessWith ports dir (errand validateP 2)
      v <- either (fail . T.unpack) pure answer
      map (\b -> (lookupKey "after" b, lookupKey "after_text" b)) (arr (lookupKey "blocks" v))
        `shouldBe` [(Just (Number 4), Just (String ("- a claim @" <> T.pack table <> ":Table")))]
  it "do edits the tree, reports changed files and refuses a second do" $
    withPlan (planWith "- [ ] add a method SHIM:SLOW") $ \ports dir -> do
      let git args = callProcess "git" (["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t"] ++ args)
      git ["init", "-q", "-b", "main"]
      git ["add", "."]
      git ["commit", "-q", "-m", "the fixture"]
      ((first, second, third), _) <- inProcessWith ports dir $ do
        p <- openDoc "plans/t.md" "markdown"
        let params = object [docOf p, "line" .= (7 :: Int)]
            method = SMethod_CustomMethod doP
            await rid = (responseForId method rid <&> \(TResponseMessage _ _ r) -> either (Left . (^. L.message)) Right r) <|> (anyMessage >> await rid)
        r1 <- sendRequest method params
        r2 <- sendRequest method params
        second <- await r2
        first <- await r1
        third <- blockMove doP params
        pure (first, second, third)
      second `shouldBe` Left "a do is already running"
      v <- either (fail . T.unpack) pure first
      lookupKey "changed" v `shouldBe` Just (Array (pure (String (T.pack (dir </> table)))))
      map (map (lookupKey "text") . linesOf) (arr (lookupKey "blocks" v))
        `shouldBe` [[Just (String ("- [ ] add a method SHIM:SLOW   @" <> T.pack table <> ":Table.label"))]]
      either Just (const Nothing) third `shouldBe` Nothing
      -- the refused second do writes no record
      (rs, 0) <- records dir
      [(erVerb r, erChanged r, erStatus r) | AnErrand r <- rs] `shouldBe` [("do", 1, Ok), ("do", 1, Ok)]
  it "(M10, M3) a plan edited above the step during a do: the do's line and the tick's land under the step, by its words" $
    withPlan (planWith "- [ ] add a method SHIM:SLOW @src/bistro/booking/models/table.py:Table\n  - test: imports @src/bistro/main__test.py:test_importing_main_opens_no_database \9675") $ \ports dir -> needsUv $ do
      (text, _) <- inProcessWith ports {pTester = (`processRunner` 120)} dir $ do
        p <- openDoc "plans/t.md" "markdown"
        let method = SMethod_CustomMethod doP
        i <- sendRequest method (object [docOf p, "line" .= (7 :: Int)])
        let begun = (() <$ message SMethod_WindowWorkDoneProgressCreate) <|> (anyMessage >> begun)
            answerOf = responseForId method i <|> (anyMessage >> answerOf)
            applied = (() <$ message SMethod_WorkspaceApplyEdit) <|> (anyMessage >> applied)
        begun
        _ <- applyEdit p (TextEdit (Range (Position 0 0) (Position 0 0)) "a line above\n")
        _ <- answerOf
        applied
        documentContents p
      let after = drop 8 (T.lines text)
      take 1 after `shouldSatisfy` \case [l] -> "- [ ] add a method" `T.isPrefixOf` l; _ -> False
      map (T.takeWhileEnd (/= '\183')) (filter ("  - " `T.isPrefixOf`) (take 4 (drop 1 after)))
        `shouldSatisfy` \ws -> any ("answered: " `T.isInfixOf`) ws && " ticked: tests green" `elem` ws
  it "do on a step with a green test child answers the facts that tick it" $
    withPlan (planWith "- [ ] add a method @src/bistro/booking/models/table.py:Table\n  - test: imports @src/bistro/main__test.py:test_importing_main_opens_no_database \9675") $ \ports dir -> needsUv $ do
      (answer, _) <- inProcessWith ports {pTester = (`processRunner` 120)} dir (errand doP 7)
      v <- either (fail . T.unpack) pure answer
      case arr (lookupKey "facts" v) of
        [child, step, journalled] -> do
          -- the do's line, then the tick's, ride with the facts
          map (T.takeWhileEnd (/= '\183')) [x | String x <- arr (lookupKey "lines" journalled)] `shouldSatisfy` \case
            [d, t] -> "answered: " `T.isInfixOf` d && t == " ticked: tests green"
            _ -> False
          child `shouldBe` object ["row" .= (8 :: Int), "text" .= ("  - test: imports @src/bistro/main__test.py:test_importing_main_opens_no_database \9679" :: Text)]
          (lookupKey "row" step, lookupKey "text" step) `shouldSatisfy` \case
            (Just (Number 7), Just (String t)) -> ("- [x] add a method @" <> T.pack table <> ":Table \10003 ") `T.isPrefixOf` t
            _ -> False
        other -> expectationFailure ("two facts, got " <> show other)
      -- the blocks are the runner's, as before
      length (arr (lookupKey "blocks" v)) `shouldBe` 1
  it "do runs the step's tests over the files it changed as they are on disk, not the editor's older text" $
    withPlan (planWith "- [ ] add a method @src/bistro/booking/models/table.py:Table\n  - test: has label @src/bistro/booking/models/table__test.py:test_table_has_label \9675") $ \ports dir -> needsUv $ do
      TIO.writeFile (dir </> "src/bistro/booking/models/table__test.py") "from bistro.booking.models.table import Table\n\n\ndef test_table_has_label() -> None:\n    assert Table(id=\"t1\", seats=2).label() == \"t1\"\n"
      (answer, _) <- inProcessWith ports {pTester = (`processRunner` 120)} dir $ do
        -- the module is open in the editor, as it was before the do
        _ <- openDoc table "python"
        errand doP 7
      v <- either (fail . T.unpack) pure answer
      [t | Just t <- map (lookupKey "text") (arr (lookupKey "facts" v))] `shouldSatisfy` \case
        [String child, String step] -> T.isSuffixOf "\9679" child && ("- [x] add a method" `T.isPrefixOf` step)
        _ -> False
  it "a runner failure leaves the plan alone and names the reason" $
    withPlan (T.replace "where is the table" "where is the table SHIM:FAIL" plan) $ \ports dir -> do
      (answer, _) <- inProcessWith ports dir (errand askP 3)
      either (T.isInfixOf "no credentials") (const False) answer `shouldBe` True
      TIO.readFile (dir </> "plans/t.md") >>= (`shouldBe` T.replace "where is the table" "where is the table SHIM:FAIL" plan)
  it "an answered ask writes an ok record with tokens, session and the answer's id" $
    withPlan plan $ \ports dir -> do
      (answer, _) <- inProcessWith ports dir (errand askP 3)
      v <- either (fail . T.unpack) pure answer
      ([AnErrand r], 0) <- records dir
      (erVerb r, erStatus r, erPlan r, erRow r, erLines r, erGrounded r) `shouldBe` ("ask", Ok, "plans/t.md", 3, 3, 1)
      (erBlocks r, erDropped r, erUnresolved r, erChanged r, erWhy r) `shouldBe` (1, 0, 1, 0, Nothing)
      erTokens r `shouldBe` Just (Tokens 2 40 1000 5000)
      (erSeconds r, erTurns r, erCost r) `shouldBe` (Just 1.2, Just 1, Just 0.0123)
      erSession r `shouldSatisfy` maybe False ("shim-" `T.isPrefixOf`)
      erId r `shouldBe` fromMaybe "" (erSession r)
      lookupKey "id" v `shouldBe` Just (String (erId r))
      erSection r `shouldBe` "Hypothesis"
      erPromptChars r `shouldSatisfy` (> 0)
      erBranch r `shouldBe` Nothing
      lastStatus dir >>= (`shouldBe` "ok")
  it "a runner failure writes an error record with why and a step log line" $
    withPlan (T.replace "where is the table" "where is the table SHIM:FAIL" plan) $ \ports dir -> do
      _ <- inProcessWith ports dir (errand askP 3)
      ([AnErrand r], 0) <- records dir
      (erStatus r, erCost r, erSession r, erSeconds r, erLines r) `shouldBe` (Error, Nothing, Nothing, Nothing, 0)
      erWhy r `shouldSatisfy` maybe False ("exited 2" `T.isInfixOf`)
      erId r `shouldSatisfy` ("ask-plans/t.md:3@" `T.isPrefixOf`)
      lastStatus dir >>= (`shouldBe` "error")
  it "a runner's is_error writes an error record that keeps the session" $
    withPlan (T.replace "where is the table" "where is the table SHIM:ERRORJSON" plan) $ \ports dir -> do
      (answer, _) <- inProcessWith ports dir (errand askP 3)
      answer `shouldBe` Left "the runner reported an error"
      ([AnErrand r], 0) <- records dir
      (erStatus r, erWhy r) `shouldBe` (Error, Just "the runner reported an error")
      erSession r `shouldSatisfy` maybe False ("shim-" `T.isPrefixOf`)
      erId r `shouldBe` fromMaybe "" (erSession r)
  it "prose outside the grammar writes an empty record that still has the session" $
    withPlan (T.replace "where is the table" "where is the table SHIM:PROSE" plan) $ \ports dir -> do
      _ <- inProcessWith ports dir (errand askP 3)
      ([AnErrand r], 0) <- records dir
      (erStatus r, erSession r, erWhy r, erBlocks r) `shouldBe` (Empty, Just "shim-prose", Just "ask answered nothing in the grammar", 0)
      lastStatus dir >>= (`shouldBe` "empty")
  it "a refused verb (ask on a step) writes nothing" $
    withPlan plan $ \ports dir -> do
      _ <- inProcessWith ports dir (errand askP 7)
      records dir >>= (`shouldBe` ([], 0))
  it "an errand in a git repository records its branch" $
    withPlan plan $ \ports dir -> do
      let git args = callProcess "git" (["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t"] ++ args)
      git ["init", "-q", "-b", "trial"]
      git ["add", "."]
      git ["commit", "-q", "-m", "the fixture"]
      _ <- inProcessWith ports dir (errand askP 3)
      ([AnErrand r], 0) <- records dir
      erBranch r `shouldBe` Just "trial"
  it "prose only is 'answered nothing in the grammar'" $
    withPlan (T.replace "where is the table" "where is the table SHIM:PROSE" plan) $ \ports dir -> do
      (answer, _) <- inProcessWith ports dir (errand askP 3)
      answer `shouldBe` Left "ask answered nothing in the grammar"
  it "cancel stops the runner" $
    withPlan (T.replace "where is the table" "where is the table SHIM:SLEEP" plan) $ \ports dir -> do
      ((code, took), _) <- inProcessWith ports dir $ do
        p <- openDoc "plans/t.md" "markdown"
        let method = SMethod_CustomMethod askP
        rid <- sendRequest method (object [docOf p, "line" .= (3 :: Int)])
        liftIO (threadDelay 300000)
        start <- liftIO getCurrentTime
        sendNotification SMethod_CancelRequest (CancelParams (case rid of IdInt n -> InL n; IdString t -> InR t))
        let await = responseForId method rid <|> (anyMessage >> await)
        TResponseMessage _ _ r <- await
        end <- liftIO getCurrentTime
        pure (either (Just . (^. L.code)) (const Nothing) r, diffUTCTime end start)
      code `shouldBe` Just (InL LSPErrorCodes_RequestCancelled)
      took `shouldSatisfy` (< 2)
      ([AnErrand r], 0) <- records dir
      (erStatus r, erSession r, erWhy r) `shouldBe` (Cancelled, Nothing, Just "cancelled")
      lastStatus dir >>= (`shouldBe` "cancelled")
  let outcomeP = Proxy :: Proxy "olifant/plans/outcome"
      -- an errand asked, then the given outcome notifications; the debugState
      -- round trip after them means the server handled them
      decided outcomes = do
        v <- errand askP 3 >>= either (fail . T.unpack) pure
        let Just (String eid) = lookupKey "id" v
        uri <- getDocUri "plans/t.md"
        forM_ outcomes $ \fields -> sendNotification (SMethod_CustomMethod outcomeP) (object (("textDocument" .= object ["uri" .= uri]) : fields eid))
        _ <- debugState
        pure eid
  it "olifant/plans/outcome appends an outcome record" $
    withPlan plan $ \ports dir -> do
      (eid, _) <- inProcessWith ports dir $ decided
        [ \eid -> ["id" .= eid, "block" .= (0 :: Int), "outcome" .= ("accepted" :: Text)]
        , \eid -> ["id" .= eid, "block" .= (1 :: Int), "outcome" .= ("rejected" :: Text)] ]
      (rs, 0) <- records dir
      length rs `shouldBe` 3
      [(orId o, orBlock o, orOutcome o) | AnOutcome o <- rs] `shouldBe` [(eid, 0, "accepted"), (eid, 1, "rejected")]
      -- its time to the second, as every record's
      raw <- TIO.readFile (dir </> ".olifant/state/errands.jsonl")
      [T.length at | Just (Object o) <- map (Aeson.decodeStrict . TE.encodeUtf8) (T.lines raw), Just (String "outcome") <- [KM.lookup "kind" o], Just (String at) <- [KM.lookup "at" o]]
        `shouldBe` [20, 20]
  it "an outcome with a bad verb, block or id is ignored" $
    withPlan plan $ \ports dir -> do
      _ <- inProcessWith ports dir $ decided
        [ \eid -> ["id" .= eid, "block" .= (0 :: Int), "outcome" .= ("maybe" :: Text)]
        , \eid -> ["id" .= eid, "block" .= ("0" :: Text), "outcome" .= ("accepted" :: Text)]
        , \eid -> ["id" .= eid, "block" .= (-1 :: Int), "outcome" .= ("accepted" :: Text)]
        , \eid -> ["id" .= eid, "block" .= (0.5 :: Double), "outcome" .= ("accepted" :: Text)]
        , \_ -> ["id" .= (7 :: Int), "block" .= (0 :: Int), "outcome" .= ("accepted" :: Text)]
        , \eid -> ["id" .= eid, "block" .= (0 :: Int)] ]
      (rs, 0) <- records dir
      length rs `shouldBe` 1

doReviewSpec :: Spec
doReviewSpec = describe "olifant/plans (the do review)" $ do
  let table = "src/bistro/booking/models/table.py"
      planWith step = T.unlines ["# T", "", "## Hypothesis", "- ? where is the table", "- a claim @" <> T.pack table <> ":Table ", "", "## Plan", step]
      docOf d = "textDocument" .= object ["uri" .= (d ^. L.uri)]
      -- the fixture app with a plan, committed, its runner the shim
      withRepo text act = withPython [("plans/t.md", text)] $ \ports dir -> do
        shim <- canonicalizePath "src/Olifant/Server/testdata/claude-shim.sh"
        TIO.appendFile (dir </> ".olifant/config.toml") ("\n[plans]\nrunner = \"" <> T.pack shim <> "\"\n")
        let git args = callProcess "git" (["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t"] ++ args)
        git ["init", "-q", "-b", "main"]
        git ["add", "."]
        git ["commit", "-q", "-m", "the fixture"]
        act ports dir
      doP = Proxy :: Proxy "olifant/plans/do"
      reviewP = Proxy :: Proxy "olifant/plans/review"
      reviewsP = Proxy :: Proxy "olifant/plans/reviews"
      chooseP = Proxy :: Proxy "olifant/plans/review/choose"
      -- a do of the plan's step (row 7): the ready notification's params, then its answer
      doStep p = do
        let method = SMethod_CustomMethod doP
        rid <- sendRequest method (object [docOf p, "line" .= (7 :: Int)])
        let readyN = (customNotification (Proxy :: Proxy "olifant/plans/review/ready") <&> \case NotMess (TNotificationMessage _ _ v) -> v) <|> (anyMessage >> readyN)
            answerOf = responseForId method rid <|> (anyMessage >> answerOf)
        ready <- readyN
        TResponseMessage _ _ r <- answerOf
        let Just (String reviewId) = lookupKey "id" ready
        pure (reviewId, ready, either (Left . (^. L.message)) Right r)
      choose p rid c = blockMove chooseP (object [docOf p, "id" .= rid, "choice" .= (c :: Text)])
      at ks v = foldl (\m k -> m >>= lookupKey k) (Just v) ks
      arr = \case
        Just (Array xs) -> toList xs
        _ -> []
      records dir = readRecords . fromMaybe "" <$> readText (diskWorkspace dir) (dir </> ".olifant/state/errands.jsonl")
  it "a do leaves a review: ready before its answer, the change a diff, kept on disk until answered" $
    withRepo (planWith "- [ ] add a method") $ \ports dir -> do
      ((rid, ready, answer, listed, view), _) <- inProcessWith ports dir $ do
        p <- openDoc "plans/t.md" "markdown"
        (rid, ready, answer) <- doStep p
        listed <- blockMove reviewsP (object [docOf p])
        view <- blockMove reviewP (object [docOf p, "id" .= rid])
        pure (rid, ready, answer, listed, view)
      v <- either (fail . T.unpack) pure answer
      lookupKey "review" v `shouldBe` Just (String rid)
      lookupKey "kind" ready `shouldBe` Just (String "do")
      l <- either (fail . T.unpack) pure listed
      map (\x -> (lookupKey "id" x, lookupKey "row" x)) (arr (lookupKey "reviews" l)) `shouldBe` [(Just (String rid), Just (Number 7))]
      w <- either (fail . T.unpack) pure view
      at ["title", "badge"] w `shouldBe` Just (String "\9670 answered")
      at ["summary"] w `shouldBe` Just (String "The do changed 1 file.")
      map (\f -> (lookupKey "file" f, lookupKey "status" f)) (arr (at ["landing", "files"] w)) `shouldBe` [(Just (String (T.pack table)), Just (String "modified"))]
      at ["landing", "diff"] w `shouldSatisfy` \case Just (String d) -> "+    def label(self) -> str: return self.id" `T.isInfixOf` d; _ -> False
      at ["landing", "edit", "documentChanges"] w `shouldSatisfy` isJust
      map (lookupKey "key") (arr (at ["offers"] w)) `shouldBe` map (Just . String) ["a", "r", "o", "t"]
      listDirectory (dir </> ".olifant/state/reviews/plans/t") >>= (`shouldSatisfy` ((== 1) . length))
  it "revert restores the tree from the reference; the review goes, a rejected outcome is recorded" $
    withRepo (planWith "- [ ] add a method") $ \ports dir -> do
      original <- TIO.readFile (dir </> table)
      ((rid, chosen, listed), _) <- inProcessWith ports dir $ do
        p <- openDoc "plans/t.md" "markdown"
        (rid, _, _) <- doStep p
        chosen <- choose p rid "revert"
        listed <- blockMove reviewsP (object [docOf p])
        pure (rid, chosen, listed)
      c <- either (fail . T.unpack) pure chosen
      (lookupKey "done" c, lookupKey "said" c) `shouldBe` (Just (Bool True), Just (String "reverted 1 file"))
      arr (lookupKey "files" c) `shouldBe` [String (T.pack (dir </> table))]
      TIO.readFile (dir </> table) >>= (`shouldBe` original)
      fmap (arr . lookupKey "reviews") listed `shouldBe` Right []
      (rs, 0) <- records dir
      [(orId o, orOutcome o) | AnOutcome o <- rs] `shouldBe` [(rid, "rejected")]
  it "keep lands the plan lines (the anchor the do found) and keeps the tree" $
    withRepo (planWith "- [ ] add a method") $ \ports dir -> do
      ((rid, chosen), _) <- inProcessWith ports dir $ do
        p <- openDoc "plans/t.md" "markdown"
        (rid, _, _) <- doStep p
        (rid,) <$> choose p rid "keep"
      c <- either (fail . T.unpack) pure chosen
      lookupKey "done" c `shouldBe` Just (Bool True)
      Aeson.encode (lookupKey "edit" c) `shouldSatisfy` \e -> ("add a method   @" <> TE.encodeUtf8 (T.pack table) <> ":Table.label") `BS.isInfixOf` BSL.toStrict e
      TIO.readFile (dir </> table) >>= (`shouldSatisfy` T.isInfixOf "def label")
      (rs, 0) <- records dir
      [(orId o, orOutcome o) | AnOutcome o <- rs] `shouldBe` [(rid, "accepted")]
  it "allow writes the denied command into [plans] do_tools and offers it no more" $
    withRepo (planWith "- [ ] add a method SHIM:DENY") $ \ports dir -> do
      ((before, chosen), _) <- inProcessWith ports dir $ do
        p <- openDoc "plans/t.md" "markdown"
        (rid, _, _) <- doStep p
        before <- blockMove reviewP (object [docOf p, "id" .= rid])
        (before,) <$> choose p rid "allow"
      b <- either (fail . T.unpack) pure before
      at ["denied"] b `shouldBe` Just (Array (pure (String "tried `python -m pytest t.py` 2 times; not allowed.") <> pure (String "Allowed: `uv run \8230`.")))
      map (lookupKey "key") (arr (at ["offers"] b)) `shouldBe` map (Just . String) ["a", "r", "o", "t", "p"]
      c <- either (fail . T.unpack) pure chosen
      lookupKey "said" c `shouldBe` Just (String "allowed Bash(python -m pytest*) in .olifant/config.toml")
      map (lookupKey "key") (arr (at ["review", "offers"] c)) `shouldBe` map (Just . String) ["a", "r", "o", "t"]
      TIO.readFile (dir </> ".olifant/config.toml") >>= (`shouldSatisfy` T.isInfixOf "do_tools = [\"Bash(python -m pytest*)\"]")
  it "test on a step with no tests says so; the review stays" $
    withRepo (planWith "- [ ] add a method") $ \ports dir -> do
      ((chosen, listed), _) <- inProcessWith ports dir $ do
        p <- openDoc "plans/t.md" "markdown"
        (rid, _, _) <- doStep p
        chosen <- choose p rid "test"
        (chosen,) <$> blockMove reviewsP (object [docOf p])
      chosen `shouldBe` Left "the step has no tests"
      fmap (length . arr . lookupKey "reviews") listed `shouldBe` Right 1
  it "a code lens sits on the step of an open review" $
    withRepo (planWith "- [ ] add a method") $ \ports dir -> do
      ((rid, lenses), _) <- inProcessWith ports dir $ do
        p <- openDoc "plans/t.md" "markdown"
        (rid, _, _) <- doStep p
        (rid,) <$> getCodeLenses p
      let args = Just (Just [String rid, String (getUri (filePathToUri (dir </> "plans/t.md")))])
      [(l ^. L.range . L.start . L.line, fmap (^. L.title) (l ^. L.command), fmap (^. L.command) (l ^. L.command), fmap (^. L.arguments) (l ^. L.command)) | l <- lenses]
        `shouldBe` [ (0, Just "1 to review", Just "olifant.plans.review", args), (7, Just "do \9670 to review", Just "olifant.plans.review", args)
                   , (7, Just "1 delegation", Just "olifant.plans.errand", Just (Just [String (getUri (filePathToUri (dir </> "plans/t.md"))), Number 7])) ]
  it "a do review and the reconcile review open at once: the title counts both and opens the do first" $
    withRepo (planWith "- [ ] add a method") $ \ports dir -> do
      -- the do's new method is in no step: with no one to ask, a new-step reading
      ((rid, lenses), _) <- inProcessWith ports dir $ do
        p <- openDoc "plans/t.md" "markdown"
        (rid, _, _) <- doStep p
        _ <- blockMove (Proxy :: Proxy "olifant/plans/reconcile") (object [docOf p])
        (rid,) <$> getCodeLenses p
      let titled = sort [(l ^. L.range . L.start . L.line, fmap (^. L.title) (l ^. L.command), fmap (take 1 . fromMaybe [] . (^. L.arguments)) (l ^. L.command)) | l <- lenses]
      -- a unit in no step counts on the title only
      titled `shouldBe` [(0, Just "2 to review", Just [String rid]), (7, Just "1 delegation", Just [String (getUri (filePathToUri (dir </> "plans/t.md")))]), (7, Just "do \9670 to review", Just [String rid])]
  describe "the record: journal lines and past dos" $ do
    let errandP = Proxy :: Proxy "olifant/plans/errand"
        errandsP = Proxy :: Proxy "olifant/plans/errands"
        planLines t = T.splitOn "\n" t
        journalOf t = [l | l <- planLines t, "  - " `T.isPrefixOf` l, " \183 do " `T.isInfixOf` l || " \183 do again " `T.isInfixOf` l || " \183 kept " `T.isInfixOf` l || " \183 reverted " `T.isInfixOf` l]
        factLines v = [x | f <- arr (lookupKey "facts" v), String x <- arr (lookupKey "lines" f)]
    it "(j1) a do's answer carries its journal line under the step" $
      withRepo (planWith "- [ ] add a method") $ \ports dir -> do
        ((_, _, answer), _) <- inProcessWith ports dir $ do
          p <- openDoc "plans/t.md" "markdown"
          doStep p
        v <- either (fail . T.unpack) pure answer
        [l | l <- factLines v, " \183 do (" `T.isInfixOf` l] `shouldSatisfy` \case
          [l] -> "  - " `T.isPrefixOf` l && T.isSuffixOf ("answered: @" <> T.pack table <> ":Table.label") l
          _ -> False
        [f | f <- arr (lookupKey "facts" v), lookupKey "after" f == Just (Number 7)] `shouldSatisfy` ((== 1) . length)
    it "(j2) keep's edit carries `kept the do's change`, and the archive says kept" $
      withRepo (planWith "- [ ] add a method") $ \ports dir -> do
        ((rid, chosen), _) <- inProcessWith ports dir $ do
          p <- openDoc "plans/t.md" "markdown"
          (rid, _, _) <- doStep p
          (rid,) <$> choose p rid "keep"
        c <- either (fail . T.unpack) pure chosen
        Aeson.encode (lookupKey "edit" c) `shouldSatisfy` \e -> "kept the do's change" `BS.isInfixOf` BSL.toStrict e
        Just r <- loadArchived ports dir "plans/t.md" rid
        rvChoice r `shouldBe` Just "kept"
    it "(j3) a do that fails lands its line by applyEdit, and is archived" $
      withRepo (planWith "- [ ] add a method SHIM:FAIL") $ \ports dir -> do
        ((answer, text), _) <- inProcessWith ports dir $ do
          p <- openDoc "plans/t.md" "markdown"
          a <- blockMove doP (object [docOf p, "line" .= (7 :: Int)])
          _ <- message SMethod_WorkspaceApplyEdit
          (a,) <$> documentContents p
        answer `shouldSatisfy` either (T.isInfixOf "no credentials") (const False)
        journalOf text `shouldSatisfy` \case
          [l] -> "error: " `T.isInfixOf` l && "no credentials" `T.isInfixOf` l
          _ -> False
        loadArchive ports dir "plans/t.md" >>= (`shouldSatisfy` \case [r] -> rvStatus r == "error"; _ -> False)
    it "(j4) a do on a plan not open lands its line when the plan is opened and reconciled" $
      withRepo (planWith "- [ ] add a method SHIM:FAIL") $ \ports dir -> do
        (text, _) <- inProcessWith ports dir $ do
          let uri = filePathToUri (dir </> "plans/t.md")
          _ <- blockMove doP (object ["textDocument" .= object ["uri" .= uri], "line" .= (7 :: Int)])
          p <- openDoc "plans/t.md" "markdown"
          -- its applyEdit comes before its answer; lsp-test applies it
          _ <- blockMove (Proxy :: Proxy "olifant/plans/reconcile") (object [docOf p])
          documentContents p
        length (journalOf text) `shouldBe` 1
    it "(j4b) a do on a plan not open lands its line when the plan is opened, with no reconcile" $
      withRepo (planWith "- [ ] add a method SHIM:FAIL") $ \ports dir -> do
        (text, _) <- inProcessWith ports dir $ do
          let uri = filePathToUri (dir </> "plans/t.md")
          _ <- blockMove doP (object ["textDocument" .= object ["uri" .= uri], "line" .= (7 :: Int)])
          p <- openDoc "plans/t.md" "markdown"
          let applied = (() <$ message SMethod_WorkspaceApplyEdit) <|> (anyMessage >> applied)
          applied
          documentContents p
        length (journalOf text) `shouldBe` 1
    it "(j5) a journal line opens its errand; a step line has none" $
      withRepo (planWith "- [ ] add a method") $ \ports dir -> do
        ((rid, onLine, onStep), _) <- inProcessWith ports dir $ do
          p <- openDoc "plans/t.md" "markdown"
          (rid, _, Right v) <- doStep p
          let [f] = [f | f <- arr (lookupKey "facts" v), any (\case String x -> " \183 do (" `T.isInfixOf` x; _ -> False) (arr (lookupKey "lines" f))]
              Just (Number after) = lookupKey "after" f
          applyEdit p (TextEdit (Range (Position (round after + 1) 0) (Position (round after + 1) 0)) (T.unlines [x | String x <- arr (lookupKey "lines" f)]))
          onLine <- blockMove errandP (object [docOf p, "row" .= (round after + 1 :: Int)])
          onStep <- blockMove errandP (object [docOf p, "row" .= (7 :: Int)])
          pure (rid, onLine, onStep)
        v <- either (fail . T.unpack) pure onLine
        (lookupKey "kind" v, lookupKey "id" v) `shouldBe` (Just (String "errand"), Just (String rid))
        at ["landing", "diff"] v `shouldSatisfy` \case Just (String d) -> "def label" `T.isInfixOf` d; _ -> False
        map (lookupKey "key") (arr (at ["offers"] v)) `shouldBe` [Just (String "o")]
        onStep `shouldBe` Right Aeson.Null
    it "(j6) reopen resumes the do's session in a fork, as a new errand with its parent, through a new review" $
      withRepo (planWith "- [ ] add a method") $ \ports dir -> do
        let argsFile = dir </> "shim-args"
        setEnv "SHIM_ARGS" argsFile
        ((rid, chosen, rid2, listed), _) <- inProcessWith ports dir (do
          p <- openDoc "plans/t.md" "markdown"
          (rid, _, _) <- doStep p
          chosen <- choose p rid "reopen"
          let method = SMethod_CustomMethod doP
          i <- sendRequest method (object [docOf p, "line" .= (7 :: Int), "resume" .= rid, "text" .= ("and a docstring" :: Text)])
          let readyN = (customNotification (Proxy :: Proxy "olifant/plans/review/ready") <&> \case NotMess (TNotificationMessage _ _ v) -> v) <|> (anyMessage >> readyN)
              answerOf = responseForId method i <|> (anyMessage >> answerOf)
          ready <- readyN
          _ <- answerOf
          listed <- blockMove errandsP (object [docOf p, "row" .= (7 :: Int)])
          pure (rid, chosen, lookupKey "id" ready, listed)) `finally` unsetEnv "SHIM_ARGS"
        c <- either (fail . T.unpack) pure chosen
        at ["reopen", "id"] c `shouldBe` Just (String rid)
        lines' <- T.lines <$> TIO.readFile argsFile
        -- specs that run alongside may add their own runs to the file
        lines' `shouldSatisfy` any (T.isInfixOf ("--resume " <> rid <> " --fork-session"))
        rid2 `shouldSatisfy` (/= Just (String rid))
        (rs, 0) <- records dir
        [erParent e | AnErrand e <- rs] `shouldBe` [Nothing, Just rid]
        -- the clock is fixed in tests: the two are at one time, in no order
        fmap (sort . map (lookupKey "id") . arr . lookupKey "errands") listed `shouldBe` Right (sort [rid2, Just (String rid)])
    it "(j7) a reopen whose step is gone is refused before it runs" $
      withRepo (planWith "- [ ] add a method") $ \ports dir -> do
        (refused, _) <- inProcessWith ports dir $ do
          p <- openDoc "plans/t.md" "markdown"
          (rid, _, _) <- doStep p
          _ <- choose p rid "keep"
          applyEdit p (TextEdit (Range (Position 7 0) (Position 8 0)) "- [ ] something else entirely\n")
          blockMove doP (object [docOf p, "line" .= (7 :: Int), "resume" .= rid, "text" .= ("more" :: Text)])
        refused `shouldBe` Left "the step is gone"
    it "(j8) a step with past dos has a delegations lens" $
      withRepo (planWith "- [ ] add a method") $ \ports dir -> do
        (lenses, _) <- inProcessWith ports dir $ do
          p <- openDoc "plans/t.md" "markdown"
          (rid, _, _) <- doStep p
          _ <- choose p rid "keep"
          getCodeLenses p
        [(l ^. L.range . L.start . L.line, fmap (^. L.command) (l ^. L.command)) | l <- lenses, fmap (^. L.title) (l ^. L.command) == Just "1 delegation"]
          `shouldBe` [(7, Just "olifant.plans.errand")]
    it "(j10) a reconcile right after a do does not send the do's line again (it rode the answer)" $
      withRepo (planWith "- [ ] add a method") $ \ports dir -> do
        (text, _) <- inProcessWith ports dir $ do
          p <- openDoc "plans/t.md" "markdown"
          _ <- doStep p
          _ <- blockMove (Proxy :: Proxy "olifant/plans/reconcile") (object [docOf p])
          documentContents p
        -- lsp-test does not apply an answer's facts: the line is in no edit the server sent
        journalOf text `shouldBe` []
    it "(j11) revert after git collected the do's reference is refused; the files and the review stay" $
      withRepo (planWith "- [ ] add a method") $ \ports dir -> do
        ((chosen, listed), _) <- inProcessWith ports dir $ do
          -- a tree with an edit of its own: the reference is a stash commit, no branch's
          liftIO (TIO.appendFile (dir </> "pyproject.toml") "# mine\n")
          p <- openDoc "plans/t.md" "markdown"
          (rid, _, _) <- doStep p
          liftIO (callProcess "git" ["-C", dir, "prune", "--expire=now"])
          chosen <- choose p rid "revert"
          (chosen,) <$> blockMove reviewsP (object [docOf p])
        chosen `shouldSatisfy` either (T.isInfixOf "reference is gone") (const False)
        TIO.readFile (dir </> table) >>= (`shouldSatisfy` T.isInfixOf "def label")
        fmap (length . arr . lookupKey "reviews") listed `shouldBe` Right 1
    it "(j9) the archive keeps the diff after git has collected the do's reference" $
      withRepo (planWith "- [ ] add a method") $ \ports dir -> do
        ((rid, _), _) <- inProcessWith ports dir $ do
          p <- openDoc "plans/t.md" "markdown"
          (rid, _, _) <- doStep p
          (rid,) <$> choose p rid "revert"
        callProcess "git" ["-C", dir, "prune", "--expire=now"]
        Just r <- loadArchived ports dir "plans/t.md" rid
        map fcPath (rvDiff r) `shouldBe` [table]
        rvChoice r `shouldBe` Just "reverted"

-- | A custom request: its answer, or the error's sentence.
blockMove :: KnownSymbol s => Proxy s -> Value -> Session (Either Text Value)
blockMove method params = request (SMethod_CustomMethod method) params <&> \(TResponseMessage _ _ r) -> either (Left . (^. L.message)) Right r

concurrencySpec :: Spec
concurrencySpec = describe "slow work, and requests meanwhile" $ do
  -- holes, the model and completion read or plan over a project: one
  -- project's slow work never holds another's requests (spec §1b)
  let uriOf d = object ["textDocument" .= object ["uri" .= (d ^. L.uri)]]
      awaiting m rid = void (responseForId m rid)
  forM_
    [ ("olifant/holes", \py -> awaiting (SMethod_CustomMethod (Proxy :: Proxy "olifant/holes")) <$> sendRequest (SMethod_CustomMethod (Proxy :: Proxy "olifant/holes")) (uriOf py))
    , ("olifant/model", \py -> awaiting (SMethod_CustomMethod (Proxy :: Proxy "olifant/model")) <$> sendRequest (SMethod_CustomMethod (Proxy :: Proxy "olifant/model")) (uriOf py))
    , ("textDocument/completion", \_ -> do
        test <- openDoc "py/src/bistro/booking/commands/create_booking__test.py" "python"
        awaiting SMethod_TextDocumentCompletion <$> sendRequest SMethod_TextDocumentCompletion (CompletionParams test (Position 34 0) Nothing Nothing Nothing))
    ]
    $ \(method, slowRequest) ->
      it ("answers another project while " <> method <> " is slow in one (review minor)") $ do
        armed <- newIORef False
        entered <- newEmptyMVar
        gate <- newEmptyMVar
        withProjects [("py", "python-htmx", "packs/python-htmx/fixtures/app"), ("rs", "rust-htmx", "packs/rust-htmx/fixtures/app")] $ \ports root -> do
          let disk = pFiles ports
              hold = atomicModifyIORef' armed (False,) >>= \held -> when held (putMVar entered () >> takeMVar gate)
              gated = disk {readText = \p -> when ("/py/" `isInfixOf` p) hold >> readText disk p}
              -- the first syntax query after arming (completion plans over the open buffer)
              slowSyntax = querying (\l q t -> hold >> querySyntax (pSyntax ports) l q t)
          _ <- forkIO (threadDelay 5000000 >> void (tryPutMVar gate ()))
          (first, _) <- inProcessWith ports {pFiles = gated, pSyntax = slowSyntax} root $ do
            py <- openDoc "py/src/bistro/booking/commands/create_booking.py" "python"
            rs <- openDoc "rs/billing/billing-service/src/cancel_invoice.rs" "rust"
            _ <- pullDiagnostics rs
            _ <- pullDiagnostics py
            liftIO (writeIORef armed True)
            slow <- slowRequest py
            liftIO (timeout 10000000 (takeMVar entered)) >>= maybe (fail (method <> " never read a file")) pure
            other <- sendRequest SMethod_TextDocumentDiagnostic (DocumentDiagnosticParams Nothing Nothing rs Nothing Nothing)
            let firstOf = (responseForId SMethod_TextDocumentDiagnostic other >> pure "other") <|> (slow >> pure "slow") <|> (anyMessage >> firstOf)
            winner <- firstOf
            liftIO (void (tryPutMVar gate ()))
            pure (winner :: Text)
          first `shouldBe` "other"
  sequential . it "answers other requests while a slow resolve (a 2 s check) runs" $ do
    fills <- canonicalizePath "packs/python-htmx/generators/handle-body/tests/create-booking/fills.toml"
    let slowCheck = Checker (\_ _ -> threadDelay 2000000 >> pure (Right (CheckReport [] [] [])))
    withPython [("src/bistro/booking/commands/close_table.py", "def handle() -> None:\n    raise NotImplementedError(\"olifant: body: close the table\")\n")] $ \ports dir -> do
      ((hoverMs, resolvedMs), _) <- inProcessWithConfig [("fills", String (T.pack fills))] ports {pChecker = const slowCheck} dir $ do
        doc <- openDoc "src/bistro/booking/commands/close_table.py" "python"
        actions <- getCodeActions doc (Range (Position 1 4) (Position 1 4))
        a <- case [x | InR x <- actions, x ^. L.title == "Fill with the model"] of
          x : _ -> pure x
          [] -> fail "no Fill action"
        t0 <- liftIO getCurrentTime
        rid <- sendRequest SMethod_CodeActionResolve a
        _ <- getHover doc (Position 0 0)
        t1 <- liftIO getCurrentTime
        let awaitResolve = (responseForId SMethod_CodeActionResolve rid >> pure ()) <|> (anyMessage >> awaitResolve)
        awaitResolve
        t2 <- liftIO getCurrentTime
        pure (realToFrac (diffUTCTime t1 t0) * 1000 :: Double, realToFrac (diffUTCTime t2 t0) * 1000 :: Double)
      resolvedMs `shouldSatisfy` (> 1500)
      hoverMs `shouldSatisfy` (< 1000)

-- | The versions an action's edit is for, in one document.
editVersionsOf :: Uri -> CodeAction -> [Int32 |? Null]
editVersionsOf uri a = [te ^. L.textDocument . L.version | Just e <- [a ^. L.edit], Just changes <- [e ^. L.documentChanges], InL te <- changes, te ^. L.textDocument . L.uri == uri]

-- | One message from the server: its headers, then that many bytes.
readMessage :: Handle -> IO BSL.ByteString
readMessage h = do
  let headers n = do
        line <- BS8.hGetLine h
        if BS8.strip line == "" then pure n else headers (maybe n fst (BS8.readInt =<< BS8.stripPrefix "Content-Length: " line))
  n <- headers 0
  BSL.fromStrict <$> BS.hGet h n

pullDiagnostics :: TextDocumentIdentifier -> Session [Diagnostic]
pullDiagnostics doc =
  request SMethod_TextDocumentDiagnostic (DocumentDiagnosticParams Nothing Nothing doc Nothing Nothing) >>= \case
    TResponseMessage _ _ (Right (DocumentDiagnosticReport (InL full))) -> pure (full ^. L.items)
    TResponseMessage _ _ other -> fail ("diagnostics: " <> show other)

-- | A code action's data with one choice made (as an editor does before
-- resolving).
withSet :: Text -> Text -> CodeAction -> CodeAction
withSet k v a = a & L.data_ %~ fmap (\case
  Object o -> Object (KM.insert "sets" (Object (KM.insert (Key.fromText k) (String v) (maybe KM.empty (\case Object s -> s; _ -> KM.empty) (KM.lookup "sets" o)))) o)
  other -> other)

-- | Every text an action's edit inserts.
editTexts :: CodeAction -> [Text]
editTexts a = [either (^. L.newText) (^. L.newText) (toEither t) | Just e <- [a ^. L.edit], Just changes <- [e ^. L.documentChanges], InL te <- changes, t <- te ^. L.edits]
  where
    toEither = \case
      InL x -> Left x
      InR y -> Right y

-- | The versions an action's edit is for.
editVersions :: CodeAction -> [Int32 |? Null]
editVersions a = [te ^. L.textDocument . L.version | Just e <- [a ^. L.edit], Just changes <- [e ^. L.documentChanges], InL te <- changes]

-- | 'withPython', its files checked by the real compiler (pyright through
-- uv): pending without uv, failing with OLIFANT_REQUIRE_TOOLS=1.
withPythonChecked :: [(FilePath, Text)] -> (Ports -> FilePath -> IO ()) -> IO ()
withPythonChecked changed act = needsUv (withPython changed (\ports dir -> act ports {pChecker = processCheckerWith} dir))

-- | An example that needs uv (the real pyright or pytest): pending without
-- it, failing with OLIFANT_REQUIRE_TOOLS=1.
needsUv :: IO () -> IO ()
needsUv act = do
  strict <- (== Just "1") <$> lookupEnv "OLIFANT_REQUIRE_TOOLS"
  findExecutable "uv" >>= \case
    Nothing -> if strict then expectationFailure "required tool missing: uv" else pendingWith "uv is not installed"
    Just _ -> act

-- | The python-htmx fixture app with some files changed, as a project on
-- disk using the pack from this repository, and ports with the real syntax.
withPython :: [(FilePath, Text)] -> (Ports -> FilePath -> IO a) -> IO a
withPython changed act = do
  tmp <- getTemporaryDirectory
  pack <- canonicalizePath "packs/python-htmx"
  fixture <- readTree "packs/python-htmx/fixtures/app"
  withSystemTempDirectory "olifant-lsp-py" $ \dir0 -> do
    dir <- canonicalizePath dir0
    forM_ (M.toList (M.union (M.fromList changed) fixture)) $ \(p, t) -> createDirectoryIfMissing True (takeDirectory (dir </> p)) >> TIO.writeFile (dir </> p) t
    createDirectoryIfMissing True (dir </> ".olifant")
    TIO.writeFile (dir </> ".olifant/config.toml") ("[packs]\npython-htmx = { path = \"" <> T.pack pack <> "\" }\n")
    act plainPorts {pSyntax = astGrepSyntax (tmp </> "olifant-test-cache")} dir

-- | The python-strangler demo (a Flask legacy app beside a python-htmx one)
-- under git: main holds it as it is, the work is on a branch `feature`.
withStrangler :: (Ports -> FilePath -> IO a) -> IO a
withStrangler act = do
  tmp <- getTemporaryDirectory
  htmx <- canonicalizePath "packs/python-htmx"
  strangler <- canonicalizePath "packs/python-strangler"
  fixture <- M.filterWithKey (\p _ -> not (any (`elem` [".venv", "__pycache__", ".pytest_cache"]) (splitDirectories p))) <$> readTree "packs/python-strangler/fixtures/app"
  withSystemTempDirectory "olifant-strangler" $ \dir0 -> do
    dir <- canonicalizePath dir0
    forM_ (M.toList fixture) $ \(p, t) -> createDirectoryIfMissing True (takeDirectory (dir </> p)) >> TIO.writeFile (dir </> p) t
    createDirectoryIfMissing True (dir </> ".olifant")
    TIO.writeFile (dir </> ".olifant/config.toml") ("[packs]\npython-htmx = { path = \"" <> T.pack htmx <> "\" }\npython-strangler = { path = \"" <> T.pack strangler <> "\" }\n\n[migrate]\nlegacy = [\"legacy/**\"]\nswitches = \"src/bistro/switches.py\"\n")
    let git args = callProcess "git" (["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t"] ++ args)
    git ["init", "-q", "-b", "main"]
    git ["add", "."]
    git ["commit", "-q", "-m", "the monolith"]
    git ["checkout", "-q", "-b", "feature"]
    act plainPorts {pSyntax = astGrepSyntax (tmp </> "olifant-test-cache"), pBaseText = gitBaseText, pBaseFiles = gitBaseFiles} dir

readTree :: FilePath -> IO (M.Map FilePath Text)
readTree root = M.fromList <$> go root
  where
    go d = do
      names <- listDirectory d
      fmap concat . forM names $ \n -> do
        let p = d </> n
        isDir <- doesDirectoryExist p
        if isDir then go p else (\t -> [(makeRelative root p, t)]) <$> TIO.readFile p

-- | The server in this process over pipes, a client session against it
-- (debugState on); the server's exit code once the session has ended.
inProcess :: FilePath -> Session a -> IO (a, Maybe Int)
inProcess = inProcessWith plainPorts

plainPorts :: Ports
plainPorts = Ports (diskWorkspace "/") noSyntax (const noChecker) (const noVerifier) (const noRunner) (\_ _ -> noRunner) (pure (UTCTime (fromGregorian 2026 10 6) 0)) Nothing Nothing (DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False)) (\_ -> pure Nothing) (\_ _ _ -> pure (Left "no base in tests")) (\_ _ -> pure (Left "no base in tests")) gitStateStamp

inProcessWith :: Ports -> FilePath -> Session a -> IO (a, Maybe Int)
inProcessWith = inProcessWithConfig []

-- | 'inProcessWith', with more initializationOptions.
inProcessWithConfig :: [(Key.Key, Value)] -> Ports -> FilePath -> Session a -> IO (a, Maybe Int)
inProcessWithConfig extra ports dir session = do
  (inRead, inWrite) <- createPipe
  (outRead, outWrite) <- createPipe
  done <- newEmptyMVar
  _ <- forkIO (runLspWithHandles ports inRead outWrite >>= putMVar done)
  a <- runSessionWithHandles inWrite outRead defaultConfig {lspConfig = KM.fromList ([("debug", Bool True), ("debounce_ms", Number 100)] ++ extra)} fullLatestClientCaps dir session
  code <- timeout 5000000 (takeMVar done)
  pure (a, code)

debugState :: Session Value
debugState =
  request (SMethod_CustomMethod (Proxy :: Proxy "olifant/debugState")) Aeson.Null >>= \case
    TResponseMessage _ _ (Right v) -> pure v
    TResponseMessage _ _ (Left err) -> fail (show err)

lookupKey :: Text -> Value -> Maybe Value
lookupKey k = \case
  Object o -> KM.lookup (Key.fromText k) o
  _ -> Nothing

-- | A project on disk (an empty `.olifant/config.toml`) with some files.
withProject :: [(FilePath, Text)] -> (FilePath -> IO a) -> IO a
withProject files act = withSystemTempDirectory "olifant-lsp" $ \dir0 -> do
  dir <- canonicalizePath dir0
  forM_ ((".olifant/config.toml", "") : files) $ \(p, t) -> createDirectoryIfMissing True (takeDirectory (dir </> p)) >> TIO.writeFile (dir </> p) t
  act dir

-- | Recognitions as suggestions (convention recognition, Task 8).
-- | Strangling a legacy codebase: rules on new code, against the base.
newCodeSpec :: Spec
newCodeSpec = describe "new code (only = \"new\", against the base)" $ do
  let legacy = "legacy/views.py"
      base = T.unlines ["def close_table(t):", "    return t", "", "def seat(p):", "    return p"]
      rule = "[rules.no-new-defs]\nfiles = \"legacy/*.py\"\nonly = \"new\"\nmatch = { kind = \"function_definition\" }\nmessage = \"New work goes in the new architecture\"\n"
      git dir args = callProcess "git" (["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t"] ++ args)
      underGit dir = do
        git dir ["init", "-q", "-b", "main"]
        git dir ["add", "."]
        git dir ["commit", "-q", "-m", "first"]
        git dir ["checkout", "-q", "-b", "feature"]
      ruleIds ds = [c | d <- ds, Just (InR c) <- [d ^. L.code]]
  it "(a) a definition new on the branch is reported; (b) an edited one is not" $
    withPython [(legacy, base), (".olifant/rules.toml", rule)] $ \ports dir -> do
      underGit dir
      ((added, edited), _) <- inProcessWith ports {pBaseText = gitBaseText} dir $ do
        doc <- openDoc legacy "python"
        changeDoc doc [TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument (base <> "\ndef waitlist(r):\n    return r\n")))]
        a <- pullDiagnostics doc
        changeDoc doc [TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument (T.replace "return t" "return t + 1" base)))]
        e <- pullDiagnostics doc
        pure (a, e)
      ruleIds added `shouldContain` ["no-new-defs"]
      ruleIds edited `shouldNotContain` ["no-new-defs"]
  it "(c) typing asks git once, not per keystroke (the base is cached until files change)" $
    withPython [(legacy, base), (".olifant/rules.toml", rule)] $ \ports dir -> do
      underGit dir
      asked <- newIORef (0 :: Int)
      let counting root ref path = modifyIORef' asked (+ 1) >> gitBaseText root ref path
      (_, _) <- inProcessWith ports {pBaseText = counting} dir $ do
        doc <- openDoc legacy "python"
        forM_ [1 .. 3 :: Int] $ \i -> do
          changeDoc doc [TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument (base <> "\n# " <> T.pack (show i) <> "\n")))]
          pullDiagnostics doc
      readIORef asked `shouldReturn` 1
  it "(e) a generator applies at {legacy}: in the legacy folder, not elsewhere" $
    withPython
      [ (legacy, base)
      , ("src/bistro/booking/x.py", base)
      , (".olifant/generators/extract/generator.toml", "description = \"d\"\nlanguage = \"python\"\n[applies_at]\npath = \"{legacy}\"\nnode = { kind = \"function_definition\" }\nlabel = \"Extract\"\n[[edits]]\ncreate = \"x.txt\"\ntext = \"x\"\n")
      ]
      $ \ports dir -> do
        config <- TIO.readFile (dir </> ".olifant/config.toml")
        TIO.writeFile (dir </> ".olifant/config.toml") (config <> "\n[migrate]\nlegacy = [\"legacy/**\"]\n")
        ((inLegacy, inSrc), _) <- inProcessWith ports dir $ do
          a <- openDoc legacy "python"
          b <- openDoc "src/bistro/booking/x.py" "python"
          (,) <$> getCodeActions a (Range (Position 1 4) (Position 1 4)) <*> getCodeActions b (Range (Position 1 4) (Position 1 4))
        let titles as = [x ^. L.title | InR x <- as]
        titles inLegacy `shouldContain` ["Extract"]
        titles inSrc `shouldNotContain` ["Extract"]
  it "(f) the project model carries the migration report" $
    withPython [(legacy, base)] $ \ports dir -> do
      config <- TIO.readFile (dir </> ".olifant/config.toml")
      TIO.writeFile (dir </> ".olifant/config.toml") (config <> "\n[migrate]\nlegacy = [\"legacy/**\"]\n")
      underGit dir
      (v, _) <- inProcessWith ports {pBaseText = gitBaseText} dir $ do
        doc <- openDoc legacy "python"
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/model")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)]]) >>= \case
          TResponseMessage _ _ (Right v) -> pure v
          TResponseMessage _ _ (Left e) -> fail (show e)
      (lookupKey "migration" v >>= lookupKey "legacy_now") `shouldBe` Just (Number 2)
  let bigEdit = T.replace "    return t" (T.unlines (replicate 20 "    t = t + 1") <> "    return t") base
      migrating dir extra = do
        config <- TIO.readFile (dir </> ".olifant/config.toml")
        TIO.writeFile (dir </> ".olifant/config.toml") (config <> "\n[migrate]\nlegacy = [\"legacy/**\"]\n" <> extra)
      untilRule ids doc = do
        let loop n = do
              ds <- pullDiagnostics doc
              if any (`elem` ids) (ruleIds ds) || n <= (0 :: Int) then pure ds else liftIO (threadDelay 100000) >> loop (n - 1)
        loop 50
  it "(g) a big change inside a legacy function: a quiet hint (is this new work?)" $
    withPython [(legacy, base)] $ \ports dir -> do
      migrating dir ""
      underGit dir
      (ds, _) <- inProcessWith ports {pBaseText = gitBaseText} dir $ do
        doc <- openDoc legacy "python"
        changeDoc doc [TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument bigEdit))]
        pullDiagnostics doc
      [(c, d ^. L.severity) | d <- ds, Just (InR c) <- [d ^. L.code], c == "migrate-new-work"] `shouldBe` [("migrate-new-work", Just DiagnosticSeverity_Hint)]
  it "(h) with Jev saying new behaviour, the hint becomes the bug-fix-only warning" $
    withPython [(legacy, base)] $ \ports dir -> do
      migrating dir "\n[decide]\nmodel = \"jev\"\n"
      underGit dir
      let answers = dir </> "answers.json"
      TIO.writeFile answers "{\"answers\": {\"bug-or-new\": \"new behaviour\"}}"
      (ds, _) <- inProcessWithConfig [("decide_answers", String (T.pack answers))] ports {pBaseText = gitBaseText} dir $ do
        doc <- openDoc legacy "python"
        changeDoc doc [TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument bigEdit))]
        untilRule ["legacy-bug-fix-only"] doc
      [(c, d ^. L.severity) | d <- ds, Just (InR c) <- [d ^. L.code], c `elem` ["legacy-bug-fix-only", "migrate-new-work"]] `shouldBe` [("legacy-bug-fix-only", Just DiagnosticSeverity_Warning)]
  it "(j) typing in a big change asks Jev once at a time, not once per keystroke (review #8)" $
    withPython [(legacy, base)] $ \ports dir -> do
      migrating dir "\n[decide]\nmodel = \"jev\"\n"
      underGit dir
      let answers = dir </> "answers.json"
      TIO.writeFile answers "wait"
      _ <- inProcessWithConfig [("decide_answers", String (T.pack answers))] ports {pBaseText = gitBaseText} dir $ do
        doc <- openDoc legacy "python"
        forM_ [1 .. 5 :: Int] $ \i -> do
          -- each keystroke changes the function's text
          changeDoc doc [TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument (T.replace "    return t" ("    t = t * " <> T.pack (show i) <> "\n    return t") bigEdit)))]
          void (pullDiagnostics doc)
      asked <- T.lines <$> TIO.readFile (answers <> ".asked")
      length asked `shouldBe` 1
  it "(k) a decision outlives the request that asked: its cancelled token does not make it unsure (review minor #9)" $
    withPython [(legacy, base)] $ \ports dir -> do
      migrating dir "\n[decide]\nmodel = \"jev\"\n"
      let answers = dir </> "answers.json"
          q = De.choice "bug-or-new" "" [("bug fix", ""), ("new behaviour", "")] []
      TIO.writeFile answers "wait"
      (token, trip) <- newCancel
      trip
      s0 <- newServerState ports
      let s = s0 {ssConfig = (ssConfig s0) {scDecideAnswers = Just answers}, ssCancel = token}
      (s', Just (_, project)) <- St.projectFor (dir </> legacy) s
      let Just decider = St.deciderFor s' project
          ask' = St.decision s' decider (legacy, "bug-or-new:f") "fp1" q (pure ())
      _ <- ask'
      threadDelay 200000
      -- by a rename: the decider polls the file, and GHC refuses to write one being read
      TIO.writeFile (answers <> ".new") "{\"answers\": {\"bug-or-new\": \"bug fix\"}}"
      renameFile (answers <> ".new") answers
      let poll n = ask' >>= \case
            Just a -> pure (Just a)
            Nothing | n > (0 :: Int) -> threadDelay 50000 >> poll (n - 1)
            Nothing -> pure Nothing
      poll 40 `shouldReturn` Just (De.Chosen "bug fix" [] De.Unknown)
  it "(i) Jev answering nonsense: the hint stands, and nothing waits on it (Review Focus 5)" $
    withPython [(legacy, base)] $ \ports dir -> do
      migrating dir "\n[decide]\nmodel = \"jev\"\n"
      underGit dir
      let answers = dir </> "answers.json"
      TIO.writeFile answers "{\"answers\": {\"bug-or-new\": \"maybe\"}}"
      ((ds, ms), _) <- inProcessWithConfig [("decide_answers", String (T.pack answers))] ports {pBaseText = gitBaseText} dir $ do
        doc <- openDoc legacy "python"
        changeDoc doc [TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument bigEdit))]
        t0 <- liftIO getCurrentTime
        d <- pullDiagnostics doc
        t1 <- liftIO getCurrentTime
        liftIO (threadDelay 500000)
        d' <- pullDiagnostics doc
        pure (d', realToFrac (diffUTCTime t1 t0) :: Double)
      [c | d <- ds, Just (InR c) <- [d ^. L.code], c `elem` ["legacy-bug-fix-only", "migrate-new-work"]] `shouldBe` ["migrate-new-work"]
      ms `shouldSatisfy` (< 1)
  it "(d) when the base moves (the branch merged into main), what was new is not any more (Review Focus 2)" $
    withPython [(legacy, base), (".olifant/rules.toml", rule)] $ \ports dir -> do
      underGit dir
      TIO.writeFile (dir </> legacy) (base <> "\ndef waitlist(r):\n    return r\n")
      git dir ["commit", "-q", "-am", "waitlist"]
      ((before, after), _) <- inProcessWith ports {pBaseText = gitBaseText} dir $ do
        doc <- openDoc legacy "python"
        b <- pullDiagnostics doc
        liftIO (git dir ["checkout", "-q", "main"] >> git dir ["merge", "-q", "feature"])
        sendNotification SMethod_WorkspaceDidChangeWatchedFiles (DidChangeWatchedFilesParams [FileEvent (filePathToUri (dir </> legacy)) FileChangeType_Changed])
        a <- pullDiagnostics doc
        pure (b, a)
      ruleIds before `shouldContain` ["no-new-defs"]
      ruleIds after `shouldNotContain` ["no-new-defs"]

  it "(d2) the base moves with no file event (Linux Neovim watches no files): the next pull sees it (review minor #10)" $
    withPython [(legacy, base), (".olifant/rules.toml", rule)] $ \ports dir -> do
      underGit dir
      TIO.writeFile (dir </> legacy) (base <> "\ndef waitlist(r):\n    return r\n")
      git dir ["commit", "-q", "-am", "waitlist"]
      ((before, after), _) <- inProcessWith ports {pBaseText = gitBaseText} dir $ do
        doc <- openDoc legacy "python"
        b <- pullDiagnostics doc
        liftIO (git dir ["checkout", "-q", "main"] >> git dir ["merge", "-q", "feature"])
        -- an edit of the buffer, so the pull is not the same answer again
        changeDoc doc [TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument (base <> "\ndef waitlist(r):\n    return r\n\n")))]
        a <- pullDiagnostics doc
        pure (b, a)
      ruleIds before `shouldContain` ["no-new-defs"]
      ruleIds after `shouldNotContain` ["no-new-defs"]

-- | The python-strangler pack on its demo: the rules.
stranglerSpec :: Spec
stranglerSpec = describe "python-strangler (the demo monolith)" $ do
  let views = "legacy/views.py"
      codes ds = [c | d <- ds, Just (InR c) <- [d ^. L.code]]
  it "(a) new work in legacy code is warned, with fixes that create it in the new architecture" $
    withStrangler $ \ports dir -> do
      original <- TIO.readFile (dir </> views)
      ((ds, titles), _) <- inProcessWith ports dir $ do
        doc <- openDoc views "python"
        changeDoc doc [TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument (original <> "\n\ndef add_to_waitlist(guest, party):\n    return {\"guest\": guest, \"party\": party}\n")))]
        ds <- pullDiagnostics doc
        let line = length (T.lines original) + 2
        as <- getCodeActions doc (Range (Position (fromIntegral line) 4) (Position (fromIntegral line) 4))
        pure (ds, [(a ^. L.title, fmap (lookupKey "generator") (a ^. L.data_)) | InR a <- as])
      codes ds `shouldContain` ["legacy-bug-fix-only"]
      -- a fix names its generator: the editor's picker opens on it
      lookup "Create it as a command" titles `shouldBe` Just (Just (Just (String "python-strangler/delegate-command")))
      titles <- pure (map fst titles)
      titles `shouldContain` ["Create it as a command"]
      titles `shouldContain` ["Create it as a query"]
  it "(d) the fix creates the command, and the legacy function only delegates to it (with its allowance)" $
    withStrangler $ \ports dir -> do
      original <- TIO.readFile (dir </> views)
      TIO.writeFile (dir </> views) (original <> "\n\ndef add_to_waitlist(guest, party):\n    return {\"guest\": guest, \"party\": party}\n")
      s <- newServerState ports
      let line = length (T.lines original) + 3
      (_, planned) <- fixPlan False (const (pure ())) (dir </> views) "legacy-bug-fix-only" line 1 [("app", "bistro"), ("service", "booking")] s
      case planned of
        Left why -> expectationFailure (T.unpack why)
        Right (_, plan) -> do
          M.keys (planWrites plan) `shouldContain` ["src/bistro/booking/commands/add_to_waitlist.py"]
          let legacyText = M.findWithDefault "" views (planWrites plan)
          legacyText `shouldSatisfy` T.isInfixOf "# olifant:allow legacy-bug-fix-only — delegates to booking/commands/add_to_waitlist\ndef add_to_waitlist(guest, party):"
          legacyText `shouldSatisfy` T.isInfixOf "def close_table(table_id):"
  it "(e) retire: a switched function keeps only its new path; the function stays, so its callers do (Review Focus 4)" $
    withStrangler $ \ports dir -> do
      original <- TIO.readFile (dir </> views)
      let switched = T.replace "def close_table(table_id):\n" "def close_table(table_id):\n    # olifant:switch close-table → booking/commands/close_table\n    if switches.on(\"close-table\"):\n        result = CloseTable().handle(CloseTableInput(id=table_id))\n        if not isinstance(result, NotYet):\n            return {\"id\": table_id, \"open\": False}\n" original
      TIO.writeFile (dir </> views) switched
      TIO.writeFile (dir </> "src/bistro/switches.py") "SWITCHES: dict[str, bool] = {}\nSWITCHES[\"close-table\"] = True\n"
      s <- newServerState ports
      let line = 1 + length (takeWhile (not . T.isPrefixOf "def close_table") (T.lines switched))
      (_, planned) <- fixPlan False (const (pure ())) (dir </> views) "switch-retire" line 1 [] s
      case planned of
        Left why -> expectationFailure (T.unpack why)
        Right (_, plan) -> do
          let after = M.findWithDefault "" views (planWrites plan)
          after `shouldSatisfy` T.isInfixOf "def close_table(table_id):\n    result = CloseTable().handle(CloseTableInput(id=table_id))\n    if not isinstance(result, NotYet):\n        return {\"id\": table_id, \"open\": False}\n    raise RuntimeError("
          after `shouldNotSatisfy` T.isInfixOf "UPDATE tables SET open = 0"
          after `shouldNotSatisfy` T.isInfixOf "olifant:switch close-table"
          after `shouldSatisfy` T.isInfixOf "def open_table(table_id):"
          -- its callers are named: they keep calling it (review minor #15)
          planWarnings plan `shouldSatisfy` any (\w -> "legacy/app.py:56" `T.isInfixOf` w && "close_table" `T.isInfixOf` w)
  it "(e4) retire is offered only once the switch's flag is on in the registry (review #5)" $
    withStrangler $ \ports dir -> do
      original <- TIO.readFile (dir </> views)
      let switched = T.replace "def close_table(table_id):\n" "def close_table(table_id):\n    # olifant:switch close-table → booking/commands/close_table\n    if switches.on(\"close-table\"):\n        result = CloseTable().handle(CloseTableInput(id=table_id))\n        if not isinstance(result, NotYet):\n            return {\"id\": table_id, \"open\": False}\n" original
          line = 1 + length (takeWhile (not . T.isPrefixOf "def close_table") (T.lines switched))
          registry flag = "SWITCHES: dict[str, bool] = {}\nSWITCHES[\"close-table\"] = " <> flag <> "\n"
          offered = do
            s <- newServerState ports
            (_, fs) <- Fx.findingsFor (dir </> views) s
            pure ("switch-retire" `elem` map Ru.fRule fs)
      TIO.writeFile (dir </> views) switched
      TIO.writeFile (dir </> "src/bistro/switches.py") (registry "False")
      offered `shouldReturn` False
      TIO.writeFile (dir </> "src/bistro/switches.py") (registry "True")
      offered `shouldReturn` True
  it "(e2) retire on a method keeps the class's indentation (review #2)" $
    withStrangler $ \ports dir -> do
      original <- TIO.readFile (dir </> views)
      let method = "\n\nclass Exporter:\n    def run(self, table_id):\n        # olifant:switch close-table → booking/commands/close_table\n        if switches.on(\"close-table\"):\n            result = CloseTable().handle(CloseTableInput(id=table_id))\n            if not isinstance(result, NotYet):\n                return {\"id\": table_id}\n        return legacy_close(table_id)\n"
      TIO.writeFile (dir </> views) (original <> method)
      TIO.writeFile (dir </> "src/bistro/switches.py") "SWITCHES: dict[str, bool] = {}\nSWITCHES[\"close-table\"] = True\n"
      s <- newServerState ports
      let line = length (T.lines original) + 4
      (_, planned) <- fixPlan False (const (pure ())) (dir </> views) "switch-retire" line 1 [] s
      case planned of
        Left why -> expectationFailure (T.unpack why)
        Right (_, plan) ->
          M.findWithDefault "" views (planWrites plan) `shouldSatisfy` T.isInfixOf "class Exporter:\n    def run(self, table_id):\n        result = CloseTable().handle(CloseTableInput(id=table_id))\n        if not isinstance(result, NotYet):\n            return {\"id\": table_id}\n        raise RuntimeError("
  it "(e3) retire on a signature over several lines keeps all of it (review #3)" $
    withStrangler $ \ports dir -> do
      original <- TIO.readFile (dir </> views)
      let fn = "\n\ndef long_sig(\n    table_id,\n    party: dict[str, int] = {\"a\": 1},\n) -> dict:\n    # olifant:switch long-sig → booking/commands/long_sig\n    if switches.on(\"long-sig\"):\n        result = LongSig().handle(LongSigInput(id=table_id))\n        if not isinstance(result, NotYet):\n            return {}\n    return {\"old\": True}\n"
      TIO.writeFile (dir </> views) (original <> fn)
      TIO.writeFile (dir </> "src/bistro/switches.py") "SWITCHES: dict[str, bool] = {}\nSWITCHES[\"long-sig\"] = True\n"
      s <- newServerState ports
      let line = length (T.lines original) + 3
      (_, planned) <- fixPlan False (const (pure ())) (dir </> views) "switch-retire" line 1 [] s
      case planned of
        Left why -> expectationFailure (T.unpack why)
        Right (_, plan) ->
          M.findWithDefault "" views (planWrites plan) `shouldSatisfy` T.isInfixOf "def long_sig(\n    table_id,\n    party: dict[str, int] = {\"a\": 1},\n) -> dict:\n    result = LongSig().handle(LongSigInput(id=table_id))\n"
  it "(f) a switch over a function with several returns leaves its old body exactly as it was (Review Focus 3)" $
    withStrangler $ \ports dir -> do
      original <- TIO.readFile (dir </> views)
      s <- newServerState ports
      createDirectoryIfMissing True (dir </> "src/bistro/booking/queries")
      TIO.writeFile (dir </> "src/bistro/booking/queries/seat_party.py") "class SeatParty: ...\nclass SeatPartyInput: ...\n"
      let sets = [("app", "bistro"), ("service", "booking"), ("kind", "queries"), ("name", "seat_party"), ("function", "seat_party"), ("file", "legacy/views.py")]
      (_, planned) <- stepsPlanWith (const (pure ())) False (dir </> views) (0, 0) [PickStep "python-strangler/switch" sets Nothing] s
      case planned of
        Left why -> expectationFailure (T.unpack why)
        Right (_, plan) -> do
          let after = M.findWithDefault "" views (planWrites plan)
              body t = takeWhile (\l -> T.null l || " " `T.isPrefixOf` l) (drop 1 (dropWhile (not . T.isPrefixOf "def seat_party") (T.lines t)))
              oldBody = body original
          -- the switch, then the old body, line for line
          drop (length (body after) - length oldBody) (body after) `shouldBe` oldBody
          T.unlines (body after) `shouldSatisfy` T.isInfixOf "olifant:switch seat-party"
  it "(f3) a switch alone over a command that does not exist is refused, saying how (review minor #14)" $
    withStrangler $ \ports dir -> do
      s <- newServerState ports
      let sets = [("app", "bistro"), ("service", "booking"), ("kind", "commands"), ("name", "close_table"), ("function", "close_table"), ("file", "legacy/views.py")]
      (_, planned) <- stepsPlanWith (const (pure ())) False (dir </> views) (0, 0) [PickStep "python-strangler/switch" sets Nothing] s
      case planned of
        Left why -> why `shouldSatisfy` T.isInfixOf "extract-command"
        Right (_, plan) -> planConflicts plan `shouldNotBe` []
  it "(f2) a switch over a generator function yields the new path's rows, then stops (review #4)" $
    withStrangler $ \ports dir -> do
      s <- newServerState ports
      createDirectoryIfMissing True (dir </> "src/bistro/booking/queries")
      TIO.writeFile (dir </> "src/bistro/booking/queries/export_rows.py") "class ExportRows: ...\nclass ExportRowsInput: ...\n"
      TIO.writeFile (dir </> "src/bistro/booking/commands/close_table.py") "class CloseTable: ...\nclass CloseTableInput: ...\n"
      let sets = [("app", "bistro"), ("service", "booking"), ("kind", "queries"), ("name", "export_rows"), ("function", "export_rows"), ("file", "legacy/views.py")]
      (_, planned) <- stepsPlanWith (const (pure ())) False (dir </> views) (0, 0) [PickStep "python-strangler/switch" sets Nothing] s
      case planned of
        Left why -> expectationFailure (T.unpack why)
        Right (_, plan) -> do
          let after = M.findWithDefault "" views (planWrites plan)
              body = T.unlines (takeWhile (not . T.isPrefixOf "    \"\"\"The day's") (drop 1 (dropWhile (not . T.isPrefixOf "def export_rows") (T.lines after))))
          body `shouldSatisfy` T.isInfixOf "            yield from "
          body `shouldSatisfy` T.isInfixOf "            return\n"
          -- and a plain function still returns its value
      (_, plain) <- stepsPlanWith (const (pure ())) False (dir </> views) (0, 0) [PickStep "python-strangler/switch" [("app", "bistro"), ("service", "booking"), ("kind", "commands"), ("name", "close_table"), ("function", "close_table"), ("file", "legacy/views.py")] Nothing] s
      case plain of
        Left why -> expectationFailure (T.unpack why)
        Right (_, plan) -> M.findWithDefault "" views (planWrites plan) `shouldNotSatisfy` T.isInfixOf "yield from"
  it "(g) the ⚡ menu offers extract as a command first where the code writes, as a query first where it reads" $
    withStrangler $ \ports dir -> do
      original <- TIO.readFile (dir </> views)
      let lineOf name = fromIntegral (length (takeWhile (not . T.isPrefixOf ("def " <> name)) (T.lines original))) + 2
      ((atClose, atToday), _) <- inProcessWith ports dir $ do
        doc <- openDoc views "python"
        let titles l = (\as -> [a ^. L.title | InR a <- as, "Extract" `T.isPrefixOf` (a ^. L.title)]) <$> getCodeActions doc (Range (Position l 4) (Position l 4))
        (,) <$> titles (lineOf "close_table") <*> titles (lineOf "todays_bookings")
      atClose `shouldBe` ["Extract close_table to a command", "Extract close_table to a query"]
      atToday `shouldBe` ["Extract todays_bookings to a query", "Extract todays_bookings to a command"]
  it "(h) extracting suggests the service whose models the function's SQL touches" $
    withStrangler $ \ports dir -> do
      -- a second service: which one the work belongs to is a real question
      createDirectoryIfMissing True (dir </> "src/bistro/kitchen/models")
      TIO.writeFile (dir </> "src/bistro/kitchen/__init__.py") ""
      TIO.writeFile (dir </> "src/bistro/kitchen/models/__init__.py") ""
      TIO.writeFile (dir </> "src/bistro/kitchen/models/dish.py") "class Dish:\n    pass\n"
      original <- TIO.readFile (dir </> views)
      let line = length (takeWhile (not . T.isPrefixOf "def close_table") (T.lines original)) + 1
      s <- newServerState ports
      (_, pv) <- previewFor (dir </> views) (line, 4) [PickStep "python-strangler/extract-command" [] Nothing] s
      [(fValue f, fSource f) | fs <- pvFields pv, f <- fs, fName f == "service"] `shouldBe` [(Just "booking", Just FromSuggested)]
  it "(b) a bug fix inside an existing legacy function is quiet" $
    withStrangler $ \ports dir -> do
      original <- TIO.readFile (dir </> views)
      (ds, _) <- inProcessWith ports dir $ do
        doc <- openDoc views "python"
        changeDoc doc [TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument (T.replace "db.audit(f\"closed {table_id}\")" "db.audit(f\"closed table {table_id}\")" original)))]
        pullDiagnostics doc
      codes ds `shouldNotContain` ["legacy-bug-fix-only"]
  it "(c) the new architecture importing legacy code is warned" $
    withStrangler $ \ports dir -> do
      TIO.writeFile (dir </> "src/bistro/booking/x.py") "from legacy.utils import money\n\nX = money(1)\n"
      (ds, _) <- inProcessWith ports dir $ do
        doc <- openDoc "src/bistro/booking/x.py" "python"
        pullDiagnostics doc
      codes ds `shouldContain` ["new-code-not-legacy"]

-- | The generate picker (the float's list, form and live preview).
generatePickerSpec :: Spec
generatePickerSpec = describe "the generate picker" $ do
  let command = "src/bistro/booking/commands/create_booking.py"
  it "(a) olifant/generators gives each one's kind and summary, and which apply at the cursor" $
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc command "python"
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/generators")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "position" .= object ["line" .= (44 :: Int), "character" .= (30 :: Int)]]) >>= \case
          TResponseMessage _ _ (Right v) -> pure v
          TResponseMessage _ _ (Left e) -> fail (show e)
      let gens = case lookupKey "generators" v of Just (Array a) -> toList a; _ -> []
          byId i = fromMaybe Aeson.Null (listToMaybe [g | g <- gens, lookupKey "id" g == Just (String i)])
      lookupKey "kind" (byId "python-htmx/command") `shouldBe` Just (String "create")
      lookupKey "summary" (byId "python-htmx/command") `shouldSatisfy` maybe False (/= String "")
      lookupKey "here" (byId "python-htmx/add-dependency") `shouldBe` Just (Bool True)
      lookupKey "here" (byId "python-htmx/command") `shouldBe` Just (Bool False)
  let pickWith doc steps = request (SMethod_CustomMethod (Proxy :: Proxy "olifant/preview"))
        (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "position" .= object ["line" .= (0 :: Int), "character" .= (0 :: Int)], "steps" .= steps]) >>= \case
          TResponseMessage _ _ (Right v) -> pure v
          TResponseMessage _ _ (Left e) -> fail (show e)
      step g sets = object ["generator" .= (g :: Text), "sets" .= object [Key.fromText k .= v | (k, v) <- sets :: [(Text, Text)]]]
      pick doc steps = pickWith doc [step g ss | (g, ss) <- steps]
      fieldsOf v = [f | Just (Array ss) <- [lookupKey "steps" v], Object st <- toList ss, Just (Array fs) <- [KM.lookup "fields" st], f <- toList fs]
      fieldNamed n v = fromMaybe Aeson.Null (listToMaybe [f | f <- reverse (fieldsOf v), lookupKey "name" f == Just (String n)])
      filesOf v = [p | Just (Array fs) <- [lookupKey "files" v], f <- toList fs, Just (String p) <- [lookupKey "path" f]]
  it "(b) previews a command with nothing set: placeholders for its name, the app a lone choice" $
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc "pyproject.toml" "toml"
        pick doc [("python-htmx/command", [])]
      lookupKey "source" (fieldNamed "app" v) `shouldBe` Just (String "only")
      lookupKey "ready" v `shouldBe` Just (Bool False)
      (lookupKey "main" v >>= lookupKey "text") `shouldSatisfy` maybe False (\case String t -> "class ⟨Command⟩Input" `T.isInfixOf` t; _ -> False)
      filesOf v `shouldSatisfy` any ("⟨command⟩__test.py" `T.isSuffixOf`)
  it "(c) infers the service from the file's folders (never its name), and is ready once the name is set" $
    withPython [] $ \ports dir -> do
      ((before, after), _) <- inProcessWith ports dir $ do
        doc <- openDoc command "python"
        (,) <$> pick doc [("python-htmx/command", [])] <*> pick doc [("python-htmx/command", [("command", "close_table")])]
      lookupKey "source" (fieldNamed "service" before) `shouldBe` Just (String "path")
      lookupKey "value" (fieldNamed "command" before) `shouldBe` Just Aeson.Null
      lookupKey "ready" after `shouldBe` Just (Bool True)
      filesOf after `shouldContain` ["src/bistro/booking/commands/close_table__test.py"]
  it "(d) says why a value is invalid, in the engine's words" $
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc command "python"
        pick doc [("python-htmx/command", [("command", "3tables")])]
      lookupKey "why" (fieldNamed "command" v) `shouldSatisfy` maybe False (\case String t -> "3tables" `T.isInfixOf` t; _ -> False)
      lookupKey "ready" v `shouldBe` Just (Bool False)
  it "(e) a dependent choice waits on what it needs (a second service: the port waits on which)" $
    withPython [("src/bistro/kitchen/ports/__init__.py", "")] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc "pyproject.toml" "toml"
        pick doc [("python-htmx/add-dependency", [])]
      lookupKey "needs" (fieldNamed "port" v) `shouldBe` Just (toJSON (["service"] :: [Text]))
  it "(f) + New repository: its value fills the choice it serves, and one plan writes both (values it has no input for, carried from the step it serves, are left out)" $
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc command "python"
        pickWith doc
          [ object ["generator" .= ("python-htmx/repository" :: Text), "sets" .= object ["entity" .= ("table" :: Text), "kind" .= ("commands" :: Text), "command" .= ("create_booking" :: Text)], "makes" .= object ["step" .= (1 :: Int), "input" .= ("port" :: Text)]]
          , step "python-htmx/add-dependency" [("kind", "commands"), ("command", "create_booking")]
          ]
      (lookupKey "value" (fieldNamed "port" v), lookupKey "source" (fieldNamed "port" v)) `shouldBe` (Just (String "table_repository"), Just (String "new"))
      lookupKey "ready" v `shouldBe` Just (Bool True)
      filesOf v `shouldContain` ["src/bistro/booking/ports/table_repository/table_repository.py"]
  it "(g) a plan that would overwrite a file says so, and is not ready (Review Focus 5)" $
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc command "python"
        pick doc [("python-htmx/command", [("command", "create_booking")])]
      lookupKey "ready" v `shouldBe` Just (Bool False)
      lookupKey "error" v `shouldSatisfy` maybe False (/= Aeson.Null)
  it "(i) python-htmx's Add a port offers + New port and + New repository" $
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc command "python"
        pick doc [("python-htmx/add-dependency", [])]
      -- full ids: a bare name could be another pack's, or a local generator
      lookupKey "new" (fieldNamed "port" v) `shouldBe` Just (toJSON (["python-htmx/port", "python-htmx/repository"] :: [Text]))
  it "(j) rust-htmx's + New repository gives the port's name as its ports/ file is named" $
    withProjects [("shop", "rust-htmx", "packs/rust-htmx/fixtures/app")] $ \ports root -> do
      s <- newServerState ports
      let file = root </> "shop/billing/billing-service/src/cancel_invoice.rs"
      (_, pv) <- previewFor file (0, 0) [PickStep "rust-htmx/service-repository" [("service", "billing"), ("aggregate", "payment")] (Just (1, "port")), PickStep "rust-htmx/add-port" [("service", "billing"), ("command", "cancel_invoice")] Nothing] s
      [(fValue f, fSource f) | fs <- drop 1 (pvFields pv), f <- fs, fName f == "port"] `shouldBe` [(Just "payment_repository", Just FromNew)]
  sequential . it "(k) previews fast: 20 round-trips in the fixture, each a different name (median under 50 ms)" $
    withPython [] $ \ports dir -> do
      (times, _) <- inProcessWith ports dir $ do
        doc <- openDoc command "python"
        _ <- pick doc [("python-htmx/command", [])]
        forM [1 .. 20 :: Int] $ \i -> do
          t0 <- liftIO getCurrentTime
          _ <- pick doc [("python-htmx/command", [("command", "close_table_" <> T.pack (show i))])]
          t1 <- liftIO getCurrentTime
          pure (realToFrac (diffUTCTime t1 t0) * 1000 :: Double)
      let median = sort times !! 10
      liftIO (putStrLn ("      picker preview: median " <> show (round median :: Int) <> " ms, max " <> show (round (maximum times) :: Int) <> " ms"))
      median `shouldSatisfy` (< 50)
  let testFile = "src/bistro/booking/commands/create_booking__test.py"
      editLines v = [l | Just (Object e) <- [lookupKey "edit" v >>= \x -> case x of Object o -> Just (Object o); _ -> Nothing], Just (Array cs) <- [KM.lookup "documentChanges" e], Object c <- toList cs, Just (Array es) <- [KM.lookup "edits" c], Object ed <- toList es, Just (Number l) <- [KM.lookup "range" ed >>= lookupKey "start" >>= lookupKey "line"]]
  it "(l) a snippet planned by the picker lands at the cursor, never at line 1 (review C1)" $
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc testFile "python"
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/plan")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "position" .= object ["line" .= (34 :: Int), "character" .= (0 :: Int)], "steps" .= [step "python-htmx/test-case" [("test", "closes_a_table")]]]) >>= \case
          TResponseMessage _ _ (Right v) -> pure v
          TResponseMessage _ _ (Left e) -> fail (show e)
      editLines v `shouldSatisfy` (\ls -> not (null ls) && all (>= 30) ls)
      -- a snippet: its body with tab stops, for the editor to expand there
      (lookupKey "plan" v >>= lookupKey "snippet" >>= lookupKey "body") `shouldSatisfy` maybe False (\case String t -> "${" `T.isInfixOf` t; _ -> False)
  it "(m) the preview's main file says where the change starts (review I4)" $
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc testFile "python"
        pickWith doc [step "python-htmx/test-case" []] -- position 0: the request's own
      (lookupKey "main" v >>= lookupKey "first") `shouldSatisfy` maybe False (\case Number n -> n >= 1; _ -> False)
  it "(n) a choice not made yet is never blamed: the preview asks for it, not a conflict its stand-in caused" $
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc command "python"
        pickWith doc
          [ object ["generator" .= ("python-htmx/repository" :: Text), "sets" .= object [], "makes" .= object ["step" .= (1 :: Int), "input" .= ("port" :: Text)]]
          , step "python-htmx/add-dependency" [("kind", "commands"), ("command", "create_booking")]
          ]
      lookupKey "error" v `shouldSatisfy` maybe False (\case String t -> "entity" `T.isInfixOf` t && not ("already exists" `T.isInfixOf` t); _ -> False)
  it "(o) a snippet's inputs come from the cursor (the open test file), as the ⚡ menu fills them" $
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc testFile "python"
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/preview")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "position" .= object ["line" .= (34 :: Int), "character" .= (0 :: Int)], "steps" .= [step "python-htmx/test-case" []]]) >>= \case
          TResponseMessage _ _ (Right v) -> pure v
          TResponseMessage _ _ (Left e) -> fail (show e)
      (lookupKey "value" (fieldNamed "test" v), lookupKey "source" (fieldNamed "test" v)) `shouldBe` (Just (String (T.pack testFile)), Just (String "here"))
  it "(p) the preview shows holes as ⟨name⟩ (what you or the model will write), not their defaults" $
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc testFile "python"
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/preview")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "position" .= object ["line" .= (34 :: Int), "character" .= (0 :: Int)], "steps" .= [step "python-htmx/test-case" []]]) >>= \case
          TResponseMessage _ _ (Right v) -> pure v
          TResponseMessage _ _ (Left e) -> fail (show e)
      (lookupKey "main" v >>= lookupKey "text") `shouldSatisfy` maybe False (\case String t -> "def test_⟨behaviour⟩()" `T.isInfixOf` t; _ -> False)
  it "(h) olifant/plan plans steps as one plan" $
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc command "python"
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/plan")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "steps" .= [step "python-htmx/repository" [("app", "bistro"), ("service", "booking"), ("entity", "table")], step "python-htmx/add-dependency" [("app", "bistro"), ("service", "booking"), ("kind", "commands"), ("command", "create_booking"), ("port", "table_repository")]]]) >>= \case
          TResponseMessage _ _ (Right v) -> pure v
          TResponseMessage _ _ (Left e) -> fail (show e)
      lookupKey "plan" v `shouldSatisfy` (/= Nothing)
      show v `shouldSatisfy` ("table_repository.py" `isInfixOf`)
  it "(q) olifant/plan at an __init__ for a port: no snippet (it is no snippet), and its edit applies cleanly" $ do
    given <- readTree "packs/python-htmx/generators/add-dependency/tests/inline/given"
    let file = "src/bistro/booking/commands/cancel_booking.py"
        text = given M.! file
        initLine = length (takeWhile (not . T.isInfixOf "def __init__") (T.lines text))
    withPython (M.toList given) $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc file "python"
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/plan")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "position" .= object ["line" .= initLine, "character" .= (4 :: Int)], "steps" .= [step "python-htmx/add-dependency" [("app", "bistro"), ("service", "booking"), ("kind", "commands"), ("command", "cancel_booking"), ("port", "booking_repository")]]]) >>= \case
          TResponseMessage _ _ (Right v) -> pure v
          TResponseMessage _ _ (Left e) -> fail (show e)
      (lookupKey "plan" v >>= lookupKey "snippet") `shouldBe` Just Aeson.Null
      let edits =
            [ (pos "start" r, pos "end" r, t)
            | Just (Array cs) <- [lookupKey "edit" v >>= lookupKey "documentChanges"]
            , Object c <- toList cs
            , Just (String u) <- [KM.lookup "textDocument" c >>= lookupKey "uri"]
            , ("/" <> T.pack file) `T.isSuffixOf` u
            , Just (Array es) <- [KM.lookup "edits" c]
            , Object e <- toList es
            , Just r <- [KM.lookup "range" e]
            , Just (String t) <- [KM.lookup "newText" e]
            ]
          pos k r = case (lookupKey k r >>= lookupKey "line", lookupKey k r >>= lookupKey "character") of
            (Just (Number l), Just (Number ch)) -> (round l, round ch) :: (Int, Int)
            _ -> (0, 0)
          -- LSP edits are disjoint: apply them from the last back
          offset ls (l, ch) = sum (map ((+ 1) . T.length) (take l ls)) + ch
          apply t (s, e, new) = let ls = T.splitOn "\n" t in T.take (offset ls s) t <> new <> T.drop (offset ls e) t
          after = foldl apply text (sortOn (\(s, _, _) -> Down s) edits)
      edits `shouldSatisfy` (not . null)
      length (filter (== "class CancelBooking:") (T.lines after)) `shouldBe` 1
      after `shouldSatisfy` T.isInfixOf "    def __init__(self, booking_repository: BookingRepository)"

recognitionSpec :: Spec
recognitionSpec = describe "recognition: this looks like a command" $ do
  let newCommand = "src/bistro/booking/commands/close_table.py"
      ownWay = "class CloseTable:\n    def __init__(self, repo):\n        self.repo = repo\n\n    def run(self, table_id):\n        table = self.repo.table(table_id)\n        self.repo.save(table)\n"
      whole t = [TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument t))]
      -- a buffer that is only in the editor (not on disk)
      openNew path text = do
        uri <- getDocUri path
        sendNotification SMethod_TextDocumentDidOpen (DidOpenTextDocumentParams (TextDocumentItem uri "python" 0 text))
        pure (TextDocumentIdentifier uri)
      changeNew doc v text = sendNotification SMethod_TextDocumentDidChange (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier (doc ^. L.uri) v) (whole text))
      suggestAt doc line =
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/suggest")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "line" .= (line :: Int)]) >>= \case
          TResponseMessage _ _ (Right v) -> pure v
          TResponseMessage _ _ (Left err) -> fail (show err)
      planOf v =
        request (SMethod_CustomMethod (Proxy :: Proxy "olifant/plan")) (object ["planId" .= lookupKey "planId" v]) >>= \case
          TResponseMessage _ _ r -> pure r
      changedFiles v = case lookupKey "edit" v >>= lookupKey "documentChanges" of
        Just (Array cs) -> [(u, T.concat [t | Just (Array es) <- [KM.lookup "edits" c], Object e <- toList es, Just (String t) <- [KM.lookup "newText" e]]) | Object c <- toList cs, Just (Object td) <- [KM.lookup "textDocument" c], Just (String u) <- [KM.lookup "uri" td]]
        _ -> []
  it "(a) recognises a command written by hand in commands/, and converts it, with its test" $
    withPython [] $ \ports dir -> do
      ((v, planned), _) <- inProcessWith ports dir $ do
        doc <- openNew newCommand ownWay
        v <- suggestAt doc 1
        p <- planOf v
        pure (v, p)
      (lookupKey "kind" v, lookupKey "band" v, lookupKey "generator" v) `shouldBe` (Just (String "recognition"), Just (String "offer"), Just (String "python-htmx/command"))
      lookupKey "label" v `shouldBe` Just (String "This looks like a command (Close Table)")
      (lookupKey "sets" v >>= lookupKey "command") `shouldBe` Just (String "close_table")
      files <- either (fail . show) (pure . changedFiles) planned
      let at name = [t | (u, t) <- files, ("/" <> name) `T.isSuffixOf` u]
      at "close_table.py" `shouldSatisfy` any (T.isInfixOf "def handle(self, input: CloseTableInput)")
      at "close_table__test.py" `shouldSatisfy` (not . null)
  it "(b) a longer file is left alone: only its companions (the test)" $
    withPython [] $ \ports dir -> do
      let long = ownWay <> T.concat ["\n    def helper_" <> T.pack (show i) <> "(self):\n        return " <> T.pack (show i) <> "\n" | i <- [1 .. 5 :: Int]]
      (planned, _) <- inProcessWith ports dir $ do
        doc <- openNew newCommand long
        suggestAt doc 1 >>= planOf
      files <- either (fail . show) (pure . changedFiles) planned
      [u | (u, _) <- files, "/close_table.py" `T.isSuffixOf` u] `shouldBe` []
      [u | (u, _) <- files, "/close_table__test.py" `T.isSuffixOf` u] `shouldSatisfy` (not . null)
  it "(c) never offers for an older version: typed on, the old offer is refused, the next is for the new text (Review Focus 1)" $
    withPython [] $ \ports dir -> do
      ((old, new, refused), _) <- inProcessWith ports dir $ do
        doc <- openNew newCommand ownWay
        old <- suggestAt doc 1
        changeNew doc 1 (ownWay <> "\n# more\n")
        TResponseMessage _ _ refused <- request (SMethod_CustomMethod (Proxy :: Proxy "olifant/plan")) (object ["planId" .= lookupKey "planId" old])
        new <- suggestAt doc 1
        pure (old, new, refused)
      either (\e -> e ^. L.code) (const (InR ErrorCodes_InternalError)) refused `shouldBe` InL LSPErrorCodes_ContentModified
      (lookupKey "version" new /= lookupKey "version" old) `shouldBe` True
  it "(d) says nothing of a file that already is the convention, nor when recognition is off" $ do
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc "src/bistro/booking/commands/create_booking.py" "python"
        changeDoc doc [TextDocumentContentChangeEvent (InL (TextDocumentContentChangePartial (Range (Position 0 0) (Position 0 0)) Nothing "# edited\n"))]
        suggestAt doc 1
      v `shouldBe` Aeson.Null
    withPython [] $ \ports dir -> do
      config <- TIO.readFile (dir </> ".olifant/config.toml")
      TIO.writeFile (dir </> ".olifant/config.toml") (config <> "\n[recognize]\nenabled = false\n")
      (v, _) <- inProcessWith ports dir $ do
        doc <- openNew newCommand ownWay
        suggestAt doc 1
      v `shouldBe` Aeson.Null
  let feedback doc generator outcome =
        sendNotification (SMethod_CustomMethod (Proxy :: Proxy "olifant/recognitionFeedback")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "generator" .= (generator :: Text), "outcome" .= (outcome :: Text)])
      named n = T.replace "CloseTable" n ownWay
  it "(f) a dismissed file is left alone until its shape changes (Review Focus 5)" $
    withPython [] $ \ports dir -> do
      createBooking <- TIO.readFile (dir </> "src/bistro/booking/commands/create_booking.py")
      ((after, rewritten), _) <- inProcessWith ports dir $ do
        doc <- openNew newCommand ownWay
        _ <- suggestAt doc 1
        feedback doc "python-htmx/command" "dismissed"
        after <- suggestAt doc 1
        changeNew doc 1 (T.replace "CreateBooking" "CloseTable" createBooking)
        rewritten <- suggestAt doc 1
        pure (after, rewritten)
      after `shouldBe` Aeson.Null
      lookupKey "kind" rewritten `shouldBe` Just (String "recognition")
  it "(g) three dismissals in a row quiet a generator to a hint; an accept makes it speak again; it lasts across restarts" $
    withPython [] $ \ports dir -> do
      let file n = "src/bistro/booking/commands/close_" <> n <> ".py"
          dismissThree = forM_ ["a", "b", "c"] $ \n -> do
            doc <- openNew (file n) (named ("Close" <> T.toUpper (T.pack n)))
            _ <- suggestAt doc 1
            feedback doc "python-htmx/command" "dismissed"
      ((quiet, loud), _) <- inProcessWith ports dir $ do
        dismissThree
        d <- openNew (file "d") (named "CloseD")
        quiet <- suggestAt d 1
        feedback d "python-htmx/command" "accepted"
        e <- openNew (file "e") (named "CloseE")
        loud <- suggestAt e 1
        pure (quiet, loud)
      (lookupKey "band" quiet, lookupKey "band" loud) `shouldBe` (Just (String "hint"), Just (String "offer"))
      ((), _) <- inProcessWith ports dir dismissThree
      (later, _) <- inProcessWith ports dir $ do
        f <- openNew (file "f") (named "CloseF")
        suggestAt f 1
      lookupKey "band" later `shouldBe` Just (String "hint")
  -- the model, when configured (Task 10): a recorded answer stands in for it
  let offPlace = "src/bistro/booking/close_table.py"
      offPlaceText = "from dataclasses import dataclass\n\n@dataclass\nclass CloseTableInput:\n    table_id: str\n\nclass CloseTable:\n    def __init__(self, bookings):\n        self.bookings = bookings\n\n    def handle(self, input):\n        return self.bookings.close(input.table_id)\n"
      withModel ports dir answer act = do
        config <- TIO.readFile (dir </> ".olifant/config.toml")
        TIO.writeFile (dir </> ".olifant/config.toml") (config <> "\n[recognize]\nmodel = \"local\"\n\n[model]\ndefault = \"local\"\n\n[model.backends.local]\nkind = \"openai-compatible\"\nurl = \"http://127.0.0.1:9/v1\"\nmodel = \"m\"\n")
        let answers = dir </> "answers.json"
        TIO.writeFile answers answer
        fst <$> inProcessWithConfig [("recognize_answers", String (T.pack answers))] ports dir (act answers)
      pushed = customNotification (Proxy :: Proxy "olifant/suggestion") >>= \case
        NotMess (TNotificationMessage _ _ v) -> pure (v :: Value)
      -- typing (a partial change: what the suggest job reacts to)
      typeIn doc v text = sendNotification SMethod_TextDocumentDidChange (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier (doc ^. L.uri) v) [TextDocumentContentChangeEvent (InL (TextDocumentContentChangePartial (Range (Position 0 0) (Position 0 0)) Nothing text))])
      -- recognize_answers: the alias of decide_answers; the decider's form
      yes = "{\"answers\": {\"recognize\": {\"choice\": \"python-htmx/command\", \"fields\": {}}}}"
  it "(h) an unsure recognition is pushed at once, then upgraded when the model says yes" $
    withPython [] $ \ports dir -> do
      (first, second) <- withModel ports dir yes $ \_ -> do
        doc <- openNew offPlace ""
        typeIn doc 1 offPlaceText
        (,) <$> pushed <*> pushed
      (lookupKey "band" first, lookupKey "band" second) `shouldBe` (Just (String "hint"), Just (String "offer"))
      lookupKey "version" second `shouldBe` lookupKey "version" first
  it "(n) asked after the model upgraded it, olifant/suggest answers the offer (on any line of the file)" $
    withPython [] $ \ports dir -> do
      v <- withModel ports dir yes $ \_ -> do
        doc <- openNew offPlace ""
        typeIn doc 1 offPlaceText
        _ <- pushed
        _ <- pushed
        suggestAt doc 9
      lookupKey "band" v `shouldBe` Just (String "offer")
  it "(o) a generator quieted by three dismissals stays a hint, even when the model says yes (review I2)" $
    withPython [] $ \ports dir -> do
      let file n = "src/bistro/booking/commands/close_" <> n <> ".py"
      v <- withModel ports dir yes $ \_ -> do
        forM_ ["a", "b", "c"] $ \n -> do
          doc <- openNew (file n) (named ("Close" <> T.toUpper (T.pack n)))
          _ <- suggestAt doc 1
          feedback doc "python-htmx/command" "dismissed"
        d <- openNew (file "d") ""
        typeIn d 1 (named "CloseD")
        _ <- pushed
        liftIO (threadDelay 1500000)
        suggestAt d 1
      lookupKey "band" v `shouldBe` Just (String "hint")
  it "(p) accepting a recognition off its place says the user's file stays (review I4)" $
    withPython [] $ \ports dir -> do
      v <- withModel ports dir yes $ \_ -> do
        doc <- openNew offPlace ""
        typeIn doc 1 offPlaceText
        _ <- pushed
        pushed
      lookupKey "summary" v `shouldSatisfy` maybe False (\case String t -> "close_table.py here stays" `T.isInfixOf` t; _ -> False)
  it "(i) typing on cancels a model still answering (its token trips)" $
    withPython [] $ \ports dir -> do
      stopped <- withModel ports dir "wait" $ \answers -> do
        doc <- openNew offPlace ""
        typeIn doc 1 offPlaceText
        _ <- pushed
        liftIO (threadDelay 200000)
        typeIn doc 2 "# more\n"
        liftIO (timeout 5000000 (let loop = doesFileExist (answers <> ".cancelled") >>= \d -> if d then pure () else threadDelay 50000 >> loop in loop))
      stopped `shouldBe` Just ()
  it "(j) with no [recognize] model, the model is never asked" $
    withPython [] $ \ports dir -> do
      let answers = dir </> "answers.json"
      TIO.writeFile answers yes
      (band, _) <- inProcessWithConfig [("recognize_answers", String (T.pack answers))] ports dir $ do
        doc <- openNew offPlace ""
        typeIn doc 1 offPlaceText
        v <- pushed
        liftIO (threadDelay 500000)
        pure (lookupKey "band" v)
      band `shouldBe` Just (String "hint")
      doesFileExist (answers <> ".asked") `shouldReturn` False
  it "(k) never recognises a file the project already had, even with a companion to add" $
    withPython [] $ \ports dir -> do
      removeFile (dir </> "src/bistro/booking/commands/create_booking__test.py")
      (v, _) <- inProcessWith ports dir $ do
        doc <- openDoc "src/bistro/booking/commands/create_booking.py" "python"
        changeDoc doc [TextDocumentContentChangeEvent (InL (TextDocumentContentChangePartial (Range (Position 0 0) (Position 0 0)) Nothing "# edited\n"))]
        suggestAt doc 1
      v `shouldBe` Aeson.Null
  it "(q) under git, an untracked file already on disk when the server started is new: recognised" $
    withPython [(newCommand, ownWay)] $ \ports dir -> do
      (v, _) <- inProcessWith ports {pTracked = \_ -> pure (Just (S.fromList ["src/bistro/booking/commands/create_booking.py"]))} dir $ do
        doc <- openDoc newCommand "python"
        changeDoc doc [TextDocumentContentChangeEvent (InL (TextDocumentContentChangePartial (Range (Position 3 0) (Position 3 0)) Nothing "\n"))]
        suggestAt doc 1
      lookupKey "kind" v `shouldBe` Just (String "recognition")
  it "(r) under git, a file it tracks is never recognised, whenever it was opened" $
    withPython [] $ \ports dir -> do
      removeFile (dir </> "src/bistro/booking/commands/create_booking__test.py")
      (v, _) <- inProcessWith ports {pTracked = \_ -> pure (Just (S.fromList ["src/bistro/booking/commands/create_booking.py"]))} dir $ do
        doc <- openDoc "src/bistro/booking/commands/create_booking.py" "python"
        changeDoc doc [TextDocumentContentChangeEvent (InL (TextDocumentContentChangePartial (Range (Position 0 0) (Position 0 0)) Nothing "# edited\n"))]
        suggestAt doc 1
      v `shouldBe` Aeson.Null
  it "(l) keeps the index a recognition built: the next edit does not read the project again" $
    withPython [] $ \ports dir -> do
      (st, _) <- inProcessWith ports dir $ do
        doc <- openNew newCommand ownWay
        _ <- suggestAt doc 1
        debugState
      lookupKey "recognitionIndexes" st `shouldBe` Just (Number 1)
  it "(m) after a save, the index and the listing it keeps are current again (review I1)" $
    withPython [] $ \ports dir -> do
      (st, _) <- inProcessWith ports dir $ do
        doc <- openNew newCommand ownWay
        _ <- suggestAt doc 1
        other <- openDoc "src/bistro/booking/commands/create_booking.py" "python"
        sendNotification SMethod_TextDocumentDidSave (DidSaveTextDocumentParams other Nothing)
        changeNew doc 1 (ownWay <> "# 1\n")
        _ <- suggestAt doc 1
        changeNew doc 2 (ownWay <> "# 2\n")
        _ <- suggestAt doc 1
        debugState
      (lookupKey "recognitionFresh" st, lookupKey "listingsFresh" st) `shouldBe` (Just (Number 1), Just (Number 1))
  sequential . it "(e) a long legacy file: recognition reads only around the edit, and answers fast (Review Focus 3)" $
    withPython [] $ \ports dir -> do
      let legacy = T.concat ["def f_" <> T.pack (show i) <> "(x):\n    return x + " <> T.pack (show i) <> "\n\n" | i <- [1 .. 1000 :: Int]]
      (times, _) <- inProcessWith ports dir $ do
        doc <- openNew "src/bistro/legacy.py" legacy
        forM [1 .. 5 :: Int] $ \_ -> do
          t0 <- liftIO getCurrentTime
          _ <- suggestAt doc 1500
          t1 <- liftIO getCurrentTime
          pure (realToFrac (diffUTCTime t1 t0) * 1000 :: Double)
      sort times !! 2 `shouldSatisfy` (< 100)

-- | Anticipation (Jev in olifant): asked after the deterministic push; a
-- sure answer pushed as an upgrade for the version that asked (recorded
-- answers stand in for Jev).
anticipationSpec :: Spec
anticipationSpec = describe "anticipation: asked after the deterministic push, pushed as an upgrade" $ do
  let command = "src/bistro/booking/commands/reschedule_booking.py"
      sure = "{\"answers\": {\"anticipate\": {\"choice\": \"python-htmx/command\", \"confidence\": 0.97}}}"
      stepLog dir = dir </> ".olifant/state/steps.log"
      -- the project asks Jev ([decide] model), answered from a file
      withJev ports dir answer act = do
        config <- TIO.readFile (dir </> ".olifant/config.toml")
        TIO.writeFile (dir </> ".olifant/config.toml") (config <> "\n[decide]\nmodel = \"jev\"\n")
        let answers = dir </> "answers.json"
        TIO.writeFile answers answer
        r <- fst <$> inProcessWithConfig [("decide_answers", String (T.pack answers))] ports dir (act answers)
        -- the step log's anticipate lines, without their time (read once the server is gone)
        found <- doesFileExist (stepLog dir)
        lines' <- if found then T.lines <$> TIO.readFile (stepLog dir) else pure []
        pure (r, [T.intercalate "\t" (drop 1 fs) | l <- lines', let fs = T.splitOn "\t" l, take 1 (drop 1 fs) == ["anticipate"]])
      openNew path = do
        uri <- getDocUri path
        sendNotification SMethod_TextDocumentDidOpen (DidOpenTextDocumentParams (TextDocumentItem uri "python" 0 ""))
        pure (TextDocumentIdentifier uri)
      typeIn doc v text = sendNotification SMethod_TextDocumentDidChange (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier (doc ^. L.uri) v) [TextDocumentContentChangeEvent (InL (TextDocumentContentChangePartial (Range (Position 0 0) (Position 0 0)) Nothing text))])
      feedback doc generator outcome =
        sendNotification (SMethod_CustomMethod (Proxy :: Proxy "olifant/recognitionFeedback")) (object ["textDocument" .= object ["uri" .= (doc ^. L.uri)], "generator" .= (generator :: Text), "outcome" .= (outcome :: Text)])
      -- until a file appears (polled by its existence: never read while written)
      appeared path = liftIO . void . timeout 5000000 $
        let loop = doesFileExist path >>= \d -> if d then pure () else threadDelay 50000 >> loop in loop
      suggestion = customNotification (Proxy :: Proxy "olifant/suggestion") >>= \case
        NotMess (TNotificationMessage _ _ v) -> pure (v :: Value)
      -- every olifant/suggestion pushed so far, in order
      pushes = do
        rid <- sendRequest (SMethod_CustomMethod (Proxy :: Proxy "olifant/debugState")) Aeson.Null
        let await seen = (responseForId (SMethod_CustomMethod (Proxy :: Proxy "olifant/debugState")) rid >> pure (reverse seen)) <|> (suggestion >>= \v -> await (v : seen)) <|> (anyMessage >> await seen)
        await []
      offerFor v sg = (lookupKey "version" sg, lookupKey "kind" sg, lookupKey "generator" sg, lookupKey "band" sg) == (Just (Number v), Just (String "recognition"), Just (String "python-htmx/command"), Just (String "offer"))
      -- the deterministic push: nothing, or a hint
      deterministic = all (\sg -> lookupKey "band" sg == Just (String "hint"))
      -- an answer under the gate, typed at a silence
      unsureBy answer = withPython [] $ \ports dir ->
        withJev ports dir answer $ \answers -> do
          doc <- openNew command
          typeIn doc 1 "class RescheduleBooking:"
          appeared (answers <> ".asked")
          liftIO (threadDelay 1000000)
          pushes
  it "(a) a sure answer (0.97) at a silence: pushed after the deterministic one, for the same version" $
    withPython [] $ \ports dir -> do
      (ps, logged) <- withJev ports dir sure $ \_ -> do
        doc <- openNew command
        typeIn doc 1 "class RescheduleBooking:"
        appeared (stepLog dir)
        pushes
      (deterministic (take (length ps - 1) ps), map (offerFor 1) (drop (length ps - 1) ps)) `shouldBe` (True, [True])
      logged `shouldBe` ["anticipate\tpython-htmx/command\t0.97\tshown"]
  it "(b) an unsure answer (0.4): only the deterministic push; logged unsure" $ do
    (ps, logged) <- unsureBy "{\"answers\": {\"anticipate\": {\"choice\": \"python-htmx/command\", \"confidence\": 0.4}}}"
    deterministic ps `shouldBe` True
    logged `shouldBe` ["anticipate\tpython-htmx/command\t0.4\tunsure"]
  it "(b2) an answer without a confidence: only the deterministic push; logged unsure, its confidence -" $ do
    (ps, logged) <- unsureBy "{\"answers\": {\"anticipate\": {\"choice\": \"python-htmx/command\"}}}"
    deterministic ps `shouldBe` True
    logged `shouldBe` ["anticipate\tpython-htmx/command\t-\tunsure"]
  it "(c) an answer landing after the buffer moved on is never pushed: logged stale (Review Focus 1)" $
    withPython [] $ \ports dir -> do
      (ps, logged) <- withJev ports dir "wait" $ \answers -> do
        doc <- openNew command
        typeIn doc 1 "class RescheduleBooking:"
        appeared (answers <> ".asked")
        typeIn doc 2 "# note\n"
        liftIO (threadDelay 500000)
        -- by a rename: the decider polls the file, and GHC refuses to write one being read
        liftIO (TIO.writeFile (answers <> ".new") sure >> renameFile (answers <> ".new") answers)
        appeared (stepLog dir)
        liftIO (threadDelay 300000)
        pushes
      filter (offerFor 1) ps `shouldBe` []
      logged `shouldBe` ["anticipate\tpython-htmx/command\t0.97\tstale"]
  it "(d) a generator quieted by three dismissals is not pushed: logged quiet (Review Focus 4)" $
    withPython [] $ \ports dir -> do
      (ps, logged) <- withJev ports dir sure $ \_ -> do
        forM_ ["a", "b", "c"] $ \n -> do
          other <- openNew ("src/bistro/booking/commands/close_" <> n <> ".py")
          feedback other "python-htmx/command" "dismissed"
        doc <- openNew command
        typeIn doc 1 "class RescheduleBooking:"
        appeared (stepLog dir)
        liftIO (threadDelay 300000)
        pushes
      filter (offerFor 1) ps `shouldBe` []
      logged `shouldBe` ["anticipate\tpython-htmx/command\t0.97\tquiet"]
  it "(e) the same prefix again, after an edit elsewhere: answered from the memo, Jev asked once" $
    withPython [] $ \ports dir -> do
      ((ps, asked), logged) <- withJev ports dir sure $ \answers -> do
        doc <- openNew command
        typeIn doc 1 "class RescheduleBooking:"
        appeared (stepLog dir)
        typeIn doc 2 "import os\n\n"
        liftIO (threadDelay 1000000)
        ps <- pushes
        asked <- liftIO (TIO.readFile (answers <> ".asked"))
        pure (ps, asked)
      asked `shouldBe` "anticipate\n"
      (any (offerFor 1) ps, any (offerFor 2) ps) `shouldBe` (True, True)
      logged `shouldBe` replicate 2 "anticipate\tpython-htmx/command\t0.97\tshown"
