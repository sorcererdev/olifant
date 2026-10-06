{-# LANGUAGE DataKinds #-}

module Olifant.LspSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, void, when)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BSL8
import System.IO (Handle, hFlush)
import Data.Aeson (Value (Array, Bool, Number, Object, String), object, toJSON, (.=))
import Data.Int (Int32)
import Data.Map.Strict qualified as M
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Functor ((<&>))
import Control.Lens ((%~), (&), (^.))
import Language.LSP.Protocol.Lens qualified as L
import System.Exit (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode)
import Data.Text.IO qualified as TIO
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf, sort)
import Data.Time (UTCTime (..), diffUTCTime, fromGregorian, getCurrentTime)
import Language.LSP.Protocol.Message (FromServerMessage, FromServerMessage' (..), LspId (..), SMethod (..), TCustomMessage (..), TNotificationMessage (..), TRequestMessage (..), TResponseMessage (..))
import Data.Text.Encoding qualified as TE
import Language.LSP.Protocol.Types
import Language.LSP.Test
import Olifant.App (Ports (..))
import Olifant.Language (CheckReport (..), Checker (..), Syntax (..), noChecker, noSyntax, processCheckerWith, processVerifierCancellable)
import Olifant.Generation (HoleStatus (..), Plan (..), PlannedHole (..))
import Olifant.Server (ServerConfig (..), ServerState (..), actionPlanWith, describeProgress, fixPlan, newServerState)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Olifant.Language.Check (CheckFailure (..), noVerifier)
import Olifant.Process (cancelled)
import Olifant.Lsp
import Olifant.Doctor (DoctorPorts (..))
import Olifant.Workspace (Workspace (listFiles, readText), diskWorkspace)
import System.Environment (lookupEnv)
import System.Directory (findExecutable, canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, listDirectory, removeDirectory, removeFile)
import System.FilePath (makeRelative, normalise, takeDirectory, (</>))
import Olifant.Language.AstGrep (astGrepSyntax)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (createPipe)
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
  holesSpec
  pickerSpec
  conventionsSpec
  scaleSpec
  concurrencySpec
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
      let throwing = Syntax (\_ _ _ -> ioError (userError "the disk went away"))
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
        doc <- openDoc test "python"
        getCompletions doc afterTest
      case [i | i <- items, i ^. L.label == "Test case"] of
        item : _ -> do
          item ^. L.insertTextFormat `shouldBe` Just InsertTextFormat_Snippet
          let body = case item ^. L.textEdit of
                Just (InL e) -> e ^. L.newText
                Just (InR e) -> e ^. L.newText
                Nothing -> maybe "" id (item ^. L.insertText)
          body `shouldSatisfy` T.isInfixOf "${1:behaviour}"
        [] -> expectationFailure ("no Test case among " <> show (map (^. L.label) items))
  it "(b) offers the snippet anticipated here as an inline completion: unfilled, a snippet" $
    withPython [] $ \ports dir -> do
      (v, _) <- inProcessWith ports dir (openDoc test "python" >>= (`inline` afterTest))
      case firstInsert v of
        Just (Object insert) -> do
          KM.lookup "kind" insert `shouldBe` Just (String "snippet")
          KM.lookup "value" insert `shouldSatisfy` \case Just (String body) -> "= command()" `T.isInfixOf` body; _ -> False
          -- the ghost as the engine draws it, and what to resolve for the filled body
          (firstItem v >>= lookupKey "ghost") `shouldSatisfy` \case Just (Array ls) -> String "def test_\10216behaviour\10217() -> None:" `elem` ls; _ -> False
          (firstItem v >>= lookupKey "data" >>= lookupKey "generator") `shouldBe` Just (String "python-htmx/test-case")
        other -> expectationFailure ("inline completion: " <> show v <> " / " <> show other)
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
  it "(e) answers an inline completion within 150 ms once warm (median of 20)" $
    withPython [] $ \ports dir -> do
      (times, _) <- inProcessWith ports dir $ do
        doc <- openDoc test "python"
        _ <- inline doc afterTest
        forM [1 .. 20 :: Int] $ \_ -> do
          t0 <- liftIO getCurrentTime
          _ <- inline doc afterTest
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
      s0 <- newServerState ports {pVerifier = (`processVerifierCancellable` 120)}
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
scaleSpec = describe "a plain client; latency at 5,000 files" $ do
  it "(d) serves a client that knows only standard LSP: diagnostics, a quickfix, a refactor, a completion" $
    withPython [("src/bistro/booking/commands/close_table.py", "import sqlite3\n\n\ndef f() -> None:\n    pass\n")] $ \ports dir -> do
      ((ds, fixes, refactors, completions), _) <- inProcessWith ports dir $ do
        bad <- openDoc "src/bistro/booking/commands/close_table.py" "python"
        ds <- pullDiagnostics bad
        fixes <- getCodeActions bad (Range (Position 0 0) (Position 0 5))
        query <- openDoc "src/bistro/booking/queries/get_day_schedule.py" "python"
        refactors <- getCodeActions query (Range (Position 18 30) (Position 18 30))
        test <- openDoc "src/bistro/booking/commands/create_booking__test.py" "python"
        completions <- getCompletions test (Position 34 0)
        resolved <- case [a | InR a <- refactors, a ^. L.kind == Just CodeActionKind_RefactorRewrite, maybe False (\d -> lookupKey "missing" d == Just (Array mempty)) (a ^. L.data_)] of
          a : _ -> Just <$> resolveCodeAction a
          [] -> pure Nothing
        pure (ds, [a | InR a <- fixes, a ^. L.kind == Just CodeActionKind_QuickFix], fmap (^. L.edit) resolved, completions)
      (null ds, null fixes, refactors, null completions) `shouldSatisfy` (\(a, b, c, d) -> not a && not b && maybe False (/= Nothing) c && not d)
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
              slowSyntax = Syntax (\l q t -> hold >> querySyntax (pSyntax ports) l q t)
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
  it "answers other requests while a slow resolve (a 2 s check) runs" $ do
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
withPythonChecked changed act = do
  strict <- (== Just "1") <$> lookupEnv "OLIFANT_REQUIRE_TOOLS"
  findExecutable "uv" >>= \case
    Nothing -> if strict then expectationFailure "required tool missing: uv" else pendingWith "uv is not installed"
    Just _ -> withPython changed (\ports dir -> act ports {pChecker = processCheckerWith} dir)

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
plainPorts = Ports (diskWorkspace "/") noSyntax (const noChecker) (const noVerifier) (pure (UTCTime (fromGregorian 2026 10 6) 0)) Nothing Nothing (DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False))

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
