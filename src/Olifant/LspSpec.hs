{-# LANGUAGE DataKinds #-}

module Olifant.LspSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM, forM_)
import Data.Aeson (Value (Array, Bool, Object, String), object, toJSON, (.=))
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
import Data.Time (UTCTime (..), fromGregorian)
import Language.LSP.Protocol.Message (SMethod (..), TResponseMessage (..))
import Language.LSP.Protocol.Types
import Language.LSP.Server (runServerWithHandles)
import Language.LSP.Test
import Olifant.App (Ports (..))
import Olifant.Language (noChecker, noSyntax)
import Olifant.Language.Check (noVerifier)
import Olifant.Lsp
import Olifant.Workspace (diskWorkspace)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, listDirectory, removeDirectory, removeFile)
import System.FilePath (makeRelative, takeDirectory, (</>))
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

-- | The versions an action's edit is for, in one document.
editVersionsOf :: Uri -> CodeAction -> [Int32 |? Null]
editVersionsOf uri a = [te ^. L.textDocument . L.version | Just e <- [a ^. L.edit], Just changes <- [e ^. L.documentChanges], InL te <- changes, te ^. L.textDocument . L.uri == uri]

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
plainPorts = Ports (diskWorkspace "/") noSyntax noChecker noVerifier (pure (UTCTime (fromGregorian 2026 10 6) 0)) Nothing Nothing

inProcessWith :: Ports -> FilePath -> Session a -> IO (a, Maybe Int)
inProcessWith ports dir session = do
  (inRead, inWrite) <- createPipe
  (outRead, outWrite) <- createPipe
  definition <- serverDefinition ports
  done <- newEmptyMVar
  _ <- forkIO (runServerWithHandles mempty mempty inRead outWrite definition >>= putMVar done)
  a <- runSessionWithHandles inWrite outRead defaultConfig {lspConfig = KM.fromList [("debug", Bool True)]} fullLatestClientCaps dir session
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
