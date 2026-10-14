{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.ServerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, takeMVar, tryPutMVar)
import Control.Monad (forM, void)
import Data.Aeson (Value (..), (.:))
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as BL
import Data.Char (isAlphaNum, isDigit)
import Data.Maybe (listToMaybe)
import Data.Map.Strict qualified as M
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Olifant.Block (Block (..), BlockState (Accepted, Drafting, Green, Judged, Red, Rejected), Draft (..), hud, open, survivors)
import Olifant.Block qualified as Block
import Olifant.Mutate (Mutant (..))
import Olifant.Language (Failure (..), Match (..), Rewrite (..), Runner (..), Syntax (..), TestRun (..))
import System.Directory (canonicalizePath, doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, (</>))
import Control.Concurrent.Async (async, replicateConcurrently_, wait)
import Data.Set qualified as S
import Olifant.Server.State qualified as St
import Olifant.Server.Block (settle)
import Olifant.Server.BlockMoves (rejectDeleted)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Olifant.Decide qualified as De
import Olifant.Fill (FillRequest (..), Filler (..), ModelFiller (..))
import System.IO.Temp (withSystemTempDirectory)
import Data.List (isSuffixOf)
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (catalogRulesConfig)
import Olifant.Doctor (DoctorPorts (..))
import Olifant.Language (CheckReport (..), Checker (..), noChecker, noRunner, noSyntax)
import Olifant.Language.Check (noVerifier)
import Olifant.Process (cancelled, gitStateStamp, newCancel)
import Olifant.Rules (RulesConfig (..), Severity (..))
import Olifant.Server
import Olifant.Workspace (Workspace (..), memoryWorkspace)
import Data.Text qualified as T
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = describe "Olifant.Server (projects across roots, open documents, jobs)" $ do
  it "(a) routes each file to its nearest project; a nested project wins over its parent" $ do
    (s0, _) <- server
    (s1, a) <- projectFor "/r1/src/x.py" s0
    (s2, b) <- projectFor "/r2/a/src/y.py" s1
    (_, c) <- projectFor "/r2/src/z.py" s2
    (fmap fst a, fmap fst b, fmap fst c) `shouldBe` (Just "/r1", Just "/r2/a", Just "/r2")
  it "(b) shows a project the unsaved text of its own open documents, not another project's" $ do
    (s0, _) <- server
    let s1 = changeDoc "/r1/src/x.py" 2 "unsaved" (openDoc "/r1/src/x.py" 1 "x = 1\n" s0)
    readText (workspaceOf s1 "/r1") "src/x.py" `shouldReturn` Just "unsaved"
    readText (workspaceOf s1 "/r2") "../r1/src/x.py" `shouldReturn` Just "x = 1\n"
    (_, Just (_, p)) <- projectFor "/r1/src/x.py" s1
    readText (prWorkspace p) "src/x.py" `shouldReturn` Just "unsaved"
  it "(c) loads a project only when one of its files is asked about" $ do
    (s0, configReads) <- server
    let s1 = openDoc "/r1/src/x.py" 1 "x" s0
    configReads `shouldReturn` 0
    (s2, _) <- projectFor "/r1/src/x.py" s1
    loaded <- configReads
    loaded `shouldSatisfy` (> 0)
    _ <- projectFor "/r1/src/other.py" s2
    configReads `shouldReturn` loaded
  it "(d) keeps each project's own conventions: a rule's severity differs per project" $ do
    (s0, _) <- server
    (s1, Just (_, p1)) <- projectFor "/r1/src/x.py" s0
    (_, Just (_, p2)) <- projectFor "/r2/src/z.py" s1
    lookup "commands-use-ports" (rcSeverity (catalogRulesConfig (prCatalog p1))) `shouldBe` Just Off
    lookup "commands-use-ports" (rcSeverity (catalogRulesConfig (prCatalog p2))) `shouldBe` Just Error
  it "(e) gives a file in no project nothing (Review Focus 2)" $ do
    (s0, _) <- server
    (_, none) <- projectFor "/elsewhere/README.md" s0
    fmap fst none `shouldBe` Nothing
  it "(f) keeps the last good project when its config breaks, and recovers (Review Focus 3)" $ do
    (s0, _) <- serverWith (\ws -> pure ws)
    (s1, Just _) <- projectFor "/r1/src/x.py" s0
    writeText (portsFiles s1) "/r1/.olifant/config.toml" "[rules\nbroken"
    s2 <- catalogChanged "/r1" s1
    case slotOf "/r1" s2 of
      Just (Broken why (Just _)) -> why `shouldSatisfy` (/= "")
      other -> expectationFailure ("expected Broken with the last good project, got " <> describeSlot other)
    (_, stillThere) <- projectFor "/r1/src/x.py" s2
    fmap fst stillThere `shouldBe` Just "/r1"
    writeText (portsFiles s2) "/r1/.olifant/config.toml" "[rules]\ncommands-use-ports = \"warn\"\n"
    s3 <- catalogChanged "/r1" s2
    case slotOf "/r1" s3 of
      Just (Loaded p) -> lookup "commands-use-ports" (rcSeverity (catalogRulesConfig (prCatalog p))) `shouldBe` Just Warn
      other -> expectationFailure ("expected Loaded, got " <> describeSlot other)
  it "(g) reads disk again once a document is closed (Review Focus 5)" $ do
    (s0, _) <- server
    let s1 = closeDoc "/r1/src/x.py" (openDoc "/r1/src/x.py" 1 "unsaved" s0)
    writeText (portsFiles s1) "/r1/src/x.py" "changed on disk"
    readText (workspaceOf s1 "/r1") "src/x.py" `shouldReturn` Just "changed on disk"
  it "(h) a newer job with the same key cancels the older; other keys run on" $ do
    jobs <- newJobs
    ran <- newIORef ([] :: [Text])
    let slow name token = do
          let loop n = cancelled token >>= \stop -> if stop || n > (100 :: Int) then pure stop else threadDelay 10000 >> loop (n + 1)
          stop <- loop 0
          record (name <> if stop then " cancelled" else " finished")
        -- jobs run on other threads at once: append atomically
        record x = atomicModifyIORef' ran (\xs -> (xs ++ [x], ()))
    first' <- submit jobs "check /r1/x.py" 0 (slow "first")
    threadDelay 20000
    second <- submit jobs "check /r1/x.py" 0 (\_ -> record "second")
    other <- submit jobs "check /r2/y.py" 0 (\_ -> record "other")
    mapM_ wait [first', second, other]
    readIORef ran >>= (`shouldMatchList` ["first cancelled", "second", "other"])
  it "turns a plan's file into the smallest change of whole lines, which gives the new text (any texts)" $ hedgehog $ do
    let line = Gen.element ["", "a", "b", "def f():", "    x = 1", "é ✓"]
        text = T.intercalate "\n" <$> Gen.list (Range.linear 0 8) line
    old <- forAll text
    new <- forAll text
    let (range, replacement) = smallestChange old new
    applyChange range replacement old === new
  it "keeps the lines a change shares at both ends" $
    smallestChange "a\nb\nc\n" "a\nB\nc\n" `shouldBe` (((1, 0), (2, 0)), "B\n")
  it "(h) a debounced job superseded before it starts never runs" $ do
    jobs <- newJobs
    ran <- newIORef ([] :: [Text])
    a <- submit jobs "suggest /r1/x.py" 50 (\_ -> modifyIORef' ran (++ ["a"]))
    b <- submit jobs "suggest /r1/x.py" 50 (\_ -> modifyIORef' ran (++ ["b"]))
    (,) <$> wait a <*> wait b `shouldReturn` (Nothing, Just ())
    readIORef ran `shouldReturn` ["b"]
  blockSpecs
  testsSpecs

-- | `olifant/tests/run`: a file's tests, run whole over the editor's text.
testsSpecs :: Spec
testsSpecs = describe "a file's tests (olifant/tests/run)" $ do
  let recording verdict = blocksRun blockSyntax (pure ()) (const verdict)
      passing _ = Right (TestRun [testId] [] Nothing)
      -- a language of the pack's with no [test] table
      plain = ("/pack/languages/plain.toml", "files = [\"**/*.txt\"]\ncomment = \"#\"\ngrammar = \"python\"\n")
  it "on a module, runs its test file, as one id" $ do
    seen <- newIORef ([] :: [[Text]])
    (s, _) <- recording passing []
    let s' = s {ssPorts = (ssPorts s) {pTester = \c -> Runner (\l ids f -> modifyIORef' seen (++ [ids]) >> runTests (pTester (ssPorts s) c) l ids f)}}
    (_, Right a) <- testsRun (root </> moduleRel) s'
    taFile a `shouldBe` root </> testRel
    readIORef seen `shouldReturn` [[T.pack testRel]]
    trPassed (taRun a) `shouldBe` [testId]
    taSeconds a `shouldSatisfy` (>= 0)
  it "answers the run's wall time in seconds" $ do
    (s, _) <- blocksRun blockSyntax (threadDelay 50000) (\_ _ -> Right (TestRun [testId] [] Nothing)) []
    (_, Right a) <- testsRun (root </> moduleRel) s
    taSeconds a `shouldSatisfy` (>= 0.05)
  it "on a test file, runs that file" $ do
    (s, _) <- recording passing []
    (_, Right a) <- testsRun (root </> testRel) s
    taFile a `shouldBe` root </> testRel
  it "runs the editor's text: an open, unsaved test file is what runs" $ do
    (s, _) <- recording (\files -> if "unsaved" `T.isInfixOf` M.findWithDefault "" testRel files then Right (TestRun [testId] [] Nothing) else Right (failing "saved text ran")) []
    (_, Right a) <- testsRun (root </> testRel) (openDoc (root </> testRel) 1 (testText <> "# unsaved\n") s)
    trFailed (taRun a) `shouldBe` []
  it "runs the editor's text: an open, unsaved module is what its tests run over" $ do
    (s, _) <- recording (\files -> if "unsaved" `T.isInfixOf` M.findWithDefault "" moduleRel files then Right (TestRun [testId] [] Nothing) else Right (failing "saved text ran")) []
    (_, Right a) <- testsRun (root </> moduleRel) (openDoc (root </> moduleRel) 1 (stubbed <> "# unsaved\n") s)
    trFailed (taRun a) `shouldBe` []
  it "on a test file whose module is gone, runs that file" $ do
    seen <- newIORef ([] :: [[Text]])
    (s, _) <- blocksRun blockSyntax (pure ()) (\ids _ -> Right (TestRun ids [] Nothing)) [("src/shop/orphan__test.py", "def test_alone() -> None:\n    assert True\n")]
    let s' = s {ssPorts = (ssPorts s) {pTester = \c -> Runner (\l ids f -> modifyIORef' seen (++ [ids]) >> runTests (pTester (ssPorts s) c) l ids f)}}
    (_, Right a) <- testsRun (root </> "src/shop/orphan__test.py") s'
    taFile a `shouldBe` root </> "src/shop/orphan__test.py"
    readIORef seen `shouldReturn` [["src/shop/orphan__test.py"]]
  it "refuses a language whose tests_for names no file, in one sentence" $ do
    (s, _) <- recording passing [("/pack/languages/blank.toml", "files = [\"**/*.md\"]\ncomment = \"#\"\ngrammar = \"python\"\n[test]\npreset = \"pytest\"\ntests_for = \"\"\n"), ("notes/a.md", "a\n")]
    (_, answer) <- testsRun (root </> "notes/a.md") s
    answer `shouldSatisfy` either (== "blank's tests_for names no test file: nothing to run") (const False)
  it "refuses a language with no [test] table, in one sentence" $ do
    (s, _) <- recording passing [plain, ("notes/a.txt", "a\n")]
    (_, answer) <- testsRun (root </> "notes/a.txt") s
    answer `shouldSatisfy` either (== "plain has no [test] table: nothing to run") (const False)
  it "refuses a module with no test file yet, naming the file it would be" $ do
    (s, _) <- recording passing [("src/shop/lonely.py", "def alone() -> int:\n    return 1\n")]
    (_, answer) <- testsRun (root </> "src/shop/lonely.py") s
    answer `shouldSatisfy` either (== "no test file for this module yet: src/shop/lonely__test.py") (const False)
  it "refuses a file in no project" $ do
    (s, _) <- recording passing []
    (_, answer) <- testsRun "/elsewhere/x.py" s
    answer `shouldSatisfy` either ("is in no Olifant project" `T.isInfixOf`) (const False)
  it "answers a failure with its line, message and output, as the editor reads it" $ do
    (s, _) <- recording (\_ -> Right (TestRun [] [(testId, Failure (Just 6) "assert 0 == 1" "E   assert 0 == 1")] Nothing)) []
    (_, Right a) <- testsRun (root </> moduleRel) s
    let json = testsJson a
        failed = case json of
          Object o | Just (Array xs) <- KM.lookup "failed" o -> toList xs
          _ -> []
    lookupIn "file" json `shouldBe` Just (String (T.pack (root </> testRel)))
    lookupIn "passed" json `shouldBe` Just (Array mempty)
    lookupIn "broken" json `shouldBe` Just Null
    failed `shouldBe` [J.object ["id" J..= testId, "line" J..= (6 :: Int), "message" J..= ("assert 0 == 1" :: Text), "output" J..= ("E   assert 0 == 1" :: Text)]]
    lookupIn "seconds" json `shouldSatisfy` \case Just (Number _) -> True; _ -> False
  it "answers a run whose tests could not run as broken, with the reason" $ do
    (s, _) <- recording (\_ -> Right (TestRun [] [] (Just "ImportError: no module named shop"))) []
    (_, Right a) <- testsRun (root </> moduleRel) s
    lookupIn "broken" (testsJson a) `shouldBe` Just (String "ImportError: no module named shop")
  it "answers a runner that cannot start as broken, with why" $ do
    (s, _) <- recording (\_ -> Left "uv is not installed") []
    (_, Right a) <- testsRun (root </> moduleRel) s
    trBroken (taRun a) `shouldBe` Just "uv is not installed"
  where
    lookupIn k = \case
      Object o -> KM.lookup (K.fromText k) o
      _ -> Nothing

-- | The loop's blocks over a tiny Python project (the real python-htmx
-- pack, in memory), a scripted test runner and a syntax that knows the one
-- container and the one boundary site the cases need.
blockSpecs :: Spec
blockSpecs = describe "blocks" $ do
  it "(a) begin on a red test finds the function under test and starts Red" $ do
    (s, _) <- blocks redOnStub []
    (_, Right b) <- blockBegin ignore (root </> testRel) 6 s
    blockFile b `shouldBe` root </> moduleRel
    blockSpan b `shouldBe` (6, 6)
    blockTestId b `shouldBe` testId
    blockState b `shouldSatisfy` \case Red _ -> True; _ -> False
    hud b `shouldSatisfy` ("test_closes_a_table" `T.isInfixOf`)
    fmap fst <$> blockFor (root </> moduleRel) s `shouldReturn` Just b
    steps s >>= (`shouldSatisfy` ("block 1\tbegin\tperson" `T.isInfixOf`))
  it "(a2) the function under test is the first call the module defines: a port's call in an assertion is passed over" $ do
    let ported = T.unlines ["from shop.close_table import CloseTable", "", "", "def test_closes_a_table() -> None:", "    table = CloseTable()", "    assert table.log.entries() == [] and table.handle(4) == 1"]
    (s, _) <- blocks redOnStub [(testRel, ported)]
    (_, Right b) <- blockBegin ignore (root </> testRel) 6 s
    (blockFile b, blockSpan b) `shouldBe` (root </> moduleRel, (6, 6))
  it "(b) a second begin in the same file is refused while one is pending" $ do
    (s, _) <- blocks redOnStub []
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    (_, again) <- blockBegin ignore (root </> moduleRel) 6 s
    again `shouldSatisfy` either ("a magic circle is open" `T.isInfixOf`) (const False)
  it "(I5) on a module with no test file, begin opens the stub as the new file's: the module's import first" $ do
    (s, _) <- blocks redOnStub [("src/shop/lonely.py", "def alone() -> int:\n    return 1\n")]
    (_, Right (tb, Just (file, at, body, edits))) <- blockStart ignore (root </> "src/shop/lonely.py") 2 s
    (file, at, edits) `shouldBe` (root </> "src/shop/lonely__test.py", 1, [])
    take 4 (T.lines body) `shouldBe` ["from shop.lonely import alone", "", "", "def test_${1:name}() -> None:"]
    (blockFile tb, blockKind tb, blockSpan tb, blockBase tb) `shouldBe` (file, Block.TestFor Nothing, (4, 6), "")
    blockState tb `shouldSatisfy` \case Red _ -> True; _ -> False
    fmap fst <$> blockFor file s `shouldReturn` Just tb
    -- one block per file: the function cannot begin again meanwhile
    (_, again) <- blockBegin ignore (root </> "src/shop/lonely.py") 2 s
    again `shouldSatisfy` either ("a magic circle is open" `T.isInfixOf`) (const False)
  it "(I5) begin on a function no test calls: the stub at the end of its test file, drafted red at its assertion, accepted; then the function begins on it" $ do
    (s0, _) <- blocksOn killedBy []
    let module' = written <> T.unlines ["", "", "class Reopen:", "    def again(self) -> int:", "        return 0"]
        s = (openDoc (root </> moduleRel) 1 module' s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right (tb, Just (testFile, at, body, imports))) <- blockStart ignore (root </> moduleRel) 15 s
    -- the editor imports Reopen (with CloseTable), then expands the stub
    (testFile, at, length imports) `shouldBe` (root </> testRel, 7, 1)
    body `shouldSatisfy` ("def test_${1:name}()" `T.isInfixOf`)
    (blockFile tb, blockKind tb, blockSpan tb) `shouldBe` (testFile, Block.TestFor Nothing, (9, 11))
    -- the person names it and writes its assertion; the arrangement drafted: red at the assertion stands
    let named = T.replace "kills_the_boundary" "reopens_again" (T.replace "table.handle(2) == 0" "Reopen().again() == 1" (expandStub at body (applyEdits imports testText)))
        st = openDoc testFile 1 named s
    (_, Right (drafted, [edit])) <- blockDraft ignore testFile st
    blockState drafted `shouldBe` Green
    -- applied: red at its assertion is a test block's green; judged, accepted
    let st2 = changeDoc testFile 2 (applyChange (feRange edit) (feText edit) named) st
    (_, Just tested) <- blockChanged testFile st2
    blockState tested `shouldBe` Judged []
    (_, Right (done, [])) <- blockAccept testFile st2
    blockState done `shouldBe` Accepted
    -- the function begins on its new red test
    Just (code, _) <- blockFor (root </> moduleRel) st2
    (blockKind code, blockTestId code, blockSpan code) `shouldBe` (Block.Code, T.pack testRel <> "::test_reopens_again", (15, 15))
    blockState code `shouldSatisfy` \case Red _ -> True; _ -> False
  it "(N5) begin on a function no test calls, its test file not importing it: the import is added once, then the stub" $ do
    let reopen = written <> T.unlines ["", "", "class Reopen:", "    def again(self) -> int:", "        return 0"]
        start existing = do
          (s0, _) <- blocksOn killedBy [(testRel, existing)]
          (_, Right (tb, Just (_, at, body, imports))) <- blockStart ignore (root </> moduleRel) 15 (openDoc (root </> moduleRel) 1 reopen s0)
          pure (tb, applyEdits imports existing, expandStub at body (applyEdits imports existing))
    -- the file imports CloseTable only: Reopen joins it (the adapter's import_merge)
    (tb, imported, opened) <- start testText
    imported `shouldBe` T.replace "import CloseTable\n" "import CloseTable, Reopen\n" testText
    length (filter ("import" `T.isInfixOf`) (T.lines opened)) `shouldBe` 1
    T.splitOn "\n" opened !! (fst (blockSpan tb) - 1) `shouldBe` "def test_kills_the_boundary() -> None:"
    blockBase tb `shouldBe` testText
    -- a file that imports it already gets no second import
    let already = T.replace "import CloseTable\n" "import CloseTable, Reopen\n" testText
    (tb2, unchanged, opened2) <- start already
    unchanged `shouldBe` already
    T.splitOn "\n" opened2 !! (fst (blockSpan tb2) - 1) `shouldBe` "def test_kills_the_boundary() -> None:"
    -- an import over several lines (parenthesised) has it already, or gains it inside
    let wrapped names = T.replace "import CloseTable\n" ("import (\n" <> T.concat ["    " <> n <> ",\n" | n <- names] <> ")\n") testText
    (_, kept, _) <- start (wrapped ["CloseTable", "Reopen"])
    kept `shouldBe` wrapped ["CloseTable", "Reopen"]
    (_, joined, _) <- start (wrapped ["CloseTable"])
    joined `shouldBe` wrapped ["CloseTable", "Reopen"]
    -- its last name with no comma: the comma comes with the new name
    (_, commaless, _) <- start (T.replace "    CloseTable,\n" "    CloseTable\n" (wrapped ["CloseTable"]))
    commaless `shouldBe` wrapped ["CloseTable", "Reopen"]
    -- a file with no import yet: it goes first
    (_, top, _) <- start (T.unlines ["def test_other() -> None:", "    assert 1 == 1"])
    T.lines top `shouldBe` ["from shop.close_table import Reopen", "", "def test_other() -> None:", "    assert 1 == 1"]
  it "(I-2) the stub's import is no line of the test block: tested after an edit, its span is the new test's" $ do
    (s0, _) <- blocksOn killedBy []
    let reopen = written <> T.unlines ["", "", "class Reopen:", "    def again(self) -> int:", "        return 0"]
        s = openDoc (root </> moduleRel) 1 reopen s0
    (_, Right (_, Just (testFile, at, body, imports))) <- blockStart ignore (root </> moduleRel) 15 s
    length imports `shouldBe` 1
    -- the editor imports, expands the stub; the person names it and writes its assertion
    let named = T.replace "kills_the_boundary" "reopens_again" (T.replace "table.handle(2) == 0" "Reopen().again() == 1" (expandStub at body (applyEdits imports testText)))
    (_, Just tested) <- blockChanged testFile (openDoc testFile 2 named s)
    let ls = T.splitOn "\n" named
    ls !! (fst (blockSpan tested) - 1) `shouldBe` "def test_reopens_again() -> None:"
    ls !! (snd (blockSpan tested) - 1) `shouldBe` "    assert Reopen().again() == 1"
  it "(I5) a test block's test that passes before any code exists is red" $ do
    (s0, _) <- blocksOn (\files -> Right (TestRun [T.pack testRel <> "::test_reopens_again"] [] Nothing) <* killedBy files) []
    let module' = written <> T.unlines ["", "", "class Reopen:", "    def again(self) -> int:", "        return 0"]
        s = openDoc (root </> moduleRel) 1 module' s0
    (_, Right (_, Just (testFile, at, body, imports))) <- blockStart ignore (root </> moduleRel) 15 s
    let named = T.replace "kills_the_boundary" "reopens_again" (expandStub at body (applyEdits imports testText))
    (_, Just tested) <- blockChanged testFile (openDoc testFile 2 named s)
    blockState tested `shouldSatisfy` \case Red f -> "passes before any code exists" `T.isInfixOf` fMessage f; _ -> False

  it "(c) draft lands only if the document is still the version it planned against (Review Focus 1)" $ do
    (s0, _) <- blocks redOnStub []
    let s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right b) <- blockBegin ignore (root </> testRel) 6 s
    (_, moved) <- blockDraft ignore (root </> moduleRel) s {ssLiveDoc = Just (\_ -> pure (Just (2, stubbed)))}
    either id (const "drafted") moved `shouldBe` "the file changed while drafting; your text stands"
    fmap (unversioned . fst) <$> blockFor (root </> moduleRel) s `shouldReturn` Just (unversioned b)
    (_, Right (b', edits)) <- blockDraft ignore (root </> moduleRel) s {ssLiveDoc = Just (\_ -> pure (Just (1, stubbed)))}
    blockState b' `shouldBe` Green
    map draftBy (blockDrafts b') `shouldBe` ["recorded"]
    blockSpan b' `shouldBe` (6, 6)
    map feVersion edits `shouldBe` [Just 1]
    map feText edits `shouldSatisfy` any ("return int(seats > 2)" `T.isInfixOf`)
    steps s >>= (`shouldSatisfy` ("\tdraft\trecorded\tgreen" `T.isInfixOf`))
  it "(d) a test that cannot run makes the block Broken and skips judging (Review Focus 2)" $ do
    (s, runs) <- blocks (const (TestRun [] [] (Just "ImportError: no module named shop"))) []
    (_, Right b) <- blockBegin ignore (root </> testRel) 6 s
    blockState b `shouldBe` Block.Broken "ImportError: no module named shop"
    open b `shouldBe` ["test", "reject"]
    (_, judged) <- blockJudge (root </> moduleRel) s
    judged `shouldSatisfy` either (const True) (const False)
    runs `shouldReturn` 1
  it "(d2) a runner that cannot start makes the block Broken too, and nothing is judged (Review Focus 2)" $ do
    (s0, runs) <- blocksWith (\t -> if "seats > 2" `T.isInfixOf` t then Left "uv is not installed" else Right (redOnStub t)) []
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s0
    let s = changeDoc (root </> moduleRel) 2 written s0
    (_, Right b) <- blockTest (root </> moduleRel) s
    blockState b `shouldBe` Block.Broken "uv is not installed"
    fmap (blockState . fst) <$> blockFor (root </> moduleRel) s `shouldReturn` Just (Block.Broken "uv is not installed")
    (_, judged) <- blockJudge (root </> moduleRel) s
    judged `shouldSatisfy` either (const True) (const False)
    runs `shouldReturn` 2
    steps s >>= (`shouldSatisfy` ("\ttest\tperson\tcannot run: uv is not installed" `T.isInfixOf`))
  it "(d3) begin on a test file whose run is broken, the cursor on no test: Broken on the first test" $ do
    (s, _) <- blocks (const (TestRun [] [] (Just "ImportError: no module named shop"))) []
    (_, Right b) <- blockBegin ignore (root </> testRel) 1 s
    (blockTestId b, blockState b) `shouldBe` (testId, Block.Broken "ImportError: no module named shop")
  it "(e) with no [model], draft says so and the block stays Red (Review Focus 5)" $ do
    (s, _) <- blocks redOnStub []
    (_, Right b) <- blockBegin ignore (root </> testRel) 6 s
    (_, drafted) <- blockDraft ignore (root </> moduleRel) s
    drafted `shouldSatisfy` either ("no model is configured" `T.isInfixOf`) (const False)
    fmap fst <$> blockFor (root </> moduleRel) s `shouldReturn` Just b
  it "(f) a green test is judged; survivors come from the span's new lines only" $ do
    -- green for the written body and for its boundary mutant (seats >= 2)
    (s0, _) <- blocks redOnStub []
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s0
    let s = changeDoc (root </> moduleRel) 2 written s0
    (_, Right b) <- blockTest (root </> moduleRel) s
    blockSpan b `shouldBe` (6, 6)
    -- `n > 0` on line 10 is a boundary site too, outside the span: no mutant
    map (\m -> (muLine m, muOperator m, muFrom m, muTo m)) (survivors b) `shouldBe` [(6, "boundary", ">", ">=")]
    Just (_, killed) <- blockFor (root </> moduleRel) s
    killed `shouldBe` 0
    let json = blockJson b killed
    field "state" json `shouldBe` Just (String "judged")
    field "killed" json `shouldBe` Just (Number 0)
    log' <- steps s
    log' `shouldSatisfy` ("\ttest\tperson\tgreen" `T.isInfixOf`)
    log' `shouldSatisfy` ("\tjudge\tmutation\t0/1 killed" `T.isInfixOf`)
  it "(g) accept after a clean judge; reject restores the base text; both finish the block and log a step" $ do
    -- the mutant fails the test: killed
    let kills t = if "seats >= 2" `T.isInfixOf` t then failing "assert 0 == 1" else redOnStub t
    (s0, _) <- blocks kills []
    -- a hand-written green body, begun on: judged at once
    let s = openDoc (root </> moduleRel) 1 written s0
    (_, Right b) <- blockBegin ignore (root </> moduleRel) 6 s
    blockState b `shouldBe` Judged []
    (_, Right (done, [])) <- blockAccept (root </> moduleRel) s
    blockState done `shouldBe` Accepted
    blockFor (root </> moduleRel) s `shouldReturn` Nothing
    -- a red block, typed green, then rejected: back to the stub
    let s1 = openDoc (root </> moduleRel) 1 stubbed s0
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s1
    let s2 = changeDoc (root </> moduleRel) 2 written s1
    (_, Right _) <- blockTest (root </> moduleRel) s2
    (_, Right (gone, [edit])) <- blockReject (root </> moduleRel) s2
    blockState gone `shouldBe` Rejected
    (fePath edit, feVersion edit) `shouldBe` (root </> moduleRel, Just 2)
    applyChange (feRange edit) (feText edit) written `shouldBe` stubbed
    blockFor (root </> moduleRel) s2 `shouldReturn` Nothing
    log' <- steps s
    log' `shouldSatisfy` ("block 1\taccept\tperson" `T.isInfixOf`)
    log' `shouldSatisfy` ("block 2\treject\tperson" `T.isInfixOf`)
    log' `shouldSatisfy` ("\tbegin\t" `T.isInfixOf`)
  it "(h) kill opens a test block in the test file with the stub at the end; drafting it is verified both ways" $ do
    (s0, _) <- blocksOn killedBy []
    let s = (openDoc (root </> moduleRel) 1 written s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right judged) <- blockBegin ignore (root </> moduleRel) 6 s
    map muLine (survivors judged) `shouldBe` [6]
    (_, Right (code, testFile, at, body)) <- blockKill (root </> moduleRel) 6 s
    (blockId code, testFile, at) `shouldBe` (blockId judged, root </> testRel, 7)
    -- the stub's test is named from the mutant (`seats > 2` → `seats >= 2`), in the name's tab stop
    body `shouldSatisfy` ("def ${1:test_boundary_int_seats}() -> None:" `T.isInfixOf`)
    Just (tb, 0) <- blockFor testFile s
    let [boundary] = survivors judged
    (blockFile tb, blockSpan tb, blockKind tb) `shouldBe` (testFile, (9, 11), Block.TestFor (Just boundary))
    blockState tb `shouldSatisfy` \case Red _ -> True; _ -> False
    -- the code block is still the module's
    fmap (blockId . fst) <$> blockFor (root </> moduleRel) s `shouldReturn` Just (blockId judged)
    -- the test block's own file has no survivor to stub a test for
    (_, fromTest) <- blockKill testFile 9 s
    either id (const "stubbed") fromTest `shouldBe` "stub a test from the code's magic circle, in its module"
    -- the editor expands the stub; the model arranges; the mutant fails it: it stands
    let st = openDoc testFile 1 (expandStub at body testText) s
    (_, Right (drafted, edits)) <- blockDraft ignore testFile st
    blockState drafted `shouldBe` Green
    map draftBy (blockDrafts drafted) `shouldBe` ["recorded"]
    map fePath edits `shouldBe` [testFile]
    map feText edits `shouldSatisfy` any ("table = CloseTable()" `T.isInfixOf`)
    -- a runner the mutant never fails: the draft does not stand
    (n0, _) <- blocksOn (\files -> if "seats >= 2" `T.isInfixOf` M.findWithDefault "" moduleRel files then Right (TestRun [testId] [] Nothing) else killedBy files) []
    let n = (openDoc (root </> moduleRel) 1 written n0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 n
    (_, Right (_, nf, nat, nbody)) <- blockKill (root </> moduleRel) 6 n
    (_, Right (refused, _)) <- blockDraft ignore nf (openDoc nf 1 (expandStub nat nbody testText) n)
    blockState refused `shouldSatisfy` \case Red f -> "does not notice the mutant" `T.isInfixOf` fMessage f; _ -> False
    steps s >>= (`shouldSatisfy` ("block 1\tkill\tperson\tboundary on line 6" `T.isInfixOf`))
    steps s >>= (`shouldSatisfy` ("block 2\tbegin\tperson" `T.isInfixOf`))
  it "(h2) kill off a survivor's line takes the top-ranked survivor: the stub and the test block are that mutant's; with none left it is refused" $ do
    (s0, _) <- blocksOn killedBy []
    let s = (openDoc (root </> moduleRel) 1 written s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right judged) <- blockBegin ignore (root </> moduleRel) 6 s
    let [boundary] = survivors judged
    (_, Right (code, testFile, at, body)) <- blockKill (root </> moduleRel) 3 s
    (blockId code, testFile, at) `shouldBe` (blockId judged, root </> testRel, 7)
    body `shouldSatisfy` ("def ${1:test_boundary_int_seats}() -> None:" `T.isInfixOf`)
    Just (tb, 0) <- blockFor testFile s
    (blockSpan tb, blockKind tb) `shouldBe` ((9, 11), Block.TestFor (Just boundary))
    steps s >>= (`shouldSatisfy` ("block 1\tkill\tperson\tboundary on line 6" `T.isInfixOf`))
    (w0, _) <- blocks redOnStub []
    let w = openDoc (root </> moduleRel) 1 written w0
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 w
    (_, Right _) <- blockWaive (root </> moduleRel) 6 "inclusive by design" w
    (_, none) <- blockKill (root </> moduleRel) 3 w
    either id (const "killed") none `shouldBe` "no survivor left: accept"
  it "(h3) a kill's stub deleted whole closes its circle: rejected (logged), the deletion stands, and the module stubs again" $ do
    (s0, _) <- blocksOn killedBy []
    let s = openDoc (root </> moduleRel) 1 written s0
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
    (_, Right (_, testFile, at, body)) <- blockKill (root </> moduleRel) 6 s
    -- its HUD names the stub's test and how to leave
    Just (tb, _) <- M.lookup testFile <$> readIORef (ssBlocks s)
    hud tb `shouldBe` "▸ test_boundary_int_seats · assert, then draft · reject drops it"
    -- a second circle in the test file is refused, naming it
    (_, busy) <- blockKill (root </> moduleRel) 6 s
    either id (const "stubbed") busy `shouldBe` "a magic circle is open in src/shop/close_table__test.py: accept or reject it there first"
    -- the editor expands the stub (tested: red on its placeholder); the person types beside it, then deletes the stub
    let stubbed = expandStub at body testText
        mine = T.replace "    table = CloseTable()\n" "    table = CloseTable()  # mine\n" testText
    (_, Just expanded) <- blockChanged testFile (openDoc testFile 1 stubbed s)
    blockState expanded `shouldSatisfy` \case Red _ -> True; _ -> False
    -- the person typed over the stub's name: the HUD (and the block's test) follow it
    hud expanded `shouldSatisfy` ("▸ test_kills_the_boundary · " `T.isPrefixOf`)
    hud expanded `shouldSatisfy` ("· assert, then draft · reject drops it" `T.isSuffixOf`)
    blockTestId expanded `shouldBe` T.pack testRel <> "::test_kills_the_boundary"
    (_, gone) <- blockChanged testFile (changeDoc testFile 2 mine s)
    fmap blockState gone `shouldBe` Just Rejected
    M.member testFile <$> readIORef (ssBlocks s) `shouldReturn` False
    steps s >>= (`shouldSatisfy` ("block 2\treject\tperson\tthe stub was deleted" `T.isInfixOf`))
    -- the code's circle is as it was: a survivor, stubbed again without a refusal
    Just (code, _) <- blockFor (root </> moduleRel) s
    map muLine (survivors code) `shouldBe` [6]
    (_, again) <- blockKill (root </> moduleRel) 6 (changeDoc testFile 2 mine s)
    either id (const "stubbed") again `shouldBe` "stubbed"
  it "(h3) a kill's stub edited (renamed mid-word, or its def line retyped) stays open" $ do
    (s0, _) <- blocksOn killedBy []
    let s = openDoc (root </> moduleRel) 1 written s0
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
    (_, Right (_, testFile, at, body)) <- blockKill (root </> moduleRel) 6 s
    let stubbed = expandStub at body testText
    (_, Just _) <- blockChanged testFile (openDoc testFile 1 stubbed s)
    (_, renaming) <- blockChanged testFile (changeDoc testFile 2 (T.replace "def test_kills_the_boundary(" "def t(" stubbed) s)
    fmap blockState renaming `shouldNotBe` Just Rejected
    M.member testFile <$> readIORef (ssBlocks s) `shouldReturn` True
    -- `cc` on the def line, then a pause: its body is still there
    (_, retyped) <- blockChanged testFile (changeDoc testFile 3 (T.replace "def test_kills_the_boundary() -> None:" "" stubbed) s)
    fmap blockState retyped `shouldNotBe` Just Rejected
    M.member testFile <$> readIORef (ssBlocks s) `shouldReturn` True
  it "(h3) a stub deleted while another move lands first is still rejected, on the block as it stands" $ do
    (s0, _) <- blocksOn killedBy []
    let s = openDoc (root </> moduleRel) 1 written s0
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
    (_, Right (_, testFile, at, body)) <- blockKill (root </> moduleRel) 6 s
    let stubbed = expandStub at body testText
        st = openDoc testFile 1 stubbed s
    (_, Just stale) <- blockChanged testFile st
    -- a test lands meanwhile: the block read before it is stale
    (_, Right _) <- blockTest testFile st
    gone <- rejectDeleted s root 0 testText stale
    fmap blockState gone `shouldBe` Just Rejected
    M.member testFile <$> readIORef (ssBlocks s) `shouldReturn` False
  it "(s1) stub in a function with no circle: a new test's stub at the end of its test file, named test_<fn>, its name the tab stop" $ do
    (s, _) <- blocks redOnStub []
    (_, Right (StubOpened tb (testFile, at, body, edits))) <- blockStub (root </> moduleRel) 6 s
    (testFile, at, edits) `shouldBe` (root </> testRel, 7, [])
    body `shouldBe` "\n\ndef ${1:test_handle}() -> None:\n    raise NotImplementedError(\"olifant: arrange: set up what the assertion below needs\")\n    assert ${2:...}\n"
    (blockFile tb, blockKind tb, blockSpan tb, blockTestId tb) `shouldBe` (testFile, Block.TestFor Nothing, (9, 11), T.pack testRel <> "::test_handle")
    blockState tb `shouldSatisfy` \case Red _ -> True; _ -> False
    hud tb `shouldBe` "▸ test_handle · assert, then draft · reject drops it"
    fmap fst <$> blockFor testFile s `shouldReturn` Just tb
    -- the test file is busy now: the stub's circle first
    (_, busy) <- blockStub (root </> moduleRel) 10 s
    either id (const "stubbed") busy `shouldBe` "a magic circle is open in src/shop/close_table__test.py: accept or reject it there first"
  it "(s1) stub in a function whose test_<fn> exists: test_<fn>_2" $ do
    (s, _) <- blocks redOnStub [(testRel, testText <> T.unlines ["", "", "def test_handle() -> None:", "    assert CloseTable().handle(4) == 1"])]
    (_, Right (StubOpened tb (_, _, body, _))) <- blockStub (root </> moduleRel) 6 s
    body `shouldSatisfy` ("def ${1:test_handle_2}() -> None:" `T.isInfixOf`)
    blockTestId tb `shouldBe` T.pack testRel <> "::test_handle_2"
  it "(s2) stub in a judged circle with a survivor is the kill: on the survivor's line, or off it the top-ranked one" $ do
    (s0, _) <- blocksOn killedBy []
    let s = openDoc (root </> moduleRel) 1 written s0
    (_, Right judged) <- blockBegin ignore (root </> moduleRel) 6 s
    let [boundary] = survivors judged
    (_, Right (StubKilled (code, testFile, at, body))) <- blockStub (root </> moduleRel) 6 s
    (blockId code, testFile, at) `shouldBe` (blockId judged, root </> testRel, 7)
    body `shouldSatisfy` ("def ${1:test_boundary_int_seats}() -> None:" `T.isInfixOf`)
    Just (tb, 0) <- blockFor testFile s
    blockKind tb `shouldBe` Block.TestFor (Just boundary)
    steps s >>= (`shouldSatisfy` ("block 1\tkill\tperson\tboundary on line 6" `T.isInfixOf`))
    (o0, _) <- blocksOn killedBy []
    let o = openDoc (root </> moduleRel) 1 written o0
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 o
    (_, Right (StubKilled (_, _, _, offBody))) <- blockStub (root </> moduleRel) 10 o
    offBody `shouldSatisfy` ("def ${1:test_boundary_int_seats}() -> None:" `T.isInfixOf`)
  it "(s3) stub refuses: outside a function, in a test file, and in a module whose circle has no survivor to take, saying why" $ do
    (s, _) <- blocks redOnStub []
    let refusal st path line = either id (const "stubbed") . snd <$> blockStub path line st
        notInOne = "put the cursor in a function to stub a test for it"
    -- the class line; the blank line after __init__'s body
    refusal s (root </> moduleRel) 1 `shouldReturn` notInOne
    refusal s (root </> moduleRel) 4 `shouldReturn` notInOne
    -- a test file with no circle: its tests are not what a stub is for
    refusal s (root </> testRel) 5 `shouldReturn` "stub a test from its module"
    -- the module's circle red, then broken
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    refusal s (root </> moduleRel) 6 `shouldReturn` "the circle is red: draft it, or reject it, then stub"
    (b, _) <- blocks (const (TestRun [] [] (Just "ImportError: no module named shop"))) []
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 b
    refusal b (root </> moduleRel) 6 `shouldReturn` "the circle cannot run: fix it, then test"
  it "(s3) stub in a test's circle (its test file) says where a stub comes from" $ do
    (s0, _) <- blocksOn killedBy []
    let s = openDoc (root </> moduleRel) 1 written s0
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
    (_, Right (StubKilled (_, testFile, _, _))) <- blockStub (root </> moduleRel) 6 s
    (_, fromTest) <- blockStub testFile 9 s
    either id (const "stubbed") fromTest `shouldBe` "stub a test from the code's magic circle, in its module"
  it "(s5) stub in a method whose name another class has too: the def at the cursor, imported by its own class into a new test file" $ do
    let repo = T.unlines ["class Memory:", "    def save(self, n: int) -> int:", "        return n", "", "", "class Sql:", "    def save(self, n: int) -> int:", "        return n + 1"]
    (s, _) <- blocks redOnStub [("src/shop/repo.py", repo)]
    (_, Right (StubOpened tb (file, at, body, edits))) <- blockStub (root </> "src/shop/repo.py") 8 s
    (file, at, edits) `shouldBe` (root </> "src/shop/repo__test.py", 1, [])
    take 4 (T.lines body) `shouldBe` ["from shop.repo import Sql", "", "", "def ${1:test_save}() -> None:"]
    blockSpan tb `shouldBe` (4, 6)
    -- begin there opens the same function's stub (no test calls it)
    (b, _) <- blocks redOnStub [("src/shop/repo.py", repo)]
    (_, Right (_, Just (_, _, begun, _))) <- blockStart ignore (root </> "src/shop/repo.py") 8 b
    T.lines begun `shouldSatisfy` (("from shop.repo import Sql" `elem`))
  it "(s5) begin on the blank line after a function is in no function, as stub is" $ do
    (s, _) <- blocks redOnStub [("src/shop/lonely.py", "def alone() -> int:\n    return 1\n\n\nX = 1\n")]
    (_, begun) <- blockStart ignore (root </> "src/shop/lonely.py") 4 s
    either id (const "begun") begun `shouldSatisfy` (/= "begun")
  it "(s5) stub in a function its test file does not import: the import joins the file's (one edit), the stub after it" $ do
    (s0, _) <- blocks redOnStub []
    let module' = written <> T.unlines ["", "", "class Reopen:", "    def again(self) -> int:", "        return 0"]
        s = openDoc (root </> moduleRel) 1 module' s0
    (_, Right (StubOpened tb (testFile, at, body, imports))) <- blockStub (root </> moduleRel) 15 s
    (testFile, at, length imports) `shouldBe` (root </> testRel, 7, 1)
    applyEdits imports testText `shouldBe` T.replace "import CloseTable\n" "import CloseTable, Reopen\n" testText
    body `shouldSatisfy` ("def ${1:test_again}() -> None:" `T.isInfixOf`)
    blockSpan tb `shouldBe` (9, 11)
  it "(s6) a bare function's stub (no method call), named, asserted and red at its assertion, accepted: the function begins on it, red, pushed" $ do
    let sumsRel = "src/shop/sums.py"
        sumsTest = "src/shop/sums__test.py"
        sums = T.unlines ["def total(a: int, b: int) -> int:", "    raise NotImplementedError(\"later\")"]
        -- test_total red at its assertion while total is the stub
        runner files = case M.lookup sumsTest files of
          Just t | "def test_total(" `T.isInfixOf` t, "NotImplementedError" `T.isInfixOf` M.findWithDefault "" sumsRel files ->
            Right (TestRun [] [(T.pack sumsTest <> "::test_total", Failure (lookup True [("assert" `T.isPrefixOf` T.strip l, i) | (i, l) <- zip [1 :: Int ..] (T.splitOn "\n" t)]) "AssertionError" "")] Nothing)
          _ -> Right (TestRun [] [] Nothing)
    (s, _) <- blocksOn runner [(sumsRel, sums)]
    seen <- newIORef ([] :: [Block])
    writeIORef (ssOnBlock s) (\b _ -> modifyIORef' seen (++ [b]))
    (_, Right (StubOpened _ (testFile, 1, body, []))) <- blockStub (root </> sumsRel) 2 s
    let arranged = T.unlines [if "olifant: arrange" `T.isInfixOf` l then "    x = 1" else l | l <- T.lines (T.replace "${1:test_total}" "test_total" (T.replace "${2:...}" "total(1, 2) == 3" body))]
        st = openDoc testFile 1 arranged s
    (_, Just ready) <- blockChanged testFile st
    hud ready `shouldSatisfy` ("red ✓ accept · the fix is drafted next" `T.isSuffixOf`)
    (_, Right (done, [])) <- blockAccept testFile st
    blockState done `shouldBe` Accepted
    Just (code, _) <- blockFor (root </> sumsRel) st
    (blockKind code, blockTestId code, blockSpan code) `shouldBe` (Block.Code, T.pack sumsTest <> "::test_total", (2, 2))
    blockState code `shouldSatisfy` \case Red _ -> True; _ -> False
    map blockId <$> readIORef seen `shouldReturn` [blockId code]
  it "(s4) stub in a module whose circle is judged with no survivor left is refused: that circle first" $ do
    (s0, _) <- blocksOn killedBy []
    let s = openDoc (root </> moduleRel) 1 written s0
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
    (_, Right _) <- blockWaive (root </> moduleRel) 6 "inclusive by design" s
    (_, refused) <- blockStub (root </> moduleRel) 6 s
    either id (const "stubbed") refused `shouldBe` "accept or reject the circle in src/shop/close_table.py first, then stub"
    M.member (root </> testRel) <$> readIORef (ssBlocks s) `shouldReturn` False
  it "(i) waive writes the comment on the survivor's line and reopens accept" $ do
    (s0, _) <- blocks redOnStub []
    let s = openDoc (root </> moduleRel) 1 written s0
    (_, Right judged) <- blockBegin ignore (root </> moduleRel) 6 s
    open judged `shouldSatisfy` notElem "accept"
    (_, empty') <- blockWaive (root </> moduleRel) 6 "  " s
    either id (const "waived") empty' `shouldBe` "a waiver needs a reason"
    (_, wrong) <- blockWaive (root </> moduleRel) 5 "why" s
    either id (const "waived") wrong `shouldBe` "no unwaived survivor on that line"
    (_, Right (b, [edit])) <- blockWaive (root </> moduleRel) 6 "inclusive by design" s
    (fePath edit, feVersion edit) `shouldBe` (root </> moduleRel, Just 1)
    let commented = applyChange (feRange edit) (feText edit) written
    T.lines commented !! 5 `shouldBe` "        return int(seats > 2)  # olifant: boundary ok: inclusive by design"
    survivors b `shouldBe` []
    open b `shouldSatisfy` elem "accept"
    -- the editor applies the waiver: that is no edit of the block's
    (_, none) <- blockChanged (root </> moduleRel) (changeDoc (root </> moduleRel) 2 commented s)
    none `shouldBe` Nothing
    steps s >>= (`shouldSatisfy` ("\twaive\tperson\tboundary: inclusive by design" `T.isInfixOf`))
  it "(j) an edit inside the span reruns the test and makes a \"you\" draft; an applied draft makes none; outside the span nothing happens" $ do
    (s0, runs) <- blocks redOnStub [("src/shop/lonely.py", "def alone() -> int:\n    return 1\n")]
    let s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    ran <- runs
    -- outside the span (line 10), and a file with no block: nothing runs
    (_, outside) <- blockChanged (root </> moduleRel) (changeDoc (root </> moduleRel) 2 (T.replace "n > 0" "n > 1" stubbed) s)
    outside `shouldBe` Nothing
    (_, elsewhere) <- blockChanged (root </> "src/shop/lonely.py") s
    elsewhere `shouldBe` Nothing
    runs `shouldReturn` ran
    -- by hand inside the span: tested, judged, a "you" draft
    (_, Just mine) <- blockChanged (root </> moduleRel) (changeDoc (root </> moduleRel) 2 written s)
    map draftBy (blockDrafts mine) `shouldBe` ["you"]
    blockState mine `shouldSatisfy` \case Judged _ -> True; _ -> False
    -- a draft the editor applied: tested and judged, no "you" draft
    (t0, _) <- blocks redOnStub []
    let t = (openDoc (root </> moduleRel) 1 stubbed t0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 t
    (_, Right (_, [edit])) <- blockDraft ignore (root </> moduleRel) t
    (_, Just applied) <- blockChanged (root </> moduleRel) (changeDoc (root </> moduleRel) 2 (applyChange (feRange edit) (feText edit) stubbed) t)
    map draftBy (blockDrafts applied) `shouldBe` ["recorded"]
    blockState applied `shouldSatisfy` \case Judged _ -> True; _ -> False
  it "(k) Jev: survivors it thinks equivalent are dimmed and last, the rest by how much they matter; with no decider, file order" $ do
    (s, _) <- blocks redOnStub []
    let b = Block.begin 1 (root </> moduleRel) Block.Code stubbed (1, 10) testId (Failure Nothing "" "")
        lineOf n = T.lines stubbed !! (n - 1)
        mutant n op to = Mutant n op "" "" (T.unlines [if i == n then to else l | (i, l) <- zip [1 ..] (T.lines stubbed)]) Nothing False
        ms = [mutant 3 "number" "        self.closed = 1", mutant 6 "string" (T.replace "later" "" (lineOf 6)), mutant 10 "boundary" "    return n >= 0"]
        jev = De.Decider $ \q -> pure $ case (De.qKind q, lookup "mutant" (De.qContext q)) of
          ("mutant-equivalent", Just m) | "\"\"" `T.isInfixOf` m -> De.Chosen "equivalent" []
          ("mutant-equivalent", _) -> De.Chosen "changes-behaviour" []
          ("mutant-matters", Just m) | ">=" `T.isInfixOf` m -> De.Chosen "high" []
          _ -> De.Unsure
    orderSurvivors s Nothing b stubbed testText (pure ()) ms >>= (`shouldBe` ms)
    answered <- newIORef (0 :: Int)
    first' <- orderSurvivors s (Just jev) b stubbed testText (atomicModifyIORef' answered (\n -> (n + 1, ()))) ms
    first' `shouldBe` ms -- nothing known yet: as given
    waitUntil ((>= 6) <$> readIORef answered)
    ordered <- orderSurvivors s (Just jev) b stubbed testText (pure ()) ms
    map (\m -> (muLine m, muDim m)) ordered `shouldBe` [(10, False), (3, False), (6, True)]
  it "(k) with a recorded decider answering \"equivalent\", the judged block's survivors are dimmed once it answers, and pushed" $
    withSystemTempDirectory "olifant-jev" $ \dir -> do
      let answers = dir </> "answers.json"
      TIO.writeFile answers "{\"answers\": {\"mutant-equivalent\": \"equivalent\", \"mutant-matters\": \"low\"}}"
      (s0, _) <- blocks redOnStub []
      writeText (portsFiles s0) (root </> ".olifant/config.toml") "[packs]\npython-htmx = { path = \"/pack\" }\n\n[decide]\nmodel = \"jev\"\n"
      pushed <- newIORef []
      writeIORef (ssOnBlock s0) (\b _ -> atomicModifyIORef' pushed (\bs -> (b : bs, ())))
      let s = (openDoc (root </> moduleRel) 1 written s0) {ssConfig = defaultServerConfig {scDecideAnswers = Just answers}}
      (_, Right judged) <- blockBegin ignore (root </> moduleRel) 6 s
      map muDim (survivors judged) `shouldBe` [False]
      waitUntil (any (all muDim . survivors) <$> readIORef pushed)
      fmap (map muDim . survivors . fst) <$> blockFor (root </> moduleRel) s `shouldReturn` Just [True]
      -- the order is a move: one begun on the block before it is stale, the order stands
      (settle s root 0 "waive" "why" (Block.Waive 6 "boundary" "why") judged >>= either pure (const (pure "landed"))) `shouldReturn` "the magic circle moved on"
      fmap (map muDim . survivors . fst) <$> blockFor (root </> moduleRel) s `shouldReturn` Just [True]
  it "(i2) a waive on a line with two operators' survivors writes a comment for each; judged again, both stay waived" $ do
    (s0, _) <- blocksOnSyntax numbersToo (Right . redOnStub . M.findWithDefault "" moduleRel) []
    let s = openDoc (root </> moduleRel) 1 written s0
    (_, Right judged) <- blockBegin ignore (root </> moduleRel) 6 s
    map (\m -> (muLine m, muOperator m)) (survivors judged) `shouldBe` [(6, "boundary"), (6, "number")]
    (_, Right (b, [edit])) <- blockWaive (root </> moduleRel) 6 "by design" s
    let commented = applyChange (feRange edit) (feText edit) written
    T.lines commented !! 5 `shouldBe` "        return int(seats > 2)  # olifant: boundary ok: by design  # olifant: number ok: by design"
    survivors b `shouldBe` []
    open b `shouldSatisfy` elem "accept"
    -- the comments carry both: judged again from the file, nothing survives unwaived
    (_, Right again) <- blockTest (root </> moduleRel) (changeDoc (root </> moduleRel) 2 commented s)
    case blockState again of
      Judged ms -> map (\m -> (muOperator m, fmap ("by design" `T.isPrefixOf`) (muWaived m))) ms `shouldBe` [("boundary", Just True), ("number", Just True)]
      other -> expectationFailure ("expected Judged, got " <> show other)
    survivors again `shouldBe` []
  it "(k2) Jev asks about two mutants of one operator on one line once each, not without end" $ do
    (s, _) <- blocks redOnStub []
    asked <- newIORef (0 :: Int)
    let b = Block.begin 1 (root </> moduleRel) Block.Code stubbed (1, 10) testId (Failure Nothing "" "")
        twin from to line = Mutant 10 "boundary" from to (T.unlines [if i == 10 then line else l | (i, l) <- zip [1 :: Int ..] (T.lines stubbed)]) Nothing False
        ms = [twin "a > b" "a >= b" "    return a >= b and c > d", twin "c > d" "c >= d" "    return a > b and c >= d"]
        counting = De.Decider $ \_ -> atomicModifyIORef' asked (\n -> (n + 1, ())) >> pure De.Unsure
        -- as the server does: each answer orders again
        again = void (orderSurvivors s (Just counting) b stubbed testText again ms)
    again
    -- the four questions answered and none in flight: nothing more is asked
    waitUntil (settled s 4)
    readIORef asked `shouldReturn` 4
  it "(k2) a decision asked by many at once is asked once" $ do
    (s, _) <- blocks redOnStub []
    asked <- newIORef (0 :: Int)
    let counting = De.Decider $ \_ -> atomicModifyIORef' asked (\n -> (n + 1, ())) >> pure De.Unsure
        q = De.Question "mutant-matters" [] [] []
        keys = [(root </> moduleRel, "matters:" <> T.pack (show i)) | i <- [1 .. 300 :: Int]]
    mapM_ (\key -> replicateConcurrently_ 8 (St.decision s counting key "fp" q (pure ()))) keys
    waitUntil (settled s (length keys))
    readIORef asked `shouldReturn` length keys
  it "(l) when the kill's test block is accepted, the code block is judged again and the killed mutant is gone" $ do
    (s0, _) <- blocksOn killedBy []
    pushed <- newIORef []
    writeIORef (ssOnBlock s0) (\b k -> modifyIORef' pushed ((b, k) :))
    let s = (openDoc (root </> moduleRel) 1 written s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
    (_, Right (_, testFile, at, body)) <- blockKill (root </> moduleRel) 6 s
    let stubbedTest = expandStub at body testText
        st = openDoc testFile 1 stubbedTest s
    (_, Right (_, [edit])) <- blockDraft ignore testFile st
    let st2 = changeDoc testFile 2 (applyChange (feRange edit) (feText edit) stubbedTest) st
    (_, Just tested) <- blockChanged testFile st2
    map draftBy (blockDrafts tested) `shouldBe` ["recorded"]
    blockState tested `shouldBe` Judged []
    (_, Right (done, [])) <- blockAccept testFile st2
    blockState done `shouldBe` Accepted
    M.member testFile <$> readIORef (ssBlocks s) `shouldReturn` False
    Just (code, killed) <- blockFor (root </> moduleRel) s
    (blockState code, killed) `shouldBe` (Judged [], 1)
    -- pushed, newest first: the code block judged again; the test block drafting
    map (\(b, k) -> (blockId b, stateName (blockState b), k)) <$> readIORef pushed `shouldReturn` [(blockId code, "judged", 1), (2, "drafting", 0)]
    log' <- steps s
    log' `shouldSatisfy` ("block 2\taccept\tperson" `T.isInfixOf`)
    log' `shouldSatisfy` ("block 1\trejudge\tperson" `T.isInfixOf`)
    log' `shouldSatisfy` ("block 1\tjudge\tmutation\t1/1 killed" `T.isInfixOf`)
  it "(C1) a judge is by the tests that passed before: with a second test red in the file, the boundary survivor is still reported" $ do
    (s0, _) <- blocksOn killedBy [(testRel, withNext)]
    let s = openDoc (root </> moduleRel) 1 stubbed s0
    (_, Right b0) <- blockBegin ignore (root </> testRel) 6 s
    blockTestId b0 `shouldBe` testId
    (_, Right b) <- blockTest (root </> moduleRel) (changeDoc (root </> moduleRel) 2 written s)
    map (\m -> (muLine m, muOperator m)) (survivors b) `shouldBe` [(6, "boundary")]
    steps s >>= (`shouldSatisfy` ("\tjudge\tmutation\t0/1 killed" `T.isInfixOf`))
  it "(C1) a kill's test stands with a sibling test red: it passes on the original with what passed before, and fails on the mutant" $ do
    (s0, _) <- blocksOn killedBy [(testRel, withNext)]
    let s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    let s1 = changeDoc (root </> moduleRel) 2 written s
    (_, Right _) <- blockTest (root </> moduleRel) s1
    (_, Right (_, testFile, at, body)) <- blockKill (root </> moduleRel) 6 s1
    let stubbedTest = expandStub at body withNext
        st = openDoc testFile 1 stubbedTest s1
    (_, Right (drafted, [edit])) <- blockDraft ignore testFile st
    blockState drafted `shouldBe` Green
    -- applied: its own test is green (the sibling stays red), so it is judged
    let st2 = changeDoc testFile 2 (applyChange (feRange edit) (feText edit) stubbedTest) st
    (_, Just tested) <- blockChanged testFile st2
    blockState tested `shouldBe` Judged []
    (_, Right _) <- blockAccept testFile st2
    Just (code, killed) <- blockFor (root </> moduleRel) s
    (blockState code, killed) `shouldBe` (Judged [], 1)
  it "(I1) a draft in flight is Drafting (pushed, only reject open); one that lands after a reject is refused and the block stays gone" $ do
    gate <- newGate
    (s0, _) <- blocksGated gate redOnStub []
    pushed <- newIORef []
    writeIORef (ssOnBlock s0) (\b _ -> atomicModifyIORef' pushed (\bs -> (b : bs, ())))
    let s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    gArm gate
    drafting <- async (blockDraft ignore (root </> moduleRel) s)
    waitUntil ((>= 1) <$> gEntered gate)
    Just (now, _) <- blockFor (root </> moduleRel) s
    blockState now `shouldSatisfy` \case Drafting _ -> True; _ -> False
    (open now, "drafting…" `T.isSuffixOf` hud now) `shouldBe` (["reject"], True)
    map blockState <$> readIORef pushed `shouldReturn` [blockState now]
    (_, accepted) <- blockAccept (root </> moduleRel) s
    accepted `shouldSatisfy` either (const True) (const False)
    (_, Right _) <- blockReject (root </> moduleRel) s
    gOpen gate
    (_, landed) <- wait drafting
    either id (const "landed") landed `shouldBe` "the magic circle moved on"
    blockFor (root </> moduleRel) s `shouldReturn` Nothing
  it "(I1) a draft that fails puts the block's state back" $ do
    (s0, _) <- blocks redOnStub []
    let s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right b) <- blockBegin ignore (root </> testRel) 6 s
    (_, Left _) <- blockDraft ignore (root </> moduleRel) s {ssLiveDoc = Just (\_ -> pure (Just (2, stubbed)))}
    fmap (unversioned . fst) <$> blockFor (root </> moduleRel) s `shouldReturn` Just (unversioned b)
  it "(I1) an edit's test that finishes after a reject does nothing" $ do
    gate <- newGate
    (s0, _) <- blocksGated gate redOnStub []
    let s = openDoc (root </> moduleRel) 1 stubbed s0
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    gArm gate
    testing <- async (blockChanged (root </> moduleRel) (changeDoc (root </> moduleRel) 2 written s))
    waitUntil ((>= 1) <$> gEntered gate)
    (_, Right _) <- blockReject (root </> moduleRel) s
    gOpen gate
    (_, tested) <- wait testing
    tested `shouldBe` Nothing
    blockFor (root </> moduleRel) s `shouldReturn` Nothing
  it "(I1) two begins at once make exactly one block" $ do
    gate <- newGate
    (s, _) <- blocksGated gate redOnStub []
    gArm gate
    one <- async (blockBegin ignore (root </> testRel) 6 s)
    two <- async (blockBegin ignore (root </> testRel) 6 s)
    waitUntil ((>= 2) <$> gEntered gate)
    gOpen gate
    results <- mapM (fmap snd . wait) [one, two]
    length [() | Right _ <- results] `shouldBe` 1
    [why | Left why <- results] `shouldSatisfy` all ("a magic circle is open" `T.isInfixOf`)
    M.size <$> readIORef (ssBlocks s) `shouldReturn` 1
  it "(I2) a draft that fails its test writes its last attempt (never the placeholder) and keeps it as a red draft" $ do
    (s0, _) <- blocks (const (failing "assert 0 == 1")) []
    let mine = T.replace "raise NotImplementedError(\"later\")" "return 0  # mine, half done" stubbed
        s = (openDoc (root </> moduleRel) 1 mine s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    (_, Right (b, [edit])) <- blockDraft ignore (root </> moduleRel) s
    let written' = applyChange (feRange edit) (feText edit) mine
    T.lines written' !! 5 `shouldBe` "        return int(seats > 2)"
    written' `shouldSatisfy` (not . ("olifant: body" `T.isInfixOf`))
    blockState b `shouldSatisfy` \case Red _ -> True; _ -> False
    map (\d -> (draftText d, draftBy d, "red" `T.isPrefixOf` draftOutcome d)) (blockDrafts b) `shouldBe` [("        return int(seats > 2)", "recorded", True)]
  it "(I2) a failed draft read one level up (the reindent rescue) is written as read, its type error the reason" $ do
    (s0, _) <- blocks (const (failing "assert 0 == 1")) []
    let deeper a b = indentOf b > indentOf a && not (":" `T.isSuffixOf` T.stripEnd a)
        indentOf = T.length . T.takeWhile (== ' ')
        complaints t = let ls = filter (not . T.null . T.strip) (T.lines t) in ["Unexpected indentation" | or (zipWith deeper ls (drop 1 ls))] ++ ["Cannot access attribute \"find_by_id\"" | "find_by_id" `T.isInfixOf` t]
        checker = Checker (\_ files -> pure (Right (CheckReport [] [(p, why) | (p, t) <- M.toList files, why <- complaints t] [])))
        rehearsal = "        booking = self.find_by_id(seats)\n                if booking is None:\n                    return 0\n                return 1"
        filler = ModelFiller "rehearsal" 0 (Filler (\_ -> pure (Right (M.fromList [("body", rehearsal)]))))
        s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssTestFiller = Just filler, ssPorts = (ssPorts s0) {pChecker = const checker}}
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    (_, Right (b, [edit])) <- blockDraft ignore (root </> moduleRel) s
    take 4 (drop 5 (T.lines (applyChange (feRange edit) (feText edit) stubbed))) `shouldBe` ["        booking = self.find_by_id(seats)", "        if booking is None:", "            return 0", "        return 1"]
    blockState b `shouldSatisfy` \case Red f -> "find_by_id" `T.isInfixOf` fMessage f && not ("Unexpected indentation" `T.isInfixOf` fMessage f); _ -> False
  it "(I2) a draft that never produced an attempt makes no edits and no draft" $ do
    (s0, _) <- blocks redOnStub [("nofill.toml", "[fills]\narrange = \"x = 1\"\n")]
    let s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "nofill.toml")}}
    (_, Right b) <- blockBegin ignore (root </> testRel) 6 s
    (_, none) <- blockDraft ignore (root </> moduleRel) s
    none `shouldSatisfy` either ("no recorded fill" `T.isInfixOf`) (const False)
    fmap (unversioned . fst) <$> blockFor (root </> moduleRel) s `shouldReturn` Just (unversioned b)
  it "(I3) the judge runs each mutant with a timeout of its own, from the baseline's wall time" $ do
    (s0, _) <- blocks redOnStub []
    seen <- newIORef []
    let s = (openDoc (root </> moduleRel) 1 written s0) {ssPorts = (ssPorts s0) {pTesterWithin = \secs c -> Runner (\l i f -> atomicModifyIORef' seen (\xs -> (secs : xs, ())) >> runTests (pTester (ssPorts s0) c) l i f)}}
    (_, Right judged) <- blockBegin ignore (root </> moduleRel) 6 s
    map muLine (survivors judged) `shouldBe` [6]
    readIORef seen `shouldReturn` [5]
  it "(I4) a code draft sees the test file; a test draft sees the module" $ do
    (s0, _) <- blocksOn killedBy []
    seen <- newIORef []
    let recording = ModelFiller "recording" 0 (Filler (\req -> atomicModifyIORef' seen (\cs -> (frContext req : cs, ())) >> pure (Right (M.fromList [("body", "return int(seats > 2)"), ("arrange", "table = CloseTable()")]))))
        s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssTestFiller = Just recording}
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    (_, Right (_, [edit])) <- blockDraft ignore (root </> moduleRel) s
    [code] <- readIORef seen
    lookup testRel code `shouldBe` Just testText
    -- judged, a survivor killed: its test drafted with the module in view
    let s1 = changeDoc (root </> moduleRel) 2 (applyChange (feRange edit) (feText edit) stubbed) s
    (_, Just _) <- blockChanged (root </> moduleRel) s1
    (_, Right (_, testFile, at, body)) <- blockKill (root </> moduleRel) 6 s1
    (_, Right _) <- blockDraft ignore testFile (openDoc testFile 1 (expandStub at body testText) s1)
    test : _ <- readIORef seen
    lookup moduleRel test `shouldBe` Just written
  it "(M1) a language with no mutation operators cannot be judged: accept stays closed" $ do
    (s0, _) <- blocks (const (TestRun [testId] [] Nothing)) [("src/shop/close_table.snk", written)]
    python <- maybe "" id <$> readText (portsFiles s0) "/pack/languages/python.toml"
    writeText (portsFiles s0) "/pack/languages/snake.toml" (T.replace "files = [\"**/*.py\"]" "files = [\"**/*.snk\"]" python)
    (_, Right b) <- blockBegin ignore (root </> "src/shop/close_table.snk") 6 s0
    blockState b `shouldBe` Block.Broken "no mutation operators for snake"
    open b `shouldSatisfy` notElem "accept"
  it "(M5) a green draft that changes nothing is judged at once (no edit comes back to test)" $ do
    (s0, _) <- blocks redOnStub []
    let s = (openDoc (root </> moduleRel) 1 written s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right judged) <- blockBegin ignore (root </> moduleRel) 6 s
    blockState judged `shouldSatisfy` \case Judged _ -> True; _ -> False
    (_, Right (b, edits)) <- blockDraft ignore (root </> moduleRel) s
    edits `shouldBe` []
    blockState b `shouldSatisfy` \case Judged _ -> True; _ -> False
  it "(M2) the step log changing under .olifant/state/ does not load the project again" $ do
    (s0, _) <- server
    (s1, Just _) <- projectFor "/r1/src/x.py" s0
    s2 <- filesChanged ["/r1/.olifant/state/steps.log"] s1
    M.lookup "/r1" (ssLoads s2) `shouldBe` M.lookup "/r1" (ssLoads s1)
    s3 <- filesChanged ["/r1/.olifant/config.toml"] s2
    M.lookup "/r1" (ssLoads s3) `shouldBe` fmap (+ 1) (M.lookup "/r1" (ssLoads s1))
  it "(I1) an edit made before a draft started, tested while it drafts, does not knock the draft out" $ do
    gate <- newGate
    (s0, _) <- blocksGated gate redOnStub []
    let s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    -- the person typed in the span, then asked for a draft before the pause ran the test
    let typed = changeDoc (root </> moduleRel) 2 (T.replace "\"later\"" "\"later!\"" stubbed) s
    gArm gate
    drafting <- async (blockDraft ignore (root </> moduleRel) typed)
    waitUntil ((>= 1) <$> gEntered gate)
    testing <- async (blockChanged (root </> moduleRel) typed)
    threadDelay 200000
    gOpen gate
    (_, tested) <- wait testing
    tested `shouldBe` Nothing
    (_, Right (b, _)) <- wait drafting
    blockState b `shouldBe` Green
  it "(N1) an edit made before a draft that then fails is tested: the failed draft does not put back the verdict on the text before it" $ do
    (s0, _) <- blocks redOnStub []
    (filler, entered, release) <- stalledFiller
    let s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssTestFiller = Just filler}
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    -- the person typed the body, then asked for a draft before the pause ran the test
    let typed = changeDoc (root </> moduleRel) 2 written s
    drafting <- async (blockDraft ignore (root </> moduleRel) typed)
    takeMVar entered
    -- the pause's test, while the model writes: the draft planned on that text
    (_, tested) <- blockChanged (root </> moduleRel) typed
    tested `shouldBe` Nothing
    putMVar release ()
    (_, failed) <- wait drafting
    failed `shouldSatisfy` either (const True) (const False)
    Just (b, _) <- blockFor (root </> moduleRel) s
    blockState b `shouldSatisfy` \case Judged _ -> True; _ -> False
    map draftBy (blockDrafts b) `shouldBe` ["you"]
  it "(N1) an edit whose test lands while a draft is in flight is tested again when the draft fails" $ do
    gate <- newGate
    (s0, _) <- blocksGated gate redOnStub []
    (filler, entered, release) <- stalledFiller
    let s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssTestFiller = Just filler}
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    let typed = changeDoc (root </> moduleRel) 2 written s
    -- the pause's test starts on the red block; the draft starts while it runs
    gArm gate
    testing <- async (blockChanged (root </> moduleRel) typed)
    waitUntil ((>= 1) <$> gEntered gate)
    drafting <- async (blockDraft ignore (root </> moduleRel) typed)
    takeMVar entered
    gOpen gate
    (_, tested) <- wait testing
    tested `shouldBe` Nothing
    putMVar release ()
    (_, Left _) <- wait drafting
    Just (b, _) <- blockFor (root </> moduleRel) s
    blockState b `shouldSatisfy` \case Judged _ -> True; _ -> False
  it "(I-1) an edit newer than the draft, its test still running when the draft fails: the block ends on the edit's verdict" $ do
    gate <- newGate
    (s0, _) <- blocksGated gate redOnStub []
    (filler, entered, release) <- stalledFiller
    -- the editor's documents now, as the adapter holds them
    live <- newIORef (1, stubbed)
    let s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssTestFiller = Just filler, ssLiveDoc = Just (\_ -> Just <$> readIORef live)}
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    -- the draft plans on the stub
    drafting <- async (blockDraft ignore (root </> moduleRel) s)
    takeMVar entered
    -- the person types the body; the pause's test starts (held)
    writeIORef live (2, written)
    gArm gate
    testing <- async (blockChanged (root </> moduleRel) (changeDoc (root </> moduleRel) 2 written s))
    waitUntil ((>= 1) <$> gEntered gate)
    -- and types again (its own test not begun yet): the editor's text is a third one
    let third = T.replace "seats > 2)" "seats > 2)  # third" written
    writeIORef live (3, third)
    -- the model gives up: the state before the draft is put back
    putMVar release ()
    waitUntil (maybe False (\(b, _) -> case blockState b of Drafting _ -> False; _ -> True) <$> blockFor (root </> moduleRel) s)
    -- then the edit's test result arrives
    gOpen gate
    void (wait testing)
    (_, Left _) <- wait drafting
    Just (b, _) <- blockFor (root </> moduleRel) s
    blockState b `shouldSatisfy` \case Judged _ -> True; _ -> False
    -- tested on the editor's text as the draft failed (only the restore's retest saw it)
    blockOn b `shouldBe` third
  it "(retry) two edits' tests begun on one version, the older landing first: the newer settles once more, on the newest text" $ do
    -- runs 1 and 2 (the two edits' tests) each wait for their own release
    calls <- newIORef (0 :: Int)
    [r1, r2] <- mapM (const newEmptyMVar) [1 .. 2 :: Int]
    let hook = atomicModifyIORef' calls (\n -> (n + 1, n)) >>= \n -> mapM_ readMVar [r | (i, r) <- zip [1 ..] [r1, r2], i == n]
    (s0, _) <- blocksHooked blockSyntax hook (Right . redOnStub . M.findWithDefault "" moduleRel) []
    let s = openDoc (root </> moduleRel) 1 stubbed s0
        older = written
        newer = T.replace "seats > 2)" "seats > 2)  # newer" written
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    one <- async (blockChanged (root </> moduleRel) (changeDoc (root </> moduleRel) 2 older s))
    waitUntil ((>= 2) <$> readIORef calls)
    two <- async (blockChanged (root </> moduleRel) (changeDoc (root </> moduleRel) 3 newer s))
    waitUntil ((>= 3) <$> readIORef calls)
    putMVar r1 ()
    void (wait one)
    putMVar r2 ()
    void (wait two)
    Just (b, _) <- blockFor (root </> moduleRel) s
    blockOn b `shouldBe` newer
    blockState b `shouldSatisfy` \case Judged _ -> True; _ -> False
  it "(M-3) a test whose work was cancelled (a newer edit's test supersedes it) settles nothing" $ do
    (s0, _) <- blocks redOnStub []
    let s = openDoc (root </> moduleRel) 1 stubbed s0
    (_, Right b) <- blockBegin ignore (root </> testRel) 6 s
    (token, trip) <- newCancel
    trip
    (_, tested) <- blockTest (root </> moduleRel) (changeDoc (root </> moduleRel) 2 written s) {ssCancel = token}
    tested `shouldSatisfy` either (const True) (const False)
    fmap fst <$> blockFor (root </> moduleRel) s `shouldReturn` Just b
  it "(N-1) an edit outside the span while an in-span edit's test runs is no edit of the block's: that test is not superseded, and lands" $ do
    gate <- newGate
    (s0, _) <- blocksGated gate redOnStub []
    let s = openDoc (root </> moduleRel) 1 stubbed s0
        outside = T.replace "n > 0" "n > 1" written
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    -- typed in the span: an edit of the block's (the adapter submits its test)
    let s1 = changeDoc (root </> moduleRel) 2 written s
    (_, True) <- blockEdited (root </> moduleRel) s1
    gArm gate
    testing <- async (blockChanged (root </> moduleRel) s1)
    waitUntil ((>= 1) <$> gEntered gate)
    -- then outside it, while that test runs: no edit of the block's (nothing submitted, nothing cancelled)
    (_, False) <- blockEdited (root </> moduleRel) (changeDoc (root </> moduleRel) 3 outside s)
    -- in the span again: it would supersede the test
    (_, True) <- blockEdited (root </> moduleRel) (changeDoc (root </> moduleRel) 3 (T.replace "seats > 2)" "seats > 3)" outside) s)
    gOpen gate
    (_, Just tested) <- wait testing
    blockState tested `shouldSatisfy` \case Judged _ -> True; _ -> False
    Just (b, _) <- blockFor (root </> moduleRel) s
    (blockOn b, blockState b) `shouldBe` (written, blockState tested)
  it "(N-1) an edit outside the span while the block is judged does not supersede the judge: it lands" $ do
    (s0, _) <- blocks redOnStub []
    release <- newEmptyMVar
    entered <- newIORef (0 :: Int)
    let within _ c = Runner (\l i f -> atomicModifyIORef' entered (\n -> (n + 1, ())) >> readMVar release >> runTests (pTester (ssPorts s0) c) l i f)
        s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssPorts = (ssPorts s0) {pTesterWithin = within}}
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    let s1 = changeDoc (root </> moduleRel) 2 written s
    testing <- async (blockChanged (root </> moduleRel) s1)
    waitUntil ((>= 1) <$> readIORef entered)
    fmap (blockState . fst) <$> blockFor (root </> moduleRel) s `shouldReturn` Just Green
    (_, False) <- blockEdited (root </> moduleRel) (changeDoc (root </> moduleRel) 3 (T.replace "n > 0" "n > 1" written) s)
    putMVar release ()
    (_, Just judged) <- wait testing
    blockState judged `shouldSatisfy` \case Judged _ -> True; _ -> False
  it "(R2-1) a waive while an edit's test runs is refused; the test lands, the block judged on the edit" $ do
    gate <- newGate
    (s0, _) <- blocksGated gate redOnStub []
    let s = openDoc (root </> moduleRel) 1 written s0
        edited = T.replace "seats > 2)" "seats > 2)  # mine" written
        s1 = changeDoc (root </> moduleRel) 2 edited s
    (_, Right judged) <- blockBegin ignore (root </> moduleRel) 6 s
    map muLine (survivors judged) `shouldBe` [6]
    gArm gate
    testing <- async (blockChanged (root </> moduleRel) s1)
    waitUntil ((>= 1) <$> gEntered gate)
    (_, waived) <- blockWaive (root </> moduleRel) 6 "by design" s1
    either id (const "waived") waived `shouldBe` "your edit is being tested; waive once its verdict lands"
    fmap fst <$> blockFor (root </> moduleRel) s `shouldReturn` Just judged
    gOpen gate
    (_, Just tested) <- wait testing
    Just (b, _) <- blockFor (root </> moduleRel) s
    (blockOn b, blockState b) `shouldBe` (edited, blockState tested)
    blockState b `shouldSatisfy` \case Judged _ -> True; _ -> False
  it "(R2-1) an accept while an edit's test runs is refused; the test lands, the block judged on the edit" $ do
    gate <- newGate
    let kills t = if "seats >= 2" `T.isInfixOf` t then failing "assert 0 == 1" else redOnStub t
    (s0, _) <- blocksGated gate kills []
    let s = openDoc (root </> moduleRel) 1 written s0
        edited = T.replace "seats > 2)" "seats > 2)  # mine" written
        s1 = changeDoc (root </> moduleRel) 2 edited s
    (_, Right judged) <- blockBegin ignore (root </> moduleRel) 6 s
    blockState judged `shouldBe` Judged []
    gArm gate
    testing <- async (blockChanged (root </> moduleRel) s1)
    waitUntil ((>= 1) <$> gEntered gate)
    (_, accepted) <- blockAccept (root </> moduleRel) s1
    either id (const "accepted") accepted `shouldBe` "your edit is being tested; accept once its verdict lands"
    fmap fst <$> blockFor (root </> moduleRel) s `shouldReturn` Just judged
    gOpen gate
    void (wait testing)
    Just (b, _) <- blockFor (root </> moduleRel) s
    (blockOn b, blockState b) `shouldBe` (edited, Judged [])
  it "(R2-1) a kill while an edit's test runs does not drop it: the test settles on the block as the kill left it" $ do
    gate <- newGate
    (s0, _) <- blocksGated gate redOnStub []
    let s = openDoc (root </> moduleRel) 1 written s0
        edited = T.replace "seats > 2)" "seats > 2)  # mine" written
        s1 = changeDoc (root </> moduleRel) 2 edited s
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
    gArm gate
    testing <- async (blockChanged (root </> moduleRel) s1)
    waitUntil ((>= 1) <$> gEntered gate)
    (_, Right _) <- blockKill (root </> moduleRel) 6 s1
    gOpen gate
    void (wait testing)
    Just (b, _) <- blockFor (root </> moduleRel) s
    blockOn b `shouldBe` edited
    blockState b `shouldSatisfy` \case Judged _ -> True; _ -> False
  it "(R3-1) an edit's test cancelled by a job that then finds nothing new (typed and undone): the edit is still tested" $ do
    gate <- newGate
    (s0, _) <- blocksGated gate redOnStub []
    let s = openDoc (root </> moduleRel) 1 written s0
        edited = T.replace "seats > 2)" "seats > 2)  # mine" written
        s1 = changeDoc (root </> moduleRel) 2 edited s
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
    (token, trip) <- newCancel
    gArm gate
    testing <- async (blockChanged (root </> moduleRel) s1 {ssCancel = token})
    waitUntil ((>= 1) <$> gEntered gate)
    -- a character typed in the span cancels it; undone before the next job runs
    trip
    gOpen gate
    (_, none) <- wait testing
    none `shouldBe` Nothing
    -- that job: the text as the cancelled test had it, still untested
    (_, Just tested) <- blockChanged (root </> moduleRel) s1
    blockState tested `shouldSatisfy` \case Judged _ -> True; _ -> False
    fmap (blockOn . fst) <$> blockFor (root </> moduleRel) s `shouldReturn` Just edited
  it "(R3-1) a judge cancelled by a job that then finds nothing new: the next job tests and judges again" $ do
    (s0, _) <- blocks redOnStub []
    release <- newEmptyMVar
    entered <- newIORef (0 :: Int)
    let within _ c = Runner (\l i f -> atomicModifyIORef' entered (\n -> (n + 1, ())) >> readMVar release >> runTests (pTester (ssPorts s0) c) l i f)
        s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssPorts = (ssPorts s0) {pTesterWithin = within}}
        s1 = changeDoc (root </> moduleRel) 2 written s
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    (token, trip) <- newCancel
    judging <- async (blockChanged (root </> moduleRel) s1 {ssCancel = token})
    waitUntil ((>= 1) <$> readIORef entered)
    trip
    putMVar release ()
    void (wait judging)
    fmap (blockState . fst) <$> blockFor (root </> moduleRel) s `shouldReturn` Just Green
    (_, Just judged) <- blockChanged (root </> moduleRel) s1
    blockState judged `shouldSatisfy` \case Judged _ -> True; _ -> False
  it "(R3-2) an accept between an edit and its test (the pause not run yet) is refused; tested, it is open" $ do
    let kills t = if "seats >= 2" `T.isInfixOf` t then failing "assert 0 == 1" else redOnStub t
    (s0, _) <- blocks kills []
    let s = openDoc (root </> moduleRel) 1 written s0
        edited = T.replace "seats > 2)" "seats > 2)  # mine" written
        s1 = changeDoc (root </> moduleRel) 2 edited s
    (_, Right judged) <- blockBegin ignore (root </> moduleRel) 6 s
    blockState judged `shouldBe` Judged []
    (_, accepted) <- blockAccept (root </> moduleRel) s1
    either id (const "accepted") accepted `shouldBe` "your edit is being tested; accept once its verdict lands"
    fmap fst <$> blockFor (root </> moduleRel) s `shouldReturn` Just judged
    (_, Just _) <- blockChanged (root </> moduleRel) s1
    (_, Right (done, _)) <- blockAccept (root </> moduleRel) s1
    blockState done `shouldBe` Accepted
  it "(R3-2) a waive between an edit and its test (the pause not run yet) is refused; tested, it is open" $ do
    (s0, _) <- blocks redOnStub []
    let s = openDoc (root </> moduleRel) 1 written s0
        s1 = changeDoc (root </> moduleRel) 2 (T.replace "seats > 2)" "seats > 2)  # mine" written) s
    (_, Right judged) <- blockBegin ignore (root </> moduleRel) 6 s
    (_, waived) <- blockWaive (root </> moduleRel) 6 "by design" s1
    either id (const "waived") waived `shouldBe` "your edit is being tested; waive once its verdict lands"
    fmap fst <$> blockFor (root </> moduleRel) s `shouldReturn` Just judged
    (_, Just _) <- blockChanged (root </> moduleRel) s1
    (_, Right _) <- blockWaive (root </> moduleRel) 6 "by design" s1
    pure ()
  it "(N1) a draft that fails, then an edit: the edit is tested" $ do
    (s0, _) <- blocks redOnStub []
    (filler, _, release) <- stalledFiller
    putMVar release ()
    let s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssTestFiller = Just filler}
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    (_, Left _) <- blockDraft ignore (root </> moduleRel) s
    Just (back, _) <- blockFor (root </> moduleRel) s
    blockState back `shouldSatisfy` \case Red _ -> True; _ -> False
    (_, Just tested) <- blockChanged (root </> moduleRel) (changeDoc (root </> moduleRel) 2 written s)
    blockState tested `shouldSatisfy` \case Judged _ -> True; _ -> False
    Just (b, _) <- blockFor (root </> moduleRel) s
    blockState b `shouldBe` blockState tested
  it "(N2) a judge that finishes after a later edit's test is stale: dropped (logged), the later green stands" $ do
    (s0, _) <- blocks redOnStub []
    -- each mutant run waits for its own release, in the order they come
    calls <- newIORef (0 :: Int)
    releases@[r1, r2] <- mapM (const newEmptyMVar) [1 .. 2 :: Int]
    let within _ c = Runner (\l i f -> atomicModifyIORef' calls (\n -> (n + 1, n)) >>= \n -> mapM_ readMVar (take 1 (drop n releases)) >> runTests (pTester (ssPorts s0) c) l i f)
        s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssPorts = (ssPorts s0) {pTesterWithin = within}}
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    -- typed green: tested, then judged (its mutant held)
    let s1 = changeDoc (root </> moduleRel) 2 written s
    first' <- async (blockTest (root </> moduleRel) s1)
    waitUntil ((>= 1) <$> readIORef calls)
    -- typed again in the span: green again, judged again (held too)
    let s2 = changeDoc (root </> moduleRel) 3 (T.replace "seats > 2)" "seats > 2)  # again" written) s
    second <- async (blockChanged (root </> moduleRel) s2)
    waitUntil ((>= 2) <$> readIORef calls)
    Just (green, _) <- blockFor (root </> moduleRel) s
    blockState green `shouldBe` Green
    -- the first judge lands: it judged the text before, so the green stands
    putMVar r1 ()
    (_, stale) <- wait first'
    either id (const "landed") stale `shouldBe` "the magic circle moved on"
    fmap (blockState . fst) <$> blockFor (root </> moduleRel) s `shouldReturn` Just Green
    steps s >>= (`shouldSatisfy` ("\tjudge\tmutation\tstale: 0/1 killed" `T.isInfixOf`))
    putMVar r2 ()
    (_, Just judged) <- wait second
    blockState judged `shouldSatisfy` \case Judged _ -> True; _ -> False
    fmap (blockState . fst) <$> blockFor (root </> moduleRel) s `shouldReturn` Just (blockState judged)
  it "(N3) test is refused while a draft is in flight" $ do
    (s0, _) <- blocks redOnStub []
    (filler, entered, release) <- stalledFiller
    let s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssTestFiller = Just filler}
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    drafting <- async (blockDraft ignore (root </> moduleRel) s)
    takeMVar entered
    (_, tested) <- blockTest (root </> moduleRel) (changeDoc (root </> moduleRel) 2 written s)
    either id (const "tested") tested `shouldBe` "a draft is in flight; wait for it, then test or draft again"
    putMVar release ()
    void (wait drafting)
  it "(N4) a new test's stub whose name is still the stub's (test_name) is not drafted (the block untouched, nothing run); named, it is" $ do
    (s0, runs) <- blocksOn killedBy []
    let reopen = written <> T.unlines ["", "", "class Reopen:", "    def again(self) -> int:", "        return 0"]
        s = (openDoc (root </> moduleRel) 1 reopen s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right (_, Just (testFile, at, body, imports))) <- blockStart ignore (root </> moduleRel) 15 s
    -- the editor expands the stub; the name's tab stop left as it was
    let assertion = T.replace "${2:...}" "Reopen().again() == 1"
        unnamed = expandStub at (T.replace "${1:name}" "name" (assertion body)) (applyEdits imports testText)
    T.lines unnamed `shouldSatisfy` elem "def test_name() -> None:"
    Just (before, _) <- blockFor testFile s
    ran <- runs
    (_, refused) <- blockDraft ignore testFile (openDoc testFile 1 unnamed s)
    either id (const "drafted") refused `shouldBe` "name the new test first: its name is still the stub's"
    fmap fst <$> blockFor testFile s `shouldReturn` Just before
    runs `shouldReturn` ran
    (_, Right (drafted, _)) <- blockDraft ignore testFile (openDoc testFile 1 (T.replace "kills_the_boundary" "reopens_again" (expandStub at (assertion body) (applyEdits imports testText))) s)
    blockState drafted `shouldBe` Green
  it "(N4) a kill's stub is named from its mutant: left as it is, draft is not refused for its name" $ do
    (s0, _) <- blocksOn killedBy []
    let s = (openDoc (root </> moduleRel) 1 written s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
    (_, Right (_, testFile, at, body)) <- blockKill (root </> moduleRel) 6 s
    -- the editor expands the stub; the person writes the assertion only
    let kept = expandStub at (T.replace "${1:test_boundary_int_seats}" "test_boundary_int_seats" body) testText
    T.lines kept `shouldSatisfy` elem "def test_boundary_int_seats() -> None:"
    (_, drafted) <- blockDraft ignore testFile (openDoc testFile 1 kept s)
    either id (const "drafted") drafted `shouldNotBe` "name the new test first: its name is still the stub's"
    drafted `shouldSatisfy` either (const False) (const True)
    steps s >>= (`shouldSatisfy` ("block 2\tdraft\t" `T.isInfixOf`))
  where
    ignore = const (pure ())
    steps s = maybe "" id <$> readText (portsFiles s) (root </> ".olifant/state/steps.log")
    field k v = parseMaybe (J.withObject "block" (.: k)) v :: Maybe Value

root, moduleRel, testRel :: FilePath
root = "/r"
moduleRel = "src/shop/close_table.py"
testRel = "src/shop/close_table__test.py"

testId :: Text
testId = "src/shop/close_table__test.py::test_closes_a_table"

stubbed, written, testText :: Text
stubbed = T.unlines ["class CloseTable:", "    def __init__(self) -> None:", "        self.closed = 0", "", "    def handle(self, seats: int) -> int:", "        raise NotImplementedError(\"later\")", "", "", "def unrelated(n: int) -> bool:", "    return n > 0"]
written = T.replace "raise NotImplementedError(\"later\")" "return int(seats > 2)" stubbed
testText = T.unlines ["from shop.close_table import CloseTable", "", "", "def test_closes_a_table() -> None:", "    table = CloseTable()", "    assert table.handle(4) == 1"]

failing :: Text -> TestRun
failing why = TestRun [] [(testId, Failure (Just 6) why "")] Nothing

-- | The module's test file run test by test (by the names it defines):
-- `test_closes_a_table` red on the stub; `test_kills_the_boundary` red on
-- the boundary mutant (`seats >= 2`); `test_next` red always (the next test,
-- still being written); any other red at its assertion. A test whose
-- arrangement is still the stub's placeholder fails there.
killedBy :: M.Map FilePath Text -> Either Text TestRun
killedBy files = Right (TestRun [i | (i, Nothing) <- results] [(i, f) | (i, Just f) <- results] Nothing)
  where
    (m, t) = (M.findWithDefault "" moduleRel files, M.findWithDefault "" testRel files)
    numbered = zip [1 :: Int ..] (T.splitOn "\n" t)
    results = [(T.pack testRel <> "::" <> n, verdict n body) | (i, l) <- numbered, Just n <- [T.stripPrefix "def " (T.strip l) >>= named], let body = takeWhile (\(_, x) -> not ("def " `T.isPrefixOf` x)) (drop i numbered)]
    named rest = let n = T.takeWhile (\c -> isAlphaNum c || c == '_') rest in if "test" `T.isPrefixOf` n && "(" `T.isPrefixOf` T.drop (T.length n) rest then Just n else Nothing
    lineOf p body = listToMaybe [i | (i, x) <- body, p x]
    verdict n body
      | Just at <- lineOf ("olifant: arrange" `T.isInfixOf`) body = Just (Failure (Just at) "NotImplementedError" "")
      | n == "test_closes_a_table" = if "NotImplementedError" `T.isInfixOf` m then Just (Failure (Just 6) "NotImplementedError: later" "") else Nothing
      | n == "test_kills_the_boundary" = if "NotImplementedError" `T.isInfixOf` m || "seats >= 2" `T.isInfixOf` m then Just (Failure (lineOf ("assert" `T.isInfixOf`) body) "assert 1 == 0" "") else Nothing
      | otherwise = Just (Failure (lineOf (("assert" `T.isPrefixOf`) . T.strip) body) "AssertionError" "")

-- | 'testText' and the next test, still red.
withNext :: Text
withNext = testText <> T.unlines ["", "", "def test_next() -> None:", "    table = CloseTable()", "    assert table.next_one() == 1"]

-- | The test file as the editor leaves it once it expands a kill's stub
-- (its line AT replaced) and the person names the test and its assertion.
expandStub :: Int -> Text -> Text -> Text
expandStub at body text = T.intercalate "\n" (take (at - 1) ls ++ [named] ++ drop at ls)
  where
    ls = T.splitOn "\n" text
    -- a begin's stub (`test_${1:name}`) or a kill's (`${1:test_<from the mutant>}`)
    named = firstStop (T.replace "test_${1:name}" "${1:name}" (T.replace "${2:...}" "table.handle(2) == 0" body))
    firstStop t = case T.breakOn "${1:" t of
      (before, stop) | not (T.null stop) -> before <> "test_kills_the_boundary" <> T.drop 1 (T.dropWhile (/= '}') stop)
      _ -> t

-- | Edits of one text, applied as the editor does (each over the last).
applyEdits :: [FileEdit] -> Text -> Text
applyEdits edits text = foldl (\t e -> applyChange (feRange e) (feText e) t) text edits

-- | The decisions: N answered, and none being asked.
settled :: ServerState -> Int -> IO Bool
settled s n = (\(known, asking) -> M.size known == n && S.null asking) <$> St.decisionsNow s

-- | A block as it stands, whatever moves it took to stand there.
unversioned :: Block -> Block
unversioned b = b {blockVersion = 0}

-- | Until a condition holds (five seconds at most).
waitUntil :: IO Bool -> IO ()
waitUntil ready = go (250 :: Int)
  where
    go 0 = expectationFailure "waited five seconds"
    go n = ready >>= \done -> if done then pure () else threadDelay 20000 >> go (n - 1)

-- | Red while the body is the stub, green once written.
redOnStub :: Text -> TestRun
redOnStub t
  | "NotImplementedError" `T.isInfixOf` t = failing "NotImplementedError: later"
  | otherwise = TestRun [testId] [] Nothing

-- | A server over /r (the module, its test, recorded fills; the python-htmx
-- pack at /pack) whose runner answers by the module's text, and how many
-- runs it made.
blocks :: (Text -> TestRun) -> [(FilePath, Text)] -> IO (ServerState, IO Int)
blocks verdict = blocksWith (Right . verdict)

-- | 'blocks' whose runner may fail to run at all (Left).
blocksWith :: (Text -> Either Text TestRun) -> [(FilePath, Text)] -> IO (ServerState, IO Int)
blocksWith verdict = blocksOn (verdict . M.findWithDefault "" moduleRel)

-- | 'blocksWith' whose runner answers by every file it is given.
blocksOn :: (M.Map FilePath Text -> Either Text TestRun) -> [(FilePath, Text)] -> IO (ServerState, IO Int)
blocksOn = blocksOnSyntax blockSyntax

-- | 'blocksOn' over another syntax.
blocksOnSyntax :: Syntax IO -> (M.Map FilePath Text -> Either Text TestRun) -> [(FilePath, Text)] -> IO (ServerState, IO Int)
blocksOnSyntax syntax = blocksHooked syntax (pure ())

-- | A gate on the runner: armed, every run waits (counted) until it opens.
data Gate = Gate {gArm :: IO (), gEntered :: IO Int, gOpen :: IO (), gWait :: IO ()}

newGate :: IO Gate
newGate = do
  armed <- newIORef False
  entered <- newIORef (0 :: Int)
  opened <- newEmptyMVar
  pure Gate
    { gArm = writeIORef armed True
    , gEntered = readIORef entered
    , gOpen = putMVar opened ()
    , gWait = readIORef armed >>= \a -> if a then atomicModifyIORef' entered (\n -> (n + 1, ())) >> readMVar opened else pure ()
    }

-- | A model that offers nothing: each ask says it ENTERED, waits for
-- RELEASE, then gives up (no attempt: the draft fails).
stalledFiller :: IO (ModelFiller IO, MVar (), MVar ())
stalledFiller = do
  entered <- newEmptyMVar
  release <- newEmptyMVar
  let filler = ModelFiller "stalled" 0 (Filler (\_ -> void (tryPutMVar entered ()) >> readMVar release >> pure (Left "the model gave up")))
  pure (filler, entered, release)

-- | 'blocks' whose runner first runs HOOK (a gate).
blocksGated :: Gate -> (Text -> TestRun) -> [(FilePath, Text)] -> IO (ServerState, IO Int)
blocksGated gate verdict = blocksHooked blockSyntax (gWait gate) (Right . verdict . M.findWithDefault "" moduleRel)

blocksHooked :: Syntax IO -> IO () -> (M.Map FilePath Text -> Either Text TestRun) -> [(FilePath, Text)] -> IO (ServerState, IO Int)
blocksHooked syntax hook verdict = blocksRun syntax hook (const verdict)

-- | 'blocksHooked' whose runner answers by the ids it is asked to run too.
blocksRun :: Syntax IO -> IO () -> ([Text] -> M.Map FilePath Text -> Either Text TestRun) -> [(FilePath, Text)] -> IO (ServerState, IO Int)
blocksRun syntax hook verdict extra = do
  pack <- readTree =<< canonicalizePath "packs/python-htmx"
  (disk, _) <-
    memoryWorkspace $
      [("/pack" </> p, t) | (p, t) <- M.toList pack]
        ++ [ (root </> ".olifant/config.toml", "[packs]\npython-htmx = { path = \"/pack\" }\n")
           , (root </> moduleRel, stubbed)
           , (root </> testRel, testText)
           , (root </> "fills.toml", "[fills]\nbody = \"return int(seats > 2)\"\narrange = \"table = CloseTable()\"\n") ]
        ++ [(root </> p, t) | (p, t) <- extra]
  runs <- newIORef (0 :: Int)
  let runner = Runner $ \_ ids files -> do
        modifyIORef' runs (+ 1)
        hook
        pure (narrow ids <$> verdict ids files)
      -- as pytest does: tests named by id run alone
      narrow ids run
        | not (null ids), all ("::" `T.isInfixOf`) ids = run {trPassed = filter (`elem` ids) (trPassed run), trFailed = filter ((`elem` ids) . fst) (trFailed run)}
        | otherwise = run
      ports = Ports disk syntax (const noChecker) (const noVerifier) (const runner) (\_ _ -> runner) (pure (UTCTime (fromGregorian 2026 10 12) 0)) Nothing Nothing (DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False)) (\_ -> pure Nothing) (\_ _ _ -> pure (Left "no base in tests")) (\_ _ -> pure (Left "no base in tests")) gitStateStamp
  s <- newServerState ports
  pure (s, readIORef runs)

-- | `function handle`'s body (the lines under its def, deeper than it), and
-- a boundary rewrite (`a > b` to `a >= b`) at every `>`; nothing else.
blockSyntax :: Syntax IO
blockSyntax = Syntax query rewrite
  where
    query _ rule src = case T.breakOn "\"^" (TE.decodeUtf8 (BL.toStrict (J.encode rule))) of
      (_, r) | not (T.null r), "$\"" `T.isInfixOf` r -> pure (Right (body (T.takeWhile (/= '$') (T.drop 2 r)) src))
      _ -> pure (Right [])
    rewrite _ rule _ src
      | parseMaybe (J.withObject "rule" (.: "pattern")) rule == Just ("$A > $B" :: Text) = pure (Right (boundaries src))
      | otherwise = pure (Right [])
    indentOf = T.length . T.takeWhile (== ' ')
    -- every def of the name, as ast-grep finds each
    body name src =
      let ls = zip [0 ..] (T.splitOn "\n" src)
       in [ Match (first, 8) (end, T.length l) "" (0, 0)
          | (i, d) <- ls, ("def " <> name <> "(") `T.isInfixOf` d
          , under@((first, _) : _) <- [takeWhile (\(_, l) -> not (T.null (T.strip l)) && indentOf l > indentOf d) (drop (i + 1) ls)]
          , let (end, l) = last under ]
    boundaries src =
      [ Rewrite (i, col) (i, col + T.length w) w (T.replace " > " " >= " w) (offset + col, offset + col + T.length w)
      | (i, l, offset) <- zip3 [0 ..] (T.splitOn "\n" src) (scanl (\o x -> o + T.length x + 1) 0 (T.splitOn "\n" src))
      , " > " `T.isInfixOf` l
      , let (pre, post) = T.breakOn " > " l
            a = T.takeWhileEnd isAlphaNum pre
            w = a <> " > " <> T.takeWhile isAlphaNum (T.drop 3 post)
            col = T.length pre - T.length a
      ]

-- | 'blockSyntax', and every integer is a site of the `number` operator.
numbersToo :: Syntax IO
numbersToo = blockSyntax {querySyntax = query}
  where
    query lang rule src
      | parseMaybe (J.withObject "rule" (.: "kind")) rule == Just ("integer" :: Text) = pure (Right (integers src))
      | otherwise = querySyntax blockSyntax lang rule src
    integers src =
      [ Match (i, col) (i, col + T.length w) w (offset + col, offset + col + T.length w)
      | (i, l, offset) <- zip3 [0 ..] (T.splitOn "\n" src) (scanl (\o x -> o + T.length x + 1) 0 (T.splitOn "\n" src))
      , (col, w) <- runs 0 l
      ]
    runs col l = case T.break isDigit l of
      (_, "") -> []
      (pre, rest) ->
        let (w, after') = T.span isDigit rest
            start = col + T.length pre
         in (start, w) : runs (start + T.length w) after'

readTree :: FilePath -> IO (M.Map FilePath Text)
readTree dir = M.fromList <$> go dir
  where
    go d = do
      names <- listDirectory d
      fmap concat . forM names $ \n -> do
        let p = d </> n
        isDir <- doesDirectoryExist p
        if isDir then go p else (\t -> [(makeRelative dir p, t)]) <$> TIO.readFile p

-- | Two roots: /r1 (commands-use-ports off), /r2 (error) holding a nested
-- project /r2/a; and how often a project config was read.
server :: IO (ServerState, IO Int)
server = serverWith pure

serverWith :: (Workspace IO -> IO (Workspace IO)) -> IO (ServerState, IO Int)
serverWith wrap = do
  (disk, _) <-
    memoryWorkspace
      [ ("/r1/.olifant/config.toml", "[rules]\ncommands-use-ports = \"off\"\n")
      , ("/r1/src/x.py", "x = 1\n")
      , ("/r2/.olifant/config.toml", "[rules]\ncommands-use-ports = \"error\"\n")
      , ("/r2/src/z.py", "z = 1\n")
      , ("/r2/a/.olifant/config.toml", "")
      , ("/r2/a/src/y.py", "y = 1\n") ]
  reads <- newIORef (0 :: Int)
  counted <- wrap disk {readText = \p -> (if "/.olifant/config.toml" `isSuffixOf` p then modifyIORef' reads (+ 1) else pure ()) >> readText disk p}
  let ports = Ports counted noSyntax (const noChecker) (const noVerifier) (const noRunner) (\_ _ -> noRunner) (pure (UTCTime (fromGregorian 2026 10 6) 0)) Nothing Nothing (DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False)) (\_ -> pure Nothing) (\_ _ _ -> pure (Left "no base in tests")) (\_ _ -> pure (Left "no base in tests")) gitStateStamp
  s <- newServerState ports
  pure (s, readIORef reads)

portsFiles :: ServerState -> Workspace IO
portsFiles = pFiles . ssPorts

describeSlot :: Maybe ProjectSlot -> String
describeSlot = \case
  Nothing -> "nothing"
  Just (Loaded _) -> "Loaded"
  Just (Broken why p) -> "Broken " <> show why <> maybe " (no project)" (const " (last good)") p

-- | Apply a replacement of a span (0-based line, UTF-16 column) to a text.
applyChange :: ((Int, Int), (Int, Int)) -> Text -> Text -> Text
applyChange ((l1, c1), (l2, c2)) replacement text = before <> replacement <> after
  where
    offset (l, c) = sum [T.length x + 1 | x <- take l (T.splitOn "\n" text)] + c
    before = T.take (offset (l1, c1)) text
    after = T.drop (offset (l2, c2)) text
