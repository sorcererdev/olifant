{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.ServerSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, takeMVar, tryPutMVar)
import Control.Monad (forM, void)
import Data.Aeson (Value (..), (.:))
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as BL
import Data.Char (isAlphaNum, isDigit)
import Data.Maybe (isNothing, listToMaybe)
import Data.Map.Strict qualified as M
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Olifant.Block (Block (..), BlockState (Accepted, Drafting, Green, Judged, Red, Rejected), Draft (..), helperSentence, hud, open, survivors)
import Olifant.Block qualified as Block
import Olifant.Mutate (Mutant (..))
import Olifant.Language (Failure (..), Match (..), Rewrite (..), Runner (..), Syntax (..), TestRun (..))
import System.Directory (canonicalizePath, doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, (</>))
import Control.Concurrent.Async (async, replicateConcurrently_, wait)
import Data.Set qualified as S
import Olifant.Server.State qualified as St
import Olifant.Server.Block (importedModules, settle, strayLine)
import Olifant.Server.BlockMoves (rejectDeleted)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Olifant.Decide qualified as De
import Olifant.Fill (FillHole (..), FillRequest (..), Filler (..), ModelFiller (..))
import System.IO.Temp (withSystemTempDirectory)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (catalogRulesConfig)
import Olifant.Doctor (DoctorPorts (..))
import Olifant.Language (CheckFailure (..), CheckReport (..), Checker (..), noChecker, noRunner, noSyntax)
import Olifant.Language.Check (Verifier (..), noVerifier)
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
  describe "the test at a line (testAt)" $ do
    let file = T.unlines
          [ "from shop.close_table import CloseTable"   -- 1
          , ""                                          -- 2
          , ""                                          -- 3
          , "def make_table() -> CloseTable:"           -- 4
          , "    return CloseTable()"                   -- 5
          , ""                                          -- 6
          , ""                                          -- 7
          , "@pytest.mark.parametrize(\"n\", [1, 2])"   -- 8
          , "def test_closes(n: int) -> None:"          -- 9
          , "    table = make_table()"                  -- 10
          , ""                                          -- 11
          , "    assert table.handle(n) == 1"           -- 12
          , ""                                          -- 13
          , ""                                          -- 14
          , "class TestSeats:"                          -- 15
          , "    def test_two(self) -> None:"           -- 16
          , "        assert make_table().handle(2) == 0" -- 17
          ]
    it "a line in a test's body is that test" $ testAt file 10 `shouldBe` Just "test_closes"
    it "a blank line inside the body is still that test" $ testAt file 11 `shouldBe` Just "test_closes"
    it "the def line is that test" $ testAt file 9 `shouldBe` Just "test_closes"
    it "a decorator line is its def's test" $ testAt file 8 `shouldBe` Just "test_closes"
    it "a method of a test class is Class::method" $ testAt file 17 `shouldBe` Just "TestSeats::test_two"
    it "outside any test, nothing: imports" $ testAt file 1 `shouldBe` Nothing
    it "outside any test, nothing: a helper that is not a test" $ testAt file 5 `shouldBe` Nothing
    it "the class line itself is no test" $ testAt file 15 `shouldBe` Nothing
    it "a line past the file's end is nothing" $ testAt file 99 `shouldBe` Nothing
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
  describe "a file's counterpart (olifant/tests/file)" $ do
    it "on a module, answers its test file, kind test" $ do
      (s, _) <- recording passing []
      (_, answer) <- testsFile (root </> moduleRel) s
      answer `shouldBe` Right (TestsFile (root </> testRel) True True)
    it "on a test file, answers the module it tests, kind module" $ do
      (s, _) <- recording passing []
      (_, answer) <- testsFile (root </> testRel) s
      answer `shouldBe` Right (TestsFile (root </> moduleRel) False True)
    it "on a module with no test file yet, answers the path it would be, which does not exist" $ do
      (s, _) <- recording passing [("src/shop/lonely.py", "def alone() -> int:\n    return 1\n")]
      (_, answer) <- testsFile (root </> "src/shop/lonely.py") s
      answer `shouldBe` Right (TestsFile (root </> "src/shop/lonely__test.py") True False)
    it "answers as the editor reads it: file, kind, exists" $ do
      (s, _) <- recording passing []
      (_, Right a) <- testsFile (root </> moduleRel) s
      testsFileJson a `shouldBe` J.object ["file" J..= (root </> testRel), "kind" J..= ("test" :: Text), "exists" J..= True]
      (_, Right m) <- testsFile (root </> testRel) s
      lookupIn "kind" (testsFileJson m) `shouldBe` Just (String "module")
    it "refuses a language with no [test] table, in one sentence" $ do
      (s, _) <- recording passing [plain, ("notes/a.txt", "a\n")]
      (_, answer) <- testsFile (root </> "notes/a.txt") s
      answer `shouldSatisfy` either (== "plain has no [test] table: no test file to jump to") (const False)
    it "refuses a test file whose module cannot be named, in one sentence" $ do
      (s, _) <- recording passing [("src/shop/orphan__test.py", "def test_alone() -> None:\n    assert True\n")]
      (_, answer) <- testsFile (root </> "src/shop/orphan__test.py") s
      answer `shouldSatisfy` either (== "no module's test file is src/shop/orphan__test.py: its module cannot be named") (const False)
    it "refuses a file that is its own test file (tests_for names the file itself), in one sentence" $ do
      (s, _) <- recording passing [("/pack/languages/inline.toml", "files = [\"**/*.txt\"]\ncomment = \"#\"\ngrammar = \"python\"\n[test]\npreset = \"pytest\"\ntests_for = \"{= dir =}/{= stem =}.txt\"\n"), ("notes/a.txt", "a\n")]
      (_, answer) <- testsFile (root </> "notes/a.txt") s
      answer `shouldSatisfy` either (== "notes/a.txt is its own test file (inline's tests_for): no other file to jump to") (const False)
    it "refuses a file in no project" $ do
      (s, _) <- recording passing []
      (_, answer) <- testsFile "/elsewhere/x.py" s
      answer `shouldSatisfy` either ("is in no Olifant project" `T.isInfixOf`) (const False)
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
    -- the name is the person's to write: the assertion is drafted from it
    hud tb `shouldBe` "\9656 src/shop/lonely__test.py \183 red: a new test: write its name \183 name it, then draft \183 reject drops it"
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
    -- the test that passed before it is kept
    blockKeep code `shouldBe` [testId]
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
  it "(I5) a new test that passes on the code as it is is green: accepted, it stays as a regression test and nothing begins on it" $ do
    (s0, _) <- blocksOn (\files -> Right (TestRun [T.pack testRel <> "::test_reopens_again"] [] Nothing) <* killedBy files) []
    let module' = written <> T.unlines ["", "", "class Reopen:", "    def again(self) -> int:", "        return 0"]
        s = openDoc (root </> moduleRel) 1 module' s0
    (_, Right (_, Just (testFile, at, body, imports))) <- blockStart ignore (root </> moduleRel) 15 s
    let named = T.replace "kills_the_boundary" "reopens_again" (expandStub at body (applyEdits imports testText))
        st = openDoc testFile 2 named s
    (_, Just tested) <- blockChanged testFile st
    blockState tested `shouldBe` Judged []
    hud tested `shouldSatisfy` ("green \10003 accept" `T.isSuffixOf`)
    steps s >>= (`shouldSatisfy` ("\ttest\tperson\tgreen: already passes" `T.isInfixOf`))
    (_, Right (done, [])) <- blockAccept testFile st
    blockState done `shouldBe` Accepted
    -- the test file keeps it; nothing moves to the module
    fmap fst <$> blockFor testFile st `shouldReturn` Nothing
    fmap fst <$> blockFor (root </> moduleRel) st `shouldReturn` Nothing
  it "(I5) a function that raises in one branch only has a body: a new test that passes on it is green" $ do
    (s0, _) <- blocksOn (\files -> Right (TestRun [T.pack testRel <> "::test_reopens_again"] [] Nothing) <* killedBy files) []
    let module' = written <> T.unlines ["", "", "class Reopen:", "    def again(self) -> int:", "        \"\"\"Reopen once.\"\"\"", "        if self is None:", "            raise NotImplementedError(\"later\")", "        return 0"]
        s = openDoc (root </> moduleRel) 1 module' s0
    (_, Right (_, Just (testFile, at, body, imports))) <- blockStart ignore (root </> moduleRel) 15 s
    let named = T.replace "kills_the_boundary" "reopens_again" (expandStub at body (applyEdits imports testText))
    (_, Just tested) <- blockChanged testFile (openDoc testFile 2 named s)
    blockState tested `shouldBe` Judged []
  it "(I5) a survivor's stub is still verified by its mutant: a test that passes on the code and on the mutant does not stand" $ do
    (n0, _) <- blocksOn (\files -> if "seats >= 2" `T.isInfixOf` M.findWithDefault "" moduleRel files then Right (TestRun [testId] [] Nothing) else killedBy files) []
    let n = (openDoc (root </> moduleRel) 1 written n0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 n
    (_, Right (_, _, _, _, Just later)) <- blockKill (root </> moduleRel) 6 n
    Right (refused, _) <- later ignore
    blockState refused `shouldSatisfy` \case Red f -> "does not notice the mutant" `T.isInfixOf` fMessage f; _ -> False
  it "(I5) a function's first test that passes against a body that raises is red: it asserts nothing about the function" $ do
    (s0, _) <- blocksOn (\files -> Right (TestRun [T.pack testRel <> "::test_reopens_again"] [] Nothing) <* killedBy files) []
    let module' = written <> T.unlines ["", "", "class Reopen:", "    def again(self) -> int:", "        raise NotImplementedError(\"later\")"]
        s = openDoc (root </> moduleRel) 1 module' s0
    (_, Right (_, Just (testFile, at, body, imports))) <- blockStart ignore (root </> moduleRel) 15 s
    let named = T.replace "kills_the_boundary" "reopens_again" (expandStub at body (applyEdits imports testText))
    (_, Just tested) <- blockChanged testFile (openDoc testFile 2 named s)
    blockState tested `shouldSatisfy` \case Red f -> fMessage f == "this test passes against a body that raises: it asserts nothing about the function"; _ -> False

  describe "(A) a stub's assertion drafted from its name" $ do
    let reopen = written <> T.unlines ["", "", "class Reopen:", "    def again(self) -> int:", "        return 0"]
        -- what the model was asked (each hole, its instruction), and what it answers
        drafter runner = do
          (s0, runs) <- blocksOn runner []
          asked <- newIORef []
          let filler = ModelFiller "drafter" 0 (Filler (\req -> modifyIORef' asked (++ [(fhName h, fhInstruction h) | h <- frHoles req]) >> pure (Right (M.fromList [("arrange", "reopen = Reopen()\nresult = reopen.again()"), ("assertion", "assert result == 1")]))))
              s = (openDoc (root </> moduleRel) 1 reopen s0) {ssTestFiller = Just filler}
          (_, Right (_, Just (testFile, at, body, imports))) <- blockStart ignore (root </> moduleRel) 15 s
          -- the person names it; ASSERTION: what they type at its tab stop (`...` left as it is)
          let opened assertion = T.replace "kills_the_boundary" "reopens_again" (expandStub at (T.replace "${2:...}" assertion body) (applyEdits imports testText))
          pure (s, testFile, opened, (asked, runs))
    it "a stub left `assert ...`: the draft writes arrangement and assertion, red at the assertion (lines below its first), accepted: the function begins" $ do
      (s, testFile, opened, (asked, runs)) <- drafter killedBy
      let named = opened "..."
          st = openDoc testFile 1 named s
      ran <- runs
      (_, Right (drafted, [edit])) <- blockDraft ignore testFile st
      blockState drafted `shouldBe` Green
      -- the two holes are verified once, together
      (subtract ran <$> runs) `shouldReturn` 1
      readIORef asked `shouldReturn` [("arrange", "answer with the lines that replace the marker only; set up what the assertion below needs, ending with `result = ` the call the test is about"), ("assertion", "answer with the lines that replace the marker only; assert what 'reopens again' means; one assert, the subject is `result`")]
      let applied = applyChange (feRange edit) (feText edit) named
          ls = T.splitOn "\n" applied
      -- the assertion's line, for the editor's cursor
      Just at <- pure (blockAssertion drafted)
      ls !! (at - 1) `shouldBe` "    assert result == 1"
      take 2 (drop (at - 3) ls) `shouldBe` ["    reopen = Reopen()", "    result = reopen.again()"]
      field "assertion_line" (blockJson drafted 0) `shouldBe` Just (Number (fromIntegral at))
      -- applied: red at its assertion, judged; accepted, the function begins on it
      let st2 = changeDoc testFile 2 applied st
      (_, Just tested) <- blockChanged testFile st2
      blockState tested `shouldBe` Judged []
      hud tested `shouldSatisfy` ("red \10003 accept \183 go make it pass" `T.isSuffixOf`)
      (_, Right _) <- blockAccept testFile st2
      Just (code, _) <- blockFor (root </> moduleRel) st2
      (blockKind code, blockTestId code) `shouldBe` (Block.Code, T.pack testRel <> "::test_reopens_again")
    it "in a test file with the pack's helper (`def command(`), both holes ask that it be passed the adapters arranged" $ do
      (s, testFile, opened, (asked, _)) <- drafter killedBy
      let helped = opened "..." <> "\n\ndef command() -> CloseTable:\n    return CloseTable()\n"
      (_, Right _) <- blockDraft ignore testFile (openDoc testFile 1 helped s)
      instructions <- map snd <$> readIORef asked
      instructions `shouldSatisfy` \is -> length is == 2 && all (helperSentence `T.isInfixOf`) is
    it "an arrangement that fails before the assertion: both holes are asked again, together, and the draft lands; one test run per attempt" $ do
      -- a test whose arrangement raises fails there, before its assertion
      let raising files = case M.lookup testRel files of
            Just t | Just at <- lookup True [("raise ValueError" `T.isInfixOf` l, i) | (i, l) <- zip [1 :: Int ..] (T.splitOn "\n" t)] ->
              Right (TestRun [] [(T.pack testRel <> "::test_reopens_again", Failure (Just at) "ValueError: boom" "")] Nothing)
            _ -> killedBy files
      (s0, runs) <- blocksOn raising []
      asked <- newIORef []
      let answers = [M.fromList [("arrange", "raise ValueError(\"boom\")"), ("assertion", "assert result == 1")], M.fromList [("arrange", "result = Reopen().again()"), ("assertion", "assert result == 1")]]
          filler = ModelFiller "drafter" 1 (Filler (\req -> atomicModifyIORef' asked (\a -> (a ++ [map fhName (frHoles req)], length a)) >>= \n -> pure (Right (answers !! min n 1))))
          s = (openDoc (root </> moduleRel) 1 reopen s0) {ssTestFiller = Just filler}
      (_, Right (_, Just (testFile, at, body, imports))) <- blockStart ignore (root </> moduleRel) 15 s
      let named = T.replace "kills_the_boundary" "reopens_again" (expandStub at (T.replace "${2:...}" "..." body) (applyEdits imports testText))
      ran <- runs
      (_, Right (drafted, [edit])) <- blockDraft ignore testFile (openDoc testFile 1 named s)
      readIORef asked `shouldReturn` [["arrange", "assertion"], ["arrange", "assertion"]]
      blockState drafted `shouldBe` Green
      feText edit `shouldSatisfy` ("result = Reopen().again()" `T.isInfixOf`)
      (subtract ran <$> runs) `shouldReturn` 2
    it "a stub whose assertion the person typed: only the arrangement is drafted" $ do
      (s, testFile, opened, (asked, _)) <- drafter killedBy
      (_, Right (drafted, _)) <- blockDraft ignore testFile (openDoc testFile 1 (opened "Reopen().again() == 1") s)
      blockState drafted `shouldBe` Green
      map fst <$> readIORef asked `shouldReturn` ["arrange"]
      blockAssertion drafted `shouldBe` Nothing
      field "assertion_line" (blockJson drafted 0) `shouldBe` Nothing
    it "a drafted test that passes on the code as it is stands: green, a regression test" $ do
      (s, testFile, opened, _) <- drafter (\files -> Right (TestRun [T.pack testRel <> "::test_reopens_again"] [] Nothing) <* killedBy files)
      let st = openDoc testFile 1 (opened "...") s
      (_, Right (drafted, [edit])) <- blockDraft ignore testFile st
      blockState drafted `shouldBe` Green
      -- applied, its test passes: a regression test to accept
      (_, Just tested) <- blockChanged testFile (changeDoc testFile 2 (applyChange (feRange edit) (feText edit) (opened "...")) st)
      hud tested `shouldSatisfy` ("green \10003 accept" `T.isSuffixOf`)
    it "a drafted test that passes against a body that raises: red, it asserts nothing about the function" $ do
      (s0, _) <- blocksOn (\files -> Right (TestRun [T.pack testRel <> "::test_reopens_again"] [] Nothing) <* killedBy files) []
      let raising = written <> T.unlines ["", "", "class Reopen:", "    def again(self) -> int:", "        raise NotImplementedError(\"later\")"]
          filler = ModelFiller "drafter" 0 (Filler (\_ -> pure (Right (M.fromList [("arrange", "result = Reopen().again()"), ("assertion", "assert result == 1")]))))
          s = (openDoc (root </> moduleRel) 1 raising s0) {ssTestFiller = Just filler}
      (_, Right (_, Just (testFile, at, body, imports))) <- blockStart ignore (root </> moduleRel) 15 s
      let named = T.replace "kills_the_boundary" "reopens_again" (expandStub at (T.replace "${2:...}" "..." body) (applyEdits imports testText))
      (_, Right (drafted, _)) <- blockDraft ignore testFile (openDoc testFile 1 named s)
      blockState drafted `shouldSatisfy` \case Red f -> "passes against a body that raises" `T.isInfixOf` fMessage f; _ -> False

  describe "(R) draft on a red test with no circle: the code is right, the test is drafted" $ do
    let testFile = root </> testRel
        wrongId = T.pack testRel <> "::test_wrong_party"
        methodId = T.pack testRel <> "::TestParty::test_wrong_party"
        -- after 'testText': lines 9 (its def), 10 (its arrangement), 11 (its assertion)
        wrongTest arrange assertion = testText <> T.unlines ["", "", "def test_wrong_party() -> None:", "    " <> arrange, "    " <> assertion]
        -- the test file's run, by its text: `test_wrong_party` red at an
        -- arrangement building `CloseTable(9)`, else at an assertion
        -- `== 5`, else green; `test_closes_a_table` red once the file
        -- patches `CloseTable.handle`, else green
        wrongRun files = Right (TestRun [i | (i, Nothing) <- results] [(i, f) | (i, Just f) <- results] Nothing)
          where
            t = M.findWithDefault "" testRel files
            lineWith frag = listToMaybe [i | (i, l) <- zip [1 :: Int ..] (T.splitOn "\n" t), frag `T.isInfixOf` l]
            results = (testId, if "CloseTable.handle =" `T.isInfixOf` t then Just (Failure (Just 6) "assert 2 == 1" "") else Nothing) : [(if "class TestParty" `T.isInfixOf` t then methodId else wrongId, wrong) | "def test_wrong_party(" `T.isInfixOf` t]
            wrong
              | Just l <- lineWith "CloseTable(9)" = Just (Failure (Just l) "TypeError: CloseTable() takes no arguments" "")
              | Just l <- lineWith "== 5" = Just (Failure (Just l) "assert 0 == 5" "")
              | otherwise = Nothing
        -- the circle's server over TEXT, its model answering each ask in turn (the last again), and what it was asked
        circle text answers = do
          (s0, _) <- blocksOn wrongRun [(testRel, text)]
          asked <- newIORef []
          let filler = ModelFiller "drafter" 0 (Filler (\req -> atomicModifyIORef' asked (\a -> (a ++ [[(fhName h, fhInstruction h) | h <- frHoles req]], length a)) >>= \n -> pure (Right (M.fromList (answers !! min n (length answers - 1))))))
          pure (s0 {ssTestFiller = Just filler}, asked)
        linesOf = T.splitOn "\n"
    it "the first ask drafts the arrangement only, the assertion as it was; green on the code, accepted: the circle closes, nothing moves to the module" $ do
      let given = wrongTest "table = CloseTable(9)" "assert table.handle(1) == 0"
      (s, asked) <- circle given [[("arrange", "table = CloseTable()")]]
      pushes <- newIORef []
      writeIORef (ssOnBlock s) (\b _ -> modifyIORef' pushes (++ [b]))
      (_, Right (drafted, [edit])) <- blockDraftAt ignore testFile 10 s
      (blockKind drafted, blockState drafted, blockTestId drafted, blockFile drafted) `shouldBe` (Block.ExistingTest False, Green, wrongId, testFile)
      -- it opened red on the test's lines, and drafted at once
      map (\b -> (stateName (blockState b), hud b)) <$> readIORef pushes `shouldReturn` [("drafting", "\9656 test_wrong_party \183 red: TypeError: CloseTable() takes no arguments \183 drafting\8230")]
      map (map fst) <$> readIORef asked `shouldReturn` [["arrange"]]
      let applied = applyChange (feRange edit) (feText edit) given
      drop 8 (linesOf applied) `shouldBe` ["def test_wrong_party() -> None:", "    table = CloseTable()", "    assert table.handle(1) == 0", ""]
      blockSpan drafted `shouldBe` (9, 11)
      blockAssertion drafted `shouldBe` Nothing
      let st = openDoc testFile 2 applied s
      (_, Just tested) <- blockChanged testFile st
      blockState tested `shouldBe` Judged []
      hud tested `shouldBe` "\9656 test_wrong_party \183 draft 1 \183 green \10003 accept"
      (_, Right (done, [])) <- blockAccept testFile st
      blockState done `shouldBe` Accepted
      fmap fst <$> blockFor testFile st `shouldReturn` Nothing
      fmap fst <$> blockFor (root </> moduleRel) st `shouldReturn` Nothing
      steps s >>= (`shouldSatisfy` ("\taccept\tperson\tgreen: test drafted" `T.isInfixOf`))
    it "still red at the assertion: the HUD says so and draft is open; the second ask drafts the arrangement and the assertion, its line carried for the cursor" $ do
      let given = wrongTest "table = CloseTable(9)" "assert table.handle(1) == 5"
      (s, asked) <- circle given [[("arrange", "table = CloseTable()")], [("arrange", "table = CloseTable()"), ("assertion", "assert table.handle(1) == 0")]]
      (_, Right (first, [edit])) <- blockDraftAt ignore testFile 10 s
      blockState first `shouldSatisfy` \case Red f -> fLine f == Just 11; _ -> False
      hud first `shouldBe` "\9656 test_wrong_party \183 draft 1 \183 red at the assertion \183 draft again to redraft it, or edit it"
      open first `shouldSatisfy` elem "draft"
      let once = applyChange (feRange edit) (feText edit) given
      drop 9 (linesOf once) `shouldBe` ["    table = CloseTable()", "    assert table.handle(1) == 5", ""]
      (_, Right (second, [edit2])) <- blockDraftAt ignore testFile 10 (openDoc testFile 2 once s)
      blockState second `shouldBe` Green
      asks <- readIORef asked
      map (map fst) asks `shouldBe` [["arrange"], ["arrange", "assertion"]]
      lookup "assertion" (asks !! 1) `shouldBe` Just "answer with the lines that replace the marker only; the code is the spec now: assert what `wrong party` means against what the code returns; keep the test's intent"
      let twice = applyChange (feRange edit2) (feText edit2) once
      Just at <- pure (blockAssertion second)
      linesOf twice !! (at - 1) `shouldBe` "    assert table.handle(1) == 0"
      field "assertion_line" (blockJson second 0) `shouldBe` Just (Number (fromIntegral at))
      -- a third draft is another candidate of both
      (_, Right (third, _)) <- blockDraftAt ignore testFile 10 (openDoc testFile 3 twice s)
      -- (the same text again: no edit to test, judged at once)
      blockState third `shouldBe` Judged []
      map (map fst) <$> readIORef asked `shouldReturn` [["arrange"], ["arrange", "assertion"], ["arrange", "assertion"]]
    it "a draft that breaks a test of the file that passed is red at it: no longer passes" $ do
      let given = wrongTest "table = CloseTable(9)" "assert table.handle(1) == 0"
      (s, _) <- circle given [[("arrange", "table = CloseTable()\nCloseTable.handle = lambda self, seats: 0")]]
      (_, Right (drafted, _)) <- blockDraftAt ignore testFile 10 s
      blockRedAt drafted `shouldBe` Just testId
      hud drafted `shouldBe` "\9656 test_wrong_party \183 draft 1 \183 red: test_closes_a_table no longer passes \183 fix or reject"
    it "refused: a green test, the cursor in no test, a circle open in the file" $ do
      let given = wrongTest "table = CloseTable(9)" "assert table.handle(1) == 0"
      (s, asked) <- circle given [[("arrange", "table = CloseTable()")]]
      (_, green) <- blockDraftAt ignore testFile 5 s
      green `shouldBe` Left "this test passes; begin on a red one, or stub a new case"
      (_, nowhere) <- blockDraftAt ignore testFile 1 s
      nowhere `shouldBe` Left "put the cursor in a test to draft it"
      -- a circle claimed in its module meanwhile (a race: with a circle
      -- open, draft drafts that one): one circle per file, refused
      modifyIORef' (ssBlocks s) (M.insert (root </> moduleRel) (Block.begin 99 (root </> moduleRel) Block.Code written (6, 6) "src/shop/other__test.py::test_x" (Failure Nothing "" ""), 0))
      (_, busy) <- blockDraftAt ignore testFile 10 s
      busy `shouldBe` Left "a magic circle is open in this file: accept or reject it first"
      readIORef asked `shouldReturn` []
      fmap fst <$> blockFor testFile s `shouldReturn` Nothing
    it "reject puts the test's lines back, and only those: another edit of the file stands" $ do
      let given = wrongTest "table = CloseTable(9)" "assert table.handle(1) == 5"
      (s, _) <- circle given [[("arrange", "table = CloseTable()")]]
      (_, Right (_, [edit])) <- blockDraftAt ignore testFile 10 s
      -- the person edits another test meanwhile
      let drafted = applyChange (feRange edit) (feText edit) given
          elsewhere = T.replace "table = CloseTable()\n    assert table.handle(4)" "table = CloseTable()  # mine\n    assert table.handle(4)" drafted
          st = openDoc testFile 2 elsewhere s
      elsewhere `shouldNotBe` drafted
      (_, Right (gone, [restore])) <- blockReject testFile st
      blockState gone `shouldBe` Rejected
      applyChange (feRange restore) (feText restore) elsewhere `shouldBe` T.replace "table = CloseTable()\n    assert table.handle(4)" "table = CloseTable()  # mine\n    assert table.handle(4)" given
    it "an assert inside a `with`: the arrangement ends before the `with`, which stays with the assertion" $ do
      let given = testText <> T.unlines ["", "", "def test_wrong_party() -> None:", "    table = CloseTable(9)", "    with pytest.raises(ValueError):", "        assert table.handle(1) == 0"]
      (s, asked) <- circle given [[("arrange", "table = CloseTable()")]]
      (_, Right (drafted, [edit])) <- blockDraftAt ignore testFile 10 s
      blockState drafted `shouldBe` Green
      map (map fst) <$> readIORef asked `shouldReturn` [["arrange"]]
      drop 8 (linesOf (applyChange (feRange edit) (feText edit) given)) `shouldBe` ["def test_wrong_party() -> None:", "    table = CloseTable()", "    with pytest.raises(ValueError):", "        assert table.handle(1) == 0", ""]
    it "two classes with a test method of one name: the circle on the second drafts, tests and restores the second, never the first" $ do
      let first = T.unlines ["", "", "class TestA:", "    def test_wrong_party(self) -> None:", "        table = CloseTable()", "        assert table.handle(4) == 1"]
          given = testText <> first <> T.unlines ["", "", "class TestParty:", "    def test_wrong_party(self) -> None:", "        table = CloseTable(9)", "        assert table.handle(1) == 5"]
          firstLines = take 6 (drop 6 (linesOf given))
      (s, _) <- circle given [[("arrange", "table = CloseTable()")], [("arrange", "table = CloseTable()"), ("assertion", "assert table.handle(1) == 0")]]
      (_, Right (once, [edit])) <- blockDraftAt ignore testFile 17 s
      (blockTestId once, blockSpan once) `shouldBe` (methodId, (16, 18))
      hud once `shouldSatisfy` ("red at the assertion" `T.isInfixOf`)
      let drafted = applyChange (feRange edit) (feText edit) given
      take 6 (drop 6 (linesOf drafted)) `shouldBe` firstLines
      drop 15 (linesOf drafted) `shouldBe` ["    def test_wrong_party(self) -> None:", "        table = CloseTable()", "        assert table.handle(1) == 5", ""]
      let st = openDoc testFile 2 drafted s
      (_, Right (twice, [edit2])) <- blockDraftAt ignore testFile 17 st
      let redrafted = applyChange (feRange edit2) (feText edit2) drafted
      (blockState twice, blockSpan twice, blockAssertion twice) `shouldBe` (Green, (16, 18), Just 18)
      take 6 (drop 6 (linesOf redrafted)) `shouldBe` firstLines
      drop 17 (linesOf redrafted) `shouldBe` ["        assert table.handle(1) == 0", ""]
      (_, Right (_, [restore])) <- blockReject testFile (openDoc testFile 3 redrafted s)
      applyChange (feRange restore) (feText restore) redrafted `shouldBe` given
    it "an assert inside a bracketed `with (` over several lines: the whole `with` stays with the assertion" $ do
      let given = testText <> T.unlines ["", "", "def test_wrong_party() -> None:", "    table = CloseTable(9)", "    with (", "        pytest.raises(ValueError),", "    ):", "        assert table.handle(1) == 0"]
      (s, _) <- circle given [[("arrange", "table = CloseTable()")]]
      (_, Right (_, [edit])) <- blockDraftAt ignore testFile 10 s
      drop 9 (linesOf (applyChange (feRange edit) (feText edit) given)) `shouldBe` ["    table = CloseTable()", "    with (", "        pytest.raises(ValueError),", "    ):", "        assert table.handle(1) == 0", ""]
    it "a def line ending in a comment is one line" $ do
      let given = testText <> T.unlines ["", "", "def test_wrong_party() -> None:  # noqa: D103", "    table = CloseTable(9)", "    assert table.handle(1) == 0"]
      (s, _) <- circle given [[("arrange", "table = CloseTable()")]]
      (_, drafted) <- blockDraftAt ignore testFile 10 s
      fmap (blockState . fst) drafted `shouldBe` Right Green
    it "a test's leading docstring is its intent: it stays outside the arrangement" $ do
      let given = testText <> T.unlines ["", "", "def test_wrong_party() -> None:", "    \"\"\"A party of one sits.\"\"\"", "    table = CloseTable(9)", "    assert table.handle(1) == 0"]
      (s, _) <- circle given [[("arrange", "table = CloseTable()")]]
      (_, Right (_, [edit])) <- blockDraftAt ignore testFile 11 s
      drop 8 (linesOf (applyChange (feRange edit) (feText edit) given)) `shouldBe` ["def test_wrong_party() -> None:", "    \"\"\"A party of one sits.\"\"\"", "    table = CloseTable()", "    assert table.handle(1) == 0", ""]
    it "a test method in a class is found: its id names its class" $ do
      let given = testText <> T.unlines ["", "", "class TestParty:", "    def test_wrong_party(self) -> None:", "        table = CloseTable(9)", "        assert table.handle(1) == 0"]
      (s, _) <- circle given [[("arrange", "table = CloseTable()")]]
      (_, Right (drafted, [edit])) <- blockDraftAt ignore testFile 11 s
      (blockTestId drafted, blockState drafted) `shouldBe` (methodId, Green)
      drop 9 (linesOf (applyChange (feRange edit) (feText edit) given)) `shouldBe` ["    def test_wrong_party(self) -> None:", "        table = CloseTable()", "        assert table.handle(1) == 0", ""]
    it "refused before any circle opens: a test with no assert line, a def over several lines" $ do
      let noAssert = testText <> T.unlines ["", "", "def test_wrong_party() -> None:", "    table = CloseTable(9)", "    table.handle(1)"]
      (s, asked) <- circle noAssert [[("arrange", "table = CloseTable()")]]
      (_, none) <- blockDraftAt ignore testFile 10 s
      none `shouldBe` Left "this test has no assert line to draft against: edit it by hand"
      let wrapped = testText <> T.unlines ["", "", "def test_wrong_party(", "    seats: int = 1,", ") -> None:", "    table = CloseTable(9)", "    assert table.handle(1) == 0"]
      (s2, asked2) <- circle wrapped [[("arrange", "table = CloseTable()")]]
      (_, several) <- blockDraftAt ignore testFile 12 s2
      several `shouldBe` Left "this test's def spans several lines: edit it by hand"
      readIORef asked `shouldReturn` []
      readIORef asked2 `shouldReturn` []
      fmap fst <$> blockFor testFile s `shouldReturn` Nothing
      fmap fst <$> blockFor testFile s2 `shouldReturn` Nothing
      steps s >>= (`shouldNotSatisfy` ("\tbegin\t" `T.isInfixOf`))
    it "stub on an existing test's circle names that circle" $ do
      let given = wrongTest "table = CloseTable(9)" "assert table.handle(1) == 5"
      (s, _) <- circle given [[("arrange", "table = CloseTable()")]]
      (_, Right _) <- blockDraftAt ignore testFile 10 s
      (_, stubAt) <- blockStub testFile 10 s
      either id (const "stubAt") stubAt `shouldBe` "this circle is a test's; stub from the module's function"
      (_, killed) <- blockKill testFile 10 s
      either id (const "killed") killed `shouldBe` "this circle is a test's; stub from the module's function"

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
  it "(f) a survivor carries its whole mutated line, so a `from` found twice on its line is marked at the right place" $ do
    (s0, _) <- blocks redOnStub []
    (_, Right b) <- blockBegin ignore (root </> testRel) 6 s0
    let m = Mutant {muLine = 2, muOperator = "boundary", muFrom = ">", muTo = ">=", muText = "def f(a, b):\n    return a > b and b > 0\nx = 1", muWaived = Nothing, muDim = False}
        survivor = field "survivors" (blockJson b {blockState = Judged [m]} 0)
    fmap (\v -> case v of Array xs -> [field "mutated" x | x <- foldr (:) [] xs]; _ -> []) survivor
      `shouldBe` Just [Just (String "    return a > b and b > 0")]
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
  it "(h) kill opens a test block in the test file with the stub at the end, drafted at once: the snippet answered, the draft lands arrangement and assertion, verified both ways; applied, accept is open" $ do
    (s0, _) <- blocksOn killedBy []
    live <- newIORef Nothing
    let s = (openDoc (root </> moduleRel) 1 written s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}, ssLiveDoc = Just (\f -> if f == root </> testRel then readIORef live else pure Nothing)}
    (_, Right judged) <- blockBegin ignore (root </> moduleRel) 6 s
    map muLine (survivors judged) `shouldBe` [6]
    (_, Right (code, testFile, at, body, Just later)) <- blockKill (root </> moduleRel) 6 s
    (blockId code, testFile, at) `shouldBe` (blockId judged, root </> testRel, 7)
    -- the stub's test is named from the mutant (`seats > 2` → `seats >= 2`), in the name's tab stop
    body `shouldSatisfy` ("def ${1:test_boundary_int_seats}() -> None:" `T.isInfixOf`)
    Just (tb, 0) <- blockFor testFile s
    let [boundary] = survivors judged
    (blockFile tb, blockSpan tb, blockKind tb) `shouldBe` (testFile, (9, 11), Block.TestFor (Just boundary))
    -- drafting while the editor expands the stub: only reject is open
    blockState tb `shouldSatisfy` \case Drafting _ -> True; _ -> False
    open tb `shouldBe` ["reject"]
    -- the code block is still the module's
    fmap (blockId . fst) <$> blockFor (root </> moduleRel) s `shouldReturn` Just (blockId judged)
    -- the test block's own file has no survivor to stub a test for
    (_, fromTest) <- blockKill testFile 9 s
    either id (const "stubbed") fromTest `shouldBe` "stub a test from the code's magic circle, in its module"
    -- the editor expands the stub (a blank last line more than the stub's
    -- text, as vim.snippet leaves it): that edit is the draft's, not tested
    let editor = editorStub at body testText
        st = openDoc testFile 1 editor s
    writeIORef live (Just (1, editor))
    (_, untested) <- blockChanged testFile st
    untested `shouldBe` Nothing
    -- the draft lands: arrangement and assertion, the mutant fails it, the code passes it
    Right (drafted, [edit]) <- later ignore
    blockState drafted `shouldBe` Green
    map draftBy (blockDrafts drafted) `shouldBe` ["recorded"]
    (fePath edit, feVersion edit) `shouldBe` (testFile, Just 1)
    let applied = applyChange (feRange edit) (feText edit) editor
        ls = T.splitOn "\n" applied
    take 3 (drop 8 ls) `shouldBe` ["def test_boundary_int_seats() -> None:", "    table = CloseTable()", "    assert table.handle(2) == 0"]
    blockAssertion drafted `shouldBe` Just 11
    -- applied: tested, judged, accept open
    (_, Just tested) <- blockChanged testFile (changeDoc testFile 2 applied st)
    blockState tested `shouldBe` Judged []
    open tested `shouldSatisfy` elem "accept"
    log' <- steps s
    log' `shouldSatisfy` ("block 1\tkill\tperson\tboundary on line 6" `T.isInfixOf`)
    [m | l <- T.lines log', "\tblock 2\t" `T.isInfixOf` ("\t" <> l), m : _ <- [drop 2 (T.splitOn "\t" l)]] `shouldSatisfy` (["begin", "drafting", "draft"] `isPrefixOf`)
    -- a runner the mutant never fails: the draft does not stand
    (n0, _) <- blocksOn (\files -> if "seats >= 2" `T.isInfixOf` M.findWithDefault "" moduleRel files then Right (TestRun [testId] [] Nothing) else killedBy files) []
    let n = (openDoc (root </> moduleRel) 1 written n0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 n
    (_, Right (_, _, _, _, Just nlater)) <- blockKill (root </> moduleRel) 6 n
    Right (refused, _) <- nlater ignore
    blockState refused `shouldSatisfy` \case Red f -> "does not notice the mutant" `T.isInfixOf` fMessage f; _ -> False
  it "(h) the editor's text for a survivor's stub, as vim.snippet.expand leaves it (placeholders' text, no markers, a blank line more), arriving while the draft waits: it lands" $ do
    (s0, _) <- blocksOn killedBy []
    live <- newIORef (Just (1, testText))
    let s = (openDoc (root </> moduleRel) 1 written s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}, ssLiveDoc = Just (\f -> if f == root </> testRel then readIORef live else pure Nothing)}
        expanded = testText <> "\n\ndef test_boundary_int_seats() -> None:\n    raise NotImplementedError(\"olifant: arrange: set up what the assertion below needs\")\n    assert ...\n\n"
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
    (_, Right (_, testFile, _, _, Just later)) <- blockKill (root </> moduleRel) 6 s
    -- the editor opens the file as it was, then expands the stub while the model writes
    _ <- forkIO (threadDelay 300000 >> writeIORef live (Just (2, expanded)))
    Right (drafted, [edit]) <- later ignore
    (fePath edit, feVersion edit) `shouldBe` (testFile, Just 2)
    blockState drafted `shouldBe` Green
    applyChange (feRange edit) (feText edit) expanded `shouldBe` testText <> "\n\ndef test_boundary_int_seats() -> None:\n    table = CloseTable()\n    assert table.handle(2) == 0\n\n"
  it "(h) a survivor's stub answered with the whole test file (its imports, its tests, the stub with a body, one more): cut to the stub's arrangement and assertion, logged" $ do
    (s0, _) <- blocksOn killedBy []
    let whole = testText <> "\n\ndef test_boundary_int_seats() -> None:\n    table = CloseTable()\n    assert table.handle(2) == 0\n\n\ndef test_more() -> None:\n    assert CloseTable().handle(9) == 1\n"
        filler = ModelFiller "local" 0 (Filler (\_ -> pure (Right (M.fromList [("arrange", whole), ("assertion", whole)]))))
        s = (openDoc (root </> moduleRel) 1 written s0) {ssTestFiller = Just filler}
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
    (_, Right (_, _, at, body, Just later)) <- blockKill (root </> moduleRel) 6 s
    Right (drafted, [edit]) <- later ignore
    blockState drafted `shouldBe` Green
    let editor = editorStub at body testText
    T.splitOn "\n" (applyChange (feRange edit) (feText edit) editor) `shouldBe` T.splitOn "\n" (testText <> "\n\ndef test_boundary_int_seats() -> None:\n    table = CloseTable()\n    assert table.handle(2) == 0\n\n")
    steps s >>= (`shouldSatisfy` ("block 2\tfill\tolifant\tcut to the hole (" `T.isInfixOf`))
    -- the push's HUD says it
    hud drafted `shouldSatisfy` ("fill cut to the hole (" `T.isInfixOf`)
  describe "(h) a survivor's stub's fills across rounds" $ do
    let rounds answers = do
          (s0, _) <- blocksOn killedBy []
          calls <- newIORef (0 :: Int)
          let filler = ModelFiller "local" 1 (Filler (\_ -> atomicModifyIORef' calls (\n -> (n + 1, n)) >>= \n -> pure (Right (M.fromList (answers !! min n (length answers - 1))))))
              s = (openDoc (root </> moduleRel) 1 written s0) {ssTestFiller = Just filler}
          (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
          (_, Right (_, _, at, body, Just later)) <- blockKill (root </> moduleRel) 6 s
          landed <- later ignore
          log' <- steps s
          pure (landed, editorStub at body testText, log')
    it "a cut fill refused as a file, then a plain one placed: the HUD says nothing of a cut" $ do
      let nested = "def test_boundary_int_seats() -> None:\n    def test_inner() -> None:\n        pass\n    table = CloseTable()\n    assert table.handle(2) == 0"
      (landed, _, log') <- rounds [[("arrange", nested), ("assertion", "assert table.handle(2) == 0")], [("arrange", "table = CloseTable()"), ("assertion", "assert table.handle(2) == 0")]]
      Right (drafted, _) <- pure landed
      blockState drafted `shouldBe` Green
      hud drafted `shouldNotSatisfy` T.isInfixOf "cut to the hole"
      log' `shouldSatisfy` T.isInfixOf "\tfill\tolifant\tthe model answered with more than the hole: a whole file"
    it "a whole file refused on the last round: the hole's earlier attempt is the one written" $ do
      let file = "import os\n\ndef test_closes_a_table() -> None:\n    pass\n\ntable = CloseTable()"
      (landed, editor, _) <- rounds [[("arrange", "table = CloseTable()"), ("assertion", "assert table.handle(2) == 1")], [("arrange", file), ("assertion", "assert table.handle(2) == 1")]]
      Right (_, [edit]) <- pure landed
      applyChange (feRange edit) (feText edit) editor `shouldSatisfy` T.isInfixOf "() -> None:\n    table = CloseTable()\n    assert table.handle(2) == 1"
  describe "(h) a survivor's stub's draft lands on the stub's span" $ do
    let landing edit0 = do
          (s0, _) <- blocksOn killedBy []
          live <- newIORef Nothing
          let s = (openDoc (root </> moduleRel) 1 written s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}, ssLiveDoc = Just (\f -> if f == root </> testRel then readIORef live else pure Nothing), ssStubWaitMs = 300}
          (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
          (_, Right (_, testFile, at, body, Just later)) <- blockKill (root </> moduleRel) 6 s
          let editor = edit0 (editorStub at body testText)
          writeIORef live (Just (3, editor))
          landed <- later ignore
          back <- blockFor testFile s
          pure (editor, landed, fst <$> back)
        drafted = "def test_boundary_int_seats() -> None:\n    table = CloseTable()\n    assert table.handle(2) == 0\n"
    it "the file gained edits elsewhere (an import, the other test): the draft lands beside them, its span and assertion where the editor has them" $ do
      let elsewhere = T.replace "    table = CloseTable()\n    assert table.handle(4)" "    table = CloseTable()  # mine\n    assert table.handle(4)" . ("import pytest\n" <>)
      (editor, Right (b, [edit]), _) <- landing elsewhere
      fePath edit `shouldBe` root </> testRel
      feVersion edit `shouldBe` Just 3
      applyChange (feRange edit) (feText edit) editor `shouldBe` elsewhere (testText <> "\n\n" <> drafted <> "\n")
      (blockSpan b, blockAssertion b) `shouldBe` ((10, 12), Just 12)
    it "whitespace in the span (trailing spaces, a blank line more, tabs for spaces) is no edit of the stub: it lands" $ do
      let spaced = T.replace "() -> None:\n" "() -> None:   \n" . T.replace "    assert ...\n" "\n\tassert ...\n"
      (editor, Right (_, [edit]), _) <- landing spaced
      T.isInfixOf drafted (applyChange (feRange edit) (feText edit) editor) `shouldBe` True
    it "the stub never expanded, the file edited elsewhere: not expanded, said so" $ do
      (_, landed, _) <- landing (const ("import pytest\n" <> testText))
      either id (const "landed") landed `shouldBe` "the stub was not expanded in the editor; draft when it is"
    it "the stub's body typed in: no draft lands, the person's text stands" $ do
      let typed = T.replace "    raise NotImplementedError(\"olifant: arrange: set up what the assertion below needs\")\n" "    table = CloseTable()\n"
      (_, landed, back) <- landing typed
      either id (const "landed") landed `shouldBe` "you typed in the stub while it drafted; your text stands \183 draft when ready"
      fmap blockState back `shouldSatisfy` \case Just (Red _) -> True; _ -> False
      -- the HUD says why nothing landed
      fmap hud back `shouldSatisfy` maybe False ("\183 your text stands \183 draft when ready" `T.isSuffixOf`)
  describe "(h) a test draft changes only its stub" $ do
    let placeholder = "raise NotImplementedError(\"olifant: arrange: set up what the assertion below needs\")"
        -- a new test's stub, named and asserted, drafted when asked
        reopening text answers = do
          (s0, _) <- blocksOn killedBy [(testRel, text)]
          let module' = written <> T.unlines ["", "", "class Reopen:", "    def again(self) -> int:", "        return 0"]
              filler = ModelFiller "local" 0 (Filler (\_ -> pure (Right (M.fromList answers))))
              s = (openDoc (root </> moduleRel) 1 module' s0) {ssTestFiller = Just filler}
          (_, Right (_, Just (testFile, at, body, imports))) <- blockStart ignore (root </> moduleRel) 15 s
          let named = T.replace "kills_the_boundary" "reopens_again" (T.replace "table.handle(2) == 0" "Reopen().again() == 1" (expandStub at body (applyEdits imports text)))
          (_, drafted) <- blockDraft ignore testFile (openDoc testFile 1 named s)
          back <- blockFor testFile s
          log' <- steps s
          pure (named, drafted, fst <$> back, log')
        leftOver = T.unlines ["from shop.close_table import CloseTable", "", "SEVEN = 7", "", "", "def test_left_over() -> None:", "    " <> placeholder, "    assert ...", "", "", "def test_closes_a_table() -> None:", "    table = CloseTable()", "    assert table.handle(SEVEN) == 1"]
    it "a stub left undrafted above this one holds the same placeholder: the fill goes into this stub, the other stands" $ do
      (named, drafted, _, _) <- reopening leftOver [("arrange", "result = Reopen().again()")]
      Right (_, [edit]) <- pure drafted
      let ls = T.splitOn "\n" (applyChange (feRange edit) (feText edit) named)
      take 3 (drop 5 ls) `shouldBe` ["def test_left_over() -> None:", "    " <> placeholder, "    assert ..."]
      take 3 (drop 15 ls) `shouldBe` ["def test_reopens_again() -> None:", "    result = Reopen().again()", "    assert Reopen().again() == 1"]
    it "an import the draft adds, a merged one, a blank line moved: no change outside the test" $ do
      let ours = strayLine (\l -> any (`T.isPrefixOf` l) ["from ", "import "])
          old = "from a import X\n\nSEVEN = 7\n\n\ndef test_a() -> None:\n    pass\n"
      ours old (6, 7) "from a import X, Y\nimport os\n\nSEVEN = 7\n\n\ndef test_a() -> None:\n    y = 1\n    pass\n" (7, 9) `shouldBe` Nothing
      ours old (6, 7) "from a import X\n\nSEVEN = 7\n\ndef test_a() -> None:\n    pass\n" (5, 6) `shouldBe` Nothing
    it "a line gone, changed or added outside the test is the first stray one" $ do
      let ours = strayLine (\l -> any (`T.isPrefixOf` l) ["from ", "import "])
          old = "from a import X\n\nSEVEN = 7\nEIGHT = 8\n\n\ndef test_a() -> None:\n    pass\n"
      ours old (7, 8) "from a import X\n\nEIGHT = 8\n\n\ndef test_a() -> None:\n    pass\n" (6, 7) `shouldBe` Just "SEVEN = 7"
      ours old (7, 8) "from a import X\n\nSEVEN = 7\nEIGHT = 9\n\n\ndef test_a() -> None:\n    pass\n" (7, 8) `shouldBe` Just "EIGHT = 8"
      ours old (7, 8) "from a import X\n\nSEVEN = 7\nEIGHT = 8\nNINE = 9\n\n\ndef test_a() -> None:\n    pass\n" (8, 9) `shouldBe` Just "NINE = 9"
  describe "(h) a new test's circle keeps the test file's other tests green" $ do
    let sevenText = T.unlines ["from shop.close_table import CloseTable", "", "SEVEN = 7", "", "", "def test_closes_a_table() -> None:", "    table = CloseTable()", "    assert table.handle(SEVEN) == 1"]
        -- a runner for pytest's NameError: the module-level SEVEN gone, the test using it fails
        withSeven files = killedBy files <&> \run ->
          if "\nSEVEN = " `T.isInfixOf` ("\n" <> M.findWithDefault "" testRel files)
            then run
            else run {trPassed = filter (/= testId) (trPassed run), trFailed = trFailed run ++ [(testId, Failure (Just 8) "NameError: name 'SEVEN' is not defined" "") | testId `elem` trPassed run]}
        withoutSeven = T.replace "SEVEN = 7\n" "# SEVEN\n"
        -- the draft landed (REMOVE: and the constant gone with it, in the one edit), tested
        viaKill remove = do
          (s0, _) <- blocksOn withSeven [(testRel, sevenText)]
          let s = (openDoc (root </> moduleRel) 1 written s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
          (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
          (_, Right (_, testFile, at, body, Just later)) <- blockKill (root </> moduleRel) 6 s
          Just (tb, _) <- blockFor testFile s
          Right (_, [edit]) <- later ignore
          let editor = editorStub at body sevenText
              applied = applyChange (feRange edit) (feText edit) editor
          (_, Just tested) <- blockChanged testFile (changeDoc testFile 2 (if remove then withoutSeven applied else applied) (openDoc testFile 1 editor s))
          pure (blockKeep tb, tested)
        viaFunction remove = do
          (s0, _) <- blocksOn withSeven [(testRel, sevenText)]
          let module' = written <> T.unlines ["", "", "class Reopen:", "    def again(self) -> int:", "        return 0"]
              s = (openDoc (root </> moduleRel) 1 module' s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
          (_, Right (tb, Just (testFile, at, body, imports))) <- blockStart ignore (root </> moduleRel) 15 s
          let named = T.replace "kills_the_boundary" "reopens_again" (T.replace "table.handle(2) == 0" "Reopen().again() == 1" (expandStub at body (applyEdits imports sevenText)))
              st = openDoc testFile 1 named s
          (_, Right (_, [edit])) <- blockDraft ignore testFile st
          let applied = applyChange (feRange edit) (feText edit) named
          (_, Just tested) <- blockChanged testFile (changeDoc testFile 2 (if remove then withoutSeven applied else applied) st)
          pure (blockKeep tb, tested)
        naming = "red: test_closes_a_table no longer passes \183 fix or reject"
    it "a survivor's stub keeps the file's passing tests: its draft landed green stays green" $ do
      (kept, tested) <- viaKill False
      (kept, blockState tested) `shouldBe` ([testId], Judged [])
    it "a survivor's stub: its draft landed with a test of the file red for it is red, naming that test" $ do
      (_, tested) <- viaKill True
      blockState tested `shouldSatisfy` \case Red _ -> True; _ -> False
      blockRedAt tested `shouldBe` Just testId
      hud tested `shouldSatisfy` (naming `T.isInfixOf`)
    it "a function's stub keeps the file's passing tests: its test red at its assertion stays so" $ do
      (kept, tested) <- viaFunction False
      (kept, blockState tested) `shouldBe` ([testId], Judged [])
    it "a function's stub: its test landed with a test of the file red for it is red, naming that test" $ do
      (_, tested) <- viaFunction True
      blockState tested `shouldSatisfy` \case Red _ -> True; _ -> False
      blockRedAt tested `shouldBe` Just testId
      hud tested `shouldSatisfy` (naming `T.isInfixOf`)
  it "(h) a survivor's stub the editor never expands: no draft lands, and the sentence says so" $ do
    (s0, _) <- blocksOn killedBy []
    let s = (openDoc (root </> moduleRel) 1 written s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}, ssLiveDoc = Just (\f -> pure (if f == root </> testRel then Just (1, testText) else Nothing)), ssStubWaitMs = 300}
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
    (_, Right (_, testFile, _, _, Just later)) <- blockKill (root </> moduleRel) 6 s
    landed <- later ignore
    either id (const "landed") landed `shouldBe` "the stub was not expanded in the editor; draft when it is"
    Just (back, _) <- blockFor testFile s
    blockState back `shouldSatisfy` \case Red _ -> True; _ -> False
  it "(h) a survivor's stub's draft the editor refuses (applyEdit not applied): red, the person's text stands" $ do
    (s0, _) <- blocksOn killedBy []
    let s = (openDoc (root </> moduleRel) 1 written s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
    (_, Right (_, testFile, at, body, Just later)) <- blockKill (root </> moduleRel) 6 s
    Right (drafted, [_]) <- later ignore
    let editor = T.dropEnd 1 (editorStub at body testText)
    (_, Right refused) <- blockUnapplied testFile (blockId drafted) (openDoc testFile 1 editor s)
    blockState refused `shouldBe` Red (Failure Nothing "the editor refused the edit" "")
    map draftOutcome (blockDrafts refused) `shouldBe` ["red: the editor refused the edit"]
    blockOn refused `shouldBe` editor
    blockAssertion refused `shouldBe` Nothing
    -- another circle's number: nothing
    (_, other) <- blockUnapplied testFile (blockId drafted + 1) s
    either id (const "moved") other `shouldBe` "the magic circle moved on"
  it "(h) a survivor's stub's draft cancelled once answered: back at the stub, red, logged" $ do
    (s0, _) <- blocksOn killedBy []
    let s = (openDoc (root </> moduleRel) 1 written s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
    (_, Right (_, testFile, _, _, Just _)) <- blockKill (root </> moduleRel) 6 s
    (_, Right back) <- blockDraftStopped "cancelled" testFile s
    blockState back `shouldSatisfy` \case Red _ -> True; _ -> False
    blockDrafts back `shouldBe` []
    steps s >>= (`shouldSatisfy` ("block 2\tdrafting\tperson\tno draft: cancelled" `T.isInfixOf`))
    -- a block not drafting is left as it is
    (_, again) <- blockDraftStopped "cancelled" testFile s
    either id (const "stopped") again `shouldBe` "no draft is in flight"
  it "(h) a survivor's stub typed over while it drafts: the person's text stands, tested; with no model, the stub waits for draft" $ do
    (s0, _) <- blocksOn killedBy []
    live <- newIORef Nothing
    let s = (openDoc (root </> moduleRel) 1 written s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}, ssLiveDoc = Just (\f -> if f == root </> testRel then readIORef live else pure Nothing)}
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
    (_, Right (_, testFile, at, body, Just later)) <- blockKill (root </> moduleRel) 6 s
    let mine = T.replace "test_boundary_int_seats" "test_kills_the_boundary" (editorStub at body testText)
    writeIORef live (Just (2, mine))
    landed <- later ignore
    either id (const "landed") landed `shouldBe` "you typed in the stub while it drafted; your text stands \183 draft when ready"
    Just (back, _) <- blockFor testFile s
    blockState back `shouldSatisfy` \case Red _ -> True; _ -> False
    blockTestId back `shouldBe` T.pack testRel <> "::test_kills_the_boundary"
    -- no model: the stub waits
    (w0, _) <- blocksOn killedBy []
    let w = openDoc (root </> moduleRel) 1 written w0
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 w
    (_, Right (_, wf, _, _, none)) <- blockKill (root </> moduleRel) 6 w
    isNothing none `shouldBe` True
    Just (waiting, _) <- blockFor wf w
    hud waiting `shouldBe` "\9656 test_boundary_int_seats \183 draft \183 reject drops it"
  it "(h2) kill off a survivor's line takes the top-ranked survivor: the stub and the test block are that mutant's; with none left it is refused" $ do
    (s0, _) <- blocksOn killedBy []
    let s = (openDoc (root </> moduleRel) 1 written s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right judged) <- blockBegin ignore (root </> moduleRel) 6 s
    let [boundary] = survivors judged
    (_, Right (code, testFile, at, body, _)) <- blockKill (root </> moduleRel) 3 s
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
    (_, Right (_, testFile, at, body, _)) <- blockKill (root </> moduleRel) 6 s
    -- its HUD names the stub's test and how to leave
    Just (tb, _) <- M.lookup testFile <$> readIORef (ssBlocks s)
    hud tb `shouldBe` "▸ test_boundary_int_seats · draft · reject drops it"
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
    hud expanded `shouldSatisfy` ("· draft · reject drops it" `T.isSuffixOf`)
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
    (_, Right (_, testFile, at, body, _)) <- blockKill (root </> moduleRel) 6 s
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
    (_, Right (_, testFile, at, body, _)) <- blockKill (root </> moduleRel) 6 s
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
    hud tb `shouldBe` "▸ test_handle · draft · reject drops it"
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
    (_, Right (StubKilled (code, testFile, at, body, _))) <- blockStub (root </> moduleRel) 6 s
    (blockId code, testFile, at) `shouldBe` (blockId judged, root </> testRel, 7)
    body `shouldSatisfy` ("def ${1:test_boundary_int_seats}() -> None:" `T.isInfixOf`)
    Just (tb, 0) <- blockFor testFile s
    blockKind tb `shouldBe` Block.TestFor (Just boundary)
    steps s >>= (`shouldSatisfy` ("block 1\tkill\tperson\tboundary on line 6" `T.isInfixOf`))
    (o0, _) <- blocksOn killedBy []
    let o = openDoc (root </> moduleRel) 1 written o0
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 o
    (_, Right (StubKilled (_, _, _, offBody, _))) <- blockStub (root </> moduleRel) 10 o
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
    (_, Right (StubKilled (_, testFile, _, _, _))) <- blockStub (root </> moduleRel) 6 s
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
    hud ready `shouldSatisfy` ("red ✓ accept · go make it pass" `T.isSuffixOf`)
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
    (_, Right (_, testFile, at, body, Just later)) <- blockKill (root </> moduleRel) 6 s
    -- drafted at once (no live documents here: the stub's text is the file)
    let stubbedTest = T.dropEnd 1 (editorStub at body testText)
        st = openDoc testFile 1 stubbedTest s
    Right (_, [edit]) <- later ignore
    let st2 = changeDoc testFile 2 (applyChange (feRange edit) (feText edit) stubbedTest) st
    (_, Just tested) <- blockChanged testFile st2
    map draftBy (blockDrafts tested) `shouldBe` ["recorded"]
    blockState tested `shouldBe` Judged []
    (_, Right (done, [])) <- blockAccept testFile st2
    blockState done `shouldBe` Accepted
    M.member testFile <$> readIORef (ssBlocks s) `shouldReturn` False
    Just (code, killed) <- blockFor (root </> moduleRel) s
    (blockState code, killed) `shouldBe` (Judged [], 1)
    -- pushed, newest first: the code block judged again (the kill's answer pushes its test block)
    map (\(b, k) -> (blockId b, stateName (blockState b), k)) <$> readIORef pushed `shouldReturn` [(blockId code, "judged", 1)]
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
    (_, Right (_, testFile, at, body, Just later)) <- blockKill (root </> moduleRel) 6 s1
    let stubbedTest = T.dropEnd 1 (editorStub at body withNext)
        st = openDoc testFile 1 stubbedTest s1
    Right (drafted, [edit]) <- later ignore
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
  describe "(I2) the parse gate: a failed Python draft that would not parse is not written" $ do
    let attempt = "        x = seats\n                return x"
        bad = any ("                return" `T.isPrefixOf`) . T.lines
        drafted parser = do
          (s0, _) <- blocks (const (failing "assert 0 == 1")) []
          asked <- newIORef []
          let filler = ModelFiller "rehearsal" 0 (Filler (\_ -> pure (Right (M.fromList [("body", attempt)]))))
              s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssTestFiller = Just filler, ssPorts = (ssPorts s0) {pVerifier = const (Verifier (\_ argv files -> modifyIORef' asked (++ [take 3 argv]) >> parser files))}}
          (_, Right b0) <- blockBegin ignore (root </> testRel) 6 s
          (_, Right (b, edits)) <- blockDraft ignore (root </> moduleRel) s
          log' <- steps s
          (b0, b, edits, log',) <$> readIORef asked
        sentinel = "OLIFANT_PARSE_ERROR: IndentationError: unexpected indent (line 7)\n"
    it "Python's parser says why (its sentinel line): the span keeps its text, the draft is red with that reason, and draft is next" $ do
      (b0, b, edits, _, asked) <- drafted (\files -> pure (Right (True, if any bad (M.elems files) then sentinel else "")))
      edits `shouldBe` []
      blockSpan b `shouldBe` blockSpan b0
      blockState b `shouldBe` Red (Failure Nothing "the draft does not parse: IndentationError: unexpected indent (line 7)" "")
      map draftOutcome (blockDrafts b) `shouldBe` ["red: the draft does not parse: IndentationError: unexpected indent (line 7)"]
      hud b `shouldSatisfy` ("red: the draft does not parse: IndentationError: unexpected indent (line 7) \183 draft" `T.isSuffixOf`)
      take 1 asked `shouldBe` [["python3", "-I", "-c"]]
    it "python3 missing, or failing without the sentinel (a pyenv shim, a traceback): the gate is off, the draft written as before, and the step log says so" $ do
      (_, b, edits, log', _) <- drafted (\_ -> pure (Left (CheckToolMissing "`python3` is not installed (needed to verify)")))
      length edits `shouldBe` 1
      blockState b `shouldSatisfy` \case Red f -> "assert 0 == 1" `T.isInfixOf` fMessage f; _ -> False
      log' `shouldSatisfy` ("\tparse\tolifant\tgate off: `python3` is not installed" `T.isInfixOf`)
      (_, _, edits', log'', _) <- drafted (\_ -> pure (Right (False, "pyenv: python3: command not found\n")))
      length edits' `shouldBe` 1
      log'' `shouldSatisfy` ("\tparse\tolifant\tgate off: pyenv: python3: command not found" `T.isInfixOf`)
    it "a file that does not parse before the draft either (the person's half-typed code): the gate is off, the draft written" $ do
      (_, _, edits, log', asked) <- drafted (\_ -> pure (Right (True, sentinel)))
      length edits `shouldBe` 1
      length asked `shouldBe` 2
      log' `shouldSatisfy` ("\tparse\tolifant\tgate off: the file does not parse without the draft either" `T.isInfixOf`)
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
    let recording = ModelFiller "recording" 0 (Filler (\req -> atomicModifyIORef' seen (\cs -> (frContext req : cs, ())) >> pure (Right (M.fromList [("body", "return int(seats > 2)"), ("arrange", "table = CloseTable()"), ("assertion", "assert table.handle(2) == 0")]))))
        s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssTestFiller = Just recording}
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    (_, Right (_, [edit])) <- blockDraft ignore (root </> moduleRel) s
    [code] <- readIORef seen
    lookup testRel code `shouldBe` Just testText
    -- judged, a survivor killed: its test drafted with the module in view
    let s1 = changeDoc (root </> moduleRel) 2 (applyChange (feRange edit) (feText edit) stubbed) s
    (_, Just _) <- blockChanged (root </> moduleRel) s1
    (_, Right (_, _, _, _, Just later)) <- blockKill (root </> moduleRel) 6 s1
    Right _ <- later ignore
    test : _ <- readIORef seen
    lookup moduleRel test `shouldBe` Just written
  describe "(I6) a draft sees the project modules its file and the file beside import" $ do
    let portRel = "src/shop/ports/table_repository.py"
        portText = T.unlines ["class TableRepository:", "    def table(self, n: int) -> int: ...", "    def save(self, n: int) -> None: ..."]
        importing = T.unlines ["import os", "from shop.ports.table_repository import TableRepository", "from shop.close_table import CloseTable", ""] <> stubbed
        contextOf extra moduleText = contextWith extra moduleText testText
        contextWith extra moduleText testText' = do
          (s0, _) <- blocksOn killedBy ((portRel, portText) : extra)
          seen <- newIORef []
          let recording = ModelFiller "recording" 0 (Filler (\req -> atomicModifyIORef' seen (\cs -> (frContext req : cs, ())) >> pure (Right (M.fromList [("body", "return int(seats > 2)"), ("arrange", "table = CloseTable()"), ("assertion", "assert table.handle(2) == 0")]))))
              s = (openDoc (root </> testRel) 1 testText' (openDoc (root </> moduleRel) 1 moduleText s0)) {ssTestFiller = Just recording}
          (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
          (_, Right (_, [edit])) <- blockDraft ignore (root </> moduleRel) s
          [code] <- readIORef seen
          pure (code, s, applyChange (feRange edit) (feText edit) moduleText, seen)
    it "a code draft sees the port its module imports, under its path; the standard library and the drafted file add nothing" $ do
      (code, _, _, _) <- contextOf [] importing
      map fst code `shouldBe` [testRel, portRel]
      lookup portRel code `shouldBe` Just portText
    it "an import of more than 120 lines is cut to 120 and says how many more" $ do
      let big = T.unlines ["x" <> T.pack (show i) <> " = " <> T.pack (show i) | i <- [0 .. 199 :: Int]]
      (code, _, _, _) <- contextOf [("src/shop/big.py", big)] ("from shop.big import x0\n" <> stubbed)
      let Just cut = lookup "src/shop/big.py" code
      length (T.lines cut) `shouldBe` 121
      take 1 (T.lines cut) `shouldBe` ["x0 = 0"]
      last (T.lines cut) `shouldBe` "# \8230 (80 more lines)"
    it "the file beside first, then the drafted file's imports, then the beside file's" $ do
      (code, _, _, _) <- contextWith [("src/shop/ports/clock.py", "class Clock: ...\n")] importing (testText <> "from shop.ports.clock import Clock\n")
      map fst code `shouldBe` [testRel, portRel, "src/shop/ports/clock.py"]
    it "what those modules import comes after them (a port's models), one level only" $ do
      let port' = "from shop.models.table import Table\n\n" <> portText
          tableText = "from shop.models.seat import Seat\n\nclass Table:\n    id: int\n    seats: int\n"
      (code, _, _, _) <- contextOf [(portRel, port'), ("src/shop/models/table.py", tableText), ("src/shop/models/seat.py", "class Seat: ...\n")] importing
      map fst code `shouldBe` [testRel, portRel, "src/shop/models/table.py"]
      lookup "src/shop/models/table.py" code `shouldBe` Just tableText
    it "at most 8 imported files, the first 8 in order" $ do
      let ms = ["m" <> T.pack (show i) | i <- [0 .. 9 :: Int]]
      (code, _, _, _) <- contextOf [(T.unpack ("src/shop/" <> m <> ".py"), m <> " = 1\n") | m <- ms] (T.unlines ["from shop." <> m <> " import " <> m | m <- ms] <> stubbed)
      map fst code `shouldBe` testRel : [T.unpack ("src/shop/" <> m <> ".py") | m <- take 8 ms]
    it "at most 600 imported lines in all: five files of 120, the sixth left out" $ do
      let ms = ["m" <> T.pack (show i) | i <- [0 .. 5 :: Int]]
          long m = T.unlines [m <> "_" <> T.pack (show j) <> " = 1" | j <- [0 .. 149 :: Int]]
      (code, _, _, _) <- contextOf [(T.unpack ("src/shop/" <> m <> ".py"), long m) | m <- ms] (T.unlines ["import shop." <> m | m <- ms] <> stubbed)
      map fst code `shouldBe` testRel : [T.unpack ("src/shop/" <> m <> ".py") | m <- take 5 ms]
      sum [length (T.lines t) - 1 | (p, t) <- code, p /= testRel] `shouldBe` 600
    it "reads Python's absolute imports: aliases, several names, brackets, a star and relative imports" $ do
      importedModules "import a.b as c, d\nfrom .x import y\nfrom p.q import (\n    r,\n)\nfrom m import n as k, *  # c\n" `shouldBe` ["a.b", "d", "p.q", "m", "m.n"]
    it "a test draft sees the module and the modules it imports; the test file itself is never repeated" $ do
      (_, s, drafted, seen) <- contextOf [] importing
      let s1 = changeDoc (root </> moduleRel) 2 drafted s
      (_, Just _) <- blockChanged (root </> moduleRel) s1
      Just (judged, _) <- blockFor (root </> moduleRel) s1
      [line] <- pure (map muLine (survivors judged))
      (_, Right (_, _, _, _, Just later)) <- blockKill (root </> moduleRel) line s1
      Right _ <- later ignore
      test : _ <- readIORef seen
      map fst test `shouldBe` [moduleRel, portRel]
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
    -- (the kill's own run of the test file waits at the gate too: the kill is settled before it)
    killing <- async (blockKill (root </> moduleRel) 6 s1)
    waitUntil ((>= 2) <$> gEntered gate)
    gOpen gate
    (_, Right _) <- wait killing
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
  it "(N4) a kill's stub is named from its mutant: left as it is, its draft is not refused for its name" $ do
    (s0, _) <- blocksOn killedBy []
    let s = (openDoc (root </> moduleRel) 1 written s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
    (_, Right (_, _, _, body, Just later)) <- blockKill (root </> moduleRel) 6 s
    T.lines (Block.snippetText body) `shouldSatisfy` elem "def test_boundary_int_seats() -> None:"
    drafted <- later ignore
    either id (const "drafted") drafted `shouldBe` "drafted"
    steps s >>= (`shouldSatisfy` ("block 2\tdraft\t" `T.isInfixOf`))
  it "(K1) a circle keeps the file's passing tests green: a draft that passes its test but breaks a sibling is red at the sibling, and accept is refused" $ do
    (s0, _) <- blocksRun blockSyntax (pure ()) (keepingSibling "seats > 2") [(testRel, siblingText)]
    let s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right b) <- blockBegin ignore (root </> testRel) 6 s
    blockKeep b `shouldBe` [siblingId]
    (_, Right (drafted, _)) <- blockDraft ignore (root </> moduleRel) s
    blockState drafted `shouldSatisfy` \case Red f -> (fLine f, fMessage f) == (Just 10, "assert 0 == 1"); _ -> False
    (blockRedAt drafted, hud drafted) `shouldSatisfy` \(at, h) -> at == Just siblingId && "red: test_sibling no longer passes · fix or reject" `T.isSuffixOf` h
    (_, accepted) <- blockAccept (root </> moduleRel) s
    accepted `shouldSatisfy` either (const True) (const False)
    steps s >>= (`shouldSatisfy` ("\tdraft\trecorded\tred: test_sibling no longer passes" `T.isInfixOf`))
  it "(K2) a draft that passes its test and the file's other passing tests is green" $ do
    (s0, _) <- blocksRun blockSyntax (pure ()) (keepingSibling "seats > 9") [(testRel, siblingText)]
    let s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    (_, Right (drafted, _)) <- blockDraft ignore (root </> moduleRel) s
    blockState drafted `shouldBe` Green
  it "(K3) the test move after an edit in the span that breaks a sibling is red at the sibling" $ do
    (s0, _) <- blocksRun blockSyntax (pure ()) (keepingSibling "seats > 5") [(testRel, siblingText)]
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s0
    let s = changeDoc (root </> moduleRel) 2 (T.replace "seats > 2" "seats > 5" written) s0
    (_, Right b) <- blockTest (root </> moduleRel) s
    blockState b `shouldSatisfy` \case Red f -> (fLine f, fMessage f) == (Just 10, "assert 0 == 1"); _ -> False
    hud b `shouldSatisfy` ("red: test_sibling no longer passes · fix or reject" `T.isSuffixOf`)
    steps s >>= (`shouldSatisfy` ("\ttest\tperson\tred: test_sibling no longer passes" `T.isInfixOf`))
    -- fixed: green again, then judged
    (_, Right fixed) <- blockTest (root </> moduleRel) (changeDoc (root </> moduleRel) 3 written s)
    blockState fixed `shouldSatisfy` \case Judged _ -> True; _ -> False
  it "(K4) a test block keeps its file's passing tests: a sibling gone red meanwhile makes it red, naming it" $ do
    (s0, _) <- blocksOn killedBy []
    let reopen = written <> T.unlines ["", "", "class Reopen:", "    def again(self) -> int:", "        return 0"]
        s = openDoc (root </> moduleRel) 1 reopen s0
    (_, Right (tb, Just (testFile, at, body, imports))) <- blockStart ignore (root </> moduleRel) 15 s
    (blockKind tb, blockKeep tb) `shouldBe` (Block.TestFor Nothing, [testId])
    -- arranged by hand (no placeholder left); test_closes_a_table passed at
    -- its begin, and the module's body goes back to the stub: red
    let arranged = T.intercalate "\n" . filter (not . T.isInfixOf "olifant: arrange") . T.splitOn "\n"
        named = arranged $ T.replace "kills_the_boundary" "reopens_again" (T.replace "table.handle(2) == 0" "Reopen().again() == 1" (expandStub at body (applyEdits imports testText)))
        st = changeDoc (root </> moduleRel) 2 (T.replace "return int(seats > 2)" "raise NotImplementedError(\"later\")" reopen) (openDoc testFile 1 named s)
    (_, Right tested) <- blockTest testFile st
    blockState tested `shouldSatisfy` \case Red _ -> True; _ -> False
    blockRedAt tested `shouldBe` Just testId
  it "(K5) a kept test gone from its file's run is kept no more, and the circle goes green" $ do
    (s0, _) <- blocksRun blockSyntax (pure ()) (asPytest [("test_sibling", "seats > 2")]) [(testRel, siblingText)]
    (_, Right b) <- blockBegin ignore (root </> testRel) 6 s0
    blockKeep b `shouldBe` [siblingId]
    let s = changeDoc (root </> moduleRel) 2 (T.replace "seats > 2" "seats > 1" written) (openDoc (root </> testRel) 1 testText s0)
    (_, Right tested) <- blockTest (root </> moduleRel) s
    blockState tested `shouldSatisfy` \case Judged _ -> True; _ -> False
    blockKeep tested `shouldBe` []
  it "(K10) a parametrize list edited: the case gone is kept no more, the case left still is (green, then red when it breaks)" $ do
    let p n = T.pack testRel <> "::test_p[" <> n <> "]"
        params vs = testText <> T.unlines ["", "", "@pytest.mark.parametrize(\"n\", [" <> vs <> "])", "def test_p(n: int) -> None:", "    assert CloseTable().handle(n) == 0"]
    (s0, _) <- blocksRun blockSyntax (pure ()) (asPytest [("test_p[2]", "seats > 1")]) [(testRel, params "1, 2")]
    (_, Right b) <- blockBegin ignore (root </> testRel) 6 s0
    blockKeep b `shouldBe` [p "1", p "2"]
    let s = changeDoc (root </> moduleRel) 2 written (openDoc (root </> testRel) 1 (params "2") s0)
    (_, Right tested) <- blockTest (root </> moduleRel) s
    blockState tested `shouldSatisfy` \case Judged _ -> True; _ -> False
    blockKeep tested `shouldBe` [p "2"]
    (_, Right broke) <- blockTest (root </> moduleRel) (changeDoc (root </> moduleRel) 3 (T.replace "seats > 2" "seats > 1" written) s)
    blockRedAt broke `shouldBe` Just (p "2")
  it "(K11) a kept method deleted from its class is kept no more while another class's of its name still runs" $ do
    let classes keepA = testText <> T.unlines ((if keepA then ["", "", "class TestA:", "    def test_y(self) -> None:", "        assert CloseTable().handle(1) == 0"] else []) ++ ["", "", "class TestB:", "    def test_y(self) -> None:", "        assert CloseTable().handle(0) == 0"])
        a = T.pack testRel <> "::TestA::test_y"
        bId = T.pack testRel <> "::TestB::test_y"
    (s0, _) <- blocksRun blockSyntax (pure ()) (asPytest []) [(testRel, classes True)]
    (_, Right b) <- blockBegin ignore (root </> testRel) 6 s0
    blockKeep b `shouldBe` [a, bId]
    let s = changeDoc (root </> moduleRel) 2 written (openDoc (root </> testRel) 1 (classes False) s0)
    (_, Right tested) <- blockTest (root </> moduleRel) s
    blockState tested `shouldSatisfy` \case Judged _ -> True; _ -> False
    blockKeep tested `shouldBe` [bId]
  it "(K6) a kill's test, accepted, is kept: a later draft that breaks it is red at it" $ do
    (s0, _) <- blocksOn killedBy [("breaks.toml", "[fills]\nbody = \"return int(seats >= 2)\"\n")]
    let s = (openDoc (root </> moduleRel) 1 written s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right _) <- blockBegin ignore (root </> moduleRel) 6 s
    (_, Right (_, testFile, at, body, Just later)) <- blockKill (root </> moduleRel) 6 s
    -- drafted at once (no live documents here: the stub's text is the file)
    let stubbedTest = T.dropEnd 1 (editorStub at body testText)
        st = openDoc testFile 1 stubbedTest s
    Right (_, [edit]) <- later ignore
    let st2 = changeDoc testFile 2 (applyChange (feRange edit) (feText edit) stubbedTest) st
    (_, Just _) <- blockChanged testFile st2
    (_, Right _) <- blockAccept testFile st2
    Just (code, _) <- blockFor (root </> moduleRel) st2
    blockKeep code `shouldBe` [T.pack testRel <> "::test_boundary_int_seats"]
    (_, Right (drafted, _)) <- blockDraft ignore (root </> moduleRel) st2 {ssConfig = defaultServerConfig {scFills = Just (root </> "breaks.toml")}}
    blockRedAt drafted `shouldBe` Just (T.pack testRel <> "::test_boundary_int_seats")
    hud drafted `shouldSatisfy` ("red: test_boundary_int_seats no longer passes · fix or reject" `T.isSuffixOf`)
  it "(K7) a function begun green keeps the tests that passed, its own aside" $ do
    (s0, _) <- blocksRun blockSyntax (pure ()) (keepingSibling "seats > 9") [(testRel, siblingText)]
    (_, Right b) <- blockBegin ignore (root </> moduleRel) 6 (openDoc (root </> moduleRel) 1 written s0)
    (blockTestId b, blockKeep b) `shouldBe` (testId, [siblingId])
  it "(K8) a kept parametrized test (its id `test_p[1]`) stays kept: a draft that breaks it is red at it" $ do
    let paramId = T.pack testRel <> "::test_p[1]"
        params = testText <> T.unlines ["", "", "@pytest.mark.parametrize(\"n\", [1])", "def test_p(n: int) -> None:", "    assert CloseTable().handle(n) == 0"]
    (s0, _) <- blocksRun blockSyntax (pure ()) (asPytest [("test_p[1]", "seats > 2")]) [(testRel, params)]
    let s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right b) <- blockBegin ignore (root </> testRel) 6 s
    blockKeep b `shouldBe` [paramId]
    (_, Right (drafted, _)) <- blockDraft ignore (root </> moduleRel) s
    (blockRedAt drafted, blockKeep drafted) `shouldBe` (Just paramId, [paramId])
    hud drafted `shouldSatisfy` ("red: test_p[1] no longer passes · fix or reject" `T.isSuffixOf`)
  it "(K9) a draft red at a kept test whose output is long (the fill's reason cut before its id) is still red at it" $ do
    let long = T.intercalate "\n" ["E   line " <> T.pack (show i) | i <- [1 .. 60 :: Int]]
    (s0, _) <- blocksRun blockSyntax (pure ()) (keepingSiblingAs siblingId long "seats > 2") [(testRel, siblingText)]
    let s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    (_, Right (drafted, _)) <- blockDraft ignore (root </> moduleRel) s
    blockRedAt drafted `shouldBe` Just siblingId
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

-- | The test file with a sibling test after the block's, and its id.
siblingText :: Text
siblingText = testText <> T.unlines ["", "", "def test_sibling() -> None:", "    assert CloseTable().handle(1) == 0"]

siblingId :: Text
siblingId = T.pack testRel <> "::test_sibling"

-- | A runner as pytest is, over the test file it is given: each test it
-- defines by its pytest id (a parametrized one's cases `test_p[1]`, a
-- method's `TestA::test_y`); `test_closes_a_table` red on the stub, a
-- test named in BREAKS red while the module holds its text, the rest
-- green. An id it does not have is no test: the run errs (exit 4).
asPytest :: [(Text, Text)] -> [Text] -> M.Map FilePath Text -> Either Text TestRun
asPytest breaks ids files
  | any (\i -> "::" `T.isInfixOf` i && i `notElem` defined && not (any ((i <> "[") `T.isPrefixOf`) defined)) ids = Right (TestRun [] [] (Just "ERROR: not found"))
  | otherwise = Right (TestRun [i | (i, Nothing) <- results] [(i, f) | (i, Just f) <- results] Nothing)
  where
    m = M.findWithDefault "" moduleRel files
    defined = map fst results
    results = [(T.pack testRel <> "::" <> n, verdict n) | n <- names Nothing [] (T.lines (M.findWithDefault "" testRel files))]
    names _ _ [] = []
    names cls params (l : rest)
      | Just c <- T.stripPrefix "class " l = names (Just (T.takeWhile (\x -> isAlphaNum x || x == '_') c)) [] rest
      | Just vs <- T.stripPrefix "@pytest.mark.parametrize(" (T.strip l) = names cls (map T.strip (T.splitOn "," (T.takeWhile (/= ']') (T.drop 1 (T.dropWhile (/= '[') vs))))) rest
      | Just d <- T.stripPrefix "def " (T.strip l), "test" `T.isPrefixOf` d =
          let n = T.takeWhile (\x -> isAlphaNum x || x == '_') d
              owned = if " " `T.isPrefixOf` l then maybe n (<> "::" <> n) cls else n
           in (if null params then [owned] else [owned <> "[" <> v <> "]" | v <- params]) ++ names (if " " `T.isPrefixOf` l then cls else Nothing) [] rest
      | not (" " `T.isPrefixOf` l), not (T.null (T.strip l)) = names Nothing params rest
      | otherwise = names cls params rest
    verdict n
      | n == "test_closes_a_table" = if "NotImplementedError" `T.isInfixOf` m then Just (Failure (Just 6) "NotImplementedError: later" "") else Nothing
      | Just frag <- lookup n breaks, frag `T.isInfixOf` m = Just (Failure (Just 10) "assert 0 == 1" "")
      | otherwise = Nothing

-- | The module's test file: `test_closes_a_table` red on the stub,
-- `test_sibling` (its assertion on line 10) red when the module holds
-- BREAKS, green otherwise.
keepingSibling :: Text -> [Text] -> M.Map FilePath Text -> Either Text TestRun
keepingSibling = keepingSiblingAs siblingId ""

-- | 'keepingSibling' with the sibling's id and its failure's output given.
keepingSiblingAs :: Text -> Text -> Text -> [Text] -> M.Map FilePath Text -> Either Text TestRun
keepingSiblingAs sid out breaks _ files = Right (TestRun ([testId | null own] ++ [sid | null sibling]) (own ++ sibling) Nothing)
  where
    m = M.findWithDefault "" moduleRel files
    own = [(testId, Failure (Just 6) "NotImplementedError: later" "") | "NotImplementedError" `T.isInfixOf` m]
    sibling = [(sid, Failure (Just 10) "assert 0 == 1" out) | breaks `T.isInfixOf` m]

-- | The module's test file run test by test (by the names it defines):
-- `test_closes_a_table` red on the stub; `test_kills_the_boundary` (or any
-- test asserting `table.handle(2) == 0`) red on the boundary mutant
-- (`seats >= 2`); `test_next` red always (the next test,
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
      | n == "test_kills_the_boundary" || any (("table.handle(2) == 0" `T.isInfixOf`) . snd) body = if "NotImplementedError" `T.isInfixOf` m || "seats >= 2" `T.isInfixOf` m then Just (Failure (lineOf ("assert" `T.isInfixOf`) body) "assert 1 == 0" "") else Nothing
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

-- | The test file as the editor leaves it once it expands a stub (its
-- line AT replaced) and nothing is typed: each tab stop its text, and a
-- blank last line more (vim.snippet's).
editorStub :: Int -> Text -> Text -> Text
editorStub at body text = T.intercalate "\n" (take (at - 1) ls ++ [Block.snippetText body] ++ drop at ls) <> "\n"
  where
    ls = T.splitOn "\n" text

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
           , (root </> "fills.toml", "[fills]\nbody = \"return int(seats > 2)\"\narrange = \"table = CloseTable()\"\nassertion = \"assert table.handle(2) == 0\"\n") ]
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
