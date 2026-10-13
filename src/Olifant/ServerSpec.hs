{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.ServerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Monad (forM, void)
import Data.Aeson (Value (..), (.:))
import Data.Aeson qualified as J
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
import Control.Concurrent.Async (async, wait)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Olifant.Decide qualified as De
import System.IO.Temp (withSystemTempDirectory)
import Data.List (isSuffixOf)
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (catalogRulesConfig)
import Olifant.Doctor (DoctorPorts (..))
import Olifant.Language (noChecker, noRunner, noSyntax)
import Olifant.Language.Check (noVerifier)
import Olifant.Process (cancelled, gitStateStamp)
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
  it "(b) a second begin in the same file is refused while one is pending; a module with no test says to write one" $ do
    (s, _) <- blocks redOnStub [("src/shop/lonely.py", "def alone() -> int:\n    return 1\n")]
    (_, Right _) <- blockBegin ignore (root </> testRel) 6 s
    (_, again) <- blockBegin ignore (root </> moduleRel) 6 s
    again `shouldSatisfy` either ("pending" `T.isInfixOf`) (const False)
    (_, lonely) <- blockBegin ignore (root </> "src/shop/lonely.py") 2 s
    lonely `shouldSatisfy` either ("no test for this module yet" `T.isInfixOf`) (const False)
  it "(c) draft lands only if the document is still the version it planned against (Review Focus 1)" $ do
    (s0, _) <- blocks redOnStub []
    let s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right b) <- blockBegin ignore (root </> testRel) 6 s
    (_, moved) <- blockDraft ignore (root </> moduleRel) s {ssLiveVersion = Just (\_ -> pure (Just 2))}
    either id (const "drafted") moved `shouldBe` "the file changed while drafting; your text stands"
    fmap fst <$> blockFor (root </> moduleRel) s `shouldReturn` Just b
    (_, Right (b', edits)) <- blockDraft ignore (root </> moduleRel) s {ssLiveVersion = Just (\_ -> pure (Just 1))}
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
    (_, nowhere) <- blockKill (root </> moduleRel) 3 s
    nowhere `shouldSatisfy` either ("no survivor" `T.isInfixOf`) (const False)
    (_, Right (code, testFile, at, body)) <- blockKill (root </> moduleRel) 6 s
    (blockId code, testFile, at) `shouldBe` (blockId judged, root </> testRel, 7)
    body `shouldSatisfy` ("def test_${1:name}()" `T.isInfixOf`)
    Just (tb, 0) <- blockFor testFile s
    let [boundary] = survivors judged
    (blockFile tb, blockSpan tb, blockKind tb) `shouldBe` (testFile, (9, 11), Block.TestFor (Just boundary))
    blockState tb `shouldSatisfy` \case Red _ -> True; _ -> False
    -- the code block is still the module's
    fmap (blockId . fst) <$> blockFor (root </> moduleRel) s `shouldReturn` Just (blockId judged)
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
    threadDelay 300000
    readIORef asked `shouldReturn` 4
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
    either id (const "landed") landed `shouldBe` "the block moved on"
    blockFor (root </> moduleRel) s `shouldReturn` Nothing
  it "(I1) a draft that fails puts the block's state back" $ do
    (s0, _) <- blocks redOnStub []
    let s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "fills.toml")}}
    (_, Right b) <- blockBegin ignore (root </> testRel) 6 s
    (_, Left _) <- blockDraft ignore (root </> moduleRel) s {ssLiveVersion = Just (\_ -> pure (Just 2))}
    fmap fst <$> blockFor (root </> moduleRel) s `shouldReturn` Just b
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
    [why | Left why <- results] `shouldSatisfy` all ("pending" `T.isInfixOf`)
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
  it "(I2) a draft that never produced an attempt makes no edits and no draft" $ do
    (s0, _) <- blocks redOnStub [("nofill.toml", "[fills]\narrange = \"x = 1\"\n")]
    let s = (openDoc (root </> moduleRel) 1 stubbed s0) {ssConfig = defaultServerConfig {scFills = Just (root </> "nofill.toml")}}
    (_, Right b) <- blockBegin ignore (root </> testRel) 6 s
    (_, none) <- blockDraft ignore (root </> moduleRel) s
    none `shouldSatisfy` either ("no recorded fill" `T.isInfixOf`) (const False)
    fmap fst <$> blockFor (root </> moduleRel) s `shouldReturn` Just b
  it "(I3) the judge runs each mutant with a timeout of its own, from the baseline's wall time" $ do
    (s0, _) <- blocks redOnStub []
    seen <- newIORef []
    let s = (openDoc (root </> moduleRel) 1 written s0) {ssPorts = (ssPorts s0) {pTesterWithin = \secs c -> Runner (\l i f -> atomicModifyIORef' seen (\xs -> (secs : xs, ())) >> runTests (pTester (ssPorts s0) c) l i f)}}
    (_, Right judged) <- blockBegin ignore (root </> moduleRel) 6 s
    map muLine (survivors judged) `shouldBe` [6]
    readIORef seen `shouldReturn` [5]
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
    named = T.replace "${1:name}" "kills_the_boundary" (T.replace "${2:...}" "table.handle(2) == 0" body)

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

-- | 'blocks' whose runner first runs HOOK (a gate).
blocksGated :: Gate -> (Text -> TestRun) -> [(FilePath, Text)] -> IO (ServerState, IO Int)
blocksGated gate verdict = blocksHooked blockSyntax (gWait gate) (Right . verdict . M.findWithDefault "" moduleRel)

blocksHooked :: Syntax IO -> IO () -> (M.Map FilePath Text -> Either Text TestRun) -> [(FilePath, Text)] -> IO (ServerState, IO Int)
blocksHooked syntax hook verdict extra = do
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
        pure (narrow ids <$> verdict files)
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
    query _ rule src
      | "^handle$" `T.isInfixOf` TE.decodeUtf8 (BL.toStrict (J.encode rule)) = pure (Right (body src))
      | otherwise = pure (Right [])
    rewrite _ rule _ src
      | parseMaybe (J.withObject "rule" (.: "pattern")) rule == Just ("$A > $B" :: Text) = pure (Right (boundaries src))
      | otherwise = pure (Right [])
    indentOf = T.length . T.takeWhile (== ' ')
    body src =
      let ls = zip [0 ..] (T.splitOn "\n" src)
       in case [(i, l) | (i, l) <- ls, "def handle(" `T.isInfixOf` l] of
            [(i, d)] -> case takeWhile (\(_, l) -> not (T.null (T.strip l)) && indentOf l > indentOf d) (drop (i + 1) ls) of
              under@((first, _) : _) -> let (end, l) = last under in [Match (first, 8) (end, T.length l) "" (0, 0)]
              [] -> []
            _ -> []
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
