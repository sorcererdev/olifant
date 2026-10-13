{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.ServerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM)
import Data.Aeson (Value (..), (.:))
import Data.Aeson qualified as J
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as BL
import Data.Char (isAlphaNum)
import Data.Map.Strict qualified as M
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Olifant.Block (Block (..), BlockState (Accepted, Green, Judged, Red, Rejected), Draft (..), hud, open, survivors)
import Olifant.Block qualified as Block
import Olifant.Mutate (Mutant (..))
import Olifant.Language (Failure (..), Match (..), Rewrite (..), Runner (..), Syntax (..), TestRun (..))
import System.Directory (canonicalizePath, doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, (</>))
import Control.Concurrent.Async (wait)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
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
blocksWith verdict extra = do
  pack <- readTree =<< canonicalizePath "packs/python-htmx"
  (disk, _) <-
    memoryWorkspace $
      [("/pack" </> p, t) | (p, t) <- M.toList pack]
        ++ [ (root </> ".olifant/config.toml", "[packs]\npython-htmx = { path = \"/pack\" }\n")
           , (root </> moduleRel, stubbed)
           , (root </> testRel, testText)
           , (root </> "fills.toml", "[fills]\nbody = \"return int(seats > 2)\"\n") ]
        ++ [(root </> p, t) | (p, t) <- extra]
  runs <- newIORef (0 :: Int)
  let runner = Runner $ \_ _ files -> do
        modifyIORef' runs (+ 1)
        pure (verdict (M.findWithDefault "" moduleRel files))
      ports = Ports disk blockSyntax (const noChecker) (const noVerifier) (const runner) (pure (UTCTime (fromGregorian 2026 10 12) 0)) Nothing Nothing (DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False)) (\_ -> pure Nothing) (\_ _ _ -> pure (Left "no base in tests")) (\_ _ -> pure (Left "no base in tests")) gitStateStamp
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
  let ports = Ports counted noSyntax (const noChecker) (const noVerifier) (const noRunner) (pure (UTCTime (fromGregorian 2026 10 6) 0)) Nothing Nothing (DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False)) (\_ -> pure Nothing) (\_ _ _ -> pure (Left "no base in tests")) (\_ _ -> pure (Left "no base in tests")) gitStateStamp
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
