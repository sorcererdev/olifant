module Olifant.Server.LoopSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.List (find, isSuffixOf, sort)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime)
import Olifant.Decide (Question (..), Shape (..))
import Olifant.Process (newCancel, neverCancel)
import Olifant.Server.Edits (FileEdit (..))
import Olifant.Server.Errand (Block (..), Ghost (..))
import Olifant.Server.Stub (StubAnswer (..))
import Olifant.Server.State (ServerConfig (..), ServerState (..))
import Olifant.Server.Telemetry (Decision (..), OutcomeRecord (..), ReconcileRecord (..), Record (..), appendRecord, readRecords, recordsPath)
import System.Directory (doesFileExist)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.App (Ports (..), Project)
import Olifant.Language (processRunner)
import Olifant.LspSpec (needsUv, withPython)
import Olifant.Plans (Anchor (..), Line (..), TestState (..), parsePlan)
import Olifant.Server (newServerState, projectFor)
import Olifant.Server.Loop
import Olifant.Server.Plans (Resolved (..), resolveAnchor)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import Control.Concurrent.Async (async, waitCatch)
import Control.Exception (bracket, throwIO)
import System.IO (hGetLine)
import System.Process (CreateProcess (..), StdStream (..), callProcess, createProcess, proc, readProcess, terminateProcess)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = factsSpec >> workSetSpec >> questionsSpec >> reconcileSpec >> stepTestsSpec

workSetSpec :: Spec
workSetSpec = describe "Olifant.Server.Loop (the work set)" $ do
  it "(a) on main the baseline is the plan's first commit; an uncommitted plan uses HEAD" $
    withPython [(table, tableText)] $ \_ dir -> do
      initRepo dir
      write dir "plans/t.md" "# T\n"
      git dir ["add", "plans"]
      git dir ["commit", "-q", "-m", "the plan"]
      planSha <- gitLine dir ["rev-parse", "HEAD"]
      write dir table labelEdited
      git dir ["commit", "-q", "-am", "an edit"]
      baseline dir "plans/t.md" `shouldReturn` Just planSha
      write dir "plans/u.md" "# U\n"
      headSha <- gitLine dir ["rev-parse", "HEAD"]
      headSha `shouldNotBe` planSha
      baseline dir "plans/u.md" `shouldReturn` Just headSha
  it "(b) on a branch the baseline is the merge-base with main" $
    withLoop "# T\n" [] $ \_ _ dir -> do
      first <- gitLine dir ["rev-parse", "HEAD"]
      write dir table labelEdited
      git dir ["commit", "-q", "-am", "more on main"]
      mainSha <- gitLine dir ["rev-parse", "HEAD"]
      git dir ["checkout", "-q", "-b", "close-table"]
      write dir table (T.replace "self.seats" "self.seats + 1" labelEdited)
      git dir ["commit", "-q", "-am", "on the branch"]
      mainSha `shouldNotBe` first
      baseline dir "plans/t.md" `shouldReturn` Just mainSha
  it "(b2) with only origin's default branch, the merge-base is with origin/<default>" $
    withLoop "# T\n" [] $ \_ _ dir -> do
      first <- gitLine dir ["rev-parse", "HEAD"]
      git dir ["update-ref", "refs/remotes/origin/main", "HEAD"]
      git dir ["symbolic-ref", "refs/remotes/origin/HEAD", "refs/remotes/origin/main"]
      git dir ["branch", "-q", "-m", "main", "close-table"]
      write dir table labelEdited
      git dir ["commit", "-q", "-am", "on the branch"]
      baseline dir "plans/t.md" `shouldReturn` Just first
  it "(c) a changed, an added and a removed unit, with fingerprints" $
    withLoop "# T\n" [(table, threeWays)] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" (parsePlan "# T\n")
      let at n = find ((== n) . cuName) (wsChanged ws)
      fmap cuChange (at "spare") `shouldBe` Just Added
      fmap cuChange (at "Table.other") `shouldBe` Just Removed
      fmap cuFingerprint (at "Table.other") `shouldBe` Just Nothing
      fmap cuHeader (at "Table.label") `shouldBe` Just "def label(self) -> str:"
      case at "Table.label" of
        Just ChangedUnit {cuChange = Changed old, cuFingerprint = Just now, cuNow = Just n, cuBase = Just b} -> do
          old `shouldNotBe` now
          map T.length [old, now] `shouldBe` [7, 7]
          n `shouldSatisfy` T.isInfixOf "\"t-\""
          b `shouldSatisfy` T.isInfixOf "return self.id"
        other -> expectationFailure ("Table.label: " <> show other)
      map cuPath (wsChanged ws) `shouldSatisfy` all (== table)
      wsBaseline ws `shouldSatisfy` (/= Nothing)
  it "(d) a moved unit is one Moved, not a Removed and an Added" $ do
    slotText <- TIO.readFile ("packs/python-htmx/fixtures/app" </> slot)
    withLoop "# T\n" [(table, T.replace helper "" tableText), (slot, slotText <> "\n\n" <> helper)] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" []
      map (\c -> (cuPath c, cuName c, cuChange c)) (wsChanged ws) `shouldBe` [(slot, "helper", Moved table "helper")]
  it "(d2) a git-mv'd module's units are Moved, and a step at the old path is touched" $ do
    let plan = "# T\n\n- [ ] keep the helper @" <> T.pack table <> ":helper\n"
        renamed = "src/bistro/booking/models/tables.py"
    withLoopPorts plan [] $ \ports dir -> do
      git dir ["mv", table, renamed]
      git dir ["commit", "-q", "-m", "a rename"]
      (s, p) <- loaded ports dir
      ws <- workSet s p "plans/t.md" (parsePlan plan)
      map (\c -> (cuPath c, cuChange c)) (wsChanged ws) `shouldSatisfy` all (\(f, c) -> f == renamed && case c of Moved old _ -> old == table; _ -> False)
      map cuName (wsChanged ws) `shouldSatisfy` (\ns -> all (`elem` ns) ["Table.label", "helper"])
      map (\(l, c) -> (lnRow l, cuName c)) (wsTouched ws) `shouldBe` [(2, "helper")]
  it "(d3) a path with non-ASCII characters is not lost" $
    withLoop "# T\n" [("src/bistro/café.py", "def crème() -> int:\n    return 1\n")] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" []
      map (\c -> (cuPath c, cuName c, cuChange c)) (wsChanged ws) `shouldBe` [("src/bistro/café.py", "crème", Added)]
  it "(e) accounted units are left out, across plans" $
    withLoopPorts "# T\n" [(table, labelEdited)] $ \ports dir -> do
      (s0, p0) <- loaded ports dir
      fp <- maybe (fail "Table.label does not resolve") pure . rsFingerprint =<< resolveAnchor s0 p0 (Anchor table "Table.label" 0 0)
      write dir "plans/other.md" ("# O\n\n- [x] x @" <> T.pack table <> ":Table.label ✓ " <> fp <> "\n")
      -- loaded again: a server sees a new file once its listing is stale
      (s, p) <- loaded ports dir
      ws <- workSet s p "plans/t.md" (parsePlan "# T\n")
      map cuName (wsChanged ws) `shouldContain` ["Table.label"]
      map cuName (wsUnaccounted ws) `shouldNotContain` ["Table.label"]
      -- its class changed with it, and is not unaccounted on its own
      map cuName (wsChanged ws) `shouldContain` ["Table"]
      map cuName (wsUnaccounted ws) `shouldNotContain` ["Table"]
  it "(f) a step's own changed anchor is touched, not unaccounted" $ do
    let plan = "# T\n\n- [ ] tidy the label @" <> T.pack table <> ":Table.label\n"
    withLoop plan [(table, labelEdited)] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" (parsePlan plan)
      map (\(l, c) -> (lnRow l, cuName c)) (wsTouched ws) `shouldBe` [(2, "Table.label")]
      map cuName (wsUnaccounted ws) `shouldNotContain` ["Table.label"]
      map cuName (wsUnaccounted ws) `shouldNotContain` ["Table"]
  it "(f2) a changed class is not unaccounted when one of its methods is" $
    withLoop "# T\n" [(table, labelEdited)] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" (parsePlan "# T\n")
      map cuName (wsChanged ws) `shouldContain` ["Table", "Table.label"]
      map cuName (wsUnaccounted ws) `shouldBe` ["Table.label"]
  it "(g) files without a language are skipped" $ do
    pyproject <- TIO.readFile "packs/python-htmx/fixtures/app/pyproject.toml"
    withLoop "# T\n" [("pyproject.toml", pyproject <> "\n# edited\n"), (table, labelEdited)] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" []
      wsChanged ws `shouldSatisfy` not . null
      map cuPath (wsChanged ws) `shouldSatisfy` all (== table)
  it "(h) no git: Nothing baseline, empty sets" $
    withPython [("plans/t.md", "# T\n")] $ \ports dir -> do
      (s, p) <- loaded ports dir
      baseline dir "plans/t.md" `shouldReturn` Nothing
      workSet s p "plans/t.md" [] `shouldReturn` WorkSet Nothing [] [] 0 []
  it "(i) the cap" $
    withLoop "# T\n" [("src/bistro/many.py", T.concat ["def f" <> tshow i <> "() -> int:\n    return " <> tshow i <> "\n\n\n" | i <- [1 .. 45 :: Int]])] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" []
      length (wsChanged ws) `shouldBe` 45
      length (wsUnaccounted ws) `shouldBe` unaccountedCap
      unaccountedCap `shouldBe` 40
      wsMore ws `shouldBe` 5

-- | The python fixture with TABLE's units and the plan, committed on main,
-- then EDITS written, and the project loaded.
withLoop :: Text -> [(FilePath, Text)] -> (ServerState -> Project -> FilePath -> IO a) -> IO a
withLoop plan edits act = withLoopPorts plan edits $ \ports dir -> loaded ports dir >>= \(s, p) -> act s p dir

-- | 'withLoop' before the project is loaded.
withLoopPorts :: Text -> [(FilePath, Text)] -> (Ports -> FilePath -> IO a) -> IO a
withLoopPorts plan edits act =
  withPython [("plans/t.md", plan), (table, tableText)] $ \ports dir -> do
    initRepo dir
    forM_ edits (uncurry (write dir))
    act ports dir

loaded :: Ports -> FilePath -> IO (ServerState, Project)
loaded ports dir = do
  s0 <- newServerState ports
  projectFor (dir </> "plans/t.md") s0 >>= \case
    (s, Just (_, p)) -> pure (s, p)
    _ -> fail "no project"

initRepo :: FilePath -> IO ()
initRepo dir = do
  git dir ["init", "-q", "-b", "main"]
  git dir ["add", "."]
  git dir ["commit", "-q", "-m", "the fixture"]

git :: FilePath -> [String] -> IO ()
git dir args = callProcess "git" (["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t"] ++ args)

gitLine :: FilePath -> [String] -> IO Text
gitLine dir args = T.strip . T.pack <$> readProcess "git" (["-C", dir] ++ args) ""

write :: FilePath -> FilePath -> Text -> IO ()
write dir path text = createDirectoryIfMissing True (takeDirectory (dir </> path)) >> TIO.writeFile (dir </> path) text

table, slot :: FilePath
table = "src/bistro/booking/models/table.py"
slot = "src/bistro/booking/models/slot.py"

tableText, helper, labelEdited, threeWays :: Text
tableText =
  T.unlines
    [ "from dataclasses import dataclass", "", "", "@dataclass(frozen=True)", "class Table:", "    id: str", "    seats: int", ""
    , "    def label(self) -> str:", "        return self.id", "", "    def other(self) -> int:", "        return self.seats", "", "" ]
    <> helper
helper = "def helper() -> int:\n    return 1\n"
labelEdited = T.replace "return self.id" "return \"t-\" + self.id" tableText
threeWays = T.replace "    def other(self) -> int:\n        return self.seats\n" "" labelEdited <> "\n\ndef spare() -> None:\n    pass\n"

tshow :: Show a => a -> Text
tshow = T.pack . show

factsSpec :: Spec
factsSpec = describe "Olifant.Server.Loop (facts)" $ do
  let ls = parsePlan "- [ ] s @src/a.py:F\n  - test: it works @tests/t.py:test_it ●\n  - test: new @tests/t.py:test_new"
      done = parsePlan "- [x] s @src/a.py:F ✓ abc1234\n  - test: it works @tests/t.py:test_it ●"
      step0 = case ls of (l : _) -> l; [] -> error "empty plan"
      cu = ChangedUnit "src/a.py" "F" (Just "abc1234") (Changed "x") "def F" Nothing Nothing
  it "(a) a moved unit is re-anchored wherever it is named" $
    reanchorFacts (parsePlan "- [ ] s @src/a.py:F\n  - also @src/a.py:F ✓ abc1234\n- claim @src/b.py:G ✓ 1111111") [ChangedUnit "src/c.py" "F" (Just "abc1234") (Moved "src/a.py" "F") "def F" Nothing Nothing]
      `shouldBe` [Replace 0 "- [ ] s @src/c.py:F", Replace 1 "  - also @src/c.py:F ✓ abc1234"]
  it "(b) a touched step's green children go pending" $
    pendingFacts ls [(step0, cu)] `shouldBe` [Replace 1 "  - test: it works @tests/t.py:test_it ○"]
  it "(c) all green ticks the step at its fingerprint" $
    testFacts ls 0 [("test_it", TestGreen), ("test_new", TestGreen)] (Just "abc1234")
      `shouldBe` [ Replace 1 "  - test: it works @tests/t.py:test_it ●", Replace 2 "  - test: new @tests/t.py:test_new ●"
                 , Replace 0 "- [x] s @src/a.py:F ✓ abc1234" ]
  it "(d) one unmarked child keeps the step open" $
    testFacts ls 0 [("test_it", TestGreen)] (Just "abc1234")
      `shouldBe` [Replace 1 "  - test: it works @tests/t.py:test_it ●"]
  it "(e) a red test on a done step un-ticks it and says so once" $ do
    let facts = testFacts done 0 [("test_it", TestRed)] (Just "abc1234")
    facts `shouldBe` [Replace 1 "  - test: it works @tests/t.py:test_it ✗", Replace 0 "- [ ] s @src/a.py:F", Insert 1 ["  - ✗ test_it red again"]]
    let applied = parsePlan "- [x] s @src/a.py:F ✓ abc1234\n  - test: it works @tests/t.py:test_it ✗\n  - ✗ test_it red again"
        inserts = filter (\f -> case f of Insert _ _ -> True; _ -> False)
    inserts (testFacts applied 0 [("test_it", TestRed)] (Just "abc1234")) `shouldBe` []
  it "(e2) a note for test_it does not hide test_item's; two reds, one noted, insert only the other" $ do
    let p = parsePlan "- [x] s @src/a.py:F ✓ abc1234\n  - test: a @tests/t.py:test_it ✗\n  - test: b @tests/t.py:test_item ●\n  - ✗ test_it red again"
    testFacts p 0 [("test_it", TestRed), ("test_item", TestRed)] Nothing
      `shouldSatisfy` elem (Insert 3 ["  - ✗ test_item red again"])
  it "(e3) only direct test children count; a title with a checkbox is kept; a nested step is skipped" $ do
    let p = parsePlan "- [ ] fix \"- [ ]\" @src/a.py:F\n  - test: a @tests/t.py:test_a ○\n  - [ ] inner @src/a.py:G\n    - test: g @tests/t.py:test_g ○"
    testFacts p 0 [("test_a", TestGreen), ("test_g", TestRed)] Nothing
      `shouldBe` [Replace 1 "  - test: a @tests/t.py:test_a ●", Replace 0 "- [x] fix \"- [ ]\" @src/a.py:F"]
  it "(f) no fingerprint ticks without a mark; factJson shapes" $ do
    testFacts ls 0 [("test_it", TestGreen), ("test_new", TestGreen)] Nothing
      `shouldSatisfy` (elem (Replace 0 "- [x] s @src/a.py:F"))
    factJson (Replace 2 "t") `shouldBe` object ["row" .= (2 :: Int), "text" .= ("t" :: Text)]
    factJson (Insert 2 ["a"]) `shouldBe` object ["after" .= (2 :: Int), "lines" .= (["a"] :: [Text])]

questionsSpec :: Spec
questionsSpec = describe "Olifant.Server.Loop (questions)" $ do
  let ls = parsePlan "# T\n\n## Other\n\n- [ ] elsewhere\n- [x] finished @src/a.py:G ✓ abc1234\n\n## Mine · branch b\n\nWhat this branch does.\n\n- [ ] here @src/a.py:F\n  - test: it works @tests/t.py:test_it ●\n"
      cu = ChangedUnit "src/a.py" "F" (Just "abc1234") (Changed "x") "def F():" (Just "def F():\n    return 2") (Just "def F():\n    return 1")
      opts q = case qShape q of Choice os -> map fst os; _ -> []
      at r = case drop r ls of (l : _) -> l; [] -> error "no row"
  it "(m) plan-step offers the open steps, the branch's section first, then none" $ do
    let q = planStepQuestion (Just "b") ls ("src/a.py", [cu])
    qKind q `shouldBe` "plan-step"
    opts q `shouldBe` ["s11", "s4", "none"]
    map fst (qContext q) `shouldBe` ["file", "units", "diff", "sections"]
    lookup "diff" (qContext q) `shouldSatisfy` maybe False (\d -> T.isInfixOf "-     return 1" d && T.isInfixOf "+     return 2" d)
    lookup "sections" (qContext q) `shouldSatisfy` maybe False (T.isInfixOf "What this branch does.")
    opts (planStepQuestion Nothing ls ("src/a.py", [cu])) `shouldBe` ["s4", "s11", "none"]
  it "(n) plan-state offers done, in progress, diverged for an open step; still done, reopened, diverged for a done one" $ do
    let q = planStateQuestion (at 11) ls [cu]
    (qKind q, opts q) `shouldBe` ("plan-state", ["done", "in_progress", "diverged"])
    map fst (qContext q) `shouldBe` ["step", "children", "diff", "tests"]
    lookup "tests" (qContext q) `shouldSatisfy` maybe False (T.isInfixOf "green")
    opts (planStateQuestion (at 5) ls [cu]) `shouldBe` ["still_done", "reopened", "diverged"]
  it "(l) parseWords reads numbered lines and leaves gaps" $
    parseWords 3 "1: the table answers label\n3: ok" `shouldBe` [Just "the table answers label", Nothing, Just "ok"]
  it "(l2) parseWords ignores numbers out of range and blank answers; gate is 0.6" $ do
    parseWords 2 "0: no\n2:   \n4: no\n1. dotted\n" `shouldBe` [Just "dotted", Nothing]
    gate `shouldBe` 0.6

reconcileSpec :: Spec
reconcileSpec = describe "Olifant.Server.Loop (reconcile)" $ do
  it "(a) nothing changed → no record, empty answer" $
    withReconcile tidyPlan [] jevConfig (Just (answersOf [("plan-step", "s7")])) $ \dir s -> do
      (_, a) <- run False dir s
      (raFacts a, length (raBlocks a)) `shouldBe` ([], 0)
      reconciles dir `shouldReturn` []
      asked dir `shouldReturn` []
  it "(b) a changed unit placed under a step is an also ghost with its unit" $
    withReconcile tidyPlan [(table, helperEdited)] jevConfig (Just (answersOf [("plan-step", "s7"), ("plan-state", "in_progress")])) $ \dir s -> do
      (_, a) <- run False dir s
      fp <- fpNow dir s "helper"
      map summary (raBlocks a) `shouldBe` [(7, Just "- [ ] tidy the helper", ["  - also @src/bistro/booking/models/table.py:helper"], False, Just (table, "helper", fp))]
      asked dir `shouldReturn` ["plan-step", "plan-state"]
  it "(c) a touched open step answered done is a replacing ticked ghost" $ do
    let plan = "# T\n\n## S\n\n- [ ] tidy the helper @src/bistro/booking/models/table.py:helper\n"
    withReconcile plan [(table, helperEdited)] jevConfig (Just (answersOf [("plan-state", "done")])) $ \dir s -> do
      (_, a) <- run False dir s
      fp <- fpNow dir s "helper"
      map summary (raBlocks a) `shouldBe` [(4, Just "- [ ] tidy the helper @src/bistro/booking/models/table.py:helper", ["- [x] tidy the helper @src/bistro/booking/models/table.py:helper ✓ " <> fp], True, Just (table, "helper", fp))]
  it "(c2) a diverged step is a changed child, the unit named when no chat rung gives words" $ do
    let plan = "# T\n\n## S\n\n- [ ] tidy the helper @src/bistro/booking/models/table.py:helper\n"
    withReconcile plan [(table, helperEdited)] jevConfig (Just (answersOf [("plan-state", "diverged")])) $ \dir s -> do
      (_, a) <- run False dir s
      map (\(r, _, ls, rep, _) -> (r, ls, rep)) (map summary (raBlocks a)) `shouldBe` [(4, ["  - ✗ changed: helper changed @src/bistro/booking/models/table.py:helper"], False)]
      fp <- fpNow dir s "helper"
      map rbUnit (raBlocks a) `shouldBe` [Just (table, "helper", fp)]
  it "(c3) an accepted changed child settles the step at the unit's fingerprint, until the unit changes" $ do
    let step = "- [ ] tidy the helper @src/bistro/booking/models/table.py:helper"
    withReconcile "# T\n" [(table, helperEdited)] jevConfig (Just (answersOf [("plan-state", "diverged")])) $ \dir s -> do
      fp <- fpNow dir s "helper"
      write dir "plans/t.md" ("# T\n\n## S\n\n" <> step <> "\n  - ✗ changed: helper changed @src/bistro/booking/models/table.py:helper ✓ " <> fp <> "\n")
      (_, a) <- run False dir s
      raBlocks a `shouldBe` []
      asked dir `shouldReturn` []
      write dir table (T.replace "return 1" "return 3" tableText)
      (_, a2) <- run False dir s
      asked dir `shouldReturn` ["plan-state"]
      map (\(r, _, ls, _, _) -> (r, ls)) (map summary (raBlocks a2)) `shouldBe` [(4, ["  - ✗ changed: helper changed @src/bistro/booking/models/table.py:helper"])]
  it "(d) a touched done step answered reopened is a replacing open line" $
    withReconcile "# T\n" [] jevConfig (Just (answersOf [("plan-state", "reopened")])) $ \dir s -> do
      old <- fpNow dir s "helper"
      let step = "- [x] tidy the helper @src/bistro/booking/models/table.py:helper ✓ " <> old
      write dir "plans/t.md" ("# T\n\n## S\n\n" <> step <> "\n")
      write dir table helperEdited
      (_, a) <- run False dir s
      map summary (raBlocks a) `shouldSatisfy` \case
        [(4, Just t, ["- [ ] tidy the helper @src/bistro/booking/models/table.py:helper"], True, Just (_, "helper", _))] -> t == step
        _ -> False
  it "(e) a removed anchored unit is a gone ghost without a question" $ do
    let plan = "# T\n\n## S\n\n- [ ] keep the helper @src/bistro/booking/models/table.py:helper\n"
    withReconcile plan [(table, T.replace helper "" tableText)] jevConfig (Just (answersOf [("plan-state", "done")])) $ \dir s -> do
      (_, a) <- run False dir s
      map summary (raBlocks a) `shouldBe` [(4, Just "- [ ] keep the helper @src/bistro/booking/models/table.py:helper", ["  - ✗ gone: @src/bistro/booking/models/table.py:helper no longer exists"], False, Just (table, "helper", ""))]
      asked dir `shouldReturn` []
      map (map dcKind . rcDecisions) <$> reconciles dir `shouldReturn` [[]]
  it "(e2) a rejected gone ghost does not return; a done step's gone unit has none" $ do
    let plan = "# T\n\n## S\n\n- [ ] keep the helper @src/bistro/booking/models/table.py:helper\n"
    withReconcile plan [(table, T.replace helper "" tableText)] jevConfig (Just (answersOf [])) $ \dir s -> do
      (s1, a1) <- run False dir s
      map rbUnit (raBlocks a1) `shouldBe` [Just (table, "helper", "")]
      now <- getCurrentTime
      appendRecord (ssPorts s) dir (AnOutcome (OutcomeRecord (raId a1) 0 now "rejected"))
      (_, a2) <- run False dir s1
      raBlocks a2 `shouldBe` []
    withReconcile (T.replace "- [ ] keep" "- [x] keep" plan) [(table, T.replace helper "" tableText)] jevConfig (Just (answersOf [])) $ \dir s -> do
      (_, a) <- run False dir s
      raBlocks a `shouldBe` []
  it "(e3) with other work to place, a rejected gone ghost is not replayed from the cache" $ do
    let plan = "# T\n\n## S\n\n- [ ] keep the helper @src/bistro/booking/models/table.py:helper\n"
        gone b = case blLines (rbBlock b) of [g] -> "gone:" `T.isInfixOf` ghText g; _ -> False
    withReconcile plan [(table, T.replace helper "" tableText <> "\n\ndef spare() -> str:\n    return \"spare\"\n")] jevConfig (Just (answersOf [("plan-step", "none")])) $ \dir s -> do
      (s1, a1) <- run False dir s
      i <- maybe (fail "no gone block") pure (lookup True (zip (map gone (raBlocks a1)) [0 ..]))
      length (raBlocks a1) `shouldBe` 2
      now <- getCurrentTime
      appendRecord (ssPorts s) dir (AnOutcome (OutcomeRecord (raId a1) i now "rejected"))
      (_, a2) <- run False dir s1
      map gone (raBlocks a2) `shouldBe` [False]
      length <$> reconciles dir `shouldReturn` 2
  it "(f) none → a new step after the section's last step, with the unit header when no chat rung" $ do
    let planned branch = "# T\n\n## Tables" <> branch <> "\n\n- [ ] count the seats\n  - test: counts @tests/test_t.py:test_count ○\n\n## Later\n\n- [ ] later\n"
        ghost = "- [ ] def spare() -> str @src/bistro/booking/models/table.py:spare"
    withReconcile (planned " · branch main") [(table, withSpare)] jevConfig (Just (answersOf [("plan-step", "none")])) $ \dir s -> do
      (_, a) <- run False dir s
      fp <- fpNow dir s "spare"
      map summary (raBlocks a) `shouldBe` [(5, Just "  - test: counts @tests/test_t.py:test_count ○", [ghost], False, Just (table, "spare", fp))]
    withReconcile (planned "") [(table, withSpare)] jevConfig (Just (answersOf [("plan-step", "none")])) $ \dir s -> do
      (_, a) <- run False dir s
      map (\(r, t, ls, _, _) -> (r, t, ls)) (map summary (raBlocks a)) `shouldBe` [(9, Just "- [ ] later", [ghost])]
  it "(f2) a new step never lands under a sub-step" $
    withReconcile "# T\n\n## Tables\n\n- [ ] count the seats\n  - [ ] a sub-step\n" [(table, withSpare)] jevConfig (Just (answersOf [("plan-step", "none")])) $ \dir s -> do
      (_, a) <- run False dir s
      map (\(r, t, ls, _, _) -> (r, t, ls)) (map summary (raBlocks a)) `shouldBe` [(5, Just "  - [ ] a sub-step", ["- [ ] def spare() -> str @src/bistro/booking/models/table.py:spare"])]
  it "(g) no decider, no ladder → one summary ghost" $
    withReconcile "# T\n\n- [ ] x\n" [(table, T.replace "return 1" "return 2" withSpare)] "" Nothing $ \dir s -> do
      (_, a) <- run False dir s
      map summary (raBlocks a) `shouldBe` [(0, Just "# T", ["- ? 2 changed units not in the plan"], False, Nothing)]
  it "(h) an unsure Jev climbs an auto rung; ask rungs only when forced" $
    withReconcile tidyPlan [(table, helperEdited)] (jevConfig <> climbConfig) (Just (answersOf [("plan-step", "unsure")])) $ \dir s -> do
      _ <- run False dir s
      _ <- run True dir s
      climbs <- map (map fst . rcClimbs) <$> reconciles dir
      case climbs of
        [auto, forced] -> do
          take 2 auto `shouldBe` ["jev", "local"]
          auto `shouldNotContain` ["remote"]
          take 3 forced `shouldBe` ["jev", "local", "remote"]
        other -> expectationFailure ("two reconcile records, got " <> show other)
      map (map dcRung . rcDecisions) <$> reconciles dir `shouldReturn` [["local"], ["remote"]]
  it "(i) the same situation answers from the cache" $
    withReconcile tidyPlan [(table, helperEdited)] jevConfig (Just (answersOf [("plan-step", "s7"), ("plan-state", "in_progress")])) $ \dir s -> do
      (s1, a1) <- run False dir s
      (_, a2) <- run False dir s1
      map summary (raBlocks a2) `shouldBe` map summary (raBlocks a1)
      raId a2 `shouldBe` raId a1
      length <$> reconciles dir `shouldReturn` 1
      length <$> asked dir `shouldReturn` 2
  it "(i2) the plan with the facts applied answers from the cache" $ do
    let plan = "# T\n\n## S\n\n- [ ] tidy the helper @src/bistro/booking/models/table.py:helper\n  - test: it works @tests/t.py:test_it ●\n"
    withReconcile plan [(table, helperEdited)] jevConfig (Just (answersOf [("plan-state", "done")])) $ \dir s -> do
      (s1, a1) <- run False dir s
      raFacts a1 `shouldBe` [Replace 5 "  - test: it works @tests/t.py:test_it ○"]
      write dir "plans/t.md" (T.replace "test_it ●" "test_it ○" plan)
      (_, a2) <- run False dir s1
      (raId a2, raFacts a2, map summary (raBlocks a2)) `shouldBe` (raId a1, [], map summary (raBlocks a1))
      length <$> reconciles dir `shouldReturn` 1
      asked dir `shouldReturn` ["plan-state"]
  it "(i3) a cancelled reconcile answers Left, records nothing, caches nothing" $
    withReconcile tidyPlan [(table, helperEdited)] jevConfig (Just (answersOf [("plan-step", "s7")])) $ \dir s -> do
      (token, trip) <- newCancel
      trip
      (s1, r) <- reconcile token False (dir </> "plans/t.md") s
      either id (const "answered") r `shouldBe` "cancelled"
      reconciles dir `shouldReturn` []
      M.null (ssReconciled s1) `shouldBe` True
  it "(i4) a reconcile cancelled during the words call answers Left, records nothing, caches nothing" $
    withHangingServer $ \port wait -> do
      let config = jevConfig <> T.unlines ["[model.backends.local]", "kind = \"openai-compatible\"", "url = \"http://127.0.0.1:" <> port <> "/v1\"", "model = \"x\"", "escalate = \"auto\"", "[plans]", "ladder = [\"local\"]"]
      withReconcile tidyPlan [(table, helperEdited)] config (Just (answersOf [("plan-step", "none")])) $ \dir s -> do
        (token, trip) <- newCancel
        running <- async (reconcile token False (dir </> "plans/t.md") s)
        -- the words call has reached the server: trip the token under it
        wait >>= (`shouldBe` True)
        trip
        (s1, r) <- either throwIO pure =<< waitCatch running
        either id (const "answered") r `shouldBe` "cancelled"
        reconciles dir `shouldReturn` []
        M.null (ssReconciled s1) `shouldBe` True
        asked dir `shouldReturn` ["plan-step"]
  it "(j) a rejected reading is not proposed again at the same fingerprint" $
    withReconcile tidyPlan [(table, helperEdited)] jevConfig (Just (answersOf [("plan-step", "s7"), ("plan-state", "in_progress")])) $ \dir s -> do
      (s1, a1) <- run False dir s
      length (raBlocks a1) `shouldBe` 1
      now <- getCurrentTime
      appendRecord (ssPorts s) dir (AnOutcome (OutcomeRecord (raId a1) 0 now "rejected"))
      (s2, a2) <- run False dir s1
      raBlocks a2 `shouldBe` []
      write dir table (T.replace "return 1" "return 3" tableText)
      (_, a3) <- run False dir s2
      map (\(_, _, _, _, u) -> fmap (\(_, n, _) -> n) u) (map summary (raBlocks a3)) `shouldBe` [Just "helper"]
  it "(l) a forced reconcile runs a touched step's tests and ticks it; an automatic one marks them pending" $ do
    let plan = "# T\n\n## S\n\n- [ ] tidy the helper @src/bistro/booking/models/table.py:helper\n  - test: it helps @" <> T.pack tableTest <> ":test_helper \9679\n"
    withReconcileTested plan [(table, T.replace "return 1" "return 0 + 1" tableText), (tableTest, T.unlines (take 5 (T.lines tableTestText)))] jevConfig (Just (answersOf [("plan-state", "in_progress")])) $ \dir s -> do
      (s1, auto) <- run False dir s
      raFacts auto `shouldBe` [Replace 5 ("  - test: it helps @" <> T.pack tableTest <> ":test_helper \9675")]
      (_, forced) <- run True dir s1
      fp <- fpNow dir s "helper"
      raFacts forced `shouldBe` [Replace 4 ("- [x] tidy the helper @src/bistro/booking/models/table.py:helper \10003 " <> fp), Replace 5 ("  - test: it helps @" <> T.pack tableTest <> ":test_helper \9679")]
      raBlocks forced `shouldBe` []
  it "(l2) a forced reconcile whose tests do not run leaves them pending and the step alone" $ do
    let child n = "  - test: " <> n <> " @src/bistro/nowhere__test.py:test_" <> n
        plan = "# T\n\n## S\n\n- [ ] tidy the helper @src/bistro/booking/models/table.py:helper\n" <> child "a" <> " \9679\n" <> child "b" <> " \10007\n"
    withReconcile plan [(table, helperEdited)] jevConfig (Just (answersOf [("plan-state", "in_progress")])) $ \dir s -> do
      (_, a) <- run True dir s
      raFacts a `shouldBe` [Replace 5 (child "a" <> " \9675"), Replace 6 (child "b" <> " \9675")]
  it "(k) the record carries decisions and per-block units; the step log has one plan-reconcile line" $
    withReconcile tidyPlan [(table, helperEdited)] jevConfig (Just (answersOf [("plan-step", "s7"), ("plan-state", "in_progress")])) $ \dir s -> do
      (_, a) <- run False dir s
      rs <- reconciles dir
      case rs of
        [r] -> do
          rcId r `shouldBe` raId a
          rcBlocks r `shouldBe` map rbUnit (raBlocks a)
          (rcPlan r, rcForced r, rcChanged r, rcUnaccounted r) `shouldBe` ("plans/t.md", False, 1, 1)
          [(dcKind d, dcChosen d, dcRung d) | d <- rcDecisions r] `shouldBe` [("plan-step", Just "s7", "jev"), ("plan-state", Just "in_progress", "jev")]
          map dcOptions (rcDecisions r) `shouldBe` [["s6", "s7", "none"], ["done", "in_progress", "diverged"]]
        other -> expectationFailure ("one reconcile record, got " <> show (length other))
      logged <- filter (T.isInfixOf "plan-reconcile") . T.lines <$> TIO.readFile (dir </> ".olifant/state/steps.log")
      map (drop 1 . T.splitOn "\t") logged `shouldSatisfy` \case
        [["plan-reconcile", "plans/t.md", "0 facts", "1 ghosts", _, "jev"]] -> True
        _ -> False
      case reconcileJson a of
        Object o -> do
          sort (KM.keys o) `shouldBe` sort ["id", "baseline", "facts", "blocks", "changed", "more", "seconds", "rungs"]
          case KM.lookup "blocks" o of
            Just (Array bs) | [Object b] <- foldr (:) [] bs -> do
              sort (KM.keys b) `shouldBe` sort ["after", "after_text", "lines", "replace", "unit"]
              KM.lookup "after" b `shouldBe` Just (Number 7)
            other -> expectationFailure ("one block, got " <> show other)
        v -> expectationFailure ("not an object: " <> show v)

-- | 'withLoopPorts' with CONFIG appended to the project's config and, when
-- given, recorded ANSWERS the decider answers from; a fresh state.
withReconcile :: Text -> [(FilePath, Text)] -> Text -> Maybe Text -> (FilePath -> ServerState -> IO a) -> IO a
withReconcile = withReconcileOn id

-- | 'withReconcile' with the real pytest (through uv): pending without uv,
-- failing with OLIFANT_REQUIRE_TOOLS=1.
withReconcileTested :: Text -> [(FilePath, Text)] -> Text -> Maybe Text -> (FilePath -> ServerState -> IO ()) -> IO ()
withReconcileTested plan edits config answers act = needsUv (withReconcileOn tested plan edits config answers act)

-- | 'withLoop' with the real pytest (through uv).
withLoopTested :: Text -> [(FilePath, Text)] -> (ServerState -> Project -> FilePath -> IO ()) -> IO ()
withLoopTested plan edits act = needsUv (withLoopPorts plan edits $ \ports dir -> loaded (tested ports) dir >>= \(s, p) -> act s p dir)

tested :: Ports -> Ports
tested ports = ports {pTester = (`processRunner` 120)}

withReconcileOn :: (Ports -> Ports) -> Text -> [(FilePath, Text)] -> Text -> Maybe Text -> (FilePath -> ServerState -> IO a) -> IO a
withReconcileOn withPorts plan edits config answers act =
  withLoopPorts plan edits $ \ports0 dir -> do
    let ports = withPorts ports0
    old <- TIO.readFile (dir </> ".olifant/config.toml")
    TIO.writeFile (dir </> ".olifant/config.toml") (old <> "\n" <> config)
    forM_ answers (TIO.writeFile (answersFile dir))
    s0 <- newServerState ports
    act dir s0 {ssConfig = (ssConfig s0) {scDecideAnswers = answersFile dir <$ answers}}

answersFile :: FilePath -> FilePath
answersFile dir = dir </> ".olifant/answers.json"

-- | Recorded answers by question kind: an option at 0.9, or a bare "unsure".
answersOf :: [(Text, Text)] -> Text
answersOf kvs = TE.decodeUtf8 (BL.toStrict (encode (object ["answers" .= object [K.fromText k .= answer v | (k, v) <- kvs]])))
  where
    answer "unsure" = String "unsure"
    answer v = object ["choice" .= v, "confidence" .= (0.9 :: Double)]

-- | The question kinds the recorded decider was asked, in order.
asked :: FilePath -> IO [Text]
asked dir = do
  let f = answersFile dir <> ".asked"
  there <- doesFileExist f
  if there then T.lines <$> TIO.readFile f else pure []

reconciles :: FilePath -> IO [ReconcileRecord]
reconciles dir = do
  there <- doesFileExist (recordsPath dir)
  rs <- if there then fst . readRecords <$> TIO.readFile (recordsPath dir) else pure []
  pure [r | AReconcile r <- rs]

run :: Bool -> FilePath -> ServerState -> IO (ServerState, ReconcileAnswer)
run forced dir s =
  reconcile neverCancel forced (dir </> "plans/t.md") s >>= \case
    (s', Right a) -> pure (s', a)
    (_, Left e) -> fail (T.unpack e)

-- | A block as (after, after text, lines, replace, unit).
summary :: ReconcileBlock -> (Int, Maybe Text, [Text], Bool, Maybe (FilePath, Text, Text))
summary b = (blAfterRow (rbBlock b), blAfterText (rbBlock b), map ghText (blLines (rbBlock b)), rbReplace b, rbUnit b)

-- | The fingerprint of a unit of the table module now.
fpNow :: FilePath -> ServerState -> Text -> IO Text
fpNow dir s unit =
  projectFor (dir </> "plans/t.md") s >>= \case
    (s', Just (_, p)) -> fromMaybe "" . rsFingerprint <$> resolveAnchor s' p (Anchor table unit 0 0)
    _ -> fail "no project"

-- | The table module's test file, with a passing and a failing test.
tableTest :: FilePath
tableTest = "src/bistro/booking/models/table__test.py"

tableTestText :: Text
tableTestText =
  T.unlines
    [ "from bistro.booking.models.table import Table, helper", "", ""
    , "def test_helper() -> None:", "    assert helper() == 1", "", ""
    , "def test_label() -> None:", "    assert Table(\"a\", 2).label() == \"a\"", "", ""
    , "def test_wrong() -> None:", "    assert Table(\"a\", 2).label() == \"b\"" ]

stepTestsSpec :: Spec
stepTestsSpec = describe "Olifant.Server.Loop (a step's tests)" $ do
  let plan = T.unlines
        [ "# T", "", "## S", ""
        , "- [ ] the table labels itself @" <> T.pack table <> ":Table.label"
        , "  - test: it labels @" <> T.pack tableTest <> ":test_label \9675"
        , "- [ ] the table is wrong @" <> T.pack table <> ":Table.other"
        , "  - test: it is wrong @" <> T.pack tableTest <> ":test_wrong \9679"
        , "- [ ] no anchor here", "- [ ] gone @" <> T.pack table <> ":Nope" ]
      planPath dir = dir </> "plans/t.md"
  it "(a) testNameFor: the step's words in snake case, past its anchor and marks" $ do
    testNameFor "the repository answers closed_on" `shouldBe` "test_the_repository_answers_closed_on"
    testNameFor "- [ ] Count the seats!  @src/a.py:F \10003 abc1234" `shouldBe` "test_count_the_seats"
    testNameFor "close -- the   table" `shouldBe` "test_close_the_table"
    testNameFor "- [ ] mail user@host about it @src/a.py:F" `shouldBe` "test_mail_user_host_about_it"
  it "(b) stepStub: a test named for the step in the unit's test file, and its child line" $
    withLoop plan [] $ \s _ dir -> do
      (s1, r) <- stepStub (planPath dir) 4 s
      st <- either (fail . T.unpack) pure r
      let name = "test_the_table_labels_itself"
          child = "  - test: the table labels itself @" <> T.pack tableTest <> ":" <> name <> " \9675"
      saName (stAnswer st) `shouldBe` name
      stChild st `shouldBe` child
      stFact st `shouldBe` Insert 5 [child]
      let edit = saEdit (stAnswer st)
      fePath edit `shouldSatisfy` (tableTest `isSuffixOf`)
      feCreate edit `shouldBe` True
      TIO.writeFile (fePath edit) (feText edit)
      doesFileExist (dir </> tableTest) `shouldReturn` True
      (feText edit) `shouldSatisfy` T.isInfixOf ("def " <> name <> "()")
      -- again, the name is made unique against the test file
      (_, r2) <- stepStub (planPath dir) 4 s1
      fmap (saName . stAnswer) r2 `shouldSatisfy` either (const False) (\n -> n /= name && name `T.isPrefixOf` n)
  it "(c) stepStub refuses a step without an anchor, and one whose unit is not found" $
    withLoop plan [] $ \s _ dir -> do
      (_, r1) <- stepStub (planPath dir) 8 s
      either id (const "stubbed") r1 `shouldBe` "the step has no anchor"
      (_, r2) <- stepStub (planPath dir) 9 s
      either id (const "stubbed") r2 `shouldBe` "the step's unit was not found"
  it "(d) stepTest on a step runs its tests: green ticks it at its fingerprint; on a test child, red keeps it open" $
    withLoopTested plan [(tableTest, tableTestText)] $ \s _ dir -> do
      fp <- fpNow dir s "Table.label"
      (s1, r) <- stepTest (planPath dir) 4 s
      t <- either (fail . T.unpack) pure r
      ttFacts t `shouldBe` [Replace 5 ("  - test: it labels @" <> T.pack tableTest <> ":test_label \9679"), Replace 4 ("- [x] the table labels itself @" <> T.pack table <> ":Table.label \10003 " <> fp)]
      (ttPassed t, ttFailed t) `shouldBe` (1, 0)
      ttSeconds t `shouldSatisfy` (> 0)
      (_, r2) <- stepTest (planPath dir) 7 s1
      t2 <- either (fail . T.unpack) pure r2
      ttFacts t2 `shouldBe` [Replace 7 ("  - test: it is wrong @" <> T.pack tableTest <> ":test_wrong \10007")]
      (ttPassed t2, ttFailed t2) `shouldBe` (0, 1)
      (_, r3) <- stepTest (planPath dir) 8 s1
      either id (const "ran") r3 `shouldBe` "the step has no tests"

tidyPlan, jevConfig, climbConfig, helperEdited, withSpare :: Text
tidyPlan = "# T\n\n## Tables\n\nThe tables a bistro seats.\n\n- [ ] count the seats\n- [ ] tidy the helper\n"
jevConfig = "[decide]\nmodel = \"jev\"\n\n[model.backends.jev]\nkind = \"jev\"\n"
climbConfig =
  T.unlines
    [ "[model.backends.local]", "kind = \"openai-compatible\"", "url = \"http://127.0.0.1:1/v1\"", "model = \"x\"", "escalate = \"auto\""
    , "[model.backends.remote]", "kind = \"openai-compatible\"", "url = \"http://127.0.0.1:1/v1\"", "model = \"y\"", "escalate = \"ask\""
    , "[plans]", "ladder = [\"local\", \"remote\"]" ]
helperEdited = T.replace "return 1" "return 2" tableText
withSpare = tableText <> "\n\ndef spare() -> str:\n    return \"spare\"\n"

-- | A local server that accepts one connection and never answers: its port,
-- and an action that waits (30 s at most) for that connection.
withHangingServer :: (Text -> IO Bool -> IO a) -> IO a
withHangingServer act = do
  let script = "import socket,sys,time\ns=socket.socket()\ns.bind(('127.0.0.1',0))\ns.listen(1)\nprint(s.getsockname()[1],flush=True)\nc,_=s.accept()\nprint('accepted',flush=True)\ntime.sleep(60)\n"
  bracket (createProcess (proc "python3" ["-I", "-c", script]) {std_out = CreatePipe}) (\(_, _, _, h) -> terminateProcess h) $ \case
    (_, Just out, _, _) -> do
      port <- T.strip . T.pack <$> hGetLine out
      act port (maybe False (== "accepted") <$> timeout 30000000 (hGetLine out))
    _ -> fail "no server"
