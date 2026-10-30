-- | Internal to Olifant.Server: a plan step's tests: stubbed from the step
-- (`olifant/plans/stub`), run over the editor's text (`olifant/plans/test`,
-- a landed `do`, a forced reconcile), and their results as facts: the
-- children's marks, the step ticked when all are green.
module Olifant.Server.LoopTests
  ( testNameFor
  , StepStub (..)
  , stepStub
  , StepTest (..)
  , stepTest
  , stepTestFacts
  , testFacts
  , checkbox
  ) where

import Data.Char (isAlphaNum, isAscii)
import Data.Map.Strict qualified as M
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.App (Ports (..), Project (..))
import Olifant.Language (Language, TestRun (..), languageFor, ownedBy)
import Olifant.Plans (Anchor (..), Kind (..), Line (..), TestState (..), Verdict (..), anchorText, childrenOf, evidenceTag, parsePlan, withTestState, withVerdict)
import Olifant.Server.LoopTypes (Fact (..))
import Olifant.Server.Plans (Resolved (..), confined, resolveAnchor, safely)
import Olifant.Server.State (ServerState (..), projectFor)
import Olifant.Server.Stub (StubAnswer (..), stubWith, testNameIn)
import Olifant.Server.Tests (TestsAnswer (..), TestsScope (..), testLineIn, testsRun)
import Olifant.Catalog (catalogLanguages)
import Olifant.Server.Unit (Named (..))
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (makeRelative, (</>))

-- | A step's words as a test's name in LANG ('testNameIn'): past its
-- checkbox, before its anchor (and so its marks), lowercased, each run of
-- other characters a break between words (pytest's: "the repository
-- answers closed_on" → "test_the_repository_answers_closed_on").
testNameFor :: Language -> Text -> Text
testNameFor lang t = testNameIn lang ws
  where
    ws = filter (not . T.null) (T.split (\c -> not (isAscii c && isAlphaNum c)) (T.toLower (wordsOf t)))

-- | A plan line's words: past its checkbox (or `test:`), before its anchor.
wordsOf :: Text -> Text
wordsOf t = T.strip (foldr (\p acc -> fromMaybe acc (T.stripPrefix p acc)) body ["- [ ]", "- [x]", "- test:"])
  where
    body = T.stripStart (maybe t (\a -> T.take (anStart a) t) (listToMaybe (parsePlan t) >>= lnAnchor))

-- | A step's stubbed test: the stub, the test child line (two deeper than
-- the step, pending) and that line as a fact, after the step's children.
data StepStub = StepStub {stAnswer :: StubAnswer, stChild :: Text, stFact :: Fact}
  deriving stock (Eq, Show)

-- | A test for the step at ROW (0-based) of the plan at PATH, named for its
-- words, in its unit's test file. Left is one sentence: no step, no anchor,
-- its unit not found, or the stub's own reason, naming its key when the language lacks one.
stepStub :: FilePath -> Int -> ServerState -> IO (ServerState, Either Text StepStub)
stepStub path row s0 =
  planAt path s0 >>= \case
    (s, Left why) -> pure (s, Left why)
    (s, Right (p, ls)) -> case drop row ls of
      step : _ | Step _ <- lnKind step -> case lnAnchor step of
        Nothing -> pure (s, Left "the step has no anchor")
        Just a ->
          resolveAnchor s p a >>= \r -> case rsUnit r of
            Nothing -> pure (s, Left "the step's unit was not found")
            Just n ->
              let lang = languageFor (catalogLanguages (prCatalog p)) (makeRelative (normalisePath (prRoot p)) (normalisePath (rsAbs r)))
               in maybe (pure (s, Left ("no language knows " <> T.pack (rsAbs r)))) (\l -> stubWith (Just (testNameFor l said)) (rsAbs r) (namedDef n) Nothing s) lang <&&> \sa ->
                let rel = makeRelative (normalisePath (prRoot p)) (normalisePath (saFile sa))
                    child = T.replicate (lnIndent step + 2) " " <> "- test: " <> wordsOf said <> " @" <> T.pack rel <> ":" <> saName sa <> " ○"
                    after = maybe row lnRow (listToMaybe (reverse (childrenOf ls row)))
                 in StepStub sa child (Insert after [child])
          where
            said = T.take (anStart a) (lnText step)
      _ -> pure (s, Left "stub needs a step line")
  where
    io <&&> f = io >>= \(s', r) -> pure (s', f <$> r)

-- | A run of a step's tests: the facts (children's marks, the tick), how
-- many passed and failed, and the seconds the runs took.
data StepTest = StepTest {ttFacts :: [Fact], ttPassed :: Int, ttFailed :: Int, ttSeconds :: Double}
  deriving stock (Eq, Show)

-- | Run the tests of the line at ROW of the plan at PATH: a step's test
-- children, or one test child (facts against its step); each test alone,
-- over the editor's text. A test not in its file is red. The tick carries
-- the step's anchor's fingerprint now. Left: no tests there, or a run that
-- could not start.
stepTest :: FilePath -> Int -> ServerState -> IO (ServerState, Either Text StepTest)
stepTest path row s0 =
  planAt path s0 >>= \case
    (s, Left why) -> pure (s, Left why)
    (s, Right (p, ls)) -> testsOf s p ls row

-- | 'stepTest' over lines already read, its facts only: a test that could
-- not run is pending; none when there are no tests.
stepTestFacts :: ServerState -> Project -> [Line] -> Int -> IO [Fact]
stepTestFacts s p ls row = maybe [] (\(_, _, facts) -> facts) . snd <$> testsRan s p ls row

testsOf :: ServerState -> Project -> [Line] -> Int -> IO (ServerState, Either Text StepTest)
testsOf s0 p ls row =
  testsRan s0 p ls row <&> \case
    (s, Nothing) -> (s, Left "the step has no tests")
    (s, Just (ran, secs, facts))
      | why : _ <- [w | Left w <- ran], length [() | Left _ <- ran] == length ran -> (s, Left why)
      | otherwise -> (s, Right (StepTest facts (count TestGreen) (count TestRed) secs))
      where
        count st = length [() | Right x <- ran, x == st]

-- | Each test of the line at ROW run (Left: why it could not), their
-- seconds, and the facts: a test that could not run, or that the run did
-- not report, is pending (never its old mark). Nothing: no tests there.
testsRan :: ServerState -> Project -> [Line] -> Int -> IO (ServerState, Maybe ([Either Text TestState], Double, [Fact]))
testsRan s0 p ls row = case drop row ls of
  target : _ | Just step <- stepOf target, tests@(_ : _) <- [a | c <- kidsOf step target, Just a <- [lnAnchor c]] -> do
    (s, ran) <- go s0 [] tests
    fp <- maybe (pure Nothing) (fmap rsFingerprint . resolveAnchor s p) (lnAnchor step)
    let states = [((anPath a, anUnit a), either (const TestPending) id r) | (a, (r, _)) <- zip tests ran]
    pure (s, Just (map fst ran, sum (map snd ran), testFacts ls (lnRow step) states fp))
  _ -> pure (s0, Nothing)
  where
    stepOf l = case lnKind l of
      Step _ -> Just l
      -- a test child's step: the nearest step above it, less indented
      TestChild -> listToMaybe [x | x <- reverse (take (lnRow l) ls), Step _ <- [lnKind x], lnIndent x < lnIndent l]
      _ -> Nothing
    kidsOf step target
      | lnKind target == TestChild = [target]
      | otherwise = [c | c <- childrenOf ls (lnRow step), lnIndent c == lnIndent step + 2, lnKind c == TestChild]
    -- every test, one failing to run keeping the others' results
    go s acc = \case
      [] -> pure (s, reverse acc)
      a : rest -> runOne s a >>= \(s', r) -> go s' (r : acc) rest
    -- one test: its state (pending when the run did not report it), its seconds
    runOne s a = case confined (anPath a) of
      Nothing -> pure (s, (Left (anchorText a <> " is outside the project"), 0))
      Just rel -> do
        text <- safely Nothing (readText (prWorkspace p) rel)
        let lang = languageFor (catalogLanguages (prCatalog p)) rel
        case text >>= \t -> lang >>= \l -> testLineIn l t (anUnit a) of
          Nothing -> pure (s, (Left (anUnit a <> " is not in " <> T.pack rel), 0))
          Just line ->
            testsRun (ScopeAt line) (normalisePath (prRoot p </> rel)) s <&> \case
              (s', Left why) -> (s', (Left why, 0))
              (s', Right ta) -> (s', (Right (stateOf ta), taSeconds ta))
    stateOf ta =
      let run = taRun ta
          own t = maybe False (`ownedBy` t) (taTest ta)
       in case () of
            _
              | Just _ <- trBroken run -> TestRed
              | any (own . fst) (trFailed run) -> TestRed
              | any own (trPassed run) -> TestGreen
              | otherwise -> TestPending

-- | The plan at PATH (its editor text, else the file) in its project.
planAt :: FilePath -> ServerState -> IO (ServerState, Either Text (Project, [Line]))
planAt path s0 =
  projectFor path s0 >>= \case
    (s, Nothing) -> pure (s, Left "this plan is in no Olifant project")
    (s, Just (_, p)) -> do
      text <- case M.lookup path (ssDocs s) of
        Just (_, t) -> pure t
        Nothing -> fromMaybe "" <$> safely Nothing (readText (pFiles (ssPorts s)) path)
      pure (s, Right (p, parsePlan text))

-- | Test results for a step's children (by each test's (path, unit), as
-- its anchor names it): the children rewritten. Every child reported (green
-- or red) by this run: the step stamped at FP, the fingerprint its marks were
-- made at, ticked or not, and ticked when all are green. A red test from
-- this run on a done step un-ticks it and notes "red again" once. A run of
-- some children only (one test child) leaves the step's tick and stamp
-- alone: the others' marks were not refreshed.
testFacts :: [Line] -> Int -> [((FilePath, Text), TestState)] -> Maybe Text -> [Fact]
testFacts ls row states fp = case drop row ls of
  [] -> []
  (step : _) ->
    let direct = [c | c <- childrenOf ls row, lnIndent c == lnIndent step + 2]
        kids = [c | c <- direct, lnKind c == TestChild]
        reported c = lnAnchor c >>= \a -> lookup (anPath a, anUnit a) states
        rewrites = [Replace (lnRow c) (withTestState (lnText c) s) | c <- kids, Just s <- [reported c]]
        reds = [anUnit a | c <- kids, Just a <- [lnAnchor c], reported c == Just TestRed]
        complete = not (null kids) && all ((`elem` [Just TestGreen, Just TestRed]) . reported) kids
        allGreen = complete && all ((== Just TestGreen) . reported) kids
        open = lnKind step == Step False
        done = lnKind step == Step True
        ticked = allGreen && open
        unticked = not (null reds) && done
        boxed
          | ticked = checkbox "- [ ]" "- [x]" (lnText step)
          | unticked = checkbox "- [x]" "- [ ]" (lnText step)
          | otherwise = lnText step
        stamped = case fp of
          Just f | complete -> withVerdict boxed (Verified f)
          _ | ticked || unticked -> withVerdict boxed NoVerdict
          _ -> boxed
        restep = [Replace row stamped | stamped /= lnText step]
        pad = T.replicate (lnIndent step + 2) " "
        noted t = any (\c -> evidenceTag c == Just "red again" && T.strip (lnText c) == "- ✗ " <> t <> " red again") direct
        fresh = [pad <> "- ✗ " <> t <> " red again" | t <- reds, not (noted t)]
        lastRow = maybe row lnRow (listToMaybe (reverse (childrenOf ls row)))
        note = [Insert lastRow fresh | unticked, not (null fresh)]
    in rewrites ++ restep ++ note

-- | Swap the checkbox right after the indent, leaving the rest of the line alone.
checkbox :: Text -> Text -> Text -> Text
checkbox from to txt =
  let (ind, rest) = T.span (== ' ') txt
  in maybe txt (\r -> ind <> to <> r) (T.stripPrefix from rest)
