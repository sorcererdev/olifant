-- | The question Jev is asked after a red round of a conjure, and the pure rules around
-- it (spec: steered-conjure-design.md §2, §3).
module Olifant.Server.ConjureNext
  ( RoundView (..)
  , RoundContext (..)
  , testSource
  , roundView
  , Decided (..)
  , RoundRecord (..)
  , roundRecord
  , TestStatus (..)
  , involved
  , shortName
  , attemptLine
  , RoundRow (..)
  , roundRows
  , isRepeat
  , isTrade
  , attemptDiff
  , undefinedNames
  , roundSummary
  , Standing (..)
  , Offer (..)
  , Cause (..)
  , causeWords
  , Next (..)
  , conjureAskGate
  , conjureClimbGate
  , offerText
  , offersAt
  , suspectFirst
  , spent
  , PolicyFacts (..)
  , Policy (..)
  , policy
  , DiagnosisFacts (..)
  , diagnosis
  , trigger
  , conjureNextQuestion
  , nextFrom
  , withHint
  , PausedAnswer (..)
  , Title (..)
  , TestEntry (..)
  , testEntries
  , OfferRow (..)
  , OfferFacts (..)
  , offerRow
  , fixMoves
  , testFix
  , paramPort
  , reconjureWhy
  , roundWords
  , fixWords
  , pausedJson
  , Proposal (..)
  , PausedConjure (..)
  , pausedLifetime
  , pruned
  , withoutKnown
  , knownNote
  , refusedSentence
  , addsReason
  , ConjureStep (..)
  , StepKind (..)
  , proposalStep
  , fixAgain
  , LatestFailure (..)
  , rootFirst
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.List (isSuffixOf, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Decide (Answer (..), Question (..), choice, confident)
import Olifant.Server.ConjureScan (Fake (..), actCall, handFake, helperOf, initParams, testSource)
import Text.Printf (printf)
import Olifant.Generation.Steer
import Olifant.Generation.Tools (ProgressEvent (..))
import Olifant.Language
import Olifant.Model (Escalate (..), Rung (..))
import Text.Regex.TDFA ((=~))

data RoundView = RoundView
  { rvRound :: Int
  , rvRung :: Text
  , rvAttempt :: Text
  , rvFailing :: [(Text, Text)]
  , rvPassing :: Int
  , rvPassingDelta :: Maybe Int
  , rvCheck :: Maybe Text
  , rvRepeat :: Bool
  , rvTests :: [(Text, Text)]
  , rvUnit :: Text
  , rvWired :: [Text]
  , rvAttempts :: [Text]
  , rvTrade :: Bool
  , rvPassingTests :: [Text]
  }
  deriving stock (Eq, Show)

-- | What Jev answered after a round: its choice, its confidence, its distribution (highest first).
data Decided = Decided {dcChoice :: Text, dcConfidence :: Maybe Double, dcProbabilities :: [(Text, Double)]}
  deriving stock (Eq, Show)

-- | A red round as the conjure keeps it: its number, rung, the attempt's body, the verify's
-- run, the check (when no run judged it), and Jev's answer (when asked).
data RoundRecord = RoundRecord {rrRound :: Int, rrRung :: Text, rrAttempt :: Text, rrRun :: Maybe TestRun, rrCheck :: Maybe Text, rrDecided :: Maybe Decided, rrSteps :: Int}
  deriving stock (Eq, Show)

-- | A step the person or the engine took in a conjure that stays in force for every later
-- round (composable): the round it follows (0: before round 1), what it is, its words for
-- the rounds list, and the files it wrote (each a diff). A round records how many steps
-- were in force when it ran ('rrSteps').
data ConjureStep = ConjureStep {csAfter :: Int, csKind :: StepKind, csText :: Text, csFiles :: [(FilePath, Text)]}
  deriving stock (Eq, Show)

-- | The wire; a test's fix (its id, its last failure's line when it was fixed); the
-- person's hint (its words); a method added to a port (the port, the method).
data StepKind = StepWire | StepTest Text (Maybe Text) | StepHint Text | StepPort Text Text
  deriving stock (Eq, Show)

-- | The words of a test fix's step for the test SHORT, from the test file BEFORE and AFTER:
-- the first line it changed (no import), less what that line assigns to.
proposalStep :: Text -> Text -> Text -> Text
proposalStep short before after = "test fixed: " <> short <> maybe "" (\l -> " (" <> l <> ")") changed
  where
    olds = map T.strip (T.lines before)
    changed = listToMaybe [rhs l | l <- map T.strip (T.lines after), not (T.null l), l `notElem` olds, not ("import " `T.isPrefixOf` l), not ("from " `T.isPrefixOf` l)]
    rhs l = case T.breakOn " = " l of
      (lhs, r) | not (T.null r), T.all (\ch -> ch == '_' || ch == '.' || ch == ' ' || ch `elem` ['a' .. 'z'] || ch `elem` ['A' .. 'Z'] || ch `elem` ['0' .. '9']) lhs -> T.drop 3 r
      _ -> l

-- | The latest round's real failure: its checker (by name) refused the body before the
-- tests ran, each problem with its file and line; or its tests ran and these failed.
data LatestFailure = LatestCheck Int Text [(FilePath, Maybe Int, Text)] | LatestTests Int [Text]
  deriving stock (Eq, Show)

-- | A check's problems with what the checker says is unknown (a consequence) after the rest.
rootFirst :: [(FilePath, Maybe Int, Text)] -> [(FilePath, Maybe Int, Text)]
rootFirst ps = [p | p <- ps, not (consequence p)] ++ [p | p <- ps, consequence p]
  where
    consequence (_, _, m) = "Type of " `T.isPrefixOf` m && ("is unknown" `T.isInfixOf` m || "is partially unknown" `T.isInfixOf` m)

-- | Whether the test TID is offered for a fix, given the STEPS so far and the rounds: Just
-- Nothing when no step fixed it; when one did, only if the last run after that step that
-- has the test fails it for another reason than it failed when fixed (Just that failure's line).
fixAgain :: [ConjureStep] -> [RoundRecord] -> Text -> Maybe (Maybe Text)
fixAgain steps records tid = case reverse [(a, was) | ConjureStep a (StepTest t was) _ _ <- steps, t == tid] of
  [] -> Just Nothing
  (a, was) : _ ->
    let runs = [run | r <- records, rrRound r > a, Just run <- [rrRun r], tid `elem` map fst (trFailed run) || tid `elem` trPassed run]
     in case reverse runs of
          run : _ | Just f <- failureLine <$> lookup tid (trFailed run), Just f /= was -> Just (Just f)
          _ -> Nothing

-- | The record of a round, from its first hole (Jev not yet asked).
roundRecord :: RoundState -> RoundRecord
roundRecord rs = RoundRecord (rsRound rs) (rsRung rs) attempt (rhRun hole) (snd (verdictOf hole)) Nothing 0
  where
    hole = firstHole rs
    attempt = case reverse (map fst (rhPrevious hole)) of
      a : _ -> a
      [] -> ""

firstHole :: RoundState -> RoundHole
firstHole rs = case rsHoles rs of
  h : _ -> h
  [] -> RoundHole "" "" [] Nothing

-- | A hole's failing tests (with their one-line messages) and its check: the run's broken
-- text, or the hole's why when no run judged it.
verdictOf :: RoundHole -> ([(Text, Text)], Maybe Text)
verdictOf hole = case rhRun hole of
  Nothing -> ([], Just why)
  Just r -> case trBroken r of
    Just b -> ([], Just (firstLines 3 b))
    Nothing
      | null (trFailed r) -> ([], Just why)
      | otherwise -> ([(i, failureLine f) | (i, f) <- trFailed r], Nothing)
  where
    why = firstLines 3 (rhWhy hole)

-- | A test involved in a conjure: whether it fails now, and the last round whose status
-- differed from now (0: the before-run).
data TestStatus = TestStatus {tsTest :: Text, tsFailing :: Bool, tsFlipped :: Maybe Int}
  deriving stock (Eq, Show)

-- | The runs in order, the before-run as round 0 (a round no run judged is left out).
timeline :: TestRun -> [RoundRecord] -> [(Int, TestRun)]
timeline before records = (0, before) : [(rrRound r, run) | r <- records, Just run <- [rrRun r]]

-- | A test's status in a run: failing, passing, or not in it.
statusIn :: Text -> TestRun -> Maybe Bool
statusIn t run
  | t `elem` map fst (trFailed run) = Just True
  | t `elem` trPassed run = Just False
  | otherwise = Nothing

-- | Every test that failed in the before-run or a round, with its status now (the last run
-- that has it); failing now first, each group in first-seen order.
involved :: TestRun -> [RoundRecord] -> [TestStatus]
involved before records = [s | s <- statuses, tsFailing s] ++ [s | s <- statuses, not (tsFailing s)]
  where
    runs = timeline before records
    ids = nub [t | (_, run) <- runs, (t, _) <- trFailed run]
    statuses = map status ids
    status t =
      let seen = [(n, f) | (n, run) <- runs, Just f <- [statusIn t run]]
          now = maybe False snd (lastOf seen)
          flipped = fst <$> lastOf [x | x@(_, f) <- seen, f /= now]
       in TestStatus t now flipped
    lastOf xs = if null xs then Nothing else Just (last xs)

-- | A test's short name beside its module's stem: less @test_<stem>_@, else less @test_@.
shortName :: Text -> Text -> Text
shortName stem tid = fromMaybe (fromMaybe name (T.stripPrefix "test_" name)) (T.stripPrefix ("test_" <> stem <> "_") name)
  where
    name = snd (T.breakOnEnd "::" tid)

-- | An attempt's first line that is neither blank nor a comment.
attemptLine :: Text -> Text
attemptLine a = case [l | l <- map T.strip (T.lines a), not (T.null l), not ("#" `T.isPrefixOf` l)] of
  l : _ -> l
  [] -> ""

-- | A round in one line: its number, rung, the attempt's first line (or the round it
-- repeats), whether it is green, the failing tests' short names.
data RoundRow = RoundRow {rwRound :: Int, rwRung :: Text, rwLine :: Text, rwGreen :: Bool, rwFailing :: [Text]}
  deriving stock (Eq, Show)

roundRows :: Text -> [RoundRecord] -> [RoundRow]
roundRows stem records = zipWith row (Nothing : map Just records) records
  where
    row prev r =
      RoundRow
        (rrRound r)
        (rrRung r)
        (case prev of
           Just p | sameRound p r -> "(the same as round " <> T.pack (show (rrRound p)) <> ")"
           _ -> attemptLine (rrAttempt r))
        (isGreen r)
        (maybe [] (\run -> [shortName stem t | (t, _) <- trFailed run]) (rrRun r))
    isGreen r = case rrRun r of
      Just run -> null (trFailed run) && isNothing (trBroken run) && isNothing (rrCheck r)
      Nothing -> False

-- | The last two attempts are the same, and no step came between them (the same body over
-- a changed test, or a port with a new method, is a new round).
isRepeat :: [RoundRecord] -> Bool
isRepeat records = case reverse records of
  a : b : _ -> sameRound b a
  _ -> False

-- | Round B is round A again: the same body, the same steps in force.
sameRound :: RoundRecord -> RoundRecord -> Bool
sameRound a b = T.strip (rrAttempt a) == T.strip (rrAttempt b) && rrSteps a == rrSteps b

-- | Over the before-run and the rounds, some test failed then passed while another passed
-- then failed.
isTrade :: TestRun -> [RoundRecord] -> Bool
isTrade before records = or [a /= b | a <- went True False, b <- went False True]
  where
    runs = map snd (timeline before records)
    ids = nub (concat [map fst (trFailed r) ++ trPassed r | r <- runs])
    went from to = [t | t <- ids, let ss = [f | r <- runs, Just f <- [statusIn t r]], from `elem` ss, to `elem` dropWhile (/= from) ss]

firstLines :: Int -> Text -> Text
firstLines n = T.intercalate "\n" . take n . T.lines

firstLine :: Text -> Text
firstLine = firstLines 1

-- | What the round's view needs beyond the round: the test file's text, the unit's header,
-- what is wired, and whether a wire may still run.
data RoundContext = RoundContext {rcTestText :: Text, rcUnit :: Text, rcWired :: [Text], rcWireable :: Bool, rcHistory :: [RoundRecord], rcBefore :: TestRun, rcStem :: Text}
  deriving stock (Eq, Show)

-- | The previous view, the round; built from the first hole. A passing delta needs a test
-- verdict in both rounds (no check on either).
roundView :: RoundContext -> Maybe RoundView -> RoundState -> RoundView
roundView ctx prev rs =
  RoundView
    { rvRound = rsRound rs
    , rvRung = rsRung rs
    , rvAttempt = attempt
    , rvFailing = failing
    , rvPassing = passing
    , rvPassingDelta = delta
    , rvCheck = check
    , rvRepeat = isRepeat history
    , rvTests = [(i, fromMaybe "(source not found)" (testSource (snd (T.breakOnEnd "::" i)) (rcTestText ctx))) | i <- map tsTest tests]
    , rvUnit = rcUnit ctx
    , rvWired = rcWired ctx
    , rvAttempts = [attemptText r | r <- roundRows (rcStem ctx) history]
    , rvTrade = isTrade (rcBefore ctx) history
    , rvPassingTests = [tsTest t | t <- tests, not (tsFailing t)]
    }
  where
    hole = firstHole rs
    (failing, check) = verdictOf hole
    passing = maybe 0 (length . trPassed) (rhRun hole)
    -- the history with this round (once: a record of it may be there already)
    history = case reverse (rcHistory ctx) of
      r : _ | rrRound r == rsRound rs -> rcHistory ctx
      _ -> rcHistory ctx ++ [roundRecord rs]
    tests = involved (rcBefore ctx) history
    attemptText r = "round " <> T.pack (show (rwRound r)) <> ": " <> rwLine r <> (if rwGreen r then " \183 green" else " \183 red: " <> T.intercalate ", " (rwFailing r))
    delta = case prev of
      Just p | check == Nothing, rvCheck p == Nothing -> Just (passing - rvPassing p)
      _ -> Nothing
    attempt = case reverse (map fst (rhPrevious hole)) of
      new : old : _ -> attemptDiff (Just old) new
      [new] -> attemptDiff Nothing new
      [] -> "none"

-- | The lines that changed between two attempts, with the first changed line's number.
attemptDiff :: Maybe Text -> Text -> Text
attemptDiff Nothing new = T.intercalate "\n" (map ("+ " <>) (T.lines new))
attemptDiff (Just old) new
  | old == new = "(the same as the previous attempt)"
  | otherwise = T.intercalate "\n" (("@@ line " <> T.pack (show (pre + 1))) : map ("- " <>) oldMid <> map ("+ " <>) newMid)
  where
    os = T.lines old
    ns = T.lines new
    pre = length (takeWhile id (zipWith (==) os ns))
    os' = drop pre os
    ns' = drop pre ns
    suf = length (takeWhile id (zipWith (==) (reverse os') (reverse ns')))
    oldMid = take (length os' - suf) os'
    newMid = take (length ns' - suf) ns'

-- | Names a checker or runner says are undefined (Python's NameError, pyright), in order.
undefinedNames :: Text -> [Text]
undefinedNames t = nub [n | m <- matches, n <- take 1 (filter (not . T.null) (drop 1 m))]
  where
    matches = t =~ ("name '([^']+)' is not defined|\"([^\"]+)\" is not defined" :: Text) :: [[Text]]

roundSummary :: RoundView -> Text
roundSummary v = case rvFailing v of
  (i, msg) : _ -> "red: " <> snd (T.breakOnEnd "::" i) <> " \183 " <> msg
  [] -> "red: " <> maybe "" firstLine (rvCheck v)

data Standing = Standing {stRoundsLeft :: Int, stSecondsLeft :: Double, stNext :: Maybe Rung, stHint :: Maybe Text, stWireable :: Bool}

-- | What a pause offers: go on, go on with the person's words, climb, run a wire (param,
-- port), re-conjure a test, stop (nothing lands), stop keeping round N's body; a proposal's
-- accept and reject; after a refused re-conjure, its proposal to see and the re-conjure again.
data Offer = OfferContinue | OfferHint | OfferClimb Text | OfferWire Text Text | OfferReconjure Text | OfferStop | OfferKeep Int | OfferAccept | OfferReject | OfferRefused | OfferReconjureAgain Text
  deriving stock (Eq, Show)

-- | Why a conjure paused: a test no body can pass, a port not derived, the same body twice,
-- bodies trading tests, the grant spent, Jev asking, Jev climbing to an `ask` rung.
data Cause = CauseTest Text | CausePort | CauseRepeat | CauseTrade | CauseBudget | CauseAsk | CauseClimb Text
  deriving stock (Eq, Show)

-- | The cause as the step log has it.
causeWords :: Cause -> [Text]
causeWords = \case
  CauseTest t -> ["test", t]
  CausePort -> ["port"]
  CauseRepeat -> ["repeat"]
  CauseTrade -> ["trade"]
  CauseBudget -> ["budget"]
  CauseAsk -> ["ask"]
  CauseClimb _ -> ["climb"]

data Next = NextAgain | NextClimb Text | NextPause Cause
  deriving stock (Eq, Show)

-- | The confidence Jev's `ask` needs to pause (a pause is cheap: the person says continue).
conjureAskGate :: Double
conjureAskGate = 0.5

-- | The confidence Jev's `climb` needs (it spends a stronger model).
conjureClimbGate :: Double
conjureClimbGate = 0.6

offerText :: Offer -> Text
offerText = \case
  OfferContinue -> "continue"
  OfferHint -> "hint"
  OfferClimb n -> "climb:" <> n
  OfferWire p q -> "wire:" <> p <> ": " <> q
  OfferReconjure i -> "reconjure:" <> i
  OfferStop -> "stop"
  OfferKeep n -> "keep:" <> T.pack (show n)
  OfferAccept -> "accept"
  OfferReject -> "reject"
  OfferRefused -> "proposal"
  OfferReconjureAgain i -> "reconjure:" <> i

-- | What the person may do at a pause, in order: continue, hint, climb (a rung above; a
-- `never` rung is still offered to the person), a wire for each candidate (@param: Port@, a
-- port pause only), a re-conjure of each involved test (suspect first), stop, and keep the
-- last round's body.
offersAt :: Standing -> Cause -> [Text] -> [Text] -> Int -> [Offer]
offersAt st cause tests candidates lastRound =
  [OfferContinue, OfferHint]
    ++ [OfferClimb n | Just (Rung n _) <- [stNext st]]
    ++ [OfferWire p q | cause == CausePort, c <- candidates, Just (p, q) <- [paramPort c]]
    ++ map OfferReconjure tests
    ++ [OfferStop]
    ++ [OfferKeep lastRound | lastRound > 0]

-- | @param: Port@ split.
paramPort :: Text -> Maybe (Text, Text)
paramPort c = case T.breakOn ": " c of
  (p, q) | not (T.null p), not (T.null q) -> Just (T.strip p, T.strip (T.drop 2 q))
  _ -> Nothing

-- | The involved tests, the flagged first, then those failing now, then the rest.
suspectFirst :: [(Text, Fake)] -> [TestStatus] -> [Text]
suspectFirst flagged statuses = nub (flaggedIn ++ [tsTest t | t <- statuses, tsFailing t] ++ map tsTest statuses)
  where
    flaggedIn = [t | (t, _) <- flagged, t `elem` map tsTest statuses]

-- | Rounds or time gone: a pause with no call to Jev.
spent :: Standing -> Bool
spent st = stRoundsLeft st <= 0 || stSecondsLeft st <= 0

-- | What the policy is decided from, after the before-run or a red round: the tests the scan
-- flags (with their fake), the tests failing now and those that failed in any round, whether
-- the class lacks the flagged fake's port, whether a wire generator applies, what is wired,
-- the wire's (param, port) when derived, and the repeat, the trade and the spent grant.
data PolicyFacts = PolicyFacts
  { pfFlagged :: [(Text, Fake)]
  , pfFailingNow :: [Text]
  , pfFailedEver :: [Text]
  , pfLacksPort :: Bool
  , pfWireApplies :: Bool
  , pfWired :: [Text]
  , pfDerived :: Maybe (Text, Text)
  , pfRepeat :: Bool
  , pfTrade :: Bool
  , pfSpent :: Bool
  }
  deriving stock (Eq, Show)

-- | Wire (param, port), pause, or ask Jev.
data Policy = PolicyWire Text Text | PolicyPause Cause | PolicyAsk
  deriving stock (Eq, Show)

-- | The policy in code (conjure-pause-context-design.md §3.3), checked in order: a flagged
-- test that failed, the port lacking, a wire applying, none run: wire when derived, else a
-- port pause; a flagged test failing now no wire can help: an unsatisfiable test; a repeat;
-- a trade; the grant spent; else Jev is asked.
policy :: PolicyFacts -> Policy
policy pf
  | wireable, Just (p, q) <- pfDerived pf = PolicyWire p q
  | wireable = PolicyPause CausePort
  | t : _ <- [t | (t, _) <- pfFlagged pf, t `elem` pfFailingNow pf], not (pfWireApplies pf) || not (null (pfWired pf)) || not (pfLacksPort pf) = PolicyPause (CauseTest t)
  | pfRepeat pf = PolicyPause CauseRepeat
  | pfTrade pf = PolicyPause CauseTrade
  | pfSpent pf = PolicyPause CauseBudget
  | otherwise = PolicyAsk
  where
    failed = [t | (t, _) <- pfFlagged pf, t `elem` pfFailingNow pf || t `elem` pfFailedEver pf]
    wireable = not (null failed) && pfLacksPort pf && pfWireApplies pf && null (pfWired pf)

-- | What the diagnosis is written from: the rung and the one above, the module's stem, the
-- unit (its method or function name, its class, its header), the test file, the before-run
-- and the rounds, the flagged tests, what is wired, the wire's candidates (@param: Port@),
-- the rounds and seconds used, and a failed wire's reason.
data DiagnosisFacts = DiagnosisFacts
  { dfRung :: Text
  , dfNext :: Maybe Text
  , dfStem :: Text
  , dfMethod :: Text
  , dfClass :: Text
  , dfHeader :: Text
  , dfTestText :: Text
  , dfBefore :: TestRun
  , dfHistory :: [RoundRecord]
  , dfFlagged :: [(Text, Fake)]
  , dfWired :: [Text]
  , dfCandidates :: [Text]
  , dfRounds :: Int
  , dfSeconds :: Int
  , dfWireFailed :: Maybe Text
  , dfFailure :: Maybe LatestFailure
  }
  deriving stock (Eq, Show)

-- | The pause in sentences: the cause's first, then every other that is true (unsatisfiable,
-- trade, same call, repeat, no progress, spent, wire failed), at most three. Plain text.
diagnosis :: DiagnosisFacts -> Cause -> [Text]
diagnosis df cause = take 3 (nub (causeSentence ++ concat others))
  where
    statuses = involved (dfBefore df) (dfHistory df)
    failingNow = [tsTest t | t <- statuses, tsFailing t]
    unsatisfiable = [t | (t, _) <- dfFlagged df, t `elem` failingNow]
    trade = isTrade (dfBefore df) (dfHistory df)
    rounds = dfHistory df
    lastRound = maybe 0 rrRound (lastOf rounds)
    calls = [c | t <- statuses, Just src <- [testSource (snd (T.breakOnEnd "::" (tsTest t))) (dfTestText df)], Just c <- [actCall (dfMethod df) src]]
    sameCall = [c | (i, c) <- zip [0 :: Int ..] calls, c `elem` drop (i + 1) calls]
    failingSets = [maybe [] (map fst . trFailed) (rrRun r) | r <- rounds]
    noProgress = not trade && case reverse failingSets of
      a : b : c : _ -> a == b && b == c && not (null a)
      _ -> False
    others =
      [ [checkSentence r tool p | Just (LatestCheck r tool (p : _)) <- [dfFailure df]]
      , take 1 (map unsatisfiableSentence unsatisfiable)
      , [tradeSentence | trade]
      , ["Both tests call " <> c <> " and expect different results." | trade, c <- take 1 sameCall]
      , [repeatSentence | isRepeat rounds]
      , ["Three rounds, the same test failing each time." | noProgress]
      , [spentSentence | cause == CauseBudget]
      , ["The wire failed: " <> w <> "." | Just w <- [dfWireFailed df]]
      ]
    causeSentence = case cause of
      CauseTest t -> [unsatisfiableSentence t]
      CausePort -> [portSentence]
      CauseRepeat -> [repeatSentence]
      CauseTrade -> [tradeSentence]
      CauseBudget -> [spentSentence]
      CauseAsk -> [dfRung df <> " is not converging."]
      CauseClimb n -> [dfRung df <> " is stuck; " <> n <> " is offered."]
    tradeSentence = "Every body so far passes one test and fails the other."
    checkSentence r tool (f, l, m) = "Round " <> tshow' r <> " did not type-check: " <> tool <> " refused it before the tests ran (" <> T.pack (takeFileName' f) <> maybe "" ((":" <>) . tshow') l <> ": " <> m <> ")."
    takeFileName' = reverse . takeWhile (/= '/') . reverse
    repeatSentence = dfRung df <> " wrote the same body in rounds " <> tshow' (lastRound - 1) <> " and " <> tshow' lastRound <> "."
    spentSentence = tshow' (dfRounds df) <> (if dfRounds df == 1 then " round" else " rounds") <> " used (" <> tshow' (dfSeconds df) <> " s); nothing passed."
    className = if T.null (dfClass df) then dfMethod df else dfClass df
    unsatisfiableSentence t = case lookup t (dfFlagged df) of
      Nothing -> "No body can pass " <> shortName (dfStem df) t <> "."
      Just f ->
        "No body can pass " <> shortName (dfStem df) t <> ": it builds " <> built f <> " and never hands it to " <> helperCall t <> ", so " <> dfMethod df <> " never sees it." <> takes t
    built f = fkVariable f <> " = " <> fkClass f <> "()"
    helperName t = testSource (snd (T.breakOnEnd "::" t)) (dfTestText df) >>= helperOf (dfMethod df)
    helperCall t = maybe "the helper" (<> "()") (helperName t)
    takes t = case (dfWired df, initParams (dfHeader df)) of
      (w : _, _) | Just (p, q) <- paramPort w -> " " <> className <> " takes " <> p <> ": " <> q <> " since the wire; the test should pass its fake as " <> maybe ("the helper (" <> p <> "=\8230)") (\h -> h <> "(" <> p <> "=\8230)") (helperName t) <> "."
      (_, []) -> " " <> className <> "() takes no ports."
      (_, ps) -> " " <> className <> " takes " <> T.intercalate ", " [n <> ": " <> ty | (n, ty) <- ps] <> "."
    portSentence = case [(t, f) | (t, f) <- dfFlagged df] of
      (t, f) : _ ->
        shortName (dfStem df) t <> " builds " <> built f <> " for a port " <> className <> " lacks" <> (if null (dfCandidates df) then "." else "; it could be " <> T.intercalate " or " (dfCandidates df) <> ".")
      [] -> className <> " lacks a port a test needs."
    lastOf xs = if null xs then Nothing else Just (last xs)

-- | What triggered the pause, for the details: Jev's distribution (@Jev: ask .68 · again
-- .29@), or the rule's word; a failed wire's reason after.
trigger :: Cause -> Maybe Decided -> Maybe Text -> Text
trigger cause decided wireFailed = base <> maybe "" ("; wire failed: " <>) wireFailed
  where
    base = case cause of
      CauseAsk -> jev
      CauseClimb _ -> jev
      CauseRepeat -> "repeat"
      CauseTrade -> "trade"
      CauseBudget -> "budget spent"
      CauseTest t -> "unsatisfiable test: " <> t
      CausePort -> "port not derived"
    jev = case decided of
      Just d | not (null (dcProbabilities d)) -> "Jev: " <> T.intercalate " \183 " [o <> " " <> weight p | (o, p) <- dcProbabilities d]
      Just d -> "Jev: " <> dcChoice d <> maybe "" ((" " <>) . weight) (dcConfidence d)
      Nothing -> "Jev"
    weight :: Double -> Text
    weight p = let t = T.pack (printf "%.2f" p) in fromMaybe t (T.stripPrefix "0" t)

tshow' :: Int -> Text
tshow' = T.pack . show

-- | Jev's question after a red round no rule paused: is the model converging.
conjureNextQuestion :: Standing -> RoundView -> Question
conjureNextQuestion st v = choice "conjure-next" instructions options context
  where
    instructions =
      "A conjure is red after this round; nothing in the state says it cannot pass (such cases never reach you). Is this model converging? `again` when the failing set shrank or the failure moved later in a test; `ask` when it is not converging and the person should look; `climb` when a stronger model is offered and this one is not converging. Attempts that trade one failing test for another (`trade: yes`) are not converging."
    options =
      [("again", "try once more with the same model")]
        ++ [("climb", "try the next, stronger model: " <> n) | Just (Rung n e) <- [stNext st], e /= EscalateNever]
        ++ [("ask", "stop and show the person where this stands")]
    yesNo b = if b then "yes" else "no"
    context =
      [ ("round", tshow' (rvRound v))
      , ("rung", rvRung v)
      , ("rounds_left", tshow' (stRoundsLeft st))
      , ("attempts", if null (rvAttempts v) then "none" else T.intercalate "\n" (rvAttempts v))
      , ("failing", case (rvFailing v, rvCheck v) of
          (fs@(_ : _), _) -> T.intercalate "\n" [i <> ": " <> m | (i, m) <- fs]
          (_, Just c) -> c
          _ -> "none")
      , ("passing", if null (rvPassingTests v) then "none" else T.intercalate "\n" (rvPassingTests v))
      , ("trade", yesNo (rvTrade v))
      , ("repeat", yesNo (rvRepeat v))
      , ("tests", if null (rvTests v) then "none" else T.intercalate "\n\n" [i <> ":\n" <> src | (i, src) <- rvTests v])
      , ("unit", if T.null (rvUnit v) then "none" else rvUnit v)
      , ("hint", fromMaybe "none" (stHint st))
      ]

-- | What to do after Jev's answer to round R: a `climb` at 'conjureClimbGate' climbs (an
-- `ask` rung pauses for the person); an `ask` at 'conjureAskGate' pauses from round 2 on (one
-- round cannot show the model is not converging); anything else is again.
nextFrom :: Standing -> Int -> Answer -> Next
nextFrom st r a = case a of
  Chosen "climb" _ _ | confident conjureClimbGate a -> case stNext st of
    Just (Rung n EscalateAuto) -> NextClimb n
    Just (Rung n EscalateAsk) -> NextPause (CauseClimb n)
    _ -> NextAgain
  Chosen "ask" _ _ | r >= 2, confident conjureAskGate a -> NextPause CauseAsk
  _ -> NextAgain

-- | The person's words, added to the why of each hole's last attempt.
withHint :: Text -> Seed -> Seed
withHint h s = s {seedPrevious = M.map addTo (seedPrevious s)}
  where
    addTo [] = []
    addTo xs = init xs ++ [(\(f, w) -> (f, w <> "\nThe person says: " <> h)) (last xs)]

-- | A steered conjure paused, as the editor is answered (conjure-pause-context-design.md §5):
-- its token, the file (absolute), the unit, the lines conjured (1-based), the round and
-- rung it paused at, the last round in a few words, why (the first diagnosis sentence), the
-- title, the diagnosis, what triggered it, every round, every involved test, the unit's
-- header and the ports it takes (@param: Port@), what keep lands (a unified diff against the
-- buffer), what each wire wrote (each file as a diff), the offers as rows, the seconds it is
-- kept, the module's stem (the tests' short names), and a re-conjured test's proposal; what
-- the person's last choice came to, when it is news (a refused re-conjure), and each round's
-- change: every file keeping that round's body lands, as a diff (the unit's file first); the
-- flagged test the first wire was run for; and (not sent) the rows a refused re-conjure
-- replaced, which 'plainPause' puts back.
data PausedAnswer = PausedAnswer
  { paToken :: Text
  , paFile :: FilePath
  , paUnit :: Text
  , paLines :: (Int, Int)
  , paRound :: Int
  , paRung :: Text
  , paSummary :: Text
  , paWhy :: Text
  , paTitle :: Title
  , paDiagnosis :: [Text]
  , paTrigger :: Text
  , paRounds :: [RoundRecord]
  , paTests :: [TestEntry]
  , paHeader :: Text
  , paPorts :: [Text]
  , paLanding :: Text
  , paWired :: [(Text, [(FilePath, Text)])]
  , paOffers :: [OfferRow]
  , paExpires :: Int
  , paStem :: Text
  , paProposal :: Maybe Proposal
  , paOutcome :: Maybe Text
  , paRoundFiles :: [(Int, [(FilePath, Text)])]
  , paWiredFor :: Maybe Text
  , paHeld :: [OfferRow]
  , paSteps :: [ConjureStep]
  , paFailure :: Maybe LatestFailure
  }
  deriving stock (Eq, Show)

-- | The pause's title: the unit (@Class.method@ for a method), the round of the grant, the
-- rung and its model, the seconds used.
data Title = Title {tiUnit :: Text, tiRound :: Int, tiOf :: Int, tiRung :: Text, tiModel :: Text, tiSeconds :: Int}
  deriving stock (Eq, Show)

-- | An involved test as the pause shows it: its status, its source, the assertion (the line
-- that failed, else its last @assert@), and the scan's note.
data TestEntry = TestEntry {teStatus :: TestStatus, teSource :: Text, teAssertion :: Maybe Text, teNote :: Maybe Text}
  deriving stock (Eq, Show)

-- | Every involved test (failing now first), from the diagnosis' facts.
testEntries :: DiagnosisFacts -> [TestEntry]
testEntries df = [entry t | t <- involved (dfBefore df) (dfHistory df)]
  where
    runs = [r | Just r <- map rrRun (reverse (dfHistory df))] ++ [dfBefore df]
    entry t =
      let tid = tsTest t
          name = snd (T.breakOnEnd "::" tid)
          src = fromMaybe "" (testSource name (dfTestText df))
          lastFailure = listToMaybe [f | r <- runs, Just f <- [lookup tid (trFailed r)]]
          lastAssert = listToMaybe (reverse [T.strip l | l <- T.lines src, "assert" `T.isPrefixOf` T.strip l])
          assertion = if tsFailing t then (lastFailure >>= assertionLine name) <|> lastAssert else lastAssert
       in TestEntry t src assertion (testNote df tid)

-- | The scan's note on a flagged test: the fake it never hands in, and what the class takes.
testNote :: DiagnosisFacts -> Text -> Maybe Text
testNote df tid = do
  f <- lookup tid (dfFlagged df)
  let name = snd (T.breakOnEnd "::" tid)
      helper = testSource name (dfTestText df) >>= helperOf (dfMethod df)
      className = if T.null (dfClass df) then dfMethod df else dfClass df
      takes = case (dfWired df, initParams (dfHeader df)) of
        (w : _, _) -> className <> " takes " <> w <> " since the wire"
        (_, []) -> className <> "() takes no ports"
        (_, ps) -> className <> " takes " <> T.intercalate ", " [n <> ": " <> ty | (n, ty) <- ps]
  Just ("builds " <> fkVariable f <> " = " <> fkClass f <> "() and never hands it to " <> maybe "the helper" (<> "()") helper <> "; " <> takes)

-- | An offer as a row: what it is, its label, what it will do.
data OfferRow = OfferRow {orOffer :: Offer, orLabel :: Text, orDetail :: Text}
  deriving stock (Eq, Show)

-- | What an offer's detail is written from: the rounds a grant gives, the rung, the one
-- above, the round paused at, the class, the suspect test, whether a wire ran, the module's
-- stem (the tests' short names), and each flagged test's fix ('fixMoves').
data OfferFacts = OfferFacts {ofGrant :: Int, ofRung :: Text, ofNext :: Maybe Text, ofRound :: Int, ofClass :: Text, ofSuspect :: Maybe Text, ofWired :: Bool, ofStem :: Text, ofFixes :: [(Text, Text)], ofAgain :: [(Text, Text)]}
  deriving stock (Eq, Show)

-- | An offer in plain words (conjure-pause-ux-design.md §6): the label says what it is, the
-- detail what it does and what follows.
offerRow :: OfferFacts -> Offer -> OfferRow
offerRow o offer = uncurry (OfferRow offer) $ case offer of
  OfferContinue -> ("continue", n (ofGrant o) <> " more rounds with " <> ofRung o <> ", from round " <> next)
  OfferHint -> ("continue with a hint\8230", "your sentence goes to " <> ofRung o <> " with round " <> next)
  OfferClimb r -> ("climb to " <> r, n (ofGrant o) <> " rounds with " <> r <> ", from round " <> next)
  OfferWire p q -> ("wire " <> p <> ": " <> q, "give " <> ofClass o <> " the port the test's fake stands for, then round " <> next)
  OfferReconjure t | Just f <- lookup t (ofAgain o) -> ("fix the test: " <> short t, "fixed already; it fails anew: " <> f <> "; re-conjure it, you see its diff")
  OfferReconjure t -> ("fix the test: " <> short t, maybe "re-conjure it from its name and the unit; you see its diff, then the rounds go on over it" (<> "; you see the diff") (lookup t (ofFixes o)))
  OfferStop -> ("stop", if ofWired o then "nothing lands, not the wire either; your text stands" else "nothing lands; your text stands")
  OfferKeep r -> ("keep round " <> n r, "lands the diff on the right, red (unsaved; u undoes it)")
  OfferAccept -> ("accept", "the test lands with the conjure; round " <> next <> " runs against it")
  OfferReject -> ("reject", "back to the rows; the test stands as it was")
  OfferRefused -> ("see the refused proposal", "the diff it wrote and why it was refused; a accepts it anyway")
  OfferReconjureAgain t -> ("fix the test: " <> short t <> ", again", "re-conjure it once more; you see its diff")
  where
    n = T.pack . show
    next = n (ofRound o + 1)
    short = shortName (ofStem o)

-- | The edit each flagged test gets, once a wire gave the class its port, in words
-- (@hand repository to command(bookings=repository)@): see 'testFix'.
fixMoves :: DiagnosisFacts -> [(Text, Text)]
fixMoves df = [(t, w) | (t, _) <- dfFlagged df, Just (w, _) <- [testFix (dfMethod df) (dfFlagged df) (dfWired df) (dfTestText df) t]]

-- | The fix of the flagged test TID in the test file's TEXT once the first of WIRED gave the
-- unit its param: the test's fake handed to its helper's call (the helper is what the test
-- calls METHOD on) as that param. Its words and the file's new text; none when the test is
-- not flagged, nothing is wired, or the test has no helper call to edit.
testFix :: Text -> [(Text, Fake)] -> [Text] -> Text -> Text -> Maybe (Text, Text)
testFix method flagged wired text tid = do
  f <- lookup tid flagged
  (p, _) <- listToMaybe wired >>= paramPort
  let name = snd (T.breakOnEnd "::" tid)
      var = fkVariable f
  helper <- testSource name text >>= helperOf method
  fixed <- handFake name helper p var text
  pure ("hand " <> var <> " to " <> helper <> "(" <> p <> "=" <> var <> ")", fixed)

-- | Why the test TID is wrong, as the re-conjure is told: its note when the scan flagged it,
-- else the pause's first sentence.
reconjureWhy :: PausedAnswer -> Text -> Text
reconjureWhy p tid = fromMaybe (fromMaybe (paWhy p) (listToMaybe (paDiagnosis p))) (listToMaybe [x | e <- paTests p, tsTest (teStatus e) == tid, Just x <- [teNote e]])

-- | A steered conjure's progress in words that name the round (design §11.4): of TOTAL, the
-- rung's model by MODELOF, the round R being run; Nothing leaves the event as it is.
roundWords :: Int -> (Text -> Text) -> Int -> ProgressEvent -> Maybe Text
roundWords total modelOf r = \case
  Filling _ n rung -> Just ("round " <> tshow' n <> " of " <> tshow' total <> " \183 asking " <> rung <> (case modelOf rung of "" -> ""; m -> " (" <> m <> ")") <> "\8230")
  Verifying _ | r > 0 -> Just ("round " <> tshow' r <> " \183 testing\8230")
  _ -> Nothing

-- | A re-conjure's progress, naming the test it fixes (SHORT).
fixWords :: Text -> ProgressEvent -> Maybe Text
fixWords short = \case
  Filling _ _ rung -> Just ("fix the test: " <> short <> " \183 asking " <> rung <> "\8230")
  Verifying _ -> Just ("fix the test: " <> short <> " \183 testing\8230")
  _ -> Nothing

-- | The paused answer's `paused` object (conjure-pause-context-design.md §5).
pausedJson :: PausedAnswer -> Value
pausedJson p =
  object $
    [ "token" .= paToken p
    , "file" .= paFile p
    , "lines" .= object ["first" .= fst (paLines p), "last" .= snd (paLines p)]
    , "round" .= paRound p
    , "rung" .= paRung p
    , "summary" .= paSummary p
    , "why" .= paWhy p
    , "title" .= object ["unit" .= tiUnit t, "round" .= tiRound t, "of" .= tiOf t, "rung" .= tiRung t, "model" .= tiModel t, "seconds" .= tiSeconds t]
    , "diagnosis" .= paDiagnosis p
    , "trigger" .= paTrigger p
    , "rounds" .= zipWith roundJson (roundRows (paStem p) (paRounds p)) (paRounds p)
    , "tests" .= map testJson (paTests p)
    , "unit" .= object ["header" .= paHeader p, "ports" .= paPorts p]
    , "landing" .= object ["diff" .= paLanding p]
    , "wired" .= [object ["param" .= fst (pq w), "port" .= snd (pq w), "test" .= (if i == 0 then paWiredFor p else Nothing), "files" .= [object ["file" .= f, "diff" .= d] | (f, d) <- fs]] | (i, (w, fs)) <- zip [0 :: Int ..] (paWired p)]
    , "offers" .= map offerJson (paOffers p)
    , "expires" .= paExpires p
    , "outcome" .= paOutcome p
    , "steps" .= map stepJson (paSteps p)
    , "failure" .= fmap failureJson (paFailure p)
    ]
      ++ ["proposal" .= object ["file" .= prFile r, "test" .= prTest r, "before" .= prBefore r, "after" .= prAfter r, "diff" .= prDiff r, "refused" .= prRefused r, "step" .= prStep r] | Just r <- [paProposal p]]
  where
    t = paTitle p
    pq w = fromMaybe (w, "") (paramPort w)
    roundJson row r =
      object
        [ "round" .= rrRound r
        , "rung" .= rrRung r
        , "attempt" .= rrAttempt r
        , "line" .= rwLine row
        , "verdict" .= (if rwGreen row then "green" else "red" :: Text)
        , "failing" .= rwFailing row
        , "failed" .= [failedJson tid f | Just run <- [rrRun r], (tid, f) <- trFailed run]
        , "passed" .= maybe [] trPassed (rrRun r)
        , "check" .= (if isNothing (rrRun r) then rrCheck r else Nothing)
        , "decide" .= fmap decideJson (rrDecided r)
        , "diff" .= fmap snd (listToMaybe =<< lookup (rrRound r) (paRoundFiles p))
        , "files" .= [object ["file" .= f, "diff" .= d] | (f, d) <- fromMaybe [] (lookup (rrRound r) (paRoundFiles p))]
        ]
    failedJson tid f =
      let ge = gotExpected f
       in object
            [ "test" .= tid
            , "message" .= fMessage f
            , "summary" .= failureLine f
            , "at" .= fLine f
            , "assertion" .= assertionLine (snd (T.breakOnEnd "::" tid)) f
            , "got" .= fmap fst ge
            , "expected" .= fmap snd ge
            , "output" .= fOutput f
            ]
    failureJson = \case
      LatestCheck r tool ps -> object ["round" .= r, "kind" .= ("check" :: Text), "tool" .= tool, "problems" .= [object ["file" .= f, "line" .= l, "message" .= m] | (f, l, m) <- ps]]
      LatestTests r ts -> object ["round" .= r, "kind" .= ("tests" :: Text), "tests" .= ts]
    stepJson st =
      object $
        [ "after" .= csAfter st
        , "kind" .= (case csKind st of StepWire -> "wire"; StepTest {} -> "test"; StepHint _ -> "hint"; StepPort {} -> "port" :: Text)
        , "text" .= csText st
        , "files" .= [object ["file" .= f, "diff" .= d] | (f, d) <- csFiles st]
        ]
          ++ ["test" .= tid | StepTest tid _ <- [csKind st]]
    decideJson d = object ["choice" .= dcChoice d, "confidence" .= dcConfidence d, "probabilities" .= object [Key.fromText o .= w | (o, w) <- dcProbabilities d]]
    testJson e =
      object
        [ "test" .= tsTest (teStatus e)
        , "source" .= teSource e
        , "status" .= (if tsFailing (teStatus e) then "failing" else "passing" :: Text)
        , "flipped" .= tsFlipped (teStatus e)
        , "assertion" .= teAssertion e
        , "note" .= teNote e
        ]
    offerJson r =
      let (choice', arg, land, hint) = case orOffer r of
            OfferContinue -> ("continue", Nothing, Nothing, Nothing)
            OfferHint -> ("continue", Nothing, Nothing, Just True)
            OfferClimb _ -> ("climb", Nothing, Nothing, Nothing)
            OfferWire a b -> ("wire", Just (a <> ": " <> b), Nothing, Nothing)
            OfferReconjure i -> ("reconjure", Just i, Nothing, Nothing)
            OfferStop -> ("stop", Nothing, Just False, Nothing)
            OfferKeep k -> ("stop", Just (T.pack (show k)), Just True, Nothing)
            OfferAccept -> ("accept", Nothing, Nothing, Nothing)
            OfferReject -> ("reject", Nothing, Nothing, Nothing)
            OfferRefused -> ("proposal", Nothing, Nothing, Nothing)
            OfferReconjureAgain i -> ("reconjure", Just i, Nothing, Nothing)
       in object ["choice" .= (choice' :: Text), "arg" .= (arg :: Maybe Text), "label" .= orLabel r, "detail" .= orDetail r, "land" .= (land :: Maybe Bool), "hint" .= (hint :: Maybe Bool)]

-- | A re-conjured test, proposed: its file (root-relative), its id, the
-- file's text as the editor (or disk) has it and as the re-conjure writes
-- it, and the unified diff between the two; why its verify refused it (a refused proposal
-- is kept to be seen, and accepted as the person's choice); the step's words once accepted.
data Proposal = Proposal {prFile :: FilePath, prTest :: Text, prBefore :: Text, prAfter :: Text, prDiff :: Text, prRefused :: Maybe Text, prStep :: Text}
  deriving stock (Eq, Show)

-- | A conjure paused, as the server keeps it under its token: the file (absolute) and its
-- version when the conjure began, where it was asked (the line, the selection), where the
-- loop resumes (the seed), the rung's index, the rungs used, the rounds and seconds used since
-- the last grant, the last round's view, the person's hint, the file as the last attempt
-- writes it (what stop lands), the names the last check left undefined, when it paused
-- (monotonic seconds); the conjure's working writes beside the unit's (root-relative path →
-- text: a wire's), each written file's editor version (absolute path; none when it was not
-- open) and its text when the conjure first read it (none: a new file), what is wired, and
-- the pause's explanation, the rounds so far, the conjure's first before-run, the tests the scan flagged and the pause's cause; a re-conjured test's proposal, the pause it was
-- proposed from (what reject answers again), and what the pause answered
-- the editor (its offers say what a resume may do); the unit's file as each round's body
-- writes it (what keeping that round lands); the tests an accepted proposal rewrote.
data PausedConjure = PausedConjure
  { pdFile :: FilePath
  , pdVersion :: Maybe Int
  , pdLine :: Int
  , pdSelected :: Maybe (Int, Int)
  , pdSeed :: Seed
  , pdRung :: Int
  , pdRungs :: [Text]
  , pdRounds :: Int
  , pdSeconds :: Double
  , pdLast :: RoundView
  , pdHint :: Maybe Text
  , pdLanding :: Text
  , pdUndefined :: [Text]
  , pdAt :: Double
  , pdWrites :: Map FilePath Text
  , pdVersions :: Map FilePath (Maybe Int)
  , pdBases :: Map FilePath (Maybe Text)
  , pdWired :: [Text]
  , pdExplanation :: Text
  , pdHistory :: [RoundRecord]
  , pdBefore :: TestRun
  , pdFlagged :: [(Text, Fake)]
  , pdCause :: Maybe Cause
  , pdProposal :: Maybe Proposal
  , pdPrevious :: Maybe PausedAnswer
  , pdShown :: PausedAnswer
  , pdLandings :: [(Int, Text)]
  , pdRewritten :: [Text]
  , pdSteps :: [ConjureStep]
  }
  deriving stock (Eq, Show)

-- | REPORT less the problems KNOWN (file, message) it repeats: each known problem takes one
-- of the same message in the same file (either path may be under a check's scope), so a
-- line moved does not make it new; their lines go with them.
withoutKnown :: [(FilePath, Text)] -> CheckReport -> CheckReport
withoutKnown known r = r {reportBroken = kept, reportLines = keptLines}
  where
    (kept, _) = foldl step ([], known) (reportBroken r)
    step (acc, left) (p, m) = case break (\(q, m') -> m' == m && samePath p q) left of
      (before, _ : after) -> (acc, before ++ after)
      _ -> (acc ++ [(p, m)], left)
    keptLines = take' kept (reportLines r)
    -- each kept problem's line, in order (a problem dropped leaves its line out)
    take' [] _ = []
    take' (k : ks) ls = case break ((== k) . fst) ls of
      (before, l : after) -> l : take' ks (before ++ after)
      _ -> take' ks ls
    samePath a b = a == b || ("/" <> a) `isSuffixOf` b || ("/" <> b) `isSuffixOf` a

-- | What the model is told of the errors a test already has (with their lines, when said).
knownNote :: [(Maybe Int, Text)] -> Text
knownNote es = T.intercalate "\n" ("the test already has these errors; fix them if you touch those lines:" : [maybe "" (\l -> "line " <> T.pack (show l) <> ": ") at <> m | (at, m) <- es])

-- | A refused re-conjure of the test SHORT, as the pause's first sentence.
refusedSentence :: Text -> Text -> Text
refusedSentence short why = "Your re-conjure of " <> short <> " was refused: " <> why <> "."

-- | What a re-conjure added that refused it: an error, and its line when said.
addsReason :: Text -> Maybe Int -> Text
addsReason m at = "it adds `" <> m <> "`" <> maybe "" (\l -> " at line " <> T.pack (show l)) at

-- | Seconds a paused conjure is kept.
pausedLifetime :: Double
pausedLifetime = 600

-- | The paused conjures still good at NOW: none older than 'pausedLifetime', none whose file
-- the editor has at another version than when it paused (DOCS: version and text by path).
-- Only the unit's file is checked here; the other files it wrote are checked at resume.
pruned :: Double -> Map FilePath (Int, Text) -> Map Text PausedConjure -> Map Text PausedConjure
pruned now docs = M.filter good
  where
    good pd = now - pdAt pd <= pausedLifetime && (fst <$> M.lookup (pdFile pd) docs) == pdVersion pd
