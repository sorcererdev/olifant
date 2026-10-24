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
  , Proposal (..)
  , PausedConjure (..)
  , pausedLifetime
  , pruned
  ) where

import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Decide (Answer (..), Question (..), choice, confident)
import Olifant.Server.ConjureScan (Fake (..), actCall, helperOf, initParams, testSource)
import Text.Printf (printf)
import Olifant.Generation.Steer
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
data RoundRecord = RoundRecord {rrRound :: Int, rrRung :: Text, rrAttempt :: Text, rrRun :: Maybe TestRun, rrCheck :: Maybe Text, rrDecided :: Maybe Decided}
  deriving stock (Eq, Show)

-- | The record of a round, from its first hole (Jev not yet asked).
roundRecord :: RoundState -> RoundRecord
roundRecord rs = RoundRecord (rsRound rs) (rsRung rs) attempt (rhRun hole) (snd (verdictOf hole)) Nothing
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
           Just p | T.strip (rrAttempt p) == T.strip (rrAttempt r) -> "(the same as round " <> T.pack (show (rrRound p)) <> ")"
           _ -> attemptLine (rrAttempt r))
        (isGreen r)
        (maybe [] (\run -> [shortName stem t | (t, _) <- trFailed run]) (rrRun r))
    isGreen r = case rrRun r of
      Just run -> null (trFailed run) && isNothing (trBroken run) && isNothing (rrCheck r)
      Nothing -> False

-- | The last two attempts are the same.
isRepeat :: [RoundRecord] -> Bool
isRepeat records = case reverse records of
  a : b : _ -> T.strip (rrAttempt a) == T.strip (rrAttempt b)
  _ -> False

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
-- accept and reject.
data Offer = OfferContinue | OfferHint | OfferClimb Text | OfferWire Text Text | OfferReconjure Text | OfferStop | OfferKeep Int | OfferAccept | OfferReject
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
      [ take 1 (map unsatisfiableSentence unsatisfiable)
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

-- | What to do after Jev's answer: a `climb` at 'conjureClimbGate' climbs (an `ask` rung
-- pauses for the person); an `ask` at 'conjureAskGate' pauses; anything else is again.
nextFrom :: Standing -> Answer -> Next
nextFrom st a = case a of
  Chosen "climb" _ _ | confident conjureClimbGate a -> case stNext st of
    Just (Rung n EscalateAuto) -> NextClimb n
    Just (Rung n EscalateAsk) -> NextPause (CauseClimb n)
    _ -> NextAgain
  Chosen "ask" _ _ | confident conjureAskGate a -> NextPause CauseAsk
  _ -> NextAgain

-- | The person's words, added to the why of each hole's last attempt.
withHint :: Text -> Seed -> Seed
withHint h s = s {seedPrevious = M.map addTo (seedPrevious s)}
  where
    addTo [] = []
    addTo xs = init xs ++ [(\(f, w) -> (f, w <> "\nThe person says: " <> h)) (last xs)]

-- | A steered conjure paused: its token, the file (absolute), the unit, the
-- lines conjured (1-based), the round and rung it paused at, the last
-- round in a few words, what the person may do (`continue`,
-- `climb:NAME`, `stop`, `reconjure:ID`, `accept`, `reject`), why it
-- paused, the explanation (Jev's words, or the server's, after a failed
-- wire's or re-conjure's), the failing tests with their full messages,
-- what is wired (lands with the conjure), and a re-conjured test's
-- proposal.
data PausedAnswer = PausedAnswer
  { paToken :: Text
  , paFile :: FilePath
  , paUnit :: Text
  , paLines :: (Int, Int)
  , paRound :: Int
  , paRung :: Text
  , paSummary :: Text
  , paOffers :: [Text]
  , paWhy :: Text
  , paExplanation :: Text
  , paFailing :: [(Text, Text)]
  , paWired :: [Text]
  , paProposal :: Maybe Proposal
  }
  deriving stock (Eq, Show)

-- | A re-conjured test, proposed: its file (root-relative), its id, the
-- file's text as the editor (or disk) has it and as the re-conjure writes
-- it, and the unified diff between the two.
data Proposal = Proposal {prFile :: FilePath, prTest :: Text, prBefore :: Text, prAfter :: Text, prDiff :: Text}
  deriving stock (Eq, Show)

-- | A conjure paused, as the server keeps it under its token: the file (absolute) and its
-- version when the conjure began, where it was asked (the line, the selection), where the
-- loop resumes (the seed), the rung's index, the rungs used, the rounds and seconds used since
-- the last grant, the last round's view, the person's hint, the file as the last attempt
-- writes it (what stop lands), the names the last check left undefined, when it paused
-- (monotonic seconds); the conjure's working writes beside the unit's (root-relative path →
-- text: a wire's), each written file's editor version (absolute path; none when it was not
-- open) and its text when the conjure first read it (none: a new file), what is wired, and
-- the pause's explanation, the rounds so far and the conjure's first before-run; a re-conjured test's proposal, the pause it was
-- proposed from (what reject answers again), and what the pause answered
-- the editor (its offers say what a resume may do).
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
  , pdProposal :: Maybe Proposal
  , pdPrevious :: Maybe PausedAnswer
  , pdShown :: PausedAnswer
  }
  deriving stock (Eq, Show)

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
