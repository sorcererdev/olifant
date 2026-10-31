-- | Internal to Olifant.Server: a conjure (`olifant/conjure`) of the unit at a
-- line, or of the selected lines: on a function, method or class, its body
-- to pass its tests; on a test, its body from its name. The tests run
-- before and after; the conjure lands whatever the after-run says. With a
-- decision model (`[decide] model`) and a `[model] ladder`, Jev is asked
-- after each red round to try again, climb the ladder, or pause; a pause is
-- kept under a token for the editor to resume (steered-conjure-design.md).
module Olifant.Server.Conjure
  ( ConjureAnswer (..)
  , PausedAnswer (..)
  , pausedJson
  , Proposal (..)
  , ConjureOutcome (..)
  , ResumeChoice (..)
  , Resume (..)
  , conjureAt
  , resumeConjure
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Control.Monad (forM_, join, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE, withExceptT)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Functor ((<&>))
import Data.List (isSuffixOf, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Text.Read (readMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import Olifant.App (PlanOptions (..), Ports (..), Project (..), placeAt, planOptions, planWith)
import Olifant.Actions (matchPathSlots)
import Olifant.Catalog (catalogConjureWire, catalogLanguages, catalogModel, describeCatalogError, findEntry, loadGenerator)
import Olifant.Decide (Answer (..), Confidence (..), Decider (..), Question)
import Olifant.Fill (Filler (..), ModelFiller (..), Stance (..))
import Olifant.Generation (HoleStatus (..), Plan (..), PlannedHole (..), ProgressEvent (..), Step (..), describeConflict, describeGenerationError)
import Olifant.Generation.Steer (RoundHole (..), RoundState (..), Seed (..), Steer (..), Steering (..), noSeed)
import Olifant.Generation.FillPhase (applyFills, assertionSpan, fillLines, projectFiles)
import Olifant.Generator (AppliesAt (..), GeneratorWith (..), InputSpec (..), Verify (..), replaceGenerator)
import Olifant.Holes (HoleSite (..), Marker (..), splitMarkers)
import Olifant.Language (AddMethod (..), CheckDef (..), CheckReport (..), Checker (..), ConjureDef (..), CtorShape (..), Failure (..), Language (..), PortShape (..), TypesDef (..), TestRun (..), failureLineIn, failureLineWith, failureOf, languageFor, runCheckIn, runTestsIn)
import Olifant.Glob (matchesGlob)
import Text.Regex.TDFA (AllTextSubmatches (..), Regex, makeRegexM, matchM, matchTest)
import Olifant.Model (Backend (..), ModelConfig (..), Rung (..))
import Olifant.Process (cancelled)
import Olifant.Server.Actions (fillerFor, rungFiller)
import Olifant.Server.ConjureNext (undefinedLabelIn, Cause (..), Decided (..), DiagnosisFacts (..), Next (..), Offer (..), OfferFacts (..), OfferRow (..), PausedAnswer (..), Title (..), addsReason, knownNote, offerRow, fixMoves, paramPort, reconjureWhy, roundWords, testFix, fixWords, methodWords, pausedJson, refusedSentence, shortName, testEntries, trigger, withoutKnown, PausedConjure (..), Policy (..), PolicyFacts (..), Proposal (..), RoundContext (..), RoundRecord (..), RoundView, Standing (..), causeWords, conjureNextQuestion, diagnosis, involved, isRepeat, isTrade, nextFrom, offersAt, pausedLifetime, policy, pruned, roundRecord, ConjureStep (..), StepKind (..), fixAgain, proposalStep, LatestFailure (..), rootFirst, roundSummary, roundView, spent, suspectFirst, undefinedNamesIn, undefinedIn, withHint)
import Olifant.Server.ConjureScan (Fake (..), Port (..), advisedIn, blankAt, conjurePortQuestion, ctorParamsIn, derivePortIn, deriveParam, fakeCalls, fakeIn, methodSignature, methodText, missingMemberIn, portCandidates, portEnd, portFrom, portsIn, servicePaths, siblingsOf, takesPortIn)
import Olifant.Server.Site (Site (..), appendStepLine, importedContext, runOver, siteAt, siteTestFile, tshow)
import Olifant.Server.Edits (FileEdit (..), smallestChange, unifiedDiff)
import Olifant.Server.State (ServerConfig (..), ServerState (..), deciderUnder)
import Olifant.Server.Tests (TestsAnswer (..), TestsScope (..))
import Olifant.Language.TestUnits (caseOfId, testCaseAtIn, testCaseLineIn, testSourceIn, testSourcesIn)
import Olifant.Server.Stub (namePrefixesIn, testWordsIn)
import Olifant.Server.Unit (isStubIn, Unit (..), ownerHeader, testOrUnitAt, testUnitAt, unitAt)
import Olifant.Template (Template, describeTemplateError, parseTemplate)
import Olifant.Workspace (Workspace (..), normalisePath, overlayMany)
import System.FilePath (joinPath, takeBaseName, takeFileName, (</>))
import Text.Printf (printf)

-- | A conjure's answer: the conjured file (absolute), the unit, the edits
-- (the unit's file first, when the conjure changed it, then every other
-- file a wire wrote), the summary, whether the after-run is green, the
-- after-run, the rounds run in all, and the models that ran them (a
-- steered conjure's rungs, in the order climbed), and whether the person
-- stopped it (a bare stop lands nothing; a keep lands the last body, red).
data ConjureAnswer = ConjureAnswer
  { daFile :: FilePath
  , daUnit :: Text
  , daEdits :: [FileEdit]
  , daSummary :: Text
  , daGreen :: Bool
  , daTests :: TestsAnswer
  , daRounds :: Int
  , daRungs :: [Text]
  , daStopped :: Bool
  }
  deriving stock (Eq, Show)

data ConjureOutcome = Landed ConjureAnswer | Paused PausedAnswer
  deriving stock (Eq, Show)

data ResumeChoice = ResumeContinue | ResumeClimb | ResumeStop | ResumeReconjure | ResumeAccept | ResumeReject | ResumeWire | ResumePort
  deriving stock (Eq, Show)

-- | The person's answer to a pause: its token, the choice, words to add to
-- the next prompt (never logged), the offer's argument (a wire's
-- @param: Port@, a re-conjure's test id), and, for a stop, whether to keep
-- the last body.
data Resume = Resume {reToken :: Text, reChoice :: ResumeChoice, reHint :: Maybe Text, reArg :: Maybe Text, reLand :: Bool}
  deriving stock (Eq, Show)

-- | A steered conjure's means: the decider, the ladder (each rung with its
-- filler), the rounds and the seconds one grant allows.
data Steered = Steered {sdDecider :: Decider IO, sdLadder :: [(Rung, ModelFiller IO)], sdRounds :: Int, sdBudget :: Double}

-- | What the hook keeps between rounds: the rung's index, the rungs used,
-- the rounds and seconds used since the last grant (SECONDS_BEFORE: before
-- this request), the last round's view and state, the hint, and the offers
-- of a pause; the wire Jev asked for (param, port), what is wired, the
-- working writes (root-relative), each written file's editor version and
-- text when first read (absolute paths), a failed wire's text for the next
-- pause; the working test file and the unit's header (Jev's context); every
-- round so far (kept across a resume), the conjure's first before-run, and
-- the module's stem (the tests' short names); the flagged test the first wire ran for; the tests rewritten by an accepted proposal; every step in force (the wire, a test's fix, a hint, a port's method), in order.
data Counters = Counters
  { cRung :: Int
  , cRungs :: [Text]
  , cRounds :: Int
  , cSecondsBefore :: Double
  , cSeconds :: Double
  , cLast :: Maybe RoundView
  , cState :: Maybe RoundState
  , cHint :: Maybe Text
  , cOffers :: [Offer]
  , cWire :: Maybe (Text, Text)
  , cWired :: [Text]
  , cWrites :: Map FilePath Text
  , cVersions :: Map FilePath (Maybe Int)
  , cBases :: Map FilePath (Maybe Text)
  , cExplanation :: Maybe Text
  , cTestText :: Text
  , cTestRel :: FilePath
  , cHeader :: Text
  , cHistory :: [RoundRecord]
  , cBefore :: TestRun
  , cPrefixes :: [Text]
  , cUnit :: Text
  , cCause :: Maybe Cause
  , cFlagged :: [(Text, Fake)]
  , cService :: Maybe ServiceFacts
  , cCandidates :: [Text]
  , cDerived :: Maybe (Text, (Maybe (Text, Text), [Text]))
  , cDiagnosis :: [Text]
  , cWiredFor :: Maybe Text
  , cRewritten :: [Text]
  , cSteps :: [ConjureStep]
  , cWireWrites :: Map FilePath Text
  }

-- | What a fill gave: why its last attempt failed its verify (none: it
-- passed), the file as it lands, with the names that attempt left
-- undefined (when it landed unfilled); or a pause: the file as the last
-- attempt writes it, and why.
data Written = Written (Maybe Text) Text [Text] | Halted Text Text

-- | The conjure's working files made ready to fill: the project over them,
-- the unit's file as they have it, the lines conjured there, the test
-- file and its before-run, and the template, verify and context built over them.
data Working = Working
  { wkProject :: Project
  , wkText :: Text
  , wkSpan :: (Int, Int)
  , wkTestText :: Text
  , wkBefore :: TestRun
  , wkTemplate :: Template
  , wkVerify :: [(Text, Verify [Template])]
  , wkContext :: [(FilePath, Text)]
  }

-- | The conjure of the unit at LINE (1-based) of PATH, or of the lines
-- SELECTED (1-based, inclusive): landed, or (steered) paused. Left is one
-- sentence on why there is none.
conjureAt :: (ProgressEvent -> IO ()) -> FilePath -> Int -> Maybe (Int, Int) -> ServerState -> IO (ServerState, Either Text ConjureOutcome)
conjureAt progress path line selected = conjureWith progress path line selected Nothing

-- | A paused conjure of PATH picked up again: stopped (its last attempt
-- lands), continued with a fresh grant of rounds, or climbed to the next
-- rung; a wrong test re-conjured into a proposal (the token kept), the
-- proposal accepted (the function conjure goes on over it) or rejected
-- (the pause before it again, the token kept); a hint goes into the next
-- prompt. A token resumes once, but for those it keeps (their lifetime
-- starts again); a file the conjure wrote that changed since ends it.
resumeConjure :: (ProgressEvent -> IO ()) -> FilePath -> Resume -> ServerState -> IO (ServerState, Either Text ConjureOutcome)
resumeConjure progress path re s = do
  now <- getMonotonicTime
  held <- M.lookup (reToken re) . snd <$> readIORef (ssPaused s)
  case held of
    Just pd
      | now - pdAt pd > pausedLifetime -> pure (s, Left gone)
      | pdFile pd /= normalisePath path -> pure (s, Left "this paused conjure is of another file")
      | Just why <- refused pd -> do
          touch s (reToken re) now
          pure (s, Left why)
      | (fst <$> M.lookup (pdFile pd) (ssDocs s)) /= pdVersion pd -> do
          _ <- takeToken s (reToken re)
          pure (s, Left "the file changed since the conjure paused; conjure again")
      | otherwise -> do
          let pd' = answered pd
          movedSince s (pdVersions pd') (pdBases pd') [(relOf pd' p, p) | p <- M.keys (pdBases pd')] >>= \case
            Just r -> do
              _ <- takeToken s (reToken re)
              pure (s, Left (changedWhile r))
            Nothing -> conjureWith progress path (pdLine pd') (pdSelected pd') (Just (re, pd')) s
    Nothing -> pure (s, Left gone)
  where
    relOf pd p = fromMaybe p (listToMaybe [r | r <- M.keys (pdWrites pd), ("/" <> r) `isSuffixOf` p])
    refused pd = case reChoice re of
      ResumeReconjure
        | Just a <- reArg re, isNothing (reconjureOf re pd) -> Just ("this pause offers no re-conjure of " <> a)
        | isNothing (reconjureOf re pd) -> Just "this pause offers no re-conjure"
      ResumeWire | isNothing (wireOf re pd) -> Just ("this pause offers no wire for " <> fromMaybe "" (reArg re))
      ResumePort | isNothing (portOf re pd) -> Just ("this pause offers no method for " <> fromMaybe "a port" (reArg re))
      ResumeStop | reLand re, null [() | OfferRow (OfferKeep _) _ _ <- paOffers (pdShown pd)] -> Just "there is no body to keep"
      ResumeStop | reLand re, Just a <- reArg re, T.strip a == "0", isNothing (keptLanding re pd) -> Just "there is no wire to keep"
      ResumeStop | reLand re, Just a <- reArg re, isNothing (keptLanding re pd) -> Just ("there is no round " <> a <> " body to keep")
      c | c `elem` [ResumeAccept, ResumeReject], isNothing (pdProposal pd) -> Just "there is no proposal to accept"
      _ -> Nothing
    -- every file a proposal writes joins the working writes
    written pd p = foldr (\(f, _, a) -> M.insert f a) (M.insert (prFile p) (prAfter p) (pdWrites pd)) (prOthers p)
    -- the test's last failure (its line), in the rounds or the before-run
    failedAs pd tid = listToMaybe [failureLineWith (paFailureShape (pdShown pd)) f | run <- reverse [r | Just r <- map rrRun (pdHistory pd)] ++ [pdBefore pd], Just f <- [lookup tid (trFailed run)]]
    -- accept: the proposed test file joins the working writes (a step); reject keeps
    -- the proposal (to answer the pause before it); any other answer drops it
    answered pd = case (reChoice re, pdProposal pd) of
      (ResumeAccept, Just p)
        | prKind p == "port" ->
            let (q, m) = T.breakOn "." (prTest p)
             in pd {pdWrites = written pd p, pdProposal = Nothing, pdPrevious = Nothing, pdSteps = pdSteps pd ++ [ConjureStep (paRound (pdShown pd)) (StepPort q (T.drop 1 m)) (prStep p) [(f, unifiedDiff f b a) | (f, b, a) <- (prFile p, prBefore p, prAfter p) : prOthers p]]}
        | otherwise -> pd {pdWrites = written pd p, pdProposal = Nothing, pdPrevious = Nothing, pdRewritten = pdRewritten pd ++ [prTest p | prTest p `notElem` pdRewritten pd], pdSteps = pdSteps pd ++ [ConjureStep (paRound (pdShown pd)) (StepTest (prTest p) (failedAs pd (prTest p))) (prStep p) [(prFile p, prDiff p)]]}
      (ResumeReject, _) -> pd
      _ -> withoutProposal pd

-- | The test a pause offers to re-conjure, by its id.
reconjureOf :: Resume -> PausedConjure -> Maybe Text
reconjureOf re pd = listToMaybe [t | OfferRow o _ _ <- paOffers (pdShown pd), Just t <- [reconjured o], maybe True (== t) (reArg re)]
  where
    reconjured = \case
      OfferReconjure t -> Just t
      OfferReconjureAgain t -> Just t
      _ -> Nothing

-- | The round a keep names (its @arg@, else the pause's round) and the unit's file as that
-- round's body writes it, when the pause has it; round 0 is the wire alone: the unit's file
-- as the wire wrote it (the person's body standing), when a wire ran.
keptLanding :: Resume -> PausedConjure -> Maybe (Int, Text)
keptLanding re pd = case reArg re of
  Nothing -> Just (paRound (pdShown pd), pdLanding pd)
  Just a -> do
    n <- readMaybe (T.unpack (T.strip a))
    if n == 0
      then if null (pdWired pd) then Nothing else (0,) <$> (unitIn (pdWireWrites pd) <|> unitIn (pdWrites pd))
      else (n,) <$> lookup n (pdLandings pd)
  where
    unitIn m = listToMaybe [t | (r, t) <- M.toList m, ("/" <> r) `isSuffixOf` pdFile pd]

-- | Whether a resume keeps nothing but the wire (keep 0).
keepsWireAlone :: Resume -> Bool
keepsWireAlone re = reChoice re == ResumeStop && reLand re && fmap T.strip (reArg re) == Just "0"

-- | A pause without what a refused re-conjure added to it: its sentence, its proposal and
-- the rows to see it and re-conjure again (the plain re-conjure row back).
plainPause :: PausedAnswer -> PausedAnswer
plainPause p = p {paOutcome = Nothing, paProposal = Nothing, paOffers = mapMaybe plain (paOffers p), paHeld = []}
  where
    plain = \case
      OfferRow OfferRefused _ _ -> Nothing
      OfferRow (OfferReconjureAgain t) _ _ -> Just (fromMaybe (OfferRow (OfferReconjure t) "" "") (listToMaybe [r | r@(OfferRow (OfferReconjure t') _ _) <- paHeld p, t' == t]))
      r -> Just r

-- | The check of LANG's files over WS with FILES on top: each problem (file, message), with
-- its line when the check says it; none when the language has no check or it cannot run.
checkedOver :: ServerState -> Workspace IO -> Language -> Map FilePath Text -> IO [((FilePath, Text), Maybe Int)]
checkedOver s ws lang files
  | isNothing (langCheck lang) = pure []
  | otherwise = do
      (scope, everything) <- projectFiles ws files lang (M.keys files)
      runCheckIn (pChecker (ssPorts s) (ssCancel s)) lang scope everything <&> \case
        Left _ -> []
        Right r -> [(b, lookup b (reportLines r)) | b <- reportBroken r]

-- | PORTS whose check leaves out the problems KNOWN (the files had them before).
tolerating :: [(FilePath, Text)] -> Ports -> Ports
tolerating known ports = ports {pChecker = \c -> let Checker run = pChecker ports c in Checker (\lang files -> fmap (withoutKnown known) <$> run lang files)}

-- | PORTS whose check leaves out the problems in the IORef, read at each check: the test file's
-- errors as the conjure began (and as its wire left them).
toleratingIn :: IORef [(FilePath, Text)] -> Ports -> Ports
toleratingIn ref ports = ports {pChecker = \c -> let Checker run = pChecker ports c in Checker (\lang files -> readIORef ref >>= \known -> fmap (withoutKnown known) <$> run lang files)}

-- | The wire a resume names (@param: Port@), when the pause offers it.
wireOf :: Resume -> PausedConjure -> Maybe (Text, Text)
wireOf re pd = do
  (p, q) <- paramPortOf =<< reArg re
  listToMaybe [(p, q) | OfferRow (OfferWire p' q') _ _ <- paOffers (pdShown pd), (p', q') == (p, q)]

-- | The method a resume names (@Port.method@, or none: the first), when the pause offers it:
-- the port, the method, its parameters after self, what it returns.
portOf :: Resume -> PausedConjure -> Maybe (Text, Text, Text, Text)
portOf re pd = listToMaybe [(q, m, ps, r) | OfferRow (OfferPort q m ps r) _ _ <- paOffers (pdShown pd), maybe True (== q <> "." <> m) (T.strip <$> reArg re)]

-- | @param: Port@ split.
paramPortOf :: Text -> Maybe (Text, Text)
paramPortOf a = case T.breakOn ":" a of
  (p, q) | not (T.null (T.strip p)), not (T.null (T.strip (T.drop 1 q))) -> Just (T.strip p, T.strip (T.drop 1 q))
  _ -> Nothing

-- | PD less its proposal, and less each proposed file's recorded version and
-- text when the conjure has not written that file.
withoutProposal :: PausedConjure -> PausedConjure
withoutProposal pd = case pdProposal pd of
  Just p ->
    let unwritten = [f | f <- prFile p : [f' | (f', _, _) <- prOthers p], M.notMember f (pdWrites pd)]
        others :: Map FilePath v -> Map FilePath v
        others = M.filterWithKey (\k _ -> not (any (\f -> ("/" <> f) `isSuffixOf` k) unwritten))
     in pd' {pdBases = others (pdBases pd), pdVersions = others (pdVersions pd)}
  _ -> pd'
  where
    pd' = pd {pdProposal = Nothing, pdPrevious = Nothing}

-- | The paused conjure under TOKEN with its lifetime started again at NOW.
touch :: ServerState -> Text -> Double -> IO ()
touch s token now = atomicModifyIORef' (ssPaused s) (\(n, m) -> ((n, M.adjust (\pd -> pd {pdAt = now}) token m), ()))

-- | PD kept under TOKEN (again).
keep :: ServerState -> Text -> PausedConjure -> IO ()
keep s token pd = atomicModifyIORef' (ssPaused s) (\(n, m) -> ((n, M.insert token pd m), ()))

gone :: Text
gone = "this paused conjure is gone (it expired, was resumed, or its file changed): conjure again"

changedWhile :: FilePath -> Text
changedWhile r = "the file changed while conjuring; your text stands (" <> T.pack r <> ")"

-- | The first of FILES (relative, absolute) whose text moved since the
-- conjure read it: an open document at another version than VERSIONS
-- says (the editor's live version when it can say), or, read when not
-- open, whose text (the editor's now, or the disk's) is not what BASES
-- holds.
movedSince :: ServerState -> Map FilePath (Maybe Int) -> Map FilePath (Maybe Text) -> [(FilePath, FilePath)] -> IO (Maybe FilePath)
movedSince s versions bases files = firstJust <$> traverse moved files
  where
    firstJust = listToMaybe . concatMap (maybe [] pure)
    moved (r, p) = do
      now <- maybe (pure (M.lookup p (ssDocs s))) ($ p) (ssLiveDoc s)
      let base = M.findWithDefault Nothing p bases
      same <- case (M.findWithDefault Nothing p versions, now) of
        (Just v, Just (v', _)) -> pure (v == v')
        (_, Just (_, t)) -> pure (Just t == base)
        (_, Nothing) -> (== base) <$> readText (pFiles (ssPorts s)) p
      pure (if same then Nothing else Just r)

-- | The paused conjure under TOKEN removed: whether it was there (one of
-- two resumes at once finds it gone).
takeToken :: ServerState -> Text -> IO Bool
takeToken s token = atomicModifyIORef' (ssPaused s) (\(n, m) -> ((n, M.delete token m), M.member token m))

-- | A conjure (RESUMING a pause, or not). Steered when the project names a
-- decider and a ladder; else as before steering, apart from the rounds and
-- rungs it answers and the undefined names its summary gives. A port
-- answer pauses the fill loop; the wire runs here, over the working files,
-- and the loop goes on from the round it paused at.
conjureWith :: (ProgressEvent -> IO ()) -> FilePath -> Int -> Maybe (Int, Int) -> Maybe (Resume, PausedConjure) -> ServerState -> IO (ServerState, Either Text ConjureOutcome)
conjureWith progress path line selected resuming s0 = do
  begun <- getMonotonicTime
  atomicModifyIORef' (ssPaused s0) (\(n, m) -> ((n, pruned begun (ssDocs s0) m), ()))
  siteAt path line s0 >>= \case
    (s, Left why) -> pure (s, Left why)
    (s, Right site) -> fmap (s,) . runExceptT $ do
      let lang = siteLang site
          project = siteProject site
          ws = prWorkspace project
          -- on a test: in a test file, or in a module that holds its own tests (Rust's)
          onTest = siteOnTest site || (isNothing selected && siteTestRel site == siteModuleRel site && isJust (testCaseAtIn lang (Just (siteModuleRel site)) (siteModuleText site) line))
          rel = if onTest then siteTestRel site else siteModuleRel site
          testRel = siteTestRel site
          file = normalisePath (siteRoot site </> rel)
          absOf r = normalisePath (siteRoot site </> r)
          started = fst <$> M.lookup file (ssDocs s)
          stepLine = appendStepLine (ssPorts s) (siteRoot site)
      text <- if onTest then maybe (throwE ("cannot read " <> T.pack rel)) pure (siteTestText site) else pure (siteModuleText site)
      -- on a test (in a test file, or a module that holds its own tests: Rust's), its
      -- `[test] unit` (hspec's `it` is no `[unit]`; Rust's brace is not in its body)
      found0 <- lift (unitAt (pSyntax (ssPorts s)) lang (Just rel) text line)
      let testy = (onTest || siteTestRel site == siteModuleRel site) && isNothing selected && isJust (testUnitAt lang (Just rel) text line)
          found = if testy then maybe found0 Right (testUnitAt lang (Just rel) text line) else found0
          unitIn working i = if testy then pure (maybe (Left ("no test at line " <> tshow i)) Right (testUnitAt lang (Just rel) working i)) else unitAt (pSyntax (ssPorts s)) lang (Just rel) working i
      (unit, span') <- case (selected, found) of
        (Just sp, _) -> pure (either (const "selection") unitName found, sp)
        (Nothing, Left why) -> throwE (why <> ": put the cursor in one to conjure it")
        (Nothing, Right u) -> maybe (throwE (unitName u <> " is on one line: select the lines to conjure")) (pure . (unitName u,)) (unitBody u)
      when (fst span' < 1 || snd span' < fst span' || snd span' > length (T.splitOn "\n" text)) (throwE "the selection is not in the file")
      when (onTest && isNothing selected && isNothing (testCaseAtIn lang (Just rel) text line)) (throwE (unit <> " is no test: conjure a test here, or a function from its module"))
      -- a module needs its test file (on a test, the file is the test file)
      testText0 <- case siteTestText site of
        Just t -> pure t
        Nothing -> throwE ("no test file for this module yet: " <> T.pack testRel <> "; stub one first (:Durandal stub)")
      steered <- lift (steeredFor s project) >>= except
      -- a resume takes its token (a climb with no stronger rung keeps it; a
      -- re-conjure or a reject puts it back)
      forM_ resuming $ \(re, pd) -> do
        let ladder = maybe [] sdLadder steered
        when (reChoice re == ResumeClimb && length ladder <= pdRung pd + 1) (lift (touch s (reToken re) begun) >> throwE "there is no stronger model in [model] ladder")
        taken <- lift (takeToken s (reToken re))
        unless taken (throwE gone)
        when (reChoice re /= ResumeStop && isNothing steered) (throwE "this project no longer names a [decide] model and a [model] ladder: conjure again")
        lift . stepLine $
          if reChoice re == ResumeStop && reLand re
            then ["conjure-resume", "keep", maybe (fromMaybe "-" (reArg re)) (tshow . fst) (keptLanding re pd)]
            else ["conjure-resume", choiceWord (reChoice re)]
      let stopping = maybe False ((== ResumeStop) . reChoice . fst) resuming
          rung0 = case resuming of
            Just (re, pd) | reChoice re == ResumeClimb -> pdRung pd + 1
            Just (_, pd) -> pdRung pd
            Nothing -> 0
      filler <- case steered of
        Just sd | (_, f) : _ <- drop rung0 (sdLadder sd) -> pure f
        _ | stopping -> pure (ModelFiller "" 0 (Filler (const (pure (Left "stopped")))))
        _ -> lift (fillerFor True s project) >>= either throwE (maybe (throwE noModel) pure)
      -- the wire a port answer runs: a function's or a method's (not a test's, nor a selection's)
      wire <- lift (if onTest || isJust selected then pure Nothing else wireFor s project (siteModuleRel site))
      let -- the unit again by name in its file as the working writes have it (a wire moves its lines down)
          relocate working = case found of
            Right u | isNothing selected -> do
              let defLine = fromMaybe "" (listToMaybe (drop (unitDef u - 1) (T.splitOn "\n" text)))
                  at = listToMaybe [i | (i, l) <- zip [1 ..] (T.splitOn "\n" working), i >= unitDef u, l == defLine]
              i <- maybe (throwE (unit <> " is no longer in " <> T.pack rel)) pure at
              u' <- lift (unitIn working i) >>= either throwE pure
              unless (unitName u' == unitName u) (throwE (unit <> " is no longer in " <> T.pack rel))
              maybe (throwE (unit <> " is on one line")) pure (unitBody u')
            _ -> pure span'
          -- the working files (the project with the conjure's WRITES on top) made ready to fill
          prepare writes = do
            let ws' = overlayMany writes ws
                working = M.findWithDefault text rel writes
                testText = M.findWithDefault testText0 testRel writes
                moduleText = M.findWithDefault (siteModuleText site) (siteModuleRel site) writes
            at <- if M.member rel writes then relocate working else pure span'
            -- the before-run: the test file as the editor has it, with the writes on top
            before <- lift (either (TestRun [] [] . Just) id <$> runOver s ws' lang [T.pack testRel])
            (template, verify, context) <-
              if onTest
                then do
                  -- (a helper in a test file was refused above)
                  name <- maybe (throwE ("no test at line " <> tshow line)) pure (testCaseAtIn lang (Just rel) working (fst at))
                  except (testConjure lang working at (T.pack testRel <> "::" <> name) (siteModuleRel site) moduleText)
                else except (functionConjure lang working at unit testRel testText before)
            imported <- lift (importedContext ws' lang (rel : map fst context) (working : map snd context))
            -- every hint the person gave stays in force, round after round
            let hints = [("the person says", h) | Just (re, pd) <- [resuming], h <- [h' | ConjureStep _ (StepHint h') _ _ <- pdSteps pd] ++ maybe [] pure (reHint re)]
            pure (Working project {prWorkspace = ws'} working at testText before template verify (context ++ imported ++ hints))
          writes0 = maybe M.empty (pdWrites . snd) resuming
      case resuming of
        -- stop: nothing lands, the working files (a wire's) are dropped
        Just (re, pd) | reChoice re == ResumeStop, not (reLand re) -> do
          lift (stepLine ["conjure", "person", T.pack rel <> "::" <> unit, "stopped \183 nothing landed"])
          let none = TestsAnswer (siteTestFile site) ScopeFile Nothing (siteRoot site) (TestRun [] [] Nothing) 0 (failureOf lang)
          pure (Landed (ConjureAnswer file unit [] "nothing landed" False none (seedRound (pdSeed pd)) (pdRungs pd) True))
        -- reject: the pause the proposal came from, under the same token
        Just (re, pd) | reChoice re == ResumeReject, Just prev <- pdPrevious pd -> do
          now <- lift getMonotonicTime
          lift (keep s (reToken re) (withoutProposal pd) {pdShown = prev, pdAt = now})
          pure (Paused prev)
        -- reconjure of a test the scan flagged, once the wire gave the unit its port: the
        -- fake handed to the helper, edited here (no model), proposed as a re-conjure is
        Just (re, pd)
          | reChoice re == ResumeReconjure
          , Just tid <- reconjureOf re pd
          , before <- M.findWithDefault testText0 testRel writes0
          , Just (_, after) <- testFix (langConjure lang) unit (pdFlagged pd) (pdWired pd) before (testSourcesIn lang (Just testRel) before) tid -> do
              now <- lift getMonotonicTime
              let plain = plainPause (pdShown pd)
                  testAbs = absOf testRel
                  proposal = Proposal testRel tid before after (unifiedDiff testRel before after) Nothing (proposalStep (langImports lang) (shortName (paPrefixes plain) tid) before after) "test" []
                  rows = map (offerRow (OfferFacts 0 "" Nothing (paRound plain) "" Nothing False (paPrefixes plain) [] [] (langTypes lang >>= tyMethod)))
                  shown = plain {paOffers = rows [OfferAccept, OfferReject], paProposal = Just proposal}
              lift . keep s (reToken re) $
                pd
                  { pdProposal = Just proposal
                  , pdPrevious = Just plain
                  , pdAt = now
                  , pdBases = M.insertWith (\_ old -> old) testAbs (Just testText0) (pdBases pd)
                  , pdVersions = M.insertWith (\_ old -> old) testAbs (fst <$> M.lookup testAbs (ssDocs s)) (pdVersions pd)
                  , pdShown = shown
                  }
              lift (stepLine ["conjure-proposal", tid])
              pure (Paused shown)
        -- port: the method a port lacks added by the pack's generators over the working files
        -- (port-method, its holes the derived signature; port-method-adapters, each adapter's
        -- body the rung's model's), proposed as a test's fix is
        Just (re, pd) | reChoice re == ResumePort, Just (q, m, ps, r) <- portOf re pd -> do
          let plain = plainPause (pdShown pd)
              sets = maybe [] snd wire ++ servicePaths (langConjure lang) (siteModuleRel site)
              short = m <> " to " <> q
          attempt <- lift (portMethodPlan s lang project (worded (methodWords short) progress) filler writes0 sets q m ps r)
          stop <- lift (cancelled (ssCancel s))
          when stop (throwE "cancelled")
          now <- lift getMonotonicTime
          case attempt of
            Right (protoRel, files) -> do
              let (pb, pa) = fromMaybe ("", "") (listToMaybe [(b, a) | (f, b, a) <- files, f == protoRel])
                  others = [x | x@(f, _, _) <- files, f /= protoRel]
                  diff = T.concat [unifiedDiff f b a | (f, b, a) <- files]
                  proposal = Proposal protoRel (q <> "." <> m) pb pa diff Nothing ("port method: " <> q <> "." <> m <> "(" <> T.drop 2 ps <> ") -> " <> r) "port" others
                  rows = map (offerRow (OfferFacts 0 "" Nothing (paRound plain) "" Nothing False (paPrefixes plain) [] [] (langTypes lang >>= tyMethod)))
                  -- the rows say what lands: the method, not a test
                  said row = case orOffer row of
                    OfferAccept -> row {orDetail = "the method lands with the conjure; round " <> tshow (paRound plain + 1) <> " runs against it"}
                    OfferReject -> row {orDetail = "back to the rows; the port stands as it was"}
                    _ -> row
                  shown = plain {paOffers = map said (rows [OfferAccept, OfferReject]), paProposal = Just proposal}
              lift . keep s (reToken re) $
                pd
                  { pdProposal = Just proposal
                  , pdPrevious = Just plain
                  , pdAt = now
                  , pdBases = foldr (\(f, b, _) -> M.insertWith (\_ old -> old) (absOf f) (if M.member f writes0 then Nothing else Just b)) (pdBases pd) files
                  , pdVersions = foldr (\(f, _, _) -> M.insertWith (\_ old -> old) (absOf f) (fst <$> M.lookup (absOf f) (ssDocs s))) (pdVersions pd) files
                  , pdShown = shown
                  }
              lift (stepLine ["conjure-proposal", q <> "." <> m])
              pure (Paused shown)
            Left why -> do
              let shown = plain {paOutcome = Just ("Adding " <> short <> " failed: " <> T.dropWhileEnd (== '.') why <> ".")}
              lift (keep s (reToken re) pd {pdShown = shown, pdAt = now})
              lift (stepLine ["conjure-port-failed", q <> "." <> m])
              pure (Paused shown)
        -- reconjure: the wrong test conjured again over the working files, proposed
        Just (re, pd) | reChoice re == ResumeReconjure, Just tid <- reconjureOf re pd -> do
          wk <- prepare writes0
          let name = caseOfId tid
              working = wkTestText wk
              moduleText = M.findWithDefault (siteModuleText site) (siteModuleRel site) writes0
              plain = plainPause (pdShown pd)
              wsHere = prWorkspace (wkProject wk)
          -- the errors the test file (and the files beside it) already have are not the re-conjure's
          known <- lift (checkedOver s wsHere lang (M.singleton testRel working))
          let s' = s {ssPorts = tolerating (map fst known) (ssPorts s)}
              ownKnown = [(l, m) | ((p, m), l) <- known, p == testRel]
          attempt <- lift . runExceptT $ do
            when (T.unpack (fst (T.breakOn "::" tid)) /= testRel) (throwE (tid <> " is not in " <> T.pack testRel))
            l <- maybe (throwE ("no test " <> name <> " in " <> T.pack testRel)) pure (testCaseLineIn lang (Just testRel) working name)
            u <- lift (testOrUnitAt (pSyntax (ssPorts s)) lang (Just testRel) working l) >>= either throwE pure
            at <- maybe (throwE (name <> " is on one line")) pure (unitBody u)
            (template, verify, context) <- except (testConjure lang working at tid (siteModuleRel site) moduleText)
            let context' =
                  context
                    ++ [("the unit under test", ownerHeader lang (wkText wk) (fst (wkSpan wk))), ("why the test is wrong", reconjureWhy (pdShown pd) tid)]
                    ++ [("the wire", "the helper now takes " <> w <> ": pass the test's fake to it") | w <- take 1 (pdWired pd)]
                    ++ [("the test's errors", knownNote ownKnown) | not (null ownKnown)]
                    ++ [("the person says", h) | Just h <- [reHint re]]
                ws' = prWorkspace (wkProject wk)
            imported <- lift (importedContext ws' lang (testRel : map fst context') (working : map snd context'))
            rounds' <- lift (newIORef 0)
            fill [] (worded (fixWords (shortName (paPrefixes plain) tid)) progress) rounds' s' (wkProject wk) testRel working template verify (context' ++ imported) filler (const Nothing)
          stop <- lift (cancelled (ssCancel s))
          when stop (throwE "cancelled")
          now <- lift getMonotonicTime
          let testAbs = absOf testRel
              -- PD proposing P (from the pause SHOWN, what reject answers again); the test file
              -- as the conjure first read it (a wire's reading stands)
              proposing p shown =
                pd
                  { pdProposal = Just p
                  , pdPrevious = Just shown
                  , pdAt = now
                  , pdBases = M.insertWith (\_ old -> old) testAbs (Just testText0) (pdBases pd)
                  , pdVersions = M.insertWith (\_ old -> old) testAbs (fst <$> M.lookup testAbs (ssDocs s)) (pdVersions pd)
                  }
              rows = map (offerRow (OfferFacts 0 "" Nothing (paRound plain) "" Nothing False (paPrefixes plain) [] [] (langTypes lang >>= tyMethod)))
          case attempt of
            Right (Written Nothing after _) | after /= testText0 -> do
              let proposal = Proposal testRel tid testText0 after (unifiedDiff testRel testText0 after) Nothing (proposalStep (langImports lang) (shortName (paPrefixes plain) tid) testText0 after) "test" []
                  shown = plain {paOffers = rows [OfferAccept, OfferReject], paProposal = Just proposal}
              lift (keep s (reToken re) (proposing proposal plain) {pdShown = shown})
              lift (stepLine ["conjure-proposal", tid])
              pure (Paused shown)
            _ -> do
              -- refused: said first; what it wrote is kept to be seen (and accepted anyway)
              let (why, written) = case attempt of
                    Left w -> (w, Nothing)
                    Right (Written (Just w) l _) -> (w, Just l)
                    Right (Halted l w) -> (w, Just l)
                    Right (Written Nothing _ _) -> ("it wrote the test as it is", Nothing)
                  written' = case written of
                    Just l | l /= testText0 -> Just l
                    _ -> Nothing
              added <- lift (maybe (pure []) (\l -> checkedOver s' wsHere lang (M.singleton testRel l)) written')
              let reason = case [(m, l) | ((p, m), l) <- added, p == testRel] of
                    (m, l) : _ -> addsReason m l
                    [] -> T.dropWhileEnd (== '.') (firstLine why)
                  refusal = (\l -> Proposal testRel tid testText0 l (unifiedDiff testRel testText0 l) (Just reason) (proposalStep (langImports lang) (shortName (paPrefixes plain) tid) testText0 l) "test" []) <$> written'
                  again r = case r of
                    OfferRow (OfferReconjure t) _ _ | t == tid -> rows ([OfferRefused | isJust refusal] ++ [OfferReconjureAgain t])
                    _ -> [r]
                  shown = plain {paOutcome = Just (refusedSentence (shortName (paPrefixes plain) tid) reason), paProposal = refusal, paOffers = concatMap again (paOffers plain), paHeld = [r | r@(OfferRow (OfferReconjure t) _ _) <- paOffers plain, t == tid]}
              lift . keep s (reToken re) $ case refusal of
                Just p -> (proposing p shown) {pdShown = shown}
                Nothing -> pd {pdShown = shown, pdAt = now}
              lift (stepLine ["conjure-reconjure-failed", tid])
              pure (Paused shown)
        _ -> do
          first <- prepare writes0
          counters <- lift . newIORef $
            let fresh =
                  Counters
                    { cRung = 0
                    , cRungs = [mfName filler]
                    , cRounds = 0
                    , cSecondsBefore = 0
                    , cSeconds = 0
                    , cLast = Nothing
                    , cState = Nothing
                    , cHint = Nothing
                    , cOffers = []
                    , cWire = Nothing
                    , cWired = []
                    , cWrites = writes0
                    , cVersions = M.singleton file started
                    , cBases = M.empty
                    , cExplanation = Nothing
                    , cTestText = wkTestText first
                    , cTestRel = testRel
                    , cHeader = ownerHeader lang (wkText first) (fst (wkSpan first))
                    , cHistory = []
                    , cBefore = wkBefore first
                    , cPrefixes = namePrefixesIn lang (T.pack (takeBaseName (siteModuleRel site)))
                    , cUnit = unit
                    , cCause = Nothing
                    , cFlagged = []
                    , cService = Nothing
                    , cCandidates = []
                    , cDerived = Nothing
                    , cDiagnosis = []
                    , cWiredFor = Nothing
                    , cRewritten = []
                    , cSteps = []
                    , cWireWrites = M.empty
                    }
             in case resuming of
                  Nothing -> fresh
                  Just (re, pd) ->
                    let climbing = reChoice re == ResumeClimb
                        spentAll = maybe True (\sd -> pdRounds pd >= sdRounds sd || pdSeconds pd >= sdBudget sd) steered
                        (rounds0, seconds0) = if climbing && not spentAll then (pdRounds pd, pdSeconds pd) else (0, 0)
                     in fresh
                          { cRung = rung0
                          , cRungs = pdRungs pd ++ [mfName filler | climbing]
                          , cRounds = rounds0
                          , cSecondsBefore = seconds0
                          , cSeconds = seconds0
                          , cLast = Just (pdLast pd)
                          , cHint = reHint re
                          , cWired = pdWired pd
                          , cVersions = M.insert file (pdVersion pd) (pdVersions pd)
                          , cBases = pdBases pd
                          , cHistory = pdHistory pd
                          , cBefore = pdBefore pd
                          , cFlagged = pdFlagged pd
                          , cWiredFor = paWiredFor (pdShown pd)
                          , cRewritten = pdRewritten pd
                          , cSteps = pdSteps pd ++ [ConjureStep (paRound (pdShown pd)) (StepHint h) ("hint: " <> h) [] | Just h <- [reHint re]]
                          , cWireWrites = pdWireWrites pd
                          }
          -- the test file's pyright errors as the conjure began (with what its wire wrote): a
          -- round that adds one is refused; these stand, and the green summary says so
          knownRef <- lift (newIORef [])
          let refreshKnown wires = unless onTest $ do
                known <- checkedOver s ws lang (M.insert testRel (M.findWithDefault testText0 testRel wires) wires)
                writeIORef knownRef [m | (m@(p, _), _) <- known, p == testRel]
          lift (refreshKnown (maybe M.empty (pdWireWrites . snd) resuming))
          rounds <- lift (newIORef (maybe 0 (seedRound . pdSeed . snd) resuming))
          -- a steered conjure's progress names the round (of the rounds this grant reaches)
          stepped <- lift $ case steered of
            Just sd -> roundsProgress (maybe 0 (seedRound . pdSeed . snd) resuming + sdRounds sd) (\r -> maybe "" beModel (lookup r (mcBackends (catalogModel (prCatalog project))))) progress
            Nothing -> pure progress
          let seed0 = case resuming of
                Just (re, pd) -> maybe id withHint (reHint re) (pdSeed pd)
                Nothing -> noSeed
              steering seed record = (\sd -> Steering seed (steer s lang stepLine begun counters sd (isJust wire) derive methodWall record)) <$> steered
              -- the method a round's check says a port the unit takes lacks, and its signature
              -- when the call and the test's fake give it
              methodWall attempt why = do
                c <- readIORef counters
                case missingMemberIn lang (map snd (ctorParamsIn (langConjure lang) (cHeader c))) why of
                  Nothing -> pure Nothing
                  Just (q, m) -> do
                    sf <- service
                    let moduleText = M.findWithDefault (siteModuleText site) (siteModuleRel site) (cWrites c)
                        -- a signature only where the pack can add the method (`[conjure.port] add`)
                        sig = (langConjure lang >>= cjAddMethod) >> listToMaybe [p | p <- sfPorts sf, ptName p == q] >>= \p -> methodSignature lang m p attempt moduleText (cTestText c)
                    pure (Just (q, m, sig))
              decider = deciderUnder (ssCancel s) s project
              -- the service the wire's inputs come from (read once)
              service = do
                c <- readIORef counters
                case cService c of
                  Just sf -> pure sf
                  Nothing -> do
                    sf <- serviceFacts ws lang (siteModuleRel site) (maybe [] snd wire ++ servicePaths (langConjure lang) (siteModuleRel site))
                    sf <$ modifyIORef' counters (\c' -> c' {cService = Just sf})
              -- the wire's (param, port) for a flagged test's fake, and the candidates (`param: Port`):
              -- by rule, else Jev's conjure-port among the service's ports (once a test)
              derive :: Maybe (Decider IO) -> (Text, Fake) -> IO (Maybe (Text, Text), [Text])
              derive dec (tid, f) = do
                c <- readIORef counters
                case cDerived c of
                  Just (t, d) | t == tid -> pure d
                  _ -> do
                    sf <- service
                    let ports = sfPorts sf
                        src = fromMaybe "" (testSourceIn lang (Just testRel) (testName tid) (cTestText c))
                    (port, weights) <- case derivePortIn (langConjure lang) f ports of
                      Just q -> pure (Just q, [])
                      Nothing
                        | length ports >= 2, Just d <- dec -> do
                            (answer, weights) <- weighOr d (conjurePortQuestion src (fakeCalls src f) ports)
                            stepLine ["conjure-port", answerChoice answer, answerConfidence answer]
                            pure (portFrom ports answer, weights)
                      _ -> pure (Nothing, [])
                    let param q = deriveParam q (sfSiblings sf) f
                        got = ((\q -> (param q, q)) <$> port, maybe [param q <> ": " <> q | q <- portCandidates weights ports] (\q -> [param q <> ": " <> q]) port)
                    got <$ modifyIORef' counters (\c' -> c' {cDerived = Just (tid, got)})
              -- the wire (P, Q) run in-process over BASE, the working files made ready again over what
              -- it wrote; a failure is logged and kept for the next pause
              wireOver base p q gid pathSets = do
                c <- lift (readIORef counters)
                let sets = [("param", p), ("port", q)] ++ pathSets
                planned <- lift (wirePlan s project base gid sets)
                ready <- case planned of
                  Left why -> pure (Left why)
                  Right new -> fmap (new,) <$> lift (runExceptT (prepare (M.union new base)))
                case ready of
                  Right (new, wk') -> do
                    -- each file the wire wrote, as the conjure first read it
                    read' <- lift (traverse (\r -> (absOf r,) <$> readText ws r) [r | r <- M.keys new, r /= rel, M.notMember (absOf r) (cBases c)])
                    let oldOf r = case M.lookup r base of
                          Just t -> t
                          Nothing | r == rel -> text
                          Nothing -> fromMaybe "" (join (lookup (absOf r) read' <|> M.lookup (absOf r) (cBases c)))
                    lift . writeIORef counters $
                      c
                        { cWire = Nothing
                        , cWired = cWired c ++ [p <> ": " <> q]
                        , cSteps = cSteps c ++ [ConjureStep (maybe 0 rsRound (cState c)) StepWire ("wire " <> p <> ": " <> q) [(r, unifiedDiff r (oldOf r) t) | (r, t) <- M.toList new, oldOf r /= t]]
                        , cWiredFor = cWiredFor c <|> listToMaybe (map fst (cFlagged c))
                        , cWrites = M.union new base
                        , cWireWrites = M.union (M.filterWithKey (\r t -> oldOf r /= t) new) (cWireWrites c)
                        , cVersions = M.union (cVersions c) (M.fromList [(a', fst <$> M.lookup a' (ssDocs s)) | (a', _) <- read'])
                        , cBases = M.union (cBases c) (M.fromList read')
                        , cTestText = wkTestText wk'
                        , cHeader = if onTest then cHeader c else ownerHeader lang (wkText wk') (fst (wkSpan wk'))
                        }
                    lift . refreshKnown . cWireWrites =<< lift (readIORef counters)
                    lift (stepLine ("conjure-wire" : gid : [k <> "=" <> v | (k, v) <- sets]))
                    pure (Right wk')
                  Left why -> do
                    lift (writeIORef counters c {cWire = Nothing, cExplanation = Just ("wire failed: " <> why)})
                    lift (stepLine ["conjure-wire-failed", gid])
                    pure (Left why)
              -- the fill, and, while it pauses for a wire, the wire and the fill again from the round it paused at
              loop wk f seed = do
                tried <- lift (runExceptT (fill [testRel | not onTest] stepped rounds s {ssPorts = toleratingIn knownRef (ssPorts s)} (wkProject wk) rel (wkText wk) (wkTemplate wk) (wkVerify wk) (wkContext wk) f (steering seed)))
                c <- lift (readIORef counters)
                stop <- lift (cancelled (ssCancel s))
                case (tried, cWire c, wire, steered, cState c) of
                  (Right (Halted _ _), Just (p, q), Just (gid, pathSets), Just sd, Just rs) | not stop -> do
                    let seed' = maybe id withHint (cHint c) (Seed (rsRound rs) (M.fromList [(rhName h, rhPrevious h) | h <- rsHoles rs]))
                        f' = maybe f snd (listToMaybe (drop (cRung c) (sdLadder sd)))
                    wireOver (cWrites c) p q gid pathSets >>= \case
                      Right wk' -> loop wk' f' seed'
                      Left _ -> loop wk f' seed'
                  _ -> pure (tried, wk)
              -- before round 1 (a function's or a method's conjure, not a resume): the scan over the
              -- before-run; a flagged test the class cannot take its fake for is wired, silently
              beforeRound1 = do
                c <- lift (readIORef counters)
                let flags = scanFlags lang (wkTestText first) (wkBefore first)
                    failing = map fst (trFailed (wkBefore first))
                lift (logFlags stepLine [] flags)
                lift (modifyIORef' counters (\c' -> c' {cFlagged = flags}))
                case (flags, wire) of
                  (flagged : _, Just (gid, pathSets)) -> do
                    (derived, _) <- lift (derive decider flagged)
                    sf <- lift service
                    let lacks = not (any (takesPortIn (langConjure lang) (cHeader c)) (maybe (map ptName (sfPorts sf)) (pure . snd) derived))
                    case policy (PolicyFacts flags failing failing lacks True (cWired c) derived False False False Nothing) of
                      PolicyWire p q -> either (const first) id <$> wireOver (cWrites c) p q gid pathSets
                      _ -> pure first
                  _ -> pure first
          (tried, wk0) <- case resuming of
            Just (re, pd) | stopping -> case keptLanding re pd of
              Just (n, l) -> pure (Right (Written Nothing l (if n == paRound (pdShown pd) then pdUndefined pd else [])), first)
              Nothing -> throwE "there is no body to keep"
            Nothing | not onTest, isNothing selected -> beforeRound1 >>= \wk1 -> loop wk1 filler seed0
            -- wire: the offered wire runs over the working files, then the rounds go on
            Just (re, pd) | reChoice re == ResumeWire, Just (p, q) <- wireOf re pd -> case wire of
              Just (gid, pathSets) -> do
                c <- lift (readIORef counters)
                wired <- wireOver (cWrites c) p q gid pathSets
                loop (either (const first) id wired) filler seed0
              Nothing -> throwE "no wire generator applies here any more: conjure again"
            _ -> loop first filler seed0
          -- a cancelled conjure lands nothing and keeps no pause
          stop <- lift (cancelled (ssCancel s))
          when stop (throwE "cancelled")
          outcome0 <- except tried
          -- a test conjure whose fill passed its verify: the scan checks it passes every fake it
          -- builds; one for a port the class lacks is wired (when a generator applies) and the
          -- test filled once more over the wired files
          let refill p q wk' = do
                c <- lift (readIORef counters)
                let f' = fromMaybe filler (steered >>= \sd -> snd <$> listToMaybe (drop (cRung c) (sdLadder sd)))
                    relabel (k, v) = (if k == rejectedBody then T.unpack ("the test as first written; the helper now takes " <> p <> ": " <> q <> ": pass the test's fake to it") else k, v)
                fill [] progress rounds s (wkProject wk') rel (wkText wk') (wkTemplate wk') (wkVerify wk') (map relabel (wkContext wk')) f' (const Nothing)
          (outcome, wk, note) <- case outcome0 of
            Written Nothing written0 _
              | onTest && isNothing selected && not stopping -> do
                  c <- lift (readIORef counters)
                  let name = fromMaybe unit (testCaseAtIn lang (Just rel) written0 (fst (wkSpan wk0)))
                      tid = T.pack testRel <> "::" <> name
                      moduleText = M.findWithDefault (siteModuleText site) (siteModuleRel site) (cWrites c)
                      still = "; the test still builds its own fake, re-conjure it"
                  case testSourceIn lang (Just rel) (testName name) written0 >>= fakeIn (langConjure lang) of
                    Nothing -> (outcome0, wk0, "") <$ lift (stepLine ["conjure-test", "ok", "-"])
                    Just f -> do
                      lift (logFlags stepLine [] [(tid, f)])
                      lift (modifyIORef' counters (\c' -> c' {cTestText = written0}))
                      (derived, _) <- lift (derive decider (tid, f))
                      let lacks = " \183 needs a port the class lacks: " <> maybe (fkVariable f <> " = " <> fkClass f <> "()") (\(p, q) -> p <> ": " <> q) derived
                      wire' <- lift (if null (cWired c) then wireFor s project (siteModuleRel site) else pure Nothing)
                      case (derived, wire') of
                        (Just (_, q), _) | takesPortIn (langConjure lang) (classHeaders lang moduleText) q -> pure (outcome0, wk0, "")
                        (Just (p, q), Just (gid, pathSets)) ->
                          wireOver (M.insert rel written0 (cWrites c)) p q gid pathSets >>= \case
                            Left _ -> pure (outcome0, wk0, lacks)
                            Right wk' -> do
                              refilled <- lift (runExceptT (refill p q wk'))
                              cancelled' <- lift (cancelled (ssCancel s))
                              when cancelled' (throwE "cancelled")
                              case refilled of
                                Right o@(Written Nothing w _)
                                  | isNothing (testSourceIn lang (Just rel) (testName (fromMaybe name (testCaseAtIn lang (Just rel) w (fst (wkSpan wk'))))) w >>= fakeIn (langConjure lang)) -> pure (o, wk', "")
                                  | otherwise -> pure (o, wk', still)
                                -- the wire and the test as first written land together
                                _ -> pure (Written Nothing (wkText wk') [], wk', still)
                        _ -> pure (outcome0, wk0, lacks)
            _ -> pure (outcome0, wk0, "")
          latest <- lift (maybe (pure started) (\live -> fmap fst <$> live file) (ssLiveDoc s))
          when (latest /= started) (throwE "the file changed while conjuring; your text stands")
          c <- lift (readIORef counters)
          -- keep 0 lands only what the wire step wrote (later steps' writes stay out)
          let wireAlone = maybe False (keepsWireAlone . fst) resuming
              landing' = if wireAlone then cWireWrites c else cWrites c
              others = [(r, absOf r, new) | (r, new) <- M.toList landing', r /= rel]
          case outcome of
            Halted landing why -> do
              (view, rs) <- maybe (throwE "the conjure paused before any round") pure ((,) <$> cLast c <*> cState c)
              now <- lift getMonotonicTime
              -- the latest round's real failure: its tests', or the check's that refused it before
              -- they ran, each problem with its line (the check run over what that round wrote)
              let checker = T.takeWhile (/= '-') . checkFormat <$> langCheck lang
              failure <- lift $ case listToMaybe (reverse (cHistory c)) of
                Just r | Just run <- rrRun r -> pure (Just (LatestTests (rrRound r) (map fst (trFailed run))))
                Just r | Just refusal <- rrCheck r -> do
                  problemsAt <- checkedOver s (prWorkspace (wkProject wk)) lang (M.singleton rel landing)
                  let tool = fromMaybe "the check" checker
                      problems = nub [(p, l, m') | ((p, m), l) <- problemsAt, let m' = T.strip (T.takeWhile (/= '\n') m), m' `T.isInfixOf` refusal]
                      -- unmatched (the check said it otherwise): the refusal's own words, no line
                      said = [(T.unpack (T.strip p), Nothing, T.strip (T.drop 1 m)) | piece <- T.splitOn "; " refusal, let (p, m) = T.breakOn ": " piece, not (T.null m)]
                  pure (Just (LatestCheck (rrRound r) tool (rootFirst (if null problems then said else problems))))
                _ -> pure Nothing
              let lastWhy = maybe "" rhWhy (listToMaybe (rsHoles rs))
                  seed' = Seed (rsRound rs) (M.fromList [(rhName h, rhPrevious h) | h <- rsHoles rs])
                  sentences = case cCause c of
                    Just cause' | not (null (cDiagnosis c)) -> diagnosis facts {dfFailure = failure} cause'
                    _ -> if null (cDiagnosis c) then [why] else cDiagnosis c
                  explanation = T.unwords sentences
                  models = catalogModel (prCatalog project)
                  grant = maybe 0 sdRounds steered
                  next = steered >>= \sd -> rungName . fst <$> listToMaybe (drop (cRung c + 1) (sdLadder sd))
                  className = classOf lang (cHeader c)
                  owner = if T.null className then unit else className
                  title = Title (if T.null className then unit else className <> "." <> unit) (rsRound rs) grant (rsRung rs) (maybe "" beModel (lookup (rsRung rs) (mcBackends models))) (round (cSeconds c))
                  facts = diagnosisFacts lang c (rsRung rs) next (round (cSeconds c))
                  cause = fromMaybe CauseAsk (cCause c)
                  lastDecided = rrDecided =<< listToMaybe (reverse (cHistory c))
                  suspects = suspectFirst (cFlagged c) (involved (cBefore c) (cHistory c))
                  again = [(t, f) | t <- suspects, Just (Just f) <- [fixAgain (failureOf lang) (cSteps c) (cHistory c) t]]
                  rows = map (offerRow (OfferFacts grant (rsRung rs) next (rsRound rs) owner (listToMaybe suspects) (not (null (cWired c))) (cPrefixes c) (fixMoves facts) again (langTypes lang >>= tyMethod))) (cOffers c)
                  -- what each written file is against what the conjure read (the unit's file: the buffer)
                  baseOf r = if r == rel then text else fromMaybe "" (M.findWithDefault Nothing (absOf r) (cBases c))
                  wiredFiles = [(r, unifiedDiff r (baseOf r) new) | (r, new) <- M.toList (cWrites c), baseOf r /= new]
                  -- each wire with what it wrote (its step's files), not the steps after it
                  wired = zip (cWired c) ([csFiles st | st@(ConjureStep _ StepWire _ _) <- cSteps c] ++ repeat [])
                  -- each round's body over the working unit's file (the last round's: what the fill left)
                  lastRound = rrRound <$> listToMaybe (reverse (cHistory c))
                  landings =
                    [ (rrRound r, if Just (rrRound r) == lastRound then landing else fillLines (Just lang) (wkText wk) (wkSpan wk) (rrAttempt r))
                    | r <- cHistory c
                    , not (T.null (T.strip (rrAttempt r)))
                    ]
                  roundFiles = [(n, (rel, unifiedDiff rel text l) : [(r, d) | (r, d) <- wiredFiles, r /= rel]) | (n, l) <- landings]
                  shownAs t =
                    PausedAnswer
                      { paToken = t
                      , paFile = file
                      , paUnit = unit
                      , paLines = span'
                      , paRound = rsRound rs
                      , paRung = rsRung rs
                      , paSummary = roundSummary view
                      , paWhy = fromMaybe why (listToMaybe sentences)
                      , paTitle = title
                      , paDiagnosis = sentences
                      , paTrigger = trigger cause lastDecided (dfWireFailed facts)
                      , paRounds = cHistory c
                      , paTests = testEntries facts
                      , paHeader = cHeader c
                      , paPorts = [n <> ": " <> ty | (n, ty) <- ctorParamsIn (langConjure lang) (cHeader c)]
                      , paLanding = unifiedDiff rel text landing
                      , paWired = wired
                      , paOffers = rows
                      , paExpires = round pausedLifetime
                      , paPrefixes = cPrefixes c
                      , paComment = langComment lang
                      , paProposal = Nothing
                      , paOutcome = Nothing
                      , paRoundFiles = roundFiles
                      , paWiredFor = cWiredFor c
                      , paHeld = []
                      , paSteps = cSteps c
                      , paFailure = failure
                      , paChecker = checker
                      , paFailureShape = failureOf lang
                      }
                  pd = PausedConjure file started line selected seed' (cRung c) (cRungs c) (cRounds c) (cSeconds c) view (cHint c) landing (undefinedNamesIn lang lastWhy) now (cWrites c) (M.delete file (cVersions c)) (cBases c) (cWired c) explanation (cHistory c) (cBefore c) (cFlagged c) (cCause c) Nothing Nothing
              token <- lift (atomicModifyIORef' (ssPaused s) (\(n, m) -> let t = "conjure-" <> tshow (n + 1) in ((n + 1, M.insert t (pd (shownAs t) landings (cRewritten c) (cSteps c) (cWireWrites c)) m), t)))
              lift (stepLine ("conjure-pause" : maybe [why] causeWords (cCause c)))
              pure (Paused (shownAs token))
            Written _ written extra -> do
              -- every other file the conjure wrote stands as it read it
              lift (movedSince s (cVersions c) (cBases c) [(r, a) | (r, a, _) <- others]) >>= maybe (pure ()) (throwE . changedWhile)
              -- the after-run: the test file with the conjure in place, over the working files
              (_, files) <- lift (projectFiles (if wireAlone then overlayMany (cWireWrites c) ws else prWorkspace (wkProject wk)) (M.empty :: Map FilePath Text) lang [testRel])
              start <- lift getMonotonicTime
              after <- lift (either (TestRun [] [] . Just) id <$> runTestsIn (pTester (ssPorts s) (ssCancel s)) lang [T.pack testRel] (M.insert rel written files))
              end <- lift getMonotonicTime
              -- the test file's errors it had at the start and still has: green stands, saying so
              staying <- lift $ do
                known <- readIORef knownRef
                if onTest || null known
                  then pure 0
                  else length . filter ((== testRel) . fst . fst) <$> checkedOver s ws lang (M.insert rel written landing')
              n <- lift (readIORef rounds)
              let kept = case resuming of
                    Just (re, pd) | reChoice re == ResumeStop, reLand re -> fst <$> keptLanding re pd
                    _ -> Nothing
                  -- the landing in a line (design §8): what came of it, then the wire and the tests rewritten
                  params = [maybe w fst (paramPort w) | w <- cWired c]
                  extras = [T.intercalate ", " params <> " wired" | not (null params)] ++ [T.intercalate ", " (map (shortName (cPrefixes c)) (cRewritten c)) <> " rewritten" | not (null (cRewritten c)), not wireAlone]
                  withExtras t = T.intercalate " \183 " (t : extras)
                  keptWhat k = if k == 0 then "the wire alone" else "round " <> tshow k
                  keptLine k = withExtras ("kept " <> keptWhat k <> " \183 " <> shortVerdict)
                  shortVerdict = case (trBroken after, trFailed after) of
                    (Nothing, fs@(_ : _)) | null (concatMap (undefinedNamesIn lang . fMessage . snd) fs ++ extra) -> "red: " <> T.intercalate ", " (map (shortName (cPrefixes c) . fst) fs)
                    _ -> functionSummary lang extra after
                  headline
                    | onTest = testSummary lang extra tid (wkBefore wk) after written
                    | isSteered && green = "green in round " <> tshow n <> " \183 " <> T.intercalate " \8594 " rungs
                    | otherwise = functionSummary lang extra after
                  staying' = [" \183 " <> T.pack (takeFileName testRel) <> " has " <> tshow staying <> " " <> fromMaybe "check" (T.takeWhile (/= '-') . checkFormat <$> langCheck lang) <> (if staying == 1 then " error" else " errors") | staying > 0, green]
                  summary = maybe (withExtras headline <> note) keptLine kept <> mconcat staying'
                  tid = T.pack testRel <> "::" <> fromMaybe unit (testCaseAtIn lang (Just rel) written (fst (wkSpan wk)))
                  green = isNothing (trBroken after) && null (trFailed after)
                  edit = [FileEdit file started False range new | written /= text, let (range, new) = smallestChange text written]
                  edits =
                    [ case M.findWithDefault Nothing a (cBases c) of
                        Nothing -> FileEdit a version True ((0, 0), (0, 0)) new
                        Just old -> let (range, t) = smallestChange old new in FileEdit a version False range t
                    | (_, a, new) <- others
                    , let version = M.findWithDefault Nothing a (cVersions c)
                    , M.findWithDefault Nothing a (cBases c) /= Just new
                    ]
                  tests = TestsAnswer (siteTestFile site) ScopeFile Nothing (siteRoot site) after (end - start) (failureOf lang)
                  isSteered = isJust steered || isJust resuming
                  rungs = if isSteered then cRungs c else [mfName filler]
                  by
                    | isSteered = T.intercalate " \8594 " rungs <> " \183 " <> tshow n <> (if n == 1 then " round" else " rounds")
                    | otherwise = mfName filler
              lift (stepLine ["conjure", "person", T.pack rel <> "::" <> unit, maybe ("conjured by " <> by <> " \183 " <> summary) (\k -> "kept " <> keptWhat k <> " \183 " <> functionSummary lang extra after) kept])
              pure (Landed (ConjureAnswer file unit (edit ++ edits) summary green tests n rungs (isJust kept)))

-- | The `[conjure] wire` generator for the unit at REL (root-relative):
-- its id and the inputs its `[applies_at] path` binds there; none when
-- unset, unknown, with no `[applies_at]`, or not matching.
wireFor :: ServerState -> Project -> FilePath -> IO (Maybe (Text, [(Text, Text)]))
wireFor s project rel = case catalogConjureWire cat of
  Nothing -> pure Nothing
  Just gid -> case findEntry cat gid of
    Left _ -> pure Nothing
    Right entry ->
      loadGenerator (pFiles (ssPorts s)) (pSyntax (ssPorts s)) cat entry <&> \case
        Right gen | Just ap <- genAppliesAt gen, Just sets <- matchPathSlots (apPath ap) rel -> Just (gid, sets)
        _ -> Nothing
  where
    cat = prCatalog project

-- | What the wire's inputs are derived from: the service's ports and the
-- @__init__@ parameters of the unit's siblings (one list a sibling).
data ServiceFacts = ServiceFacts {sfPorts :: [Port], sfSiblings :: [[(Text, Text)]]}
  deriving stock (Eq, Show)

-- | The service of the unit at REL (root-relative) in LANG, by the wire's path sets (@app@,
-- @service@) over the pack's `[conjure] service`: the ports (`[conjure.port]`) of its files
-- under the service that `port.files` names, and the constructor parameters
-- (`[conjure.constructor]`) of every other file `constructor.files` names, test files left out.
serviceFacts :: Workspace IO -> Language -> FilePath -> [(Text, Text)] -> IO ServiceFacts
serviceFacts ws lang rel sets = case serviceBase cj sets of
  Just base -> do
    let texts fs = concat <$> traverse (\f -> maybe [] (pure . (f,)) <$> readText ws f) fs
    ports <- maybe (pure []) (\ps -> (`portsIn'` ps) <$> (texts =<< filesUnder ws base (psFiles ps))) (cj >>= cjPort)
    siblingFiles <- maybe (pure []) (filesUnder ws base . ctFiles) (cj >>= cjConstructor)
    siblings <- texts (siblingsOf lang (normalisePath rel) siblingFiles)
    pure (ServiceFacts ports (filter (not . null) (map (ctorParamsIn cj . snd) siblings)))
  Nothing -> pure (ServiceFacts [] [])
  where
    cj = langConjure lang
    portsIn' files ps = portsIn ps files

-- | The service's folder: the pack's `[conjure] service` with the wire's SETS in it; none
-- without one, or when a set it names is missing.
serviceBase :: Maybe ConjureDef -> [(Text, Text)] -> Maybe FilePath
serviceBase cj sets = do
  t <- cj >>= cjService
  segs <- traverse seg (T.splitOn "/" t)
  pure (joinPath (map T.unpack segs))
  where
    seg x = case T.stripSuffix "=}" =<< T.stripPrefix "{=" x of
      Just k -> lookup (T.strip k) sets
      Nothing -> Just x

-- | The files under BASE (root-relative) that one of GLOBS (relative to BASE) matches.
filesUnder :: Workspace IO -> FilePath -> [Text] -> IO [FilePath]
filesUnder ws base globs = do
  found <- listFiles ws base
  pure [normalisePath (base </> f) | f <- found, any (`matchesGlob` f) globs]

-- | The generator GID run in-process with SETS over the project with the
-- conjure's WRITES on top (no model fills it): the files it writes
-- (root-relative), those it leaves as they were left out. Left on a
-- catalog or generation error, a conflict, or when it writes nothing.
wirePlan :: ServerState -> Project -> Map FilePath Text -> Text -> [(Text, Text)] -> IO (Either Text (Map FilePath Text))
wirePlan s project writes gid sets = case findEntry cat gid of
  Left err -> pure (Left (describeCatalogError err))
  Right entry ->
    loadGenerator (pFiles ports) (pSyntax ports) cat entry >>= \case
      Left err -> pure (Left (describeCatalogError err))
      Right gen ->
        planWith ports project {prWorkspace = overlayMany writes (prWorkspace project)} planOptions {poFiller = Nothing, poCancel = ssCancel s} [Step gen (M.fromList sets)] <&> \case
          Left err -> Left (describeGenerationError err)
          Right plan
            | not (null (planConflicts plan)) -> Left (T.intercalate "; " (map describeConflict (planConflicts plan)))
            | M.null (changed plan) -> Left (gid <> " writes nothing here")
            | otherwise -> Right (changed plan)
  where
    cat = prCatalog project
    ports = ssPorts s
    changed plan = M.filterWithKey (\r t -> M.lookup r (planBefore plan) /= Just (Just t)) (planWrites plan)

-- | The method M (its parameters PS, returning R) added to the port Q over the project with
-- the conjure's WRITES on top, the service's SETS: the pack's `[conjure.port] add` placed on a
-- blank line after Q's last line (made blank when it is not), its holes filled with the
-- signature as the person would fill its tab stops (@name@, @params@, @returns@, the parts
-- `[types] method` names), then its `adapters`, when it names one, over that, given the slots
-- of its `[applies_at] path` over Q's file, the SETS, and @name@, @params@, @returns@ and
-- @signature@, each only when it declares that input (the bodies it leaves FILLER fills).
-- The port's file and each file changed (root-relative, before, after); Left: why not.
portMethodPlan :: ServerState -> Language -> Project -> (ProgressEvent -> IO ()) -> ModelFiller IO -> Map FilePath Text -> [(Text, Text)] -> Text -> Text -> Text -> Text -> IO (Either Text (FilePath, [(FilePath, Text, Text)]))
portMethodPlan s lang project progress filler writes sets q m ps r = runExceptT $ do
  ps' <- maybe (throwE ("no `[conjure.port]` for " <> langName lang <> ": conjure finds no ports")) pure (langConjure lang >>= cjPort)
  am <- maybe (throwE ("no `[conjure.port] add` for " <> langName lang <> ": add " <> m <> " to " <> q <> " yourself")) pure (langConjure lang >>= cjAddMethod)
  base <- maybe (throwE ("no `[conjure] service` for " <> langName lang <> ": conjure cannot find the service")) pure (serviceBase (langConjure lang) sets)
  found <- lift (filesUnder ws base (psFiles ps'))
  texts <- lift (traverse (\f -> (f,) <$> readText ws f) found)
  (protoRel, protoText) <- maybe (throwE ("no port " <> q <> " under " <> T.pack base)) pure (listToMaybe [(f, t) | (f, Just t) <- texts, any ((== q) . ptName) (portsIn ps' [(f, t)])])
  line <- maybe (throwE ("no port " <> q <> " in " <> T.pack protoRel)) pure (portEnd ps' q protoText)
  let addId = amGenerator am
  method0 <- load addId
  let here = project {prWorkspace = overlayMany (M.singleton protoRel (blankAt line protoText)) ws}
  method <- lift (placeAt ports here (protoRel, line) [method0]) >>= either throwE (maybe (throwE (addId <> " placed nothing")) pure . listToMaybe)
  planned <- lift (planWith ports here planOptions {poAt = Just (protoRel, line), poSnippet = True, poCancel = ssCancel s} [Step method M.empty]) >>= either (throwE . describeGenerationError) pure
  marked <- maybe (throwE (addId <> " wrote no method there")) pure (M.lookup protoRel (planMarked planned))
  let values = [("name", m), ("params", ps), ("returns", r)]
  protoAfter <- T.concat <$> traverse (either (\mk -> maybe (throwE (addId <> " has a hole `" <> markerName mk <> "` the signature does not fill")) pure (lookup (markerName mk) values)) pure) (splitMarkers marked)
  let ws' = overlayMany (M.singleton protoRel protoAfter) ws
  changed <- case amAdapters am of
    Nothing -> pure []
    Just adaptersId -> do
      adapters <- load adaptersId
      let declared = map inputName (genInputs adapters)
          slots = fromMaybe [] (genAppliesAt adapters >>= \ap -> matchPathSlots (apPath ap) protoRel)
          offered = slots ++ sets ++ values ++ [("signature", methodText (langTypes lang >>= tyMethod) m ps r)]
          inputs = [(k, v) | k <- nub declared, Just v <- [lookup k offered]]
      plan <- lift (planWith ports project {prWorkspace = ws'} planOptions {poFiller = Just filler, poProgress = progress, poCancel = ssCancel s} [Step adapters (M.fromList inputs)]) >>= either (throwE . describeGenerationError) pure
      unless (null (planConflicts plan)) (throwE (T.intercalate "; " (map describeConflict (planConflicts plan))))
      pure [(f, t) | (f, t) <- M.toList (planWrites plan), M.lookup f (planBefore plan) /= Just (Just t), f /= protoRel]
  befores <- lift (traverse (\(f, t) -> (\b -> (f, fromMaybe "" b, t)) <$> readText ws f) changed)
  pure (protoRel, (protoRel, protoText, protoAfter) : befores)
  where
    cat = prCatalog project
    ports = ssPorts s
    ws = overlayMany writes (prWorkspace project)
    load gid = case findEntry cat gid of
      Left err -> throwE (describeCatalogError err)
      Right entry -> lift (loadGenerator (pFiles ports) (pSyntax ports) cat entry) >>= either (throwE . describeCatalogError) pure

choiceWord :: ResumeChoice -> Text
choiceWord = \case
  ResumeContinue -> "continue"
  ResumeClimb -> "climb"
  ResumeStop -> "stop"
  ResumeReconjure -> "reconjure"
  ResumeAccept -> "accept"
  ResumeReject -> "reject"
  ResumeWire -> "wire"
  ResumePort -> "port"

-- | The steered path's means, when the project names a decider and a
-- ladder (the client's model, if any, in place of the first rung); Left
-- when a rung's filler cannot be made.
steeredFor :: ServerState -> Project -> IO (Either Text (Maybe Steered))
steeredFor s project = case (deciderUnder (ssCancel s) s project, mcLadder models) of
  (Just decider', r0 : rs) -> do
    let ladder = maybe r0 (\n -> r0 {rungName = n}) (scModel (ssConfig s)) : rs
    fillers <- traverse (rungFiller s project . rungName) ladder
    pure ((\fs -> Just (Steered decider' (zip ladder fs) (mcRounds models) (fromIntegral (mcBudget models)))) <$> sequence fillers)
  _ -> pure (Right Nothing)
  where
    models = catalogModel (prCatalog project)

-- | The hook after each red round: a spent grant pauses; else Jev is asked
-- (a failure to ask is `unsure`) and its answer followed. A cancelled
-- conjure asks nothing (the loop stops at its next turn). RECORD wraps a
-- climbed rung's filler as the first one is. A port answer, when a wire
-- generator applies (WIRES) and none ran yet, pauses the loop for the
-- wire (`cWire`).
steer :: ServerState -> Language -> ([Text] -> IO ()) -> Double -> IORef Counters -> Steered -> Bool -> (Maybe (Decider IO) -> (Text, Fake) -> IO (Maybe (Text, Text), [Text])) -> (Text -> Text -> IO (Maybe (Text, Text, Maybe (Text, Text)))) -> (ModelFiller IO -> ModelFiller IO) -> RoundState -> IO (Steer IO)
steer s lang stepLine begun counters sd wires derive wall record rs =
  cancelled (ssCancel s) >>= \case
    True -> pure Again
    False -> do
      c0 <- readIORef counters
      now <- getMonotonicTime
      let record0 = (roundRecord (failureOf lang) rs) {rrSteps = length (cSteps c0)}
          history0 = cHistory c0 ++ [record0]
          runs = cBefore c0 : [r | Just r <- map rrRun history0]
          flags = nubOn fst (concatMap (scanFlags lang (cTestText c0)) runs)
      logFlags stepLine (cFlagged c0) flags
      (derived, candidates) <- case flags of
        flagged : _ -> derive (Just (sdDecider sd)) flagged
        [] -> pure (Nothing, [])
      c <- readIORef counters
      let wireable = wires && null (cWired c) && isNothing (cExplanation c)
          view = roundView (RoundContext (cTestText c) (cHeader c) (cWired c) wireable (cHistory c) (cBefore c) (cPrefixes c) (testSourcesIn lang (Just (cTestRel c)) (cTestText c)) (failureOf lang) (langComment lang)) (cLast c) rs
          used = cRounds c + 1
          seconds = cSecondsBefore c + (now - begun)
          above = drop (cRung c + 1) (sdLadder sd)
          standing = Standing (sdRounds sd - used) (sdBudget sd - seconds) (fst <$> listToMaybe above) (cHint c) wireable
          failingNow = [t | Just r <- [rrRun record0], (t, _) <- trFailed r]
          failedEver = nub [t | r <- runs, (t, _) <- trFailed r]
          ports = maybe [] sfPorts (cService c)
          lacks = not (any (takesPortIn (langConjure lang) (cHeader c)) (maybe (map ptName ports) (pure . snd) derived))
      -- a check that refused the body for a method a port lacks
      missing <- case rrRun record0 of
        Nothing -> wall (rrAttempt record0) (maybe "" rhWhy (listToMaybe (rsHoles rs)))
        Just _ -> pure Nothing
      let facts0 = PolicyFacts flags failingNow failedEver lacks wireable (cWired c) derived (isRepeat history0) (isTrade (cBefore c) history0) (spent standing) ((\(q, m, _) -> (q, m)) <$> missing)
          withSignature = \case
            PolicyPause (CauseMethod q m _) -> PolicyPause (CauseMethod q m (missing >>= \(_, _, sig) -> sig))
            p -> p
      (next, decided) <- case withSignature (policy facts0) of
        PolicyWire p q -> pure (Left (p, q), Nothing)
        PolicyPause cause -> pure (Right (NextPause cause), Nothing)
        PolicyAsk -> do
          (answer, weights) <- weighOr (sdDecider sd) (conjureNextQuestion standing view)
          stepLine ["conjure-next", answerChoice answer, answerConfidence answer, tshow (rsRound rs), rsRung rs]
          pure (Right (nextFrom standing (rsRound rs) answer), Just (decidedOf answer weights))
      let history = cHistory c ++ [record0 {rrDecided = decided}]
          c' = c {cRounds = used, cSeconds = seconds, cLast = Just view, cState = Just rs, cHistory = history, cFlagged = flags, cCandidates = candidates}
          facts = diagnosisFacts lang c' (rsRung rs) (rungName . fst <$> listToMaybe above) (round seconds)
      case next of
        Left (p, q) -> Pause "wire" <$ writeIORef counters c' {cWire = Just (p, q)}
        Right (NextClimb _) | (r, f) : _ <- above -> Climb (record f) <$ writeIORef counters c' {cRung = cRung c + 1, cRungs = cRungs c ++ [rungName r]}
        Right (NextPause cause) -> do
          let suspects = [t | t <- suspectFirst flags (involved (cBefore c) history), isJust (fixAgain (failureOf lang) (cSteps c) history t)]
              sentences = diagnosis facts cause
          Pause (fromMaybe "" (listToMaybe sentences)) <$ writeIORef counters c' {cOffers = offersAt standing cause suspects candidates (rsRound rs), cCause = Just cause, cDiagnosis = sentences}
        _ -> Again <$ writeIORef counters c'

-- | The scan over a run: each failing test of RUN whose source in the test file's TEXT builds a
-- fake it never passes in.
scanFlags :: Language -> Text -> TestRun -> [(Text, Fake)]
scanFlags lang text run = [(t, f) | (t, _) <- trFailed run, Just f <- [testSourceIn lang (Just (fileOfId t)) (testName t) text >>= fakeIn (langConjure lang)]]
  where
    fileOfId = T.unpack . fst . T.breakOn "::"

-- | A test id's function name (less its module and any parameters).
testName :: Text -> Text
testName t = fst (T.breakOn "[" (snd (T.breakOnEnd "::" t)))

-- | Each flag not logged before (in SEEN), logged once.
logFlags :: ([Text] -> IO ()) -> [(Text, Fake)] -> [(Text, Fake)] -> IO ()
logFlags stepLine seen flags = forM_ [(t, f) | (t, f) <- flags, t `notElem` map fst seen] $ \(t, f) -> stepLine ["conjure-test", "flagged", t, fkVariable f, fkClass f]

nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn key = go []
  where
    go _ [] = []
    go seen (x : xs)
      | key x `elem` seen = go seen xs
      | otherwise = x : go (key x : seen) xs

-- | The header of each top-level owner (`[conjure.constructor] owner`: a class) with a
-- constructor in a module of LANG: the ports the class under a test takes.
classHeaders :: Language -> Text -> Text
classHeaders lang t = case langConjure lang >>= cjConstructor of
  Nothing -> ""
  Just ct -> T.intercalate "\n\n" [h | (i, l) <- zip [1 ..] (T.lines t), matchesRe (ctOwner ct) l, let h = ownerHeader lang t i, any (matchesRe (ctStart ct)) (T.lines h)]

-- | The diagnosis' facts from the counters: the rung and the one above, the seconds used.
diagnosisFacts :: Language -> Counters -> Text -> Maybe Text -> Int -> DiagnosisFacts
diagnosisFacts lang c rung next seconds =
  DiagnosisFacts
    { dfRung = rung
    , dfNext = next
    , dfPrefixes = cPrefixes c
    , dfMethod = cUnit c
    , dfClass = classOf lang (cHeader c)
    , dfHeader = cHeader c
    , dfTestText = cTestText c
    , dfBefore = cBefore c
    , dfHistory = cHistory c
    , dfFlagged = cFlagged c
    , dfWired = cWired c
    , dfCandidates = cCandidates c
    , dfRounds = cRounds c
    , dfSeconds = seconds
    , dfWireFailed = T.stripPrefix "wire failed: " =<< cExplanation c
    , dfFailure = Nothing
    , dfSources = testSourcesIn lang (Just (cTestRel c)) (cTestText c)
    , dfAssertion = langAssertion lang
    , dfConjure = langConjure lang
    , dfFailureShape = failureOf lang
    }

-- | The owner a header opens (its `[conjure.constructor] owner` group 1: a class), or none.
classOf :: Language -> Text -> Text
classOf lang header = case (T.lines header, langConjure lang >>= cjConstructor) of
  (l : _, Just ct) | Just (_ : n : _) <- regexGroups (ctOwner ct) (T.strip l) -> n
  _ -> ""

-- | Whether RE matches LINE (an invalid one matches nothing).
matchesRe :: Text -> Text -> Bool
matchesRe re l = maybe False (`matchTest` T.unpack l) (makeRegexM (T.unpack re) :: Maybe Regex)

-- | The groups of RE's match in LINE, the whole match first; none when it does not match.
regexGroups :: Text -> Text -> Maybe [Text]
regexGroups re l = do
  rx <- makeRegexM (T.unpack re) :: Maybe Regex
  AllTextSubmatches gs <- matchM rx (T.unpack l) :: Maybe (AllTextSubmatches [] String)
  pure (map T.pack gs)

-- | Jev's answer to Q with its distribution (a failure to ask is `unsure`; a cancel is rethrown).
weighOr :: Decider IO -> Question -> IO (Answer, [(Text, Double)])
weighOr d q =
  try (weigh d q) >>= \case
    Right a -> pure a
    Left e | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
    Left (_ :: SomeException) -> pure (Unsure, [])

-- | An answer as a round keeps it.
decidedOf :: Answer -> [(Text, Double)] -> Decided
decidedOf a weights = case a of
  Chosen o _ (Confidence d) -> Decided o (Just d) weights
  Chosen o _ _ -> Decided o Nothing weights
  _ -> Decided "unsure" Nothing weights

-- | An answer's choice and confidence, as the step log has them.
answerChoice :: Answer -> Text
answerChoice = \case
  Chosen o _ _ -> o
  _ -> "unsure"

answerConfidence :: Answer -> Text
answerConfidence = \case
  Chosen _ _ (Confidence d) -> T.pack (printf "%.2f" d)
  _ -> "-"

-- | The context key of a body on screen that is no stub.
rejectedBody :: FilePath
rejectedBody = "the body on screen, rejected by the person (write another)"

noModel :: Text
noModel = "no model is configured: set [model] in .olifant/config.toml to conjure"

-- | A function's conjure: the file with the span's lines one `block` hole
-- called `body` (at the span's first line's indentation), verified by its
-- test file run whole (`verify = "tests"`: every test there must pass);
-- the context: the test file, the failures with their tests' sources, the
-- passing tests to keep, and the body on screen when it is no stub
-- (rejected by the person).
functionConjure :: Language -> Text -> (Int, Int) -> Text -> FilePath -> Text -> TestRun -> Either Text (Template, [(Text, Verify [Template])], [(FilePath, Text)])
functionConjure lang text (l1, l2) unit testRel testText before = do
  let ls = T.splitOn "\n" text
      body = take (l2 - l1 + 1) (drop (l1 - 1) ls)
      short = snd . T.breakOnEnd "::"
      failed = trFailed before
      instruction = oneLine $ case (trBroken before, failed) of
        (Just why, _) -> "the tests could not run (" <> firstLine why <> "); write the body of " <> unit <> " its tests describe"
        (_, []) -> "the tests pass; write the body of " <> unit <> " they describe, keeping them passing"
        _ -> "make the failing tests pass (" <> T.intercalate ", " [short t <> ": " <> failureLineIn lang f | (t, f) <- failed] <> ") and keep the file's other tests passing; the tests and their messages are in the context; write the least code that does it"
      failing = T.intercalate "\n\n" [t <> "\n" <> fMessage f <> "\n" <> T.unlines (sourceOf (short t)) | (t, f) <- failed]
      sourceOf n = maybe [] T.lines (testSourceIn lang (Just testRel) (snd (T.breakOnEnd "::" (fst (T.breakOn "[" n)))) testText)
      context =
        [(testRel, testText)]
          ++ [("failing now (make these pass)", failing) | not (null failed)]
          ++ [("passing now (keep these passing)", T.intercalate "\n" (trPassed before)) | not (null (trPassed before))]
          ++ [(rejectedBody, T.intercalate "\n" (dedent body)) | not (isStubIn lang body)]
  template <- holeOver ls (l1, l2) instruction
  pure (template, [("body", VerifyTests)], context)

-- | A test's conjure: the test file with the span's lines one `block` hole
-- called `body`, worded from the test's name; verified red at its
-- assertion or passing (`VerifyRedAt`, passing allowed: an error before
-- the assertion is asked again); the context: the module under test in
-- full, the notes in the stub's body, and the body on screen when it is no
-- stub (rejected by the person). The siblings are in the file itself.
-- TID is the test's full id (`path::name`).
testConjure :: Language -> Text -> (Int, Int) -> Text -> FilePath -> Text -> Either Text (Template, [(Text, Verify [Template])], [(FilePath, Text)])
testConjure lang text (l1, l2) tid moduleRel moduleText = do
  let ls = T.splitOn "\n" text
      body = take (l2 - l1 + 1) (drop (l1 - 1) ls)
      short = snd (T.breakOnEnd "::" tid)
      words' = testWordsIn lang short
      notes = [T.strip l | l <- body, langComment lang `T.isPrefixOf` T.strip l]
      instruction = advisedIn (maybe [] cjAdvice (langConjure lang)) ls (oneLine ("write the body of the test \"" <> words' <> "\": arrange, act, and assert what its name says, one behaviour, against the module in the context; it should fail at its assertion until the code does it"))
      context =
        [(moduleRel, moduleText)]
          ++ [("notes in the test's body", T.intercalate "\n" notes) | not (null notes)]
          ++ [(rejectedBody, T.intercalate "\n" (dedent body)) | not (isStubIn lang body)]
  template <- holeOver ls (l1, l2) instruction
  pure (template, [("body", VerifyRedAt tid l1 True)], context)

-- | A test's after-run in a few words: its own outcome (at its assertion,
-- before it, passing, not run), then each sibling that passed before and
-- fails now; then the names EXTRA (left undefined by the last attempt) that
-- it does not already name.
testSummary :: Language -> [Text] -> Text -> TestRun -> TestRun -> Text -> Text
testSummary lang extra tid before after written = base <> (if null missing then "" else " \183 undefined: " <> T.intercalate ", " missing)
  where
    base = own <> T.concat ["; red: " <> short t <> " no longer passes" | t <- broke]
    missing = [n | n <- nub extra, not (n `T.isInfixOf` base)]
    short = snd . T.breakOnEnd "::"
    isOwn t = t == tid || (tid <> "[") `T.isPrefixOf` t
    broke = [t | (t, _) <- trFailed after, not (isOwn t), t `elem` trPassed before]
    own = case (trBroken after, [f | (t, f) <- trFailed after, isOwn t]) of
      (Just why, _) -> "red: could not run \183 " <> firstLine why
      (_, f : _)
        | Just l <- fLine f, Just (a, z) <- assertionSpan lang (Just (T.unpack (fst (T.breakOn "::" tid)))) (short tid) written, l >= a, l <= z -> "red at the assertion \183 " <> failureLineIn lang f
        | otherwise -> "red: error before the assertion \183 " <> failureLineIn lang f
      _
        | any isOwn (trPassed after) -> "green: already passes"
        | otherwise -> "red: " <> short tid <> " did not run"

-- | LS with the lines L1..L2 one `block` hole called `body`, at L1's
-- indentation, worded INSTRUCTION.
holeOver :: [Text] -> (Int, Int) -> Text -> Either Text Template
holeOver ls (l1, l2) instruction =
  either (Left . describeTemplateError) Right (parseTemplate (T.intercalate "\n" (take (l1 - 1) ls ++ [indent <> "{= ? body: block | " <> instruction <> " =}"] ++ drop l2 ls)))
  where
    indent = T.takeWhile (`elem` [' ', '\t']) (maybe "" id (listToMaybe (drop (l1 - 1) ls)))

-- | Lines less their common indentation.
dedent :: [Text] -> [Text]
dedent ls = map (T.drop n) ls
  where
    n = minimum (maxBound : [T.length (T.takeWhile (== ' ') l) | l <- ls, not (T.null (T.strip l))])

firstLine :: Text -> Text
firstLine = T.strip . T.takeWhile (/= '\n')

-- | One line, no marker's end in it (an instruction).
oneLine :: Text -> Text
oneLine = T.replace "=}" "= }" . T.intercalate "; " . filter (not . T.null) . map T.strip . T.lines

-- | PROGRESS with the events WORDS names said in its words.
worded :: (ProgressEvent -> Maybe Text) -> (ProgressEvent -> IO ()) -> ProgressEvent -> IO ()
worded words' progress e = progress (maybe e Said (words' e))

-- | PROGRESS for a steered loop of TOTAL rounds: each event in 'roundWords', the round
-- being run kept from its last fill.
roundsProgress :: Int -> (Text -> Text) -> (ProgressEvent -> IO ()) -> IO (ProgressEvent -> IO ())
roundsProgress total modelOf progress = do
  current <- newIORef 0
  pure $ \e -> do
    case e of
      Filling _ r _ -> writeIORef current r
      _ -> pure ()
    r <- readIORef current
    worded (roundWords total modelOf r) progress e

-- | A function's after-run in a few words: a name left undefined (by a
-- failure's message, or EXTRA: the last attempt's check) is named first,
-- by the label of the runner's `undefined` entry that read it (else
-- @undefined:@).
functionSummary :: Language -> [Text] -> TestRun -> Text
functionSummary lang extra run = case (trBroken run, trFailed run) of
  (Just why, _) -> "red: could not run \183 " <> firstLine why
  (_, failed) | names@(_ : _) <- nub (map fst found ++ extra) -> (if null failed then "green \183 " else "red: ") <> label <> " " <> T.intercalate ", " names
    where
      found = concatMap (undefinedIn lang . fMessage . snd) failed
      label = undefinedLabelIn lang found
  (_, []) -> "green"
  (_, (t, f) : _) -> "red: " <> snd (T.breakOnEnd "::" t) <> " \183 " <> failureLineIn lang f

-- | TEMPLATE planned over REL (TEXT) with VERIFY and CONTEXT: the file as
-- the fill writes it, or, when no attempt passed its verify, as the last
-- attempt writes it (a conjure always lands), with the names that attempt
-- left undefined; or, steered (STEERING, given how to wrap a climbed
-- rung's filler), a pause. Left when the model gave no attempt at all, or
-- the plan could not be made. ROUNDS keeps the highest round asked.
fill :: [FilePath] -> (ProgressEvent -> IO ()) -> IORef Int -> ServerState -> Project -> FilePath -> Text -> Template -> [(Text, Verify [Template])] -> [(FilePath, Text)] -> ModelFiller IO -> ((ModelFiller IO -> ModelFiller IO) -> Maybe (Steering IO)) -> ExceptT Text IO Written
fill judged progress rounds s project rel text template verify context filler steering = do
  attempts <- lift (newIORef M.empty)
  prior <- lift (newIORef M.empty)
  let record f = f {mfFiller = Filler (\req -> runFill (mfFiller f) req >>= \r -> either (const (pure ())) answered r >> pure r)}
      answered fills = readIORef attempts >>= writeIORef prior >> modifyIORef' attempts (M.union fills)
      progress' e = case e of
        Filling _ r _ -> modifyIORef' rounds (max r) >> progress e
        Reindented hole f -> modifyIORef' attempts (M.insert hole f) >> progress e
        CutToHole hole _ f -> modifyIORef' attempts (M.insert hole f) >> progress e
        -- a whole file answered is never written: the hole's earlier attempt is
        WholeFile hole -> readIORef prior >>= \was -> modifyIORef' attempts (M.alter (const (M.lookup hole was)) hole) >> progress e
        _ -> progress e
      gen = (replaceGenerator rel (1, length (T.lines text)) text template) {genVerify = verify}
      options = planOptions {poFiller = Just (record filler), poProgress = progress', poCancel = ssCancel s, poContext = context, poStance = ConjureStance, poSteering = steering record, poJudged = judged}
  plan <- withExceptT describeGenerationError (ExceptT (planWith (ssPorts s) project options [Step gen M.empty]))
  unless (null (planConflicts plan)) (throwE (T.intercalate "; " (map describeConflict (planConflicts plan))))
  tried <- lift (readIORef attempts)
  case planHoles plan of
    [] -> throwE "the conjure made no hole"
    hs
      | all (\h -> case phStatus h of HoleFilled _ _ -> True; _ -> False) hs -> pure (Written Nothing (M.findWithDefault text rel (planWrites plan)) [])
      | why : _ <- [w | h <- hs, HoleSkipped w <- [phStatus h]] -> throwE why
      | otherwise -> do
          let red = [(h, w) | h <- hs, Just w <- [redWhy (phStatus h)]]
              redWhy = \case
                HoleUnfilled w -> Just w
                HolePaused w -> Just w
                _ -> Nothing
              pausedWhy = [w | h <- hs, HolePaused w <- [phStatus h]]
              why = maybe "" snd (listToMaybe red)
              offered = [(phSite h, a) | (h, _) <- red, Just a <- [M.lookup (holeName (phSite h)) tried]]
              langOf = languageFor (catalogLanguages (prCatalog project))
              landing = M.findWithDefault text rel (applyFills langOf (planWrites plan) offered)
          case pausedWhy of
            -- every red hole paused: the last attempt (if any) is what stop lands
            w : _ | length pausedWhy == length red -> pure (Halted landing w)
            _ -> do
              when (null offered) (throwE ("the model gave no conjure: " <> why))
              pure (Written (Just why) landing (maybe [] (`undefinedNamesIn` why) (langOf rel)))
