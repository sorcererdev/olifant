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
  , Proposal (..)
  , ConjureOutcome (..)
  , ResumeChoice (..)
  , Resume (..)
  , conjureAt
  , resumeConjure
  ) where

import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Control.Monad (forM_, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE, withExceptT)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Functor ((<&>))
import Data.List (isSuffixOf, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import Olifant.App (PlanOptions (..), Ports (..), Project (..), planOptions, planWith)
import Olifant.Actions (matchPathSlots)
import Olifant.Catalog (catalogConjureWire, catalogLanguages, catalogModel, describeCatalogError, findEntry, loadGenerator)
import Olifant.Decide (Answer (..), Confidence (..), Decider (..), Question)
import Olifant.Fill (Filler (..), ModelFiller (..), Stance (..))
import Olifant.Generation (HoleStatus (..), Plan (..), PlannedHole (..), ProgressEvent (..), Step (..), describeConflict, describeGenerationError)
import Olifant.Generation.Steer (RoundHole (..), RoundState (..), Seed (..), Steer (..), Steering (..), noSeed)
import Olifant.TestName (helpedIn)
import Olifant.Generation.FillPhase (applyFills, assertionSpan, projectFiles)
import Olifant.Generator (AppliesAt (..), GeneratorWith (..), Verify (..), replaceGenerator)
import Olifant.Holes (HoleSite (..))
import Olifant.Language (Failure (..), TestRun (..), languageFor, runTestsIn)
import Olifant.Model (ModelConfig (..), Rung (..))
import Olifant.Process (cancelled)
import Olifant.Server.Actions (fillerFor, rungFiller)
import Olifant.Server.ConjureNext (Next (..), Offer (..), PausedAnswer (..), PausedConjure (..), Proposal (..), RoundContext (..), RoundView, Standing (..), TestCheck (..), conjureNextQuestion, conjureTestQuestion, testCheckFrom, nextFrom, offerText, offersFor, pausedLifetime, pruned, roundSummary, roundView, spent, undefinedNames, withHint)
import Olifant.Server.ConjureNext qualified as ConjureNext (testSource)
import Olifant.Server.Site (Site (..), appendStepLine, importedContext, runOver, siteAt, siteTestFile, testBody, tshow)
import Olifant.Server.Edits (FileEdit (..), smallestChange, unifiedDiff)
import Olifant.Server.State (ServerConfig (..), ServerState (..), deciderUnder)
import Olifant.Server.Tests (TestsAnswer (..), TestsScope (..), testAt, testLineOf)
import Olifant.Server.Unit (Unit (..), unitAt, unitHeader)
import Olifant.Template (Template, describeTemplateError, parseTemplate)
import Olifant.Workspace (Workspace (..), normalisePath, overlayMany)
import System.FilePath ((</>))
import Text.Printf (printf)

-- | A conjure's answer: the conjured file (absolute), the unit, the edits
-- (the unit's file first, when the conjure changed it, then every other
-- file a wire wrote), the summary, whether the after-run is green, the
-- after-run, the rounds run in all, and the models that ran them (a
-- steered conjure's rungs, in the order climbed).
data ConjureAnswer = ConjureAnswer
  { daFile :: FilePath
  , daUnit :: Text
  , daEdits :: [FileEdit]
  , daSummary :: Text
  , daGreen :: Bool
  , daTests :: TestsAnswer
  , daRounds :: Int
  , daRungs :: [Text]
  }
  deriving stock (Eq, Show)

data ConjureOutcome = Landed ConjureAnswer | Paused PausedAnswer
  deriving stock (Eq, Show)

data ResumeChoice = ResumeContinue | ResumeClimb | ResumeStop | ResumeReconjure | ResumeAccept | ResumeReject
  deriving stock (Eq, Show)

-- | The person's answer to a pause: its token, the choice, and words to add
-- to the next prompt (never logged).
data Resume = Resume {reToken :: Text, reChoice :: ResumeChoice, reHint :: Maybe Text}
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
-- pause; and the working test file and the unit's header (Jev's context).
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
  , cHeader :: Text
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
      ResumeReconjure | isNothing (reconjureOf pd) -> Just "this pause offers no re-conjure"
      c | c `elem` [ResumeAccept, ResumeReject], isNothing (pdProposal pd) -> Just "there is no proposal to accept"
      _ -> Nothing
    -- accept: the proposed test file joins the working writes; reject keeps
    -- the proposal (to answer the pause before it); any other answer drops it
    answered pd = case (reChoice re, pdProposal pd) of
      (ResumeAccept, Just p) -> pd {pdWrites = M.insert (prFile p) (prAfter p) (pdWrites pd), pdProposal = Nothing, pdPrevious = Nothing}
      (ResumeReject, _) -> pd
      _ -> withoutProposal pd

-- | The test a pause offers to re-conjure, by its id.
reconjureOf :: PausedConjure -> Maybe Text
reconjureOf pd = listToMaybe [t | o <- paOffers (pdShown pd), Just t <- [T.stripPrefix "reconjure:" o]]

-- | PD less its proposal, and less the proposed file's recorded version and
-- text when the conjure has not written that file.
withoutProposal :: PausedConjure -> PausedConjure
withoutProposal pd = case pdProposal pd of
  Just p
    | M.notMember (prFile p) (pdWrites pd) ->
        let others :: Map FilePath v -> Map FilePath v
            others = M.filterWithKey (\k _ -> not (("/" <> prFile p) `isSuffixOf` k))
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
          onTest = siteOnTest site
          rel = if onTest then siteTestRel site else siteModuleRel site
          testRel = siteTestRel site
          file = normalisePath (siteRoot site </> rel)
          absOf r = normalisePath (siteRoot site </> r)
          started = fst <$> M.lookup file (ssDocs s)
          stepLine = appendStepLine (ssPorts s) (siteRoot site)
      text <- if onTest then maybe (throwE ("cannot read " <> T.pack rel)) pure (siteTestText site) else pure (siteModuleText site)
      found <- lift (unitAt (pSyntax (ssPorts s)) lang text line)
      (unit, span') <- case (selected, found) of
        (Just sp, _) -> pure (either (const "selection") unitName found, sp)
        (Nothing, Left why) -> throwE (why <> ": put the cursor in one to conjure it")
        (Nothing, Right u) -> maybe (throwE (unitName u <> " is on one line: select the lines to conjure")) (pure . (unitName u,)) (unitBody u)
      when (fst span' < 1 || snd span' < fst span' || snd span' > length (T.splitOn "\n" text)) (throwE "the selection is not in the file")
      when (onTest && isNothing selected && not ("test" `T.isPrefixOf` unit)) (throwE (unit <> " is no test: conjure a test here, or a function from its module"))
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
        lift (stepLine ["conjure-resume", choiceWord (reChoice re)])
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
      wire <- lift (if onTest || isJust selected || isNothing steered then pure Nothing else wireFor s project (siteModuleRel site))
      let -- the unit again by name in its file as the working writes have it (a wire moves its lines down)
          relocate working = case found of
            Right u | isNothing selected -> do
              let defLine = fromMaybe "" (listToMaybe (drop (unitDef u - 1) (T.splitOn "\n" text)))
                  at = listToMaybe [i | (i, l) <- zip [1 ..] (T.splitOn "\n" working), i >= unitDef u, l == defLine]
              i <- maybe (throwE (unit <> " is no longer in " <> T.pack rel)) pure at
              u' <- lift (unitAt (pSyntax (ssPorts s)) lang working i) >>= either throwE pure
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
                  name <- maybe (throwE ("no test at line " <> tshow line)) pure (testAt working (fst at))
                  except (testConjure working at (T.pack testRel <> "::" <> name) (siteModuleRel site) moduleText)
                else except (functionConjure working at unit testRel testText before)
            imported <- lift (importedContext ws' lang (rel : map fst context) (working : map snd context))
            pure (Working project {prWorkspace = ws'} working at testText before template verify (context ++ imported))
          writes0 = maybe M.empty (pdWrites . snd) resuming
      case resuming of
        -- reject: the pause the proposal came from, under the same token
        Just (re, pd) | reChoice re == ResumeReject, Just prev <- pdPrevious pd -> do
          now <- lift getMonotonicTime
          lift (keep s (reToken re) (withoutProposal pd) {pdShown = prev, pdAt = now})
          pure (Paused prev)
        -- reconjure: the wrong test conjured again over the working files, proposed
        Just (re, pd) | reChoice re == ResumeReconjure, Just tid <- reconjureOf pd -> do
          wk <- prepare writes0
          let name = snd (T.breakOnEnd "::" tid)
              working = wkTestText wk
              moduleText = M.findWithDefault (siteModuleText site) (siteModuleRel site) writes0
          attempt <- lift . runExceptT $ do
            when (T.unpack (fst (T.breakOn "::" tid)) /= testRel) (throwE (tid <> " is not in " <> T.pack testRel))
            l <- maybe (throwE ("no test " <> name <> " in " <> T.pack testRel)) pure (testLineOf working name)
            u <- lift (unitAt (pSyntax (ssPorts s)) lang working l) >>= either throwE pure
            at <- maybe (throwE (name <> " is on one line")) pure (unitBody u)
            (template, verify, context) <- except (testConjure working at tid (siteModuleRel site) moduleText)
            let context' =
                  context
                    ++ [("the unit under test", unitHeader (wkText wk) (fst (wkSpan wk))), ("why the test is wrong", paWhy (pdShown pd))]
                    ++ [("the person says", h) | Just h <- [reHint re]]
                ws' = prWorkspace (wkProject wk)
            imported <- lift (importedContext ws' lang (testRel : map fst context') (working : map snd context'))
            rounds' <- lift (newIORef 0)
            fill progress rounds' s (wkProject wk) testRel working template verify (context' ++ imported) filler (const Nothing)
          stop <- lift (cancelled (ssCancel s))
          when stop (throwE "cancelled")
          now <- lift getMonotonicTime
          case attempt of
            Right (Written Nothing after _) | after /= testText0 -> do
              let proposal = Proposal testRel tid testText0 after (unifiedDiff testRel testText0 after)
                  shown = (pdShown pd) {paOffers = map offerText [OfferAccept, OfferReject], paProposal = Just proposal}
                  testAbs = absOf testRel
                  -- the test file as the conjure first read it (a wire's reading stands)
                  pd' =
                    pd
                      { pdProposal = Just proposal
                      , pdPrevious = Just (pdShown pd)
                      , pdShown = shown
                      , pdAt = now
                      , pdBases = M.insertWith (\_ old -> old) testAbs (Just testText0) (pdBases pd)
                      , pdVersions = M.insertWith (\_ old -> old) testAbs (fst <$> M.lookup testAbs (ssDocs s)) (pdVersions pd)
                      }
              lift (keep s (reToken re) pd')
              lift (stepLine ["conjure-proposal", tid])
              pure (Paused shown)
            _ -> do
              let why = case attempt of
                    Left w -> w
                    Right (Written (Just w) _ _) -> w
                    Right (Halted _ w) -> w
                    Right (Written Nothing _ _) -> "it wrote the test as it is"
                  shown = (pdShown pd) {paExplanation = "the re-conjure failed: " <> why}
              lift (keep s (reToken re) pd {pdShown = shown, pdAt = now})
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
                    , cHeader = unitHeader (wkText first) (fst (wkSpan first))
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
                          }
          rounds <- lift (newIORef (maybe 0 (seedRound . pdSeed . snd) resuming))
          let seed0 = case resuming of
                Just (re, pd) -> maybe id withHint (reHint re) (pdSeed pd)
                Nothing -> noSeed
              steering seed record = (\sd -> Steering seed (steer s stepLine begun counters sd (isJust wire) record)) <$> steered
              -- the fill, and, while it pauses for a wire, the wire and the fill again from the round it paused at
              loop wk f seed = do
                tried <- lift (runExceptT (fill progress rounds s (wkProject wk) rel (wkText wk) (wkTemplate wk) (wkVerify wk) (wkContext wk) f (steering seed)))
                c <- lift (readIORef counters)
                stop <- lift (cancelled (ssCancel s))
                case (tried, cWire c, wire, steered, cState c) of
                  (Right (Halted _ _), Just (p, q), Just (gid, pathSets), Just sd, Just rs) | not stop -> do
                    let sets = [("param", p), ("port", q)] ++ pathSets
                        seed' = maybe id withHint (cHint c) (Seed (rsRound rs) (M.fromList [(rhName h, rhPrevious h) | h <- rsHoles rs]))
                        f' = maybe f snd (listToMaybe (drop (cRung c) (sdLadder sd)))
                    planned <- lift (wirePlan s project (cWrites c) gid sets)
                    ready <- case planned of
                      Left why -> pure (Left why)
                      Right new -> fmap (new,) <$> lift (runExceptT (prepare (M.union new (cWrites c))))
                    case ready of
                      Right (new, wk') -> do
                        -- each file the wire wrote, as the conjure first read it
                        read' <- lift (traverse (\r -> (absOf r,) <$> readText ws r) [r | r <- M.keys new, r /= rel, M.notMember (absOf r) (cBases c)])
                        lift . writeIORef counters $
                          c
                            { cWire = Nothing
                            , cWired = cWired c ++ [p <> ": " <> q]
                            , cWrites = M.union new (cWrites c)
                            , cVersions = M.union (cVersions c) (M.fromList [(a, fst <$> M.lookup a (ssDocs s)) | (a, _) <- read'])
                            , cBases = M.union (cBases c) (M.fromList read')
                            , cTestText = wkTestText wk'
                            , cHeader = unitHeader (wkText wk') (fst (wkSpan wk'))
                            }
                        lift (stepLine ("conjure-wire" : gid : [k <> "=" <> v | (k, v) <- sets]))
                        loop wk' f' seed'
                      Left why -> do
                        lift (writeIORef counters c {cWire = Nothing, cExplanation = Just ("wire failed: " <> why)})
                        lift (stepLine ["conjure-wire-failed", gid])
                        loop wk f' seed'
                  _ -> pure (tried, wk)
          (tried, wk0) <- case resuming of
            Just (_, pd) | stopping -> pure (Right (Written Nothing (pdLanding pd) (pdUndefined pd)), first)
            _ -> loop first filler seed0
          -- a cancelled conjure lands nothing and keeps no pause
          stop <- lift (cancelled (ssCancel s))
          when stop (throwE "cancelled")
          outcome0 <- except tried
          -- a test conjure whose fill passed its verify: Jev checks it builds
          -- no port the class lacks; a port is wired (when a generator
          -- applies) and the test filled once more over the wired files
          let refill p q wk' = do
                c <- lift (readIORef counters)
                let f' = fromMaybe filler (steered >>= \sd -> snd <$> listToMaybe (drop (cRung c) (sdLadder sd)))
                    relabel (k, v) = (if k == rejectedBody then T.unpack ("the test as first written; the helper now takes " <> p <> ": " <> q <> ": pass the test's fake to it") else k, v)
                fill progress rounds s (wkProject wk') rel (wkText wk') (wkTemplate wk') (wkVerify wk') (map relabel (wkContext wk')) f' (const Nothing)
          (outcome, wk, note) <- case (outcome0, deciderUnder (ssCancel s) s project) of
            (Written Nothing written0 _, Just decider')
              | onTest && isNothing selected && not stopping -> do
                  c <- lift (readIORef counters)
                  let name = fromMaybe unit (testAt written0 (fst (wkSpan wk0)))
                      moduleText = M.findWithDefault (siteModuleText site) (siteModuleRel site) (cWrites c)
                  answer <- lift (askOr decider' (conjureTestQuestion (fromMaybe "" (ConjureNext.testSource name written0)) (classHeaders moduleText)))
                  lift (stepLine ["conjure-test", answerChoice answer, answerConfidence answer])
                  case testCheckFrom answer of
                    TestOk -> pure (outcome0, wk0, "")
                    TestPort p q -> do
                      let lacks = " \183 needs a port the class lacks: " <> p <> ": " <> q
                      wire' <- lift (if null (cWired c) then wireFor s project (siteModuleRel site) else pure Nothing)
                      case wire' of
                        Nothing -> pure (outcome0, wk0, lacks)
                        Just (gid, pathSets) -> do
                          let sets = [("param", p), ("port", q)] ++ pathSets
                              filled = M.insert rel written0 (cWrites c)
                          planned <- lift (wirePlan s project filled gid sets)
                          ready <- case planned of
                            Left why -> pure (Left why)
                            Right new -> fmap (new,) <$> lift (runExceptT (prepare (M.union new filled)))
                          case ready of
                            Left _ -> do
                              lift (stepLine ["conjure-wire-failed", gid])
                              pure (outcome0, wk0, lacks)
                            Right (new, wk') -> do
                              -- each file the wire wrote, as the conjure first read it (the test file's is the editor's)
                              read' <- lift (traverse (\r -> (absOf r,) <$> readText ws r) [r | r <- M.keys new, r /= rel, M.notMember (absOf r) (cBases c)])
                              lift . writeIORef counters $
                                c
                                  { cWired = cWired c ++ [p <> ": " <> q]
                                  , cWrites = M.union new filled
                                  , cVersions = M.union (cVersions c) (M.fromList [(a, fst <$> M.lookup a (ssDocs s)) | (a, _) <- read'])
                                  , cBases = M.union (cBases c) (M.fromList read')
                                  }
                              lift (stepLine ("conjure-wire" : gid : [k <> "=" <> v | (k, v) <- sets]))
                              refilled <- lift (runExceptT (refill p q wk'))
                              cancelled' <- lift (cancelled (ssCancel s))
                              when cancelled' (throwE "cancelled")
                              case refilled of
                                Right o@(Written Nothing _ _) -> pure (o, wk', "")
                                -- the wire and the test as first written land together
                                _ -> pure (Written Nothing (wkText wk') [], wk', "; the test still builds its own fake, re-conjure it")
            _ -> pure (outcome0, wk0, "")
          latest <- lift (maybe (pure started) (\live -> fmap fst <$> live file) (ssLiveDoc s))
          when (latest /= started) (throwE "the file changed while conjuring; your text stands")
          c <- lift (readIORef counters)
          let others = [(r, absOf r, new) | (r, new) <- M.toList (cWrites c), r /= rel]
          case outcome of
            Halted landing why -> do
              (view, rs) <- maybe (throwE "the conjure paused before any round") pure ((,) <$> cLast c <*> cState c)
              now <- lift getMonotonicTime
              let lastWhy = maybe "" rhWhy (listToMaybe (rsHoles rs))
                  seed' = Seed (rsRound rs) (M.fromList [(rhName h, rhPrevious h) | h <- rsHoles rs])
                  explanation = maybe why (<> "; " <> why) (cExplanation c)
                  failing = [(t, fMessage f) | h <- take 1 (rsHoles rs), Just r <- [rhRun h], (t, f) <- trFailed r]
                  shownAs t = PausedAnswer t file unit span' (rsRound rs) (rsRung rs) (roundSummary view) (map offerText (cOffers c)) why explanation failing (cWired c) Nothing
                  pd = PausedConjure file started line selected seed' (cRung c) (cRungs c) (cRounds c) (cSeconds c) view (cHint c) landing (undefinedNames lastWhy) now (cWrites c) (M.delete file (cVersions c)) (cBases c) (cWired c) explanation Nothing Nothing
              token <- lift (atomicModifyIORef' (ssPaused s) (\(n, m) -> let t = "conjure-" <> tshow (n + 1) in ((n + 1, M.insert t (pd (shownAs t)) m), t)))
              lift (stepLine ["conjure-pause", why])
              pure (Paused (shownAs token))
            Written _ written extra -> do
              -- every other file the conjure wrote stands as it read it
              lift (movedSince s (cVersions c) (cBases c) [(r, a) | (r, a, _) <- others]) >>= maybe (pure ()) (throwE . changedWhile)
              -- the after-run: the test file with the conjure in place, over the working files
              (_, files) <- lift (projectFiles (prWorkspace (wkProject wk)) (M.empty :: Map FilePath Text) lang [testRel])
              start <- lift getMonotonicTime
              after <- lift (either (TestRun [] [] . Just) id <$> runTestsIn (pTester (ssPorts s) (ssCancel s)) lang [T.pack testRel] (M.insert rel written files))
              end <- lift getMonotonicTime
              n <- lift (readIORef rounds)
              let summary = wiredNote (if onTest then testSummary extra tid (wkBefore wk) after written else functionSummary extra after) <> note
                  wiredNote t = if null (cWired c) then t else t <> " \183 wired " <> T.intercalate ", " (cWired c)
                  tid = T.pack testRel <> "::" <> fromMaybe unit (testAt written (fst (wkSpan wk)))
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
                  tests = TestsAnswer (siteTestFile site) ScopeFile Nothing (siteRoot site) after (end - start)
                  isSteered = isJust steered || isJust resuming
                  rungs = if isSteered then cRungs c else [mfName filler]
                  by
                    | isSteered = T.intercalate " \8594 " rungs <> " \183 " <> tshow n <> (if n == 1 then " round" else " rounds")
                    | otherwise = mfName filler
              lift (stepLine ["conjure", "person", T.pack rel <> "::" <> unit, "conjured by " <> by <> " \183 " <> summary])
              pure (Landed (ConjureAnswer file unit (edit ++ edits) summary green tests n rungs))

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

choiceWord :: ResumeChoice -> Text
choiceWord = \case
  ResumeContinue -> "continue"
  ResumeClimb -> "climb"
  ResumeStop -> "stop"
  ResumeReconjure -> "reconjure"
  ResumeAccept -> "accept"
  ResumeReject -> "reject"

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
steer :: ServerState -> ([Text] -> IO ()) -> Double -> IORef Counters -> Steered -> Bool -> (ModelFiller IO -> ModelFiller IO) -> RoundState -> IO (Steer IO)
steer s stepLine begun counters sd wires record rs =
  cancelled (ssCancel s) >>= \case
    True -> pure Again
    False -> do
      c <- readIORef counters
      now <- getMonotonicTime
      let wireable = wires && null (cWired c)
          view = roundView (RoundContext (cTestText c) (cHeader c) (cWired c) wireable) (cLast c) rs
          used = cRounds c + 1
          seconds = cSecondsBefore c + (now - begun)
          above = drop (cRung c + 1) (sdLadder sd)
          standing = Standing (sdRounds sd - used) (sdBudget sd - seconds) (fst <$> listToMaybe above) (cHint c) wireable
      next <- case spent standing of
        Just n -> pure n
        Nothing -> do
          answer <- askOr (sdDecider sd) (conjureNextQuestion standing view)
          stepLine ["conjure-next", answerChoice answer, answerConfidence answer, tshow (rsRound rs), rsRung rs]
          pure (nextFrom standing view answer)
      let c' = c {cRounds = used, cSeconds = seconds, cLast = Just view, cState = Just rs}
      case next of
        NextClimb _ | (r, f) : _ <- above -> Climb (record f) <$ writeIORef counters c' {cRung = cRung c + 1, cRungs = cRungs c ++ [rungName r]}
        NextPause why offers -> Pause why <$ writeIORef counters c' {cOffers = offers}
        NextWire p q -> Pause "wire" <$ writeIORef counters c' {cWire = Just (p, q), cOffers = offersFor standing}
        _ -> Again <$ writeIORef counters c'

-- | Jev's answer to Q (a failure to ask is `unsure`; a cancel is rethrown).
askOr :: Decider IO -> Question -> IO Answer
askOr d q =
  try (ask d q) >>= \case
    Right a -> pure a
    Left e | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
    Left (_ :: SomeException) -> pure Unsure

-- | An answer's choice and confidence, as the step log has them.
answerChoice :: Answer -> Text
answerChoice = \case
  Chosen o _ _ -> o
  _ -> "unsure"

answerConfidence :: Answer -> Text
answerConfidence = \case
  Chosen _ _ (Confidence d) -> T.pack (printf "%.2f" d)
  _ -> "-"

-- | The header of each top-level class with an @__init__@ in a module: the
-- ports the class under a test takes.
classHeaders :: Text -> Text
classHeaders t = T.intercalate "\n\n" [h | (i, l) <- zip [1 ..] (T.lines t), "class " `T.isPrefixOf` l, let h = unitHeader t i, "__init__" `T.isInfixOf` h]

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
functionConjure :: Text -> (Int, Int) -> Text -> FilePath -> Text -> TestRun -> Either Text (Template, [(Text, Verify [Template])], [(FilePath, Text)])
functionConjure text (l1, l2) unit testRel testText before = do
  let ls = T.splitOn "\n" text
      body = take (l2 - l1 + 1) (drop (l1 - 1) ls)
      short = snd . T.breakOnEnd "::"
      failed = trFailed before
      instruction = oneLine $ case (trBroken before, failed) of
        (Just why, _) -> "the tests could not run (" <> firstLine why <> "); write the body of " <> unit <> " its tests describe"
        (_, []) -> "the tests pass; write the body of " <> unit <> " they describe, keeping them passing"
        _ -> "make the failing tests pass (" <> T.intercalate ", " [short t <> ": " <> firstLine (fMessage f) | (t, f) <- failed] <> ") and keep the file's other tests passing; the tests and their messages are in the context; write the least code that does it"
      failing = T.intercalate "\n\n" [t <> "\n" <> fMessage f <> "\n" <> T.unlines (testSource (short t)) | (t, f) <- failed]
      testSource n = let base = fst (T.breakOn "[" n) in ("def " <> base <> "(…):") : testBody base testText
      context =
        [(testRel, testText)]
          ++ [("failing now (make these pass)", failing) | not (null failed)]
          ++ [("passing now (keep these passing)", T.intercalate "\n" (trPassed before)) | not (null (trPassed before))]
          ++ [(rejectedBody, T.intercalate "\n" (dedent body)) | not (isStub body)]
  template <- holeOver ls (l1, l2) instruction
  pure (template, [("body", VerifyTests)], context)

-- | A test's conjure: the test file with the span's lines one `block` hole
-- called `body`, worded from the test's name; verified red at its
-- assertion or passing (`VerifyRedAt`, passing allowed: an error before
-- the assertion is asked again); the context: the module under test in
-- full, the notes in the stub's body, and the body on screen when it is no
-- stub (rejected by the person). The siblings are in the file itself.
-- TID is the test's full id (`path::name`).
testConjure :: Text -> (Int, Int) -> Text -> FilePath -> Text -> Either Text (Template, [(Text, Verify [Template])], [(FilePath, Text)])
testConjure text (l1, l2) tid moduleRel moduleText = do
  let ls = T.splitOn "\n" text
      body = take (l2 - l1 + 1) (drop (l1 - 1) ls)
      short = snd (T.breakOnEnd "::" tid)
      words' = T.replace "_" " " (fromMaybe short (T.stripPrefix "test_" short))
      notes = [T.strip l | l <- body, "#" `T.isPrefixOf` T.strip l]
      instruction = helpedIn ls (oneLine ("write the body of the test \"" <> words' <> "\": arrange, act, and assert what its name says, one behaviour, against the module in the context; it should fail at its assertion until the code does it"))
      context =
        [(moduleRel, moduleText)]
          ++ [("notes in the test's body", T.intercalate "\n" notes) | not (null notes)]
          ++ [(rejectedBody, T.intercalate "\n" (dedent body)) | not (isStub body)]
  template <- holeOver ls (l1, l2) instruction
  pure (template, [("body", VerifyRedAt tid l1 True)], context)

-- | A test's after-run in a few words: its own outcome (at its assertion,
-- before it, passing, not run), then each sibling that passed before and
-- fails now; then the names EXTRA (left undefined by the last attempt) that
-- it does not already name.
testSummary :: [Text] -> Text -> TestRun -> TestRun -> Text -> Text
testSummary extra tid before after written = base <> (if null missing then "" else " \183 NameError " <> T.intercalate ", " missing)
  where
    base = own <> T.concat ["; red: " <> short t <> " no longer passes" | t <- broke]
    missing = [n | n <- nub extra, not (n `T.isInfixOf` base)]
    short = snd . T.breakOnEnd "::"
    isOwn t = t == tid || (tid <> "[") `T.isPrefixOf` t
    broke = [t | (t, _) <- trFailed after, not (isOwn t), t `elem` trPassed before]
    own = case (trBroken after, [f | (t, f) <- trFailed after, isOwn t]) of
      (Just why, _) -> "red: could not run \183 " <> firstLine why
      (_, f : _)
        | Just l <- fLine f, Just (a, z) <- assertionSpan (short tid) written, l >= a, l <= z -> "red at the assertion \183 " <> firstLine (fMessage f)
        | otherwise -> "red: error before the assertion \183 " <> firstLine (fMessage f)
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

-- | A body that is only a stub: no line but `...`, `pass`, `assert ...`,
-- a `raise NotImplementedError…`, an Olifant placeholder, or a comment.
isStub :: [Text] -> Bool
isStub = all stubLine . filter (not . T.null) . map T.strip
  where
    stubLine l = l `elem` ["...", "pass", "assert ..."] || "raise NotImplementedError" `T.isPrefixOf` l || "olifant:" `T.isInfixOf` l || "#" `T.isPrefixOf` l

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

-- | A function's after-run in a few words: a name left undefined (by a
-- failure's message, or EXTRA: the last attempt's check) is named first.
functionSummary :: [Text] -> TestRun -> Text
functionSummary extra run = case (trBroken run, trFailed run) of
  (Just why, _) -> "red: could not run \183 " <> firstLine why
  (_, failed) | names@(_ : _) <- nub (concatMap (undefinedNames . fMessage . snd) failed ++ extra) -> (if null failed then "green \183 NameError " else "red: NameError ") <> T.intercalate ", " names
  (_, []) -> "green"
  (_, (t, f) : _) -> "red: " <> snd (T.breakOnEnd "::" t) <> " \183 " <> firstLine (fMessage f)

-- | TEMPLATE planned over REL (TEXT) with VERIFY and CONTEXT: the file as
-- the fill writes it, or, when no attempt passed its verify, as the last
-- attempt writes it (a conjure always lands), with the names that attempt
-- left undefined; or, steered (STEERING, given how to wrap a climbed
-- rung's filler), a pause. Left when the model gave no attempt at all, or
-- the plan could not be made. ROUNDS keeps the highest round asked.
fill :: (ProgressEvent -> IO ()) -> IORef Int -> ServerState -> Project -> FilePath -> Text -> Template -> [(Text, Verify [Template])] -> [(FilePath, Text)] -> ModelFiller IO -> ((ModelFiller IO -> ModelFiller IO) -> Maybe (Steering IO)) -> ExceptT Text IO Written
fill progress rounds s project rel text template verify context filler steering = do
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
      options = planOptions {poFiller = Just (record filler), poProgress = progress', poCancel = ssCancel s, poContext = context, poStance = ConjureStance, poSteering = steering record}
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
              pure (Written (Just why) landing (undefinedNames why))
