-- | Internal to Olifant.Server: the loop's reconciliation of a plan with
-- the tree: the work set (what changed since a baseline commit, which of it
-- the plans already account for, which steps it touches), the facts that
-- land without a model, and the reconcile that asks Jev (and the ladder)
-- about the rest and answers ghosts.
module Olifant.Server.Loop
  ( Change (..)
  , ChangedUnit (..)
  , WorkSet (..)
  , baseline
  , changedUnits
  , accounted
  , workSet
  , unaccountedCap
  , Fact (..)
  , reanchorFacts
  , pendingFacts
  , testFacts
  , factJson
  , rejectedReadings
    -- * the questions
  , planStepQuestion
  , planStateQuestion
  , Need (..)
  , planWords
  , parseWords
  , Rungs (..)
  , rungsFor
  , gate
  , decideOver
    -- * the reconcile
  , Decision (..)
  , ReconcileBlock (..)
  , ReconcileAnswer (..)
  , reconcile
  , reconcileJson
  ) where

import Control.Monad (forM, join)
import Data.Aeson (Value, object, (.=))
import Data.Bifunctor (bimap)
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.Functor ((<&>))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (delete, find, isPrefixOf, mapAccumL, sort, sortOn, stripPrefix)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..), PlansConfig (..))
import Olifant.Decide (Answer (..), Confidence (..), Question (..), qOptions)
import Olifant.Language.Adapter (languageFor)
import Olifant.Model (ModelConfig (..), Rung (..))
import Olifant.Plans (Anchor (..), Kind (..), Line (..), Section (..), TestState (..), Verdict (..), childrenOf, evidenceTag, parsePlan, sections, stepDone, testState, withTestState, withVerdict)
import Olifant.Process (Cancel, runBytes)
import Olifant.Server.Errand (Block (..), Ghost (..))
import Olifant.Server.LoopQuestions
import Olifant.Server.LoopTypes
import Olifant.Server.Plans (Resolved (..), confined, currentBranch, gitOut, resolveAnchor, safely)
import Olifant.Server.State (ServerState (..), appendStepLine, projectFor)
import Olifant.Server.Telemetry (Decision (..), OutcomeRecord (..), ReconcileRecord (..), Record (..), appendRecord, errandId, readRecords, recordsPath)
import Olifant.Server.Unit (Named (..), fingerprint, unitsOf)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, takeExtension, (</>))

-- | What the loop works over: the baseline commit (none without git), every
-- changed unit (moves folded), those no plan accounts for (capped, the rest
-- counted), and the steps whose own anchor changed.
data WorkSet = WorkSet
  { wsBaseline :: Maybe Text
  , wsChanged :: [ChangedUnit]
  , wsUnaccounted :: [ChangedUnit]
  , wsMore :: Int
  , wsTouched :: [(Line, ChangedUnit)]
  }
  deriving stock (Eq, Show)

-- | The most unaccounted units a work set lists.
unaccountedCap :: Int
unaccountedCap = 40

-- | The commit a plan is reconciled against, at ROOT for the plan at PLAN
-- (relative): on the default branch (or detached, or with none) the commit
-- that added the plan, HEAD while it is uncommitted; on another branch its
-- merge-base with the default. Nothing without git or a commit.
baseline :: FilePath -> FilePath -> IO (Maybe Text)
baseline root plan =
  gitOut root ["rev-parse", "HEAD"] >>= \case
    Just [h] -> Just . T.pack <$> from h
    _ -> pure Nothing
  where
    from h = do
      def <- defaultBranch root
      cur <- currentBranch root
      case (def, cur) of
        (Just d, Just b) | b /= d -> do
          local <- mergeBase (T.unpack d)
          remote <- maybe (mergeBase ("origin/" <> T.unpack d)) (pure . Just) local
          pure (fromMaybe h remote)
        _ -> gitOut root ["log", "--diff-filter=A", "--follow", "--format=%H", "--", plan] <&> \case
          Just ls@(_ : _) -> last ls
          _ -> h
    -- the default branch as a local branch, else only as origin's
    mergeBase d = gitOut root ["merge-base", d, "HEAD"] <&> (>>= listToMaybe)

-- | The default branch: origin's HEAD, else `main`, else `master`, if any.
defaultBranch :: FilePath -> IO (Maybe Text)
defaultBranch root =
  gitOut root ["symbolic-ref", "-q", "--short", "refs/remotes/origin/HEAD"] >>= \case
    Just [r] -> pure (Just (T.pack (fromMaybe r (stripPrefix "origin/" r))))
    _ -> firstOf ["main", "master"]
  where
    firstOf = \case
      [] -> pure Nothing
      b : bs -> gitOut root ["rev-parse", "-q", "--verify", b] >>= maybe (firstOf bs) (const (pure (Just (T.pack b))))

-- | Every unit that differs from BASE, over the files git reports changed or
-- untracked (not the plans nor Olifant's own, only those of a known
-- language): now through the workspace (open buffers win), then through
-- `git show`. A removed unit whose base fingerprint an added one has now is
-- one `Moved`, on the new path and name.
changedUnits :: ServerState -> Project -> Text -> IO [ChangedUnit]
changedUnits s p base = do
  -- no renames: a renamed file's old path keeps its removed units (and its
  -- moves); paths unquoted, so non-ASCII ones are not lost
  diffed <- gitOut root ["-c", "core.quotePath=false", "diff", "--no-renames", "--name-only", "--relative", b]
  untracked <- gitOut root ["-c", "core.quotePath=false", "ls-files", "--others", "--exclude-standard"]
  let files = filter kept (nubOrd (map normalisePath (concat (catMaybes [diffed, untracked]))))
  folded . concat <$> mapM fileUnits files
  where
    root = prRoot p
    b = T.unpack base
    plansDir = normalisePath (pcDir (catalogPlans (prCatalog p)))
    kept f = not (any (\d -> (d <> "/") `isPrefixOf` f) [plansDir, ".olifant"])
    units lang t
      | T.null t = pure (Right [])
      | otherwise = safely (Left "") (unitsOf (pSyntax (ssPorts s)) lang t)
    fileUnits f = case languageFor (catalogLanguages (prCatalog p)) f of
      Nothing -> pure []
      Just lang -> do
        now <- fromMaybe "" <$> safely Nothing (readText (prWorkspace p) f)
        old <- fromMaybe "" <$> gitText root ["show", b <> ":./" <> f]
        (,) <$> units lang now <*> units lang old <&> \case
          (Right ns, Right os) -> compareUnits f now old ns os
          _ -> []

-- | One file's units now and at the base, compared by name: each with its
-- base fingerprint when removed (for pairing moves).
compareUnits :: FilePath -> Text -> Text -> [Named] -> [Named] -> [(ChangedUnit, Maybe Text)]
compareUnits f now old ns os =
  [ (unit n (Just fpN) change (Just (cut now n)) (cut old <$> o), Nothing)
  | n <- nubOrdOn namedName ns
  , let fpN = fingerprint now (namedLines n)
        o = M.lookup (namedName n) olds
        fpO = fingerprint old . namedLines <$> o
  , change <- case fpO of
      Nothing -> [Added]
      Just fp | fp /= fpN -> [Changed fp]
      _ -> []
  ]
    ++ [ (unit o Nothing Removed Nothing (Just (cut old o)), Just (fingerprint old (namedLines o)))
       | o <- nubOrdOn namedName os
       , not (M.member (namedName o) nows)
       ]
  where
    olds = byName os
    nows = byName ns
    byName xs = M.fromListWith (\_ first -> first) [(namedName x, x) | x <- xs]
    unit n fp change nowT baseT = ChangedUnit f (namedName n) fp change (header n) nowT baseT
    header n = T.strip (fromMaybe "" (listToMaybe (drop (namedDef n - 1) (T.splitOn "\n" (if M.member (namedName n) nows then now else old)))))
    cut t n = let (a, z) = namedLines n in T.intercalate "\n" (take (min 60 (z - a + 1)) (drop (a - 1) (T.splitOn "\n" t)))

-- | Each added unit that has a removed one's base fingerprint becomes a
-- `Moved` from it; the removed one goes.
folded :: [(ChangedUnit, Maybe Text)] -> [ChangedUnit]
folded xs = [cu | (i, cu) <- zip [0 :: Int ..] out, i `notElem` used]
  where
    removed = [(i, fp, cu) | (i, (cu, Just fp)) <- zip [0 ..] xs]
    (left, out) = mapAccumL pair removed (map fst xs)
    used = [i | (i, _, _) <- removed, i `notElem` [j | (j, _, _) <- left]]
    pair avail cu = case (cuChange cu, cuFingerprint cu) of
      (Added, Just fp) | r@(_, _, from) : _ <- [r | r@(_, rfp, _) <- avail, rfp == fp] ->
        (delete r avail, cu {cuChange = Moved (cuPath from) (cuName from), cuBase = cuBase from})
      _ -> (avail, cu)

-- | The (path, unit, fingerprint) every plan under the plans folder accounts
-- for: each anchor verified at a fingerprint, each test child's unit as it
-- is now; and the readings rejected.
accounted :: ServerState -> Project -> IO (S.Set (FilePath, Text, Text))
accounted s p = do
  files <- safely [] (listFiles ws dir)
  entries <- forM [f | f <- files, takeExtension f == ".md"] $ \f -> do
    ls <- parsePlan . fromMaybe "" <$> safely Nothing (readText ws (dir </> f))
    tests <- forM [a | l <- ls, lnKind l == TestChild, Just a <- [lnAnchor l]] $ \a ->
      resolveAnchor s p a <&> \r -> (rsRel r,,) <$> fmap namedName (rsUnit r) <*> rsFingerprint r
    pure ([(rel, anUnit a, fp) | l <- ls, Just a <- [lnAnchor l], Verified fp <- [lnVerdict l], Just rel <- [confined (anPath a)]] ++ catMaybes tests)
  rejected <- rejectedReadings s (prRoot p)
  pure (S.fromList (concat entries) <> rejected)
  where
    ws = prWorkspace p
    dir = pcDir (catalogPlans (prCatalog p))

-- | The (path, unit, fingerprint) of every reconcile ghost rejected on this
-- machine (the repo at ROOT's records): each outcome names the latest
-- reconcile record before it with its id, and that record's block.
rejectedReadings :: ServerState -> FilePath -> IO (S.Set (FilePath, Text, Text))
rejectedReadings s root = do
  txt <- fromMaybe "" <$> safely Nothing (readText (pFiles (ssPorts s)) (recordsPath root))
  pure (S.fromList (catMaybes (snd (mapAccumL step M.empty (fst (readRecords txt))))))
  where
    step known = \case
      AReconcile r -> (M.insert (rcId r) (rcBlocks r) known, Nothing)
      AnOutcome o | orOutcome o == "rejected", orBlock o >= 0 -> (known, M.lookup (orId o) known >>= \bs -> join (listToMaybe (drop (orBlock o) bs)))
      _ -> (known, Nothing)

-- | The work set of the plan at PLAN (relative) with lines LS. Unaccounted
-- keeps git's order (changed files, then untracked): the workspace has no
-- cheap modification stamp.
workSet :: ServerState -> Project -> FilePath -> [Line] -> IO WorkSet
workSet s p plan ls =
  baseline (prRoot p) plan >>= \case
    Nothing -> pure (WorkSet Nothing [] [] 0 [])
    Just b -> do
      changed <- changedUnits s p b
      acc <- accounted s p
      let isAccounted cu = maybe False (\fp -> (cuPath cu, cuName cu, fp) `S.member` acc) (cuFingerprint cu)
          unaccounted = [cu | cu <- changed, not (isAccounted cu), not (any (`S.member` own) (keys cu))]
          (shown, rest) = splitAt unaccountedCap unaccounted
      pure (WorkSet (Just b) changed shown (length rest) [(l, cu) | (l, k) <- steps, cu <- changed, k `elem` keys cu])
  where
    steps = [(l, (rel, anUnit a)) | l <- ls, Step _ <- [lnKind l], Just a <- [lnAnchor l], Just rel <- [confined (anPath a)]]
    own = S.fromList (map snd steps)
    -- a unit's (path, name), and where it was before a move
    keys cu = (cuPath cu, cuName cu) : [(o, n) | Moved o n <- [cuChange cu]]

-- | A read-only git command's whole output; Nothing when it fails.
gitText :: FilePath -> [String] -> IO (Maybe Text)
gitText root args =
  runBytes "git" (["-C", root] ++ args) (Just root) "" <&> \case
    Right (ExitSuccess, out, _) -> Just (TE.decodeUtf8Lenient out)
    _ -> Nothing

-- | Every line whose anchor names a moved unit's old (path, name): the
-- anchor rewritten to the new ones, the verdict kept.
reanchorFacts :: [Line] -> [ChangedUnit] -> [Fact]
reanchorFacts ls cus =
  [ Replace (lnRow l) (T.take (anStart a) (lnText l) <> "@" <> T.pack (cuPath cu) <> ":" <> cuName cu <> T.drop (anEnd a) (lnText l))
  | l <- ls, Just a <- [lnAnchor l]
  , cu <- take 1 [c | c@ChangedUnit {cuChange = Moved op on} <- cus, op == anPath a, on == anUnit a] ]

-- | A touched step's test children at green or red go back to pending.
pendingFacts :: [Line] -> [(Line, ChangedUnit)] -> [Fact]
pendingFacts ls touched =
  [ Replace (lnRow c) (withTestState (lnText c) TestPending)
  | (s, _) <- touched, c <- childrenOf ls (lnRow s), lnKind c == TestChild
  , testState c `elem` [Just TestGreen, Just TestRed] ]

-- | Test results for a step's children (by test unit name): the children
-- rewritten; all green ticks the step at FP; a red test on a done step
-- un-ticks it and notes "red again" once.
testFacts :: [Line] -> Int -> [(Text, TestState)] -> Maybe Text -> [Fact]
testFacts ls row states fp = case drop row ls of
  [] -> []
  (step : _) ->
    let direct = [c | c <- childrenOf ls row, lnIndent c == lnIndent step + 2]
        kids = [c | c <- direct, lnKind c == TestChild]
        stateOf c = fromMaybe (testState c) (Just <$> (lnAnchor c >>= \a -> lookup (anUnit a) states))
        rewrites = [Replace (lnRow c) (withTestState (lnText c) s) | c <- kids, Just a <- [lnAnchor c], Just s <- [lookup (anUnit a) states]]
        reds = [anUnit a | c <- kids, Just a <- [lnAnchor c], stateOf c == Just TestRed]
        allGreen = not (null kids) && all ((== Just TestGreen) . stateOf) kids
        open = lnKind step == Step False
        tick = [Replace row (withVerdict (checkbox "- [ ]" "- [x]" (lnText step)) (maybe NoVerdict Verified fp)) | allGreen, open]
        pad = T.replicate (lnIndent step + 2) " "
        noted t = any (\c -> evidenceTag c == Just "red again" && T.strip (lnText c) == "- ✗ " <> t <> " red again") direct
        fresh = [pad <> "- ✗ " <> t <> " red again" | t <- reds, not (noted t)]
        lastRow = maybe row lnRow (listToMaybe (reverse (childrenOf ls row)))
        untick = [Replace row (withVerdict (checkbox "- [x]" "- [ ]" (lnText step)) NoVerdict) | not (null reds), lnKind step == Step True]
        note = [Insert lastRow fresh | not (null reds), lnKind step == Step True, not (null fresh)]
    in rewrites ++ tick ++ untick ++ note

-- | A fact for the wire: {"row", "text"} or {"after", "lines"}.
factJson :: Fact -> Value
factJson (Replace n t) = object ["row" .= n, "text" .= t]
factJson (Insert n ls) = object ["after" .= n, "lines" .= ls]

-- | Swap the checkbox right after the indent, leaving the rest of the line alone.
checkbox :: Text -> Text -> Text -> Text
checkbox from to txt =
  let (ind, rest) = T.span (== ' ') txt
  in maybe txt (\r -> ind <> to <> r) (T.stripPrefix from rest)

-- | The facts of a work set over lines LS: re-anchors, then the pending
-- marks over the re-anchored lines, one fact per row (the later wins).
loopFacts :: [Line] -> WorkSet -> [Fact]
loopFacts ls ws = mergeFacts (moved ++ pendingFacts (applyFacts moved ls) (wsTouched ws))
  where
    moved = reanchorFacts ls (wsChanged ws)

-- | Replacements by row, the later winning, in row order; inserts after.
mergeFacts :: [Fact] -> [Fact]
mergeFacts fs = [Replace r t | (r, t) <- M.toList (M.fromList [(r, t) | Replace r t <- fs])] ++ [f | f@(Insert _ _) <- fs]

-- | Lines with the replacements applied (rows are kept).
applyFacts :: [Fact] -> [Line] -> [Line]
applyFacts fs ls = parsePlan (T.intercalate "\n" [fromMaybe (lnText l) (M.lookup (lnRow l) new) | l <- ls])
  where
    new = M.fromList [(r, t) | Replace r t <- mergeFacts fs]

-- | What became of one file's plan-step question.
data Placement = Placed Line [ChangedUnit] | Unplaced [ChangedUnit] | Unasked [ChangedUnit]

-- | Reconcile the plan at PATH (its editor text, else the file) with the
-- tree: the facts, and the readings as ghost blocks. Nothing changed (or
-- nothing left to say): the facts alone, no question, no record. The same
-- situation as last time: the last blocks, fresh facts, no record.
-- Otherwise Jev and the ladder (`ask` rungs only when FORCED) answer a
-- plan-step per changed file and a plan-state per touched or placed step,
-- within the budget; one words call fills the lines; the reconcile is
-- recorded with every decision and logged as one step line. Nothing is
-- written to the plan.
reconcile :: Cancel -> Bool -> FilePath -> ServerState -> IO (ServerState, Either Text ReconcileAnswer)
reconcile token forced path s0 = do
  start <- getMonotonicTime
  projectFor path s0 >>= \case
    (s, Nothing) -> pure (s, Left "this plan is in no Olifant project")
    (s, Just (_, p)) -> do
      text <- case M.lookup path (ssDocs s) of
        Just (_, t) -> pure t
        Nothing -> fromMaybe "" <$> safely Nothing (readText (pFiles (ssPorts s)) path)
      let ls0 = parsePlan text
          root = prRoot p
          rel = makeRelative root (normalisePath path)
      ws <- workSet s p rel ls0
      rejected <- rejectedReadings s root
      branch <- currentBranch root
      now <- pNow (ssPorts s)
      let facts = loopFacts ls0 ws
          -- ghosts are read against the plan as the facts leave it
          ls = applyFacts facts ls0
          lineAt r = fromMaybe (Line r 0 Prose "" Nothing NoVerdict) (listToMaybe (drop r ls))
          unaccounted = [cu | cu <- wsUnaccounted ws, cuChange cu /= Removed]
          touched = M.toList (M.fromListWith (flip (++)) [(lnRow l, [cu]) | (l, cu) <- wsTouched ws])
          -- steps whose unit is gone, unless they say so already
          gone = [(step, cus) | (r, cus) <- touched, any ((== Removed) . cuChange) cus, let step = lineAt r, not (any ((== Just "gone") . evidenceTag) (directChildren ls step))]
          -- a step settled already: verified at the unit now, its tests
          -- deciding it, or its reading rejected at this fingerprint
          settled step cu =
            maybe False (\fp -> lnVerdict step == Verified fp || (cuPath cu, cuName cu, fp) `S.member` rejected) (cuFingerprint cu)
              || (stepDone step && any ((== TestChild) . lnKind) (directChildren ls step))
          asking = [(lineAt r, cus) | (r, cus) <- touched, not (any ((== Removed) . cuChange) cus), not (all (settled (lineAt r)) cus)]
          key = T.intercalate "\0" [text, if forced then "forced" else "auto", tshow (sort (map triple (wsChanged ws))), tshow (sort (map triple unaccounted)), tshow [(lnRow l, map triple cus) | (l, cus) <- asking]]
          answer i blocks more secs rungs = ReconcileAnswer i (wsBaseline ws) facts blocks (length (wsChanged ws)) more secs rungs
          rid = errandId now "reconcile" rel 0 Nothing
      case M.lookup path (ssReconciled s) of
        _ | null unaccounted && null asking && null gone -> do
          end <- getMonotonicTime
          pure (s, Right (answer rid [] (wsMore ws) (end - start) []))
        Just (k, cached) | k == key -> pure (s, Right cached {raFacts = facts})
        _ -> do
          let rungs = rungsFor s {ssCancel = token} p forced
              models = catalogModel (prCatalog p)
              cfg = catalogPlans (prCatalog p)
              budget = fromIntegral (fromMaybe (mcBudget models) (pcBudget cfg)) :: Double
              timeout = mcTimeout models
              canAsk = isJust (rgJev rungs) || not (null (rgChat rungs))
          asked <- newIORef ([], [])
          let -- one question, unless past the budget; recorded as a decision
              askQ q = do
                t <- getMonotonicTime
                if t - start > budget
                  then pure Nothing
                  else do
                    (a, climbs) <- decideOver token timeout rungs q
                    modifyIORef' asked (\(ds, cs) -> (decisionOf q a climbs : ds, reverse climbs ++ cs))
                    pure (Just (a, accepted' q a))
              accepted' q a = case a of
                Chosen c _ _ | accepted q a -> Just c
                _ -> Nothing
          placements <-
            if not canAsk
              then pure []
              else forM (byFile unaccounted) $ \(f, cus) ->
                askQ (planStepQuestion branch ls (f, cus)) <&> \case
                  Nothing -> Unasked cus
                  Just (_, Just c) | Just r <- T.stripPrefix "s" c, [(row, "")] <- reads (T.unpack r), Step False <- lnKind (lineAt row) -> Placed (lineAt row) cus
                  Just _ -> Unplaced cus
          let gained = M.fromListWith (flip (++)) [(lnRow l, cus) | Placed l cus <- placements]
              stateRows = M.toList (M.unionWith (++) (M.fromList [(lnRow l, cus) | (l, cus) <- asking]) (M.map (const []) gained))
          states <-
            if not canAsk
              then pure []
              else forM stateRows $ \(r, own) -> do
                let units = own ++ M.findWithDefault [] r gained
                askQ (planStateQuestion (lineAt r) ls units) <&> \case
                  Nothing -> (lineAt r, own, units, Nothing, True)
                  Just (_, c) -> (lineAt r, own, units, c, False)
          let unplaced = concat [cus | Unplaced cus <- placements]
              whys = [(step, units, own) | (step, own, units, Just "diverged", _) <- states]
              needs = map NeedStepLine unplaced ++ [NeedWhy step units | (step, units, _) <- whys]
              skipped = sum [length cus | Unasked cus <- placements] + sum [length own | (_, own, _, _, True) <- states]
          t <- getMonotonicTime
          said <- case rgChat rungs of
            (r, b) : _ | not (null needs), t - start <= budget -> do
              w <- planWords token timeout b needs
              t' <- getMonotonicTime
              modifyIORef' asked (\(ds, cs) -> (ds, (rungName r, t' - t) : cs))
              pure w
            _ -> pure (map (const Nothing) needs)
          stepFps <- forM states $ \(step, own, _, _, _) -> (lnRow step,) <$> case [fp | cu <- own, Just fp <- [cuFingerprint cu]] of
            fp : _ -> pure (Just fp)
            [] -> maybe (pure Nothing) (fmap rsFingerprint . resolveAnchor s p) (lnAnchor step)
          let (stepWords, whyWords) = splitAt (length unplaced) said
              fpOf step = join (lookup (lnRow step) stepFps)
              unitOf step own = case [(cuPath cu, cuName cu, fp) | cu <- own, Just fp <- [cuFingerprint cu]] of
                u : _ -> Just u
                [] -> (\a fp -> (anPath a, anUnit a, fp)) <$> lnAnchor step <*> fpOf step
              child step = (T.replicate (lnIndent step + 2) " " <>)
              after step lines' replace unit = ReconcileBlock (Block (lnRow step) (Just (lnText step)) (map ghost lines')) replace unit
              also = [after step [child step ("- also " <> anchorOf cu)] False (triple' cu) | Placed step cus <- placements, cu <- cus]
              stateGhosts =
                [ after step [t'] True (unitOf step own)
                | (step, own, _, Just c, _) <- states
                , Just t' <- [case c of
                    _ | c `elem` ["done", "still_done"] -> Just (withVerdict (checkbox "- [ ]" "- [x]" (lnText step)) (maybe NoVerdict Verified (fpOf step)))
                    "reopened" -> Just (withVerdict (checkbox "- [x]" "- [ ]" (lnText step)) NoVerdict)
                    _ -> Nothing] ]
              goneGhosts =
                [ after step [child step ("- ✗ gone: " <> anchorOf cu <> " no longer exists")] False Nothing
                | (step, cus) <- gone, cu <- take 1 [c | c <- cus, cuChange c == Removed] ]
              whyGhosts =
                [ after step [child step ("- ✗ changed: " <> fromMaybe (T.intercalate ", " (map cuName units) <> " changed") w)] False (unitOf step own)
                | ((step, units, own), w) <- zip whys whyWords ]
              (atStep, indent) = newStepPlace branch ls
              newSteps =
                [ ReconcileBlock (Block (lnRow atStep) (Just (lnText atStep)) [ghost (indent <> "- [ ] " <> fromMaybe (headerWords cu) w <> " " <> anchorOf cu)]) False (triple' cu)
                | (cu, w) <- zip unplaced stepWords ]
              title = fromMaybe (lineAt 0) (find (\l -> case lnKind l of Heading _ -> True; _ -> False) ls)
              more = wsMore ws + skipped
              summaryGhost n what = ReconcileBlock (Block (lnRow title) (Just (lnText title)) [ghost ("- ? " <> tshow n <> " " <> what)]) False Nothing
              notes
                | not canAsk = [summaryGhost (length unaccounted + wsMore ws) "changed units not in the plan" | not (null unaccounted)]
                | otherwise = [summaryGhost more "more changed units not in the plan" | more > 0]
              blocks = notes ++ sortOn (blAfterRow . rbBlock) (stateGhosts ++ also ++ goneGhosts ++ whyGhosts) ++ newSteps
          (decisions, climbs) <- bimap reverse reverse <$> readIORef asked
          end <- getMonotonicTime
          let secs = end - start
              rungNames = nubOrd (map fst climbs)
              a = answer rid blocks more secs rungNames
          appendRecord (ssPorts s) root . AReconcile $
            ReconcileRecord
              { rcId = rid, rcAt = now, rcPlan = rel, rcBranch = branch, rcBaseline = wsBaseline ws, rcForced = forced
              , rcChanged = length (wsChanged ws), rcUnaccounted = length (wsUnaccounted ws), rcMore = more, rcFacts = length facts
              , rcSeconds = secs, rcClimbs = climbs, rcDecisions = decisions, rcBlocks = map rbUnit blocks }
          appendStepLine (ssPorts s) root ["plan-reconcile", T.pack rel, tshow (length facts) <> " facts", tshow (length blocks) <> " ghosts", T.pack (showFFloat (Just 2) secs "") <> " s", T.intercalate "," rungNames]
          pure (s {ssReconciled = M.insert path (key, a) (ssReconciled s)}, Right a)
  where
    triple cu = (cuPath cu, cuName cu, fromMaybe "" (cuFingerprint cu))
    triple' cu = (cuPath cu, cuName cu,) <$> cuFingerprint cu
    anchorOf cu = "@" <> T.pack (cuPath cu) <> ":" <> cuName cu
    ghost t = Ghost t (listToMaybe (parsePlan t) >>= lnAnchor)
    -- a header as a step's words: its first line without the block opener
    headerWords cu = T.strip (T.dropWhileEnd (`elem` [':', '{', ' ']) (cuHeader cu))
    decisionOf q a climbs =
      Decision
        { dcKind = qKind q
        , dcOptions = qOptions q
        , dcChosen = case a of Chosen c _ _ -> Just c; _ -> Nothing
        , dcConfidence = case a of Chosen _ _ (Confidence x) -> Just x; _ -> Nothing
        , dcRung = maybe "" fst (listToMaybe (reverse climbs))
        }

-- | Units grouped by file, in the order files first appear.
byFile :: [ChangedUnit] -> [(FilePath, [ChangedUnit])]
byFile cus = [(f, [cu | cu <- cus, cuPath cu == f]) | f <- nubOrd (map cuPath cus)]

-- | A step's children one level in.
directChildren :: [Line] -> Line -> [Line]
directChildren ls step = [c | c <- childrenOf ls (lnRow step), lnIndent c == lnIndent step + 2]

-- | Where new steps go: after the last step (and its children) of the
-- section bound to BRANCH, else of the last section with steps, else at
-- the plan's end; with that step's indent.
newStepPlace :: Maybe Text -> [Line] -> (Line, Text)
newStepPlace branch ls = case (mine, [s | s <- reverse secs, not (null (steps s))]) of
  (sec : _, _) -> at (last (steps sec))
  ([], sec : _) -> at (last (steps sec))
  _ -> (fromMaybe (Line 0 0 Prose "" Nothing NoVerdict) (listToMaybe (reverse ls)), "")
  where
    secs = sections ls
    steps sec = [l | l <- ls, lnRow l >= secRow sec, lnRow l < secEnd sec, case lnKind l of Step _ -> True; _ -> False]
    mine = [sec | Just b <- [branch], sec <- secs, secBranch sec == Just b, not (null (steps sec))]
    at step = (fromMaybe step (listToMaybe (reverse (childrenOf ls (lnRow step)))), T.replicate (lnIndent step) " ")

-- | An answer for the wire: {id, baseline, facts, blocks: [{after,
-- after_text, lines: [{text, fp}], replace, unit}], changed, more, seconds,
-- rungs}. A line's fp is its block's unit's, on a line anchoring that unit
-- that is not an open step (the plugin stamps it on accept).
reconcileJson :: ReconcileAnswer -> Value
reconcileJson a =
  object
    [ "id" .= raId a, "baseline" .= raBaseline a, "facts" .= map factJson (raFacts a)
    , "blocks" .= map blockJson (raBlocks a), "changed" .= raChanged a, "more" .= raMore a
    , "seconds" .= raSeconds a, "rungs" .= raRungs a ]
  where
    blockJson b =
      object
        [ "after" .= blAfterRow (rbBlock b), "after_text" .= blAfterText (rbBlock b)
        , "lines" .= [object ["text" .= ghText g, "fp" .= fpFor (rbUnit b) g] | g <- blLines (rbBlock b)]
        , "replace" .= rbReplace b
        , "unit" .= fmap (\(path, unit, fp) -> object ["path" .= path, "unit" .= unit, "fp" .= fp]) (rbUnit b) ]
    fpFor unit g = case (unit, ghAnchor g, lnKind <$> listToMaybe (parsePlan (ghText g))) of
      (Just (path, name, fp), Just an, Just k) | anPath an == path, anUnit an == name, k /= Step False -> Just fp
      _ -> Nothing

tshow :: Show a => a -> Text
tshow = T.pack . show
