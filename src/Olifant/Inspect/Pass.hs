-- | One inspect pass over one saved file (design steps 1-8): the units that
-- differ from HEAD and are not inspected yet, screened; the finished ones sent
-- to the find call in packs under the spend caps; each finding placed,
-- matched against the store, filtered and verified; the store updated.
module Olifant.Inspect.Pass
  ( InspectDeps (..)
  , InspectOutcome (..)
  , changedOf
  , inspectPass
  , spentToday
  , findEstimate
  , claimEstimate
  , capNoticeDue
  , unitFingerprints
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (Exception, SomeException, handle, throwIO, try)
import Control.Monad (foldM, unless, when)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, TimeZone, UTCTime, localDay, utcToLocalTime)
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..), PlansConfig (..), InspectConfig (..))
import Olifant.Decide (Answer (No, Yes), Confidence (..), Decider (..), Question (..))
import Olifant.Decide qualified as D
import Olifant.Glob (matchesGlob)
import Olifant.Language (Language (..))
import Olifant.Language.Adapter (languageFor)
import Olifant.Plans (Kind (..))
import Olifant.Plans.Sentences (Sentence (..))
import Olifant.Rules (Finding (..))
import Olifant.Server.Checker (CheckAsk (..), CheckMode (..), Checker, fences, fileHash)
import Olifant.Server.Errand (RunResult (..), Status (..))
import Olifant.Server.Errand qualified as E
import Olifant.Server.Fixes (findingsFor)
import Olifant.Server.Loop (checkWith, compareUnits)
import Olifant.Server.LoopTypes (Change (..), ChangedUnit (..))
import Olifant.Server.State (ServerState (..))
import Olifant.Server.Telemetry (ErrandRecord (..), Record (..), appendRecord, errandId, readRecords, recordsPath)
import Olifant.Server.Unit (Named (..), fingerprint, unitsOf)
import Olifant.Process (Cancel, gitIn)
import Olifant.Inspect.Find
import Olifant.Inspect.Judge
import Olifant.Inspect.Steering (Steering (..), noSteering, steering, steeringPath)
import Olifant.Inspect.Store
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (makeRelative, (</>))

data InspectDeps = InspectDeps
  { vdDecider :: Decider IO
  , vdFinder :: Finder
  , vdCheckerFor :: PlansConfig -> Checker
  , -- | the user's zone: `day_cost` is a local day
    vdZone :: TimeZone
  , -- | units a find call already in flight holds (their fingerprint unmoved): left out
    vdSkip :: Set Text
  , -- | told each pack's (unit, fingerprint) pairs before its find call
    vdOnFind :: [(Text, Text)] -> IO ()
  , -- | told when the pass has stored findings before its claim checks finish (the editor refreshes)
    vdProgress :: IO ()
  }

data InspectOutcome
  = InspectSkipped Text
  | InspectDone {voChanged :: Bool, voCapped :: Bool, voDayCapped :: Bool, voSpent :: Double}
  deriving stock (Eq, Show)

-- | The units of a file (text now) that differ from HEAD: added, or changed
-- from their HEAD fingerprint. No HEAD, or an untracked file: every unit.
changedOf :: ServerState -> Project -> Language -> FilePath -> Text -> [Named] -> IO [Named]
changedOf s p lang rel text units =
  gitIn (prRoot p) ["show", "HEAD:./" <> rel] >>= \case
    Left _ -> pure units
    Right old ->
      safeUnits (unitsOf (pSyntax (ssPorts s)) lang (Just rel) old) >>= \case
        Nothing -> pure units
        Just olds ->
          let names = S.fromList [cuName cu | (cu, _) <- compareUnits rel text old units olds, isChange (cuChange cu)]
           in pure [n | n <- units, namedName n `S.member` names]
  where
    isChange = \case
      Added -> True
      Changed _ -> True
      _ -> False

safeUnits :: IO (Either Text [Named]) -> IO (Maybe [Named])
safeUnits act = (try act :: IO (Either SomeException (Either Text [Named]))) >>= \case
  Right (Right ns) -> pure (Just ns)
  _ -> pure Nothing

-- | Each unit's fingerprint and first line in the file as it is now (empty
-- when it has no language or its units do not read): what an in-flight find
-- call is held to, and where the margin draws a finding.
unitFingerprints :: ServerState -> Project -> FilePath -> IO (M.Map Text (Text, Int))
unitFingerprints s p path = case languageFor (catalogLanguages (prCatalog p)) rel of
  Nothing -> pure M.empty
  Just lang -> do
    text <- fromMaybe "" <$> readText (prWorkspace p) rel
    ns <- fromMaybe [] <$> safeUnits (unitsOf (pSyntax (ssPorts s)) lang (Just rel) text)
    pure (M.fromList [(namedName n, (fingerprint text (namedLines n), fst (namedLines n))) | n <- ns])
  where
    rel = normalisePath (makeRelative (prRoot p) path)

-- | Today's inspect spend (find calls and claim checks), by the local day.
spentToday :: TimeZone -> UTCTime -> [Record] -> Double
spentToday zone now records =
  sum [fromMaybe 0 (erCost e) | AnErrand e <- records, erVerb e `elem` ["inspect", "inspect-check"], day (erAt e) == day now]
  where
    day = localDay . utcToLocalTime zone

-- | What the next find call is likely to cost: the mean of the last 20 priced
-- ones, else 1a's means (sonnet $0.04, haiku $0.005).
findEstimate :: InspectConfig -> [Record] -> Double
findEstimate cfg = meanOr (if "haiku" `T.isInfixOf` vcFindModel cfg then 0.005 else 0.04) "inspect"

-- | What a claim check is likely to cost: the mean of the last 20 priced
-- inspect-checks, else the design's $0.016 (1a measured $0.008).
claimEstimate :: [Record] -> Double
claimEstimate = meanOr 0.016 "inspect-check"

meanOr :: Double -> Text -> [Record] -> Double
meanOr def verb records = case take 20 (reverse [c | AnErrand e <- records, erVerb e == verb, Just c <- [erCost e]]) of
  [] -> def
  cs -> sum cs / fromIntegral (length cs)

-- | The day cap's editor message is sent the first time a local day's cap stops a pass.
capNoticeDue :: Maybe Day -> Day -> Bool
capNoticeDue last today = last /= Just today

pYes :: (Answer, [(Text, Double)]) -> Double
pYes (a, _) = case a of
  Yes (Confidence c) -> c
  No (Confidence c) -> 1 - c
  _ -> 0

-- | Jev gave no answer (a failure, a timeout, no key): inspect does not run on a guess.
data JevSilent = JevSilent
  deriving stock (Show)

instance Exception JevSilent

inspectPass :: Cancel -> ServerState -> Project -> InspectDeps -> FilePath -> IO InspectOutcome
inspectPass token s p deps path = handle (\JevSilent -> pure (InspectSkipped "Jev did not answer")) (inspectPassOn token s p deps path)

inspectPassOn :: Cancel -> ServerState -> Project -> InspectDeps -> FilePath -> IO InspectOutcome
inspectPassOn _token s p deps path
  | any (`matchesGlob` rel) (vcExclude cfg) = pure (InspectSkipped "excluded")
  | not (readablePath fenced rel) = pure (InspectSkipped "fenced")
  | otherwise =
      readInspectStore files root >>= \case
        Left why -> pure (InspectSkipped ("the inspect store does not read: " <> why))
        Right store0 -> case languageFor (catalogLanguages cat) rel of
          Nothing -> pure (InspectSkipped "no language")
          Just lang -> do
            text <- fromMaybe "" <$> readText (prWorkspace p) rel
            safeUnits (unitsOf (pSyntax ports) lang (Just rel) text) >>= \case
              Just units@(_ : _) -> withUnits store0 lang text units
              _ -> pure (InspectSkipped "no units")
  where
    root = prRoot p
    cat = prCatalog p
    cfg = catalogInspect cat
    pcfg = catalogPlans cat
    ports = ssPorts s
    files = pFiles ports
    rel = normalisePath (makeRelative root path)
    fenced = fences pcfg
    -- every Jev question of the pass: no answer stops the pass, with no record
    weighed q = weigh (vdDecider deps) q >>= \case
      (D.Unsure, _) -> throwIO JevSilent
      a -> pure a

    withUnits store0 lang text units = do
      st <- maybe noSteering steering <$> readText files (root </> steeringPath)
      changed <- changedOf s p lang rel text units
      let fpOf n = fingerprint text (namedLines n)
          inspected = M.findWithDefault S.empty rel (vsInspected store0)
          todo = [n | n <- changed, fpOf n `S.notMember` inspected, namedName n `S.notMember` vdSkip deps]
      -- no unfinished screen for now (user ruling, 2026-11-15): every changed unit is
      -- inspected, and Jev's nitpick filter decides what is shown
      let finished = [(n, False) | n <- todo]
      -- a waiting finding is verified when the margin has room, even with nothing new to find
      let openHere = length [f | f <- vsFindings store0, plFile (vfPlace f) == rel, vfStatus f == Open, vfKind f `elem` vcKinds cfg]
          room = openHere < vcMaxShown cfg && any (\f -> plFile (vfPlace f) == rel && vfStatus f == Waiting) (vsFindings store0)
      -- findings in units that left the file are gone, even when nothing else changed
      let names = S.fromList (map namedName units)
          goneIds = S.fromList [vfId f | f <- vsFindings store0, plFile (vfPlace f) == rel, maybe False (`S.notMember` names) (plUnit (vfPlace f)), vfStatus f `elem` [Open, Waiting, Withheld]]
      if null finished && not room
        then
          if S.null goneIds
            then pure (InspectDone False False False 0)
            else do
              let swept = store0 {vsFindings = [if vfId f `S.member` goneIds then f {vfStatus = Gone} else f | f <- vsFindings store0]}
              _ <- updateInspectStore files root (\current -> (mergeInto store0 swept current, ()))
              pure (InspectDone True False False 0)
        else find st store0 lang text units (S.fromList (map namedName changed)) finished

    find st store0 lang text units changedNames finished = do
      (_, reported) <- findingsFor path s
      diff <- gitIn root ["diff", "--no-color", "--no-ext-diff", "HEAD", "--", rel] >>= \case
        Right dt | not (T.null (T.strip dt)) -> pure dt
        _ -> pure text
      records <- maybe [] (fst . readRecords) <$> readText files (recordsPath root)
      now <- pNow ports
      let today = spentToday (vdZone deps) now records
          est = findEstimate cfg records
          over spent e = spent + e > vcPassCost cfg || overDay spent e
          overDay spent e = today + spent + e > vcDayCost cfg
          reportedText = [fRule f <> " at " <> T.pack rel <> maybe "" (\((l, _), _) -> ":" <> T.pack (show l)) (fRange f) <> ": " <> fMessage f | f <- reported]
          findUnit (n, inProgress) = FindUnit rel (namedName n) (namedLines n) (nonEmpty (headerOf text units n)) (cut text n) inProgress
          openOn names = [f | f <- vsFindings store0, plFile (vfPlace f) == rel, vfStatus f `elem` [Open, Waiting], maybe False (`elem` names) (plUnit (vfPlace f))]
      -- the find calls, one a pack, while the caps allow
      (spent1, capped, answered) <-
        foldM
          ( \(spent, cappedNames, done) pack ->
              let names = map fuName pack
               in if not (null cappedNames) || over spent est
                    then pure (spent, cappedNames ++ names, done)
                    else do
                      let ask = FindAsk pack diff (stText st) reportedText (openOn names)
                      vdOnFind deps [(fuName u, fingerprint text (fuLines u)) | u <- pack]
                      r <- vdFinder deps ask
                      -- an unpriced answer, a timeout or a failed session is charged its estimate
                      let cost = case r of
                            Right rr -> fromMaybe est (rrCost rr)
                            Left fl | E.rfStatus fl == E.Timeout || isJust (E.rfSession fl) -> est
                            Left _ -> 0
                          parsed = either (Left . E.rfWhy) (parseFind . rrText) r
                      logFind now pack r parsed cost
                      pure (spent + cost, cappedNames, done ++ either (const []) (\reply -> [(names, reply)]) parsed)
          )
          (0, [], [])
          (packUnits (map findUnit finished))
      spentRef <- newIORef spent1
      cappedRef <- newIORef (not (null capped))
      dayCappedRef <- newIORef (not (null capped) && overDay spent1 est)
      let raws = concatMap (frFindings . snd) answered
          resolvedIds = S.fromList [i | (_, reply) <- answered, (i, False, _) <- frOpen reply]
          codeAt t ln = citedCode t ln
      textOf <- memoText
      -- place, match, filter
      vs1 <-
        foldM
          ( \vs raw -> do
              let (pl, rule, st0) = place st rel text units changedNames raw
                  raw' = raw {rfRule = rule}
                  unitName = fromMaybe "" (plUnit pl)
                  newF keep status askedOn = InspectFinding (findingId now pl (rfStatement raw)) (rfKind raw) rule (rfStatement raw) pl (rfEvidence raw) [] keep askedOn Nothing Nothing status 1 (vcFindModel cfg) 0 now
              codeA <- (`codeAt` fst (rfLines raw)) <$> textOf (rfFile raw)
              let cands = [c | c <- samePairs vs raw' pl, vfStatus c `notElem` [Resolved, Gone]]
              match <- firstM cands $ \c -> do
                codeB <- (`codeAt` fst (plLines (vfPlace c))) <$> textOf (plFile (vfPlace c))
                a <- weighed (sameProblem (findingText (rfKind raw) (rfFile raw) (fst (rfLines raw)) rule (rfStatement raw)) (findingText (vfKind c) (plFile (vfPlace c)) (fst (plLines (vfPlace c))) (vfRule c) (vfStatement c)) codeA codeB)
                pure (pYes a >= sameCut)
              let nitpickOf r code = do
                    let q = nitpick st r code
                    a <- weighed q
                    pure (keepOf a, askedHash q pl)
              case match of
                Just c -> case matchStored (newF Nothing Open Nothing) c unitName of
                  Drop -> pure vs
                  Merge -> do
                    -- kept by this pass: its place, wording and evidence are the
                    -- ones just checked on the code as it is (id, status and
                    -- dismissals stay the stored finding's)
                    c' <- remerge st c {vfPlace = pl, vfStatement = rfStatement raw, vfEvidence = rfEvidence raw} textOf
                    prints <- citationsOk s p pcfg (rawOf c')
                    pure (replaceF c' {vfSeen = vfSeen c + 1, vfPrints = either (const (vfPrints c')) id prints} vs)
                  New -> addNew vs st0 raw' codeA nitpickOf newF pl
                Nothing -> addNew vs st0 raw' codeA nitpickOf newF pl
          )
          store0
          raws
      -- the store is written as the pass goes: each write lays the pass's version
      -- over the store as it is now, against the version the pass last wrote
      baseRef <- newIORef store0
      let writeStore vsW = do
            base <- readIORef baseRef
            _ <- updateInspectStore files root (\current -> (mergeInto base vsW current, ()))
            writeIORef baseRef vsW
          movedNow = do
            text2 <- fromMaybe "" <$> readText (prWorkspace p) rel
            units2 <- fromMaybe [] <$> safeUnits (unitsOf (pSyntax ports) lang (Just rel) text2)
            let fpsNow = M.fromList [(namedName n, fingerprint text2 (namedLines n)) | n <- units2]
            pure (S.fromList [namedName n | n <- units, M.lookup (namedName n) fpsNow /= Just (fingerprint text (namedLines n))])
          oldIds = S.fromList (map vfId (vsFindings store0))
          dropMoved moved vsD = vsD {vsFindings = [f | f <- vsFindings vsD, vfId f `S.member` oldIds || maybe True (`S.notMember` moved) (plUnit (vfPlace f))]}
      -- resolved by the find call's answer
      let vs2 = vs1 {vsFindings = [if vfId f `S.member` resolvedIds && vfStatus f `elem` [Open, Waiting] then f {vfStatus = Resolved} else f | f <- vsFindings vs1]}
      -- verify, best kept first, in rounds: a finding with no claim check passes at
      -- once and is stored (the editor shows it); the round's claim checks run side
      -- by side. The pass's new findings may take up to max_shown places; an older
      -- waiting one only while the margin has room. A check that fails frees its
      -- place for the next round.
      let oldIds0 = S.fromList [vfId f | f <- vsFindings store0, vfStatus f == Waiting]
          openBefore = length [f | f <- vsFindings vs2, plFile (vfPlace f) == rel, vfStatus f == Open, vfKind f `elem` vcKinds cfg]
          shown f = vfKind f `elem` vcKinds cfg
          needsClaim f = vfKind f `elem` ["bug", "assumption"]
          waiting = sortOn (Down . fromMaybe 0 . vfKeep) [f | f <- vsFindings vs2, plFile (vfPlace f) == rel, vfStatus f == Waiting]
          rounds vs _ _ [] = pure vs
          rounds vs openNow newPassed cands = do
            -- the round: candidates that would fit if all passed
            let fits (o, n, picked) f
                  | isNew && n >= vcMaxShown cfg = (o, n, picked)
                  | not isNew && o >= vcMaxShown cfg = (o, n, picked)
                  | otherwise = (o + c, n + (if isNew then c else 0), picked ++ [f])
                  where
                    isNew = vfId f `S.notMember` oldIds0
                    c = if shown f then 1 else 0
                (_, _, batch) = foldl fits (openNow, newPassed, []) cands
                rest = [f | f <- cands, vfId f `notElem` map vfId batch]
            if null batch
              then pure vs
              else do
                cited <- mapM (\f -> (f,) <$> citationsOk s p pcfg (rawOf f)) batch
                -- claim checks are reserved within the caps before any starts
                spent0 <- readIORef spentRef
                let e = claimEstimate records
                    reserve (r, ok) (f, Right _) | needsClaim f =
                      if spent0 + r + e > vcPassCost cfg || today + spent0 + r + e > vcDayCost cfg then (r, ok) else (r + e, ok ++ [vfId f])
                    reserve acc _ = acc
                    (_, toCheck) = foldl reserve (0, []) cited
                    settled = flip map cited $ \case
                      (f, Left why) -> f {vfStatus = Unsure, vfVerdict = Just why}
                      (f, Right prints)
                        | needsClaim f -> f {vfPrints = prints}
                        | otherwise -> f {vfPrints = prints, vfStatus = Open}
                when (length toCheck < length [() | (f, Right _) <- cited, needsClaim f]) $ do
                  modifyIORef' cappedRef (const True)
                  modifyIORef' dayCappedRef (|| today + spent0 + e > vcDayCost cfg)
                let vsA = foldr replaceF vs settled
                -- shown now, before the checks: what passed without one
                unless (null toCheck) $ do
                  moved <- movedNow
                  writeStore (dropMoved moved vsA)
                  vdProgress deps
                checked <- mapConcurrently (\f -> if vfId f `elem` toCheck then claimOf f else pure f) settled
                let vsB = foldr replaceF vsA checked
                    passedNow = [f | f <- checked, vfStatus f == Open, shown f]
                    newNow = length [f | f <- passedNow, vfId f `S.notMember` oldIds0]
                    unchecked = [f | f <- checked, vfStatus f == Waiting, vfId f `notElem` toCheck, needsClaim f]
                -- a check that failed frees its place; a capped one stops the rounds
                if null unchecked then rounds vsB (openNow + length passedNow) (newPassed + newNow) rest else pure vsB
          claimOf f = do
            let t = vfStatement f
                x = Sentence 0 0 0 (T.length t) Prose t "" Nothing Nothing
            r <- try (checkWith "inspect-check" s p pcfg (vdCheckerFor deps) True now rel (CheckAsk CheckClaim False []) x)
            case r of
              Left (_ :: SomeException) -> pure f
              Right (res, cost) -> do
                atomicModifyIORef' spentRef (\x0 -> (x0 + cost, ()))
                let verdictWord = either (const "failed") (const (statusName (claimVerdict res))) res
                pure f {vfStatus = claimVerdict res, vfVerdict = Just verdictWord, vfCost = vfCost f + cost}
      vs3 <- rounds vs2 openBefore (0 :: Int) waiting
      -- units gone from the file
      let names = S.fromList (map namedName units)
          vs4 = vs3 {vsFindings = [if plFile (vfPlace f) == rel && maybe False (`S.notMember` names) (plUnit (vfPlace f)) && vfStatus f `elem` [Open, Waiting, Withheld] then f {vfStatus = Gone} else f | f <- vsFindings vs3]}
      -- a unit whose fingerprint moved while the pass ran: its new findings drop, it stays uninspected
      movedNames <- movedNow
      let vs5 = dropMoved movedNames vs4
          inspectedNow = S.fromList [fingerprint text (namedLines n) | (ns, _) <- answered, n <- units, namedName n `elem` ns, namedName n `S.notMember` movedNames]
          inspectedNames = S.fromList [n | (ns, _) <- answered, n <- ns]
          vs6 =
            vs5
              { vsInspected = if S.null inspectedNow then vsInspected vs5 else M.insertWith S.union rel inspectedNow (vsInspected vs5)
              , vsCapped = cleanEmpty (M.adjust (`S.difference` inspectedNames) rel (if null capped then vsCapped vs5 else M.insertWith S.union rel (S.fromList capped) (vsCapped vs5)))
              }
      writeStore vs6
      spent <- readIORef spentRef
      wasCapped <- readIORef cappedRef
      dayCapped <- readIORef dayCappedRef
      pure (InspectDone (vs6 /= store0) wasCapped dayCapped spent)

    addNew vs st0 raw codeA nitpickOf newF _
      | st0 == Elsewhere = pure (appendF (newF Nothing Elsewhere Nothing) vs)
      | otherwise = do
          (keep, asked) <- nitpickOf raw codeA
          pure (appendF (newF (Just keep) (if keep >= vcCut cfg then Waiting else Withheld) (Just asked)) vs)

    -- a merged finding: a withheld one is asked again when what it was asked
    -- on changed; a refuted or unsure one is verified again when its prints moved
    remerge st c textOf = case vfStatus c of
      Withheld -> do
        code <- (`citedCode` fst (plLines (vfPlace c))) <$> textOf (plFile (vfPlace c))
        let r = rawOf c
            q = nitpick st r code
            h = askedHash q (vfPlace c)
        if Just h == vfAskedOn c
          then pure c
          else do
            keep <- keepOf <$> weighed q
            pure c {vfKeep = Just keep, vfAskedOn = Just h, vfStatus = if keep >= vcCut cfg then Waiting else Withheld}
      status | status `elem` [Refuted, Unsure] -> do
        prints <- citationsOk s p pcfg (rawOf c)
        pure (if either (const False) (/= vfPrints c) prints then c {vfStatus = Waiting} else c)
      _ -> pure c

    -- what the nitpick question was asked on: the unit, the rule's text (or
    -- the Rules section), the wording
    askedHash q pl = fileHash (T.intercalate "\0" [fromMaybe "" (plFingerprint pl), fromMaybe "" (lookup "project rule" (qContext q) <|> lookup "project rules" (qContext q)), T.pack (show (qShape q))])

    logFind now pack r parsed charged =
      let whole = either (const Nothing) Just r
          (status, why) = case (r, parsed) of
            (Left fl, _) -> (E.rfStatus fl, Just (E.rfWhy fl))
            (Right _, Left w) -> (Empty, Just w)
            (Right _, Right _) -> (Ok, Nothing)
       in appendRecord ports root . AnErrand $
            ErrandRecord
              { erId = errandId now "inspect" rel 0 (whole >>= rrSession), erAt = now, erVerb = "inspect", erPlan = rel, erRow = 0
              , erSection = T.intercalate "," (map fuName pack), erBranch = Nothing, erModel = vcFindModel cfg, erStatus = status, erWhy = why
              , erSession = whole >>= rrSession, erSeconds = rrSeconds <$> whole, erTurns = whole >>= rrTurns, erTokens = whole >>= rrTokens
              , erCost = if charged > 0 then Just charged else Nothing, erModels = maybe [] rrModels whole, erPromptChars = 0, erGrounded = 0
              , erBlocks = 0, erLines = 0, erDropped = 0, erUnresolved = 0, erChanged = 0, erDenials = maybe 0 rrDenials whole
              , erParent = Nothing, erAuto = True, erMode = Just "find", erVerdict = either (const Nothing) (Just . T.pack . show . length . frFindings) parsed, erWeb = False, erEscalated = False }

    memoText = do
      ref <- newIORef M.empty
      pure $ \f -> if not (readablePath fenced f) then pure "" else do
        m <- readIORef ref
        case M.lookup f m of
          Just t -> pure t
          Nothing -> do
            t <- fromMaybe "" <$> readText (prWorkspace p) f
            modifyIORef' ref (M.insert f t)
            pure t

-- | The unit's own lines.
cut :: Text -> Named -> Text
cut text n = let (a, z) = namedLines n in T.intercalate "\n" (take (z - a + 1) (drop (a - 1) (T.lines text)))

-- | The def line of the innermost unit that encloses this one, or empty.
headerOf :: Text -> [Named] -> Named -> Text
headerOf text units n =
  let (a, z) = namedLines n
      outer = sortOn (\m -> let (b, y) = namedLines m in y - b) [m | m <- units, namedName m /= namedName n, let (b, y) = namedLines m, b <= a, z <= y, (b, y) /= (a, z)]
   in maybe "" (\m -> T.strip (fromMaybe "" (listToMaybe (drop (namedDef m - 1) (T.lines text))))) (listToMaybe outer)

nonEmpty :: Text -> Maybe Text
nonEmpty t = if T.null t then Nothing else Just t

rawOf :: InspectFinding -> RawFinding
rawOf f = RawFinding (vfKind f) (plFile (vfPlace f)) (plLines (vfPlace f)) (vfStatement f) (vfRule f) (vfEvidence f)

firstM :: [a] -> (a -> IO Bool) -> IO (Maybe a)
firstM [] _ = pure Nothing
firstM (x : xs) ok = ok x >>= \yes -> if yes then pure (Just x) else firstM xs ok

replaceF :: InspectFinding -> InspectStore -> InspectStore
replaceF f vs = vs {vsFindings = [if vfId g == vfId f then f else g | g <- vsFindings vs]}

appendF :: InspectFinding -> InspectStore -> InspectStore
appendF f vs = vs {vsFindings = vsFindings vs ++ [f]}

cleanEmpty :: M.Map FilePath (Set Text) -> M.Map FilePath (Set Text)
cleanEmpty = M.filter (not . S.null)

-- | The pass's store (MINE, made from BASE) laid over the store as it is now:
-- a finding the pass changed is taken when the user left it as the pass found
-- it (a dismissal made meanwhile stands); new findings are added; one the
-- pass had written and since dropped goes.
mergeInto :: InspectStore -> InspectStore -> InspectStore -> InspectStore
mergeInto base mine current =
  InspectStore
    { vsFindings = [pick f | f <- vsFindings current, not (vfId f `M.member` baseIds && vfId f `M.notMember` mineIds)] ++ [f | f <- vsFindings mine, vfId f `M.notMember` currentIds, vfId f `M.notMember` baseIds]
    , vsInspected = M.unionWith S.union (vsInspected current) (vsInspected mine)
    , vsCapped = vsCapped mine
    }
  where
    baseIds = M.fromList [(vfId f, f) | f <- vsFindings base]
    currentIds = M.fromList [(vfId f, f) | f <- vsFindings current]
    mineIds = M.fromList [(vfId f, f) | f <- vsFindings mine]
    pick f = case (M.lookup (vfId f) baseIds, M.lookup (vfId f) mineIds) of
      (Just b, Just m) | vfStatus b == vfStatus f -> m
      _ -> f
