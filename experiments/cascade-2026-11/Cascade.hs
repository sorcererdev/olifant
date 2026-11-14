{-# LANGUAGE OverloadedStrings, LambdaCase #-}
-- | Plans m2c, the cascade experiment (not product code). Every check the prose-corpus gates
-- recorded for sonnet (src/Olifant/Server/testdata/prose-corpus-checks.json) is done again by
-- haiku under the same prompt and tools (a web check keeps web tools and no file tools, a code
-- check the reverse, ruling C), each reply goes through the engine's 'confirm', then one Jev
-- request asks three guard Nouls about the haiku verdict. The escalation rule (citation failed,
-- cannot_tell, or any guard under the threshold) is replayed at 0.6, 0.7 and 0.8 against the
-- recorded sonnet replies, and scored against the corpus labels.
--
-- CASCADE_LIVE=1 asks haiku and Jev for what the recordings here lack (haiku-checks.json,
-- guard-jev.json, guard-times.json); without it everything replays. It stops before a live
-- haiku check once the recorded haiku spend passes $4.
--
-- Since ruling 2c-2 (2026-11-14) the guards are the engine's own ('guardQuestions' over
-- 'guardEvidence': the lines around each cited line), and a citation failure that
-- 'citesOnlyUnreadable' keeps does not escalate. The first run's answers stay in
-- guard-jev.json under their own question keys; the second run's times are under "v2: KEY".
-- OLIFANT_PROSE_CORPUS names a checkout of electionnighttracker at 8488bb80.
module Main (main) where

import Control.Monad (forM, forM_, when)
import Data.Aeson (FromJSON (..), Value (..), eitherDecodeFileStrict, encode, withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.List (isPrefixOf, sort, sortOn)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import GHC.Clock (getMonotonicTime)
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..), PlansConfig (..))
import Olifant.Decide (Answer (..), Confidence (..), Decider (..), questionKey, replayDecider)
import Olifant.Decide qualified as D
import Olifant.Doctor (DoctorPorts (..))
import Olifant.Language (noChecker, noRunner, noSyntax)
import Olifant.Language.AstGrep (astGrepSyntax)
import Olifant.Language.Check (noVerifier)
import Olifant.Language.Grammar (GrammarFailure (NoSource))
import Olifant.Model (Backend (..))
import Olifant.Plans (Kind (Prose), Line (..), parsePlan)
import Olifant.Plans.Sentences (Sentence (..), sentenceKey, sentences)
import Olifant.Process (gitStateStamp, neverCancel)
import Olifant.Server (newServerState, projectFor)
import Olifant.Server.Cascade (citesOnlyUnreadable, guardEvidence, guardQuestions)
import Olifant.Server.Checker
import Olifant.Server.Errand (RunFailure (..), RunResult (..))
import Olifant.Server.State (ServerState (..))
import Olifant.Server.Triage
import Olifant.Workspace (diskWorkspace)
import System.Directory (doesFileExist, getTemporaryDirectory)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Text.Printf (printf)

here, corpusFile, jevRecording, sonnetFile, haikuFile, guardFile, timesFile :: FilePath
here = "experiments/cascade-2026-11"
corpusFile = "src/Olifant/Server/testdata/prose-corpus.json"
jevRecording = "src/Olifant/Server/testdata/prose-corpus-jev.json"
sonnetFile = "src/Olifant/Server/testdata/prose-corpus-checks.json"
haikuFile = here </> "haiku-checks.json"
guardFile = here </> "guard-jev.json"
timesFile = here </> "guard-times.json"

spendStop :: Double
spendStop = 4.00

thresholds :: [Double]
thresholds = [0.6, 0.7, 0.8]

-- the corpus entry, as ProseCorpusSpec reads it
data Entry = Entry
  { enId :: Text, enLine :: Text, enHeading :: Text, enLead :: Maybe Text, enKind :: Text
  , enVerdict :: Maybe Text, enContested :: Bool, enCode :: [FilePath], enCorpus :: Int
  , enProgress :: Maybe Text, enIdea :: Bool, enRows :: [Text]
  , enMustSay :: [[Text]], enMustNot :: [Text] }

instance FromJSON Entry where
  parseJSON = withObject "entry" $ \o ->
    Entry <$> o .: "id" <*> o .: "line" <*> o .: "heading" <*> o .:? "lead" <*> o .: "kind"
      <*> o .:? "verdict" <*> o .: "contested" <*> o .:? "code" .!= [] <*> o .: "corpus"
      <*> o .:? "progress" <*> o .:? "idea" .!= False <*> o .:? "rows" .!= []
      <*> o .:? "mustSay" .!= [] <*> o .:? "mustNotSay" .!= []

markedQuestions :: [Text]
markedQuestions = ["H1", "S2", "S3", "R1", "R8"]

stripped :: Entry -> Entry
stripped e = e {enId = enId e <> "s", enLine = fromMaybe (enLine e) (T.stripPrefix "- ? " (enLine e)), enCorpus = 6}

sentenceOf :: Entry -> Sentence
sentenceOf e =
  let ls = parsePlan (T.intercalate "\n" (enLine e : map ("  " <>) (enRows e)))
      ss = sentences ls
      kind = case ls of
        l : _ -> lnKind l
        [] -> Prose
   in Sentence 0 0 0 (T.length (enLine e)) kind (T.unwords (map snText ss)) (enHeading e) (enLead e) (case ss of (s : _) -> snAnchor s; [] -> Nothing)

plainPorts :: Ports
plainPorts = Ports (diskWorkspace "/") noSyntax (const noChecker) (const noVerifier) (const noRunner) (\_ _ -> noRunner) (pure (UTCTime (fromGregorian 2026 10 6) 0)) Nothing Nothing (DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False) (\_ -> pure Nothing) (\_ -> pure []) (\_ -> pure (Left NoSource))) (\_ -> pure Nothing) (\_ _ _ -> pure (Left "no base")) (\_ _ -> pure (Left "no base")) gitStateStamp

-- | One check the gates make: its sentence, what is asked, and the entries that share its key.
data Check = Check {ckKey :: Text, ckSentence :: Sentence, ckAsk :: CheckAsk, ckEntries :: [Entry]}

-- | One model's reply after 'confirm' (Nothing: the run or the parse failed), its cost and seconds.
-- 'dnKeep': a citation failure the cascade keeps ('citesOnlyUnreadable').
data Done = Done {dnConfirmed :: Maybe Confirmed, dnCost :: Double, dnSeconds :: Double, dnNote :: Text, dnKeep :: Bool}

data Guards = Guards {gWhy :: Double, gEvidence :: Double, gFollows :: Double, gSeconds :: Double}

main :: IO ()
main = do
  checkout <- maybe (hPutStrLn stderr "set OLIFANT_PROSE_CORPUS" >> exitFailure) pure =<< lookupEnv "OLIFANT_PROSE_CORPUS"
  live <- (== Just "1") <$> lookupEnv "CASCADE_LIVE"
  entries <- either fail pure =<< eitherDecodeFileStrict corpusFile
  tmp <- getTemporaryDirectory
  s0 <- newServerState plainPorts {pSyntax = astGrepSyntax (tmp </> "olifant-test-cache")}
  (s, p) <-
    projectFor (checkout </> "plans/corpus.md") s0 >>= \case
      (s, Just (_, p)) -> pure (s, p)
      _ -> fail "the corpus checkout does not load as a project"
  let cfg = (catalogPlans (prCatalog p)) {pcWeb = True, pcWebFetch = []}
      haikuCfg = cfg {pcCheckModel = "haiku"}
      liveJev = D.jevDecider neverCancel 30 (Jev "jev-1.14.0" "JEV_API_KEY") (hPutStrLn stderr . T.unpack)
      triageJev = replayDecider jevRecording Nothing
  -- the checks, as the two gates route them (Jev replayed)
  let codeEntries = filter ((/= 4) . enCorpus) entries
      all' = codeEntries ++ [stripped e | e <- codeEntries, enId e `elem` markedQuestions]
  codeChecks <- fmap catMaybes . forM all' $ \e -> do
    let x = sentenceOf e
    t <- triage False (Just triageJev) x
    pure $ case route False t of
      Verify -> Just (Check (sentenceKey (snText x)) x (CheckAsk CheckClaim False []) [e])
      Progress -> Just (Check (sentenceKey (snText x)) x (CheckAsk CheckProgress False []) [e])
      Answer -> Just (Check (sentenceKey (snText x)) x (CheckAsk CheckAnswer False []) [e])
      _ -> Nothing
  webChecks <- fmap catMaybes . forM [e | e <- entries, enCorpus e == 4] $ \e -> do
    let x = sentenceOf e
    t <- triage True (Just triageJev) x
    let r = route True t
        mode = case r of
          Progress -> CheckProgress
          Answer -> CheckAnswer
          _ -> CheckClaim
    pure $ if r `elem` [Verify, Answer, Progress]
      then Just (Check (sentenceKey (snText x)) x (CheckAsk mode (mode /= CheckProgress && useWeb True t) []) [e])
      else Nothing
  -- one check a key (a stripped twin, or Q2 routed to R2's step, shares its key and reply)
  let merged = foldl (\acc c -> case break ((== ckKey c) . ckKey) acc of
                        (a, m : b) -> a ++ [m {ckEntries = ckEntries m ++ ckEntries c}] ++ b
                        _ -> acc ++ [c]) [] (codeChecks ++ webChecks)
  printf "%d checks (%d code, %d web)\n" (length merged) (length [() | c <- merged, not (ckWeb (ckAsk c))]) (length [() | c <- merged, ckWeb (ckAsk c)])
  hFlush stdout
  let sonnet = replayChecker sonnetFile Nothing
      haiku c x = do
        spent <- recordedSpend haikuFile
        when (live && spent > spendStop) $ do
          hPutStrLn stderr (printf "haiku spend $%.3f past $%.2f: stopping" spent spendStop)
          exitFailure
        replayChecker haikuFile Nothing c x >>= \case
          Right r -> pure (Right r)
          Left f | live -> replayChecker haikuFile (Just (liveChecker neverCancel haikuCfg checkout)) c x
                 | otherwise -> pure (Left f)
      -- the three guards are one live Jev request (a batch shares one state); each answer is
      -- then written under its own key, so a replay is per question
      guardDecider = if live then batchRecorded guardFile liveJev else replayDecider guardFile Nothing
  rows <- forM merged $ \c -> do
    sd <- runOne s p cfg sonnet c
    hd0 <- runOne s p cfg haiku c
    keep <- maybe (pure False) (citesOnlyUnreadable s p cfg) (dnConfirmed hd0)
    let hd = hd0 {dnKeep = keep}
    g <- case dnConfirmed hd of
      Just conf | cfJudgement conf /= JCannotTell -> Just <$> guards s p live guardDecider c conf
      _ -> pure Nothing
    printf "%-5s %-7s web=%-5s sonnet=%-11s haiku=%-11s $%.3f %5.1fs guards=%s\n"
      (T.unpack (labelId c)) (T.unpack (checkModeName (ckMode (ckAsk c)))) (show (ckWeb (ckAsk c)))
      (verdictOf sd) (verdictOf hd) (dnCost hd) (dnSeconds hd) (maybe "-" showGuards g)
    hFlush stdout
    pure (c, sd, hd, g)
  report rows

-- | Replays when every question of the batch is recorded; else asks LIVE the whole batch in one
-- request and records each answer.
batchRecorded :: FilePath -> Decider IO -> Decider IO
batchRecorded file live = (replayDecider file Nothing) {weighAll = go}
  where
    go qs = do
      recorded <- recordedKeys
      if all ((`elem` recorded) . questionKey) qs
        then weighAll (replayDecider file Nothing) qs
        else do
          fresh <- weighAll live qs
          forM_ (zip qs fresh) $ \(q, a) -> case fst a of
            Unsure -> pure ()
            _ -> () <$ weigh (replayDecider file (Just (D.Decider (const (pure (fst a))) (mapM (const (pure (fst a)))) (const (pure a)) (mapM (const (pure a)))))) q
          pure fresh
    recordedKeys = doesFileExist file >>= \case
      False -> pure []
      True -> eitherDecodeFileStrict file >>= \case
        Right (Object o) | Just (Object rs) <- KM.lookup "answers" o -> pure (map Key.toText (KM.keys rs))
        _ -> pure []

labelId :: Check -> Text
labelId c = T.intercalate "/" (map enId (ckEntries c))

verdictOf :: Done -> String
verdictOf d = maybe "failed" (T.unpack . judgementName . cfJudgement) (dnConfirmed d)

showGuards :: Guards -> String
showGuards g = printf "why %.2f ev %.2f follows %.2f (%.1fs)" (gWhy g) (gEvidence g) (gFollows g) (gSeconds g)

recordedSpend :: FilePath -> IO Double
recordedSpend file = do
  there <- doesFileExist file
  if not there then pure 0 else
    eitherDecodeFileStrict file >>= \case
      Right (Object o) | Just (Object rs) <- KM.lookup "replies" o ->
        pure (sum [toRealFloat n | Object r <- KM.elems rs, Just (Number n) <- [KM.lookup "cost" r]])
      _ -> pure 0

runOne :: ServerState -> Project -> PlansConfig -> Checker -> Check -> IO Done
runOne s p cfg checker c =
  checker (ckAsk c) (ckSentence c) >>= \case
    Left f -> pure (Done Nothing 0.10 0 ("run failed: " <> T.pack (show (rfStatus f))) False)
    Right rr -> case parseCheckReply (ckAsk c) (rrText rr) of
      Left why -> pure (Done Nothing (fromMaybe 0.10 (rrCost rr)) (rrSeconds rr) ("no verdict: " <> why) False)
      Right reply -> do
        conf <- confirm s p (fences cfg) reply
        pure (Done (Just conf) (fromMaybe 0.10 (rrCost rr)) (rrSeconds rr) "" False)

-- | The engine's three guard Nouls ('guardQuestions'), one Jev request, over the haiku verdict
-- and the engine's evidence ('guardEvidence').
guards :: ServerState -> Project -> Bool -> Decider IO -> Check -> Confirmed -> IO Guards
guards s p live d c conf = do
  ev <- guardEvidence s p (ckAsk c) conf
  let qs = guardQuestions (ckAsk c) (ckSentence c) conf ev
  t0 <- getMonotonicTime
  as <- weighAll d qs
  t1 <- getMonotonicTime
  times <- readTimes
  secs <- case KM.lookup (Key.fromText timeKey) times of
    Just (Number n) -> pure (toRealFloat n)
    _ -> do
      when live $ BL.writeFile timesFile (encode (Object (KM.insert (Key.fromText timeKey) (Number (realToFrac (t1 - t0))) times)) <> "\n")
      pure (t1 - t0)
  case map yesP as of
    [w, e, f] -> pure (Guards w e f secs)
    _ -> pure (Guards 0 0 0 secs)
  where
    timeKey = "v2: " <> ckKey c
    readTimes = doesFileExist timesFile >>= \case
      False -> pure KM.empty
      True -> eitherDecodeFileStrict timesFile >>= \case
        Right (Object o) -> pure o
        _ -> pure KM.empty
    yesP (a, _) = case a of
      Yes (Confidence x) -> x
      No (Confidence x) -> 1 - x
      _ -> 0

-- the scoring --------------------------------------------------------------------------------

-- | Against the label of the check's labelled entry: Just (agrees, wrong-sure); Nothing when the
-- check has no label to score (contested, prose, unlabelled).
scored :: Check -> Maybe Confirmed -> Maybe (Bool, Bool)
scored c mc = listToMaybe (mapMaybe one (sortOn (\e -> enCorpus e /= 2 && enCorpus e /= 4) (ckEntries c)))
  where
    judged = judgementName . cfJudgement <$> mc
    lower = T.toLower
    answerText = maybe "" (\x -> lower (T.unwords (catMaybes [cfAnswer x, cfDetail x]))) mc
    saysAll e = all (any (\w -> lower w `T.isInfixOf` answerText)) (enMustSay e)
    saysNot e = any (\w -> lower w `T.isInfixOf` answerText) (enMustNot e)
    citedUnder e = maybe False (\x -> any (\ci -> any (`isPrefixOf` ciPath ci) (enCode e)) (cfEvidence x)) mc
    sourced = maybe False (not . null . cfSources) mc
    web = ckWeb (ckAsk c)
    one e
      | enCorpus e == 4 = Just $ case enKind e of
          "question" -> (judged == Just "answered" && saysAll e && not (saysNot e) && (not web || sourced), judged == Just "answered" && saysNot e)
          _ -> (judged == enVerdict e && (not web || sourced), maybe False (`elem` ["right", "wrong"]) judged && judged /= enVerdict e)
      | enId e `elem` markedQuestions || enId e `elem` map (<> "s") markedQuestions =
          Just (judged == Just "answered" && saysAll e && not (saysNot e) && citedUnder e, judged == Just "answered" && saysNot e)
      | Just pr <- enProgress e, ckMode (ckAsk c) == CheckProgress =
          Just (judged == Just pr, maybe False (`elem` ["done", "misfit"]) judged && judged /= Just pr)
      | enKind e == "claim", not (enContested e), Just v <- enVerdict e, enCorpus e == 2, ckMode (ckAsk c) == CheckClaim =
          let cited = case mc of
                Just x | v `elem` ["right", "wrong"] -> not (null (cfEvidence x)) && (null (enCode e) || any (\ci -> any (`isPrefixOf` ciPath ci) (enCode e)) (cfEvidence x))
                _ -> True
           in Just (judged == Just v && cited, maybe False (`elem` ["right", "wrong"]) judged && judged /= Just v)
      | otherwise = Nothing

escalates :: Double -> Done -> Maybe Guards -> Maybe Text
escalates t hd g = case dnConfirmed hd of
  Nothing -> Just "haiku failed"
  Just x
    | cfJudgement x == JCannotTell -> if dnKeep hd then Nothing else Just (if cfWhy x == "cited code not found" then "citation" else "cannot_tell")
    | otherwise -> case g of
        Nothing -> Just "no guards"
        Just gs ->
          let low = [n | (n, v) <- [("why-fits", gWhy gs), ("evidence-fits", gEvidence gs), ("verdict-follows", gFollows gs)], v < t]
           in if null low then Nothing else Just (T.intercalate "+" low)

median :: [Double] -> Double
median [] = 0
median xs = let ys = sort xs; n = length ys in if odd n then ys !! (n `div` 2) else (ys !! (n `div` 2 - 1) + ys !! (n `div` 2)) / 2

mean :: [Double] -> Double
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

report :: [(Check, Done, Done, Maybe Guards)] -> IO ()
report rows = do
  let sCost = mean [dnCost sd | (_, sd, _, _) <- rows]
      sMed = median [dnSeconds sd | (_, sd, _, _) <- rows]
      hCost = mean [dnCost hd | (_, _, hd, _) <- rows]
      hMed = median [dnSeconds hd + maybe 0 gSeconds g | (_, _, hd, g) <- rows]
      sonnetAgrees = [labelId c | (c, sd, _, _) <- rows, Just (True, _) <- [scored c (dnConfirmed sd)]]
      sonnetWrongSure = [labelId c | (c, sd, _, _) <- rows, Just (_, True) <- [scored c (dnConfirmed sd)]]
      haikuAgrees = [labelId c | (c, _, hd, _) <- rows, Just (True, _) <- [scored c (dnConfirmed hd)]]
      haikuWrongSure = [labelId c | (c, _, hd, _) <- rows, Just (_, True) <- [scored c (dnConfirmed hd)]]
      labelled = length [() | (c, sd, _, _) <- rows, isJust (scored c (dnConfirmed sd))]
  printf "\nsonnet: agrees %d/%d labelled, wrong-sure %s, mean $%.4f, median %.1fs over %d checks\n" (length sonnetAgrees) labelled (show sonnetWrongSure) sCost sMed (length rows)
  printf "haiku alone: agrees %d/%d, wrong-sure %s, mean $%.4f (no Jev), median %.1fs (with the guard request)\n" (length haikuAgrees) labelled (show haikuWrongSure) hCost hMed
  printf "bars: agree wherever sonnet did (%d), no new wrong-sure, mean <= $%.4f, median <= %.1fs\n" (length sonnetAgrees) (sCost / 2) (sMed * 1.5)
  forM_ thresholds $ \t -> do
    let per = [ (c, sd, hd, g, esc, final, cost, secs)
              | (c, sd, hd, g) <- rows
              , let esc = escalates t hd g
                    final = if isJust esc then dnConfirmed sd else dnConfirmed hd
                    cost = dnCost hd + (if isJust esc then dnCost sd else 0)
                    secs = dnSeconds hd + maybe 0 gSeconds g + (if isJust esc then dnSeconds sd else 0) ]
        lost = [labelId c | (c, _, _, _, _, final, _, _) <- per, labelId c `elem` sonnetAgrees, fmap fst (scored c final) /= Just True]
        newWS = [labelId c | (c, _, _, _, _, final, _, _) <- per, Just (_, True) <- [scored c final], labelId c `notElem` sonnetWrongSure]
        escN = length [() | (_, _, _, _, Just _, _, _, _) <- per]
        agreeN = length [() | (c, _, _, _, _, final, _, _) <- per, Just (True, _) <- [scored c final]]
        mc = mean [x | (_, _, _, _, _, _, x, _) <- per]
        md = median [x | (_, _, _, _, _, _, _, x) <- per]
        pass = null lost && null newWS && mc <= sCost / 2 && md <= sMed * 1.5
        -- haiku kept (not escalated) but wrong against a label: the guards missed it
        missed = [ (labelId c, g) | (c, _, hd, g, Nothing, _, _, _) <- per, Just (False, _) <- [scored c (dnConfirmed hd)]]
        -- escalated though haiku agreed with the label: spend the guards caused
        needless = [ (labelId c, e) | (c, _, hd, _, Just e, _, _, _) <- per, Just (True, False) <- [scored c (dnConfirmed hd)]]
    printf "\nthreshold %.1f: escalated %d/%d; agrees %d/%d; lost where sonnet agreed %s; new wrong-sure %s; mean $%.4f (%.0f%% of sonnet); median %.1fs (%.2fx sonnet) -> %s\n"
      t escN (length per) agreeN labelled (show lost) (show newWS) mc (100 * mc / sCost) md (md / sMed) (if pass then "PASS" else "fail" :: String)
    forM_ missed $ \(i, g) -> printf "  kept but off the label: %s guards %s\n" (T.unpack i) (maybe "-" showGuards g)
    forM_ needless $ \(i, e) -> printf "  escalated though haiku agreed: %s (%s)\n" (T.unpack i) (T.unpack e)
  printf "\nper check:\n"
  forM_ rows $ \(c, sd, hd, g) ->
    printf "%-9s %-8s web=%-5s sonnet=%-11s %-14s $%.3f %4.1fs | haiku=%-11s %-14s $%.3f %4.1fs | %s | %s\n"
      (T.unpack (labelId c)) (T.unpack (checkModeName (ckMode (ckAsk c)))) (show (ckWeb (ckAsk c)))
      (verdictOf sd) (score' c sd) (dnCost sd) (dnSeconds sd)
      (verdictOf hd) (score' c hd) (dnCost hd) (dnSeconds hd)
      (maybe "-" showGuards g) (T.unpack (dnNote hd <> maybe "" (\x -> " why: " <> T.take 160 (cfWhy x) <> maybe "" (\a -> " answer: " <> T.take 160 a) (cfAnswer x)) (dnConfirmed hd)))
  where
    score' :: Check -> Done -> String
    score' c d = case scored c (dnConfirmed d) of
      Nothing -> "unlabelled"
      Just (True, _) -> "agrees"
      Just (False, True) -> "WRONG-SURE"
      Just (False, False) -> "off"
