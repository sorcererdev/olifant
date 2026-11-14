{-# LANGUAGE OverloadedStrings #-}
-- | The milestone-1 gate on the prose corpus (`testdata/prose-corpus.json`:
-- the user's own plan sentences, labelled from nightvote at 8488bb80). Each
-- entry is triaged (a recorded Jev), routed, and when routed to verify,
-- checked (a recorded checker) and confirmed against a checkout at that
-- commit. Pending unless `OLIFANT_PROSE_CORPUS` names such a checkout;
-- `OLIFANT_RECORD_CHECKS=1` asks Jev (`jev-1.14.0`, `JEV_API_KEY`) and the
-- checker live and records it (about $3; Jev too with `OLIFANT_RECORD_JEV=1`): the checker only for the
-- sentences the recording has no reply for, so a run that hits the suite's
-- per-example limit resumes where it stopped. Milestone 2a adds the step gate:
-- each entry routed to Progress gets the progress check, against the
-- builder's progress labels (done, open, misfit) and idea flags.
module Olifant.Server.ProseCorpusSpec (spec) where

import Control.Monad (forM, unless, when)
import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, withObject, (.!=), (.:), (.:?))
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..), PlansConfig (..))
import Olifant.Decide (recordMissing, replayDecider)
import Olifant.Decide qualified as D
import Olifant.Language.AstGrep (astGrepSyntax)
import Olifant.LspSpec (plainPorts)
import Olifant.Model (Backend (..))
import Olifant.Plans (Kind (Prose), Line (..), parsePlan)
import Olifant.Plans.Sentences (Sentence (..), sentences)
import Olifant.Process (neverCancel)
import Olifant.Server (newServerState, projectFor)
import Olifant.Server.Checker
import Olifant.Server.Errand (RunResult (..))
import Olifant.Server.Triage
import System.Directory (getTemporaryDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import Test.Hspec
import Text.Printf (printf)

corpusFile, jevRecording, checksRecording :: FilePath
corpusFile = "src/Olifant/Server/testdata/prose-corpus.json"
jevRecording = "src/Olifant/Server/testdata/prose-corpus-jev.json"
checksRecording = "src/Olifant/Server/testdata/prose-corpus-checks.json"

-- | Labelled claims the triage is known to set aside (user ruling 2026-11-09): A4, "We would
-- have to diagnose … and redeploy everything …", reads to Jev as process (about-code 0.67,
-- under 0.70). Asserted as Aside, so a change that fixes or breaks it shows.
knownMisses :: [Text]
knownMisses = ["A4"]

-- | Labelled steps the triage is known to set aside (user ruling 2026-11-12): R4, "For
-- displaying the values, we should make displaying columns configurable …", reads to Jev as not
-- about code (about-code 0.56), so it gets no progress check. It counts outside the step bar, as
-- A4 is outside the claim bar, and is asserted Aside.
knownStepMisses :: [Text]
knownStepMisses = ["R4"]

-- | Labelled claims whose reply is right but whose citation olifant cannot confirm (user ruling
-- 2026-11-12): H4's reply says wrong (as labelled) and cites desk.js:askAboutUnsaved, but olifant
-- has no JavaScript language, so the unit citation is voided and it reads cannot_tell. Outside
-- the "claims right with citation" bar, as A4 is outside the claim bar; asserted cannot_tell, so
-- a future JavaScript language makes this test speak up. Backlog: all languages.
knownCitationMisses :: [Text]
knownCitationMisses = ["H4"]

corpusCommit :: String
corpusCommit = "8488bb80"

-- | One labelled sentence: its line as written, its section and lead, its
-- label (claim | question | step | prose), and for a claim its verdict and
-- the code a right citation points into ([] = anywhere).
data Entry = Entry
  { enId :: Text, enLine :: Text, enHeading :: Text, enLead :: Maybe Text, enKind :: Text
  , enVerdict :: Maybe Text, enContested :: Bool, enCode :: [FilePath], enCorpus :: Int
  , enProgress :: Maybe Text, enIdea :: Bool, enRows :: [Text]
  , -- | 2b: a question's expected answer, the phrase groups an agreeing answer says (each
    -- group any-of, case-insensitive, all groups needed), the phrases only a contradicting
    -- one says; whether a question wants an answer (corpus 5: false); invented by the builder
    enExpect :: Maybe Text, enMustSay :: [[Text]], enMustNot :: [Text], enWantsAnswer :: Maybe Bool, enInvented :: Bool }

instance FromJSON Entry where
  parseJSON = withObject "entry" $ \o ->
    Entry <$> o .: "id" <*> o .: "line" <*> o .: "heading" <*> o .:? "lead" <*> o .: "kind"
      <*> o .:? "verdict" <*> o .: "contested" <*> o .:? "code" .!= [] <*> o .: "corpus"
      <*> o .:? "progress" <*> o .:? "idea" .!= False <*> o .:? "rows" .!= []
      <*> o .:? "expect" <*> o .:? "mustSay" .!= [] <*> o .:? "mustNotSay" .!= [] <*> o .:? "wantsAnswer" <*> o .:? "invented" .!= False

-- | A marked question (`- ? `) as a paragraph: Jev must find it. It shares the marked
-- line's sentence key, so it shares its recorded answer.
stripped :: Entry -> Entry
stripped e = e {enId = enId e <> "s", enLine = fromMaybe (enLine e) (T.stripPrefix "- ? " (enLine e)), enCorpus = 6}

-- | The entry as the pass would see it: the line's kind and its text with the
-- markers stripped (one sentence, even when the line holds two), the
-- labelled heading and lead.
sentenceOf :: Entry -> Sentence
sentenceOf e =
  -- a step's continuation rows (indented under it) are part of it
  let ls = parsePlan (T.intercalate "\n" (enLine e : map ("  " <>) (enRows e)))
      ss = sentences ls
      kind = case ls of
        l : _ -> lnKind l
        [] -> Prose
   in Sentence 0 0 0 (T.length (enLine e)) kind (T.unwords (map snText ss)) (enHeading e) (enLead e) (case ss of (s : _) -> snAnchor s; [] -> Nothing)

data Row = Row {rwEntry :: Entry, rwTriage :: Triage, rwRoute :: Route, rwVerdict :: Maybe Confirmed, rwCost :: Maybe Double}

-- | Corpus 2 is the user's sentences (27); 5 the questions that should not be answered; 6 the
-- stripped twins of corpus 2's marked questions (built here, not in the file).
markedQuestions :: [Text]
markedQuestions = ["H1", "S2", "S3", "R1", "R8"]

spec :: Spec
spec = describe "Olifant.Server.ProseCorpus" $ describe "the prose corpus gate" $ do
  it "meets the milestone-1 bars" $ withCheckout measure
  -- 2b: corpus 4 (sentences public sources settle) checked with web on
  it "meets the web bars" $ withCheckout webMeasure
  where
    withCheckout act =
      lookupEnv "OLIFANT_PROSE_CORPUS" >>= \case
        Nothing -> pendingWith "set OLIFANT_PROSE_CORPUS to a checkout of electionnighttracker at 8488bb80"
        Just checkout -> do
          (code, out, _) <- readProcessWithExitCode "git" ["-C", checkout, "rev-parse", "HEAD"] ""
          if code /= ExitSuccess || not (corpusCommit `isPrefixOf` out)
            then pendingWith ("the checkout at " <> checkout <> " is not at " <> corpusCommit)
            else act checkout

measure :: FilePath -> IO ()
measure checkout = do
  -- corpus 4 is the web gate's ('webMeasure')
  entries <- filter ((/= 4) . enCorpus) <$> (either fail pure =<< eitherDecodeFileStrict corpusFile)
  recordLive <- (== Just "1") <$> lookupEnv "OLIFANT_RECORD_CHECKS"
  recordJev <- (== Just "1") <$> lookupEnv "OLIFANT_RECORD_JEV"
  batch <- (== Just "1") <$> lookupEnv "OLIFANT_JEV_BATCH"
  tmp <- getTemporaryDirectory
  s0 <- newServerState plainPorts {pSyntax = astGrepSyntax (tmp </> "olifant-test-cache")}
  (s, p) <-
    projectFor (checkout </> "plans/corpus.md") s0 >>= \case
      (s, Just (_, p)) -> pure (s, p)
      _ -> fail "the corpus checkout does not load as a project"
  let cfg = catalogPlans (prCatalog p)
      liveJev = D.jevDecider neverCancel 30 (Jev "jev-1.14.0" "JEV_API_KEY") (hPutStrLn stderr . T.unpack)
      -- Jev's answers are kept as recorded; OLIFANT_RECORD_JEV=1 asks only the questions the
      -- recording lacks (a check recording, or a new Noul, must not move recorded triage)
      decider = if recordJev then recordMissing jevRecording liveJev else replayDecider jevRecording Nothing
      -- recording asks live only for the sentences the file has no reply for (a run
      -- stays under the suite's per-example limit; delete a reply to ask it again)
      replayed = replayChecker checksRecording Nothing
      checker mode x
        | recordLive = replayed mode x >>= \case
            Right r -> pure (Right r)
            Left _ -> replayChecker checksRecording (Just (liveChecker neverCancel cfg checkout)) mode x
        | otherwise = replayed mode x
  if batch then batchKind liveJev entries else do
    let all' = entries ++ [stripped e | e <- entries, enId e `elem` markedQuestions]
    rows <- forM all' $ \e -> do
      let x = sentenceOf e
      t <- triage False (Just decider) x
      let r = route False t
      let mode = case r of
            Progress -> CheckProgress
            Answer -> CheckAnswer
            _ -> CheckClaim
      if r `notElem` [Verify, Progress, Answer]
        then pure (Row e t r Nothing Nothing)
        else
          checker (CheckAsk mode False []) x >>= \case
            Left f -> do
              hPutStrLn stderr ("check failed: " <> T.unpack (enId e) <> ": " <> show f)
              pure (Row e t r Nothing Nothing)
            Right rr -> case parseCheckReply (CheckAsk mode False []) (rrText rr) of
              Left why -> do
                hPutStrLn stderr ("no verdict: " <> T.unpack (enId e) <> ": " <> T.unpack why)
                pure (Row e t r Nothing (rrCost rr))
              Right reply -> do
                c <- confirm s p (fences cfg) reply
                pure (Row e t r (Just c) (rrCost rr))
    let c2 = [r | r <- rows, enCorpus (rwEntry r) == 2]
        kindAgrees = length [() | r <- c2, trKind (rwTriage r) == enKind (rwEntry r)]
        claims = [r | r <- c2, enKind (rwEntry r) == "claim"]
        -- R6 is not about code; H5, a contested checkbox line, is a step by the grammar and
        -- leaves the bar (user ruling 2026-11-09)
        missedClaims = [enId (rwEntry r) | r <- claims, rwRoute r /= Verify, enId (rwEntry r) `notElem` ["R6", "H5"] ++ knownMisses]
        knownRoutes = [(enId (rwEntry r), rwRoute r) | r <- rows, enId (rwEntry r) `elem` knownMisses]
        nonClaimsChecked = [enId (rwEntry r) | r <- c2, enKind (rwEntry r) /= "claim", rwRoute r == Verify]
        uncontested = [r | r <- rows, enKind (rwEntry r) == "claim", not (enContested (rwEntry r)), isJust (enVerdict (rwEntry r))]
        judged r = judgementName . cfJudgement <$> rwVerdict r
        wrongVerdict = [enId (rwEntry r) | r <- uncontested, rwRoute r == Verify, judged r /= enVerdict (rwEntry r) || not (citedAt r), enId (rwEntry r) `notElem` knownCitationMisses]
        knownCitations = [(enId (rwEntry r), judged r) | r <- rows, enId (rwEntry r) `elem` knownCitationMisses]
        wrongSure = [enId (rwEntry r) | r <- uncontested, Just j <- [judged r], j `elem` ["right", "wrong"], Just j /= enVerdict (rwEntry r)]
        costs = [fromMaybe 0.10 (rwCost r) | r <- rows, rwRoute r == Verify]
        -- the step gate (2a)
        steps = [r | r <- rows, isJust (enProgress (rwEntry r))]
        stepTable = map stepLine steps
        known = [(enId (rwEntry r), judged r, maybe False (T.isInfixOf "Staff" . cfWhy) (rwVerdict r)) | r <- steps, enId (rwEntry r) `elem` ["H2", "R2", "X1", "S4"]]
        knownRight = length [() | (i, j, staff) <- known, if i == "S4" then j == Just "misfit" && staff else j == Just "done"]
        notIdeas = [r | r <- steps, not (enIdea (rwEntry r))]
        stepsOff = [enId (rwEntry r) | r <- notIdeas, judged r /= enProgress (rwEntry r), enId (rwEntry r) `notElem` knownStepMisses]
        knownStepRoutes = [(enId (rwEntry r), rwRoute r) | r <- steps, enId (rwEntry r) `elem` knownStepMisses]
        stepsWrongSure = [enId (rwEntry r) | r <- notIdeas, Just j <- [judged r], j `elem` ["done", "misfit"], Just j /= enProgress (rwEntry r)]
        ideasRouted = [enId (rwEntry r) | r <- steps, enIdea (rwEntry r), rwRoute r == Progress]
        stepCosts = [fromMaybe 0.10 (rwCost r) | r <- steps, rwRoute r == Progress]
        stepMean = if null stepCosts then 0 else sum stepCosts / fromIntegral (length stepCosts)
        meanCost = if null costs then 0 else sum costs / fromIntegral (length costs)
    let table = map rowLine rows ++ ["", "steps:"] ++ stepTable
    mapM_ (hPutStrLn stderr) table
    maybe (pure ()) (\f -> TIO.writeFile f (T.unlines (map T.pack table))) =<< lookupEnv "OLIFANT_PROSE_TABLE"
    hPutStrLn stderr (printf "steps: known %d/4; off (not ideas) %s; wrong-sure %s; ideas routed to a check %s; mean cost $%.3f over %d checks"
      knownRight (show stepsOff) (show stepsWrongSure) (show ideasRouted) stepMean (length stepCosts))
    hPutStrLn stderr (printf "kind agrees %d/%d; claims not routed (besides R6) %s; non-claims routed %s; verdict or citation off %s; wrong-sure %s; mean cost $%.3f over %d checks"
      kindAgrees (length c2) (show missedClaims) (show nonClaimsChecked) (show wrongVerdict) (show wrongSure) meanCost (length costs))
    when (length c2 /= 27) (expectationFailure "corpus 2 is 27 entries")
    unless (kindAgrees >= 25) (expectationFailure ("kind agrees on " <> show kindAgrees <> " of 27, under 25"))
    missedClaims `shouldBe` []
    -- a known miss stays visible: a change that routes it to a check (or anywhere else) fails here
    knownRoutes `shouldBe` [(k, Aside) | k <- knownMisses]
    unless (length nonClaimsChecked <= 1) (expectationFailure ("non-claims routed to a check: " <> show nonClaimsChecked))
    wrongVerdict `shouldBe` []
    knownCitations `shouldBe` [(k, Just "cannot_tell") | k <- knownCitationMisses]
    wrongSure `shouldBe` []
    unless (meanCost <= 0.12) (expectationFailure (printf "mean cost a check $%.3f, over $0.12" meanCost))
    -- the step gate
    unless (knownRight == 4) (expectationFailure ("known steps " <> show knownRight <> " of 4: " <> show known))
    unless (length stepsOff <= 1) (expectationFailure ("labelled steps off: " <> show stepsOff))
    knownStepRoutes `shouldBe` [(k, Aside) | k <- knownStepMisses]
    stepsWrongSure `shouldBe` []
    unless (length ideasRouted <= 2) (expectationFailure ("idea lines routed to a check: " <> show ideasRouted))
    -- the bar is $0.125 (user ruling 2026-11-12; the plan had $0.12)
    unless (stepMean <= 0.125) (expectationFailure (printf "mean cost a progress check $%.3f, over $0.125" stepMean))
    -- the question gate (2b): marked questions answered from the code, their stripped twins
    -- found by Jev, and the questions that should not be answered left alone
    let lower = T.toLower
        answerText r = maybe "" (\c -> lower (T.unwords (catMaybes [cfAnswer c, cfDetail c]))) (rwVerdict r)
        saysAll r = all (any (\w -> lower w `T.isInfixOf` answerText r)) (enMustSay (rwEntry r))
        saysNot r = any (\w -> lower w `T.isInfixOf` answerText r) (enMustNot (rwEntry r))
        citedUnder r = case rwVerdict r of
          Just c -> any (\x -> any (`isPrefixOf` ciPath x) (enCode (rwEntry r))) (cfEvidence c)
          Nothing -> False
        -- a question's answer: its marked row's, else its stripped twin's (one recorded answer)
        answerOf q = listToMaybe [r | r <- rows, enId (rwEntry r) `elem` [q, q <> "s"], isJust (rwVerdict r)]
        answered = [(q, answerOf q) | q <- markedQuestions]
        agrees r = judged r == Just "answered" && saysAll r && not (saysNot r) && citedUnder r
        answersAgree = [q | (q, Just r) <- answered, agrees r]
        answersWrongSure = [q | (q, Just r) <- answered, judged r == Just "answered", saysNot r]
        answerCosts = [fromMaybe 0.10 (rwCost r) | (_, Just r) <- answered]
        answerMean = if null answerCosts then 0 else sum answerCosts / fromIntegral (length answerCosts)
        strippedAnswered = [enId (rwEntry r) | r <- rows, enCorpus (rwEntry r) == 6, rwRoute r == Answer]
        shouldNotAnswered = [enId (rwEntry r) | r <- rows, enCorpus (rwEntry r) == 5, rwRoute r == Answer]
        questionRows = [r | r <- rows, enId (rwEntry r) `elem` markedQuestions || enCorpus (rwEntry r) `elem` [5, 6]]
        questionTable = map questionLine questionRows
    mapM_ (hPutStrLn stderr) ("" : "questions:" : questionTable)
    maybe (pure ()) (\f -> TIO.appendFile f (T.unlines (map T.pack ("" : "questions:" : questionTable)))) =<< lookupEnv "OLIFANT_PROSE_TABLE"
    hPutStrLn stderr (printf "questions: stripped routed to an answer %d/5 %s; should-not-answer routed %s; answers agree %d/5 %s; wrong-sure %s; mean cost $%.3f over %d answers"
      (length strippedAnswered) (show strippedAnswered) (show shouldNotAnswered) (length answersAgree) (show answersAgree) (show answersWrongSure) answerMean (length answerCosts))
    unless (length strippedAnswered >= 4) (expectationFailure ("stripped questions routed to an answer: " <> show strippedAnswered <> ", under 4 of 5"))
    unless (length shouldNotAnswered <= 1) (expectationFailure ("questions that should not be answered routed to an answer: " <> show shouldNotAnswered))
    unless (length answersAgree >= 4) (expectationFailure ("answers that agree, cited: " <> show answersAgree <> ", under 4 of 5"))
    answersWrongSure `shouldBe` []
    unless (answerMean <= 0.15) (expectationFailure (printf "mean cost an answer $%.3f, over $0.15" answerMean))
  where
    questionLine r =
      let e = rwEntry r
          t = rwTriage r
       in printf "%-4s %-9s %-7s wants=%s about=%.2f %-11s $%-5s %s"
            (T.unpack (enId e)) (if "- ? " `T.isPrefixOf` enLine e then "marked" else if enCorpus e == 5 then "no-answer" else "stripped" :: String)
            (T.unpack (routeName (rwRoute r))) (maybe "-" (printf "%.2f") (trWantsAnswer t) :: String) (trAboutCode t)
            (maybe "-" (T.unpack . judgementName . cfJudgement) (rwVerdict r)) (maybe "-" (printf "%.3f") (rwCost r) :: String)
            (maybe "" (T.unpack . fromMaybe "" . cfAnswer) (rwVerdict r))
    citedAt r = case (rwVerdict r, enVerdict (rwEntry r)) of
      (Just c, Just v) | v `elem` ["right", "wrong"] ->
        not (null (cfEvidence c)) && (null (enCode (rwEntry r)) || any (\x -> any (`isPrefixOf` ciPath x) (enCode (rwEntry r))) (cfEvidence c))
      _ -> True
    stepLine r =
      let e = rwEntry r
       in printf "%-3s label=%-6s idea=%-5s %-8s %-11s $%-5s %s"
            (T.unpack (enId e)) (maybe "-" T.unpack (enProgress e)) (show (enIdea e)) (T.unpack (routeName (rwRoute r)))
            (maybe "-" (T.unpack . judgementName . cfJudgement) (rwVerdict r)) (maybe "-" (printf "%.3f") (rwCost r) :: String)
            (maybe "" (T.unpack . cfWhy) (rwVerdict r))
    rowLine r =
      let e = rwEntry r
          t = rwTriage r
       in printf "%-3s %-8s %-8s p=%.2f about=%.2f %-8s %-12s label=%-11s %s"
            (T.unpack (enId e)) (T.unpack (enKind e)) (T.unpack (trKind t)) (trKindP t) (trAboutCode t) (T.unpack (routeName (rwRoute r)))
            (maybe "-" (T.unpack . judgementName . cfJudgement) (rwVerdict r)) (maybe "-" T.unpack (enVerdict e))
            (maybe "" (printf "$%.3f") (rwCost r) :: String)

-- | OLIFANT_JEV_BATCH=1: corpus 2's kind as production asks it, the live Jev's one request a
-- sentence with all four questions (web on), not the recording's one question at a time.
-- Jev only, no checks (about a cent); the number goes in the report.
batchKind :: D.Decider IO -> [Entry] -> IO ()
batchKind live entries = do
  let c2 = [e | e <- entries, enCorpus e == 2]
  ts <- forM c2 $ \e -> (e,) <$> triage True (Just live) (sentenceOf e)
  let agrees = [enId e | (e, t) <- ts, trKind t == enKind e]
      off = [(enId e, enKind e, trKind t) | (e, t) <- ts, trKind t /= enKind e]
  hPutStrLn stderr (printf "batch kind agrees %d/%d; off %s" (length agrees) (length c2) (show off))
  unless (length agrees >= 25) (expectationFailure ("batch kind agrees on " <> show (length agrees) <> " of 27, under 25"))

-- | The web gate (2b): `[plans] web = true`, `web_fetch = []` (the default a user turns on).
-- Every entry is triaged (to count the ones given web tools); only corpus 4 (ten sentences
-- public sources settle, labelled by the builder with their source) is checked, with the web
-- tools when 'useWeb'. The routing bars are asserted (corpus 4 routed to a check, web or code,
-- per ruling D; corpus 2 and 3 given web tools); quality, cost and wrong-sure are
-- reported (they set the `web` default, the user's ruling 2026-11-14).
webMeasure :: FilePath -> IO ()
webMeasure checkout = do
  entries <- either fail pure =<< eitherDecodeFileStrict corpusFile
  recordLive <- (== Just "1") <$> lookupEnv "OLIFANT_RECORD_CHECKS"
  recordJev <- (== Just "1") <$> lookupEnv "OLIFANT_RECORD_JEV"
  tmp <- getTemporaryDirectory
  s0 <- newServerState plainPorts {pSyntax = astGrepSyntax (tmp </> "olifant-test-cache")}
  (s, p) <-
    projectFor (checkout </> "plans/corpus.md") s0 >>= \case
      (s, Just (_, p)) -> pure (s, p)
      _ -> fail "the corpus checkout does not load as a project"
  let cfg = (catalogPlans (prCatalog p)) {pcWeb = True, pcWebFetch = []}
      liveJev = D.jevDecider neverCancel 30 (Jev "jev-1.14.0" "JEV_API_KEY") (hPutStrLn stderr . T.unpack)
      decider = if recordJev then recordMissing jevRecording liveJev else replayDecider jevRecording Nothing
      replayed = replayChecker checksRecording Nothing
      checker ask x
        | recordLive = replayed ask x >>= \case
            Right r -> pure (Right r)
            Left _ -> replayChecker checksRecording (Just (liveChecker neverCancel cfg checkout)) ask x
        | otherwise = replayed ask x
  rows <- forM entries $ \e -> do
    let x = sentenceOf e
    t <- triage True (Just decider) x
    let r = route True t
        mode = case r of
          Progress -> CheckProgress
          Answer -> CheckAnswer
          _ -> CheckClaim
        ask = CheckAsk mode (mode /= CheckProgress && useWeb True t) []
    if enCorpus e /= 4 || r `notElem` [Verify, Answer, Progress]
      then pure (Row e t r Nothing Nothing, False)
      else
        checker ask x >>= \case
          Left f -> do
            hPutStrLn stderr ("check failed: " <> T.unpack (enId e) <> ": " <> show f)
            pure (Row e t r Nothing Nothing, ckWeb ask)
          Right rr -> case parseCheckReply ask (rrText rr) of
            Left why -> do
              hPutStrLn stderr ("no verdict: " <> T.unpack (enId e) <> ": " <> T.unpack why)
              pure (Row e t r Nothing (rrCost rr), ckWeb ask)
            Right reply -> do
              c <- confirm s p (fences cfg) reply
              pure (Row e t r (Just c) (rrCost rr), ckWeb ask)
  let given (r, _) = useWeb True (rwTriage r) && rwRoute r `elem` [Verify, Answer]
      c4 = [x | x@(r, _) <- rows, enCorpus (rwEntry r) == 4]
      c23 = [x | x@(r, _) <- rows, enCorpus (rwEntry r) `elem` [2, 3]]
      c4Given = [enId (rwEntry r) | x@(r, _) <- c4, given x]
      c4Checked = [enId (rwEntry r) | (r, _) <- c4, rwRoute r `elem` [Verify, Answer, Progress]]
      c23Given = [enId (rwEntry r) | x@(r, _) <- c23, given x]
      judged r = judgementName . cfJudgement <$> rwVerdict r
      lower = T.toLower
      answerText r = maybe "" (\c -> lower (T.unwords (catMaybes [cfAnswer c, cfDetail c]))) (rwVerdict r)
      saysAll r = all (any (\w -> lower w `T.isInfixOf` answerText r)) (enMustSay (rwEntry r))
      saysNot r = any (\w -> lower w `T.isInfixOf` answerText r) (enMustNot (rwEntry r))
      sourced r = maybe False (not . null . cfSources) (rwVerdict r)
      rightWithSource r = sourced r && case enKind (rwEntry r) of
        "question" -> judged r == Just "answered" && saysAll r && not (saysNot r)
        _ -> judged r == enVerdict (rwEntry r)
      rightN = [enId (rwEntry r) | (r, _) <- c4, rightWithSource r]
      wrongSure = [enId (rwEntry r) | (r, _) <- c4, case enKind (rwEntry r) of
        "question" -> judged r == Just "answered" && saysNot r
        _ -> maybe False (`elem` ["right", "wrong"]) (judged r) && judged r /= enVerdict (rwEntry r)]
      webCosts = [fromMaybe unpriced (rwCost r) | (r, web) <- c4, web, isJust (rwCost r) || isJust (rwVerdict r)]
      unpriced = 0.10
      webMean = if null webCosts then 0 else sum webCosts / fromIntegral (length webCosts)
      table = map webLine c4
      webLine (r, web) =
        let t = rwTriage r
         in printf "%-4s %-8s jev=%s %.2f public=%s code=%.2f %-8s web=%-5s %-11s label=%-11s $%-5s %s"
              (T.unpack (enId (rwEntry r))) (T.unpack (enKind (rwEntry r))) (T.unpack (trKind t)) (trKindP t) (maybe "-" (printf "%.2f") (trAboutPublic t) :: String) (trAboutCode t)
              (T.unpack (routeName (rwRoute r))) (show web) (maybe "-" (T.unpack . judgementName . cfJudgement) (rwVerdict r))
              (maybe "-" T.unpack (enVerdict (rwEntry r))) (maybe "-" (printf "%.3f") (rwCost r) :: String)
              (maybe "" (\c -> T.unpack (T.intercalate "," (map (sourceDomain . soUrl) (cfSources c)) <> " | " <> fromMaybe (cfWhy c) (cfAnswer c))) (rwVerdict r))
  mapM_ (hPutStrLn stderr) ("" : "web:" : table)
  maybe (pure ()) (\f -> TIO.appendFile f (T.unlines (map T.pack ("" : "web:" : table)))) =<< lookupEnv "OLIFANT_PROSE_TABLE"
  hPutStrLn stderr (printf "web: corpus 4 routed to a check %d/10 %s; given web tools %d/10 %s; corpus 2 and 3 given web tools %d/%d %s; right with a source %d/10 %s; wrong-sure %s; mean cost a web check $%.3f over %d (default bar: <= $0.30 and >= 7/10: %s)"
    (length c4Checked) (show c4Checked) (length c4Given) (show c4Given) (length c23Given) (length c23) (show c23Given) (length rightN) (show rightN) (show wrongSure) webMean (length webCosts)
    (if webMean <= 0.30 && length rightN >= 7 then "met" else "missed" :: String))
  when (length c4 /= 10) (expectationFailure "corpus 4 is 10 entries")
  -- User ruling D (2026-11-14): the bar counts corpus 4 routed to a check, web or code. Ruling C
  -- sends a sentence the code settles too (W1, W8, W9) to the code check, so "given web tools"
  -- is reported, not asserted.
  unless (length c4Checked >= 8) (expectationFailure ("corpus 4 routed to a check: " <> show c4Checked <> ", under 8 of 10"))
  unless (length c23Given <= 1) (expectationFailure ("corpus 2 and 3 given web tools: " <> show c23Given <> ", over 1"))
