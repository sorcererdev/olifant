{-# LANGUAGE OverloadedStrings #-}
-- | The milestone-1 gate on the prose corpus (`testdata/prose-corpus.json`:
-- the user's own plan sentences, labelled from nightvote at 8488bb80). Each
-- entry is triaged (a recorded Jev), routed, and when routed to verify,
-- checked (a recorded checker) and confirmed against a checkout at that
-- commit. Pending unless `OLIFANT_PROSE_CORPUS` names such a checkout;
-- `OLIFANT_RECORD_CHECKS=1` asks Jev (`jev-1.14.0`, `JEV_API_KEY`) and the
-- checker live and records both (about $3).
module Olifant.Server.ProseCorpusSpec (spec) where

import Control.Monad (forM, unless, when)
import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, withObject, (.!=), (.:), (.:?))
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..))
import Olifant.Decide (replayDecider)
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

corpusCommit :: String
corpusCommit = "8488bb80"

-- | One labelled sentence: its line as written, its section and lead, its
-- label (claim | question | step | prose), and for a claim its verdict and
-- the code a right citation points into ([] = anywhere).
data Entry = Entry
  { enId :: Text, enLine :: Text, enHeading :: Text, enLead :: Maybe Text, enKind :: Text
  , enVerdict :: Maybe Text, enContested :: Bool, enCode :: [FilePath], enCorpus :: Int }

instance FromJSON Entry where
  parseJSON = withObject "entry" $ \o ->
    Entry <$> o .: "id" <*> o .: "line" <*> o .: "heading" <*> o .:? "lead" <*> o .: "kind"
      <*> o .:? "verdict" <*> o .: "contested" <*> o .:? "code" .!= [] <*> o .: "corpus"

-- | The entry as the pass would see it: the line's kind and its text with the
-- markers stripped (one sentence, even when the line holds two), the
-- labelled heading and lead.
sentenceOf :: Entry -> Sentence
sentenceOf e =
  let ls = parsePlan (enLine e)
      ss = sentences ls
      kind = case ls of
        l : _ -> lnKind l
        [] -> Prose
   in Sentence 0 0 0 (T.length (enLine e)) kind (T.unwords (map snText ss)) (enHeading e) (enLead e) (case ss of (s : _) -> snAnchor s; [] -> Nothing)

data Row = Row {rwEntry :: Entry, rwTriage :: Triage, rwRoute :: Route, rwVerdict :: Maybe Confirmed, rwCost :: Maybe Double}

spec :: Spec
spec = describe "Olifant.Server.ProseCorpus" $ describe "the prose corpus gate" $
  it "meets the milestone-1 bars" $
    lookupEnv "OLIFANT_PROSE_CORPUS" >>= \case
      Nothing -> pendingWith "set OLIFANT_PROSE_CORPUS to a checkout of electionnighttracker at 8488bb80"
      Just checkout -> do
        (code, out, _) <- readProcessWithExitCode "git" ["-C", checkout, "rev-parse", "HEAD"] ""
        if code /= ExitSuccess || not (corpusCommit `isPrefixOf` out)
          then pendingWith ("the checkout at " <> checkout <> " is not at " <> corpusCommit)
          else measure checkout

measure :: FilePath -> IO ()
measure checkout = do
  entries <- either fail pure =<< eitherDecodeFileStrict corpusFile
  recordLive <- (== Just "1") <$> lookupEnv "OLIFANT_RECORD_CHECKS"
  tmp <- getTemporaryDirectory
  s0 <- newServerState plainPorts {pSyntax = astGrepSyntax (tmp </> "olifant-test-cache")}
  (s, p) <-
    projectFor (checkout </> "plans/corpus.md") s0 >>= \case
      (s, Just (_, p)) -> pure (s, p)
      _ -> fail "the corpus checkout does not load as a project"
  let cfg = catalogPlans (prCatalog p)
      liveJev = D.jevDecider neverCancel 30 (Jev "jev-1.14.0" "JEV_API_KEY") (hPutStrLn stderr . T.unpack)
      decider = replayDecider jevRecording (if recordLive then Just liveJev else Nothing)
      checker = replayChecker checksRecording (if recordLive then Just (liveChecker neverCancel cfg checkout) else Nothing)
  rows <- forM entries $ \e -> do
    let x = sentenceOf e
    t <- triage (Just decider) x
    let r = route t
    if r /= Verify
      then pure (Row e t r Nothing Nothing)
      else
        checker CheckClaim x >>= \case
          Left f -> do
            hPutStrLn stderr ("check failed: " <> T.unpack (enId e) <> ": " <> show f)
            pure (Row e t r Nothing Nothing)
          Right rr -> case parseCheckReply CheckClaim (rrText rr) of
            Left why -> do
              hPutStrLn stderr ("no verdict: " <> T.unpack (enId e) <> ": " <> T.unpack why)
              pure (Row e t r Nothing (rrCost rr))
            Right reply -> do
              c <- confirm s p (fences cfg) reply
              pure (Row e t r (Just c) (rrCost rr))
  let table = map rowLine rows
  mapM_ (hPutStrLn stderr) table
  maybe (pure ()) (\f -> TIO.writeFile f (T.unlines (map T.pack table))) =<< lookupEnv "OLIFANT_PROSE_TABLE"
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
      wrongVerdict = [enId (rwEntry r) | r <- uncontested, rwRoute r == Verify, judged r /= enVerdict (rwEntry r) || not (citedAt r)]
      wrongSure = [enId (rwEntry r) | r <- uncontested, Just j <- [judged r], j `elem` ["right", "wrong"], Just j /= enVerdict (rwEntry r)]
      costs = [fromMaybe 0.10 (rwCost r) | r <- rows, rwRoute r == Verify]
      meanCost = if null costs then 0 else sum costs / fromIntegral (length costs)
  hPutStrLn stderr (printf "kind agrees %d/%d; claims not routed (besides R6) %s; non-claims routed %s; verdict or citation off %s; wrong-sure %s; mean cost $%.3f over %d checks"
    kindAgrees (length c2) (show missedClaims) (show nonClaimsChecked) (show wrongVerdict) (show wrongSure) meanCost (length costs))
  when (length c2 /= 27) (expectationFailure "corpus 2 is 27 entries")
  unless (kindAgrees >= 23) (expectationFailure ("kind agrees on " <> show kindAgrees <> " of 27, under 23"))
  missedClaims `shouldBe` []
  -- a known miss stays visible: a change that routes it to a check (or anywhere else) fails here
  knownRoutes `shouldBe` [(k, Aside) | k <- knownMisses]
  unless (length nonClaimsChecked <= 1) (expectationFailure ("non-claims routed to a check: " <> show nonClaimsChecked))
  wrongVerdict `shouldBe` []
  wrongSure `shouldBe` []
  unless (meanCost <= 0.12) (expectationFailure (printf "mean cost a check $%.3f, over $0.12" meanCost))
  where
    citedAt r = case (rwVerdict r, enVerdict (rwEntry r)) of
      (Just c, Just v) | v `elem` ["right", "wrong"] ->
        not (null (cfEvidence c)) && (null (enCode (rwEntry r)) || any (\x -> any (`isPrefixOf` ciPath x) (enCode (rwEntry r))) (cfEvidence c))
      _ -> True
    rowLine r =
      let e = rwEntry r
          t = rwTriage r
       in printf "%-3s %-8s %-8s p=%.2f about=%.2f %-8s %-12s label=%-11s %s"
            (T.unpack (enId e)) (T.unpack (enKind e)) (T.unpack (trKind t)) (trKindP t) (trAboutCode t) (T.unpack (routeName (rwRoute r)))
            (maybe "-" (T.unpack . judgementName . cfJudgement) (rwVerdict r)) (maybe "-" T.unpack (enVerdict e))
            (maybe "" (printf "$%.3f") (rwCost r) :: String)
