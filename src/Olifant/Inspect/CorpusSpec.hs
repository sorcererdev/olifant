-- | The inspect 1b replay gate on 1a's 58 bistro cases (design: "Replay tests
-- use replayDecider and replayChecker on the 1a recordings"). Each case is
-- the conventions base cloned, its patch applied (uncommitted), 1a's
-- steering file, and inspect on; every changed Python file goes through
-- 'inspectPass'. The find calls replay 1a's sonnet findings (split by file),
-- the claim checks 1a's verdicts, and Jev its recording
-- (`testdata/inspect-corpus-*.json`, written by `experiments/vet-1b-2026-11/finds.py`).
--
-- Runs only with OLIFANT_INSPECT_CORPUS=<notes>/experiments (the cases live
-- there). OLIFANT_RECORD_JEV=1 (with JEV_API_KEY) asks Jev what is not
-- recorded; OLIFANT_RECORD_CHECKS=1 asks the checker (sonnet) for a claim
-- 1a never checked. OLIFANT_INSPECT_TABLE=<file> writes the result table;
-- OLIFANT_INSPECT_CASES=vm02,b01 runs only those cases and prints their findings
-- (the bars then fail: a look, not a measure).
module Olifant.Inspect.CorpusSpec (spec) where

import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (Value (..), eitherDecodeFileStrict)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (nub, sort)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (utc)
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..))
import Olifant.Decide (jevDecider, recordMissing, replayDecider)
import Olifant.Language.AstGrep (astGrepSyntax)
import Olifant.LspSpec (plainPorts)
import Olifant.Model (Backend (..))
import Olifant.Plans.Sentences (Sentence (..), sentenceKey)
import Olifant.Process (neverCancel)
import Olifant.Server.Checker (liveChecker, readReplies, replayChecker)
import Olifant.Server.Errand (RunResult (..))
import Olifant.Server.State (newServerState, projectFor)
import Olifant.Inspect.Find (FindAsk (..), FindUnit (..))
import Olifant.Inspect.Pass (InspectDeps (..), inspectPass)
import Olifant.Inspect.Store (Place (..), InspectFinding (..), InspectStore (..), readInspectStore, shownIn)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, removeDirectoryRecursive)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import Test.Hspec

testdata :: FilePath
testdata = "src/Olifant/Inspect/testdata"

data Case = Case {cSet :: Text, cHits :: [Text], cPatch :: FilePath, cChanged :: [FilePath]}

spec :: Spec
spec = describe "inspect corpus (the 1b replay gate)" $
  it "meets the 1b bars on the 1a cases" $
    lookupEnv "OLIFANT_INSPECT_CORPUS" >>= \case
      Nothing -> pendingWith "set OLIFANT_INSPECT_CORPUS to <notes>/experiments"
      Just exp' -> gate exp'

gate :: FilePath -> IO ()
gate exp' = do
  recordJev <- (== Just "1") <$> lookupEnv "OLIFANT_RECORD_JEV"
  recordChecks <- (== Just "1") <$> lookupEnv "OLIFANT_RECORD_CHECKS"
  cases <- either fail (pure . casesOf) =<< eitherDecodeFileStrict (testdata </> "inspect-corpus-labels.json")
  finds <- either fail pure =<< eitherDecodeFileStrict (testdata </> "inspect-corpus-finds.json") :: IO (M.Map Text (M.Map FilePath Text))
  tmp <- getTemporaryDirectory
  let jevFile = testdata </> "inspect-corpus-jev.json"
      checksFile = testdata </> "inspect-corpus-checks.json"
      live = jevDecider neverCancel 60 (Jev "jev-1.14.0" "JEV_API_KEY") (const (pure ()))
      decider = if recordJev then recordMissing jevFile live else replayDecider jevFile Nothing
      base = exp' </> "code-checker-conventions-2026-11-14/base"
      steeringText = exp' </> "vet-1a-2026-11-15/vet.md"
  only <- maybe Nothing (Just . T.splitOn "," . T.pack) <$> lookupEnv "OLIFANT_INSPECT_CASES"
  rows <- forM [x | x@(cid, _) <- M.toList cases, maybe True (cid `elem`) only] $ \(cid, c) -> withSystemTempDirectory ("inspect-corpus-" <> T.unpack cid) $ \dir -> do
    callProcess "git" ["clone", "-q", base, dir </> "t"]
    let tree = dir </> "t"
    callProcess "git" ["-C", tree, "apply", exp' </> "code-checker-conventions-2026-11-14" </> cPatch c]
    doesDirectoryExist (tree </> ".olifant") >>= \there -> when there (removeDirectoryRecursive (tree </> ".olifant"))
    createDirectoryIfMissing True (tree </> ".olifant")
    TIO.readFile steeringText >>= TIO.writeFile (tree </> ".olifant/inspect.md")
    TIO.writeFile (tree </> ".olifant/config.toml") "languages = [\"python\"]\n[plans]\ncheck_model = \"sonnet\"\n[inspect]\nenabled = true\npass_cost = 5\nday_cost = 1000\n"
    let ports = plainPorts {pSyntax = astGrepSyntax (tmp </> "olifant-test-cache")}
        caseFinds = M.findWithDefault M.empty cid finds
        finder a = do
          let file = maybe "" fuFile (case faUnits a of u : _ -> Just u; [] -> Nothing)
              text = M.findWithDefault "{\"findings\": [], \"open\": []}" file caseFinds
          pure (Right (RunResult text 0 (Just 0.04) Nothing Nothing Nothing [] 0 []))
        checkerFor cfg ask x = do
          replies <- readReplies checksFile
          let known = KM.member (K.fromText (sentenceKey (snText x))) replies
          replayChecker checksFile (if not known && recordChecks then Just (liveChecker neverCancel cfg tree) else Nothing) ask x
    s0 <- newServerState ports
    shown <- newIORef []
    forM_ (cChanged c) $ \f ->
      projectFor (tree </> f) s0 >>= \case
        (s, Just (_, p)) -> do
          _ <- inspectPass neverCancel s p (InspectDeps decider finder checkerFor utc mempty (const (pure ()))) (tree </> f)
          vs <- either (fail . T.unpack) pure =<< readInspectStore (pFiles ports) tree
          let cfg = catalogInspect (prCatalog p)
          when (only /= Nothing) $ forM_ (vsFindings vs) $ \g -> putStrLn (T.unpack cid <> " " <> f <> " " <> show (vfStatus g, vfKind g, vfKeep g, vfVerdict g, plUnit (vfPlace g), plLines (vfPlace g)) <> " " <> T.unpack (T.take 90 (vfStatement g)))
          modifyIORef' shown (++ [(f, vfStatement g) | g <- shownIn cfg f vs])
        _ -> fail ("no project at " <> tree)
    found <- nub <$> readIORef shown
    let statements = map snd found
        hit = any (`elem` cHits c) statements
    pure (cid, cSet c, length statements, hit)
  let bySet k = [r | r@(_, s, _, _) <- rows, s == k]
      hits k = length [() | (_, _, _, True) <- bySet k]
      perCase k = fromIntegral (sum [n | (_, _, n, _) <- bySet k]) / fromIntegral (max 1 (length (bySet k))) :: Double
      table =
        T.unlines $
          ["| set | cases | with a hit shown | shown a case |", "|---|---|---|---|"]
            ++ ["| " <> k <> " | " <> tshow (length (bySet k)) <> " | " <> tshow (hits k) <> " | " <> T.pack (show (perCase k)) <> " |" | k <- ["V-must", "B", "T", "N", "V-taste"]]
            ++ ["", "| case | set | shown | hit |", "|---|---|---|---|"]
            ++ ["| " <> cid <> " | " <> s <> " | " <> tshow n <> " | " <> (if h then "yes" else "") <> " |" | (cid, s, n, h) <- sort rows]
  lookupEnv "OLIFANT_INSPECT_TABLE" >>= mapM_ (\f -> TIO.writeFile f table)
  TIO.putStrLn table
  (hits "V-must", hits "B") `shouldSatisfy` (\(v, b) -> v >= 16 && b >= 6)
  unless (perCase "T" <= 0.5 && perCase "N" <= 0.5) $ expectationFailure ("noise bar missed: " <> show (perCase "T", perCase "N"))
  where
    tshow = T.pack . show

casesOf :: Value -> M.Map Text Case
casesOf = \case
  Object o -> M.fromList [(K.toText k, c) | (k, Object v) <- KM.toList o, let c = caseOf v]
  _ -> M.empty
  where
    caseOf v =
      Case
        (str "set" v)
        [t | String t <- arr "hits" v]
        (T.unpack (str "patch" v))
        [T.unpack t | String t <- arr "changed" v]
    str k v = case KM.lookup k v of
      Just (String t) -> t
      _ -> ""
    arr k v = case KM.lookup k v of
      Just (Array xs) -> toList xs
      _ -> []
