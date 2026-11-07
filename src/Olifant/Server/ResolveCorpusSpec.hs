-- | The resolver measured on the spike's corpus: electionnighttracker's
-- `plans/raw-votes.md` at 5ac7f05, every anchored claim and step with its
-- anchor stripped, resolved against the stripped plan over `nightvote/`,
-- with a recorded Jev (`OLIFANT_RECORD_JEV=1` records it again).
module Olifant.Server.ResolveCorpusSpec (spec) where

import Control.Exception (evaluate)
import Control.Monad (forM, forM_, when)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Clock (getMonotonicTime)
import Olifant.App (Ports (..))
import Olifant.Decide (replayDecider)
import Olifant.Language.AstGrep (astGrepSyntax)
import Olifant.LspSpec (plainPorts)
import Olifant.Plans (Anchor (..), Kind (..), Line (..), parsePlan)
import Olifant.Process (neverCancel)
import Olifant.Server (newServerState, projectFor)
import Olifant.Server.Resolve
import Olifant.Server.State (deciderUnder)
import System.Directory (copyFile, doesDirectoryExist, doesFileExist, getHomeDirectory, getTemporaryDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode, callCommand)
import Test.Hspec
import Text.Printf (printf)

recording :: FilePath
recording = "src/Olifant/Server/testdata/resolve-corpus-jev.json"

corpusCommit :: String
corpusCommit = "5ac7f05"

spec :: Spec
spec = describe "Olifant.Server.ResolveCorpus" $ describe "Resolve corpus" $
  it "resolves at least 55% of the spike's 76 lines right and at most 6% wrong and sure" $ do
    home <- getHomeDirectory
    checkout <- fromMaybe (home </> "projects/electionnighttracker") <$> lookupEnv "OLIFANT_RESOLVE_CORPUS"
    present <- doesDirectoryExist checkout
    (code, _, _) <- if present then readProcessWithExitCode "git" ["-C", checkout, "cat-file", "-e", corpusCommit] "" else pure (ExitFailure 1, "", "")
    if code /= ExitSuccess then pendingWith ("no corpus checkout at " <> corpusCommit) else measure checkout

measure :: FilePath -> IO ()
measure checkout = withSystemTempDirectory "olifant-corpus" $ \dir -> do
  callCommand ("git -C '" <> checkout <> "' archive " <> corpusCommit <> " nightvote plans/raw-votes.md .olifant/config.toml .olifant/languages | tar -x -C '" <> dir <> "'")
  planText <- TIO.readFile (dir </> "plans/raw-votes.md")
  let original = parsePlan planText
      corpus = [(l, UnitRef (anPath a) (anUnit a)) | l <- original, isClaimOrStep (lnKind l), Just a <- [lnAnchor l]]
      strip l = maybe (lnText l) (\a -> T.stripEnd (T.take (anStart a) (lnText l))) (lnAnchor l)
      strippedText = T.unlines (map strip original)
      stripped = parsePlan strippedText
  TIO.writeFile (dir </> "plans/raw-votes.md") strippedText
  recordLive <- (== Just "1") <$> lookupEnv "OLIFANT_RECORD_JEV"
  -- the corpus commit's config names no decide model; recording asks the
  -- one the checkout's own config names
  when recordLive (copyFile (checkout </> ".olifant/config.toml") (dir </> ".olifant/config.toml"))
  tmp <- getTemporaryDirectory
  s0 <- newServerState plainPorts {pSyntax = astGrepSyntax (tmp </> "olifant-test-cache")}
  (s, p) <-
    projectFor (dir </> "plans/raw-votes.md") s0 >>= \case
      (s, Just (_, p)) -> pure (s, p)
      _ -> fail "the corpus does not load as a project"
  i0 <- getMonotonicTime
  index <- projectIndex s p
  i1 <- getMonotonicTime
  file <-
    if recordLive
      then pure recording
      else do
        let f = dir </> "jev.json"
        exists <- doesFileExist recording
        when exists (copyFile recording f)
        pure f
  let live = if recordLive then deciderUnder neverCancel s p else Nothing
  when (recordLive && null live) (fail "OLIFANT_RECORD_JEV=1 but the corpus has no [decide] model")
  let d = replayDecider file live
  rows <- forM corpus $ \(l0, truth) -> do
    let l = stripped !! lnRow l0
        c = lineContext stripped l
    t0 <- getMonotonicTime
    ranked <- evaluate (let cs = candidates (lcLine c) index in sum (map snd cs) `seq` cs)
    t1 <- getMonotonicTime
    (r, ds) <- resolveLine d (pure True) c (map fst ranked)
    t2 <- getMonotonicTime
    pure (lcLine c, truth, map (upRef . fst) ranked, r, length ds, (t1 - t0) * 1000, t2 - t1)
  missed <- doesFileExist (file <> ".missed") >>= \case
    True -> lines <$> readFile (file <> ".missed")
    False -> pure []
  let n = length rows
      inEight = length [() | (_, t, cs, _, _, _, _) <- rows, t `elem` cs]
      right = length [() | (_, t, _, Resolved u _ _, _, _, _) <- rows, u == t]
      wrongSure = length [() | (_, t, _, Resolved u _ _, _, _, _) <- rows, u /= t]
      unresolved = n - right - wrongSure
      calls = sum [k | (_, _, _, _, k, _, _) <- rows]
      jevSeconds = sum [x | (_, _, _, _, _, _, x) <- rows]
      times = sort [x | (_, _, _, _, _, x, _) <- rows]
      median = if null times then 0 else times !! (length times `div` 2)
      worst = if null times then 0 else last times
  printf "corpus size          %d\n" n
  printf "units indexed        %d (%.1f s)\n" (length index) (i1 - i0)
  printf "truth in the 8       %d\n" inEight
  printf "right automatic      %d\n" right
  printf "wrong sure           %d\n" wrongSure
  printf "unresolved           %d\n" unresolved
  printf "Jev calls            %d\n" calls
  printf "Jev seconds          %.1f\n" jevSeconds
  printf "candidates ms        median %.1f, worst %.1f\n" median worst
  let py = [row | row@(_, t, _, _, _, _, _) <- rows, "nightvote/" `T.isPrefixOf` T.pack (urPath t), ".py" `T.isSuffixOf` T.pack (urPath t)]
  printf "nightvote Python     %d lines: right %d, wrong sure %d, truth in the 8 %d\n" (length py) (length [() | (_, t, _, Resolved u _ _, _, _, _) <- py, u == t]) (length [() | (_, t, _, Resolved u _ _, _, _, _) <- py, u /= t]) (length [() | (_, t, cs, _, _, _, _) <- py, t `elem` cs])
  forM_ [row | row@(_, t, cs, r, _, _, _) <- rows, t `notElem` cs || wrongPick t r] $ \(line, t, _, r, _, _, _) ->
    TIO.putStrLn ("  miss: " <> T.take 90 line <> " | truth " <> refText t <> " | " <> shown r)
  n `shouldBe` 76
  missed `shouldBe` []
  right * 100 `shouldSatisfy` (>= 55 * 76)
  wrongSure * 100 `shouldSatisfy` (<= 6 * 76)
  median `shouldSatisfy` (<= 50)
  where
    isClaimOrStep = \case
      Claim -> True
      Step _ -> True
      _ -> False
    wrongPick t = \case
      Resolved u _ _ -> u /= t
      _ -> False
    shown :: Resolution -> Text
    shown = \case
      Resolved u r x -> "picked " <> refText u <> " (" <> roundName r <> ", " <> T.pack (printf "%.2f" x) <> ")"
      Unresolved _ x -> "unresolved (" <> T.pack (printf "%.2f" x) <> ")"
