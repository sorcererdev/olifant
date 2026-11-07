-- | The resolver measured on the spike's corpus: electionnighttracker's
-- `plans/raw-votes.md` at 5ac7f05, every anchored claim and step with its
-- anchor stripped, resolved against the stripped plan over `nightvote/`,
-- with a recorded Jev (`OLIFANT_RECORD_JEV=1` records it again).
module Olifant.Server.ResolveCorpusSpec (spec) where

import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Clock (getMonotonicTime)
import Olifant.App (Ports (..))
import Olifant.Decide (Answer (..), Decider (..), Question (..), Shape (..), replayDecider)
import Olifant.App (Project (..))
import Olifant.Catalog (Catalog (..))
import Olifant.Language.Adapter (languageFor)
import Data.Char (isAlphaNum)
import Data.Functor ((<&>))
import Data.Maybe (isJust)
import Data.Set qualified as S
import System.FilePath (takeBaseName)
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
  it "keeps the measured baseline on the 76 lines: at least 31 right, at most 21 wrong and sure" $ do
    home <- getHomeDirectory
    checkout <- fromMaybe (home </> "projects/electionnighttracker") <$> lookupEnv "OLIFANT_RESOLVE_CORPUS"
    present <- doesDirectoryExist checkout
    (code, _, _) <- if present then readProcessWithExitCode "git" ["-C", checkout, "cat-file", "-e", corpusCommit] "" else pure (ExitFailure 1, "", "")
    if code /= ExitSuccess then pendingWith ("no corpus checkout at " <> corpusCommit) else measure checkout

-- | How a measurement asks: as committed (A); with every other line's anchor
-- kept, so the plan around a line carries its units (C); with each option's
-- name `path:Unit — header` (D). C and D are experiments, each recorded in
-- its own file (`OLIFANT_CORPUS_RECORDING`).
data Variant = Committed | AnchorsAround | NamedOptions
  deriving stock (Eq, Show)

measure :: FilePath -> IO ()
measure checkout = withSystemTempDirectory "olifant-corpus" $ \dir -> do
  callCommand ("git -C '" <> checkout <> "' archive " <> corpusCommit <> " nightvote plans/raw-votes.md .olifant/config.toml .olifant/languages | tar -x -C '" <> dir <> "'")
  planText <- TIO.readFile (dir </> "plans/raw-votes.md")
  variant <-
    lookupEnv "OLIFANT_CORPUS_VARIANT" <&> \case
      Just "C" -> AnchorsAround
      Just "D" -> NamedOptions
      _ -> Committed
  let original = parsePlan planText
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
  -- the corpus: anchored claims and steps whose anchor is a file in a
  -- language the project configures; the rest are outside its languages
  let anchored = [(l, UnitRef (anPath a) (anUnit a)) | l <- original, isClaimOrStep (lnKind l), Just a <- [lnAnchor l]]
      inLanguage (_, t) = isJust (languageFor (catalogLanguages (prCatalog p)) (urPath t))
      corpus = filter inLanguage anchored
      outside = filter (not . inLanguage) anchored
  i0 <- getMonotonicTime
  index <- projectIndex s p
  i1 <- getMonotonicTime
  recordingFile <- fromMaybe recording <$> lookupEnv "OLIFANT_CORPUS_RECORDING"
  file <-
    if recordLive
      then pure recordingFile
      else do
        let f = dir </> "jev.json"
        exists <- doesFileExist recordingFile
        when exists (copyFile recordingFile f)
        pure f
  let live = if recordLive then deciderUnder neverCancel s p else Nothing
  when (recordLive && null live) (fail "OLIFANT_RECORD_JEV=1 but the corpus has no [decide] model")
  let replayed = replayDecider file live
      d = if variant == NamedOptions then namedOptions replayed else replayed
      contextOf l0 = case variant of
        AnchorsAround ->
          -- only this line stripped; the lines around keep their anchors
          let plan = parsePlan (T.unlines [if lnRow x == lnRow l0 then strip x else lnText x | x <- original])
              l = plan !! lnRow l0
           in (lineContext plan l) {lcAround = rawAround plan l}
        _ -> lineContext stripped (stripped !! lnRow l0)
      resolveAll = mapM $ \(l0, truth) -> do
        let c = contextOf l0
        t0 <- getMonotonicTime
        ranked <- evaluate (let cs = candidates (lcLine c) index in sum (map snd cs) `seq` cs)
        t1 <- getMonotonicTime
        (r, ds) <- resolveLine d (pure True) c (map fst ranked)
        t2 <- getMonotonicTime
        pure (lcLine c, truth, map (upRef . fst) ranked, r, length ds, (t1 - t0) * 1000, t2 - t1)
  rows <- resolveAll corpus
  outsideRows <- resolveAll outside
  missed <- doesFileExist (file <> ".missed") >>= \case
    True -> lines <$> readFile (file <> ".missed")
    False -> pure []
  let n = length rows
      units = S.fromList (map upRef index)
      count f = length [() | row <- rows, f row]
      isRight (_, t, _, r, _, _, _) = case r of Resolved u _ _ -> u == t; _ -> False
      isWrong (_, t, _, r, _, _, _) = case r of Resolved u _ _ -> u /= t; _ -> False
      sureAbove x (_, t, _, r, _, _, _) = case r of Resolved u _ c -> u /= t && c > x; _ -> False
      near (_, t, _, r, _, _, _) = case r of Resolved u _ _ -> u /= t && nearRight t u; _ -> False
      named (line, t, _, r, _, _, _) = case r of Resolved u _ _ -> u /= t && namesUnit line u; _ -> False
      possible = [row | row@(_, t, _, _, _, _, _) <- rows, t `S.member` units]
      right = count isRight
      wrongSure = count isWrong
      unresolved = n - right - wrongSure
      inEight = count (\(_, t, cs, _, _, _, _) -> t `elem` cs)
      calls = sum [k | (_, _, _, _, k, _, _) <- rows]
      jevSeconds = sum [x | (_, _, _, _, _, _, x) <- rows]
      times = sort [x | (_, _, _, _, _, x, _) <- rows]
      median = if null times then 0 else times !! (length times `div` 2)
      worst = if null times then 0 else last times
      outsideResolved = length [() | (_, _, _, Resolved {}, _, _, _) <- outsideRows]
  printf "variant              %s\n" (show variant)
  printf "corpus size          %d (outside the project's languages: %d, resolved to a unit: %d)\n" n (length outside) outsideResolved
  printf "units indexed        %d (%.1f s)\n" (length index) (i1 - i0)
  printf "truth in the 8       %d\n" inEight
  printf "right automatic      %d\n" right
  printf "wrong sure           %d (above 0.8: %d, above 0.9: %d)\n" wrongSure (count (sureAbove 0.8)) (count (sureAbove 0.9))
  printf "  of them near-right %d (a method of the true class, or the class of a true method)\n" (count near)
  printf "  of them named      %d (the line names the picked unit too)\n" (count named)
  printf "unresolved           %d\n" unresolved
  printf "Jev calls            %d\n" calls
  printf "Jev seconds          %.1f\n" jevSeconds
  printf "candidates ms        median %.1f, worst %.1f\n" median worst
  printf "truth a unit         %d lines: right %d, wrong sure %d\n" (length possible) (length (filter isRight possible)) (length (filter isWrong possible))
  forM_ [row | row@(_, t, cs, r, _, _, _) <- rows, t `notElem` cs || wrongPick t r] $ \(line, t, _, r, _, _, _) ->
    TIO.putStrLn ("  miss: " <> T.take 90 line <> " | truth " <> refText t <> " | " <> shown r)
  if variant /= Committed
    then pendingWith ("a measurement (" <> show variant <> "), not the gate")
    else do
      -- regression guards at the measured baseline (33 right, 19 wrong and
      -- sure; Jev's noise about 2): the check is the gate against a wrong unit
      n `shouldBe` 76
      missed `shouldBe` []
      right `shouldSatisfy` (>= 31)
      wrongSure `shouldSatisfy` (<= 21)
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

-- | A pick one step from the truth: a method of the true class, or the class
-- of a true method (same file).
nearRight :: UnitRef -> UnitRef -> Bool
nearRight t u = urPath t == urPath u && ((urName t <> ".") `T.isPrefixOf` urName u || (urName u <> ".") `T.isPrefixOf` urName t)

-- | Whether a line names a unit: its last name part, its outer name, or its
-- file's stem as a whole word of the line.
namesUnit :: Text -> UnitRef -> Bool
namesUnit line u = any (`elem` ws) (filter (not . T.null) [T.takeWhileEnd (/= '.') (urName u), T.takeWhile (/= '.') (urName u), T.pack (takeBaseName (urPath u))])
  where
    ws = T.split (\c -> not (isAlphaNum c || c == '_')) line

-- | The three list lines either side, each without its indentation and
-- `- ` but with its anchor and verdict kept.
rawAround :: [Line] -> Line -> Text
rawAround ls l = T.intercalate "\n" (map raw (above ++ below))
  where
    listed = [x | x <- ls, isList (lnKind x)]
    above = reverse (take 3 (reverse [x | x <- listed, lnRow x < lnRow l]))
    below = take 3 [x | x <- listed, lnRow x > lnRow l]
    raw x = let t = T.stripStart (lnText x) in T.strip (fromMaybe t (T.stripPrefix "- " t))
    isList = \case
      Heading _ -> False
      Prose -> False
      _ -> True

-- | A decider asked with each choice option named `path:Unit — header` (its
-- description's first line), its answer named back.
namedOptions :: Decider IO -> Decider IO
namedOptions d = Decider (fmap fst . w) (traverse (fmap fst . w)) w
  where
    w q = case qShape q of
      Choice os -> do
        let renamed = [(k <> " — " <> T.strip (T.takeWhile (/= '\n') desc), (k, desc)) | (k, desc) <- os]
            back x = maybe x fst (lookup x renamed)
        (a, ps) <- weigh d q {qShape = Choice [(k', desc) | (k', (_, desc)) <- renamed]}
        let a' = case a of
              Chosen c fs conf -> Chosen (back c) fs conf
              other -> other
        pure (a', [(back o, x) | (o, x) <- ps])
      _ -> weigh d q
