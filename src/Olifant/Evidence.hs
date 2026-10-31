-- | Evidence for doctor: each slot of a language run on a sample of the
-- project's own files, so a row says what the rule (or provider) found, not
-- only which key provides it.
--
-- The sample is the first 'sampleSize' files, in path order, that match the
-- language's `files`. Units, test units, anchors and holes are proven on it;
-- tests only when doctor is asked to run them ('epRun').
module Olifant.Evidence
  ( Proof (..)
  , Evidence (..)
  , EvidencePorts (..)
  , sampleSize
  , sampleOf
  , evidenceFor
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (object, (.=))
import Data.Either (lefts)
import Data.List (nub, sort, sortOn)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Anchor (Located (..), Miss (..), Phrase (..), RenderedAnchor (..), parsePhrase, positionWords, resolveLayers)
import Olifant.Holes (Marker (..), placeholderFor)
import Olifant.Language
import Olifant.Language.TestUnits (TestUnit (..), testUnitsReport)
import Olifant.Server.Unit (Named (..), unitsOf)

-- | What a slot's run on the sample shows. 'Proven' carries the evidence;
-- 'Unproven' is "not seen" or "not run"; 'Disproven' carries the line or
-- reason that failed.
data Proof = Proven Text | Unproven Text | Disproven Text
  deriving stock (Eq, Show)

data Evidence = Evidence {evUnits, evTestUnits, evTests, evAnchors, evHoles :: Proof}
  deriving stock (Eq, Show)

-- | What evidence reads: the project's files (relative to its root), one
-- file's text, the syntax port, and a test runner (only with `doctor --run`).
data EvidencePorts = EvidencePorts
  { epFiles :: IO [FilePath]
  , epRead :: FilePath -> IO (Maybe Text)
  , epSyntax :: Syntax IO
  , epRun :: Maybe (Language -> [TestId] -> IO (Either Text TestRun))
  }

-- | How many files a sample holds.
sampleSize :: Int
sampleSize = 40

-- | The first 'sampleSize' files, in path order, that match the language's
-- `files`.
sampleOf :: Language -> [FilePath] -> [FilePath]
sampleOf lang files = take sampleSize (sort (nub [f | f <- files, any (`matchesGlob` f) (langFiles lang)]))

-- | Each slot of LANG run on a sample of the project's files.
evidenceFor :: EvidencePorts -> Language -> IO Evidence
evidenceFor ports lang = do
  files <- epFiles ports
  let sample = sampleOf lang files
      known = S.fromList files
  texts <- catMaybes <$> mapM (\p -> fmap (p,) <$> epRead ports p) sample
  units <- if hasUnits lang then mapConcurrently (\(p, t) -> (p,t,) <$> unitsOf (epSyntax ports) lang t) texts else pure []
  -- the sample's test files: its sources' `tests_for` files that exist, and
  -- any sampled file that holds test units itself (a module with its own tests)
  let td = langTest lang
      testsFor = case td of
        Just d -> [t | (p, _) <- texts, not (isTestFile d p), let t = testsForFile d p, S.member t known]
        Nothing -> []
      holding = [p | (p, t) <- texts, hasTestUnits lang, either (const False) (not . null) (testUnitsReport lang t)]
      testFiles = sort (nub (testsFor ++ holding))
  testTexts <- catMaybes <$> mapM (\p -> fmap (p,) <$> maybe (epRead ports p) (pure . Just) (lookup p texts)) testFiles
  -- anchors are tried in the sample's sources (another module's test file is none)
  anchors <- anchorsProof ports lang [(p, t, ns) | (p, t, Right ns) <- units, not (maybe False (`isTestFile` p) td)]
  holes <- holesProof ports lang
  tests <- testsProof ports lang (map fst testTexts)
  testUnits <- testUnitsProof ports lang testTexts
  pure (Evidence (unitsProof lang texts units) testUnits tests anchors holes)

-- | Units: how many in how many files, with one example; with none, the
-- first line the rule did not take, or why the slot failed.
unitsProof :: Language -> [(FilePath, Text)] -> [(FilePath, Text, Either Text [Named])] -> Proof
unitsProof lang texts results
  | not (hasUnits lang) = Unproven "no [unit] kinds"
  | null texts = Unproven ("not seen (no file matches " <> T.intercalate ", " (langFiles lang) <> ")")
  | otherwise = case [(p, n) | (p, _, Right ns) <- results, n <- ns] of
      found@((p, n) : _) -> Proven (count (length found) <> " found in " <> count (length texts) <> " files (e.g. " <> T.pack p <> ":" <> count (namedDef n) <> " " <> namedName n <> ")")
      [] -> case lefts [r | (_, _, r) <- results] of
        why : _ -> Disproven why
        [] -> Disproven ("0 found in " <> count (length texts) <> " files" <> maybe "" ("; " <>) (firstLineOf texts))

-- | Test units: how many in the sample's test files, with one example.
testUnitsProof :: EvidencePorts -> Language -> [(FilePath, Text)] -> IO Proof
testUnitsProof ports lang testTexts
  | not (hasTestUnits lang) = pure (Unproven "no [test] unit")
  | null testTexts = pure (Unproven ("not seen (no test file in the " <> count sampleSize <> " sampled)"))
  | otherwise =
      let reports = [(p, t, testUnitsReport lang t) | (p, t) <- testTexts]
       in case ([(p, u) | (p, _, Right us) <- reports, u <- us], lefts [r | (_, _, r) <- reports]) of
            (found@((p, u) : _), _) -> pure (Proven (count (length found) <> " found in " <> count (length [() | (_, _, Right (_ : _)) <- reports]) <> " test files (e.g. " <> T.pack p <> ":" <> count (fst (tuLines u)) <> " " <> fromMaybe "" (listToMaybe (reverse (tuPath u))) <> ")"))
            ([], why : _) -> pure (Disproven why)
            ([], []) -> do
              -- the first unit by the units slot in the first test file: a line the rule missed
              let (p, t) = case testTexts of
                    x : _ -> x
                    [] -> ("", "")
              first <- if hasUnits lang then unitsOf (epSyntax ports) lang t else pure (Left "")
              pure . Disproven . ("0 found; " <>) $ case first of
                Right (n : _) -> T.pack p <> ":" <> count (namedDef n) <> " is `" <> lineOf t (namedDef n) <> "`"
                _ -> fromMaybe (T.pack p <> " is empty") (firstLineOf [(p, t)])

-- | Tests: one sample test file run, with `doctor --run`.
testsProof :: EvidencePorts -> Language -> [FilePath] -> IO Proof
testsProof ports lang testFiles = case (langTest lang, epRun ports) of
  (Nothing, _) -> pure (Unproven "no [test]")
  (_, Nothing) -> pure (Unproven "not run (olifant doctor --run)")
  (Just td, Just run) -> case testFiles of
    [] -> pure (Unproven ("not run: no test file in the " <> count sampleSize <> " sampled"))
    f : _ ->
      run lang [T.pack f] >>= \case
        Left why -> pure (Disproven why)
        Right r | Just why <- trBroken r -> pure (Disproven (T.pack f <> ": " <> why))
        Right r ->
          pure (Proven ("ran " <> fromMaybe "the test command" (checkProgram (testCommand td)) <> " on " <> T.pack f <> ", " <> count (length (trPassed r) + length (trFailed r)) <> " results parsed (" <> testFormat td <> ")"))

-- | Holes: each kind's placeholder, put in its `wrap`, parses.
holesProof :: EvidencePorts -> Language -> IO Proof
holesProof ports lang = case [(k, h) | (k, h) <- langHoles lang, not (T.null (kindWrap h))] of
  [] -> pure (Unproven "no [holes.*]")
  kinds -> do
    results <- mapM check kinds
    pure $ case [(k, why) | (k, Just why) <- results] of
      (k, why) : _ -> Disproven ("[holes." <> k <> "]: " <> why)
      [] -> case [k | (k, Nothing) <- results] of
        ok -> Proven (T.intercalate ", " ok <> " wrappers parse with their placeholders")
  where
    check (k, h) = case placeholderFor lang (Marker "doctor" k "") "doctor" of
      Left why -> pure (k, Just why)
      Right ph -> do
        let wrapped = T.replace "{= fill =}" ph (T.replace "{= fill|indent =}" (indented ph) (kindWrap h))
        querySyntax (epSyntax ports) lang (object ["kind" .= ("ERROR" :: Text)]) wrapped >>= \case
          Right [] -> pure (k, Nothing)
          Right _ -> pure (k, Just (firstNonBlank wrapped <> " does not parse"))
          Left f -> pure (k, Just (describeSyntaxFailure f))
    indented f = T.intercalate "\n" [if T.null (T.strip l) then l else "    " <> l | l <- T.splitOn "\n" f]
    firstNonBlank t = fromMaybe "" (listToMaybe [T.strip l | l <- T.lines t, not (T.null (T.strip l))])

-- | Anchors: each declared target with at most one parameter, tried on the
-- sample's units' bare names until one resolves (at most 'triesPerTarget'
-- tries, the names whose unit mentions the target's words first). Targets
-- with more parameters are "not tried".
anchorsProof :: EvidencePorts -> Language -> [(FilePath, Text, [Named])] -> IO Proof
anchorsProof ports lang sources = case langTargets lang of
  [] -> pure (Unproven "no [targets.*]")
  targets -> do
    let (tryable, skipped) = (filter ((<= 1) . length . params) targets, filter ((> 1) . length . params) targets)
    tried <- mapConcurrently tryTarget tryable
    let proven = [found | Right found <- tried]
        failed = [(p, why) | (p, Left why) <- zip (map targetPhrase tryable) tried]
        text =
          T.intercalate "; " . filter (not . T.null) $
            [ T.intercalate " · " proven
            , T.intercalate " · " ["✗ " <> p <> ": " <> why | (p, why) <- failed]
            , if null skipped then "" else "not tried: " <> T.intercalate ", " (map targetPhrase skipped)
            ]
    pure $ case (proven, failed) of
      (_ : _, _) -> Proven text
      ([], _ : _) -> Disproven text
      ([], []) -> Unproven text
  where
    params t = [w | w <- targetShape t, isParam w]
    isParam w = T.all (\c -> c `elem` ['A' .. 'Z'] || c == '_') w && T.any (`elem` ['A' .. 'Z']) w
    -- the position the target's example uses (`end of`), else `end of`
    positionOf t = case parsePhrase (targetExample t) of
      Right (Phrase pos _) -> positionWords pos
      Left _ -> "end of"
    tryTarget t =
      let literal = [w | w <- targetShape t, not (isParam w), w /= "in"]
          candidates = case params t of
            [] -> [(p, txt, Nothing) | (p, txt, _) <- sources]
            _ ->
              map snd . sortOn fst $
                [ ((Down (all (`T.isInfixOf` spanText txt n) literal), i), (p, txt, Just (lastPart (namedName n))))
                | (i, (p, txt, n)) <- zip [0 :: Int ..] [(p, txt, n) | (p, txt, ns) <- sources, n <- ns]
                ]
          phraseFor name = T.unwords (positionOf t : [if isParam w then fromMaybe w name else w | w <- targetShape t])
          go lastMiss [] = pure (Left (fromMaybe "no unit in the sample to try" lastMiss))
          go _ ((p, txt, name) : rest) =
            resolveLayers (epSyntax ports) (Just lang) [PhraseAnchor (phraseFor name)] txt >>= \case
              Right loc -> pure (Right (phraseFor name <> " (" <> T.pack p <> ":" <> count (locLine loc + 1) <> ")"))
              Left misses -> go (Just (T.intercalate "; " (map missReason misses))) rest
       in go Nothing (take triesPerTarget (nubOn (\(_, _, n) -> n) candidates))
    lastPart = last . T.splitOn "."
    spanText txt n = let (a, z) = namedLines n in T.unlines (take (z - a + 1) (drop (a - 1) (T.lines txt)))
    nubOn f = go S.empty
      where
        go _ [] = []
        go seen (x : xs) = if S.member (f x) seen then go seen xs else x : go (S.insert (f x) seen) xs

-- | How many names an anchors proof tries per target.
triesPerTarget :: Int
triesPerTarget = 6

count :: Int -> Text
count = T.pack . show

-- | Line N (1-based) of a text, stripped.
lineOf :: Text -> Int -> Text
lineOf t n = maybe "" T.strip (listToMaybe (drop (n - 1) (T.lines t)))

-- | The first non-blank line of the first file that has one: `path:N is `line``.
firstLineOf :: [(FilePath, Text)] -> Maybe Text
firstLineOf texts = listToMaybe [T.pack p <> ":" <> count i <> " is `" <> T.strip l <> "`" | (p, t) <- texts, (i, l) <- zip [1 ..] (T.lines t), not (T.null (T.strip l))]
