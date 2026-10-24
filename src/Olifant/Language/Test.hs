-- | The test port's vocabulary (spec §5.6): a runner executes a project's
-- tests, a parser reads what the framework printed. The processes live in
-- "Olifant.Language.Check" beside the compiler's and the verifier's.
module Olifant.Language.Test
  ( TestId
  , Failure (..)
  , TestRun (..)
  , Runner (..)
  , noRunner
  , scriptedRunner
  , scriptFileRunner
  , parsePytest
  , failureLine
  , assertionLine
  , gotExpected
  , testsForFile
  , keptVerdict
  , ranIn
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), eitherDecodeStrict)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Char (isAlphaNum)
import Data.Containers.ListUtils (nubOrd)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Olifant.Language.Adapter (Language, TestDef (..))
import System.Exit (ExitCode (..))
import System.FilePath (normalise, takeBaseName, takeDirectory)

-- | @path::name@, or a file.
type TestId = Text

-- | A test file's run, as a fill verified by its test OWN takes it, keeping KEEP
-- (tests that passed before; a sibling red otherwise is passed over): own
-- red (any of its cases, a parametrized one's `own[…]`) is its red; else
-- the first kept test that failed is (that test, the runner's failure);
-- else green while own passed. Own a file: every test of it must pass. A
-- kept test the run does not have is gone: not judged ('ranIn').
keptVerdict :: TestId -> [TestId] -> TestRun -> Either (Maybe TestId, Failure) ()
keptVerdict own keep run
  | not ("::" `T.isInfixOf` own), (_, f) : _ <- failed = Left (Nothing, f)
  | (_, f) : _ <- [(t, f) | (t, f) <- failed, isOwn t] = Left (Nothing, f)
  | (t, f) : _ <- [(t, f) | t <- keep, Just f <- [lookup t failed]] = Left (Just t, f)
  | "::" `T.isInfixOf` own, not (any isOwn (trPassed run)) = Left (Nothing, Failure Nothing (snd (T.breakOnEnd "::" own) <> " did not run: is it still in its test file?") "")
  | otherwise = Right ()
  where
    failed = trFailed run
    isOwn t = t == own || (own <> "[") `T.isPrefixOf` t

-- | Whether a run has a test (passed or failed).
ranIn :: TestRun -> TestId -> Bool
ranIn run t = t `elem` trPassed run || t `elem` map fst (trFailed run)

-- | Why a test is red: the line of the test that failed, what the framework
-- said (pytest's whole @E@ block, gutter stripped), and its whole section of
-- the report.
data Failure = Failure {fLine :: Maybe Int, fMessage :: Text, fOutput :: Text}
  deriving stock (Eq, Show)

-- | What a run found. @trBroken@ is a run that could not judge the tests (an
-- import error at collection, a timeout, no tests): not the same as red.
data TestRun = TestRun {trPassed :: [TestId], trFailed :: [(TestId, Failure)], trBroken :: Maybe Text}
  deriving stock (Eq, Show)

-- | Runs tests over FILES (a project's text), rooted where the language says;
-- never throws.
newtype Runner m = Runner {runTests :: Language -> [TestId] -> Map FilePath Text -> m (Either Text TestRun)}

-- | No runner at all: every run is refused.
noRunner :: Applicative m => Runner m
noRunner = Runner (\_ _ _ -> pure (Left "no test runner"))

-- | Verdicts by test id (tests): `"pass"`, or `{"line": N, "message": "…"}`
-- for a failure; an id the script does not name passes. A file's id runs
-- every test in it, each by its id as pytest reports it: those the file
-- defines (a top-level `def test…(`) and those the script names in it (a
-- file it is not given: the file's id alone).
scriptedRunner :: Applicative m => Map TestId Value -> Runner m
scriptedRunner script = Runner $ \_ ids files ->
  let under i = [k | k <- M.keys script, (i <> "::") `T.isPrefixOf` k]
      inFile i = case M.lookup (T.unpack i) files of
        Just text | not ("::" `T.isInfixOf` i) -> nubOrd ([i <> "::" <> n | Just n <- map defined (T.lines text)] ++ under i)
        _ -> []
      tests i = case inFile i of
        [] -> [i]
        ts -> ts
      failing i = [(k, f) | (k, v) <- M.toList script, k == i || (i <> "::") `T.isPrefixOf` k, Just f <- [failureIn v]]
      failed = concatMap failing ids
   in pure (Right (TestRun [t | i <- ids, t <- tests i, t `notElem` map fst failed, null (failing t) || t /= i] failed Nothing))
  where
    defined l = do
      rest <- T.stripPrefix "def " l
      let n = T.takeWhile (\c -> isAlphaNum c || c == '_') rest
      if "test" `T.isPrefixOf` n && "(" `T.isPrefixOf` T.drop (T.length n) rest then Just n else Nothing
    failureIn = \case
      Object o -> Just (Failure (KM.lookup "line" o >>= \case Number n -> toBoundedInteger n; _ -> Nothing) (case KM.lookup "message" o of Just (String m) -> m; _ -> "failed") (case KM.lookup "output" o of Just (String x) -> x; _ -> ""))
      _ -> Nothing

-- | 'scriptedRunner' over a JSON file, read again at every run (a test may
-- change what it says between runs); a file that does not read is a
-- runner that cannot start.
scriptFileRunner :: FilePath -> Runner IO
scriptFileRunner file = Runner $ \lang ids files ->
  (try (BS.readFile file) :: IO (Either SomeException BS.ByteString)) >>= \case
    Left e -> pure (Left ("the test script does not read: " <> T.pack (show e)))
    Right bytes -> case eitherDecodeStrict bytes of
      Right (Object o) -> runTests (scriptedRunner (M.fromList [(Key.toText k, v) | (k, v) <- KM.toList o])) lang ids files
      _ -> pure (Left ("the test script is not a JSON object: " <> T.pack file))

-- | A module's test file, from the language's @tests_for@: @{= dir =}@ and
-- @{= stem =}@ are the module's folder and name.
testsForFile :: TestDef -> FilePath -> FilePath
testsForFile td file =
  normalise (T.unpack (T.replace "{= stem =}" (T.pack (takeBaseName file)) (T.replace "{= dir =}" (T.pack (takeDirectory file)) (testsFor td))))

-- | pytest's @-q -rA --tb=short@ report: the summary lines say which tests
-- passed or failed, their failure sections where and why. A run with no
-- verdicts to read (collection errors, a usage error) is broken.
parsePytest :: ExitCode -> Text -> TestRun
parsePytest code out = TestRun passed failed broken
  where
    ls = T.lines out
    summary = [(status, rest) | l <- ls, Just (status, rest) <- [statusLine l]]
    idOf = T.takeWhile (/= ' ')
    passed = [idOf rest | ("PASSED", rest) <- summary]
    failed = [(tid, failureOf tid rest) | (s, rest) <- summary, s `elem` ["FAILED", "ERROR"], let tid = idOf rest]
    failureOf tid rest =
      let -- pytest heads a class method's section `TestX.test_y`
          name = T.replace "::" "." (T.drop 2 (snd (T.breakOn "::" tid)))
          section = takeWhile (not . isRule) (drop 1 (dropWhile (not . isHeaderOf name) ls))
          line = listToMaybe [n | l <- reverse section, Just n <- [lineOf (T.takeWhile (/= ':') tid) l]]
          said = case [T.stripStart (T.drop 1 l) | l <- section, "E " `T.isPrefixOf` l] of
            es@(_ : _) -> T.intercalate "\n" (map T.stripEnd es)
            [] -> case T.breakOn " - " rest of
              (_, r) | not (T.null r) -> T.strip (T.drop 3 r)
              _ -> ""
       in Failure line said (T.unlines section)
    broken
      | code == ExitFailure 5 = Just "no tests collected"
      | code == ExitSuccess = Nothing
      | null failed = Just (T.unlines (take 40 (filter (not . T.null) ls)))
      | code `notElem` [ExitFailure 1] = Just (T.unlines (take 40 (filter (not . T.null) ls)))
      | otherwise = Nothing
    statusLine l = case T.words l of
      (s : tid : _) | s `elem` ["PASSED", "FAILED", "ERROR"], "::" `T.isInfixOf` tid -> Just (s, T.drop (T.length s + 1) l)
      _ -> Nothing
    -- `___ test_name ___` starts a failure's section; `===` rules end the last
    isHeaderOf name l = isUnderscored l && (" " <> name <> " ") `T.isInfixOf` l
    isUnderscored l = "__" `T.isPrefixOf` l && "__" `T.isSuffixOf` T.stripEnd l
    isRule l = isUnderscored l || "==" `T.isPrefixOf` l
    lineOf file l = case T.breakOn ":" l of
      (f, rest) | f == file, Right (n, _) <- TR.decimal (T.drop 1 rest) -> Just n
      _ -> Nothing

-- | A failure in one line: the first line of its message less a leading
-- @AssertionError: @; when that is bare (@assert False@, @assert None@,
-- @AssertionError@, nothing), the first @+  where@ line, less that.
failureLine :: Failure -> Text
failureLine f
  | bare first, w : _ <- [T.strip r | l <- T.lines (fMessage f), Just r <- [T.stripPrefix "+  where " (T.strip l)]] = w
  | otherwise = first
  where
    first = headLine f
    bare l = l `elem` ["assert False", "assert None", "AssertionError", ""]

headLine :: Failure -> Text
headLine f = case T.lines (fMessage f) of
  l : _ -> let s = T.strip l in fromMaybe s (T.stripPrefix "AssertionError: " s)
  [] -> ""

-- | The test's source line that failed: the line after the first
-- @…: in NAME@ line of the section (NAME the test's short name).
assertionLine :: Text -> Failure -> Maybe Text
assertionLine name f = case drop 1 (dropWhile (not . (T.isSuffixOf (": in " <> name) . T.stripEnd)) (T.lines (fOutput f))) of
  l : _ | not (T.null (T.strip l)) -> Just (T.strip l)
  _ -> Nothing

-- | What came back and what was expected, for the two shapes the panel reads:
-- @assert A == B@ (A got, B expected) and @+  where False = isinstance(A, T)@.
gotExpected :: Failure -> Maybe (Text, Text)
gotExpected f
  | Just e <- T.stripPrefix "assert " (headLine f), (a, b) <- T.breakOn " == " e, not (T.null b) = Just (T.strip a, T.strip (T.drop 4 b))
  | i : _ <- [r | l <- T.lines (fMessage f), Just r <- [T.stripPrefix "+  where False = isinstance(" (T.strip l)], ")" `T.isSuffixOf` r] =
      let inner = T.dropEnd 1 i
          (a, t) = T.breakOnEnd ", " inner
       in if T.null a then Nothing else Just (T.strip (T.dropEnd 2 a), T.strip t)
  | otherwise = Nothing
