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
  , testsForFile
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), eitherDecodeStrict)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Olifant.Language.Adapter (Language, TestDef (..))
import System.Exit (ExitCode (..))
import System.FilePath (normalise, takeBaseName, takeDirectory)

-- | @path::name@, or a file.
type TestId = Text

-- | Why a test is red: the line of the test that failed, what the framework
-- said (the assertion), and its whole section of the report.
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

-- | Verdicts by test id, never looking at the files (tests): `"pass"`, or
-- `{"line": N, "message": "…"}` for a failure. A file's id runs every
-- scripted test in it; an id the script does not name passes.
scriptedRunner :: Applicative m => Map TestId Value -> Runner m
scriptedRunner script = Runner $ \_ ids _ ->
  let failing i = [(k, f) | (k, v) <- M.toList script, k == i || (i <> "::") `T.isPrefixOf` k, Just f <- [failureIn v]]
      failed = concatMap failing ids
   in pure (Right (TestRun [i | i <- ids, null (failing i)] failed Nothing))
  where
    failureIn = \case
      Object o -> Just (Failure (KM.lookup "line" o >>= \case Number n -> toBoundedInteger n; _ -> Nothing) (case KM.lookup "message" o of Just (String m) -> m; _ -> "failed") "")
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
          said = case T.breakOn " - " rest of
            (_, r) | not (T.null r) -> T.strip (T.drop 3 r)
            _ -> maybe "" (T.strip . T.drop 1) (listToMaybe (reverse [l | l <- section, "E " `T.isPrefixOf` l]))
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
