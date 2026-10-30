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
  , failureLineIn
  , failureLineWith
  , headLineIn
  , assertionLineIn
  , gotExpectedIn
  , assertionLineWith
  , gotExpectedWith
  , failureOf
  , testsForFile
  , isTestFile
  , keptVerdict
  , ownedBy
  , ranIn
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), eitherDecodeStrict)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Containers.ListUtils (nubOrd)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Olifant.Language.TestUnits (TestUnit (..), testUnitsIn)
import Olifant.Language.Adapter (FailureDef (..), GotExpected (..), Language (..), TestDef (..), regexName)
import Text.Regex.TDFA (AllTextSubmatches (..), Regex, makeRegexM, matchM, (=~))
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
    isOwn = ownedBy own

-- | Whether test T is OWN's: OWN itself, one of its parameters
-- (pytest's @own[…]@), or a test of the group OWN (@own::…@).
ownedBy :: TestId -> TestId -> Bool
ownedBy own t = t == own || (own <> "[") `T.isPrefixOf` t || (own <> "::") `T.isPrefixOf` t

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
scriptedRunner script = Runner $ \lang ids files ->
  let under i = [k | k <- M.keys script, (i <> "::") `T.isPrefixOf` k]
      inFile i = case M.lookup (T.unpack i) files of
        Just text | not ("::" `T.isInfixOf` i) -> nubOrd ([i <> "::" <> n | n <- defined lang text] ++ under i)
        _ -> []
      tests i = case inFile i of
        [] -> [i]
        ts -> ts
      failing i = [(k, f) | (k, v) <- M.toList script, k == i || (i <> "::") `T.isPrefixOf` k, Just f <- [failureIn v]]
      failed = concatMap failing ids
   in pure (Right (TestRun [t | i <- ids, t <- tests i, t `notElem` map fst failed, null (failing t) || t /= i] failed Nothing))
  where
    -- the file's outermost tests by the language's `[test] unit` rules (a
    -- group's tests are not listed: the script names them)
    defined lang text = case langTest lang of
      Just td -> [n | TestUnit [n] _ <- testUnitsIn (testUnitRules td) (testAbove td) text, not (isGroup n text td)]
      Nothing -> []
    isGroup n text td = any (\u -> take 1 (tuPath u) == [n] && length (tuPath u) > 1) (testUnitsIn (testUnitRules td) (testAbove td) text)
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

-- | Whether FILE is another module's test file by the language's @tests_for@: its
-- path matches the template, and the stem it binds is not FILE's own (Rust's
-- module that holds its own tests is no test file).
isTestFile :: TestDef -> FilePath -> Bool
isTestFile td file = fromMaybe False $ do
  rx <- makeRegexM ("^" <> concatMap regexOf pieces <> "$") :: Maybe Regex
  AllTextSubmatches (_ : gs) <- matchM rx (normalise file) :: Maybe (AllTextSubmatches [] String)
  stem <- lookup True (zip [p == "stem" | p <- slots] gs)
  pure (stem /= takeBaseName file)
  where
    -- the template in pieces: @{= dir =}/@ (a folder, or none at the root), @{= stem =}@, literals
    pieces :: [Either String Char]
    pieces = go (T.unpack (testsFor td))
    go :: String -> [Either String Char]
    go str
      | Just rest <- stripStr "{= dir =}/" str = Left "dir" : go rest
      | Just rest <- stripStr "{= stem =}" str = Left "stem" : go rest
      | c : rest <- str = Right c : go rest
      | otherwise = []
    stripStr pre str = if take (length pre) str == pre then Just (drop (length pre) str) else Nothing
    slots = [p | Left p <- pieces]
    regexOf = \case
      Left "dir" -> "(.*/)?"
      Left _ -> "([^/]+)"
      Right c -> if c `elem` ("\\.[]()*+?{}|^$" :: String) then ['[', c, ']'] else [c]

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

-- | A failure in one line, by LANG's `[test.failure]`: its message's first
-- line ('headLineIn'); when that says nothing (empty, or `bare`), the first
-- line `explain` matches, by its last group.
failureLineIn :: Language -> Failure -> Text
failureLineIn = failureLineWith . failureOf

-- | 'failureLineIn' by the `[test.failure]` table alone.
failureLineWith :: Maybe FailureDef -> Failure -> Text
failureLineWith fd f
  | bare, w : _ <- [T.strip x | Just re <- [fd >>= fdExplain], l <- T.lines (fMessage f), Just x <- [regexName re (T.strip l)]] = w
  | otherwise = first
  where
    first = headLineWith fd f
    bare = T.null first || any (first =~) (maybe [] fdBare fd)

-- | A failure's message's first line, stripped, less LANG's `[test.failure]
-- head_strip` match at its front.
headLineIn :: Language -> Failure -> Text
headLineIn = headLineWith . failureOf

headLineWith :: Maybe FailureDef -> Failure -> Text
headLineWith fd f = case T.lines (fMessage f) of
  l : _ -> let x = T.strip l in maybe x (\re -> stripFront re x) (fd >>= fdHeadStrip)
  [] -> ""
  where
    stripFront re x = case x =~ re :: (Text, Text, Text) of
      (before, m, after) | T.null before, not (T.null m) -> after
      _ -> x

-- | The test's source line that failed: the line after the first output line
-- LANG's `[test.failure] line_after` (over @{= name =}@, NAME the test's short
-- name) matches; Nothing without one.
assertionLineIn :: Language -> Text -> Failure -> Maybe Text
assertionLineIn = assertionLineWith . failureOf

-- | 'assertionLineIn' by the `[test.failure]` table alone (a record kept without its language).
assertionLineWith :: Maybe FailureDef -> Text -> Failure -> Maybe Text
assertionLineWith fd name f = do
  after <- fd >>= fdLineAfter
  let re = T.replace "{= name =}" (literal name) after
  case drop 1 (dropWhile (\l -> not (T.stripEnd l =~ re)) (T.lines (fOutput f))) of
    l : _ | not (T.null (T.strip l)) -> Just (T.strip l)
    _ -> Nothing

-- | What came back and what was expected: the first of LANG's `[test.failure]
-- got_expected` entries that matches one of the message's lines (stripped, its
-- first by 'headLineIn'), its two groups; Nothing without one.
gotExpectedIn :: Language -> Failure -> Maybe (Text, Text)
gotExpectedIn = gotExpectedWith . failureOf

-- | 'gotExpectedIn' by the `[test.failure]` table alone.
gotExpectedWith :: Maybe FailureDef -> Failure -> Maybe (Text, Text)
gotExpectedWith fd f = listToMaybe [ge | g <- maybe [] fdGotExpected fd, l <- ls, Just ge <- [groups g l]]
  where
    ls = headLineWith fd f : map T.strip (drop 1 (T.lines (fMessage f)))
    groups g l = case getAllTextSubmatches (l =~ gePattern g) :: [Text] of
      m@(_ : _) | Just a <- at (geGot g) m, Just b <- at (geExpected g) m, not (T.null a) -> Just (a, b)
      _ -> Nothing
    at i m = T.strip <$> listToMaybe (drop i m)

-- | LANG's `[test.failure]`.
failureOf :: Language -> Maybe FailureDef
failureOf lang = langTest lang >>= testFailure

-- | TEXT as an expression matching itself.
literal :: Text -> Text
literal = T.concatMap (\c -> if c `elem` ("\\^$.|?*+()[]{}" :: String) then T.pack ['\\', c] else T.singleton c)
