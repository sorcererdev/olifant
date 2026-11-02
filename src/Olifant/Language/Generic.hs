-- | Check report formats no one compiler owns: SARIF (what most linters and
-- analysers can write), an errorformat (a regex over a tool's lines), and
-- olifant-json (what a `convert` command writes). They read broken files;
-- only olifant-json can also say which cases a `case` lacks.
module Olifant.Language.Generic
  ( ErrorFormat (..)
  , LinesFormat (..)
  , JunitFormat (..)
  , noJunitFormat
  , sarifSupport
  , errorFormatSupport
  , olifantJsonSupport
  ) where

import Data.Aeson (FromJSON (..), Value, eitherDecodeStrict, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Olifant.Language.Diagnostics
import Text.Read (readMaybe)
import Text.Regex.TDFA (AllTextSubmatches (..), Regex, makeRegexM, matchM, matchTest)

-- | `[check.errorformat]`: a line of the tool's output matching 'efPattern'
-- is a problem, its file, line and message in those groups (1-based). With
-- 'efSeverity' and 'efError', it counts only when that group matches
-- 'efError'; without 'efError', every match counts.
data ErrorFormat = ErrorFormat {efPattern :: Text, efFile :: Int, efLine :: Int, efMessage :: Int, efSeverity :: Maybe Int, efError :: Maybe Text}
  deriving stock (Eq, Show)

-- | `[test.lines]`: a test report read line by line (hspec's checks, cargo
-- test). A line matching 'lfPass' or 'lfFail' is a test named by its last
-- group; with 'lfNest', under the lines above it at smaller indentation.
-- Each 'lfFailure' match (group 1 a path, group 2 a line) opens a failure
-- block, which runs to the next one or to an 'lfEnd' match, without lines
-- matching 'lfDrop'. (Here, not in "Olifant.Language.TestFormats", so the
-- adapter can hold it without an import cycle.)
data LinesFormat = LinesFormat {lfPass :: Text, lfFail :: Text, lfNest :: Bool, lfFailure :: Maybe Text, lfEnd :: Maybe Text, lfDrop :: Maybe Text, lfSplit :: Maybe Text}
  deriving stock (Eq, Show)

-- | `[test.junit]`: how a JUnit report's names become a test id. 'jfJoin'
-- splits the @classname@ into groups (without it the classname is no part
-- of the id); 'jfSplit' splits the @name@ (without it the name is one
-- segment).
data JunitFormat = JunitFormat {jfJoin :: Maybe Text, jfSplit :: Maybe Text}
  deriving stock (Eq, Show)

-- | Neither: the name alone is the id.
noJunitFormat :: JunitFormat
noJunitFormat = JunitFormat Nothing Nothing

-- | A support whose only knowledge is its parser: patterns are written as
-- reported, every constructor is visible, the declaration is the nearest
-- unindented line above.
generic :: Text -> (ByteString -> Either Text CheckReport) -> CompilerSupport
generic name parse =
  CompilerSupport
    { supportFormat = name
    , supportParse = parse
    , supportPattern = \_ _ _ p -> p
    , supportVisible = \_ _ _ -> True
    , supportUndefined = const Nothing
    , supportFunction = declarationAt
    , supportReadsStdout = True
    , supportSpans = Nothing
    }

named :: Text -> Either String a -> Either Text a
named format = either (\e -> Left (format <> ": " <> T.pack e)) Right

-- | SARIF 2.1: each result of level "error" is a broken file at its first
-- location's line, said as `ruleId: message`; other levels are ignored.
sarifSupport :: CompilerSupport
sarifSupport = generic "sarif" $ \raw -> named "sarif" $ do
  v <- eitherDecodeStrict raw
  results <- parseEither sarifResults v
  let errors = [(p, t, l) | (lvl, p, t, l) <- results, lvl == "error"]
  pure (Report [] [(p, t) | (p, t, _) <- errors] [] [((p, t), l) | (p, t, Just l) <- errors])

sarifResults :: Value -> Parser [(Text, FilePath, Text, Maybe Int)]
sarifResults = withObject "sarif" $ \o -> do
  runs <- o .:? "runs" .!= []
  concat <$> traverse (withObject "run" (\r -> r .:? "results" .!= [] >>= traverse result)) runs
  where
    result = withObject "result" $ \r -> do
      level <- r .:? "level" .!= "warning"
      rule <- r .:? "ruleId"
      msg <- r .: "message" >>= withObject "message" (.: "text")
      locs <- r .:? "locations" .!= []
      (path, line) <- case locs of
        loc : _ -> flip (withObject "location") loc $ \l -> do
          phys <- l .: "physicalLocation"
          flip (withObject "physicalLocation") phys $ \p -> do
            uri <- p .: "artifactLocation" >>= withObject "artifactLocation" (.: "uri")
            line <- p .:? "region" >>= maybe (pure Nothing) (withObject "region" (.:? "startLine"))
            pure (T.unpack (fromMaybe uri (T.stripPrefix "file://" uri)), line)
        [] -> fail "a result has no location"
      pure (level :: Text, path, maybe msg (\ru -> ru <> ": " <> msg) rule, line)

-- | Each line of the output (stdout, then stderr) that matches the pattern
-- and whose severity passes is a broken file at that line.
errorFormatSupport :: ErrorFormat -> CompilerSupport
errorFormatSupport ef = generic "errorformat" $ \raw -> case makeRegexM (T.unpack (efPattern ef)) :: Maybe Regex of
  Nothing -> Left ("errorformat: `pattern` is not a valid expression: " <> efPattern ef)
  Just rx -> do
    found <- catMaybes <$> traverse (line rx) (T.lines (TE.decodeUtf8With lenientDecode raw))
    pure (Report [] [(p, m) | (p, m, _) <- found] [] [((p, m), l) | (p, m, l) <- found])
  where
    severityPasses groups = case (efSeverity ef, efError ef) of
      (Just i, Just re) | Just s <- at i groups -> maybe False (`matchTest` s) (makeRegexM (T.unpack re) :: Maybe Regex)
      (Just _, Just _) -> False
      _ -> True
    at i groups = if i >= 0 && i < length groups then Just (groups !! i) else Nothing
    line rx l = case getAllTextSubmatches <$> (matchM rx (T.unpack l) :: Maybe (AllTextSubmatches [] String)) of
      Nothing -> Right Nothing
      Just groups
        | not (severityPasses groups) -> Right Nothing
        | otherwise -> case (at (efFile ef) groups, at (efLine ef) groups >>= readMaybe, at (efMessage ef) groups) of
            (Just f, Just n, Just m) -> Right (Just (f, T.strip (T.pack m), n))
            _ -> Left ("errorformat: the line `" <> l <> "` has no file, line number and message in groups " <> T.intercalate ", " (map (T.pack . show) [efFile ef, efLine ef, efMessage ef]))

-- | What a `convert` command writes:
-- @{"broken": [{"file", "message", "line"?}], "missing": [{"file", "case": [start, end], "patterns": [..], "type"?}]}@,
-- either key optional; `case` lines are 0-based.
olifantJsonSupport :: CompilerSupport
olifantJsonSupport = generic "olifant-json" $ \raw -> named "olifant-json" $ do
  v <- eitherDecodeStrict raw
  flip parseEither v $ withObject "report" $ \o -> do
    broken <- o .:? "broken" .!= [] >>= traverse (withObject "broken" (\b -> (,,) <$> b .: "file" <*> b .: "message" <*> b .:? "line"))
    missingPs <- o .:? "missing" .!= [] >>= traverse (withObject "missing" (\m -> do
      (s, e) <- m .: "case" >>= caseLines
      MissingPatterns <$> m .: "file" <*> pure (s, e) <*> m .: "patterns" <*> m .:? "type"))
    pure (Report missingPs [(f, t) | (f, t, _) <- broken] [] [((f, t), l) | (f, t, Just l) <- broken])
  where
    caseLines :: Value -> Parser (Int, Int)
    caseLines v = parseJSON v >>= \case
      [s, e] -> pure (s, e)
      _ -> fail "`case` is [start, end]"
