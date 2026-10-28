-- | Test reports any framework can write (spec §5.6, layer 2): `lines` (a
-- regex per line: hspec's checks, cargo test), TAP, JUnit XML, and
-- olifant-json. A parser says each test as the report names it
-- ('Reported'); 'attribute' turns that name into a test id under one of the
-- files the run asked for.
module Olifant.Language.TestFormats
  ( LinesFormat (..)
  , Reported (..)
  , parseLines
  , parseTap
  , parseJunit
  , parseTestJson
  , attribute
  , fromReported
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (Value, eitherDecodeStrict, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import Data.List (find, isSuffixOf)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language.Generic (LinesFormat (..))
import Olifant.Language.Test (Failure (..), TestId, TestRun (..))
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, normalise, splitDirectories)
import Text.Read (readMaybe)
import Text.Regex.TDFA (AllTextSubmatches (..), Regex, makeRegexM, matchM, matchTest)
import Text.XML.Light (QName (..), elChildren, elName, findAttr, parseXMLDoc, strContent)

-- | One test as a report says it: the file it names (when it does), its
-- name's segments (enclosing groups, then its own), and why it failed (no
-- failure: it passed).
data Reported = Reported {rpPath :: Maybe FilePath, rpSegments :: [Text], rpFailure :: Maybe Failure}
  deriving stock (Eq, Show)

compiled :: Text -> Maybe Regex
compiled re = makeRegexM (T.unpack re)

groupsOf :: Regex -> Text -> Maybe [Text]
groupsOf rx l = map T.pack . getAllTextSubmatches <$> (matchM rx (T.unpack l) :: Maybe (AllTextSubmatches [] String))

-- | The last group of a match (the whole match when there is none), stripped.
lastGroup :: Regex -> Text -> Maybe Text
lastGroup rx l = groupsOf rx l >>= \gs -> case T.strip (last gs) of
  "" -> Nothing
  n -> Just n

indentOf :: Text -> Int
indentOf = T.length . T.takeWhile (== ' ')

dedent :: [Text] -> Text
dedent ls = T.intercalate "\n" (map (T.drop n) ls)
  where
    n = if null ls then 0 else minimum (map indentOf ls)

-- | Every test line of a `lines` report, in order; the k-th failure block
-- belongs to the k-th failed test.
parseLines :: LinesFormat -> Text -> [Reported]
parseLines lf out = attach blocks tests
  where
    ls = zip [0 :: Int ..] (map (T.dropWhileEnd (== '\r')) (T.lines out))
    matcher = maybe (const False) matchTest . (>>= compiled)
    passRx = compiled (lfPass lf)
    failRx = compiled (lfFail lf)
    isEnd = matcher (lfEnd lf)
    isDrop = matcher (lfDrop lf)
    failureRx = lfFailure lf >>= compiled
    firstEnd = listToMaybe [i | (i, l) <- ls, isEnd l]
    testAt l = case (passRx >>= (`lastGroup` l), failRx >>= (`lastGroup` l)) of
      (Just n, _) -> Just (n, False)
      (_, Just n) -> Just (n, True)
      _ -> Nothing
    tests = [(i, l, n, failed) | (i, l) <- ls, maybe True (i <) firstEnd, Just (n, failed) <- [testAt l]]
    -- the enclosing groups: for each smaller indentation, the nearest line
    -- above that is neither blank nor a test
    parents i l = go (indentOf l) (reverse (take i ls))
      where
        go 0 _ = []
        go _ [] = []
        go bound ((_, p) : rest)
          | T.null (T.strip p) || isJust (testAt p) || indentOf p >= bound = go bound rest
          | otherwise = go (indentOf p) rest ++ [T.strip p]
    segmentsOf i l n = (if lfNest lf then parents i l else []) ++ [n]
    -- (path, line, block lines) per failure match
    blocks = case failureRx of
      Nothing -> []
      Just rx -> blocksFrom rx ls
    blocksFrom rx = \case
      [] -> []
      (_, l) : rest -> case groupsOf rx l of
        Just (_ : path : line : _) ->
          let (body, after) = break (\(_, x) -> isJust (groupsOf rx x) || isEnd x) rest
           in (T.unpack path, readMaybe (T.unpack line), [x | (_, x) <- body, not (T.null (T.strip x))]) : blocksFrom rx after
        _ -> blocksFrom rx rest
    attach bs = \case
      [] -> []
      (i, l, n, False) : rest -> Reported Nothing (segmentsOf i l n) Nothing : attach bs rest
      (i, l, n, True) : rest -> case bs of
        (path, line, body) : bs' -> Reported (Just path) (segmentsOf i l n) (Just (Failure line (dedent (filter (not . isDrop) body)) (dedent body))) : attach bs' rest
        [] -> Reported Nothing (segmentsOf i l n) (Just (Failure Nothing "failed" "")) : attach [] rest

-- | TAP: @ok N - d@ passes, @not ok N - d@ fails (its YAML block, between
-- @---@ and @...@, the message); a @# SKIP@ or @# TODO@ is neither;
-- @Bail out!@ is no run.
parseTap :: Text -> Either Text [Reported]
parseTap out = go (T.lines out)
  where
    go = \case
      [] -> Right []
      l : rest
        | Just why <- T.stripPrefix "Bail out!" (T.strip l) -> Left (T.strip why)
        | Just d <- T.stripPrefix "not ok" (T.strip l) ->
            let (yaml, after) = yamlBlock rest
             in (maybe id ((:) . (\n -> Reported Nothing [n] (Just (Failure Nothing (if null yaml then "failed" else dedent yaml) (dedent yaml))))) (described d) <$> go after)
        | Just d <- T.stripPrefix "ok" (T.strip l) -> (maybe id ((:) . (\n -> Reported Nothing [n] Nothing)) (described d) <$> go rest)
        | otherwise -> go rest
    -- the description after the number and a dash; Nothing for a directive
    described d =
      let (desc, directive) = T.breakOn "#" (T.dropWhile (`elem` [' ', '-']) (T.dropWhile (`elem` ['0' .. '9']) (T.stripStart d)))
          isDirective = any (`T.isPrefixOf` T.toUpper (T.strip (T.drop 1 directive))) ["SKIP", "TODO"]
       in if isDirective || T.null (T.strip desc) then Nothing else Just (T.strip desc)
    yamlBlock rest = case rest of
      l : more | T.strip l == "---" -> let (body, after) = break ((== "...") . T.strip) more in (filter (not . T.null . T.strip) body, drop 1 after)
      _ -> ([], rest)

-- | JUnit XML: each @testcase@ is a test named by its @name@, in its @file@;
-- a @failure@ or @error@ child fails it (its @message@, its text, its
-- @line@); a @skipped@ child makes it neither.
parseJunit :: Text -> Either Text [Reported]
parseJunit out = case parseXMLDoc (T.unpack out) of
  Nothing -> Left "junit-xml: the report is not XML"
  Just root
    | not (any (\e -> any (`named` e) ["testsuites", "testsuite", "testcase"]) (descendants root)) -> Left "junit-xml: the report has no testsuite"
    | otherwise -> Right (mapMaybe testcase (descendants root))
  where
    descendants e = e : concatMap descendants (elChildren e)
    named n e = qName (elName e) == n
    attr n e = T.pack <$> findAttr (QName n Nothing Nothing) e
    testcase e
      | not (named "testcase" e) = Nothing
      | any (named "skipped") (elChildren e) = Nothing
      | otherwise = do
          name <- attr "name" e
          let failure = find (\c -> named "failure" c || named "error" c) (elChildren e)
              line = (attr "line" e >>= readMaybe . T.unpack) <|> (failure >>= attr "line" >>= readMaybe . T.unpack)
          Just
            ( Reported
                (T.unpack <$> attr "file" e)
                [name]
                ((\f -> Failure line (fromMaybe "failed" (attr "message" f)) (T.strip (T.pack (strContent f)))) <$> failure)
            )

-- | olifant-json: @{"passed": [id], "failed": [{"id", "line"?, "message",
-- "output"?}], "broken": null | "why"}@, every key optional. Ids are the
-- runner's to attribute.
parseTestJson :: ByteString -> Either Text TestRun
parseTestJson raw = either (\e -> Left ("olifant-json: " <> T.pack e)) Right $ do
  v <- eitherDecodeStrict raw :: Either String Value
  flip parseEither v $ withObject "test report" $ \o -> do
    passed <- o .:? "passed" .!= []
    failed <- o .:? "failed" .!= [] >>= traverse (withObject "failed" (\f -> (,) <$> f .: "id" <*> (Failure <$> f .:? "line" <*> f .: "message" <*> f .:? "output" .!= "")))
    broken <- o .:? "broken"
    pure (TestRun passed failed broken)

-- | The id of a reported test, under the requested files (with their
-- `file_group`s): the file is the one its path names (made relative and
-- normalised); else the one whose group is its first segment; else the only
-- file; else none. The first segment is dropped when it is that file's group.
attribute :: [(FilePath, Maybe Text)] -> Reported -> TestId
attribute files r = case file of
  Just (f, g) -> T.pack f <> "::" <> T.intercalate "::" (dropGroup g (rpSegments r))
  Nothing -> T.intercalate "::" (rpSegments r)
  where
    byPath = rpPath r >>= \p -> find (\(f, _) -> samePath (normalise p) (normalise f)) files
    samePath p f
      | isAbsolute p = splitDirectories f `isSuffixOf` splitDirectories p
      | otherwise = splitDirectories p == splitDirectories f
    byGroup = case rpSegments r of
      s : _ -> find (\(_, g) -> g == Just s) files
      [] -> Nothing
    only = case files of
      [one] -> Just one
      _ -> Nothing
    file = byPath <|> byGroup <|> only
    dropGroup g = \case
      s : rest | Just s == g, not (null rest) -> rest
      ss -> ss

-- | A run from what a report said: each test attributed to the requested
-- files. No tests with a non-zero exit is broken, with the first 40
-- non-blank lines of the output; no tests with exit 0 is broken: no tests ran.
fromReported :: [(FilePath, Maybe Text)] -> ExitCode -> Text -> [Reported] -> TestRun
fromReported files code out rs
  | null rs, code /= ExitSuccess = TestRun [] [] (Just (case take 40 (filter (not . T.null . T.strip) (T.lines out)) of
      [] -> "the tests exited " <> T.pack (show code) <> " and said nothing"
      said -> T.intercalate "\n" said))
  | null rs = TestRun [] [] (Just "no tests ran")
  | otherwise = TestRun [attribute files r | r <- rs, Nothing <- [rpFailure r]] [(attribute files r, f) | r <- rs, Just f <- [rpFailure r]] Nothing
