-- | Test reports any framework can write (spec §5.6, layer 2): `lines` (a
-- regex per line: hspec's checks, cargo test), TAP, JUnit XML, and
-- olifant-json. A parser says each test as the report names it
-- ('Reported'); 'attribute' turns that name into a test id under one of the
-- files the run asked for.
module Olifant.Language.TestFormats
  ( LinesFormat (..)
  , JunitFormat (..)
  , noJunitFormat
  , Reported (..)
  , parseLines
  , parseTap
  , parseJunit
  , junitNotXml
  , parseTestJson
  , attribute
  , fromReported
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (Value, eitherDecodeStrict, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import Data.List (find, isPrefixOf, isSuffixOf)
import Data.Char (isDigit)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language.Generic (JunitFormat (..), LinesFormat (..), noJunitFormat)
import Olifant.Language.Test (Failure (..), TestId, TestRun (..))
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, isAbsolute, normalise, splitDirectories)
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
    segmentsOf i l n = (if lfNest lf then parents i l else []) ++ maybe [n] (\sep -> filter (not . T.null) (T.splitOn sep n)) (lfSplit lf)
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

-- | What 'parseJunit' says of text that is not XML.
junitNotXml :: Text
junitNotXml = "junit-xml: the report is not XML"

-- | JUnit XML: each @testcase@ is a test; its id is its @file@ (else its
-- @testsuite@'s), the @classname@'s groups by 'jfJoin' (less the leading
-- ones that spell the file's path: @tests.test_x@ in @tests/test_x.py@), then
-- its @name@ split by 'jfSplit'. A @failure@ or @error@ child fails it: the
-- @message@ (followed by the text when it is one line the text lacks), the
-- text, and the line (the last @FILE:N@ of the text for the test's file,
-- else the failure's @line@, else the testcase's); a @skipped@ child makes
-- it neither. A testcase with no classname and an @error@ is not a test but
-- a collection failure: the whole report is Left with its message and text.
parseJunit :: JunitFormat -> Text -> Either Text [Reported]
parseJunit jf out = case parseXMLDoc (T.unpack out) of
  Nothing -> Left junitNotXml
  Just root
    | not (any (\e -> any (`named` e) ["testsuites", "testsuite", "testcase"]) (descendants root)) -> Left "junit-xml: the report has no testsuite"
    | otherwise -> catMaybes <$> traverse (uncurry testcase) (cases Nothing root)
  where
    descendants e = e : concatMap descendants (elChildren e)
    named n e = qName (elName e) == n
    attr n e = T.pack <$> findAttr (QName n Nothing Nothing) e
    -- each testcase with the file of the testsuite around it
    cases suiteFile e
      | named "testcase" e = [(suiteFile, e)]
      | otherwise = concatMap (cases (if named "testsuite" e then attr "file" e <|> suiteFile else suiteFile)) (elChildren e)
    testcase suiteFile e
      | any (named "skipped") (elChildren e) = Right Nothing
      | Just err <- find (named "error") (elChildren e), maybe True T.null (attr "classname" e) =
          Left ("junit-xml: " <> T.intercalate "\n" (filter (not . T.null) [fromMaybe "" (attr "message" err)] ++ take 40 (filter (not . T.null . T.strip) (T.lines (T.pack (strContent err))))))
      | otherwise = Right $ do
          name <- attr "name" e
          let failure = find (\c -> named "failure" c || named "error" c) (elChildren e)
              file = attr "file" e <|> suiteFile
              groups = case jfJoin jf of
                Nothing -> []
                Just j -> dropPath file (filter (not . T.null) (T.splitOn j (fromMaybe "" (attr "classname" e))))
              own = maybe [name] (\sep -> filter (not . T.null) (T.splitOn sep name)) (jfSplit jf)
          Just (Reported (T.unpack <$> file) (groups ++ own) (failureOf file e <$> failure))
    dropPath file segs = drop (length (takeWhile id (zipWith (==) segs (maybe [] pathParts file)))) segs
    pathParts f = map T.pack (splitDirectories (dropExtension (T.unpack f)))
    failureOf file e f =
      let text = T.strip (T.pack (strContent f))
          msg = attr "message" f
          line = (file >>= lastFrame text) <|> (attr "line" f >>= readMaybe . T.unpack) <|> (attr "line" e >>= readMaybe . T.unpack)
          message = case msg of
            Nothing -> "failed"
            Just m | T.any (== '\n') m || T.null text || m `T.isInfixOf` text -> m
                   | otherwise -> m <> "\n" <> text
       in Failure line message text
    -- the last @FILE:N@ in a failure's text
    lastFrame text file = listToMaybe (reverse [n | seg <- drop 1 (T.splitOn (file <> ":") text), Just n <- [readMaybe (T.unpack (T.takeWhile isDigit seg))]])

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
    -- a group of several segments (a Rust module, `money::rates`) leads them all
    groupSegs = T.splitOn "::"
    leads g ss = let gs = groupSegs g in not (null ss) && gs `isPrefixOf` ss
    byGroup = find (\(_, g) -> maybe False (`leads` rpSegments r) g) files
    only = case files of
      [one] -> Just one
      _ -> Nothing
    file = byPath <|> byGroup <|> only
    dropGroup g ss = case g of
      Just g' | leads g' ss, rest@(_ : _) <- drop (length (groupSegs g')) ss -> rest
      _ -> ss

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
