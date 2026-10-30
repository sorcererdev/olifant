-- | Tests and their groups found by rule (`[test] unit`): a line matching
-- one of the rules' expressions starts a test or a group, named by its last
-- group (hspec's `describe "…"`, `it "…"`, pytest's `def test_x`). A rule
-- with `after` counts only below a line matching it (Rust's `#[test]`),
-- looking past lines that match `above`. Lines matching `above` right over
-- a unit (decorators, attributes) are that unit's. A test's path is its
-- enclosing groups' names, then its own.
module Olifant.Language.TestUnits
  ( TestUnit (..)
  , testUnitsIn
  , testAtBy
  , testLineBy
  , anchorName
  , testCaseAtIn
  , testCaseUnitAtIn
  , testCaseLineIn
  , testSourceIn
  , testSourcesIn
  , lastAssertionIn
  ) where

import Data.Char (isAlphaNum)
import Data.List (sortOn)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language.Adapter (Language (..), TestDef (..), UnitRule (..), regexName)
import Text.Regex.TDFA (AllTextSubmatches (..), Regex, makeRegexM, matchM, matchTest)

-- | A test or a group: its path (enclosing groups, then its name) and its
-- lines (1-based, inclusive; the `above` lines over it included).
data TestUnit = TestUnit {tuPath :: [Text], tuLines :: (Int, Int)}
  deriving stock (Eq, Show)

-- | Every unit of TEXT by RULES, ABOVE being the expression of the lines
-- that belong to the unit below them, in order. A unit runs from its line
-- to the line before the next non-blank line at its indentation or less
-- (trailing blank lines left out), and up over the `above` lines right
-- over it (with their continuation lines: deeper, or closing a bracket);
-- its parents are the units whose lines hold it.
testUnitsIn :: [UnitRule] -> Maybe Text -> Text -> [TestUnit]
testUnitsIn rules above text = [TestUnit (parentsOf sp ++ [n]) sp | (sp, n) <- spans]
  where
    isAbove = maybe (const False) matcher above
    namers = [(fmap unescape . maybe (regexName (urPattern r)) (groupOf (urPattern r)) (urName r), matcher <$> urAfter r) | r <- rules]
    ls = zip [1 :: Int ..] (T.lines text)
    lineAt i = maybe "" snd (listToMaybe (drop (i - 1) ls))
    indentOf = T.length . T.takeWhile (== ' ')
    blank = T.null . T.strip
    nameAt i l = listToMaybe (mapMaybe (\(namer, after) -> namer l >>= \n -> if maybe True (afterHolds i) after then Just n else Nothing) namers)
    -- a line above, past `above` lines, matches `after`
    afterHolds i after = go (i - 1)
      where
        go j
          | j < 1 = False
          | after (lineAt j) = True
          | isAbove (lineAt j) = go (j - 1)
          | otherwise = False
    starts = [(i, l, n) | (i, l) <- ls, Just n <- [nameAt i l]]
    spans = [((headOf i l, endOf i l), n) | (i, l, n) <- starts]
    endOf i l =
      let rest = [x | x@(j, _) <- ls, j > i]
          body = takeWhile (\(_, x) -> blank x || indentOf x > indentOf l) rest
          kept = reverse (dropWhile (blank . snd) (reverse body))
       in maybe i fst (listToMaybe (reverse kept))
    -- the topmost `above` line at the unit's indentation in the run right
    -- over it of such lines and their continuations
    headOf i l = go (i - 1) i
      where
        d = indentOf l
        go j top
          | j < 1 || blank x = top
          | indentOf x == d && isAbove x = go (j - 1) j
          | indentOf x > d || (indentOf x == d && T.take 1 (T.strip x) `elem` [")", "]", "}"]) = go (j - 1) top
          | otherwise = top
          where
            x = lineAt j
    parentsOf (a, z) = [n | ((b, y), n) <- spans, b < a, z <= y]

-- | Group N of RE's match in a line, stripped; Nothing when it does not
-- match or the group is empty.
groupOf :: Text -> Int -> Text -> Maybe Text
groupOf re n = case makeRegexM (T.unpack re) :: Maybe Regex of
  Nothing -> const Nothing
  Just rx -> \txt -> do
    groups <- getAllTextSubmatches <$> (matchM rx (T.unpack txt) :: Maybe (AllTextSubmatches [] String))
    g <- listToMaybe (drop n groups)
    let t = T.strip (T.pack g)
    if T.null t then Nothing else Just t

-- | A name read out of a string literal: @\\"@ is @"@, @\\\\@ is @\\@.
unescape :: Text -> Text
unescape t
  | "\\" `T.isInfixOf` t = T.pack (go (T.unpack t))
  | otherwise = t
  where
    go ('\\' : c : rest) | c `elem` ['"', '\\'] = c : go rest
    go (c : rest) = c : go rest
    go [] = []

-- | Whether RE matches a line (an invalid expression matches nothing).
-- Partly applied, it compiles once.
matcher :: Text -> Text -> Bool
matcher re = case makeRegexM (T.unpack re) :: Maybe Regex of
  Nothing -> const False
  Just rx -> matchTest rx . T.unpack

-- | The innermost unit at LINE (1-based), its path joined by @::@. A blank
-- line after a unit's last line is still in it, as far as the next line
-- that is not.
testAtBy :: [UnitRule] -> Maybe Text -> Text -> Int -> Maybe Text
testAtBy rules above text line =
  T.intercalate "::" . tuPath <$> listToMaybe (sortOn (\u -> (Down (fst (tuLines u)), snd (tuLines u))) [u | u <- testUnitsIn rules above text, let (a, z) = tuLines u, a <= line, line <= throughBlanks z])
  where
    ls = T.lines text
    throughBlanks z = z + length (takeWhile (T.null . T.strip) (drop z ls))

-- | The first line of the unit whose 'anchorName' is NAME (a plan anchor's
-- dotted name: `Olifant.Plans.parsePlan`, `TestX.test_y`).
testLineBy :: [UnitRule] -> Maybe Text -> Text -> Text -> Maybe Int
testLineBy rules above text name = listToMaybe [fst (tuLines u) | u <- testUnitsIn rules above text, anchorName (tuPath u) == name]

-- | A test's name in a plan anchor: its path joined by @.@, each run of
-- characters outside @[A-Za-z0-9_.]@ written as one @_@
-- (@["Olifant.Plans", "parses \"x\" twice"]@ is
-- @Olifant.Plans.parses_x_twice@).
anchorName :: [Text] -> Text
anchorName = T.intercalate "." . map squash
  where
    ok c = (isAlphaNum c && c < '\128') || c == '_' || c == '.'
    squash t = case T.span ok t of
      (a, rest)
        | T.null rest -> a
        | otherwise -> a <> "_" <> squash (T.dropWhile (not . ok) rest)

-- | The rules and `above` of LANG's `[test] unit` (none without them).
rulesOf :: Language -> ([UnitRule], Maybe Text)
rulesOf lang = maybe ([], Nothing) (\td -> (testUnitRules td, testAbove td)) (langTest lang)

-- | The test case at LINE (1-based) of a test file in LANG, named as
-- 'testAtBy' names it: the innermost unit there when no other unit lies
-- inside it (a group's own lines, a class or a describe, are none).
testCaseAtIn :: Language -> Text -> Int -> Maybe Text
testCaseAtIn lang text line = T.intercalate "::" . tuPath <$> testCaseUnitAtIn lang text line

-- | The test case at LINE as its unit (see 'testCaseAtIn').
testCaseUnitAtIn :: Language -> Text -> Int -> Maybe TestUnit
testCaseUnitAtIn lang text line = (listToMaybe (sortOn (\u -> (Down (fst (tuLines u)), snd (tuLines u))) [u | u <- units, let (a, z) = tuLines u, a <= line, line <= throughBlanks z]) >>= leaf)
  where
    (rules, above) = rulesOf lang
    units = testUnitsIn rules above text
    ls = T.lines text
    throughBlanks z = z + length (takeWhile (T.null . T.strip) (drop z ls))
    leaf u = if any (\o -> o /= u && fst (tuLines u) <= fst (tuLines o) && snd (tuLines o) <= snd (tuLines u)) units then Nothing else Just u

-- | The first line (1-based) of the test case NAME (as 'testCaseAtIn'
-- names it) in a test file in LANG.
testCaseLineIn :: Language -> Text -> Text -> Maybe Int
testCaseLineIn lang text name = listToMaybe [i | i <- [1 .. length (T.lines text)], testCaseAtIn lang text i == Just name]

-- | The source of the test whose own name (its path's last) is NAME in a
-- test file in LANG ('testSourcesIn').
testSourceIn :: Language -> Text -> Text -> Maybe Text
testSourceIn lang name text = lookup name (testSourcesIn lang text)

-- | Each unit of a test file in LANG by its own name (its path's last), in
-- order, with its source: from the line its rule matched (lines `above` it
-- left out) to its end, trailing blank lines dropped.
testSourcesIn :: Language -> Text -> [(Text, Text)]
testSourcesIn lang text = [(n, T.intercalate "\n" (drop (own a z - a) (slice a z))) | u <- testUnitsIn rules above text, Just n <- [listToMaybe (reverse (tuPath u))], let (a, z) = tuLines u]
  where
    (rules, above) = rulesOf lang
    ls = T.lines text
    slice a z = take (z - a + 1) (drop (a - 1) ls)
    isAbove = maybe (const False) matcher above
    starts l = any (\r -> matcher (urPattern r) l) rules
    -- the unit's own line: the first in it that a rule matches and is no `above` line
    own a z = maybe a fst (listToMaybe [x | x@(_, l) <- zip [a ..] (slice a z), starts l, not (isAbove l)])

-- | The last line of TEXT matching LANG's `[test] assertion`, stripped.
lastAssertionIn :: Language -> Text -> Maybe Text
lastAssertionIn lang text = listToMaybe (reverse [T.strip l | l <- T.lines text, any (`matcher` l) (langAssertion lang)])
