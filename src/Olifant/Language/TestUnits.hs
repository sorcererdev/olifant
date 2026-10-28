-- | Tests and their groups found by rule (`[test] unit`): a line matching
-- one of the expressions starts a test or a group, named by its last group
-- (hspec's `describe "…"`, `it "…"`). A test's path is its enclosing groups'
-- names, then its own.
module Olifant.Language.TestUnits
  ( TestUnit (..)
  , testUnitsIn
  , testAtBy
  , testLineBy
  ) where

import Data.List (sortOn)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language.Adapter (regexName)

-- | A test or a group: its path (enclosing groups, then its name) and its
-- lines (1-based, inclusive).
data TestUnit = TestUnit {tuPath :: [Text], tuLines :: (Int, Int)}
  deriving stock (Eq, Show)

-- | Every unit of TEXT, in order. A unit runs from its line to the line
-- before the next non-blank line at its indentation or less (trailing blank
-- lines left out); its parents are the units whose lines hold it.
testUnitsIn :: [Text] -> Text -> [TestUnit]
testUnitsIn patterns text = [TestUnit (parentsOf sp ++ [n]) sp | (sp, n) <- spans]
  where
    namers = map regexName patterns
    ls = zip [1 :: Int ..] (T.lines text)
    indentOf = T.length . T.takeWhile (== ' ')
    blank = T.null . T.strip
    starts = [(i, l, n) | (i, l) <- ls, Just n <- [listToMaybe (mapMaybe ($ l) namers)]]
    spans = [((i, endOf i l), n) | (i, l, n) <- starts]
    endOf i l =
      let rest = [x | x@(j, _) <- ls, j > i]
          body = takeWhile (\(_, x) -> blank x || indentOf x > indentOf l) rest
          kept = reverse (dropWhile (blank . snd) (reverse body))
       in maybe i fst (listToMaybe (reverse kept))
    parentsOf (a, z) = [n | ((b, y), n) <- spans, b < a, z <= y]

-- | The innermost unit at LINE (1-based), its path joined by @::@.
testAtBy :: [Text] -> Text -> Int -> Maybe Text
testAtBy patterns text line =
  T.intercalate "::" . tuPath <$> listToMaybe (sortOn (\u -> (Down (fst (tuLines u)), snd (tuLines u))) [u | u <- testUnitsIn patterns text, let (a, z) = tuLines u, a <= line, line <= z])

-- | The first line of the unit whose path joined by @.@ is NAME (a plan
-- anchor's dotted name: `Olifant.Plans.parsePlan`).
testLineBy :: [Text] -> Text -> Text -> Maybe Int
testLineBy patterns text name = listToMaybe [fst (tuLines u) | u <- testUnitsIn patterns text, T.intercalate "." (tuPath u) == name]
