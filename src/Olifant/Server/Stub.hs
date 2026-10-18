-- | Internal to Olifant.Server: a new test's stub (`olifant/stub`) for the
-- function, method or class at a line of a module, at the end of its test
-- file. No model, no block: the test file's edit and where the test's
-- name is.
module Olifant.Server.Stub
  ( classAt
  , testNameOf
  , belowDecorators
  ) where

import Data.Char (isAlphaNum, isDigit, isLower, isUpper, toLower)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Server.Block (defName)

-- | The class enclosing LINE (1-based) of a Python module: its name and
-- its `class` line. A blank line is in it only when the next line that is
-- not blank is.
classAt :: Text -> Int -> Maybe (Text, Int)
classAt text line
  | line < 1 || line > length ls = Nothing
  | otherwise = listToMaybe [(n, d) | (d, l) <- reverse (take line ls), Just n <- [className l], encloses d l]
  where
    ls = zip [1 :: Int ..] (T.lines text)
    indentOf = T.length . T.takeWhile (== ' ')
    blank = T.null . T.strip
    next = listToMaybe [l | (i, l) <- ls, i >= line, not (blank l)]
    encloses d dl =
      d == line
        || ( all (\(_, l) -> blank l || indentOf l > indentOf dl) [x | x@(i, _) <- ls, i > d, i <= line]
               && maybe False (\l -> indentOf l > indentOf dl) next
           )
    className l = do
      rest <- T.stripPrefix "class " (T.strip l)
      let n = T.takeWhile (\c -> isAlphaNum c || c == '_') rest
      if T.null n then Nothing else Just n

-- | A unit's test name: `test_` and its name in snake case, the
-- underscores at its ends dropped (`CloseTable` → `test_close_table`).
testNameOf :: Text -> Text
testNameOf n = "test_" <> T.pack (go ' ' (T.unpack (T.dropAround (== '_') n)))
  where
    go prev (c : cs)
      | isUpper c && (isLower prev || isDigit prev) = '_' : toLower c : go c cs
      | otherwise = toLower c : go c cs
    go _ [] = []

-- | A decorator line (1-based) stands for the def or class below it.
belowDecorators :: Text -> Int -> Int
belowDecorators text line = case drop (line - 1) ls of
  l : _ | "@" `T.isPrefixOf` T.strip l -> maybe line fst (listToMaybe [x | x@(_, l') <- drop line (zip [1 ..] ls), isJust (defName l') || "class " `T.isPrefixOf` T.strip l'])
  _ -> line
  where
    ls = T.lines text
