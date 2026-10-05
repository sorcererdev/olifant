-- | A file's header: the comments and docstring it starts with, where a
-- first import goes after (a fill's imports; an empty `imports` target).
module Olifant.Language.Header (headerEnd) where

import Data.Text (Text)
import Data.Text qualified as T

-- | How many lines at the top are comments or a docstring.
headerEnd :: [Text] -> Int
headerEnd = go 0
  where
    go i (l : rest)
      | "#" `T.isPrefixOf` l || "//" `T.isPrefixOf` l || "--" `T.isPrefixOf` l = go (i + 1) rest
      | Just q <- quote l = let n = closing q (T.drop 3 (T.strip l)) rest in go (i + 1 + n) (drop n rest)
    go i _ = i
    quote l = case T.take 3 (T.strip l) of
      q | q `elem` ["\"\"\"", "\'\'\'"] -> Just q
      _ -> Nothing
    -- lines after the opening one, up to and including the closing one
    closing q first rest
      | q `T.isInfixOf` first = 0
      | otherwise = length (takeWhile (not . T.isInfixOf q) rest) + 1
