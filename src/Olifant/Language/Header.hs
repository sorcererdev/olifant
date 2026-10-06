-- | A file's header: the comments and docstring it starts with, where a
-- first import goes after (a fill's imports; an empty `imports` target).
module Olifant.Language.Header (headerEnd) where

import Data.Text (Text)
import Data.Text qualified as T

-- | How many lines at the top are comments or a docstring (a string prefix
-- such as @r@ or @b@ allowed), with any blank lines before them.
headerEnd :: [Text] -> Int
headerEnd ls = case span (T.null . T.strip) ls of
  (blanks, rest) | n <- go 0 rest, n > 0 -> length blanks + n
  _ -> 0
  where
    go i (l : rest)
      | "#" `T.isPrefixOf` l || "//" `T.isPrefixOf` l || "--" `T.isPrefixOf` l = go (i + 1) rest
      | Just (q, after) <- quote l = let n = closing q after rest in go (i + 1 + n) (drop n rest)
    go i _ = i
    quote l =
      let s = T.strip l
          prefix = T.takeWhile (`elem` ("rRbBuUfF" :: String)) s
          body = T.drop (T.length prefix) s
       in case T.take 3 body of
            q | T.length prefix <= 2, q `elem` ["\"\"\"", "\'\'\'"] -> Just (q, T.drop 3 body)
            _ -> Nothing
    -- lines after the opening one, up to and including the closing one
    closing q first rest
      | q `T.isInfixOf` first = 0
      | otherwise = length (takeWhile (not . T.isInfixOf q) rest) + 1
