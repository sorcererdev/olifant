-- | File globs: `**` spans directories, `*` stays within one segment.
module Olifant.Glob (matchesGlob, globPrefix) where

import Data.List (tails)
import Data.Text (Text)
import Data.Text qualified as T
import System.FilePath (joinPath, splitDirectories)

matchesGlob :: Text -> FilePath -> Bool
matchesGlob g p = go (T.splitOn "/" g) (map T.pack (splitDirectories p))
  where
    go [] [] = True
    go ["**"] _ = True
    go ("**" : gs) ps = any (go gs) (tails ps)
    go (x : gs) (y : ps) = segment (T.unpack x) (T.unpack y) && go gs ps
    go _ _ = False
    segment ('*' : rest) s = any (segment rest) (tails s)
    segment (c : rest) (x : xs) = c == x && segment rest xs
    segment [] [] = True
    segment _ _ = False

-- | The directories a glob names literally before its first wildcard; paths
-- are reported relative to this (`rel`, `dotted` in query records).
globPrefix :: Text -> FilePath
globPrefix g = case break (T.any (== '*')) (T.splitOn "/" g) of
  (_, []) -> ""
  (lits, _) -> joinPath (map T.unpack lits)
