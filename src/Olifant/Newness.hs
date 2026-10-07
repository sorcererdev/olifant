-- | Which lines of a file are new since a base: what "new work" is, for
-- rules that judge only new code (`only = "new"`, the strangler work).
module Olifant.Newness (NewLines (..), newLinesOf, wholeNew, newCount) where

import Data.Map.Strict qualified as M
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T

-- | A file's new lines (1-based): all of it (no base version: a new or
-- untracked file), some, or unknown (no base: why).
data NewLines = AllNew | NewIn (Set Int) | NoBase Text
  deriving stock (Eq, Show)

-- | The lines of NOW that are new since BASE: not in a longest common
-- subsequence with it, and not moved (a line whose text was removed from
-- elsewhere in the file is moved, not new: moving is not new work). The
-- common prefix and suffix are trimmed first (most edits are local).
newLinesOf :: Text -> Text -> Set Int
newLinesOf base now =
  let bs = T.lines base
      ns = T.lines now
      pre = length (takeWhile id (zipWith (==) bs ns))
      bs1 = drop pre bs
      ns1 = drop pre ns
      suf = length (takeWhile id (zipWith (==) (reverse bs1) (reverse ns1)))
      bm = take (length bs1 - suf) bs1
      nm = take (length ns1 - suf) ns1
      pairs = lcsPairs bm nm
      keptB = S.fromList (map fst pairs)
      keptN = S.fromList (map snd pairs)
      -- removed from the base (as a multiset of non-blank texts)
      removed = M.fromListWith (+) [(T.strip l, 1 :: Int) | (i, l) <- zip [1 ..] bm, i `S.notMember` keptB, not (T.null (T.strip l))]
      added = [(j, l) | (j, l) <- zip [1 ..] nm, j `S.notMember` keptN]
      moved = go removed added
        where
          go _ [] = S.empty
          go left ((j, l) : rest) = case M.lookup (T.strip l) left of
            Just k | k > 0, not (T.null (T.strip l)) -> S.insert j (go (M.insert (T.strip l) (k - 1) left) rest)
            _ -> go left rest
   in S.fromList [pre + j | (j, _) <- added, j `S.notMember` moved]

-- | The (i, j) pairs (1-based) of a longest common subsequence of two lists:
-- a table of LCS lengths built row by row, walked back from the end.
lcsPairs :: [Text] -> [Text] -> [(Int, Int)]
lcsPairs as bs = walk (length as) (length bs) []
  where
    m = length bs
    rows = scanl row (replicate (m + 1) (0 :: Int)) as
    row prev a = reverse (fst (foldl cell ([0], 0) (zip3 prev (drop 1 prev) bs)))
      where
        cell (acc, left) (diag, up, b) = let v = if a == b then diag + 1 else max up left in (v : acc, v)
    -- indexed (a walk back is a few thousand lookups at most)
    table = Seq.fromList (map Seq.fromList rows)
    aSeq = Seq.fromList as
    bSeq = Seq.fromList bs
    at i j = Seq.index (Seq.index table i) j
    aAt i = Seq.index aSeq (i - 1)
    bAt j = Seq.index bSeq (j - 1)
    walk i j acc
      | i == 0 || j == 0 = acc
      | aAt i == bAt j = walk (i - 1) (j - 1) ((i, j) : acc)
      | at (i - 1) j >= at i (j - 1) = walk (i - 1) j acc
      | otherwise = walk i (j - 1) acc

-- | Whether a range (1-based (line, col) pairs) is wholly new.
wholeNew :: NewLines -> ((Int, Int), (Int, Int)) -> Bool
wholeNew n ((l1, _), (l2, _)) = case n of
  AllNew -> True
  NoBase _ -> False
  NewIn s -> all (`S.member` s) [l1 .. l2]

-- | How many lines of a range are new.
newCount :: NewLines -> ((Int, Int), (Int, Int)) -> Int
newCount n ((l1, _), (l2, _)) = case n of
  AllNew -> l2 - l1 + 1
  NoBase _ -> 0
  NewIn s -> length (filter (`S.member` s) [l1 .. l2])
