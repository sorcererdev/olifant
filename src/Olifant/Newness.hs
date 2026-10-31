-- | Which lines of a file are new since a base: what "new work" is, for
-- rules that judge only new code (`only = "new"`, the strangler work).
module Olifant.Newness (NewLines (..), newLinesOf, newSince, wholeNew, newCount, wasDefined, lcsPairs) where

import Data.Array qualified as A
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe (listToMaybe)
import Olifant.Language.Adapter (Language)
import Olifant.Language.TreeSitter (definedNamesIn)

-- | A file's new lines (1-based): all of it (no base version: a new or
-- untracked file), some, or unknown (no base: why).
-- `NewIn` carries the names defined at the base too: a definition there is
-- not new work, even rewritten whole (a small function's bug fix).
data NewLines = AllNew | NewIn (Set Int) (Set Text) | NoBase Text
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

-- | The (i, j) pairs (1-based) of a longest common subsequence of two
-- lists: Myers' O(ND) diff (time and memory grow with the edits, not the
-- file: a 5,000-line legacy file changed at both ends is a few steps).
-- Past 'maxEdits' (a rewrite), lines pair by text in order instead.
lcsPairs :: [Text] -> [Text] -> [(Int, Int)]
lcsPairs as bs = case forward 0 (IM.singleton 1 0) [] of
  Just trace -> backtrack trace n m []
  Nothing -> byText
  where
    n = length as
    m = length bs
    aA = A.listArray (1, max 1 n) (if n == 0 then [""] else as)
    bA = A.listArray (1, max 1 m) (if m == 0 then [""] else bs)
    snake x y
      | x < n && y < m && aA A.! (x + 1) == bA A.! (y + 1) = snake (x + 1) (y + 1)
      | otherwise = (x, y)
    -- each step's frontier (V by diagonal), kept for the walk back
    forward d v trace
      | d > maxEdits = Nothing
      | otherwise =
          let step (vAcc, done) k =
                let down = k == -d || (k /= d && get vAcc (k - 1) < get vAcc (k + 1))
                    x0 = if down then get vAcc (k + 1) else get vAcc (k - 1) + 1
                    (x, y) = snake x0 (x0 - k)
                 in (IM.insert k x vAcc, done || (x >= n && y >= m))
              (v', finished) = foldl step (v, False) [-d, -d + 2 .. d]
           in if finished then Just (v : trace) else forward (d + 1) v' (v : trace)
    get v k = IM.findWithDefault 0 k v
    -- from the end back: a step's snake is common lines; its move an edit
    backtrack [] _ _ acc = acc
    backtrack (v : rest) x y acc =
      let d = length rest
          k = x - y
          down = k == -d || (k /= d && get v (k - 1) < get v (k + 1))
          pk = if down then k + 1 else k - 1
          px = get v pk
          py = px - pk
          -- the point after this step's edit; from there to (x, y) is common
          (mx, my) = if down then (px, py + 1) else (px + 1, py)
          common = [(mx + t, my + t) | t <- [1 .. x - mx]]
       in if d == 0 then [(t, t) | t <- [1 .. x], t <= y] ++ acc else backtrack rest px py (common ++ acc)
    -- a rewrite: each line pairs with the next unpaired base line of its text
    byText =
      let go _ [] = []
          go used ((j, l) : more) = case [i | (i, a) <- zip [1 ..] as, a == l, i `S.notMember` used, i > lastUsed used] of
            i : _ -> (i, j) : go (S.insert i used) more
            [] -> go used more
          lastUsed used = maybe 0 fst (S.maxView used)
       in go S.empty (zip [1 ..] bs)

-- | Past this many inserted and removed lines, a file is a rewrite.
maxEdits :: Int
maxEdits = 1000

-- | Whether a range (1-based (line, col) pairs) is wholly new.
wholeNew :: NewLines -> ((Int, Int), (Int, Int)) -> Bool
wholeNew n ((l1, _), (l2, _)) = case n of
  AllNew -> True
  NoBase _ -> False
  NewIn s _ -> all (`S.member` s) [l1 .. l2]

-- | How many lines of a range are new.
newCount :: NewLines -> ((Int, Int), (Int, Int)) -> Int
newCount n ((l1, _), (l2, _)) = case n of
  AllNew -> l2 - l1 + 1
  NoBase _ -> 0
  NewIn s _ -> length (filter (`S.member` s) [l1 .. l2])

-- | A file's newness against its base text: the new lines, and the names
-- the base defines (its definitions, by the language's queries; none
-- without a language or a linked grammar).
newSince :: Maybe Language -> Text -> Text -> NewLines
newSince lang base now = NewIn (newLinesOf base now) (S.fromList (maybe [] (`definedNamesIn` base) lang))

-- | Whether a name was defined at the base (unknown base: no).
wasDefined :: NewLines -> Text -> Bool
wasDefined n name = case n of
  NewIn _ names -> name `S.member` names
  _ -> False


