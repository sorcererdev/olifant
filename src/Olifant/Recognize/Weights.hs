-- | Internal to Olifant.Recognize: how much each pattern says (spec:
-- recognition §2). A pattern in every file of a project (`self.x = x`)
-- tells little; one in a single file much more. Without these weights a
-- large file "explains" almost any code (the spike, 2026-10-06).
module Olifant.Recognize.Weights
  ( Weights (..)
  , weightsOf
  , containment
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Olifant.Recognize.Shape (Shingle)

-- | Each pattern's rarity (idf), the weight of one never seen, and the
-- boilerplate: patterns in more than 30% of the files, ignored.
data Weights = Weights {wIdf :: Map Shingle Double, wDefault :: Double, wBoilerplate :: Set Shingle}
  deriving stock (Eq, Show)

-- | Weights over a project's files of one language (each file's patterns).
-- Boilerplate needs at least 4 files to mean anything.
weightsOf :: [Set Shingle] -> Weights
weightsOf files =
  Weights
    { wIdf = M.map (\c -> log ((1 + n) / (1 + c))) df
    , wDefault = log (1 + n)
    , wBoilerplate = if length files < 4 then S.empty else M.keysSet (M.filter (> 0.3 * n) df)
    }
  where
    n = fromIntegral (length files) :: Double
    df = M.fromListWith (+) [(s, 1) | f <- files, s <- S.toList f]

-- | How much of a change a reference explains: the weighted share of the
-- change's patterns (boilerplate removed) found in the reference; 0 for an
-- empty change.
containment :: Weights -> Set Shingle -> Set Shingle -> Double
containment w change reference
  | total <= 0 = 0
  | otherwise = sum (map weight (S.toList (S.intersection mine reference))) / total
  where
    mine = change `S.difference` wBoilerplate w
    total = sum (map weight (S.toList mine))
    weight s = max 0 (M.findWithDefault (wDefault w) s (wIdf w))
