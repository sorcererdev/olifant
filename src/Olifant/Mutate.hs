-- | Mutation testing's vocabulary. For now only the `Mutant` type, which
-- `Olifant.Block` judges by; the mutating itself arrives with the judge.
module Olifant.Mutate (Mutant (..)) where

import Data.Text (Text)

-- | One mutation of a file: the line, the operator that made it, what it
-- changed from and to, and the whole mutated file. `muWaived` holds the
-- reason a surviving mutant was accepted; `muDim` marks one Jev thinks
-- equivalent to the original.
data Mutant = Mutant
  { muLine :: Int,
    muOperator :: Text,
    muFrom :: Text,
    muTo :: Text,
    muText :: Text,
    muWaived :: Maybe Text,
    muDim :: Bool
  }
  deriving stock (Eq, Show)
