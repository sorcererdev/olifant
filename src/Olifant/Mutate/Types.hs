-- | The mutant itself, apart from the modules that make and judge it.
module Olifant.Mutate.Types (Mutant (..)) where

import Data.Text (Text)

-- | One mutation of a file: the line, the operator that made it, what it
-- changed from and to, and the whole mutated file. `muWaived` holds the
-- reason a surviving mutant was accepted; `muDim` marks one the decision model thinks
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
