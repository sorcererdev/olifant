module Olifant.Language.Diagnostics (MissingPatterns (..)) where

import Data.Text (Text)

-- | A `case` (0-based start and end line) and the patterns it lacks.
data MissingPatterns = MissingPatterns {missingFile :: FilePath, missingCase :: (Int, Int), missingPatterns :: [Text]}
  deriving stock (Eq, Show)
