module Olifant.Language.Diagnostics (MissingPatterns (..), CheckReport (..)) where

import Data.Text (Text)

-- | A `case` (0-based start and end line) and the patterns it lacks.
data MissingPatterns = MissingPatterns {missingFile :: FilePath, missingCase :: (Int, Int), missingPatterns :: [Text]}
  deriving stock (Eq, Show)

-- | What a check found: `case`s missing constructors, and files with any
-- other problem (path, problem title). A broken file hides its dependents'
-- missing patterns, so callers must not read "nothing missing" as "complete".
data CheckReport = CheckReport {reportMissing :: [MissingPatterns], reportBroken :: [(FilePath, Text)]}
  deriving stock (Eq, Show)
