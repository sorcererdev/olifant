-- | Internal to Olifant.Server: the loop's data, below the server state
-- (which caches reconciles) and the loop's modules (which make them).
module Olifant.Server.LoopTypes
  ( Change (..)
  , ChangedUnit (..)
  , Fact (..)
  , ReadingKind (..)
  , ReconcileBlock (..)
  , ReconcileAnswer (..)
  ) where

import Data.Text (Text)
import Olifant.Server.Errand (Block)

-- | How a unit differs from the baseline: `Changed` carries the base
-- fingerprint, `Moved` the old (path, name).
data Change = Added | Changed Text | Removed | Moved FilePath Text
  deriving stock (Eq, Show)

-- | A unit that differs from the baseline: its relative path and name, its
-- current fingerprint (none when removed), its first line, and its text now
-- and at the base, each cut to 60 lines.
data ChangedUnit = ChangedUnit
  { cuPath :: FilePath
  , cuName :: Text
  , cuFingerprint :: Maybe Text
  , cuChange :: Change
  , cuHeader :: Text
  , cuNow :: Maybe Text
  , cuBase :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | A plan edit the engine makes without a model: a row's new text, lines
-- inserted after a row, or rows from the first to the last (inclusive) as
-- new lines (0-based rows of the request's document).
data Fact = Replace Int Text | Insert Int [Text] | ReplaceRows Int Int [Text]
  deriving stock (Eq, Show)

-- | What a reading says: a step looks done (or still done, at a new
-- fingerprint), open again, wrong (diverged: `✗ changed`), its unit gone; a unit in
-- no step placed under one (`also`) or as a new step.
data ReadingKind = Done | Reopened | Also | Gone | Diverged | NewStep
  deriving stock (Eq, Show)

-- | A reading as a block of plan lines: whether it replaces the line it
-- follows (a step's new state), the (path, unit, fingerprint) it is about,
-- which a rejection of it marks as read, its kind, for a unit in no step
-- the new step it would be (after its place), and the changed unit.
data ReconcileBlock = ReconcileBlock
  { rbBlock :: Block
  , rbReplace :: Bool
  , rbUnit :: Maybe (FilePath, Text, Text)
  , rbKind :: ReadingKind
  , rbNewStep :: Maybe Block
  , rbChange :: Maybe ChangedUnit
  }
  deriving stock (Eq, Show)

-- | A reconcile's answer: its record's id (outcomes name it), the baseline,
-- the facts, the ghost blocks, how many units changed, how many were left
-- unasked (the cap and the budget), its seconds and the rungs it called,
-- and the plan's version it read (Nothing: read from disk).
data ReconcileAnswer = ReconcileAnswer
  { raId :: Text
  , raBaseline :: Maybe Text
  , raFacts :: [Fact]
  , raBlocks :: [ReconcileBlock]
  , raChanged :: Int
  , raMore :: Int
  , raSeconds :: Double
  , raRungs :: [Text]
  , raVersion :: Maybe Int
  }
  deriving stock (Eq, Show)
