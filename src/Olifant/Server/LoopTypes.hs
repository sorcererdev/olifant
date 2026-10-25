-- | Internal to Olifant.Server: the loop's data, below the server state
-- (which caches reconciles) and the loop's modules (which make them).
module Olifant.Server.LoopTypes
  ( Change (..)
  , ChangedUnit (..)
  , Fact (..)
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

-- | A plan edit the engine makes without a model: a row's new text, or
-- lines inserted after a row (0-based rows of the request's document).
data Fact = Replace Int Text | Insert Int [Text]
  deriving stock (Eq, Show)

-- | A reading as a ghost block: whether it replaces the line it follows (a
-- step's new state), and the (path, unit, fingerprint) it is about, which
-- a rejection of it marks as read.
data ReconcileBlock = ReconcileBlock {rbBlock :: Block, rbReplace :: Bool, rbUnit :: Maybe (FilePath, Text, Text)}
  deriving stock (Eq, Show)

-- | A reconcile's answer: its record's id (outcomes name it), the baseline,
-- the facts, the ghost blocks, how many units changed, how many were left
-- unasked (the cap and the budget), its seconds and the rungs it called.
data ReconcileAnswer = ReconcileAnswer
  { raId :: Text
  , raBaseline :: Maybe Text
  , raFacts :: [Fact]
  , raBlocks :: [ReconcileBlock]
  , raChanged :: Int
  , raMore :: Int
  , raSeconds :: Double
  , raRungs :: [Text]
  }
  deriving stock (Eq, Show)
