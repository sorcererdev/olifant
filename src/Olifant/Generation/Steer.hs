-- | What a steered fill loop and its hook share: the state of a red round, where a
-- steered loop starts, and what the hook may answer.
module Olifant.Generation.Steer
  ( RoundHole (..)
  , RoundState (..)
  , Seed (..)
  , noSeed
  , Steer (..)
  , Steering (..)
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Olifant.Fill (ModelFiller)
import Olifant.Language (TestRun)

-- | One hole after a red round: why its fill does not stand, every (fill, why) so far (oldest
-- first, this round's last when a fill came), and the verify's run when one ran.
data RoundHole = RoundHole {rhName :: Text, rhWhy :: Text, rhPrevious :: [(Text, Text)], rhRun :: Maybe TestRun}
  deriving stock (Eq, Show)

-- | A red round: its number (counting seeded rounds), the filler's name (mfName), the holes still red.
data RoundState = RoundState {rsRound :: Int, rsRung :: Text, rsHoles :: [RoundHole]}
  deriving stock (Eq, Show)

-- | Where a steered loop starts: rounds already run, each hole's previous attempts.
data Seed = Seed {seedRound :: Int, seedPrevious :: Map Text [(Text, Text)]}
  deriving stock (Eq, Show)

noSeed :: Seed
noSeed = Seed 0 M.empty

data Steer m = Again | Climb (ModelFiller m) | Pause Text

data Steering m = Steering {stSeed :: Seed, stSteer :: RoundState -> m (Steer m)}
