-- | Internal to Olifant.Recognize: what a recognition is.
module Olifant.Recognize.Types
  ( Change (..)
  , Band (..)
  , Recognition (..)
  ) where

import Data.Text (Text)
import Olifant.Generator (GeneratorId)

-- | A file as it is now (project-relative), or the region around an edit.
data Change = Change {chPath :: FilePath, chText :: Text}
  deriving stock (Eq, Show)

-- | An offer (`⚡ … Tab: review`) or a quiet hint.
data Band = Offer | Hint
  deriving stock (Eq, Show)

data Recognition = Recognition
  { rcGenerator :: GeneratorId
  , rcName :: Text
  , rcBand :: Band
  , rcScore :: Double
  , rcShape :: Double
  , rcPlace :: Double
  , rcLike :: Text
  , rcInputs :: [(Text, Text)]
  , -- | a choice: another generator nearly as likely
    rcOr :: Maybe GeneratorId
  , -- | another generator the shape favours over this one (and how much):
    -- where a model's answer is worth asking
    rcRival :: Maybe (GeneratorId, Double)
  , rcReasons :: [Text]
  }
  deriving stock (Eq, Show)

