-- | The fill phase (spec §6.4): one request for every hole in a plan, each
-- fill parsed as its kind and compile-checked, failing holes retried with
-- the diagnostics, and any still failing left as their placeholder.
module Olifant.Fill
  ( FillHole (..)
  , FillRequest (..)
  , Filler (..)
  , ModelFiller (..)
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)

-- | One hole as the model sees it: what it is, the file around it with the
-- hole marked ⟦name⟧, and on a retry the previous fill and why it failed.
data FillHole = FillHole
  { fhName :: Text
  , fhKind :: Text
  , fhInstruction :: Text
  , fhFile :: FilePath
  , fhContext :: Text
  , fhPrevious :: Maybe (Text, Text)
  }
  deriving stock (Eq, Show)

data FillRequest = FillRequest {frHoles :: [FillHole], frInputs :: [(Text, Text)]}
  deriving stock (Eq, Show)

-- | The port to a model: fills by hole name, or why there are none.
newtype Filler m = Filler {runFill :: FillRequest -> m (Either Text (Map Text Text))}

-- | A configured model: its name (for the plan), retries, and its port.
data ModelFiller m = ModelFiller {mfName :: Text, mfRetries :: Int, mfFiller :: Filler m}
