-- | The check port (spec §5.6): compile files, report `case`s missing constructors.
module Olifant.Language.Check
  ( Checker (..)
  , MissingPatterns (..)
  , CheckFailure (..)
  , noChecker
  , describeCheckFailure
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Olifant.Language.Adapter (Language)
import Olifant.Language.Diagnostics (MissingPatterns (..))

data CheckFailure = CheckToolMissing Text | CheckFailed Text
  deriving stock (Eq, Show)

newtype Checker m = Checker {runCheck :: Language -> Map FilePath Text -> m (Either CheckFailure [MissingPatterns])}

noChecker :: Applicative m => Checker m
noChecker = Checker (\_ _ -> pure (Left (CheckFailed "no checker is configured")))

describeCheckFailure :: CheckFailure -> Text
describeCheckFailure = \case
  CheckToolMissing why -> why
  CheckFailed why -> why
