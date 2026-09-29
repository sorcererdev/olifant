-- | Language knowledge lives in packs as data (spec §12): adapters, the
-- ast-grep rules behind `at` targets, and the ports that run tools.
module Olifant.Language
  ( module Olifant.Language.Adapter
  , module Olifant.Language.Rule
  ) where

import Olifant.Language.Adapter
import Olifant.Language.Rule
