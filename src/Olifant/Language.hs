-- | Language knowledge lives in packs as data (spec §12): adapters, the
-- ast-grep rules behind `at` targets, and the ports that run tools.
module Olifant.Language
  ( module Olifant.Language.Adapter
  , module Olifant.Language.Rule
  , module Olifant.Language.Syntax
  , module Olifant.Language.Check
  , module Olifant.Language.Diagnostics
  , module Olifant.Language.Support
  , module Olifant.Language.Test
  ) where

import Olifant.Language.Adapter
import Olifant.Language.Check
import Olifant.Language.Diagnostics
import Olifant.Language.Support
import Olifant.Language.Rule
import Olifant.Language.Syntax
import Olifant.Language.Test
