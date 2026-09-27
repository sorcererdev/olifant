-- | Generator definitions: loading `generator.toml`, resolving inputs, deriving values.
module Olifant.Generator
  ( module Olifant.Generator.Definition
  , module Olifant.Generator.Inputs
  , parseGenerator
  ) where

import Olifant.Generator.Definition
import Olifant.Generator.Inputs
import Olifant.Generator.Load
