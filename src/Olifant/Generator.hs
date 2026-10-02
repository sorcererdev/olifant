-- | Generator definitions: loading `generator.toml`, resolving inputs, deriving values.
module Olifant.Generator
  ( module Olifant.Generator.Definition
  , module Olifant.Generator.Inputs
  , withChoices
  , ChoiceView (..)
  , choiceViews
  , needsChoices
  , parseGenerator
  ) where

import Olifant.Generator.Choices (ChoiceView (..), choiceViews, needsChoices, withChoices)
import Olifant.Generator.Definition
import Olifant.Generator.Inputs
import Olifant.Generator.Load
