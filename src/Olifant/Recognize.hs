-- | Convention recognition (spec: convention-recognition-design.md): when
-- someone hand-writes what a project's convention produces, which generator
-- it is and with which inputs.
module Olifant.Recognize
  ( module Olifant.Recognize.Shape
  , module Olifant.Recognize.Weights
  , module Olifant.Recognize.Paths
  , module Olifant.Recognize.Index
  , module Olifant.Recognize.Inputs
  ) where

import Olifant.Recognize.Inputs
import Olifant.Recognize.Index
import Olifant.Recognize.Paths
import Olifant.Recognize.Shape
import Olifant.Recognize.Weights
