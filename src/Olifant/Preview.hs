-- | The preview's public interface. Other units and specs import only this module.
--
-- The generate picker's form and placeholders: what each input is and where
-- it came from, and how a plan made before every input is set shows the
-- missing ones (⟨name⟩).
module Olifant.Preview
  ( Field (..)
  , Source (..)
  , fieldsFor
  , markValue
  , unmark
  ) where

import Olifant.Preview.Fields (Field (..), Source (..), fieldsFor)
import Olifant.Preview.Marks (markValue, unmark)
