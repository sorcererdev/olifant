-- | Olifant's template core: one internal form for `.dt` templates and
-- exemplars. This is the only module other units (and specs) import.
module Olifant.Template
  ( Template
  , templateSource
  , Value (..)
  , Env
  , TemplateError (..)
  , describeTemplateError
  , parseTemplate
  , render
  , evaluate
  , freeVariables
  , Binding (..)
  , ExemplarError (..)
  , describeExemplarError
  , exemplar
  ) where

import Olifant.Template.Error
import Olifant.Template.Exemplar
import Olifant.Template.Parse
import Olifant.Template.Render
import Olifant.Template.Syntax (Template, freeVariables, templateSource)
import Olifant.Template.Value
