-- | Which compiler support a `[check] format` selects (spec §5.6).
module Olifant.Language.Support (supportFor, knownFormats) where

import Data.Text (Text)
import Olifant.Language.Diagnostics (CompilerSupport (..))
import Olifant.Language.Elm (elmSupport)

supports :: [CompilerSupport]
supports = [elmSupport]

supportFor :: Text -> Maybe CompilerSupport
supportFor f = case [s | s <- supports, supportFormat s == f] of
  s : _ -> Just s
  [] -> Nothing

knownFormats :: [Text]
knownFormats = map supportFormat supports
