-- | Which compiler support a `[check] format` selects (spec §5.6).
module Olifant.Language.Support (supportFor, knownFormats) where

import Data.Text (Text)
import Olifant.Language.Diagnostics (CompilerSupport (..))
import Olifant.Language.Elm (elmSupport)
import Olifant.Language.Ghc (ghcSupport)
import Olifant.Language.Pyright (pyrightSupport)
import Olifant.Language.Rustc (rustcSupport)

supports :: [CompilerSupport]
supports = [elmSupport, ghcSupport, pyrightSupport, rustcSupport]

supportFor :: Text -> Maybe CompilerSupport
supportFor f = case [s | s <- supports, supportFormat s == f] of
  s : _ -> Just s
  [] -> Nothing

knownFormats :: [Text]
knownFormats = map supportFormat supports
