-- | Which compiler support a `[check] format` selects (spec §5.6).
module Olifant.Language.Support (supportFor, formatSupport, knownFormats, knownTestFormats, supportsMissing) where

import Data.Text (Text)
import Olifant.Language.Diagnostics (CompilerSupport (..))
import Olifant.Language.Elm (elmSupport)
import Olifant.Language.Generic (ErrorFormat, errorFormatSupport, olifantJsonSupport, sarifSupport)
import Olifant.Language.Ghc (ghcSupport)
import Olifant.Language.Pyright (pyrightSupport)
import Olifant.Language.Rustc (rustcSupport)

supports :: [CompilerSupport]
supports = [elmSupport, ghcSupport, pyrightSupport, rustcSupport, sarifSupport, olifantJsonSupport]

-- | The support of a named format (not `errorformat`, which needs its table: 'formatSupport').
supportFor :: Text -> Maybe CompilerSupport
supportFor f = case [s | s <- supports, supportFormat s == f] of
  s : _ -> Just s
  [] -> Nothing

-- | The support of a check's format and its `[check.errorformat]`.
formatSupport :: Text -> Maybe ErrorFormat -> Maybe CompilerSupport
formatSupport "errorformat" ef = errorFormatSupport <$> ef
formatSupport f _ = supportFor f

knownFormats :: [Text]
knownFormats = map supportFormat supports ++ ["errorformat"]

-- | Whether a format can say which cases a `case` lacks (what `complete` and
-- `uncovered` read).
supportsMissing :: Text -> Bool
supportsMissing = (`elem` ["ghc-json", "pyright-json", "rustc-json", "elm-json", "olifant-json"])

-- | The @[test] format@s a test runner can read.
knownTestFormats :: [Text]
knownTestFormats = ["pytest"]
