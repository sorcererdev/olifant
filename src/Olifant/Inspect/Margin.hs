-- | What inspect shows in the editor: the margin of one file as pulled
-- diagnostics (source `olifant-inspect`, a hint on the finding's first line,
-- code = its kind, `-stale` when its unit changed since it was inspected), and
-- the *Show why* page.
module Olifant.Inspect.Margin
  ( inspectDiagnostics
  , shownLines
  , whyText
  , inspectSource
  ) where

import Data.Aeson (Value (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Types
import Olifant.Catalog (InspectConfig)
import Olifant.Inspect.Steering (Steering, ruleText)
import Olifant.Inspect.Store (Place (..), InspectFinding (..), InspectStore, shownIn)

inspectSource :: Text
inspectSource = "olifant-inspect"

-- | The file's shown findings ('shownIn'), each at its line now (1-based).
-- UNITS are the file's units now: name to (fingerprint, first line). A
-- finding moves with its unit; one whose unit is gone is not shown.
shownLines :: InspectConfig -> Map Text (Text, Int) -> FilePath -> InspectStore -> [(Int, InspectFinding)]
shownLines cfg units file vs =
  [ (line, f)
  | f <- shownIn cfg file vs
  , let pl = vfPlace f
        first = fst (plLines pl)
  , Just line <- [case plUnit pl of
      Nothing -> Just first
      Just u -> case M.lookup u units of
        Nothing -> Nothing
        Just (_, startNow) -> Just (maybe first (\startThen -> startNow + first - startThen) (plUnitStart pl))]
  ]

-- | The margin as pulled diagnostics: a hint on each shown finding's line,
-- code = its kind, `-stale` when its unit's fingerprint moved.
inspectDiagnostics :: InspectConfig -> Map Text (Text, Int) -> FilePath -> InspectStore -> [Diagnostic]
inspectDiagnostics cfg units file vs =
  [ Diagnostic (Range (Position l 0) (Position (l + 1) 0)) (Just DiagnosticSeverity_Hint) (Just (InR code)) Nothing (Just inspectSource) (vfStatement f) Nothing Nothing (Just (String (vfId f)))
  | (line, f) <- shownLines cfg units file vs
  , let l = fromIntegral (max 0 (line - 1))
        moved = case (plUnit (vfPlace f), plFingerprint (vfPlace f)) of
          (Just u, Just fp) -> fmap fst (M.lookup u units) /= Just fp
          _ -> False
        code = vfKind f <> (if moved then "-stale" else "")
  ]

-- | The *Show why* page: the statement, the rule quoted, the evidence, the verdict.
whyText :: Steering -> InspectFinding -> Text
whyText st f =
  T.unlines $
    [ "# " <> vfKind f <> " at " <> T.pack (plFile pl) <> ":" <> tshow (fst (plLines pl))
    , ""
    , vfStatement f
    , ""
    ]
      ++ maybe [] (\r -> ["## The rule: " <> r, "", fromMaybe "(no longer in inspect.md)" (ruleText st r), ""]) (vfRule f)
      ++ ["## Evidence", ""]
      ++ ["- " <> T.pack p <> ":" <> tshow a <> (if z /= a then "-" <> tshow z else "") | (p, (a, z)) <- vfEvidence f]
      ++ ["", "## Checked", "", "- not a nitpick: " <> maybe "-" (T.pack . show) (vfKeep f), "- claim check: " <> fromMaybe "not needed for this kind" (vfVerdict f), "- seen " <> tshow (vfSeen f) <> " time" <> (if vfSeen f == 1 then "" else "s")]
  where
    pl = vfPlace f
    tshow :: Show a => a -> Text
    tshow = T.pack . show
