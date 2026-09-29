module Olifant.Generation.Planner
  ( decideItems
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isNothing)
import Data.Text (Text)
import Olifant.Generation.Compose (PlanItem (..))
import Olifant.Generation.Marker
import Olifant.Generation.Plan
import Olifant.Generation.Render (RenderedEdit (..), renderedPath)
import Olifant.Generator (OnExists (..))
import Olifant.Workspace (isSafeRelative)

-- | Decide every item in order, threading file contents so later edits and
-- requirements see earlier ones. Edits are numbered 1.. across the whole
-- plan. @blocked@ maps paths that cannot be written to the reason.
decideItems :: [PlanItem] -> Map FilePath (Maybe Text) -> Map FilePath Text -> ([PlannedEdit], [Conflict], Map FilePath Text)
decideItems items original blocked = finish (foldl step (original, [], [], 0 :: Int) items)
  where
    finish (current, edits, conflicts, _) =
      ( reverse edits
      , reverse conflicts
      , M.fromList [(p, c) | (p, Just c) <- M.toList current, M.lookup p original /= Just (Just c)]
      )
    step (current, edits, conflicts, n) = \case
      PlanRequire path required hint ->
        case [p | p <- required, isNothing (M.findWithDefault Nothing p current)] of
          [] -> (current, edits, conflicts, n)
          missing -> (current, edits, RequirementUnmet path missing hint : conflicts, n)
      PlanEdit path edit ->
        let (current', edits', conflicts') = decideEdit path (n + 1) edit current edits conflicts
         in (current', edits', conflicts', n + 1)
    decideEdit path n edit current edits conflicts
      | not (isSafeRelative (renderedPath edit)) = (current, edits, UnsafePath n (renderedPath edit) : conflicts)
      | Just why <- M.lookup (renderedPath edit) blocked = (current, edits, TargetUnusable n (renderedPath edit) why : conflicts)
      | otherwise = case edit of
          RenderedCreate p content onExists -> case existing p of
            Nothing -> (M.insert p (Just content) current, PlannedEdit path n p (WillCreate content) : edits, conflicts)
            Just old
              | old == content -> (current, PlannedEdit path n p (Skipped UnchangedContent) : edits, conflicts)
              | otherwise -> case onExists of
                  ErrorIfExists -> (current, edits, TargetExists n p : conflicts)
                  SkipIfExists -> (current, PlannedEdit path n p (Skipped AlreadyExists) : edits, conflicts)
                  OverwriteIfExists -> (M.insert p (Just content) current, PlannedEdit path n p (WillOverwrite content) : edits, conflicts)
          RenderedInsert p marker snippet -> case existing p of
            Nothing -> (current, edits, InsertTargetMissing n p : conflicts)
            Just old -> case markerBlock marker old >>= \block -> fmap (block,) (insertBeforeMarker marker snippet old) of
              Left MarkerMissing -> (current, edits, MarkerNotFound n p marker : conflicts)
              Left (MarkerAmbiguous k) -> (current, edits, MarkerRepeated n p marker k : conflicts)
              Right (block, (line, updated))
                | alreadyPresent snippet block -> (current, PlannedEdit path n p (Skipped AlreadyPresent) : edits, conflicts)
                | otherwise -> (M.insert p (Just updated) current, PlannedEdit path n p (WillInsert line snippet) : edits, conflicts)
      where
        existing p = M.findWithDefault Nothing p current
