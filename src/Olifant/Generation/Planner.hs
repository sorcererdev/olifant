module Olifant.Generation.Planner
  ( Locate
  , decideItems
  ) where

import Control.Monad (foldM)
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.List (nub, sort)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Olifant.Anchor (Located (..), Miss, RenderedAnchor, insertLines, presentAt)
import Olifant.Generation.Compose (PlanItem (..))
import Olifant.Generation.Plan
import Olifant.Generation.Render (RenderedEdit (..), renderedPath, renderedUnless)
import Olifant.Glob (matchesGlob)
import Olifant.Generator (OnExists (..))
import Olifant.Workspace (isSafeRelative)

-- | Resolve an insert's anchors in a file's current text (spec §5.2).
type Locate m = FilePath -> [RenderedAnchor] -> Text -> m (Either [Miss] Located)

-- | Decide every positional item in order, threading file contents so later
-- edits and requirements see earlier ones. Edits are numbered 1.. across the
-- plan; `complete` edits are left for the check phase (spec §5.6) and are
-- not numbered here. @blocked@ maps paths that cannot be written to the
-- reason; @onDisk@ holds required paths that exist even if unreadable.
-- Returns the edits, conflicts, the contents after these edits, and the
-- next edit number.
decideItems :: Monad m => Locate m -> [FilePath] -> [PlanItem] -> Map FilePath (Maybe Text) -> Map FilePath Text -> Set FilePath -> m ([PlannedEdit], [Conflict], Map FilePath (Maybe Text), Int)
decideItems locate diskPaths items original blocked onDisk = finish <$> foldM step (original, [], [], 0 :: Int) items
  where
    finish (current, edits, conflicts, n) = (reverse edits, reverse conflicts, current, n + 1)
    step acc@(current, edits, conflicts, n) = let n' = n + 1 in \case
      PlanRequire path required hint ->
        pure $ case [p | p <- required, isNothing (M.findWithDefault Nothing p current), p `S.notMember` onDisk] of
          [] -> acc
          missing -> (current, edits, RequirementUnmet path missing hint : conflicts, n)
      PlanEdit _ (RenderedComplete _) -> pure acc
      PlanEdit path edit
        | Just g <- renderedUnless edit, Just hit <- matchUnless g current ->
            pure (current, planned path n' (fromMaybe "" (renderedPath edit)) (Skipped (UnlessMatched hit)) : edits, conflicts, n')
        | otherwise ->
            decideEdit path n' edit current <&> \(current', new, conflict) ->
              (current', new : edits, maybe conflicts (: conflicts) conflict, n')
    -- `unless` sees files on disk and files this plan has created so far.
    matchUnless g current =
      listToMaybe (sort (nub [p | p <- diskPaths ++ [q | (q, Just _) <- M.toList current], matchesGlob g (p :: FilePath)]))
    decideEdit path n edit current = case edit of
      RenderedCreate p content onExists _
        | Just problem <- unwritable p -> pure (current, planned path n p Conflicted, Just problem)
        | otherwise -> pure $ case existing p of
            Nothing -> (M.insert p (Just content) current, planned path n p (WillCreate content), Nothing)
            Just old
              | old == content -> (current, planned path n p (Skipped UnchangedContent), Nothing)
              | otherwise -> case onExists of
                  ErrorIfExists -> (current, planned path n p Conflicted, Just (TargetExists n p))
                  SkipIfExists -> (current, planned path n p (Skipped AlreadyExists), Nothing)
                  OverwriteIfExists -> (M.insert p (Just content) current, planned path n p (WillOverwrite content), Nothing)
      RenderedInsert p anchors snippet _
        | Just problem <- unwritable p -> pure (current, planned path n p Conflicted, Just problem)
        | otherwise -> case existing p of
            Nothing -> pure (current, planned path n p Conflicted, Just (InsertTargetMissing n p))
            Just old ->
              locate p anchors old <&> \case
                Left misses -> (current, planned path n p Conflicted, Just (AnchorUnresolved n p misses))
                Right loc
                  | presentAt loc snippet old -> (current, (planned path n p (Skipped AlreadyPresent)) {editAnchor = Just (locUsed loc)}, Nothing)
                  | otherwise ->
                      let (line, updated) = insertLines loc snippet old
                       in (M.insert p (Just updated) current, (planned path n p (WillInsert line snippet)) {editAnchor = Just (locUsed loc)}, Nothing)
      RenderedComplete _ -> pure (current, planned path n "" Conflicted, Nothing)
      where
        existing p = M.findWithDefault Nothing p current
        unwritable p
          | not (isSafeRelative p) = Just (UnsafePath n p)
          | Just why <- M.lookup p blocked = Just (TargetUnusable n p why)
          | otherwise = Nothing
