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
import Data.Text qualified as T
import Olifant.Anchor (Located (..), Miss, RenderedAnchor, insertLines, placedText, presentAt)
import Olifant.Generation.Compose (PlanItem (..))
import Olifant.Generation.Plan
import Olifant.Generation.Render (RenderedEdit (..), renderedPath, renderedUnless)
import Olifant.Glob (matchesGlob)
import Olifant.Holes (matchesWithHoles)
import Olifant.Generator (OnExists (..), Unless (..))
import Text.Regex.TDFA (Regex, makeRegexM, matchTest)
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
      -- `complete` edits are planned later, by the check loop.
      PlanEdit _ (RenderedComplete _) -> pure acc
      PlanEdit path edit
        | Just u <- renderedUnless edit, Just why <- matchUnless (fromMaybe "" (renderedPath edit)) u current ->
            pure (current, planned path n' (fromMaybe "" (renderedPath edit)) (Skipped why) : edits, conflicts, n')
      PlanEdit path (RenderedCreate p content onExists _) -> pure (record n' (decideCreate path n' p content onExists current))
      PlanEdit path (RenderedInsert p anchors snippet _) -> record n' <$> decideInsert path n' p anchors snippet current
      PlanEdit path (RenderedReplace p range old new) -> pure (record n' (decideReplace path n' p range old new current))
      where
        record m (current', new, conflict) = (current', new : edits, maybe conflicts (: conflicts) conflict, m)
    -- `unless.exists` sees files on disk and files this plan has created so
    -- far; `unless.matches` the target's lines as this plan has them so far.
    matchUnless target u current = case u of
      UnlessExists g -> UnlessMatched <$> listToMaybe (sort (nub [p | p <- diskPaths ++ [q | (q, Just _) <- M.toList current], matchesGlob g (p :: FilePath)]))
      UnlessMatches re -> do
        rx <- makeRegexM (T.unpack re) :: Maybe Regex
        content <- M.findWithDefault Nothing target current
        listToMaybe [UnlessFound target i | (i, l) <- zip [1 ..] (T.lines content), matchTest rx (T.unpack l)]
    decideCreate path n p content onExists current
      | Just problem <- unwritable n p = (current, planned path n p Conflicted, Just problem)
      | otherwise = case M.findWithDefault Nothing p current of
            Nothing -> (M.insert p (Just content) current, planned path n p (WillCreate content), Nothing)
            Just old
              | matchesWithHoles content old -> (current, planned path n p (Skipped UnchangedContent), Nothing)
              | otherwise -> case onExists of
                  ErrorIfExists -> (current, planned path n p Conflicted, Just (TargetExists n p))
                  SkipIfExists -> (current, planned path n p (Skipped AlreadyExists), Nothing)
                  OverwriteIfExists -> (M.insert p (Just content) current, planned path n p (WillOverwrite content), Nothing)
    decideInsert path n p anchors snippet current
      | Just problem <- unwritable n p = pure (current, planned path n p Conflicted, Just problem)
      | otherwise = case M.findWithDefault Nothing p current of
            Nothing -> pure (current, planned path n p Conflicted, Just (InsertTargetMissing n p))
            Just old ->
              locate p anchors old <&> \case
                Left misses -> (current, planned path n p Conflicted, Just (AnchorUnresolved n p misses))
                Right loc
                  | presentAt loc snippet old -> (current, (planned path n p (Skipped AlreadyPresent)) {editAnchor = Just (locUsed loc)}, Nothing)
                  | otherwise ->
                      let (line, updated) = insertLines loc snippet old
                       in (M.insert p (Just updated) current, (planned path n p (WillInsert line (placedText loc snippet))) {editAnchor = Just (locUsed loc)}, Nothing)
    -- A rule's fix: the lines must still read what the rule saw.
    decideReplace path n p (l1, l2) old new current
      | Just problem <- unwritable n p = (current, planned path n p Conflicted, Just problem)
      | otherwise = case M.findWithDefault Nothing p current of
          Nothing -> (current, planned path n p Conflicted, Just (InsertTargetMissing n p))
          Just content ->
            let ls = T.lines content
                (before, rest) = splitAt (l1 - 1) ls
                (target, after) = splitAt (l2 - l1 + 1) rest
                newLines = if T.null new then [] else T.lines new
                same a b = map T.stripEnd a == map T.stripEnd b
             in if same target (T.lines old)
                  then (M.insert p (Just (T.unlines (before ++ newLines ++ after))) current, planned path n p (WillReplace l1 (length target) new), Nothing)
                  else
                    if not (null newLines) && same (take (length newLines) rest) newLines
                      then (current, planned path n p (Skipped AlreadyPresent), Nothing)
                      else (current, planned path n p Conflicted, Just (TargetUnusable n p "the code to replace changed since the rule saw it; check again"))
    unwritable n p
      | not (isSafeRelative p) = Just (UnsafePath n p)
      | Just why <- M.lookup p blocked = Just (TargetUnusable n p why)
      | otherwise = Nothing
