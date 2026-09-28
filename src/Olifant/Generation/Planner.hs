module Olifant.Generation.Planner
  ( RenderedEdit (..)
  , renderedPath
  , renderEdits
  , decideEdits
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Generation.Marker
import Olifant.Generation.Plan
import Olifant.Generator (Anchor (..), EditWith (..), OnExists (..))
import Olifant.Template (Env, Template, TemplateError, render)
import Olifant.Workspace (isSafeRelative, normalisePath)

data RenderedEdit
  = RenderedCreate FilePath Text OnExists
  | -- | path, marker, snippet
    RenderedInsert FilePath Text Text

renderedPath :: RenderedEdit -> FilePath
renderedPath = \case
  RenderedCreate p _ _ -> p
  RenderedInsert p _ _ -> p

renderEdits :: Env -> [EditWith Template] -> Either TemplateError [RenderedEdit]
renderEdits env = traverse $ \case
  Create target src onExists -> RenderedCreate <$> path target <*> render env src <*> pure onExists
  Insert target (Marker m) src -> RenderedInsert <$> path target <*> pure m <*> render env src
  where
    path t = normalisePath . T.unpack <$> render env t

-- | Decide what every edit does, threading file contents so later edits see
-- earlier ones. @blocked@ maps paths that cannot be written to the reason.
-- Returns edits, conflicts, and the final contents to write.
decideEdits :: [RenderedEdit] -> Map FilePath (Maybe Text) -> Map FilePath Text -> ([PlannedEdit], [Conflict], Map FilePath Text)
decideEdits rendered original blocked = finish (foldl step (original, [], []) (zip [1 ..] rendered))
  where
    finish (current, edits, conflicts) =
      ( reverse edits
      , reverse conflicts
      , M.fromList [(p, c) | (p, Just c) <- M.toList current, M.lookup p original /= Just (Just c)]
      )
    step (current, edits, conflicts) (n, edit)
      | not (isSafeRelative (renderedPath edit)) = (current, edits, UnsafePath n (renderedPath edit) : conflicts)
      | Just why <- M.lookup (renderedPath edit) blocked = (current, edits, TargetUnusable n (renderedPath edit) why : conflicts)
      | otherwise = case edit of
          RenderedCreate p content onExists -> case existing p of
            Nothing -> (M.insert p (Just content) current, PlannedEdit n p (WillCreate content) : edits, conflicts)
            Just old
              | old == content -> (current, PlannedEdit n p (Skipped UnchangedContent) : edits, conflicts)
              | otherwise -> case onExists of
                  ErrorIfExists -> (current, edits, TargetExists n p : conflicts)
                  SkipIfExists -> (current, PlannedEdit n p (Skipped AlreadyExists) : edits, conflicts)
                  OverwriteIfExists -> (M.insert p (Just content) current, PlannedEdit n p (WillOverwrite content) : edits, conflicts)
          RenderedInsert p marker snippet -> case existing p of
            Nothing -> (current, edits, InsertTargetMissing n p : conflicts)
            Just old -> case markerBlock marker old >>= \block -> fmap (block,) (insertBeforeMarker marker snippet old) of
              Left MarkerMissing -> (current, edits, MarkerNotFound n p marker : conflicts)
              Left (MarkerAmbiguous k) -> (current, edits, MarkerRepeated n p marker k : conflicts)
              Right (block, (line, updated))
                | alreadyPresent snippet block -> (current, PlannedEdit n p (Skipped AlreadyPresent) : edits, conflicts)
                | otherwise -> (M.insert p (Just updated) current, PlannedEdit n p (WillInsert line snippet) : edits, conflicts)
      where
        existing p = M.findWithDefault Nothing p current
