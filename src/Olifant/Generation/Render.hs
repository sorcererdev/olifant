-- | Rendering a generator's edits in an environment: target paths and
-- contents become concrete, ready for the planner to decide.
module Olifant.Generation.Render
  ( RenderedEdit (..)
  , renderedPath
  , renderEdits
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Generator (Anchor (..), EditWith (..), OnExists (..))
import Olifant.Template (Env, Template, TemplateError, render)
import Olifant.Workspace (normalisePath)

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
