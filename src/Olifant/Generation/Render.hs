-- | Rendering a generator's edits in an environment: target paths, anchors
-- and contents become concrete, ready for the planner to decide.
module Olifant.Generation.Render
  ( RenderedEdit (..)
  , CompleteSpec (..)
  , renderedPath
  , renderedUnless
  , renderEdits
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Anchor (RenderedAnchor (..))
import Olifant.Generator (AnchorSpec (..), EditWith (..), OnExists (..))
import Olifant.Language (renderRule)
import Olifant.Template (Env, Template, TemplateError, render)
import Olifant.Workspace (normalisePath)

data RenderedEdit
  = -- | path, content, on_exists, unless-glob
    RenderedCreate FilePath Text OnExists (Maybe Text)
  | -- | path, anchors (tried in order), snippet
    RenderedInsert FilePath [RenderedAnchor] Text (Maybe Text)
  | -- | resolved after a check (spec §5.6); the body is rendered per site
    RenderedComplete CompleteSpec

data CompleteSpec = CompleteSpec
  { completeConstructor :: Text
  , completeWhere :: Maybe Text
  , completeBody :: Maybe Template
  , completeEnv :: Env
  , completeLanguage :: Text
  }

renderedPath :: RenderedEdit -> Maybe FilePath
renderedPath = \case
  RenderedCreate p _ _ _ -> Just p
  RenderedInsert p _ _ _ -> Just p
  RenderedComplete _ -> Nothing

-- | The first argument is the generator's language (it selects the checker
-- for `complete` edits).
renderEdits :: Text -> Env -> [EditWith Template] -> Either TemplateError [RenderedEdit]
renderEdits language env = traverse $ \case
  Create target src onExists u -> RenderedCreate <$> path target <*> render env src <*> pure onExists <*> traverse (render env) u
  Insert target anchors src u -> RenderedInsert <$> path target <*> traverse anchor anchors <*> render env src <*> traverse (render env) u
  Complete ctor wher body -> (\c w -> RenderedComplete (CompleteSpec c w body env language)) <$> render env ctor <*> traverse (render env) wher
  where
    path t = normalisePath . T.unpack <$> render env t
    anchor = \case
      AtPhrase t -> PhraseAnchor <$> render env t
      AtRule r pos -> (`RuleAnchor` pos) <$> renderRule env r

renderedUnless :: RenderedEdit -> Maybe Text
renderedUnless = \case
  RenderedCreate _ _ _ u -> u
  RenderedInsert _ _ _ u -> u
  RenderedComplete _ -> Nothing
