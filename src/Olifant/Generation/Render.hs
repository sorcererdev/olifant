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
import Olifant.Generator (AnchorSpec (..), EditWith (..), OnExists (..), Unless (..))
import Olifant.Language (renderRule)
import Olifant.Template (Env, Template, TemplateError (..), render)
import Olifant.Workspace (normalisePath)

data RenderedEdit
  = -- | path, content, on_exists, unless
    RenderedCreate FilePath Text OnExists (Maybe (Unless Text))
  | -- | path, anchors (tried in order), snippet, unless
    RenderedInsert FilePath [RenderedAnchor] Text (Maybe (Unless Text))
  | -- | resolved after a check (spec §5.6); the body is rendered per site
    RenderedComplete CompleteSpec
  | -- | path, lines (1-based, inclusive), the text they must hold, the new text
    RenderedReplace FilePath (Int, Int) Text Text

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
  RenderedReplace p _ _ _ -> Just p

-- | The first argument is the generator's language (it selects the checker
-- for `complete` edits).
renderEdits :: Text -> Env -> [EditWith Template] -> Either TemplateError [RenderedEdit]
renderEdits language env = traverse $ \case
  Create target src onExists u -> RenderedCreate <$> path target <*> render env src <*> pure onExists <*> traverse (traverse (render env)) u
  Insert target anchors src u -> RenderedInsert <$> path target <*> traverse anchor anchors <*> render env src <*> traverse (traverse (render env)) u
  Complete ctor wher body -> (\c w -> RenderedComplete (CompleteSpec c w body env language)) <$> render env ctor <*> traverse (render env) wher
  Replace target range old src -> (\p t -> RenderedReplace p range old t) <$> path target <*> render env src
  -- placed by `gen --at` before planning ("Olifant.Cursor"); never reaches here otherwise
  Cursor _ -> Left (TypeMismatch "a cursor edit (a snippet) needs a cursor: gen --at FILE:LINE:COL")
  where
    path t = normalisePath . T.unpack <$> render env t
    anchor = \case
      AtPhrase t -> PhraseAnchor <$> render env t
      AtRule r pos -> (`RuleAnchor` pos) <$> renderRule env r

renderedUnless :: RenderedEdit -> Maybe (Unless Text)
renderedUnless = \case
  RenderedCreate _ _ _ u -> u
  RenderedInsert _ _ _ u -> u
  RenderedComplete _ -> Nothing
  RenderedReplace {} -> Nothing
