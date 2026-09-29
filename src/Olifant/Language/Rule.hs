-- | ast-grep rules (kind/inside/has/regex/not/any/all …) whose strings are
-- templates, so a target like `case in FUNCTION` can use its parameters.
module Olifant.Language.Rule (Rule (..), ruleFromTree, renderRule) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Bifunctor (first)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Olifant.Template (Env, Template, TemplateError, describeTemplateError, parseTemplate, render)
import Olifant.Toml (Tree (..), describeTree, inKey)

data Rule = RObject [(Text, Rule)] | RList [Rule] | RText Template | RInt Integer | RBool Bool
  deriving stock (Show)

ruleFromTree :: Tree -> Either Text Rule
ruleFromTree = \case
  TText s -> RText <$> first describeTemplateError (parseTemplate s)
  TInt i -> Right (RInt i)
  TBool b -> Right (RBool b)
  TList xs -> RList <$> traverse ruleFromTree xs
  TTable t -> RObject <$> traverse (\(k, v) -> inKey k ((k,) <$> ruleFromTree v)) (M.toList t)
  other -> Left ("expected part of an ast-grep rule, got " <> describeTree other)

renderRule :: Env -> Rule -> Either TemplateError Value
renderRule env = \case
  RText t -> String <$> render env t
  RInt i -> Right (toJSON i)
  RBool b -> Right (Bool b)
  RList xs -> toJSON <$> traverse (renderRule env) xs
  RObject kvs -> object <$> traverse (\(k, v) -> (Key.fromText k .=) <$> renderRule env v) kvs
