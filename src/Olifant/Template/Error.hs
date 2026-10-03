module Olifant.Template.Error (TemplateError (..), describeTemplateError) where

import Data.Text (Text)
import Data.Text qualified as T

data TemplateError
  = SyntaxError Int Text
  | UndefinedVariable Text
  | TypeMismatch Text
  deriving stock (Eq, Show)

describeTemplateError :: TemplateError -> Text
describeTemplateError = \case
  SyntaxError line msg -> "line " <> T.pack (show line) <> ": " <> msg
  UndefinedVariable v -> "undefined variable `" <> v <> "`"
  TypeMismatch msg -> msg
