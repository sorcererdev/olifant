module Olifant.Template.Error (TemplateError (..), describeTemplateError) where

import Data.Text (Text)
import Data.Text qualified as T

data TemplateError
  = SyntaxError Int Text
  | HolesNotYetSupported Int
  | UndefinedVariable Text
  | TypeMismatch Text
  deriving stock (Eq, Show)

describeTemplateError :: TemplateError -> Text
describeTemplateError = \case
  SyntaxError line msg -> "line " <> T.pack (show line) <> ": " <> msg
  HolesNotYetSupported line ->
    "line " <> T.pack (show line) <> ": holes (<$ … $>) arrive in step 3; write {{ '<$' }} for a literal <$"
  UndefinedVariable v -> "undefined variable `" <> v <> "`"
  TypeMismatch msg -> msg
