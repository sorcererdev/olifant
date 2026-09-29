-- | The syntax port: run an ast-grep rule over a file's text.
module Olifant.Language.Syntax (Syntax (..), Match (..), SyntaxFailure (..), noSyntax, describeSyntaxFailure) where

import Data.Aeson (Value)
import Data.Text (Text)
import Olifant.Language.Adapter (Language)

-- | A matched node: 0-based (line, column) of its start and end.
data Match = Match {matchStart :: (Int, Int), matchEnd :: (Int, Int), matchText :: Text}
  deriving stock (Eq, Show)

data SyntaxFailure = SyntaxToolMissing Text | SyntaxFailed Text
  deriving stock (Eq, Show)

newtype Syntax m = Syntax {querySyntax :: Language -> Value -> Text -> m (Either SyntaxFailure [Match])}

noSyntax :: Applicative m => Syntax m
noSyntax = Syntax (\_ _ _ -> pure (Left (SyntaxToolMissing "ast-grep is not installed (brew install ast-grep)")))

describeSyntaxFailure :: SyntaxFailure -> Text
describeSyntaxFailure = \case
  SyntaxToolMissing why -> why
  SyntaxFailed why -> why
