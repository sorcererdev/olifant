-- | The syntax port: run an ast-grep rule over a file's text.
module Olifant.Language.Syntax (Syntax (..), Match (..), Rewrite (..), SyntaxFailure (..), querying, noSyntax, describeSyntaxFailure) where

import Data.Aeson (Value)
import Data.Text (Text)
import Olifant.Language.Adapter (Language)

-- | A matched node: 0-based (line, column) of its start and end, and the
-- byte offsets of the same range in the UTF-8 source.
data Match = Match {matchStart :: (Int, Int), matchEnd :: (Int, Int), matchText :: Text, matchBytes :: (Int, Int)}
  deriving stock (Eq, Show)

-- | A matched node with the text a rule's `fix` makes of it, positioned as a `Match`.
data Rewrite = Rewrite {rwStart :: (Int, Int), rwEnd :: (Int, Int), rwText :: Text, rwReplacement :: Text, rwBytes :: (Int, Int)}
  deriving stock (Eq, Show)

data SyntaxFailure = SyntaxToolMissing Text | SyntaxFailed Text
  deriving stock (Eq, Show)

-- | `querySyntax` finds a rule's matches in a text; `rewriteSyntax` takes the
-- rule, a fix and the text, and finds each match with the fix applied to it.
data Syntax m = Syntax
  { querySyntax :: Language -> Value -> Text -> m (Either SyntaxFailure [Match]),
    rewriteSyntax :: Language -> Value -> Text -> Text -> m (Either SyntaxFailure [Rewrite])
  }

-- | A syntax that only queries; rewriting it is a tool failure.
querying :: Applicative m => (Language -> Value -> Text -> m (Either SyntaxFailure [Match])) -> Syntax m
querying q = Syntax q (\_ _ _ _ -> pure (Left (SyntaxFailed "this syntax cannot rewrite")))

noSyntax :: Applicative m => Syntax m
noSyntax = Syntax (\_ _ _ -> missing) (\_ _ _ _ -> missing)
  where
    missing = pure (Left (SyntaxToolMissing "ast-grep is not installed (brew install ast-grep)"))

describeSyntaxFailure :: SyntaxFailure -> Text
describeSyntaxFailure = \case
  SyntaxToolMissing why -> why
  SyntaxFailed why -> why
