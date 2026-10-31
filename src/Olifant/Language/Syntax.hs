-- | The syntax port: run an ast-grep rule over a file's text.
module Olifant.Language.Syntax (Syntax (..), Match (..), Rewrite (..), SyntaxFailure (..), querying, noSyntax, describeSyntaxFailure) where

import Data.Aeson (Value)
import Data.Text (Text)
import Olifant.Language.Adapter (Language)
import Olifant.Provider (Provider)

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
-- rule, a fix and the text, and finds each match with the fix applied to it;
-- `runSlotProvider` runs a slot's provider on a request (the anchors slot,
-- which resolves in any monad, asks through it).
data Syntax m = Syntax
  { querySyntax :: Language -> Value -> Text -> m (Either SyntaxFailure [Match]),
    rewriteSyntax :: Language -> Value -> Text -> Text -> m (Either SyntaxFailure [Rewrite]),
    runSlotProvider :: Provider -> Value -> m (Either Text Value)
  }

-- | A syntax that only queries; rewriting it is a tool failure.
querying :: Applicative m => (Language -> Value -> Text -> m (Either SyntaxFailure [Match])) -> Syntax m
querying q = Syntax q (\_ _ _ _ -> pure (Left (SyntaxFailed "this syntax cannot rewrite"))) noProviderRunner

noSyntax :: Applicative m => Syntax m
noSyntax = Syntax (\_ _ _ -> missing) (\_ _ _ _ -> missing) noProviderRunner
  where
    missing = pure (Left (SyntaxToolMissing "ast-grep is not installed (brew install ast-grep)"))

-- | A syntax that cannot run providers: every slot it is asked is unavailable.
noProviderRunner :: Applicative m => Provider -> Value -> m (Either Text Value)
noProviderRunner _ _ = pure (Left "no provider runner")

describeSyntaxFailure :: SyntaxFailure -> Text
describeSyntaxFailure = \case
  SyntaxToolMissing why -> why
  SyntaxFailed why -> why
