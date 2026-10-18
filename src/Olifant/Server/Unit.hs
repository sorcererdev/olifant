-- | Internal to Olifant.Server: the unit at a line, the innermost
-- definition (a function, method or class) enclosing it, by the node kinds
-- the language's `[unit]` lists.
module Olifant.Server.Unit
  ( Unit (..)
  , unitAt
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (object, (.=))
import Data.Char (isAlphaNum)
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language (Language (..), Match (..), Syntax (..), describeSyntaxFailure)
import Olifant.Server.Site (defName, tshow)

-- | A unit (1-based lines): its name, its header line, its node's lines,
-- and its body's (the lines after the header; Nothing for a one-line def).
data Unit = Unit
  { unitName :: Text
  , unitDef :: Int
  , unitLines :: (Int, Int)
  , unitBody :: Maybe (Int, Int)
  }
  deriving stock (Eq, Show)

-- | The unit enclosing LINE (1-based) of TEXT: the innermost node of the
-- language's `[unit]` kinds there; a decorator's is the def it decorates.
-- Left is one sentence on why there is none.
unitAt :: Syntax IO -> Language -> Text -> Int -> IO (Either Text Unit)
unitAt syntax lang text line
  | null (langUnitKinds lang) = pure (Left (langName lang <> " has no [unit] kinds: Olifant cannot find a function or class in it"))
  | otherwise =
      querySyntax syntax lang rule text <&> \case
        Left f -> Left (describeSyntaxFailure f)
        Right ms -> maybe (Left ("no function or class at line " <> tshow line)) Right (pick [(a + 1, z + 1) | Match (a, _) (z, _) _ _ <- ms])
  where
    rule = object ["any" .= [object ["kind" .= k] | k <- langUnitKinds lang]]
    ls = T.splitOn "\n" text
    lineAt i = fromMaybe "" (listToMaybe (drop (i - 1) ls))
    -- the innermost around the line: the latest start, then the earliest end
    pick spans = listToMaybe (sortOn (\(a, z) -> (Down a, z)) [sp | sp@(a, z) <- spans, a <= line, line <= z]) >>= unitOf
    unitOf (a, z) = do
      (d, n) <- listToMaybe [(i, n) | i <- [a .. z], Just n <- [headerName (lineAt i)]]
      let colon = listToMaybe [i | i <- [d .. z], ":" `T.isSuffixOf` T.stripEnd (fst (T.breakOn " #" (lineAt i)))]
          body = colon >>= \c -> if c + 1 <= z then Just (c + 1, z) else Nothing
      pure (Unit n d (a, z) body)
    headerName l = defName l <|> className l
    className l = do
      rest <- T.stripPrefix "class " (T.strip l)
      let n = T.takeWhile (\c -> isAlphaNum c || c == '_') rest
      if T.null n then Nothing else Just n
