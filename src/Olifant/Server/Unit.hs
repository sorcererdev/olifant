-- | Internal to Olifant.Server: the unit at a line, the innermost
-- definition (a function, method or class) enclosing it, by the node kinds
-- the language's `[unit]` lists.
module Olifant.Server.Unit
  ( Unit (..)
  , unitAt
  , unitHeader
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

-- | The header of the unit at LINE (1-based) of a Python module: for a method, its
-- class line and the class's `__init__` def line (through its closing `:`); for a free
-- function, its def line; empty when neither.
unitHeader :: Text -> Int -> Text
unitHeader text line = case ([c | c <- cands, isHead "class " (snd c)], [d | d <- cands, not (isHead "class " (snd d))]) of
  ((ci, cl) : _, _ : _) -> T.intercalate "\n" (cl : initDef ci cl)
  (_, (_, d) : _) -> T.stripEnd d
  _ -> ""
  where
    cands = [(i, l) | (i, l) <- reverse (take line ls), isHead "class " l || isHead "def " l || isHead "async def " l, enclosing i l]
    ls = zip [1 :: Int ..] (T.lines text)
    indentOf = T.length . T.takeWhile (== ' ')
    blank = T.null . T.strip
    isHead p = (p `T.isPrefixOf`) . T.stripStart
    enclosing i l =
      i == line
        || all (\(_, x) -> blank x || indentOf x > indentOf l) [x | x@(j, _) <- ls, j > i, j <= line]
    initDef ci cl =
      let body = takeWhile (\(_, x) -> blank x || indentOf x > indentOf cl) (drop ci ls)
       in case dropWhile (not . isHead "def __init__(" . snd) body of
            [] -> []
            found -> [T.intercalate "\n" (map snd (upToColon found))]
    upToColon [] = []
    upToColon (x@(_, l) : r)
      | ":" `T.isSuffixOf` T.stripEnd (T.takeWhile (/= '#') l) = [x]
      | otherwise = x : upToColon r
