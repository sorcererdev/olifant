-- | Internal to Olifant.Server: the unit at a line, the innermost
-- definition (a function, method or class) enclosing it, by the node kinds
-- the language's `[unit]` lists.
module Olifant.Server.Unit
  ( Unit (..)
  , unitAt
  , unitHeader
  , Named (..)
  , unitsOf
  , unitNamed
  , unitNameOf
  , fingerprint
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (object, (.=))
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum)
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Numeric (showHex)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
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

-- | A unit of a file by name (1-based, inclusive lines, like 'Unit'): its
-- name qualified one level by containment (`Outer.Inner`), its lines, and
-- the line of its definition (after any decorators).
data Named = Named {namedName :: Text, namedLines :: (Int, Int), namedDef :: Int}
  deriving stock (Eq, Show)

-- | Every unit of TEXT (each match of the language's `[unit]` kinds), named.
-- A match with another's span, or with the name and end of the match around it
-- (a decorated def), is the same unit.
unitsOf :: Syntax IO -> Language -> Text -> IO (Either Text [Named])
unitsOf syntax lang text
  | null (langUnitKinds lang) = pure (Left (langName lang <> " has no [unit] kinds: Olifant cannot find a function or class in it"))
  | otherwise =
      querySyntax syntax lang rule text <&> \case
        Left f -> Left (describeSyntaxFailure f)
        Right ms -> Right (qualify (dedupe (concatMap named (sortOn (\(a, z) -> (a, Down z)) [(a + 1, z + 1) | Match (a, _) (z, _) _ _ <- ms])))) 
  where
    rule = object ["any" .= [object ["kind" .= k] | k <- langUnitKinds lang]]
    ls = T.splitOn "\n" text
    lineAt i = fromMaybe "" (listToMaybe (drop (i - 1) ls))
    -- (span, def line, bare name)
    named (a, z) = [((a, z), d, n) | (d, n) <- take 1 [(i, n) | i <- [a .. z], Just n <- [unitNameOf lang (lineAt i)], python || i == a]]
    python = langName lang == "python"
    dedupe = foldl keep []
    keep acc u@(sp@(_, z), _, n) = case [() | (sp'@(_, z'), _, n') <- acc, sp' == sp || (n' == n && z' == z && contains sp' sp)] of
      [] -> acc ++ [u]
      _ -> acc
    contains (a, z) (b, y) = a <= b && y <= z
    qualify us =
      [ Named (maybe n (<> "." <> n) (parentOf sp)) sp d
      | (sp, d, n) <- us
      , let parentOf s = listToMaybe [pn | (ps, _, pn) <- sortOn (\((a, z), _, _) -> (Down a, z)) us, ps /= s, contains ps s]
      ]

-- | The unit called NAME: the exact qualified name (the earliest of several),
-- or a bare name that is the last part of exactly one qualified name.
unitNamed :: Text -> [Named] -> Maybe Named
unitNamed name ns = case sortOn namedLines [n | n <- ns, namedName n == name] of
  n : _ -> Just n
  [] -> case [n | n <- ns, ("." <> name) `T.isSuffixOf` namedName n] of
    [n] -> Just n
    _ -> Nothing

-- | The name on a unit's first line: Python's `def`/`async def`/`class`; for
-- other languages a type's second word, an instance's head, or a bind's first word.
unitNameOf :: Language -> Text -> Maybe Text
unitNameOf lang line
  | langName lang == "python" = do
      rest <- listToMaybe (mapMaybe (`T.stripPrefix` stripped) ["def ", "async def ", "class "])
      nonEmpty (T.takeWhile (\c -> isAlphaNum c || c == '_') (T.stripStart rest))
  | otherwise = case T.words stripped of
      [] -> Nothing
      w : rest
        | w `elem` ["data", "newtype", "type", "class"] -> listToMaybe rest
        | w == "instance" -> nonEmpty (T.strip (fst (T.breakOn " where" (T.drop (T.length w) stripped))))
        | otherwise -> Just w
  where
    stripped = T.strip line
    nonEmpty t = if T.null t then Nothing else Just t

-- | Seven lowercase hex digits naming the lines (1-based, inclusive) of TEXT:
-- FNV-1a 64 over them trimmed, blanks dropped, joined by newlines.
fingerprint :: Text -> (Int, Int) -> Text
fingerprint text (a, z) = T.pack (take 7 (pad (showHex (BS.foldl' step 14695981039346656037 (encodeUtf8 body)) "")))
  where
    body = T.intercalate "\n" (filter (not . T.null) (map T.strip (take (z - a + 1) (drop (a - 1) (T.splitOn "\n" text)))))
    step h b = (h `xor` fromIntegral b) * 1099511628211 :: Word64
    pad s = replicate (16 - length s) '0' ++ s
