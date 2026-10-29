-- | Internal to Olifant.Server: the unit at a line, the innermost
-- definition (a function, method or class) enclosing it, by the node kinds
-- the language's `[unit]` lists.
module Olifant.Server.Unit
  ( Unit (..)
  , unitAt
  , unitHeader
  , isStubIn
  , Named (..)
  , unitsOf
  , unitAround
  , unitNamed
  , unitNameIn
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
import Olifant.Language (Language (..), Match (..), Syntax (..), describeSyntaxFailure, regexName)
import Olifant.Server.Site (tshow)
import Text.Regex.TDFA (Regex, makeRegexM, matchTest)

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
-- language's `[unit]` kinds there, named by `[unit] name` on its first line
-- that has one (a decorator's is the def below it); its body starts after
-- the first line that opens a block (`opens_block`, its comment cut).
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
      (d, n) <- listToMaybe [(i, n) | i <- [a .. z], Just n <- [unitNameIn lang (lineAt i)]]
      let opener = listToMaybe [i | i <- [d .. z], opens (fst (T.breakOn (" " <> langComment lang) (lineAt i)))]
          body = opener >>= \c -> if c + 1 <= z then Just (c + 1, z) else Nothing
      pure (Unit n d (a, z) body)
    opens = case langOpensBlock lang >>= \re -> makeRegexM (T.unpack re) :: Maybe Regex of
      Just rx -> matchTest rx . T.unpack . T.stripEnd
      Nothing -> const True

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

-- | Every unit of TEXT (each match of the language's `[unit]` kinds), named
-- by 'unitNameIn' on the first line of its span that has a name. A match
-- with another's span, or with the name and end of the match around it (a
-- decorated def), is the same unit. With `[unit] join`, units of one name at
-- one indentation separated only by blank lines (or touching) are one: a
-- Haskell signature and its equations.
unitsOf :: Syntax IO -> Language -> Text -> IO (Either Text [Named])
unitsOf syntax lang text
  | null (langUnitKinds lang) = pure (Left (langName lang <> " has no [unit] kinds: Olifant cannot find a function or class in it"))
  | otherwise =
      querySyntax syntax lang rule text <&> \case
        Left f -> Left (describeSyntaxFailure f)
        Right ms -> Right (qualify (joined (dedupe (concatMap named (sortOn (\(a, z) -> (a, Down z)) [(a + 1, z + 1) | Match (a, _) (z, _) _ _ <- ms])))))
  where
    rule = object ["any" .= [object ["kind" .= k] | k <- langUnitKinds lang]]
    ls = T.splitOn "\n" text
    lineAt i = fromMaybe "" (listToMaybe (drop (i - 1) ls))
    nameOf = unitNameIn lang
    -- (span, def line, bare name)
    named (a, z) = [((a, z), d, n) | (d, n) <- take 1 [(i, n) | i <- [a .. z], Just n <- [nameOf (lineAt i)]]]
    dedupe = foldl keep []
    keep acc u@(sp@(_, z), _, n) = case [() | (sp'@(_, z'), _, n') <- acc, sp' == sp || (n' == n && z' == z && contains sp' sp)] of
      [] -> acc ++ [u]
      _ -> acc
    contains (a, z) (b, y) = a <= b && y <= z
    joined us
      | langUnitJoin lang = dedupe (foldl join [] us)
      | otherwise = us
    -- a unit joins the latest earlier one of its name and indentation that ends
    -- before it with only blank lines between
    join acc u@((a, z), _, n) = case [i | (i, ((_, e), d', n')) <- reverse (zip [0 :: Int ..] acc), n' == n, e < a, indentAt d' == indentAt a, all (T.null . T.strip . lineAt) [e + 1 .. a - 1]] of
      i : _ -> [if j == i then ((a', z), d', n') else x | (j, x@((a', _), d', n')) <- zip [0 ..] acc]
      [] -> acc ++ [u]
    indentAt i = T.length (T.takeWhile (== ' ') (lineAt i))
    qualify us =
      [ Named (maybe n (<> "." <> n) (parentOf sp)) sp d
      | (sp, d, n) <- us
      , let parentOf s = listToMaybe [pn | (ps, _, pn) <- sortOn (\((a, z), _, _) -> (Down a, z)) us, ps /= s, contains ps s]
      ]

-- | The innermost unit (by 'unitsOf') whose lines hold LINE (1-based) of
-- TEXT: a decorator's line is its def's. Left is one sentence on why there
-- is none.
unitAround :: Syntax IO -> Language -> Text -> Int -> IO (Either Text Named)
unitAround syntax lang text line =
  unitsOf syntax lang text <&> \case
    Left why -> Left why
    Right ns -> case sortOn (\n -> (Down (fst (namedLines n)), snd (namedLines n))) [n | n <- ns, let (a, z) = namedLines n, a <= line, line <= z] of
      n : _ -> Right n
      [] -> Left ("no function or class at line " <> tshow line <> ": put the cursor in one to stub its test")

-- | The unit called NAME: the exact qualified name (the earliest of several),
-- or a bare name that is the last part of exactly one qualified name.
unitNamed :: Text -> [Named] -> Maybe Named
unitNamed name ns = case sortOn namedLines [n | n <- ns, namedName n == name] of
  n : _ -> Just n
  [] -> case [n | n <- ns, ("." <> name) `T.isSuffixOf` namedName n] of
    [n] -> Just n
    _ -> Nothing

-- | The name on a unit's line by the language's `[unit] name` expressions:
-- the first that matches gives its last group (the whole match when it has
-- none), stripped; Nothing when none matches or the name is empty.
unitNameIn :: Language -> Text -> Maybe Text
unitNameIn lang = \line -> listToMaybe (mapMaybe ($ line) namers)
  where
    namers = map regexName (langUnitNames lang)

-- | Seven lowercase hex digits naming the lines (1-based, inclusive) of TEXT:
-- FNV-1a 64 over them trimmed, blanks dropped, joined by newlines, except
-- where a formatter reflows ('reflowed'), so formatting never changes it.
fingerprint :: Text -> (Int, Int) -> Text
fingerprint text (a, z) = T.pack (take 7 (pad (showHex (BS.foldl' step 14695981039346656037 (encodeUtf8 body)) "")))
  where
    body = reflowed (filter (not . T.null) (map T.strip (take (z - a + 1) (drop (a - 1) (T.splitOn "\n" text)))))
    step h b = (h `xor` fromIntegral b) * 1099511628211 :: Word64
    pad s = replicate (16 - length s) '0' ++ s

-- | Trimmed lines joined as one would be had a formatter not split it:
-- after an opening bracket and before a closing one with nothing between
-- (a magic trailing comma dropped), after a comma with one space, before a
-- leading comma with none; every other break is a newline. Where any break
-- was joined, a space just inside a bracket goes too (leading-comma style
-- opens `[ a`). A body with no such break joins as before.
reflowed :: [Text] -> Text
reflowed = \case
  [] -> ""
  l : ls -> go False l ls
  where
    go joined acc [] = if joined then tight acc else acc
    go joined acc (n : rest)
      | Just (c, _) <- T.uncons n, c `elem` (")]}" :: String) = go True (fromMaybe acc (T.stripSuffix "," acc) <> n) rest
      | Just (_, c) <- T.unsnoc acc, c `elem` ("([{" :: String) = go True (acc <> n) rest
      | "," `T.isSuffixOf` acc = go True (acc <> " " <> n) rest
      | "," `T.isPrefixOf` n = go True (acc <> n) rest
      | otherwise = go joined (acc <> "\n" <> n) rest
    tight t = foldr (\(from, to) -> T.replace from to) t ([(o <> " ", o) | o <- ["(", "[", "{"]] ++ [(" " <> c, c) | c <- [")", "]", "}"]])

-- | Whether a body's LINES are only a stub in LANG: each non-blank line,
-- stripped, matches one of its `stub_lines`, is a comment, or holds a hole
-- (@olifant:@).
isStubIn :: Language -> [Text] -> Bool
isStubIn lang = all stubLine . filter (not . T.null) . map T.strip
  where
    stubs = [rx | re <- langStubLines lang, Just rx <- [makeRegexM (T.unpack re) :: Maybe Regex]]
    stubLine l = any (`matchTest` T.unpack l) stubs || "olifant:" `T.isInfixOf` l || langComment lang `T.isPrefixOf` l
