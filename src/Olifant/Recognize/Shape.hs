-- | Internal to Olifant.Recognize: code as token shapes (spec: recognition
-- §2). Keywords and punctuation stay as they are; a name becomes its case
-- shape and word count, so `CloseTable` and `SeatParty` read the same;
-- literals become @str@ / @num@. Names, style and string contents do not
-- matter, only how the code is built.
module Olifant.Recognize.Shape
  ( Keywords (..)
  , Shingle
  , keywordsFrom
  , shapes
  , shingles
  , nameShingle
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Template.Syntax (Template (..))
import Olifant.Template.Syntax (Segment (..))

-- | The words kept as they are, and the language's line comment.
data Keywords = Keywords {kwWords :: Set Text, kwComment :: Text}
  deriving stock (Eq, Show)

type Shingle = (Text, Text, Text)

-- | A language's keywords: the words in its templates' literal text (not
-- their variables), plus the adapter's own list.
keywordsFrom :: Text -> [Template] -> [Text] -> Keywords
keywordsFrom comment templates own =
  Keywords (S.fromList (own <> concatMap (concatMap literalWords . templateSegments) templates)) comment
  where
    literalWords = \case
      Lit t -> names (scan comment t)
      Out _ -> []
      If branches els -> concatMap (concatMap literalWords . snd) branches <> concatMap literalWords els
      For _ _ body -> concatMap literalWords body
      Hole _ _ body -> concatMap literalWords body
    names toks = [w | Name w <- toks]

-- | The code's tokens as shapes; line breaks are @NL@ (never two in a row,
-- never first).
shapes :: Keywords -> Text -> [Text]
shapes kw = collapse . map shapeOf . scan (kwComment kw)
  where
    shapeOf = \case
      Name w | w `S.member` kwWords kw -> w
      Name w -> nameShape w
      Str -> "str"
      Num -> "num"
      Punct p -> p
      Break -> "NL"
    collapse = go True
      where
        go _ [] = []
        go first ("NL" : rest)
          | first = go True rest
          | otherwise = "NL" : go True (dropWhile (== "NL") rest)
        go _ (t : rest) = t : go False rest

-- | Overlapping windows of three tokens.
shingles :: [Text] -> Set Shingle
shingles ts = S.fromList (zip3 ts (drop 1 ts) (drop 2 ts))

-- | Does a window carry a name (an identifier's shape)?
nameShingle :: Shingle -> Bool
nameShingle (a, b, c) = any isNameShape [a, b, c]

isNameShape :: Text -> Bool
isNameShape t = any (`T.isPrefixOf` t) ["Pascal", "snake", "lower", "UPPER"] && T.all isDigit (T.dropWhile isAlpha t) && not (T.null (T.dropWhile isAlpha t))

-- | A name's case and word count (at most 3): @Pascal2@, @snake2@, @lower1@, @UPPER1@.
nameShape :: Text -> Text
nameShape w =
  let core = T.dropWhile (== '_') w
      ws = splitWords core
      n = T.pack (show (max 1 (min 3 (length ws))))
      kind
        | T.length (T.filter isAlpha core) > 1 && T.all (\c -> not (isLower c)) core = "UPPER"
        | maybe False (isUpper . fst) (T.uncons core) = "Pascal"
        | T.any (== '_') core = "snake"
        | otherwise = "lower"
   in kind <> n

splitWords :: Text -> [Text]
splitWords = concatMap camel . filter (not . T.null) . T.splitOn "_"
  where
    camel t = filter (not . T.null) (go (T.unpack t) "" [])
      where
        go [] cur acc = reverse (T.pack (reverse cur) : acc)
        go (c : cs) cur acc
          | isUpper c, not (null cur), maybe False isLower (safeHead cur) = go cs [c] (T.pack (reverse cur) : acc)
          | otherwise = go cs (c : cur) acc
        safeHead = \case
          (x : _) -> Just x
          [] -> Nothing

data Token = Name Text | Str | Num | Punct Text | Break
  deriving stock (Eq, Show)

-- | One pass over the text: strings (single, double, triple-quoted), the
-- language's line comments (dropped), names, numbers, punctuation.
scan :: Text -> Text -> [Token]
scan comment = go
  where
    go t = case T.uncons t of
      Nothing -> []
      Just (c, rest)
        | c == '\n' -> Break : go rest
        | isSpace c -> go rest
        | not (T.null comment), comment `T.isPrefixOf` t -> go (T.dropWhile (/= '\n') t)
        | Just body <- T.stripPrefix "\"\"\"" t -> Str : go (after "\"\"\"" body)
        | Just body <- T.stripPrefix "'''" t -> Str : go (after "'''" body)
        | c == '"' -> Str : go (closing '"' rest)
        | c == '\'', Just r <- charLiteral rest -> Str : go r
        | isAlpha c || c == '_' -> let (w, r) = T.span (\x -> isAlphaNum x || x == '_') t in Name w : go r
        | isDigit c -> let (_, r) = T.span (\x -> isAlphaNum x || x == '_' || x == '.') t in Num : go r
        | Just (p, r) <- twoChar t -> Punct p : go r
        | otherwise -> Punct (T.singleton c) : go rest
    after delim body = case T.breakOn delim body of
      (_, r) | T.null r -> ""
      (_, r) -> T.drop (T.length delim) r
    -- to the closing quote on the line (escapes skipped); an unclosed one ends the line
    closing q = loop
      where
        loop s = case T.uncons s of
          Nothing -> ""
          Just ('\\', r) -> loop (T.drop 1 r)
          Just ('\n', _) -> s
          Just (x, r) | x == q -> r
          Just (_, r) -> loop r
    -- a 'quoted' string or char; a lone ' (a Rust lifetime) is punctuation
    charLiteral s = case T.breakOn "'" s of
      (inside, r) | not (T.null r), not (T.any (== '\n') inside) -> Just (T.drop 1 r)
      _ -> Nothing
    twoChar t = case T.take 2 t of
      p | p `elem` ["->", "=>", "::", "==", "!=", "<=", ">=", "**"] -> Just (p, T.drop 2 t)
      _ -> Nothing
