-- | The name a survivor's new test gets, from its mutant.
module Olifant.TestName
  ( stubNameFor,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, toLower)
import Data.List (find, nub)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Mutate (Mutant (..))

-- | The name of a survivor's test, from its mutant: a literal's first four
-- words (`string`), else the operator and the names on the mutated line
-- (or, with none there, in what it changed from), four at most, leaving
-- out KEYWORDS, `self` and the heads of attributes (`seats` of
-- `table.seats`); with no name at all, the line. AFFIXES: the text the
-- language's `[test] name` puts before and after the words (pytest's
-- @test_@). COMMENT: the language's comment marker (no name after it). A
-- name in DEFINED (the test file's tests) gets `_2`, `_3`, ….
stubNameFor :: Mutant -> (Text, Text) -> Text -> [Text] -> [Text] -> Text
stubNameFor m (before, after) comment defined keywords = unique (capped (before <> fromMaybe (operator <> "_line" <> T.pack (show (muLine m))) described) <> after)
  where
    operator = snakeOf (muOperator m)
    unique base = fromMaybe base (find (`notElem` defined) (base : [base <> "_" <> T.pack (show i) | i <- [2 :: Int ..]]))
    -- 60 characters at most, cut between words
    capped t = case T.splitOn "_" t of
      first : rest -> T.intercalate "_" (first : takeFitting first rest)
      [] -> t
    takeFitting acc (w : ws) | T.length acc + 1 + T.length w <= 60 = w : takeFitting (acc <> "_" <> w) ws
    takeFitting _ _ = []
    described
      | muOperator m == "string" = nonEmpty (snakeOf (T.unwords (take 4 (T.words (literalBody (muFrom m))))))
      | otherwise = ((operator <> "_") <>) <$> nonEmpty (T.intercalate "_" (take 4 (nub (namesOf mutatedLine <> namesOf (muFrom m)))))
    mutatedLine = fromMaybe "" (listToMaybe (drop (muLine m - 1) (T.splitOn "\n" (muText m))))
    namesOf = filter (`notElem` ("self" : keywords)) . identifiers comment
    nonEmpty t = if T.null t then Nothing else Just t

-- | A literal's text without its prefix (`f`, `rb`, …) and quotes.
literalBody :: Text -> Text
literalBody lit = T.dropAround (`elem` ['"', '\'']) (if T.any isQuote (T.take 1 rest) then rest else lit)
  where
    rest = T.dropWhile (`elem` ("rRbBfFuU" :: String)) lit
    isQuote c = c == '"' || c == '\''

-- | Lowercase ASCII letters and digits, every run of
-- other characters one `_`, none at the ends.
snakeOf :: Text -> Text
snakeOf = T.intercalate "_" . filter (not . T.null) . T.split (== '_') . T.map (\c -> if isAsciiLower c || isAsciiUpper c || isDigit c then toLower c else '_')

-- | The names on a line of code, in order: none in a string or a comment,
-- an attribute's head left out (`seats` of `table.seats`), a string's
-- prefix (`f"…"`) no name.
identifiers :: Text -> Text -> [Text]
identifiers comment t = case T.uncons t of
  Nothing -> []
  Just (c, rest)
    | not (T.null comment), comment `T.isPrefixOf` t -> []
    | c == '"' || c == '\'' -> identifiers comment (afterString c rest)
    | isIdentStart c ->
        let (name, after) = T.span isIdent t
            next = T.take 1 after
         in if next == "." || next == "\"" || next == "'" then identifiers comment after else name : identifiers comment after
    | isDigit c -> identifiers comment (T.dropWhile isIdent t)
    | otherwise -> identifiers comment rest
  where
    afterString q s = case T.uncons s of
      Nothing -> ""
      Just ('\\', s') -> afterString q (T.drop 1 s')
      Just (x, s') | x == q -> s'
      Just (_, s') -> afterString q s'

isIdentStart, isIdent :: Char -> Bool
isIdentStart c = isAsciiLower c || isAsciiUpper c || c == '_'
isIdent c = isIdentStart c || isDigit c
