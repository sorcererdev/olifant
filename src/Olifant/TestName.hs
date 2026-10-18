-- | The names a new test gets: a survivor's stub named from its mutant,
-- a name its test file does not define yet; and the sentence a test file
-- with the pack's helper adds to an instruction.
module Olifant.TestName
  ( stubNameFor,
    uniqueTestName,
    helpedIn,
    helperSentenceFor,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, toLower)
import Data.List (find, nub)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Mutate (Mutant (..))

-- | An instruction in a test file (its lines) with the pack's helper: the
-- arranged adapters go through it ('helperSentenceFor').
helpedIn :: [Text] -> Text -> Text
helpedIn ls t = maybe t (\h -> t <> "; " <> helperSentenceFor h) (listToMaybe [h | l <- ls, h <- ["command", "query"], ("def " <> h <> "(") `T.isPrefixOf` l])

-- | 'helperSentence' for a helper named for what it builds (`command`, `query`).
helperSentenceFor :: Text -> Text
helperSentenceFor h = "build the " <> h <> " with the helper, passing the adapters you arranged (`" <> h <> "(booking_repository=repository)`); never call the helper bare after arranging a repository"

-- | The name of a kill's test, from its mutant: a literal's first four
-- words (`string`), else the operator and the names on the mutated line
-- (or, with none there, in what it changed from), four at most, leaving
-- out KEYWORDS, `self` and the heads of attributes (`seats` of
-- `table.seats`); with no name at all, the line. A name TESTTEXT defines
-- already gets `_2`, `_3`, … ('uniqueTestName').
stubNameFor :: Mutant -> Text -> [Text] -> Text
stubNameFor m testText keywords = uniqueTestName base testText
  where
    operator = snakeOf (muOperator m)
    base = capped ("test_" <> fromMaybe (operator <> "_line" <> T.pack (show (muLine m))) described)
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
    namesOf = filter (`notElem` ("self" : keywords)) . identifiers
    nonEmpty t = if T.null t then Nothing else Just t

-- | NAME, or, TESTTEXT defining it already, the first of NAME_2, NAME_3, …
-- it does not define.
uniqueTestName :: Text -> Text -> Text
uniqueTestName base testText = fromMaybe base (find (`notElem` defined) (base : [base <> "_" <> T.pack (show i) | i <- [2 :: Int ..]]))
  where
    defined = [n | l <- T.lines testText, let st = T.strip l, Just rest <- [T.stripPrefix "def " (fromMaybe st (T.stripPrefix "async " st))], let n = T.takeWhile isIdent rest, "(" `T.isPrefixOf` T.drop (T.length n) rest]

-- | A literal's text without its prefix (`f`, `rb`, …) and quotes.
literalBody :: Text -> Text
literalBody lit = T.dropAround (`elem` ['"', '\'']) (if T.any isQuote (T.take 1 rest) then rest else lit)
  where
    rest = T.dropWhile (`elem` ("rRbBfFuU" :: String)) lit
    isQuote c = c == '"' || c == '\''

-- | Lowercase ASCII letters and digits (a Python name's), every run of
-- other characters one `_`, none at the ends.
snakeOf :: Text -> Text
snakeOf = T.intercalate "_" . filter (not . T.null) . T.split (== '_') . T.map (\c -> if isAsciiLower c || isAsciiUpper c || isDigit c then toLower c else '_')

-- | The names on a line of code, in order: none in a string or a comment,
-- an attribute's head left out (`seats` of `table.seats`), a string's
-- prefix (`f"…"`) no name.
identifiers :: Text -> [Text]
identifiers t = case T.uncons t of
  Nothing -> []
  Just (c, rest)
    | c == '#' -> []
    | c == '"' || c == '\'' -> identifiers (afterString c rest)
    | isIdentStart c ->
        let (name, after) = T.span isIdent t
            next = T.take 1 after
         in if next == "." || next == "\"" || next == "'" then identifiers after else name : identifiers after
    | isDigit c -> identifiers (T.dropWhile isIdent t)
    | otherwise -> identifiers rest
  where
    afterString q s = case T.uncons s of
      Nothing -> ""
      Just ('\\', s') -> afterString q (T.drop 1 s')
      Just (x, s') | x == q -> s'
      Just (_, s') -> afterString q s'

isIdentStart, isIdent :: Char -> Bool
isIdentStart c = isAsciiLower c || isAsciiUpper c || c == '_'
isIdent c = isIdentStart c || isDigit c
