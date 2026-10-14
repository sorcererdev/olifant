-- | A draft is a hole made on demand: the pure builders of the templates
-- the loop plans with. A code draft replaces a span of the file with one
-- block hole worded from the failing test; a test draft is a stmt hole
-- inside a test that a kill stub began. No IO here.
module Olifant.Block.Draft
  ( draftTemplate,
    testDraftTemplate,
    killStub,
    stubName,
    stubNameFor,
    uniqueTestName,
    testStub,
    instructionFor,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, toLower)
import Data.List (find, nub)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language (Failure (..), HoleKind (..), Language (..), TestId)
import Olifant.Mutate (Mutant (..))
import Olifant.Template (Template, Value (..), describeTemplateError, parseTemplate, render)

-- | The file with the lines of the span (1-based, inclusive) replaced by one
-- block hole at the span's indentation, and the hole's instruction. The
-- test's file text says what its assertion is; the failure says why it is red.
draftTemplate :: Language -> Text -> (Int, Int) -> TestId -> Failure -> Text -> Either Text (Template, Text)
draftTemplate _lang file (l1, l2) tid failure testText
  | l1 < 1 || l2 < l1 || l2 > length ls = Left ("lines " <> T.pack (show l1) <> "-" <> T.pack (show l2) <> " are not in the file")
  | otherwise = do
      let instruction = instructionFor tid testText failure
          indent = T.takeWhile (`elem` [' ', '\t']) (ls !! (l1 - 1))
          marker = indent <> "{= ? body: block | " <> instruction <> " =}"
      template <- either (Left . describeTemplateError) Right (parseTemplate (T.intercalate "\n" (take (l1 - 1) ls ++ [marker] ++ drop l2 ls)))
      Right (template, instruction)
  where
    ls = T.splitOn "\n" file

-- | A test file with the line holding a kill stub's placeholder (1-based)
-- made a stmt hole called @arrange@, at the line's indentation.
testDraftTemplate :: Language -> Text -> Int -> Either Text Template
testDraftTemplate lang testText line
  | line < 1 || line > length ls = Left ("no line " <> T.pack (show line))
  | placeholder lang `T.isInfixOf` T.strip current = parseWith
  | otherwise = Left ("line " <> T.pack (show line) <> " does not hold the stub's placeholder")
  where
    ls = T.splitOn "\n" testText
    current = ls !! (line - 1)
    indent = T.takeWhile (`elem` [' ', '\t']) current
    parseWith =
      either (Left . describeTemplateError) Right . parseTemplate $
        T.intercalate "\n" (take (line - 1) ls ++ [indent <> "{= ? arrange: stmt | " <> arrangeInstruction <> " =}"] ++ drop line ls)

-- | A kill's stub: 'testStub', for a survivor to be killed by, its test
-- named from the mutant ('stubNameFor'): where it goes, the stub, the name.
killStub :: Language -> Text -> Mutant -> (Int, Text, Text)
killStub lang testText m = (at, stub, name)
  where
    name = stubNameFor m testText (langKeywords lang)
    (at, stub) = testStub lang testText (Just name)

-- | The name a begin's stub gives its test until the person types one
-- (its tab stop, `test_${1:name}`, left as it was).
stubName :: Text
stubName = "test_name"

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

-- | Where a new test goes (the line after the file's last non-blank line)
-- and the stub: two blank lines, then a test whose name and assertion are
-- tab stops and whose arrangement is a stmt hole. Its name's tab stop
-- holds NAME, or with none 'stubName' (`test_${1:name}`).
testStub :: Language -> Text -> Maybe Text -> (Int, Text)
testStub lang testText name = (lastNonBlank + 1, stub)
  where
    lastNonBlank = length (dropWhileEnd' (T.null . T.strip) (T.splitOn "\n" testText))
    ind = langIndent lang
    stop = maybe ("test_${1:" <> T.drop (T.length "test_") stubName <> "}") (\n -> "${1:" <> n <> "}") name
    stub = "\n\ndef " <> stop <> "() -> None:\n" <> ind <> placeholder lang <> "\n" <> ind <> "assert ${2:...}\n"

-- | What the model is asked: the test in words, its assertion, why it fails
-- now, and the stance. Never holds a marker's end or a newline.
instructionFor :: TestId -> Text -> Failure -> Text
instructionFor tid testText failure =
  oneLine . T.intercalate ". " $
    [ "make the test \"" <> words' <> "\" pass",
      "its assertion: " <> T.intercalate "; " asserts,
      "it fails now with: " <> fMessage failure,
      "write the least code that makes it pass"
    ]
  where
    name = snd (T.breakOnEnd "::" tid)
    words' = T.replace "_" " " (fromMaybe name (T.stripPrefix "test_" name))
    asserts = assertionLines name testText

-- | The @assert@ lines of the named test (its def to the next line at or
-- below the def's own indentation).
assertionLines :: Text -> Text -> [Text]
assertionLines name testText =
  [ T.strip l
  | l <- body,
    "assert" `T.isPrefixOf` T.strip l
  ]
  where
    indentOf = T.length . T.takeWhile (== ' ')
    afterDef = dropWhile (not . isDef) (T.lines testText)
    isDef l = ("def " <> name <> "(") `T.isPrefixOf` T.strip l
    body = case afterDef of
      [] -> []
      (d : rest) -> takeWhile (\l -> T.null (T.strip l) || indentOf l > indentOf d) rest

-- | The stmt placeholder of the language, rendered for the arrangement hole.
placeholder :: Language -> Text
placeholder lang = case lookup "stmt" (langHoles lang) of
  Just k -> either (const arrangeInstruction) id (render env (kindPlaceholder k))
  Nothing -> arrangeInstruction
  where
    env = M.fromList [("hole", VText "arrange"), ("instruction", VText arrangeInstruction)]

arrangeInstruction :: Text
arrangeInstruction = "set up what the assertion below needs"

oneLine :: Text -> Text
oneLine = T.replace "=}" "= }" . T.intercalate "; " . filter (not . T.null) . map T.strip . T.lines

dropWhileEnd' :: (a -> Bool) -> [a] -> [a]
dropWhileEnd' p = reverse . dropWhile p . reverse
