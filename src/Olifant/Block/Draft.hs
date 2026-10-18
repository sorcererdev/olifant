-- | A draft is a hole made on demand: the pure builders of the templates
-- the loop plans with. A code draft replaces a span of the file with one
-- block hole worded from the failing test; a test draft is a stmt hole
-- inside a test that a kill stub began. No IO here.
module Olifant.Block.Draft
  ( draftTemplate,
    testDraftTemplate,
    existingTestTemplate,
    existingAssertionInstruction,
    killStub,
    stubName,
    stubNameFor,
    uniqueTestName,
    testStub,
    instructionFor,
    stubAssertion,
    assertionInstruction,
    helperSentence,
    helpedIn,
    snippetText,
  )
where

import Data.Char (isDigit)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language (Failure (..), HoleKind (..), Language (..), TestId)
import Olifant.Mutate (Mutant (..))
import Olifant.Template (Template, Value (..), describeTemplateError, parseTemplate, render)
import Olifant.TestName (helpedIn, helperSentenceFor, stubNameFor, uniqueTestName)

-- | The file with the lines of the span (1-based, inclusive) replaced by one
-- block hole at the span's indentation, and the hole's instruction. The
-- test's file text says what its assertion is; the failure says why it is
-- red (at KEPT, a test the block keeps, when its own test passes).
draftTemplate :: Language -> Text -> (Int, Int) -> TestId -> Failure -> Maybe TestId -> Text -> Either Text (Template, Text)
draftTemplate _lang file (l1, l2) tid failure kept testText
  | l1 < 1 || l2 < l1 || l2 > length ls = Left ("lines " <> T.pack (show l1) <> "-" <> T.pack (show l2) <> " are not in the file")
  | otherwise = do
      let instruction = instructionFor tid testText failure kept
          indent = T.takeWhile (`elem` [' ', '\t']) (ls !! (l1 - 1))
          marker = indent <> "{= ? body: block | " <> instruction <> " =}"
      template <- either (Left . describeTemplateError) Right (parseTemplate (T.intercalate "\n" (take (l1 - 1) ls ++ [marker] ++ drop l2 ls)))
      Right (template, instruction)
  where
    ls = T.splitOn "\n" file

-- | A test file with the line holding a stub's placeholder (1-based)
-- made a stmt hole called @arrange@, at the line's indentation; with
-- ASSERTION (its line, still the stub's 'stubAssertion', and the
-- instruction), that line made a stmt hole called @assertion@ too.
testDraftTemplate :: Language -> Text -> Int -> Maybe (Int, Text) -> Either Text Template
testDraftTemplate lang testText line assertion
  | line < 1 || line > length ls = Left ("no line " <> T.pack (show line))
  | not (placeholder lang `T.isInfixOf` T.strip current) = Left ("line " <> T.pack (show line) <> " does not hold the stub's placeholder")
  | Just (a, _) <- assertion, a < 1 || a > length ls || T.strip (ls !! (a - 1)) /= stubAssertion = Left ("line " <> T.pack (show a) <> " is not the stub's assertion")
  | otherwise =
      either (Left . describeTemplateError) Right . parseTemplate $
        T.intercalate "\n" (zipWith marked [1 ..] ls)
  where
    ls = T.splitOn "\n" testText
    current = ls !! (line - 1)
    indentOf = T.takeWhile (`elem` [' ', '\t'])
    arrange = maybe arrangeInstruction (const (arrangeInstruction <> ", ending with `result = ` the call the test is about")) assertion
    helped = helpedIn ls
    -- another stub left in the file holding this hole's placeholder would take
    -- the fill (a fill goes where its placeholder first is): the instruction differs
    arranged
      | any (placeholderWith lang (helped arrange) `T.isInfixOf`) [l | (i, l) <- zip [1 :: Int ..] ls, i /= line] = helped arrange <> "; this stub's, not another one's in the file"
      | otherwise = helped arrange
    marked i l
      | i == line = indentOf l <> "{= ? arrange: stmt | " <> arranged <> " =}"
      | Just (a, instruction) <- assertion, i == a = indentOf l <> "{= ? assertion: stmt | " <> helped (oneLine instruction) <> " =}"
      | otherwise = l

-- | An existing test's draft (the code is right, the test is red at
-- MESSAGE): the test file with the test NAME's arrangement, the lines
-- after line D (its def, or a leading docstring's end) before line A, one
-- stmt hole called @arrange@ (none: a hole on a new line after D); with
-- ASSERTION, its assertion (A, the body's statement holding its first
-- assert, to Z, the test's last line) one stmt hole called @assertion@
-- too. Each at the indentation of line A.
existingTestTemplate :: Text -> Text -> Int -> (Int, Int) -> Text -> Bool -> Either Text Template
existingTestTemplate testText name d (a, z) message assertion
  | d < 1 || a <= d || z < a || z > length ls = Left ("lines " <> T.pack (show d) <> "-" <> T.pack (show z) <> " are not the test " <> name <> "'s")
  | otherwise =
      either (Left . describeTemplateError) Right . parseTemplate . T.intercalate "\n" $
        take d ls
          ++ [indent <> "{= ? arrange: stmt | " <> helped arrange <> " =}"]
          ++ if assertion then (indent <> "{= ? assertion: stmt | " <> helped (existingAssertionInstruction name) <> " =}") : drop z ls else drop (a - 1) ls
  where
    ls = T.splitOn "\n" testText
    indent = T.takeWhile (`elem` [' ', '\t']) (ls !! (a - 1))
    helped = helpedIn ls
    arrange = oneLine ("the code is right and this test is red (" <> message <> "): " <> arrangeInstruction <> " against the code as it is" <> if assertion then "" else "; the assertion below stays as it is")

-- | What the model is asked for an existing test's assertion once the
-- person asked again at a red assertion: the code's behaviour is the spec.
existingAssertionInstruction :: Text -> Text
existingAssertionInstruction name = "the code is the spec now: assert what `" <> T.replace "_" " " (fromMaybe name (T.stripPrefix "test_" name)) <> "` means against what the code returns; keep the test's intent"

-- | What a test draft is told in a file whose tests build their subject
-- with a helper (python-htmx's `def command(…)`, its ports keyword
-- arguments with fakes for defaults): an adapter arranged and then not
-- passed is never used.
helperSentence :: Text
helperSentence = helperSentenceFor "command"

-- | A stub's assertion as the editor expands it (its tab stop's text):
-- left so, the draft writes it.
stubAssertion :: Text
stubAssertion = "assert ..."

-- | What the model is asked for a stub's assertion: what the test's name
-- means, one assert on `result`; a kill's must pass on the code and fail
-- on its mutant.
assertionInstruction :: Text -> Maybe Mutant -> Text
assertionInstruction name killing =
  oneLine $
    "assert what '" <> words' <> "' means; one assert, the subject is `result`"
      <> maybe "" (\m -> "; it passes on the code as it is and fails when line " <> T.pack (show (muLine m)) <> "'s `" <> muFrom m <> "` becomes `" <> muTo m <> "`") killing
  where
    words' = T.replace "_" " " (fromMaybe name (T.stripPrefix "test_" name))

-- | A snippet's text as the editor leaves it expanded: each tab stop
-- (`${1:text}`) its text.
snippetText :: Text -> Text
snippetText t = case T.breakOn "${" t of
  (before, "") -> before
  (before, rest) ->
    let inner = T.drop 2 rest
        (n, afterN) = T.span isDigit inner
        (text, after) = T.breakOn "}" (T.drop 1 afterN)
     in if not (T.null n) && ":" `T.isPrefixOf` afterN && not (T.null after)
          then before <> text <> snippetText (T.drop 1 after)
          else before <> "${" <> snippetText inner

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

-- | Where a new test goes (the line after the file's last non-blank line)
-- and the stub: two blank lines, then a test whose name and assertion are
-- tab stops and whose arrangement is a stmt hole (an assertion left as
-- its tab stop's `...` is drafted too). Its name's tab stop
-- holds NAME, or with none 'stubName' (`test_${1:name}`).
testStub :: Language -> Text -> Maybe Text -> (Int, Text)
testStub lang testText name = (lastNonBlank + 1, stub)
  where
    lastNonBlank = length (dropWhileEnd' (T.null . T.strip) (T.splitOn "\n" testText))
    ind = langIndent lang
    stop = maybe ("test_${1:" <> T.drop (T.length "test_") stubName <> "}") (\n -> "${1:" <> n <> "}") name
    stub = "\n\ndef " <> stop <> "() -> None:\n" <> ind <> placeholder lang <> "\n" <> ind <> "assert ${2:...}\n"

-- | What the model is asked: the test in words, its assertion, why it fails
-- now, and the stance; red at KEPT (a test that passed when the circle
-- began, its own test passing), that test, its assertion and why it fails
-- instead. Never holds a marker's end or a newline.
instructionFor :: TestId -> Text -> Failure -> Maybe TestId -> Text
instructionFor tid testText failure kept =
  oneLine . T.intercalate ". " $ case kept of
    Nothing ->
      [ "make the test \"" <> words' tid <> "\" pass",
        "its assertion: " <> asserts tid,
        "it fails now with: " <> fMessage failure,
        "write the least code that makes it pass"
      ]
    Just k ->
      [ "make the test \"" <> words' tid <> "\" pass and keep the test \"" <> words' k <> "\" passing",
        "its assertion: " <> asserts tid,
        name k <> " no longer passes; its assertion: " <> asserts k,
        "the runner says: " <> fMessage failure,
        "write the least code that makes both pass"
      ]
  where
    name = snd . T.breakOnEnd "::"
    words' t = T.replace "_" " " (fromMaybe (name t) (T.stripPrefix "test_" (name t)))
    asserts t = T.intercalate "; " (assertionLines (name t) testText)

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
placeholder lang = placeholderWith lang arrangeInstruction

-- | 'placeholder' for an arrangement hole with another instruction.
placeholderWith :: Language -> Text -> Text
placeholderWith lang instruction = case lookup "stmt" (langHoles lang) of
  Just k -> either (const arrangeInstruction) id (render env (kindPlaceholder k))
  Nothing -> arrangeInstruction
  where
    env = M.fromList [("hole", VText "arrange"), ("instruction", VText instruction)]

arrangeInstruction :: Text
arrangeInstruction = "set up what the assertion below needs"

oneLine :: Text -> Text
oneLine = T.replace "=}" "= }" . T.intercalate "; " . filter (not . T.null) . map T.strip . T.lines

dropWhileEnd' :: (a -> Bool) -> [a] -> [a]
dropWhileEnd' p = reverse . dropWhile p . reverse
