-- | A draft is a hole made on demand: the pure builders of the templates
-- the loop plans with. A code draft replaces a span of the file with one
-- block hole worded from the failing test; a test draft is a stmt hole
-- inside a test that a kill stub began. No IO here.
module Olifant.Block.Draft
  ( draftTemplate,
    testDraftTemplate,
    killStub,
    instructionFor,
  )
where

import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language (Failure (..), HoleKind (..), Language (..), TestId)
import Olifant.Mutate (Mutant)
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

-- | Where a new test goes (the line after the file's last non-blank line)
-- and the stub: a test whose name and assertion are tab stops and whose
-- arrangement is a stmt hole, for a survivor to be killed by.
killStub :: Language -> Text -> Mutant -> (Int, Text)
killStub lang testText _mutant = (lastNonBlank + 1, stub)
  where
    lastNonBlank = length (dropWhileEnd' (T.null . T.strip) (T.splitOn "\n" testText))
    ind = langIndent lang
    stub = "\n\ndef test_${1:name}() -> None:\n" <> ind <> placeholder lang <> "\n" <> ind <> "assert ${2:...}\n"

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
