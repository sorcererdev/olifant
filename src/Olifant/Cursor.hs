-- | Snippets at the cursor (spec §5.3): where a cursor edit's text goes and
-- how it is indented. `gen --at FILE:LINE:COL` turns a generator's cursor
-- edit into a replacement of that line ('atCursor'), so placement, drift
-- checks and holes are the ones every replacement has.
module Olifant.Cursor
  ( cursorIndent
  , indentTemplate
  , atCursor
  , isSnippet
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Generator.Definition (EditWith (..), Generator, GeneratorWith (..))
import Olifant.Template.Syntax (Segment (..), Template (..))

-- | The indentation for a snippet on 1-based line @n@ of @ls@, given one
-- level (@unit@): a line with anything on it keeps its own leading
-- whitespace; an empty one (Esc strips autoindent) takes the previous
-- non-blank line's, one level deeper when that line opens a block.
cursorIndent :: Text -> [Text] -> Int -> Text
cursorIndent unit ls n = case drop (n - 1) ls of
  l : _ | not (T.null l) -> T.takeWhile isBlank l
  _ -> case filter (not . T.null . T.strip) (reverse (take (n - 1) ls)) of
    [] -> ""
    prev : _ -> T.takeWhile isBlank prev <> (if opens (T.stripEnd prev) then unit else "")
  where
    isBlank c = c == ' ' || c == '\t'
    opens l = any (`T.isSuffixOf` l) ["{", "(", "[", "="] || any (\w -> l == w || (" " <> w) `T.isSuffixOf` l) ["where", "do"]

-- | Indent every non-empty line the template renders: literal text gets the
-- indent after each newline (unless another newline follows), and so does
-- the start.
indentTemplate :: Text -> Template -> Template
indentTemplate ind (Template src segs) = Template src (Lit ind : go segs)
  where
    go = map $ \case
      Lit t -> Lit (T.intercalate "\n" (indentRest (T.splitOn "\n" t)))
      other -> other
    -- the first piece continues the current line; later ones start lines
    indentRest = \case
      [] -> []
      first : rest -> first : [if T.null l then l else ind <> l | l <- rest]

-- | The generator with its cursor edit placed: a replacement of line @n@
-- of @path@ (whose current lines are @ls@), indented by 'cursorIndent'.
atCursor :: Text -> FilePath -> Int -> [Text] -> Generator -> Generator
atCursor unit path n ls g = g {genEdits = map place (genEdits g)}
  where
    old = case drop (n - 1) ls of
      l : _ -> l
      [] -> ""
    place = \case
      Cursor src -> Replace (Template (T.pack path) [Lit (T.pack path)]) (n, n) old (indentTemplate (cursorIndent unit ls n) src)
      other -> other

-- | A snippet: a generator whose edit goes at the cursor.
isSnippet :: GeneratorWith s -> Bool
isSnippet g = not (null [() | Cursor {} <- genEdits g])
