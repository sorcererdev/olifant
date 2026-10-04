-- | Snippets at the cursor (spec §5.3): where a cursor edit's text goes and
-- how it is indented. `gen --at FILE:LINE:COL` turns a generator's cursor
-- edit into a replacement of that line ('atCursor'), so placement, drift
-- checks and holes are the ones every replacement has.
module Olifant.Cursor
  ( cursorIndent
  , indentTemplate
  , atCursor
  , atCursorIndented
  , afterIndent
  , afterRule
  , isSnippet
  ) where

import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Aeson (Value)
import Olifant.Language (Language, Match (..), Rule, Syntax, querySyntax, renderRule)
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
    go = \case
      [] -> []
      Lit t : rest -> Lit (T.intercalate "\n" (indentRest (not (null rest)) (T.splitOn "\n" t))) : go rest
      other : rest -> other : go rest
    -- The first piece continues the current line; later ones start lines.
    -- An empty piece is an empty line, unless it is the last one and more
    -- of the template follows on its line (a hole, a value).
    indentRest more = \case
      [] -> []
      first : rest ->
        let n = length rest
         in first : [if T.null l && not (more && i == n) then l else ind <> l | (i, l) <- zip [1 ..] rest]

-- | The generator with its cursor edit placed: a replacement of line @n@
-- of @path@ (whose current lines are @ls@), indented by 'cursorIndent'.
atCursor :: Text -> FilePath -> Int -> [Text] -> Generator -> Generator
atCursor unit path n ls = atCursorIndented (cursorIndent unit ls n) path n ls

-- | 'atCursor' with a given indentation.
atCursorIndented :: Text -> FilePath -> Int -> [Text] -> Generator -> Generator
atCursorIndented ind path n ls g = g {genEdits = map place (genEdits g)}
  where
    old = case drop (n - 1) ls of
      l : _ -> l
      [] -> ""
    place = \case
      Cursor src -> Replace (Template (T.pack path) [Lit (T.pack path)]) (n, n) old (indentTemplate ind src)
      other -> other

-- | For a snippet anticipated `after` a node: that node's indentation (the
-- line above may be deeper — a Python class's last field), when line @n@
-- (1-based) follows one with only blank lines between.
afterIndent :: Monad m => Syntax m -> Language -> Value -> [Text] -> Int -> m (Maybe Text)
afterIndent syntax lang rule ls n =
  querySyntax syntax lang rule (T.intercalate "\n" ls) <&> \case
    Right ms -> case sortOn (Down . fst . matchEnd) [m | m <- ms, follows m] of
      m : _ -> Just (T.takeWhile (\c -> c == ' ' || c == '\t') (fromMaybe "" (listToMaybe (drop (fst (matchStart m)) ls))))
      [] -> Nothing
    Left _ -> Nothing
  where
    follows m =
      let end = fst (matchEnd m)
       in end < n - 1 && all (T.null . T.strip) (take (n - 2 - end) (drop (end + 1) ls))

-- | An `after` rule as ast-grep takes it (a snippet's rule uses no inputs).
afterRule :: Rule -> Maybe Value
afterRule = either (const Nothing) Just . renderRule mempty

-- | A snippet: a generator whose edit goes at the cursor.
isSnippet :: GeneratorWith s -> Bool
isSnippet g = not (null [() | Cursor {} <- genEdits g])
