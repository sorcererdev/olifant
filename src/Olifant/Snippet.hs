-- | Snippets (spec §4.9): typed holes with a source, and what an editor
-- needs to expand a snippet — ghost lines (holes as ⟨name⟩) and an LSP
-- snippet body whose holes are tab stops.
--
-- A hole's source travels in its marker's kind ("text@yours"), so placing,
-- filling and the plan see it without another channel: 'withHoleSources'
-- writes it at load; 'holeSource' reads it back.
module Olifant.Snippet
  ( Snippet (..)
  , withHoleSources
  , holeSource
  , snippetOf
  , lspEscape
  ) where

import Data.List (mapAccumL)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Generator.Definition (GeneratorWith (..), HoleSource (..))
import Olifant.Holes (Marker (..), splitMarkers)
import Olifant.Template.Syntax (Segment (..), Template (..))

-- | Where the snippet goes (file, 1-based line it replaces), its ghost
-- lines, and its LSP body.
data Snippet = Snippet {snFile :: FilePath, snLine :: Int, snGhost :: [Text], snBody :: Text}
  deriving stock (Eq, Show)

-- | Mark each `[holes.NAME]` hole in the generator's templates with its
-- source; a derived hole's instruction becomes its value. A name no
-- template uses is an error.
withHoleSources :: GeneratorWith Template -> Either Text (GeneratorWith Template)
withHoleSources g = case [n | (n, _) <- genHoles g, n `notElem` names] of
  n : _ -> Left ("[holes." <> n <> "]: no hole called " <> n <> " in this generator's templates")
  [] -> Right (fmap retemplate g)
  where
    names = concatMap (\(Template _ segs) -> holeNames segs) (foldr (:) [] g)
    holeNames = concatMap $ \case
      Hole n _ _ -> [n]
      If branches els -> concatMap (holeNames . snd) branches ++ holeNames els
      For _ _ body -> holeNames body
      _ -> []
    retemplate (Template src segs) = Template src (map mark segs)
    mark = \case
      Hole n kind instr -> case lookup n (genHoles g) of
        Just Yours -> Hole n (kind <> "@yours") instr
        Just (Derived (Template _ value)) -> Hole n (kind <> "@derived") value
        _ -> Hole n kind instr
      If branches els -> If [(c, map mark b) | (c, b) <- branches] (map mark els)
      For v e body -> For v e (map mark body)
      other -> other

-- | A marker kind's (kind, source): "text@yours" → ("text", "yours").
holeSource :: Text -> (Text, Text)
holeSource k = case T.breakOn "@" k of
  (kind, "") -> (kind, "model")
  (kind, rest) -> (kind, T.drop 1 rest)

-- | The snippet for lines of marked text (markers still in place) placed at
-- @line@ of @file@; @fillOf@ gives a model hole's fill. Holes become tab
-- stops numbered in reading order, `$0` at the end.
snippetOf :: FilePath -> Int -> Text -> (Text -> Maybe Text) -> Snippet
snippetOf file line marked fillOf =
  let pieces = splitMarkers marked
      (_, rendered) = mapAccumL piece (1 :: Int) pieces
      piece n = \case
        Right lit -> (n, (lit, lspEscape lit))
        Left m ->
          let (_, source) = holeSource (markerKind m)
              name = markerName m
              (ghost, value) = case source of
                "yours" -> ("⟨" <> name <> "⟩", name)
                "derived" -> (markerInstruction m, markerInstruction m)
                _ -> maybe ("⟨" <> name <> "⟩", name) (\f -> (f, f)) (fillOf name)
           in (n + 1, (ghost, "${" <> T.pack (show n) <> ":" <> lspEscape value <> "}"))
   in Snippet file line (T.splitOn "\n" (T.concat (map fst rendered))) (T.concat (map snd rendered) <> "$0")

-- | Text as an LSP snippet literal: `\`, `$` and `}` escaped.
lspEscape :: Text -> Text
lspEscape = T.concatMap $ \case
  c | c `elem` ("\\$}" :: String) -> T.pack ['\\', c]
  c -> T.singleton c
