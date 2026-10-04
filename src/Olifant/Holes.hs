-- | Holes in a plan (spec §6.4). Rendering turns `{= ? name: kind | … =}`
-- into a one-line marker; the plan keeps markers in its working text, shows
-- checks the kind's placeholder instead, and at the end names every hole
-- (a repeated name gets "[file:line]") and writes its placeholder.
module Olifant.Holes
  ( Marker (..)
  , HoleSite (..)
  , splitMarkers
  , hasMarkers
  , placeholderize
  , placeHoles
  , matchesWithHoles
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language.Adapter (HoleKind (..), Language (..), languageFor)
import Olifant.Template (Value (..), render)

data Marker = Marker {markerName :: Text, markerKind :: Text, markerInstruction :: Text}
  deriving stock (Eq, Show)

-- | A hole in a planned file: its (unique) name, kind, instruction, file,
-- 1-based line, and the placeholder written for it.
data HoleSite = HoleSite
  { holeName :: Text
  , holeKind :: Text
  , holeInstruction :: Text
  , holeFile :: FilePath
  , holeLine :: Int
  , holePlaceholder :: Text
  }
  deriving stock (Eq, Show)

open, close :: Text
open = "\10214olifant-hole "
close = "\10215"

-- | Text and markers, in order.
splitMarkers :: Text -> [Either Marker Text]
splitMarkers t = case T.breakOn open t of
  (before, "") -> [Right before | not (T.null before)]
  (before, rest) ->
    let (inner, after) = T.breakOn close (T.drop (T.length open) rest)
        (name, r1) = T.breakOn "|" inner
        (kind, r2) = T.breakOn "|" (T.drop 1 r1)
     in [Right before | not (T.null before)] ++ [Left (Marker name kind (T.drop 1 r2))] ++ splitMarkers (T.drop (T.length close) after)

hasMarkers :: Text -> Bool
hasMarkers = T.isInfixOf open

-- | The kind's placeholder for a hole called @name@.
placeholderFor :: Language -> Marker -> Text -> Either Text Text
placeholderFor lang m name = case lookup kind (langHoles lang) of
  Nothing -> Left ("`" <> kind <> "` is not a hole kind of the " <> langName lang <> " adapter (kinds: " <> kinds <> ")")
  -- a tab stop (spec §4.9) shows its name, or its derived value
  Just _ | source == "yours" -> Right name
  Just _ | source == "derived" -> Right (markerInstruction m)
  Just k -> either (const (Left ("the " <> langName lang <> " adapter's [holes." <> markerKind m <> "] placeholder failed to render"))) Right (render (M.fromList [("hole", VText name), ("instruction", VText (markerInstruction m))]) (kindPlaceholder k))
  where
    (kind, source) = case T.breakOn "@" (markerKind m) of
      (k, "") -> (k, "model")
      (k, rest) -> (k, T.drop 1 rest)
    kinds = if null (langHoles lang) then "none" else T.intercalate ", " (map fst (langHoles lang))

-- | What a check sees: every marker as its placeholder (by its own name).
-- Markers in a file with no adapter, or of an unknown kind, stay (the final
-- 'placeHoles' reports them).
placeholderize :: [Language] -> Map FilePath Text -> Map FilePath Text
placeholderize langs = M.mapWithKey $ \p t -> case languageFor langs p of
  Just lang | hasMarkers t -> T.concat [either (\m -> either (const (marker m)) id (placeholderFor lang m (markerName m))) id piece | piece <- splitMarkers t]
  _ -> t
  where
    marker m = open <> markerName m <> "|" <> markerKind m <> "|" <> markerInstruction m <> close

-- | Name every hole in @files@ (a name used more than once gets
-- "[file:line]") and write its placeholder. Problems: a file with markers
-- but no language adapter, or a kind the adapter does not declare.
placeHoles :: [Language] -> Map FilePath Text -> Either [Text] (Map FilePath Text, [HoleSite])
placeHoles langs files =
  let found = [(p, line, m) | (p, t) <- M.toAscList files, hasMarkers t, (line, l) <- zip [1 :: Int ..] (T.splitOn "\n" t), Left m <- splitMarkers l]
      counts = M.fromListWith (+) [(markerName m, 1 :: Int) | (_, _, m) <- found]
      unique (p, line, m) = if M.findWithDefault 0 (markerName m) counts > 1 then markerName m <> "[" <> T.pack p <> ":" <> T.pack (show line) <> "]" else markerName m
      sites = [(p, line, m, unique x) | x@(p, line, m) <- found]
      placed = [(p, line, m, n, langOf p >>= \lang -> placeholderFor lang m n) | (p, line, m, n) <- sites]
      problems = [T.pack p <> ": " <> why | (p, _, _, _, Left why) <- placed]
      langOf p = maybe (Left ("holes need a language adapter for " <> T.pack p)) Right (languageFor langs p)
   in if not (null problems)
        then Left problems
        else
          let holes = [HoleSite n (markerKind m) (markerInstruction m) p line ph | (p, line, m, n, Right ph) <- placed]
              rewrite p t =
                let mine = sortOn holeLine [h | h <- holes, holeFile h == p]
                    ls = T.splitOn "\n" t
                    fill (i, l) = case [h | h <- mine, holeLine h == i] of
                      [] -> l
                      hs -> T.concat (go hs (splitMarkers l))
                    go hs = \case
                      [] -> []
                      Right x : rest -> x : go hs rest
                      Left _ : rest -> case hs of
                        (h : hs') -> holePlaceholder h : go hs' rest
                        [] -> go hs rest
                 in T.intercalate "\n" (map fill (zip [1 ..] ls))
           in Right (M.mapWithKey (\p t -> if hasMarkers t then rewrite p t else t) files, holes)

-- | Does @actual@ match @planned@ where every hole may be anything (its
-- placeholder, or whatever filled it)? Used where a plan compares its text
-- with a file's (a re-run is a no-op, spec §5.4). Anchored and linear: the
-- text before the first hole must start the file, the text after the last
-- must end it, and the pieces between appear in order.
matchesWithHoles :: Text -> Text -> Bool
matchesWithHoles planned actual
  | not (hasMarkers planned) = planned == actual
  | otherwise = go (merge (splitMarkers planned)) actual
  where
    -- adjacent holes act as one
    merge = \case
      Left m : Left _ : rest -> merge (Left m : rest)
      x : rest -> x : merge rest
      [] -> []
    go pieces rest = case pieces of
      [] -> T.null rest
      [Right lit] -> lit == rest
      Right lit : more -> maybe False (go more) (T.stripPrefix lit rest)
      [Left _] -> True
      Left _ : [Right lit] -> lit `T.isSuffixOf` rest
      Left _ : Right lit : more -> case T.breakOn lit rest of
        (_, "") -> False
        (_, found) -> go more (T.drop (T.length lit) found)
      Left _ : more -> go more rest
