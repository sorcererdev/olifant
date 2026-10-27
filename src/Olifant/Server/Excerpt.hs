-- | Internal to Olifant.Server: a step's excerpts, fences of its unit's code
-- written under it ("```python @app/a.py:Table"), kept current by the
-- reconcile while the step is open and frozen (" · done") once it is ticked;
-- the clip they show, and the hover over an anchor.
module Olifant.Server.Excerpt
  ( Excerpt (..)
  , excerptsOf
  , renderFence
  , clip
  , excerptFacts
  , unitExcerpt
  , hoverText
  , unitHover
  ) where

import Data.Char (isSpace)
import Data.Functor ((<&>))
import Data.List (nub, sort)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..))
import Olifant.Language.Adapter (Language (..), languageFor)
import Olifant.Plans (Anchor (..), Line (..), anchorText, excerptInfo, excerptSpans, stepDone)
import Olifant.Server.Edits (editScript)
import Olifant.Server.LoopTypes (Fact (..))
import Olifant.Server.Plans (Resolved (..), gitText, resolveAnchor, safely)
import Olifant.Server.State (ServerState (..))
import Olifant.Server.Unit (Named (..), unitNamed, unitsOf)

-- | An excerpt: its step's row, its fence's opening and closing rows
-- (0-based), the language and anchor of its info string, whether it is
-- frozen, and its code (without the fence's indent).
data Excerpt = Excerpt
  { exStep :: Int
  , exOpen :: Int
  , exClose :: Int
  , exLang :: Text
  , exAnchor :: Anchor
  , exDone :: Bool
  , exBody :: [Text]
  }
  deriving stock (Eq, Show)

-- | Every step's excerpts in LS.
excerptsOf :: [Line] -> [Excerpt]
excerptsOf ls =
  [ Excerpt s o c lang a done [unindent (lnIndent (rowOf s) + 2) (lnText (rowOf r)) | r <- [o + 1 .. c - 1]]
  | (s, o, c) <- excerptSpans ls
  , Just (lang, a, done) <- [excerptInfo (lnText (rowOf o))] ]
  where
    rowOf r = ls !! r
    unindent n l
      | T.all isSpace l = ""
      | otherwise = T.drop (min n (T.length (T.takeWhile (== ' ') l))) l

-- | An excerpt's lines at INDENT: the opening line, the code (a blank line
-- as the indent alone), the closing line; the fence one backtick longer
-- than the longest run in the code, at least three.
renderFence :: Int -> Text -> Anchor -> Bool -> [Text] -> [Text]
renderFence indent lang a done body =
  [pad <> fence <> lang <> " " <> anchorText a <> (if done then " · done" else "")] ++ map line body ++ [pad <> fence]
  where
    pad = T.replicate indent " "
    fence = T.replicate (fenceLength body) "`"
    line l = if T.all isSpace l then pad else pad <> l

fenceLength :: [Text] -> Int
fenceLength body = max 3 (1 + maximum (0 : [T.length r | l <- body, r <- T.group l, T.head r == '`']))

-- | What an excerpt shows of unit N in a file's LINES, given the unit's lines
-- at the baseline: a unit of 30 lines or fewer whole; else its header (its
-- first line to its definition's) and each line changed since the base with
-- two lines of context, a "…" at each gap and after the last piece when the
-- unit goes on; with no base, or no change, its first 30 lines and "…". At
-- most 60 lines.
clip :: Named -> [Text] -> Maybe [Text] -> [Text]
clip n file base
  | len <= 30 = us
  | otherwise = capped (if null marks then take 30 us ++ ["…"] else pieces)
  where
    (a, z) = namedLines n
    us = take (z - a + 1) (drop (a - 1) file)
    len = length us
    marks = maybe [] changed base
    -- each line added at the base, and where a line was removed
    changed bs = nub (go 0 (editScript bs us))
      where
        go j ((c, _) : rest) = case c of
          ' ' -> go (j + 1) rest
          '+' -> j : go (j + 1) rest
          _ -> min (len - 1) j : go j rest
        go _ [] = []
    spans = merge (sort ((0, max 0 (namedDef n - a)) : [(max 0 (i - 2), min (len - 1) (i + 2)) | i <- marks]))
    merge ((x, y) : (x', y') : rest) | x' <= y + 1 = merge ((x, max y y') : rest)
    merge (p : rest) = p : merge rest
    merge [] = []
    pieces = concat [["…" | x > prev + 1] ++ take (y - x + 1) (drop x us) | ((x, y), prev) <- zip spans (-1 : map snd spans)]
      ++ ["…" | maybe False ((< len - 1) . snd) (listToMaybe (reverse spans))]
    capped xs
      | length xs <= 60 = xs
      | otherwise = reverse (dropWhile (== "…") (reverse (take 59 xs))) ++ ["…"]

-- | The facts that keep LS's excerpts: a ticked step's live excerpt frozen
-- (its opening line alone); an open step's live one rendered again from its
-- anchor (where MOVED takes it, the info string following) when it differs
-- from the text. An anchor that does not RESOLVE (to a language and the
-- clipped code), and a frozen excerpt, are left as they are.
excerptFacts :: [Line] -> (Anchor -> Maybe Anchor) -> (Anchor -> Maybe (Text, [Text])) -> [Fact]
excerptFacts ls moved resolve = concatMap one (excerptsOf ls)
  where
    rowOf r = ls !! r
    one e
      | exDone e = []
      | stepDone step = [Replace (exOpen e) (T.stripEnd (lnText (rowOf (exOpen e))) <> " · done")]
      | Just (lang, body) <- resolve a
      , let new = renderFence (lnIndent step + 2) lang a False body
      , map T.stripEnd new /= map (T.stripEnd . lnText . rowOf) [exOpen e .. exClose e] =
          [ReplaceRows (exOpen e) (exClose e) new]
      | otherwise = []
      where
        step = rowOf (exStep e)
        a = fromMaybe (exAnchor e) (moved (exAnchor e))

-- | The unit an anchor names as an excerpt shows it: its language's name and
-- its clipped code, against the unit at BASE (`git show`); Nothing when it
-- does not resolve.
unitExcerpt :: ServerState -> Project -> Maybe Text -> Anchor -> IO (Maybe (Text, [Text]))
unitExcerpt s p base a = do
  r <- resolveAnchor s p a
  case (rsUnit r, rsText r, languageFor (catalogLanguages (prCatalog p)) (rsRel r)) of
    (Just u, Just t, Just lang) -> do
      old <- maybe (pure Nothing) (\b -> gitText (prRoot p) ["show", T.unpack b <> ":./" <> rsRel r]) base
      units <- case old of
        Just o | not (T.null o) -> safely (Left "") (unitsOf (pSyntax (ssPorts s)) lang o)
        _ -> pure (Right [])
      let was = case units of
            Right us | Just w <- unitNamed (namedName u) us, namedName w == namedName u, Just o <- old ->
              let (x, y) = namedLines w in Just (take (y - x + 1) (drop (x - 1) (T.splitOn "\n" o)))
            _ -> Nothing
      pure (Just (langName lang, clip u (T.splitOn "\n" t) was))
    _ -> pure Nothing

-- | A hover over an anchor: "`path:Unit` · lines a–b", then the unit's
-- code (LINES of its file, in LANG), cut at 60 lines and a "…".
hoverText :: FilePath -> Text -> Text -> Named -> [Text] -> Text
hoverText rel unit lang n file =
  T.intercalate "\n" (["`" <> T.pack rel <> ":" <> unit <> "` · lines " <> num a <> "–" <> num z, "", fence <> lang] ++ body ++ [fence])
  where
    (a, z) = namedLines n
    us = take (z - a + 1) (drop (a - 1) file)
    body = if length us > 60 then take 60 us ++ ["…"] else us
    fence = T.replicate (fenceLength body) "`"
    num = T.pack . show

-- | The hover over anchor A: its unit's code ('hoverText'); Nothing when it
-- does not resolve.
unitHover :: ServerState -> Project -> Anchor -> IO (Maybe Text)
unitHover s p a =
  resolveAnchor s p a <&> \r -> case (rsUnit r, rsText r, languageFor (catalogLanguages (prCatalog p)) (rsRel r)) of
    (Just u, Just t, Just lang) -> Just (hoverText (rsRel r) (anUnit a) (langName lang) u (T.splitOn "\n" t))
    _ -> Nothing
