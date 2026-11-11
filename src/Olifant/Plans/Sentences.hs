{-# LANGUAGE OverloadedStrings #-}
-- | The sentences of a plan: the units triage and the checker work on. Pure.
--
-- Steps and questions are one sentence each, judged whole; headings are one
-- sentence (the "· …" suffix dropped); test, also, evidence and journal lines,
-- fenced code, front matter and lines that are only a link give none;
-- everything else (paragraphs, numbered lines, bullets, blockquotes) is split.
-- A line ends a sentence: a paragraph's (or bullet's) next row starts a new
-- one, unless the join predicate says the row continues on the next (a
-- hard-wrapped paragraph; Jev decides which, see 'joinCandidates'), and then
-- the rows are joined before splitting, so a sentence may span rows.
module Olifant.Plans.Sentences
  ( Sentence (..)
  , sentences
  , sentencesWith
  , sentencesJoined
  , joinCandidates
  , pairKey
  , sentenceKey
  ) where

import Data.Char (isAlphaNum, isDigit, isHexDigit, isSpace, isUpper, toLower)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Olifant.Plans

-- | Positions are 0-based code points; the end is exclusive.
data Sentence = Sentence
  { snRow :: Int, snCol :: Int, snEndRow :: Int, snEndCol :: Int
  , snKind :: Kind            -- ^ the kind of the line the sentence starts on
  , snText :: Text            -- ^ the sentence alone: markers, anchor and verdict stripped
  , snHeading :: Text         -- ^ the section title, "" before any heading
  , snLead :: Maybe Text      -- ^ the enclosing question, else the previous sentence of the block
  , snAnchor :: Maybe Anchor }
  deriving (Eq, Show)

-- | A run of rows read as one text: whole (steps, questions, headings) or split.
data Unit = Unit
  { uKind :: Kind, uSplit :: Bool, uJoin :: Joins
  , uSegs :: [(Int, Int, Text)]   -- ^ (row, column the text starts at, text), in order
  , uLines :: [Line] }

-- | What later rows may continue the unit.
data Joins = NoJoin | JoinProse | JoinQuote deriving (Eq)

-- | Every line ends its sentences: no row joins the next.
sentences :: [Line] -> [Sentence]
sentences = sentencesWith (const False)

-- | The sentences when row r continues on row r+1 exactly when JOINS r.
sentencesWith :: (Int -> Bool) -> [Line] -> [Sentence]
sentencesWith joins ls = withLeads (concatMap (splitUnit joins ls) (units ls (skippedRows ls)))
  where
    withLeads ss = reverse (snd (foldl' lead (Nothing, []) ss))
    asked = questionParents ls
    lead (prev, acc) (blk, s) =
      let l = case M.lookup (snRow s) asked of
            Just q -> Just q
            Nothing -> case prev of { Just (b, t) | b == blk -> Just t; _ -> Nothing }
      in (Just (blk, snText s), s { snLead = l } : acc)

-- | The sentences with the joins a store keeps: by 'pairKey' of a candidate's
-- two rows, True joins; anything else (no answer, False) does not.
sentencesJoined :: M.Map Text Bool -> [Line] -> [Sentence]
sentencesJoined stored ls = sentencesWith joined ls
  where
    cands = M.fromList [(r, pairKey u n) | (r, u, n) <- joinCandidates ls]
    joined r = maybe False (\k -> M.lookup k stored == Just True) (M.lookup r cands)

-- | The rows that may continue on the next: (row, its text, the next row's
-- text), for adjacent rows of one paragraph, bullet or quote where the upper
-- row does not end with . ? ! or : (closing quotes and parens after one
-- count as the end).
joinCandidates :: [Line] -> [(Int, Text, Text)]
joinCandidates ls =
  [ (r1, T.strip t1, T.strip t2)
  | (_, u) <- units ls (skippedRows ls), uSplit u
  , ((r1, _, t1), (_, _, t2)) <- zip (uSegs u) (drop 1 (uSegs u))
  , not (endsSentence t1) ]
  where
    endsSentence t = case T.unsnoc (T.dropWhileEnd (`elem` ("\"”'’)]" :: String)) (T.stripEnd t)) of
      Just (_, c) -> c `elem` (".?!:" :: String)
      Nothing -> True

-- | A candidate's key in the store: the two rows' 'sentenceKey's.
pairKey :: Text -> Text -> Text
pairKey upper next = sentenceKey upper <> " ⏎ " <> sentenceKey next

skippedRows :: [Line] -> S.Set Int
skippedRows ls = fencedRows ls `S.union` frontMatter ls

-- | Rows inside a closed fence, fences included. An opener never closed is no fence.
fencedRows :: [Line] -> S.Set Int
fencedRows ls = go (map (\l -> (lnRow l, lnText l)) ls)
  where
    go [] = S.empty
    go ((r, t) : rest) = case fenceTicks t of
      Just n | not (T.any (== '`') (T.drop n (T.stripStart t))) ->
        case break (closes n . snd) rest of
          (inside, (c, _) : after) -> S.fromList (r : c : map fst inside) `S.union` go after
          (_, []) -> go rest
      _ -> go rest
    closes n t = maybe False (>= n) (fenceTicks t) && T.all (== '`') (T.strip t)
    fenceTicks t = let n = T.length (T.takeWhile (== '`') (T.stripStart t)) in if n >= 3 then Just n else Nothing

-- | A "---" block at the very top.
frontMatter :: [Line] -> S.Set Int
frontMatter (l : rest) | T.strip (lnText l) == "---" =
  case break ((== "---") . T.strip . lnText) rest of
    (inside, c : _) -> S.fromList (lnRow l : lnRow c : map lnRow inside)
    _ -> S.empty
frontMatter _ = S.empty

-- | For each row under a question (by indent), that question's text; the innermost wins.
questionParents :: [Line] -> M.Map Int Text
questionParents ls = M.fromList
  [ (lnRow c, wordsOnly (T.drop 4 (T.stripStart (lnText q))))
  | q <- ls, lnKind q == Question, c <- childrenOf ls (lnRow q) ]

units :: [Line] -> S.Set Int -> [(Int, Unit)]
units ls skipped = finish (foldl' step (0, Nothing, []) ls)
  where
    finish (_, cur, acc) = reverse (maybe acc (: acc) cur)
    step (blk, cur, acc) l
      | lnRow l `S.member` skipped || T.all isSpace (lnText l) = (blk + 1, Nothing, flush cur acc)
      | otherwise = case lnKind l of
          Heading _ -> (blk + 2, Nothing, (blk + 1, unit NoJoin False (blk + 1) l (headingSeg l)) : flush cur acc)
          Step _ -> new NoJoin False (marked 6)
          Question -> new NoJoin False (marked 4)
          Claim -> new JoinProse True (marked 2)
          Prose
            | Just n <- numbered body -> new JoinProse True (marked n)
            | "> " `T.isPrefixOf` body || body == ">" ->
                if fmap (uJoin . snd) cur == Just JoinQuote then cont (marked 1) else new JoinQuote True (marked 1)
            | onlyLink body -> (blk, Nothing, flush cur acc)
            | Just (_, u) <- cur, uJoin u == JoinProse -> cont (lnRow l, ind, body)
            | otherwise -> new JoinProse True (lnRow l, ind, body)
          _ -> (blk, Nothing, flush cur acc)
      where
        ind = lnIndent l
        body = T.drop ind (lnText l)
        marked n = (lnRow l, ind + n, T.drop n body)
        new j sp seg@(_, _, t)
          | onlyLink t = (blk, Nothing, flush cur acc)
          | otherwise = (blk, Just (blk, unit j sp blk l seg), flush cur acc)
        cont seg = case cur of
          Just (b, u) -> (blk, Just (b, u { uSegs = uSegs u ++ [seg], uLines = uLines u ++ [l] }), acc)
          Nothing -> new JoinProse True seg
    flush cur acc = maybe acc (: acc) cur
    unit j sp _ l seg = Unit (lnKind l) sp j [seg] [l]

-- | "1 - ", "1. ", "1) ": the marker's length.
numbered :: Text -> Maybe Int
numbered b =
  let ds = T.takeWhile isDigit b
      rest = T.drop (T.length ds) b
  in if T.null ds then Nothing
     else listToMaybe [T.length ds + T.length m | m <- [" - ", ". ", ") "], m `T.isPrefixOf` rest]

-- | A line that is only a URL, "<URL>" or "[text](URL)".
onlyLink :: Text -> Bool
onlyLink t0 = isUrl t || bracketed || markdownLink
  where
    t = T.strip t0
    isUrl u = ("http://" `T.isPrefixOf` u || "https://" `T.isPrefixOf` u) && not (T.any isSpace u)
    bracketed = "<" `T.isPrefixOf` t && ">" `T.isSuffixOf` t && isUrl (T.drop 1 (T.dropEnd 1 t))
    markdownLink = "[" `T.isPrefixOf` t && ")" `T.isSuffixOf` t
      && case T.breakOn "](" t of
           (_, r) | not (T.null r) -> isUrl (T.drop 2 (T.dropEnd 1 r))
           _ -> False

headingSeg :: Line -> (Int, Int, Text)
headingSeg l =
  let ind = lnIndent l
      body = T.drop ind (lnText l)
      hashes = T.length (T.takeWhile (== '#') body)
      afterHashes = T.drop hashes body
      sp = T.length (T.takeWhile (== ' ') afterHashes)
  in (lnRow l, ind + hashes + sp, fst (T.breakOn " · " (T.drop sp afterHashes)))

splitUnit :: (Int -> Bool) -> [Line] -> (Int, Unit) -> [(Int, Sentence)]
splitUnit joins ls (blk, u) = mapMaybe toSentence spans
  where
    -- The joined text, each character with its (row, column); the joining space takes the next row's start.
    segs = uSegs u
    chars = concat (joinWith (zip3 (Nothing : map (\(r, _, _) -> Just r) segs) [0 :: Int ..] segs))
    joinWith ss = [ [(' ', (r, c - 1)) | i > 0] ++ zip (T.unpack t) [(r, c + k) | k <- [0 ..]] | (_, i, (r, c, t)) <- ss ]
    -- the index of each joining space and the row above it
    joints = scanJoints 0 (zip3 (Nothing : map (\(r, _, _) -> Just r) segs) [0 :: Int ..] segs)
    scanJoints _ [] = []
    scanJoints at ((above, i, (_, _, t)) : rest) =
      let here = [(at, r) | i > 0, Just r <- [above]]
          w = (if i > 0 then 1 else 0) + T.length t
      in here ++ scanJoints (at + w) rest
    str = map fst chars
    n = length str
    -- a row that does not join the next ends its sentence at the joining space
    forced = [k | (k, r) <- joints, not (joins r)]
    cuts = if uSplit u then S.toAscList (S.fromList (ends str ++ forced)) else []
    spans = zip (0 : cuts) (cuts ++ [n])
    posArr = M.fromList (zip [0 ..] (map snd chars))
    charAt = M.fromList (zip [0 :: Int ..] str)
    toSentence (a, b) =
      let idx = [i | i <- [a .. b - 1], maybe False (not . isSpace) (M.lookup i charAt)]
      in case idx of
           [] -> Nothing
           (i0 : more) ->
             let i1 = if null more then i0 else last more
                 (r0, c0) = posArr M.! i0
                 (r1, c1) = posArr M.! i1
                 raw = T.pack (take (i1 - i0 + 1) (drop i0 str))
                 txt = wordsOnly raw
                 anc = listToMaybe
                   [ a' | l <- uLines u, Just a' <- [lnAnchor l]
                   , let p = (lnRow l, anStart a'), p >= (r0, c0), p <= (r1, c1) ]
             in if not (T.any isAlphaNum txt) then Nothing
                else Just (blk, Sentence r0 c0 r1 (c1 + 1) (uKind u) txt (headingOf r0) Nothing anc)
    headingOf r = fst (T.breakOn " · " (secTitle (sectionOf ls r)))

-- | Indices just after each sentence end, except the text's end.
ends :: String -> [Int]
ends s = [i + 1 | i <- [0 .. n - 1], isEnd i]
  where
    n = length s
    arr = M.fromList (zip [0 ..] s)
    at i = M.lookup i arr
    opaque = opaqueSet s
    isEnd i =
      at i `elem` map Just ".?!"
        && not (i `S.member` opaque)
        && at (i + 1) == Just ' '
        && maybe False (\c -> isUpper c || c `elem` ("\"“'‘" :: String)) (at (i + 2))
        && not (abbreviation i)
    abbreviation i =
      let w = map toLower (reverse (takeWhile (not . isSpace) (reverse (take (i + 1) s))))
          w' = dropWhile (`elem` ("(\"“'‘" :: String)) w
      in w' `elem` ["e.g.", "i.e.", "vs.", "etc.", "dr."]

-- | Indices inside code spans and URLs (a URL's trailing punctuation is not part of it).
opaqueSet :: String -> S.Set Int
opaqueSet s = go 0 s
  where
    go _ [] = S.empty
    go i cs@(c : rest)
      | c == '`', Just k <- lookupTick rest = S.fromList [i .. i + k + 1] `S.union` go (i + k + 2) (drop (k + 1) rest)
      | startsUrl cs && (i == 0 || not (isAlphaNum (s !! (i - 1)))) =
          let tok = takeWhile (\x -> not (isSpace x) && x /= '>') cs
              core = reverse (dropWhile (`elem` (".,;:!?)" :: String)) (reverse tok))
              k = length core
          in S.fromList [i .. i + k - 1] `S.union` go (i + k) (drop k cs)
      | otherwise = go (i + 1) rest
    lookupTick rest = case break (== '`') rest of
      (inside, _ : _) -> Just (length inside)
      _ -> Nothing
    startsUrl cs = take 7 cs == "http://" || take 8 cs == "https://"

-- | Words joined by one space, with anchor ("@path:Unit") and verdict ("✓ <hex>", "? not found") tokens dropped.
wordsOnly :: Text -> Text
wordsOnly = T.unwords . go . T.words
  where
    go (w : rest) | "@" `T.isPrefixOf` w, T.any (== ':') w = go rest
    go ("✓" : h : rest) | not (T.null h), T.all isHexDigit h = go rest
    go ("✓" : rest) = go rest
    go ("?" : "not" : "found" : rest) = go rest
    go (w : rest) = w : go rest
    go [] = []

-- | A sentence's key in the store: its words, anchor and verdict tokens removed.
sentenceKey :: Text -> Text
sentenceKey = wordsOnly
