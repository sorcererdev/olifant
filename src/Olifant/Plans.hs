{-# LANGUAGE OverloadedStrings #-}
-- | The plans's public interface. Other units and specs import only this module.
-- A pure parser for markdown plan files: line kinds, anchors, verdicts, sections.
module Olifant.Plans
  ( Kind (..)
  , Anchor (..)
  , Verdict (..)
  , Line (..)
  , Section (..)
  , parsePlan
  , sections
  , sectionOf
  , childrenOf
  , withVerdict
  , TestState (..)
  , testState
  , withTestState
  , evidenceTag
  , stepDone
  , anchorText
  ) where

import Data.List (mapAccumL)
import Data.Char (isAlpha, isAlphaNum, isDigit, isHexDigit, isSpace)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

data Kind = Heading Int | Claim | Question | Step Bool | TestChild | AlsoChild | Evidence | Journal | Prose
  deriving (Eq, Show)

-- | Columns of "@path:Unit", 0-based, end exclusive.
data Anchor = Anchor { anPath :: FilePath, anUnit :: Text, anStart :: Int, anEnd :: Int }
  deriving (Eq, Show)

data Verdict = Verified Text | NotFound | NoVerdict
  deriving (Eq, Show)

data Line = Line
  { lnRow :: Int, lnIndent :: Int, lnKind :: Kind, lnText :: Text
  , lnAnchor :: Maybe Anchor, lnVerdict :: Verdict }
  deriving (Eq, Show)

-- | Rows [secRow, secEnd). A heading "Title · branch name" names the section's branch.
data Section = Section { secTitle :: Text, secRow :: Int, secEnd :: Int, secBranch :: Maybe Text }
  deriving (Eq, Show)

parsePlan :: Text -> [Line]
parsePlan t = snd (mapAccumL step False (zip [0 ..] (T.lines t)))
  where
    -- A line opening with three backticks toggles the fence; it and everything inside is Prose.
    step inFence (row, txt)
      | isFence txt = (not inFence, prose row txt)
      | inFence = (inFence, prose row txt)
      | otherwise = (inFence, line row txt)
    isFence = ("```" `T.isPrefixOf`) . T.stripStart
    prose row txt = (line row txt) { lnKind = Prose, lnAnchor = Nothing, lnVerdict = NoVerdict }

line :: Int -> Text -> Line
line row txt = Line row ind kind txt anc (maybe NoVerdict (verdictAfter txt) anc)
  where
    ind = T.length (T.takeWhile (== ' ') txt)
    body = T.drop ind txt
    kind = kindOf body
    -- A journal line's words are a record, not a claim: no anchor in them.
    anc = if kind == Journal then Nothing else findAnchor txt

kindOf :: Text -> Kind
kindOf b
  | Just n <- headingLevel b = Heading n
  | "- test: " `T.isPrefixOf` b = TestChild
  | "- also @" `T.isPrefixOf` b = AlsoChild
  | "- ✓ " `T.isPrefixOf` b || "- ✗ " `T.isPrefixOf` b = Evidence
  | "- ? " `T.isPrefixOf` b = Question
  | "- [ ] " `T.isPrefixOf` b = Step False
  | "- [x] " `T.isPrefixOf` b = Step True
  | Just r <- T.stripPrefix "- " b, isJournal r = Journal
  | "- " `T.isPrefixOf` b = Claim
  | otherwise = Prose

-- | "25 Oct 21:31 · …": a day of one or two digits, a month, a time, a dot.
isJournal :: Text -> Bool
isJournal r = case T.words (T.take 16 r) of
  (d : m : hm : _) ->
    T.length d `elem` [1, 2] && T.all isDigit d
      && T.length m == 3 && T.all isAlpha m
      && T.length hm == 5 && T.index hm 2 == ':' && T.all isDigit (T.filter (/= ':') hm)
      && (d <> " " <> m <> " " <> hm <> " · ") `T.isPrefixOf` r
  _ -> False

headingLevel :: Text -> Maybe Int
headingLevel b
  | n >= 1 && n <= 6 && (T.null rest || T.head rest == ' ') = Just n
  | otherwise = Nothing
  where
    hashes = T.takeWhile (== '#') b
    n = T.length hashes
    rest = T.drop n b

-- | The last match of @(\S+):([\w.]+) on the line.
findAnchor :: Text -> Maybe Anchor
findAnchor txt = listToMaybe (reverse [a | i <- ats, Just a <- [matchAt i]])
  where
    ats = [i | (i, c) <- zip [0 ..] (T.unpack txt), c == '@']
    matchAt i =
      let after = T.drop (i + 1) txt
          tok = T.takeWhile (not . isSpace) after
          colons = reverse [k | (k, c) <- zip [0 ..] (T.unpack tok), c == ':', k > 0]
          try k =
            let unit = T.dropWhileEnd (== '.') (T.takeWhile isUnitChar (T.drop (k + 1) tok))
            in if T.null unit then Nothing
               else Just (Anchor (T.unpack (T.take k tok)) unit i (i + 1 + k + 1 + T.length unit))
      in listToMaybe [a | k <- colons, Just a <- [try k]]

isUnitChar :: Char -> Bool
isUnitChar c = isAlphaNum c || c == '_' || c == '.'

-- | Split a verdict token off the text after the anchor: (verdict, length consumed incl. leading spaces).
verdictTok :: Text -> (Verdict, Int)
verdictTok rest
  | Just r <- T.stripPrefix "✓ " s
  , let h = T.take 7 r
  , T.length h == 7, T.all isHexDigit h
  = (Verified h, lead + 2 + 7)
  | "? not found" `T.isPrefixOf` s = (NotFound, lead + T.length "? not found")
  | otherwise = (NoVerdict, 0)
  where
    s = T.dropWhile (== ' ') rest
    lead = T.length rest - T.length s

verdictAfter :: Text -> Anchor -> Verdict
verdictAfter txt a = fst (verdictTok (T.drop (anEnd a) txt))

sections :: [Line] -> [Section]
sections ls = pre ++ go heads
  where
    n = length ls
    heads = [(lnRow l, splitBranch (T.strip (T.dropWhile (== '#') (T.strip (lnText l))))) | l <- ls, Heading _ <- [lnKind l]]
    splitBranch h = case T.breakOn " · branch " h of
      (t, rest) | not (T.null rest) -> (t, Just (T.strip (T.drop (T.length " · branch ") rest)))
      _ -> (h, Nothing)
    firstH = case heads of { ((r, _) : _) -> r; [] -> n }
    pre = [Section "" 0 firstH Nothing | firstH > 0]
    go ((r, (t, b)) : rest@((r', _) : _)) = Section t r r' b : go rest
    go [(r, (t, b))] = [Section t r n b]
    go [] = []

sectionOf :: [Line] -> Int -> Section
sectionOf ls row = case [s | s <- sections ls, secRow s <= row, row < secEnd s] of
  (s : _) -> s
  [] -> Section "" 0 (length ls) Nothing

childrenOf :: [Line] -> Int -> [Line]
childrenOf ls row = case drop row ls of
  (p : rest) -> takeWhile (\l -> lnIndent l > lnIndent p) rest
  [] -> []

anchorText :: Anchor -> Text
anchorText a = "@" <> T.pack (anPath a) <> ":" <> anUnit a

withVerdict :: Text -> Verdict -> Text
withVerdict txt v = case findAnchor txt of
  Nothing -> txt
  Just a ->
    let pre = T.take (anEnd a) txt
        rest = T.drop (anEnd a) txt
        (_, used) = verdictTok rest
        tl = T.drop used rest
        mark = case v of
          Verified h -> " ✓ " <> h
          NotFound -> " ? not found"
          NoVerdict -> ""
    in pre <> mark <> tl

-- | The mark at the end of a test child: not run yet, green, red.
data TestState = TestPending | TestGreen | TestRed
  deriving (Eq, Show)

testMark :: TestState -> Text
testMark TestPending = "○"
testMark TestGreen = "●"
testMark TestRed = "✗"

lastTok :: Text -> Text
lastTok = T.takeWhileEnd (not . isSpace) . T.stripEnd

testState :: Line -> Maybe TestState
testState l
  | lnKind l /= TestChild = Nothing
  | otherwise = lookup (lastTok (lnText l)) [(testMark s, s) | s <- [TestPending, TestGreen, TestRed]]

-- | Replace the trailing mark, or append one.
withTestState :: Text -> TestState -> Text
withTestState txt s
  | lastTok txt `elem` map testMark [TestPending, TestGreen, TestRed] = T.dropWhileEnd isSpace (T.dropEnd 1 (T.stripEnd txt)) <> " " <> testMark s
  | otherwise = T.stripEnd txt <> " " <> testMark s

-- | "- ✗ changed: …" -> "changed"; "- ✗ test_x red again" -> "red again"; "- ✓ …" -> Nothing.
evidenceTag :: Line -> Maybe Text
evidenceTag l
  | lnKind l /= Evidence = Nothing
  | Just _ <- T.stripPrefix "- ✗ " body, "red again" `T.isSuffixOf` T.stripEnd body = Just "red again"
  | Just r <- T.stripPrefix "- ✗ " body
  , let (w, c) = T.breakOn ":" r
  , not (T.null c), not (T.null w), not (T.any isSpace w) = Just w
  | otherwise = Nothing
  where body = T.stripStart (lnText l)

stepDone :: Line -> Bool
stepDone l = lnKind l == Step True
