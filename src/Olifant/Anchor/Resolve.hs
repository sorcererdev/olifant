-- | Resolving `at` anchors in a file's text (spec §5.2, §5.4).
module Olifant.Anchor.Resolve
  ( RenderedAnchor (..)
  , Located (..)
  , Spacing (..)
  , Miss (..)
  , resolveLayers
  , locateContainer
  , locateContainers
  , itemsOfTarget
  , insertLines
  , grownLine
  , insertedText
  , placedText
  , Opening (..)
  , alreadyPresent
  , presentAt
  , describeMisses
  ) where

import Olifant.Language.Header (headerEnd)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Char (isAlphaNum)
import Data.Functor ((<&>))
import Data.List (maximumBy, minimumBy, nub, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Anchor.Phrase
import Olifant.Language
import Olifant.Holes (hasMarkers, splitMarkers)
import Olifant.Template (Value (VText), describeTemplateError)
import Text.Regex.TDFA (Regex, makeRegexM, matchTest)

data RenderedAnchor = PhraseAnchor Text | RuleAnchor Aeson.Value Position
  deriving stock (Eq, Show)

-- | 'locSpacing': a blank line to add around the new text, copied from the
-- spacing between the target's neighbouring items (spec §5.2). 'locOpen':
-- an empty container on one line, @(line, from, to)@ its columns, which the
-- insert opens (`()` → `(` / text / `)`) instead of adding whole lines.
data Located = Located {locLine :: Int, locIndent :: Text, locRegion :: Text, locUsed :: Text, locSpacing :: Spacing, locOpen :: Maybe Opening, locComment :: Text}
  deriving stock (Eq, Show)

-- | How an empty container is opened: 'OpenLine' splits its one line at
-- @(line, from, to)@, the bracket columns; 'OpenLines' inserts before the
-- closing line of a container spread over lines. Either way the text becomes
-- the first item, so a leading separator is dropped ('placedText').
--
-- 'Grow': a one-line container with items, in a language with a
-- `separator`: the new item joins it inline when the line stays within
-- 'growWidth', else every item goes on its own line ('grownLine').
data Opening
  = OpenLine Int Int Int
  | OpenLines
  | Grow {growLine :: Int, growOpen :: Int, growClose :: Int, growItems :: [Text], growSep :: Text, growWidth :: Int, growIndent :: Text, growAtStart :: Bool}
  deriving stock (Eq, Show)

data Spacing = NoSpacing | BlankBefore | BlankAfter
  deriving stock (Eq, Show)

data Miss = Miss {missAnchor :: Text, missReason :: Text, missTool :: Bool}
  deriving stock (Eq, Show)

describeMisses :: [Miss] -> Text
describeMisses = T.intercalate "; " . map (\m -> "`" <> missAnchor m <> "`: " <> missReason m)

resolveLayers :: Monad m => Syntax m -> Maybe Language -> [RenderedAnchor] -> Text -> m (Either [Miss] Located)
resolveLayers syntax lang anchors content = go [] anchors
  where
    go misses [] = pure (Left (reverse misses))
    go misses (a : rest) =
      resolveOne syntax lang a content >>= \case
        Right loc -> pure (Right loc)
        Left miss -> go (miss : misses) rest

resolveOne :: Monad m => Syntax m -> Maybe Language -> RenderedAnchor -> Text -> m (Either Miss Located)
resolveOne syntax lang anchor content = case anchor of
  RuleAnchor rule pos -> case lang of
    Nothing -> pure (Left (miss "no language adapter for this file (add languages/<name>.toml to a pack)"))
    Just l ->
      querySyntax syntax l rule content >>= \case
        Left f -> pure (Left (toolMiss f))
        Right [m] -> pure (around m pos)
        Right ms -> pure (Left (miss ("the rule matches " <> count ms <> " places; it must match once")))
  PhraseAnchor p -> case parsePhrase p of
    Left err -> pure (Left (miss err))
    Right (Phrase pos (MarkerTarget m)) -> pure (markerAt pos m)
    Right (Phrase pos (LineTarget which re)) -> pure (lineAt pos which re)
    -- `start of file` / `end of file`: built in, for any file (even empty);
    -- the whole file is the region, so a re-run finds its own line.
    Right (Phrase pos (NamedTarget ["file"])) ->
      let lastLine = length ls - (if lineAt' (length ls - 1) == "" then 1 else 0)
       in pure (Right (located (if pos `elem` [StartOf, Before] then 0 else lastLine) "" content))
    Right (Phrase pos (NamedTarget ws)) -> case lang of
      Nothing -> pure (Left (miss "no language adapter for this file (add languages/<name>.toml to a pack)"))
      Just l -> named l pos ws
  where
    used = case anchor of
      PhraseAnchor p -> p
      RuleAnchor _ pos -> "ast rule (" <> positionWords pos <> ")"
    miss why = Miss used why False
    toolMiss f = Miss used (describeSyntaxFailure f) (case f of SyntaxToolMissing _ -> True; SyntaxFailed _ -> False)
    ls = T.splitOn "\n" (T.replace "\r\n" "\n" content)
    indentAt i = T.takeWhile (\c -> c == ' ' || c == '\t') (if i < length ls then ls !! i else "")
    regionOf a b = T.intercalate "\n" (take (b - a + 1) (drop a ls))
    located line indent region = Located line indent region used NoSpacing Nothing (maybe "" langComment lang)
    count = T.pack . show . length
    around m pos =
      let (s, e) = (fst (matchStart m), fst (matchEnd m))
       in if pos `elem` [StartOf, Before]
            then wholeLine "the match" (startsLine (matchStart m)) (located s (indentAt s) (regionOf s e))
            else wholeLine "the match" (endsLine (matchEnd m)) (located (e + 1) (indentAt s) (regionOf s e))
    -- Items separated by a blank line (Elm case branches, Rust methods).
    covered sep inside texts =
      let strip rest = \case
            [] -> rest
            i : is -> let (b, a) = T.breakOn i rest in strip (b <> T.drop (T.length i) a) is
       in T.all (`elem` (" \t" <> T.unpack sep)) (strip inside texts)
    blankBetween a b = any (T.null . T.strip . lineAt') [fst (matchEnd a) + 1 .. fst (matchStart b) - 1]
    -- Olifant inserts whole lines, so a position must begin or end its line.
    lineAt' i = if i < length ls then ls !! i else ""
    -- A separator next to an item (`, b` in Elm, `x: X,` in Rust) is part of its line.
    startsLine (l, col) = T.strip (T.take col (lineAt' l)) `elem` ["", ",", "|"]
    endsLine (l, col) = T.strip (T.drop col (lineAt' l)) `elem` ["", ",", ";"]
    wholeLine what ok loc
      | ok = Right loc
      | otherwise = Left (miss (what <> " shares its line with other code; Olifant inserts whole lines (use a multi-line layout, or another anchor)"))
    beforeAfter pos i what = case pos of
      Before -> Right i
      After -> Right (i + 1)
      _ -> Left (miss ("use `before " <> what <> "` or `after " <> what <> "`: " <> what <> "s have no items"))
    markerAt pos m = case [i | (i, l) <- zip [0 ..] ls, T.strip l == T.strip m] of
      [] -> Left (miss "marker not found")
      [i] -> (\line -> located line (indentAt i) (markerBlock i)) <$> beforeAfter pos i "marker"
      hits -> Left (miss ("marker appears " <> count hits <> " times; it must appear once"))
    markerBlock i =
      let depth = T.length (indentAt i)
          inBlock l = T.null (T.strip l) || T.length (T.takeWhile (\c -> c == ' ' || c == '\t') l) >= depth
       in T.unlines (reverse (takeWhile inBlock (reverse (take i ls))))
    lineAt pos which re = case makeRegexM (T.unpack re) :: Maybe Regex of
      Nothing -> Left (miss "not a valid regular expression (POSIX extended syntax)")
      Just rx ->
        let hits = [i | (i, l) <- zip [0 ..] ls, matchTest rx (T.unpack l)]
            runAround i = let isHit j = j `elem` hits
                              start = last (takeWhile isHit [i, i - 1 .. 0])
                              end = last (takeWhile isHit [i .. length ls - 1])
                           in regionOf start end
         in case hits of
              [] -> Left (miss "no line matches")
              (h : _) ->
                let i = if which == FirstLine then h else last hits
                 in (\line -> located line (indentAt i) (runAround i)) <$> beforeAfter pos i "line"
    named l pos ws =
      itemsOfTarget syntax l (positionWords pos) ws content <&> \case
        Left (why, tool) -> Left (Miss used why tool)
        Right Nothing -> Left (miss "not found")
        Right (Just (_, []))
          | Just "after header" <- targetWhenEmpty =<< target, pos `elem` [StartOf, EndOf] ->
              let h = headerEnd ls
               in Right ((located h "" (regionOf 0 (max 0 (length ls - 1)))) {locSpacing = if h == 0 then NoSpacing else BlankBefore})
        Right (Just (c, items)) -> (\loc -> if maybe True targetSpaced target then loc else loc {locSpacing = NoSpacing}) <$> place l separator pos c items
      where
        target = case [t | t <- langTargets l, Just _ <- [matchShape t ws]] of
          t : _ -> Just t
          [] -> Nothing
        -- the target's separator over the language's; "" never grows
        separator = case targetSeparator =<< target of
          Just s | T.null s -> Nothing
          Just s -> Just s
          Nothing -> langSeparator l
    place l separator pos c items =
      let (cs, ce) = (fst (matchStart c), fst (matchEnd c))
          region = regionOf cs ce
          body = T.strip (matchText c)
          bracketed =
            maybe False (\(o, _) -> o `elem` ("([{" :: String)) (T.uncons body)
              && maybe False (\(_, z) -> z `elem` (")]}" :: String)) (T.unsnoc body)
              && (cs == ce || maybe False ((`elem` (")]}" :: String)) . fst) (T.uncons (T.strip (lineAt' ce))))
       in case (pos, items) of
            (Before, _) -> wholeLine "the target's start" (startsLine (matchStart c)) (located cs (indentAt cs) region)
            (After, _) -> wholeLine "the target's end" (endsLine (matchEnd c)) (located (ce + 1) (indentAt cs) region)
            -- An empty container is opened (spec §7.1): on one line, the line
            -- is split around it; spread over lines, the text goes before its
            -- closing line, indented like what is inside, or one level in.
            (_, [])
              -- Only a bracketed container (`()`, `{}`, `[]`) is opened; any
              -- other empty target (a whole file, say) is still refused.
              | not bracketed -> Left (miss "has no items to insert beside")
              -- "Empty" means no items matched; anything else inside (`true`,
              -- `..Default::default()`) would be lost or mis-joined, so only
              -- blank or comment-only containers are opened.
              | not (all (\x -> T.null x || langComment l `T.isPrefixOf` x) (map T.strip (T.lines (T.drop 1 (T.dropEnd 1 body))))) ->
                  Left (miss "has content that is not one of the target's items (add its kind to the target's `items`)")
              | cs == ce -> Right ((located cs (indentAt cs <> langIndent l) region) {locOpen = Just (OpenLine cs (snd (matchStart c)) (snd (matchEnd c)))})
              | otherwise ->
                  let inside = [indentAt i | i <- [cs + 1 .. ce - 1], not (T.null (T.strip (lineAt' i)))]
                   in Right ((located ce (case inside of (i : _) -> i; [] -> indentAt cs <> langIndent l) region) {locOpen = Just OpenLines})
            (p, _ : _)
              | p `elem` [StartOf, EndOf]
              , cs == ce
              , bracketed
              , Just sep <- separator
              -- every bit of code between the brackets is an item: what is
              -- not (a lambda, `*`, an operator) would be lost; refuse instead
              , covered sep (T.drop (snd (matchStart c) + 1) (T.take (snd (matchEnd c) - 1) (lineAt' cs))) (map matchText (sortOn matchStart items)) ->
                  Right ((located cs (indentAt cs) region) {locOpen = Just (Grow cs (snd (matchStart c)) (snd (matchEnd c)) (map (T.strip . matchText) (sortOn matchStart items)) sep (langLineLength l) (langIndent l) (p == StartOf))})
            (StartOf, _) ->
              let f = minimumBy (comparing matchStart) items
                  spacing = case sortOn matchStart items of
                    (a : b : _) | blankBetween a b -> BlankAfter
                    _ -> NoSpacing
               in wholeLine "the first item" (startsLine (matchStart f)) ((located (fst (matchStart f)) (indentAt (fst (matchStart f))) region) {locSpacing = spacing})
            (EndOf, _) ->
              let z = maximumBy (comparing matchEnd) items
                  spacing = case reverse (sortOn matchStart items) of
                    (b : a : _) | blankBetween a b -> BlankBefore
                    _ -> NoSpacing
               in wholeLine "the last item" (endsLine (matchEnd z)) ((located (fst (matchEnd z) + 1) (indentAt (fst (matchStart z))) region) {locSpacing = spacing})

-- | A target's container and its items: the container's direct children, top
-- level (as `end of` sees them). 'Right Nothing' when the container is absent;
-- 'Left' (why, a tool is missing) when the target is unknown, ambiguous or the
-- syntax tool fails. @prefix@ ("end of", "items of") only shapes suggestions.
itemsOfTarget :: Monad m => Syntax m -> Language -> Text -> [Text] -> Text -> m (Either (Text, Bool) (Maybe (Match, [Match])))
itemsOfTarget syntax l prefix ws content = case [(t, bs) | t <- langTargets l, Just bs <- [matchShape t ws]] of
  [] ->
    let suggestion = maybe "" (\s -> "; did you mean `" <> s <> "`?") (closest (prefix <> " " <> T.unwords ws) (map targetExample (langTargets l)))
     in pure (Left ("unknown target `" <> T.unwords ws <> "` for " <> langName l <> suggestion <> " (targets: " <> T.intercalate ", " (map targetPhrase (langTargets l)) <> ")", False))
  ((t, binds) : _) -> do
    let env = M.fromList [(k, VText v) | (k, v) <- binds]
    case (,) <$> renderRule env (targetContainer t) <*> renderRule env (targetItems t) of
      Left err -> pure (Left (describeTemplateError err, False))
      Right (containerRule, itemsRule) ->
        querySyntax syntax l containerRule content >>= \case
          Left f -> pure (Left (failure f))
          Right [] -> pure (Right Nothing)
          Right [c] -> do
            -- Items are the container's direct children (ast-grep's default
            -- `inside` checks only the parent), so branches of a nested case
            -- never count, even when their parent branch was filtered out as a
            -- catch-all.
            let within = object ["all" .= [itemsRule, object ["inside" .= containerRule]]]
            querySyntax syntax l within content <&> \case
              Left f -> Left (failure f)
              Right found -> Right (Just (c, topLevel (filter (inside c) found)))
          Right cs -> pure (Left ("matches " <> T.pack (show (length cs)) <> " places; it must match once", False))
  where
    failure f = (describeSyntaxFailure f, case f of SyntaxToolMissing _ -> True; SyntaxFailed _ -> False)
    -- the items query already asks for a strict ancestor; an item may span its
    -- container exactly (a Python function whose body is one `match`)
    inside c m = matchStart m >= matchStart c && matchEnd m <= matchEnd c
    topLevel ms = [m | m <- ms, not (any (\o -> o /= m && matchStart o <= matchStart m && matchEnd m <= matchEnd o) ms)]

-- | The container of a named target, as 0-based (start, end) lines (used by
-- `complete` edits' `where`, spec §5.6).
locateContainer :: Monad m => Syntax m -> Language -> [Text] -> Text -> m (Either Miss (Int, Int))
locateContainer syntax l ws content =
  locateContainers syntax l ws content <&> \case
    Right [c] -> Right c
    Right _ -> Left (Miss (T.unwords ws) "not found exactly once" False)
    Left miss -> Left miss

-- | Every container of a named target, as 0-based (start, end) lines, in
-- the file's order (a name two classes define: both).
locateContainers :: Monad m => Syntax m -> Language -> [Text] -> Text -> m (Either Miss [(Int, Int)])
locateContainers syntax l ws content = case [(t, bs) | t <- langTargets l, Just bs <- [matchShape t ws]] of
  [] -> pure (Left (Miss (T.unwords ws) "unknown target" False))
  ((t, binds) : _) -> case renderRule (M.fromList [(k, VText v) | (k, v) <- binds]) (targetContainer t) of
    Left err -> pure (Left (Miss (T.unwords ws) (describeTemplateError err) False))
    Right rule ->
      querySyntax syntax l rule content <&> \case
        Right cs -> Right [(fst (matchStart c), fst (matchEnd c)) | c <- cs]
        Left f -> Left (Miss (T.unwords ws) (describeSyntaxFailure f) True)

insertLines :: Located -> Text -> Text -> (Int, Text)
insertLines loc snippet content =
  let newline = if "\r\n" `T.isInfixOf` content then "\r\n" else "\n"
      ls = T.splitOn newline content
      new = [if T.null (T.strip l) then "" else locIndent loc <> l | l <- T.splitOn "\n" (placedText loc snippet)]
      i = min (locLine loc) (length ls)
   in case (grownLine loc snippet content, locOpen loc) of
        -- A one-line container grown: its line replaced.
        (Just (n, replacement), _) -> (n, T.intercalate newline (take (n - 1) ls ++ T.splitOn "\n" replacement ++ drop n ls))
        -- Split the container's line: up to the opening bracket, the text,
        -- then the closing bracket onwards at the line's own indentation.
        (_, Just (OpenLine l from to)) | l < length ls ->
          let line = ls !! l
              base = T.takeWhile (\c -> c == ' ' || c == '\t') line
           in (l + 2, T.intercalate newline (take l ls ++ [T.take (from + 1) line] ++ new ++ [base <> T.drop (to - 1) line] ++ drop (l + 1) ls))
        _ -> (i + 1, T.intercalate newline (take i ls ++ new ++ drop i ls))

-- | A 'Grow' site: the 1-based line the container is on, and what replaces
-- it: the line with the item inline when that fits, else the container
-- spread one item per line (the closing bracket and whatever followed it on
-- the last line). A separator the line already ended with is not doubled.
grownLine :: Located -> Text -> Text -> Maybe (Int, Text)
grownLine loc snippet content = case locOpen loc of
  Just g@Grow {} | growLine g < length ls ->
    let line = ls !! growLine g
        before = T.take (growOpen g + 1) line
        after = T.drop (growClose g - 1) line
        new = let t = T.strip snippet in T.strip (fromMaybe t (T.stripSuffix (growSep g) t))
        items = if growAtStart g then new : growItems g else growItems g ++ [new]
        inline = before <> T.intercalate (growSep g <> " ") items <> after
        base = T.takeWhile (\ch -> ch == ' ' || ch == '\t') line
        spread = [before] ++ [base <> growIndent g <> i <> growSep g | i <- items] ++ [base <> after]
     in Just (growLine g + 1, T.intercalate "\n" (if T.length inline <= growWidth g then [inline] else spread))
  _ -> Nothing
  where
    ls = T.splitOn "\n" (T.replace "\r\n" "\n" content)

-- | 'insertedText' at a located site: when the site opens an empty
-- container the text is its first item, so a leading `,` is dropped (a
-- generator written for lists with leading commas, Elm's style).
placedText :: Located -> Text -> Text
placedText loc snippet = insertedText (locSpacing loc) (maybe snippet (const (firstItem snippet)) (locOpen loc))

firstItem :: Text -> Text
firstItem snippet =
  let (lead, rest) = T.span (`elem` (" \t\n" :: String)) snippet
   in maybe snippet (\r -> lead <> T.stripStart r) (T.stripPrefix "," rest)

-- | The lines an insert adds, before the site's indentation: the text less
-- its common indentation (so text written at its absolute column is not
-- indented twice), plus the blank line 'Spacing' asks for unless the text
-- already has it. The plan shows this, so it matches what is written.
insertedText :: Spacing -> Text -> Text
insertedText spacing snippet =
  let body = dedentLines (T.splitOn "\n" (fromMaybe snippet (T.stripSuffix "\n" snippet)))
      blank = maybe False (T.null . T.strip)
      spaced = case spacing of
        BlankBefore | not (blank (listToMaybe body)) -> "" : body
        BlankAfter | not (blank (listToMaybe (reverse body))) -> body ++ [""]
        _ -> body
   in T.intercalate "\n" spaced
  where
    dedentLines ls =
      let lead l = T.length (T.takeWhile (\c -> c == ' ' || c == '\t') l)
          common = case [lead l | l <- ls, not (T.null (T.strip l))] of
            [] -> 0
            ns -> minimum ns
       in map (\l -> if T.null (T.strip l) then "" else T.drop common l) ls

-- | Is the snippet already where this anchor would put it? Checks the
-- anchor's region (a container or marker block) and the snippet-sized window
-- on each side of the insertion line: a previous `after` run left the text
-- just below it, a previous `before` run just above (spec §5.4). Commented-out
-- lines (a template's example hint) are left out of both.
presentAt :: Located -> Text -> Text -> Bool
presentAt loc snippet content =
  any present (nub [snippet, firstItem snippet])
  where
    -- A commented-out copy (a template's example hint) is not the text, so
    -- comment lines are compared on neither side (unless the text is only
    -- comments).
    present t
      | T.null (T.strip (uncommented t)) = alreadyPresent t (locRegion loc) || alreadyPresent t window
      | otherwise = alreadyPresent (uncommented t) (uncommented (locRegion loc)) || alreadyPresent (uncommented t) (uncommented window)
    uncommented
      | T.null (locComment loc) = id
      | otherwise = T.intercalate "\n" . filter (not . (locComment loc `T.isPrefixOf`) . T.stripStart) . T.splitOn "\n"
    ls = T.splitOn "\n" (T.replace "\r\n" "\n" content)
    k = max 1 (length (T.lines snippet))
    window = T.intercalate "\n" (take (2 * k) (drop (max 0 (locLine loc - k)) ls))

-- | Whitespace-insensitive "is this snippet already in the region?", on
-- whole tokens (spec §5.4).
alreadyPresent :: Text -> Text -> Bool
alreadyPresent snippet content
  -- A hole matches whatever fills it: the text around it, in order.
  | hasMarkers snippet = inOrder [n | Right x <- splitMarkers snippet, let n = normalise x, not (T.null n)] haystack
  | otherwise = T.null needle || any bounded (T.breakOnAll needle haystack)
  where
    inOrder [] _ = True
    inOrder (c : cs) h = case T.breakOn c h of
      (_, "") -> False
      (_, rest) -> inOrder cs (T.drop (T.length c) rest)
    normalise = T.unwords . T.words
    needle = normalise snippet
    haystack = normalise content
    bounded (before, rest) =
      not (joins (lastChar before) (firstChar needle))
        && not (joins (lastChar needle) (firstChar (T.drop (T.length needle) rest)))
    joins a b = maybe False isIdent a && maybe False isIdent b
    isIdent c = isAlphaNum c || c == '_'
    firstChar = fmap fst . T.uncons
    lastChar = fmap snd . T.unsnoc
