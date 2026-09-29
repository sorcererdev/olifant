-- | Resolving `at` anchors in a file's text (spec §5.2, §5.4).
module Olifant.Anchor.Resolve
  ( RenderedAnchor (..)
  , Located (..)
  , Miss (..)
  , resolveLayers
  , locateContainer
  , insertLines
  , alreadyPresent
  , describeMisses
  ) where

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Char (isAlphaNum)
import Data.List (maximumBy, minimumBy)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Anchor.Phrase
import Olifant.Language
import Olifant.Template (Value (VText), describeTemplateError)
import Text.Regex.TDFA (Regex, makeRegexM, matchTest)

data RenderedAnchor = PhraseAnchor Text | RuleAnchor Aeson.Value Position
  deriving stock (Eq, Show)

data Located = Located {locLine :: Int, locIndent :: Text, locRegion :: Text, locUsed :: Text}
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
        Right [m] -> pure (Right (around m pos))
        Right ms -> pure (Left (miss ("the rule matches " <> count ms <> " places; it must match once")))
  PhraseAnchor p -> case parsePhrase p of
    Left err -> pure (Left (miss err))
    Right (Phrase pos (MarkerTarget m)) -> pure (markerAt pos m)
    Right (Phrase pos (LineTarget which re)) -> pure (lineAt pos which re)
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
    located line indent region = Located line indent region used
    count = T.pack . show . length
    around m pos =
      let (s, e) = (fst (matchStart m), fst (matchEnd m))
          line = if pos `elem` [StartOf, Before] then s else e + 1
       in located line (indentAt s) (regionOf s e)
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
    named l pos ws = case [(t, bs) | t <- langTargets l, Just bs <- [matchShape t ws]] of
      [] ->
        let examples = map targetExample (langTargets l)
            suggestion = maybe "" (\s -> "; did you mean `" <> s <> "`?") (closest (positionWords pos <> " " <> T.unwords ws) examples)
         in pure (Left (miss ("unknown target `" <> T.unwords ws <> "` for " <> langName l <> suggestion <> " (targets: " <> T.intercalate ", " (map targetPhrase (langTargets l)) <> ")")))
      ((t, binds) : _) -> do
        let env = M.fromList [(k, VText v) | (k, v) <- binds]
        case (,) <$> renderRule env (targetContainer t) <*> renderRule env (targetItems t) of
          Left err -> pure (Left (miss (describeTemplateError err)))
          Right (containerRule, itemsRule) ->
            querySyntax syntax l containerRule content >>= \case
              Left f -> pure (Left (toolMiss f))
              Right [c] -> do
                -- Items are the container's direct children (ast-grep's default
                -- `inside` checks only the parent), so branches of a nested
                -- case never count, even when their parent branch was filtered
                -- out as a catch-all.
                let within = object ["all" .= [itemsRule, object ["inside" .= containerRule]]]
                querySyntax syntax l within content >>= \case
                  Left f -> pure (Left (toolMiss f))
                  Right found -> pure (place pos c (topLevel (filter (inside c) found)))
              Right [] -> pure (Left (miss "not found"))
              Right cs -> pure (Left (miss ("matches " <> count cs <> " places; it must match once")))
    place pos c items =
      let (cs, ce) = (fst (matchStart c), fst (matchEnd c))
          region = regionOf cs ce
       in case (pos, items) of
            (Before, _) -> Right (located cs (indentAt cs) region)
            (After, _) -> Right (located (ce + 1) (indentAt cs) region)
            (_, []) -> Left (miss "has no items to insert beside")
            (StartOf, _) -> let f = minimumBy (comparing matchStart) items in Right (located (fst (matchStart f)) (indentAt (fst (matchStart f))) region)
            (EndOf, _) -> let z = maximumBy (comparing matchEnd) items in Right (located (fst (matchEnd z) + 1) (indentAt (fst (matchStart z))) region)
    inside c m = matchStart m >= matchStart c && matchEnd m <= matchEnd c && m /= c
    topLevel ms = [m | m <- ms, not (any (\o -> o /= m && matchStart o <= matchStart m && matchEnd m <= matchEnd o) ms)]

-- | The container of a named target, as 0-based (start, end) lines (used by
-- `complete` edits' `where`, spec §5.6).
locateContainer :: Monad m => Syntax m -> Language -> [Text] -> Text -> m (Either Miss (Int, Int))
locateContainer syntax l ws content = case [(t, bs) | t <- langTargets l, Just bs <- [matchShape t ws]] of
  [] -> pure (Left (Miss (T.unwords ws) "unknown target" False))
  ((t, binds) : _) -> case renderRule (M.fromList [(k, VText v) | (k, v) <- binds]) (targetContainer t) of
    Left err -> pure (Left (Miss (T.unwords ws) (describeTemplateError err) False))
    Right rule ->
      querySyntax syntax l rule content >>= \case
        Right [c] -> pure (Right (fst (matchStart c), fst (matchEnd c)))
        Right _ -> pure (Left (Miss (T.unwords ws) "not found exactly once" False))
        Left f -> pure (Left (Miss (T.unwords ws) (describeSyntaxFailure f) True))

insertLines :: Located -> Text -> Text -> (Int, Text)
insertLines loc snippet content =
  let newline = if "\r\n" `T.isInfixOf` content then "\r\n" else "\n"
      ls = T.splitOn newline content
      new = [if T.null (T.strip l) then "" else locIndent loc <> l | l <- T.splitOn "\n" (dropFinalNewline snippet)]
      i = min (locLine loc) (length ls)
   in (i + 1, T.intercalate newline (take i ls ++ new ++ drop i ls))
  where
    dropFinalNewline t = fromMaybe t (T.stripSuffix "\n" t)

-- | Whitespace-insensitive "is this snippet already in the region?", on
-- whole tokens (spec §5.4). Moved from Generation.Marker.
alreadyPresent :: Text -> Text -> Bool
alreadyPresent snippet content = T.null needle || any bounded (T.breakOnAll needle haystack)
  where
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
