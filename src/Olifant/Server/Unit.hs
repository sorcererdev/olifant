-- | Internal to Olifant.Server: the unit at a line, the innermost
-- definition (a function, method or class) enclosing it, by the node kinds
-- the language's `[unit]` lists.
module Olifant.Server.Unit
  ( Unit (..)
  , unitAt
  , testUnitAt
  , testOrUnitAt
  , ownerHeader
  , isStubIn
  , Named (..)
  , unitsOf
  , unitAnswers
  , unitAround
  , unitNamed
  , unitLookup
  , unitLookupIn
  , unitsWithKinds
  , unitNameIn
  , fingerprint
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (Value, object, (.=))
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum, isSpace)
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Numeric (showHex)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language (ConjureDef (..), CtorShape (..), Language (..), Match (..), Syntax (..), TestDef (..), describeSyntaxFailure, regexName)
import Olifant.Provider (UnitAnswer (..), providedUnits)
import Olifant.Language.TestUnits (TestUnit (..), testCaseUnitAtIn)
import Olifant.Language.TreeSitter (definitionKinds, definitionLines)
import Text.Regex.TDFA (Regex, makeRegexM, matchTest)

-- | A unit (1-based lines): its name, its header line, its node's lines,
-- and its body's (the lines after the header; Nothing for a one-line def).
data Unit = Unit
  { unitName :: Text
  , unitDef :: Int
  , unitLines :: (Int, Int)
  , unitBody :: Maybe (Int, Int)
  }
  deriving stock (Eq, Show)

-- | The unit enclosing LINE (1-based) of TEXT: the innermost node of the
-- language's `[unit]` kinds there, named by `[unit] name` on its first line
-- that has one (a decorator's is the def below it); its body starts after
-- the first line that opens a block (`opens_block`, its comment cut).
-- Left is one sentence on why there is none.
unitAt :: Syntax IO -> Language -> Maybe FilePath -> Text -> Int -> IO (Either Text Unit)
unitAt syntax lang file text line = case langUnitProvider lang of
  -- a provider (or, with no [unit] kinds, the definitions a linked
  -- grammar's queries capture): the innermost answer holding the line, its
  -- body as a rule's
  _ | byAnswers ->
    unitAnswers syntax lang file text <&> \case
      Left why -> Left why
      Right us -> case sortOn (\u -> (Down (uaLine u), uaEnd u)) [u | u <- us, uaLine u <= line, line <= uaEnd u] of
        u : _ -> let d = defLine lang ls u in Right (Unit (uaName u) d (uaLine u, uaEnd u) (bodyOf lang ls d (uaEnd u)))
        [] -> Left noUnit
  _
    | null (langUnitKinds lang) -> pure (Left (noKinds lang))
    | otherwise ->
        querySyntax syntax lang (kindsRule lang) text <&> \case
          Left f -> Left (describeSyntaxFailure f)
          Right ms -> maybe (Left noUnit) Right (pick [(a + 1, z + 1) | Match (a, _) (z, _) _ _ <- ms])
  where
    byAnswers = isJust (langUnitProvider lang) || (null (langUnitKinds lang) && isJust (definitionLines lang text))
    noUnit = "no function or class at line " <> tshow line
    ls = T.splitOn "\n" text
    lineAt i = fromMaybe "" (listToMaybe (drop (i - 1) ls))
    -- the innermost around the line: the latest start, then the earliest end
    pick spans = listToMaybe (sortOn (\(a, z) -> (Down a, z)) [sp | sp@(a, z) <- spans, a <= line, line <= z]) >>= unitOf
    unitOf (a, z) = do
      (d, n) <- listToMaybe [(i, n) | i <- [a .. z], Just n <- [unitNameIn lang (lineAt i)]]
      pure (Unit n d (a, z) (bodyOf lang ls d z))

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Why a language without `[unit] kinds` (or a provider) has no units.
noKinds :: Language -> Text
noKinds lang = langName lang <> " has no [unit] kinds: Olifant cannot find a function or class in it"

-- | The ast-grep rule for the language's `[unit] kinds`.
kindsRule :: Language -> Value
kindsRule lang = object ["any" .= [object ["kind" .= k] | k <- langUnitKinds lang]]

-- | The units slot's answer for TEXT (at FILE, when the caller knows it).
-- By rule: each match of `[unit] kinds` (1-based lines), named by
-- 'unitNameIn' on the first line of its span that has a name (a match with
-- none is no unit), in order of start, the longer first. By provider: the
-- program's answer. 'unitsOf' folds either the same way.
unitAnswers :: Syntax IO -> Language -> Maybe FilePath -> Text -> IO (Either Text [UnitAnswer])
unitAnswers syntax lang file text = case langUnitProvider lang of
  Just p -> providedUnits p file text
  Nothing
    -- no [unit] kinds and a linked grammar: every function and class-like
    -- definition its queries capture, named by its @name, decorators in
    | null (langUnitKinds lang)
    , Just ds <- definitionKinds lang text ->
        pure (Right [UnitAnswer n a z (Just k) | (n, k, _, (a, z)) <- sortOn (\(_, _, _, (a, z)) -> (a, Down z)) ds])
    | null (langUnitKinds lang) -> pure (Left (noKinds lang))
    | otherwise ->
        querySyntax syntax lang (kindsRule lang) text <&> \case
          Left f -> Left (describeSyntaxFailure f)
          Right ms -> Right (concatMap named (sortOn (\(a, z) -> (a, Down z)) [(a + 1, z + 1) | Match (a, _) (z, _) _ _ <- ms]))
  where
    ls = T.splitOn "\n" text
    lineAt i = fromMaybe "" (listToMaybe (drop (i - 1) ls))
    named (a, z) = [UnitAnswer n a z Nothing | n <- take 1 [n | i <- [a .. z], Just n <- [unitNameIn lang (lineAt i)]]]

-- | A unit's definition line: the first of its lines with a name by
-- `[unit] name` (a decorator's unit is the def below it), else its first.
defLine :: Language -> [Text] -> UnitAnswer -> Int
defLine lang ls u = fromMaybe (uaLine u) (listToMaybe [i | i <- [uaLine u .. uaEnd u], isJust (unitNameIn lang (lineAt i))])
  where
    lineAt i = fromMaybe "" (listToMaybe (drop (i - 1) ls))

-- | The test case at LINE (1-based) of a test file in LANG as a unit (by
-- `[test] unit`: hspec's `it`, which no `[unit]` kind is): named by its
-- own name, its def its first line below its `above` lines; its body the
-- block's lines indented deeper than the def (Rust's closing brace is not
-- in it).
testUnitAt :: Language -> Maybe FilePath -> Text -> Int -> Maybe Unit
testUnitAt lang file text line = do
  u <- testCaseUnitAtIn lang file text line
  n <- listToMaybe (reverse (tuPath u))
  let (a, z) = tuLines u
      isAbove l = maybe False (\rx -> matchTest rx (T.unpack l)) (langTest lang >>= testAbove >>= \re -> makeRegexM (T.unpack re) :: Maybe Regex)
      d = fromMaybe a (listToMaybe [i | i <- [a .. z], not (isAbove (lineAt i))])
      -- the block: through its last line deeper than the def (a closing brace is not in it)
      deeper i = not (T.null (T.strip (lineAt i))) && indent (lineAt i) > indent (lineAt d)
      body = bodyOf lang ls d z >>= \(c, _) -> case [i | i <- [c .. z], deeper i] of
        [] -> Nothing
        is -> Just (c, last is)
  pure (Unit n d (a, z) body)
  where
    indent = T.length . T.takeWhile isSpace
    ls = T.splitOn "\n" text
    lineAt i = fromMaybe "" (listToMaybe (drop (i - 1) ls))

-- | The test at LINE of a test file as a unit ('testUnitAt'), else the
-- `[unit]` there ('unitAt'): what a re-conjure takes for a test's hole.
testOrUnitAt :: Syntax IO -> Language -> Maybe FilePath -> Text -> Int -> IO (Either Text Unit)
testOrUnitAt syntax lang file text line = maybe (unitAt syntax lang file text line) (pure . Right) (testUnitAt lang file text line)

-- | A unit's body in lines LS: from the line after the first from D (its
-- def) that opens a block (`opens_block`, its comment cut) through Z.
bodyOf :: Language -> [Text] -> Int -> Int -> Maybe (Int, Int)
bodyOf lang ls d z = opener >>= \c -> if c + 1 <= z then Just (c + 1, z) else Nothing
  where
    lineAt i = fromMaybe "" (listToMaybe (drop (i - 1) ls))
    opener = listToMaybe [i | i <- [d .. z], opens (fst (T.breakOn (" " <> langComment lang) (lineAt i)))]
    opens = case langOpensBlock lang >>= \re -> makeRegexM (T.unpack re) :: Maybe Regex of
      Just rx -> matchTest rx . T.unpack . T.stripEnd
      Nothing -> const True

-- | The header of the unit at LINE (1-based) of a module in LANG: for a method, the line
-- of what owns it (`[conjure.constructor] owner`: its class) and its constructor's
-- (`start`) through the line that opens its block (`opens_block`); for a free function, its
-- own line (named by `[unit] name`); empty when neither.
ownerHeader :: Language -> Text -> Int -> Text
ownerHeader lang text line = case ([c | c <- cands, isOwner (snd c)], [d | d <- cands, not (isOwner (snd d))]) of
  ((ci, cl) : _, _ : _) -> T.intercalate "\n" (cl : initDef ci cl)
  (_, (_, d) : _) -> T.stripEnd d
  _ -> ""
  where
    ct = langConjure lang >>= cjConstructor
    matcherOf re = maybe (const False) (\rx -> matchTest rx . T.unpack) (makeRegexM (T.unpack re) :: Maybe Regex)
    isOwner = maybe (const False) (\c -> matcherOf (ctOwner c) . T.stripStart) ct
    isStart = maybe (const False) (\c -> matcherOf (ctStart c) . T.stripStart) ct
    opens = maybe (const False) (\re -> matcherOf re . T.stripEnd) (langOpensBlock lang)
    isHead l = isOwner l || isJust (unitNameIn lang l)
    cands = [(i, l) | (i, l) <- reverse (take line ls), isHead l, enclosing i l]
    ls = zip [1 :: Int ..] (T.lines text)
    indentOf = T.length . T.takeWhile (== ' ')
    blank = T.null . T.strip
    enclosing i l =
      i == line
        || all (\(_, x) -> blank x || indentOf x > indentOf l) [x | x@(j, _) <- ls, j > i, j <= line]
    initDef ci cl =
      let body = takeWhile (\(_, x) -> blank x || indentOf x > indentOf cl) (drop ci ls)
       in case dropWhile (not . isStart . snd) body of
            [] -> []
            found -> [T.intercalate "\n" (map snd (upToOpen found))]
    upToOpen [] = []
    upToOpen (x@(_, l) : r)
      | opens (fst (T.breakOn (langComment lang) l)) = [x]
      | otherwise = x : upToOpen r

-- | A unit of a file by name (1-based, inclusive lines, like 'Unit'): its
-- name qualified one level by containment (`Outer.Inner`), its lines, and
-- the line of its definition (after any decorators).
data Named = Named {namedName :: Text, namedLines :: (Int, Int), namedDef :: Int}
  deriving stock (Eq, Show)

-- | Every unit of TEXT (each match of the language's `[unit]` kinds), named
-- by 'unitNameIn' on the first line of its span that has a name. A match
-- with another's span, or with the name and end of the match around it (a
-- decorated def), is the same unit. With `[unit] join`, units of one name at
-- one indentation separated only by blank lines (or touching) are one: a
-- Haskell signature and its equations.
unitsOf :: Syntax IO -> Language -> Maybe FilePath -> Text -> IO (Either Text [Named])
unitsOf syntax lang file text = fmap (map fst) <$> unitsWithKinds syntax lang file text

-- | 'unitsOf', each unit with its answer's kind (a linked grammar's
-- capture: `definition.impl`, …), when its span is an answer's.
unitsWithKinds :: Syntax IO -> Language -> Maybe FilePath -> Text -> IO (Either Text [(Named, Maybe Text)])
unitsWithKinds syntax lang file text =
  unitAnswers syntax lang file text <&> fmap (\us -> let kinds = M.fromList [((uaLine u, uaEnd u), k) | u <- us, Just k <- [uaKind u]] in map (\n -> (n, M.lookup (namedLines n) kinds)) (named us))
  where
    named us = qualify (joined (dedupe [((uaLine u, uaEnd u), defLine lang ls u, uaName u) | u <- sortOn (\u -> (uaLine u, Down (uaEnd u))) us]))
    ls = T.splitOn "\n" text
    lineAt i = fromMaybe "" (listToMaybe (drop (i - 1) ls))
    -- (span, def line, bare name)
    dedupe = foldl keep []
    keep acc u@(sp@(_, z), _, n) = case [() | (sp'@(_, z'), _, n') <- acc, sp' == sp || (n' == n && z' == z && contains sp' sp)] of
      [] -> acc ++ [u]
      _ -> acc
    contains (a, z) (b, y) = a <= b && y <= z
    joined us
      | langUnitJoin lang = dedupe (foldl join [] us)
      | otherwise = us
    -- a unit joins the latest earlier one of its name and indentation that ends
    -- before it with only blank lines between
    join acc u@((a, z), _, n) = case [i | (i, ((_, e), d', n')) <- reverse (zip [0 :: Int ..] acc), n' == n, e < a, indentAt d' == indentAt a, all (T.null . T.strip . lineAt) [e + 1 .. a - 1]] of
      i : _ -> [if j == i then ((a', z), d', n') else x | (j, x@((a', _), d', n')) <- zip [0 ..] acc]
      [] -> acc ++ [u]
    indentAt i = T.length (T.takeWhile (== ' ') (lineAt i))
    qualify us =
      [ Named (maybe n (<> "." <> n) (parentOf sp)) sp d
      | (sp, d, n) <- us
      , let parentOf s = listToMaybe [pn | (ps, _, pn) <- sortOn (\((a, z), _, _) -> (Down a, z)) us, ps /= s, contains ps s]
      ]

-- | The innermost unit (by 'unitsOf') whose lines hold LINE (1-based) of
-- TEXT: a decorator's line is its def's. Left is one sentence on why there
-- is none.
unitAround :: Syntax IO -> Language -> Maybe FilePath -> Text -> Int -> IO (Either Text Named)
unitAround syntax lang file text line =
  unitsOf syntax lang file text <&> \case
    Left why -> Left why
    Right ns -> case sortOn (\n -> (Down (fst (namedLines n)), snd (namedLines n))) [n | n <- ns, let (a, z) = namedLines n, a <= line, line <= z] of
      n : _ -> Right n
      [] -> Left ("no function or class at line " <> tshow line <> ": put the cursor in one to stub its test")

-- | The unit called NAME: the exact qualified name (the earliest of several),
-- or a bare name that is the last part of exactly one qualified name.
unitNamed :: Text -> [Named] -> Maybe Named
unitNamed name = either (const Nothing) Just . unitLookup name

-- | 'unitLookup' over units with their kinds: a type and its inherent impl
-- sharing a name (a `definition.impl` beside a unit of another kind) are
-- one thing to an anchor, the type, as before; any other shared name is
-- ambiguous.
unitLookupIn :: Text -> [(Named, Maybe Text)] -> Either Text Named
unitLookupIn name ks = case [n | (n, k) <- ks, namedName n == name, k /= Just "definition.impl"] of
  [n] | any (\(m, k) -> namedName m == name && k == Just "definition.impl") ks -> Right n
  _ -> unitLookup name (map fst ks)

-- | 'unitNamed', with why there is none: no such unit, or a name several
-- units have (never the first of them silently).
unitLookup :: Text -> [Named] -> Either Text Named
unitLookup name ns = case sortOn namedLines [n | n <- ns, namedName n == name] of
  [n] -> Right n
  dups@(_ : _) -> Left ("`" <> name <> "` names " <> tshow (length dups) <> " units (lines " <> T.intercalate ", " [tshow (fst (namedLines n)) | n <- dups] <> ")")
  [] -> case [n | n <- ns, ("." <> name) `T.isSuffixOf` namedName n] of
    [n] -> Right n
    [] -> Left ("no unit `" <> name <> "`")
    several -> Left ("`" <> name <> "` names " <> tshow (length several) <> " units (" <> T.intercalate ", " (map namedName several) <> ")")

-- | The name on a unit's line by the language's `[unit] name` expressions:
-- the first that matches gives its last group (the whole match when it has
-- none), stripped; Nothing when none matches or the name is empty.
unitNameIn :: Language -> Text -> Maybe Text
unitNameIn lang = \line -> listToMaybe (mapMaybe ($ line) namers)
  where
    namers = map regexName (langUnitNames lang)

-- | Seven lowercase hex digits naming the lines (1-based, inclusive) of TEXT:
-- FNV-1a 64 over them trimmed, blanks dropped, joined by newlines, except
-- where a formatter reflows ('reflowed'), so formatting never changes it.
fingerprint :: Text -> (Int, Int) -> Text
fingerprint text (a, z) = T.pack (take 7 (pad (showHex (BS.foldl' step 14695981039346656037 (encodeUtf8 body)) "")))
  where
    body = reflowed (filter (not . T.null) (map T.strip (take (z - a + 1) (drop (a - 1) (T.splitOn "\n" text)))))
    step h b = (h `xor` fromIntegral b) * 1099511628211 :: Word64
    pad s = replicate (16 - length s) '0' ++ s

-- | Trimmed lines joined as one would be had a formatter not split it:
-- after an opening bracket and before a closing one with nothing between
-- (a magic trailing comma dropped), after a comma with one space, before a
-- leading comma with none; every other break is a newline. Where any break
-- was joined, a space just inside a bracket goes too (leading-comma style
-- opens `[ a`). A body with no such break joins as before.
reflowed :: [Text] -> Text
reflowed = \case
  [] -> ""
  l : ls -> go False l ls
  where
    go joined acc [] = if joined then tight acc else acc
    go joined acc (n : rest)
      | Just (c, _) <- T.uncons n, c `elem` (")]}" :: String) = go True (fromMaybe acc (T.stripSuffix "," acc) <> n) rest
      | Just (_, c) <- T.unsnoc acc, c `elem` ("([{" :: String) = go True (acc <> n) rest
      | "," `T.isSuffixOf` acc = go True (acc <> " " <> n) rest
      | "," `T.isPrefixOf` n = go True (acc <> n) rest
      | otherwise = go joined (acc <> "\n" <> n) rest
    tight t = foldr (\(from, to) -> T.replace from to) t ([(o <> " ", o) | o <- ["(", "[", "{"]] ++ [(" " <> c, c) | c <- [")", "]", "}"]])

-- | Whether a body's LINES are only a stub in LANG: each non-blank line,
-- stripped, matches one of its `stub_lines`, is a comment, or holds a hole
-- (@olifant:@).
isStubIn :: Language -> [Text] -> Bool
isStubIn lang = all stubLine . filter (not . T.null) . map T.strip
  where
    stubs = [rx | re <- langStubLines lang, Just rx <- [makeRegexM (T.unpack re) :: Maybe Regex]]
    stubLine l = any (`matchTest` T.unpack l) stubs || "olifant:" `T.isInfixOf` l || langComment lang `T.isPrefixOf` l
