-- | Internal to Olifant.Server: the conjure's scan of a test file (a fake a test builds and
-- never passes in), and the wire's inputs derived from the service (its ports, its
-- siblings' @__init__@), with Jev's question for a port no rule names
-- (conjure-pause-context-design.md §3.1, §3.2). Pure: the reading is in "Olifant.Server.Conjure".
module Olifant.Server.ConjureScan
  ( Fake (..)
  , unpassedFake
  , fakeCalls
  , actCall
  , helperOf
  , handFake
  , Port (..)
  , portsIn
  , derivePort
  , ctorParams
  , deriveParam
  , takesPort
  , conjurePortQuestion
  , fakeIn
  , handFakeIn
  , derivePortIn
  , ctorParamsIn
  , takesPortIn
  , conjurePortGate
  , portFrom
  , portCandidates
  , missingMemberIn
  , callLines
  , snippetLine
  , signatureTemplate
  , readSignature
  , methodText
  , portEnd
  , advisedIn
  , blankAt
  , siblingsOf
  , servicePaths
  , groups
  , balanced
  , occurrencesWith
  , snake
  ) where

import Data.Char (isAlphaNum, isSpace, isUpper, toLower)
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Decide (Answer (..), Question, choice, confident)
import Olifant.Holes (Marker (..), hasMarkers, splitMarkers)
import Olifant.Language (Advice (..), CheckDef (..), ConjureDef (..), CtorShape (..), MissingMember (..), FakeShape (..), Language (..), PortShape (..), TypesDef (..), isTestFile, regexName)
import Data.Array qualified
import System.FilePath (splitDirectories)
import Text.Regex.TDFA (AllTextSubmatches (..), Regex, makeRegexM, matchAll, matchM, matchTest, (=~))

-- | A fake a test builds: the name it is bound to and the class (or @Mock@) called.
data Fake = Fake {fkVariable :: Text, fkClass :: Text}
  deriving stock (Eq, Show)

-- | The first fake bound in a test's body (SRC, its source: its own line, then its body)
-- that is never an argument of a call in that body (positional or keyword); none when every
-- fake is passed.
unpassedFake :: FakeShape -> Text -> Maybe Fake
unpassedFake fs src = do
  let body = T.intercalate "\n" (drop 1 (T.lines src))
  listToMaybe [f | f <- mapMaybe (bound fs) (T.lines body), not (passedIn (isJust (fsKeyword fs)) body (fkVariable f))]

-- | A line binding a fake by the shape's `bound` (the variable, the class called), the class
-- one of its `classes` or longer than one of its `prefixes` that starts it.
bound :: FakeShape -> Text -> Maybe Fake
bound fs l = do
  [var, cls] <- take 2 . drop 1 <$> groups (fsBound fs) l
  if T.null var || not (fakeClass cls) then Nothing else Just (Fake var cls)
  where
    fakeClass c = c `elem` fsClasses fs || any (\p -> p `T.isPrefixOf` c && T.length c > T.length p) (fsPrefixes fs)

-- | The groups of RE's match in a line (the whole match first); none when it does not match.
groups :: Text -> Text -> Maybe [Text]
groups re l = do
  rx <- makeRegexM (T.unpack re) :: Maybe Regex
  AllTextSubmatches gs <- matchM rx (T.unpack l) :: Maybe (AllTextSubmatches [] String)
  pure (map T.pack gs)

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || c == '_'

-- | VAR is an argument of a call in BODY: after @(@ or @,@ (and, when KEYWORDS, an optional
-- @name=@), before @)@ or @,@.
passedIn :: Bool -> Text -> Text -> Bool
passedIn keywords body var = any argAt (occurrences var body)
  where
    argAt (before, after) =
      let b = T.stripEnd before
          a = T.stripStart after
          opens t = maybe False (\(_, c) -> c `elem` ['(', ',']) (T.unsnoc (T.stripEnd t))
          keyword = case T.unsnoc b of
            Just (b', '=') | keywords, not ("=" `T.isSuffixOf` b'), not (any (`T.isSuffixOf` b') ["!", "<", ">"]) ->
              let b'' = T.stripEnd b'
                  name = T.takeWhileEnd isIdent b''
               in not (T.null name) && opens (T.dropEnd (T.length name) b'')
            _ -> False
       in (opens b || keyword) && maybe False ((`elem` [')', ',']) . fst) (T.uncons a)

-- | Each whole-word occurrence of VAR in T (not an attribute): the text before and after it.
occurrences :: Text -> Text -> [(Text, Text)]
occurrences = occurrencesWith False

-- | 'occurrences', attributes too when DOTTED.
occurrencesWith :: Bool -> Text -> Text -> [(Text, Text)]
occurrencesWith dotted var t = [(b, a) | (b, a) <- splits, whole b a]
  where
    splits = [(T.dropEnd (T.length var) b, a) | (b, a) <- breaks t]
    breaks x = case T.breakOn var x of
      (_, r) | T.null r -> []
      (b, r) -> let a = T.drop (T.length var) r in (b <> var, a) : [(b <> var <> b', a') | (b', a') <- breaks a]
    whole b a = maybe True (\(_, c) -> not (isIdent c) && (dotted || c /= '.')) (T.unsnoc b) && maybe True (\(c, _) -> not (isIdent c)) (T.uncons a)

-- | The attributes a test's SOURCE uses on the fake (@var.x@), unique, in order.
fakeCalls :: Text -> Fake -> [Text]
fakeCalls src f = nub [n | (_, a) <- occurrences (fkVariable f) src, Just r <- [T.stripPrefix "." a], let n = T.takeWhile isIdent r, not (T.null n)]

-- | The first call of METHOD in a test's SOURCE, its parentheses balanced.
actCall :: Text -> Text -> Maybe Text
actCall method src = listToMaybe [method <> "(" <> args | (_, a) <- occurrencesWith True method src, Just r <- [T.stripPrefix "(" a], Just args <- [balanced r]]

-- | The text up to and with the @)@ closing an open @(@.
balanced :: Text -> Maybe Text
balanced = go (1 :: Int) ""
  where
    go depth acc t = case T.uncons t of
      Nothing -> Nothing
      Just (c, rest)
        | c == '(' -> go (depth + 1) (T.snoc acc c) rest
        | c == ')', depth == 1 -> Just (T.snoc acc c)
        | c == ')' -> go (depth - 1) (T.snoc acc c) rest
        | otherwise -> go depth (T.snoc acc c) rest

-- | The callee of @x = callee(…)@ in a test's SOURCE, where the test later calls @x.METHOD(@.
helperOf :: Text -> Text -> Maybe Text
helperOf method src = listToMaybe [callee | (i, l) <- zip [0 :: Int ..] ls, Just (x, callee) <- [assigned l], any (T.isInfixOf (x <> "." <> method <> "(")) (drop (i + 1) ls)]
  where
    ls = T.lines src
    assigned l = do
      let (x, r1) = T.span isIdent (T.stripStart l)
      r2 <- T.stripPrefix "=" (T.stripStart r1)
      if "=" `T.isPrefixOf` r2 then Nothing else Just ()
      let (callee, r3) = T.span isIdent (T.stripStart r2)
      if T.null x || T.null callee || not ("(" `T.isPrefixOf` r3) then Nothing else Just (x, callee)

-- | A test's source SRC (its own line, then its body) with its fake VAR handed to its first
-- call of HELPER as PARAM, by the shape's `keyword` (@{= param =}={= var =}@: @command()@
-- becomes @command(bookings=repository)@, @command(x)@ becomes
-- @command(x, bookings=repository)@); none without a `keyword`, when its body has no such call
-- on one line, or the call already has the fake.
handFake :: FakeShape -> Text -> Text -> Text -> Text -> Maybe Text
handFake fs helper param var src = do
  kw <- fsKeyword fs
  let arg = T.replace "{= param =}" param (T.replace "{= var =}" var kw)
  (i, l') <- listToMaybe [(i, l') | (i, l) <- drop 1 numbered, Just l' <- [handIn arg l]]
  pure (T.intercalate "\n" [if j == i then l' else l | (j, l) <- numbered])
  where
    numbered = zip [0 :: Int ..] (T.splitOn "\n" src)
    handIn arg l =
      listToMaybe
        [ b <> helper <> "(" <> (if T.null (T.strip inner) then "" else inner <> ", ") <> arg <> ")" <> T.drop (T.length args) r
        | (b, a) <- occurrences helper l
        , Just r <- [T.stripPrefix "(" a]
        , Just args <- [balanced r]
        , let inner = T.dropEnd 1 args
        , not (passedIn True ("(" <> args) var)
        ]

-- | A service's port: its name and its method lines.
data Port = Port {ptName :: Text, ptMethods :: [Text]}
  deriving stock (Eq, Show)

-- | Each port of FILES by the shape (a line its `start` matches, group 1 the name) with its
-- block's lines (stripped) that its `method` matches, sorted by name. A block is the lines
-- after the start up to the next one at column 0.
portsIn :: PortShape -> [(FilePath, Text)] -> [Port]
portsIn ps files = sortOn ptName [p | (_, t) <- files, p <- ports (T.lines t)]
  where
    isMethod = maybe (const False) (\rx -> matchTest rx . T.unpack) (makeRegexM (T.unpack (psMethod ps)) :: Maybe Regex)
    ports = \case
      [] -> []
      l : rest
        | Just (_ : name : _) <- groups (psStart ps) l
        , not (T.null name) ->
            let (body, after) = break topLevel rest
             in Port name [T.strip d | d <- body, isMethod (T.strip d)] : ports after
        | otherwise -> ports rest
    topLevel l = not (T.null l) && not (isSpace (T.head l))

-- | The port a fake stands for: the fake's class less @Fake@/@InMemory@/@Stub@ names one; the
-- variable's last word is exactly one port's last word; the service's only port; else none.
derivePort :: FakeShape -> Fake -> [Port] -> Maybe Text
derivePort fs f ports
  | n : _ <- [n | p <- fsPrefixes fs, Just n <- [T.stripPrefix p (fkClass f)], n `elem` names] = Just n
  | [n] <- [n | n <- names, lastWord (snake n) == lastWord (T.toLower (fkVariable f))] = Just n
  | [n] <- names = Just n
  | otherwise = Nothing
  where
    names = map ptName ports
    lastWord = snd . T.breakOnEnd "_"

-- | A class name in snake case.
snake :: Text -> Text
snake = T.pack . go . T.unpack
  where
    go = \case
      [] -> []
      c : cs -> toLower c : concatMap (\x -> if isUpper x then ['_', toLower x] else [x]) cs

-- | Each constructor of a module's TEXT by the shape (its parameters after a match of its
-- `start`, to the bracket closing the one it opens): the (param, Type) pairs its `param`
-- matches (group 1 the name, the last non-empty group the type, as `[unit] annotation`
-- reads it: @Arc<dyn X>@ is @X@; the rest, @self@ or an untyped one, left out).
ctorParams :: CtorShape -> Text -> [(Text, Text)]
ctorParams ct text = concat [params a | r <- starts, Just a <- [balanced r]]
  where
    starts = case makeRegexM (T.unpack (ctStart ct)) :: Maybe Regex of
      Nothing -> []
      Just rx -> [T.pack (drop (o + n) s) | let s = T.unpack text, (o, n) <- map (\m -> m Data.Array.! 0) (matchAll rx s)]
    params a = mapMaybe param (splitTop (T.dropEnd 1 a))
    param p = case groups (ctParam ct) (T.strip p) of
      Just (_ : n : rest) | not (T.null n), t : _ <- reverse (filter (not . T.null) rest) -> Just (n, T.strip t)
      _ -> Nothing
    splitTop = go (0 :: Int) ""
    go depth acc t = case T.uncons t of
      Nothing -> [acc]
      Just (c, rest)
        | c == ',', depth == 0 -> acc : go depth "" rest
        | c `elem` ['(', '['] -> go (depth + 1) (T.snoc acc c) rest
        | c `elem` [')', ']'] -> go (depth - 1) (T.snoc acc c) rest
        | otherwise -> go depth (T.snoc acc c) rest

-- | The wire's param for PORT: the commonest name the SIBLINGS' @__init__@s give it (a tie:
-- the fake's variable when among them, else the first by name); none: the variable.
deriveParam :: Text -> [[(Text, Text)]] -> Fake -> Text
deriveParam port siblings f = case M.toList counts of
  [] -> fkVariable f
  cs ->
    let top = maximum (map snd cs)
        tied = [n | (n, k) <- cs, k == top]
     in if fkVariable f `elem` tied then fkVariable f else minimum tied
  where
    counts = M.fromListWith (+) [(n, 1 :: Int) | ps <- siblings, (n, t) <- nub ps, t == port]

-- | The unit's HEADER (its class and @__init__@) takes a parameter of type PORT.
takesPort :: CtorShape -> Text -> Text -> Bool
takesPort ct header port = port `elem` map snd (ctorParams ct header)

-- | Jev's question when no rule names the port: which of the service's PORTS the fake in
-- the test's SOURCE stands for, by the methods it USES on it.
conjurePortQuestion :: Text -> [Text] -> [Port] -> Question
conjurePortQuestion src uses ports = choice "conjure-port" instructions options context
  where
    instructions = "A test builds a fake and never passes it in. Which of the service's ports does the fake stand for, by the methods the test uses on it?"
    options = [(ptName p, T.intercalate "; " (ptMethods p)) | p <- ports]
    context =
      [ ("test", src)
      , ("uses", if null uses then "none" else T.intercalate ", " uses)
      , ("ports", T.intercalate "\n\n" [T.intercalate "\n" (ptName p : map ("    " <>) (ptMethods p)) | p <- ports])
      ]

conjurePortGate :: Double
conjurePortGate = 0.6

-- | A confident choice naming one of PORTS.
portFrom :: [Port] -> Answer -> Maybe Text
portFrom ports a = case a of
  Chosen c _ _ | confident conjurePortGate a, c `elem` map ptName ports -> Just c
  _ -> Nothing

-- | The wire's candidates: the two most probable of PORTS (by Jev's distribution), else
-- the first two by name.
portCandidates :: [(Text, Double)] -> [Port] -> [Text]
portCandidates probabilities ports = take 2 (nub (probable ++ names))
  where
    names = map ptName ports
    probable = [n | (n, _) <- sortOn (Down . snd) probabilities, n `elem` names]

-- | The member a check says a port lacks, by LANG's `[check] missing_member`, in its words
-- WHY, its type one of PORTS (the types the unit takes): (the type, the member).
missingMemberIn :: Language -> [Text] -> Text -> Maybe (Text, Text)
missingMemberIn lang ports why = do
  mm <- langCheck lang >>= checkMissingMember
  listToMaybe [(q, m) | g <- why =~ mmPattern mm :: [[Text]], Just m <- [at (mmMember mm) g], Just q <- [at (mmType mm) g], not (T.null m), q `elem` ports]
  where
    at i g = listToMaybe (drop i g)

-- | The lines of TEXT (a round's body, a test) that name the method M (@.M@: a call, a
-- fake's @x.M = …@), stripped: what the model is shown to write M's signature from.
callLines :: Text -> Text -> [Text]
callLines m text = [T.strip l | l <- T.lines text, ("." <> m) `T.isInfixOf` l]

-- | The marked line of a placed snippet (the first line of MARKED with a hole), stripped.
snippetLine :: Text -> Maybe Text
snippetLine marked = listToMaybe [T.strip l | l <- T.lines marked, hasMarkers l]

-- | The signature within a snippet's marked LINE, as a template for 'methodText': from its
-- first hole to its last, each hole as @{= name =}@ (@{= name =}(self{= params =}) -> {= returns =}@).
signatureTemplate :: Text -> Text
signatureTemplate line =
  let parts = splitMarkers line
      isLit = either (const False) (const True)
      trimmed = reverse (dropWhile isLit (reverse (dropWhile isLit parts)))
   in T.concat [either (\mk -> "{= " <> markerName mk <> " =}") id x | x <- trimmed]

-- | The model's LINE read through a snippet's marked line SNIPPET for the method M: its
-- holes' values (@params@, @returns@), when LINE is one line of the snippet's shape that
-- names M. The text after the last hole is lenient: all of it (@: ...@), its first word, or
-- nothing. Nothing for a fence, two lines, another name or a missing prefix: no method is
-- ever written from a garbled answer.
readSignature :: Text -> Text -> Text -> Maybe (Text, Text)
readSignature snippet m line0 = do
  let line = T.strip line0
  if T.any (== '\n') line || "```" `T.isPrefixOf` line then Nothing else Just ()
  vals <- go (splitMarkers snippet) line
  if lookup "name" vals == Just m then Just (fromMaybe "" (lookup "params" vals), T.strip (fromMaybe "" (lookup "returns" vals))) else Nothing
  where
    go [] rest = if T.null (T.strip rest) then Just [] else Nothing
    go (Right lit : ps) rest = T.stripPrefix lit rest >>= go ps
    go [Left mk] rest = Just [(markerName mk, rest)]
    go [Left mk, Right lit] rest =
      let w = T.takeWhile (not . isSpace) (T.stripStart lit)
          r = T.stripEnd rest
       in Just . pure . (markerName mk,) $ case T.breakOn lit r of
            (v, after) | not (T.null after) -> v
            _ | not (T.null w), Just v <- T.stripSuffix w r -> v
            _ -> r
    go (Left mk : Right lit : ps) rest = case T.breakOn lit rest of
      (v, after) | not (T.null after) -> ((markerName mk, v) :) <$> go ps (T.drop (T.length lit) after)
      _ -> Nothing
    go (Left _ : Left _ : _) _ = Nothing

-- | The line (1-based) a method goes in the file TEXT to end the port Q (by the shape's
-- `start`): the line after its block's last (the block as 'portsIn' reads it); none when Q
-- is not there. That line may be past the file's end or not blank: 'blankAt' makes it one.
portEnd :: PortShape -> Text -> Text -> Maybe Int
portEnd ps q text = do
  let numbered = zip [1 :: Int ..] (T.splitOn "\n" text)
  start <- listToMaybe [i | (i, l) <- numbered, Just (_ : n : _) <- [groups (psStart ps) l], n == q]
  let body = takeWhile (\(_, l) -> T.null (T.strip l) || isSpace (T.head l)) (drop start numbered)
      lastLine = maybe start fst (listToMaybe (reverse [x | x@(_, l) <- body, not (T.null (T.strip l))]))
  pure (lastLine + 1)

-- | TEXT with a blank line at LINE (1-based), for a snippet that replaces the line it is
-- placed on: as it is when that line is blank, else with an empty line put there.
blankAt :: Int -> Text -> Text
blankAt line text = case drop (line - 1) ls of
  l : _ | T.null (T.strip l) -> text
  _ -> T.intercalate "\n" (take (line - 1) ls ++ [""] ++ drop (line - 1) ls)
  where
    ls = T.splitOn "\n" text

-- | 'unpassedFake' by a pack's `[conjure.fake]`; none without one.
fakeIn :: Maybe ConjureDef -> Text -> Maybe Fake
fakeIn cj src = (cj >>= cjFake) >>= \fs -> unpassedFake fs src

-- | 'handFake' by a pack's `[conjure.fake]`; none without one.
handFakeIn :: Maybe ConjureDef -> Text -> Text -> Text -> Text -> Maybe Text
handFakeIn cj helper param var src = (cj >>= cjFake) >>= \fs -> handFake fs helper param var src

-- | 'derivePort' by a pack's `[conjure.fake]`; by the variable or the only port without one.
derivePortIn :: Maybe ConjureDef -> Fake -> [Port] -> Maybe Text
derivePortIn cj = derivePort (fromMaybe (FakeShape "" [] [] Nothing Nothing) (cj >>= cjFake))

-- | 'ctorParams' by a pack's `[conjure.constructor]`; none without one.
ctorParamsIn :: Maybe ConjureDef -> Text -> [(Text, Text)]
ctorParamsIn cj text = maybe [] (`ctorParams` text) (cj >>= cjConstructor)

-- | 'takesPort' by a pack's `[conjure.constructor]`; never without one.
takesPortIn :: Maybe ConjureDef -> Text -> Text -> Bool
takesPortIn cj header port = port `elem` map snd (ctorParamsIn cj header)

-- | A method's signature in the language's words: `[types] method` over its NAME, PARAMS
-- (each @, @ and a parameter) and what it RETURNS; the name alone without one.
methodText :: Maybe Text -> Text -> Text -> Text -> Text
methodText template name params returns = maybe name (T.replace "{= name =}" name . T.replace "{= params =}" params . T.replace "{= returns =}" returns) template

-- | The files beside the unit at REL a service's constructors are read from: FILES less
-- REL and every test file ('isTestFile').
siblingsOf :: Language -> FilePath -> [FilePath] -> [FilePath]
siblingsOf lang rel files = [f | f <- files, f /= rel, not (maybe False (`isTestFile` f) (langTest lang))]

-- | A module's sets by its path and the pack's `[conjure] service` (@src/{= app =}/{= service =}@
-- binds @app@ and @service@ for @src/bistro/booking/commands/x.py@): the service's sets when
-- no wire generator binds them. The path goes on past the service, by a file at least.
servicePaths :: Maybe ConjureDef -> FilePath -> [(Text, Text)]
servicePaths cj rel = case cj >>= cjService of
  Nothing -> []
  Just t ->
    let segs = T.splitOn "/" t
        parts = map T.pack (splitDirectories rel)
        bind x p = case T.stripSuffix "=}" =<< T.stripPrefix "{=" x of
          Just k -> Just [(T.strip k, p)]
          Nothing -> if x == p then Just [] else Nothing
     in if length parts >= length segs + 1 then maybe [] concat (sequence (zipWith bind segs parts)) else []

-- | An instruction T in a test file (its lines LS) with the pack's ADVICE (`[[conjure.advice]]`):
-- the sentence of the first advice whose `when` matches a line (by line, then by order) added.
advisedIn :: [Advice] -> [Text] -> Text -> Text
advisedIn advice ls t = maybe t (\say -> t <> "; " <> say) (listToMaybe [avSay a | l <- ls, a <- advice, matches (avWhen a) l])
  where
    matches re l = maybe False (`matchTest` T.unpack l) (makeRegexM (T.unpack re) :: Maybe Regex)
