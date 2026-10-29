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
  , protocolPorts
  , derivePort
  , initParams
  , deriveParam
  , takesPort
  , conjurePortQuestion
  , conjurePortGate
  , portFrom
  , portCandidates
  , missingMethod
  , methodSignature
  , protocolEnd
  ) where

import Data.Char (isAlphaNum, isSpace, isUpper, toLower)
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Decide (Answer (..), Question, choice, confident)

-- | A fake a test builds: the name it is bound to and the class (or @Mock@) called.
data Fake = Fake {fkVariable :: Text, fkClass :: Text}
  deriving stock (Eq, Show)

-- | The first fake bound in a test's body (SRC, its source: its own line, then its body)
-- that is never an argument of a call in that body (positional or keyword); none when every
-- fake is passed.
unpassedFake :: Text -> Maybe Fake
unpassedFake src = do
  let body = T.intercalate "\n" (drop 1 (T.lines src))
  listToMaybe [f | f <- mapMaybe bound (T.lines body), not (passedIn body (fkVariable f))]

-- | @    var = Class(@ for a fake's class: @Mock@, @MagicMock@, @Fake…@, @InMemory…@, @Stub…@.
bound :: Text -> Maybe Fake
bound l = do
  rest <- if T.null l || not (isSpace (T.head l)) then Nothing else Just (T.stripStart l)
  let (var, r1) = T.span isIdent rest
  r2 <- T.stripPrefix "=" (T.stripStart r1)
  if "=" `T.isPrefixOf` r2 then Nothing else Just ()
  let (cls, r3) = T.span isIdent (T.stripStart r2)
  if T.null var || not ("(" `T.isPrefixOf` r3) || not (fakeClass cls) then Nothing else Just (Fake var cls)
  where
    fakeClass c = c `elem` ["Mock", "MagicMock"] || any (\p -> p `T.isPrefixOf` c && T.length c > T.length p) ["Fake", "InMemory", "Stub"]

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || c == '_'

-- | VAR is an argument of a call in BODY: after @(@ or @,@ (and an optional @name=@), before
-- @)@ or @,@.
passedIn :: Text -> Text -> Bool
passedIn body var = any argAt (occurrences var body)
  where
    argAt (before, after) =
      let b = T.stripEnd before
          a = T.stripStart after
          opens t = maybe False (\(_, c) -> c `elem` ['(', ',']) (T.unsnoc (T.stripEnd t))
          keyword = case T.unsnoc b of
            Just (b', '=') | not ("=" `T.isSuffixOf` b'), not (any (`T.isSuffixOf` b') ["!", "<", ">"]) ->
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
-- call of HELPER as PARAM: @command()@ becomes @command(bookings=repository)@, @command(x)@
-- becomes @command(x, bookings=repository)@; none when its body has no such call on one line,
-- or the call already has the fake.
handFake :: Text -> Text -> Text -> Text -> Maybe Text
handFake helper param var src = do
  (i, l') <- listToMaybe [(i, l') | (i, l) <- drop 1 numbered, Just l' <- [handIn l]]
  pure (T.intercalate "\n" [if j == i then l' else l | (j, l) <- numbered])
  where
    numbered = zip [0 :: Int ..] (T.splitOn "\n" src)
    handIn l =
      listToMaybe
        [ b <> helper <> "(" <> (if T.null (T.strip inner) then "" else inner <> ", ") <> param <> "=" <> var <> ")" <> T.drop (T.length args) r
        | (b, a) <- occurrences helper l
        , Just r <- [T.stripPrefix "(" a]
        , Just args <- [balanced r]
        , let inner = T.dropEnd 1 args
        , not (passedIn ("(" <> args) var)
        ]

-- | A service's port: its Protocol class and its method lines.
data Port = Port {ptName :: Text, ptMethods :: [Text]}
  deriving stock (Eq, Show)

-- | Each @class X(Protocol):@ of FILES with its @def@ lines (stripped), sorted by name.
protocolPorts :: [(FilePath, Text)] -> [Port]
protocolPorts files = sortOn ptName [p | (_, t) <- files, p <- ports (T.lines t)]
  where
    ports = \case
      [] -> []
      l : rest
        | Just r <- T.stripPrefix "class " l
        , (name, r') <- T.span isIdent r
        , "(Protocol):" `T.isPrefixOf` T.strip r' ->
            let (body, after) = break topLevel rest
             in Port name [T.strip d | d <- body, "def " `T.isPrefixOf` T.strip d] : ports after
        | otherwise -> ports rest
    topLevel l = not (T.null l) && not (isSpace (T.head l))

-- | The port a fake stands for: the fake's class less @Fake@/@InMemory@/@Stub@ names one; the
-- variable's last word is exactly one port's last word; the service's only port; else none.
derivePort :: Fake -> [Port] -> Maybe Text
derivePort f ports
  | n : _ <- [n | p <- ["Fake", "InMemory", "Stub"], Just n <- [T.stripPrefix p (fkClass f)], n `elem` names] = Just n
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

-- | Each @__init__@ of a module's TEXT: its (param, Type) pairs (@self@ and untyped ones left out).
initParams :: Text -> [(Text, Text)]
initParams text = concat [params a | (_, r) <- T.breakOnAll "def __init__(" text, Just a <- [balanced (T.drop (T.length "def __init__(") r)]]
  where
    params a = mapMaybe param (splitTop (T.dropEnd 1 a))
    param p = case T.breakOn ":" (T.strip p) of
      (n, t) | not (T.null t), T.all isIdent n, not (T.null n) -> Just (n, T.strip (fst (T.breakOn "=" (T.drop 1 t))))
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
takesPort :: Text -> Text -> Bool
takesPort header port = port `elem` map snd (initParams header)

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

-- | The method a check says a port lacks: @Cannot access attribute "M" for class "Q"@ in its
-- words WHY, Q one of PORTS (the classes the unit takes): (Q, M).
missingMethod :: [Text] -> Text -> Maybe (Text, Text)
missingMethod ports why = listToMaybe [(q, m) | (_, r) <- T.breakOnAll "Cannot access attribute \"" why, Just (m, q) <- [names (T.drop (T.length "Cannot access attribute \"") r)], q `elem` ports]
  where
    names r = do
      let (m, r1) = T.breakOn "\"" r
      r2 <- T.stripPrefix "\" for class \"" r1
      let (q, r3) = T.breakOn "\"" r2
      if T.null m || T.null q || T.null r3 then Nothing else Just (m, q)

-- | The signature of the method M a BODY calls on a port (@x = self.p.M(args)@, or the call
-- alone), as the port-method snippet takes it: its parameters after @self@ (@, id: str@) and
-- what it returns. The names: the test file's fake (@.M = lambda id: None@), else the
-- arguments'; their types: an argument @a.f@ where the unit's MODULE has @a: T@ and @T@'s
-- field @f: ty@, a string or a number; what it returns: @None@ for the call alone, else the
-- port's own entity (a type its methods return whose name starts the port's) with @| None@
-- when the body tests the result against @None@ or the fake returns it. None when any of
-- that is not known.
methodSignature :: Text -> Port -> Text -> Text -> Text -> Maybe (Text, Text)
methodSignature m port body moduleText testText = do
  (result, args) <- listToMaybe [(assignedTo l, as) | l <- T.lines body, (_, a) <- T.breakOnAll ("." <> m <> "(") l, Just r <- [balanced (T.drop (T.length m + 2) a)], let as = splitArgs (T.dropEnd 1 r)]
  types <- traverse argType args
  let fakeParams = listToMaybe [(splitArgs ps, T.strip res) | (_, a) <- T.breakOnAll ("." <> m <> " = lambda") testText, let r = T.drop (T.length m + 10) a, let (ps, res0) = T.breakOn ":" (T.takeWhile (/= '\n') r), not (T.null res0), let res = T.drop 1 res0]
      names = case fakeParams of
        Just (ns, _) | length ns == length args -> ns
        _ -> map argName args
      fakeNone = maybe False ((== "None") . snd) fakeParams
  if any T.null names then Nothing else Just ()
  returns <- case result of
    Nothing -> Just "None"
    Just v -> do
      e <- entity
      let optional = fakeNone || any (\l -> (v <> " is None") `T.isInfixOf` l || (v <> " is not None") `T.isInfixOf` l) (T.lines body)
      Just (if optional then e <> " | None" else e)
  pure (T.concat [", " <> n <> ": " <> t | (n, t) <- zip names types], returns)
  where
    assignedTo l =
      let (v, r) = T.span isIdent (T.stripStart l)
       in if not (T.null v) && "=" `T.isPrefixOf` T.stripStart r && not ("==" `T.isPrefixOf` T.stripStart r) then Just v else Nothing
    splitArgs t = filter (not . T.null) (map T.strip (T.splitOn "," t))
    argName a = case T.breakOnEnd "." a of
      ("", n) -> if T.all isIdent n then n else ""
      (_, n) -> if T.all isIdent n then n else ""
    argType a
      | Just _ <- T.stripPrefix "\"" a = Just "str"
      | T.all (`elem` ['0' .. '9']) a, not (T.null a) = Just "int"
      | (o, f) <- T.breakOn "." a, not (T.null f) = do
          ty <- paramType o
          fieldType ty (T.drop 1 f)
      | otherwise = Nothing
    -- @o: T@ in a def line of the module
    paramType o = listToMaybe [T.strip (T.takeWhile (\ch -> ch /= ',' && ch /= ')') r) | l <- T.lines moduleText, "def " `T.isPrefixOf` T.strip l, (_, r0) <- T.breakOnAll ("(" <> o <> ": ") l <> T.breakOnAll (" " <> o <> ": ") l, let r = T.drop (T.length o + 3) r0]
    -- @f: ty@ in class T's body
    fieldType ty f = case dropWhile (not . (("class " <> ty) `T.isPrefixOf`)) (T.lines moduleText) of
      _ : rest -> listToMaybe [T.strip t | l <- takeWhile (\l -> T.null l || isSpace (T.head l)) rest, Just t <- [T.stripPrefix (f <> ":") (T.strip l)]]
      [] -> Nothing
    entity = listToMaybe [e | d <- ptMethods port, let ret = snd (T.breakOnEnd "->" d), e <- idents ret, e /= ptName port, e `T.isPrefixOf` ptName port]
    idents t = [w | w <- T.split (not . isIdent) t, not (T.null w), isUpper (T.head w)]

-- | The line (1-based) a method goes in the file TEXT to end the Protocol class Q: the line
-- after its last, when that is blank or the end of the file; none when Q is not there.
protocolEnd :: Text -> Text -> Maybe Int
protocolEnd q text = do
  let ls = T.splitOn "\n" text
      numbered = zip [1 :: Int ..] ls
  start <- listToMaybe [i | (i, l) <- numbered, ("class " <> q <> "(") `T.isPrefixOf` l]
  let body = takeWhile (\(_, l) -> T.null (T.strip l) || isSpace (T.head l)) (drop start numbered)
      lastLine = maybe start fst (listToMaybe (reverse [x | x@(_, l) <- body, not (T.null (T.strip l))]))
      next = lastLine + 1
  case drop (next - 1) ls of
    l : _ | T.null (T.strip l) -> Just next
    _ -> Nothing
