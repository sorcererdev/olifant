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
  , Port (..)
  , protocolPorts
  , derivePort
  , initParams
  , deriveParam
  , takesPort
  , testSource
  , conjurePortQuestion
  , conjurePortGate
  , portFrom
  , portCandidates
  ) where

import Data.Char (isAlphaNum, isSpace, isUpper, toLower)
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Decide (Answer (..), Question, choice, confident)

-- | A test's source (it lives here, not in Site, which would import ConjureNext in a cycle): its def line and its body, trailing blank lines dropped.
testSource :: Text -> Text -> Maybe Text
testSource name text = case dropWhile (not . isDef) (T.lines text) of
  [] -> Nothing
  d : rest -> Just (T.intercalate "\n" (d : reverse (dropWhile (T.null . T.strip) (reverse (takeWhile (\l -> T.null (T.strip l) || indentOf l > indentOf d) rest)))))
  where
    isDef l = ("def " <> name <> "(") `T.isPrefixOf` T.strip l
    indentOf = T.length . T.takeWhile (== ' ')

-- | A fake a test builds: the name it is bound to and the class (or @Mock@) called.
data Fake = Fake {fkVariable :: Text, fkClass :: Text}
  deriving stock (Eq, Show)

-- | The first fake bound in the test NAME's body (in TEXT, a test file) that is never an
-- argument of a call in that body (positional or keyword); none when every fake is passed.
unpassedFake :: Text -> Text -> Maybe Fake
unpassedFake text name = do
  src <- testSource name text
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
