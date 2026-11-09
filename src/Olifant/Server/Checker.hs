{-# LANGUAGE OverloadedStrings #-}
-- | The plan checker: a `claude -p` run (verb 'Check') that searches the
-- repository for the code a sentence is about and judges the sentence by it,
-- citing the code. Fenced off the plans, `.olifant/` and `[plans]
-- not_evidence` by deny rules and the appended system prompt. Every
-- citation is checked here before a verdict is shown ('confirm'): a right
-- or wrong never rests on code the engine could not find.
module Olifant.Server.Checker
  ( Judgement (..)
  , judgementName
  , Citation (..)
  , CheckReply (..)
  , Confirmed (..)
  , UnitRef (..)
  , refText
  , evidenceText
  , Checker
  , fences
  , checkPrompt
  , parseCheckReply
  , confirm
  , liveChecker
  , recordedChecker
  , replayChecker
  , checkerUnder
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), eitherDecodeStrict, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (isDigit, toLower)
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (isNothing, listToMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..), PlansConfig (..))
import Olifant.Language.Adapter (languageFor)
import Olifant.Plans (anchorText)
import Olifant.Plans.Sentences (Sentence (..), sentenceKey)
import Olifant.Process (Cancel)
import Olifant.Server.Confine (confined, safely)
import Olifant.Server.Errand (RunFailure (..), RunResult (..), Status (..), Verb (Check), checkFences, runClaude)
import Olifant.Server.State (ServerConfig (..), ServerState (..))
import Olifant.Server.Unit (Named (..), fingerprint, unitLookupIn, unitsWithKinds)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (isAbsolute, makeRelative)

-- | A unit by its path and (qualified) name.
data UnitRef = UnitRef {urPath :: FilePath, urName :: Text}
  deriving stock (Eq, Ord, Show)

-- | `path:Name`, as an anchor writes it.
refText :: UnitRef -> Text
refText r = T.pack (urPath r) <> ":" <> urName r

data Judgement = JRight | JWrong | JCannotTell
  deriving (Eq, Show)

judgementName :: Judgement -> Text
judgementName = \case
  JRight -> "right"
  JWrong -> "wrong"
  JCannotTell -> "cannot_tell"

-- | Where a verdict rests: a path, and a unit or a line (1-based) in it.
data Citation = Citation {ciPath :: FilePath, ciUnit :: Maybe Text, ciLine :: Maybe Int}
  deriving (Eq, Show)

data CheckReply = CheckReply {crJudgement :: Judgement, crWhy :: Text, crEvidence :: [Citation]}
  deriving (Eq, Show)

-- | A reply after its citations were checked: the first cited unit found,
-- with its fingerprint, is the sentence's evidence unit.
data Confirmed = Confirmed
  { cfJudgement :: Judgement, cfWhy :: Text, cfEvidence :: [Citation]
  , cfUnit :: Maybe UnitRef, cfFingerprint :: Maybe Text }
  deriving (Eq, Show)

type Checker = Sentence -> IO (Either RunFailure RunResult)

-- | Where a confirmed verdict rests, in words: its unit as `path:Unit`, else
-- the first citation's path (and line), else "".
evidenceText :: Confirmed -> Text
evidenceText c = case (cfUnit c, cfEvidence c) of
  (Just u, _) -> refText u
  (Nothing, x : _) -> T.pack (ciPath x) <> maybe "" (\l -> ":" <> T.pack (show l)) (ciLine x)
  _ -> ""

fences :: PlansConfig -> [FilePath]
fences = checkFences

checkPrompt :: Sentence -> Text
checkPrompt s =
  T.unlines $
    [ "Check one sentence of a software plan against this repository (the current directory)."
    , ""
    , "Section: " <> (if T.null (snHeading s) then "(none)" else snHeading s) ]
      ++ ["Lead (the sentence or question above it): " <> l | Just l <- [snLead s]]
      ++ ["Sentence: " <> snText s]
      ++ ["Hint: the writer pointed at `" <> T.drop 1 (anchorText a) <> "`: a place to start, not the answer." | Just a <- [snAnchor s]]
      ++ [ ""
         , "Use Read, Grep and Glob to find the code the sentence is about, and read it."
         , "Rule: judge what the sentence says is true of the code now; if it also wishes or asks for something, ignore that part."
         , "If the code cannot settle it (it is about people, documents or outside services), the verdict is cannot_tell."
         , ""
         , "Answer with one fenced ```json block holding one object, after any notes:"
         , "```json"
         , "{\"verdict\": \"right | wrong | cannot_tell\", \"why\": \"<one sentence>\", \"evidence\": [{\"path\": \"<path relative to the current directory>\", \"unit\": \"<function or class name, or null>\", \"line\": <line number, or null>}]}"
         , "```"
         , "For right or wrong, give at least one evidence entry: the code the verdict rests on." ]

-- | The last ```json block, else the first `{` from which the rest of the
-- reply decodes as an object. Right or wrong with no evidence is refused.
parseCheckReply :: Text -> Either Text CheckReply
parseCheckReply reply = do
  v <- maybe (Left "no JSON object in the reply") Right (maybe bare Just fenced)
  o <- case v of
    Object o -> Right o
    _ -> Left "the reply's JSON is not an object"
  j <- case KM.lookup "verdict" o of
    Just (String t) -> case T.map toLower (T.replace " " "_" (T.strip t)) of
      "right" -> Right JRight
      "wrong" -> Right JWrong
      "cannot_tell" -> Right JCannotTell
      other -> Left ("unknown verdict `" <> other <> "`")
    _ -> Left "no verdict"
  let why = case KM.lookup "why" o of
        Just (String t) -> T.strip t
        _ -> ""
      evidence = case KM.lookup "evidence" o of
        Just (Array xs) -> [c | Object e <- foldr (:) [] xs, Just c <- [citation e]]
        _ -> []
  if j /= JCannotTell && null evidence then Left "a verdict with no evidence" else Right (CheckReply j why evidence)
  where
    decode t = either (const Nothing) Just (eitherDecodeStrict (TE.encodeUtf8 t)) :: Maybe Value
    fenced = case drop 1 (T.splitOn "```json" reply) of
      [] -> Nothing
      parts -> decode (fst (T.breakOn "```" (last parts)))
    bare = listToMaybe [v | i <- braces, Just v@(Object _) <- [decode (T.dropWhileEnd (/= '}') (T.drop i reply))]]
    braces = [i | (i, c) <- zip [0 ..] (T.unpack reply), c == '{']
    citation e = case KM.lookup "path" e of
      Just (String p0) ->
        let (p, suffix) = splitSuffix (T.strip p0)
            unit = case KM.lookup "unit" e of
              Just (String u) | not (T.null (T.strip u)) -> Just (T.strip u)
              _ -> case suffix of
                Just sfx | not (T.all isDigit sfx) -> Just sfx
                _ -> Nothing
            line = case KM.lookup "line" e of
              Just (Number n) -> Just (round (toRealFloat n :: Double))
              _ -> case suffix of
                Just sfx | not (T.null sfx), T.all isDigit sfx -> Just (read (T.unpack sfx))
                _ -> Nothing
        in if T.null p then Nothing else Just (Citation (T.unpack p) unit line)
      _ -> Nothing
    -- "path:42" or "path:Unit"
    splitSuffix p = case T.breakOnEnd ":" p of
      (before, after) | T.length before > 1, not (T.null after), not ("/" `T.isInfixOf` after) -> (T.dropEnd 1 before, Just after)
      _ -> (p, Nothing)

-- | Each citation checked: a path under the root, outside the fences, that
-- exists; a unit found there by name, else a line inside the file (its
-- enclosing unit, if any), else no unit or line at all (the whole file).
-- Any citation that fails makes the verdict cannot_tell, "cited code not found".
confirm :: ServerState -> Project -> [FilePath] -> CheckReply -> IO Confirmed
confirm s p fs r = do
  checked <- traverse cite (crEvidence r)
  let found = [u | Just (Just u) <- checked]
      unit = listToMaybe found
  pure $
    if any isNothing checked
      then Confirmed JCannotTell (if crJudgement r == JCannotTell then crWhy r else "cited code not found") (crEvidence r) Nothing Nothing
      else Confirmed (crJudgement r) (crWhy r) (crEvidence r) (fst <$> unit) (snd <$> unit)
  where
    root = prRoot p
    relOf path
      | isAbsolute path = if (root <> "/") `isPrefixOf` path then confined (makeRelative root path) else Nothing
      | otherwise = confined path
    fencedOff rel = any (\f -> let f' = normalisePath f in rel == f' || (f' <> "/") `isPrefixOf` rel) fs
    cite c = case relOf (ciPath c) of
      Nothing -> pure Nothing
      Just rel0 -> do
        let rel = normalisePath rel0
        if fencedOff rel
          then pure Nothing
          else safely Nothing (readText (prWorkspace p) rel) >>= \case
            Nothing -> pure Nothing
            Just t -> do
              units <- case languageFor (catalogLanguages (prCatalog p)) rel of
                Just lang -> safely [] (either (const []) id <$> unitsWithKinds (pSyntax (ssPorts s)) lang (Just rel) t)
                Nothing -> pure []
              let byName = ciUnit c >>= \u -> either (const Nothing) Just (unitLookupIn u units)
                  rows = length (T.lines t)
                  lineIn = maybe False (\l -> l >= 1 && l <= rows) (ciLine c)
                  enclosing l = listToMaybe (sortOn (\n -> snd (namedLines n) - fst (namedLines n)) [n | (n, _) <- units, fst (namedLines n) <= l, l <= snd (namedLines n)])
                  mk n = Just (UnitRef rel (namedName n), fingerprint t (namedLines n))
              pure $ case (byName, ciUnit c, ciLine c) of
                (Just n, _, _) -> Just (mk n)
                (Nothing, _, Just l) | lineIn -> Just (enclosing l >>= mk)
                (Nothing, Nothing, Nothing) -> Just Nothing
                _ -> Nothing

-- | One live check: the runner under `check_timeout`, the prompt on stdin, cwd the root.
liveChecker :: Cancel -> PlansConfig -> FilePath -> Checker
liveChecker token cfg root s = runClaude token cfg {pcTimeout = pcCheckTimeout cfg} Check [] Nothing root (checkPrompt s)

-- | Recorded replies (tests): `{"replies": {KEY: TEXT or {"text", "cost", "seconds"}}}`
-- by 'sentenceKey' (an object may add "delay", seconds). Each key asked is appended to FILE.asked and each prompt to
-- FILE.prompts; a key not in the file is a failure.
recordedChecker :: FilePath -> Checker
recordedChecker file s = do
  let key = sentenceKey (snText s)
  appendFile (file <> ".asked") (T.unpack key <> "\n")
  BS.appendFile (file <> ".prompts") (TE.encodeUtf8 (checkPrompt s <> "\n"))
  replies <- readReplies file
  case KM.lookup (Key.fromText key) replies of
    Just v | Just r <- resultOf v -> do
      -- "delay": seconds the reply takes (a check in flight, for cancel tests)
      case v of
        Object o | Just (Number d) <- KM.lookup "delay" o -> threadDelay (round (toRealFloat d * 1000000 :: Double))
        _ -> pure ()
      pure (Right r)
    _ -> pure (Left (RunFailure Empty "no recorded reply" Nothing))

-- | As 'replayDecider': with no live checker, replay (a key not in the file
-- is a failure, noted in FILE.missed); with one, run it and record its reply.
replayChecker :: FilePath -> Maybe Checker -> Checker
replayChecker file live s = do
  let key = sentenceKey (snText s)
  replies <- readReplies file
  case live of
    Nothing -> case KM.lookup (Key.fromText key) replies >>= resultOf of
      Just r -> pure (Right r)
      Nothing -> Left (RunFailure Empty "no recorded reply" Nothing) <$ appendFile (file <> ".missed") (T.unpack key <> "\n")
    Just run ->
      run s >>= \case
        Left f -> pure (Left f)
        Right r -> do
          let entry = object (["text" .= rrText r, "seconds" .= rrSeconds r, "models" .= map fst (rrModels r)] ++ ["cost" .= c | Just c <- [rrCost r]])
          BL.writeFile file (encode (object ["replies" .= Object (KM.insert (Key.fromText key) entry replies)]) <> "\n")
          pure (Right r)

-- | The recorded checker when the server has `scCheckAnswers` (tests), else the live one.
checkerUnder :: ServerState -> Cancel -> PlansConfig -> FilePath -> Checker
checkerUnder s token cfg root = maybe (liveChecker token cfg root) recordedChecker (scCheckAnswers (ssConfig s))

readReplies :: FilePath -> IO (KM.KeyMap Value)
readReplies file =
  (try (BS.readFile file) :: IO (Either SomeException BS.ByteString)) >>= \case
    Right raw | Right (Object o) <- eitherDecodeStrict raw, Just (Object rs) <- KM.lookup "replies" o -> pure rs
    _ -> pure KM.empty

resultOf :: Value -> Maybe RunResult
resultOf = \case
  String t -> Just (blank t)
  Object o | Just (String t) <- KM.lookup "text" o ->
    Just (blank t) {rrCost = num "cost" o, rrSeconds = maybe 0 id (num "seconds" o), rrModels = []}
  _ -> Nothing
  where
    blank t = RunResult t 0 Nothing Nothing Nothing Nothing [] 0 []
    num k o = case KM.lookup k o of
      Just (Number n) -> Just (toRealFloat n)
      _ -> Nothing
