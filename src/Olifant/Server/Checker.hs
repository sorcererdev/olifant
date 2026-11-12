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
  , judgementNamed
  , CheckMode (..)
  , checkModeName
  , checkModeOf
  , Print (..)
  , fileHash
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
import Data.Bits (xor)
import Data.Char (isDigit, toLower)
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (isNothing, listToMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import Numeric (showHex)
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..), PlansConfig (..))
import Olifant.Language.Adapter (languageFor)
import Olifant.Plans (Kind (Step), anchorText)
import Olifant.Plans.Sentences (Sentence (..), sentenceKey)
import Olifant.Process (Cancel)
import Olifant.Server.Confine (confined, safely)
import Olifant.Server.Errand (RunFailure (..), RunResult (..), Status (..), Verb (Check), checkFences, runClaude)
import Olifant.Server.State (ServerConfig (..), ServerState (..))
import Olifant.Server.Triage (Triage (..))
import Olifant.Server.Unit (Named (..), fingerprint, unitLookupIn, unitsWithKinds)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.Directory (canonicalizePath)
import System.FilePath (isAbsolute, makeRelative, (</>))

-- | A unit by its path and (qualified) name.
data UnitRef = UnitRef {urPath :: FilePath, urName :: Text}
  deriving stock (Eq, Ord, Show)

-- | `path:Name`, as an anchor writes it.
refText :: UnitRef -> Text
refText r = T.pack (urPath r) <> ":" <> urName r

-- | A verdict: right, wrong (a claim), done, open, misfit (a step's
-- progress), or cannot tell (either).
data Judgement = JRight | JWrong | JCannotTell | JDone | JOpen | JMisfit
  deriving (Eq, Show)

judgementName :: Judgement -> Text
judgementName = \case
  JRight -> "right"
  JWrong -> "wrong"
  JCannotTell -> "cannot_tell"
  JDone -> "done"
  JOpen -> "open"
  JMisfit -> "misfit"

-- | A verdict by its JSON word.
judgementNamed :: Text -> Maybe Judgement
judgementNamed t = lookup t [(judgementName j, j) | j <- [JRight, JWrong, JCannotTell, JDone, JOpen, JMisfit]]

-- | What a check asks: whether a claim is so ('CheckClaim', JSON "verify"),
-- or how far the code has got with a step ('CheckProgress', "progress").
data CheckMode = CheckClaim | CheckProgress
  deriving (Eq, Ord, Show)

checkModeName :: CheckMode -> Text
checkModeName = \case
  CheckClaim -> "verify"
  CheckProgress -> "progress"

-- | The mode a sentence is checked in: a step's progress, else a claim.
checkModeOf :: Triage -> CheckMode
checkModeOf t = if trKind t == "step" then CheckProgress else CheckClaim

-- | One confirmed citation's print: the unit's 'fingerprint' when it named
-- or fell inside one, else the whole file's 'fileHash' (unit Nothing).
data Print = Print {ptPath :: FilePath, ptUnit :: Maybe Text, ptHash :: Text}
  deriving (Eq, Ord, Show)

-- | A file's hash: 7 hex of FNV-1a 64 over its whole text (as 'fingerprint'
-- hashes a unit).
fileHash :: Text -> Text
fileHash t = T.pack (take 7 (pad (showHex (BS.foldl' step 14695981039346656037 (TE.encodeUtf8 t)) "")))
  where
    step h b = (h `xor` fromIntegral b) * 1099511628211 :: Word64
    pad x = replicate (16 - length x) '0' ++ x

-- | Where a verdict rests: a path, and a unit or a line (1-based) in it.
data Citation = Citation {ciPath :: FilePath, ciUnit :: Maybe Text, ciLine :: Maybe Int}
  deriving (Eq, Show)

data CheckReply = CheckReply {crJudgement :: Judgement, crWhy :: Text, crEvidence :: [Citation]}
  deriving (Eq, Show)

-- | A reply after its citations were checked: the first cited unit found,
-- with its fingerprint, is the sentence's evidence unit; every citation
-- found has its print ('Print'), so a later change to any of them shows.
data Confirmed = Confirmed
  { cfJudgement :: Judgement, cfWhy :: Text, cfEvidence :: [Citation]
  , cfUnit :: Maybe UnitRef, cfFingerprint :: Maybe Text, cfPrints :: [Print] }
  deriving (Eq, Show)

-- | One check of a sentence in a mode.
type Checker = CheckMode -> Sentence -> IO (Either RunFailure RunResult)

-- | Where a confirmed verdict rests, in words: its unit as `path:Unit`, else
-- the first citation's path (and line), else "".
evidenceText :: Confirmed -> Text
evidenceText c = case (cfUnit c, cfEvidence c) of
  (Just u, _) -> refText u
  (Nothing, x : _) -> T.pack (ciPath x) <> maybe "" (\l -> ":" <> T.pack (show l)) (ciLine x)
  _ -> ""

fences :: PlansConfig -> [FilePath]
fences = checkFences

-- | The prompt: a claim judged as the code stands now ('CheckClaim'), or a
-- step's progress, done, open or misfit ('CheckProgress').
checkPrompt :: CheckMode -> Sentence -> Text
checkPrompt CheckProgress s =
  T.unlines $
    [ "Check how far this repository (the current directory) has got with one step of a software plan."
    , ""
    , "Section: " <> (if T.null (snHeading s) then "(none)" else snHeading s) ]
      ++ ["Lead (the sentence or question above it): " <> l | Just l <- [snLead s]]
      ++ ["Step: " <> snText s]
      ++ [if snKind s == Step True then "The writer ticked this step: they say it is done." else "The step is not ticked."]
      ++ ["Hint: the writer pointed at `" <> T.drop 1 (anchorText a) <> "`: a place to start, not the answer." | Just a <- [snAnchor s]]
      ++ [ ""
         , "Use Read, Grep and Glob to find the code the step is about, and read it."
         , "Behaviour can live in any layer: server code, templates, and front-end scripts and styles. Before you conclude that something does not exist, search for it across the whole repository."
         , "Rule: judge the step against the code now. Give one verdict:"
         , "- done: the code already does all of what the step says. Cite the code that does it."
         , "- open: the code does not do it yet, and nothing in the code says the step cannot be done as written. Cite the code the step would change when it exists; none is fine for a step that makes something new."
         , "- misfit: the step as written rests on something that is not so in the code, or would not get what it says. Say in why what is not so, and cite the code that shows it. A step that also states something about the code now is a misfit when that statement is false."
         , "If the code cannot settle it (it is about people, documents or outside services), the verdict is cannot_tell."
         , ""
         , "Answer with one fenced ```json block holding one object, after any notes:"
         , "```json"
         , "{\"verdict\": \"done | open | misfit | cannot_tell\", \"why\": \"<one sentence>\", \"evidence\": [{\"path\": \"<path relative to the current directory>\", \"unit\": \"<function or class name, or null>\", \"line\": <line number, or null>}]}"
         , "```"
         , "For done or misfit, give at least one evidence entry: the code the verdict rests on." ]
checkPrompt CheckClaim s =
  T.unlines $
    [ "Check one sentence of a software plan against this repository (the current directory)."
    , ""
    , "Section: " <> (if T.null (snHeading s) then "(none)" else snHeading s) ]
      ++ ["Lead (the sentence or question above it): " <> l | Just l <- [snLead s]]
      ++ ["Sentence: " <> snText s]
      ++ ["Hint: the writer pointed at `" <> T.drop 1 (anchorText a) <> "`: a place to start, not the answer." | Just a <- [snAnchor s]]
      ++ [ ""
         , "Use Read, Grep and Glob to find the code the sentence is about, and read it."
         , "Behaviour can live in any layer: server code, templates, and front-end scripts and styles. Before you conclude that something does not exist, search for it across the whole repository."
         , "Rule: judge what the sentence says is true of the code now; if it also wishes or asks for something, ignore that part."
         , "If the code cannot settle it (it is about people, documents or outside services), the verdict is cannot_tell."
         , ""
         , "Answer with one fenced ```json block holding one object, after any notes:"
         , "```json"
         , "{\"verdict\": \"right | wrong | cannot_tell\", \"why\": \"<one sentence>\", \"evidence\": [{\"path\": \"<path relative to the current directory>\", \"unit\": \"<function or class name, or null>\", \"line\": <line number, or null>}]}"
         , "```"
         , "For right or wrong, give at least one evidence entry: the code the verdict rests on." ]

-- | The last ```json block, else the first `{` from which the rest of the
-- reply decodes as an object. A verdict of the other mode is refused, and
-- so is right, wrong, done or misfit with no evidence.
parseCheckReply :: CheckMode -> Text -> Either Text CheckReply
parseCheckReply mode reply = do
  v <- maybe (Left "no JSON object in the reply") Right (maybe bare Just fenced)
  o <- case v of
    Object o -> Right o
    _ -> Left "the reply's JSON is not an object"
  j <- case KM.lookup "verdict" o of
    Just (String t) ->
      let w = T.map toLower (T.replace " " "_" (T.strip t))
       in case judgementNamed w of
            Just j | j `elem` wordsOf mode -> Right j
            _ -> Left ("unknown verdict `" <> w <> "`")
    _ -> Left "no verdict"
  let why = case KM.lookup "why" o of
        Just (String t) -> T.strip t
        _ -> ""
      evidence = case KM.lookup "evidence" o of
        Just (Array xs) -> [c | Object e <- foldr (:) [] xs, Just c <- [citation e]]
        _ -> []
  if j `elem` [JRight, JWrong, JDone, JMisfit] && null evidence then Left "a verdict with no evidence" else Right (CheckReply j why evidence)
  where
    wordsOf = \case
      CheckClaim -> [JRight, JWrong, JCannotTell]
      CheckProgress -> [JDone, JOpen, JMisfit, JCannotTell]
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
-- Any citation that fails makes the verdict cannot_tell, "cited code not
-- found"; except for open, which rests on no code: the citation is dropped.
-- Each citation found has its 'Print': the unit's fingerprint (named, or
-- enclosing the line), else the whole file's 'fileHash'.
confirm :: ServerState -> Project -> [FilePath] -> CheckReply -> IO Confirmed
confirm s p fs r = do
  checked0 <- traverse (\c -> (c,) <$> cite c) (crEvidence r)
  let checked = if crJudgement r == JOpen then [x | x@(_, Just _) <- checked0] else checked0
      found = [u | (_, Just (Just u, _)) <- checked]
      unit = listToMaybe found
      prints = [pr | (_, Just (_, pr)) <- checked]
  pure $
    if any (isNothing . snd) checked
      then Confirmed JCannotTell (if crJudgement r == JCannotTell then crWhy r else "cited code not found") (crEvidence r) Nothing Nothing []
      else Confirmed (crJudgement r) (crWhy r) (map fst checked) (fst <$> unit) (snd <$> unit) prints
  where
    root = prRoot p
    relOf path
      | isAbsolute path = if (root <> "/") `isPrefixOf` path then confined (makeRelative root path) else Nothing
      | otherwise = confined path
    fencedOff rel = any (\f -> let f' = normalisePath f in rel == f' || (f' <> "/") `isPrefixOf` rel) fs
    -- the path as the disk resolves it (symlinks followed): under the root
    -- and outside the fences, else not found
    resolvedOk rel = do
      canonRoot <- canonicalizePath root
      canon <- canonicalizePath (root </> rel)
      let inside = (canonRoot <> "/") `isPrefixOf` canon
          canonFenced = any (\f -> let f' = normalisePath f in (canonRoot </> f') == canon || ((canonRoot </> f') <> "/") `isPrefixOf` canon) fs
      pure (inside && not canonFenced)
    cite c = case relOf (ciPath c) of
      Nothing -> pure Nothing
      Just rel0 -> do
        let rel = normalisePath rel0
        ok <- safely False (resolvedOk rel)
        if fencedOff rel || not ok
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
                  mk n = let fp = fingerprint t (namedLines n) in (Just (UnitRef rel (namedName n), fp), Print rel (Just (namedName n)) fp)
                  whole = (Nothing, Print rel Nothing (fileHash t))
              pure $ case (byName, ciUnit c, ciLine c) of
                (Just n, _, _) -> Just (mk n)
                -- a unit named but not found is a guess, whatever its line
                (Nothing, Nothing, Just l) | lineIn -> Just (maybe whole mk (enclosing l))
                (Nothing, Nothing, Nothing) -> Just whole
                _ -> Nothing

-- | One live check: the runner under `check_timeout`, the prompt on stdin, cwd the root.
liveChecker :: Cancel -> PlansConfig -> FilePath -> Checker
liveChecker token cfg root mode s = runClaude token cfg {pcTimeout = pcCheckTimeout cfg} Check [] Nothing root (checkPrompt mode s)

-- | Recorded replies (tests): `{"replies": {KEY: TEXT or {"text", "cost", "seconds"}}}`
-- by 'sentenceKey' (an object may add "delay", seconds). Each key asked is appended to FILE.asked and each prompt to
-- FILE.prompts; a key not in the file is a failure.
recordedChecker :: FilePath -> Checker
recordedChecker file mode s = do
  let key = sentenceKey (snText s)
  appendFile (file <> ".asked") (T.unpack key <> "\n")
  BS.appendFile (file <> ".prompts") (TE.encodeUtf8 (checkPrompt mode s <> "\n"))
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
replayChecker file live mode s = do
  let key = sentenceKey (snText s)
  replies <- readReplies file
  case live of
    Nothing -> case KM.lookup (Key.fromText key) replies >>= resultOf of
      Just r -> pure (Right r)
      Nothing -> Left (RunFailure Empty "no recorded reply" Nothing) <$ appendFile (file <> ".missed") (T.unpack key <> "\n")
    Just run ->
      run mode s >>= \case
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
