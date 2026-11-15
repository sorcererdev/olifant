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
  , Source (..)
  , sourceDomain
  , CheckAsk (..)
  , CheckReply (..)
  , Confirmed (..)
  , UnitRef (..)
  , refText
  , evidenceText
  , Checker
  , fences
  , checkPrompt
  , checkPromptWith
  , parseCheckReply
  , confirm
  , liveChecker
  , recordedChecker
  , recordedCheckerAs
  , recordedAsked
  , replayChecker
  , readReplies
  , resultOf
  , recordLock
  , checkerUnder
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), eitherDecodeStrict, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Bits (xor)
import Data.Char (isDigit, toLower)
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
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
import Olifant.Server.Errand (Belief (..), BeliefCheck (..), RunFailure (..), RunResult (..), Status (..), Verb (Check), checkFences, runClaude, webTools)
import Olifant.Server.State (ServerConfig (..), ServerState (..))
import Olifant.Server.Triage (Triage (..))
import Olifant.Server.Unit (Named (..), fingerprint, unitLookupIn, unitsWithKinds)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (isAbsolute, makeRelative, takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)

-- | A unit by its path and (qualified) name.
data UnitRef = UnitRef {urPath :: FilePath, urName :: Text}
  deriving stock (Eq, Ord, Show)

-- | `path:Name`, as an anchor writes it.
refText :: UnitRef -> Text
refText r = T.pack (urPath r) <> ":" <> urName r

-- | A verdict: right, wrong (a claim), done, open, misfit (a step's
-- progress), answered (a question), or cannot tell (any).
data Judgement = JRight | JWrong | JCannotTell | JDone | JOpen | JMisfit | JAnswered
  deriving (Eq, Show)

judgementName :: Judgement -> Text
judgementName = \case
  JRight -> "right"
  JWrong -> "wrong"
  JCannotTell -> "cannot_tell"
  JDone -> "done"
  JOpen -> "open"
  JMisfit -> "misfit"
  JAnswered -> "answered"

-- | A verdict by its JSON word.
judgementNamed :: Text -> Maybe Judgement
judgementNamed t = lookup t [(judgementName j, j) | j <- [JRight, JWrong, JCannotTell, JDone, JOpen, JMisfit, JAnswered]]

-- | What a check asks: whether a claim is so ('CheckClaim', JSON "verify"),
-- how far the code has got with a step ('CheckProgress', "progress"), or
-- the answer to a question ('CheckAnswer', "answer").
data CheckMode = CheckClaim | CheckProgress | CheckAnswer
  deriving (Eq, Ord, Show)

checkModeName :: CheckMode -> Text
checkModeName = \case
  CheckClaim -> "verify"
  CheckProgress -> "progress"
  CheckAnswer -> "answer"

-- | The mode a sentence is checked in: a step's progress, a question's
-- answer, else a claim.
checkModeOf :: Triage -> CheckMode
checkModeOf t = case trKind t of
  "step" -> CheckProgress
  "question" -> CheckAnswer
  _ -> CheckClaim

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

-- | A web page a verdict or answer rests on: the model's word and a link
-- (shown, never opened or confirmed by olifant).
data Source = Source {soUrl :: Text, soTitle :: Text}
  deriving (Eq, Show)

-- | A source's host, without a leading "www." (what the margin names).
sourceDomain :: Text -> Text
sourceDomain u = fromMaybe host (T.stripPrefix "www." host)
  where
    rest = snd (T.breakOnEnd "://" u)
    authority = T.takeWhile (`notElem` ("/?#" :: String)) rest
    host = T.toLower (T.takeWhile (/= ':') (snd (T.breakOnEnd "@" authority)))

-- | A check's reply: the verdict, why, the code cited; for an answer, the
-- answer (at most three sentences) and its optional detail; with web, the
-- pages it rests on.
data CheckReply = CheckReply
  { crJudgement :: Judgement, crWhy :: Text, crEvidence :: [Citation]
  , crAnswer :: Maybe Text, crDetail :: Maybe Text, crSources :: [Source] }
  deriving (Eq, Show)

-- | A reply after its citations were checked: the first cited unit found,
-- with its fingerprint, is the sentence's evidence unit; every citation
-- found has its print ('Print'), so a later change to any of them shows.
-- The answer, its detail and the sources pass through as given.
data Confirmed = Confirmed
  { cfJudgement :: Judgement, cfWhy :: Text, cfEvidence :: [Citation]
  , cfUnit :: Maybe UnitRef, cfFingerprint :: Maybe Text, cfPrints :: [Print]
  , cfAnswer :: Maybe Text, cfDetail :: Maybe Text, cfSources :: [Source] }
  deriving (Eq, Show)

-- | What one check is asked: its mode, whether it may use the web, and
-- (an answer under a `- ? ` line) the writer's beliefs under the question.
data CheckAsk = CheckAsk {ckMode :: CheckMode, ckWeb :: Bool, ckBeliefs :: [Belief]}
  deriving (Eq, Show)

-- | One check of a sentence.
type Checker = CheckAsk -> Sentence -> IO (Either RunFailure RunResult)

-- | Where a confirmed verdict rests, in words: its unit as `path:Unit`, else
-- the first citation's path (and line), else "".
evidenceText :: Confirmed -> Text
evidenceText c = case (cfUnit c, cfEvidence c) of
  (Just u, _) -> refText u
  (Nothing, x : _) -> T.pack (ciPath x) <> maybe "" (\l -> ":" <> T.pack (show l)) (ciLine x)
  _ -> ""

fences :: PlansConfig -> [FilePath]
fences = checkFences

-- | The prompt: a claim judged as the code stands now ('CheckClaim'), a
-- step's progress, done, open or misfit ('CheckProgress'), or a question
-- answered ('CheckAnswer'). With 'ckWeb' (never for progress), a claim or
-- answer may rest on public web pages, given in sources.
checkPrompt :: CheckAsk -> Sentence -> Text
checkPrompt = checkPromptWith []

-- | 'checkPrompt' naming the hosts the check may fetch from (`[plans] web_fetch`).
checkPromptWith :: [Text] -> CheckAsk -> Sentence -> Text
checkPromptWith hosts ask s = case ckMode ask of
  CheckProgress -> progressPrompt s
  CheckClaim | ckWeb ask -> webCheckPrompt hosts CheckClaim s
  CheckAnswer | ckWeb ask -> webCheckPrompt hosts CheckAnswer s
  CheckClaim -> claimPrompt s
  CheckAnswer -> answerPrompt ask s

-- | A web check's prompt: the sentence and its section heading, and nothing
-- of the repository (no lead, hint, beliefs or code); it rests on pages alone
-- (user ruling 2026-11-14).
webCheckPrompt :: [Text] -> CheckMode -> Sentence -> Text
webCheckPrompt hosts mode s =
  T.unlines $
    [ case mode of
        CheckAnswer -> "Answer one question from a software plan, from public technical sources."
        _ -> "Check one sentence of a software plan against public technical sources."
    , ""
    , "Section: " <> (if T.null (snHeading s) then "(none)" else snHeading s)
    , (if mode == CheckAnswer then "Question: " else "Sentence: ") <> snText s
    , ""
    , "Search the web (WebSearch) for what public sources settle: a library's or framework's documentation and release notes, a standard, a public API's reference"
        <> (if null hosts then "" else "; and read pages on " <> T.intercalate ", " hosts <> " (WebFetch)")
        <> ". You cannot see the project's code."
    , if mode == CheckAnswer
        then "Rule: answer from public sources as they stand for the version the question names."
        else "Rule: judge whether what the sentence says is true, for the version it names; if it also wishes or asks for something, ignore that part."
    , "If no public source settles it (it is about this project's own code, people, opinions, plans or private documents), the verdict is cannot_tell."
    , ""
    , "Answer with one fenced ```json block holding one object, after any notes:"
    , "```json"
    , case mode of
        CheckAnswer -> "{\"verdict\": \"answered | cannot_tell\", \"answer\": \"<the answer, at most three sentences, written to be read in a hover>\", \"detail\": \"<optional: more, for a reader who wants all of it; or null>\", \"why\": \"<one sentence: what the answer rests on>\"" <> sourcesShape <> "}"
        _ -> "{\"verdict\": \"right | wrong | cannot_tell\", \"why\": \"<one sentence>\"" <> sourcesShape <> "}"
    , "```"
    , if mode == CheckAnswer
        then "For answered, give at least one source: the page the answer rests on."
        else "For right or wrong, give at least one source: the page the verdict rests on." ]

progressPrompt :: Sentence -> Text
progressPrompt s =
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
         , "- open: the code does not do it yet, and doing it as written would get what the step says. Cite the code the step would change when it exists; none is fine for a step that makes something new."
         , "- misfit: the step as written rests on something that is not so in the code, or doing it as written would not get what it says: the means it names (a field, a flag, a filter, a file) does not cover what the step is for. Read what the named means covers in the code before you answer open. Say in why what is not so, and cite the code that shows it. A step that also states something about the code now is a misfit when that statement is false."
         , "Name roles, fields and functions as the code names them."
         , "If the step points back at something (\"these new fields\", \"it\") that the section and lead do not name, take the most likely thing in the code the section is about, and judge that."
         , "If the code cannot settle it (it is about people, documents or outside services), the verdict is cannot_tell."
         , ""
         , "Answer with one fenced ```json block holding one object, after any notes:"
         , "```json"
         , "{\"verdict\": \"done | open | misfit | cannot_tell\", \"why\": \"<one sentence>\", \"evidence\": [{\"path\": \"<path relative to the current directory>\", \"unit\": \"<function or class name, or null>\", \"line\": <line number, or null>}]}"
         , "```"
         , "For done or misfit, give at least one evidence entry: the code the verdict rests on."
         , "In evidence, \"unit\" is the exact name of one function, class or method as the file defines it (Class.method for a method), or null. For anything else (a template, a heading, part of a function, several names) give null and the line. A unit the file does not define voids the verdict." ]
claimPrompt :: Sentence -> Text
claimPrompt s =
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
         ]
      ++ ["If the code cannot settle it (it is about people, documents or outside services), the verdict is cannot_tell."]
      ++ [ ""
         , "Answer with one fenced ```json block holding one object, after any notes:"
         , "```json"
         , "{\"verdict\": \"right | wrong | cannot_tell\", \"why\": \"<one sentence>\", \"evidence\": [" <> evidenceShape <> "]}"
         , "```"
         , "For right or wrong, give at least one evidence entry: the code the verdict rests on."
         , "In evidence, \"unit\" is the exact name of one function, class or method as the file defines it (Class.method for a method), or null. For anything else (a template, a heading, part of a function, several names) give null and the line. A unit the file does not define voids the verdict." ]

answerPrompt :: CheckAsk -> Sentence -> Text
answerPrompt ask s =
  T.unlines $
    [ "Answer one question from a software plan, from this repository (the current directory)."
    , ""
    , "Section: " <> (if T.null (snHeading s) then "(none)" else snHeading s) ]
      ++ ["Lead (the sentence or question above it): " <> l | Just l <- [snLead s]]
      ++ ["Question: " <> snText s]
      ++ ["Hint: the writer pointed at `" <> T.drop 1 (anchorText a) <> "`: a place to start, not the answer." | Just a <- [snAnchor s]]
      ++ (if null (ckBeliefs ask) then [] else ["", "THE WRITER'S BELIEFS UNDER IT:"] ++ map beliefLine (ckBeliefs ask) ++ ["Each belief gets its own check; answer the question, and say so where the code shows a belief is wrong."])
      ++ [ ""
         , "Use Read, Grep and Glob to find the code the question is about, and read it."
         , "Behaviour can live in any layer: server code, templates, and front-end scripts and styles. Before you conclude that something does not exist, search for it across the whole repository."
         , "Rule: answer from the code as it is now. If the question points back at something (\"it\", \"these\") that the section and lead do not name, take the most likely thing in the code the section is about."
         ]
      ++ [ "If the code cannot settle it (it is about people, opinions, plans or private documents, or what a library does that this repository does not show), the verdict is cannot_tell."
         , ""
         , "Answer with one fenced ```json block holding one object, after any notes:"
         , "```json"
         , "{\"verdict\": \"answered | cannot_tell\", \"answer\": \"<the answer, at most three sentences, written to be read in a hover>\", \"detail\": \"<optional: more, for a reader who wants all of it; or null>\", \"why\": \"<one sentence: what the answer rests on>\", \"evidence\": [" <> evidenceShape <> "]}"
         , "```"
         , "For answered, give at least one evidence entry: the code the answer rests on."
         , "In evidence, \"unit\" is the exact name of one function, class or method as the file defines it (Class.method for a method), or null. For anything else (a template, a heading, part of a function, several names) give null and the line. A unit the file does not define voids the verdict." ]

-- | A belief as the answer prompt gives it: its words, and its check.
beliefLine :: Belief -> Text
beliefLine (Belief b c) = "- " <> b <> " — " <> case c of
  BelievedRight u -> "right, about " <> u
  BelievedWrong why u -> "wrong (" <> why <> "), about " <> u
  NotChecked -> "not checked"

evidenceShape :: Text
evidenceShape = "{\"path\": \"<path relative to the current directory>\", \"unit\": \"<function or class name, or null>\", \"line\": <line number, or null>}"

sourcesShape :: Text
sourcesShape = ", \"sources\": [{\"url\": \"<the page's address>\", \"title\": \"<the page's title>\"}]"

-- | The last ```json block, else the first `{` from which the rest of the
-- reply decodes as an object. A verdict of another mode is refused, and
-- so is right, wrong, done, misfit or answered with neither evidence nor a
-- kept source, and answered with no answer. Sources are kept only for a web
-- check ('ckWeb'): an http(s) address with a host, at most five; a web check
-- rests on sources alone (its citations are dropped; none is no source).
parseCheckReply :: CheckAsk -> Text -> Either Text CheckReply
parseCheckReply ask reply = do
  v <- maybe (Left "no JSON object in the reply") Right (maybe bare Just fenced)
  o <- case v of
    Object o -> Right o
    _ -> Left "the reply's JSON is not an object"
  j <- case KM.lookup "verdict" o of
    Just (String t) ->
      let w = T.map toLower (T.replace " " "_" (T.strip t))
       in case judgementNamed w of
            Just j | j `elem` wordsOf (ckMode ask) -> Right j
            _ -> Left ("unknown verdict `" <> w <> "`")
    _ -> Left "no verdict"
  let why = case KM.lookup "why" o of
        Just (String t) -> T.strip t
        _ -> ""
      evidence = case KM.lookup "evidence" o of
        Just (Array xs) -> [c | Object e <- foldr (:) [] xs, Just c <- [citation e]]
        _ -> []
      text k = case KM.lookup k o of
        Just (String t) | not (T.null (T.strip t)) -> Just (T.strip t)
        _ -> Nothing
      answer = if j == JAnswered then text "answer" else Nothing
      sources = if not (ckWeb ask) then [] else case KM.lookup "sources" o of
        Just (Array xs) -> take 5 [x | Object e <- foldr (:) [] xs, Just x <- [source e]]
        _ -> []
  if j == JAnswered && isNothing answer then Left "an answer with no text"
  else if ckWeb ask && j `elem` [JRight, JWrong, JAnswered] && null sources then Left "a verdict with no source"
  else if j `elem` [JRight, JWrong, JDone, JMisfit, JAnswered] && null evidence && null sources then Left "a verdict with no evidence"
  -- a web check rests on pages alone: it saw no code, so it cites none
  else Right (CheckReply j why (if ckWeb ask then [] else evidence) answer (if j == JAnswered then text "detail" else Nothing) sources)
  where
    wordsOf = \case
      CheckClaim -> [JRight, JWrong, JCannotTell]
      CheckProgress -> [JDone, JOpen, JMisfit, JCannotTell]
      CheckAnswer -> [JAnswered, JCannotTell]
    -- an http(s) address with a host: never javascript:, file: or a repository path
    source e = case KM.lookup "url" e of
      Just (String u0)
        | let u = T.strip u0
        , any (`T.isPrefixOf` T.toLower u) ["http://", "https://"]
        , not (T.null (sourceDomain u))
        , T.all (\c -> c > ' ') u ->
            Just (Source u (case KM.lookup "title" e of
              Just (String t) | not (T.null (T.strip t)) -> T.strip t
              _ -> sourceDomain u))
      _ -> Nothing
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
-- A `.env*` file is never found (a secret is never evidence).
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
      then Confirmed JCannotTell (if crJudgement r == JCannotTell then crWhy r else "cited code not found") (crEvidence r) Nothing Nothing [] Nothing Nothing []
      else Confirmed (crJudgement r) (crWhy r) (map fst checked) (fst <$> unit) (snd <$> unit) prints (crAnswer r) (crDetail r) (crSources r)
  where
    root = prRoot p
    relOf path
      | isAbsolute path = if (root <> "/") `isPrefixOf` path then confined (makeRelative root path) else Nothing
      | otherwise = confined path
    -- a `.env*` file is never evidence: a secret, whatever the mode or web
    secret rel = ".env" `isPrefixOf` takeFileName rel
    fencedOff rel = any (\f -> let f' = normalisePath f in rel == f' || (f' <> "/") `isPrefixOf` rel) fs
    -- the path as the disk resolves it (symlinks followed): under the root
    -- and outside the fences, else not found
    resolvedOk rel = do
      canonRoot <- canonicalizePath root
      canon <- canonicalizePath (root </> rel)
      let inside = (canonRoot <> "/") `isPrefixOf` canon
          canonFenced = any (\f -> let f' = normalisePath f in (canonRoot </> f') == canon || ((canonRoot </> f') <> "/") `isPrefixOf` canon) fs
      pure (inside && not canonFenced && not (secret canon))
    cite c = case relOf (ciPath c) of
      Nothing -> pure Nothing
      Just rel0 -> do
        let rel = normalisePath rel0
        ok <- safely False (resolvedOk rel)
        if fencedOff rel || secret rel || not ok
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
-- A web check ('ckWeb') gets the web tools ('webTools') and runs outside the repository.
liveChecker :: Cancel -> PlansConfig -> FilePath -> Checker
liveChecker token cfg root ask s
  -- a web check runs in an empty directory: no repository file, CLAUDE.md or settings reaches it
  | ckWeb ask = withSystemTempDirectory "olifant-web-check" $ \dir -> run (webTools cfg) dir
  | otherwise = run [] root
  where
    run tools cwd = runClaude token cfg {pcTimeout = pcCheckTimeout cfg} Check tools Nothing cwd (checkPromptWith (pcWebFetch cfg) ask s)

-- | Recorded replies (tests): `{"replies": {KEY: TEXT or {"text", "cost", "seconds"}}}`
-- by 'sentenceKey' (an object may add "delay", seconds; "gate", a path the reply
-- waits for to exist; or "throw", a message the check raises as an IO error).
-- Each key asked is appended to FILE.asked and each prompt to
-- FILE.prompts; a key not in the file is a failure.
recordedChecker :: FilePath -> Checker
recordedChecker = recordedCheckerAs ""

-- | 'recordedChecker' for one model (the cascade's two): a reply under
-- "MODEL: KEY" is that model's, and is asked (in FILE.asked) under that
-- name; else the plain KEY's.
recordedCheckerAs :: Text -> FilePath -> Checker
recordedCheckerAs model file ask s = do
  let plain = sentenceKey (snText s)
      named = model <> ": " <> plain
  replies0 <- readReplies file
  let key = if not (T.null model) && KM.member (Key.fromText named) replies0 then named else plain
  -- checks run side by side (Check the plan): one writer at a time
  withMVar recordLock $ \() -> do
    appendFile (file <> ".asked") (T.unpack key <> "\n")
    BS.appendFile (file <> ".prompts") (TE.encodeUtf8 (checkPrompt ask s <> "\n"))
  replies <- readReplies file
  case KM.lookup (Key.fromText key) replies of
    Just (Object o) | Just (String why) <- KM.lookup "throw" o -> ioError (userError (T.unpack why))
    Just v | Just r <- resultOf v -> do
      -- "gate": the reply waits until the test makes this file (an order the test sets)
      case v of
        Object o | Just (String gate) <- KM.lookup "gate" o ->
          let go = doesFileExist (T.unpack gate) >>= \there -> if there then pure () else threadDelay 2000 >> go in go
        _ -> pure ()
      -- "delay": seconds the reply takes (a check in flight, for cancel tests)
      case v of
        Object o | Just (Number d) <- KM.lookup "delay" o -> threadDelay (round (toRealFloat d * 1000000 :: Double))
        _ -> pure ()
      pure (Right r)
    _ -> pure (Left (RunFailure Empty "no recorded reply" Nothing))

-- | As 'replayDecider': with no live checker, replay (a key not in the file
-- is a failure, noted in FILE.missed); with one, run it and record its reply.
replayChecker :: FilePath -> Maybe Checker -> Checker
replayChecker file live ask s = do
  let key = sentenceKey (snText s)
  replies <- readReplies file
  case live of
    Nothing -> case KM.lookup (Key.fromText key) replies >>= resultOf of
      Just r -> pure (Right r)
      Nothing -> Left (RunFailure Empty "no recorded reply" Nothing) <$ appendFile (file <> ".missed") (T.unpack key <> "\n")
    Just run ->
      run ask s >>= \case
        Left f -> pure (Left f)
        Right r -> withMVar recordLock $ \() -> do
          replies' <- readReplies file
          let entry = object (["text" .= rrText r, "seconds" .= rrSeconds r, "models" .= map fst (rrModels r)] ++ ["cost" .= c | Just c <- [rrCost r]])
          BL.writeFile file (encode (object ["replies" .= Object (KM.insert (Key.fromText key) entry replies')]) <> "\n")
          pure (Right r)

-- | The keys 'recordedChecker' was asked, in order (tests), read under its
-- writers' lock: in one process GHC refuses to open a file for reading while
-- it is open for appending ("resource busy"), so a test that reads FILE.asked
-- while checks run side by side must read it here.
recordedAsked :: FilePath -> IO [Text]
recordedAsked file = withMVar recordLock $ \() -> do
  there <- doesFileExist (file <> ".asked")
  if there then T.lines . TE.decodeUtf8 <$> BS.readFile (file <> ".asked") else pure []

{-# NOINLINE recordLock #-}
recordLock :: MVar ()
recordLock = unsafePerformIO (newMVar ())

-- | The recorded checker when the server has `scCheckAnswers` (tests), else the live one.
checkerUnder :: ServerState -> Cancel -> PlansConfig -> FilePath -> Checker
checkerUnder s token cfg root = maybe (liveChecker token cfg root) (recordedCheckerAs (pcCheckModel cfg)) (scCheckAnswers (ssConfig s))

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
