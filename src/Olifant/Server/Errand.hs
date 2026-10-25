-- | The runner for plan errands: one `claude -p` call, its JSON reply read
-- into text, seconds, cost, session, turns, tokens, models and denials.
-- Nothing here throws; every failure is a Left with a status and a short reason.
module Olifant.Server.Errand
  ( Verb (..), RunResult (..), Tokens (..), ModelUse (..), Status (..), RunFailure (..)
  , statusName, runnerArgs, runClaude
  , Grounded (..), Ghost (..), Block (..)
  , errandPrompt, parseAnswer, groundingLimit, cutBody
  ) where

import Data.Aeson (Value (..), decodeStrict)
import Data.ByteString (ByteString)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Scientific (toRealFloat)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Olifant.Catalog (PlansConfig (..))
import Olifant.Plans (Anchor (..), Kind (..), Line (..), Section (..), anchorText, childrenOf, parsePlan, sectionOf)
import Olifant.Process (Cancel, runBytesCancellable)
import Olifant.Process qualified as P
import System.Exit (ExitCode (..))

data Verb = Ask | Validate | Do deriving (Eq, Show)

-- | Token counts; @tkCacheWrite@ is the runner's @cache_creation_input_tokens@.
data Tokens = Tokens {tkInput, tkOutput, tkCacheWrite, tkCacheRead :: Int}
  deriving (Eq, Show)

-- | One model's share of a run (the runner's @modelUsage@ entry).
data ModelUse = ModelUse {muTokens :: Tokens, muCost :: Maybe Double}
  deriving (Eq, Show)

data RunResult = RunResult
  { rrText :: Text, rrSeconds :: Double, rrCost :: Maybe Double
  , rrSession :: Maybe Text, rrTurns :: Maybe Int, rrTokens :: Maybe Tokens
  , rrModels :: [(Text, ModelUse)], rrDenials :: Int }
  deriving (Eq, Show)

-- | How an errand ended. 'runClaude' never returns 'Ok' or 'Empty' as a failure;
-- 'Empty' (answered, nothing in the grammar) is the caller's to set.
data Status = Ok | Empty | Error | Timeout | Cancelled deriving (Eq, Show)

-- | 'rfSession' is the runner's @session_id@ when it answered but reported
-- @is_error@; Nothing when it never answered (timeout, cancel, exit, exec).
data RunFailure = RunFailure {rfStatus :: Status, rfWhy :: Text, rfSession :: Maybe Text}
  deriving (Eq, Show)

statusName :: Status -> Text
statusName = \case
  Ok -> "ok"
  Empty -> "empty"
  Error -> "error"
  Timeout -> "timeout"
  Cancelled -> "cancelled"

-- | The flags after the runner's name; @testTools@ are the project's test commands (for 'Do').
runnerArgs :: PlansConfig -> Verb -> [Text] -> [String]
runnerArgs cfg verb testTools =
  ["-p", "--output-format", "json", "--model", T.unpack (pcModel cfg)] ++ case verb of
    Do -> ["--permission-mode", "acceptEdits", "--allowedTools", T.unpack (T.intercalate "," (["Read", "Grep", "Glob", "Edit", "Write"] ++ testTools ++ pcDoTools cfg))]
    _ -> ["--allowedTools", "Read,Grep,Glob"]

runClaude :: Cancel -> PlansConfig -> Verb -> [Text] -> FilePath -> Text -> IO (Either RunFailure RunResult)
runClaude token cfg verb testTools root prompt =
  runBytesCancellable token (Just (pcTimeout cfg)) "env" (["-u", "CLAUDECODE", pcRunner cfg] ++ runnerArgs cfg verb testTools) (Just root) (TE.encodeUtf8 prompt) >>= \case
    Left why -> pure (Left (RunFailure Error why Nothing))
    Right P.Cancelled -> pure (Left (RunFailure Cancelled "cancelled" Nothing))
    Right P.TimedOut -> pure (Left (RunFailure Timeout ("timed out after " <> T.pack (show (pcTimeout cfg)) <> " s") Nothing))
    Right (P.Finished (ExitFailure n) out err) ->
      pure (Left (RunFailure Error (T.pack (pcRunner cfg) <> " exited " <> T.pack (show n) <> ": " <> first120 out err) Nothing))
    Right (P.Finished ExitSuccess out err) -> pure $ case decodeStrict out of
      Just (Object o) -> fromObject o
      _ -> Left (RunFailure Error (first120 out err) Nothing)

fromObject :: KM.KeyMap Value -> Either RunFailure RunResult
fromObject o = case KM.lookup "result" o of
  _ | KM.lookup "is_error" o == Just (Bool True) -> Left (RunFailure Error (T.take 120 (maybe "the runner reported an error" asText (KM.lookup "result" o))) session)
  Just (String t) ->
    Right
      RunResult
        { rrText = t, rrSeconds = maybe 0 (/ 1000) (num "duration_ms" o), rrCost = num "total_cost_usd" o
        , rrSession = session
        , rrTurns = round <$> num "num_turns" o
        , rrTokens = case KM.lookup "usage" o of
            Just (Object u) -> Just (tokens "input_tokens" "output_tokens" "cache_creation_input_tokens" "cache_read_input_tokens" u)
            _ -> Nothing
        , rrModels = case KM.lookup "modelUsage" o of
            Just (Object m) -> [(K.toText k, modelUse u) | (k, Object u) <- KM.toList m]
            _ -> []
        , rrDenials = case KM.lookup "permission_denials" o of
            Just (Array a) -> length a
            _ -> 0 }
  _ -> Left (RunFailure Error (T.take 120 (T.pack (show (Object o)))) session)
  where
    session = case KM.lookup "session_id" o of
      Just (String s) -> Just s
      _ -> Nothing
    num k m = case KM.lookup (K.fromText k) m of
      Just (Number n) -> Just (toRealFloat n :: Double)
      _ -> Nothing
    count k m = maybe 0 round (num k m)
    tokens i out w r m = Tokens (count i m) (count out m) (count w m) (count r m)
    modelUse u = ModelUse (tokens "inputTokens" "outputTokens" "cacheCreationInputTokens" "cacheReadInputTokens" u) (num "costUSD" u)
    asText = \case
      String t -> t
      v -> T.pack (show v)

first120 :: ByteString -> ByteString -> Text
first120 out err = T.take 120 (T.strip (TE.decodeUtf8Lenient (out <> err)))

-- | A resolved unit's text for the prompt.
data Grounded = Grounded {grAnchor :: Anchor, grHeader :: Text, grBody :: Text}
  deriving (Eq, Show)

-- | One line to insert; @ghAnchor@ is found in @ghText@.
data Ghost = Ghost {ghText :: Text, ghAnchor :: Maybe Anchor}
  deriving (Eq, Show)

-- | Lines to insert after row @blAfterRow@; @blAfterText@ is the model's verbatim "AFTER:" text.
data Block = Block {blAfterRow :: Int, blAfterText :: Maybe Text, blLines :: [Ghost]}
  deriving (Eq, Show)

-- | At most this many units are grounded into one prompt.
groundingLimit :: Int
groundingLimit = 20

-- | A unit's body cut at 2000 characters, with a "…" line.
cutBody :: Text -> Text
cutBody b
  | T.length b > 2000 = T.take 2000 b <> "\n…"
  | otherwise = b

anchorRule, noProse :: Text
anchorRule =
  "An anchor is @<path>:<Unit>. <path> is relative to the project root and names a file of this repository;"
    <> " <Unit> is a top-level class or function name (Name) or a method (Class.method) defined in that file."
noProse = "No prose, no explanations, no code fences, no markdown headings, no blank lines."

errandPrompt :: Verb -> Text -> [Line] -> Line -> [Grounded] -> Text
errandPrompt verb plan ls target grounded =
  T.intercalate "\n" $
    ["<doc>", plan, "</doc>", ""]
      ++ ["<units>"] ++ concatMap unit grounded ++ ["</units>", ""]
      ++ [middle, "", contract, "", "Output only lines in this grammar, nothing else."]
  where
    unit g = [anchorText (grAnchor g), grBody g]
    body = T.strip (lnText target)
    strip1 pre = T.strip . maybe body id . T.stripPrefix pre
    middle = case (verb, lnKind target) of
      (Validate, Heading _) ->
        let sec = sectionOf ls (lnRow target)
         in T.intercalate "\n" $
              ["Validate one section of the doc above against this repository: read the code its claims and steps point at, and say what is wrong, missing or not yet asked.", "", "SECTION: " <> secTitle sec]
                ++ [lnText l | l <- ls, lnRow l >= secRow sec, lnRow l < secEnd sec]
      (Validate, _) -> "Validate this claim of the doc above against this repository: read the code it points at.\n\nCLAIM: " <> body
      (Do, TestChild) ->
        "Make this TEST of the plan above real in this repository: write its body so it checks the sentence, and make it pass.\n\nTEST: " <> strip1 "- test:" body
      (Do, _) ->
        T.intercalate "\n" $
          ["Do this STEP of the plan above in this repository.", "", "STEP: " <> T.strip (T.drop 5 body)]
            ++ map lnText (childrenOf ls (lnRow target))
      (Ask, _) -> "Answer the question below about this repository; read the code you need.\n\nQUESTION: " <> T.strip (T.drop 1 (T.dropWhile (/= '?') body))
    contract = T.intercalate "\n" $ case verb of
      Ask ->
        [ "Answer with 1 to 5 claim lines, each exactly:"
        , "  - <a claim, one short sentence> @<path>:<Unit>"
        , "using the anchor of the code that supports the claim; a claim may omit the anchor only if no code supports it."
        , "Then optionally one last line, exactly:"
        , "  - <the conclusion, one short sentence>"
        , anchorRule, noProse ]
      Validate ->
        [ "Output one or more blocks. A block is one line"
        , "AFTER: <a line of that section, copied verbatim>"
        , "followed by 1 to 3 lines to insert after it, each exactly one of:"
        , "- ? <a question the author should ask>"
        , "- [ ] <a missing step> · by hand"
        , "  - ✗ <why the claim above is wrong or incomplete>"
        , "  - ✓ <a confirmation, with its evidence> @<path>:<Unit>"
        , "The ✗ and ✓ lines go only after a claim line. At most 6 inserted lines in total."
        , anchorRule, noProse ]
      Do ->
        [ "Implement it here: edit the code, keep the change minimal, follow this repository's conventions,"
        , "and run the relevant tests. When you are finished, your final message is only"
        , "one line per unit you created or changed, exactly"
        , "DONE @<path>:<Unit>"
        , "or, if you cannot do it, one line, exactly"
        , "BLOCKED <why, one sentence>"
        , anchorRule, noProse ]

-- | Blocks and the count of dropped (out-of-grammar) lines. Total: never fails.
parseAnswer :: Verb -> [Line] -> Line -> Text -> ([Block], Int)
parseAnswer verb ls target out = case (verb, lnKind target) of
  (Ask, Question) -> parseAsk target rows
  (Validate, Heading _) -> parseValidate ls target rows
  (Validate, Claim) -> parseValidate ls target rows
  (Do, Step _) -> parseDo ls target rows
  (Do, TestChild) -> parseDo ls target rows
  _ -> ([], 0)
  where
    rows = filter (not . T.null) (map T.strip (T.lines (T.replace "\r" "" out)))

-- | "text   @anchor" with the anchor at the very end, text non-empty.
tailAnchor :: Text -> Maybe (Text, Anchor)
tailAnchor t = case findAnchorEnd of
  Just a | anEnd a == T.length t, let pre = T.strip (T.take (anStart a) t), not (T.null pre) -> Just (pre, a)
  _ -> Nothing
  where findAnchorEnd = anchorIn t

anchorIn :: Text -> Maybe Anchor
anchorIn t = case parsePlan t of
  (l : _) -> lnAnchor l
  [] -> Nothing

ghostOf :: Text -> Ghost
ghostOf t = Ghost t (anchorIn t)

pad :: Int -> Text -> Text
pad n = (T.replicate n " " <>)

parseAsk :: Line -> [Text] -> ([Block], Int)
parseAsk target rows = (blocks, dropped)
  where
    ind = lnIndent target + 2
    (kept, dropped, _, _) = foldl step ([], 0 :: Int, 0 :: Int, 0 :: Int) rows
    step (k, d, nA, nC) l = case itemText l of
      Just t | not ("?" `T.isPrefixOf` t), not ("[" `T.isPrefixOf` t) ->
        case tailAnchor t of
          Just (pre, a) | nA < 5 -> (k ++ [ghostOf (pad ind ("- " <> pre <> "   " <> anchorText a))], d, nA + 1, nC)
          Nothing | not (T.isInfixOf "@" t), nC < 1 -> (k ++ [Ghost (pad ind ("- " <> t)) Nothing], d, nA, nC + 1)
          _ -> (k, d + 1, nA, nC)
      _ -> (k, d + 1, nA, nC)
    blocks = [Block (lnRow target) Nothing kept | not (null kept)]

itemText :: Text -> Maybe Text
itemText l = T.strip <$> (T.stripPrefix "- " l `orElse` T.stripPrefix "* " l)
  where orElse a b = maybe b Just a

parseValidate :: [Line] -> Line -> [Text] -> ([Block], Int)
parseValidate ls target rows = (blocks, dropped)
  where
    sec = sectionOf ls (lnRow target)
    secLines = [l | l <- ls, lnRow l >= secRow sec, lnRow l < secEnd sec]
    lastRow = max (secRow sec) (secEnd sec - 1)
    -- state: finished blocks (reversed), current (after text, lines reversed), total, dropped
    (done, cur, _, dropped) = foldl step ([], Nothing, 0 :: Int, 0 :: Int) rows
    step (bs, c, tot, d) l
      | Just a <- T.stripPrefix "AFTER:" l =
          let a' = T.strip a
           in if T.null a' then (bs, Nothing, tot, d + 1) else (flush bs c, Just (a', []), tot, d)
      | Just (a, acc) <- c, length acc < 3, tot < 6, Just g <- item a l = (bs, Just (a, acc ++ [g]), tot + 1, d)
      | otherwise = (bs, c, tot, d + 1)
    flush bs c = maybe bs (: bs) c
    item after l
      | "- ? " `T.isPrefixOf` l || "- [ ] " `T.isPrefixOf` l && " · by hand" `T.isSuffixOf` l = Just (Ghost (pad (afterInd after) l) Nothing)
      | "- ✗ " `T.isPrefixOf` l = Just (Ghost (pad (afterInd after + 2) l) Nothing)
      | Just r <- T.stripPrefix "- ✓ " l, Just (pre, a) <- tailAnchor r = Just (ghostOf (pad (afterInd after + 2) ("- ✓ " <> pre <> "   " <> anchorText a)))
      | otherwise = Nothing
    afterInd after = maybe (lnIndent target) lnIndent (locate after)
    blocks = [Block (maybe lastRow lnRow (locate a)) (Just a) gs | (a, gs) <- reverse (flush done cur), not (null gs)]
    locate want =
      let tests =
            [ (== want)
            , \t -> T.length want > 8 && want `T.isPrefixOf` t
            , \t -> T.length want > 8 && want `T.isInfixOf` t ]
       in listToMaybe' [l | f <- tests, Just l <- [find (f . T.strip . lnText) secLines]]
    listToMaybe' = foldr (const . Just) Nothing

parseDo :: [Line] -> Line -> [Text] -> ([Block], Int)
parseDo ls target rows = (blocks, dropped)
  where
    stepLine = case lnKind target of
      TestChild -> maybe target id (find isStepAbove (reverse (takeWhile ((< lnRow target) . lnRow) ls)))
      _ -> target
    isStepAbove l = case lnKind l of
      Step _ -> lnIndent l < lnIndent target
      _ -> False
    ind = lnIndent stepLine + 2
    parsed = map classify rows
    classify l
      | Just r <- T.stripPrefix "DONE" l, T.isPrefixOf " " r || T.isPrefixOf "\t" r
      , Just a <- exactAnchor (T.strip r) = Right (Left a)
      | Just r <- T.stripPrefix "BLOCKED" l, T.isPrefixOf " " r, not (T.null (T.strip r)) = Right (Right (T.strip r))
      | "- ? " `T.isPrefixOf` l = Left (Just (Ghost (pad ind l) Nothing))
      | otherwise = Left Nothing
    exactAnchor r = case T.stripPrefix "@" r of
      Just _ -> case anchorIn r of
        Just a | anStart a == 0, anEnd a == T.length r -> Just a
        _ -> Nothing
      Nothing -> Nothing
    anchors = nub' [a | Right (Left a) <- parsed]
    nub' = foldl (\acc a -> if any (same a) acc then acc else acc ++ [a]) []
    same a b = anPath a == anPath b && anUnit a == anUnit b
    blocked = take 1 [w | Right (Right w) <- parsed]
    questions = [g | Left (Just g) <- parsed]
    usedDone = length [() | Right (Left _) <- parsed]
    usedBlocked = length [() | Right (Right _) <- parsed]
    dropped = length [() | Left Nothing <- parsed] + (usedDone - length anchors) + (usedBlocked - length blocked)
    unanchored = case lnAnchor stepLine of
      Nothing | Step _ <- lnKind stepLine -> True
      _ -> False
    doneGhosts = case anchors of
      (a : rest) | unanchored -> Ghost (T.stripEnd (lnText stepLine) <> "   " <> anchorText a) Nothing : map also rest
      as -> map also as
    also a = Ghost (pad ind ("- also " <> anchorText a)) (anchorIn (pad ind ("- also " <> anchorText a)))
    gs = fixFirst doneGhosts ++ [Ghost (pad ind ("- ✗ blocked: " <> w)) Nothing | w <- blocked] ++ questions
    fixFirst (g : rest) | unanchored, not (null anchors) = g {ghAnchor = anchorIn (ghText g)} : rest
    fixFirst x = x
    blocks = [Block (lnRow stepLine) Nothing gs | not (null gs)]
