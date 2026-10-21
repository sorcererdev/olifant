-- | Typed decisions, progressively (Jev-shaped; today any chat model behind `[decide] model`): a question with
-- known options, an answer validated against them, "unsure" always
-- allowed. Asked only where the deterministic answer is unsure; everything
-- works without it (spec: strangler-design.md §3.4).
module Olifant.Decide
  ( Shape (..)
  , Question (..)
  , Confidence (..)
  , Answer (..)
  , Decider (..)
  , decider
  , choice
  , noul
  , score
  , qOptions
  , confident
  , fromValue
  , noDecider
  , validated
  , decisionPrompt
  , decodeAnswer
  , curlDecider
  , recordedDecider
  ) where

import Control.Concurrent (threadDelay)
import Data.Aeson (Value (..), eitherDecodeStrict)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Olifant.Model (Backend, askBackend)
import Olifant.Naming (parseName)
import Olifant.Process (Cancel, cancelled)

-- | What kind of answer is wanted: one of named options (each with what it
-- means), a yes or no, or a level among described ones.
data Shape = Choice [(Text, Text)] | Noul | Score [Text]
  deriving stock (Eq, Show)

-- | What is asked: its kind (what decision), the instructions, the shape of
-- the answer, typed details wanted (name, type: ident | text), and the
-- context it decides from.
data Question = Question
  { qKind :: Text
  , qInstructions :: Text
  , qShape :: Shape
  , qFields :: [(Text, Text)]
  , qContext :: [(Text, Text)]
  }
  deriving stock (Eq, Show)

-- | How sure the answerer is, when it says.
data Confidence = Confidence Double | Unknown
  deriving stock (Eq, Show)

-- | An option with its typed details; a yes or a no; a level (its index and
-- its score); or unsure.
data Answer
  = Chosen Text [(Text, Text)] Confidence
  | Yes Confidence
  | No Confidence
  | Level Int Double Confidence
  | Unsure
  deriving stock (Eq, Show)

data Decider m = Decider {ask :: Question -> m Answer, askAll :: [Question] -> m [Answer]}

-- | A decider from one question at a time; a batch is asked in order.
decider :: Applicative m => (Question -> m Answer) -> Decider m
decider f = Decider f (traverse f)

-- | Kind, instructions, the options (name, meaning), the context.
choice :: Text -> Text -> [(Text, Text)] -> [(Text, Text)] -> Question
choice kind instructions options = Question kind instructions (Choice options) []

-- | Kind, instructions, the context: a yes or a no.
noul :: Text -> Text -> [(Text, Text)] -> Question
noul kind instructions = Question kind instructions Noul []

-- | Kind, instructions, the described levels (from 0), the context.
score :: Text -> Text -> [Text] -> [(Text, Text)] -> Question
score kind instructions levels = Question kind instructions (Score levels) []

-- | The option names a question allows (the levels' indices for a score).
qOptions :: Question -> [Text]
qOptions q = case qShape q of
  Choice os -> map fst os
  Noul -> ["yes", "no"]
  Score ls -> [T.pack (show i) | (i, _) <- zip [0 :: Int ..] ls]

-- | Is the answer known to be at least this sure?
confident :: Double -> Answer -> Bool
confident gate = \case
  Chosen _ _ c -> sure c
  Yes c -> sure c
  No c -> sure c
  Level _ _ c -> sure c
  Unsure -> False
  where
    sure (Confidence x) = x >= gate
    sure Unknown = False

-- | No model: always unsure (the deterministic answer stands).
noDecider :: Applicative m => Decider m
noDecider = decider (const (pure Unsure))

-- | An answer fitting the question's shape (an option, with only the fields
-- asked for, each of its type; a yes or no for a noul; a level in range);
-- anything else is unsure.
validated :: Question -> Answer -> Answer
validated q a = case (qShape q, a) of
  (Choice _, Chosen c fs _) | c `elem` qOptions q, all ok fs -> a
  (Noul, Yes _) -> a
  (Noul, No _) -> a
  (Score ls, Level n _ _) | n >= 0, n < length ls -> a
  _ -> Unsure
  where
    ok (k, v) = case lookup k (qFields q) of
      Just "ident" -> either (const False) (const True) (parseName v)
      Just _ -> True
      Nothing -> False

-- | The question as a prompt: one JSON object back.
decisionPrompt :: Question -> Text
decisionPrompt q =
  T.unlines $
    ["Decide: " <> qKind q] ++ [qInstructions q | not (T.null (qInstructions q))]
      ++ [""]
      ++ shapeLines
      ++ (if null (qFields q) then [] else ["Also give these fields: " <> T.intercalate ", " [k <> " (" <> t <> ")" | (k, t) <- qFields q]])
      ++ concat [["", "## " <> k, v] | (k, v) <- qContext q]
      ++ ["", "Answer with one JSON object and nothing else: {\"choice\": " <> answerHint <> ", \"confidence\": <0 to 1>, \"fields\": {…}}"]
  where
    shapeLines = case qShape q of
      Choice os -> ["Options: " <> T.intercalate " | " (map fst os) <> " | unsure"] ++ [k <> ": " <> m | (k, m) <- os, not (T.null m)]
      Noul -> ["Options: yes | no | unsure"]
      Score ls -> "Levels (give the number), or unsure:" : [T.pack (show i) <> ": " <> l | (i, l) <- zip [0 :: Int ..] ls]
    answerHint = case qShape q of
      Choice _ -> "\"<an option, or unsure>\""
      Noul -> "<true, false, or \"unsure\">"
      Score _ -> "<a level number, or \"unsure\">"

-- | A reply read leniently (the first JSON object in it), then validated.
decodeAnswer :: Question -> Text -> Answer
decodeAnswer q reply =
  let body = T.dropWhileEnd (/= '}') (T.dropWhile (/= '{') reply)
   in case eitherDecodeStrict (TE.encodeUtf8 body) of
        Right v -> validated q (fromValue v)
        Left _ -> Unsure

-- | A bare string, boolean or number, or an object with a "choice" and
-- optionally "fields" and a "confidence". Not yet validated.
fromValue :: Value -> Answer
fromValue = \case
  Object o
    | Just v <- KM.lookup "choice" o ->
        let fields = [(Key.toText k, x) | Just (Object fs) <- [KM.lookup "fields" o], (k, String x) <- KM.toList fs]
            confidence = case KM.lookup "confidence" o of
              Just (Number n) -> Confidence (realToFrac n)
              _ -> Unknown
         in bare v fields confidence
  v -> bare v [] Unknown
  where
    bare v fields confidence = case v of
      String c | c /= "unsure" -> Chosen c fields confidence
      Bool True -> Yes confidence
      Bool False -> No confidence
      Number n -> Level (round n) (realToFrac n) confidence
      _ -> Unsure

-- | A configured backend, under the work's token: its answer validated; a
-- timeout or an error is unsure.
curlDecider :: Cancel -> Int -> Backend -> Decider IO
curlDecider token timeout backend = decider $ \q ->
  askBackend token timeout backend system (decisionPrompt q) >>= \case
    Left _ -> pure Unsure
    Right reply -> pure (decodeAnswer q reply)
  where
    system = "You make one typed decision about a codebase. Answer only with the JSON asked for."

-- | Recorded answers (tests): `{"answers": {KIND: CHOICE or {choice, fields}}}`;
-- "wait" waits until the file holds an answer, or the work is cancelled
-- (then unsure). Each question asked
-- is noted in FILE.asked.
recordedDecider :: FilePath -> Cancel -> Decider IO
recordedDecider file token = decider $ \q -> do
  appendFile (file <> ".asked") (T.unpack (qKind q) <> "\n")
  -- "wait": until the file holds an answer, or the work is cancelled
  let current = T.strip . T.pack <$> readFile file
      loop = do
        answer <- current
        stop <- cancelled token
        if answer /= "wait" then pure (Just answer) else if stop then pure Nothing else threadDelay 20000 >> loop
  loop >>= \case
    Nothing -> pure Unsure
    Just answer -> pure $ case eitherDecodeStrict (TE.encodeUtf8 answer) of
      Right (Object o) | Just (Object as) <- KM.lookup "answers" o, Just v <- KM.lookup (Key.fromText (qKind q)) as -> validated q (fromValue v)
      _ -> Unsure
