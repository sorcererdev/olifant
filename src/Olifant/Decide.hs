-- | Typed decisions, progressively (Jev-shaped; Jev or any chat model behind `[decide] model`): a question with
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
  , jevBody
  , jevAnswers
  , jevWeighed
  , jevDecider
  , recordedDecider
  ) where

import Control.Concurrent (threadDelay)
import Data.Aeson (Value (..), eitherDecodeStrict, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.List (maximumBy, sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Scientific (toRealFloat)
import GHC.Clock (getMonotonicTime)
import Text.Read (readMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Olifant.Model (Backend (..), askBackend, sendJev)
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

-- | Ask one question, a batch in order, or one with the answer's
-- distribution (option, probability; highest first; none when the
-- answerer gives none).
data Decider m = Decider {ask :: Question -> m Answer, askAll :: [Question] -> m [Answer], weigh :: Question -> m (Answer, [(Text, Double)])}

-- | A decider from one question at a time; a batch is asked in order.
decider :: Applicative m => (Question -> m Answer) -> Decider m
decider f = Decider f (traverse f) (fmap (,[]) . f)

-- | A decider from one weighed question at a time.
weighed :: Applicative m => (Question -> m (Answer, [(Text, Double)])) -> Decider m
weighed f = Decider (fmap fst . f) (traverse (fmap fst . f)) f

-- | A distribution, highest first.
byWeight :: [(Text, Double)] -> [(Text, Double)]
byWeight = sortOn (Down . snd)

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

-- | A batch as Jev's body (without the model): one `state` of every
-- question's context (a name shared by two questions is one field, and must
-- have one value), and each question under its kind. A choice may always
-- be answered `unsure`.
jevBody :: [Question] -> Value
jevBody qs = object ["state" .= Object (KM.fromList [(Key.fromText k, String v) | (k, v) <- state]), "questions" .= Object (KM.fromList [(Key.fromText (qKind q), question q) | q <- qs])]
  where
    state = foldl merge [] (concatMap qContext qs)
    merge acc (k, v) = case lookup k acc of
      Nothing -> acc ++ [(k, v)]
      Just w
        | w == v -> acc
        | otherwise -> error ("jevBody: the context `" <> T.unpack k <> "` has two values in one batch (" <> T.unpack (T.intercalate ", " (map qKind qs)) <> ")")
    question q = case qShape q of
      Choice os -> object ["type" .= ("choice" :: Text), "instructions" .= qInstructions q, "criteria" .= Object (KM.fromList [(Key.fromText o, String d) | (o, d) <- os ++ [("unsure", "none of these fits")]])]
      Noul -> object ["type" .= ("noul" :: Text), "instructions" .= qInstructions q]
      Score ls -> object ["type" .= ("score" :: Text), "instructions" .= qInstructions q, "criteria" .= ls]

-- | Jev's reply, one answer a question in order, each validated. A reply
-- that is not JSON, lacks a question, or answers one outside its shape
-- makes the whole batch unsure.
jevAnswers :: [Question] -> ByteString -> [Answer]
jevAnswers qs = map fst . jevWeighed qs

-- | 'jevAnswers' with each choice's distribution (its `probabilities`,
-- highest first; none for other shapes or an unsure batch).
jevWeighed :: [Question] -> ByteString -> [(Answer, [(Text, Double)])]
jevWeighed qs raw = fromMaybe (map (const (Unsure, [])) qs) $ do
  Object o <- either (const Nothing) Just (eitherDecodeStrict raw)
  Object as <- KM.lookup "answers" o
  traverse (\q -> KM.lookup (Key.fromText (qKind q)) as >>= \v -> (,distribution q v) <$> answer q v) qs
  where
    distribution q = \case
      Object a | Choice _ <- qShape q, Just (Object ps) <- KM.lookup "probabilities" a -> byWeight [(Key.toText k, p) | (k, v) <- KM.toList ps, Just p <- [number v]]
      _ -> []
    answer q = \case
      Object a -> do
        raw' <- case (qShape q, KM.lookup "type" a) of
          (Choice _, Just "choice") -> case KM.lookup "choice" a of
            Just (String "unsure") -> Just Unsure
            Just (String c) -> Just (Chosen c [] (confidenceOf a))
            _ -> Nothing
          (Noul, Just "noul") -> do
            p <- number =<< KM.lookup "noul" a
            Just (if p >= 0.5 then Yes (Confidence p) else No (Confidence (1 - p)))
          (Score _, Just "score") -> do
            Object ps <- KM.lookup "probabilities" a
            levels <- traverse (\(k, v) -> (,) <$> readMaybe (T.unpack (Key.toText k)) <*> number v) (KM.toList ps)
            (n, _) <- if null levels then Nothing else Just (maximumBy (comparing snd) levels)
            Just (Level n (fromMaybe (fromIntegral n) (number =<< KM.lookup "score" a)) (confidenceOf a))
          _ -> Nothing
        case (raw', validated q raw') of
          (Unsure, _) -> Just Unsure
          (_, Unsure) -> Nothing
          (_, ok) -> Just ok
      _ -> Nothing
    number = \case
      Number n -> Just (toRealFloat n :: Double)
      _ -> Nothing
    confidenceOf a = maybe Unknown Confidence (number =<< KM.lookup "confidence" a)

-- | A Jev backend, under the work's token: a batch is one call; each call's
-- input tokens and time go to LOG as "decide\tjev\tN tokens\tM ms". No
-- key, an error, a timeout or a malformed reply is unsure for the batch.
jevDecider :: Cancel -> Int -> Backend -> (Text -> IO ()) -> Decider IO
jevDecider token timeout backend logLine = Decider (\q -> fst . head' <$> weighs [q]) (fmap (map fst) . weighs) (\q -> head' <$> weighs [q])
  where
    head' = \case
      a : _ -> a
      [] -> (Unsure, [])
    weighs [] = pure []
    weighs qs = do
      let body = case jevBody qs of
            Object o -> Object (KM.insert "model" (String (beModel backend)) o)
            v -> v
      start <- getMonotonicTime
      sendJev token timeout backend body >>= \case
        Left _ -> pure (map (const (Unsure, [])) qs)
        Right raw -> do
          end <- getMonotonicTime
          logLine (T.intercalate "\t" ["decide", "jev", tshow (tokens raw body) <> " tokens", tshow (round ((end - start) * 1000) :: Int) <> " ms"])
          pure (jevWeighed qs raw)
    -- the reply's count when it gives one; else about four bytes a token
    tokens raw body = case eitherDecodeStrict raw of
      Right (Object o) | Just (Object u) <- KM.lookup "usage" o, Just (Number n) <- KM.lookup "input_tokens" u -> round n :: Int
      _ -> fromIntegral (BL.length (encode body) `div` 4)
    tshow = T.pack . show

-- | Recorded answers (tests): `{"answers": {KIND: CHOICE or {choice, fields,
-- confidence, probabilities: {OPTION: P}}}}`;
-- "wait" waits until the file holds an answer, or the work is cancelled
-- (then unsure, noted in FILE.cancelled). Each question asked
-- is noted in FILE.asked.
recordedDecider :: FilePath -> Cancel -> Decider IO
recordedDecider file token = weighed $ \q -> do
  appendFile (file <> ".asked") (T.unpack (qKind q) <> "\n")
  -- "wait": until the file holds an answer, or the work is cancelled
  let current = T.strip . T.pack <$> readFile file
      loop = do
        answer <- current
        stop <- cancelled token
        if answer /= "wait" then pure (Just answer) else if stop then Nothing <$ writeFile (file <> ".cancelled") "" else threadDelay 20000 >> loop
  loop >>= \case
    Nothing -> pure (Unsure, [])
    Just answer -> pure $ case eitherDecodeStrict (TE.encodeUtf8 answer) of
      Right (Object o) | Just (Object as) <- KM.lookup "answers" o, Just v <- KM.lookup (Key.fromText (qKind q)) as -> (validated q (fromValue v), probabilities v)
      _ -> (Unsure, [])
  where
    probabilities = \case
      Object a | Just (Object ps) <- KM.lookup "probabilities" a -> byWeight [(Key.toText k, toRealFloat n) | (k, Number n) <- KM.toList ps]
      _ -> []
