-- | Typed decisions (Jev, or any model), progressively: a question with
-- known options, an answer validated against them, "unsure" always
-- allowed. Asked only where the deterministic answer is unsure; everything
-- works without it (spec: strangler-design.md §3.4).
module Olifant.Decide
  ( Question (..)
  , Answer (..)
  , Decider (..)
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

-- | What is asked: its kind (what decision), the options, typed details
-- wanted (name, type: ident | text), and the context it decides from.
data Question = Question {qKind :: Text, qOptions :: [Text], qFields :: [(Text, Text)], qContext :: [(Text, Text)]}
  deriving stock (Eq, Show)

-- | An option, with its typed details; or unsure.
data Answer = Chosen Text [(Text, Text)] | Unsure
  deriving stock (Eq, Show)

newtype Decider m = Decider {ask :: Question -> m Answer}

-- | No model: always unsure (the deterministic answer stands).
noDecider :: Applicative m => Decider m
noDecider = Decider (const (pure Unsure))

-- | An answer that is one of the options, with only the fields asked for,
-- each of its type; anything else is unsure.
validated :: Question -> Answer -> Answer
validated q = \case
  Chosen c fs
    | c `elem` qOptions q, all ok fs -> Chosen c fs
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
    ["Decide: " <> qKind q, "", "Options: " <> T.intercalate " | " (qOptions q) <> " | unsure"]
      ++ (if null (qFields q) then [] else ["Also give these fields: " <> T.intercalate ", " [k <> " (" <> t <> ")" | (k, t) <- qFields q]])
      ++ concat [["", "## " <> k, v] | (k, v) <- qContext q]
      ++ ["", "Answer with one JSON object and nothing else: {\"choice\": \"<an option, or unsure>\", \"fields\": {…}}"]

-- | A reply read leniently (the first JSON object in it), then validated.
decodeAnswer :: Question -> Text -> Answer
decodeAnswer q reply =
  let body = T.dropWhileEnd (/= '}') (T.dropWhile (/= '{') reply)
   in case eitherDecodeStrict (TE.encodeUtf8 body) of
        Right v -> validated q (fromValue v)
        Left _ -> Unsure

fromValue :: Value -> Answer
fromValue = \case
  String c | c /= "unsure" -> Chosen c []
  Object o | Just (String c) <- KM.lookup "choice" o, c /= "unsure" ->
    Chosen c [(Key.toText k, v) | Just (Object fs) <- [KM.lookup "fields" o], (k, String v) <- KM.toList fs]
  _ -> Unsure

-- | A configured backend, under the work's token: its answer validated; a
-- timeout or an error is unsure.
curlDecider :: Cancel -> Int -> Backend -> Decider IO
curlDecider token timeout backend = Decider $ \q ->
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
recordedDecider file token = Decider $ \q -> do
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
