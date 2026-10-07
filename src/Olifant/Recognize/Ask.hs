-- | Internal to Olifant.Recognize: asking a model (Jev, or a local LLM) when
-- the deterministic answer is unsure — a hint, a choice, a rival shape. A
-- typed question: which of these conventions is this code, with which
-- inputs. Optional and never trusted blindly: inputs are validated, a
-- malformed or missing answer is "unsure", nothing waits on it.
module Olifant.Recognize.Ask
  ( Verdict (..)
  , Recognizer (..)
  , noRecognizer
  , unsure
  , consult
  , recognizerPrompt
  , decodeVerdicts
  , curlRecognizer
  ) where

import Data.Aeson (Value (..), eitherDecodeStrict)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.List (nub)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Olifant.Generator (Choices (..), EditWith (..), Generator, GeneratorId (..), GeneratorWith (..), InputSpec (..), InputType (..), Scalar (..), generatorName)
import Olifant.Model (Backend, askBackend)
import Olifant.Process (Cancel)
import Olifant.Recognize.Index (sampleText)
import Olifant.Recognize.Inputs (validInputs)
import Olifant.Recognize.Types (Band (..), Change (..), Recognition (..))

-- | A model's answer for one candidate.
data Verdict = Yes [(Text, Text)] | No | Unsure
  deriving stock (Eq, Show)

-- | Is this change any of these conventions? One verdict per candidate.
newtype Recognizer m = Recognizer {decide :: Change -> [Generator] -> m [Verdict]}

-- | No model: always unsure.
noRecognizer :: Applicative m => Recognizer m
noRecognizer = Recognizer (\_ cands -> pure (map (const Unsure) cands))

-- | Is the deterministic answer unsure — a hint, a choice, a shape that
-- favours another convention?
unsure :: Recognition -> Bool
unsure r = rcBand r == Hint || isJust (rcOr r) || isJust (rcRival r)

-- | A recognition, sharpened by a model when unsure: a yes makes it an offer
-- (for that convention, with the inputs it read, validated), all no make it
-- nothing, anything else leaves it as it was.
consult :: Monad m => Recognizer m -> [Generator] -> Change -> Recognition -> m (Maybe Recognition)
consult recognizer gens change r
  | not (unsure r) = pure (Just r)
  | null candidates = pure (Just r)
  | otherwise = do
      verdicts <- decide recognizer change candidates
      pure $ case [(g, inputs) | (g, Yes inputs) <- zip candidates verdicts] of
        (g, inputs) : _
          | genId g == rcGenerator r ->
              Just r {rcBand = Offer, rcInputs = merge (validInputs g inputs) (rcInputs r), rcOr = Nothing, rcRival = Nothing, rcReasons = rcReasons r <> ["the model agrees"]}
          | otherwise ->
              Just r {rcGenerator = genId g, rcName = generatorName g, rcBand = Offer, rcInputs = validInputs g inputs, rcOr = Nothing, rcRival = Nothing, rcReasons = ["the model says it is a " <> T.toLower (generatorName g)]}
        []
          | length verdicts == length candidates && all (== No) verdicts -> Nothing
          | otherwise -> Just r
  where
    ids = nub (rcGenerator r : maybe [] pure (rcOr r) <> maybe [] (pure . fst) (rcRival r))
    candidates = mapMaybe (\i -> listToMaybe [g | g <- gens, genId g == i]) ids
    merge new old = new <> [(k, v) | (k, v) <- old, k `notElem` map fst new]

-- | The question: the candidates (id, name, description, inputs with their
-- types and choices, the opening of each file they make), then the code,
-- and the answer's JSON.
recognizerPrompt :: Change -> [Generator] -> Text
recognizerPrompt (Change path text) cands =
  T.unlines $
    ["Someone is writing code in their project. Is it an instance of one of the project's conventions below, and with which inputs?", ""]
      <> concatMap candidate cands
      <> [ "The code, in " <> T.pack path <> ":"
         , "```"
         , T.unlines (take 200 (T.lines text))
         , "```"
         , ""
         , "Answer with JSON only: {\"answers\": [{\"generator\": \"ID\", \"is\": \"yes\" | \"no\" | \"unsure\", \"inputs\": {\"NAME\": \"VALUE\"}}]} — one answer per convention, inputs only for a yes, values as names in snake_case."
         ]
  where
    candidate g =
      [ "Convention " <> generatorIdText (genId g) <> " — " <> generatorName g <> ": " <> genDescription g
      , "Inputs: " <> T.intercalate ", " (map input (genInputs g))
      ]
        <> concat [["It writes a file that begins:", "```", T.unlines (take 20 (T.lines (sampleText t))), "```"] | Create {editSource = t} <- take 1 [e | e@Create {editSource = s} <- genEdits g, not (T.null (T.strip (sampleText s)))]]
        <> [""]
    input i = inputName i <> " (" <> kind (inputType i) <> ")"
    kind = \case
      ScalarInput IdentS -> "ident"
      ScalarInput TextS -> "text"
      ScalarInput BoolS -> "bool"
      ScalarInput (EnumS cs) -> "enum: " <> T.intercalate ", " cs
      ScalarInput (EnumOf _ _ (Resolved cs _)) -> "enum: " <> T.intercalate ", " cs
      ScalarInput (EnumOf {}) -> "enum"
      ListInput _ -> "list"

-- | The verdicts in a model's answer, one per candidate (a missing one is
-- unsure); the JSON may be wrapped in prose or a fence.
decodeVerdicts :: [GeneratorId] -> Text -> Either Text [Verdict]
decodeVerdicts ids reply = case eitherDecodeStrict (TE.encodeUtf8 (braces reply)) of
  Right (Object o) | Just (Array as) <- KM.lookup "answers" o -> Right [verdictFor i (toList as) | i <- ids]
  _ -> Left "the model's answer is not the JSON asked for"
  where
    braces t = case (T.breakOn "{" t, T.breakOnEnd "}" t) of
      ((_, fromOpen), (toClose, _)) | not (T.null fromOpen), not (T.null toClose) -> T.take (T.length fromOpen - (T.length t - T.length toClose)) fromOpen
      _ -> t
    verdictFor (GeneratorId i) answers = case [a | Object a <- answers, KM.lookup "generator" a == Just (String i)] of
      a : _ -> case KM.lookup "is" a of
        Just (String "yes") -> Yes [(Key.toText k, v) | Just (Object ins) <- [KM.lookup "inputs" a], (k, String v) <- KM.toList ins]
        Just (String "no") -> No
        _ -> Unsure
      [] -> Unsure

-- | A model recogniser over a configured backend: unsure for every
-- candidate when it does not answer, or answers something else.
curlRecognizer :: Cancel -> Int -> Backend -> Recognizer IO
curlRecognizer token timeout backend = Recognizer $ \change cands ->
  askBackend token timeout backend system (recognizerPrompt change cands) >>= \case
    Left _ -> pure (map (const Unsure) cands)
    Right reply -> pure (either (const (map (const Unsure) cands)) id (decodeVerdicts (map genId cands) reply))
  where
    system = "You recognise which of a project's conventions a piece of code is. Answer only with the JSON asked for."
