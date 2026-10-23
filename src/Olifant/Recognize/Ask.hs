-- | Internal to Olifant.Recognize: asking the decider (`[decide] model`;
-- `[recognize] model` is its alias) when the deterministic answer is unsure
-- — a hint, a choice, a rival shape. One typed question, the `recognize`
-- Choice: which of these conventions is this code, with which inputs.
-- Optional and never trusted blindly: the answer is validated, a malformed
-- or missing one is unsure, nothing waits on it.
module Olifant.Recognize.Ask
  ( unsure
  , recognizeQuestion
  , consult
  ) where

import Data.List (nub)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text qualified as T
import Olifant.Decide (Answer (..), Decider (..), Question (..), choice)
import Olifant.Generator (Generator, GeneratorWith (..), InputSpec (..), InputType (..), Scalar (..), generatorIdText, generatorName)
import Olifant.Recognize.Inputs (validInputs)
import Olifant.Recognize.Types (Band (..), Change (..), Recognition (..))

-- | Is the deterministic answer unsure — a hint, a choice, a shape that
-- favours another convention?
unsure :: Recognition -> Bool
unsure r = rcBand r == Hint || isJust (rcOr r) || isJust (rcRival r)

-- | The candidates an unsure recognition is between: its convention, its
-- alternative, its rival.
candidatesOf :: [Generator] -> Recognition -> [Generator]
candidatesOf gens r = mapMaybe (\i -> listToMaybe [g | g <- gens, genId g == i]) ids
  where
    ids = nub (rcGenerator r : maybe [] pure (rcOr r) <> maybe [] (pure . fst) (rcRival r))

-- | The `recognize` choice: the candidates (generator id, its summary; the
-- backend adds unsure), the inputs of the best one it could not read as
-- fields, from the path and the code; the instructions name each. Nothing
-- when no candidate is known.
recognizeQuestion :: [Generator] -> Change -> Recognition -> Maybe Question
recognizeQuestion gens (Change path text) r = case candidatesOf gens r of
  [] -> Nothing
  cands@(best : _) ->
    Just (choice "recognize" instructions (map described cands) [("path", T.pack path), ("code", T.unlines (take 200 (T.lines text)))])
      { qFields = [(inputName i, fieldType (inputType i)) | i <- genInputs best, ScalarInput _ <- [inputType i], inputName i `notElem` map fst (rcInputs r)]
      }
  where
    instructions = "Which of these generators is the code in code, at path, a hand-written instance of? Give the inputs asked for as identifiers from code."
    described g = (generatorIdText (genId g), fromMaybe (generatorName g) (genSummary g))
    fieldType = \case
      ScalarInput TextS -> "text"
      _ -> "ident"

-- | A recognition, sharpened by the decider when unsure: a convention
-- chosen makes it an offer (for that convention, with the inputs it read,
-- validated); anything else leaves it as it was.
consult :: Monad m => Decider m -> [Generator] -> Change -> Recognition -> m (Maybe Recognition)
consult d gens change r
  | not (unsure r) = pure (Just r)
  | otherwise = case recognizeQuestion gens change r of
      Nothing -> pure (Just r)
      Just q ->
        ask d q >>= \answer -> pure . Just $ case answer of
          Chosen c inputs _
            | g : _ <- [g | g <- candidatesOf gens r, generatorIdText (genId g) == c] ->
                if genId g == rcGenerator r
                  then r {rcBand = Offer, rcInputs = merge (validInputs g inputs) (rcInputs r), rcOr = Nothing, rcRival = Nothing, rcReasons = rcReasons r <> ["the model agrees"]}
                  else r {rcGenerator = genId g, rcName = generatorName g, rcBand = Offer, rcInputs = validInputs g inputs, rcOr = Nothing, rcRival = Nothing, rcReasons = ["the model says it is a " <> T.toLower (generatorName g)]}
          _ -> r
  where
    merge new old = new <> [(k, v) | (k, v) <- old, k `notElem` map fst new]
