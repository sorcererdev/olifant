-- | Internal to Olifant.Preview: test the fields through that unit's public module.
--
-- The picker's form: each input's value and where it came from, checked
-- against its type the way `olifant gen` checks it.
module Olifant.Preview.Fields (Field (..), Source (..), fieldsFor) where

import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Generator (Choices (..), GeneratorDef, GeneratorWith (..), InputProblem (..), InputSpec (..), InputType (..), Scalar (..), resolveInput)

-- | Where a field's value came from.
data Source = FromYou | FromPath | FromHere | OnlyChoice | FromDefault | FromNew | FromSuggested
  deriving stock (Eq, Show)

data Field = Field
  { fName :: Text
  , fValue :: Maybe Text
  , fSource :: Maybe Source
  , fValid :: Bool
  , -- | why the value is not valid (none for a missing value)
    fWhy :: Maybe Text
  , -- | an enum's options, when known
    fChoices :: Maybe [Text]
  , -- | inputs a dependent choice waits on
    fNeeds :: [Text]
  , -- | generators that make a new value (`new`)
    fNew :: [Text]
  }
  deriving stock (Eq, Show)

-- | The form as it stands: the user's values, then what the file's path
-- gives, then what the cursor's action gives, then a lone choice, then a
-- default; each checked against its type.
fieldsFor :: GeneratorDef -> [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)] -> [Field]
fieldsFor d user path cursor = map field (genInputs d)
  where
    field i =
      let name = inputName i
          found =
            listToMaybe
              ( [(v, FromYou) | Just v <- [lookup name user]]
                  ++ [(v, FromPath) | Just v <- [lookup name path]]
                  ++ [(v, FromHere) | Just v <- [lookup name cursor]]
                  ++ [(v, OnlyChoice) | Just [v] <- [choicesOf i]]
                  ++ [(v, FromDefault) | Just v <- [inputDefault i]]
              )
          (valid, why) = case found of
            Nothing -> (inputOptional i, Nothing)
            Just (v, _) -> case resolveInput (M.singleton name v) i of
              Right _ -> (True, Nothing)
              Left (InvalidInput _ reason) -> (False, Just reason)
              Left _ -> (False, Nothing)
       in case (found, inputType i) of
            -- a dependent choice's value its inputs no longer offer: cleared, with a note
            (Just (v, _), ScalarInput (EnumOf q _ (Resolved cs ctx)))
              | not valid, not (null ctx), v `notElem` cs ->
                  Field name Nothing Nothing False (Just ("cleared " <> v <> ": not one of the " <> q <> " of " <> T.intercalate ", " [k <> "=" <> x | (k, x) <- ctx])) (choicesOf i) (needsOf i) (map fst (inputNew i))
            _ -> Field name (fst <$> found) (snd <$> found) valid why (choicesOf i) (needsOf i) (map fst (inputNew i))
    choicesOf i = case inputType i of
      ScalarInput (EnumS cs) -> Just cs
      ScalarInput (EnumOf _ _ (Resolved cs _)) -> Just cs
      _ -> Nothing
    needsOf i = case inputType i of
      ScalarInput (EnumOf _ _ (NeedsInputs ns)) -> ns
      _ -> []
