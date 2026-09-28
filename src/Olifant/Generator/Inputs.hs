module Olifant.Generator.Inputs
  ( InputProblem (..)
  , EnvError (..)
  , buildEnv
  , describeEnvError
  , describeInputType
  ) where

import Control.Monad (foldM, unless, when, zipWithM)
import Data.Bifunctor (bimap)
import Data.Either (lefts, rights)
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Olifant.Generator.Definition
import Olifant.Naming (parseName)
import Olifant.Template (Env, Template, TemplateError, Value (..), describeTemplateError, evaluate, freeVariables)

data InputProblem = MissingInput InputSpec | InvalidInput Text Text | UnknownInput Text
  deriving stock (Eq, Show)

data EnvError = InputProblems [InputProblem] | DeriveFailed Text TemplateError | DeriveCycle [Text]
  deriving stock (Eq, Show)

-- | Resolve inputs (from --set values, defaults, or empties for optionals),
-- add @now@, and evaluate derived values. Reports every input problem at once.
buildEnv :: GeneratorWith s -> Map Text Text -> UTCTime -> Either EnvError Env
buildEnv gen sets now = do
  let derivedNames = map fst (genDerived gen)
      inputNames = map inputName (genInputs gen)
      unknown = [UnknownInput k | k <- M.keys sets, k `notElem` inputNames, k `notElem` derivedNames]
      resolved = map (resolveInput sets) (genInputs gen)
      problems = unknown ++ lefts resolved
  unless (null problems) (Left (InputProblems problems))
  let base = M.insert "now" (VTime now) (M.fromList (rights resolved))
      overrides = M.fromList [(k, v) | (k, v) <- M.toList sets, k `elem` derivedNames]
  deriveAll (genDerived gen) overrides base

resolveInput :: Map Text Text -> InputSpec -> Either InputProblem (Text, Value)
resolveInput sets spec = case maybe (inputDefault spec) Just (M.lookup name sets) of
  Just raw -> bimap (InvalidInput name) (name,) (parseValue (inputType spec) raw)
  Nothing
    | inputOptional spec -> Right (name, emptyValue (inputType spec))
    | otherwise -> Left (MissingInput spec)
  where
    name = inputName spec

parseValue :: InputType -> Text -> Either Text Value
parseValue (ScalarInput s) raw = parseScalar s raw
parseValue (ListInput item) raw = VList <$> traverse (parseItem item) items
  where
    items = filter (not . T.null) (map T.strip (T.splitOn "," raw))

parseItem :: ListItem -> Text -> Either Text Value
parseItem (ScalarItems s) raw = parseScalar s raw
parseItem (RecordItems fields) raw = do
  let parts = map T.strip (T.splitOn ":" raw)
  when (length parts > length fields) $
    Left ("`" <> raw <> "` has more than " <> T.pack (show (length fields)) <> " parts (" <> T.intercalate ":" (map fieldName fields) <> ")")
  VRecord . M.fromList <$> zipWithM field fields (map Just parts ++ repeat Nothing)
  where
    field f part = case maybe (fieldDefault f) Just part of
      Just v -> (fieldName f,) <$> parseScalar (fieldType f) v
      Nothing -> Left ("`" <> raw <> "` is missing `" <> fieldName f <> "`")

parseScalar :: Scalar -> Text -> Either Text Value
parseScalar s raw = case s of
  IdentS -> VName <$> parseName raw
  TextS -> Right (VText raw)
  BoolS -> case T.toLower raw of
    "true" -> Right (VBool True)
    "false" -> Right (VBool False)
    _ -> Left ("expected true or false, got `" <> raw <> "`")
  EnumS choices
    | raw `elem` choices -> Right (VText raw)
    | otherwise -> Left ("expected one of " <> T.intercalate ", " choices <> "; got `" <> raw <> "`")

emptyValue :: InputType -> Value
emptyValue = \case
  ListInput _ -> VList []
  ScalarInput BoolS -> VBool False
  ScalarInput TextS -> VText ""
  _ -> VNull

-- | Evaluate derived values in dependency order (TOML tables are unordered).
-- A --set override replaces a derived value but takes the type the derived
-- value would have had, so @--set is_list=false@ is a bool, not truthy text.
deriveAll :: [(Text, Template)] -> Map Text Text -> Env -> Either EnvError Env
deriveAll derived overrides = go derived
  where
    go [] env = Right env
    go pending env =
      let pendingNames = S.fromList (map fst pending)
          (ready, blocked) = partition (\(_, t) -> S.null (freeVariables t `S.intersection` pendingNames)) pending
       in if null ready
            then Left (DeriveCycle (map fst blocked))
            else foldM step env ready >>= go blocked
    step env (k, t) = do
      v <- case (evaluate env t, M.lookup k overrides) of
        (Left e, Nothing) -> Left (DeriveFailed k e)
        (Right derivedValue, Nothing) -> Right derivedValue
        (Right derivedValue, Just raw) -> coerceOverride k derivedValue raw
        (Left _, Just raw) -> Right (VText raw)
      Right (M.insert k v env)

coerceOverride :: Text -> Value -> Text -> Either EnvError Value
coerceOverride k derivedValue raw = case derivedValue of
  VBool _ -> invalid (parseScalar BoolS raw)
  VName _ -> invalid (parseScalar IdentS raw)
  VList _ -> Left (InputProblems [InvalidInput k "a list-valued derived value cannot be overridden with --set"])
  _ -> Right (VText raw)
  where
    invalid = either (\r -> Left (InputProblems [InvalidInput k r])) Right

describeEnvError :: EnvError -> Text
describeEnvError = \case
  InputProblems ps -> "invalid inputs: " <> T.intercalate "; " (map describeProblem ps)
  DeriveFailed k e -> "derived value `" <> k <> "` failed: " <> describeTemplateError e
  DeriveCycle ks -> "derived values depend on each other in a cycle: " <> T.intercalate ", " ks
  where
    describeProblem = \case
      MissingInput s -> "missing `" <> inputName s <> "` (" <> describeInputType (inputType s) <> ")"
      InvalidInput n r -> "`" <> n <> "`: " <> r
      UnknownInput n -> "unknown input `" <> n <> "`"

describeInputType :: InputType -> Text
describeInputType = \case
  ScalarInput s -> scalar s
  ListInput (ScalarItems s) -> "list<" <> scalar s <> ">"
  ListInput (RecordItems fs) -> "list<" <> T.intercalate ":" (map fieldName fs) <> ">"
  where
    scalar = \case
      IdentS -> "ident"
      TextS -> "text"
      BoolS -> "bool"
      EnumS cs -> "enum(" <> T.intercalate "|" cs <> ")"
