module Olifant.Generator.Inputs
  ( InputProblem (..)
  , EnvError (..)
  , buildEnv
  , describeEnvError
  , describeInputType
  , inputsUsed
  , envBefore
  , resolveInput
  , showContext
  ) where

import Control.Monad (foldM, unless, zipWithM)
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
import Olifant.Anchor (closest)
import Olifant.Naming (nameWords, parseName)
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
      troubled = S.fromList [n | InvalidInput n _ <- lefts resolved]
      -- "its choices depend on X; set it first" misleads when X was set but
      -- is invalid: X's own problem says it all. (A missing X keeps it.)
      waitsOnTrouble = \case
        InvalidInput n _ | Just ns <- dependsOn n -> all (`S.member` troubled) ns
        _ -> False
      dependsOn n = case [c | InputSpec {inputName = m, inputType = ScalarInput (EnumOf _ _ c)} <- genInputs gen, m == n] of
        [NeedsInputs ns] -> Just ns
        _ -> Nothing
      problems = unknown ++ filter (not . waitsOnTrouble) (lefts resolved)
  unless (null problems) (Left (InputProblems problems))
  let base = M.insert "now" (VTime now) (M.fromList (rights resolved))
      overrides = M.fromList [(k, v) | (k, v) <- M.toList sets, k `elem` derivedNames]
  deriveAll (genDerived gen) overrides base

-- | The env a query serving input @name@ sees (spec §4.1): the inputs
-- declared above it that parse (enums against their resolved choices), and
-- the derived values that evaluate from them. Problems are ignored here;
-- 'buildEnv' reports them.
envBefore :: GeneratorWith s -> Map Text Text -> UTCTime -> Text -> Env
envBefore gen sets now name =
  let above = takeWhile ((/= name) . inputName) (genInputs gen)
      -- --set overrides of derived values win, as in 'buildEnv'.
      overrides = M.fromList [(k, VText v) | (k, v) <- M.toList sets, k `elem` map fst (genDerived gen)]
   in deriveSome (M.unions [overrides, M.fromList (rights (map (resolveInput sets) above)), M.singleton "now" (VTime now)])
  where
    deriveSome env =
      let step e (k, t) = either (const e) (\v -> M.insert k v e) (evaluate e t)
          next = foldl step env [d | d@(k, _) <- genDerived gen, k `M.notMember` env]
       in if M.size next == M.size env then env else deriveSome next

resolveInput :: Map Text Text -> InputSpec -> Either InputProblem (Text, Value)
resolveInput sets spec = case maybe (inputDefault spec) Just (M.lookup name sets) of
  Just raw -> case parseValue (inputType spec) raw of
    Right (VList []) | not (inputOptional spec) -> Left (InvalidInput name "needs at least one item (it is not optional)")
    parsed -> bimap (InvalidInput name) (name,) parsed
  Nothing
    | inputOptional spec -> Right (name, emptyValue (inputType spec))
    | otherwise -> Left (MissingInput spec)
  where
    name = inputName spec

parseValue :: InputType -> Text -> Either Text Value
parseValue (ScalarInput s) raw = parseScalar s raw
parseValue (ListInput item) raw = VList <$> traverse (parseItem item) items
  where
    items = filter (not . T.null) (map T.strip (splitTopLevel ',' raw))

parseItem :: ListItem -> Text -> Either Text Value
parseItem (ScalarItems s) raw = parseScalar s raw
parseItem (RecordItems fields) raw = do
  -- The last field takes the rest, so `name:Dict String Int` or a type with
  -- colons inside brackets stays whole.
  let parts = map T.strip (limitParts (length fields) (splitTopLevel ':' raw))
  VRecord . M.fromList <$> zipWithM field fields (map Just parts ++ repeat Nothing)
  where
    field f part = case maybe (fieldDefault f) Just part of
      Just v -> (fieldName f,) <$> parseScalar (fieldType f) v
      Nothing -> Left ("`" <> raw <> "` is missing `" <> fieldName f <> "`")

parseScalar :: Scalar -> Text -> Either Text Value
parseScalar s raw = case s of
  IdentS
    | looksLikeNameAndType -> Left ("`" <> raw <> "` looks like a name followed by other words (e.g. a type); pass only the name, like " <> firstChunk <> ", and give the rest to its own input")
    | otherwise -> VName <$> parseName raw
  TextS -> Right (VText raw)
  BoolS -> case T.toLower raw of
    "true" -> Right (VBool True)
    "false" -> Right (VBool False)
    _ -> Left ("expected true or false, got `" <> raw <> "`")
  EnumS choices -> oneOf choices
  EnumOf q _ choices -> case choices of
    Unresolved -> Left "its choices were not resolved (an Olifant bug)"
    NeedsInputs ns -> Left ("its choices depend on " <> T.intercalate ", " ["`" <> n <> "`" | n <- ns] <> "; set " <> (if length ns == 1 then "it" else "them") <> " first")
    Resolved [] ctx -> Left ("the query `" <> q <> "` found nothing" <> (if null ctx then "" else " for " <> showContext ctx) <> ", so there is nothing to choose")
    Resolved cs ctx -> oneOfIn (if null ctx then "" else " (the " <> q <> " of " <> showContext ctx <> ")") cs
  where
    oneOf = oneOfIn ""
    oneOfIn suffix choices
      | raw `elem` choices = Right (VText raw)
      | otherwise = Left ("expected one of " <> T.intercalate ", " choices <> suffix <> "; got `" <> raw <> "`" <> maybe "" (\c -> " (did you mean " <> c <> "?)") (closest raw choices))
    chunks = T.words raw
    firstChunk = case chunks of
      (c : _) -> c
      [] -> raw
    -- "Blog Post" is a title-case name; "SetDisplayName String" is a name plus a type.
    looksLikeNameAndType = length chunks > 1 && any (\c -> either (const False) ((> 1) . length . nameWords) (parseName c)) chunks

-- | Split on a separator, ignoring separators nested inside () [] {}.
splitTopLevel :: Char -> Text -> [Text]
splitTopLevel sep = map T.pack . go (0 :: Int) "" . T.unpack
  where
    go _ acc [] = [reverse acc]
    go depth acc (c : cs)
      | c == sep && depth == 0 = reverse acc : go depth "" cs
      | c `elem` ("([{" :: String) = go (depth + 1) (c : acc) cs
      | c `elem` (")]}" :: String) = go (max 0 (depth - 1)) (c : acc) cs
      | otherwise = go depth (c : acc) cs

-- | Keep at most @n@ parts; the last one takes the remainder (rejoined).
limitParts :: Int -> [Text] -> [Text]
limitParts n parts
  | n <= 0 || length parts <= n = parts
  | otherwise = take (n - 1) parts ++ [T.intercalate ":" (drop (n - 1) parts)]

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
      EnumOf _ _ (Resolved cs _) -> "enum(" <> T.intercalate "|" cs <> ")"
      EnumOf _ _ (NeedsInputs ns) -> "enum, depends on " <> T.intercalate ", " ns
      EnumOf q _ Unresolved -> "enum(one of query " <> q <> ")"

-- | The inputs (and derived values) a set of variables reaches, through
-- derived values (spec §4.1).
inputsUsed :: [(Text, Template)] -> S.Set Text -> S.Set Text
inputsUsed derived = go S.empty
  where
    go seen vs =
      let new = vs `S.difference` seen
       in if S.null new then seen else go (seen `S.union` new) (S.unions [freeVariables t | (k, t) <- derived, k `S.member` new])

-- | `service=billing, kind=x`: the values a dependent query used.
showContext :: [(Text, Text)] -> Text
showContext = T.intercalate ", " . map (\(k, v) -> k <> "=" <> v)
