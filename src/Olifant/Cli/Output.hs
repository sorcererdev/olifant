module Olifant.Cli.Output
  ( encodeJson
  , listJson
  , listHuman
  , showJson
  , showHuman
  , planJson
  , planHuman
  , errorJson
  , errorHuman
  , inputProblemFields
  ) where

import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Types (Pair)
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Olifant.Catalog (CatalogEntry (..))
import Olifant.Generation
import Olifant.Generator
import Olifant.Template (templateSource)

encodeJson :: Value -> Text
encodeJson = TL.toStrict . TLE.decodeUtf8 . encode

idText :: GeneratorId -> Text
idText = generatorIdText

listJson :: [CatalogEntry] -> [(GeneratorId, FilePath, Text)] -> Value
listJson es broken =
  object
    [ "generators" .= [object ["id" .= idText (entryId e), "pack" .= entryPack e, "description" .= genDescription (entryDefinition e)] | e <- es]
    , "invalid" .= [object ["id" .= idText gid, "path" .= p, "error" .= err] | (gid, p, err) <- broken]
    , "tip" .= compositionTip
    ]

-- | How to do a whole task in one call (agents pay per turn; spec §16).
compositionTip :: Text
compositionTip = "run a whole task in one call: olifant gen A --set k=v + B --set k=v (or --steps FILE with [{\"gen\": ..., \"set\": {...}}]); `olifant show GEN` lists inputs and examples"

listHuman :: [CatalogEntry] -> [(GeneratorId, FilePath, Text)] -> Text
listHuman es broken =
  T.unlines $
    [T.justifyLeft width ' ' (idText (entryId e)) <> "  " <> genDescription (entryDefinition e) | e <- es]
      ++ [T.justifyLeft width ' ' (idText gid) <> "  INVALID: " <> err | (gid, _, err) <- broken]
      ++ ["", "Tip: " <> compositionTip]
  where
    width = maximum (0 : map (T.length . idText . entryId) es ++ [T.length (idText gid) | (gid, _, _) <- broken])

showJson :: CatalogEntry -> Value
showJson e =
  object
    [ "id" .= idText (entryId e)
    , "description" .= genDescription d
    , "language" .= genLanguage d
    , "version" .= genVersion d
    , "inputs" .= [object ["name" .= inputName i, "type" .= describeInputType (inputType i), "optional" .= inputOptional i, "default" .= inputDefault i, "prompt" .= inputPrompt i] | i <- genInputs d]
    , "derived" .= map fst (genDerived d)
    , "edits" .= map editJson (genEdits d)
    , "examples" .= genExamples d
    ]
  where
    d = entryDefinition e
    editJson = \case
      Create p _ _ -> object ["kind" .= ("create" :: Text), "path" .= templateSource p]
      Insert p _ _ -> object ["kind" .= ("insert" :: Text), "path" .= templateSource p]

showHuman :: CatalogEntry -> Text
showHuman e =
  T.unlines $
    [idText (entryId e) <> ": " <> genDescription d, "", "inputs:"]
      ++ concat
        [ ("  --set " <> inputName i <> "=<" <> describeInputType (inputType i) <> ">" <> (if inputOptional i then "  (optional)" else "") <> maybe "" (\v -> "  (default " <> v <> ")") (inputDefault i))
            : ["      " <> p | Just p <- [inputPrompt i]]
        | i <- genInputs d
        ]
      ++ ["", "edits:"]
      ++ [case ed of Create p _ _ -> "  create " <> templateSource p; Insert p _ _ -> "  insert " <> templateSource p | ed <- genEdits d]
      ++ (if null (genExamples d) then [] else "" : "examples:" : ["  " <> x | x <- genExamples d])
  where
    d = entryDefinition e

planJson :: Bool -> Plan -> Value
planJson applied plan =
  object
    [ "plan_version" .= (2 :: Int)
    , "generator" .= idText (planGenerator plan)
    , "applied" .= applied
    , "steps" .= [object ["gen" .= g, "set" .= s] | (g, s) <- planSteps plan]
    , "now" .= planNow plan
    , "edits" .= map editJson (planEdits plan)
    , "conflicts" .= [object ["code" .= conflictCode c, "message" .= describeConflict c] | c <- planConflicts plan]
    ]
  where
    editJson (PlannedEdit st n p action) = object (["id" .= ("e" <> T.pack (show n)), "step" .= showStepPath st, "path" .= p] ++ actionFields action)
    -- Applied runs omit contents: the caller already has them on disk, and
    -- agents pay for every token of output in every later turn.
    content c = ["content" .= c | not applied]
    actionFields = \case
      WillCreate c -> ["status" .= ("create" :: Text)] ++ content c
      WillOverwrite c -> ["status" .= ("overwrite" :: Text)] ++ content c
      WillInsert l s -> ["status" .= ("insert" :: Text), "line" .= l] ++ content s
      Skipped r -> ["status" .= ("skipped" :: Text), "reason" .= skipReason r]
      Conflicted -> ["status" .= ("conflict" :: Text)]

planHuman :: Bool -> Plan -> Text
planHuman applied plan = T.unlines (header : map editLine (planEdits plan) ++ map (("  ! " <>) . describeConflict) (planConflicts plan)) <> details
  where
    header =
      (if applied then "" else "Plan: ")
        <> idText (planGenerator plan)
        <> " ("
        <> count (planEdits plan) "edit"
        <> ", "
        <> count (planConflicts plan) "conflict"
        <> ")"
        <> (if hasConflicts plan then ": nothing written" else "")
    count xs noun = T.pack (show (length xs)) <> " " <> noun <> (if length xs == 1 then "" else "s")
    multiStep = length (nub (map editStep (planEdits plan))) > 1
    stepTag st = if multiStep then "  [" <> showStepPath st <> "]" else ""
    editLine (PlannedEdit st _ p action) = (<> stepTag st) $ case action of
      WillCreate c -> "  + " <> T.pack p <> " (" <> lineCount c <> ")"
      WillOverwrite c -> "  ~ " <> T.pack p <> " (overwritten, " <> lineCount c <> ")"
      WillInsert l s -> "  ~ " <> T.pack p <> " (+" <> lineCount s <> " at line " <> T.pack (show l) <> ")"
      Skipped r -> "  = " <> T.pack p <> " (skipped: " <> skipReason r <> ")"
      Conflicted -> "  ! " <> T.pack p <> " (conflict, see below)"
    lineCount c = let n = length (T.lines c) in T.pack (show n) <> (if n == 1 then " line" else " lines")
    details
      | applied = ""
      | otherwise = T.concat ["\n--- " <> T.pack p <> "\n" <> c | PlannedEdit _ _ p a <- planEdits plan, Just c <- [contentOf a]]
    contentOf = \case
      WillCreate c -> Just c
      WillOverwrite c -> Just c
      WillInsert _ s -> Just s
      Skipped _ -> Nothing
      Conflicted -> Nothing

skipReason :: SkipReason -> Text
skipReason = \case
  AlreadyExists -> "already exists"
  AlreadyPresent -> "already present"
  UnchangedContent -> "unchanged"

conflictCode :: Conflict -> Text
conflictCode = \case
  TargetExists {} -> "target_exists"
  InsertTargetMissing {} -> "insert_target_missing"
  MarkerNotFound {} -> "anchor_unresolved"
  MarkerRepeated {} -> "marker_repeated"
  UnsafePath {} -> "unsafe_path"
  RequirementUnmet {} -> "requires_unmet"
  TargetUnusable {} -> "target_unusable"

errorJson :: Text -> Text -> [Pair] -> Value
errorJson code message extra = object ["error" .= object (["code" .= code, "message" .= message] ++ extra)]

errorHuman :: Text -> Maybe Text -> Text
errorHuman message hint = "error: " <> message <> maybe "" ("\nhint: " <>) hint <> "\n"

inputProblemFields :: [InputProblem] -> [Pair]
inputProblemFields ps =
  [ "missing" .= [object ["name" .= inputName s, "type" .= describeInputType (inputType s)] | MissingInput s <- ps]
  , "invalid" .= [object ["name" .= n, "reason" .= r] | InvalidInput n r <- ps]
  , "unknown" .= [n | UnknownInput n <- ps]
  , "hint" .= T.unwords ["--set " <> inputName s <> "=<" <> describeInputType (inputType s) <> ">" | MissingInput s <- ps]
  ]
