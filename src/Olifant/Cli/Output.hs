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
    ]

listHuman :: [CatalogEntry] -> [(GeneratorId, FilePath, Text)] -> Text
listHuman es broken =
  T.unlines $
    [T.justifyLeft width ' ' (idText (entryId e)) <> "  " <> genDescription (entryDefinition e) | e <- es]
      ++ [T.justifyLeft width ' ' (idText gid) <> "  INVALID: " <> err | (gid, _, err) <- broken]
  where
    width = maximum (0 : map (T.length . idText . entryId) es ++ [T.length (idText gid) | (gid, _, _) <- broken])

showJson :: CatalogEntry -> Value
showJson e =
  object
    [ "id" .= idText (entryId e)
    , "description" .= genDescription d
    , "language" .= genLanguage d
    , "version" .= genVersion d
    , "inputs" .= [object ["name" .= inputName i, "type" .= describeInputType (inputType i), "optional" .= inputOptional i, "default" .= inputDefault i] | i <- genInputs d]
    , "derived" .= map fst (genDerived d)
    , "edits" .= map editJson (genEdits d)
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
      ++ ["  --set " <> inputName i <> "=<" <> describeInputType (inputType i) <> ">" <> (if inputOptional i then "  (optional)" else "") | i <- genInputs d]
      ++ ["", "edits:"]
      ++ [case ed of Create p _ _ -> "  create " <> templateSource p; Insert p _ _ -> "  insert " <> templateSource p | ed <- genEdits d]
  where
    d = entryDefinition e

planJson :: Bool -> Plan -> Value
planJson applied plan =
  object
    [ "plan_version" .= (1 :: Int)
    , "generator" .= idText (planGenerator plan)
    , "applied" .= applied
    , "inputs" .= planSets plan
    , "now" .= planNow plan
    , "edits" .= map editJson (planEdits plan)
    , "conflicts" .= [object ["code" .= conflictCode c, "message" .= describeConflict c] | c <- planConflicts plan]
    ]
  where
    editJson (PlannedEdit n p action) = object (["id" .= ("e" <> T.pack (show n)), "path" .= p] ++ actionFields action)
    actionFields = \case
      WillCreate c -> ["status" .= ("create" :: Text), "content" .= c]
      WillOverwrite c -> ["status" .= ("overwrite" :: Text), "content" .= c]
      WillInsert l s -> ["status" .= ("insert" :: Text), "line" .= l, "content" .= s]
      Skipped r -> ["status" .= ("skipped" :: Text), "reason" .= skipReason r]

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
    editLine (PlannedEdit _ p action) = case action of
      WillCreate c -> "  + " <> T.pack p <> " (" <> lineCount c <> ")"
      WillOverwrite c -> "  ~ " <> T.pack p <> " (overwritten, " <> lineCount c <> ")"
      WillInsert l s -> "  ~ " <> T.pack p <> " (+" <> lineCount s <> " at line " <> T.pack (show l) <> ")"
      Skipped r -> "  = " <> T.pack p <> " (skipped: " <> skipReason r <> ")"
    lineCount c = let n = length (T.lines c) in T.pack (show n) <> (if n == 1 then " line" else " lines")
    details
      | applied = ""
      | otherwise = T.concat ["\n--- " <> T.pack p <> "\n" <> c | PlannedEdit _ p a <- planEdits plan, Just c <- [contentOf a]]
    contentOf = \case
      WillCreate c -> Just c
      WillOverwrite c -> Just c
      WillInsert _ s -> Just s
      Skipped _ -> Nothing

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
