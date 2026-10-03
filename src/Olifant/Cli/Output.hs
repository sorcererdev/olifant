module Olifant.Cli.Output
  ( encodeJson
  , listJson
  , listHuman
  , showJson
  , showHuman
  , planJson
  , anchorsJson
  , anchorsHuman
  , whereJson
  , whereHuman
  , planHuman
  , syncCheckHuman
  , errorJson
  , errorHuman
  , inputProblemFields
  ) where

import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Types (Pair)
import Data.List (nub)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Olifant.Anchor (Located (..), positionWords)
import Olifant.Catalog (CatalogEntry (..))
import Olifant.Generation
import Olifant.Holes (HoleSite (..))
import Olifant.Generator
import Olifant.Language (Language (..), TargetDef (..), targetPhrase)
import Olifant.Template (templateSource)
import System.FilePath ((</>))

encodeJson :: Value -> Text
encodeJson = TL.toStrict . TLE.decodeUtf8 . encode

idText :: GeneratorId -> Text
idText = generatorIdText

type Subs = CatalogEntry -> [(UseStep, Maybe CatalogEntry)]

-- | Each enum's choices as `show` prints them (spec §4.1).
type Views = CatalogEntry -> M.Map Text ChoiceView

listJson :: Bool -> Subs -> Views -> [CatalogEntry] -> [(GeneratorId, FilePath, Text)] -> Value
listJson detail subs views es broken =
  object
    [ "generators" .= [if detail then showJson (views e) (subs e) e else summary e | e <- es]
    , "invalid" .= [object ["id" .= idText gid, "path" .= p, "error" .= err] | (gid, p, err) <- broken]
    , "tip" .= compositionTip
    ]
  where
    summary e = object ["id" .= idText (entryId e), "pack" .= entryPack e, "description" .= genDescription (entryDefinition e)]

-- | How to do a whole task in one call (agents pay per turn; spec §16).
compositionTip :: Text
compositionTip = "run a whole task in one call: olifant gen A --set k=v + B --set k=v (or --steps FILE with [{\"gen\": ..., \"set\": {...}}]); `olifant show GEN` lists inputs and examples"

listHuman :: Bool -> Subs -> Views -> [CatalogEntry] -> [(GeneratorId, FilePath, Text)] -> Text
listHuman detail subs views es broken
  | detail = T.intercalate "\n" [showHuman (views e) (subs e) e | e <- es] <> T.unlines (["INVALID " <> idText gid <> ": " <> err | (gid, _, err) <- broken] ++ ["", "Tip: " <> compositionTip])
  | otherwise =
      T.unlines $
        [T.justifyLeft width ' ' (idText (entryId e)) <> "  " <> genDescription (entryDefinition e) | e <- es]
          ++ [T.justifyLeft width ' ' (idText gid) <> "  INVALID: " <> hang err | (gid, _, err) <- broken]
          ++ ["", "Tip: " <> compositionTip]
  where
    width = maximum (0 : map (T.length . idText . entryId) es ++ [T.length (idText gid) | (gid, _, _) <- broken])
    -- A multi-line error (a parser's caret diagram) stays under its generator.
    hang err = T.intercalate "\n" (case T.lines err of (l : ls) -> l : map (T.replicate (width + 4) " " <>) ls; [] -> [])

inputsJson :: M.Map Text ChoiceView -> GeneratorDef -> [Value]
inputsJson views d =
  [ object (["name" .= inputName i, "type" .= describeInputType (inputType i), "optional" .= inputOptional i, "default" .= inputDefault i, "prompt" .= inputPrompt i] ++ maybe [] choiceFields (M.lookup (inputName i) views))
  | i <- genInputs d
  ]
  where
    choiceFields = \case
      Flat cs -> ["choices" .= cs]
      Grouped ns gs ->
        ["depends_on" .= ns, "choices_by" .= M.fromList [(groupKey combo, cs) | (combo, Right cs) <- gs]]
          ++ ["errors" .= M.fromList errs | let errs = [(groupKey combo, why) | (combo, Left why) <- gs], not (null errs)]
      DependsOn ns -> ["depends_on" .= ns]
    -- One input: its value (`billing`); several: `a=x, b=y`.
    groupKey = \case
      [(_, v)] -> v
      combo -> T.intercalate ", " [k <> "=" <> v | (k, v) <- combo]

-- | A dependent input's choices, one line per group, under its prompt.
choiceLines :: Maybe ChoiceView -> [Text]
choiceLines = \case
  Just (Grouped _ gs) ->
    let label combo = T.intercalate ", " [k <> "=" <> v | (k, v) <- combo] <> ":"
        width = maximum (0 : [T.length (label c) | (c, _) <- gs])
        shown = \case
          Left why -> "query failed: " <> why
          Right [] -> "(none)"
          Right cs -> T.intercalate " | " cs
     in ["      " <> T.justifyLeft width ' ' (label combo) <> "  " <> shown r | (combo, r) <- gs]
  Just (DependsOn ns) -> ["      depends on " <> T.intercalate ", " ns <> "; set " <> (if length ns == 1 then "it" else "them") <> " to see the choices"]
  _ -> []

showJson :: M.Map Text ChoiceView -> [(UseStep, Maybe CatalogEntry)] -> CatalogEntry -> Value
showJson views subs e =
  object
    [ "id" .= idText (entryId e)
    , "description" .= genDescription d
    , "language" .= genLanguage d
    , "version" .= genVersion d
    , "inputs" .= inputsJson views d
    , "derived" .= map fst (genDerived d)
    , "edits" .= map editJson (genEdits d)
    , "examples" .= genExamples d
    , "source" .= (entryDir e </> "generator.toml")
    , "uses"
        .= [ object
              [ "generator" .= useGenerator u
              , "resolved" .= fmap (idText . entryId) sub
              , "for_each" .= useForEach u
              , "with" .= M.fromList [(k, templateSource t) | (k, t) <- useWith u]
              , "inputs" .= maybe [] (inputsJson M.empty . entryDefinition) sub
              ]
           | (u, sub) <- subs
           ]
    ]
  where
    d = entryDefinition e
    editJson = \case
      Create p _ _ _ -> object ["kind" .= ("create" :: Text), "path" .= templateSource p]
      Insert p anchors _ _ -> object ["kind" .= ("insert" :: Text), "path" .= templateSource p, "at" .= map anchorText anchors]
      Complete c w _ -> object ["kind" .= ("complete" :: Text), "constructor" .= templateSource c, "where" .= fmap templateSource w]

showHuman :: M.Map Text ChoiceView -> [(UseStep, Maybe CatalogEntry)] -> CatalogEntry -> Text
showHuman views subs e =
  T.unlines $
    [idText (entryId e) <> ": " <> genDescription d, "", "inputs:"]
      ++ concat
        [ ("  --set " <> inputName i <> "=<" <> describeInputType (inputType i) <> ">" <> (if inputOptional i then "  (optional)" else "") <> maybe "" (\v -> "  (default " <> v <> ")") (inputDefault i))
            : ["      " <> p | Just p <- [inputPrompt i]]
            ++ choiceLines (M.lookup (inputName i) views)
        | i <- genInputs d
        ]
      ++ ["", "edits:"]
      ++ [editLine ed | ed <- genEdits d]
      ++ [ "  (a complete body can use {= pattern =}, the missing case as the site writes it, e.g. Just Route.About; also {= site.file =} and {= site.function =})"
         | any (\case Complete {} -> True; _ -> False) (genEdits d)
         ]
      ++ (if null subs then [] else "" : "uses:" : concatMap useLines subs)
      ++ (if null (genExamples d) then [] else "" : "examples:" : ["  " <> x | x <- genExamples d])
      ++ ["", "defined in " <> T.pack (entryDir e </> "generator.toml")]
  where
    d = entryDefinition e
    editLine = \case
      Create p _ _ _ -> "  create " <> templateSource p
      Insert p anchors _ _ -> "  insert " <> templateSource p <> " at " <> T.intercalate " | " (map anchorText anchors)
      Complete c w _ -> "  complete " <> templateSource c <> maybe "" (\x -> " in " <> templateSource x) w
    useLines (u, sub) =
      ("  " <> maybe (useGenerator u) (idText . entryId) sub <> maybe "" (\f -> " (for each item of " <> f <> ")") (useForEach u))
        : ("    with " <> T.unwords [k <> "=" <> templateSource t | (k, t) <- useWith u])
        : ["    its input --set " <> inputName i <> "=<" <> describeInputType (inputType i) <> ">" | Just s' <- [sub], i <- genInputs (entryDefinition s')]

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
    , "warnings" .= planWarnings plan
    , "holes" .= map holeJson (planHoles plan)
    , -- Whole files for editors, which apply them as buffer changes.
      "files" .= [object ["path" .= p, "before" .= M.lookup p (planBefore plan), "after" .= t] | not applied, (p, t) <- M.toAscList (planWrites plan)]
    , "todos" .= [object ["edit" .= editId e, "file" .= siteFile s, "function" .= siteFunction s, "pattern" .= sitePattern s] | e <- planEdits plan, Just s <- [editSite e], siteTodo s]
    ]
  where
    editId e = "e" <> T.pack (show (editNumber e))
    editJson e =
      object $
        ["id" .= editId e, "step" .= showStepPath (editStep e), "path" .= editPath e]
          ++ actionFields (editAction e)
          ++ ["anchor_used" .= a | Just a <- [editAnchor e], Nothing <- [editSite e]]
          ++ concat [["found_by" .= ("compiler" :: Text), "site" .= siteFunction s, "todo" .= siteTodo s] | Just s <- [editSite e]]
    -- Applied runs omit contents: the caller already has them on disk, and
    -- agents pay for every token of output in every later turn.
    content c = ["content" .= c | not applied]
    actionFields = \case
      WillCreate c -> ["status" .= ("create" :: Text)] ++ content c
      WillOverwrite c -> ["status" .= ("overwrite" :: Text)] ++ content c
      WillInsert l s -> ["status" .= ("insert" :: Text), "line" .= l] ++ content s
      WillRefresh r a d c -> ["status" .= ("region" :: Text), "region" .= r, "added" .= a, "removed" .= d] ++ content c
      Skipped r -> ["status" .= ("skipped" :: Text), "reason" .= skipReason r]
      Conflicted -> ["status" .= ("conflict" :: Text)]

-- | A hole in the plan (spec §6.4): where, and whether it was filled.
holeJson :: PlannedHole -> Value
holeJson h =
  let site = phSite h
      (status, model, attempts, why) = case phStatus h of
        HoleFilled m n -> ("filled" :: Text, Just m, Just n, Nothing)
        HoleUnfilled w -> ("unfilled", Nothing, Nothing, Just w)
        HoleSkipped w -> ("skipped", Nothing, Nothing, Just w)
   in object ["name" .= holeName site, "kind" .= holeKind site, "file" .= holeFile site, "line" .= holeLine site, "status" .= status, "model" .= model, "attempts" .= attempts, "why" .= why, "fill" .= phFill h]

planHuman :: Bool -> Plan -> Text
planHuman applied plan = T.unlines (header : map editLine (planEdits plan) ++ map (("  ! " <>) . describeConflict) (planConflicts plan) ++ todoLines ++ map ("  ? warning: " <>) (planWarnings plan) ++ holeLines) <> details
  where
    holeLines
      | null (planHoles plan) = []
      | otherwise = "holes:" : [holeText h | h <- planHoles plan]
    holeText h =
      let site = phSite h
          status = case phStatus h of
            HoleFilled m n -> "filled by " <> m <> (if n > 1 then " (attempt " <> T.pack (show n) <> ")" else "") <> maybe "" (\f -> ": " <> T.intercalate " ⏎ " (T.lines f)) (phFill h)
            HoleUnfilled w -> "unfilled: " <> w
            HoleSkipped w -> "skipped: " <> w
       in "  " <> holeName site <> " (" <> holeKind site <> ") " <> T.pack (holeFile site) <> ":" <> T.pack (show (holeLine site)) <> "  " <> status
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
    todoLines = ["  ? " <> T.pack (siteFile s) <> ": " <> siteFunction s <> " needs a branch for " <> sitePattern s <> " (left as a todo)" | e <- planEdits plan, Just s <- [editSite e], siteTodo s]
    editLine (PlannedEdit st _ p action _ site) = (<> stepTag st) $ case action of
      WillCreate c -> "  + " <> T.pack p <> " (" <> lineCount c <> ")"
      WillOverwrite c -> "  ~ " <> T.pack p <> " (overwritten, " <> lineCount c <> ")"
      WillInsert l s -> "  ~ " <> T.pack p <> " (+" <> lineCount s <> " at line " <> T.pack (show l) <> foundBy site <> ")"
      WillRefresh r a d _ -> "  ~ " <> T.pack p <> " (region " <> r <> ": +" <> T.pack (show a) <> " −" <> T.pack (show d) <> ")"
      Skipped r -> "  = " <> T.pack p <> " (skipped: " <> skipReason r <> ")"
      Conflicted -> "  ! " <> T.pack p <> " (conflict, see below)"
    foundBy = maybe "" (\x -> ", compiler: " <> siteFunction x <> (if siteTodo x then ", todo" else ""))
    lineCount c = let n = length (T.lines c) in T.pack (show n) <> (if n == 1 then " line" else " lines")
    -- The text of each edit, for reviewing a --plan. Not after an applied
    -- run, and not when conflicts mean nothing would be written anyway.
    details
      | applied || hasConflicts plan = ""
      | otherwise = T.concat ["\n--- " <> T.pack p <> "\n" <> c | PlannedEdit _ _ p a _ _ <- planEdits plan, Just c0 <- [contentOf a], let c = if "\n" `T.isSuffixOf` c0 then c0 else c0 <> "\n"]
    contentOf = \case
      WillCreate c -> Just c
      WillOverwrite c -> Just c
      WillInsert _ s -> Just s
      WillRefresh _ _ _ c -> Just c
      Skipped _ -> Nothing
      Conflicted -> Nothing

skipReason :: SkipReason -> Text
skipReason = \case
  AlreadyExists -> "already exists"
  AlreadyPresent -> "already present"
  UnchangedContent -> "unchanged"
  UnlessMatched p -> "unless: " <> T.pack p <> " exists"
  UnlessFound p n -> "unless: line " <> T.pack (show n) <> " of " <> T.pack p <> " matches"

conflictCode :: Conflict -> Text
conflictCode = \case
  TargetExists {} -> "target_exists"
  InsertTargetMissing {} -> "insert_target_missing"
  AnchorUnresolved {} -> "anchor_unresolved"
  CompleteFailed {} -> "complete_failed"
  RegionBroken {} -> "region_broken"
  UnsafePath {} -> "unsafe_path"
  RequirementUnmet {} -> "requires_unmet"
  TargetUnusable {} -> "target_unusable"
  HoleProblem {} -> "hole_problem"

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

anchorText :: AnchorSpec -> Text
anchorText = \case
  AtPhrase t -> templateSource t
  AtRule _ pos -> "ast rule (" <> positionWords pos <> ")"

builtinForms :: [Text]
builtinForms = ["before|after marker TEXT", "before|after first line matching REGEX", "before|after last line matching REGEX", "start of|end of file"]

anchorsJson :: Language -> Value
anchorsJson lang =
  object
    [ "language" .= langName lang
    , "positions" .= (["start of", "end of", "before", "after"] :: [Text])
    , "builtin" .= builtinForms
    , "targets" .= [object ["phrase" .= targetPhrase t, "example" .= targetExample t] | t <- langTargets lang]
    ]

anchorsHuman :: Language -> Text
anchorsHuman lang =
  T.unlines $
    [langName lang <> ": at = \"<start of|end of|before|after> <target>\"", "", "targets:"]
      ++ ["  " <> T.justifyLeft width ' ' (targetPhrase t) <> "e.g. " <> targetExample t | t <- langTargets lang]
      ++ ["", "built in (any file):"]
      ++ map ("  " <>) builtinForms
  where
    width = maximum (0 : map (T.length . targetPhrase) (langTargets lang)) + 2

-- | Up to three lines either side of where text would go.
whereContext :: Located -> Text -> ([Text], [Text])
whereContext loc content =
  let ls = T.lines content
      i = locLine loc
   in (drop (max 0 (i - 3)) (take i ls), take 3 (drop i ls))

whereJson :: FilePath -> Text -> Located -> Text -> Value
whereJson file phrase loc content =
  let (before, after) = whereContext loc content
   in object ["file" .= file, "phrase" .= phrase, "line" .= (locLine loc + 1), "indent" .= T.length (locIndent loc), "before" .= before, "after" .= after]

whereHuman :: FilePath -> Located -> Text -> Text
whereHuman file loc content =
  let (before, after) = whereContext loc content
   in T.unlines ([T.pack file <> ":" <> T.pack (show (locLine loc + 1))] ++ map ("    " <>) before ++ ["  ▶ " <> locIndent loc <> "(inserted here)"] ++ map ("    " <>) after)

-- | `olifant sync --check` for people: a lint result, not a plan.
syncCheckHuman :: Plan -> Text
syncCheckHuman plan
  | null drifted && null (planConflicts plan) = "regions up to date\n"
  | otherwise =
      T.unlines $
        [ count (length rs) <> " out of date in " <> T.pack f <> ": " <> T.intercalate ", " [r <> " (+" <> T.pack (show a) <> " −" <> T.pack (show d) <> ")" | (r, a, d) <- rs] <> "; nothing written, run `olifant sync`"
        | (f, rs) <- M.toList (M.fromListWith (flip (++)) [(p, [x]) | (p, x) <- drifted])
        ]
          ++ map (("  ! " <>) . describeConflict) (planConflicts plan)
  where
    drifted = [(editPath e, (r, a, d)) | e <- planEdits plan, WillRefresh r a d _ <- [editAction e]]
    count n = T.pack (show n) <> (if n == 1 then " region" else " regions")
