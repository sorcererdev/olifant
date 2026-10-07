-- | What Olifant says, as JSON and text: the CLI prints it, the language server sends it, agents read it.
module Olifant.Output
  ( encodeJson
  , generatorKind
  , shortSummary
  , doctorJson
  , fileActionsJson
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

import Control.Monad (join)
import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Types (Pair)
import Data.List (nub)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Olifant.Anchor (Located (..), positionWords)
import Olifant.Doctor (Check (..), Status (..))
import Olifant.Actions (Action (..), FileAction (..), Nearby (..), Range (..))
import Olifant.Cursor (isSnippet)
import Data.Maybe (isJust)
import Olifant.Catalog (CatalogEntry (..))
import Olifant.Snippet (Snippet (..), holeSource)
import Olifant.Generation
import Olifant.Holes (HoleSite (..))
import Olifant.Generator
import Olifant.Language (Language (..), TargetDef (..), targetPhrase)
import Olifant.Template.Syntax (Segment (..), Template (..))
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
    summary e = object ["id" .= idText (entryId e), "name" .= generatorName (entryDefinition e), "pack" .= entryPack e, "description" .= genDescription (entryDefinition e), "kind" .= generatorKind (entryDefinition e), "summary" .= shortSummary (entryDefinition e)]

-- | What a generator does, for a picker's tag: create (it makes files),
-- fill (it makes none, and the model writes a body: it declares
-- [holes.NAME] or an inline text has a hole), or extend (it only edits).
generatorKind :: GeneratorDef -> Text
generatorKind d
  | any isCreate (genEdits d) = "create"
  | not (null (genHoles d)) || any inlineHole (genEdits d) = "fill"
  | otherwise = "extend"
  where
    isCreate = \case
      Create {} -> True
      _ -> False
    inlineHole e = case e of
      Create {editSource = InlineText t} -> hasHole t
      Insert {editSource = InlineText t} -> hasHole t
      _ -> False
    hasHole t = any (\case Hole {} -> True; _ -> False) (templateSegments t)

-- | A short line for a picker's list: `summary`, else the description up to
-- its first `:`, `;` or ` —`.
shortSummary :: GeneratorDef -> Text
shortSummary d = case genSummary d of
  Just s -> s
  Nothing -> T.strip (fst (T.breakOn " —" (T.takeWhile (`notElem` (":;" :: String)) (genDescription d))))

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
    , "name" .= generatorName d
    , "description" .= genDescription d
    , "kind" .= generatorKind d
    , "summary" .= shortSummary d
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
      Replace p (l1, l2) _ _ -> object ["kind" .= ("replace" :: Text), "path" .= templateSource p, "lines" .= [l1, l2]]
      Cursor _ -> object ["kind" .= ("cursor" :: Text)]

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
      Replace p (l1, l2) _ _ -> "  replace " <> templateSource p <> " lines " <> T.pack (show l1) <> "-" <> T.pack (show l2)
      Cursor _ -> "  at the cursor (a snippet: gen --at FILE:LINE:COL)"
    useLines (u, sub) =
      ("  " <> maybe (useGenerator u) (idText . entryId) sub <> maybe "" (\f -> " (for each item of " <> f <> ")") (useForEach u))
        : ("    with " <> T.unwords [k <> "=" <> templateSource t | (k, t) <- useWith u])
        : ["    its input --set " <> inputName i <> "=<" <> describeInputType (inputType i) <> ">" | Just s' <- [sub], i <- genInputs (entryDefinition s')]

-- | Every action in a file, where it applies and how an editor may offer it
-- (unasked, as a snippet, on a trigger: spec §4.8, §4.9), and the near
-- misses (`actions --file`).
fileActionsJson :: [(Text, GeneratorDef)] -> [FileAction] -> [Nearby] -> Value
fileActionsJson gens acts near =
  object
    [ "actions" .= [object (["generator" .= actGenerator a, "name" .= nameOf (actGenerator a), "label" .= actLabel a, "range" .= fmap rangeJson r, "sets" .= M.fromList (actSets a), "missing" .= actMissing a] ++ suggestOf (actGenerator a)) | FileAction a r <- acts]
    , "nearby" .= [object ["generator" .= nbGenerator n, "name" .= nameOf (nbGenerator n), "label" .= nbLabel n, "why" .= nbWhy n] | n <- near]
    ]
  where
    nameOf gid = maybe gid generatorName (lookup gid gens)
    rangeJson (Range (l1, c1) (l2, c2)) = object ["start" .= [l1, c1], "end" .= [l2, c2]]
    suggestOf gid =
      let g = lookup gid gens
          ap = genAppliesAt =<< g
       in ["suggest" .= maybe False apSuggest ap, "snippet" .= maybe False isSnippet g, "trigger" .= maybe False (isJust . apTrigger) ap]

-- | `doctor`'s checks, as editors read them.
doctorJson :: [Check] -> Value
doctorJson checks = object ["checks" .= [object ["name" .= checkName c, "status" .= status (checkStatus c), "detail" .= checkDetail c, "fix" .= checkFix c] | c <- checks]]
  where
    status = \case Ok -> "ok" :: Text; Warn -> "warn"; Bad -> "error"

planJson :: Bool -> Plan -> Value
planJson applied plan =
  object $
    [ "plan_version" .= (2 :: Int)
    , "generator" .= idText (planGenerator plan)
    , "applied" .= applied
    , "steps" .= [object ["gen" .= g, "set" .= s] | (g, s) <- planSteps plan]
    , "now" .= planNow plan
    , "edits" .= map editJson (planEdits plan)
    , "conflicts" .= [object ["code" .= conflictCode c, "message" .= describeConflict c] | c <- planConflicts plan]
    , "warnings" .= planWarnings plan
    , "holes" .= map holeJson (planHoles plan)
    , "snippet" .= fmap (\s -> object ["file" .= snFile s, "line" .= snLine s, "ghost" .= snGhost s, "body" .= snBody s]) (planSnippet plan)
    , "todos" .= [object ["edit" .= editId e, "file" .= siteFile s, "function" .= siteFunction s, "pattern" .= sitePattern s] | e <- planEdits plan, Just s <- [editSite e], siteTodo s]
    ]
      -- Whole files for editors, which apply them as buffer changes; not on
      -- applied runs (like content).
      ++ ["files" .= [object ["path" .= p, "before" .= join (M.lookup p (planBefore plan)), "after" .= t] | (p, t) <- M.toAscList (planWrites plan)] | not applied]
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
      WillReplace l n s -> ["status" .= ("replace" :: Text), "line" .= l, "removed" .= n] ++ content s
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
      (kind, source) = holeSource (holeKind site)
   in object ["name" .= holeName site, "kind" .= kind, "source" .= source, "file" .= holeFile site, "line" .= holeLine site, "placeholder" .= holePlaceholder site, "status" .= status, "model" .= model, "attempts" .= attempts, "why" .= why, "fill" .= phFill h]

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
      WillReplace l n s -> "  ~ " <> T.pack p <> " (" <> (if T.null s then "−" <> T.pack (show n) <> " line" <> (if n == 1 then "" else "s") else "replaced " <> T.pack (show n) <> " line" <> (if n == 1 then "" else "s") <> " with " <> lineCount s) <> " at line " <> T.pack (show l) <> ")"
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
      WillReplace _ _ s -> Just s
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
  RequirementRefused {} -> "requires_unmet"
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
