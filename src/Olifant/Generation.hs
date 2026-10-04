-- | Running generators: expand composed steps (spec §7), render their edits,
-- plan them against the workspace as one atomic plan, and (unless plan-only
-- or conflicted) write them.
module Olifant.Generation
  ( Mode (..)
  , GenerationError (..)
  , describeGenerationError
  , generationToolMissing
  , Step (..)
  , Resolve
  , ResolveFailure (..)
  , noResolve
  , StepPath
  , showStepPath
  , generate
  , RegionMode (..)
  , refreshRegions
  , regionInstances
  , unsetRegions
  , absorbDuplicates
  , module Olifant.Generation.Plan
  , module Olifant.Generation.Tools
  ) where

import Control.Monad (filterM, foldM, forM, forM_, when)
import Data.Bifunctor (first)
import Data.Aeson (object, (.=))
import Data.Functor ((<&>))
import Data.List (isPrefixOf, nub, sortOn)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Olifant.Generation.Complete (completeEdits)
import Olifant.Generation.Regions (RegionMode (..), absorbDuplicates, refreshRegions, regionInstances, unsetRegions)
import Olifant.Generation.Compose
import Olifant.Generation.Plan
import Olifant.Anchor (resolveLayers)
import Olifant.Generation.Planner (decideItems)
import Olifant.Generation.Tools
import Olifant.Generation.Render (RenderedEdit (..), renderedPath, renderedUnless)
import Olifant.Generator (EnvError, GeneratorId (..), GeneratorWith (..), Unless (..), describeEnvError, needsChoices, withChoices)
import Olifant.Language (Language, Match (..), Syntax (..), languageFor)
import Olifant.Holes (HoleSite (..), matchesWithHoles, placeHoles, placeholderize)
import Olifant.Generation.FillPhase (fillPhase)
import Olifant.Query (QueryFailure (..), failed, renderQuery, runQuery)
import Olifant.Template (TemplateError, Value (..), describeTemplateError)
import Olifant.Workspace (PathState (..), Workspace (..), isSafeRelative)
import System.FilePath (takeDirectory)

data Mode = PlanOnly | Apply
  deriving stock (Eq, Show)

data GenerationError
  = InvalidInputs StepPath EnvError
  | TemplateFailed StepPath TemplateError
  | UnknownGenerator StepPath Text Text
  | InvalidGenerator StepPath Text Text
  | CompositionCycle StepPath
  | QueryFailed StepPath QueryFailure
  deriving stock (Eq, Show)

-- | A generation error caused only by a missing tool (pack tests skip it).
generationToolMissing :: GenerationError -> Bool
generationToolMissing = \case
  QueryFailed _ f -> failureToolMissing f
  _ -> False

-- | Plan text against a file's, a hole matching whatever fills it (re-runs).
sameWithHoles :: Maybe (Maybe Text) -> Maybe (Maybe Text) -> Bool
sameWithHoles (Just (Just new)) (Just (Just actual)) = matchesWithHoles new actual
sameWithHoles a b = a == b

-- | An edit's text as written: holes as their placeholders.
showHoles :: [Language] -> PlannedEdit -> PlannedEdit
showHoles langs e =
  let shown t = M.findWithDefault t (editPath e) (placeholderize langs (M.singleton (editPath e) t))
   in e {editAction = case editAction e of
            WillCreate t -> WillCreate (shown t)
            WillOverwrite t -> WillOverwrite (shown t)
            WillInsert n t -> WillInsert n (shown t)
            other -> other}

-- | The message for a 'GenerationError', naming the step it happened in.
describeGenerationError :: GenerationError -> Text
describeGenerationError = \case
  InvalidInputs path e -> inStep path (describeEnvError e)
  TemplateFailed path e -> inStep path (describeTemplateError e)
  UnknownGenerator path ref why -> inStep path ("`" <> ref <> "`: " <> why)
  InvalidGenerator path ref why -> inStep path ("`" <> ref <> "`: " <> why)
  QueryFailed path f -> inStep path (failureWhy f)
  CompositionCycle path -> "generators use each other in a cycle: " <> showStepPath path
  where
    inStep path msg = showStepPath path <> ": " <> msg

generate :: Monad m => Workspace m -> Resolve m -> Tools m -> [Step] -> UTCTime -> Mode -> m (Either GenerationError Plan)
generate ws resolve tools steps now mode =
  expandSteps resolve bindQueries choose now steps >>= \case
    Left err -> pure (Left (fromComposition err))
    Right items -> do
      let targets = nub [p | PlanEdit _ e <- items, Just p <- [renderedPath e]]
          required = nub [p | PlanRequire _ ps _ <- items, p <- ps]
      inspected <- traverse (\p -> (p,) <$> inspectTarget p) (nub (targets ++ required))
      onDisk <- S.fromList <$> filterM (\p -> if isSafeRelative p then (/= Absent) <$> inspectPath ws p else pure False) required
      let originals = M.fromList [(p, t) | (p, Right t) <- inspected]
          blocked = M.fromList [(p, why) | (p, Left why) <- inspected, p `elem` targets]
          ids = [generatorIdText (genId (stepGenerator s)) | s <- steps]
      -- `unless` globs need the project's paths; list them only when used.
      diskPaths <- if any (\case Just (UnlessExists _) -> True; _ -> False) [renderedUnless e | PlanEdit _ e <- items] then listFiles ws "." else pure []
      let completes = [(p, c) | PlanEdit p (RenderedComplete c) <- items]
          -- Both phases (spec §5.6): positional edits, then compiler-found branches.
          phases originals' = do
            (positional, pc, afterPositional, next) <- decideItems locate diskPaths items originals' blocked onDisk
            (compiled, cc, ws', afterComplete) <- completeEdits tools ws completes next afterPositional
            (refreshed, rc, current') <- refreshRegions SkipUnset tools ws (next + length compiled) afterComplete
            pure (positional ++ compiled ++ refreshed, pc ++ cc ++ rc, ws', current')
      firstPass <- phases originals
      (edits, conflicts, warnings, current) <- case [p | TargetExists _ p <- (\(_, c, _, _) -> c) firstPass] of
        [] -> pure firstPass
        -- A file this plan creates and then edits no longer matches its
        -- template on a re-run, so its create reports `target_exists`. If
        -- planning as if it were absent reproduces it exactly, the run is a
        -- no-op for it, not a conflict (spec §5.4).
        existing -> do
          (edits2, conflicts2, warnings2, current2) <- phases (foldr (\p -> M.insert p Nothing) originals existing)
          let unchanged p = sameWithHoles (M.lookup p current2) (M.lookup p originals)
          pure $
            if null conflicts2 && all unchanged existing
              then ([if editPath e `elem` existing then e {editAction = Skipped UnchangedContent, editSite = Nothing} else e | e <- edits2], conflicts2, warnings2, current2)
              else firstPass
      -- Holes (spec §6.4): name them, write their placeholders; a file whose
      -- text already matches the plan, holes and all, is left as it is.
      -- A file a rule's fix rewrote is changed even when its old code would
      -- "match" the new hole (holes match any fill on re-runs).
      let replaced = [editPath e | e <- edits, WillReplace {} <- [editAction e]]
          changed = M.fromList [(p, c) | (p, Just c) <- M.toList current, if p `elem` replaced then Just (Just c) /= M.lookup p originals else not (sameWithHoles (Just (Just c)) (M.lookup p originals))]
          (placed, holeConflicts, holes0) = case placeHoles (toolLanguages tools) changed of
            Left problems -> (changed, map HoleProblem problems, [])
            Right (files, hs) -> (files, [], [PlannedHole h (HoleUnfilled "no model is configured") Nothing | h <- hs])
          shown = map (showHoles (toolLanguages tools)) edits
      -- The fill phase (spec §6.4): only with a model, and only when nothing
      -- else went wrong.
      -- Each written file's previous text, for editors: compiler-found files
      -- are named by no edit, so they are read here.
      let previous p = maybe (readText ws p) pure (M.lookup p originals)
      (writes, holes, fillWarnings) <-
        if null (conflicts ++ holeConflicts)
          then fillPhase ws tools (concatMap (M.toList . stepSets) steps) placed holes0
          else pure (placed, holes0, [])
      before <- traverse (\p -> (p,) <$> previous p) (M.keys writes)
      -- A list this plan should extend but cannot (no fences yet): say so,
      -- or the new file is silently left out (review I5).
      unset <- unsetRegions tools ws
      let created = [p | (p, Nothing) <- before]
          unsetWarnings =
            [ "region `" <> r <> "` is not set up in " <> T.pack f <> ", so it will not list " <> T.intercalate ", " (map T.pack near) <> " (run `olifant sync --init " <> r <> "`)"
            | (f, r) <- unset
            , let near = [c | c <- created, takeDirectory f `isPrefixOf` takeDirectory c, c /= f]
            , not (null near)
            ]
      -- Parse pre-check (spec §4.6): a touched file that does not parse is
      -- warned about first; placement and checks around it may go wrong.
      -- Every existing file an edit names (even when all its edits
      -- conflicted, review I7), and every file the compiler found.
      let touched = M.toList (M.fromList ([(p, t) | (p, Just t) <- M.toList originals, p `elem` targets] ++ [(p, t) | (p, Just t) <- before]))
      unparsed <- fmap concat . forM touched $ \(p, old) ->
        case languageFor (toolLanguages tools) p of
          Nothing -> pure []
          Just lang ->
            querySyntax (toolSyntax tools) lang (object ["kind" .= ("ERROR" :: Text)]) old <&> \case
              Right ms
                | m : _ <- sortOn matchStart ms ->
                let near = T.take 20 (T.takeWhile (/= '\n') (matchText m))
                 in [(p, T.pack p <> ":" <> T.pack (show (fst (matchStart m) + 1)) <> " does not parse (near `" <> near <> "`) — this plan may place code wrongly, and checks will fail")]
              _ -> []
      let pointBack w
            | "does not compile" `T.isInfixOf` w, any (\(p, _) -> T.pack p `T.isInfixOf` w) unparsed = w <> " (see the parse warning above)"
            | otherwise = w
          -- A touched file that was already broken makes the compiler blame
          -- the plan's code; say where the break really is.
          pointBackConflict = \case
            CompleteFailed st why tm -> CompleteFailed st (pointBack why) tm
            c -> c
      -- What the plan shows is what is written: fills, not placeholders.
      let filled = [(holePlaceholder (phSite h), f) | h <- holes, Just f <- [phFill h]]
          withFills t = foldl (\acc (ph, f) -> T.replace ph f acc) t filled
          shownFilled = [e {editAction = case editAction e of
                               WillCreate t -> WillCreate (withFills t)
                               WillOverwrite t -> WillOverwrite (withFills t)
                               WillInsert n t -> WillInsert n (withFills t)
                               other -> other} | e <- shown]
          plan = Plan (GeneratorId (T.intercalate " + " ids)) (zip ids (map stepSets steps)) now shownFilled (map pointBackConflict (conflicts ++ holeConflicts)) (map snd unparsed ++ map pointBack (warnings ++ fillWarnings) ++ unsetWarnings) writes holes (M.fromList before)
      when (mode == Apply && not (hasConflicts plan)) $
        forM_ (M.toList writes) (uncurry (writeText ws))
      pure (Right plan)
  where
    -- `choices_from` (spec §4.1), per step: the project's files plus what the
    -- run has created so far, read from the plan first. The project is
    -- listed only when a generator has such an input.
    choose prior sets gen
      | not (needsChoices gen) = pure (Right gen)
      | otherwise = do
          disk <- listFiles ws "."
          withChoices (toolQueries tools) (nub (disk ++ map fst prior)) (\p -> maybe (readText ws p) (pure . Just) (lookup p prior)) sets now gen
    -- `[query.NAME]`: render each query with the generator's inputs and
    -- derived values (never another query's records, so their order does not
    -- matter), run it over the project as it is on disk, and bind the records
    -- as a list.
    bindQueries env queries = do
      paths <- listFiles ws "."
      let one acc (name, q) = case acc of
            Left why -> pure (Left why)
            Right e -> case renderQuery env q of
              Left err -> pure (Left (failed ("query `" <> name <> "`: " <> describeTemplateError err)))
              Right q' -> fmap (\rs -> M.insert name (VList (map VRecord rs)) e) . first (\f -> f {failureWhy = "query `" <> name <> "`: " <> failureWhy f}) <$> runQuery (toolQueries tools) paths (readText ws) q'
      foldM one (Right env) queries
    locate p anchors content = resolveLayers (toolSyntax tools) (languageFor (toolLanguages tools) p) anchors content
    fromComposition = \case
      StepInputs p e -> InvalidInputs p e
      StepTemplate p e -> TemplateFailed p e
      StepUnknown p ref why -> UnknownGenerator p ref why
      StepInvalid p ref why -> InvalidGenerator p ref why
      StepCycle p -> CompositionCycle p
      StepQuery p why -> QueryFailed p why
    -- Right: current text (or Nothing if absent). Left: why it cannot be written.
    -- Unsafe paths are never touched; the planner reports them.
    inspectTarget p
      | not (isSafeRelative p) = pure (Right Nothing)
      | otherwise = do
          parentFiles <- filterM (fmap isFile . inspectPath ws) (parents p)
          case parentFiles of
            (parent : _) -> pure (Left ("`" <> T.pack parent <> "` is a file, not a directory"))
            [] ->
              inspectPath ws p <&> \case
                Absent -> Right Nothing
                Readable t -> Right (Just t)
                Unreadable -> Left "it exists but is not UTF-8 text, so Olifant will not touch it"
                Directory -> Left "it is a directory"
    isFile = \case
      Readable _ -> True
      Unreadable -> True
      _ -> False
    parents = takeWhile (/= ".") . drop 1 . iterate takeDirectory
