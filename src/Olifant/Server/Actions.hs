-- | Internal to Olifant.Server: the ⚡ actions at a cursor, and an action
-- planned there (a snippet placed at the cursor, as `gen --at` does) with
-- the choices made — against the documents as they are now.
module Olifant.Server.Actions (actionsFor, menuActionsFor, actionEdits, actionPlan, actionPlanWith, fillerFor, rungFiller, describeProgress) where

import Data.Functor ((<&>))
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Actions (Action (..))
import Olifant.App (PlanOptions (..), Ports (..), Project (..), actionsAt', generatorsOf, menuOnly, modelFillerWith, placeAt, planOptions, planWith)
import Olifant.Catalog (describeCatalogError, findEntry, loadGenerator)
import Olifant.Cursor (isSnippet)
import Olifant.Fill (ModelFiller (..), parseFills, replayFiller)
import Olifant.Generation (Plan (..), ProgressEvent (..), Step (..), describeConflict, describeGenerationError)
import Olifant.Generation.FillPhase (cutSentence, wholeFileReason)
import Olifant.Workspace (Workspace (..))
import Olifant.Snippet (Snippet)
import Olifant.Server.Edits (FileEdit, editsOfPlan)
import Olifant.Migration (Extract (..), Strangler (..), extractDecision, extractKind, functionText)
import Olifant.Catalog (Catalog (..))
import Olifant.Language (Language (..), languageFor)
import Olifant.Rules (fingerprint)
import Olifant.Decide (Answer (..), choice)
import Data.List (partition)
import Data.Maybe (fromMaybe, listToMaybe)
import Olifant.Server.State (decision, deciderFor, ServerConfig (..), ServerState (..), memo, projectFor)
import System.FilePath (makeRelative)

-- | The actions at a position (1-based line, 1-based byte column) of a
-- file, each with whether it is a snippet; none in no project.
actionsFor :: FilePath -> (Int, Int) -> ServerState -> IO (ServerState, [(Action, Bool)])
actionsFor = actionsWith "actions" id

-- | The actions the ⚡ menu and the picker offer at a position: those of
-- generators 'inMenu' ('actionsFor' keeps every one: a picker's steps).
menuActionsFor :: FilePath -> (Int, Int) -> ServerState -> IO (ServerState, [(Action, Bool)])
menuActionsFor = actionsWith "menuActions" menuOnly

actionsWith :: Text -> (Project -> Project) -> FilePath -> (Int, Int) -> ServerState -> IO (ServerState, [(Action, Bool)])
actionsWith key offered path pos = memo key path (T.pack (show pos)) $ \s ->
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', [])
    (s', Just (root, project)) -> do
      acts0 <- actionsAt' (ssPorts s') (offered project) (makeRelative root path) pos
      acts <- extractOrder s' project path acts0
      let snippets = M.fromList [(k, isSnippet g) | (k, g) <- generatorsOf project]
      pure (s', [(a, M.findWithDefault False (actGenerator a) snippets) | a <- acts])

-- | Which extract leads (decision 2): of the strangler pack's extract
-- generators offered here, the one the function's code reads as (its
-- `signs`); unsure, a decider's answer (asked once, in the background)
-- decides it; else the first.
extractOrder :: ServerState -> Project -> FilePath -> [Action] -> IO [Action]
extractOrder s project path acts = case (catalogStrangler (prCatalog project), [a | a <- acts, isExtract a]) of
  (_, []) -> pure acts
  (Nothing, _) -> pure acts
  (Just st, extracts) -> do
    let rel = makeRelative (prRoot project) path
        lang = languageFor (catalogLanguages (prCatalog project)) rel
        functionOf a = lookup (stExtractFunction st) (actSets a)
    content <- fromMaybe "" <$> readText (prWorkspace project) rel
    let text = fromMaybe "" (listToMaybe [t | a <- extracts, Just f <- [functionOf a], Just t <- [functionText (maybe [] langUnitNames lang) content f]])
        function = fromMaybe "" (listToMaybe [f | a <- extracts, Just f <- [functionOf a]])
        (decisionId, question, options) = extractDecision (stExtract st)
    preferred <- case extractKind (maybe "" langComment lang) (stExtract st) text of
      Just k -> pure (Just k)
      Nothing -> case deciderFor s project of
        Nothing -> pure Nothing
        Just decider ->
          decision s decider (path, decisionId <> ":" <> function) (fingerprint [text]) (choice decisionId question options [(stExtractFunction st, text)]) (pure ()) <&> \case
            Just (Chosen k _ _) -> Just k
            _ -> Nothing
    let first = case [exGenerator e | Just k <- [preferred], e <- stExtract st, exName e == k] ++ map exGenerator (stExtract st) of
          g : _ -> g
          [] -> ""
        (lead, rest) = partition (\a -> actGenerator a == first) extracts
    pure ([a | a <- acts, not (isExtract a)] ++ lead ++ rest)
  where
    isExtract a = maybe False (\st -> actGenerator a `elem` map exGenerator (stExtract st)) (catalogStrangler (prCatalog project))

-- | A generator planned at a cursor (1-based line) with its inputs set: the
-- edits, and the snippet it placed at the cursor, if it is one. With
-- @fill@, the project's model (or the client's) fills its holes.
actionEdits :: Bool -> FilePath -> Int -> Text -> [(Text, Text)] -> ServerState -> IO (ServerState, Either Text ([FileEdit], Maybe Snippet))
actionEdits fill path line generator sets s =
  actionPlan fill path line generator sets s <&> \(s', planned) ->
    (s', fmap (\(root, plan) -> (editsOfPlan s' root plan, planSnippet plan)) planned)

-- | 'actionEdits' as the plan itself, with the project's root.
actionPlan :: Bool -> FilePath -> Int -> Text -> [(Text, Text)] -> ServerState -> IO (ServerState, Either Text (FilePath, Plan))
actionPlan = actionPlanWith (const (pure ()))

-- | 'actionPlan', telling @progress@ what it is doing (checking, asking the
-- model, testing a fill).
actionPlanWith :: (ProgressEvent -> IO ()) -> Bool -> FilePath -> Int -> Text -> [(Text, Text)] -> ServerState -> IO (ServerState, Either Text (FilePath, Plan))
actionPlanWith progress fill path line generator sets s =
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', Left (T.pack path <> " is in no Olifant project"))
    (s', Just (root, project)) -> fmap (s',) $ do
      let ports = ssPorts s'
          rel = makeRelative root path
      case findEntry (prCatalog project) generator of
        Left err -> pure (Left (describeCatalogError err))
        Right entry ->
          loadGenerator (pFiles ports) (pSyntax ports) (prCatalog project) entry >>= \case
            Left err -> pure (Left (describeCatalogError err))
            Right gen0 ->
              placeAt ports project (rel, line) [gen0] >>= \case
                Left why -> pure (Left why)
                Right gens -> fillerFor fill s' project >>= \case
                  Left why -> pure (Left why)
                  Right filler ->
                    planWith ports project planOptions {poAt = Just (rel, line), poSnippet = isSnippet gen0, poFiller = filler, poProgress = progress, poCancel = ssCancel s'} [Step g (M.fromList sets) | g <- gens] >>= \case
                      Left err -> pure (Left (describeGenerationError err))
                      Right plan
                        | not (null (planConflicts plan)) -> pure (Left (T.intercalate "; " (map describeConflict (planConflicts plan))))
                        | otherwise -> pure (Right (root, plan))

-- | The model that fills holes: none unless asked; recorded fills when the
-- client gave some (tests); else the client's model, or the project's.
fillerFor :: Bool -> ServerState -> Project -> IO (Either Text (Maybe (ModelFiller IO)))
fillerFor False _ _ = pure (Right Nothing)
fillerFor True s _ | Just filler <- ssTestFiller s = pure (Right (Just filler))
fillerFor True s project = case scFills (ssConfig s) of
  Just file -> fmap Just <$> recordedFiller s file
  Nothing -> pure (modelFillerWith (ssCancel s) (prCatalog project) (scModel (ssConfig s)))

-- | The filler of one rung of a steered conjure's ladder, named for the rung
-- (the backend's name): the test filler when set (every rung), recorded
-- fills when the client gave some; else that backend.
rungFiller :: ServerState -> Project -> Text -> IO (Either Text (ModelFiller IO))
rungFiller s project name = fmap (\f -> f {mfName = name}) <$> case (ssTestFiller s, scFills (ssConfig s)) of
  (Just filler, _) -> pure (Right filler)
  (_, Just file) -> recordedFiller s file
  _ -> pure (modelFillerWith (ssCancel s) (prCatalog project) (Just name) >>= maybe (Left ("no backend " <> name <> " under [model.backends]")) Right)

recordedFiller :: ServerState -> FilePath -> IO (Either Text (ModelFiller IO))
recordedFiller s file =
  readText (pFiles (ssPorts s)) file <&> \case
    Nothing -> Left ("recorded fills: cannot read " <> T.pack file)
    Just src -> ModelFiller "recorded" 0 . replayFiller <$> parseFills src

-- | A progress event in words, as editors show them.
describeProgress :: ProgressEvent -> Text
describeProgress = \case
  Checking lang -> "checking " <> lang <> "…"
  Filling holes attempt model -> "asking " <> model <> " to fill " <> T.intercalate ", " holes <> (if attempt > 1 then " (try " <> T.pack (show attempt) <> ")" else "") <> "…"
  Filled hole _ -> hole <> " filled"
  Unfilled hole why -> hole <> " left as a placeholder: " <> why
  AutoImported hole imp -> hole <> ": " <> imp
  Reindented hole _ -> hole <> ": read one level up"
  CutToHole hole n _ -> hole <> ": " <> cutSentence n
  WholeFile hole -> hole <> ": " <> wholeFileReason
  Verifying hole -> "testing " <> hole <> "…"
  Said words -> words
