-- | Internal to Olifant.Server: the ⚡ actions at a cursor, and an action
-- planned there (a snippet placed at the cursor, as `gen --at` does) with
-- the choices made — against the documents as they are now.
module Olifant.Server.Actions (actionsFor, menuActionsFor, actionEdits, actionPlan, actionPlanWith, fillerFor, describeProgress) where

import Data.Functor ((<&>))
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Actions (Action (..))
import Olifant.App (PlanOptions (..), Ports (..), Project (..), actionsAt', generatorsOf, modelFillerWith, placeAt, planOptions, planWith)
import Olifant.Catalog (describeCatalogError, findEntry, loadGenerator)
import Olifant.Cursor (isSnippet)
import Olifant.Generator (GeneratorWith (..))
import Olifant.Fill (ModelFiller (..), parseFills, replayFiller)
import Olifant.Generation (Plan (..), ProgressEvent (..), Step (..), describeConflict, describeGenerationError)
import Olifant.Workspace (Workspace (..))
import Olifant.Snippet (Snippet)
import Olifant.Server.Edits (FileEdit, editsOfPlan)
import Olifant.Migration (functionText, readsOrWrites)
import Olifant.Rules (fingerprint)
import Olifant.Decide (Answer (..), Question (..))
import Data.List (partition)
import Data.Maybe (fromMaybe, listToMaybe)
import Olifant.Server.State (decision, deciderFor, ServerConfig (..), ServerState (..), memo, projectFor)
import System.FilePath (makeRelative)

-- | The actions at a position (1-based line, 1-based byte column) of a
-- file, each with whether it is a snippet; none in no project.
actionsFor :: FilePath -> (Int, Int) -> ServerState -> IO (ServerState, [(Action, Bool)])
actionsFor path pos = memo "actions" path (T.pack (show pos)) $ \s ->
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', [])
    (s', Just (root, project)) -> do
      acts0 <- actionsAt' (ssPorts s') project (makeRelative root path) pos
      acts <- extractOrder s' project path acts0
      let snippets = M.fromList [(k, isSnippet g) | (k, g) <- generatorsOf project]
      pure (s', [(a, M.findWithDefault False (actGenerator a) snippets) | a <- acts])

-- | The actions the ⚡ menu and the picker offer at a position: those of
-- generators with `menu = false` left out (they stay callable: the CLI,
-- other generators, completion, a picker's steps).
menuActionsFor :: FilePath -> (Int, Int) -> ServerState -> IO (ServerState, [(Action, Bool)])
menuActionsFor path pos s = do
  (s', acts) <- actionsFor path pos s
  (s'', found) <- projectFor path s'
  let menu = maybe M.empty (\(_, project) -> M.fromList [(k, genMenu g) | (k, g) <- generatorsOf project]) found
  pure (s'', [x | x@(a, _) <- acts, M.findWithDefault True (actGenerator a) menu])

-- | Extract as a command or a query (decision 2): the one the function's
-- code reads as first (it writes: command; it only reads: query); unsure,
-- a decider's answer (asked once, in the background) decides it.
extractOrder :: ServerState -> Project -> FilePath -> [Action] -> IO [Action]
extractOrder s project path acts = case [a | a <- acts, isExtract a] of
  [] -> pure acts
  extracts -> do
    content <- fromMaybe "" <$> readText (prWorkspace project) (makeRelative (prRoot project) path)
    let text = fromMaybe "" (listToMaybe [t | a <- extracts, Just f <- [lookup "function" (actSets a)], Just t <- [functionText content f]])
        function = fromMaybe "" (listToMaybe [f | a <- extracts, Just f <- [lookup "function" (actSets a)]])
    preferred <- case readsOrWrites text of
      Just k -> pure (Just k)
      Nothing -> case deciderFor s project of
        Nothing -> pure Nothing
        Just decider ->
          decision s decider (path, "command-or-query:" <> function) (fingerprint [text]) (Question "command-or-query" ["command", "query"] [] [("function", text)]) (pure ()) <&> \case
            Just (Chosen k _) -> Just k
            _ -> Nothing
    let first = case preferred of
          Just "query" -> "/extract-query"
          _ -> "/extract-command"
        (lead, rest) = partition (\a -> first `T.isSuffixOf` actGenerator a) extracts
    pure ([a | a <- acts, not (isExtract a)] ++ lead ++ rest)
  where
    isExtract a = any (`T.isSuffixOf` actGenerator a) ["/extract-command", "/extract-query"]

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
  Just file ->
    readText (pFiles (ssPorts s)) file <&> \case
      Nothing -> Left ("recorded fills: cannot read " <> T.pack file)
      Just src -> (\fills -> Just (ModelFiller "recorded" 0 (replayFiller fills))) <$> parseFills src
  Nothing -> pure (modelFillerWith (ssCancel s) (prCatalog project) (scModel (ssConfig s)))

-- | A progress event in words, as editors show them.
describeProgress :: ProgressEvent -> Text
describeProgress = \case
  Checking lang -> "checking " <> lang <> "…"
  Filling holes attempt model -> "asking " <> model <> " to fill " <> T.intercalate ", " holes <> (if attempt > 1 then " (try " <> T.pack (show attempt) <> ")" else "") <> "…"
  Filled hole _ -> hole <> " filled"
  Unfilled hole why -> hole <> " left as a placeholder: " <> why
  AutoImported hole imp -> hole <> ": " <> imp
  Reindented hole _ -> hole <> ": read one level up"
  Verifying hole -> "testing " <> hole <> "…"
