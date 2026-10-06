-- | Internal to Olifant.Server: the ⚡ actions at a cursor, and an action
-- planned there (a snippet placed at the cursor, as `gen --at` does) with
-- the choices made — against the documents as they are now.
module Olifant.Server.Actions (actionsFor, actionEdits, actionPlan, actionPlanWith, fillerFor, describeProgress) where

import Data.Functor ((<&>))
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Actions (Action (..))
import Olifant.App (PlanOptions (..), Ports (..), Project (..), actionsAt', generatorsOf, modelFiller, placeAt, planOptions, planWith)
import Olifant.Catalog (describeCatalogError, findEntry, loadGenerator)
import Olifant.Cursor (isSnippet)
import Olifant.Fill (ModelFiller (..), parseFills, replayFiller)
import Olifant.Generation (Plan (..), ProgressEvent (..), Step (..), describeConflict, describeGenerationError)
import Olifant.Workspace (Workspace (..))
import Olifant.Snippet (Snippet)
import Olifant.Server.Edits (FileEdit, editsOfPlan)
import Olifant.Server.State (ServerConfig (..), ServerState (..), projectFor)
import System.FilePath (makeRelative)

-- | The actions at a position (1-based line, 1-based byte column) of a
-- file, each with whether it is a snippet; none in no project.
actionsFor :: FilePath -> (Int, Int) -> ServerState -> IO (ServerState, [(Action, Bool)])
actionsFor path pos s =
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', [])
    (s', Just (root, project)) -> do
      acts <- actionsAt' (ssPorts s') project (makeRelative root path) pos
      let snippets = M.fromList [(k, isSnippet g) | (k, g) <- generatorsOf project]
      pure (s', [(a, M.findWithDefault False (actGenerator a) snippets) | a <- acts])

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
                    planWith ports project planOptions {poAt = Just (rel, line), poFiller = filler, poProgress = progress} [Step g (M.fromList sets) | g <- gens] >>= \case
                      Left err -> pure (Left (describeGenerationError err))
                      Right plan
                        | not (null (planConflicts plan)) -> pure (Left (T.intercalate "; " (map describeConflict (planConflicts plan))))
                        | otherwise -> pure (Right (root, plan))

-- | The model that fills holes: none unless asked; recorded fills when the
-- client gave some (tests); else the client's model, or the project's.
fillerFor :: Bool -> ServerState -> Project -> IO (Either Text (Maybe (ModelFiller IO)))
fillerFor False _ _ = pure (Right Nothing)
fillerFor True s project = case scFills (ssConfig s) of
  Just file ->
    readText (pFiles (ssPorts s)) file <&> \case
      Nothing -> Left ("recorded fills: cannot read " <> T.pack file)
      Just src -> (\fills -> Just (ModelFiller "recorded" 0 (replayFiller fills))) <$> parseFills src
  Nothing -> pure (modelFiller (prCatalog project) (scModel (ssConfig s)))

-- | A progress event in words, as editors show them.
describeProgress :: ProgressEvent -> Text
describeProgress = \case
  Checking lang -> "checking " <> lang <> "…"
  Filling holes attempt model -> "asking " <> model <> " to fill " <> T.intercalate ", " holes <> (if attempt > 1 then " (try " <> T.pack (show attempt) <> ")" else "") <> "…"
  Filled hole _ -> hole <> " filled"
  Unfilled hole why -> hole <> " left as a placeholder: " <> why
  AutoImported hole imp -> hole <> ": " <> imp
  Verifying hole -> "testing " <> hole <> "…"
