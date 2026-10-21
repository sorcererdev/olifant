-- | Internal to Olifant.Server: what follows from an edit, offered unasked
-- (spec §4.8): the first suggested action whose place covers the edited
-- line, planned without the model. A short insert at the line is a ghost;
-- anything else is a follow-up, summarised ("+1 line here, +2 in
-- failures.py") for the editor to offer and review.
module Olifant.Server.Suggest (Suggestion (..), suggestionFor, suggestionOf) where

import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Actions (Action (..), FileAction (..), Range (..))
import Olifant.App (actionsIn, generatorsOf, menuOnly)
import Olifant.Cursor (isSnippet)
import Olifant.Generation (Plan (..))
import Olifant.Generator (AppliesAt (..), GeneratorWith (..))
import Olifant.Recognize (Recognition)
import Olifant.Server.Actions (actionPlan)
import Olifant.Server.Edits (FileEdit (..), editsOfPlan)
import Olifant.Server.State (ServerState (..), memo, projectFor)
import Olifant.Workspace (normalisePath)
import System.FilePath (makeRelative, takeFileName, (</>))

-- | A suggestion: "ghost" (its lines, from a line) or "followup" (a
-- summary), the action's label, and the plan with its project's root.
data Suggestion = Suggestion
  { sgKind :: Text
  , sgLabel :: Text
  , sgSummary :: Text
  , sgLine :: Int
  , sgLines :: [Text]
  , sgRoot :: FilePath
  , sgPlan :: Plan
  , -- | what was planned, and at which line: to plan it again, filled
    sgGenerator :: Text
  , sgSets :: [(Text, Text)]
  , sgAt :: Int
  , -- | a recognition's band, choice and reasons (convention recognition)
    sgRecognition :: Maybe Recognition
  }

-- | What follows from an edit at a line (1-based) of a file; nothing when
-- no suggested action covers it, or its plan changes nothing.
suggestionFor :: FilePath -> Int -> ServerState -> IO (ServerState, Maybe Suggestion)
suggestionFor path line = memo "suggest" path (T.pack (show line)) $ \s ->
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', Nothing)
    (s', Just (root, project)) -> do
      (acts, _) <- actionsIn (ssPorts s') (menuOnly project) (makeRelative root path)
      let suggested = M.fromList [(k, not (isSnippet g) && maybe False apSuggest (genAppliesAt g)) | (k, g) <- generatorsOf project]
          candidates =
            [ fa
            | fa@(FileAction a (Just (Range (l1, _) (l2, _)))) <- acts
            , l1 <= line && line <= l2
            , M.findWithDefault False (actGenerator a) suggested
            , null (actMissing a)
            ]
          try [] = pure Nothing
          try (fa : rest) = snd <$> suggestionOf fa path line s' >>= maybe (try rest) (pure . Just)
      (s',) <$> try candidates

-- | One action at a line (1-based) as its suggestion: a ghost when its
-- plan is a short insert at the line, else a follow-up; nothing when its
-- plan fails or changes nothing.
suggestionOf :: FileAction -> FilePath -> Int -> ServerState -> IO (ServerState, Maybe Suggestion)
suggestionOf (FileAction a _) path line s =
  actionPlan False path line (actGenerator a) (actSets a) s >>= \case
    (s', Right (r, plan)) | not (M.null (planWrites plan)) -> pure (s', Just (classify r plan))
    (s', _) -> pure (s', Nothing)
  where
    classify r plan = case editsOfPlan s r plan of
      [e]
        | fePath e == normalisePath path
        , ((l1, 0), end) <- feRange e
        , end == (l1, 0)
        , let inserted = T.lines (feText e)
        , length inserted <= 8
        , l1 + 1 `elem` [line, line + 1] ->
            Suggestion "ghost" (actLabel a) "" (l1 + 1) inserted r plan (actGenerator a) (actSets a) line Nothing
      _ -> Suggestion "followup" (actLabel a) (summary r plan) line [] r plan (actGenerator a) (actSets a) line Nothing
    summary root plan =
      let counts = [(p, added (fromMaybe Nothing (M.lookup p (planBefore plan))) new) | (p, new) <- M.toList (planWrites plan)]
          here = sum [n | (p, n) <- counts, normalisePath (root </> p) == normalisePath path]
          elsewhere = [(p, n) | (p, n) <- counts, normalisePath (root </> p) /= normalisePath path, n > 0]
          plural n = T.pack (show n) <> (if n == 1 then " line" else " lines")
       in T.intercalate ", " $
            ["+" <> plural here <> " here" | here > 0]
              ++ ["+" <> T.pack (show (sum (map snd elsewhere))) <> " in " <> T.intercalate ", " [T.pack (takeFileName p) | (p, _) <- elsewhere] | not (null elsewhere)]
    added before new = max 0 (length (T.lines new) - maybe 0 (length . T.lines) before)
