-- | Internal to Olifant.Server: a file's rule findings (with their fixes'
-- open choices), and a fix planned as edits — always against the
-- documents as they are now.
module Olifant.Server.Fixes (findingsFor, fixEdits, fixPlan) where

import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.App (FixFailure (..), PlanOptions (..), findingsWith, fixSteps, newnessWith, planOptions, planWith)
import Olifant.Catalog (describeCatalogError)
import Olifant.Generation (Plan (..), ProgressEvent, describeConflict, describeGenerationError)
import Olifant.Server.Actions (fillerFor)
import Olifant.Rules (Finding (..), RuleDef (..), RulesConfig (..), Severity (..))
import Olifant.App (Project (..))
import Olifant.Catalog (Catalog (..))
import Control.Concurrent (forkIO)
import Control.Monad (forM, join, void, when)
import Data.IORef (atomicModifyIORef', modifyIORef', readIORef)
import Data.List (find)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Olifant.Server.Edits (FileEdit, editsOfPlan)
import Olifant.Server.Recognize (deciderFor)
import Olifant.Decide (Answer (..), Decider (..), Question (..))
import Olifant.Server.State (cachedBase, ServerState (..), memo, projectFor)
import System.FilePath (makeRelative)

-- | The rules' findings in a file (project-relative paths in them); none
-- for a file in no project.
findingsFor :: FilePath -> ServerState -> IO (ServerState, [Finding])
findingsFor path s0 = do
  (s1, fs) <- rawFindings path s0
  -- decisions arrive in the background: applied on every ask, not memoised
  projectFor path s1 >>= \case
    (s2, Just (_, project)) -> (s2,) <$> decided s2 project path fs
    (s2, Nothing) -> pure (s2, fs)

-- | A file's findings as the rules and the migration see them (memoised
-- per edit).
rawFindings :: FilePath -> ServerState -> IO (ServerState, [Finding])
rawFindings path = memo "findings" path "" $ \s ->
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', [])
    (s', Just (root, project)) -> (s',) . fst <$> findingsWith (newnessWith (cachedBase s') project) (ssPorts s') project [makeRelative root path]

-- | New work in disguise, sharpened (decision 1): a decider's answer for a
-- hint — new behaviour makes it the bug-fix-only warning, a bug fix drops
-- it, unsure keeps the hint. Unanswered, it is asked once in the background
-- (the deterministic hint stands meanwhile); when it is, the editor is told.
decided :: ServerState -> Project -> FilePath -> [Finding] -> IO [Finding]
decided s project path fs = case deciderFor s project of
  Nothing -> pure fs
  Just decider -> do
    known <- readIORef (ssDecisions s)
    fmap concat . forM fs $ \f ->
      if fRule f /= "migrate-new-work"
        then pure [f]
        else case M.lookup (path, fFingerprint f) known of
          Just (Chosen "new behaviour" _) -> pure [f {fRule = "legacy-bug-fix-only", fSeverity = severityOf "legacy-bug-fix-only", fMessage = "New work goes in the new architecture (legacy is bug-fix-only): this change adds behaviour"}]
          Just (Chosen "bug fix" _) -> pure []
          Just _ -> pure [f]
          Nothing -> do
            fresh <- atomicModifyIORef' (ssAsking s) (\a -> (S.insert (path, fFingerprint f) a, S.notMember (path, fFingerprint f) a))
            when fresh . void . forkIO $ do
              answer <- ask decider (Question "bug-or-new" ["bug fix", "new behaviour"] [] [("function", fText f)])
              modifyIORef' (ssDecisions s) (M.insert (path, fFingerprint f) answer)
              join (readIORef (ssOnDecided s))
            pure [f]
  where
    cat = prCatalog project
    severityOf r = fromMaybe (maybe Warn ruleSeverity (find ((== r) . ruleId) (catalogRules cat))) (lookup r (rcSeverity (catalogRulesConfig cat)))

-- | The edits of a rule's fix (the @n@th, 1-based) at a line of a file,
-- with the choices made: planned now, so a finding that moved or went
-- away says so instead of editing stale text.
fixEdits :: FilePath -> Text -> Int -> Int -> [(Text, Text)] -> ServerState -> IO (ServerState, Either Text [FileEdit])
fixEdits path rule line n sets s = fixPlan False (const (pure ())) path rule line n sets s <&> \(s', planned) -> (s', (\(root, plan) -> editsOfPlan s' root plan) <$> planned)

-- | 'fixEdits' as the plan itself, with the project's root; with @fill@,
-- the model fills its holes (as `olifant fix` does), telling @progress@.
fixPlan :: Bool -> (ProgressEvent -> IO ()) -> FilePath -> Text -> Int -> Int -> [(Text, Text)] -> ServerState -> IO (ServerState, Either Text (FilePath, Plan))
fixPlan fill progress path rule line n sets s =
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', Left (T.pack path <> " is in no Olifant project"))
    (s', Just (root, project)) ->
      fmap (s',) $
        fixSteps (ssPorts s') project rule (makeRelative root path) line n sets >>= \case
          Left failure -> pure (Left (describeFix failure))
          Right (context, steps) ->
            fillerFor fill s' project >>= \case
             Left why -> pure (Left why)
             Right filler ->
              first describeGenerationError <$> planWith (ssPorts s') project planOptions {poContext = context, poFiller = filler, poProgress = progress, poCancel = ssCancel s'} steps >>= \case
              Left why -> pure (Left why)
              Right plan
                | not (null (planConflicts plan)) -> pure (Left (T.intercalate "; " (map describeConflict (planConflicts plan))))
                | otherwise -> pure (Right (root, plan))
  where
    describeFix = \case
      FixNotFound why -> why <> " (the code changed since: ask for the actions again)"
      FixUsage why -> why
      FixCatalog err -> describeCatalogError err
      FixFailed why -> why
