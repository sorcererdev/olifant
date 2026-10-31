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
import Olifant.Rules (Finding (..), RuleDef (..), RulesConfig (..), Severity (..), definedNameIn)
import Olifant.App (Project (..))
import Olifant.Catalog (Catalog (..))
import Control.Monad (forM, join)
import Data.IORef (readIORef)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Olifant.Server.Edits (FileEdit, editsOfPlan)
import Olifant.Migration (Strangler (..))
import Olifant.Decide (Answer (..), choice)
import Olifant.Server.State (decision, deciderFor, cachedBase, ServerState (..), memo, projectFor)
import System.FilePath (makeRelative)
import Olifant.Language (languageFor)

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
-- hint — new behaviour makes it the strangler pack's `bug_fix_only` rule's
-- finding (none declared: the hint stays), a bug fix drops it, unsure keeps
-- the hint. Unanswered, it is asked once in the background
-- (the deterministic hint stands meanwhile); when it is, the editor is told.
decided :: ServerState -> Project -> FilePath -> [Finding] -> IO [Finding]
decided s project path fs = case deciderFor s project of
  Nothing -> pure fs
  Just decider ->
    fmap concat . forM fs $ \f ->
      if fRule f /= "migrate-new-work"
        then pure [f]
        else
          decision s decider (path, "bug-or-new:" <> definedNameIn (languageFor (catalogLanguages cat) (makeRelative (prRoot project) path)) (fText f)) (fFingerprint f) (choice "bug-or-new" "Does this change fix a bug in the function, or add new behaviour" [("bug fix", ""), ("new behaviour", "")] [("function", fText f)]) (join (readIORef (ssOnDecided s))) <&> \case
            Just (Chosen "new behaviour" _ _) -> case stBugFixOnly =<< catalogStrangler cat of
              Just r -> [f {fRule = r, fSeverity = severityOf r, fMessage = maybe "New work goes in the new architecture" ruleMessage (find ((== r) . ruleId) (catalogRules cat)) <> ": this change adds behaviour"}]
              Nothing -> [f]
            Just (Chosen "bug fix" _ _) -> []
            _ -> [f]
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
          Right (context, steps, warnings) ->
            fillerFor fill s' project >>= \case
             Left why -> pure (Left why)
             Right filler ->
              first describeGenerationError <$> planWith (ssPorts s') project planOptions {poContext = context, poFiller = filler, poProgress = progress, poCancel = ssCancel s'} steps >>= \case
              Left why -> pure (Left why)
              Right plan
                | not (null (planConflicts plan)) -> pure (Left (T.intercalate "; " (map describeConflict (planConflicts plan))))
                | otherwise -> pure (Right (root, plan {planWarnings = planWarnings plan ++ warnings}))
  where
    describeFix = \case
      FixNotFound why -> why <> " (the code changed since: ask for the actions again)"
      FixUsage why -> why
      FixCatalog err -> describeCatalogError err
      FixFailed why -> why
