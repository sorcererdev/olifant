-- | `complete` edits (spec §5.6): after the positional edits, run the
-- language's check and add a branch to every `case` the compiler reports as
-- missing a constructor this plan introduced. Unmatched sites get the
-- adapter's placeholder and are reported as todos.
module Olifant.Generation.Complete (completeEdits) where

import Control.Monad (filterM, foldM)
import Data.Char (isAlphaNum)
import Data.List (find, nub, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Ord (Down (..))
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Anchor (Located (..), insertLines, locateContainer)
import Olifant.Generation.Compose (StepPath)
import Olifant.Generation.Plan
import Olifant.Generation.Render (CompleteSpec (..))
import Olifant.Generation.Tools (Tools (..))
import Olifant.Language
import Olifant.Language.Elm (enclosingFunction, qualifyPattern)
import Olifant.Template (Value (..), describeTemplateError, render)
import Olifant.Workspace (Workspace (..))
import System.FilePath (splitDirectories)

maxRounds :: Int
maxRounds = 5

-- | One branch to add: which spec produced it, where, and its text.
data Branch = Branch {brStep :: StepPath, brSite :: MissingPatterns, brInfo :: Site, brText :: Text}

completeEdits :: Monad m => Tools m -> Workspace m -> [(StepPath, CompleteSpec)] -> Int -> Map FilePath (Maybe Text) -> m ([PlannedEdit], [Conflict], Map FilePath (Maybe Text))
completeEdits tools ws specs firstNumber current0 = foldM perLanguage ([], [], current0) (nub (map (completeLanguage . snd) specs))
  where
    perLanguage (edits, conflicts, current) name = case [s | s@(_, c) <- specs, completeLanguage c == name] of
      [] -> pure (edits, conflicts, current)
      mine@((step, _) : _) -> case find (\l -> langName l == name && isJust (langCheck l)) (toolLanguages tools) of
        Nothing -> pure (edits, conflicts ++ [CompleteFailed step ("no language adapter with a [check] for `" <> name <> "`") False], current)
        Just lang -> do
          files <- gather lang current
          (es, cs, current') <- loop lang step mine 1 S.empty (firstNumber + length edits) files current []
          pure (edits ++ es, conflicts ++ cs, current')

    ignored = ["elm-stuff", "node_modules", ".git", "dist-newstyle", "target"] :: [FilePath]

    -- Everything the check needs: the adapter's sources and context files,
    -- as they will be after the positional edits.
    gather lang current = do
      paths <- listFiles ws "."
      let globs = langFiles lang ++ maybe [] checkContext (langCheck lang)
          wanted p = not (any (`elem` ignored) (splitDirectories p)) && any (`matchesGlob` p) globs
      disk <- traverse (\p -> (p,) <$> readText ws p) (filter wanted paths)
      pure (M.union (M.fromList [(p, t) | (p, Just t) <- M.toList current, wanted p]) (M.fromList [(p, t) | (p, Just t) <- disk]))

    loop lang step mine rnd seen n files current acc
      | rnd > maxRounds = pure (reverse acc, [CompleteFailed step ("the compiler still reports missing cases after " <> T.pack (show maxRounds) <> " rounds") False], current)
      | otherwise =
          runCheck (toolChecker tools) lang files >>= \case
            Left f -> pure (reverse acc, [CompleteFailed step (describeCheckFailure f) (isToolMissing f)], current)
            Right diags -> do
              let wanted = [(d, p, cands) | d <- diags, p <- missingPatterns d, let cands = [s | s@(_, c) <- mine, mentions (completeConstructor c) p], not (null cands)]
                  key (d, p, _) = (missingFile d, fst (missingCase d), p)
              case [k | w <- wanted, let k@(_, _, _) = key w, k `S.member` seen] of
                ((f, _, p) : _) -> pure (reverse acc, [CompleteFailed step ("the compiler still reports " <> p <> " in " <> T.pack f <> " after a branch was added") False], current)
                []
                  | null wanted -> pure (reverse acc, [], current)
                  | otherwise -> do
                      built <- traverse (branchFor lang files) wanted
                      case sequence built of
                        Left why -> pure (reverse acc, [CompleteFailed step why False], current)
                        Right branches -> do
                          let (edits, files') = applyBranches n branches files
                              current' = foldr (\(p, t) -> M.insert p (Just t)) current [(p, t) | (p, t) <- M.toList files', M.lookup p files /= Just t]
                          loop lang step mine (rnd + 1) (S.union seen (S.fromList (map key wanted))) (n + length edits) files' current' (reverse edits ++ acc)

    branchFor lang files (d, p, cands) = do
      let file = missingFile d
          content = M.findWithDefault "" file files
          fn = enclosingFunction content (fst (missingCase d))
      matching <- filterM (whereMatches lang content d . snd) cands
      let chosen = listToMaybe matching
          (step, spec) = maybe (head' cands) id chosen
          pat = if langQualify lang == Just "elm" then qualifyPattern content (completeConstructor spec) p else p
          site = VRecord (M.fromList [("file", VText (T.pack file)), ("function", VText fn)])
          text = case chosen of
            Just _ -> either (Left . describeTemplateError) Right (render (M.insert "pattern" (VText pat) (M.insert "site" site (completeEnv spec))) (completeBody spec))
            Nothing -> todoBranch lang pat fn
      pure (Branch step d (Site file fn pat (isNothing chosen)) <$> text)
      where
        head' (x : _) = x
        head' [] = error "branchFor: candidates are never empty"

    whereMatches lang content d spec = case T.words <$> completeWhere spec of
      Nothing -> pure True
      Just [] -> pure True
      Just ws' -> either (const False) ((== fst (missingCase d)) . fst) <$> locateContainer (toolSyntax tools) lang ws' content

    todoBranch lang pat fn = case (langBranch lang, langPlaceholder lang) of
      (Just b, Just ph) -> either (Left . describeTemplateError) Right $ do
        body <- render (M.fromList [("message", VText ("olifant: " <> fn <> " " <> pat))]) ph
        render (M.fromList [("pattern", VText pat), ("body", VText body)]) b
      _ -> Left ("the " <> langName lang <> " adapter needs `branch` and `placeholder` to leave a todo for " <> pat <> " in " <> fn)

    isToolMissing = \case
      CheckToolMissing _ -> True
      CheckFailed _ -> False

-- | Insert every branch, bottom-up within a file so earlier line numbers stay
-- valid; edits are numbered in diagnostic order. Reported lines are those at
-- insertion time.
applyBranches :: Int -> [Branch] -> Map FilePath Text -> ([PlannedEdit], Map FilePath Text)
applyBranches n branches files0 = (map snd (sortOn fst numbered), final)
  where
    indexed = zip [n ..] branches
    order = sortOn (\(i, b) -> (missingFile (brSite b), Down (snd (missingCase (brSite b))), Down i)) indexed
    (numbered, final) = foldl step ([], files0) order
    step (acc, files) (i, b) =
      let file = missingFile (brSite b)
          (line, updated) = insertBranch (missingCase (brSite b)) (brText b) (M.findWithDefault "" file files)
          edit = PlannedEdit (brStep b) i file (WillInsert (line + 1) (brText b)) (Just ("compiler: missing " <> sitePattern (brInfo b))) (Just (brInfo b))
       in ((i, edit) : acc, M.insert file updated files)

insertBranch :: (Int, Int) -> Text -> Text -> (Int, Text)
insertBranch (caseStart, caseEnd) branch content =
  let ls = T.lines content
      indent = case [l | l <- drop (caseStart + 1) ls, not (T.null (T.strip l))] of
        (l : _) -> T.takeWhile (\c -> c == ' ' || c == '\t') l
        [] -> "    "
   in insertLines (Located (caseEnd + 1) indent "" "") ("\n" <> branch) content

bareName :: Text -> Text
bareName = snd . T.breakOnEnd "."

mentions :: Text -> Text -> Bool
mentions ctor pattern = bareName ctor `elem` T.split (\c -> not (isAlphaNum c || c == '_')) pattern
