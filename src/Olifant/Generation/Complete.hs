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
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Ord (Down (..))
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Anchor (Located (..), Spacing (..), insertLines, locateContainer)
import Olifant.Generation.Compose (StepPath, showStepPath)
import Olifant.Generation.Plan
import Olifant.Generation.Render (CompleteSpec (..))
import Olifant.Workspace (pruned)
import Olifant.Generation.Tools (Tools (..))
import Olifant.Language
import Olifant.Language.Elm (enclosingFunction, qualifyPattern, visibleAt)
import Olifant.Template (Value (..), describeTemplateError, render)
import Olifant.Workspace (Workspace (..))
import System.FilePath (splitDirectories)

maxRounds :: Int
maxRounds = 5

-- | One branch to add: which spec produced it, where, and its text.
data Branch = Branch {brStep :: StepPath, brSite :: MissingPatterns, brInfo :: Site, brText :: Text}

-- | Returns the added edits, conflicts, warnings, and the contents after them.
completeEdits :: Monad m => Tools m -> Workspace m -> [(StepPath, CompleteSpec)] -> Int -> Map FilePath (Maybe Text) -> m ([PlannedEdit], [Conflict], [Text], Map FilePath (Maybe Text))
completeEdits tools ws specs firstNumber current0 = foldM perLanguage ([], [], [], current0) (nub (map (completeLanguage . snd) specs))
  where
    perLanguage (edits, conflicts, warnings, current) name = case [s | s@(_, c) <- specs, completeLanguage c == name] of
      [] -> pure (edits, conflicts, warnings, current)
      mine@((step, _) : _) -> case find (\l -> langName l == name && isJust (langCheck l)) (toolLanguages tools) of
        Nothing -> pure (edits, conflicts ++ [CompleteFailed step ("no language adapter with a [check] for `" <> name <> "`") False], warnings, current)
        Just lang -> do
          (disk, files) <- gather lang current
          (es, cs, ws', current') <- loop lang step mine disk 1 S.empty (firstNumber + length edits) files current []
          pure (edits ++ es, conflicts ++ cs, warnings ++ ws', current')


    -- Everything the check needs: the adapter's sources and context files,
    -- as they will be after the positional edits.
    gather lang current = do
      paths <- listFiles ws "."
      let globs = langFiles lang ++ maybe [] checkContext (langCheck lang)
          wanted p = not (any (`elem` pruned) (splitDirectories p)) && any (`matchesGlob` p) globs
      read' <- traverse (\p -> (p,) <$> readText ws p) (filter wanted paths)
      let disk = M.fromList [(p, t) | (p, Just t) <- read']
      pure (disk, M.union (M.fromList [(p, t) | (p, Just t) <- M.toList current, wanted p]) disk)

    loop lang step mine disk rnd seen n files current acc
      | rnd > maxRounds = pure (reverse acc, [CompleteFailed step ("the compiler still reports missing cases after " <> T.pack (show maxRounds) <> " rounds") False], [], current)
      | otherwise =
          runCheck (toolChecker tools) lang files >>= \case
            Left f -> pure (reverse acc, [CompleteFailed step (describeCheckFailure f) (isToolMissing f)], [], current)
            Right report -> do
              let diags = reportMissing report
                  -- Candidates share the pattern's bare name and could appear
                  -- at the site at all (a Route module never sees Main's).
                  visible d c = langQualify lang /= Just "elm" || visibleAt (M.findWithDefault "" (missingFile d) files) (completeConstructor c)
                  wanted = [(d, p, cands) | d <- diags, p <- missingPatterns d, Just cands <- [nonEmpty [s | s@(_, c) <- mine, mentions (completeConstructor c) p, visible d c]]]
                  key (d, p, _) = (missingFile d, fst (missingCase d), p)
              case [k | w <- wanted, let k@(_, _, _) = key w, k `S.member` seen] of
                ((f, _, p) : _) -> pure (reverse acc, [CompleteFailed step ("the compiler still reports " <> p <> " in " <> T.pack f <> " after a branch was added") False], [], current)
                []
                  | null wanted -> pure (finish (reportBroken report))
                  | otherwise -> do
                      built <- traverse (branchFor lang files) wanted
                      case sequence built of
                        Left why -> pure (reverse acc, [CompleteFailed step why False], [], current)
                        Right branches -> do
                          let (edits, files') = applyBranches n branches files
                              current' = foldr (\(p, t) -> M.insert p (Just t)) current [(p, t) | (p, t) <- M.toList files', M.lookup p files /= Just t]
                          loop lang step mine disk (rnd + 1) (S.union seen (S.fromList (map key wanted))) (n + length edits) files' current' (reverse edits ++ acc)

      where
        -- Nothing left to add. A file that does not compile hides the
        -- missing cases of every module that imports it: if the plan changed
        -- it, the generated code is broken (conflict); otherwise the cases
        -- could not be checked (warning).
        finish broken =
          let changed f = M.lookup f disk /= M.lookup f files
              ours = [b | b@(f, _) <- broken, changed f]
              describe bs = T.intercalate ", " (nub [T.pack f <> " (" <> t <> ")" | (f, t) <- bs])
           in case (ours, broken) of
                (_ : _, _) -> (reverse acc, [CompleteFailed step ("the generated code does not compile: " <> describe ours) False], [], current)
                ([], _ : _) -> (reverse acc, [], [showStepPath step <> ": could not check for missing cases because the project does not compile: " <> describe broken], current)
                ([], []) -> (reverse acc, [], [], current)

    branchFor lang files (d, p, cands) = do
      let file = missingFile d
          content = M.findWithDefault "" file files
          fn = enclosingFunction content (fst (missingCase d))
      matching <- filterM (whereMatches lang content d . snd) (NE.toList cands)
      let chosen = listToMaybe matching
          (step, spec) = fromMaybe (NE.head cands) chosen
          ctors = nub (map (completeConstructor . snd) (NE.toList cands))
          pat = if langQualify lang == Just "elm" then qualifyPattern content (completeConstructor spec) p else p
          site = VRecord (M.fromList [("file", VText (T.pack file)), ("function", VText fn)])
          text = case chosen of
            Just _ -> either (Left . describeTemplateError) Right (render (M.insert "pattern" (VText pat) (M.insert "site" site (completeEnv spec))) (completeBody spec))
            Nothing -> todoBranch lang pat fn
      -- Elm prints constructors unqualified, so with no `where` to decide,
      -- two edits sharing a bare name cannot be told apart: never guess.
      pure $
        if isNothing chosen && length ctors > 1
          then Left (p <> " in " <> fn <> " (" <> T.pack file <> ") could be any of " <> T.intercalate ", " ctors <> "; add a `complete` edit with `where = \"case in " <> fn <> "\"`, or give the case a catch-all")
          else Branch step d (Site file fn pat (isNothing chosen)) <$> text

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
-- valid. Edits are numbered in that application order, so each reported line
-- is where the text lands when the plan's edits are replayed in order.
applyBranches :: Int -> [Branch] -> Map FilePath Text -> ([PlannedEdit], Map FilePath Text)
applyBranches n branches files0 = (reverse edits, final)
  where
    order = sortOn (\(k, b) -> (missingFile (brSite b), Down (snd (missingCase (brSite b))), Down k)) (zip [0 :: Int ..] branches)
    (edits, final) = foldl step ([], files0) (zip [n ..] (map snd order))
    step (acc, files) (i, b) =
      let file = missingFile (brSite b)
          (line, updated) = insertBranch (missingCase (brSite b)) (brText b) (M.findWithDefault "" file files)
          edit = PlannedEdit (brStep b) i file (WillInsert (line + 1) (brText b)) Nothing (Just (brInfo b))
       in (edit : acc, M.insert file updated files)

insertBranch :: (Int, Int) -> Text -> Text -> (Int, Text)
insertBranch (caseStart, caseEnd) branch content =
  let ls = T.lines content
      indent = case [l | l <- drop (caseStart + 1) ls, not (T.null (T.strip l))] of
        (l : _) -> T.takeWhile (\c -> c == ' ' || c == '\t') l
        [] -> "    "
   in insertLines (Located (caseEnd + 1) indent "" "" NoSpacing) ("\n" <> branch) content

bareName :: Text -> Text
bareName = snd . T.breakOnEnd "."

mentions :: Text -> Text -> Bool
mentions ctor pattern = bareName ctor `elem` T.split (\c -> not (isAlphaNum c || c == '_')) pattern
