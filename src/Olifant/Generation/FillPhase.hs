-- | The fill phase of `gen` (spec §6.4): one request for every hole, each
-- fill parsed as its kind (the adapter's `wrap`) and compile-checked (its
-- `[check]`), failing holes retried with the reason, and any still failing
-- left as their placeholder. Holes never block a generation.
module Olifant.Generation.FillPhase (fillPhase) where

import Control.Monad (foldM, forM, forM_)
import Data.Aeson (object, (.=))
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Fill
import Olifant.Generation.Plan (HoleStatus (..), PlannedHole (..))
import Olifant.Generation.Tools (ProgressEvent (..), Tools (..))
import Olifant.Holes (HoleSite (..))
import Olifant.Language
import Olifant.Workspace (Workspace (..), pruned)
import System.FilePath (splitDirectories)

-- | A hole being tried: attempts so far and the previous (fill, why).
data Try = Try {tryHole :: PlannedHole, tryAttempts :: Int, tryPrevious :: Maybe (Text, Text)}

fillPhase :: Monad m => Workspace m -> Tools m -> [(Text, Text)] -> Map FilePath Text -> [PlannedHole] -> m (Map FilePath Text, [PlannedHole], [Text])
fillPhase ws tools inputs base holes = case toolFiller tools of
  Nothing -> pure (base, holes, [])
  Just _ | null holes -> pure (base, holes, [])
  Just mf -> loop mf 1 M.empty [] [Try h 0 Nothing | h <- holes]
  where
    -- accepted: hole name -> (site, fill, attempts); notes: why a check could not run
    loop mf r accepted notes pending = do
      toolProgress tools (Filling [name t | t <- pending] r (mfName mf))
      reply <- runFill (mfFiller mf) (FillRequest [fillHole t | t <- pending] inputs (toolFillContext tools))
      let tried = [t {tryAttempts = tryAttempts t + 1} | t <- pending]
          next accepted' notes' failed
            | null failed || r >= 1 + max 0 (mfRetries mf) = do
                forM_ failed (\(t, why) -> toolProgress tools (Unfilled (name t) why))
                pure (finish mf accepted' notes' failed)
            | otherwise = loop mf (r + 1) accepted' notes' (map fst failed)
      case reply of
        -- An unreadable reply is a failed attempt, retried like any other.
        Left why -> next accepted notes [(t, why) | t <- tried]
        Right fills -> do
          let noFill = [(t, "the model returned no fill for `" <> name t <> "`") | t <- tried, M.notMember (name t) fills]
              offered = [(t, f) | t <- tried, Just f <- [M.lookup (name t) fills]]
              escaped = [(t, f, why) | (t, f) <- offered, Just why <- [escapes f]]
          parsed <- forM [(t, f) | (t, f) <- offered, Nothing <- [escapes f]] $ \(t, f) -> (t,f,) <$> parses (site t) f
          let badParse = escaped ++ [(t, f, "does not parse as " <> holeKind (site t)) | (t, f, False) <- parsed]
              candidates = [(t, f) | (t, f, True) <- parsed]
              known = [(s', f') | (s', f', _) <- M.elems accepted]
              with cs = applyFills base (known ++ [(site t, f) | (t, f) <- cs])
          (broken0, note) <- checkFiles (with candidates) [site t | (t, _) <- candidates]
          -- errors the files had with their placeholders are not the fills'
          -- (python review I1); checked only when something is broken
          already <- if M.null broken0 then pure M.empty else fst <$> checkFiles (with []) [site t | (t, _) <- candidates]
          let notAlready = M.filter (not . null) . M.mapWithKey (\p es -> filter (`notElem` M.findWithDefault [] p already) es)
              broken = notAlready broken0
          -- All together compile: every candidate stands. Otherwise each is
          -- checked alone beside what is already accepted, so a fill that
          -- breaks another file is the one blamed (review I5).
          verdicts <-
            if M.null broken
              then pure [(t, f, Nothing) | (t, f) <- candidates]
              else forM candidates $ \(t, f) -> do
                (alone0, _) <- checkFiles (with [(t, f)]) [site t]
                let alone = notAlready alone0
                pure (t, f, if M.null alone then Nothing else Just (T.intercalate "; " (concat (M.elems alone))))
          let good = [(t, f) | (t, f, Nothing) <- verdicts]
              compileFailed = [(t, f, why) | (t, f, Just why) <- verdicts]
          -- Each good alone may still clash together: then none of them stands.
          (together, _) <- if M.null broken || null good then pure (M.empty, Nothing) else (\(b, n) -> (notAlready b, n)) <$> checkFiles (with good) [site t | (t, _) <- good]
          let clash = if M.null together then [] else [(t, f, "this fill and another do not compile together: " <> T.intercalate "; " (concat (M.elems together))) | (t, f) <- good]
              good' = if null clash then good else []
              accepted' = foldr (\(t, f) -> M.insert (name t) (site t, f, tryAttempts t)) accepted good'
              failed = noFill ++ [(t {tryPrevious = Just (f, why)}, why) | (t, f, why) <- badParse ++ compileFailed ++ clash]
          forM_ good' (\(t, _) -> toolProgress tools (Filled (name t) (tryAttempts t)))
          next accepted' (maybe notes (: notes) note) failed

    finish mf accepted notes failed =
      let status h = case (M.lookup (holeName (phSite h)) accepted, [why | (t, why) <- failed, name t == holeName (phSite h)]) of
            (Just (_, f, n), _) -> h {phStatus = HoleFilled (mfName mf) n, phFill = Just f}
            (_, why : _) -> h {phStatus = HoleUnfilled why}
            _ -> h
          warnings = ["holes were filled but not compile-checked: " <> n | n <- nub notes, not (M.null accepted)]
       in (applyFills base [(s', f') | (s', f', _) <- M.elems accepted], map status holes, warnings)

    name = holeName . site
    site = phSite . tryHole

    fillHole t =
      let s = site t
       in FillHole (holeName s) (holeKind s) (holeInstruction s) (holeFile s) (context s) (tryPrevious t)

    -- The file around the hole (100 lines each side), the hole marked.
    context s =
      let ls = T.splitOn "\n" (M.findWithDefault "" (holeFile s) base)
          i = holeLine s - 1
          marked = [if j == i then T.replace (holePlaceholder s) ("\10214" <> holeName s <> "\10215") l else l | (j, l) <- zip [0 ..] ls] -- placeholders are unique (they name their hole)
       in T.intercalate "\n" (take 201 (drop (max 0 (i - 100)) marked))

    parses s f = case (languageFor (toolLanguages tools) (holeFile s), lookup (holeKind s) . langHoles =<< languageFor (toolLanguages tools) (holeFile s)) of
      (Just lang, Just k) ->
        querySyntax (toolSyntax tools) lang (object ["kind" .= ("ERROR" :: Text)]) (T.replace "{= fill =}" f (T.replace "{= fill|indent =}" (indented f) (kindWrap k))) >>= \case
          Right [] -> pure True
          Right _ -> pure False
          Left _ -> pure True -- no parser: the check decides
      _ -> pure True

    indented f = T.intercalate "\n" [if T.null (T.strip l) then l else "    " <> l | l <- T.splitOn "\n" f]

    -- Broken files (with messages) among those holding these holes, by each
    -- language's check over the project as this plan has it.
    -- Also why a check could not run (it then validates nothing: review I6).
    checkFiles files sites = do
      let names = nub [langName l | s <- sites, Just l <- [languageFor (toolLanguages tools) (holeFile s)], isJust (langCheck l)]
          langs = [l | l <- toolLanguages tools, langName l `elem` names]
      foldM (\(acc, note) lang -> (\(b, n) -> (M.unionWith (++) acc b, maybe note Just n)) <$> checkLang files lang) (M.empty, Nothing) langs
    checkLang files lang = do
      paths <- listFiles ws "."
      let globs = langFiles lang ++ maybe [] checkContext (langCheck lang)
          wanted p = not (any (`elem` pruned) (splitDirectories p)) && any (`matchesGlob` p) globs
      disk <- forM (filter wanted paths) (\p -> (p,) <$> readText ws p)
      let all' = M.union (M.filterWithKey (\p _ -> wanted p) files) (M.fromList [(p, t) | (p, Just t) <- disk])
      toolProgress tools (Checking (langName lang))
      runCheck (toolChecker tools) lang all' >>= \case
        Left f -> pure (M.empty, Just (describeCheckFailure f)) -- parse validation stands; the plan says so
        Right report -> pure (M.fromListWith (flip (++)) [(p, [T.pack p <> ": " <> why]) | (p, why) <- reportBroken report], Nothing)

-- | Every fill in place of its placeholder. Placeholders name their hole
-- (adapters must use @{= hole =}@), so each is found by its text wherever
-- earlier fills moved it; continuation lines take that line's indentation.
applyFills :: Map FilePath Text -> [(HoleSite, Text)] -> Map FilePath Text
applyFills = foldl (\files (s, f) -> M.adjust (fillIn s f) (holeFile s) files)
  where
    fillIn s f t = case T.breakOn (holePlaceholder s) t of
      (_, "") -> t
      (before, after) ->
        let lineStart = snd (T.breakOnEnd "\n" before)
            indent = T.takeWhile (`elem` (" \t" :: String)) lineStart
            body = T.intercalate ("\n" <> indent) (T.splitOn "\n" (T.strip f))
         in before <> body <> T.drop (T.length (holePlaceholder s)) after

-- | Why a fill cannot stand in its hole before any parse: it unbalances
-- brackets (it would close the code around it), or it is a placeholder.
escapes :: Text -> Maybe Text
escapes f
  | "olifant:" `T.isInfixOf` f = Just "the fill is the placeholder, not code"
  | not (balanced (0 :: Int) (T.unpack f)) = Just "the fill reaches outside its hole (unbalanced brackets)"
  | otherwise = Nothing
  where
    balanced d = \case
      [] -> d == 0
      '"' : rest -> balanced d (skipString rest)
      c : rest
        | c `elem` ("([{" :: String) -> balanced (d + 1) rest
        | c `elem` (")]}" :: String) -> d > 0 && balanced (d - 1) rest
        | otherwise -> balanced d rest
    skipString = \case
      [] -> []
      '\\' : _ : rest -> skipString rest
      '"' : rest -> rest
      _ : rest -> skipString rest
