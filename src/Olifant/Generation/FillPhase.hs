-- | The fill phase of `gen` (spec §6.4): one request for every hole, each
-- fill parsed as its kind (the adapter's `wrap`) and compile-checked (its
-- `[check]`), failing holes retried with the reason, and any still failing
-- left as their placeholder. Holes never block a generation.
module Olifant.Generation.FillPhase (fillPhase) where

import Control.Monad (foldM, forM)
import Data.Aeson (object, (.=))
import Data.List (nub, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Fill
import Olifant.Generation.Plan (HoleStatus (..), PlannedHole (..))
import Olifant.Generation.Tools (Tools (..))
import Olifant.Holes (HoleSite (..))
import Olifant.Language
import Olifant.Workspace (Workspace (..), pruned)
import System.FilePath (splitDirectories)

-- | A hole being tried: attempts so far and the previous (fill, why).
data Try = Try {tryHole :: PlannedHole, tryAttempts :: Int, tryPrevious :: Maybe (Text, Text)}

fillPhase :: Monad m => Workspace m -> Tools m -> [(Text, Text)] -> Map FilePath Text -> [PlannedHole] -> m (Map FilePath Text, [PlannedHole])
fillPhase ws tools inputs base holes = case toolFiller tools of
  Nothing -> pure (base, holes)
  Just _ | null holes -> pure (base, holes)
  Just mf -> loop mf 1 M.empty [Try h 0 Nothing | h <- holes]
  where
    -- accepted: hole name -> (site, fill, attempts)
    loop mf r accepted pending = do
      reply <- runFill (mfFiller mf) (FillRequest [fillHole t | t <- pending] inputs)
      let tried = [t {tryAttempts = tryAttempts t + 1} | t <- pending]
      case reply of
        Left why -> pure (finish mf accepted [(t, why) | t <- tried])
        Right fills -> do
          let noFill = [(t, "the model returned no fill for `" <> name t <> "`") | t <- tried, M.notMember (name t) fills]
          parsed <- forM [(t, f) | t <- tried, Just f <- [M.lookup (name t) fills]] $ \(t, f) -> (t,f,) <$> parses (site t) f
          let badParse = [(t, f, "does not parse as " <> holeKind (site t)) | (t, f, False) <- parsed]
              candidates = [(t, f) | (t, f, True) <- parsed]
              files = applyFills base ([(s', f') | (s', f', _) <- M.elems accepted] ++ [(site t, f) | (t, f) <- candidates])
          broken <- checkFiles files [site t | (t, _) <- candidates]
          let compileFailed = [(t, f, T.intercalate "; " msgs) | (t, f) <- candidates, Just msgs <- [M.lookup (holeFile (site t)) broken]]
              good = [(t, f) | (t, f) <- candidates, M.notMember (holeFile (site t)) broken]
              accepted' = foldr (\(t, f) -> M.insert (name t) (site t, f, tryAttempts t)) accepted good
              failed = noFill ++ [(t {tryPrevious = Just (f, why)}, why) | (t, f, why) <- badParse ++ compileFailed]
          if null failed || r >= 1 + max 0 (mfRetries mf)
            then pure (finish mf accepted' failed)
            else loop mf (r + 1) accepted' (map fst failed)

    finish mf accepted failed =
      let status h = case (M.lookup (holeName (phSite h)) accepted, [why | (t, why) <- failed, name t == holeName (phSite h)]) of
            (Just (_, f, n), _) -> h {phStatus = HoleFilled (mfName mf) n, phFill = Just f}
            (_, why : _) -> h {phStatus = HoleUnfilled why}
            _ -> h
       in (applyFills base [(s', f') | (s', f', _) <- M.elems accepted], map status holes)

    name = holeName . site
    site = phSite . tryHole

    fillHole t =
      let s = site t
       in FillHole (holeName s) (holeKind s) (holeInstruction s) (holeFile s) (context s) (tryPrevious t)

    -- The file around the hole (100 lines each side), the hole marked.
    context s =
      let ls = T.splitOn "\n" (M.findWithDefault "" (holeFile s) base)
          i = holeLine s - 1
          marked = [if j == i then T.replace (holePlaceholder s) ("\10214" <> holeName s <> "\10215") l else l | (j, l) <- zip [0 ..] ls]
       in T.intercalate "\n" (take 201 (drop (max 0 (i - 100)) marked))

    parses s f = case (languageFor (toolLanguages tools) (holeFile s), lookup (holeKind s) . langHoles =<< languageFor (toolLanguages tools) (holeFile s)) of
      (Just lang, Just k) ->
        querySyntax (toolSyntax tools) lang (object ["kind" .= ("ERROR" :: Text)]) (T.replace "{= fill =}" f (kindWrap k)) >>= \case
          Right [] -> pure True
          Right _ -> pure False
          Left _ -> pure True -- no parser: the check decides
      _ -> pure True

    -- Broken files (with messages) among those holding these holes, by each
    -- language's check over the project as this plan has it.
    checkFiles files sites = do
      let names = nub [langName l | s <- sites, Just l <- [languageFor (toolLanguages tools) (holeFile s)], isJust (langCheck l)]
          langs = [l | l <- toolLanguages tools, langName l `elem` names]
      foldM (\acc lang -> M.unionWith (++) acc <$> checkLang files lang) M.empty langs
    checkLang files lang = do
      paths <- listFiles ws "."
      let globs = langFiles lang ++ maybe [] checkContext (langCheck lang)
          wanted p = not (any (`elem` pruned) (splitDirectories p)) && any (`matchesGlob` p) globs
      disk <- forM (filter wanted paths) (\p -> (p,) <$> readText ws p)
      let all' = M.union (M.filterWithKey (\p _ -> wanted p) files) (M.fromList [(p, t) | (p, Just t) <- disk])
      runCheck (toolChecker tools) lang all' >>= \case
        Left _ -> pure M.empty -- the check cannot run: parse validation stands
        Right report -> pure (M.fromListWith (flip (++)) [(p, [T.pack p <> ": " <> why]) | (p, why) <- reportBroken report])

-- | Every fill in place of its placeholder, bottom-up per file so line
-- numbers stay valid; continuation lines take the hole's indentation.
applyFills :: Map FilePath Text -> [(HoleSite, Text)] -> Map FilePath Text
applyFills base fills = foldl apply base (sortOn (\(s, _) -> (holeFile s, Down (holeLine s))) fills)
  where
    apply files (s, f) = M.adjust (fillAt s f) (holeFile s) files
    fillAt s f t =
      let ls = T.splitOn "\n" t
          i = holeLine s - 1
          indent l = T.takeWhile (`elem` (" \t" :: String)) l
          put l =
            let (before, after) = T.breakOn (holePlaceholder s) l
                body = T.intercalate ("\n" <> indent l) (T.splitOn "\n" (T.strip f))
             in if T.null after then l else before <> body <> T.drop (T.length (holePlaceholder s)) after
       in if i < 0 || i >= length ls then t else T.intercalate "\n" (take i ls ++ [put (ls !! i)] ++ drop (i + 1) ls)
