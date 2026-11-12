-- | Internal to Olifant.Server: which verdicts went stale. A check keeps a
-- print of every citation it rests on ('Olifant.Server.Checker.Print'); a
-- verdict is stale when any of them differs from the tree now, or its unit
-- or file is gone. The tree now is read through the server's cache of
-- cited files ('ssCitePrints'), so a diagnostics pull after an edit of the
-- plan alone reads no cited file again.
module Olifant.Server.Stale
  ( CitedFile (..)
  , printsNow
  , isStale
  , staleKeys
  , citedPrints
  , staleIn
  ) where

import Data.IORef (atomicModifyIORef', readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..))
import Olifant.Language.Adapter (languageFor)
import Olifant.Server.Checker (Confirmed (..), Judgement (..), Print (..), fileHash)
import Olifant.Server.Claims (CheckRecord (..), ClaimStore, SentenceEntry (..))
import Olifant.Server.Confine (safely)
import Olifant.Server.State (CitedFile (..), ServerState (..))
import Olifant.Server.Unit (Named (..), fingerprint, unitsWithKinds)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath ((</>))

-- | The cited files (paths relative to the project root) as they are now:
-- from the cache while the file has not changed (on disk, or its version
-- in the editor), else read and parsed once and kept.
printsNow :: ServerState -> Project -> [FilePath] -> IO (Map FilePath CitedFile)
printsNow s p rels = M.fromList <$> mapM one (S.toList (S.fromList (map normalisePath rels)))
  where
    one rel = do
      let abs' = normalisePath (prRoot p </> rel)
          version = fst <$> M.lookup abs' (ssDocs s)
      known <- M.lookup abs' <$> readIORef (ssCitePrints s)
      case known of
        Just (v, c) | v == version -> pure (rel, c)
        _ -> do
          c <- readCited rel
          atomicModifyIORef' (ssCitePrints s) (\m -> (M.insert abs' (version, c) m, ()))
          pure (rel, c)
    readCited rel =
      safely Nothing (readText (prWorkspace p) rel) >>= \case
        Nothing -> pure (CitedFile Nothing M.empty)
        Just t -> do
          units <- case languageFor (catalogLanguages (prCatalog p)) rel of
            Just lang -> safely [] (either (const []) id <$> unitsWithKinds (pSyntax (ssPorts s)) lang (Just rel) t)
            Nothing -> pure []
          pure (CitedFile (Just (fileHash t)) (M.fromList [(namedName n, fingerprint t (namedLines n)) | (n, _) <- units]))

-- | A check whose cited code changed since: a print whose file or unit is
-- gone, or whose hash differs. A check with no prints, or one that could
-- not tell, is never stale.
isStale :: Map FilePath CitedFile -> CheckRecord -> Bool
isStale now c = cfJudgement cf /= JCannotTell && any moved (cfPrints cf)
  where
    cf = cjConfirmed c
    moved pr = case M.lookup (ptPath pr) now of
      Nothing -> False
      Just f -> case (cdHash f, ptUnit pr) of
        (Nothing, _) -> True
        (Just h, Nothing) -> h /= ptHash pr
        (Just _, Just u) -> M.lookup u (cdUnits f) /= Just (ptHash pr)

-- | The keys of a store whose check is stale.
staleKeys :: Map FilePath CitedFile -> ClaimStore -> Set Text
staleKeys now = M.keysSet . M.filter (maybe False (isStale now) . seCheck)

-- | Every print of a store's checks: the files to read.
citedPrints :: ClaimStore -> [Print]
citedPrints = concat . mapMaybe (fmap (cfPrints . cjConfirmed) . seCheck) . M.elems

-- | The stale keys of a plan's store, the cited files read through the cache.
staleIn :: ServerState -> Project -> ClaimStore -> IO (Set Text)
staleIn s p store = (`staleKeys` store) <$> printsNow s p (map ptPath (citedPrints store))
