-- | Internal to Olifant.Server: the loop's reconciliation of a plan with
-- the tree. Part 1, the work set: what changed since a baseline commit,
-- which of it the plans already account for, and which steps it touches.
-- The facts and Jev's questions over it come in later parts.
module Olifant.Server.Loop
  ( Change (..)
  , ChangedUnit (..)
  , WorkSet (..)
  , baseline
  , changedUnits
  , accounted
  , workSet
  , unaccountedCap
  ) where

import Control.Monad (forM)
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.Functor ((<&>))
import Data.List (delete, isPrefixOf, mapAccumL, stripPrefix)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..), PlansConfig (..))
import Olifant.Language.Adapter (languageFor)
import Olifant.Plans (Anchor (..), Kind (..), Line (..), Verdict (..), parsePlan)
import Olifant.Process (runBytes)
import Olifant.Server.Plans (Resolved (..), confined, currentBranch, gitOut, resolveAnchor, safely)
import Olifant.Server.State (ServerState (..))
import Olifant.Server.Unit (Named (..), fingerprint, unitsOf)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension, (</>))

-- | How a unit differs from the baseline: `Changed` carries the base
-- fingerprint, `Moved` the old (path, name).
data Change = Added | Changed Text | Removed | Moved FilePath Text
  deriving stock (Eq, Show)

-- | A unit that differs from the baseline: its relative path and name, its
-- current fingerprint (none when removed), its first line, and its text now
-- and at the base, each cut to 60 lines.
data ChangedUnit = ChangedUnit
  { cuPath :: FilePath
  , cuName :: Text
  , cuFingerprint :: Maybe Text
  , cuChange :: Change
  , cuHeader :: Text
  , cuNow :: Maybe Text
  , cuBase :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | What the loop works over: the baseline commit (none without git), every
-- changed unit (moves folded), those no plan accounts for (capped, the rest
-- counted), and the steps whose own anchor changed.
data WorkSet = WorkSet
  { wsBaseline :: Maybe Text
  , wsChanged :: [ChangedUnit]
  , wsUnaccounted :: [ChangedUnit]
  , wsMore :: Int
  , wsTouched :: [(Line, ChangedUnit)]
  }
  deriving stock (Eq, Show)

-- | The most unaccounted units a work set lists.
unaccountedCap :: Int
unaccountedCap = 40

-- | The commit a plan is reconciled against, at ROOT for the plan at PLAN
-- (relative): on the default branch (or detached, or with none) the commit
-- that added the plan, HEAD while it is uncommitted; on another branch its
-- merge-base with the default. Nothing without git or a commit.
baseline :: FilePath -> FilePath -> IO (Maybe Text)
baseline root plan =
  gitOut root ["rev-parse", "HEAD"] >>= \case
    Just [h] -> Just . T.pack <$> from h
    _ -> pure Nothing
  where
    from h = do
      def <- defaultBranch root
      cur <- currentBranch root
      case (def, cur) of
        (Just d, Just b) | b /= d -> gitOut root ["merge-base", T.unpack d, "HEAD"] <&> fromMaybe h . (>>= listToMaybe)
        _ -> gitOut root ["log", "--diff-filter=A", "--follow", "--format=%H", "--", plan] <&> \case
          Just ls@(_ : _) -> last ls
          _ -> h

-- | The default branch: origin's HEAD, else `main`, else `master`, if any.
defaultBranch :: FilePath -> IO (Maybe Text)
defaultBranch root =
  gitOut root ["symbolic-ref", "-q", "--short", "refs/remotes/origin/HEAD"] >>= \case
    Just [r] -> pure (Just (T.pack (fromMaybe r (stripPrefix "origin/" r))))
    _ -> firstOf ["main", "master"]
  where
    firstOf = \case
      [] -> pure Nothing
      b : bs -> gitOut root ["rev-parse", "-q", "--verify", b] >>= maybe (firstOf bs) (const (pure (Just (T.pack b))))

-- | Every unit that differs from BASE, over the files git reports changed or
-- untracked (not the plans nor Olifant's own, only those of a known
-- language): now through the workspace (open buffers win), then through
-- `git show`. A removed unit whose base fingerprint an added one has now is
-- one `Moved`, on the new path and name.
changedUnits :: ServerState -> Project -> Text -> IO [ChangedUnit]
changedUnits s p base = do
  diffed <- gitOut root ["diff", "--name-only", "--relative", b]
  untracked <- gitOut root ["ls-files", "--others", "--exclude-standard"]
  let files = filter kept (nubOrd (map normalisePath (concat (catMaybes [diffed, untracked]))))
  folded . concat <$> mapM fileUnits files
  where
    root = prRoot p
    b = T.unpack base
    plansDir = normalisePath (pcDir (catalogPlans (prCatalog p)))
    kept f = not (any (\d -> (d <> "/") `isPrefixOf` f) [plansDir, ".olifant"])
    units lang t
      | T.null t = pure (Right [])
      | otherwise = safely (Left "") (unitsOf (pSyntax (ssPorts s)) lang t)
    fileUnits f = case languageFor (catalogLanguages (prCatalog p)) f of
      Nothing -> pure []
      Just lang -> do
        now <- fromMaybe "" <$> safely Nothing (readText (prWorkspace p) f)
        old <- fromMaybe "" <$> gitText root ["show", b <> ":./" <> f]
        (,) <$> units lang now <*> units lang old <&> \case
          (Right ns, Right os) -> compareUnits f now old ns os
          _ -> []

-- | One file's units now and at the base, compared by name: each with its
-- base fingerprint when removed (for pairing moves).
compareUnits :: FilePath -> Text -> Text -> [Named] -> [Named] -> [(ChangedUnit, Maybe Text)]
compareUnits f now old ns os =
  [ (unit n (Just fpN) change (Just (cut now n)) (cut old <$> o), Nothing)
  | n <- nubOrdOn namedName ns
  , let fpN = fingerprint now (namedLines n)
        o = M.lookup (namedName n) olds
        fpO = fingerprint old . namedLines <$> o
  , change <- case fpO of
      Nothing -> [Added]
      Just fp | fp /= fpN -> [Changed fp]
      _ -> []
  ]
    ++ [ (unit o Nothing Removed Nothing (Just (cut old o)), Just (fingerprint old (namedLines o)))
       | o <- nubOrdOn namedName os
       , not (M.member (namedName o) nows)
       ]
  where
    olds = byName os
    nows = byName ns
    byName xs = M.fromListWith (\_ first -> first) [(namedName x, x) | x <- xs]
    unit n fp change nowT baseT = ChangedUnit f (namedName n) fp change (header n) nowT baseT
    header n = T.strip (fromMaybe "" (listToMaybe (drop (namedDef n - 1) (T.splitOn "\n" (if M.member (namedName n) nows then now else old)))))
    cut t n = let (a, z) = namedLines n in T.intercalate "\n" (take (min 60 (z - a + 1)) (drop (a - 1) (T.splitOn "\n" t)))

-- | Each added unit that has a removed one's base fingerprint becomes a
-- `Moved` from it; the removed one goes.
folded :: [(ChangedUnit, Maybe Text)] -> [ChangedUnit]
folded xs = [cu | (i, cu) <- zip [0 :: Int ..] out, i `notElem` used]
  where
    removed = [(i, fp, cu) | (i, (cu, Just fp)) <- zip [0 ..] xs]
    (left, out) = mapAccumL pair removed (map fst xs)
    used = [i | (i, _, _) <- removed, i `notElem` [j | (j, _, _) <- left]]
    pair avail cu = case (cuChange cu, cuFingerprint cu) of
      (Added, Just fp) | r@(_, _, from) : _ <- [r | r@(_, rfp, _) <- avail, rfp == fp] ->
        (delete r avail, cu {cuChange = Moved (cuPath from) (cuName from), cuBase = cuBase from})
      _ -> (avail, cu)

-- | The (path, unit, fingerprint) every plan under the plans folder accounts
-- for: each anchor verified at a fingerprint, each test child's unit as it
-- is now; and the readings rejected.
accounted :: ServerState -> Project -> IO (S.Set (FilePath, Text, Text))
accounted s p = do
  files <- safely [] (listFiles ws dir)
  entries <- forM [f | f <- files, takeExtension f == ".md"] $ \f -> do
    ls <- parsePlan . fromMaybe "" <$> safely Nothing (readText ws (dir </> f))
    tests <- forM [a | l <- ls, lnKind l == TestChild, Just a <- [lnAnchor l]] $ \a ->
      resolveAnchor s p a <&> \r -> (rsRel r,,) <$> fmap namedName (rsUnit r) <*> rsFingerprint r
    pure ([(rel, anUnit a, fp) | l <- ls, Just a <- [lnAnchor l], Verified fp <- [lnVerdict l], Just rel <- [confined (anPath a)]] ++ catMaybes tests)
  pure (S.fromList (concat entries) <> rejected)
  where
    ws = prWorkspace p
    dir = pcDir (catalogPlans (prCatalog p))
    -- Task 4: rejected readings
    rejected = S.empty

-- | The work set of the plan at PLAN (relative) with lines LS. Unaccounted
-- keeps git's order (changed files, then untracked): the workspace has no
-- cheap modification stamp.
workSet :: ServerState -> Project -> FilePath -> [Line] -> IO WorkSet
workSet s p plan ls =
  baseline (prRoot p) plan >>= \case
    Nothing -> pure (WorkSet Nothing [] [] 0 [])
    Just b -> do
      changed <- changedUnits s p b
      acc <- accounted s p
      let isAccounted cu = maybe False (\fp -> (cuPath cu, cuName cu, fp) `S.member` acc) (cuFingerprint cu)
          unaccounted = [cu | cu <- changed, not (isAccounted cu), not (any (`S.member` own) (keys cu))]
          (shown, rest) = splitAt unaccountedCap unaccounted
      pure (WorkSet (Just b) changed shown (length rest) [(l, cu) | (l, k) <- steps, cu <- changed, k `elem` keys cu])
  where
    steps = [(l, (rel, anUnit a)) | l <- ls, Step _ <- [lnKind l], Just a <- [lnAnchor l], Just rel <- [confined (anPath a)]]
    own = S.fromList (map snd steps)
    -- a unit's (path, name), and where it was before a move
    keys cu = (cuPath cu, cuName cu) : [(o, n) | Moved o n <- [cuChange cu]]

-- | A read-only git command's whole output; Nothing when it fails.
gitText :: FilePath -> [String] -> IO (Maybe Text)
gitText root args =
  runBytes "git" (["-C", root] ++ args) (Just root) "" <&> \case
    Right (ExitSuccess, out, _) -> Just (TE.decodeUtf8Lenient out)
    _ -> Nothing
