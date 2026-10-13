-- | Internal to Olifant.Server: the moves on a judged block's survivors —
-- kill one (a test block in the test file, begun on a stub the editor
-- expands) or waive one (a comment on its line) — and the editor's edits
-- of a block's file, told apart: a draft it applied, the person's edit
-- inside the span, or an edit elsewhere (nothing to do).
module Olifant.Server.BlockMoves (blockKill, blockWaive, blockChanged) where

import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.IORef (atomicModifyIORef', modifyIORef', readIORef)
import Data.List (find, nub)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.App (Project (..))
import Olifant.Block (Block (..), Draft (..), Kind (..), Move (..), begin, hud, killStub, survivors)
import Olifant.Catalog (catalogLanguages)
import Olifant.Language (Failure (..), languageFor)
import Olifant.Mutate (Mutant (..), waiverLine)
import Olifant.Server.Block (appendStep, claim, fileOf, settle, testNow, tshow, withBlock)
import Olifant.Server.Edits (FileEdit (..), smallestChange)
import Olifant.Server.State (ServerState (..), projectFor)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (makeRelative, (</>))

-- | Kill the survivor on a line (1-based): the code block stays as it is;
-- a test block opens in its test file on the stub the editor expands — the
-- test file, the line to put it at, and the stub (tab stops for the test's
-- name and assertion, a hole for its arrangement).
blockKill :: FilePath -> Int -> ServerState -> IO (ServerState, Either Text (Block, FilePath, Int, Text))
blockKill path line = withBlock path $ \s root project lang b killed -> runExceptT $ do
  when (blockKind b /= Code) (throwE "kill a survivor from its code block, in its module")
  m <- maybe (throwE "no survivor on that line to kill") pure (find ((== line) . muLine) (survivors b))
  let testRel = T.unpack (fileOf (blockTestId b))
      testFile = normalisePath (root </> testRel)
  busy <- lift (M.member testFile <$> readIORef (ssBlocks s))
  when busy (throwE pendingTest)
  testText <- lift (readText (prWorkspace project) testRel) >>= maybe (throwE ("cannot read " <> T.pack testRel)) pure
  let (at, body) = killStub lang testText m
  b' <- ExceptT (settle s root killed "kill" (muOperator m <> " on line " <> tshow line) (Kill line) b)
  n <- lift (atomicModifyIORef' (ssNextBlock s) (\i -> (i + 1, i)))
  -- the stub's two blank lines, then its def, arrangement and assertion;
  -- its test is the test file's (its name is the person's to type)
  let tb = begin n testFile (TestFor (Just m)) testText (at + 2, at + 4) (T.pack testRel) (Failure Nothing "" "")
  claimed <- lift (claim s [] tb)
  unless claimed (throwE pendingTest)
  lift $ do
    modifyIORef' (ssBlockSeen s) (M.insert testFile testText)
    appendStep (ssPorts s) root tb "begin" (hud tb)
  pure (b', testFile, at, body)
  where
    pendingTest = "a test block is pending in the test file: accept or reject it first"

-- | Waive the unwaived survivors on a line, for a reason: one comment per
-- operator, written on its line (one edit), and each survivor waived.
blockWaive :: FilePath -> Int -> Text -> ServerState -> IO (ServerState, Either Text (Block, [FileEdit]))
blockWaive path line reason0 = withBlock path $ \s root project lang b killed -> runExceptT $ do
  let reason = T.unwords (T.words reason0)
      rel = makeRelative root (blockFile b)
  when (T.null reason) (throwE "a waiver needs a reason")
  let ops = nub [muOperator m | m <- survivors b, muLine m == line]
  when (null ops) (throwE "no unwaived survivor on that line")
  now <- lift (readText (prWorkspace project) rel) >>= maybe (throwE ("cannot read " <> T.pack rel)) pure
  let waived = foldl (\acc op -> waiverLine lang acc line op reason) now ops
      (range, text) = smallestChange now waived
      version = fst <$> M.lookup (blockFile b) (ssDocs s)
      waiveAll b0 = foldl (\acc op -> acc >>= \bk -> ExceptT (settle s root killed "waive" (op <> ": " <> reason) (Waive line op reason) bk)) (pure b0) ops
  b' <- waiveAll b
  -- the editor applying the comments is not an edit of the block's
  lift (modifyIORef' (ssBlockSeen s) (M.insert (blockFile b) waived))
  pure (b', [FileEdit (blockFile b) version False range text | now /= waived])

-- | The block's file changed (the caller debounces): a draft the editor
-- applied is tested (and judged); the person's edit inside the span is
-- tested as theirs; an edit elsewhere, or of a file with no block of its
-- own, does nothing (Nothing).
blockChanged :: FilePath -> ServerState -> IO (ServerState, Maybe Block)
blockChanged path s0 = do
  let file = normalisePath path
  own <- M.lookup file <$> readIORef (ssBlocks s0)
  case own of
    Nothing -> pure (s0, Nothing)
    Just (b, _) ->
      projectFor file s0 >>= \case
        (s, Nothing) -> pure (s, Nothing)
        (s, Just (root, project)) -> do
          now <- fromMaybe "" <$> readText (prWorkspace project) (makeRelative root file)
          seen <- fromMaybe (blockBase b) . M.lookup file <$> readIORef (ssBlockSeen s)
          let spanText = T.intercalate "\n" (take (snd (blockSpan b) - fst (blockSpan b) + 1) (drop (fst (blockSpan b) - 1) (T.splitOn "\n" now)))
              applied = case listToMaybe (reverse (blockDrafts b)) of
                Just d -> draftBy d /= "you" && draftText d == spanText
                Nothing -> False
          if now == seen || not (touches seen now (blockSpan b))
            then pure (s, Nothing)
            else case languageFor (catalogLanguages (prCatalog project)) (makeRelative root file) of
              Nothing -> pure (s, Nothing)
              Just lang -> (s,) . either (const Nothing) Just <$> testNow s root project lang b (maybe 0 snd own) applied

-- | Whether the change from OLD to NEW touches the lines of a span
-- (1-based, inclusive), as the lines of either text count: lines changed
-- or deleted within it, or lines inserted inside it or right after it.
touches :: Text -> Text -> (Int, Int) -> Bool
touches old new (s1, s2) = hit (p + 1, length os - q) || hit (p + 1, length ns - q)
  where
    (os, ns) = (T.splitOn "\n" old, T.splitOn "\n" new)
    p = length (takeWhile id (zipWith (==) os ns))
    q = length (takeWhile id (take (min (length os) (length ns) - p) (zipWith (==) (reverse os) (reverse ns))))
    hit (a, z)
      | z < a = a >= s1 && a <= s2 + 1
      | otherwise = a <= s2 && z >= s1
