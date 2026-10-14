-- | Internal to Olifant.Server: the moves on a judged block's survivors —
-- kill one (a test block in the test file, begun on a stub the editor
-- expands) or waive one (a comment on its line) — and the editor's edits
-- of a block's file, told apart: a draft it applied, the person's edit
-- inside the span, or an edit elsewhere (nothing to do).
module Olifant.Server.BlockMoves (blockKill, blockWaive, blockChanged, blockEdited) where

import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.IORef (atomicModifyIORef', modifyIORef', readIORef)
import Data.List (nub)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.App (Project (..))
import Olifant.Block (Block (..), Draft (..), Kind (..), Move (..), begin, hud, killStub, killTarget, stubNameFor, survivors)
import Olifant.Catalog (catalogLanguages)
import Olifant.Language (Failure (..), Language (..), languageFor)
import Olifant.Mutate (Mutant (..), waiverLine)
import Olifant.Server.Block (appendStep, awaitingVerdict, claim, fileOf, settle, settleOn, spanEdited, testNow, tshow, withBlock)
import Olifant.Server.Edits (FileEdit (..), smallestChange)
import Olifant.Server.State (ServerState (..), projectFor)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (makeRelative, (</>))

-- | Kill the survivor on a line (1-based), or the top-ranked one when the
-- line has none ('killTarget'): the code block stays as it is;
-- a test block opens in its test file on the stub the editor expands — the
-- test file, the line to put it at, and the stub (tab stops for the test's
-- name and assertion, a hole for its arrangement).
blockKill :: FilePath -> Int -> ServerState -> IO (ServerState, Either Text (Block, FilePath, Int, Text))
blockKill path line = withBlock path $ \s root project lang b killed -> runExceptT $ do
  when (blockKind b /= Code) (throwE "stub a test from the code's magic circle, in its module")
  m <- either throwE pure (killTarget line b)
  let testRel = T.unpack (fileOf (blockTestId b))
      testFile = normalisePath (root </> testRel)
  busy <- lift (M.member testFile <$> readIORef (ssBlocks s))
  when busy (throwE (pendingTest testRel))
  testText <- lift (readText (prWorkspace project) testRel) >>= maybe (throwE ("cannot read " <> T.pack testRel)) pure
  let (at, body) = killStub lang testText m
      named = stubNameFor m testText (langKeywords lang)
  b' <- ExceptT (settle s root killed "kill" (muOperator m <> " on line " <> tshow (muLine m)) (Kill (muLine m)) b)
  n <- lift (atomicModifyIORef' (ssNextBlock s) (\i -> (i + 1, i)))
  -- the stub's two blank lines, then its def, arrangement and assertion;
  -- its test is the stub's, named from the mutant (the HUD shows it; the
  -- person may type over it: the block's tests are the file's new ones)
  let tb = begin n testFile (TestFor (Just m)) testText (at + 2, at + 4) (T.pack testRel <> "::" <> named) (Failure Nothing "" "")
  claimed <- lift (claim s [] tb)
  unless claimed (throwE (pendingTest testRel))
  lift $ do
    modifyIORef' (ssBlockSeen s) (M.insert testFile testText)
    appendStep (ssPorts s) root tb "begin" (hud tb)
  pure (b', testFile, at, body)
  where
    pendingTest rel = "a magic circle is open in " <> T.pack rel <> ": accept or reject it there first"

-- | Waive the unwaived survivors on a line, for a reason: one comment per
-- operator, written on its line (one edit), and each survivor waived.
blockWaive :: FilePath -> Int -> Text -> ServerState -> IO (ServerState, Either Text (Block, [FileEdit]))
blockWaive path line reason0 = withBlock path $ \s root project lang b killed -> runExceptT $ do
  let reason = T.unwords (T.words reason0)
      rel = makeRelative root (blockFile b)
  when (T.null reason) (throwE "a waiver needs a reason")
  -- its line is the verdict's: an edit still being tested moves it
  lift (awaitingVerdict s root project b "waive") >>= maybe (pure ()) throwE
  let ops = nub [muOperator m | m <- survivors b, muLine m == line]
  when (null ops) (throwE "no unwaived survivor on that line")
  now <- lift (readText (prWorkspace project) rel) >>= maybe (throwE ("cannot read " <> T.pack rel)) pure
  let waived = foldl (\acc op -> waiverLine lang acc line op reason) now ops
      (range, text) = smallestChange now waived
      version = fst <$> M.lookup (blockFile b) (ssDocs s)
      waiveAll b0 = foldl (\acc op -> acc >>= \bk -> ExceptT (settleOn s root killed "waive" (op <> ": " <> reason) (Just waived) (Waive line op reason) bk)) (pure b0) ops
  b' <- waiveAll b
  -- the editor applying the comments is not an edit of the block's
  lift (modifyIORef' (ssBlockSeen s) (M.insert (blockFile b) waived))
  pure (b', [FileEdit (blockFile b) version False range text | now /= waived])

-- | The block's file changed (the caller debounces): a draft the editor
-- applied is tested (and judged); the person's edit inside the span is
-- tested as theirs; an edit elsewhere, or of a file with no block of its
-- own, does nothing (Nothing). A test block whose stub the person deleted
-- whole (no def left in its span) is rejected as it stands: the file is
-- not put back, so the deletion and whatever else they typed stay.
blockChanged :: FilePath -> ServerState -> IO (ServerState, Maybe Block)
blockChanged path s0 =
  spanEdit path s0 >>= \case
    (s, Nothing) -> pure (s, Nothing)
    (s, Just (b, killed, root, _, now))
      | stubGone b now -> (s,) . either (const Nothing) Just <$> settle s root killed "reject" "the stub was deleted" Reject b
    (s, Just (b, killed, root, project, now)) -> do
      let spanText = T.intercalate "\n" (take (snd (blockSpan b) - fst (blockSpan b) + 1) (drop (fst (blockSpan b) - 1) (T.splitOn "\n" now)))
          applied = case listToMaybe (reverse (blockDrafts b)) of
            Just d -> draftBy d /= "you" && draftText d == spanText
            Nothing -> False
      case languageFor (catalogLanguages (prCatalog project)) (makeRelative root (blockFile b)) of
        Nothing -> pure (s, Nothing)
        Just lang -> (s,) . either (const Nothing) Just <$> testNow s root project lang b killed applied

-- | A test block's span with no def in it now (`def `, after the
-- indentation): its stub is gone. A test renamed as it is typed keeps
-- its def, so it stays.
stubGone :: Block -> Text -> Bool
stubGone b now = case blockKind b of
  Code -> False
  TestFor _ -> not (any isDef spanLines)
  where
    (l1, l2) = blockSpan b
    spanLines = take (l2 - l1 + 1) (drop (l1 - 1) (T.splitOn "\n" now))
    isDef l = any (`T.isPrefixOf` T.stripStart l) ["def ", "async def "]

-- | Whether the file's text now is an edit of its block's span (since the
-- text last tested): the adapter tests it once typing settles, and only
-- such an edit supersedes the test or judge running for the block (an
-- edit elsewhere leaves it to land).
blockEdited :: FilePath -> ServerState -> IO (ServerState, Bool)
blockEdited path s0 = fmap isJust <$> spanEdit path s0

-- | The file's block (its killed count, project and root) and the file
-- now, when the change since the text last tested touches the block's span.
spanEdit :: FilePath -> ServerState -> IO (ServerState, Maybe (Block, Int, FilePath, Project, Text))
spanEdit path s0 = do
  let file = normalisePath path
  own <- M.lookup file <$> readIORef (ssBlocks s0)
  case own of
    Nothing -> pure (s0, Nothing)
    Just (b, killed) ->
      projectFor file s0 >>= \case
        (s, Nothing) -> pure (s, Nothing)
        (s, Just (root, project)) -> (s,) . fmap (b,killed,root,project,) <$> spanEdited s root project b
