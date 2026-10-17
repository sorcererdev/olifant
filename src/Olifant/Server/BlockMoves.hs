-- | Internal to Olifant.Server: the moves on a judged block's survivors —
-- kill one (a test block in the test file, begun on a stub the editor
-- expands) or waive one (a comment on its line) —, a stub for a function's
-- new test, and the editor's edits
-- of a block's file, told apart: a draft it applied, the person's edit
-- inside the span, or an edit elsewhere (nothing to do).
module Olifant.Server.BlockMoves (blockKill, Killed, blockStub, Stubbed (..), blockWaive, blockChanged, blockEdited, rejectDeleted) where

import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Functor ((<&>))
import Data.IORef (atomicModifyIORef', modifyIORef', readIORef)
import Data.List (nub)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.App (Project (..))
import Olifant.Block (Block (..), BlockState (..), Draft (..), Kind (..), Move (..), begin, hud, killStub, killTarget, pending, snippetText, survivors, uniqueTestName)
import Olifant.Catalog (catalogLanguages)
import Olifant.Language (Failure (..), languageFor)
import Olifant.Mutate (Mutant (..), waiverLine)
import Olifant.Newness (newLinesOf)
import Olifant.Server.Block (Site (..), StubDraft, appendStep, draftStub, awaitingVerdict, blockFor, claim, fileOf, functionAt, openTestStub, passingIn, settle, settleOn, siteAt, siteFile, siteTestFile, spanEdited, testNow, tshow, withBlock)
import Olifant.Server.Edits (FileEdit (..), smallestChange)
import Olifant.Server.State (ServerState (..), projectFor)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (makeRelative, (</>))

-- | Kill the survivor on a line (1-based), or the top-ranked one when the
-- line has none ('killTarget'): the code block stays as it is;
-- a test block opens in its test file on the stub the editor expands — the
-- test file, the line to put it at, and the stub (tab stops for the test's
-- name and assertion, a hole for its arrangement) — and, with a model,
-- is drafted at once (its test is named from the mutant): the draft to run
-- once the stub is answered ('draftStub').
blockKill :: FilePath -> Int -> ServerState -> IO (ServerState, Either Text Killed)
blockKill path line = withBlock path $ \s root project lang b killed -> runExceptT $ do
  case blockKind b of
    Code -> pure ()
    ExistingTest _ -> throwE stubFromTestCircle
    TestFor _ -> throwE stubFromModule
  m <- either throwE pure (killTarget line b)
  let testRel = T.unpack (fileOf (blockTestId b))
      testFile = normalisePath (root </> testRel)
  busy <- lift (M.member testFile <$> readIORef (ssBlocks s))
  when busy (throwE (pendingTest testRel))
  testText <- lift (readText (prWorkspace project) testRel) >>= maybe (throwE ("cannot read " <> T.pack testRel)) pure
  let (at, body, named) = killStub lang testText m
  b' <- ExceptT (settle s root killed "kill" (muOperator m <> " on line " <> tshow (muLine m)) (Kill (muLine m)) b)
  n <- lift (atomicModifyIORef' (ssNextBlock s) (\i -> (i + 1, i)))
  -- the stub's two blank lines, then its def, arrangement and assertion;
  -- its test is the stub's, named from the mutant (the HUD shows it; the
  -- person may type over it: the block's tests are the file's new ones)
  -- the test file's passing tests (before the stub is in it) are kept
  kept <- lift (passingIn s (prWorkspace project) lang testRel)
  let tb = (begin n testFile (TestFor (Just m)) testText (at + 2, at + 4) (T.pack testRel <> "::" <> named) (Failure Nothing "" "")) {blockKeep = kept}
  claimed <- lift (claim s [] tb)
  unless claimed (throwE (pendingTest testRel))
  lift $ do
    modifyIORef' (ssBlockSeen s) (M.insert testFile testText)
    appendStep (ssPorts s) root tb "begin" (hud tb)
  -- the test file as the editor leaves it once it expands the stub
  let ls = T.splitOn "\n" testText
      stubbed = T.intercalate "\n" (take (at - 1) ls ++ [snippetText body] ++ drop at ls)
  later <- lift (draftStub s root project lang tb stubbed)
  pure (b', testFile, at, body, later)

-- | A kill: the code block, the test file, the stub's line and text, and
-- the stub's draft, begun ('draftStub'), when a model drafts it.
type Killed = (Block, FilePath, Int, Text, Maybe StubDraft)

-- | A stub in a test's own circle: survivors are the code circle's.
stubFromModule :: Text
stubFromModule = "stub a test from the code's magic circle, in its module"

-- | A stub in an existing test's circle: it has no survivors, and no code circle.
stubFromTestCircle :: Text
stubFromTestCircle = "this circle is a test's; stub from the module's function"

-- | Why a stub waits: a circle is open in its test file.
pendingTest :: FilePath -> Text
pendingTest rel = "a magic circle is open in " <> T.pack rel <> ": accept or reject it there first"

-- | What a stub opened: a kill's ('blockKill': the code block, and its
-- test block's stub), or a new test's for a function (its test block, and
-- the stub with the edits the editor applies first).
data Stubbed = StubKilled Killed | StubOpened Block (FilePath, Int, Text, [FileEdit])

-- | Stub a test at a line (1-based): in a judged code circle with a
-- survivor left, the kill ('blockKill': the line's survivor, or the
-- top-ranked); else, with no circle in the module and the cursor in one of
-- its functions, a new test's stub for it at the end of its test file (as
-- a begin opens one for a function no test calls), its test named
-- `test_<function>`, or `_2`, `_3`, … when the file has that name already.
-- A circle open in the module with no survivor to take is finished first
-- (one circle before the next case begins), each state saying how; a test
-- file is not where a stub comes from.
blockStub :: FilePath -> Int -> ServerState -> IO (ServerState, Either Text Stubbed)
blockStub path line s0 =
  blockFor path s0 >>= \case
    Just (b, _) | blockKind b == Code -> case blockState b of
      Judged _ | not (null (survivors b)) -> fmap (fmap StubKilled) <$> blockKill path line s0
      Judged _ ->
        projectFor (blockFile b) s0 <&> \(s, found) ->
          (s, Left ("accept or reject the circle in " <> T.pack (maybe (blockFile b) (\(root, _) -> makeRelative root (blockFile b)) found) <> " first, then stub"))
      Red _ -> pure (s0, Left "the circle is red: draft it, or reject it, then stub")
      Broken _ -> pure (s0, Left "the circle cannot run: fix it, then test")
      _ -> pure (s0, Left "the circle is being tested; stub once its verdict lands")
    -- a test's own circle, in its test file: the kill's sentence
    Just (b, _) | ExistingTest _ <- blockKind b -> pure (s0, Left stubFromTestCircle)
    Just _ -> pure (s0, Left stubFromModule)
    Nothing ->
      siteAt path line s0 >>= \case
        (s, Left why) -> pure (s, Left why)
        (s, Right site) -> fmap (s,) . runExceptT $ do
          when (siteOnTest site) (throwE "stub a test from its module")
          (fn, _, _) <- lift (functionAt s site line) >>= maybe (throwE "put the cursor in a function to stub a test for it") pure
          -- refused before a block number is taken ('claim' refuses it too)
          busy <- lift (M.member (siteTestFile site) <$> readIORef (ssBlocks s))
          when busy (throwE (pendingTest (siteTestRel site)))
          let name = uniqueTestName ("test_" <> fn) (fromMaybe "" (siteTestText site))
          uncurry StubOpened <$> openTestStub s site fn (Just name) [siteFile site] (pendingTest (siteTestRel site))

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
      | stubGone b now -> (s,) <$> rejectDeleted s root killed now b
    (s, Just (b, killed, root, project, now)) -> do
      let spanText = T.intercalate "\n" (take (snd (blockSpan b) - fst (blockSpan b) + 1) (drop (fst (blockSpan b) - 1) (T.splitOn "\n" now)))
          applied = case listToMaybe (reverse (blockDrafts b)) of
            Just d -> draftBy d /= "you" && draftText d == spanText
            Nothing -> False
      case languageFor (catalogLanguages (prCatalog project)) (makeRelative root (blockFile b)) of
        Nothing -> pure (s, Nothing)
        Just lang -> (s,) . either (const Nothing) Just <$> testNow s root project lang b killed applied

-- | A test block whose stub is deleted whole in NOW: no line new since it
-- began is left, non-blank, at or after its span's first line. A test
-- renamed as it is typed, or its def line retyped, keeps lines of its own.
stubGone :: Block -> Text -> Bool
stubGone b now = case blockKind b of
  Code -> False
  -- an existing test's lines were there before its circle: none is new
  ExistingTest _ -> False
  TestFor _ -> not (any kept (S.toList (newLinesOf (blockBase b) now)))
  where
    ls = T.lines now
    kept l = l >= fst (blockSpan b) && maybe False (not . T.null . T.strip) (listToMaybe (drop (l - 1) ls))

-- | Reject a test block whose stub was deleted (the file as the person
-- left it: no edit). The block read may be stale (a test or draft landed
-- meanwhile): then the block as it stands, while it is the same circle
-- and its stub is still gone.
rejectDeleted :: ServerState -> FilePath -> Int -> Text -> Block -> IO (Maybe Block)
rejectDeleted s root killed now b =
  settle s root killed "reject" "the stub was deleted" Reject b >>= \case
    Right gone -> pure (Just gone)
    Left _ ->
      M.lookup (blockFile b) <$> readIORef (ssBlocks s) >>= \case
        Just (current, k) | blockId current == blockId b, blockVersion current /= blockVersion b, pending current, stubGone current now -> rejectDeleted s root k now current
        _ -> pure Nothing

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
    -- a stub being drafted: its expansion, or the person's typing, is the
    -- draft's to settle (it lands only on the stub as it planned)
    Just (b, _) | TestFor _ <- blockKind b, Drafting _ <- blockState b -> pure (s0, Nothing)
    Just (b, killed) ->
      projectFor file s0 >>= \case
        (s, Nothing) -> pure (s, Nothing)
        (s, Just (root, project)) -> (s,) . fmap (b,killed,root,project,) <$> spanEdited s root project b
