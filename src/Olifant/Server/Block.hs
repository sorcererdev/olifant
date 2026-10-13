-- | Internal to Olifant.Server: the loop's blocks. One is begun on a red
-- test (or on a function whose test is green: judged at once), drafted as a
-- hole its one test verifies, tested again on the editor's text, judged by
-- mutation, then accepted or rejected; every move is a line of the step
-- log. The machine is "Olifant.Block"'s: this module tells it what happened.
module Olifant.Server.Block
  ( blockFor
  , blockBegin
  , blockDraft
  , blockTest
  , blockJudge
  , blockAccept
  , blockReject
  , blockJson
  , stateName
  , appendStep
  , underTest
    -- * for the other moves ("Olifant.Server.BlockMoves")
  , withBlock
  , settle
  , testNow
  , fileOf
  , tshow
  , claim
  , pushed
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad (unless, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE, withExceptT)
import Data.Aeson (Value, object, (.=))
import Data.Char (isAlphaNum)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (isSuffixOf, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime)
import GHC.Clock (getMonotonicTime)
import Olifant.Anchor (locateContainer)
import Olifant.App (PlanOptions (..), Ports (..), Project (..), planOptions, planWith)
import Olifant.Block
import Olifant.Decide (Decider)
import Olifant.Catalog (catalogLanguages)
import Olifant.Fill (Filler (..), ModelFiller (..), Stance (..))
import Olifant.Generation (HoleStatus (..), Plan (..), PlannedHole (..), ProgressEvent (..), Step (..), describeConflict, describeGenerationError)
import Olifant.Generation.FillPhase (applyFills, projectFiles)
import Olifant.Generator (GeneratorWith (..), Verify (..), replaceGenerator)
import Olifant.Holes (HoleSite (..))
import Olifant.Language (Failure (..), Language (..), Syntax, TestId, TestRun (..), describeSyntaxFailure, languageFor, runTestsIn, testsForFile)
import Olifant.Mutate (Mutant (..), Verdict (..), capMutants, judge, judgeTimeout, mutantsOf)
import Olifant.Mutate.Python (pythonOperators)
import Olifant.Newness (newLinesOf)
import Olifant.Server.Actions (fillerFor)
import Olifant.Server.Edits (FileEdit (..), editsOfPlan, smallestChange)
import Olifant.Server.BlockJev (orderSurvivors)
import Olifant.Server.State (ServerState (..), deciderFor, projectFor)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (makeRelative, (</>))

-- | The pending block of a file (its module or its test), with how many of
-- its mutants were killed when it was last judged.
blockFor :: FilePath -> ServerState -> IO (Maybe (Block, Int))
blockFor path s = do
  blocks <- readIORef (ssBlocks s)
  let p = normalisePath path
  pure (M.lookup p blocks <|> listToMaybe [bk | bk@(b, _) <- M.elems blocks, ("/" <> T.unpack (fileOf (blockTestId b))) `isSuffixOf` p])

-- | Begin a block at a cursor (1-based line). On a test file: its red test
-- (the one at the cursor, else the first) and the function it calls. On a
-- module: its test file's red test, or, all green, the function at the
-- cursor, judged at once. A test that cannot run begins a Broken block.
blockBegin :: (ProgressEvent -> IO ()) -> FilePath -> Int -> ServerState -> IO (ServerState, Either Text Block)
blockBegin progress path line s0 =
  projectFor path s0 >>= \case
    (s, Nothing) -> pure (s, Left (T.pack path <> " is in no Olifant project"))
    (s, Just (root, project)) -> fmap (s,) . runExceptT $ do
      let rel = makeRelative root path
          ws = prWorkspace project
          syntax = pSyntax (ssPorts s)
      lang <- maybe (throwE ("no language knows " <> T.pack rel)) pure (languageFor (catalogLanguages (prCatalog project)) rel)
      td <- maybe (throwE (langName lang <> " has no [test] table: a block needs its tests")) pure (langTest lang)
      paths <- lift (listFiles ws ".")
      let moduleRel = fromMaybe rel (listToMaybe [m | m <- paths, m /= rel, testsForFile td m == rel])
          onTest = moduleRel /= rel
          testRel = if onTest then rel else testsForFile td rel
          file = normalisePath (root </> moduleRel)
      busy <- lift (isJust <$> blockFor file s)
      when busy (throwE "a block is pending in this file: accept or reject it first")
      moduleText <- lift (readText ws moduleRel) >>= maybe (throwE ("cannot read " <> T.pack moduleRel)) pure
      testText <- lift (readText ws testRel) >>= maybe (throwE ("no test for this module yet; write one (its name and assertion) in " <> T.pack testRel <> ", then begin on it")) pure
      lift (progress (Verifying (T.pack testRel)))
      run <- ExceptT (runOver s ws lang [T.pack testRel])
      let tests = [T.pack testRel <> "::" <> n | n <- testNames testText]
          atCursor = defAbove line (if onTest then testText else moduleText)
          cursorTest = [t | onTest, Just n <- [atCursor], let t = T.pack testRel <> "::" <> n, t `elem` tests]
          calling = [t | not onTest, Just n <- [atCursor], t <- tests, n `elem` callsIn (testName t) testText]
          spanOf tid = lift (underTest syntax lang testText tid moduleText) >>= maybe (throwE ("cannot find the function " <> testName tid <> " calls in " <> T.pack moduleRel)) pure
          atFunction = case atCursor of
            Nothing -> throwE ("no function at line " <> T.pack (show line))
            Just n -> lift (functionSpan syntax lang n moduleText) >>= maybe (throwE ("cannot find function " <> n <> " in " <> T.pack moduleRel)) pure
          chosen = listToMaybe (cursorTest ++ calling)
          new tid sp st = do
            n <- lift (atomicModifyIORef' (ssNextBlock s) (\i -> (i + 1, i)))
            pure ((begin n file Code moduleText sp tid (Failure Nothing "" "")) {blockState = st})
      b <- case (trBroken run, trFailed run) of
        (Just why, _)
          | onTest, Just tid <- listToMaybe (cursorTest ++ tests) -> spanOf tid >>= \sp -> new tid sp (Broken why)
          | onTest -> throwE ("the tests cannot run: " <> why)
          | otherwise -> atFunction >>= \sp -> new (fromMaybe (T.pack testRel) chosen) sp (Broken why)
        (_, failed@((first, f0) : _)) -> do
          let (tid, f) = fromMaybe (first, f0) (listToMaybe [(t, f') | t <- cursorTest ++ calling, Just f' <- [lookup t failed]])
          sp <- spanOf tid
          new tid sp (Red f)
        _
          | onTest -> throwE "every test here passes; write a failing one (its name and assertion) and begin on it"
          | otherwise -> atFunction >>= \sp -> new (fromMaybe (T.pack testRel) chosen) sp Green
      claimed <- lift (claim s [normalisePath (root </> testRel)] b)
      unless claimed (throwE "a block is pending in this file: accept or reject it first")
      lift (modifyIORef' (ssBlockSeen s) (M.insert file moduleText))
      lift (appendStep (ssPorts s) root b "begin" (hud b))
      -- a hand-written green body goes straight to the judge
      if blockState b == Green then lift (either (const b) id <$> judgeIn s root project lang b 0) else pure b

-- | Draft (or the next draft): the block's span made a hole the model fills
-- with the drafts stance, verified by the block's one test; the edits write
-- the file. Nothing lands if the document moved on while the model wrote.
-- A test block's hole is its stub's arrangement: a kill's must pass on the
-- code and fail on the mutant, a new test's fail at its assertion.
blockDraft :: (ProgressEvent -> IO ()) -> FilePath -> ServerState -> IO (ServerState, Either Text (Block, [FileEdit]))
blockDraft progress path = withBlock path $ \s root project lang b killed ->
  fillerFor True s project >>= \case
    Left why -> pure (Left why)
    Right Nothing -> pure (Left "no model is configured ([model] in .olifant/config.toml); write the body yourself, the block still judges it")
    Right (Just filler) ->
      -- Drafting is stored and pushed at once: only reject is open while the model writes
      settle s root killed "drafting" "asked for a draft" StartDraft b >>= \case
        Left why -> pure (Left why)
        Right drafting -> do
          pushed s drafting killed
          runExceptT (draftIn progress s root project lang b drafting killed filler) >>= \case
            Right done -> pure (Right done)
            Left why -> do
              -- nothing lands: back to the state it began from, unless the block moved on
              settle s root killed "drafting" ("no draft: " <> why) DraftFailed drafting >>= either (const (pure ())) (\back -> pushed s back killed)
              pure (Left why)

-- | Tell the adapter a block changed with no request asking (it pushes it).
pushed :: ServerState -> Block -> Int -> IO ()
pushed s b killed = readIORef (ssOnBlock s) >>= \push -> push b killed

-- | The draft itself, the block (as it was, and DRAFTING, as stored) in hand.
draftIn :: (ProgressEvent -> IO ()) -> ServerState -> FilePath -> Project -> Language -> Block -> Block -> Int -> ModelFiller IO -> ExceptT Text IO (Block, [FileEdit])
draftIn progress s root project lang b drafting killed filler = do
  let file = blockFile b
      rel = makeRelative root file
      ws = prWorkspace project
      started = fst <$> M.lookup file (ssDocs s)
  text <- lift (readText ws rel) >>= maybe (throwE ("cannot read " <> T.pack rel)) pure
  (template, verify) <- case blockKind b of
    Code -> do
      let failure = case blockState b of
            Red f -> f
            _ -> Failure Nothing "the person asked for another draft" ""
      testText <- fromMaybe "" <$> lift (readText ws (T.unpack (fileOf (blockTestId b))))
      (template, _) <- except (draftTemplate lang text (blockSpan b) (blockTestId b) failure testText)
      pure (template, ("body", VerifyIds [blockTestId b]))
    TestFor killing -> do
      let (s1, s2) = blockSpan b
          candidates = [s1 .. s2] ++ [1 .. length (T.splitOn "\n" text)]
      (line, template) <- maybe (throwE "the test has no arrangement left to draft: its placeholder is gone") pure (listToMaybe [(l, t) | l <- candidates, Right t <- [testDraftTemplate lang text l]])
      let new = newTests b text
      verify <- case killing of
        Nothing -> pure (VerifyRedAt (blockTestId b) (line + 1))
        Just m -> do
          when (null new) (throwE "name the new test first: its name is still the stub's")
          moduleRel <- lift (moduleOf project lang rel) >>= maybe (throwE ("no module is tested by " <> T.pack rel)) pure
          -- what passed before the stub: it must still pass (a sibling red then is passed over)
          (_, files) <- lift (projectFiles ws M.empty lang [rel])
          keep <- lift (runTestsIn (pTester (ssPorts s) (ssCancel s)) lang [T.pack rel] (M.insert rel (blockBase b) files)) >>= \case
            Left why -> throwE ("the tests cannot run: " <> why)
            Right run
              | Just why <- trBroken run -> throwE ("the tests cannot run: " <> why)
              | otherwise -> pure (trPassed run)
          pure (VerifyKills (T.pack rel) (keep ++ new) new moduleRel (muText m))
      pure (template, ("arrange", verify))
  -- each fill the model offers, the last kept: a draft that fails its test
  -- is written as its last attempt, never as the placeholder
  attempts <- lift (newIORef M.empty)
  let recording = filler {mfFiller = Filler (\req -> runFill (mfFiller filler) req >>= \r -> either (const (pure ())) (\fills -> modifyIORef' attempts (M.union fills)) r >> pure r)}
      gen = (replaceGenerator rel (1, length (T.lines text)) text template) {genVerify = [verify]}
      previous = [draftText d | d <- blockDrafts b, draftBy d /= "you"]
      context = [("previous drafts (rejected by the person)", T.intercalate "\n\n---\n\n" previous) | not (null previous)]
      options = planOptions {poFiller = Just recording, poProgress = progress, poCancel = ssCancel s, poContext = context, poStance = DraftStance}
  plan <- withExceptT describeGenerationError (ExceptT (planWith (ssPorts s) project options [Step gen M.empty]))
  unless (null (planConflicts plan)) (throwE (T.intercalate "; " (map describeConflict (planConflicts plan))))
  latest <- lift (maybe (pure started) ($ file) (ssLiveVersion s))
  when (latest /= started) (throwE "the file changed while drafting; your text stands")
  tried <- lift (readIORef attempts)
  (planned, outcome, edits) <- case planHoles plan of
    h : _ | HoleFilled _ _ <- phStatus h -> pure (M.findWithDefault text rel (planWrites plan), Right (), editsOfPlan s root plan)
    h : _
      | HoleUnfilled why <- phStatus h ->
          case M.lookup (holeName (phSite h)) tried of
            Nothing -> throwE ("the model gave no draft: " <> why)
            Just attempt -> do
              let langOf = languageFor (catalogLanguages (prCatalog project))
                  written = M.findWithDefault text rel (applyFills langOf (planWrites plan) [(phSite h, attempt)])
                  (range, new) = smallestChange text written
              pure (written, Left (Failure Nothing why ""), [FileEdit file started False range new | written /= text])
      | HoleSkipped why <- phStatus h -> throwE why
    _ -> throwE "the draft made no hole"
  let changed = changedSpan text planned (blockSpan b)
      grown = length (T.splitOn "\n" planned) - length (T.splitOn "\n" text)
      -- a test block keeps its whole test (its def to its assertion)
      sp@(l1, l2) = case blockKind b of
        Code -> changed
        TestFor _ -> (min (fst changed) (fst (blockSpan b)), max (snd changed) (snd (blockSpan b) + grown))
      drafted = T.intercalate "\n" (take (l2 - l1 + 1) (drop (l1 - 1) (T.splitOn "\n" planned)))
  b' <- ExceptT (settle s root killed "draft" (either (const "red") (const "green") outcome) (Drafted drafted (mfName filler) outcome) drafting {blockSpan = sp})
  pure (b', edits)

-- | Run the block's test on the editor's text: red, green (then judged) or
-- broken; its span is the lines new since the block began.
blockTest :: FilePath -> ServerState -> IO (ServerState, Either Text Block)
blockTest path = withBlock path $ \s root project lang b killed -> testNow s root project lang b killed False

-- | 'blockTest' on the block's file as it is now. APPLIED: that text is the
-- latest draft, as the editor applied it: no edit of the person's (no "you"
-- draft), and the draft's span stands.
testNow :: ServerState -> FilePath -> Project -> Language -> Block -> Int -> Bool -> IO (Either Text Block)
testNow s root project lang b killed applied = do
  let ws = prWorkspace project
  now <- fromMaybe "" <$> readText ws (makeRelative root (blockFile b))
  modifyIORef' (ssBlockSeen s) (M.insert (blockFile b) now)
  -- a test block's own tests are the ones it added (its siblings may be red)
  let ids = case (blockKind b, newTests b now) of
        (TestFor _, new@(_ : _)) -> new
        _ -> [blockTestId b]
  runOver s ws lang ids >>= \case
    -- a runner that cannot start is a test that cannot run: never judged
    Left why -> settle s root killed "test" ("cannot run: " <> why) (CannotRun why) b
    Right run -> do
      let new = newLinesOf (blockBase b) now
          sp = if applied || S.null new then blockSpan b else (S.findMin new, S.findMax new)
          (move, result) = case (trBroken run, trFailed run) of
            (Just why, _) -> (CannotRun why, "cannot run: " <> why)
            (_, (_, f) : _) -> (landed sp (Left f), "red")
            _ -> (landed sp (Right ()), "green")
          landed = if applied then Applied else Edited
      settle s root killed "test" result move b >>= \case
        Right b' | blockState b' == Green -> judgeIn s root project lang b' killed
        other -> pure other

-- | Judge a green block: the mutants of its span's new lines (the whole
-- span when none is new: a body begun on as it was), each run against the
-- test's file and judged by the tests that passed on it unmutated (a
-- sibling test still red, the next one being written, notices nothing).
blockJudge :: FilePath -> ServerState -> IO (ServerState, Either Text Block)
blockJudge path = withBlock path $ \s root project lang b killed -> judgeIn s root project lang b killed

-- | A test block is not mutated: green, it is judged. A code block's
-- survivors come in Jev's order (what it already said; the rest as it
-- answers, pushed).
judgeIn :: ServerState -> FilePath -> Project -> Language -> Block -> Int -> IO (Either Text Block)
judgeIn s root project lang b killedBefore = case (step (Judge []) b, blockKind b) of
  (Left why, _) -> pure (Left why)
  (Right _, TestFor _) -> settle s root killedBefore "judge" "a test: nothing to mutate" (Judge []) b
  (Right _, Code) -> do
    let ws = prWorkspace project
        rel = makeRelative root (blockFile b)
        (l1, l2) = blockSpan b
        testFile = fileOf (blockTestId b)
        decider = deciderFor s project
    now <- fromMaybe "" <$> readText ws rel
    testText <- fromMaybe "" <$> readText ws (T.unpack testFile)
    let new = S.filter (\l -> l >= l1 && l <= l2) (newLinesOf (blockBase b) now)
        scope = if S.null new then S.fromList [l1 .. l2] else new
        operators = if langName lang == "python" then pythonOperators else []
    verdicts <-
      mutantsOf (pSyntax (ssPorts s)) lang operators now scope >>= \case
        Left f -> pure (Left (describeSyntaxFailure f))
        Right [] -> pure (Right [])
        Right ms -> do
          (_, files) <- projectFiles ws M.empty lang [T.unpack testFile]
          -- the baseline: which tests pass on the files as they are, and how
          -- long they take (each mutant's run gets a timeout from it)
          t0 <- getMonotonicTime
          baseline <- runTestsIn (pTester (ssPorts s) (ssCancel s)) lang [testFile] files
          t1 <- getMonotonicTime
          case baseline of
            Left why -> pure (Left why)
            Right base
              | Just why <- trBroken base -> pure (Left why)
              | otherwise -> judge (pTesterWithin (ssPorts s) (judgeTimeout (t1 - t0)) (ssCancel s)) 4 lang rel files testFile (trPassed base) (capMutants 20 ms)
    case verdicts of
      Left why -> settle s root killedBefore "judge" ("cannot run: " <> why) (CannotRun why) b
      Right vs -> do
        let surviving = [m | (m, Survived) <- vs]
            killed = length vs - length surviving
        settle s root killed "judge" (tshow killed <> "/" <> tshow (length vs) <> " killed") (Judge surviving) b >>= \case
          -- in Jev's order: what it already said now, the rest as it answers
          Right judged -> Right . fromMaybe judged <$> reorder s decider now testText False (blockFile judged)
          other -> pure other

-- | The file's judged block in Jev's order, stored (and, PUSH, pushed)
-- when that changed it; each answer that lands later orders it again.
reorder :: ServerState -> Maybe (Decider IO) -> Text -> Text -> Bool -> FilePath -> IO (Maybe Block)
reorder s decider moduleText testText push file =
  M.lookup file <$> readIORef (ssBlocks s) >>= \case
    Just (b, killed) | Judged ms <- blockState b -> do
      ordered <- orderSurvivors s decider b moduleText testText (void (reorder s decider moduleText testText True file)) ms
      let b' = b {blockState = Judged ordered}
      -- only over the block as it was (a move meanwhile wins)
      stored <- atomicModifyIORef' (ssBlocks s) $ \m -> case M.lookup file m of
        Just (now, _) | now == b, ordered /= ms -> (M.insert file (b', killed) m, True)
        _ -> (m, False)
      when (stored && push) (pushed s b' killed)
      pure (if stored then Just b' else Nothing)
    _ -> pure Nothing

-- | Accept a judged block whose survivors are all waived. A test block
-- (a kill's) accepted: its module's code block is judged again, and pushed.
blockAccept :: FilePath -> ServerState -> IO (ServerState, Either Text (Block, [FileEdit]))
blockAccept path = withBlock path $ \s root project _ b killed ->
  settle s root killed "accept" "accepted" Accept b >>= \case
    Right done | TestFor _ <- blockKind b -> rejudgeFor s root project (blockFile b) >> pure (Right (done, []))
    other -> pure (fmap (,[]) other)

-- | A test file gained a test: its module's judged code block is judged
-- again (its test is green: what was judged still stands), and pushed.
rejudgeFor :: ServerState -> FilePath -> Project -> FilePath -> IO ()
rejudgeFor s root project testFile = do
  let rel = makeRelative root testFile
      langs = catalogLanguages (prCatalog project)
  found <- maybe (pure Nothing) (\tl -> moduleOf project tl rel) (languageFor langs rel)
  case found of
    Nothing -> pure ()
    Just moduleRel -> do
      let file = normalisePath (root </> moduleRel)
      M.lookup file <$> readIORef (ssBlocks s) >>= \case
        Just (cb, killed) | blockKind cb == Code, Judged _ <- blockState cb, Just lang <- languageFor langs moduleRel ->
          settle s root killed "rejudge" "its test file gained a test" Rejudge cb
            >>= either (pure . Left) (\green -> judgeIn s root project lang green killed)
            >>= \case
              Right cb' -> do
                killed' <- maybe killed snd . M.lookup file <$> readIORef (ssBlocks s)
                readIORef (ssOnBlock s) >>= \push -> push cb' killed'
              Left _ -> pure ()
        _ -> pure ()

-- | The module a test file tests (by the language's `tests_for`).
moduleOf :: Project -> Language -> FilePath -> IO (Maybe FilePath)
moduleOf project lang testRel = case langTest lang of
  Nothing -> pure Nothing
  Just td -> listToMaybe . filter (\m -> m /= testRel && testsForFile td m == testRel) <$> listFiles (prWorkspace project) "."


-- | Reject a block: its file goes back to the text it had when it began.
blockReject :: FilePath -> ServerState -> IO (ServerState, Either Text (Block, [FileEdit]))
blockReject path = withBlock path $ \s root project _ b killed -> do
  now <- fromMaybe "" <$> readText (prWorkspace project) (makeRelative root (blockFile b))
  let version = fst <$> M.lookup (blockFile b) (ssDocs s)
      restore = [FileEdit (blockFile b) version False range text | now /= blockBase b, let (range, text) = smallestChange now (blockBase b)]
  fmap (,restore) <$> settle s root killed "reject" "rejected" Reject b

-- | A block as the editor shows it, with its killed count.
blockJson :: Block -> Int -> Value
blockJson b killed =
  object
    [ "id" .= blockId b
    , "file" .= blockFile b
    , "span" .= object ["first" .= fst (blockSpan b), "last" .= snd (blockSpan b)]
    , "state" .= stateName (blockState b)
    , "hud" .= hud b
    , "drafts" .= [object ["text" .= draftText d, "by" .= draftBy d, "outcome" .= draftOutcome d] | d <- blockDrafts b]
    , "survivors" .= [object ["line" .= muLine m, "operator" .= muOperator m, "from" .= flat (muFrom m), "to" .= flat (muTo m), "waived" .= muWaived m, "dim" .= muDim m] | m <- judged]
    , "open" .= open b
    , "killed" .= killed
    ]
  where
    judged = case blockState b of
      Judged ms -> ms
      _ -> []
    flat = T.replace "\n" "⏎"

-- | A state's kind, as the editor names it.
stateName :: BlockState -> Text
stateName = \case
  Red _ -> "red"
  Drafting _ -> "drafting"
  Green -> "green"
  Judged _ -> "judged"
  Broken _ -> "broken"
  Accepted -> "accepted"
  Rejected -> "rejected"

-- | One line of the step log (`.olifant/state/steps.log`, ignored by git):
-- time, block, move, who made it (the person, the model, mutation), result.
-- A log that cannot be written never stops a move.
appendStep :: Ports -> FilePath -> Block -> Text -> Text -> IO ()
appendStep ports root b move result = do
  now <- pNow ports
  let path = root </> ".olifant/state/steps.log"
      who = case move of
        "draft" -> maybe "model" draftBy (listToMaybe (reverse (blockDrafts b)))
        "judge" -> "mutation"
        _ -> "person"
      entry = T.intercalate "\t" [T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now), "block " <> tshow (blockId b), move, who, T.unwords (T.words result)]
  void (try (readText (pFiles ports) path >>= \old -> writeText (pFiles ports) path (fromMaybe "" old <> entry <> "\n")) :: IO (Either SomeException ()))

-- | The function a test exercises: the first `.NAME(` call on its assert
-- lines (then its body's, last first) that the module defines → its body's
-- lines (1-based, inclusive). A port's call (`bookings.booking_at(`) is
-- passed over.
underTest :: Syntax IO -> Language -> Text -> TestId -> Text -> IO (Maybe (Int, Int))
underTest syntax lang testText tid moduleText = firstFound (callsIn (testName tid) testText)
  where
    firstFound [] = pure Nothing
    firstFound (n : ns) = functionSpan syntax lang n moduleText >>= maybe (firstFound ns) (pure . Just)

-- | A function's body (the language's `function NAME` target), 1-based.
functionSpan :: Syntax IO -> Language -> Text -> Text -> IO (Maybe (Int, Int))
functionSpan syntax lang name text = either (const Nothing) (\(a, z) -> Just (a + 1, z + 1)) <$> locateContainer syntax lang ["function", name] text

-- | The pending block of a file, its project's root, project and language.
withBlock :: FilePath -> (ServerState -> FilePath -> Project -> Language -> Block -> Int -> IO (Either Text a)) -> ServerState -> IO (ServerState, Either Text a)
withBlock path work s0 =
  blockFor path s0 >>= \case
    Nothing -> pure (s0, Left "no block is pending in this file; begin one on a red test")
    Just (b, killed) ->
      projectFor (blockFile b) s0 >>= \case
        (s, Nothing) -> pure (s, Left (T.pack (blockFile b) <> " is in no Olifant project"))
        (s, Just (root, project)) -> case languageFor (catalogLanguages (prCatalog project)) (makeRelative root (blockFile b)) of
          Nothing -> pure (s, Left ("no language knows " <> T.pack (blockFile b)))
          Just lang -> (s,) <$> work s root project lang b killed

-- | Take a move on the block B as it was read: the block as it now stands
-- is kept (a finished one dropped), and the move logged. Blocks are
-- written from requests and background jobs at once: the move lands only
-- while the stored block is still B's (its number, and its state's kind),
-- one atomic step; a block gone or moved on meanwhile refuses it.
settle :: ServerState -> FilePath -> Int -> Text -> Text -> Move -> Block -> IO (Either Text Block)
settle s root killed name result move b = case step move b of
  Left why -> pure (Left why)
  Right b' -> do
    landed <- atomicModifyIORef' (ssBlocks s) $ \m -> case M.lookup (blockFile b) m of
      Just (now, _)
        | blockId now == blockId b, stateName (blockState now) == stateName (blockState b) ->
            ((if pending b' then M.insert (blockFile b') (b', killed) else M.delete (blockFile b')) m, True)
      _ -> (m, False)
    if landed
      then appendStep (ssPorts s) root b' name result >> pure (Right b')
      else pure (Left "the block moved on")

-- | Store a new block in its file unless a block is pending there or in
-- one of OTHERS (its test file): one atomic step, so two begins at once
-- make one block.
claim :: ServerState -> [FilePath] -> Block -> IO Bool
claim s others b = atomicModifyIORef' (ssBlocks s) $ \m ->
  if any (`M.member` m) (blockFile b : others) then (m, False) else (M.insert (blockFile b) (b, 0) m, True)

-- | Tests (ids, or test files) run over the project's files as the editor has them.
runOver :: ServerState -> Workspace IO -> Language -> [TestId] -> IO (Either Text TestRun)
runOver s ws lang ids = do
  (_, files) <- projectFiles ws (M.empty :: Map FilePath Text) lang (map (T.unpack . fileOf) ids)
  runTestsIn (pTester (ssPorts s) (ssCancel s)) lang ids files

-- | The lines (1-based) where NEW differs from OLD; FALLBACK when it does not.
changedSpan :: Text -> Text -> (Int, Int) -> (Int, Int)
changedSpan old new fallback
  | lastNew < common + 1 = fallback
  | otherwise = (common + 1, lastNew)
  where
    (os, ns) = (T.splitOn "\n" old, T.splitOn "\n" new)
    common = length (takeWhile id (zipWith (==) os ns))
    shared = length (takeWhile id (take (min (length os) (length ns) - common) (zipWith (==) (reverse os) (reverse ns))))
    lastNew = length ns - shared

-- | The functions a test calls, most likely under test first (see 'underTest').
callsIn :: Text -> Text -> [Text]
callsIn name testText = nub (concatMap calls asserts ++ reverse (concatMap calls body))
  where
    body = testBody name testText
    asserts = filter (("assert" `T.isPrefixOf`) . T.strip) body
    calls l = [n | seg <- drop 1 (T.splitOn "." l), let n = T.takeWhile ident seg, not (T.null n), "(" `T.isPrefixOf` T.drop (T.length n) seg]

-- | A test's lines: its def to the next line at or left of the def's indentation.
testBody :: Text -> Text -> [Text]
testBody name text = case dropWhile (not . isDef) (T.lines text) of
  [] -> []
  d : rest -> takeWhile (\l -> T.null (T.strip l) || indentOf l > indentOf d) rest
  where
    isDef l = ("def " <> name <> "(") `T.isPrefixOf` T.strip l
    indentOf = T.length . T.takeWhile (== ' ')

-- | A test block's new tests: those its file defines now and did not when
-- the block began (the person named them in the stub).
newTests :: Block -> Text -> [TestId]
newTests b now = [fileOf (blockTestId b) <> "::" <> n | n <- testNames now, n `notElem` testNames (blockBase b)]

-- | The tests a test file defines (`def test…(`).
testNames :: Text -> [Text]
testNames text = [n | Just n <- map defName (T.lines text), "test" `T.isPrefixOf` n]

-- | The name of the def at or above a line (1-based).
defAbove :: Int -> Text -> Maybe Text
defAbove line text = listToMaybe [n | Just n <- map defName (reverse (take line (T.lines text)))]

defName :: Text -> Maybe Text
defName l = do
  rest <- T.stripPrefix "def " (fromMaybe stripped (T.stripPrefix "async " stripped))
  let n = T.takeWhile ident rest
  if not (T.null n) && "(" `T.isPrefixOf` T.drop (T.length n) rest then Just n else Nothing
  where
    stripped = T.strip l

ident :: Char -> Bool
ident c = isAlphaNum c || c == '_'

-- | A test id's file (`file::name`), and its name.
fileOf, testName :: TestId -> Text
fileOf = fst . T.breakOn "::"
testName = snd . T.breakOnEnd "::"

tshow :: Show a => a -> Text
tshow = T.pack . show
