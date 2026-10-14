-- | Internal to Olifant.Server: the loop's blocks. One is begun on a red
-- test (or on a function whose test is green: judged at once), drafted as a
-- hole its one test verifies, tested again on the editor's text, judged by
-- mutation, then accepted or rejected; every move is a line of the step
-- log. The machine is "Olifant.Block"'s: this module tells it what happened.
module Olifant.Server.Block
  ( blockFor
  , blockBegin
  , blockStart
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
  , settleOn
  , awaitingVerdict
  , spanEdited
  , testNow
  , fileOf
  , runOver
  , tshow
  , claim
  , pushed
  , Site (..)
  , siteAt
  , siteFile
  , siteTestFile
  , functionAt
  , openTestStub
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad (unless, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE, withExceptT)
import Data.Aeson (Value, object, (.=))
import Data.Char (isAlphaNum)
import Data.Functor ((<&>))
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (isSuffixOf, nub, stripPrefix)
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
import Olifant.Generation.FillPhase (addImports, applyFills, isImportLine, projectFiles)
import Olifant.Generator (GeneratorWith (..), Verify (..), replaceGenerator)
import Olifant.Template (Template)
import Olifant.Holes (HoleSite (..))
import Olifant.Language (Failure (..), Language (..), Syntax, TestId, TestRun (..), describeSyntaxFailure, languageFor, runTestsIn, testsForFile)
import Olifant.Mutate (Mutant (..), Verdict (..), capMutants, judge, judgeTimeout, mutantsOf)
import Olifant.Mutate.Python (pythonOperators)
import Olifant.Newness (newLinesOf)
import Olifant.Process (cancelled)
import Olifant.Server.Actions (fillerFor)
import Olifant.Server.Edits (FileEdit (..), editsOfPlan, smallestChange)
import Olifant.Server.BlockDecide (orderSurvivors)
import Olifant.Server.State (ServerState (..), deciderFor, projectFor, workspaceOf)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (dropExtension, makeRelative, splitDirectories, (</>))

-- | The pending block of a file (its module or its test), with how many of
-- its mutants were killed when it was last judged.
blockFor :: FilePath -> ServerState -> IO (Maybe (Block, Int))
blockFor path s = do
  blocks <- readIORef (ssBlocks s)
  let p = normalisePath path
  pure (M.lookup p blocks <|> listToMaybe [bk | bk@(b, _) <- M.elems blocks, ("/" <> T.unpack (fileOf (blockTestId b))) `isSuffixOf` p])

-- | Begin a block at a cursor (1-based line). On a test file: its red test
-- (the one at the cursor, else the first) and the function it calls. On a
-- module: the red test calling the function at the cursor, or, those
-- green, that function, judged at once; a function no test calls opens
-- the stub of its first test instead (see 'blockStart'). A test that
-- cannot run begins a Broken block.
blockBegin :: (ProgressEvent -> IO ()) -> FilePath -> Int -> ServerState -> IO (ServerState, Either Text Block)
blockBegin progress path line s0 = fmap (fmap fst) <$> blockStart progress path line s0

-- | 'blockBegin', with the stub the editor expands when it opened one: the
-- test file (absolute), the line to put it at, its text (tab stops for
-- the test's name and assertion, a hole for its arrangement), and the
-- edits the editor applies first (the line counts them). A function no
-- test calls (no test file, or none of its tests calls it) begins a test
-- block on the stub at the end of its test file, drafted red at its
-- assertion; accepted, the function begins on it. The stub needs the
-- import of the function, or of its class, in Python: a new file starts
-- with it; a file that lacks it gains it, as the adapter's imports join a
-- file's (merged into the line importing from its module, else after the
-- last import).
blockStart :: (ProgressEvent -> IO ()) -> FilePath -> Int -> ServerState -> IO (ServerState, Either Text (Block, Maybe (FilePath, Int, Text, [FileEdit])))
blockStart progress path line s0 =
  siteAt path line s0 >>= \case
    (s, Left why) -> pure (s, Left why)
    (s, Right site) -> fmap (s,) . runExceptT $ do
      let root = siteRoot site
          project = siteProject site
          lang = siteLang site
          ws = prWorkspace project
          syntax = pSyntax (ssPorts s)
          moduleRel = siteModuleRel site
          testRel = siteTestRel site
          onTest = siteOnTest site
          file = siteFile site
          testFile = siteTestFile site
          moduleText = siteModuleText site
          testText0 = siteTestText site
          atCursor = siteAtCursor site
          pendingHere = "a magic circle is open in this file: accept or reject it first"
      busy <- lift (isJust <$> blockFor file s)
      when busy (throwE pendingHere)
      let number = lift (atomicModifyIORef' (ssNextBlock s) (\i -> (i + 1, i)))
      -- a function no test calls: its first test's stub opens
      untested <- case (onTest, atCursor) of
        (False, Just n) | not (any (calledIn n) (maybe [] (\t -> map (`testBody` t) (testNames t)) testText0)) -> lift (functionSpan syntax lang n moduleText) <&> fmap (const n)
        _ -> pure Nothing
      case untested of
        Just n -> fmap Just <$> openTestStub s site n Nothing [file] pendingHere
        Nothing -> do
          testText <- maybe (throwE ("no test for this module yet; write one (its name and assertion) in " <> T.pack testRel <> ", then begin on it")) pure testText0
          lift (progress (Verifying (T.pack testRel)))
          run <- ExceptT (runOver s ws lang [T.pack testRel])
          let tests = [T.pack testRel <> "::" <> n | n <- testNames testText]
              cursorTest = [t | onTest, Just n <- [atCursor], let t = T.pack testRel <> "::" <> n, t `elem` tests]
              calling = [t | not onTest, Just n <- [atCursor], t <- tests, n `elem` callsIn (testName t) testText]
              spanOf tid = lift (underTest syntax lang testText tid moduleText) >>= maybe (throwE ("cannot find the function " <> testName tid <> " calls in " <> T.pack moduleRel)) pure
              atFunction = case atCursor of
                Nothing -> throwE ("no function at line " <> T.pack (show line))
                Just n -> lift (functionSpan syntax lang n moduleText) >>= maybe (throwE ("cannot find function " <> n <> " in " <> T.pack moduleRel)) pure
              chosen = listToMaybe (cursorTest ++ calling)
              new tid sp st = do
                k <- number
                pure ((begin k file Code moduleText sp tid (Failure Nothing "" "")) {blockState = st})
          b <- case (trBroken run, trFailed run) of
            (Just why, _)
              | onTest, Just tid <- listToMaybe (cursorTest ++ tests) -> spanOf tid >>= \sp -> new tid sp (Broken why)
              | onTest -> throwE ("the tests cannot run: " <> why)
              | otherwise -> atFunction >>= \sp -> new (fromMaybe (T.pack testRel) chosen) sp (Broken why)
            (_, failed@((first, f0) : _))
              -- the tests calling the function pass: another test red is not its
              | not onTest, not (null calling), all (`notElem` map fst failed) calling -> atFunction >>= \sp -> new (fromMaybe (T.pack testRel) chosen) sp Green
              | otherwise -> do
                  let (tid, f) = fromMaybe (first, f0) (listToMaybe [(t, f') | t <- cursorTest ++ calling, Just f' <- [lookup t failed]])
                  sp <- spanOf tid
                  new tid sp (Red f)
            _
              | onTest -> throwE "every test here passes; write a failing one (its name and assertion) and begin on it"
              | otherwise -> atFunction >>= \sp -> new (fromMaybe (T.pack testRel) chosen) sp Green
          claimed <- lift (claim s [testFile] b)
          unless claimed (throwE pendingHere)
          lift (modifyIORef' (ssBlockSeen s) (M.insert file moduleText))
          lift (appendStep (ssPorts s) root b "begin" (hud b))
          -- a hand-written green body goes straight to the judge
          (,Nothing) <$> if blockState b == Green then lift (either (const b) id <$> judgeIn s root project lang b 0) else pure b

-- | Where a begin or a stub at a cursor stands: the module and its test
-- file (the cursor in either), their texts, and the def at or above the
-- cursor.
data Site = Site
  { siteRoot :: FilePath
  , siteProject :: Project
  , siteLang :: Language
  , siteModuleRel :: FilePath
  , siteTestRel :: FilePath
  , siteOnTest :: Bool
  , siteModuleText :: Text
  , siteTestText :: Maybe Text
  , siteAtCursor :: Maybe Text
  }

siteFile, siteTestFile :: Site -> FilePath
siteFile site = normalisePath (siteRoot site </> siteModuleRel site)
siteTestFile site = normalisePath (siteRoot site </> siteTestRel site)

-- | The site of a cursor (1-based line) in a file: its project, its
-- language's tests, the module and the test file; or why there is none.
siteAt :: FilePath -> Int -> ServerState -> IO (ServerState, Either Text Site)
siteAt path line s0 =
  projectFor path s0 >>= \case
    (s, Nothing) -> pure (s, Left (T.pack path <> " is in no Olifant project"))
    (s, Just (root, project)) -> fmap (s,) . runExceptT $ do
      let rel = makeRelative root path
          ws = prWorkspace project
      lang <- maybe (throwE ("no language knows " <> T.pack rel)) pure (languageFor (catalogLanguages (prCatalog project)) rel)
      td <- maybe (throwE (langName lang <> " has no [test] table: a magic circle needs its tests")) pure (langTest lang)
      paths <- lift (listFiles ws ".")
      let moduleRel = fromMaybe rel (listToMaybe [m | m <- paths, m /= rel, testsForFile td m == rel])
          onTest = moduleRel /= rel
          testRel = if onTest then rel else testsForFile td rel
      moduleText <- lift (readText ws moduleRel) >>= maybe (throwE ("cannot read " <> T.pack moduleRel)) pure
      testText0 <- lift (readText ws testRel)
      pure (Site root project lang moduleRel testRel onTest moduleText testText0 (defAbove line (if onTest then fromMaybe "" testText0 else moduleText)))

-- | The function a cursor (1-based line) is in, in a module: the def at or
-- above it, while its body reaches the cursor.
functionAt :: ServerState -> Site -> Int -> IO (Maybe Text)
functionAt s site line = case (siteOnTest site, siteAtCursor site) of
  (False, Just n) ->
    functionSpan (pSyntax (ssPorts s)) (siteLang site) n (siteModuleText site) <&> \case
      Just (_, z) | line <= z -> Just n
      _ -> Nothing
  _ -> pure Nothing

-- | A new test's stub for the function FN at the end of its test file (the
-- import of the function, or of its class, in Python: a new file starts
-- with it; a file that lacks it gains it, as the adapter's imports join a
-- file's), and its test block begun on it, drafted red at its assertion,
-- stored unless a block is pending in the test file or in OTHERS (REFUSED
-- says so). Its test's name is NAME, given, else the person's to type.
-- The stub: the test file (absolute), the line to put it at, its text, and
-- the edits the editor applies first (the line counts them).
openTestStub :: ServerState -> Site -> Text -> Maybe Text -> [FilePath] -> Text -> ExceptT Text IO (Block, (FilePath, Int, Text, [FileEdit]))
openTestStub s site n name others refused = do
  let lang = siteLang site
      testRel = siteTestRel site
      testFile = siteTestFile site
      testText0 = siteTestText site
      base = fromMaybe "" testText0
      (at0, stub) = testStub lang base name
      imp = importOf lang (siteModuleRel site) (siteModuleText site) n
      -- an existing file: the import joins its imports (unless it has it), the stub after
      imported = maybe base (\i -> if isJust testText0 then addImports lang [i] base else base) imp
      edits = [FileEdit testFile (fst <$> M.lookup testFile (ssDocs s)) False range new | imported /= base, let (range, new) = smallestChange base imported]
      at = at0 + length (T.splitOn "\n" imported) - length (T.splitOn "\n" base)
      body = maybe stub (\i -> i <> "\n" <> stub) (if isJust testText0 then Nothing else imp)
      defAt = at + length (takeWhile (not . ("def " `T.isPrefixOf`)) (T.splitOn "\n" body))
      -- named, the block's test is the stub's; else the file's until the person names it
      (tid, failure) = case name of
        Just t -> (T.pack testRel <> "::" <> t, Failure Nothing "" "")
        Nothing -> (T.pack testRel, Failure Nothing "a new test: write its name and assertion" "")
  k <- lift (atomicModifyIORef' (ssNextBlock s) (\i -> (i + 1, i)))
  let tb = begin k testFile (TestFor Nothing) base (defAt, defAt + 2) tid failure
  claimed <- lift (claim s others tb)
  unless claimed (throwE refused)
  lift (modifyIORef' (ssBlockSeen s) (M.insert testFile base))
  lift (appendStep (ssPorts s) (siteRoot site) tb "begin" (hud tb))
  pure (tb, (testFile, at, body, edits))

-- | Whether a test's lines call a function by its name (as a method or not).
calledIn :: Text -> [Text] -> Bool
calledIn n = any (\l -> any (\(pre, _) -> T.null pre || not (ident (T.last pre))) (T.breakOnAll (n <> "(") l))

-- | The line importing a function into a new test file (the class it is a
-- method of, if any): Python's `from pkg.module import Name`, the module's
-- path less its `src/` folder; none for another language.
importOf :: Language -> FilePath -> Text -> Text -> Maybe Text
importOf lang moduleRel moduleText fn
  | langName lang /= "python" = Nothing
  | otherwise = Just ("from " <> dotted <> " import " <> owner)
  where
    dotted = T.intercalate "." (map T.pack (splitDirectories (dropExtension (fromMaybe moduleRel (stripPrefix "src/" moduleRel)))))
    ls = T.lines moduleText
    owner = case break (\l -> ("def " <> fn <> "(") `T.isPrefixOf` T.strip l) ls of
      (above, d : _) | T.isPrefixOf " " d || T.isPrefixOf "\t" d -> case [l | l <- reverse above, not (T.null (T.strip l)), not (" " `T.isPrefixOf` l || "\t" `T.isPrefixOf` l)] of
        l : _ | Just c <- T.stripPrefix "class " l -> T.takeWhile ident c
        _ -> fn
      _ -> fn

-- | Draft (or the next draft): the block's span made a hole the model fills
-- with the drafts stance, verified by the block's one test; the edits write
-- the file. Nothing lands if the document moved on while the model wrote.
-- A test block's hole is its stub's arrangement: a kill's must pass on the
-- code and fail on the mutant, a new test's fail at its assertion.
blockDraft :: (ProgressEvent -> IO ()) -> FilePath -> ServerState -> IO (ServerState, Either Text (Block, [FileEdit]))
blockDraft progress path = withBlock path $ \s root project lang b killed ->
  fillerFor True s project >>= \case
    Left why -> pure (Left why)
    Right Nothing -> pure (Left "no model is configured ([model] in .olifant/config.toml); write the body yourself, the magic circle still judges it")
    Right (Just filler) -> do
      -- a test block's stub not ready to draft is refused before anything moves
      text <- fromMaybe "" <$> readText (prWorkspace project) (makeRelative root (blockFile b))
      let ready = case blockKind b of
            TestFor _ -> void (testDraftParts lang b text)
            Code -> Right ()
      case ready of
        Left why -> pure (Left why)
        -- Drafting is stored and pushed at once: only reject is open while the model writes
        Right () ->
          settle s root killed "drafting" "asked for a draft" StartDraft b >>= \case
            Left why -> pure (Left why)
            Right drafting -> do
              pushed s drafting killed
              runExceptT (draftIn progress s root project lang b drafting killed filler) >>= \case
                Right done -> pure (Right done)
                Left why -> do
                  -- nothing lands: back to the state it began from, unless the block moved on
                  settle s root killed "drafting" ("no draft: " <> why) DraftFailed drafting >>= either (const (pure ())) (restored s root project lang killed)
                  pure (Left why)

-- | A failed draft put its block's state back: pushed, or, when the file
-- as the editor has it now is no longer the text that state was
-- established on (the person typed while the model wrote: that edit's
-- test was the draft's to settle, or is still running and will be
-- stale), tested again first, as an edit is, on that text.
restored :: ServerState -> FilePath -> Project -> Language -> Int -> Block -> IO ()
restored s root project lang killed back = do
  let file = blockFile back
  live <- maybe (pure Nothing) ($ file) (ssLiveDoc s)
  now <- maybe (fromMaybe "" <$> readText (prWorkspace project) (makeRelative root file)) (pure . snd) live
  -- the project as the editor has it now, for the test
  let current = maybe s (\doc -> s {ssDocs = M.insert file doc (ssDocs s)}) live
      project' = project {prWorkspace = workspaceOf current root}
  if now == blockOn back
    then pushed s back killed
    else
      testNow current root project' lang back killed False >>= \case
        Right tested -> do
          killed' <- maybe killed snd . M.lookup (blockFile tested) <$> readIORef (ssBlocks s)
          pushed s tested killed'
        -- the block moved on meanwhile: whatever moved it pushed it
        Left _ -> pure ()

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
  -- the text the draft plans on is seen: an edit made before it (its test
  -- still to come) does not knock the draft out; one made after it does
  lift (modifyIORef' (ssBlockSeen s) (M.insert file text))
  -- the hole, its verifier, and the file beside it the model sees (spec §5):
  -- a code draft its test file, a test draft its module
  (template, verify, beside) <- case blockKind b of
    Code -> do
      let failure = case blockState b of
            Red f -> f
            _ -> Failure Nothing "the person asked for another draft" ""
      testText <- fromMaybe "" <$> lift (readText ws (T.unpack (fileOf (blockTestId b))))
      (template, _) <- except (draftTemplate lang text (blockSpan b) (blockTestId b) failure testText)
      pure (template, ("body", VerifyIds [blockTestId b]), [(T.unpack (fileOf (blockTestId b)), testText)])
    TestFor killing -> do
      ((line, template), newTest, new) <- except (testDraftParts lang b text)
      tested <- lift (moduleOf project lang rel)
      moduleText <- maybe (pure Nothing) (lift . readText ws) tested
      verify <- case killing of
        -- a new test: it must fail, at its assertion
        Nothing -> pure (VerifyRedAt newTest (line + 1))
        Just m -> do
          moduleRel <- maybe (throwE ("no module is tested by " <> T.pack rel)) pure tested
          -- what passed before the stub: it must still pass (a sibling red then is passed over)
          (_, files) <- lift (projectFiles ws M.empty lang [rel])
          keep <- lift (runTestsIn (pTester (ssPorts s) (ssCancel s)) lang [T.pack rel] (M.insert rel (blockBase b) files)) >>= \case
            Left why -> throwE ("the tests cannot run: " <> why)
            Right run
              | Just why <- trBroken run -> throwE ("the tests cannot run: " <> why)
              | otherwise -> pure (trPassed run)
          pure (VerifyKills (T.pack rel) (keep ++ new) new moduleRel (muText m))
      pure (template, ("arrange", verify), [(m, t) | Just m <- [tested], Just t <- [moduleText]])
  -- each fill the model offers, the last kept: a draft that fails its test
  -- is written as its last attempt, never as the placeholder
  attempts <- lift (newIORef M.empty)
  let recording = filler {mfFiller = Filler (\req -> runFill (mfFiller filler) req >>= \r -> either (const (pure ())) (\fills -> modifyIORef' attempts (M.union fills)) r >> pure r)}
      gen = (replaceGenerator rel (1, length (T.lines text)) text template) {genVerify = [verify]}
      previous = [draftText d | d <- blockDrafts b, draftBy d /= "you"]
      context = beside ++ [("previous drafts (rejected by the person)", T.intercalate "\n\n---\n\n" previous) | not (null previous)]
      options = planOptions {poFiller = Just recording, poProgress = progress, poCancel = ssCancel s, poContext = context, poStance = DraftStance}
  plan <- withExceptT describeGenerationError (ExceptT (planWith (ssPorts s) project options [Step gen M.empty]))
  unless (null (planConflicts plan)) (throwE (T.intercalate "; " (map describeConflict (planConflicts plan))))
  latest <- lift (maybe (pure started) (\live -> fmap fst <$> live file) (ssLiveDoc s))
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
  b' <- ExceptT (settleOn s root killed "draft" (either (const "red") (const "green") outcome) (Just planned) (Drafted drafted (mfName filler) outcome) drafting {blockSpan = sp})
  -- a green draft that changes nothing brings no edit back to be tested: judged now
  if null edits && blockState b' == Green then (,[]) <$> ExceptT (judgeIn s root project lang b' killed) else pure (b', edits)

-- | A test block's stub as a draft needs it: its arrangement's line and
-- hole, its new test and all its new tests; or why it cannot be drafted yet.
testDraftParts :: Language -> Block -> Text -> Either Text ((Int, Template), TestId, [TestId])
testDraftParts lang b text = do
  let (s1, s2) = blockSpan b
      candidates = [s1 .. s2] ++ [1 .. length (T.splitOn "\n" text)]
      new = newTests b text
  hole <- maybe (Left "the test has no arrangement left to draft: its placeholder is gone") Right (listToMaybe [(l, t) | l <- candidates, Right t <- [testDraftTemplate lang text l]])
  -- the stub names its test `test_name` until the person types over it
  newTest <- case new of
    t : _ | all ((/= stubName) . testName) new -> Right t
    _ -> Left "name the new test first: its name is still the stub's"
  pure (hole, newTest, new)

-- | Run the block's test on the editor's text: red, green (then judged) or
-- broken; its span is the lines new since the block began.
blockTest :: FilePath -> ServerState -> IO (ServerState, Either Text Block)
blockTest path = withBlock path $ \s root project lang b killed -> case blockState b of
  -- its result and the draft's would race: the later would land over the newer
  Drafting _ -> pure (Left "a draft is in flight; wait for it, then test or draft again")
  _ -> testNow s root project lang b killed False

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
        -- none new yet: the test file (a kill's stub names its test before it exists)
        (TestFor _, []) -> [fileOf (blockTestId b)]
        _ -> [blockTestId b]
      -- a test of the newest text (no test or draft begun since) that finds
      -- the block moved on meanwhile (an older test landed first, the decider
      -- reordered) settles once more, on the block as it now stands
      -- (again each time another move lands first, while that still holds:
      -- each round needs a newer version, so it ends)
      -- a test block's test is its first new one, as the person named it
      named on = case (blockKind on, newTests on now) of
        (TestFor _, t : _) -> on {blockTestId = t}
        _ -> on
      settleTest result move = attempt b killed
        where
          attempt on k =
            settleOn s root k "test" result (Just now) move (named on) >>= \case
              Left moved -> do
                current <- M.lookup (blockFile b) <$> readIORef (ssBlocks s)
                seen <- M.lookup (blockFile b) <$> readIORef (ssBlockSeen s)
                case current of
                  Just (c, k')
                    | blockId c == blockId b, blockVersion c /= blockVersion on, not (isDrafting (blockState c)), seen == Just now -> attempt c k'
                  _ -> pure (Left moved)
              landed -> pure landed
  ran <- runOver s ws lang ids
  -- work cancelled (a newer edit's test superseded it) is no result: nothing settles
  stopped <- cancelled (ssCancel s)
  if stopped
    then do
      -- the text it tested is untested again (unless a newer test began):
      -- the job that cancelled it, finding that text, tests it
      atomicModifyIORef' (ssBlockSeen s) (\m -> (if M.lookup (blockFile b) m == Just now then M.insert (blockFile b) (blockOn b) m else m, ()))
      pure (Left "the test was cancelled; a newer one runs")
    else case ran of
    -- a runner that cannot start is a test that cannot run: never judged
    Left why -> settleTest ("cannot run: " <> why) (CannotRun why)
    Right run -> do
      -- its span runs from its first new line to its last; an import (a
      -- stub's, joined to the file's) or a blank line is no line of the block's
      let numbered = M.fromList (zip [1 ..] (T.splitOn "\n" now))
          ownLine l = not (T.null (T.strip l)) && not (isImportLine lang l)
          new = S.filter (maybe False ownLine . (`M.lookup` numbered)) (newLinesOf (blockBase b) now)
          sp = if applied || S.null new then blockSpan b else (S.findMin new, S.findMax new)
          (move, result) = case (trBroken run, trFailed run) of
            (Just why, _) -> (CannotRun why, "cannot run: " <> why)
            -- a new test is green for its block when it fails at its assertion
            _ | TestFor Nothing <- blockKind b -> either (\f -> (landed sp (Left f), "red")) (const (landed sp (Right ()), "green")) (redAtAssertion b now run)
            (_, (_, f) : _) -> (landed sp (Left f), "red")
            _ -> (landed sp (Right ()), "green")
          landed = if applied then Applied else Edited
      settleTest result move >>= \case
        Right b' | blockState b' == Green -> judgeIn s root project lang b' killed
        other -> pure other

-- | A new test's run as its test block takes it: Right when it fails at
-- its assertion (it tests what is not written yet), else why not.
redAtAssertion :: Block -> Text -> TestRun -> Either Failure ()
redAtAssertion b now run = case newTests b now of
  [] -> Left (Failure Nothing "name the new test and write its assertion" "")
  t : _ -> case lookup t (trFailed run) of
    Nothing -> Left (Failure Nothing "the test passes before any code exists: it does not test the new behaviour" "")
    Just f
      | fLine f == assertion t -> Right ()
      | otherwise -> Left f {fMessage = "the test fails before its assertion: " <> fMessage f}
  where
    numbered = zip [1 :: Int ..] (T.splitOn "\n" now)
    assertion t = case dropWhile (\(_, l) -> not (("def " <> testName t <> "(") `T.isPrefixOf` T.strip l)) numbered of
      _ : rest -> listToMaybe [i | (i, l) <- rest, "assert" `T.isPrefixOf` T.strip l]
      [] -> Nothing

-- | Judge a green block: the mutants of its span's new lines (the whole
-- span when none is new: a body begun on as it was), each run against the
-- test's file and judged by the tests that passed on it unmutated (a
-- sibling test still red, the next one being written, notices nothing).
blockJudge :: FilePath -> ServerState -> IO (ServerState, Either Text Block)
blockJudge path = withBlock path $ \s root project lang b killed -> judgeIn s root project lang b killed

-- | A test block is not mutated: green, it is judged. A code block's
-- survivors come in the decision model's order (what it already said; the rest as it
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
    verdicts <- case operators of
      -- nothing to mutate is nothing judged: accept stays closed
      [] -> pure (Left ("no mutation operators for " <> langName lang))
      _ -> mutantsOf (pSyntax (ssPorts s)) lang operators now scope >>= \case
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
    stopped <- cancelled (ssCancel s)
    if stopped
      then do
        -- green and unjudged: its text is untested again (unless a newer test
        -- began), so the job that cancelled it, finding that text, tests it
        atomicModifyIORef' (ssBlockSeen s) (\m -> (if M.lookup (blockFile b) m == Just (blockOn b) then M.insert (blockFile b) (blockBase b) m else m, ()))
        pure (Left "the judge was cancelled; a newer test runs")
      else case verdicts of
      -- a judge stands on the text its test passed on (blockOn as the test left it)
      Left why -> settle s root killedBefore "judge" ("cannot run: " <> why) (CannotRun why) b
      Right vs -> do
        let surviving = [m | (m, Survived) <- vs]
            killed = length vs - length surviving
        settle s root killed "judge" (tshow killed <> "/" <> tshow (length vs) <> " killed") (Judge surviving) b >>= \case
          -- in the decision model's order: what it already said now, the rest as it answers
          Right judged -> Right . fromMaybe judged <$> reorder s decider now testText False (blockFile judged)
          other -> pure other

-- | The file's judged block in the decision model's order, stored (and, PUSH, pushed)
-- when that changed it; each answer that lands later orders it again.
reorder :: ServerState -> Maybe (Decider IO) -> Text -> Text -> Bool -> FilePath -> IO (Maybe Block)
reorder s decider moduleText testText push file =
  M.lookup file <$> readIORef (ssBlocks s) >>= \case
    Just (b, killed) | Judged ms <- blockState b -> do
      ordered <- orderSurvivors s decider b moduleText testText (void (reorder s decider moduleText testText True file)) ms
      -- a new version: a move begun on the old order is stale, not its eraser
      let b' = b {blockState = Judged ordered, blockVersion = blockVersion b + 1}
      -- only over the block as it was (a move meanwhile wins)
      stored <- atomicModifyIORef' (ssBlocks s) $ \m -> case M.lookup file m of
        Just (now, _) | now == b, ordered /= ms -> (M.insert file (b', killed) m, True)
        _ -> (m, False)
      when (stored && push) (pushed s b' killed)
      pure (if stored then Just b' else Nothing)
    _ -> pure Nothing

-- | Accept a judged block whose survivors are all waived. A test block
-- (a kill's) accepted: its module's code block is judged again, and
-- pushed; a new test's: its function begins on it.
blockAccept :: FilePath -> ServerState -> IO (ServerState, Either Text (Block, [FileEdit]))
blockAccept path = withBlock path $ \s root project _ b killed ->
  awaitingVerdict s root project b "accept" >>= maybe (settle s root killed "accept" "accepted" Accept b) (pure . Left) >>= \case
    Right done | TestFor Nothing <- blockKind b -> beginOnNewTest s root project b >> pure (Right (done, []))
    Right done | TestFor _ <- blockKind b -> rejudgeFor s root project (blockFile b) >> pure (Right (done, []))
    other -> pure (fmap (,[]) other)

-- | Why a move on the block's verdict (waive, accept) must wait, if it
-- must: a test of a text other than the one the verdict stands on has
-- begun since (the person's edit, its verdict still to land).
-- An edit of the span not tested yet (its pause still to run) waits too.
awaitingVerdict :: ServerState -> FilePath -> Project -> Block -> Text -> IO (Maybe Text)
awaitingVerdict s root project b move = do
  seen <- M.lookup (blockFile b) <$> readIORef (ssBlockSeen s)
  untested <- spanEdited s root project b
  pure $
    if maybe False (/= blockOn b) seen || isJust untested
      then Just ("your edit is being tested; " <> move <> " once its verdict lands")
      else Nothing

-- | The block's file now, when the change since the text last tested
-- touches the block's span (an edit of the block's, to be tested).
spanEdited :: ServerState -> FilePath -> Project -> Block -> IO (Maybe Text)
spanEdited s root project b = do
  now <- fromMaybe "" <$> readText (prWorkspace project) (makeRelative root (blockFile b))
  seen <- fromMaybe (blockBase b) . M.lookup (blockFile b) <$> readIORef (ssBlockSeen s)
  pure (if now == seen || not (touches seen now (blockSpan b)) then Nothing else Just now)

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

-- | A new test accepted: the function it calls begins on it (red), as a
-- begin on that test would, and is pushed.
beginOnNewTest :: ServerState -> FilePath -> Project -> Block -> IO ()
beginOnNewTest s root project b = do
  now <- fromMaybe "" <$> readText (prWorkspace project) (makeRelative root (blockFile b))
  case [i | t : _ <- [newTests b now], (i, l) <- zip [1 ..] (T.splitOn "\n" now), ("def " <> testName t <> "(") `T.isPrefixOf` T.strip l] of
    defLine : _ ->
      blockBegin (const (pure ())) (blockFile b) defLine s >>= \case
        (_, Right code) -> pushed s code 0
        (_, Left _) -> pure ()
    [] -> pure ()

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
    Nothing -> pure (s0, Left "no magic circle is open in this file; begin one on a red test")
    Just (b, killed) ->
      projectFor (blockFile b) s0 >>= \case
        (s, Nothing) -> pure (s, Left (T.pack (blockFile b) <> " is in no Olifant project"))
        (s, Just (root, project)) -> case languageFor (catalogLanguages (prCatalog project)) (makeRelative root (blockFile b)) of
          Nothing -> pure (s, Left ("no language knows " <> T.pack (blockFile b)))
          Just lang -> (s,) <$> work s root project lang b killed

-- | Take a move on the block B as it was read: the block as it now stands
-- is kept (a finished one dropped, its version bumped), and the move
-- logged. Blocks are written from requests and background jobs at once
-- (a test, a draft, a judge each runs a while): the move lands only while
-- the stored block is still B (its number, and its version), one atomic
-- step; a result begun on a block gone or moved on meanwhile is stale,
-- logged as such and dropped.
settle :: ServerState -> FilePath -> Int -> Text -> Text -> Move -> Block -> IO (Either Text Block)
settle s root killed name result = settleOn s root killed name result Nothing

-- | 'settle', the state it lands established on the file's text ON (a
-- test's, a draft's, a waiver's); without one, on the text it was (a
-- judge's verdict stands on the text its test passed on).
settleOn :: ServerState -> FilePath -> Int -> Text -> Text -> Maybe Text -> Move -> Block -> IO (Either Text Block)
settleOn s root killed name result on move b = case step move b of
  Left why -> pure (Left why)
  Right stepped -> do
    let b' = maybe stepped (\t -> stepped {blockOn = t}) on
        stored = b' {blockVersion = blockVersion b + 1}
    landed <- atomicModifyIORef' (ssBlocks s) $ \m -> case M.lookup (blockFile b) m of
      Just (now, _)
        | blockId now == blockId b, blockVersion now == blockVersion b ->
            ((if pending stored then M.insert (blockFile stored) (stored, killed) else M.delete (blockFile stored)) m, True)
      _ -> (m, False)
    if landed
      then appendStep (ssPorts s) root stored name result >> pure (Right stored)
      else appendStep (ssPorts s) root b' name ("stale: " <> result) >> pure (Left "the magic circle moved on")

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

isDrafting :: BlockState -> Bool
isDrafting = \case
  Drafting _ -> True
  _ -> False

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
