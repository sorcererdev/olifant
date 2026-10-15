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
  , StubDraft
  , draftStub
  , blockUnapplied
  , blockDraftStopped
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
  , importedModules
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM, unless, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE, withExceptT)
import Data.Aeson (Value, object, (.=))
import Data.Char (isAlphaNum)
import Data.Functor ((<&>))
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (isSuffixOf, nub, sort, stripPrefix)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime)
import GHC.Clock (getMonotonicTime)
import Olifant.Anchor (locateContainer, locateContainers)
import Olifant.App (PlanOptions (..), Ports (..), Project (..), planOptions, planWith)
import Olifant.Block
import Olifant.Decide (Decider)
import Olifant.Catalog (catalogLanguages)
import Olifant.Fill (Filler (..), ModelFiller (..), Stance (..))
import Olifant.Generation (HoleStatus (..), Plan (..), PlannedHole (..), ProgressEvent (..), Step (..), describeConflict, describeGenerationError)
import Olifant.Generation.FillPhase (addImports, applyFills, assertionSpan, isImportLine, projectFiles, testsFailed)
import Olifant.Generator (GeneratorWith (..), Verify (..), replaceGenerator)
import Olifant.Template (Template)
import Olifant.Holes (HoleSite (..))
import Olifant.Language (Failure (..), Language (..), Syntax, TestId, TestRun (..), Verifier (..), describeCheckFailure, describeSyntaxFailure, languageFor, ranIn, runTestsIn, testsForFile)
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
blockStart = startFor Nothing

-- | 'blockStart', on a test whose function is known (SUBJECT: its name
-- and its def's line in the module) rather than found by its calls.
startFor :: Maybe (Text, Int) -> (ProgressEvent -> IO ()) -> FilePath -> Int -> ServerState -> IO (ServerState, Either Text (Block, Maybe (FilePath, Int, Text, [FileEdit])))
startFor subject progress path line s0 =
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
      here <- lift (functionAt s site line)
      untested <- case here of
        Just (n, _, _) | not (any (calledIn n) (maybe [] (\t -> map (`testBody` t) (testNames t)) testText0)) -> pure (Just n)
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
              -- the function known (a new test's): its def at its line, else the first of its name
              known = case subject of
                Just (fn, d) -> functionSpanAfter syntax lang fn d moduleText >>= maybe (functionSpanAfter syntax lang fn 1 moduleText) (pure . Just)
                Nothing -> pure Nothing
              spanOf tid = lift (known >>= maybe (underTest syntax lang testText tid moduleText) (pure . Just)) >>= maybe (throwE ("cannot find the function " <> testName tid <> " calls in " <> T.pack moduleRel)) pure
              atFunction = maybe (throwE ("no function at line " <> T.pack (show line))) (\(_, _, sp) -> pure sp) here
              chosen = listToMaybe (cursorTest ++ calling)
              -- what passed now (its test aside) must still pass for it to be green
              new tid sp st = do
                k <- number
                pure ((begin k file Code moduleText sp tid (Failure Nothing "" "")) {blockState = st, blockKeep = filter (/= tid) (trPassed run)})
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
  , -- | the line (1-based) of that def
    siteDefLine :: Maybe Int
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
      let above = defLineAbove line (if onTest then fromMaybe "" testText0 else moduleText)
      pure (Site root project lang moduleRel testRel onTest moduleText testText0 (snd <$> above) (fst <$> above))

-- | The function a cursor (1-based line) is in, in a module: the def at or
-- above it (its name, its line) while its body reaches the cursor, and that
-- body. The def is the one at that line, whatever else shares its name.
functionAt :: ServerState -> Site -> Int -> IO (Maybe (Text, Int, (Int, Int)))
functionAt s site line = case (siteOnTest site, siteAtCursor site, siteDefLine site) of
  (False, Just n, Just d) ->
    functionSpanAfter (pSyntax (ssPorts s)) (siteLang site) n d (siteModuleText site) <&> \case
      Just sp@(_, z) | line <= z -> Just (n, d, sp)
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
      imp = importOf lang (siteModuleRel site) (siteModuleText site) n (siteDefLine site)
      -- an existing file: the import joins its imports (unless it has it), the stub after
      imported = maybe base (\i -> if isJust testText0 then addImports lang [i] base else base) imp
      edits = [FileEdit testFile (fst <$> M.lookup testFile (ssDocs s)) False range new | imported /= base, let (range, new) = smallestChange base imported]
      at = at0 + length (T.splitOn "\n" imported) - length (T.splitOn "\n" base)
      body = maybe stub (\i -> i <> "\n" <> stub) (if isJust testText0 then Nothing else imp)
      defAt = at + length (takeWhile (not . ("def " `T.isPrefixOf`)) (T.splitOn "\n" body))
      -- named, the block's test is the stub's; else the file's until the person names it
      (tid, failure) = case name of
        Just t -> (T.pack testRel <> "::" <> t, Failure Nothing "" "")
        Nothing -> (T.pack testRel, Failure Nothing "a new test: write its name" "")
  k <- lift (atomicModifyIORef' (ssNextBlock s) (\i -> (i + 1, i)))
  let tb = (begin k testFile (TestFor Nothing) base (defAt, defAt + 2) tid failure) {blockSubject = (n,) <$> siteDefLine site}
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
importOf :: Language -> FilePath -> Text -> Text -> Maybe Int -> Maybe Text
importOf lang moduleRel moduleText fn defLine
  | langName lang /= "python" = Nothing
  | otherwise = Just ("from " <> dotted <> " import " <> owner)
  where
    dotted = T.intercalate "." (map T.pack (splitDirectories (dropExtension (fromMaybe moduleRel (stripPrefix "src/" moduleRel)))))
    ls = T.lines moduleText
    -- the def at its line (another class may define the name too), else the first of the name
    isDef l = ("def " <> fn <> "(") `T.isPrefixOf` T.strip l
    (above, at) = case defLine of
      Just d | maybe False isDef (listToMaybe (drop (d - 1) ls)) -> splitAt (d - 1) ls
      _ -> break isDef ls
    owner = case at of
      d : _ | T.isPrefixOf " " d || T.isPrefixOf "\t" d -> case [l | l <- reverse above, not (T.null (T.strip l)), not (" " `T.isPrefixOf` l || "\t" `T.isPrefixOf` l)] of
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
              draftOrRestore progress s root project lang b drafting killed filler Nothing

-- | A survivor's stub drafted at once (its test named from the mutant):
-- the work to run once the stub is answered, its test block Drafting
-- (logged) meanwhile; none without a model (the stub waits for draft).
type StubDraft = (ProgressEvent -> IO ()) -> IO (Either Text (Block, [FileEdit]))

-- | 'StubDraft' for the test block TB, its file STUBBED as the editor
-- leaves it once it expands the stub: the draft plans on that text and
-- lands on the editor's file once the stub is in it, unless the person
-- typed in it meanwhile (their text stands).
draftStub :: ServerState -> FilePath -> Project -> Language -> Block -> Text -> IO (Maybe StubDraft)
draftStub s root project lang tb stubbed =
  fillerFor True s project >>= \case
    Right (Just filler) | Right _ <- testDraftParts lang tb stubbed ->
      settle s root 0 "drafting" "the survivor's stub is drafted at once" StartDraft tb <&> \case
        -- planned on the file with the stub in it
        Right drafting ->
          let docs = M.insert (blockFile tb) (maybe 0 fst (M.lookup (blockFile tb) (ssDocs s)), stubbed) (ssDocs s)
              project' = project {prWorkspace = workspaceOf s {ssDocs = docs} root}
           in Just (\progress -> draftOrRestore progress s root project' lang tb drafting 0 filler (Just stubbed))
        Left _ -> Nothing
    _ -> pure Nothing

-- | The draft of B, stored as DRAFTING: landed, or, when nothing lands,
-- the block back in the state it began from (unless it moved on).
draftOrRestore :: (ProgressEvent -> IO ()) -> ServerState -> FilePath -> Project -> Language -> Block -> Block -> Int -> ModelFiller IO -> Maybe Text -> IO (Either Text (Block, [FileEdit]))
draftOrRestore progress s root project lang b drafting killed filler stubbed =
  runExceptT (draftIn progress s root project lang b drafting killed filler stubbed) >>= \case
    Right done -> pure (Right done)
    Left why -> do
      settle s root killed "drafting" ("no draft: " <> why) DraftFailed drafting >>= either (const (pure ())) (restored s root project lang killed)
      pure (Left why)

-- | The editor refused the edit of block NUMBER's latest draft (a
-- survivor's stub's, sent with no request asking): that draft is red for
-- it, and the file as the editor has it stands.
blockUnapplied :: FilePath -> Int -> ServerState -> IO (ServerState, Either Text Block)
blockUnapplied path number = withBlock path $ \s root project _ b killed ->
  if blockId b /= number
    then pure (Left "the magic circle moved on")
    else do
      now <- fromMaybe "" <$> readText (prWorkspace project) (makeRelative root (blockFile b))
      modifyIORef' (ssBlockSeen s) (M.insert (blockFile b) now)
      settleOn s root killed "draft" "red: the editor refused the edit" (Just now) (Unapplied "the editor refused the edit") b

-- | A draft in flight stopped from outside it (WHY: its progress was
-- cancelled once its stub was answered): the block back in the state it
-- began from, as a draft that fails puts it.
blockDraftStopped :: Text -> FilePath -> ServerState -> IO (ServerState, Either Text Block)
blockDraftStopped why path = withBlock path $ \s root project lang b killed -> case blockState b of
  Drafting _ ->
    settle s root killed "drafting" ("no draft: " <> why) DraftFailed b >>= \case
      Right back -> restored s root project lang killed back >> (maybe (Right back) (Right . fst) . M.lookup (blockFile b) <$> readIORef (ssBlocks s))
      Left moved -> pure (Left moved)
  _ -> pure (Left "no draft is in flight")

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

-- | The draft itself, the block (as it was, and DRAFTING, as stored) in
-- hand; on its file as it is, or STUBBED: as the editor will have it once
-- it expands a stub ('draftStub').
draftIn :: (ProgressEvent -> IO ()) -> ServerState -> FilePath -> Project -> Language -> Block -> Block -> Int -> ModelFiller IO -> Maybe Text -> ExceptT Text IO (Block, [FileEdit])
draftIn progress s root project lang b drafting killed filler stubbed = do
  let file = blockFile b
      rel = makeRelative root file
      ws = prWorkspace project
      started = fst <$> M.lookup file (ssDocs s)
  text <- maybe (lift (readText ws rel) >>= maybe (throwE ("cannot read " <> T.pack rel)) pure) pure stubbed
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
      (template, _) <- except (draftTemplate lang text (blockSpan b) (blockTestId b) failure (blockRedAt b) testText)
      -- its test's file runs whole: its test and those it keeps must pass
      pure (template, [("body", VerifyKeep (blockTestId b) (blockKeep b))], [(T.unpack (fileOf (blockTestId b)), testText)])
    TestFor killing -> do
      ((line, template), newTest, new, drafting') <- except (testDraftParts lang b text)
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
      -- a drafted assertion is verified as the arrangement is (each with the other in place)
      pure (template, [("arrange", verify)] ++ [("assertion", verify) | drafting'], [(m, t) | Just m <- [tested], Just t <- [moduleText]])
  -- and the project modules either one imports (ports, models), capped
  imported <- lift (importedContext ws lang (rel : map fst beside) (text : map snd beside))
  -- each fill the model offers, the last kept: a draft that fails its test
  -- is written as its last attempt, never as the placeholder
  -- (a fill the fill phase read one level up is kept as read)
  attempts <- lift (newIORef M.empty)
  let recording = filler {mfFiller = Filler (\req -> runFill (mfFiller filler) req >>= \r -> either (const (pure ())) (\fills -> modifyIORef' attempts (M.union fills)) r >> pure r)}
      progress' e = case e of
        Reindented hole f -> modifyIORef' attempts (M.insert hole f) >> progress e
        _ -> progress e
      gen = (replaceGenerator rel (1, length (T.lines text)) text template) {genVerify = verify}
      previous = [draftText d | d <- blockDrafts b, draftBy d /= "you"]
      context = beside ++ imported ++ [("previous drafts (rejected by the person)", T.intercalate "\n\n---\n\n" previous) | not (null previous)]
      options = planOptions {poFiller = Just recording, poProgress = progress', poCancel = ssCancel s, poContext = context, poStance = DraftStance}
  plan <- withExceptT describeGenerationError (ExceptT (planWith (ssPorts s) project options [Step gen M.empty]))
  unless (null (planConflicts plan)) (throwE (T.intercalate "; " (map describeConflict (planConflicts plan))))
  latest <- lift (maybe (pure started) (\live -> fmap fst <$> live file) (ssLiveDoc s))
  when (isNothing stubbed && latest /= started) (throwE "the file changed while drafting; your text stands")
  tried <- lift (readIORef attempts)
  -- (an attempt that would not parse is not written: the file keeps its
  -- text, the draft is recorded red with Python's reason)
  (planned, outcome, edits, unwritten) <- case planHoles plan of
    [] -> throwE "the draft made no hole"
    hs
      | all (\h -> case phStatus h of HoleFilled _ _ -> True; _ -> False) hs -> pure (M.findWithDefault text rel (planWrites plan), Right (), editsOfPlan s root plan, Nothing)
      | why : _ <- [w | h <- hs, HoleSkipped w <- [phStatus h]] -> throwE why
      | otherwise -> do
          -- each hole left unfilled is written as its last attempt
          let unfilled = [(h, w) | h <- hs, HoleUnfilled w <- [phStatus h]]
              why = maybe "" snd (listToMaybe unfilled)
              offered = [(phSite h, a) | (h, _) <- unfilled, Just a <- [M.lookup (holeName (phSite h)) tried]]
          when (null offered) (throwE ("the model gave no draft: " <> why))
          let langOf = languageFor (catalogLanguages (prCatalog project))
              written = M.findWithDefault text rel (applyFills langOf (planWrites plan) offered)
              (range, new) = smallestChange text written
          lift (unparseable s root drafting lang rel text written) >>= \case
            Just err -> pure (text, Left (Nothing, Failure Nothing (unparsedDraft <> ": " <> err) ""), [], Just written)
            Nothing -> do
              red <- lift (draftRedOn s ws lang rel b written why)
              pure (written, Left red, [FileEdit file started False range new | written /= text], Nothing)
  let shown = fromMaybe planned unwritten
      changed = changedSpan text shown (blockSpan b)
      grown = length (T.splitOn "\n" shown) - length (T.splitOn "\n" text)
      -- a test block keeps its whole test (its def to its assertion)
      (l1, l2) = case blockKind b of
        Code -> changed
        TestFor _ -> (min (fst changed) (fst (blockSpan b)), max (snd changed) (snd (blockSpan b) + grown))
      -- an unwritten draft leaves the span where it was
      sp = maybe (l1, l2) (const (blockSpan b)) unwritten
      drafted = T.intercalate "\n" (take (l2 - l1 + 1) (drop (l1 - 1) (T.splitOn "\n" shown)))
      -- a drafted assertion: its line, for the editor's cursor
      assertionAt = case blockKind b of
        TestFor _ | isNothing unwritten, Right (_, newTest, _, True) <- testDraftParts lang b text -> fst <$> assertionSpan (testName newTest) planned
        _ -> Nothing
  -- a stub's draft lands on the editor's file once the stub is in it, as
  -- the draft planned on it (its end aside: the editor's trailing lines)
  (onText, edits') <- case stubbed of
    Nothing -> pure (planned, edits)
    Just _ -> do
      (version, now) <- lift (stubInEditor s file (blockBase b) text) >>= maybe (throwE "the stub was not expanded in the editor; draft when it is") pure
      unless (T.stripEnd now == T.stripEnd text) (throwE "the file changed while drafting; your text stands")
      let landed = T.stripEnd planned <> T.drop (T.length (T.stripEnd now)) now
      -- the editor's text is the one seen: the draft applied is an edit, tested
      lift (modifyIORef' (ssBlockSeen s) (M.insert file now))
      pure (landed, [FileEdit file version False range new | landed /= now, let (range, new) = smallestChange now landed])
  let (result, move) = case outcome of
        Left (Just t, f) -> ("red: " <> noLongerPasses t, AtKept t (Drafted drafted (mfName filler) (Left f)))
        Left (Nothing, f) -> ("red", Drafted drafted (mfName filler) (Left f))
        Right () -> ("green", Drafted drafted (mfName filler) (Right ()))
  b' <- ExceptT (settleOn s root killed "draft" result (Just onText) move drafting {blockSpan = sp, blockAssertion = assertionAt})
  -- a green draft that changes nothing brings no edit back to be tested: judged now
  if null edits' && blockState b' == Green then (,[]) <$> ExceptT (judgeIn s root project lang b' killed) else pure (b', edits')

-- | The file as the editor has it once a stub is in it (its text no
-- longer BEFORE, the file without it), its version; waited for
-- ('ssStubWaitMs': the editor expands the stub as the answer arrives),
-- Nothing when it never came. No live documents: STUBBED is the file.
stubInEditor :: ServerState -> FilePath -> Text -> Text -> IO (Maybe (Maybe Int, Text))
stubInEditor s file before stubbed = case ssLiveDoc s of
  Nothing -> pure (Just (fst <$> M.lookup file (ssDocs s), stubbed))
  Just live -> go live (max 1 (ssStubWaitMs s `div` 50))
  where
    go live n =
      live file >>= \case
        Just (v, t) | T.stripEnd t /= T.stripEnd before -> pure (Just (Just v, t))
        _ | n <= 0 -> pure Nothing
        _ -> threadDelay 50000 >> go live (n - 1 :: Int)

-- | A code draft's red (WHY, the fill's) on the text it wrote: a test the
-- block keeps that fails there while its own test passes is that test's
-- red (the runner's line and message), as a test would find; else WHY.
-- Run only when the fill failed its tests ('testsFailed' begins WHY; the
-- reason may be cut before a kept test's id): a compile failure or clash
-- is WHY as it is.
draftRedOn :: ServerState -> Workspace IO -> Language -> FilePath -> Block -> Text -> Text -> IO (Maybe TestId, Failure)
draftRedOn s ws lang rel b written why
  | blockKind b /= Code || null (blockKeep b) || not (testsFailed `T.isPrefixOf` why) = pure fallback
  | otherwise = do
      let testFile = fileOf (blockTestId b)
      (_, files) <- projectFiles ws (M.empty :: Map FilePath Text) lang [T.unpack testFile]
      runTestsIn (pTester (ssPorts s) (ssCancel s)) lang [testFile] (M.insert rel written files) <&> \case
        Right run | Nothing <- trBroken run, Left kept@(Just _, _) <- outcomeOf b run -> kept
        _ -> fallback
  where
    fallback = (Nothing, Failure Nothing why "")

-- | The project modules a draft sees beside its file and the file beside it
-- (KNOWN, never repeated): the Python files their TEXTS import, an absolute
-- import's module (or a name imported from it that is a module) found as
-- @src/a/b/c.py@ or @src/a/b/c/__init__.py@, then what those import (one
-- level, so the cap cuts these first); in import order, at most
-- 'importedFiles' of them, each at most 'importedLines' lines (cut with a
-- line saying how many more) and 'importedTotal' in all. The standard
-- library and third-party packages are no file of the project: skipped.
importedContext :: Workspace IO -> Language -> [FilePath] -> [Text] -> IO [(FilePath, Text)]
importedContext ws lang known texts
  | langName lang /= "python" = pure []
  | otherwise = do
      direct <- resolved known texts
      -- one level further: what those import (a port's models)
      further <- resolved (known ++ map fst direct) (map snd direct)
      pure (capped importedTotal (take importedFiles (direct ++ further)))
  where
    resolved seen ts = do
      found <- forM (concatMap importedModules ts) $ \m ->
        let path = T.unpack (T.intercalate "/" (T.splitOn "." m))
         in firstFile ["src" </> path <> ".py", "src" </> path </> "__init__.py"]
      pure (dedupe [(p, t) | Just (p, t) <- found, p `notElem` seen, not (T.null (T.strip t))])
    firstFile = \case
      [] -> pure Nothing
      p : rest -> readText ws p >>= maybe (firstFile rest) (pure . Just . (p,))
    dedupe = foldr (\x acc -> x : filter ((/= fst x) . fst) acc) []
    capped _ [] = []
    capped budget ((p, t) : rest)
      | budget <= 0 = []
      | otherwise =
          let ls = T.lines t
              keep = minimum [length ls, importedLines, budget]
              cut
                | keep < length ls = T.intercalate "\n" (take keep ls ++ ["# \8230 (" <> T.pack (show (length ls - keep)) <> " more lines)"])
                | otherwise = t
           in (p, cut) : capped (budget - keep) rest

-- | The modules a Python file's absolute imports name, in order: @import
-- a.b, c@ names @a.b@ and @c@; @from a.b import x, y@ names @a.b@, then
-- @a.b.x@ and @a.b.y@ (a name may be a module). Relative imports are not
-- followed.
importedModules :: Text -> [Text]
importedModules t = concatMap named (map T.strip (T.lines t))
  where
    named l
      | Just rest <- T.stripPrefix "from " l
      , (m, names) <- T.breakOn " import " rest
      , not (T.null names)
      , not ("." `T.isPrefixOf` m) =
          m : [m <> "." <> n | n <- map nameOf (T.splitOn "," (T.drop (T.length " import ") names)), not (T.null n)]
      | Just rest <- T.stripPrefix "import " l = filter (not . T.null) (map nameOf (T.splitOn "," rest))
      | otherwise = []
    -- a name less its alias, brackets and comment
    nameOf = T.takeWhile (\c -> ident c || c == '.') . T.dropWhile (`elem` (" (" :: String)) . T.takeWhile (/= '#')

-- | A draft's imported modules: how many files, lines each, lines in all.
importedFiles, importedLines, importedTotal :: Int
importedFiles = 8
importedLines = 120
importedTotal = 600

-- | Why the draft's file (AFTER) would not parse, by Python's own parser
-- run through the verify port, when the file without it (BEFORE) does; or
-- Nothing: it parses, it is not Python, or the gate is off for this draft
-- (no Python could run, it failed without saying, or the file did not
-- parse before the draft either), which the step log records.
unparseable :: ServerState -> FilePath -> Block -> Language -> FilePath -> Text -> Text -> IO (Maybe Text)
unparseable s root b lang rel before after
  | langName lang /= "python" = pure Nothing
  | otherwise =
      parsing after >>= \case
        Right Nothing -> pure Nothing
        Right (Just err) ->
          parsing before >>= \case
            Right Nothing -> pure (Just err)
            Right (Just _) -> off "the file does not parse without the draft either"
            Left why -> off why
        Left why -> off why
  where
    off why = appendStep (ssPorts s) root b "parse" ("gate off: " <> why) >> pure Nothing
    -- Right: the parser's reason, if any; Left: why it could not say
    parsing t =
      runVerify (pVerifier (ssPorts s) (ssCancel s)) lang ["python3", "-I", "-c", parseScript, T.pack rel] (M.singleton rel t) <&> \case
        Left f -> Left (describeCheckFailure f)
        Right (ok, out)
          | err : _ <- [T.strip r | l <- T.lines out, Just r <- [T.stripPrefix parseSentinel (T.strip l)]] -> Right (Just err)
          | ok -> Right Nothing
          | otherwise -> Left (fromMaybe "python3 failed" (listToMaybe (filter (not . T.null) (map T.strip (T.lines out)))))
    -- only a syntax error is the sentinel; any other failure is no verdict
    parseScript = T.unlines
      [ "import ast, sys"
      , "try:"
      , "    ast.parse(open(sys.argv[1], encoding='utf-8').read(), sys.argv[1])"
      , "except SyntaxError as e:"
      , "    print(f'" <> parseSentinel <> " {type(e).__name__}: {e.msg} (line {e.lineno})')" ]

-- | The line the parse script begins a syntax error's reason with.
parseSentinel :: Text
parseSentinel = "OLIFANT_PARSE_ERROR:"

-- | A test block's stub as a draft needs it: its arrangement's line and
-- the template (its arrangement a hole, and its assertion too while it is
-- still the stub's `assert ...`), its new test and all its new tests, and
-- whether the assertion is drafted; or why it cannot be drafted yet.
testDraftParts :: Language -> Block -> Text -> Either Text ((Int, Template), TestId, [TestId], Bool)
testDraftParts lang b text = do
  let (s1, s2) = blockSpan b
      ls = T.splitOn "\n" text
      candidates = [s1 .. s2] ++ [1 .. length ls]
      new = newTests b text
  line <- maybe (Left "the test has no arrangement left to draft: its placeholder is gone") Right (listToMaybe [l | l <- candidates, Right _ <- [testDraftTemplate lang text l Nothing]])
  -- the stub names its test `test_name` until the person types over it
  newTest <- case new of
    t : _ | all ((/= stubName) . testName) new -> Right t
    _ -> Left "name the new test first: its name is still the stub's"
  -- the stub's assertion below its arrangement, in the test, left as it was
  let indentOf = T.length . T.takeWhile (`elem` [' ', '\t'])
      below = takeWhile (\(_, l) -> T.null (T.strip l) || indentOf l >= indentOf (ls !! (line - 1))) (drop line (zip [1 ..] ls))
      killing = case blockKind b of
        TestFor m -> m
        Code -> Nothing
      assertion = listToMaybe [(i, assertionInstruction (testName newTest) killing) | (i, l) <- below, T.strip l == stubAssertion]
  template <- testDraftTemplate lang text line assertion
  pure ((line, template), newTest, new, isJust assertion)

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
        -- a code block's: its test's file, whole (its test and those it
        -- keeps are judged from it; a kept id is never asked for: one gone
        -- from the file would stop the run)
        _ -> [fileOf (blockTestId b)]
      -- a test of the newest text (no test or draft begun since) that finds
      -- the block moved on meanwhile (an older test landed first, the decider
      -- reordered) settles once more, on the block as it now stands
      -- (again each time another move lands first, while that still holds:
      -- each round needs a newer version, so it ends)
      -- a test block's test is its first new one, as the person named it
      -- a code block's kept test the run did not have is gone: kept no more
      named present on = case (blockKind on, newTests on now) of
        (TestFor _, t : _) -> on {blockTestId = t}
        (Code, _) -> on {blockKeep = filter present (blockKeep on)}
        _ -> on
      settleTest present result move = attempt b killed
        where
          attempt on k =
            settleOn s root k "test" result (Just now) move (named present on) >>= \case
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
    Left why -> settleTest (const True) ("cannot run: " <> why) (CannotRun why)
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
            _ | Code <- blockKind b -> case outcomeOf b {blockKeep = filter present (blockKeep b)} run of
              Left (Just t, f) -> (AtKept t (landed sp (Left f)), "red: " <> noLongerPasses t)
              Left (Nothing, f) -> (landed sp (Left f), "red")
              Right () -> (landed sp (Right ()), "green")
            (_, (_, f) : _) -> (landed sp (Left f), "red")
            _ -> (landed sp (Right ()), "green")
          landed = if applied then Applied else Edited
          -- a broken run has nothing: it drops nothing
          present t = isJust (trBroken run) || ranIn run t
      settleTest present result move >>= \case
        Right b' | blockState b' == Green -> judgeIn s root project lang b' killed
        other -> pure other

-- | A new test's run as its test block takes it: Right when it fails at
-- its assertion (its first assert line to its end: an assertion over
-- several lines fails within it; it tests what is not written yet), else
-- why not.
redAtAssertion :: Block -> Text -> TestRun -> Either Failure ()
redAtAssertion b now run = case newTests b now of
  [] -> Left (Failure Nothing "name the new test" "")
  t : _ -> case lookup t (trFailed run) of
    Nothing -> Left (Failure Nothing "the test passes before any code exists: it does not test the new behaviour" "")
    Just f
      | Just l <- fLine f, Just (a, z) <- assertionSpan (testName t) now, l >= a, l <= z -> Right ()
      | otherwise -> Left f {fMessage = "the test fails before its assertion: " <> fMessage f}

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
    Right done | TestFor _ <- blockKind b -> do
      -- a kill's tests pass on the code (its draft was verified so): the code block keeps them
      now <- fromMaybe "" <$> readText (prWorkspace project) (makeRelative root (blockFile b))
      rejudgeFor s root project (blockFile b) (newTests b now)
      pure (Right (done, []))
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

-- | A new test accepted: its function (the one its stub was opened for,
-- else the one it calls) begins on it (red), as a begin on that test
-- would, and is pushed.
beginOnNewTest :: ServerState -> FilePath -> Project -> Block -> IO ()
beginOnNewTest s root project b = do
  now <- fromMaybe "" <$> readText (prWorkspace project) (makeRelative root (blockFile b))
  case [i | t : _ <- [newTests b now], (i, l) <- zip [1 ..] (T.splitOn "\n" now), ("def " <> testName t <> "(") `T.isPrefixOf` T.strip l] of
    defLine : _ ->
      startFor (blockSubject b) (const (pure ())) (blockFile b) defLine s >>= \case
        (_, Right (code, _)) -> pushed s code 0
        (_, Left _) -> pure ()
    [] -> pure ()

-- | A test file gained tests (NEW): its module's judged code block keeps
-- them and is judged again (its test is green: what was judged still
-- stands), and pushed.
rejudgeFor :: ServerState -> FilePath -> Project -> FilePath -> [TestId] -> IO ()
rejudgeFor s root project testFile new = do
  let rel = makeRelative root testFile
      langs = catalogLanguages (prCatalog project)
  found <- maybe (pure Nothing) (\tl -> moduleOf project tl rel) (languageFor langs rel)
  case found of
    Nothing -> pure ()
    Just moduleRel -> do
      let file = normalisePath (root </> moduleRel)
      M.lookup file <$> readIORef (ssBlocks s) >>= \case
        Just (cb, killed) | blockKind cb == Code, Judged _ <- blockState cb, Just lang <- languageFor langs moduleRel ->
          settle s root killed "rejudge" "its test file gained a test" Rejudge cb {blockKeep = nub (blockKeep cb ++ new)}
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
  object $
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
      ++ ["assertion_line" .= l | Just l <- [blockAssertion b]]
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
        "parse" -> "olifant"
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

-- | The body (1-based) of the function NAME whose def is on line D: the
-- first of the name's bodies that starts at or after it.
functionSpanAfter :: Syntax IO -> Language -> Text -> Int -> Text -> IO (Maybe (Int, Int))
functionSpanAfter syntax lang name d text = either (const Nothing) (\cs -> listToMaybe (sort [(a + 1, z + 1) | (a, z) <- cs, a + 1 >= d])) <$> locateContainers syntax lang ["function", name] text

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

-- | The def at or above a line (1-based): its line and name.
defLineAbove :: Int -> Text -> Maybe (Int, Text)
defLineAbove line text = listToMaybe [(i, n) | (i, Just n) <- reverse (take line (zip [1 ..] (map defName (T.lines text))))]

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
