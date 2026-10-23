-- | Internal to Olifant.Server: a conjure (`olifant/conjure`) of the unit at a
-- line, or of the selected lines: on a function, method or class, its body
-- to pass its tests; on a test, its body from its name. The tests run
-- before and after; the conjure lands whatever the after-run says. Nothing
-- is stored.
module Olifant.Server.Conjure
  ( ConjureAnswer (..)
  , conjureAt
  ) where

import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE, withExceptT)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import Olifant.App (PlanOptions (..), Ports (..), Project (..), planOptions, planWith)
import Olifant.Catalog (catalogLanguages)
import Olifant.Fill (Filler (..), ModelFiller (..), Stance (..))
import Olifant.Generation (HoleStatus (..), Plan (..), PlannedHole (..), ProgressEvent (..), Step (..), describeConflict, describeGenerationError)
import Olifant.TestName (helpedIn)
import Olifant.Generation.FillPhase (applyFills, assertionSpan, projectFiles)
import Olifant.Generator (GeneratorWith (..), Verify (..), replaceGenerator)
import Olifant.Holes (HoleSite (..))
import Olifant.Language (Failure (..), TestRun (..), languageFor, runTestsIn)
import Olifant.Server.Actions (fillerFor)
import Olifant.Server.Site (Site (..), appendStepLine, importedContext, runOver, siteAt, siteTestFile, testBody, tshow)
import Olifant.Server.Edits (FileEdit (..), smallestChange)
import Olifant.Server.State (ServerState (..))
import Olifant.Server.Tests (TestsAnswer (..), TestsScope (..), testAt)
import Olifant.Server.Unit (Unit (..), unitAt)
import Olifant.Template (Template, describeTemplateError, parseTemplate)
import Olifant.Workspace (normalisePath)
import System.FilePath ((</>))

-- | A conjure's answer: the conjured file (absolute), the unit, the edit
-- (none when the conjure changed nothing), the summary, whether the
-- after-run is green, and the after-run.
data ConjureAnswer = ConjureAnswer
  { daFile :: FilePath
  , daUnit :: Text
  , daEdit :: Maybe FileEdit
  , daSummary :: Text
  , daGreen :: Bool
  , daTests :: TestsAnswer
  }
  deriving stock (Eq, Show)

-- | The conjure of the unit at LINE (1-based) of PATH, or of the lines
-- SELECTED (1-based, inclusive). Left is one sentence on why there is none.
conjureAt :: (ProgressEvent -> IO ()) -> FilePath -> Int -> Maybe (Int, Int) -> ServerState -> IO (ServerState, Either Text ConjureAnswer)
conjureAt progress path line selected s0 =
  siteAt path line s0 >>= \case
    (s, Left why) -> pure (s, Left why)
    (s, Right site) -> fmap (s,) . runExceptT $ do
      let lang = siteLang site
          project = siteProject site
          ws = prWorkspace project
          onTest = siteOnTest site
          rel = if onTest then siteTestRel site else siteModuleRel site
          file = normalisePath (siteRoot site </> rel)
          started = fst <$> M.lookup file (ssDocs s)
      text <- if onTest then maybe (throwE ("cannot read " <> T.pack rel)) pure (siteTestText site) else pure (siteModuleText site)
      found <- lift (unitAt (pSyntax (ssPorts s)) lang text line)
      (unit, span') <- case (selected, found) of
        (Just sp, _) -> pure (either (const "selection") unitName found, sp)
        (Nothing, Left why) -> throwE (why <> ": put the cursor in one to conjure it")
        (Nothing, Right u) -> maybe (throwE (unitName u <> " is on one line: select the lines to conjure")) (pure . (unitName u,)) (unitBody u)
      when (fst span' < 1 || snd span' < fst span' || snd span' > length (T.splitOn "\n" text)) (throwE "the selection is not in the file")
      when (onTest && isNothing selected && not ("test" `T.isPrefixOf` unit)) (throwE (unit <> " is no test: conjure a test here, or a function from its module"))
      -- a module needs its test file (on a test, the file is the test file)
      testText <- case siteTestText site of
        Just t -> pure t
        Nothing -> throwE ("no test file for this module yet: " <> T.pack (siteTestRel site) <> "; stub one first (:Durandal stub)")
      filler <- lift (fillerFor True s project) >>= either throwE (maybe (throwE noModel) pure)
      -- the before-run: the test file as the editor has it
      before <- lift (either (TestRun [] [] . Just) id <$> runOver s ws lang [T.pack (siteTestRel site)])
      (template, verify, context) <-
        if onTest
          then do
            -- (a helper in a test file was refused above)
            name <- maybe (throwE ("no test at line " <> tshow line)) pure (testAt text (fst span'))
            except (testConjure text span' (T.pack (siteTestRel site) <> "::" <> name) (siteModuleRel site) (siteModuleText site))
          else except (functionConjure text span' unit (siteTestRel site) testText before)
      imported <- lift (importedContext ws lang (rel : map fst context) (text : map snd context))
      written <- fill progress s project rel text template verify (context ++ imported) filler
      latest <- lift (maybe (pure started) (\live -> fmap fst <$> live file) (ssLiveDoc s))
      when (latest /= started) (throwE "the file changed while conjuring; your text stands")
      -- the after-run: the test file with the conjure in place
      (_, files) <- lift (projectFiles ws (M.empty :: Map FilePath Text) lang [siteTestRel site])
      start <- lift getMonotonicTime
      after <- lift (either (TestRun [] [] . Just) id <$> runTestsIn (pTester (ssPorts s) (ssCancel s)) lang [T.pack (siteTestRel site)] (M.insert rel written files))
      end <- lift getMonotonicTime
      let summary = if onTest then testSummary tid before after written else functionSummary after
          tid = T.pack (siteTestRel site) <> "::" <> fromMaybe unit (testAt written (fst span'))
          green = isNothing (trBroken after) && null (trFailed after)
          edit = [FileEdit file started False range new | written /= text, let (range, new) = smallestChange text written]
          tests = TestsAnswer (siteTestFile site) ScopeFile Nothing (siteRoot site) after (end - start)
      lift (appendStepLine (ssPorts s) (siteRoot site) ["conjure", "person", T.pack rel <> "::" <> unit, "conjured by " <> mfName filler <> " \183 " <> summary])
      pure (ConjureAnswer file unit (listToMaybe edit) summary green tests)

noModel :: Text
noModel = "no model is configured: set [model] in .olifant/config.toml to conjure"

-- | A function's conjure: the file with the span's lines one `block` hole
-- called `body` (at the span's first line's indentation), verified by its
-- test file run whole (`verify = "tests"`: every test there must pass);
-- the context: the test file, the failures with their tests' sources, the
-- passing tests to keep, and the body on screen when it is no stub
-- (rejected by the person).
functionConjure :: Text -> (Int, Int) -> Text -> FilePath -> Text -> TestRun -> Either Text (Template, [(Text, Verify [Template])], [(FilePath, Text)])
functionConjure text (l1, l2) unit testRel testText before = do
  let ls = T.splitOn "\n" text
      body = take (l2 - l1 + 1) (drop (l1 - 1) ls)
      short = snd . T.breakOnEnd "::"
      failed = trFailed before
      instruction = oneLine $ case (trBroken before, failed) of
        (Just why, _) -> "the tests could not run (" <> firstLine why <> "); write the body of " <> unit <> " its tests describe"
        (_, []) -> "the tests pass; write the body of " <> unit <> " they describe, keeping them passing"
        _ -> "make the failing tests pass (" <> T.intercalate ", " [short t <> ": " <> firstLine (fMessage f) | (t, f) <- failed] <> ") and keep the file's other tests passing; the tests and their messages are in the context; write the least code that does it"
      failing = T.intercalate "\n\n" [t <> "\n" <> fMessage f <> "\n" <> T.unlines (testSource (short t)) | (t, f) <- failed]
      testSource n = let base = fst (T.breakOn "[" n) in ("def " <> base <> "(…):") : testBody base testText
      context =
        [(testRel, testText)]
          ++ [("failing now (make these pass)", failing) | not (null failed)]
          ++ [("passing now (keep these passing)", T.intercalate "\n" (trPassed before)) | not (null (trPassed before))]
          ++ [("the body on screen, rejected by the person (write another)", T.intercalate "\n" (dedent body)) | not (isStub body)]
  template <- holeOver ls (l1, l2) instruction
  pure (template, [("body", VerifyTests)], context)

-- | A test's conjure: the test file with the span's lines one `block` hole
-- called `body`, worded from the test's name; verified red at its
-- assertion or passing (`VerifyRedAt`, passing allowed: an error before
-- the assertion is asked again); the context: the module under test in
-- full, the notes in the stub's body, and the body on screen when it is no
-- stub (rejected by the person). The siblings are in the file itself.
-- TID is the test's full id (`path::name`).
testConjure :: Text -> (Int, Int) -> Text -> FilePath -> Text -> Either Text (Template, [(Text, Verify [Template])], [(FilePath, Text)])
testConjure text (l1, l2) tid moduleRel moduleText = do
  let ls = T.splitOn "\n" text
      body = take (l2 - l1 + 1) (drop (l1 - 1) ls)
      short = snd (T.breakOnEnd "::" tid)
      words' = T.replace "_" " " (fromMaybe short (T.stripPrefix "test_" short))
      notes = [T.strip l | l <- body, "#" `T.isPrefixOf` T.strip l]
      instruction = helpedIn ls (oneLine ("write the body of the test \"" <> words' <> "\": arrange, act, and assert what its name says, one behaviour, against the module in the context; it should fail at its assertion until the code does it"))
      context =
        [(moduleRel, moduleText)]
          ++ [("notes in the test's body", T.intercalate "\n" notes) | not (null notes)]
          ++ [("the body on screen, rejected by the person (write another)", T.intercalate "\n" (dedent body)) | not (isStub body)]
  template <- holeOver ls (l1, l2) instruction
  pure (template, [("body", VerifyRedAt tid l1 True)], context)

-- | A test's after-run in a few words: its own outcome (at its assertion,
-- before it, passing, not run), then each sibling that passed before and
-- fails now.
testSummary :: Text -> TestRun -> TestRun -> Text -> Text
testSummary tid before after written = own <> T.concat ["; red: " <> short t <> " no longer passes" | t <- broke]
  where
    short = snd . T.breakOnEnd "::"
    isOwn t = t == tid || (tid <> "[") `T.isPrefixOf` t
    broke = [t | (t, _) <- trFailed after, not (isOwn t), t `elem` trPassed before]
    own = case (trBroken after, [f | (t, f) <- trFailed after, isOwn t]) of
      (Just why, _) -> "red: could not run \183 " <> firstLine why
      (_, f : _)
        | Just l <- fLine f, Just (a, z) <- assertionSpan (short tid) written, l >= a, l <= z -> "red at the assertion \183 " <> firstLine (fMessage f)
        | otherwise -> "red: error before the assertion \183 " <> firstLine (fMessage f)
      _
        | any isOwn (trPassed after) -> "green: already passes"
        | otherwise -> "red: " <> short tid <> " did not run"

-- | LS with the lines L1..L2 one `block` hole called `body`, at L1's
-- indentation, worded INSTRUCTION.
holeOver :: [Text] -> (Int, Int) -> Text -> Either Text Template
holeOver ls (l1, l2) instruction =
  either (Left . describeTemplateError) Right (parseTemplate (T.intercalate "\n" (take (l1 - 1) ls ++ [indent <> "{= ? body: block | " <> instruction <> " =}"] ++ drop l2 ls)))
  where
    indent = T.takeWhile (`elem` [' ', '\t']) (maybe "" id (listToMaybe (drop (l1 - 1) ls)))

-- | A body that is only a stub: no line but `...`, `pass`, `assert ...`,
-- a `raise NotImplementedError…`, an Olifant placeholder, or a comment.
isStub :: [Text] -> Bool
isStub = all stubLine . filter (not . T.null) . map T.strip
  where
    stubLine l = l `elem` ["...", "pass", "assert ..."] || "raise NotImplementedError" `T.isPrefixOf` l || "olifant:" `T.isInfixOf` l || "#" `T.isPrefixOf` l

-- | Lines less their common indentation.
dedent :: [Text] -> [Text]
dedent ls = map (T.drop n) ls
  where
    n = minimum (maxBound : [T.length (T.takeWhile (== ' ') l) | l <- ls, not (T.null (T.strip l))])

firstLine :: Text -> Text
firstLine = T.strip . T.takeWhile (/= '\n')

-- | One line, no marker's end in it (an instruction).
oneLine :: Text -> Text
oneLine = T.replace "=}" "= }" . T.intercalate "; " . filter (not . T.null) . map T.strip . T.lines

-- | A function's after-run in a few words.
functionSummary :: TestRun -> Text
functionSummary run = case (trBroken run, trFailed run) of
  (Just why, _) -> "red: could not run \183 " <> firstLine why
  (_, []) -> "green"
  (_, (t, f) : _) -> "red: " <> snd (T.breakOnEnd "::" t) <> " \183 " <> firstLine (fMessage f)

-- | TEMPLATE planned over REL (TEXT) with VERIFY and CONTEXT: the file as
-- the fill writes it, or, when no attempt passed its verify, as the last
-- attempt writes it (a conjure always lands). Left when the model gave no
-- attempt at all, or the plan could not be made.
fill :: (ProgressEvent -> IO ()) -> ServerState -> Project -> FilePath -> Text -> Template -> [(Text, Verify [Template])] -> [(FilePath, Text)] -> ModelFiller IO -> ExceptT Text IO Text
fill progress s project rel text template verify context filler = do
  attempts <- lift (newIORef M.empty)
  prior <- lift (newIORef M.empty)
  let recording = filler {mfFiller = Filler (\req -> runFill (mfFiller filler) req >>= \r -> either (const (pure ())) answered r >> pure r)}
      answered fills = readIORef attempts >>= writeIORef prior >> modifyIORef' attempts (M.union fills)
      progress' e = case e of
        Reindented hole f -> modifyIORef' attempts (M.insert hole f) >> progress e
        CutToHole hole _ f -> modifyIORef' attempts (M.insert hole f) >> progress e
        -- a whole file answered is never written: the hole's earlier attempt is
        WholeFile hole -> readIORef prior >>= \was -> modifyIORef' attempts (M.alter (const (M.lookup hole was)) hole) >> progress e
        _ -> progress e
      gen = (replaceGenerator rel (1, length (T.lines text)) text template) {genVerify = verify}
      options = planOptions {poFiller = Just recording, poProgress = progress', poCancel = ssCancel s, poContext = context, poStance = ConjureStance}
  plan <- withExceptT describeGenerationError (ExceptT (planWith (ssPorts s) project options [Step gen M.empty]))
  unless (null (planConflicts plan)) (throwE (T.intercalate "; " (map describeConflict (planConflicts plan))))
  tried <- lift (readIORef attempts)
  case planHoles plan of
    [] -> throwE "the conjure made no hole"
    hs
      | all (\h -> case phStatus h of HoleFilled _ _ -> True; _ -> False) hs -> pure (M.findWithDefault text rel (planWrites plan))
      | why : _ <- [w | h <- hs, HoleSkipped w <- [phStatus h]] -> throwE why
      | otherwise -> do
          let unfilled = [(h, w) | h <- hs, HoleUnfilled w <- [phStatus h]]
              why = maybe "" snd (listToMaybe unfilled)
              offered = [(phSite h, a) | (h, _) <- unfilled, Just a <- [M.lookup (holeName (phSite h)) tried]]
          when (null offered) (throwE ("the model gave no conjure: " <> why))
          let langOf = languageFor (catalogLanguages (prCatalog project))
          pure (M.findWithDefault text rel (applyFills langOf (planWrites plan) offered))
