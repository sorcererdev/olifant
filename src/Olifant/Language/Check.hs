-- | The check port (spec §5.6): compile files, report `case`s missing constructors.
module Olifant.Language.Check
  ( Checker (..)
  , MissingPatterns (..)
  , CheckReport (..)
  , CheckFailure (..)
  , noChecker
  , describeCheckFailure
  , processChecker
  , processCheckerWith
  , Verifier (..)
  , noVerifier
  , processVerifier
  , processVerifierWith
  , processVerifierCancellable
  , processRunner
  , runTestsIn
  , compileClean
  , checkProgram
  , contextKey
  , checkScope
  , runCheckIn
  , runVerifyIn
  , convertReport
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, putMVar, tryTakeMVar)
import Control.Exception (bracket)
import Control.Monad (forM_)
import System.IO.Unsafe (unsafePerformIO)
import Data.Bits (xor)
import Data.Word (Word64)
import Numeric (showHex)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Aeson (encode, object, (.=))
import Data.Functor ((<&>))
import Data.List (isPrefixOf, nub, sort)
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as S
import Olifant.Workspace (pruned)
import Olifant.Provider (decodeArgs, runProvider)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Olifant.Language.Adapter (CheckDef (..), ImportsDef (..), Language (..), Select (..), TestDef (..), matchesGlob, moduleOf, regexName, supportOf)
import Olifant.Template (Value (VText), parseTemplate, render)
import Olifant.Language.TestFormats (Reported (..), attribute, fromReported, parseJunit, parseLines, parseTap, parseTestJson)
import Data.Containers.ListUtils (nubOrd)
import Olifant.Language.Test (Runner (..), TestId, TestRun (..), parsePytest)
import Olifant.Process (Cancel, Outcome (..), neverCancel, runBytesCancellable)
import System.Directory (XdgDirectory (..), canonicalizePath, createDirectoryIfMissing, findExecutable, getXdgDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, splitDirectories, takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Olifant.Language.Diagnostics (CheckReport (..), CompilerSupport (..), MissingPatterns (..))

data CheckFailure = CheckToolMissing Text | CheckFailed Text
  deriving stock (Eq, Show)

newtype Checker m = Checker {runCheck :: Language -> Map FilePath Text -> m (Either CheckFailure CheckReport)}

-- | A hole's `verify` command run over the project's files (a scratch copy,
-- like a check): passed, and its output.
newtype Verifier m = Verifier {runVerify :: Language -> [Text] -> Map FilePath Text -> m (Either CheckFailure (Bool, Text))}

noVerifier :: Applicative m => Verifier m
noVerifier = Verifier (\_ _ _ -> pure (Left (CheckFailed "no verifier is configured")))

-- | 'processVerifierWith' a 120 s limit.
processVerifier :: Verifier IO
processVerifier = processVerifierWith 120

-- | The command in a scratch copy of the files (as a check runs, with
-- @{= cache =}@ and @{= context =}@), stopped after @secs@ seconds: exit 0
-- passes; the output (stdout then stderr) is what it said.
processVerifierWith :: Int -> Verifier IO
processVerifierWith = processVerifierCancellable neverCancel

-- | 'processVerifierWith', killed when the token is tripped ("cancelled").
processVerifierCancellable :: Cancel -> Int -> Verifier IO
processVerifierCancellable token secs = Verifier $ \lang argv files ->
  scratchRun token secs lang "olifant-verify" argv (maybe "none" (`contextKey` files) (langCheck lang)) Nothing [] (const Nothing) files <&> \case
    Left ScratchEmpty -> Left (CheckFailed "empty verify command")
    Left ScratchNoProgram -> Left (CheckFailed "the verify command runs no program")
    Left (ScratchMissing program) -> Left (CheckToolMissing ("`" <> program <> "` is not installed (needed to verify)"))
    Left (ScratchFailed why) -> Left (CheckFailed why)
    Right Cancelled -> Left (CheckFailed "cancelled")
    Right TimedOut -> Right (False, "the tests timed out (" <> T.pack (show secs) <> " s)")
    Right (Finished code out err) -> Right (code == ExitSuccess, TE.decodeUtf8With lenientDecode (out <> err))

-- | Why 'scratchRun' ran nothing.
data ScratchFailure = ScratchEmpty | ScratchNoProgram | ScratchMissing Text | ScratchFailed Text

-- | A command run in a scratch copy of the files, as a check runs: its
-- program (past a leading @env@) must be installed; @{= cache =}@ (the
-- slot's, when it holds one) and @{= context =}@ (the given key) are
-- expanded in its arguments, and @special@ may replace a whole argument by
-- several. Stopped after @secs@. With a @convert@ command, its stdout is
-- the outcome's stdout (and stderr is empty).
scratchRun :: Cancel -> Int -> Language -> String -> [Text] -> Text -> Maybe Int -> [Text] -> (Text -> Maybe [String]) -> Map FilePath Text -> IO (Either ScratchFailure Outcome)
scratchRun token secs lang prefix command ctx slot convert special files = case (command, checkProgram command) of
  ([], _) -> pure (Left ScratchEmpty)
  (_, Nothing) -> pure (Left ScratchNoProgram)
  (exe : args, Just program) ->
    (findExecutable (T.unpack program) >>= \case
      Nothing -> pure Nothing
      Just _ -> findExecutable (T.unpack exe)) >>= \case
      Nothing -> pure (Left (ScratchMissing program))
      Just path -> withSystemTempDirectory prefix $ \tmp0 -> do
        tmp <- canonicalizePath tmp0
        forM_ (M.toList files) $ \(p, t) -> do
          createDirectoryIfMissing True (takeDirectory (tmp </> p))
          BS.writeFile (tmp </> p) (TE.encodeUtf8 t)
        cache <- getXdgDirectory XdgCache ("olifant" </> "check" </> T.unpack (langName lang) </> maybe "" (\n -> "slot-" <> show n) slot)
        createDirectoryIfMissing True cache
        let expand a = fromMaybe [T.unpack (T.replace "{= context =}" ctx (T.replace "{= cache =}" (T.pack cache) a))] (special (T.strip a))
        runBytesCancellable token (Just secs) path (concatMap expand args) (Just tmp) "" >>= \case
          Left why -> pure (Left (ScratchFailed why))
          Right (Finished code out err)
            | not (null convert) ->
                convertReport convert tmp code out err <&> \case
                  Left why -> Left (ScratchFailed ("the convert command failed: " <> why))
                  Right report -> Right (Finished code report "")
          Right outcome -> pure (Right outcome)

-- | The language's test command over a scratch copy of the files (as a
-- verify runs, with @{= cache =}@ and @{= context =}@; the one argument
-- @{= tests =}@ becomes the test ids), stopped after @secs@ seconds, its
-- report read by the test format.
processRunner :: Cancel -> Int -> Runner IO
processRunner token secs = Runner $ \lang ids files -> case langTest lang of
  Nothing -> pure (Left (langName lang <> " has no [test] in its adapter"))
  Just td -> do
    let limit = fromMaybe secs (testTimeout td)
        ctx = contextKeyOf (testContext td) files
        -- a runner with a build cache (cabal's dist): a slot of its own
        around
          | testExclusive td = inSlot slotsPerKey (langName lang <> ":test:" <> ctx)
          | otherwise = ($ Nothing)
        requested = requestedFiles lang td ids files
        -- an argument twice (a flag each test repeats: `--exact`) once
        byRule = maybe (map T.unpack ids) (\sel -> map T.unpack (nub (concatMap (selected lang td files sel) ids))) (testSelect td)
        run args =
          around $ \slot ->
            scratchRun token limit lang "olifant-test" (testCommand td) ctx slot (testConvert td) (\a -> if a == "{= tests =}" then Just args else Nothing) files <&> \case
              Left ScratchEmpty -> Left "empty test command"
              Left ScratchNoProgram -> Left "the test command runs no program"
              Left (ScratchMissing program) -> Left ("`" <> program <> "` is not installed (needed to run the tests)")
              Left (ScratchFailed why) -> Left why
              Right Cancelled -> Left "cancelled"
              Right TimedOut -> Right (TestRun [] [] (Just ("the tests timed out (" <> T.pack (show limit) <> " s)")))
              Right (Finished code out err) -> Right (readTestReport td requested code out err)
    -- a selection provider answers the arguments instead of [test.select]
    case testSelectProvider td of
      Nothing -> run byRule
      Just p ->
        runProvider p (object ["tests" .= ids]) >>= \answer -> case answer >>= decodeArgs of
          Left why -> pure (Left ("the test selection provider: " <> why))
          Right args -> run (map T.unpack args)

-- | The file of a test id (the id itself for a file's).
fileOfId :: TestId -> FilePath
fileOfId = T.unpack . fst . T.breakOn "::"

-- | The files a run asks for, each with its group: its `file_group` (from
-- its text), else its module by the language's `[imports] module_of` (none
-- when that is empty: a Rust crate's root).
requestedFiles :: Language -> TestDef -> [TestId] -> Map FilePath Text -> [(FilePath, Maybe Text)]
requestedFiles lang td ids files = nubOrd [(f, groupOf f) | f <- map fileOfId ids]
  where
    groupOf f = case testFileGroup td of
      Just re -> M.lookup f files >>= regexName re
      Nothing -> langImportsDef lang >>= imModuleOf >>= \mo -> let m = moduleOf mo f in if T.null m then Nothing else Just m

-- | One id as the runner's arguments by `[test.select]`: a file's by
-- 'selFile', a test's by 'selTest', templates with @{= group =}@ the file's
-- group (empty with none) and @{= name =}@ its segments below the file
-- joined by 'selJoin'.
selected :: Language -> TestDef -> Map FilePath Text -> Select -> TestId -> [Text]
selected lang td files sel i = map fill (if T.null rest then selFile sel else selTest sel)
  where
    (_, rest) = T.breakOn "::" i
    name = T.intercalate (selJoin sel) (T.splitOn "::" (T.drop 2 rest))
    grp = fromMaybe "" (lookup (fileOfId i) (requestedFiles lang td [i] files) >>= id)
    fill a = either (const (T.replace "{= id =}" i (T.replace "{= name =}" name (T.replace "{= group =}" grp a)))) id (parseTemplate a >>= render (M.fromList [("group", VText grp), ("name", VText name), ("id", VText i)]))

-- | A run's report read by the `[test] format`, from stdout then stderr
-- (olifant-json, and a converted report, from stdout); a report a parser
-- cannot read is a broken run with its reason.
readTestReport :: TestDef -> [(FilePath, Maybe Text)] -> ExitCode -> ByteString -> ByteString -> TestRun
readTestReport td requested code out err = case testFormat td of
  "pytest" -> parsePytest code said
  "lines" -> maybe (broken "[test] format \"lines\" has no [test.lines]") (\lf -> fromReported requested code said (parseLines lf said)) (testLines td)
  "tap" -> either broken (fromReported requested code said) (parseTap said)
  "junit-xml" -> either broken (fromReported requested code said) (parseJunit said)
  _ -> either broken refine (parseTestJson out)
  where
    said = TE.decodeUtf8With lenientDecode (out <> err)
    broken why = TestRun [] [] (Just why)
    -- an id without a file is a test's name: attributed like a report's
    own i
      | "::" `T.isInfixOf` i = i
      | otherwise = attribute requested (Reported Nothing [i] Nothing)
    -- a report that says nothing follows the other formats' rules; its own `broken` wins
    refine run
      | isJust (trBroken run) = run
      | null (trPassed run), null (trFailed run) = fromReported requested code said []
      | otherwise = attributed run
    attributed run = run {trPassed = map own (trPassed run), trFailed = [(own i, f) | (i, f) <- trFailed run]}

noChecker :: Applicative m => Checker m
noChecker = Checker (\_ _ -> pure (Left (CheckFailed "no checker is configured")))

describeCheckFailure :: CheckFailure -> Text
describeCheckFailure = \case
  CheckToolMissing why -> why
  CheckFailed why -> why

-- | The process adapter: write the files into a scratch directory, run the
-- adapter's check command there, and parse its report by format.
processChecker :: Checker IO
processChecker = processCheckerWith neverCancel

-- | 'processChecker', its compiler killed when the token is tripped
-- ("cancelled").
processCheckerWith :: Cancel -> Checker IO
processCheckerWith token = Checker $ \lang files -> case langCheck lang of
  Nothing -> pure (Left (CheckFailed (langName lang <> " has no [check] in its adapter")))
  Just check ->
    runIn token lang check files <&> \case
      Left f -> Left f
      Right (code, out, err, dir) -> case supportOf check of
        Nothing -> Left (CheckFailed ("unknown check format `" <> checkFormat check <> "`"))
        Just support -> case supportParse support (reportInput check support out err) of
          -- what the tool said is the reason (uv offline, no lockfile: python review I4)
          Left why
            | code /= ExitSuccess, not (BS.null err) -> Left (CheckFailed (why <> "; it said: " <> T.strip (T.take 1000 (TE.decodeUtf8With lenientDecode err))))
            | otherwise -> Left (CheckFailed why)
          -- (a converted report is the verdict, but a tool that failed and left nothing is not clean)
          Right (CheckReport [] [] [])
            | code /= ExitSuccess -> Left (CheckFailed ("the check failed without a report: " <> T.take 500 (TE.decodeUtf8With lenientDecode err)))
          Right report -> Right (maybe id ($ files) (supportSpans support) (relativeTo dir report))

-- | The bytes a support parses: an errorformat reads stdout then stderr; a
-- converted report is stdout; otherwise the stream the support names.
reportInput :: CheckDef -> CompilerSupport -> ByteString -> ByteString -> ByteString
reportInput check support out err
  | not (null (checkConvert check)) = out
  | Just _ <- checkErrorFormat check = out <> "\n" <> err
  | supportReadsStdout support = out
  | otherwise = err

-- | A `convert` command, run in the scratch directory with
-- @{"v": 1, "exit": N, "stdout": "…", "stderr": "…"}@ on stdin and a 60 s limit:
-- what it prints is the report; a non-zero exit is Left with what it said
-- on stderr.
convertReport :: [Text] -> FilePath -> ExitCode -> ByteString -> ByteString -> IO (Either Text ByteString)
convertReport argv dir code out err = case argv of
  [] -> pure (Left "empty convert command")
  exe : args ->
    findExecutable (T.unpack exe) >>= \case
      Nothing -> pure (Left ("`" <> exe <> "` is not installed (needed to convert the report)"))
      Just path ->
        runBytesCancellable neverCancel (Just 60) path (map T.unpack args) (Just dir) (BL.toStrict (encode input)) <&> \case
          Left why -> Left why
          Right (Finished ExitSuccess converted _) -> Right converted
          Right (Finished _ _ said) -> Left (nonEmpty (T.strip (T.take 1000 (TE.decodeUtf8With lenientDecode said))))
          Right TimedOut -> Left "it timed out (60 s)"
          Right Cancelled -> Left "cancelled"
  where
    input = object ["v" .= (1 :: Int), "exit" .= exitNumber code, "stdout" .= TE.decodeUtf8With lenientDecode out, "stderr" .= TE.decodeUtf8With lenientDecode err]
    exitNumber = \case
      ExitSuccess -> 0
      ExitFailure n -> n
    nonEmpty t = if T.null t then "it exited non-zero and said nothing" else t

-- | @{= context =}@ in a check command: a short hash of the check's context
-- files (pyproject.toml, uv.lock…), so each project and lockfile gets its own
-- environment in the cache, and a changed lock a fresh one.
contextKey :: CheckDef -> Map FilePath Text -> Text
contextKey check = contextKeyOf (checkContext check)

-- | 'contextKey' of the context globs themselves (a [test] has its own).
contextKeyOf :: [Text] -> Map FilePath Text -> Text
contextKeyOf globs files =
  let ctx = [(p, t) | (p, t) <- M.toAscList files, any (`matchesGlob` p) globs]
      bytes = BS.unpack (TE.encodeUtf8 (T.concat [T.pack p <> "\0" <> t <> "\0" | (p, t) <- ctx]))
      fnv = foldl (\h b -> (h `xor` fromIntegral b) * 1099511628211) (14695981039346656037 :: Word64) bytes
   in T.pack (take 12 (showHex fnv ""))

-- | The program a check command runs: past a leading `env` and its
-- `NAME=value` settings (`env UV_PROJECT_ENVIRONMENT=… uv run pyright` runs uv).
checkProgram :: [Text] -> Maybe Text
checkProgram = \case
  "env" : rest -> case dropWhile ("=" `T.isInfixOf`) rest of
    p : _ -> Just p
    [] -> Nothing
  p : _ -> Just p
  [] -> Nothing

-- | Whether the files compile cleanly (exit 0); pack tests use it to prove
-- generated output builds.
compileClean :: Language -> Map FilePath Text -> IO (Either Text ())
compileClean lang files = case langCheck lang of
  Nothing -> pure (Left "no [check] command")
  Just check ->
    runIn neverCancel lang check files <&> \case
      Left f -> Left (describeCheckFailure f)
      Right (ExitSuccess, _, _, _) -> Right ()
      -- The support's broken files when it reads any (cargo's JSON), else stderr.
      Right (ExitFailure _, out, err, _) -> Left $ case supportOf check of
        Just support
          | Right report <- supportParse support (reportInput check support out err)
          , not (null (reportBroken report)) ->
              T.unlines [T.pack f <> ": " <> t | (f, t) <- reportBroken report]
          -- only missing cases (pyright reports them as errors): say which
          | Right report <- supportParse support (reportInput check support out err)
          , not (null (reportMissing report)) ->
              T.unlines [T.pack (missingFile m) <> ":" <> T.pack (show (fst (missingCase m) + 1)) <> ": missing cases " <> T.intercalate ", " (missingPatterns m) | m <- reportMissing report]
        _ -> T.take 2000 (TE.decodeUtf8With lenientDecode err)

-- | The argument @{= files =}@ expands to the sources the adapter's globs
-- select; @{= cache =}@ (anywhere in an argument) to a per-language cache
-- directory that outlives the scratch copy (cargo's `--target-dir`).
runIn :: Cancel -> Language -> CheckDef -> Map FilePath Text -> IO (Either CheckFailure (ExitCode, ByteString, ByteString, FilePath))
runIn token lang check files = around $ \slot -> case (checkCommand check, checkProgram (checkCommand check)) of
  ([], _) -> pure (Left (CheckFailed "empty check command"))
  (_, Nothing) -> pure (Left (CheckFailed "the check command runs no program"))
  (exe : args, Just program) ->
    -- the program the command runs (`env VAR=… uv …` runs uv) must be installed
    (findExecutable (T.unpack program) >>= \case
      Nothing -> pure Nothing
      Just _ -> findExecutable (T.unpack exe)) >>= \case
      Nothing -> pure (Left (CheckToolMissing ("`" <> program <> "` is not installed (needed to check " <> langName lang <> ")")))
      Just path -> withSystemTempDirectory "olifant-check" $ \tmp0 -> do
        tmp <- canonicalizePath tmp0
        forM_ (M.toList files) $ \(p, t) -> do
          createDirectoryIfMissing True (takeDirectory (tmp </> p))
          BS.writeFile (tmp </> p) (TE.encodeUtf8 t)
        -- an exclusive check's cache is its slot's: no other check uses it meanwhile
        cache <- getXdgDirectory XdgCache ("olifant" </> "check" </> T.unpack (langName lang) </> maybe "" (\n -> "slot-" <> show n) slot)
        createDirectoryIfMissing True cache
        let sources = sort [p | p <- M.keys files, any (`matchesGlob` p) (langFiles lang)]
            expand a
              | T.strip a == "{= files =}" = sources
              | otherwise = [T.unpack (T.replace "{= context =}" (contextKey check files) (T.replace "{= cache =}" (T.pack cache) a))]
        ran <- runBytesCancellable token Nothing path (concatMap expand args) (Just tmp) ""
        case ran of
          Left why -> pure (Left (CheckFailed why))
          Right (Finished code out err)
            | null (checkConvert check) -> pure (Right (code, out, err, tmp))
            -- the report is what the convert command printed; the tool's
            -- exit stays for compileClean
            | otherwise ->
                convertReport (checkConvert check) tmp code out err <&> \case
                  Left why -> Left (CheckFailed ("the check's convert command failed: " <> why))
                  Right report -> Right (code, report, err, tmp)
          Right _ -> pure (Left (CheckFailed "cancelled"))

  where
    -- how a check runs beside others: in a slot of its workspace (a cache of
    -- its own: exclusive), one at a time for its language (not concurrent),
    -- or freely
    around
      | checkExclusive check = inSlot slotsPerKey (langName lang <> ":" <> contextKey check files)
      | not (checkConcurrent check) = \act -> inSlot 1 ("language:" <> langName lang) (const (act Nothing))
      | otherwise = ($ Nothing)

-- | Run in one of a key's slots (a cache of its own), the slot held until
-- the run and its scratch copy are done: cargo's fingerprints follow the
-- source files of a cache's last build, so a check sharing a cache with a
-- running one, or starting before the last one's copy is gone, finds its
-- crates fresh and reports nothing of its own. Up to 'slotsPerKey' checks of
-- one workspace run at once in this process, each slot's cache kept warm.
inSlot :: Int -> Text -> (Maybe Int -> IO a) -> IO a
inSlot n key act = do
  slots <- modifyMVar checkSlots $ \m -> case M.lookup key m of
    Just ss -> pure (m, ss)
    Nothing -> traverse (const (newMVar ())) [1 .. n] <&> \ss -> (M.insert key ss m, ss)
  let free = \case
        [] -> threadDelay 20000 >> free (zip [0 ..] slots)
        (i, s) : rest -> tryTakeMVar s >>= maybe (free rest) (const (pure (i, s)))
  bracket (free (zip [0 ..] slots)) (\(_, s) -> putMVar s ()) (act . Just . fst)

slotsPerKey :: Int
slotsPerKey = 4

{-# NOINLINE checkSlots #-}
checkSlots :: MVar (Map Text [MVar ()])
checkSlots = unsafePerformIO (newMVar M.empty)

-- | GHC already prints relative paths; makeRelative leaves those unchanged.
relativeTo :: FilePath -> CheckReport -> CheckReport
relativeTo dir (Report missingPs broken unplaced ls) =
  Report [rel m | m <- missingPs] [(makeRelative dir p, t) | (p, t) <- broken] [(rel m, f) | (m, f) <- unplaced] [((makeRelative dir p, t), l) | ((p, t), l) <- ls]
  where
    rel m = m {missingFile = makeRelative dir (missingFile m)}

-- | What a check of the @judged@ files runs over, from the project's
-- @paths@: its scope, the judged files' package (the nearest folder holding
-- one of the check's `root` markers, or the outermost; the project, "",
-- when there is none or the files sit in different packages), and the
-- language's files, the check's context files and the tests' `context` and
-- `files` under it (globs read from
-- the scope), as project paths.
checkScope :: Language -> [FilePath] -> [FilePath] -> (FilePath, [FilePath])
checkScope lang paths judged = (scope, [p | p <- paths, Just rel <- [inside scope p], not (any (`elem` pruned) (splitDirectories p)), any (`matchesGlob` rel) globs])
  where
    check = langCheck lang
    globs = langFiles lang ++ maybe [] checkContext check ++ maybe [] (\t -> testContext t ++ testFiles t) (langTest lang)
    scope = packageScope (maybe [] checkRoot check) (maybe False checkRootOutermost check) paths judged

-- | The package of the judged files: the nearest folder (or the outermost)
-- holding one of the root markers; the project, "", when there is none or
-- the files sit in different packages.
packageScope :: [Text] -> Bool -> [FilePath] -> [FilePath] -> FilePath
packageScope roots outermost paths judged = case nub (map packageOf judged) of
  [one] -> one
  _ -> ""
  where
    present = S.fromList paths
    packageOf f = case [d | d <- ancestors (takeDirectory f), any (\m -> joined d (T.unpack m) `S.member` present) roots] of
      [] -> ""
      nearest : further -> if outermost then foldl (\_ d -> d) nearest further else nearest
    -- nearest first, the project ("") last
    ancestors d
      | d `elem` [".", "", "/"] = [""]
      | otherwise = d : ancestors (takeDirectory d)
    joined d m = if null d then m else d </> m

-- | A path under a scope, relative to it.
inside :: FilePath -> FilePath -> Maybe FilePath
inside "" p = Just p
inside scope p
  | (scope <> "/") `isPrefixOf` p = Just (drop (length scope + 1) p)
  | otherwise = Nothing

-- | The check run inside @scope@ over files keyed by project path: the
-- checker sees paths relative to the scope; its report says project paths.
runCheckIn :: Monad m => Checker m -> Language -> FilePath -> Map FilePath Text -> m (Either CheckFailure CheckReport)
runCheckIn checker lang scope files = fmap (outOf scope) <$> runCheck checker lang (into scope files)

-- | A verify command run inside @scope@: its arguments that name project
-- files under the scope name them from the scope.
runVerifyIn :: Verifier m -> Language -> FilePath -> [Text] -> Map FilePath Text -> m (Either CheckFailure (Bool, Text))
runVerifyIn verifier lang scope argv files = runVerify verifier lang (map fromScope argv) (into scope files)
  where
    fromScope a
      | null scope = a
      | otherwise = maybe a T.pack (inside scope (T.unpack a))

-- | The tests (ids of the project) run inside the judged files' package
-- (the nearest folder with one of the language's @[test] root@ markers): the
-- runner sees ids and files from that scope; the ids it reports say project
-- paths again.
runTestsIn :: Monad m => Runner m -> Language -> [TestId] -> Map FilePath Text -> m (Either Text TestRun)
runTestsIn runner lang ids files = fmap (upTo scope) <$> runTests runner lang (map fromScope ids) (into scope files)
  where
    scope = packageScope (maybe [] testRoot (langTest lang)) False (M.keys files) (map (T.unpack . T.takeWhile (/= ':')) ids)
    fromScope a
      | null scope = a
      | otherwise = maybe a T.pack (inside scope (T.unpack a))
    upTo "" run = run
    upTo sc run = run {trPassed = map up (trPassed run), trFailed = [(up i, f) | (i, f) <- trFailed run]}
      where up i = T.pack sc <> "/" <> i

into :: FilePath -> Map FilePath Text -> Map FilePath Text
into "" files = files
into scope files = M.fromList [(rel, t) | (p, t) <- M.toList files, Just rel <- [inside scope p]]

outOf :: FilePath -> CheckReport -> CheckReport
outOf "" report = report
outOf scope (Report missingPs broken unplaced ls) =
  Report [up m | m <- missingPs] [(scope </> p, t) | (p, t) <- broken] [(up m, f) | (m, f) <- unplaced] [((scope </> p, t), l) | ((p, t), l) <- ls]
  where
    up m = m {missingFile = scope </> missingFile m}
