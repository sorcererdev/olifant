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
  , compileClean
  , checkProgram
  , contextKey
  , checkScope
  , runCheckIn
  , runVerifyIn
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import Control.Monad (forM_)
import System.IO.Unsafe (unsafePerformIO)
import Data.Bits (xor)
import Data.Word (Word64)
import Numeric (showHex)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.List (isPrefixOf, nub, sort)
import Data.Set qualified as S
import Olifant.Workspace (pruned)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Olifant.Language.Adapter (CheckDef (..), Language (..), matchesGlob)
import Olifant.Language.Support (supportFor)
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
processVerifierCancellable token secs = Verifier $ \lang argv files -> case (argv, checkProgram argv) of
  ([], _) -> pure (Left (CheckFailed "empty verify command"))
  (_, Nothing) -> pure (Left (CheckFailed "the verify command runs no program"))
  (exe : args, Just program) ->
    (findExecutable (T.unpack program) >>= \case
      Nothing -> pure Nothing
      Just _ -> findExecutable (T.unpack exe)) >>= \case
      Nothing -> pure (Left (CheckToolMissing ("`" <> program <> "` is not installed (needed to verify)")))
      Just path -> withSystemTempDirectory "olifant-verify" $ \tmp0 -> do
        tmp <- canonicalizePath tmp0
        forM_ (M.toList files) $ \(p, t) -> do
          createDirectoryIfMissing True (takeDirectory (tmp </> p))
          BS.writeFile (tmp </> p) (TE.encodeUtf8 t)
        cache <- getXdgDirectory XdgCache ("olifant" </> "check" </> T.unpack (langName lang))
        createDirectoryIfMissing True cache
        let ctx = maybe "none" (`contextKey` files) (langCheck lang)
            expand a = T.unpack (T.replace "{= context =}" ctx (T.replace "{= cache =}" (T.pack cache) a))
        runBytesCancellable token (Just secs) path (map expand args) (Just tmp) "" <&> \case
          Left why -> Left (CheckFailed why)
          Right Cancelled -> Left (CheckFailed "cancelled")
          Right TimedOut -> Right (False, "the tests timed out (" <> T.pack (show secs) <> " s)")
          Right (Finished code out err) -> Right (code == ExitSuccess, TE.decodeUtf8With lenientDecode (out <> err))

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
      Right (code, out, err, dir) -> case supportFor (checkFormat check) of
        Nothing -> Left (CheckFailed ("unknown check format `" <> checkFormat check <> "`"))
        Just support -> case supportParse support (if supportReadsStdout support then out else err) of
          -- what the tool said is the reason (uv offline, no lockfile: python review I4)
          Left why
            | code /= ExitSuccess, not (BS.null err) -> Left (CheckFailed (why <> "; it said: " <> T.strip (T.take 1000 (TE.decodeUtf8With lenientDecode err))))
            | otherwise -> Left (CheckFailed why)
          Right (CheckReport [] [] [])
            | code /= ExitSuccess -> Left (CheckFailed ("the check failed without a report: " <> T.take 500 (TE.decodeUtf8With lenientDecode err)))
          Right report -> Right (maybe id ($ files) (supportSpans support) (relativeTo dir report))

-- | @{= context =}@ in a check command: a short hash of the check's context
-- files (pyproject.toml, uv.lock…), so each project and lockfile gets its own
-- environment in the cache, and a changed lock a fresh one.
contextKey :: CheckDef -> Map FilePath Text -> Text
contextKey check files =
  let ctx = [(p, t) | (p, t) <- M.toAscList files, any (`matchesGlob` p) (checkContext check)]
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
      Right (ExitFailure _, out, err, _) -> Left $ case supportFor (checkFormat check) of
        Just support
          | Right report <- supportParse support (if supportReadsStdout support then out else err)
          , not (null (reportBroken report)) ->
              T.unlines [T.pack f <> ": " <> t | (f, t) <- reportBroken report]
          -- only missing cases (pyright reports them as errors): say which
          | Right report <- supportParse support (if supportReadsStdout support then out else err)
          , not (null (reportMissing report)) ->
              T.unlines [T.pack (missingFile m) <> ":" <> T.pack (show (fst (missingCase m) + 1)) <> ": missing cases " <> T.intercalate ", " (missingPatterns m) | m <- reportMissing report]
        _ -> T.take 2000 (TE.decodeUtf8With lenientDecode err)

-- | The argument @{= files =}@ expands to the sources the adapter's globs
-- select; @{= cache =}@ (anywhere in an argument) to a per-language cache
-- directory that outlives the scratch copy (cargo's `--target-dir`).
runIn :: Cancel -> Language -> CheckDef -> Map FilePath Text -> IO (Either CheckFailure (ExitCode, ByteString, ByteString, FilePath))
runIn token lang check files = (if checkExclusive check then oneAtATime (langName lang <> ":" <> contextKey check files) else id) $ case (checkCommand check, checkProgram (checkCommand check)) of
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
        cache <- getXdgDirectory XdgCache ("olifant" </> "check" </> T.unpack (langName lang))
        createDirectoryIfMissing True cache
        let sources = sort [p | p <- M.keys files, any (`matchesGlob` p) (langFiles lang)]
            expand a
              | T.strip a == "{= files =}" = sources
              | otherwise = [T.unpack (T.replace "{= context =}" (contextKey check files) (T.replace "{= cache =}" (T.pack cache) a))]
        ran <- runBytesCancellable token Nothing path (concatMap expand args) (Just tmp) ""
        pure $ case ran of
          Left why -> Left (CheckFailed why)
          Right (Finished code out err) -> Right (code, out, err, tmp)
          Right _ -> Left (CheckFailed "cancelled")

-- | One run at a time per key in this process, its scratch copy gone before
-- the next starts: cargo's fingerprints follow the source files of the last
-- build, so a check that starts while the previous one's copy is still on
-- disk finds its crates fresh and reports nothing of its own.
oneAtATime :: Text -> IO a -> IO a
oneAtATime key act = do
  lock <- modifyMVar checkLocks $ \m -> case M.lookup key m of
    Just l -> pure (m, l)
    Nothing -> newMVar () <&> \l -> (M.insert key l m, l)
  withMVar lock (const act)

{-# NOINLINE checkLocks #-}
checkLocks :: MVar (Map Text (MVar ()))
checkLocks = unsafePerformIO (newMVar M.empty)

-- | GHC already prints relative paths; makeRelative leaves those unchanged.
relativeTo :: FilePath -> CheckReport -> CheckReport
relativeTo dir (CheckReport missingPs broken unplaced) =
  CheckReport [rel m | m <- missingPs] [(makeRelative dir p, t) | (p, t) <- broken] [(rel m, f) | (m, f) <- unplaced]
  where
    rel m = m {missingFile = makeRelative dir (missingFile m)}

-- | What a check of the @judged@ files runs over, from the project's
-- @paths@: its scope, the judged files' package (the nearest folder holding
-- one of the check's `root` markers, or the outermost; the project, "",
-- when there is none or the files sit in different packages), and the
-- language's files and the check's context files under it (globs read from
-- the scope), as project paths.
checkScope :: Language -> [FilePath] -> [FilePath] -> (FilePath, [FilePath])
checkScope lang paths judged = (scope, [p | p <- paths, Just rel <- [inside scope p], not (any (`elem` pruned) (splitDirectories p)), any (`matchesGlob` rel) globs])
  where
    check = langCheck lang
    globs = langFiles lang ++ maybe [] checkContext check
    present = S.fromList paths
    scope = case nub (map packageOf judged) of
      [one] -> one
      _ -> ""
    packageOf f = case [d | d <- ancestors (takeDirectory f), any (\m -> joined d (T.unpack m) `S.member` present) (maybe [] checkRoot check)] of
      [] -> ""
      nearest : further -> if maybe False checkRootOutermost check then foldl (\_ d -> d) nearest further else nearest
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

into :: FilePath -> Map FilePath Text -> Map FilePath Text
into "" files = files
into scope files = M.fromList [(rel, t) | (p, t) <- M.toList files, Just rel <- [inside scope p]]

outOf :: FilePath -> CheckReport -> CheckReport
outOf "" report = report
outOf scope (CheckReport missingPs broken unplaced) =
  CheckReport [up m | m <- missingPs] [(scope </> p, t) | (p, t) <- broken] [(up m, f) | (m, f) <- unplaced]
  where
    up m = m {missingFile = scope </> missingFile m}
