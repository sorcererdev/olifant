-- | The check port (spec §5.6): compile files, report `case`s missing constructors.
module Olifant.Language.Check
  ( Checker (..)
  , MissingPatterns (..)
  , CheckReport (..)
  , CheckFailure (..)
  , noChecker
  , describeCheckFailure
  , processChecker
  , compileClean
  , checkProgram
  , contextKey
  ) where

import Control.Monad (forM_)
import Data.Bits (xor)
import Data.Word (Word64)
import Numeric (showHex)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Olifant.Language.Adapter (CheckDef (..), Language (..), matchesGlob)
import Olifant.Language.Support (supportFor)
import Olifant.Process (runBytes)
import System.Directory (XdgDirectory (..), canonicalizePath, createDirectoryIfMissing, findExecutable, getXdgDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Olifant.Language.Diagnostics (CheckReport (..), CompilerSupport (..), MissingPatterns (..))

data CheckFailure = CheckToolMissing Text | CheckFailed Text
  deriving stock (Eq, Show)

newtype Checker m = Checker {runCheck :: Language -> Map FilePath Text -> m (Either CheckFailure CheckReport)}

noChecker :: Applicative m => Checker m
noChecker = Checker (\_ _ -> pure (Left (CheckFailed "no checker is configured")))

describeCheckFailure :: CheckFailure -> Text
describeCheckFailure = \case
  CheckToolMissing why -> why
  CheckFailed why -> why

-- | The process adapter: write the files into a scratch directory, run the
-- adapter's check command there, and parse its report by format.
processChecker :: Checker IO
processChecker = Checker $ \lang files -> case langCheck lang of
  Nothing -> pure (Left (CheckFailed (langName lang <> " has no [check] in its adapter")))
  Just check ->
    runIn lang check files <&> \case
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
    runIn lang check files <&> \case
      Left f -> Left (describeCheckFailure f)
      Right (ExitSuccess, _, _, _) -> Right ()
      -- The support's broken files when it reads any (cargo's JSON), else stderr.
      Right (ExitFailure _, out, err, _) -> Left $ case supportFor (checkFormat check) of
        Just support
          | Right report <- supportParse support (if supportReadsStdout support then out else err)
          , not (null (reportBroken report)) ->
              T.unlines [T.pack f <> ": " <> t | (f, t) <- reportBroken report]
        _ -> T.take 2000 (TE.decodeUtf8With lenientDecode err)

-- | The argument @{= files =}@ expands to the sources the adapter's globs
-- select; @{= cache =}@ (anywhere in an argument) to a per-language cache
-- directory that outlives the scratch copy (cargo's `--target-dir`).
runIn :: Language -> CheckDef -> Map FilePath Text -> IO (Either CheckFailure (ExitCode, ByteString, ByteString, FilePath))
runIn lang check files = case (checkCommand check, checkProgram (checkCommand check)) of
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
        ran <- runBytes path (concatMap expand args) (Just tmp) ""
        pure $ case ran of
          Left why -> Left (CheckFailed why)
          Right (code, out, err) -> Right (code, out, err, tmp)

-- | GHC already prints relative paths; makeRelative leaves those unchanged.
relativeTo :: FilePath -> CheckReport -> CheckReport
relativeTo dir (CheckReport missingPs broken unplaced) =
  CheckReport [rel m | m <- missingPs] [(makeRelative dir p, t) | (p, t) <- broken] [(rel m, f) | (m, f) <- unplaced]
  where
    rel m = m {missingFile = makeRelative dir (missingFile m)}
