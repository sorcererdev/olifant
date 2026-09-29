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
  ) where

import Control.Monad (forM_)
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
import Olifant.Language.Elm (parseElmReport)
import Olifant.Language.Process (runBytes)
import System.Directory (canonicalizePath, createDirectoryIfMissing, findExecutable)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Olifant.Language.Diagnostics (CheckReport (..), MissingPatterns (..))

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
      Right (code, err, dir) -> case checkFormat check of
        "elm-json" -> case parseElmReport err of
          Left why -> Left (CheckFailed why)
          Right (CheckReport [] [])
            | code /= ExitSuccess -> Left (CheckFailed ("the check failed without a report: " <> T.take 500 (TE.decodeUtf8With lenientDecode err)))
          Right report -> Right (relativeTo dir report)
        other -> Left (CheckFailed ("unknown check format `" <> other <> "`"))

-- | Whether the files compile cleanly (exit 0); pack tests use it to prove
-- generated output builds.
compileClean :: Language -> Map FilePath Text -> IO (Either Text ())
compileClean lang files = case langCheck lang of
  Nothing -> pure (Left "no [check] command")
  Just check ->
    runIn lang check files <&> \case
      Left f -> Left (describeCheckFailure f)
      Right (ExitSuccess, _, _) -> Right ()
      Right (ExitFailure _, err, _) -> Left (T.take 2000 (TE.decodeUtf8With lenientDecode err))

-- | The argument @{= files =}@ expands to the sources the adapter's globs select.
runIn :: Language -> CheckDef -> Map FilePath Text -> IO (Either CheckFailure (ExitCode, ByteString, FilePath))
runIn lang check files = case checkCommand check of
  [] -> pure (Left (CheckFailed "empty check command"))
  (exe : args) ->
    findExecutable (T.unpack exe) >>= \case
      Nothing -> pure (Left (CheckToolMissing ("`" <> exe <> "` is not installed (needed to check " <> langName lang <> ")")))
      Just path -> withSystemTempDirectory "olifant-check" $ \tmp0 -> do
        tmp <- canonicalizePath tmp0
        forM_ (M.toList files) $ \(p, t) -> do
          createDirectoryIfMissing True (takeDirectory (tmp </> p))
          BS.writeFile (tmp </> p) (TE.encodeUtf8 t)
        let sources = sort [p | p <- M.keys files, any (`matchesGlob` p) (langFiles lang)]
            expand a = if T.strip a == "{= files =}" then sources else [T.unpack a]
        ran <- runBytes path (concatMap expand args) (Just tmp) ""
        pure $ case ran of
          Left why -> Left (CheckFailed why)
          Right (code, _, err) -> Right (code, err, tmp)

relativeTo :: FilePath -> CheckReport -> CheckReport
relativeTo dir (CheckReport missing broken) =
  CheckReport [m {missingFile = makeRelative dir (missingFile m)} | m <- missing] [(makeRelative dir p, t) | (p, t) <- broken]
