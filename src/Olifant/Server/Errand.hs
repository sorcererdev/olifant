-- | The runner for plan errands: one `claude -p` call, its JSON reply read
-- into text, seconds and cost. Nothing here throws; every failure is a Left
-- with a short reason.
module Olifant.Server.Errand (Verb (..), RunResult (..), runnerArgs, runClaude) where

import Data.Aeson (Value (..), decodeStrict)
import Data.ByteString (ByteString)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Olifant.Catalog (PlansConfig (..))
import Olifant.Process (Cancel, Outcome (..), runBytesCancellable)
import System.Exit (ExitCode (..))

data Verb = Ask | Validate | Do deriving (Eq, Show)

data RunResult = RunResult {rrText :: Text, rrSeconds :: Double, rrCost :: Maybe Double}
  deriving (Eq, Show)

-- | The flags after the runner's name; @testTools@ are the project's test commands (for 'Do').
runnerArgs :: PlansConfig -> Verb -> [Text] -> [String]
runnerArgs cfg verb testTools =
  ["-p", "--output-format", "json", "--model", T.unpack (pcModel cfg)] ++ case verb of
    Do -> ["--permission-mode", "acceptEdits", "--allowedTools", T.unpack (T.intercalate "," (["Read", "Grep", "Glob", "Edit", "Write"] ++ testTools ++ pcDoTools cfg))]
    _ -> ["--allowedTools", "Read,Grep,Glob"]

runClaude :: Cancel -> PlansConfig -> Verb -> [Text] -> FilePath -> Text -> IO (Either Text RunResult)
runClaude token cfg verb testTools root prompt =
  runBytesCancellable token (Just (pcTimeout cfg)) "env" (["-u", "CLAUDECODE", pcRunner cfg] ++ runnerArgs cfg verb testTools) (Just root) (TE.encodeUtf8 prompt) >>= \case
    Left why -> pure (Left why)
    Right Cancelled -> pure (Left "cancelled")
    Right TimedOut -> pure (Left ("timed out after " <> T.pack (show (pcTimeout cfg)) <> " s"))
    Right (Finished (ExitFailure n) out err) ->
      pure (Left (T.pack (pcRunner cfg) <> " exited " <> T.pack (show n) <> ": " <> first120 out err))
    Right (Finished ExitSuccess out err) -> pure $ case decodeStrict out of
      Just (Object o) -> fromObject o
      _ -> Left (first120 out err)

fromObject :: KM.KeyMap Value -> Either Text RunResult
fromObject o = case KM.lookup "result" o of
  _ | KM.lookup "is_error" o == Just (Bool True) -> Left (T.take 120 (maybe "the runner reported an error" asText (KM.lookup "result" o)))
  Just (String t) -> Right (RunResult t (maybe 0 (/ 1000) (num "duration_ms")) (num "total_cost_usd"))
  _ -> Left (T.take 120 (T.pack (show (Object o))))
  where
    num k = case KM.lookup (K.fromText k) o of
      Just (Number n) -> Just (toRealFloat n)
      _ -> Nothing
    asText = \case
      String t -> t
      v -> T.pack (show v)

first120 :: ByteString -> ByteString -> Text
first120 out err = T.take 120 (T.strip (TE.decodeUtf8Lenient (out <> err)))
