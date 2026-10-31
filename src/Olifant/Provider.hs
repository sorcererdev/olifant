-- | The slot protocol: a program that answers one of a language's slots
-- (units, test units, an anchor, a test selection) in place of the rule.
--
-- A request is a JSON object on stdin, always carrying @"v": 1@; the answer
-- is JSON on stdout. Lines are 1-based and inclusive, columns 1-based and
-- counted in characters. A provider runs from the project root, is stopped
-- after 10 s, and is pure by contract: the same request gives the same
-- answer, which is what lets 'providerPure' answer from pure code.
module Olifant.Provider
  ( Provider (..),
    runProvider,
    providerPure,
    runProviderMemo,
    textRequest,
    providedUnits,
    lineCount,
    providerTimeout,
    providerCoolOff,
    withVersion,
    UnitAnswer (..),
    AnchorAnswer (..),
    decodeUnits,
    decodeAnchor,
    decodeArgs,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Data.Aeson (FromJSON (..), Key, Result (..), ToJSON (..), Value (..), eitherDecodeStrict, encode, fromJSON, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Functor ((<&>))
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Olifant.Process (Outcome (..), neverCancel, runBytesCancellable)
import System.Directory (doesFileExist, findExecutable, makeAbsolute)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, (</>))
import System.IO.Unsafe (unsafePerformIO)
import GHC.Clock (getMonotonicTime)

-- | A program answering a slot: its argv, and the folder it runs from (the
-- project root, set by the catalog; 'Nothing' is the current folder).
data Provider = Provider {provArgv :: [Text], provRoot :: Maybe FilePath}
  deriving stock (Eq, Show)

-- | How long a provider may run, in seconds.
providerTimeout :: Int
providerTimeout = 10

-- | The request as sent: an object gains @"v": 1@.
withVersion :: Value -> Value
withVersion = \case
  Object o -> Object (KM.insert "v" (Number 1) o)
  other -> object ["v" .= (1 :: Int), "request" .= other]

-- | Run a provider on a request. 'Left' is why the slot is unavailable, in
-- one line: the program is missing, it timed out, its stderr's first line
-- (on a non-zero exit), or why its answer is not JSON.
--
-- A circuit breaker per provider (argv and root): once it times out, or its
-- program is missing or cannot start, every request to it is answered with
-- that same 'Left', at once, for 'providerCoolOff' seconds. A broken
-- provider then costs one timeout, not one per file asked.
runProvider :: Provider -> Value -> IO (Either Text Value)
runProvider p request = ranAnswer <$> runGuarded p request

-- | How a provider's run ended: its answer (memoised: the same request
-- gives the same one), a failure of the program itself (trips the breaker,
-- not memoised), or a stop (neither).
data Ran = Answered (Either Text Value) | Broken Text | Stopped Text

ranAnswer :: Ran -> Either Text Value
ranAnswer = \case
  Answered a -> a
  Broken why -> Left why
  Stopped why -> Left why

-- | How long, in seconds, a provider that timed out or would not start is
-- not asked again.
providerCoolOff :: Double
providerCoolOff = 60

-- | 'runOnce' behind the provider's breaker.
runGuarded :: Provider -> Value -> IO Ran
runGuarded p request = do
  let key = (provArgv p, provRoot p)
  now <- getMonotonicTime
  M.lookup key <$> readMVar providerBreaker >>= \case
    Just (till, why) | now < till -> pure (Broken why)
    _ ->
      runOnce p request >>= \case
        ran@(Broken why) -> do
          at <- getMonotonicTime
          modifyMVar_ providerBreaker (pure . M.insert key (at + providerCoolOff, why))
          pure ran
        ran -> pure ran

{-# NOINLINE providerBreaker #-}
providerBreaker :: MVar (Map ([Text], Maybe FilePath) (Double, Text))
providerBreaker = unsafePerformIO (newMVar M.empty)

-- | One run of a provider, no breaker.
runOnce :: Provider -> Value -> IO Ran
runOnce (Provider argv root) request = case argv of
  [] -> pure (Answered (Left "the provider names no program"))
  prog : args ->
    resolve prog >>= \case
      Nothing -> pure (Broken ("`" <> prog <> "` is not installed"))
      Just exe ->
        runBytesCancellable neverCancel (Just providerTimeout) exe (map T.unpack args) root (BL.toStrict (encode (withVersion request))) <&> \case
          Left why -> Broken why
          Right TimedOut -> Broken ("it timed out (" <> T.pack (show providerTimeout) <> " s)")
          Right Cancelled -> Stopped "it was cancelled"
          Right (Finished (ExitFailure n) _ err) ->
            Answered . Left $ case filter (not . T.null) (map T.strip (T.lines (TE.decodeUtf8Lenient err))) of
              l : _ -> l
              [] -> "it exited " <> T.pack (show n) <> " and said nothing"
          Right (Finished ExitSuccess out _) ->
            Answered (either (Left . ("its answer is not JSON: " <>) . T.pack) Right (eitherDecodeStrict out))
  where
    -- a name with a slash is a path, against the root; a bare name is on PATH
    resolve prog
      | "/" `T.isInfixOf` prog = do
          let path = T.unpack prog
          full <- if isAbsolute path then pure path else makeAbsolute (maybe path (</> path) root)
          exists <- doesFileExist full
          pure (if exists then Just full else Nothing)
      | otherwise = findExecutable (T.unpack prog)

-- | 'runProvider', memoised for the process by (argv, root, request). The
-- request holds the file's path (when the caller knows it) and its text, so
-- an edited or another file is asked again. Only answers are kept: a
-- timeout, a missing program or a stop is asked again once the breaker
-- lets it.
runProviderMemo :: Provider -> Value -> IO (Either Text Value)
runProviderMemo p request = do
  let key = (provArgv p, provRoot p, BL.toStrict (encode request))
  M.lookup key <$> readMVar providerMemo >>= \case
    Just answer -> pure answer
    Nothing ->
      -- run outside the lock: a slow provider must not hold up other slots
      runGuarded p request >>= \case
        Answered answer -> do
          modifyMVar_ providerMemo $ \m ->
            pure (M.insert key answer (if M.size m >= memoCap then M.empty else m))
          pure answer
        ran -> pure (ranAnswer ran)

-- | 'runProviderMemo' from pure code (test units are read in pure code).
-- Sound because a provider is pure by contract: the same request gives the
-- same answer.
providerPure :: Provider -> Value -> Either Text Value
providerPure p request = unsafePerformIO (runProviderMemo p request)
{-# NOINLINE providerPure #-}

-- | A units, test-units or anchor request: the text, and its path when the
-- caller knows it.
textRequest :: Maybe FilePath -> Text -> [(Key, Value)] -> Value
textRequest file text extra = object (["text" .= text] <> maybe [] (\f -> ["file" .= f]) file <> extra)

-- | A units provider's answer for a text, checked against its lines.
providedUnits :: Provider -> Maybe FilePath -> Text -> IO (Either Text [UnitAnswer])
providedUnits p file text = runProviderMemo p (textRequest file text []) <&> (>>= decodeUnits (lineCount text))

-- | A text's lines as an answer counts them: a last line without a newline
-- is one.
lineCount :: Text -> Int
lineCount = length . T.lines

-- | Past this many answers the memo starts over, so a long-lived server
-- that sees many edits does not keep every text.
memoCap :: Int
memoCap = 512

{-# NOINLINE providerMemo #-}
providerMemo :: MVar (Map ([Text], Maybe FilePath, ByteString) (Either Text Value))
providerMemo = unsafePerformIO (newMVar M.empty)

-- | One unit (or test unit) in an answer: its name and inclusive 1-based
-- lines, and optionally a kind.
data UnitAnswer = UnitAnswer {uaName :: Text, uaLine :: Int, uaEnd :: Int, uaKind :: Maybe Text}
  deriving stock (Eq, Show)

instance FromJSON UnitAnswer where
  parseJSON = withObject "unit" $ \o -> UnitAnswer <$> o .: "name" <*> o .: "line" <*> o .: "end" <*> o .:? "kind"

instance ToJSON UnitAnswer where
  toJSON (UnitAnswer n l e k) = object (["name" .= n, "line" .= l, "end" .= e] <> maybe [] (\x -> ["kind" .= x]) k)

-- | Where inserted lines start (1-based; one past the last line is the end
-- of the file) and their indentation (a 1-based column).
data AnchorAnswer = AnchorAnswer {aaLine :: Int, aaColumn :: Int}
  deriving stock (Eq, Show)

instance FromJSON AnchorAnswer where
  parseJSON = withObject "anchor" $ \o -> AnchorAnswer <$> o .: "line" <*> o .: "column"

-- | A units answer for a text of @n@ lines, checked whole: an entry before
-- line 1, ending before it starts, past the text, or repeating an earlier
-- entry's span rejects the answer, naming the first such entry.
decodeUnits :: Int -> Value -> Either Text [UnitAnswer]
decodeUnits n v = case v of
  Array xs -> do
    units <- traverse decodeOne (zip [0 :: Int ..] (foldr (:) [] xs))
    case find (\(_, why) -> why /= Nothing) (zip [0 :: Int ..] (checks units)) of
      Just (i, Just why) -> Left ("entry " <> T.pack (show i) <> ": " <> why)
      _ -> Right units
  _ -> Left "the answer is not a list of units"
  where
    decodeOne (i, x) = case fromJSON x of
      Success u -> Right u
      Error why -> Left ("entry " <> T.pack (show i) <> ": " <> T.pack why)
    checks units = go S.empty units
      where
        go _ [] = []
        go seen (u : us) = bad seen u : go (S.insert (uaLine u, uaEnd u) seen) us
    bad seen u
      | uaLine u < 1 = Just ("line " <> num (uaLine u) <> " is before the first line")
      | uaEnd u < uaLine u = Just ("end " <> num (uaEnd u) <> " is before line " <> num (uaLine u))
      | uaEnd u > n = Just ("end " <> num (uaEnd u) <> " is past the last line (" <> num n <> ")")
      | S.member (uaLine u, uaEnd u) seen = Just ("lines " <> num (uaLine u) <> "-" <> num (uaEnd u) <> " repeat an earlier entry")
      | otherwise = Nothing
    num = T.pack . show

-- | An anchor answer for a text of @n@ lines: @null@ is "not known here".
decodeAnchor :: Int -> Value -> Either Text (Maybe AnchorAnswer)
decodeAnchor n = \case
  Null -> Right Nothing
  v -> case fromJSON v of
    Error why -> Left (T.pack why)
    Success a
      | aaLine a < 1 || aaLine a > n + 1 -> Left ("line " <> num (aaLine a) <> " is outside the text (1-" <> num (n + 1) <> ")")
      | aaColumn a < 1 -> Left ("column " <> num (aaColumn a) <> " is before the first column")
      | otherwise -> Right (Just a)
  where
    num = T.pack . show

-- | A selection answer: the argv that takes the place of @{= tests =}@.
decodeArgs :: Value -> Either Text [Text]
decodeArgs v = case fromJSON v of
  Success args -> Right args
  Error _ -> Left "the answer is not a list of strings"

