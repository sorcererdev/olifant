-- | Claude Code's own transcripts (@~/.claude/projects/<slug>/**/*.jsonl@):
-- one message per assistant line that carries @message.usage@, deduped by
-- message id and request id, and priced at list prices.
module Olifant.Transcripts
  ( Message (..)
  , slugOf
  , parseLine
  , dedupe
  , messageCost
  , readTranscripts
  ) where

import Data.Aeson (FromJSON, Value (..), decodeStrict, fromJSON)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Char (isAlphaNum)
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Olifant.Catalog (Price (..))
import Olifant.Server.Errand (Tokens (..))
import System.FilePath ((</>))

-- | One assistant message's usage. 'msWrite1h' is the part of
-- 'tkCacheWrite' written with the 1-hour TTL; 'msKey' is
-- (@message.id@, @requestId@).
data Message = Message
  { msSession :: Text
  , msBranch :: Maybe Text
  , msAt :: UTCTime
  , msModel :: Text
  , msTokens :: Tokens
  , msWrite1h :: Int
  , msKey :: (Text, Text)
  }
  deriving stock (Eq, Show)

-- | Claude Code's project directory name: every non-alphanumeric character
-- becomes @-@.
slugOf :: FilePath -> FilePath
slugOf = map (\c -> if isAlphaNum c then c else '-')

-- | The message on one transcript line; 'Nothing' for a line without
-- @message.usage@ or one that does not parse.
parseLine :: Text -> Maybe Message
parseLine line = do
  Object o <- decodeStrict (TE.encodeUtf8 line)
  Object msg <- KM.lookup "message" o
  Object usage <- KM.lookup "usage" msg
  session <- str "sessionId" o
  at <- field "timestamp" o
  model <- str "model" msg
  mid <- str "id" msg
  let count k = fromMaybe 0 (field k usage)
      write = count "cache_creation_input_tokens"
      write1h = case KM.lookup "cache_creation" usage of
        Just (Object split) -> fromMaybe 0 (field "ephemeral_1h_input_tokens" split)
        _ -> write
  pure
    Message
      { msSession = session
      , msBranch = str "gitBranch" o
      , msAt = at
      , msModel = model
      , msTokens = Tokens (count "input_tokens") (count "output_tokens") write (count "cache_read_input_tokens")
      , msWrite1h = write1h
      , msKey = (mid, fromMaybe "" (str "requestId" o))
      }
  where
    str k o = case KM.lookup (K.fromText k) o of
      Just (String t) -> Just t
      _ -> Nothing
    field :: FromJSON a => Text -> A.Object -> Maybe a
    field k o = KM.lookup (K.fromText k) o >>= \v -> case fromJSON v of
      A.Success a -> Just a
      A.Error _ -> Nothing

-- | The first of each 'msKey' (streaming writes a message's usage more than
-- once).
dedupe :: [Message] -> [Message]
dedupe = go S.empty
  where
    go _ [] = []
    go seen (m : ms)
      | msKey m `S.member` seen = go seen ms
      | otherwise = m : go (S.insert (msKey m) seen) ms

-- | The message's list price in USD; 'Nothing' when the model is not in the
-- table (ids are matched exactly).
messageCost :: [(Text, Price)] -> Message -> Maybe Double
messageCost prices m = do
  p <- lookup (msModel m) prices
  let Tokens input output write cacheRead = msTokens m
      d = fromIntegral :: Int -> Double
      w1h = msWrite1h m
  pure
    ( ( d input * prInput p + d output * prOutput p + d cacheRead * prCacheRead p
          + d w1h * prCacheWrite1h p + d (write - w1h) * prCacheWrite5m p
      )
        / 1e6
    )

-- | Every message under DIR: lists it recursively with the first function
-- (paths relative to DIR, as 'Olifant.Workspace.listFiles' gives them),
-- reads each @*.jsonl@ with the second, parses only lines containing
-- @usage@, and dedupes.
readTranscripts :: (FilePath -> IO [FilePath]) -> (FilePath -> IO (Maybe Text)) -> FilePath -> IO [Message]
readTranscripts list readFile' dir = do
  files <- filter (".jsonl" `isSuffixOf`) <$> list dir
  texts <- mapM (readFile' . (dir </>)) files
  pure (dedupe [m | Just t <- texts, l <- T.lines t, "usage" `T.isInfixOf` l, Just m <- [parseLine l]])
