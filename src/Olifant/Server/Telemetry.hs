-- | Errand telemetry: one JSON object per line in
-- `.olifant/state/errands.jsonl` (ignored by git). An @errand@ record per
-- plan errand, on every ending; an @outcome@ record per ghost block the
-- plugin accepts or rejects (an errand's or a reconcile's); a @reconcile@
-- record per reconcile that asked or answered something. Appending is best-effort and never throws:
-- telemetry must never fail an errand.
module Olifant.Server.Telemetry
  ( ErrandRecord (..), OutcomeRecord (..), ReconcileRecord (..), Decision (..), Record (..)
  , recordJson, parseRecord, recordsPath, appendRecord, readRecords, errandId
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (void)
import Data.Aeson (Value (..), decodeStrict, encode, object, (.:), (.:?), (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Object, Pair, Parser, parseMaybe)
import Data.ByteString.Lazy qualified as BL
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Olifant.App (Ports (..))
import Olifant.Server.Errand (ModelUse (..), Status (..), Tokens (..), statusName)
import Olifant.Workspace (Workspace (..))
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

-- | One errand, however it ended (the spec's @errand@ kind).
-- 'erBlocks' is the number of ghost blocks answered.
data ErrandRecord = ErrandRecord
  { erId :: Text, erAt :: UTCTime, erVerb :: Text, erPlan :: FilePath, erRow :: Int, erSection :: Text
  , erBranch :: Maybe Text, erModel :: Text, erStatus :: Status, erWhy :: Maybe Text
  , erSession :: Maybe Text, erSeconds :: Maybe Double, erTurns :: Maybe Int, erTokens :: Maybe Tokens
  , erCost :: Maybe Double, erModels :: [(Text, ModelUse)], erPromptChars :: Int, erGrounded :: Int
  , erBlocks :: Int, erLines :: Int, erDropped :: Int, erUnresolved :: Int, erChanged :: Int, erDenials :: Int
  , -- | the errand a reopen continued
    erParent :: Maybe Text }
  deriving (Eq, Show)

-- | A decision on one ghost block (0-based) of an errand's answer.
data OutcomeRecord = OutcomeRecord {orId :: Text, orBlock :: Int, orAt :: UTCTime, orOutcome :: Text}
  deriving (Eq, Show)

-- | One typed question a reconcile asked: its kind, its options, the option
-- chosen (none when unsure) with its confidence, and the rung that answered
-- (or the last one tried).
data Decision = Decision {dcKind :: Text, dcOptions :: [Text], dcChosen :: Maybe Text, dcConfidence :: Maybe Double, dcRung :: Text}
  deriving (Eq, Show)

-- | One reconcile of a plan: its work set's sizes, the facts, the seconds,
-- each model call (rung, seconds), each decision, and per ghost block (in
-- the answer's order) the unit it is about, which a rejected outcome for
-- that block marks as read.
data ReconcileRecord = ReconcileRecord
  { rcId :: Text, rcAt :: UTCTime, rcPlan :: FilePath, rcBranch :: Maybe Text, rcBaseline :: Maybe Text, rcForced :: Bool
  , rcChanged :: Int, rcUnaccounted :: Int, rcMore :: Int, rcFacts :: Int, rcSeconds :: Double
  , rcClimbs :: [(Text, Double)], rcDecisions :: [Decision], rcBlocks :: [Maybe (FilePath, Text, Text)] }
  deriving (Eq, Show)

data Record = AnErrand ErrandRecord | AnOutcome OutcomeRecord | AReconcile ReconcileRecord
  deriving (Eq, Show)

-- | The record as the spec names its fields; absent Maybe fields are omitted.
recordJson :: Record -> Value
recordJson = \case
  AnErrand e ->
    object $
      [ "kind" .= ("errand" :: Text), "id" .= erId e, "at" .= stamp (erAt e), "verb" .= erVerb e
      , "plan" .= erPlan e, "row" .= erRow e, "section" .= erSection e, "model" .= erModel e
      , "status" .= statusName (erStatus e), "models" .= modelsJson (erModels e)
      , "prompt_chars" .= erPromptChars e, "grounded" .= erGrounded e, "blocks" .= erBlocks e
      , "lines" .= erLines e, "dropped" .= erDropped e, "unresolved" .= erUnresolved e
      , "changed" .= erChanged e, "denials" .= erDenials e ]
        ++ catMaybes
          [ ("branch" .=) <$> erBranch e, ("why" .=) <$> erWhy e, ("session" .=) <$> erSession e
          , ("seconds" .=) <$> erSeconds e, ("turns" .=) <$> erTurns e
          , ("tokens" .=) . object . tokensPairs <$> erTokens e, ("cost" .=) <$> erCost e
          , ("parent" .=) <$> erParent e ]
  AnOutcome o ->
    object ["kind" .= ("outcome" :: Text), "id" .= orId o, "block" .= orBlock o, "at" .= stamp (orAt o), "outcome" .= orOutcome o]
  AReconcile r ->
    object $
      [ "kind" .= ("reconcile" :: Text), "id" .= rcId r, "at" .= stamp (rcAt r), "plan" .= rcPlan r, "forced" .= rcForced r
      , "changed" .= rcChanged r, "unaccounted" .= rcUnaccounted r, "more" .= rcMore r, "facts" .= rcFacts r
      , "seconds" .= rcSeconds r, "climbs" .= [object ["rung" .= n, "seconds" .= t] | (n, t) <- rcClimbs r]
      , "decisions" .= map decisionJson (rcDecisions r)
      , "blocks" .= [maybe Null (\(path, unit, fp) -> object ["path" .= path, "unit" .= unit, "fp" .= fp]) b | b <- rcBlocks r] ]
        ++ catMaybes [("branch" .=) <$> rcBranch r, ("baseline" .=) <$> rcBaseline r]
  where
    decisionJson d = object ["kind" .= dcKind d, "options" .= dcOptions d, "chosen" .= dcChosen d, "confidence" .= dcConfidence d, "rung" .= dcRung d]
    modelsJson ms = object [K.fromText m .= object (tokensPairs (muTokens u) ++ maybe [] (\c -> ["cost" .= c]) (muCost u)) | (m, u) <- ms]
    -- whole seconds, UTC (aeson would add the fraction)
    stamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

tokensPairs :: Tokens -> [Pair]
tokensPairs t = ["input" .= tkInput t, "output" .= tkOutput t, "cache_write" .= tkCacheWrite t, "cache_read" .= tkCacheRead t]

-- | The inverse of 'recordJson'; an unknown kind or status is Nothing.
-- @models@ comes back in key order.
parseRecord :: Value -> Maybe Record
parseRecord = parseMaybe $ \case
  Object o ->
    o .: "kind" >>= \case
      ("errand" :: Text) -> AnErrand <$> errandP o
      "outcome" -> AnOutcome <$> (OutcomeRecord <$> o .: "id" <*> o .: "block" <*> o .: "at" <*> o .: "outcome")
      "reconcile" -> AReconcile <$> reconcileP o
      k -> fail ("unknown kind " <> T.unpack k)
  _ -> fail "not an object"
  where
    errandP o = do
      st <- o .: "status"
      status <- maybe (fail ("unknown status " <> T.unpack st)) pure (find ((== st) . statusName) [Ok, Empty, Error, Timeout, Cancelled])
      ms <- o .: "models"
      models <- traverse (\(k, v) -> (K.toText k,) <$> modelP v) (KM.toList ms)
      tokens <- traverse tokensP =<< o .:? "tokens"
      ErrandRecord
        <$> o .: "id" <*> o .: "at" <*> o .: "verb" <*> o .: "plan" <*> o .: "row" <*> o .: "section"
        <*> o .:? "branch" <*> o .: "model" <*> pure status <*> o .:? "why"
        <*> o .:? "session" <*> o .:? "seconds" <*> o .:? "turns" <*> pure tokens
        <*> o .:? "cost" <*> pure models <*> o .: "prompt_chars" <*> o .: "grounded"
        <*> o .: "blocks" <*> o .: "lines" <*> o .: "dropped" <*> o .: "unresolved" <*> o .: "changed" <*> o .: "denials"
        <*> o .:? "parent"
    reconcileP o = do
      climbs <- o .: "climbs" >>= traverse (\case Object c -> (,) <$> c .: "rung" <*> c .: "seconds"; _ -> fail "a climb is not an object")
      decisions <- o .: "decisions" >>= traverse (\case Object d -> Decision <$> d .: "kind" <*> d .: "options" <*> d .:? "chosen" <*> d .:? "confidence" <*> d .: "rung"; _ -> fail "a decision is not an object")
      blocks <- o .: "blocks" >>= traverse (\case Null -> pure Nothing; Object b -> (\p u f -> Just (p, u, f)) <$> b .: "path" <*> b .: "unit" <*> b .: "fp"; _ -> fail "a block is not an object or null")
      ReconcileRecord
        <$> o .: "id" <*> o .: "at" <*> o .: "plan" <*> o .:? "branch" <*> o .:? "baseline" <*> o .: "forced"
        <*> o .: "changed" <*> o .: "unaccounted" <*> o .: "more" <*> o .: "facts" <*> o .: "seconds"
        <*> pure climbs <*> pure decisions <*> pure blocks
    modelP = \case
      Object m -> ModelUse <$> tokensP m <*> m .:? "cost"
      _ -> fail "model use is not an object"

tokensP :: Object -> Parser Tokens
tokensP t = Tokens <$> t .: "input" <*> t .: "output" <*> t .: "cache_write" <*> t .: "cache_read"

-- | Where a repo's records live.
recordsPath :: FilePath -> FilePath
recordsPath root = root </> ".olifant/state/errands.jsonl"

-- | Serialises appends in this process: the file is read, extended and
-- written whole, so two unguarded appends would lose a line.
{-# NOINLINE recordLock #-}
recordLock :: MVar ()
recordLock = unsafePerformIO (newMVar ())

-- | Append one record as one line to the repo at ROOT. Never throws: a
-- record that cannot be written is dropped.
appendRecord :: Ports -> FilePath -> Record -> IO ()
appendRecord ports root r =
  void . (try :: IO () -> IO (Either SomeException ())) . withMVar recordLock $ \() -> do
    let path = recordsPath root
        line = TE.decodeUtf8 (BL.toStrict (encode (recordJson r))) <> "\n"
    old <- readText (pFiles ports) path
    new <- evaluate (fromMaybe "" old <> line)
    writeText (pFiles ports) path new

-- | Every record in a file's text, and how many non-blank lines were not one.
readRecords :: Text -> ([Record], Int)
readRecords txt = (catMaybes parsed, length (filter (== Nothing) parsed))
  where
    parsed = [decodeStrict (TE.encodeUtf8 l) >>= parseRecord | l <- T.lines txt, not (T.null (T.strip l))]

-- | An errand's id: the runner's session when it answered, else
-- @<verb>-<plan>:<row>\@<time>@ (only failed errands lack a session, and no
-- outcome can reach them).
errandId :: UTCTime -> Text -> FilePath -> Int -> Maybe Text -> Text
errandId at verb plan row = fromMaybe (verb <> "-" <> T.pack plan <> ":" <> T.pack (show row) <> "@" <> T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" at))
