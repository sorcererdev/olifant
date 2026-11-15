-- | Inspect's store, `.olifant/state/inspect/findings.json`: every finding inspect has
-- made in a project, with its scores, verdicts and the user's dismissals;
-- the unit fingerprints already inspected per file; and the units a cap left
-- waiting. An unreadable store is never dropped (dismissed findings would
-- come back): inspect stops for the project until it reads again.
module Olifant.Inspect.Store
  ( Status (..)
  , statusName
  , statusNamed
  , Place (..)
  , InspectFinding (..)
  , InspectStore (..)
  , emptyInspectStore
  , inspectStorePath
  , readInspectStore
  , updateInspectStore
  , findingId
  , dismiss
  , shownIn
  , encodeStore
  , decodeStore
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), eitherDecodeStrict, encode, object, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser, parseEither, withObject)
import Data.Bits (xor)
import Data.ByteString.Lazy qualified as BL
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.Word (Word64)
import Numeric (showHex)
import Olifant.Catalog (InspectConfig (..))
import Olifant.Server.Checker (Print (..))
import Olifant.Workspace (Workspace (..))
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

data Status = Open | Waiting | Withheld | Refuted | Unsure | Elsewhere | DismissedHere | DismissedEverywhere | Resolved | Gone
  deriving stock (Eq, Ord, Show, Enum, Bounded)

statusName :: Status -> Text
statusName = \case
  Open -> "open"
  Waiting -> "waiting"
  Withheld -> "withheld"
  Refuted -> "refuted"
  Unsure -> "unsure"
  Elsewhere -> "elsewhere"
  DismissedHere -> "dismissed-here"
  DismissedEverywhere -> "dismissed-everywhere"
  Resolved -> "resolved"
  Gone -> "gone"

statusNamed :: Text -> Maybe Status
statusNamed t = lookup t [(statusName s, s) | s <- [minBound .. maxBound]]

-- | Where a finding is: its file (relative to the root), the unit holding its
-- first line, its lines, and that unit's fingerprint when it was found.
-- 'plUnitStart' is the unit's first line when the finding was made: the
-- margin moves the finding with its unit.
data Place = Place {plFile :: FilePath, plUnit :: Maybe Text, plLines :: (Int, Int), plFingerprint :: Maybe Text, plUnitStart :: Maybe Int}
  deriving stock (Eq, Show)

data InspectFinding = InspectFinding
  { vfId :: Text
  , vfKind :: Text
  , vfRule :: Maybe Text
  , vfStatement :: Text
  , vfPlace :: Place
  , vfEvidence :: [(FilePath, (Int, Int))]
  , vfPrints :: [Print]
  , -- | 1 - P(nitpick)
    vfKeep :: Maybe Double
  , -- | what the nitpick question was asked on (unit fingerprint, rule text, wording), hashed
    vfAskedOn :: Maybe Text
  , -- | the claim check's judgement word, or "cited code not found"
    vfVerdict :: Maybe Text
  , vfWhy :: Maybe Text
  , vfStatus :: Status
  , vfSeen :: Int
  , vfModel :: Text
  , vfCost :: Double
  , vfAt :: UTCTime
  }
  deriving stock (Eq, Show)

data InspectStore = InspectStore
  { vsFindings :: [InspectFinding]
  , -- | per file, the unit fingerprints already inspected
    vsInspected :: Map FilePath (Set Text)
  , -- | per file, the unit names a cap left waiting
    vsCapped :: Map FilePath (Set Text)
  }
  deriving stock (Eq, Show)

emptyInspectStore :: InspectStore
emptyInspectStore = InspectStore [] M.empty M.empty

inspectStorePath :: FilePath
inspectStorePath = ".olifant/state/inspect/findings.json"

-- | The project's store: no file is an empty store; a file that does not
-- parse, or of another version, is Left (inspect stops; nothing overwrites it).
readInspectStore :: Workspace IO -> FilePath -> IO (Either Text InspectStore)
readInspectStore ws root =
  (try :: IO a -> IO (Either SomeException a)) (readText ws (root </> inspectStorePath)) >>= \case
    Left e -> pure (Left (T.pack (show e)))
    Right Nothing -> pure (Right emptyInspectStore)
    Right (Just t) -> pure (decodeStore t)

-- | Read, change and write the store under one lock; Left (nothing written)
-- when it does not read.
updateInspectStore :: Workspace IO -> FilePath -> (InspectStore -> (InspectStore, a)) -> IO (Either Text a)
updateInspectStore ws root f = withMVar storeLock $ \() ->
  readInspectStore ws root >>= \case
    Left why -> pure (Left why)
    Right vs -> do
      let (vs', a) = f vs
          file = root </> inspectStorePath
      written <- (try :: IO () -> IO (Either SomeException ())) $ do
        writeText ws (file <> ".tmp") (encodeStore vs')
        renameFile ws (file <> ".tmp") file
      pure (either (Left . T.pack . show) (const (Right a)) written)

storeLock :: MVar ()
storeLock = unsafePerformIO (newMVar ())
{-# NOINLINE storeLock #-}

-- | 12 hex digits of FNV-1a 64 over the time, file, lines and statement.
findingId :: UTCTime -> Place -> Text -> Text
findingId at pl statement =
  let (a, z) = plLines pl
      bytes = TE.encodeUtf8 (T.intercalate "\0" [T.pack (show at), T.pack (plFile pl), T.pack (show a), T.pack (show z), statement])
      h = BL.foldl' (\acc b -> (acc `xor` fromIntegral b) * 1099511628211) (14695981039346656037 :: Word64) (BL.fromStrict bytes)
      hex = showHex h ""
   in T.pack (take 12 (replicate (16 - length hex) '0' <> hex))

-- | Dismiss a finding here (False) or everywhere (True).
dismiss :: Bool -> Text -> InspectStore -> InspectStore
dismiss everywhere i vs = vs {vsFindings = [if vfId f == i then f {vfStatus = if everywhere then DismissedEverywhere else DismissedHere} else f | f <- vsFindings vs]}

-- | The margin of one file: its open findings of a shown kind, best kept first, at most max_shown.
shownIn :: InspectConfig -> FilePath -> InspectStore -> [InspectFinding]
shownIn cfg file vs =
  take (vcMaxShown cfg) (sortOn (Down . fromMaybe 0 . vfKeep) [f | f <- vsFindings vs, vfStatus f == Open, vfKind f `elem` vcKinds cfg, plFile (vfPlace f) == file])

encodeStore :: InspectStore -> Text
encodeStore vs =
  TE.decodeUtf8 (BL.toStrict (encode (object ["version" .= (1 :: Int), "findings" .= map findingJson (vsFindings vs), "inspected" .= fmap S.toList (vsInspected vs), "capped" .= fmap S.toList (vsCapped vs)]))) <> "\n"

decodeStore :: Text -> Either Text InspectStore
decodeStore t = either (Left . T.pack) Right (eitherDecodeStrict (TE.encodeUtf8 t) >>= parseEither store)
  where
    store = withObject "inspect store" $ \o -> do
      v <- o .: "version"
      if v /= (1 :: Int) then fail ("inspect store version " <> show v <> ", this olifant reads 1") else pure ()
      InspectStore
        <$> (o .: "findings" >>= mapM findingP)
        <*> (fmap S.fromList . fromMaybe M.empty <$> o .:? "inspected")
        <*> (fmap S.fromList . fromMaybe M.empty <$> o .:? "capped")

findingJson :: InspectFinding -> Value
findingJson f =
  object
    [ "id" .= vfId f
    , "kind" .= vfKind f
    , "rule" .= vfRule f
    , "statement" .= vfStatement f
    , "place" .= object ["file" .= plFile pl, "unit" .= plUnit pl, "lines" .= pair (plLines pl), "fingerprint" .= plFingerprint pl, "unit_start" .= plUnitStart pl]
    , "evidence" .= [object ["file" .= p, "lines" .= pair ls] | (p, ls) <- vfEvidence f]
    , "prints" .= [object ["path" .= ptPath p, "unit" .= ptUnit p, "hash" .= ptHash p] | p <- vfPrints f]
    , "keep" .= vfKeep f
    , "asked_on" .= vfAskedOn f
    , "verdict" .= vfVerdict f
    , "why" .= vfWhy f
    , "status" .= statusName (vfStatus f)
    , "seen" .= vfSeen f
    , "model" .= vfModel f
    , "cost" .= vfCost f
    , "at" .= vfAt f
    ]
  where
    pl = vfPlace f
    pair (a, z) = [a, z]

findingP :: Value -> Parser InspectFinding
findingP = withObject "finding" $ \o -> do
  pl <- o .: "place" >>= withObject "place" (\p -> Place <$> p .: "file" <*> p .:? "unit" <*> (p .: "lines" >>= pairP) <*> p .:? "fingerprint" <*> p .:? "unit_start")
  ev <- o .: "evidence" >>= mapM (withObject "evidence" (\e -> (,) <$> e .: "file" <*> (e .: "lines" >>= pairP)))
  ps <- o .: "prints" >>= mapM (withObject "print" (\p -> Print <$> p .: "path" <*> p .:? "unit" <*> p .: "hash"))
  st <- o .: "status" >>= \s -> maybe (fail ("unknown status " <> T.unpack s)) pure (statusNamed s)
  InspectFinding
    <$> o .: "id"
    <*> o .: "kind"
    <*> o .:? "rule"
    <*> o .: "statement"
    <*> pure pl
    <*> pure ev
    <*> pure ps
    <*> o .:? "keep"
    <*> o .:? "asked_on"
    <*> o .:? "verdict"
    <*> o .:? "why"
    <*> pure st
    <*> o .: "seen"
    <*> o .: "model"
    <*> o .: "cost"
    <*> o .: "at"
  where
    pairP v = case v of
      [a, z] -> pure (a, z)
      _ -> fail "lines are [first, last]"
