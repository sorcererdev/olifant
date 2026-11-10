-- | Internal to Olifant.Server: what the claim pass knows about a plan's
-- sentences, kept per plan in `.olifant/state/claims/` (version 2) and keyed
-- by a sentence's words ('sentenceKey', not its row): its triage and route,
-- the check the checker made (with the cited code), or the last failure.
-- Beside the entries, the joins: for each pair of rows Jev was asked about
-- ('Olifant.Plans.Sentences.pairKey'), whether the second continues the first.
module Olifant.Server.Claims
  ( CheckRecord (..)
  , Failure (..)
  , SentenceEntry (..)
  , ClaimStore
  , lineKey
  , claimsPath
  , readClaims
  , writeClaims
  , writeClaimsWith
  , readJoins
  , Joins
  , joinsOf
  , updateJoins
  , sentencesOf
  , pruneOrphanClaims
  , pruneClaims
  , keepGone
  , queued
  , retryAfter
  , checkedKeys
  , claimsOf
  , storeClaims
  , updateClaims
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_, unless, void)
import Data.Aeson (Value (..), decodeStrict, encode, object, (.:), (.:?), (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, parseMaybe, withObject)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Data.Dynamic (fromDynamic, toDyn)
import Data.IORef (atomicModifyIORef', readIORef)
import Olifant.App (Ports (..))
import Olifant.Server.State (ServerState (..))
import Data.Set (Set)
import Data.Set qualified as S
import Olifant.Plans (Line (..))
import Olifant.Plans.Sentences (Sentence (..), sentenceKey, sentencesJoined)
import Olifant.Server.Checker (Citation (..), Confirmed (..), Judgement (..), UnitRef (..), judgementName)
import Olifant.Server.Triage (Route (..), Triage (..), routeName)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (dropExtension, isAbsolute, makeRelative, takeExtension, (</>))
import System.IO.Unsafe (unsafePerformIO)

-- | One check as stored: the confirmed verdict, the model that made it, its
-- cost (when the runner said), its seconds, and when.
data CheckRecord = CheckRecord {cjConfirmed :: Confirmed, cjModel :: Text, cjCost :: Maybe Double, cjSeconds :: Double, cjAt :: UTCTime}
  deriving stock (Eq, Show)

-- | A check that failed (the runner, or a reply with no verdict), and when.
data Failure = Failure {fuWhy :: Text, fuAt :: UTCTime}
  deriving stock (Eq, Show)

-- | What the pass knows of one sentence, and since when it is no longer in
-- the plan (kept for 'keepGone', so an undo finds its triage and check).
data SentenceEntry = SentenceEntry
  { seTriage :: Triage, seRoute :: Route, seCheck :: Maybe CheckRecord
  , seFailure :: Maybe Failure, seAt :: UTCTime, seGone :: Maybe UTCTime }
  deriving stock (Eq, Show)

-- | A plan's entries by 'sentenceKey'.
type ClaimStore = Map Text SentenceEntry

-- | Jev's answers on pairs of rows, by 'pairKey': True joins them.
type Joins = Map Text Bool

-- | A list line's key: the words after its marker, as 'sentenceKey' (the
-- key of a one-sentence bullet).
lineKey :: Line -> Text
lineKey l =
  let b = T.drop 2 (T.stripStart (lnText l))
   in sentenceKey (fromMaybe b (T.stripPrefix "[ ] " b <|> T.stripPrefix "[x] " b <|> T.stripPrefix "? " b))
  where
    a <|> b = maybe b Just a

-- | How long a failed check waits before the pass tries it again.
retryAfter :: NominalDiffTime
retryAfter = 600

-- | Where a plan's store is: `.olifant/state/claims/<plan without .md>.json`
-- under ROOT (the plan relative to ROOT, or absolute under it).
claimsPath :: FilePath -> FilePath -> FilePath
claimsPath root plan = root </> ".olifant/state/claims" </> dropExtension rel <> ".json"
  where
    rel = normalisePath (if isAbsolute plan then makeRelative root plan else plan)

-- | A plan's store; missing or unreadable is empty.
readClaims :: Ports -> FilePath -> FilePath -> IO ClaimStore
readClaims ports root plan =
  either (const M.empty) id <$> (try :: IO ClaimStore -> IO (Either SomeException ClaimStore)) (do
    txt <- readText (pFiles ports) (claimsPath root plan)
    evaluate (fromMaybe M.empty (txt >>= decodeStrict . TE.encodeUtf8 >>= parseMaybe store)))
  where
    -- a store of another version is dropped: the pass triages again
    store = withObject "claims" $ \o -> do
      v <- o .:? "version"
      if v /= Just (2 :: Int) then fail "another version" else pure ()
      cs <- o .: "claims"
      withObject "entries" (\es -> M.fromList <$> mapM (\(k, v) -> (K.toText k,) <$> entry v) (KM.toList es)) cs

-- | A plan's joins (the "joins" field, optional); missing or unreadable is none.
readJoins :: Ports -> FilePath -> FilePath -> IO Joins
readJoins ports root plan =
  either (const M.empty) id <$> (try :: IO Joins -> IO (Either SomeException Joins)) (do
    txt <- readText (pFiles ports) (claimsPath root plan)
    evaluate (fromMaybe M.empty (txt >>= decodeStrict . TE.encodeUtf8 >>= parseMaybe joinsField)))
  where
    joinsField = withObject "claims" $ \o -> do
      v <- o .:? "version"
      if v /= Just (2 :: Int) then fail "another version" else pure ()
      fromMaybe M.empty <$> o .:? "joins"

-- | Write a plan's store with no joins ('writeClaimsWith').
writeClaims :: Ports -> FilePath -> FilePath -> ClaimStore -> IO ()
writeClaims ports root plan cs = writeClaimsWith ports root plan cs M.empty

-- | Write a plan's store and joins, under a lock; a store that cannot be
-- written is not written (never thrown).
writeClaimsWith :: Ports -> FilePath -> FilePath -> ClaimStore -> Joins -> IO ()
writeClaimsWith ports root plan cs js =
  void . (try :: IO () -> IO (Either SomeException ())) . withMVar claimsLock $ \() -> do
    let body = TE.decodeUtf8 (BL.toStrict (encode (object (["version" .= (2 :: Int), "claims" .= Object (KM.fromList [(K.fromText k, entryJson e) | (k, e) <- M.toList cs])] ++ ["joins" .= js | not (M.null js)])))) <> "\n"
    -- whole or not at all: a crash mid-write must not leave half a store
    let file = claimsPath root plan
    writeText (pFiles ports) (file <> ".tmp") body
    renameFile (pFiles ports) (file <> ".tmp") file

-- | Remove the stores of plans that no longer exist under ROOT (renamed or
-- deleted; their lines would be asked again under the new name anyway).
pruneOrphanClaims :: Ports -> FilePath -> IO ()
pruneOrphanClaims ports root =
  void . (try :: IO () -> IO (Either SomeException ())) $ do
    let dir = root </> ".olifant/state/claims"
    files <- listAllFiles (pFiles ports) dir
    forM_ [f | f <- files, takeExtension f == ".json"] $ \f -> do
      there <- fileExists (pFiles ports) (root </> dropExtension f <> ".md")
      unless there (removeFile (pFiles ports) (dir </> f))

{-# NOINLINE claimsLock #-}
claimsLock :: MVar ()
claimsLock = unsafePerformIO (newMVar ())

entryJson :: SentenceEntry -> Value
entryJson e =
  object
    [ "triage" .= object ["kind" .= trKind t, "p" .= trKindP t, "about" .= trAboutCode t, "by" .= trBy t]
    , "route" .= routeName (seRoute e)
    , "check" .= fmap checkJson (seCheck e)
    , "failure" .= fmap (\f -> object ["why" .= fuWhy f, "at" .= fuAt f]) (seFailure e)
    , "at" .= seAt e
    , "gone" .= seGone e
    ]
  where
    t = seTriage e
    refJson r = object ["path" .= urPath r, "name" .= urName r]
    checkJson c =
      let cf = cjConfirmed c
       in object
            [ "verdict" .= judgementName (cfJudgement cf)
            , "why" .= cfWhy cf
            , "evidence" .= [object ["path" .= ciPath x, "unit" .= ciUnit x, "line" .= ciLine x] | x <- cfEvidence cf]
            , "unit" .= fmap refJson (cfUnit cf)
            , "fingerprint" .= cfFingerprint cf
            , "model" .= cjModel c
            , "cost" .= cjCost c
            , "seconds" .= cjSeconds c
            , "at" .= cjAt c
            ]

entry :: Value -> Parser SentenceEntry
entry = withObject "entry" $ \o ->
  SentenceEntry
    <$> (o .: "triage" >>= triage)
    <*> (o .: "route" >>= route)
    <*> (o .:? "check" >>= traverse check)
    <*> (o .:? "failure" >>= traverse (withObject "failure" (\f -> Failure <$> f .: "why" <*> f .: "at")))
    <*> o .: "at"
    <*> o .:? "gone"
  where
    triage = withObject "triage" $ \t -> Triage <$> t .: "kind" <*> t .: "p" <*> t .: "about" <*> t .: "by"
    route = \case
      String "verify" -> pure Verify
      String "progress" -> pure Progress
      String "asked" -> pure Asked
      String "aside" -> pure Aside
      _ -> fail "a route"
    ref = withObject "unit" $ \r -> UnitRef <$> r .: "path" <*> r .: "name"
    citation = withObject "citation" $ \c -> Citation <$> c .: "path" <*> c .:? "unit" <*> c .:? "line"
    check = withObject "check" $ \c -> do
      j <- (c .: "verdict" :: Parser Text) >>= \case
        "right" -> pure JRight
        "wrong" -> pure JWrong
        "cannot_tell" -> pure JCannotTell
        _ -> fail "a verdict"
      cf <- Confirmed j <$> c .: "why" <*> (c .: "evidence" >>= mapM citation) <*> (c .:? "unit" >>= traverse ref) <*> c .:? "fingerprint"
      CheckRecord cf <$> c .: "model" <*> c .:? "cost" <*> c .: "seconds" <*> c .: "at"

-- | How long the entry of a sentence no longer in the plan is kept: an
-- edit undone, or a sentence cut and pasted back, finds its triage and its
-- paid check instead of buying them again.
keepGone :: NominalDiffTime
keepGone = 86400

-- | The store at NOW for the plan's sentences: a key no sentence has (an
-- edited sentence's old words) is marked gone, and dropped once gone for
-- 'keepGone'; a key back in the plan is no longer gone. A heading is not
-- part of a key, so renaming one keeps its section's entries.
pruneClaims :: UTCTime -> [Sentence] -> ClaimStore -> ClaimStore
pruneClaims now ss = M.mapMaybeWithKey keep
  where
    present = S.fromList (map (sentenceKey . snText) ss)
    keep k e
      | k `S.member` present = Just e {seGone = Nothing}
      | Just t <- seGone e = if addUTCTime keepGone t > now then Just e else Nothing
      | otherwise = Just e {seGone = Just now}

-- | The sentences a pass may check, in plan order, one per key: routed to
-- verify, not checked, and not failed in the last 'retryAfter'. A sentence
-- with no entry is not queued (it is triaged first).
queued :: UTCTime -> [Sentence] -> ClaimStore -> [Sentence]
queued now ss store = go S.empty ss
  where
    go _ [] = []
    go seen (s : rest)
      | k `S.member` seen = go seen rest
      | Just e <- M.lookup k store, ready e = s : go (S.insert k seen) rest
      | otherwise = go (S.insert k seen) rest
      where
        k = sentenceKey (snText s)
    ready e =
      seRoute e == Verify
        && seCheck e == Nothing
        && maybe True (\f -> addUTCTime retryAfter (fuAt f) <= now) (seFailure e)

-- | The keys with a right or wrong check (a checked belief answers its question).
checkedKeys :: ClaimStore -> Set Text
checkedKeys = M.keysSet . M.filter (\e -> maybe False ((/= JCannotTell) . cfJudgement . cjConfirmed) (seCheck e))

-- | A plan's store as the server holds it (ROOT, the plan's path): read
-- from disk on first use.
claimsOf :: ServerState -> FilePath -> FilePath -> IO ClaimStore
claimsOf s root plan = do
  known <- M.lookup (heldAt root plan) <$> readIORef (ssClaims s)
  case known >>= fromDynamic of
    Just cs -> pure cs
    Nothing -> do
      cs <- readClaims (ssPorts s) root plan
      -- another reader may have loaded it first: theirs is kept
      atomicModifyIORef' (ssClaims s) $ \m -> case M.lookup (heldAt root plan) m >>= fromDynamic of
        Just theirs -> (m, theirs)
        Nothing -> (M.insert (heldAt root plan) (toDyn cs) m, cs)

-- | A plan's store replaced by CS, kept by the server and written to disk
-- (tests; the pass and Check this use 'updateClaims').
storeClaims :: ServerState -> FilePath -> FilePath -> ClaimStore -> IO ()
storeClaims s root plan cs = void (updateClaims s root plan (const cs))

-- | Change a plan's store: F applied to the store as the server holds it
-- now, kept and written to disk, all under one lock, so two writers (a
-- pass and Check this, two passes) never write from a stale copy. The
-- store after.
updateClaims :: ServerState -> FilePath -> FilePath -> (ClaimStore -> ClaimStore) -> IO ClaimStore
updateClaims s root plan f =
  withMVar updateLock $ \() -> do
    cs <- f <$> claimsOf s root plan
    js <- joinsOf s root plan
    atomicModifyIORef' (ssClaims s) (\m -> (M.insert (heldAt root plan) (toDyn cs) m, ()))
    writeClaimsWith (ssPorts s) root plan cs js
    pure cs

-- | A plan's joins as the server holds them: read from disk on first use.
joinsOf :: ServerState -> FilePath -> FilePath -> IO Joins
joinsOf s root plan = do
  known <- M.lookup (joinsAt root plan) <$> readIORef (ssClaims s)
  case known >>= fromDynamic of
    Just js -> pure js
    Nothing -> do
      js <- readJoins (ssPorts s) root plan
      atomicModifyIORef' (ssClaims s) $ \m -> case M.lookup (joinsAt root plan) m >>= fromDynamic of
        Just theirs -> (m, theirs)
        Nothing -> (M.insert (joinsAt root plan) (toDyn js) m, js)

-- | Change a plan's joins as 'updateClaims' changes its entries (same lock,
-- same file).
updateJoins :: ServerState -> FilePath -> FilePath -> (Joins -> Joins) -> IO Joins
updateJoins s root plan f =
  withMVar updateLock $ \() -> do
    js <- f <$> joinsOf s root plan
    cs <- claimsOf s root plan
    atomicModifyIORef' (ssClaims s) (\m -> (M.insert (joinsAt root plan) (toDyn js) m, ()))
    writeClaimsWith (ssPorts s) root plan cs js
    pure js

-- | The sentences of LINES, a plan's, with the joins the server holds for it:
-- what the pass, the margin, hover, code actions and definition all read.
sentencesOf :: ServerState -> FilePath -> FilePath -> [Line] -> IO [Sentence]
sentencesOf s root plan ls = (`sentencesJoined` ls) <$> joinsOf s root plan

{-# NOINLINE updateLock #-}
updateLock :: MVar ()
updateLock = unsafePerformIO (newMVar ())

-- | Where the server holds a plan's joins, beside its store.
joinsAt :: FilePath -> FilePath -> FilePath
joinsAt root plan = heldAt root plan <> "#joins"

-- | A plan's absolute path, the server's key for its store.
heldAt :: FilePath -> FilePath -> FilePath
heldAt root plan = normalisePath (if isAbsolute plan then plan else root </> plan)
