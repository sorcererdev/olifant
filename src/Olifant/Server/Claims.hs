-- | Internal to Olifant.Server: what the claim pass knows about a plan's
-- lines, kept per plan in `.olifant/state/claims/` and keyed by a line's
-- words (not its row): the unit each resolved to, by which round, its
-- candidates, and the check against that unit, with the unit's fingerprint
-- when it was checked. And the check itself: a typed question on the plan's
-- chat ladder.
module Olifant.Server.Claims
  ( Check (..)
  , ClaimEntry (..)
  , ClaimStore
  , claimKey
  , claimsPath
  , readClaims
  , writeClaims
  , checkQuestion
  , checkClaim
  , Work (..)
  , workLine
  , passWork
  , pruneClaims
  , checkedKeys
  , claimsOf
  , storeClaims
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (void)
import Data.Aeson (Value (..), decodeStrict, encode, object, (.:), (.:?), (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, parseMaybe, withObject)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.Dynamic (fromDynamic, toDyn)
import Data.IORef (modifyIORef', readIORef)
import Olifant.App (Ports (..))
import Olifant.Server.State (ServerState (..))
import Olifant.Decide (Answer (..), Confidence (..), choice, qOptions)
import Olifant.Decide qualified as D
import Data.Set (Set)
import Data.Set qualified as S
import Olifant.Plans (Kind (..), Line (..), childrenOf)
import Olifant.Process (Cancel)
import Olifant.Server.LoopQuestions (Rungs (..), accepted, decideOver)
import Olifant.Server.Resolve (LineContext (..), UnitRef (..), bareLine, refText)
import Olifant.Server.Telemetry (Decision (..))
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (dropExtension, isAbsolute, makeRelative, (</>))
import System.IO.Unsafe (unsafePerformIO)

-- | A claim checked against its unit: right, wrong (and why), or about some
-- other code than this unit.
data Check = CheckRight | CheckWrong Text | CheckElsewhere
  deriving stock (Eq, Show)

-- | What the pass knows of one line: its unit (none when unresolved), the
-- round that found it (`jev-1`, `jev-2`, `jev-3`, `none`, or `elsewhere`
-- when its check rejected the unit), its candidates best first, the top
-- probability Jev gave, its check, the unit's fingerprint at the check, and
-- when.
data ClaimEntry = ClaimEntry
  { ceUnit :: Maybe UnitRef
  , ceRung :: Text
  , ceCandidates :: [UnitRef]
  , ceTop :: Double
  , ceCheck :: Maybe Check
  , ceFingerprint :: Maybe Text
  , ceAt :: UTCTime
  }
  deriving stock (Eq, Show)

-- | A plan's entries by 'claimKey'.
type ClaimStore = Map Text ClaimEntry

-- | A line's words as a key: without its indentation, its `- `, a checkbox,
-- and its anchor and verdict. An edited line is a new key.
claimKey :: Line -> Text
claimKey l = T.strip (fromMaybe b (listToMaybe [r | p <- ["[ ] ", "[x] "], Just r <- [T.stripPrefix p b]]))
  where
    b = bareLine l

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
    store = withObject "claims" $ \o -> do
      cs <- o .: "claims"
      withObject "entries" (\es -> M.fromList <$> mapM (\(k, v) -> (K.toText k,) <$> entry v) (KM.toList es)) cs

-- | Write a plan's store, under a lock; a store that cannot be written is
-- not written (never thrown).
writeClaims :: Ports -> FilePath -> FilePath -> ClaimStore -> IO ()
writeClaims ports root plan cs =
  void . (try :: IO () -> IO (Either SomeException ())) . withMVar claimsLock $ \() -> do
    let body = TE.decodeUtf8 (BL.toStrict (encode (object ["version" .= (1 :: Int), "claims" .= Object (KM.fromList [(K.fromText k, entryJson e) | (k, e) <- M.toList cs])]))) <> "\n"
    writeText (pFiles ports) (claimsPath root plan) body

{-# NOINLINE claimsLock #-}
claimsLock :: MVar ()
claimsLock = unsafePerformIO (newMVar ())

entryJson :: ClaimEntry -> Value
entryJson e =
  object
    [ "unit" .= fmap refJson (ceUnit e)
    , "rung" .= ceRung e
    , "candidates" .= map refJson (ceCandidates e)
    , "top" .= ceTop e
    , "check" .= fmap checkJson (ceCheck e)
    , "fingerprint" .= ceFingerprint e
    , "at" .= ceAt e
    ]
  where
    refJson r = object ["path" .= urPath r, "name" .= urName r]
    checkJson = \case
      CheckRight -> object ["verdict" .= ("right" :: Text)]
      CheckWrong why -> object ["verdict" .= ("wrong" :: Text), "why" .= why]
      CheckElsewhere -> object ["verdict" .= ("elsewhere" :: Text)]

entry :: Value -> Parser ClaimEntry
entry = withObject "entry" $ \o ->
  ClaimEntry
    <$> (o .:? "unit" >>= traverse ref)
    <*> o .: "rung"
    <*> (o .: "candidates" >>= mapM ref)
    <*> o .: "top"
    <*> (o .:? "check" >>= traverse check)
    <*> o .:? "fingerprint"
    <*> o .: "at"
  where
    ref = withObject "unit" $ \r -> UnitRef <$> r .: "path" <*> r .: "name"
    check = withObject "check" $ \c ->
      (c .: "verdict" :: Parser Text) >>= \case
        "right" -> pure CheckRight
        "wrong" -> CheckWrong <$> c .: "why"
        "elsewhere" -> pure CheckElsewhere
        _ -> fail "a verdict"

-- | Is this plan line right about this unit's code? Right, wrong (why, in
-- one short sentence), or about other code.
checkQuestion :: LineContext -> UnitRef -> Text -> D.Question
checkQuestion c u code =
  (choice "plan-check" "Is this plan line right about this code? If wrong, say why in one short sentence." options context) {D.qFields = [("why", "text")]}
  where
    options =
      [ ("right", "the code does what the line says")
      , ("wrong", "the code contradicts the line, or lacks what it says")
      , ("elsewhere", "this code is not what the line is about")
      ]
    context =
      [("line", lcLine c), ("unit", refText u), ("code", T.intercalate "\n" (take 120 (T.lines code)))]
        ++ [("question", q) | Just q <- [lcQuestion c]]

-- | A claim checked on the chat rungs (never Jev), under their escalate
-- policy: the check when one answers at the gate, and the decision asked.
checkClaim :: Cancel -> Int -> Rungs -> LineContext -> UnitRef -> Text -> IO (Maybe Check, [Decision])
checkClaim token timeout rungs c u code
  | null (rgChat rungs) = pure (Nothing, [])
  | otherwise = do
      let q = checkQuestion c u code
      (a, climbs) <- decideOver token timeout rungs {rgJev = Nothing} q
      let decided = [Decision (D.qKind q) (qOptions q) (chosen a) (confOf a) name | Just (name, _) <- [listToMaybe (reverse climbs)]]
          check = case a of
            Chosen "right" _ _ | accepted q a -> Just CheckRight
            Chosen "wrong" fs _ | accepted q a -> Just (CheckWrong (let w = T.strip (fromMaybe "" (lookup "why" fs)) in if T.null w then "no reason given" else w))
            Chosen "elsewhere" _ _ | accepted q a -> Just CheckElsewhere
            _ -> Nothing
      pure (check, decided)
  where
    chosen = \case
      Chosen x _ _ -> Just x
      _ -> Nothing
    confOf = \case
      Chosen _ _ (Confidence x) -> Just x
      _ -> Nothing

-- | What the pass does for a line: resolve it and check it (a claim),
-- resolve it only (a step), or write the anchor its right check earned (a
-- write that did not land last time), or check a resolved claim again (its
-- last check got no answer).
data Work = ResolveAndCheck Line | ResolveOnly Line | WriteAnchor Line ClaimEntry | CheckAgain Line ClaimEntry
  deriving stock (Eq, Show)

-- | The pass's work in plan order: each unanchored claim and step not in the
-- store; each unanchored line whose entry is right, with a unit and a
-- fingerprint. A bare claim beside an anchored claim or evidence under a
-- question is that answer's conclusion, not a belief: never work. With a
-- RETRY cutoff (given when a chat rung can check), a claim whose entry holds
-- a unit and no check, stamped before the cutoff, is checked again: its last
-- check got no answer.
passWork :: Maybe UTCTime -> [Line] -> ClaimStore -> [Work]
passWork retry ls store = firstPerKey (concatMap work ls)
  where
    work l
      | lnAnchor l /= Nothing || T.null key = []
      | Just e <- M.lookup key store = case e of
          ClaimEntry {ceCheck = Just CheckRight, ceUnit = Just _, ceFingerprint = Just _} | listLine -> [WriteAnchor l e]
          ClaimEntry {ceCheck = Nothing, ceUnit = Just _, ceAt = at}
            | lnKind l == Claim, Just cutoff <- retry, at < cutoff, not (conclusion l) -> [CheckAgain l e]
          _ -> []
      | lnKind l == Claim, not (conclusion l) = [ResolveAndCheck l]
      | Step _ <- lnKind l = [ResolveOnly l]
      | otherwise = []
      where
        key = claimKey l
        listLine = case lnKind l of
          Claim -> True
          Step _ -> True
          _ -> False
    -- one key is one question: lines with the same words share it
    firstPerKey = go S.empty
      where
        go _ [] = []
        go seen (w : ws)
          | k `S.member` seen = go seen ws
          | otherwise = w : go (S.insert k seen) ws
          where
            k = claimKey (workLine w)
    conclusion l = case parentOf l of
      Just q
        | lnKind q == Question ->
            any (\c -> lnKind c == Evidence || (lnKind c == Claim && lnAnchor c /= Nothing)) (directKids q)
      _ -> False
    parentOf l = listToMaybe [p | p <- reverse (take (lnRow l) ls), lnIndent p < lnIndent l, lnKind p /= Prose]
    directKids q = [c | c <- childrenOf ls (lnRow q), lnIndent c == lnIndent q + 2]

-- | The line a piece of work is about.
workLine :: Work -> Line
workLine = \case
  ResolveAndCheck l -> l
  ResolveOnly l -> l
  WriteAnchor l _ -> l
  CheckAgain l _ -> l

-- | The store without keys no line of the plan has (an edited line's old
-- words).
pruneClaims :: [Line] -> ClaimStore -> ClaimStore
pruneClaims ls store = M.restrictKeys store (S.fromList (map claimKey ls))

-- | The keys whose lines have a stored verdict, right or wrong (a checked
-- belief answers its question; one about other code does not).
checkedKeys :: ClaimStore -> Set Text
checkedKeys = M.keysSet . M.filter (\e -> case ceCheck e of Just CheckRight -> True; Just (CheckWrong _) -> True; _ -> False)

-- | A plan's store as the server holds it (ROOT, the plan's path): read
-- from disk on first use.
claimsOf :: ServerState -> FilePath -> FilePath -> IO ClaimStore
claimsOf s root plan = do
  known <- M.lookup (heldAt root plan) <$> readIORef (ssClaims s)
  case known >>= fromDynamic of
    Just cs -> pure cs
    Nothing -> do
      cs <- readClaims (ssPorts s) root plan
      modifyIORef' (ssClaims s) (M.insert (heldAt root plan) (toDyn cs))
      pure cs

-- | A plan's store kept by the server and written to disk.
storeClaims :: ServerState -> FilePath -> FilePath -> ClaimStore -> IO ()
storeClaims s root plan cs = do
  modifyIORef' (ssClaims s) (M.insert (heldAt root plan) (toDyn cs))
  writeClaims (ssPorts s) root plan cs

-- | A plan's absolute path, the server's key for its store.
heldAt :: FilePath -> FilePath -> FilePath
heldAt root plan = normalisePath (if isAbsolute plan then plan else root </> plan)
