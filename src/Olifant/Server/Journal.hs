{-# LANGUAGE OverloadedStrings #-}
-- | The plan's record of the work: a dated line the engine writes under a
-- step when something happened to it (a do ended, a review was answered,
-- its tests ticked or unticked it). Each event is written once, keyed, so
-- a line the user deletes is never written again. The store,
-- `.olifant/state/journal/<plan>.jsonl`, keeps every event and whether its
-- line has landed in the plan.
module Olifant.Server.Journal
  ( Event (..)
  , Entry (..)
  , EntryState (..)
  , DoEnd (..)
  , journalLine
  , journalPath
  , record
  , entries
  , pendingEntries
  , markLanded
  , markLost
  , pendingFacts
  , withJournal
  , entryAt
  , doEvent
  , keepEvent
  , testEvents
  , transitions
  , testStates
  , recordTests
  , movedEvent
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.Aeson (Value (..), decodeStrict, encode, object, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Lazy qualified as BL
import Data.List (find, sortOn)
import Data.Ord (Down (..))
import Control.Monad (void)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (TimeZone, UTCTime, defaultTimeLocale, formatTime, utcToZonedTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Olifant.App (Ports (..))
import Olifant.Plans (Anchor (..), Kind (..), Line (..), TestState (..), childrenOf, parsePlan, testState)
import Olifant.Server.LoopTypes (Fact (..))
import Olifant.Server.Review (sameStep, stepKey)
import Olifant.Workspace (Workspace (..))
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath (dropExtension, takeExtension, (</>))

-- | Something that happened to a step: its key (written once), the step's
-- line and row when it happened (found again by its words), when, the
-- words, and the errand behind it.
data Event = Event
  { evKey :: Text
  , evStep :: Text
  , evRow :: Int
  , evAt :: UTCTime
  , evWords :: Text
  , evErrand :: Maybe Text
  }
  deriving stock (Eq, Show)

data EntryState = Pending | Landed | Lost
  deriving stock (Eq, Show)

-- | An event as stored: whether its line landed, and the line (no indent).
data Entry = Entry {enEvent :: Event, enState :: EntryState, enLine :: Text}
  deriving stock (Eq, Show)

-- | How a do ended, for its line.
data DoEnd = DoAnswered Text | DoBlocked Text | DoEmpty | DoError Text | DoTimeout | DoCancelled
  deriving stock (Eq, Show)

-- | "- 25 Oct 21:31 · words", in ZONE.
journalLine :: TimeZone -> Event -> Text
journalLine zone e = "- " <> T.pack (formatTime defaultTimeLocale "%-d %b %H:%M" (utcToZonedTime zone (evAt e))) <> " · " <> evWords e

journalPath :: FilePath -> FilePath -> FilePath
journalPath root plan = root </> ".olifant/state/journal" </> (if takeExtension plan == ".md" then dropExtension plan else plan) <> ".jsonl"

entries :: Ports -> FilePath -> FilePath -> IO [Entry]
-- readers take the lock too: a file half rewritten is never read
entries ports root plan = withMVar journalLock (\() -> readEntries ports root plan)

readEntries :: Ports -> FilePath -> FilePath -> IO [Entry]
readEntries ports root plan = maybe [] (mapMaybe parseEntry . T.lines) <$> readText (pFiles ports) (journalPath root plan)

pendingEntries :: Ports -> FilePath -> FilePath -> IO [Entry]
pendingEntries ports root plan = filter ((== Pending) . enState) <$> entries ports root plan

save :: Ports -> FilePath -> FilePath -> [Entry] -> IO ()
save ports root plan es = writeText (pFiles ports) (journalPath root plan) (T.concat [entryJson e <> "\n" | e <- es])

-- | One writer at a time: the store is read whole and rewritten, from the
-- do's, the reconcile's and the tests' threads.
{-# NOINLINE journalLock #-}
journalLock :: MVar ()
journalLock = unsafePerformIO (newMVar ())

-- | Store EVENTS not seen before (by key) as pending; the new ones.
record :: Ports -> TimeZone -> FilePath -> FilePath -> [Event] -> IO [Entry]
record ports zone root plan evs = recordWith (const id) ports zone root plan evs

-- | 'record', EVENTS first passed through a filter of the store as it is.
recordWith :: ([Entry] -> [Event] -> [Event]) -> Ports -> TimeZone -> FilePath -> FilePath -> [Event] -> IO [Entry]
recordWith pick ports zone root plan evs0 = withMVar journalLock $ \() -> do
  old <- readEntries ports root plan
  let evs = pick old evs0
  let seen = map (evKey . enEvent) old
      fresh = foldl' (\acc e -> if evKey e `elem` seen || any ((== evKey e) . evKey . enEvent) acc then acc else acc ++ [Entry e Pending (journalLine zone e)]) [] evs
  if null fresh then pure [] else fresh <$ save ports root plan (old ++ fresh)

markAs :: EntryState -> Ports -> FilePath -> FilePath -> [Text] -> IO ()
markAs st ports root plan keys
  | null keys = pure ()
  | otherwise = withMVar journalLock $ \() -> do
      es <- readEntries ports root plan
      save ports root plan [if evKey (enEvent e) `elem` keys then e {enState = st} else e | e <- es]

markLanded, markLost :: Ports -> FilePath -> FilePath -> [Text] -> IO ()
markLanded = markAs Landed
markLost = markAs Lost

-- | Where pending ENTRIES go in the plan's lines: one insertion per step,
-- after its last child, lines in event order; the keys placed; the keys
-- whose step is gone.
pendingFacts :: [Line] -> [Entry] -> ([Fact], [Text], [Text])
pendingFacts ls es = (withJournal [] [Insert r xs | (r, xs, _) <- placed], [k | (k, Just _) <- found], [k | (k, Nothing) <- found])
  where
    found = [(evKey (enEvent e), (\s -> (s, e)) <$> stepOf (enEvent e)) | e <- es, enState e == Pending]
    -- at one row, a deeper step's lines first (they belong inside it)
    placed = sortOn (\(r, _, ind) -> (r, Down ind)) [(after s, [T.replicate (lnIndent s + 2) " " <> enLine e], lnIndent s) | (_, Just (s, e)) <- found]
    after s = case childrenOf ls (lnRow s) of
      [] -> lnRow s
      kids -> lnRow (last kids)
    stepOf e =
      let same l = isStep l && sameStep (evStep e) (lnText l)
       in case [l | l <- ls, lnRow l == evRow e, same l] of
            l : _ -> Just l
            [] -> find same ls
    isStep l = case lnKind l of Step _ -> True; _ -> False

-- | FACTS with JOURNAL's insertions added: one at a row that already has
-- an insertion joins it, after its lines (so the order is kept however the
-- client applies insertions).
withJournal :: [Fact] -> [Fact] -> [Fact]
withJournal = foldl' add
  where
    add fs (Insert r xs)
      | any (isAt r) fs = [case f of Insert r' ys | r' == r -> Insert r' (ys ++ xs); _ -> f | f <- fs]
      | otherwise = fs ++ [Insert r xs]
    add fs f = fs ++ [f]
    isAt r = \case Insert r' _ -> r' == r; _ -> False

-- | The entry behind the journal line at ROW of LS: the same text, and of
-- those, the one recorded for the step the line sits under.
entryAt :: [Entry] -> [Line] -> Int -> Maybe Entry
entryAt es ls row = case [l | l <- ls, lnRow l == row, lnKind l == Journal] of
  l : _ ->
    let same = [e | e <- es, enLine e == T.strip (lnText l)]
        owner = listToMaybe [s | s <- reverse (takeWhile ((< row) . lnRow) ls), isStep s, lnIndent s < lnIndent l]
        mine = [e | Just s <- [owner], e <- same, sameStep (evStep (enEvent e)) (lnText s)]
     in listToMaybe (mine ++ same)
  [] -> Nothing
  where
    isStep s = case lnKind s of Step _ -> True; _ -> False

cut :: Text -> Text
cut t =
  let one = T.unwords (T.words t)
   in if T.length one > 80 then T.take 79 one <> "…" else one

-- | A do's line: how it ended (AGAIN: a reopen).
doEvent :: Text -> Text -> Int -> UTCTime -> Bool -> Text -> DoEnd -> Event
doEvent eid step row at again model end =
  Event ("do|" <> eid) step row at (cut (verb <> " (" <> model <> ") · " <> how)) (Just eid)
  where
    verb = if again then "do again" else "do"
    how = case end of
      DoAnswered s -> "answered: " <> s
      DoBlocked w -> "blocked: " <> w
      DoEmpty -> "answered nothing"
      DoError w -> "error: " <> w
      DoTimeout -> "timed out"
      DoCancelled -> "cancelled"

-- | A do review answered: CHOICE is kept, kept-ticked (a step with no
-- tests, ticked by the keep), reverted or reopened.
keepEvent :: Text -> Text -> Text -> Int -> UTCTime -> Text -> Event
keepEvent rid eid step row at choice = Event ("review|" <> rid <> "||" <> choice) step row at words' (Just eid)
  where
    words' = case choice of
      "reverted" -> "reverted the do's change"
      "reopened" -> "kept the do's change · asked for more"
      "kept-ticked" -> "kept the do's change · ticked: the step has no tests"
      _ -> "kept the do's change"

-- | A test run's line for the step at ROW (LS before FACTS): a tick, an
-- untick, a first red or a green, once per fingerprint FP.
testEvents :: [Line] -> Int -> [Fact] -> [((FilePath, Text), TestState)] -> Maybe Text -> UTCTime -> [Event]
testEvents ls row0 facts states fp at = case drop row ls of
  s : _ | isStepK (lnKind s), not (null states) ->
    let key c = "tests|" <> fst (stepKey (lnText s)) <> "|" <> fromMaybe "" fp <> "|" <> c
        to = [k | Replace r t <- facts, r == row, k : _ <- [map lnKind (parsePlan t)]]
        reds = [u | ((_, u), TestRed) <- states]
        ev c w = [Event (key c) (lnText s) row at w Nothing]
     in case reds of
          u : _
            | to == [Step False] && lnKind s == Step True -> ev "red" "unticked: tests red again"
            | otherwise -> ev "red" ("tests red: " <> u)
          []
            | all ((== TestGreen) . snd) states, to == [Step True] -> ev "green" "ticked: tests green"
            | all ((== TestGreen) . snd) states -> ev "green" "tests green"
            | otherwise -> []
  _ -> []
  where
    isStepK = \case Step _ -> True; _ -> False
    -- a test child's run is its step's
    row = case drop row0 ls of
      l : _ | lnKind l == TestChild -> maybe row0 lnRow (listToMaybe [s | s <- reverse (take row0 ls), isStepK (lnKind s), lnIndent s < lnIndent l])
      _ -> row0

-- | The test children's marks in a run's FACTS, by test.
testStates :: [Fact] -> [((FilePath, Text), TestState)]
testStates facts = [((anPath a, anUnit a), st) | Replace _ t <- facts, l <- take 1 (parsePlan t), lnKind l == TestChild, Just a <- [lnAnchor l], Just st <- [testState l]]

-- | A test run's lines (the step at ROW of LS, its FACTS, its unit's
-- fingerprint FP) recorded; with no fingerprint, by change of state.
recordTests :: Ports -> TimeZone -> FilePath -> FilePath -> [Line] -> Int -> [Fact] -> Maybe Text -> UTCTime -> IO ()
recordTests ports zone root plan ls row facts fp at =
  void (recordWith (if isJust fp then const id else transitions) ports zone root plan (testEvents ls row facts (testStates facts) fp at))

-- | A step with no fingerprint keys its tests' lines by change of state:
-- of EVENTS, a `tests|<step>||<state>` one is dropped when the step's last
-- such line in ENTRIES has that state, else keyed by its time (written).
-- Every other event is as it is.
transitions :: [Entry] -> [Event] -> [Event]
transitions es = concatMap one
  where
    one e = case T.splitOn "|" (evKey e) of
      ["tests", step, "", st] ->
        let prior = [p | en <- es, ["tests", s', "", p, _] <- [T.splitOn "|" (evKey (enEvent en))], s' == step]
         in if listToMaybe (reverse prior) == Just st then [] else [e {evKey = evKey e <> "|" <> T.pack (iso8601Show (evAt e))}]
      _ -> [e]

-- | A step whose anchor a reconcile moved to NEW (path:Unit).
movedEvent :: Text -> Int -> UTCTime -> Text -> Event
movedEvent step row at new = Event ("moved|" <> fst (stepKey step) <> "|" <> new) step row at ("moved: now " <> T.dropWhile (== '@') new) Nothing

entryJson :: Entry -> Text
entryJson (Entry e st l) =
  TE.decodeUtf8 . BL.toStrict . encode $
    object
      [ "key" .= evKey e, "step" .= evStep e, "row" .= evRow e, "at" .= iso8601Show (evAt e), "words" .= evWords e
      , "errand" .= evErrand e, "state" .= stateName st, "line" .= l ]
  where
    stateName = \case Pending -> "pending" :: Text; Landed -> "landed"; Lost -> "lost"

parseEntry :: Text -> Maybe Entry
parseEntry t = decodeStrict (TE.encodeUtf8 t) >>= parseMaybe p
  where
    p :: Value -> Parser Entry
    p = \case
      Object o -> do
        at <- maybe (fail "at") pure . iso8601ParseM =<< o .: "at"
        e <- Event <$> o .: "key" <*> o .: "step" <*> o .: "row" <*> pure at <*> o .: "words" <*> o .:? "errand"
        st <- o .: "state" >>= \case ("landed" :: Text) -> pure Landed; "lost" -> pure Lost; _ -> pure Pending
        Entry e st <$> o .: "line"
      _ -> fail "an entry is an object"
