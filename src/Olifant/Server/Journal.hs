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
  , movedEvent
  ) where

import Data.Aeson (Value (..), decodeStrict, encode, object, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Lazy qualified as BL
import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (TimeZone, UTCTime, defaultTimeLocale, formatTime, utcToZonedTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Olifant.App (Ports (..))
import Olifant.Plans (Kind (..), Line (..), TestState (..), childrenOf, parsePlan)
import Olifant.Server.LoopTypes (Fact (..))
import Olifant.Server.Review (stepKey)
import Olifant.Workspace (Workspace (..))
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
entries ports root plan = maybe [] (mapMaybe parseEntry . T.lines) <$> readText (pFiles ports) (journalPath root plan)

pendingEntries :: Ports -> FilePath -> FilePath -> IO [Entry]
pendingEntries ports root plan = filter ((== Pending) . enState) <$> entries ports root plan

save :: Ports -> FilePath -> FilePath -> [Entry] -> IO ()
save ports root plan es = writeText (pFiles ports) (journalPath root plan) (T.concat [entryJson e <> "\n" | e <- es])

-- | Store EVENTS not seen before (by key) as pending; the new ones.
record :: Ports -> TimeZone -> FilePath -> FilePath -> [Event] -> IO [Entry]
record ports zone root plan evs = do
  old <- entries ports root plan
  let seen = map (evKey . enEvent) old
      fresh = foldl' (\acc e -> if evKey e `elem` seen || any ((== evKey e) . evKey . enEvent) acc then acc else acc ++ [Entry e Pending (journalLine zone e)]) [] evs
  if null fresh then pure [] else fresh <$ save ports root plan (old ++ fresh)

markAs :: EntryState -> Ports -> FilePath -> FilePath -> [Text] -> IO ()
markAs st ports root plan keys
  | null keys = pure ()
  | otherwise = do
      es <- entries ports root plan
      save ports root plan [if evKey (enEvent e) `elem` keys then e {enState = st} else e | e <- es]

markLanded, markLost :: Ports -> FilePath -> FilePath -> [Text] -> IO ()
markLanded = markAs Landed
markLost = markAs Lost

-- | Where pending ENTRIES go in the plan's lines: one insertion per step,
-- after its last child, lines in event order; the keys placed; the keys
-- whose step is gone.
pendingFacts :: [Line] -> [Entry] -> ([Fact], [Text], [Text])
pendingFacts ls es = (withJournal [] [Insert r xs | (r, xs) <- placed], [k | (k, Just _) <- found], [k | (k, Nothing) <- found])
  where
    found = [(evKey (enEvent e), (\s -> (s, e)) <$> stepOf (enEvent e)) | e <- es, enState e == Pending]
    placed = [(after s, [T.replicate (lnIndent s + 2) " " <> enLine e]) | (_, Just (s, e)) <- found]
    after s = case childrenOf ls (lnRow s) of
      [] -> lnRow s
      kids -> lnRow (last kids)
    stepOf e =
      let want = stepKey (evStep e)
          same l = isStep l && stepKey (lnText l) == want
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

-- | The entry behind a journal line (its text, indent ignored).
entryAt :: [Entry] -> Text -> Maybe Entry
entryAt es t = find ((== T.strip t) . enLine) es

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

-- | A do review answered: CHOICE is kept, reverted or reopened.
keepEvent :: Text -> Text -> Text -> Int -> UTCTime -> Text -> Event
keepEvent rid eid step row at choice = Event ("review|" <> rid <> "||" <> choice) step row at words' (Just eid)
  where
    words' = case choice of
      "reverted" -> "reverted the do's change"
      "reopened" -> "kept the do's change · asked for more"
      _ -> "kept the do's change"

-- | A test run's line for the step at ROW (LS before FACTS): a tick, an
-- untick, a first red or a green, once per fingerprint FP.
testEvents :: [Line] -> Int -> [Fact] -> [((FilePath, Text), TestState)] -> Maybe Text -> UTCTime -> [Event]
testEvents ls row facts states fp at = case drop row ls of
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
