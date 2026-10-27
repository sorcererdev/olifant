-- | Internal to Olifant.Server: a reconcile's readings as one review (plans
-- UI redesign, A for reconcile). Each reconcile that has readings replaces
-- the plan's `reconcile` review, under the same folder as the `do` reviews;
-- one with none deletes it. A reading keeps a key (its kind, path, unit and
-- fingerprint) so one answered is not offered again by the next reconcile.
-- A reading's plan edit is derived from the plan when it is shown or
-- answered: its step is found again by its text, never at a stale row.
module Olifant.Server.ReviewReadings
  ( Reading (..)
  , ReadingsReview (..)
  , readingsId
  , Choice (..)
  , choiceOf
  , outcomeOf
  , readingKey
  , entryKind
  , readingsReview
  , readingFacts
  , readingRow
  , readingLens
  , applyFactsText
  , readingsPayload
  , answerReading
  , readingsFile
  , saveReadings
  , loadReadings
  , dropReadings
  , replaceReadings
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (Value (..), decodeStrict, encode, object, (.:), (.=))
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Lazy qualified as BL
import Data.List (find, sortOn)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Olifant.App (Ports (..))
import Olifant.Plans (Anchor (..), Kind (..), Line (..), Verdict (..), parsePlan, withVerdict)
import Olifant.Server.Edits (unifiedDiff)
import Olifant.Server.Errand (Block (..), Ghost (..))
import Olifant.Server.LoopTests (checkbox)
import Olifant.Server.LoopTypes
import Olifant.Server.Review (reviewsDir, stepKey)
import Olifant.Workspace (Workspace (..))
import System.FilePath (takeFileName, (</>))

-- | One reading: its kind, its block's index in the reconcile record (an
-- outcome names it), the (path, unit, fingerprint) it is about, the line
-- its block follows (row and text then: the step, or where a new step
-- goes), its lines, for a unit in no step the new step it would be (after
-- that line), and the unit's diff since the baseline.
data Reading = Reading
  { rdKind :: ReadingKind
  , rdBlock :: Int
  , rdUnit :: (FilePath, Text, Text)
  , rdAfter :: (Int, Text)
  , rdLines :: [Text]
  , rdNew :: Maybe ((Int, Text), Text)
  , rdDiff :: Text
  }
  deriving stock (Eq, Show)

-- | A plan's readings: the reconcile record they came from, the plan (root
-- relative), the readings not yet answered, and the keys answered.
data ReadingsReview = ReadingsReview
  { rrErrand :: Text
  , rrPlan :: FilePath
  , rrReadings :: [Reading]
  , rrAnswered :: [Text]
  }
  deriving stock (Eq, Show)

-- | The review's id (one per plan).
readingsId :: Text
readingsId = "reconcile"

-- | What the person answers a reading: apply its edit (a step's new state,
-- its note, or a unit placed where Jev put it), leave it, place a unit
-- under another open step (its row), as a new step, or ignore it.
data Choice = Apply | Leave | Place Int | New | Ignore
  deriving stock (Eq, Show)

-- | A choice from the wire: its name and, for `place`, the step's row.
choiceOf :: Text -> Maybe Int -> Maybe Choice
choiceOf name row = case (name, row) of
  ("apply", _) -> Just Apply
  ("leave", _) -> Just Leave
  ("place", Just r) -> Just (Place r)
  ("new", _) -> Just New
  ("ignore", _) -> Just Ignore
  _ -> Nothing

-- | The outcome record a choice leaves: the truth label for Jev.
outcomeOf :: Choice -> Text
outcomeOf = \case
  Leave -> "rejected"
  Ignore -> "rejected"
  _ -> "accepted"

-- | A reading as an entry's kind: a unit in no step is `unplaced` whether
-- Jev placed it or not.
entryKind :: ReadingKind -> Text
entryKind = \case
  Done -> "done"
  Reopened -> "reopened"
  Diverged -> "changed"
  Gone -> "gone"
  Also -> "unplaced"
  NewStep -> "unplaced"

kindName :: ReadingKind -> Text
kindName = \case
  Done -> "done"
  Reopened -> "reopened"
  Diverged -> "diverged"
  Gone -> "gone"
  Also -> "also"
  NewStep -> "new"

kindNamed :: Text -> Maybe ReadingKind
kindNamed t = find ((== t) . kindName) [Done, Reopened, Diverged, Gone, Also, NewStep]

-- | A reading's key, and its entry's id: "kind|path|unit|fingerprint", and
-- for a step's reading "|" and the step's words (two steps may read one
-- unit; a tick or a verdict does not change the words).
readingKey :: Reading -> Text
readingKey r =
  let (p, u, fp) = rdUnit r
      step = [fst (stepKey (snd (rdAfter r))) | rdKind r `notElem` [Also, NewStep]]
   in T.intercalate "|" ([entryKind (rdKind r), T.pack p, u, fp] ++ step)

-- | The readings of a reconcile's ANSWER for the plan at PLAN, but those
-- whose key is ANSWERED; Nothing when none is left.
readingsReview :: [Text] -> FilePath -> ReconcileAnswer -> Maybe ReadingsReview
readingsReview answered plan a = case [r | r <- zipWith reading [0 ..] (raBlocks a), readingKey r `notElem` answered] of
  [] -> Nothing
  rs -> Just (ReadingsReview (raId a) plan rs answered)
  where
    reading i b =
      let bl = rbBlock b
          cu = rbChange b
          unit = fromMaybe (maybe ("", "", "") (\c -> (cuPath c, cuName c, fromMaybe "" (cuFingerprint c))) cu) (rbUnit b)
          new = rbNewStep b >>= \nb -> (\g -> ((blAfterRow nb, fromMaybe "" (blAfterText nb)), ghText g)) <$> listToMaybe (blLines nb)
          diff = maybe "" (\c -> unifiedDiff (cuPath c) (fromMaybe "" (cuBase c)) (fromMaybe "" (cuNow c))) cu
       in Reading (rbKind b) i unit (blAfterRow bl, fromMaybe "" (blAfterText bl)) (map ghText (blLines bl)) new diff

-- | The line of LS a reading's (row, text) names now: the same text (at
-- that row, else the nearest), else a step with the same words and anchor.
locate :: [Line] -> (Int, Text) -> Maybe Line
locate ls (row, text) = case sortOn (\l -> abs (lnRow l - row)) [l | l <- ls, lnText l == text] of
  l : _ -> Just l
  [] -> case sortOn (\l -> abs (lnRow l - row)) [l | l <- ls, stepLike l, stepKey (lnText l) == stepKey text] of
    l : _ | stepLike' text -> Just l
    _ -> Nothing
  where
    stepLike l = case lnKind l of Step _ -> True; TestChild -> True; _ -> False
    stepLike' t = maybe False stepLike (listToMaybe (parsePlan t))

isOpenStep :: Line -> Bool
isOpenStep l = lnKind l == Step False

-- | The step a reading is about, as a row of LS now (a new step's place is
-- no step: Nothing).
readingRow :: [Line] -> Reading -> Maybe Int
readingRow ls r
  | rdKind r == NewStep = Nothing
  | otherwise = lnRow <$> locate ls (rdAfter r)

-- | The lens a reading puts on its step (a new step's: none; the plan's
-- title counts it).
readingLens :: Reading -> Maybe Text
readingLens r = case rdKind r of
  Done -> Just "looks done \183 review"
  Reopened -> Just "looks open \183 review"
  Diverged -> Just "looks wrong \183 review"
  Gone -> Just "gone \183 review"
  Also -> Just "not in the plan \183 review"
  NewStep -> Nothing

-- | A line written under STEP: indented one level in, stamped with the
-- reading's fingerprint when it anchors the reading's unit and is no open
-- step (the fingerprint says what was read).
childLine :: Reading -> Line -> Text -> Text
childLine r step t = stamped r (T.replicate (lnIndent step + 2) " " <> T.stripStart t)

stamped :: Reading -> Text -> Text
stamped r t =
  let (p, u, fp) = rdUnit r
   in case listToMaybe (parsePlan t) of
        Just l | not (T.null fp), Just an <- lnAnchor l, anPath an == p, anUnit an == u, lnKind l /= Step False -> withVerdict t (Verified fp)
        _ -> t

-- | The plan edit of answering reading R with CHOICE, in the plan's lines
-- LS now; Left when the choice is not one of its offers or its step is
-- gone.
readingFacts :: [Line] -> Reading -> Choice -> Either Text [Fact]
readingFacts ls r choice = case (rdKind r, choice) of
  (_, Leave) | not unplaced -> Right []
  (_, Ignore) | unplaced -> Right []
  (Done, Apply) -> stepNow >>= \s -> Right [Replace (lnRow s) (withVerdict (checkbox "- [ ]" "- [x]" (lnText s)) verdict)]
  (Reopened, Apply) -> stepNow >>= \s -> Right [Replace (lnRow s) (withVerdict (checkbox "- [x]" "- [ ]" (lnText s)) NoVerdict)]
  (k, Apply) | k `elem` [Diverged, Gone, Also] -> stepNow >>= \s -> Right [Insert (lnRow s) (map (childLine r s) (rdLines r))]
  (NewStep, Apply) -> newStep
  (_, New) | unplaced -> newStep
  (_, Place row) | unplaced -> case [l | l <- ls, lnRow l == row, isOpenStep l] of
    s : _ -> Right [Insert row [childLine r s ("- also " <> anchorText)]]
    [] -> Left "that line is no open step"
  _ -> Left "not an offer of this reading"
  where
    unplaced = rdKind r `elem` [Also, NewStep]
    (path, unit, _) = rdUnit r
    anchorText = "@" <> T.pack path <> ":" <> unit
    stepNow = maybe (Left "its step is no longer in the plan") Right (locate ls (rdAfter r))
    verdict = maybe NoVerdict lnVerdict (listToMaybe (rdLines r) >>= listToMaybe . parsePlan)
    -- after its place now, else at the plan's end
    newStep = case rdNew r of
      Nothing -> Left "no new step for this reading"
      Just (at, line) ->
        let row = maybe (lastRow ls) lnRow (locate ls at)
         in Right [Insert row [stamped r line]]
    lastRow xs = maybe 0 lnRow (listToMaybe (reverse [l | l <- xs, not (T.null (T.strip (lnText l)))]))

-- | TEXT with FACTS applied: replacements by row, then each insertion after
-- its row (rows of TEXT).
applyFactsText :: [Fact] -> Text -> Text
applyFactsText facts text = T.intercalate "\n" (concat [line i l | (i, l) <- zip [0 ..] (T.splitOn "\n" text)])
  where
    line i l = fromMaybe l (lookup i [(r, t) | Replace r t <- facts]) : concat [ls | Insert r ls <- facts, r == i]

-- | The review as the window draws it (the pause's shape): a title, a
-- summary, and an entry per reading with its first sentence, its offers,
-- its landing (EDIT of its `a` facts, and the plan's diff over the unit's).
-- TEXT is the plan now.
readingsPayload :: ReadingsReview -> Text -> ([Fact] -> Value) -> Value
readingsPayload rr text edit =
  object
    [ "id" .= readingsId
    , "kind" .= ("reconcile" :: Text)
    , "plan" .= rrPlan rr
    , "title" .= object ["verb" .= ("reconcile" :: Text), "count" .= n]
    , "summary" .= (T.pack (show n) <> (if n == 1 then " reading" else " readings") <> " to review")
    , "entries" .= map entry (rrReadings rr)
    , "details" .= [section "readings" (T.unlines [readingKey r | r <- rrReadings rr]), section "reconcile" (rrErrand rr)]
    ]
  where
    n = length (rrReadings rr)
    ls = parsePlan text
    numbers = zip [lnRow l | l <- ls, isStep l] [1 :: Int ..]
    isStep l = case lnKind l of Step _ -> True; _ -> False
    number row = lookup row numbers
    stepName row = maybe "a step" (\k -> "Step " <> T.pack (show k)) (number row)
    entry r =
      let row = readingRow ls r
          facts = either (const []) id (readingFacts ls r Apply)
          after = applyFactsText facts text
          planDiff = if null facts then "" else unifiedDiff (rrPlan rr) text after
          (path, unit, _) = rdUnit r
       in object
            [ "id" .= readingKey r
            , "kind" .= entryKind (rdKind r)
            , "row" .= row
            , "step" .= (row >>= number)
            , "unit" .= object ["path" .= path, "unit" .= unit]
            , "summary" .= sentence r row
            , "offers" .= offers r row
            , "landing" .= object ["edit" .= (if null facts then Null else edit facts), "diff" .= (planDiff <> (if T.null planDiff || T.null (rdDiff r) then "" else "\n") <> rdDiff r), "facts" .= length facts]
            ]
    sentence r row =
      let (path, unit, _) = rdUnit r
          name = maybe "A step" stepName row
       in case rdKind r of
            Done -> name <> " looks done"
            Reopened -> name <> " looks open again"
            Diverged -> name <> " looks wrong: " <> why r
            Gone -> name <> "'s unit is gone: " <> unit
            _ -> "Not in any step: `" <> unit <> "` in " <> T.pack (takeFileName path)
    why r = case listToMaybe (rdLines r) >>= T.stripPrefix "- ✗ changed: " . T.strip of
      Just w -> T.strip (fst (T.breakOn " @" w))
      Nothing -> "it changed"
    open = take 9 [l | l <- ls, isOpenStep l]
    offers r row = case rdKind r of
      k | k `elem` [Also, NewStep] ->
        [ offer "apply" "a" (if k == Also then "place it under " <> maybe "its step" stepName row else "add it as a new step") (if k == Also then "- also @" <> unitOf r else maybe "" snd (rdNew r)) [] ]
          ++ [offer "place" (T.pack (show i)) ("under " <> stepName (lnRow l)) (T.strip (lnText l)) ["step" .= lnRow l] | (i, l) <- zip [1 :: Int ..] open]
          ++ [offer "new" "n" "a new step" (maybe "" snd (rdNew r)) [] | isJust (rdNew r)]
          ++ [offer "ignore" "i" "not work: ignore it" "until it changes again" []]
      Done -> [offer "apply" "a" "tick it" "the step's line ticked, at the unit now" [], leave "the step stays open"]
      Reopened -> [offer "apply" "a" "reopen it" "the step's tick goes" [], leave "the step stays ticked"]
      Diverged -> [offer "apply" "a" "write the ✗ changed note" (T.strip (fromMaybe "" (listToMaybe (rdLines r)))) [], leave "nothing written"]
      Gone -> [offer "apply" "a" "write the ✗ gone note" (T.strip (fromMaybe "" (listToMaybe (rdLines r)))) [], leave "nothing written"]
      _ -> []
    unitOf r = let (p, u, _) = rdUnit r in T.pack p <> ":" <> u
    leave detail = offer "leave" "r" "leave it" detail []
    offer choice key label detail more = object (["choice" .= (choice :: Text), "key" .= (key :: Text), "label" .= (label :: Text), "detail" .= (detail :: Text)] ++ more)
    section title body = object ["title" .= (title :: Text), "text" .= (body :: Text)]

-- | READING answered: it leaves the review and its key is kept; Nothing
-- when it was the last. A key not in the review leaves it as it is.
answerReading :: ReadingsReview -> Text -> Maybe ReadingsReview
answerReading rr key
  | key `notElem` map readingKey (rrReadings rr) = Just rr
  | otherwise = case filter ((/= key) . readingKey) (rrReadings rr) of
      [] -> Nothing
      rs -> Just rr {rrReadings = rs, rrAnswered = rrAnswered rr ++ [key]}

-- | Where a plan's readings live: beside its `do` reviews.
readingsFile :: FilePath -> FilePath -> FilePath
readingsFile root plan = reviewsDir root plan </> T.unpack readingsId <> ".json"

saveReadings :: Ports -> FilePath -> ReadingsReview -> IO ()
saveReadings ports root rr = writeText (pFiles ports) (readingsFile root (rrPlan rr)) (TE.decodeUtf8 (BL.toStrict (encode (record rr))) <> "\n")

loadReadings :: Ports -> FilePath -> FilePath -> IO (Maybe ReadingsReview)
loadReadings ports root plan = (>>= parseReadings) <$> readText (pFiles ports) (readingsFile root plan)

dropReadings :: Ports -> FilePath -> FilePath -> IO ()
dropReadings ports root plan = void (try (removeFile (pFiles ports) (readingsFile root plan)) :: IO (Either SomeException ()))

-- | A reconcile's ANSWER replaces the plan's readings (the keys answered
-- carried over); none left, the file goes.
replaceReadings :: Ports -> FilePath -> FilePath -> ReconcileAnswer -> IO (Maybe ReadingsReview)
replaceReadings ports root plan a = do
  old <- loadReadings ports root plan
  case readingsReview (maybe [] rrAnswered old) plan a of
    Nothing -> Nothing <$ dropReadings ports root plan
    Just rr -> Just rr <$ saveReadings ports root rr

record :: ReadingsReview -> Value
record rr =
  object
    [ "id" .= readingsId, "errand" .= rrErrand rr, "plan" .= rrPlan rr, "answered" .= rrAnswered rr
    , "readings" .= map reading (rrReadings rr) ]
  where
    reading r =
      let (p, u, fp) = rdUnit r
       in object
            [ "kind" .= kindName (rdKind r), "block" .= rdBlock r, "path" .= p, "unit" .= u, "fp" .= fp
            , "after" .= fst (rdAfter r), "after_text" .= snd (rdAfter r), "lines" .= rdLines r
            , "new" .= fmap (\((row, t), line) -> object ["after" .= row, "after_text" .= t, "line" .= line]) (rdNew r)
            , "diff" .= rdDiff r ]

parseReadings :: Text -> Maybe ReadingsReview
parseReadings txt = decodeStrict (TE.encodeUtf8 txt) >>= parseMaybe p
  where
    p :: Value -> Parser ReadingsReview
    p = \case
      Object o -> ReadingsReview <$> o .: "errand" <*> o .: "plan" <*> (mapMaybe id <$> (mapM reading =<< o .: "readings")) <*> o .: "answered"
      _ -> fail "readings are an object"
    reading = \case
      Object o -> do
        kind <- kindNamed <$> o .: "kind"
        r <- Reading Done <$> o .: "block" <*> ((,,) <$> o .: "path" <*> o .: "unit" <*> o .: "fp")
          <*> ((,) <$> o .: "after" <*> o .: "after_text") <*> o .: "lines"
          <*> (traverse new =<< o .: "new") <*> o .: "diff"
        pure ((\k -> r {rdKind = k}) <$> kind)
      _ -> fail "a reading is an object"
    new = \case
      Object o -> (\a t l -> ((a, t), l)) <$> o .: "after" <*> o .: "after_text" <*> o .: "line"
      _ -> fail "new"
