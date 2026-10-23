-- | The question Jev is asked after a red round of a conjure, and the pure rules around
-- it (spec: steered-conjure-design.md §2, §3).
module Olifant.Server.ConjureNext
  ( RoundView (..)
  , roundView
  , attemptDiff
  , undefinedNames
  , roundSummary
  , Standing (..)
  , Offer (..)
  , Next (..)
  , conjureNextGate
  , offersFor
  , offerText
  , spent
  , conjureNextQuestion
  , nextFrom
  , withHint
  ) where

import Data.List (nub)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Decide (Answer (..), Question, choice, confident)
import Olifant.Generation.Steer
import Olifant.Language
import Olifant.Model (Escalate (..), Rung (..))
import Text.Regex.TDFA ((=~))

data RoundView = RoundView
  { rvRound :: Int
  , rvRung :: Text
  , rvAttempt :: Text
  , rvFailing :: [(Text, Text)]
  , rvPassing :: Int
  , rvPassingDelta :: Maybe Int
  , rvCheck :: Maybe Text
  , rvRepeat :: Bool
  }
  deriving stock (Eq, Show)

firstLines :: Int -> Text -> Text
firstLines n = T.intercalate "\n" . take n . T.lines

firstLine :: Text -> Text
firstLine = firstLines 1

-- | The previous view, the round; built from the first hole. A passing delta needs a test
-- verdict in both rounds (no check on either).
roundView :: Maybe RoundView -> RoundState -> RoundView
roundView prev rs =
  RoundView
    { rvRound = rsRound rs
    , rvRung = rsRung rs
    , rvAttempt = attempt
    , rvFailing = failing
    , rvPassing = passing
    , rvPassingDelta = delta
    , rvCheck = check
    , rvRepeat = maybe False (\p -> rvFailing p == failing && rvCheck p == check) prev
    }
  where
    hole = case rsHoles rs of
      h : _ -> h
      [] -> RoundHole "" "" [] Nothing
    why = firstLines 3 (rhWhy hole)
    (failing, passing, check) = case rhRun hole of
      Nothing -> ([], 0, Just why)
      Just r -> case trBroken r of
        Just b -> ([], length (trPassed r), Just (firstLines 3 b))
        Nothing
          | null (trFailed r) -> ([], length (trPassed r), Just why)
          | otherwise -> ([(i, firstLine (fMessage f)) | (i, f) <- trFailed r], length (trPassed r), Nothing)
    delta = case prev of
      Just p | check == Nothing, rvCheck p == Nothing -> Just (passing - rvPassing p)
      _ -> Nothing
    attempt = case reverse (map fst (rhPrevious hole)) of
      new : old : _ -> attemptDiff (Just old) new
      [new] -> attemptDiff Nothing new
      [] -> "none"

-- | The lines that changed between two attempts, with the first changed line's number.
attemptDiff :: Maybe Text -> Text -> Text
attemptDiff Nothing new = T.intercalate "\n" (map ("+ " <>) (T.lines new))
attemptDiff (Just old) new
  | old == new = "(the same as the previous attempt)"
  | otherwise = T.intercalate "\n" (("@@ line " <> T.pack (show (pre + 1))) : map ("- " <>) oldMid <> map ("+ " <>) newMid)
  where
    os = T.lines old
    ns = T.lines new
    pre = length (takeWhile id (zipWith (==) os ns))
    os' = drop pre os
    ns' = drop pre ns
    suf = length (takeWhile id (zipWith (==) (reverse os') (reverse ns')))
    oldMid = take (length os' - suf) os'
    newMid = take (length ns' - suf) ns'

-- | Names a checker or runner says are undefined (Python's NameError, pyright), in order.
undefinedNames :: Text -> [Text]
undefinedNames t = nub [n | m <- matches, n <- take 1 (filter (not . T.null) (drop 1 m))]
  where
    matches = t =~ ("name '([^']+)' is not defined|\"([^\"]+)\" is not defined" :: Text) :: [[Text]]

roundSummary :: RoundView -> Text
roundSummary v = case rvFailing v of
  (i, msg) : _ -> "red: " <> snd (T.breakOnEnd "::" i) <> " \183 " <> msg
  [] -> "red: " <> maybe "" firstLine (rvCheck v)

data Standing = Standing {stRoundsLeft :: Int, stSecondsLeft :: Double, stNext :: Maybe Rung, stHint :: Maybe Text}

data Offer = OfferContinue | OfferClimb Text | OfferStop
  deriving stock (Eq, Show)

data Next = NextAgain | NextClimb Text | NextPause Text [Offer]
  deriving stock (Eq, Show)

conjureNextGate :: Double
conjureNextGate = 0.6

-- | What the person may do at a pause. A `never` rung is still offered to the person.
offersFor :: Standing -> [Offer]
offersFor st = [OfferContinue] ++ [OfferClimb n | Just (Rung n _) <- [stNext st]] ++ [OfferStop]

offerText :: Offer -> Text
offerText = \case
  OfferContinue -> "continue"
  OfferClimb n -> "climb:" <> n
  OfferStop -> "stop"

-- | Rounds or time gone: a pause with no call to Jev.
spent :: Standing -> Maybe Next
spent st
  | stRoundsLeft st <= 0 || stSecondsLeft st <= 0 = Just (NextPause "budget spent" (offersFor st))
  | otherwise = Nothing

conjureNextQuestion :: Standing -> RoundView -> Question
conjureNextQuestion st v = choice "conjure-next" instructions options context
  where
    instructions =
      "A conjure is red after this round. Is it converging? The same failure twice (repeat: yes) is not; a smaller failing set, or a different and later failure, is. Choose again while it converges, climb when this model is stuck and a stronger one is offered, ask when the person should look."
    options =
      [("again", "try once more with the same model")]
        ++ [("climb", "try the next, stronger model: " <> n) | Just (Rung n e) <- [stNext st], e /= EscalateNever]
        ++ [("ask", "stop and show the person where this stands")]
    context =
      [ ("round", num (rvRound v))
      , ("rung", rvRung v)
      , ("attempt", rvAttempt v)
      , ("failing", if null (rvFailing v) then "none" else T.intercalate "\n" [i <> ": " <> m | (i, m) <- rvFailing v])
      , ("passing", num (rvPassing v) <> maybe "" (\d -> " (" <> (if d >= 0 then "+" else "") <> num d <> ")") (rvPassingDelta v))
      , ("check", fromMaybe "none" (rvCheck v))
      , ("repeat", if rvRepeat v then "yes" else "no")
      , ("rounds_left", num (stRoundsLeft st))
      , ("seconds_left", num (round (stSecondsLeft st) :: Int))
      , ("hint", fromMaybe "none" (stHint st))
      ]
    num :: Int -> Text
    num = T.pack . show

-- | What to do after Jev's answer; anything not confident, or not an offered option, is again.
nextFrom :: Standing -> RoundView -> Answer -> Next
nextFrom st v a = case a of
  Chosen o _ _ | confident conjureNextGate a -> case o of
    "climb" -> case stNext st of
      Just (Rung n EscalateAuto) -> NextClimb n
      Just (Rung n EscalateAsk) -> NextPause ("Jev would climb to " <> n) (offersFor st)
      _ -> NextAgain
    "ask" -> NextPause (if rvRepeat v then "the same failure twice" else "Jev asks you to look") (offersFor st)
    _ -> NextAgain
  _ -> NextAgain

-- | The person's words, added to the why of each hole's last attempt.
withHint :: Text -> Seed -> Seed
withHint h s = s {seedPrevious = M.map addTo (seedPrevious s)}
  where
    addTo [] = []
    addTo xs = init xs ++ [(\(f, w) -> (f, w <> "\nThe person says: " <> h)) (last xs)]
