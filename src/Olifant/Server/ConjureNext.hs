-- | The question Jev is asked after a red round of a conjure, and the pure rules around
-- it (spec: steered-conjure-design.md §2, §3).
module Olifant.Server.ConjureNext
  ( RoundView (..)
  , RoundContext (..)
  , testSource
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
  , PausedConjure (..)
  , pausedLifetime
  , pruned
  ) where

import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Decide (Answer (..), Question (..), choice, confident)
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
  , rvTests :: [(Text, Text)]
  , rvUnit :: Text
  , rvWired :: [Text]
  }
  deriving stock (Eq, Show)

-- | A test's source (it lives here, not in Site, which would import this module in a cycle): its def line and its body, trailing blank lines dropped.
testSource :: Text -> Text -> Maybe Text
testSource name text = case dropWhile (not . isDef) (T.lines text) of
  [] -> Nothing
  d : rest -> Just (T.intercalate "\n" (d : reverse (dropWhile (T.null . T.strip) (reverse (takeWhile (\l -> T.null (T.strip l) || indentOf l > indentOf d) rest)))))
  where
    isDef l = ("def " <> name <> "(") `T.isPrefixOf` T.strip l
    indentOf = T.length . T.takeWhile (== ' ')

firstLines :: Int -> Text -> Text
firstLines n = T.intercalate "\n" . take n . T.lines

firstLine :: Text -> Text
firstLine = firstLines 1

-- | What the round's view needs beyond the round: the test file's text, the unit's header,
-- what is wired, and whether a wire may still run.
data RoundContext = RoundContext {rcTestText :: Text, rcUnit :: Text, rcWired :: [Text], rcWireable :: Bool}
  deriving stock (Eq, Show)

-- | The previous view, the round; built from the first hole. A passing delta needs a test
-- verdict in both rounds (no check on either).
roundView :: RoundContext -> Maybe RoundView -> RoundState -> RoundView
roundView ctx prev rs =
  RoundView
    { rvRound = rsRound rs
    , rvRung = rsRung rs
    , rvAttempt = attempt
    , rvFailing = failing
    , rvPassing = passing
    , rvPassingDelta = delta
    , rvCheck = check
    , rvRepeat = maybe False (\p -> rvFailing p == failing && rvCheck p == check) prev
    , rvTests = [(i, fromMaybe "(source not found)" (testSource (snd (T.breakOnEnd "::" i)) (rcTestText ctx))) | (i, _) <- failing]
    , rvUnit = rcUnit ctx
    , rvWired = rcWired ctx
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

data Standing = Standing {stRoundsLeft :: Int, stSecondsLeft :: Double, stNext :: Maybe Rung, stHint :: Maybe Text, stWireable :: Bool}

data Offer = OfferContinue | OfferClimb Text | OfferStop | OfferReconjure Text | OfferAccept | OfferReject
  deriving stock (Eq, Show)

data Next = NextAgain | NextClimb Text | NextPause Text [Offer] | NextWire Text Text
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
  OfferReconjure i -> "reconjure:" <> i
  OfferAccept -> "accept"
  OfferReject -> "reject"

-- | Rounds or time gone: a pause with no call to Jev.
spent :: Standing -> Maybe Next
spent st
  | stRoundsLeft st <= 0 || stSecondsLeft st <= 0 = Just (NextPause "budget spent" (offersFor st))
  | otherwise = Nothing

conjureNextQuestion :: Standing -> RoundView -> Question
conjureNextQuestion st v = (choice "conjure-next" instructions options context) {qFields = [("param", "ident"), ("port", "ident"), ("test", "text"), ("why", "text")]}
  where
    instructions =
      "A conjure is red after this round. Is it converging? The same failure twice (repeat: yes) is not; a smaller failing set, or a different and later failure, is. Choose again while it converges, climb when this model is stuck and a stronger one is offered, ask when the person should look. Choose port when a failing test names a collaborator the class lacks (fill param and port with the constructor argument and its type). Choose test when a failing test cannot be satisfied by any implementation (fill test with its id and why with the reason)."
    options =
      [("again", "try once more with the same model")]
        ++ [("climb", "try the next, stronger model: " <> n) | Just (Rung n e) <- [stNext st], e /= EscalateNever]
        ++ [("ask", "stop and show the person where this stands")]
        ++ [("port", "wire a missing port into the class, then go again") | stWireable st]
        ++ [("test", "a failing test is wrong: offer to re-conjure it")]
    context =
      [ ("round", num (rvRound v))
      , ("rung", rvRung v)
      , ("attempt", rvAttempt v)
      , ("failing", if null (rvFailing v) then "none" else T.intercalate "\n" [i <> ": " <> m | (i, m) <- rvFailing v])
      , ("passing", num (rvPassing v) <> maybe "" (\d -> " (" <> (if d >= 0 then "+" else "") <> num d <> ")") (rvPassingDelta v))
      , ("check", fromMaybe "none" (rvCheck v))
      , ("tests", if null (rvTests v) then "none" else T.intercalate "\n\n" [i <> ":\n" <> src | (i, src) <- rvTests v])
      , ("unit", if T.null (rvUnit v) then "none" else rvUnit v)
      , ("wired", if null (rvWired v) then "none" else T.intercalate "\n" (rvWired v))
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
  Chosen o fs _ | confident conjureNextGate a -> case o of
    "climb" -> case stNext st of
      Just (Rung n EscalateAuto) -> NextClimb n
      Just (Rung n EscalateAsk) -> NextPause ("Jev would climb to " <> n) (offersFor st)
      _ -> NextAgain
    "ask" -> NextPause (fromMaybe (if rvRepeat v then "the same failure twice" else "Jev asks you to look") (field "why" fs)) (offersFor st)
    "port" -> case (field "param" fs, field "port" fs) of
      (Just p, Just t)
        | stWireable st -> NextWire p t
        | otherwise -> NextPause ("needs a port the class lacks: " <> p <> ": " <> t <> "; no wire generator applies here") (offersFor st)
      _ -> NextPause "Jev asks you to look" (offersFor st)
    "test" -> case field "test" fs of
      Just t | t `elem` map fst (rvFailing v) -> NextPause (fromMaybe "a test cannot be satisfied" (field "why" fs)) (offersFor st ++ [OfferReconjure t])
      _ -> NextPause "Jev asks you to look" (offersFor st)
    _ -> NextAgain
  _ -> NextAgain
  where
    field k fs = case lookup k fs of
      Just x | not (T.null (T.strip x)) -> Just x
      _ -> Nothing

-- | The person's words, added to the why of each hole's last attempt.
withHint :: Text -> Seed -> Seed
withHint h s = s {seedPrevious = M.map addTo (seedPrevious s)}
  where
    addTo [] = []
    addTo xs = init xs ++ [(\(f, w) -> (f, w <> "\nThe person says: " <> h)) (last xs)]

-- | A conjure paused, as the server keeps it under its token: the file (absolute) and its
-- version when the conjure began, where it was asked (the line, the selection), where the
-- loop resumes (the seed), the rung's index, the rungs used, the rounds and seconds used since
-- the last grant, the last round's view, the person's hint, the file as the last attempt
-- writes it (what stop lands), the names the last check left undefined, when it paused
-- (monotonic seconds); the conjure's working writes beside the unit's (root-relative path →
-- text: a wire's), each written file's editor version (absolute path; none when it was not
-- open) and its text when the conjure first read it (none: a new file), what is wired, and
-- the pause's explanation.
data PausedConjure = PausedConjure
  { pdFile :: FilePath
  , pdVersion :: Maybe Int
  , pdLine :: Int
  , pdSelected :: Maybe (Int, Int)
  , pdSeed :: Seed
  , pdRung :: Int
  , pdRungs :: [Text]
  , pdRounds :: Int
  , pdSeconds :: Double
  , pdLast :: RoundView
  , pdHint :: Maybe Text
  , pdLanding :: Text
  , pdUndefined :: [Text]
  , pdAt :: Double
  , pdWrites :: Map FilePath Text
  , pdVersions :: Map FilePath (Maybe Int)
  , pdBases :: Map FilePath (Maybe Text)
  , pdWired :: [Text]
  , pdExplanation :: Text
  }
  deriving stock (Eq, Show)

-- | Seconds a paused conjure is kept.
pausedLifetime :: Double
pausedLifetime = 600

-- | The paused conjures still good at NOW: none older than 'pausedLifetime', none whose file
-- the editor has at another version than when it paused (DOCS: version and text by path).
-- Only the unit's file is checked here; the other files it wrote are checked at resume.
pruned :: Double -> Map FilePath (Int, Text) -> Map Text PausedConjure -> Map Text PausedConjure
pruned now docs = M.filter good
  where
    good pd = now - pdAt pd <= pausedLifetime && (fst <$> M.lookup (pdFile pd) docs) == pdVersion pd
