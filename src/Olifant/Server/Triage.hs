{-# LANGUAGE OverloadedStrings #-}
-- | Triage: what a plan sentence is, and whether the code can settle it.
--
-- The grammar decides explicit lines (a checkbox is a step, "- ? " a
-- question). Jev decides the rest: one request asks the sentence's kind (a
-- choice with no "unsure" option) and whether it is about the code (a Noul).
-- The route is code, not Jev: a claim or step sure enough on both counts is
-- checked; everything else is stored aside.
module Olifant.Server.Triage
  ( Route (..)
  , Triage (..)
  , kindThreshold
  , aboutThreshold
  , byGrammar
  , triageQuestions
  , fromAnswers
  , noJev
  , route
  , routeName
  , triage
  , joinQuestion
  , joinThreshold
  , joinsAnswer
  , decideJoins
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Olifant.Decide (Answer (..), Confidence (..), Decider (..), Question (..), Shape (..), choice, noul)
import Olifant.Plans (Kind (Claim, Heading, Step))
import qualified Olifant.Plans as P
import Olifant.Plans.Sentences (Sentence (..), pairKey)

data Route = Verify | Progress | Asked | Aside
  deriving (Eq, Show)

-- | The kind ("claim" | "question" | "step" | "prose"), its probability, the
-- probability the code can settle it, and who said so ("grammar" | "jev").
data Triage = Triage { trKind :: Text, trKindP :: Double, trAboutCode :: Double, trBy :: Text }
  deriving (Eq, Show)

-- | The two thresholds a sentence must pass to be checked; tuned on the
-- prose corpus (ProseCorpusSpec), never per call.
kindThreshold, aboutThreshold :: Double
kindThreshold = 0.80
aboutThreshold = 0.70

routeName :: Route -> Text
routeName = \case
  Verify -> "verify"
  Progress -> "progress"
  Asked -> "asked"
  Aside -> "aside"

byGrammar :: Sentence -> Maybe Triage
byGrammar s = case snKind s of
  Step _ -> Just (Triage "step" 1 1 "grammar")
  P.Question -> Just (Triage "question" 1 0 "grammar")
  _ -> Nothing

-- | With no Jev: bullets are claims (as before prose plans), the rest prose.
noJev :: Sentence -> Triage
noJev s = case snKind s of
  Claim -> Triage "claim" 1 1 "grammar"
  _ -> fromMaybe (Triage "prose" 1 0 "grammar") (byGrammar s)

route :: Triage -> Route
route t
  | trKindP t < kindThreshold = Aside
  | trKind t == "question" = Asked
  | trAboutCode t < aboutThreshold = Aside
  | trKind t == "claim" = Verify
  | trKind t == "step" = Progress
  | otherwise = Aside

-- | The two questions, sharing one state: the sentence, the kind of line it
-- sits on, its section heading and its lead (omitted when there is none).
triageQuestions :: Sentence -> [Question]
triageQuestions s =
  [ (choice "sentence-kind" kindInstructions kindOptions context) {qUnsure = False}
  , (noul "about-code" aboutInstructions context) {qShape = Noul (Just (aboutTrue, aboutFalse))}
  ]
  where
    context =
      [("sentence", snText s), ("line", lineForm (snKind s)), ("heading", if snHeading s == "" then "(none)" else snHeading s)]
        ++ [("lead", l) | Just l <- [snLead s]]

lineForm :: Kind -> Text
lineForm = \case
  Heading _ -> "a heading"
  Claim -> "a bullet"
  P.Question -> "a question bullet"
  Step _ -> "a checkbox step"
  _ -> "a paragraph or numbered line"

kindInstructions :: Text
kindInstructions = "What kind of sentence is this, in a software plan its author writes in prose about their own codebase?"

kindOptions :: [(Text, Text)]
kindOptions =
  [ ("claim", "Covers: a statement that something is or is not so about the software now, which reading the code could confirm or refute: what the code contains, how the app behaves when someone uses it (what happens after a click, what is kept or lost), or what fixing something would take (whether it needs a code change and a redeploy). Does not cover: what to do, wishes, opinions, estimates of effort or cost, or facts about people, documents or outside services. Examples: \"There is no AI in the codebase yet.\"; \"WebFetcher retries a failed request three times\"; \"Saving the form clears the search box.\"")
  , ("question", "Covers: the author asks something. Does not cover: a statement or a task phrased with a question mark at the end of a request to do it. Examples: \"Do we have raw vote totals from previous years stored?\"; \"Is the 2018 results model not in admin?\"")
  , ("step", "Covers: says what to do or what was done: a task, change, proposal or idea for work. Does not cover: how the code is now, or opinions about the work. Examples: \"Add D and R fields to Comparison 2018\"; \"Make sure these new fields don't break the app if the data is missing.\"")
  , ("prose", "Covers: opinion, estimate of effort or cost, process, context, narration or a wish the code cannot settle. Does not cover: statements about how the code is now, or tasks. Examples: \"We should break these up into small individual PRs\"; \"Having AI scrape specific gov websites at a regular cadence would not be difficult or expensive.\"")
  ]

aboutInstructions, aboutTrue, aboutFalse :: Text
aboutInstructions = "Can this sentence be settled by reading this project's source code?"
aboutTrue = "can be settled by reading this project's source code: what it contains, how the app behaves for its users, or what changing it would require"
aboutFalse = "is only about people, opinions, plans for future work, external services or documents, and says nothing about what the software does or what changing it takes"

-- | Jev's two answers (kind, about-code), in that order. The kind's
-- probability is its share of the distribution (else the answer's
-- confidence); an unsure batch is prose at zero.
fromAnswers :: [(Answer, [(Text, Double)])] -> Triage
fromAnswers as = case as of
  [(k, dist), (a, _)] -> case k of
    Chosen kind _ conf -> Triage kind (fromMaybe (confOf conf) (lookup kind dist)) (about a) "jev"
    _ -> unsure
  _ -> unsure
  where
    unsure = Triage "prose" 0 0 "jev"
    confOf = \case
      Confidence p -> p
      Unknown -> 0
    about = \case
      Yes c -> confOf c
      No (Confidence p) -> 1 - p
      _ -> 0

-- | The grammar first; with no decider, 'noJev'; else one Jev request.
triage :: Maybe (Decider IO) -> Sentence -> IO Triage
triage md s = case byGrammar s of
  Just t -> pure t
  Nothing -> case md of
    Nothing -> pure (noJev s)
    Just d -> fromAnswers <$> weighAll d (triageQuestions s)

-- | The probability Jev must give "the second line continues" for two rows to be joined.
joinThreshold :: Double
joinThreshold = 0.70

-- | Does the row below continue the sentence of the row above (a
-- hard-wrapped paragraph), or start a new thought? No unsure option.
joinQuestion :: Text -> Text -> Question
joinQuestion upper next =
  (noul "line-continues" joinInstructions [("first line", upper), ("second line", next)])
    { qShape = Noul (Just (joinTrue, joinFalse)), qUnsure = False }

joinInstructions, joinTrue, joinFalse :: Text
joinInstructions = "Two consecutive lines of a plan written in prose; the author usually writes one thought per line and often leaves out the full stop. Does the second line continue the sentence the first line started (the first was wrapped mid-sentence)?"
joinTrue = "the first line stops mid-sentence (it ends on a word that needs more: an article, a preposition, a conjunction, an auxiliary verb, an unfinished clause) and the second line completes that same sentence"
joinFalse = "the first line is a complete thought on its own, even without a full stop, and the second line starts another thought"

-- | Jev's answer to 'joinQuestion': joined at 'joinThreshold' or more.
joinsAnswer :: (Answer, [(Text, Double)]) -> Bool
joinsAnswer (a, _) = case a of
  Yes (Confidence p) -> p >= joinThreshold
  No (Confidence p) -> 1 - p >= joinThreshold
  _ -> False

-- | The join answers for CANDIDATES ('joinCandidates' rows), by 'pairKey':
-- one request a pair (a Jev batch shares one state, and every pair has its
-- own lines), asked while MORE says so. An unsure answer (Jev failed or
-- would not say) is left out, so the pair is asked again next time; no
-- Jev decides nothing (no row joins).
decideJoins :: Maybe (Decider IO) -> IO Bool -> [(Int, Text, Text)] -> IO [(Text, Bool)]
decideJoins md more cands = case md of
  Nothing -> pure []
  Just d -> go d cands
  where
    go _ [] = pure []
    go d ((_, u, n) : rest) =
      more >>= \case
        False -> pure []
        True -> do
          as <- weighAll d [joinQuestion u n]
          let kept = [(pairKey u n, joinsAnswer a) | a@(ans, _) : _ <- [as], definite ans]
          (kept ++) <$> go d rest
    definite = \case
      Yes _ -> True
      No _ -> True
      _ -> False
