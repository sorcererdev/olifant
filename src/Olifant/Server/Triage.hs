{-# LANGUAGE OverloadedStrings #-}
-- | Triage: what a plan sentence is, and whether the code can settle it.
--
-- The grammar decides explicit lines (a checkbox is a step, "- ? " a
-- question). Jev decides the rest: one request asks the sentence's kind (a
-- choice with no "unsure" option), whether the code can settle it, whether
-- the writer wants it answered, and (with `[plans] web`) whether public
-- sources can settle it (three Nouls). A `- ? ` line asks Jev only whether
-- the code (and public sources) can settle it.
-- The route is code, not Jev, and never buys a check that can only end
-- "could not tell": a claim, step or wanted question something can settle
-- is checked or answered; everything else is stored aside.
module Olifant.Server.Triage
  ( Route (..)
  , Triage (..)
  , kindThreshold
  , aboutThreshold
  , questionAboutThreshold
  , codeSettles
  , byGrammar
  , triageQuestions
  , gradeQuestions
  , fromAnswers
  , noJev
  , route
  , routeName
  , settleable
  , useWeb
  , triage
  , needsRetriage
  , retriage
  , joinQuestion
  , joinThreshold
  , joinsAnswer
  , decideJoins
  ) where

import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Olifant.Decide (Answer (..), Confidence (..), Decider (..), Question (..), Shape (..), choice, noul)
import Olifant.Plans (Kind (Claim, Heading, Step))
import qualified Olifant.Plans as P
import Olifant.Plans.Sentences (Sentence (..), pairKey)

data Route = Verify | Progress | Answer | Asked | Aside
  deriving (Eq, Show)

-- | The kind ("claim" | "question" | "step" | "prose"), its probability, the
-- probability the code can settle it, and who said so ("grammar" | "jev");
-- the probability the writer wants it answered, and that public sources can
-- settle it (Nothing: not asked; about-public is asked only with web on).
data Triage = Triage
  { trKind :: Text, trKindP :: Double, trAboutCode :: Double, trBy :: Text
  , trWantsAnswer :: Maybe Double, trAboutPublic :: Maybe Double }
  deriving (Eq, Show)

-- | The two thresholds a sentence must pass to be checked; tuned on the
-- prose corpus (ProseCorpusSpec), never per call.
kindThreshold, aboutThreshold :: Double
kindThreshold = 0.80
aboutThreshold = 0.70

-- | About-code a question needs to be answered from the code (claims and
-- steps keep 'aboutThreshold'): a question that wants an answer is settled
-- more readily than Jev says ("Do we have raw vote totals stored?" at 0.62).
-- User ruling 2026-11-14, on the question corpus.
questionAboutThreshold :: Double
questionAboutThreshold = 0.50

routeName :: Route -> Text
routeName = \case
  Verify -> "verify"
  Progress -> "progress"
  Answer -> "answer"
  Asked -> "asked"
  Aside -> "aside"

-- | A `- ? ` line with no Jev: answered from the code (the degraded mode).
byGrammar :: Sentence -> Maybe Triage
byGrammar s = case snKind s of
  Step _ -> Just (Triage "step" 1 1 "grammar" Nothing Nothing)
  P.Question -> Just (Triage "question" 1 1 "grammar" (Just 1) Nothing)
  _ -> Nothing

-- | With no Jev: bullets are claims (as before prose plans), the rest prose.
noJev :: Sentence -> Triage
noJev s = case snKind s of
  Claim -> Triage "claim" 1 1 "grammar" Nothing Nothing
  _ -> fromMaybe (Triage "prose" 1 0 "grammar" Nothing Nothing) (byGrammar s)

-- | Something can settle it: the code, or (with web) public sources.
settleable :: Bool -> Triage -> Bool
settleable web t = codeSettles t || useWeb web t

-- | The code settles it: about-code at the kind's threshold (a question's is
-- 'questionAboutThreshold', the rest 'aboutThreshold').
codeSettles :: Triage -> Bool
codeSettles t = trAboutCode t >= (if trKind t == "question" then questionAboutThreshold else aboutThreshold)

-- | A check of this sentence is a web check (web tools, no file tools): web
-- is on, public sources settle it and the code does not. A sentence both
-- settle takes the code check (user ruling 2026-11-14).
useWeb :: Bool -> Triage -> Bool
useWeb web t = web && not (codeSettles t) && maybe False (>= aboutThreshold) (trAboutPublic t)

-- | The route, with @web@ on or off: never a check that can only end "could not tell".
route :: Bool -> Triage -> Route
route web t
  | trKindP t < kindThreshold = Aside
  | trKind t == "question" = if maybe False (>= aboutThreshold) (trWantsAnswer t) && settleable web t then Answer else Asked
  | trKind t == "claim" = if settleable web t then Verify else Aside
  | trKind t == "step" = if trAboutCode t >= aboutThreshold then Progress else Aside
  | otherwise = Aside

-- | The questions, sharing one state: the sentence, the kind of line it
-- sits on, its section heading and its lead (omitted when there is none).
-- Three, and @about-public@ with web on.
triageQuestions :: Bool -> Sentence -> [Question]
triageQuestions web s = [kindQuestion s, aboutQuestion s, wantsQuestion s] ++ [publicQuestion s | web]

-- | What a `- ? ` line asks Jev: its kind and wish are the writer's mark.
gradeQuestions :: Bool -> Sentence -> [Question]
gradeQuestions web s = aboutQuestion s : [publicQuestion s | web]

kindQuestion, aboutQuestion, wantsQuestion, publicQuestion :: Sentence -> Question
kindQuestion s = (choice "sentence-kind" kindInstructions kindOptions (contextOf s)) {qUnsure = False}
aboutQuestion s = (noul "about-code" aboutInstructions (contextOf s)) {qShape = Noul (Just (aboutTrue, aboutFalse))}
wantsQuestion s = (noul "wants-answer" wantsInstructions (contextOf s)) {qShape = Noul (Just (wantsTrue, wantsFalse)), qUnsure = False}
publicQuestion s = (noul "about-public" publicInstructions (contextOf s)) {qShape = Noul (Just (publicTrue, publicFalse)), qUnsure = False}

contextOf :: Sentence -> [(Text, Text)]
contextOf s =
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

wantsInstructions, wantsTrue, wantsFalse :: Text
wantsInstructions = "Does the writer of this software plan want this sentence answered: a real question about the code, the product or another technical matter?"
wantsTrue = "Covers: a real question the writer wants answered, about the code, the product or another technical matter. Examples: \"Do we have raw vote totals from previous years stored?\"; \"Can I be on desk and be staff?\"; \"Does bulk_create return primary keys on Postgres?\""
wantsFalse = "Covers: not a question, or a question nobody wants answered: a request or proposal phrased as a question, a question to a person or the team, a rhetorical question, a question inside a step. Examples: \"Can we put hand entry at the top?\"; \"Should we split this into PRs?\"; \"(and total?)\""

publicInstructions, publicTrue, publicFalse :: Text
publicInstructions = "Can public technical sources settle this sentence: a library's or framework's documentation, a standard, a public API's reference?"
publicTrue = "Covers: what a named library, framework, language, standard or public API does or offers, which its public documentation or release notes settle. Examples: \"htmx 4 drops hx-on\"; \"Does bulk_create return primary keys on Postgres?\""
publicFalse = "Covers: only this project's own code can settle it, or it is about people, opinions, plans or a private document. Examples: \"WebFetcher retries a failed request three times\"; \"We should break these up into small PRs\"; \"The Google sheet has a tab per county\""

aboutInstructions, aboutTrue, aboutFalse :: Text
aboutInstructions = "Can this sentence be settled by reading this project's source code?"
aboutTrue = "can be settled by reading this project's source code: what it contains, how the app behaves for its users, or what changing it would require"
aboutFalse = "is only about people, opinions, plans for future work, external services or documents, and says nothing about what the software does or what changing it takes"

-- | Jev's answers, by question kind. The kind's probability is its share
-- of the distribution (else the answer's confidence); no kind answer, or an
-- unsure one, is prose at zero. A Noul's probability is its yes; a Noul not
-- in the batch is Nothing (not asked).
fromAnswers :: [(Text, (Answer, [(Text, Double)]))] -> Triage
fromAnswers as = case lookup "sentence-kind" as of
  Just (Chosen kind _ conf, dist) ->
    Triage kind (fromMaybe (confOf conf) (lookup kind dist)) (maybe 0 nounP (lookup "about-code" as)) "jev"
      (nounP <$> lookup "wants-answer" as) (nounP <$> lookup "about-public" as)
  _ -> Triage "prose" 0 0 "jev" Nothing Nothing

confOf :: Confidence -> Double
confOf = \case
  Confidence p -> p
  Unknown -> 0

nounP :: (Answer, a) -> Double
nounP (a, _) = case a of
  Yes c -> confOf c
  No (Confidence p) -> 1 - p
  _ -> 0

weighed' :: Decider IO -> [Question] -> IO [(Text, (Answer, [(Text, Double)]))]
weighed' d qs = zip (map qKind qs) <$> weighAll d qs

-- | The grammar first (a step; a `- ? ` line asks Jev only 'gradeQuestions');
-- with no decider, 'noJev'; else one Jev request.
triage :: Bool -> Maybe (Decider IO) -> Sentence -> IO Triage
triage web md s = case (snKind s, md) of
  (P.Question, Just d) -> graded <$> weighed' d (gradeQuestions web s)
  _ -> case byGrammar s of
    Just t -> pure t
    Nothing -> case md of
      Nothing -> pure (noJev s)
      Just d -> fromAnswers <$> weighed' d (triageQuestions web s)
  where
    graded as = Triage "question" 1 (maybe 0 nounP (lookup "about-code" as)) "grammar" (Just 1) (nounP <$> lookup "about-public" as)

-- | A stored triage that lacks a Noul this pass needs: @wants-answer@ (a 2a
-- entry), or @about-public@ with web on. Steps by grammar never.
needsRetriage :: Bool -> Triage -> Bool
needsRetriage web t =
  (trBy t == "jev" || trKind t == "question")
    && (isNothing (trWantsAnswer t) || web && isNothing (trAboutPublic t))

-- | Asks only the missing Nouls, in one request, and keeps the kind, its
-- probability and about-code (a 2a `- ? ` line, never asked about-code by
-- Jev, is asked it now). No decider: a `- ? ` line takes the no-Jev triage
-- (answered from the code); anything else is unchanged.
retriage :: Bool -> Maybe (Decider IO) -> Sentence -> Triage -> IO Triage
retriage web md s t
  | not (needsRetriage web t) = pure t
  | otherwise = case md of
      Nothing
        | grammarQuestion && isNothing (trWantsAnswer t) -> pure (fromMaybe t (byGrammar s))
        | otherwise -> pure t
      Just d
        | null qs -> pure t
        | otherwise -> fill <$> weighed' d qs
  where
    grammarQuestion = trBy t == "grammar" && trKind t == "question"
    qs
      | grammarQuestion && isNothing (trWantsAnswer t) = gradeQuestions web s
      | otherwise = [wantsQuestion s | isNothing (trWantsAnswer t)] ++ [publicQuestion s | web, isNothing (trAboutPublic t)]
    fill as =
      t { trAboutCode = maybe (trAboutCode t) nounP (lookup "about-code" as)
        , trWantsAnswer = if grammarQuestion then Just 1 else maybe (trWantsAnswer t) (Just . nounP) (lookup "wants-answer" as)
        , trAboutPublic = maybe (trAboutPublic t) (Just . nounP) (lookup "about-public" as) }

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
