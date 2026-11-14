{-# LANGUAGE OverloadedStrings #-}
-- | `[plans] check_model = "cascade"` (plans m2c): haiku checks first, Jev
-- guards its verdict, and sonnet checks again only on doubt. The guards are
-- three Nouls in one Jev request over the sentence, the kind of check, the
-- verdict, the reason (and answer) and the evidence: `why-fits` (the reason
-- speaks to what the sentence states), `evidence-fits` (the cited code is
-- what the sentence is about; for a web check, which cites no code, the
-- pages are the named library's own documentation for the version named)
-- and `verdict-follows` (the verdict follows from the reason). The
-- escalation is code, not Jev ('escalation'). Measured on the prose corpus
-- (experiments/cascade-2026-11): at 0.60 every bar held.
module Olifant.Server.Cascade
  ( cascadeModel
  , cascadeFirst
  , cascadeStrong
  , cascadeThreshold
  , isCascade
  , guardQuestions
  , guardEvidence
  , guardScores
  , escalation
  ) where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..), PlansConfig (..))
import Olifant.Decide (Answer (..), Confidence (..), Question (..), Shape (..), noul)
import Olifant.Language.Adapter (languageFor)
import Olifant.Plans.Sentences (Sentence (..))
import Olifant.Server.Checker (CheckAsk (..), CheckMode (..), Citation (..), Confirmed (..), Judgement (..), Source (..))
import Olifant.Server.Confine (confined, safely)
import Olifant.Server.State (ServerState (..))
import Olifant.Server.Unit (Named (..), unitLookupIn, unitsWithKinds)
import Olifant.Workspace (Workspace (..), normalisePath)

-- | The `check_model` that turns the cascade on, and the two models it runs.
cascadeModel, cascadeFirst, cascadeStrong :: Text
cascadeModel = "cascade"
cascadeFirst = "haiku"
cascadeStrong = "sonnet"

-- | A guard under this sends the check to 'cascadeStrong'. 0.60 is the one
-- threshold of 0.6, 0.7 and 0.8 that met every bar on the corpus (0.70 missed
-- the cost bar by $0.0004 a check).
cascadeThreshold :: Double
cascadeThreshold = 0.60

isCascade :: PlansConfig -> Bool
isCascade cfg = pcCheckModel cfg == cascadeModel

-- | The three guards, sharing one state (one Jev request).
guardQuestions :: CheckAsk -> Sentence -> Confirmed -> Text -> [Question]
guardQuestions ask s c evidence =
  [ (noul "why-fits" whyInstructions ctx) {qShape = Noul (Just (whyTrue, whyFalse)), qUnsure = False}
  , (noul "evidence-fits" (if web then evWebInstructions else evCodeInstructions) ctx)
      {qShape = Noul (Just (if web then (evWebTrue, evWebFalse) else (evCodeTrue, evCodeFalse))), qUnsure = False}
  , (noul "verdict-follows" followsInstructions ctx) {qShape = Noul (Just (followsTrue, followsFalse)), qUnsure = False} ]
  where
    web = ckWeb ask
    ctx =
      [ ("sentence", snText s)
      , ("check", checkWords (ckMode ask) web)
      , ("verdict", verdictWords (cfJudgement c))
      , ("why", T.unwords (catMaybes [("Answer: " <>) <$> cfAnswer c, ("Detail: " <>) <$> cfDetail c, Just ("Reason: " <> cfWhy c)]))
      , ("evidence", if T.null evidence then "(nothing cited)" else evidence) ]

-- | What the guards are shown as evidence: a web check's pages (title and
-- address); a code check's citations (up to three), each `path:Unit` with
-- the unit's first 12 lines (else 12 from the cited line, else the file's
-- first 12), read from the workspace.
guardEvidence :: ServerState -> Project -> CheckAsk -> Confirmed -> IO Text
guardEvidence s p ask c
  | ckWeb ask = pure (T.intercalate "\n" [soTitle x <> " <" <> soUrl x <> ">" | x <- cfSources c])
  | otherwise = T.intercalate "\n\n" . catMaybes <$> mapM one (take 3 (cfEvidence c))
  where
    one ci = case confined (ciPath ci) of
      Nothing -> pure Nothing
      Just rel0 -> do
        let rel = normalisePath rel0
        safely Nothing (readText (prWorkspace p) rel) >>= \case
          Nothing -> pure Nothing
          Just t -> do
            units <- case languageFor (catalogLanguages (prCatalog p)) rel of
              Just lang -> safely [] (either (const []) id <$> unitsWithKinds (pSyntax (ssPorts s)) lang (Just rel) t)
              Nothing -> pure []
            let byName = ciUnit ci >>= \u -> either (const Nothing) Just (unitLookupIn u units)
                start = case (byName, ciLine ci) of
                  (Just n, _) -> fst (namedLines n)
                  (_, Just l) -> l
                  _ -> 1
                label = T.pack rel <> maybe "" (":" <>) (ciUnit ci) <> maybe "" (\l -> " line " <> T.pack (show l)) (ciLine ci)
            pure (Just (label <> "\n" <> T.unlines (take 12 (drop (max 0 (start - 1)) (T.lines t)))))

-- | Each guard's yes, in 'guardQuestions' order (an unsure answer is 0).
guardScores :: [Answer] -> [(Text, Double)]
guardScores as = zip ["why-fits", "evidence-fits", "verdict-follows"] (map yes as ++ replicate (3 - length as) 0)
  where
    yes = \case
      Yes (Confidence x) -> x
      No (Confidence x) -> 1 - x
      _ -> 0

-- | Why the first check goes to 'cascadeStrong', or Nothing to keep it: a
-- citation that failed, a cannot_tell, or a guard under the threshold.
escalation :: Double -> Confirmed -> [(Text, Double)] -> Maybe Text
escalation t c scores
  | cfJudgement c == JCannotTell = Just (if cfWhy c == "cited code not found" then "citation" else "cannot_tell")
  | null low = Nothing
  | otherwise = Just (T.intercalate "+" low)
  where
    low = [n | (n, v) <- scores, v < t]

checkWords :: CheckMode -> Bool -> Text
checkWords mode web = case (mode, web) of
  (CheckClaim, False) -> "a statement checked against the project's code"
  (CheckClaim, True) -> "a statement checked against public web sources (the checker saw no code)"
  (CheckProgress, _) -> "a step of work checked for how far the project's code has got with it"
  (CheckAnswer, False) -> "a question answered from the project's code"
  (CheckAnswer, True) -> "a question answered from public web sources (the checker saw no code)"

verdictWords :: Judgement -> Text
verdictWords = \case
  JRight -> "right: what the sentence states is true"
  JWrong -> "wrong: what the sentence states is false"
  JDone -> "done: the code already does all of what the step says"
  JOpen -> "open: the code does not do it yet, and doing it as written would get what the step says"
  JMisfit -> "misfit: the step rests on something that is not so in the code, or doing it as written would not get what it says"
  JAnswered -> "answered: the answer below answers the question"
  JCannotTell -> "cannot_tell"

whyInstructions, whyTrue, whyFalse :: Text
whyInstructions = "A checker read a software project and judged one sentence of the project's plan; `check` says what kind of check it was, `verdict` its verdict, `why` its reason (and answer), `evidence` what it cites. Does the checker's reason speak to what the sentence itself states, asks, or asks to be done?"
whyTrue = "the reason addresses the very thing the sentence says, asks or asks for: the same thing, behaviour and condition, read the way the writer most likely meant it"
whyFalse = "the reason addresses something else: another component or behaviour, a reframed or narrower question, or only part of what the sentence says while the verdict covers all of it"

evCodeInstructions, evCodeTrue, evCodeFalse :: Text
evCodeInstructions = "A checker read a software project and judged one sentence of the project's plan; `evidence` is the code it cites (each unit's name and first lines). Is the cited code what the sentence is about: where what it states would be true or false, what it asks about, or what the step would change or has changed?"
evCodeTrue = "the cited code is where the behaviour, data or feature the sentence names lives, or plainly shows it (or, for a step not done yet, the code it would change)"
evCodeFalse = "the cited code is beside the point: another feature, a similarly named thing, a caller or test that does not show what the reason claims; or nothing is cited for a verdict that rests on code"

evWebInstructions, evWebTrue, evWebFalse :: Text
evWebInstructions = "A checker judged one sentence of a software plan from public web sources (it saw no code); `evidence` is the pages it cites, title and address. Are the cited pages authoritative public sources for what the sentence is about: the named library's, framework's or standard's own documentation, release notes, changelog or migration guide, for the version the sentence names?"
evWebTrue = "the pages are the named project's own documentation, release notes, changelog, migration guide or reference, for the version named (or one that covers it)"
evWebFalse = "the pages are about another library or another version, are third-party posts or forums repeating it, or there are none"

followsInstructions, followsTrue, followsFalse :: Text
followsInstructions = "A checker judged one sentence of a software plan; `verdict` is its verdict, with what the verdict means, and `why` is its reason (and answer). Does the verdict follow from the checker's own reason?"
followsTrue = "if the reason is true, it establishes this verdict and not another: e.g. a reason showing the statement false with the verdict wrong, a reason showing all of the step in the code with the verdict done, an answer the reason supports"
followsFalse = "the reason supports another verdict, hedges, or contradicts the verdict: e.g. the reason says the code does part of it but the verdict is done, the reason finds the statement true but the verdict is wrong, or the answer says more than the reason shows"
