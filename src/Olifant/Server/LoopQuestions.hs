-- | Internal to Olifant.Server: the loop's questions (which open step a
-- change is for, what state a step is in), the words its ghosts need from
-- a chat rung, and the climb that asks them: Jev first, then the ladder.
module Olifant.Server.LoopQuestions
  ( planStepQuestion
  , planStateQuestion
  , openSteps
  , unitDiff
  , Need (..)
  , planWords
  , parseWords
  , Rungs (..)
  , rungsFor
  , gate
  , accepted
  , decideOver
  ) where

import Control.Monad (guard)
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.List (partition)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import Olifant.App (Project (..))
import Olifant.Catalog (Catalog (..), PlansConfig (..))
import Olifant.Decide (Answer (..), Decider (..), Question, choice, confident, curlDecider, qOptions)
import Olifant.Model (Backend (..), Escalate (..), ModelConfig (..), Rung (..), askBackend)
import Olifant.Plans (Kind (..), Line (..), Section (..), TestState (..), childrenOf, sectionOf, sections, testState)
import Olifant.Process (Cancel, cancelled)
import Olifant.Server.LoopTypes (Change (..), ChangedUnit (..))
import Olifant.Server.Plans (safely)
import Olifant.Server.State (ServerState (..), deciderUnder)

-- | Which open step a file's change is for: one option per open step
-- (`s<row>`, the current branch's section's first), or none. Its context
-- carries the first 8 units' headers and diffs, and every section's prose.
planStepQuestion :: Maybe Text -> [Line] -> (FilePath, [ChangedUnit]) -> Question
planStepQuestion branch ls (path, units) =
  choice "plan-step" instructions ([("s" <> tshow (lnRow l), T.strip (lnText l)) | l <- openSteps branch ls] ++ [("none", "no open step covers this change")]) $
    [ ("file", T.pack path)
    , ("units", T.intercalate "\n" (map cuHeader shown))
    , ("diff", T.intercalate "\n\n" (map unitDiff shown))
    , ("sections", sectionsText ls)
    ]
  where
    shown = take 8 units
    instructions = "The units in units changed in the file at file; diff shows each against the plan's baseline. Which open step of the plan is this change for? sections holds the plan's sections and what each says. Answer none when no open step covers it."

-- | A plan's open steps, those of the section bound to BRANCH first.
openSteps :: Maybe Text -> [Line] -> [Line]
openSteps branch ls = mine ++ rest
  where
    (mine, rest) = partition (\l -> branch /= Nothing && secBranch (sectionOf ls (lnRow l)) == branch) [l | l <- ls, lnKind l == Step False]

-- | Each section's title with its prose lines.
sectionsText :: [Line] -> Text
sectionsText ls = T.intercalate "\n\n" [T.intercalate "\n" (("## " <> secTitle sec) : prose sec) | sec <- sections ls]
  where
    prose sec = [T.strip (lnText l) | l <- ls, lnRow l >= secRow sec, lnRow l < secEnd sec, lnKind l == Prose, not (T.null (T.strip (lnText l)))]

-- | What state a step is in after its units changed: an open step done, in
-- progress or diverged; a done one still done, reopened or diverged. Its
-- context: the step, its children, its units' diffs, its tests' states.
planStateQuestion :: Line -> [Line] -> [ChangedUnit] -> Question
planStateQuestion step ls units =
  choice "plan-state" instructions options $
    [ ("step", T.strip (lnText step))
    , ("children", T.intercalate "\n" (map (T.strip . lnText) kids))
    , ("diff", T.intercalate "\n\n" (map unitDiff (take 8 units)))
    , ("tests", T.intercalate "\n" [T.strip (lnText c) <> ": " <> stateName (testState c) | c <- kids, lnKind c == TestChild])
    ]
  where
    kids = childrenOf ls (lnRow step)
    diverged = ("diverged", "the change departs from what the step says")
    options
      | lnKind step == Step True = [("still_done", "the step is still done after the change"), ("reopened", "the change undoes the step: it is open again"), diverged]
      | otherwise = [("done", "the change completes the step"), ("in_progress", "the step is begun, not finished"), diverged]
    instructions = "The units in diff changed under the plan step in step (its children and their test states in children and tests). What state is the step in now?"
    stateName = \case
      Just TestGreen -> "green"
      Just TestRed -> "red"
      Just TestPending -> "not run since the change"
      Nothing -> "no mark"

-- | A unit against the base, at most 60 lines: a heading, up to three
-- lines of context each side, base lines `- `, current lines `+ `.
unitDiff :: ChangedUnit -> Text
unitDiff cu = T.intercalate "\n" (take 60 (heading : map ("  " <>) (takeEnd 3 (take pre base)) ++ map ("- " <>) old ++ map ("+ " <>) new ++ map ("  " <>) (take 3 (takeEnd post now))))
  where
    heading = "@@ " <> T.pack (cuPath cu) <> ":" <> cuName cu <> " (" <> what <> ")"
    what = case cuChange cu of
      Added -> "added"
      Changed _ -> "changed"
      Removed -> "removed"
      Moved p n -> "moved from " <> T.pack p <> ":" <> n
    base = maybe [] T.lines (cuBase cu)
    now = maybe [] T.lines (cuNow cu)
    pre = length (takeWhile id (zipWith (==) base now))
    post = length (takeWhile id (zipWith (==) (reverse (drop pre base)) (reverse (drop pre now))))
    old = dropEnd post (drop pre base)
    new = dropEnd post (drop pre now)
    takeEnd n xs = drop (length xs - n) xs
    dropEnd n xs = take (length xs - n) xs

-- | A line a ghost needs from a chat rung: a new step's text for an
-- unplaced unit, or why a step diverged (its units).
data Need = NeedStepLine ChangedUnit | NeedWhy Line [ChangedUnit]
  deriving stock (Eq, Show)

-- | Every need's words in one call to BACKEND: numbered needs in, "N:
-- <line>" out; Nothing where a line is missing or the call fails.
planWords :: Cancel -> Int -> Backend -> [Need] -> IO [Maybe Text]
planWords _ _ _ [] = pure []
planWords token timeout backend needs =
  safely (Left "") (askBackend token timeout backend system prompt) <&> \case
    Left _ -> map (const Nothing) needs
    Right reply -> parseWords (length needs) reply
  where
    system = "You write single lines for a markdown plan. Answer only with the numbered lines asked for."
    prompt = T.unlines (["Write one line for each numbered item. Answer with exactly one line per item, `N: <line>`, at most 12 words each, no markdown, nothing else.", ""] ++ zipWith item [1 :: Int ..] needs)
    item i = \case
      NeedStepLine cu -> tshow i <> ". A plan step (an imperative sentence, no anchor, no checkbox) for this change:\n" <> unitDiff cu <> "\n"
      NeedWhy step cus -> tshow i <> ". Why the plan step `" <> T.strip (lnText step) <> "` no longer matches the code:\n" <> T.intercalate "\n\n" (map unitDiff (take 4 cus)) <> "\n"

-- | Lines `N: <text>` (or `N. <text>`) for items 1 to N; the first answer
-- to each wins; Nothing where none is given or it is blank.
parseWords :: Int -> Text -> [Maybe Text]
parseWords n reply = [M.lookup i found | i <- [1 .. n]]
  where
    found = M.fromListWith (\_ first -> first) [(i, w) | l <- T.lines reply, Just (i, w) <- [numbered (T.strip l)], i >= 1, i <= n]
    numbered l = do
      let (digits, rest) = T.span isDigit l
      (sep, after) <- T.uncons rest
      guard (not (T.null digits) && sep `elem` [':', '.'])
      let w = cleaned (T.strip after)
      guard (not (T.null w))
      Just (read (T.unpack digits), w)
    -- a model may echo the list's dash or a checkbox
    cleaned w = case (T.stripPrefix "- [ ] " w, T.stripPrefix "- " w) of
      (Just r, _) -> T.strip r
      (_, Just r) -> T.strip r
      _ -> w

-- | The rungs a reconcile may ask: the decide model (Jev), and the chat
-- ladder (`[plans] ladder`, else `[model] ladder`) without Jev backends or
-- the decide model itself, keeping `auto` rungs, and `ask` ones when forced.
data Rungs = Rungs {rgJev :: Maybe (Decider IO), rgChat :: [(Rung, Backend)]}

-- | The rungs for a project, under the state's token; FORCED allows `ask` rungs.
rungsFor :: ServerState -> Project -> Bool -> Rungs
rungsFor s p forced = Rungs (deciderUnder (ssCancel s) s p) chat
  where
    cat = prCatalog p
    m = catalogModel cat
    names = fromMaybe (map rungName (mcLadder m)) (pcLadder (catalogPlans cat))
    chat =
      [ (Rung n e, b)
      | n <- names, Just n /= catalogDecideModel cat
      , Just b <- [lookup n (mcBackends m)], not (isJev b)
      , let e = fromMaybe EscalateAsk (lookup n (mcEscalate m))
      , allowed e ]
    isJev = \case Jev {} -> True; _ -> False
    allowed = \case
      EscalateAuto -> True
      EscalateAsk -> forced
      EscalateNever -> False

-- | The confidence an answer needs to be taken.
gate :: Double
gate = 0.6

-- | An answer taken: one of the question's options at the gate.
accepted :: Question -> Answer -> Bool
accepted q a = case a of
  Chosen c _ _ -> c `elem` qOptions q && confident gate a
  _ -> False

-- | Ask Jev, then each chat rung, until one answers at the gate (or the
-- work is cancelled): the last answer, and each rung asked with its seconds.
decideOver :: Cancel -> Int -> Rungs -> Question -> IO (Answer, [(Text, Double)])
decideOver token timeout rungs q = go ([("jev", d) | Just d <- [rgJev rungs]] ++ [(rungName r, curlDecider token timeout b) | (r, b) <- rgChat rungs]) Unsure []
  where
    go [] a climbs = pure (a, reverse climbs)
    go ((name, d) : rest) a0 climbs =
      cancelled token >>= \case
        True -> pure (a0, reverse climbs)
        False -> do
          start <- getMonotonicTime
          a <- safely Unsure (ask d q)
          end <- getMonotonicTime
          let climbs' = (name, end - start) : climbs
          if accepted q a then pure (a, reverse climbs') else go rest a climbs'

tshow :: Show a => a -> Text
tshow = T.pack . show
