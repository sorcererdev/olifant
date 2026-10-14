-- | A block (the person's "magic circle"): one pending change of a few
-- lines, bounded by one test, and the pure machine that moves it from red
-- to accepted. No IO here; the server feeds it what happened (a draft
-- landed, the judge came back) and shows the person `open` and `hud`.
module Olifant.Block
  ( Kind (..),
    Draft (..),
    BlockState (..),
    Block (..),
    Move (..),
    begin,
    step,
    open,
    survivors,
    killTarget,
    hud,
    pending,
    draftTemplate,
    testDraftTemplate,
    killStub,
    stubName,
    stubNameFor,
    uniqueTestName,
    testStub,
    instructionFor,
  )
where

import Control.Applicative ((<|>))
import Data.List (find)
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Block.Draft (draftTemplate, instructionFor, killStub, stubName, stubNameFor, testDraftTemplate, testStub, uniqueTestName)
import Olifant.Language (Failure (..), TestId)
import Olifant.Mutate (Mutant (..))

-- | A code block, or a test block (red at its assert; a kill also fails on the mutant).
data Kind = Code | TestFor (Maybe Mutant)
  deriving stock (Eq, Show)

-- | A body offered for the block; `draftBy` is a model's name or "you".
data Draft = Draft {draftText :: Text, draftBy :: Text, draftOutcome :: Text}
  deriving stock (Eq, Show)

-- | 'Drafting' keeps the state the draft began from: a draft that fails
-- (or is refused) puts it back.
data BlockState = Red Failure | Drafting BlockState | Green | Judged [Mutant] | Broken Text | Accepted | Rejected
  deriving stock (Eq, Show)

data Block = Block
  { blockId :: Int,
    blockFile :: FilePath,
    blockKind :: Kind,
    blockBase :: Text,
    blockSpan :: (Int, Int),
    blockTestId :: TestId,
    blockState :: BlockState,
    blockDrafts :: [Draft],
    -- | the file's text the state was established on (the server records
    -- it as a test, a draft or a waiver lands; the base when it began): a
    -- draft that fails puts back a state the file may have left
    blockOn :: Text,
    -- | bumped by every move the server stores: a result begun on an
    -- older version is stale (the block moved on while it ran)
    blockVersion :: Int,
    -- | a new test's block opened for a function (by a stub or a begin):
    -- its name and its def's line (1-based) in the module, where the
    -- function begins once the test is accepted
    blockSubject :: Maybe (Text, Int)
  }
  deriving stock (Eq, Show)

-- | What can happen to a block.
data Move
  = StartDraft
  | -- | the draft gave nothing to land (no attempt, or the block moved on): back to before it
    DraftFailed
  | Drafted Text Text (Either Failure ())
  | Edited (Int, Int) (Either Failure ())
  | -- | the latest draft, as the editor applied it, tested: no draft of the person's
    Applied (Int, Int) (Either Failure ())
  | CannotRun Text
  | Judge [Mutant]
  | -- | a judged block judged again (its test file gained a test): green until it is
    Rejudge
  | -- | the line, the operator, the reason
    Waive Int Text Text
  | Kill Int
  | Accept
  | Reject
  deriving stock (Eq, Show)

begin :: Int -> FilePath -> Kind -> Text -> (Int, Int) -> TestId -> Failure -> Block
begin i file kind base sp test failure =
  Block i file kind base sp test (Red failure) [] base 0 Nothing

-- | Take a move. Left says why it is not open, in one sentence for the person.
step :: Move -> Block -> Either Text Block
step mv b
  | not (pending b) = Left "this circle is closed; begin another"
  | otherwise = case (mv, blockState b) of
      (StartDraft, s) | not (drafting s) && not (broken s) -> Right b {blockState = Drafting s}
      (DraftFailed, Drafting before) -> Right b {blockState = before}
      (Drafted text by outcome, s) | drafting s || s == Green || red s -> Right (landed (blockSpan b) outcome (addDraft (Draft text by (outcomeText outcome)) b))
      (Edited sp outcome, _) -> Right (landed sp outcome (editDraft outcome b))
      (Applied sp outcome, _) | not (null (blockDrafts b)) -> Right (landed sp outcome b)
      (Rejudge, Judged _) -> Right b {blockState = Green}
      (CannotRun why, _) -> Right b {blockState = Broken why}
      (Judge ms, Green) -> Right b {blockState = Judged ms}
      (Waive line op reason, Judged ms)
        | any (\m -> here m && isNothing (muWaived m)) ms ->
            Right b {blockState = Judged [if here m then m {muWaived = Just reason} else m | m <- ms]}
        | otherwise -> Left "no unwaived survivor on that line"
        where
          here m = muLine m == line && muOperator m == op
      (Kill line, Judged _) -> b <$ killTarget line b
      (Accept, Judged ms)
        | all (isJust . muWaived) ms -> Right b {blockState = Accepted}
        | otherwise -> Left (T.pack (show (length (filter (isNothing . muWaived) ms))) <> " survivor(s): stub a test or waive each, then accept")
      (Accept, _) -> Left "accept needs the test green and the circle judged"
      (Reject, _) -> Right b {blockState = Rejected}
      _ -> Left ("`" <> moveName mv <> "` is not open here")
  where
    broken = \case Broken _ -> True; _ -> False
    drafting = \case Drafting _ -> True; _ -> False
    red = \case Red _ -> True; _ -> False
    outcomeText = either (("red: " <>) . fMessage) (const "green")
    landed sp outcome b' = b' {blockSpan = sp, blockState = either Red (const Green) outcome}
    addDraft d b' = b' {blockDrafts = blockDrafts b' ++ [d]}
    -- editing twice is one draft: the second edit only updates its outcome
    editDraft outcome b' = case reverse (blockDrafts b') of
      (d : rest) | draftBy d == "you" -> b' {blockDrafts = reverse (d {draftOutcome = outcomeText outcome} : rest)}
      _ -> addDraft (Draft "" "you" (outcomeText outcome)) b'

moveName :: Move -> Text
moveName = \case
  StartDraft -> "draft"
  DraftFailed -> "draft"
  Drafted {} -> "draft"
  Edited {} -> "edit"
  Applied {} -> "test"
  Rejudge -> "rejudge"
  CannotRun _ -> "test"
  Judge _ -> "judge"
  Waive {} -> "waive"
  -- the person stubs a test for a survivor; the test then kills it
  Kill _ -> "stub"
  Accept -> "accept"
  Reject -> "reject"

-- | The moves open now.
open :: Block -> [Text]
open b = case blockState b of
  Red _ -> drafting
  Green -> drafting
  Judged ms
    | null (unwaived ms) -> ["accept", "draft", "reject"]
    | otherwise -> ["stub", "waive", "draft", "reject"]
  Broken _ -> ["test", "reject"]
  Drafting _ -> ["reject"]
  Accepted -> []
  Rejected -> []
  where
    -- the first draft or another candidate: draft either way
    drafting = ["draft", "reject", "test"]

unwaived :: [Mutant] -> [Mutant]
unwaived = filter (isNothing . muWaived)

-- | Unwaived survivors (none unless Judged).
survivors :: Block -> [Mutant]
survivors b = case blockState b of
  Judged ms -> unwaived ms
  _ -> []

-- | The survivor a kill on a line (1-based) takes: the unwaived one on
-- that line, or else the first unwaived in the block's order (the decision
-- model's ranking, when it has ranked them).
killTarget :: Int -> Block -> Either Text Mutant
killTarget line b = maybe (Left "no survivor left: accept") Right (find ((== line) . muLine) left <|> listToMaybe left)
  where
    left = survivors b

-- | The one HUD line: where the block is, and the next step when it is the person's.
hud :: Block -> Text
hud b = "▸ " <> testName <> " · " <> draftPart <> statePart
  where
    testName = snd (T.breakOnEnd "::" (blockTestId b))
    draftPart
      | null (blockDrafts b) = ""
      | otherwise = "draft " <> T.pack (show (length (blockDrafts b))) <> " · "
    statePart = case blockState b of
      Red f -> case filter (not . T.null) [if T.null (fMessage f) then "" else "red: " <> fMessage f, redCue] of
        [] -> "red"
        parts -> T.intercalate " · " parts
      Drafting _ -> "drafting…"
      Green -> "green · judging…"
      Judged ms -> case length (unwaived ms) of
        -- a new test red at its assertion is ready: accepted, its function begins on it
        0 | TestFor Nothing <- blockKind b -> "red ✓ accept · the fix is drafted next"
        0 -> "clean ✓ accept"
        1 -> "1 survivor · stub a test or waive"
        n -> T.pack (show n) <> " survivors · stub a test or waive each"
      Broken why -> "cannot run: " <> why
      Accepted -> "accepted"
      Rejected -> "rejected"
    -- red's next step: none once the model's draft failed (it tried; the
    -- person reads it); a test's stub no model has drafted yet: a new
    -- test's is named first (draft refuses a stub still called test_name),
    -- a kill's is named already (the HUD's first field), and reject drops
    -- either; else draft
    redCue = case (reverse (blockDrafts b), blockKind b) of
      -- (one that would not parse was never written: draft again)
      (d : _, _) | draftBy d /= "you" -> if "red: the draft does not parse" `T.isPrefixOf` draftOutcome d then "draft" else ""
      -- a begin's stub is named once its test has a name of the person's
      (_, TestFor Nothing) | testName == stubName || not ("::" `T.isInfixOf` blockTestId b) -> "name it, then draft · reject drops it"
      (_, TestFor Nothing) -> "assert, then draft · reject drops it"
      (_, TestFor (Just _)) -> "assert, then draft · reject drops it"
      _ -> "draft"

-- | Not yet Accepted or Rejected.
pending :: Block -> Bool
pending b = blockState b `notElem` [Accepted, Rejected]
