-- | A block: one pending change of a few lines, bounded by one test, and
-- the pure machine that moves it from red to accepted. No IO here; the
-- server feeds it what happened (a draft landed, the judge came back) and
-- shows the person `open` and `hud`.
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
    hud,
    pending,
    draftTemplate,
    testDraftTemplate,
    killStub,
    instructionFor,
  )
where

import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Block.Draft (draftTemplate, instructionFor, killStub, testDraftTemplate)
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
    blockDrafts :: [Draft]
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
  Block i file kind base sp test (Red failure) []

-- | Take a move. Left says why it is not open, in one sentence for the person.
step :: Move -> Block -> Either Text Block
step mv b
  | not (pending b) = Left "this block is finished; begin another"
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
      (Kill line, Judged ms)
        | any ((== line) . muLine) ms -> Right b
        | otherwise -> Left "no survivor on that line to kill"
      (Accept, Judged ms)
        | all (isJust . muWaived) ms -> Right b {blockState = Accepted}
        | otherwise -> Left (T.pack (show (length (filter (isNothing . muWaived) ms))) <> " survivor(s): kill or waive each, then accept")
      (Accept, _) -> Left "accept needs the test green and the block judged"
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
  Kill _ -> "kill"
  Accept -> "accept"
  Reject -> "reject"

-- | The moves open now.
open :: Block -> [Text]
open b = case blockState b of
  Red _ -> drafting
  Green -> drafting
  Judged ms
    | null (unwaived ms) -> ["accept", "next", "reject"]
    | otherwise -> ["kill", "waive", "next", "reject"]
  Broken _ -> ["test", "reject"]
  Drafting _ -> ["reject"]
  Accepted -> []
  Rejected -> []
  where
    drafting = [if null (blockDrafts b) then "draft" else "next", "reject", "test"]

unwaived :: [Mutant] -> [Mutant]
unwaived = filter (isNothing . muWaived)

-- | Unwaived survivors (none unless Judged).
survivors :: Block -> [Mutant]
survivors b = case blockState b of
  Judged ms -> unwaived ms
  _ -> []

-- | The one HUD line.
hud :: Block -> Text
hud b = "▸ " <> testName <> " · " <> draftPart <> statePart
  where
    testName = snd (T.breakOnEnd "::" (blockTestId b))
    draftPart
      | null (blockDrafts b) = ""
      | otherwise = "draft " <> T.pack (show (length (blockDrafts b))) <> " · "
    statePart = case blockState b of
      Red f -> "red: " <> fMessage f
      Drafting _ -> "drafting…"
      Green -> "green · judging…"
      Judged ms -> case length (unwaived ms) of
        0 -> "clean ✓ accept"
        1 -> "1 survivor"
        n -> T.pack (show n) <> " survivors"
      Broken why -> "cannot run: " <> why
      Accepted -> "accepted"
      Rejected -> "rejected"

-- | Not yet Accepted or Rejected.
pending :: Block -> Bool
pending b = blockState b `notElem` [Accepted, Rejected]
