-- | The language server's logic (olifant lsp spec §1): projects across any
-- number of roots, each loaded when first needed, seeing the editor's
-- unsaved documents; slow work as cancellable background jobs. The LSP
-- adapter ("Olifant.Server.Lsp") maps messages onto it.
module Olifant.Server
  ( ServerState (..)
  , ServerConfig (..)
  , defaultServerConfig
  , ProjectSlot (..)
  , newServerState
  , openDoc
  , changeDoc
  , closeDoc
  , projectFor
  , brokenFor
  , workspaceOf
  , catalogChanged
  , filesChanged
  , slotOf
  , fileSaved
  , memo
  , FileEdit (..)
  , editsOfPlan
  , smallestChange
  , editorPosition
  , findingsFor
  , actionsFor
  , menuActionsFor
  , completionsFor
  , Suggestion (..)
  , suggestionFor
  , actionPlan
  , actionPlanWith
  , describeProgress
  , regionEdits
  , Hole (..)
  , holesFor
  , holeAt
  , fillHoleEdits
  , ghostFor
  , LineHint (..)
  , hintsFor
  , actionEdits
  , bytePosition
  , fixEdits
  , fixPlan
  , fileActionsFor
  , fillHolePlan
  , modelFor
  , recognitionFor
  , recognitionPlan
  , suggestOrRecognize
  , recognitionFeedback
  , recognitionUpgrade
  , deciderFor
  , Jobs
  , newJobs
  , submit
  , PickStep (..)
  , Preview (..)
  , previewFor
  , stepsPlanWith
  , blockFor
  , blockBegin
  , blockStart
  , blockDraft
  , blockDraftAt
  , blockTest
  , blockJudge
  , blockAccept
  , blockReject
  , blockKill
  , Killed
  , StubDraft
  , blockUnapplied
  , blockDraftStopped
  , blockStub
  , Stubbed (..)
  , blockWaive
  , blockChanged
  , blockEdited
  , orderSurvivors
  , blockJson
  , stateName
  , appendStep
  , underTest
  , TestsAnswer (..)
  , testsRun
  , testAt
  , belowDecorators
  , classAt
  , testNameOf
  , StubAnswer (..)
  , stubAt
  , Unit (..)
  , unitAt
  , DraftAnswer (..)
  , draftAt
  , MutateAnswer (..)
  , mutateAt
  , TestsScope (..)
  , testsJson
  , TestsFile (..)
  , testsFile
  , testsFileJson
  ) where

import Olifant.Server.Order (orderSurvivors)
import Olifant.Server.BlockMoves (Killed, Stubbed (..), blockChanged, blockEdited, blockKill, blockStub, blockWaive)
import Olifant.Server.Block (StubDraft, appendStep, blockDraftStopped, blockUnapplied, blockAccept, blockBegin, blockDraft, blockDraftAt, blockStart, blockFor, blockJson, blockJudge, blockReject, blockTest, stateName, underTest)
import Olifant.Server.Actions (actionEdits, actionPlan, actionPlanWith, actionsFor, describeProgress, menuActionsFor)
import Olifant.Server.Holes (Hole (..), fillHoleEdits, fillHolePlan, holeAt, holesFor)
import Olifant.Server.Regions (regionEdits)
import Olifant.Server.Suggest (Suggestion (..), suggestionFor)
import Olifant.Server.Completion (completionsFor, ghostFor)
import Olifant.Server.Edits (FileEdit (..), bytePosition, editorPosition, editsOfPlan, smallestChange)
import Olifant.Server.Fixes (findingsFor, fixEdits, fixPlan)
import Olifant.Server.Model (modelFor)
import Olifant.Server.Recognize (recognitionFeedback, recognitionFor, recognitionPlan, recognitionUpgrade, suggestOrRecognize)
import Olifant.Server.Hints (LineHint (..), fileActionsFor, hintsFor)
import Olifant.Server.Jobs (Jobs, newJobs, submit)
import Olifant.Server.Preview (PickStep (..), Preview (..), previewFor, stepsPlanWith)
import Olifant.Server.State
import Olifant.Server.Stub (StubAnswer (..), belowDecorators, classAt, stubAt, testNameOf)
import Olifant.Server.Unit (Unit (..), unitAt)
import Olifant.Server.Draft (DraftAnswer (..), draftAt)
import Olifant.Server.Mutate (MutateAnswer (..), mutateAt)
import Olifant.Server.Tests (TestsAnswer (..), TestsFile (..), testsFile, testsFileJson, testsJson, testsRun, testAt, TestsScope (..))
