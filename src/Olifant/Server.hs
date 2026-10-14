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
  , blockTest
  , blockJudge
  , blockAccept
  , blockReject
  , blockKill
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
  , testsJson
  ) where

import Olifant.Server.BlockDecide (orderSurvivors)
import Olifant.Server.BlockMoves (Stubbed (..), blockChanged, blockEdited, blockKill, blockStub, blockWaive)
import Olifant.Server.Block (appendStep, blockAccept, blockBegin, blockDraft, blockStart, blockFor, blockJson, blockJudge, blockReject, blockTest, stateName, underTest)
import Olifant.Server.Actions (actionEdits, actionPlan, actionPlanWith, actionsFor, describeProgress)
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
import Olifant.Server.Tests (TestsAnswer (..), testsJson, testsRun)
