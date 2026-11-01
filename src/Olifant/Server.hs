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
  , decision
  , appendStepLine
  , quietCounts
  , Moment (..)
  , Anticipation (..)
  , anticipationAsk
  , sureAnswer
  , anticipationFrom
  , Jobs
  , newJobs
  , submit
  , PickStep (..)
  , Preview (..)
  , previewFor
  , stepsPlanWith
  , orderSurvivors
  , TestsAnswer (..)
  , testsRun
  , unitAround
  , Named (..)
  , StubAnswer (..)
  , stubAt
  , Unit (..)
  , unitAt
  , ConjureAnswer (..)
  , PausedAnswer (..)
  , pausedJson
  , Proposal (..)
  , ConjureOutcome (..)
  , ResumeChoice (..)
  , Resume (..)
  , conjureAt
  , resumeConjure
  , MutateAnswer (..)
  , waiveAt
  , mutateAt
  , TestsScope (..)
  , testsJson
  , TestsFile (..)
  , testsFile
  , testsFileJson
  , isPlanPath
  , Resolved (..)
  , resolveAnchor
  , planDiagnostics
  , AnchorPrints
  , PlanToken (..)
  , planTokenTypes
  , planTokenModifiers
  , planTokens
  , planFolds
  , anchorItems
  , newStepSnippet
  , newStepText
  , FileItem (..)
  , planFiles
  , ErrandAnswer (..)
  , runErrand
  , Verb (..)
  , Block (..)
  , Ghost (..)
  ) where

import Olifant.Server.Order (orderSurvivors)
import Olifant.Server.Plans (AnchorPrints, ErrandAnswer (..), FileItem (..), PlanToken (..), Resolved (..), anchorItems, isPlanPath, newStepSnippet, newStepText, planDiagnostics, planFiles, planFolds, planTokenModifiers, planTokenTypes, planTokens, resolveAnchor, runErrand)
import Olifant.Server.Errand (Block (..), Ghost (..), Verb (..))
import Olifant.Server.Actions (actionEdits, actionPlan, actionPlanWith, actionsFor, describeProgress, menuActionsFor)
import Olifant.Server.Holes (Hole (..), fillHoleEdits, fillHolePlan, holeAt, holesFor)
import Olifant.Server.Regions (regionEdits)
import Olifant.Server.Suggest (Suggestion (..), suggestionFor)
import Olifant.Server.Completion (completionsFor, ghostFor)
import Olifant.Server.Edits (FileEdit (..), bytePosition, editorPosition, editsOfPlan, smallestChange)
import Olifant.Server.Fixes (findingsFor, fixEdits, fixPlan)
import Olifant.Server.Model (modelFor)
import Olifant.Server.Anticipate (Anticipation (..), Moment (..), anticipationAsk, anticipationFrom, sureAnswer)
import Olifant.Server.Recognize (quietCounts, recognitionFeedback, recognitionFor, recognitionPlan, recognitionUpgrade, suggestOrRecognize)
import Olifant.Server.Hints (LineHint (..), fileActionsFor, hintsFor)
import Olifant.Server.Jobs (Jobs, newJobs, submit)
import Olifant.Server.Preview (PickStep (..), Preview (..), previewFor, stepsPlanWith)
import Olifant.Server.State
import Olifant.Server.Stub (StubAnswer (..), stubAt)
import Olifant.Server.Unit (Named (..), Unit (..), unitAround, unitAt)
import Olifant.Server.Conjure (ConjureAnswer (..), ConjureOutcome (..), PausedAnswer (..), Proposal (..), pausedJson, Resume (..), ResumeChoice (..), conjureAt, resumeConjure)
import Olifant.Server.Mutate (MutateAnswer (..), mutateAt)
import Olifant.Server.Waive (waiveAt)
import Olifant.Server.Tests (TestsAnswer (..), TestsFile (..), testsFile, testsFileJson, testsJson, testsRun, TestsScope (..))
