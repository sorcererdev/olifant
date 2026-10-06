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
  , Jobs
  , newJobs
  , submit
  ) where

import Olifant.Server.Actions (actionEdits, actionPlan, actionPlanWith, actionsFor, describeProgress)
import Olifant.Server.Holes (Hole (..), fillHoleEdits, holeAt, holesFor)
import Olifant.Server.Regions (regionEdits)
import Olifant.Server.Suggest (Suggestion (..), suggestionFor)
import Olifant.Server.Completion (completionsFor, ghostFor)
import Olifant.Server.Edits (FileEdit (..), bytePosition, editorPosition, editsOfPlan, smallestChange)
import Olifant.Server.Fixes (findingsFor, fixEdits)
import Olifant.Server.Hints (LineHint (..), hintsFor)
import Olifant.Server.Jobs (Jobs, newJobs, submit)
import Olifant.Server.State
