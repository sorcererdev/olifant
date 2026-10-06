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
  , slotOf
  , Jobs
  , newJobs
  , submit
  ) where

import Olifant.Server.Jobs (Jobs, newJobs, submit)
import Olifant.Server.State
