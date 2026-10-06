-- | Internal to Olifant.Process: a cancellation token. Work polls it; the
-- language server trips it when newer work supersedes it or the client
-- cancels the request.
module Olifant.Process.Cancel (Cancel, newCancel, neverCancel, cancelled) where

import Data.IORef (atomicWriteIORef, newIORef, readIORef)

newtype Cancel = Cancel (IO Bool)

-- | A token and the action that trips it.
newCancel :: IO (Cancel, IO ())
newCancel = do
  tripped <- newIORef False
  pure (Cancel (readIORef tripped), atomicWriteIORef tripped True)

-- | Work nobody cancels (the CLI's).
neverCancel :: Cancel
neverCancel = Cancel (pure False)

cancelled :: Cancel -> IO Bool
cancelled (Cancel tripped) = tripped
