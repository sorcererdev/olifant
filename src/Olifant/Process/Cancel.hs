-- | Internal to Olifant.Process: a cancellation token. Work polls it; the
-- language server trips it when newer work supersedes it or the client
-- cancels the request.
module Olifant.Process.Cancel (Cancel, newCancel, neverCancel, cancelAfter, cancelled, eitherCancel) where

import Data.IORef (atomicModifyIORef', atomicWriteIORef, newIORef, readIORef)

newtype Cancel = Cancel (IO Bool)

-- | A token and the action that trips it.
newCancel :: IO (Cancel, IO ())
newCancel = do
  tripped <- newIORef False
  pure (Cancel (readIORef tripped), atomicWriteIORef tripped True)

-- | Work nobody cancels (the CLI's).
neverCancel :: Cancel
neverCancel = Cancel (pure False)

-- | A token that reads as tripped from its N+1th look on (for tests that
-- stop work at a known point).
cancelAfter :: Int -> IO Cancel
cancelAfter n = do
  looks <- newIORef (0 :: Int)
  pure (Cancel (atomicModifyIORef' looks (\i -> (i + 1, i >= n))))

cancelled :: Cancel -> IO Bool
cancelled (Cancel tripped) = tripped

-- | Tripped when either token is.
eitherCancel :: Cancel -> Cancel -> Cancel
eitherCancel a b = Cancel ((||) <$> cancelled a <*> cancelled b)
