-- | Internal to Olifant.Server: slow work (checks, model fills, verify) run
-- in the background by key (`check FILE`, `suggest FILE`). A newer job with
-- the same key cancels the older (its processes are killed); a debounced
-- job waits first, and never runs if superseded meanwhile.
module Olifant.Server.Jobs (Jobs, newJobs, submit) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Monad (when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Olifant.Process (Cancel, cancelled, newCancel)

-- | Each key's running job (its number and how to cancel it).
data Jobs = Jobs (MVar Int) (MVar (Map Text (Int, IO ())))

newJobs :: IO Jobs
newJobs = Jobs <$> newMVar 0 <*> newMVar M.empty

-- | Run @work@ under @key@ after @delayMs@, cancelling the key's previous
-- job: 'Nothing' when it was superseded before it started.
submit :: Jobs -> Text -> Int -> (Cancel -> IO a) -> IO (Async (Maybe a))
submit (Jobs counter running) key delayMs work = do
  n <- modifyMVar counter (\c -> pure (c + 1, c + 1))
  (token, trip) <- newCancel
  modifyMVar_ running $ \m -> do
    maybe (pure ()) snd (M.lookup key m)
    pure (M.insert key (n, trip) m)
  async $ do
    when (delayMs > 0) (threadDelay (delayMs * 1000))
    stopped <- cancelled token
    result <- if stopped then pure Nothing else Just <$> work token
    modifyMVar_ running (pure . M.update (\(n', t) -> if n' == n then Nothing else Just (n', t)) key)
    pure result
