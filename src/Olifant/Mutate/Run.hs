-- | Judging mutants: each one's file run over the project in its own shadow.
module Olifant.Mutate.Run (Verdict (..), judge, judgeTimeout) where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_)
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isNothing)
import Data.Text (Text)
import Olifant.Language (Language, Runner, TestId, TestRun (..), runTestsIn)
import Olifant.Mutate.Types (Mutant (..))

-- | Whether the tests noticed a mutant.
data Verdict = Killed | Survived
  deriving stock (Eq, Show)

-- | Run each mutant's file over the project in its own shadow (the runner's
-- temp tree), N at a time, judged by PASSING: the tests that passed on the
-- files as they are (a sibling test already red notices nothing). A mutant
-- one of them fails, or whose run is broken or timed out, is Killed. A run
-- that cannot start is the whole result's `Left`; with no passing test
-- there is nothing to judge by. Results keep the mutants' order.
judge :: Runner IO -> Int -> Language -> FilePath -> Map FilePath Text -> TestId -> [TestId] -> [Mutant] -> IO (Either Text [(Mutant, Verdict)])
judge runner workers lang path files testFile passing mutants
  | null passing = pure (Left "no passing test to judge by")
  | otherwise = do
      sem <- newQSem (max 1 workers)
      results <- forConcurrently mutants $ \m ->
        bracket_ (waitQSem sem) (signalQSem sem) $
          runTestsIn runner lang [testFile] (M.insert path (muText m) files)
            <&> fmap (\run -> (m, if isNothing (trBroken run) && not (any ((`elem` passing) . fst) (trFailed run)) then Survived else Killed))
      pure (sequence results)

-- | A mutant's run may take five times the baseline run's wall time (in
-- seconds), at least 5 s and at most 120 s: a mutant that loops forever
-- (a timeout is a kill) holds the judge seconds, not minutes.
judgeTimeout :: Double -> Int
judgeTimeout secs = min 120 (max 5 (ceiling (5 * secs)))
