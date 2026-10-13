-- | Judging mutants: each one's file run over the project in its own shadow.
module Olifant.Mutate.Run (Verdict (..), judge) where

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
-- temp tree), N at a time; a broken or timed-out run is Killed. A run that
-- cannot start is the whole result's `Left`. Results keep the mutants' order.
judge :: Runner IO -> Int -> Language -> FilePath -> Map FilePath Text -> TestId -> [Mutant] -> IO (Either Text [(Mutant, Verdict)])
judge runner workers lang path files testFile mutants = do
  sem <- newQSem (max 1 workers)
  results <- forConcurrently mutants $ \m ->
    bracket_ (waitQSem sem) (signalQSem sem) $
      runTestsIn runner lang [testFile] (M.insert path (muText m) files)
        <&> fmap (\run -> (m, if null (trFailed run) && isNothing (trBroken run) then Survived else Killed))
  pure (sequence results)
