-- | Every spec runs in parallel (`hspec -j`, the cores by default): most
-- wait on compilers and models, not the CPU. A spec that measures time is
-- marked `sequential` where it is.
--
-- Every example also fails after 'exampleLimit' seconds instead of hanging
-- the suite (hspec has no timeout of its own): the slowest example takes
-- about 17 s on a quiet machine, so the limit leaves room for a loaded one.
module SpecHook (hook, failAfter, exampleLimit) where

import System.Timeout (timeout)
import Test.Hspec (Spec, around_, expectationFailure, parallel)

hook :: Spec -> Spec
hook = parallel . around_ (failAfter exampleLimit)

exampleLimit :: Int
exampleLimit = 180

-- | ACT, failed when it runs past LIMIT seconds.
failAfter :: Int -> IO () -> IO ()
failAfter limit act =
  timeout (limit * 1000000) act >>= \case
    Just () -> pure ()
    Nothing -> expectationFailure ("timed out after " <> show limit <> " s (SpecHook)")
