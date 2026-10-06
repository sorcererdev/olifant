-- | Every spec runs in parallel (`hspec -j`, the cores by default): most
-- wait on compilers and models, not the CPU. A spec that measures time is
-- marked `sequential` where it is.
module SpecHook (hook) where

import Test.Hspec (Spec, parallel)

hook :: Spec -> Spec
hook = parallel
