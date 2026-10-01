-- | A sum type with the two kinds of case `complete` extends (spec §5.6).
module Olifant.SampleErrors (SampleError (..), describeSampleError, sampleExitCode) where

data SampleError
  = SampleMissing String
  | SampleBroken String Int

describeSampleError :: SampleError -> String
describeSampleError = \case
  SampleMissing p -> "missing " <> p
  SampleBroken p n -> "broken " <> p <> " at line " <> show n

sampleExitCode :: SampleError -> Int
sampleExitCode e = case e of
  SampleMissing _ -> 3
  SampleBroken _ _ -> 1
