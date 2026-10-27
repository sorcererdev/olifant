module SpecHookSpec (spec) where

import Control.Concurrent (threadDelay)
import SpecHook (failAfter)
import Test.Hspec

spec :: Spec
spec = describe "SpecHook (every example)" $ do
  it "fails an example that runs past its limit instead of hanging the suite" $
    failAfter 1 (threadDelay 3000000) `shouldThrow` anyException
  it "lets one that finishes in time pass" $
    failAfter 1 (pure ()) `shouldReturn` ()
