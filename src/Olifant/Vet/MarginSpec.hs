module Olifant.Vet.MarginSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Language.LSP.Protocol.Types
import Olifant.Catalog (defaultVet)
import Olifant.Vet.Margin
import Olifant.Vet.Steering (steering)
import Olifant.Vet.Store
import Olifant.Vet.StoreSpec (finding)
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Vet.Margin" $ do
  let fps = M.fromList [("f", ("abc1234", 10))]
      store = emptyVetStore {vsFindings = [finding (T.pack [c]) "bug" Open k | (c, k) <- zip "abcd" [0.9, 0.8, 0.95, 0.85]]}
  it "draws the shown findings: three of four, source olifant-vet, a hint on the first line" $ do
    let ds = vetDiagnostics defaultVet fps "src/a.py" store
    length ds `shouldBe` 3
    [(src, sev, code, start) | Diagnostic (Range start _) sev code _ src _ _ _ _ <- ds] `shouldBe` replicate 3 (Just "olifant-vet", Just DiagnosticSeverity_Hint, Just (InR "bug"), Position 9 0)
  it "a finding whose unit moved since is drawn stale" $
    [code | Diagnostic _ _ code _ _ _ _ _ _ <- vetDiagnostics defaultVet (M.fromList [("f", ("0000000", 10))]) "src/a.py" store] `shouldBe` replicate 3 (Just (InR "bug-stale"))
  it "(I6) a finding moves with its unit" $
    [start | Diagnostic (Range start _) _ _ _ _ _ _ _ _ <- vetDiagnostics defaultVet (M.fromList [("f", ("abc1234", 25))]) "src/a.py" store] `shouldBe` replicate 3 (Position 24 0)
  it "(I6) a finding whose unit is gone is not drawn" $
    vetDiagnostics defaultVet (M.fromList [("g", ("abc1234", 10))]) "src/a.py" store `shouldBe` []
  it "another file's findings are not drawn" $
    vetDiagnostics defaultVet fps "src/b.py" store `shouldBe` []
  it "the why page quotes the statement and the rule" $ do
    let t = whyText (steering "## Rules\n### A rule\nNo twos at all.\n") (finding "a" "bug" Open 0.9)
    t `shouldSatisfy` T.isInfixOf "It breaks the rule."
    t `shouldSatisfy` T.isInfixOf "No twos at all."
    t `shouldSatisfy` T.isInfixOf "src/b.py:3"
