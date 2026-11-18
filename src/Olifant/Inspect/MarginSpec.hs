module Olifant.Inspect.MarginSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Language.LSP.Protocol.Types
import Olifant.Catalog (defaultInspect)
import Olifant.Inspect.Margin
import Olifant.Inspect.Steering (steering)
import Olifant.Inspect.Store
import Olifant.Inspect.StoreSpec (finding)
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Inspect.Margin" $ do
  let fps = M.fromList [("f", ("abc1234", 10))]
      store = emptyInspectStore {vsFindings = [finding (T.pack [c]) "bug" Open k | (c, k) <- zip "abcd" [0.9, 0.8, 0.95, 0.85]]}
  it "draws the shown findings: three of four, source olifant-inspect, a hint on the first line" $ do
    let ds = inspectDiagnostics defaultInspect fps "src/a.py" store
    length ds `shouldBe` 3
    [(src, sev, code, start) | Diagnostic (Range start _) sev code _ src _ _ _ _ <- ds] `shouldBe` replicate 3 (Just "olifant-inspect", Just DiagnosticSeverity_Hint, Just (InR "bug"), Position 9 0)
  it "a finding whose unit moved since is drawn stale" $
    [code | Diagnostic _ _ code _ _ _ _ _ _ <- inspectDiagnostics defaultInspect (M.fromList [("f", ("0000000", 10))]) "src/a.py" store] `shouldBe` replicate 3 (Just (InR "bug-stale"))
  it "(I6) a finding moves with its unit" $
    [start | Diagnostic (Range start _) _ _ _ _ _ _ _ _ <- inspectDiagnostics defaultInspect (M.fromList [("f", ("abc1234", 25))]) "src/a.py" store] `shouldBe` replicate 3 (Position 24 0)
  it "(I6) a finding whose unit is gone is not drawn" $
    inspectDiagnostics defaultInspect (M.fromList [("g", ("abc1234", 10))]) "src/a.py" store `shouldBe` []
  it "another file's findings are not drawn" $
    inspectDiagnostics defaultInspect fps "src/b.py" store `shouldBe` []
  it "the why page quotes the statement and the rule" $ do
    let t = whyText (steering "## Rules\n### A rule\nNo twos at all.\n") (finding "a" "bug" Open 0.9)
    t `shouldSatisfy` T.isInfixOf "It breaks the rule."
    t `shouldSatisfy` T.isInfixOf "No twos at all."
    t `shouldSatisfy` T.isInfixOf "src/b.py:3"
  describe "findingsNear (what a conjure step is told)" $ do
    let at' i st pl ev = (finding i "bug" st 0.5) {vfPlace = pl, vfEvidence = ev}
        inUnit = Place "src/a.py" (Just "f") (10, 12) (Just "abc1234") (Just 10)
        elsewhere = Place "src/c.py" (Just "h") (1, 2) Nothing (Just 1)
        inTests = Place "src/a__test.py" (Just "test_f") (3, 4) Nothing (Just 3)
        fs =
          [ at' "open" Open inUnit []
          , at' "waiting" Waiting inUnit []
          , at' "dismissed" DismissedHere inUnit []
          , at' "resolved" Resolved inUnit []
          , at' "tests" Open inTests []
          , at' "cites" Open elsewhere [("src/a.py", (11, 11))]
          , at' "far" Open elsewhere [("src/a.py", (40, 41))]
          ]
        near units = findingsNear units "src/a.py" "f" (10, 14) "src/a__test.py" emptyInspectStore {vsFindings = fs}
    it "open or waiting, placed in the unit or its test file, or citing the unit's lines" $
      map (vfId . fst) (near M.empty) `shouldBe` ["open", "waiting", "tests", "cites"]
    it "stale when the unit's fingerprint moved, as the margin says it" $ do
      lookup "open" [(vfId f, st) | (f, st) <- near (M.fromList [("src/a.py", M.fromList [("f", ("0000000", 10))])])] `shouldBe` Just True
      lookup "open" [(vfId f, st) | (f, st) <- near (M.fromList [("src/a.py", fps)])] `shouldBe` Just False
