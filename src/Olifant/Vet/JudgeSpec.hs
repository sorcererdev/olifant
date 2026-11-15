module Olifant.Vet.JudgeSpec (spec) where

import Data.IORef (readIORef, writeIORef)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..), PlansConfig (..))
import Olifant.Decide (Answer (No, Yes), Confidence (..), Question (..), Shape (..))
import Olifant.Decide qualified as D
import Olifant.Plans (Kind (..))
import Olifant.Plans.Sentences (Sentence (..))
import Olifant.Server.Checker (CheckAsk (..), CheckMode (..))
import Olifant.Server.CheckerSpec (checkerServer)
import Olifant.Server.Errand (RunResult (..))
import Olifant.Server.Loop (checkWith)
import Olifant.Server.State (ServerState (..))
import Olifant.Server.Telemetry (ErrandRecord (..), Record (..), readRecords, recordsPath)
import Olifant.Server.Unit (Named (..))
import Olifant.Vet.Find (RawFinding (..))
import Olifant.Vet.Judge
import Olifant.Vet.Steering (steering)
import Olifant.Vet.Store
import Olifant.Vet.StoreSpec (finding)
import Olifant.Workspace (Workspace (..))
import Test.Hspec

raw :: (Int, Int) -> RawFinding
raw ls = RawFinding "bug" "src/a.py" ls "It breaks." Nothing [("src/b.py", (1, 2))]

text30 :: T.Text
text30 = T.unlines ["line " <> T.pack (show i) | i <- [1 :: Int .. 30]]

spec :: Spec
spec = describe "Olifant.Vet.Judge" $ do
  let st = steering "## Rules\n### A\nNo sqlite outside adapters.\n### B\nTime comes from the clock.\n"
      units = [Named "f" (10, 20) 10, Named "g" (30, 40) 30]
  describe "place" $ do
    it "a finding in a changed unit is open, with the unit" $ do
      let (pl, _, stt) = place st "src/a.py" text30 units (S.fromList ["f"]) (raw (12, 13))
      (plUnit pl, stt) `shouldBe` (Just "f", Open)
      plFingerprint pl `shouldSatisfy` (/= Nothing)
    it "a range that starts before a changed unit and overlaps it is open in that unit (1a's rule)" $ do
      let (pl, _, stt) = place st "src/a.py" text30 units (S.fromList ["f"]) (raw (3, 15))
      (plUnit pl, stt) `shouldBe` (Just "f", Open)
    it "outside any changed unit it is elsewhere" $ do
      let (_, _, stt) = place st "src/a.py" text30 units (S.fromList ["f"]) (raw (32, 33))
      stt `shouldBe` Elsewhere
    it "in another file it is elsewhere" $ do
      let (_, _, stt) = place st "src/a.py" text30 units (S.fromList ["f"]) (raw (12, 13)) {rfFile = "src/other.py"}
      stt `shouldBe` Elsewhere
    it "a rule that is not a heading is removed" $ do
      let (_, r1, _) = place st "src/a.py" text30 units (S.fromList ["f"]) (raw (12, 13)) {rfRule = Just "No such rule"}
          (_, r2, _) = place st "src/a.py" text30 units (S.fromList ["f"]) (raw (12, 13)) {rfRule = Just "A"}
      (r1, r2) `shouldBe` (Nothing, Just "A")
  describe "matchStored (the design's step-4 table)" $ do
    let new = finding "n" "bug" Open 0.9
        stored s = (finding "s" "bug" s 0.9)
    it "open and waiting merge" $ do
      matchStored new (stored Open) "f" `shouldBe` Merge
      matchStored new (stored Waiting) "f" `shouldBe` Merge
    it "dismissed here drops in the same unit, and is new in another" $ do
      matchStored new (stored DismissedHere) "f" `shouldBe` Drop
      matchStored new (stored DismissedHere) "g" `shouldBe` New
    it "dismissed everywhere drops" $ matchStored new (stored DismissedEverywhere) "g" `shouldBe` Drop
    it "withheld, refuted, unsure and elsewhere merge" $
      map (\s -> matchStored new (stored s) "f") [Withheld, Refuted, Unsure, Elsewhere] `shouldBe` [Merge, Merge, Merge, Merge]
    it "resolved and gone are new" $
      map (\s -> matchStored new (stored s) "f") [Resolved, Gone] `shouldBe` [New, New]
  describe "samePairs" $
    it "takes stored findings sharing the file, the rule or a cited line" $ do
      let a = finding "a" "bug" Open 0.9
          b = (finding "b" "bug" Open 0.9) {vfPlace = Place "src/z.py" Nothing (1, 1) Nothing, vfRule = Just "Other", vfEvidence = []}
          c = b {vfId = "c", vfRule = Just "A rule"}
          d = b {vfId = "d", vfEvidence = [("src/b.py", (2, 5))]}
      map vfId (samePairs emptyVetStore {vsFindings = [a, b, c, d]} (raw (12, 13)) {rfRule = Just "A rule"} (Place "src/a.py" Nothing (12, 13) Nothing)) `shouldBe` ["a", "c", "d"]
  describe "the Jev questions" $ do
    it "nitpick: the 1a wording, with the cited rule's text" $ do
      let q = nitpick st (raw (12, 13)) {rfRule = Just "A"} "code"
      qKind q `shouldBe` "nitpick"
      qInstructions q `shouldBe` "Is this code review finding a nitpick?"
      lookup "project rule" (qContext q) `shouldBe` Just "No sqlite outside adapters."
      lookup "place" (qContext q) `shouldBe` Just "src/a.py:12"
    it "nitpick: with no rule, the whole Rules section" $
      lookup "project rules" (qContext (nitpick st (raw (12, 13)) "code")) `shouldBe` Just "## Rules\n### A\nNo sqlite outside adapters.\n### B\nTime comes from the clock."
    it "nitpick: vet.md's sections replace the criteria" $ do
      let q = nitpick (steering "## A nitpick is\nonly typos\n## Worth saying is\nanything\n") (raw (12, 13)) "code"
      qShape q `shouldBe` Noul (Just ("only typos", "anything"))
    it "same problem: the 1a state" $ do
      let q = sameProblem "a" "b" "ca" "cb"
      (qKind q, map fst (qContext q)) `shouldBe` ("same", ["finding A", "finding B", "code at A", "code at B"])
    it "keepOf is 1 - P(nitpick)" $ do
      keepOf (Yes (Confidence 0.3), []) `shouldBe` 0.7
      keepOf (No (Confidence 0.9), []) `shouldBe` 0.9
      keepOf (D.Unsure, []) `shouldBe` 1
  describe "citationsOk (hostile places)" $ do
    let check ev = do
          (s, p) <- checkerServer
          citationsOk s p (catalogPlans (prCatalog p)) (raw (1, 1)) {rfEvidence = ev}
        bad ev = check ev >>= (`shouldSatisfy` either (const True) (const False))
    it "refuses a path outside the root" $ bad [("../a.py", (1, 2))]
    it "refuses a fenced directory" $ bad [("docs/table.txt", (1, 1))]
    it "refuses .env" $ bad [(".env", (1, 1))]
    it "refuses a first line past the end of the file" $ bad [("src/a.py", (50, 60))]
    it "clamps an end line past the end of the file" $ check [("src/models.py", (25, 999))] >>= (`shouldSatisfy` either (const False) (not . null))
  describe "checkWith" $
    it "logs vet-check and leaves the plans' estimates" $ do
      (s, p) <- checkerServer
      let root = prRoot p
          cfg = (catalogPlans (prCatalog p)) {pcCheckModel = "sonnet"}
          reply = "```json\n{\"verdict\": \"right\", \"why\": \"yes\", \"evidence\": [{\"path\": \"src/a.py\", \"line\": 2}]}\n```"
          checker _ _ _ = pure (Right (RunResult reply 1 (Just 0.02) Nothing Nothing Nothing [] 0 []))
          x = Sentence 0 0 0 10 Prose "It breaks." "" Nothing Nothing
      writeIORef (ssEstimates s) (M.fromList [(root, M.empty)])
      _ <- checkWith "vet-check" s p cfg checker True (UTCTime (fromGregorian 2026 11 15) 0) "src/a.py" (CheckAsk CheckClaim False []) x
      readIORef (ssEstimates s) >>= (`shouldBe` M.fromList [(root, M.empty)])
      recs <- maybe [] (fst . readRecords) <$> readText (pFiles (ssPorts s)) (recordsPath root)
      [erVerb e | AnErrand e <- recs] `shouldBe` ["vet-check"]
