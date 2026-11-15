module Olifant.Inspect.StoreSpec (spec, finding) where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.Catalog (defaultInspect)
import Olifant.Server.Checker (Print (..))
import Olifant.Inspect.Store
import Olifant.Workspace (diskWorkspace)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

at0 :: UTCTime
at0 = UTCTime (fromGregorian 2026 11 15) 0

-- | A finding for specs: id, kind, status, keep.
finding :: Text -> Text -> Status -> Double -> InspectFinding
finding i kind st keep =
  InspectFinding i kind (Just "A rule") "It breaks the rule." (Place "src/a.py" (Just "f") (10, 12) (Just "abc1234") (Just 10)) [("src/b.py", (3, 4))] [Print "src/b.py" (Just "g") "def5678"] (Just keep) Nothing (Just "right") (Just "because") st 1 "sonnet" 0.04 at0

spec :: Spec
spec = describe "Olifant.Inspect.Store" $ do
  it "round trip" $ do
    let vs = InspectStore [finding (statusName s) "bug" s 0.9 | s <- [minBound .. maxBound]] (M.fromList [("src/a.py", S.fromList ["abc1234"])]) (M.fromList [("src/a.py", S.fromList ["g"])])
    decodeStore (encodeStore vs) `shouldBe` Right vs
  it "no file is empty" $ withSystemTempDirectory "inspect" $ \dir ->
    readInspectStore (diskWorkspace "/") dir >>= (`shouldBe` Right emptyInspectStore)
  it "corrupt file is Left, and update leaves it" $ withSystemTempDirectory "inspect" $ \dir -> do
    let file = dir </> inspectStorePath
    createDirectoryIfMissing True (dir </> ".olifant/state/inspect")
    TIO.writeFile file "{\"version\": 9}"
    r <- updateInspectStore (diskWorkspace "/") dir (\vs -> (vs, ()))
    r `shouldSatisfy` either (const True) (const False)
    TIO.readFile file >>= (`shouldBe` "{\"version\": 9}")
  it "update writes, and a read gives it back" $ withSystemTempDirectory "inspect" $ \dir -> do
    let f = finding "x" "bug" Open 0.9
    updateInspectStore (diskWorkspace "/") dir (\vs -> (vs {vsFindings = [f]}, ())) >>= (`shouldBe` Right ())
    readInspectStore (diskWorkspace "/") dir >>= (`shouldBe` Right emptyInspectStore {vsFindings = [f]})
  it "dismiss here and everywhere" $ do
    let vs = emptyInspectStore {vsFindings = [finding "a" "bug" Open 0.9, finding "b" "bug" Open 0.9]}
    map vfStatus (vsFindings (dismiss False "a" (dismiss True "b" vs))) `shouldBe` [DismissedHere, DismissedEverywhere]
  it "shownIn caps and orders" $ do
    let opens = [finding i "bug" Open k | (i, k) <- [("a", 0.9), ("b", 0.8), ("c", 0.95), ("d", 0.85), ("e", 0.99)]]
        others = [finding "w" "bug" Waiting 1, finding "h" "bug" Withheld 1, finding "z" "design" Open 1]
    map vfId (shownIn defaultInspect "src/a.py" emptyInspectStore {vsFindings = opens ++ others}) `shouldBe` ["e", "c", "a"]
    shownIn defaultInspect "src/other.py" emptyInspectStore {vsFindings = opens} `shouldBe` []
  it "findingId is 12 hex digits and stable" $ do
    let pl = Place "src/a.py" Nothing (1, 2) Nothing Nothing
    findingId at0 pl "s" `shouldBe` findingId at0 pl "s"
    length (show (findingId at0 pl "s")) `shouldBe` 14
