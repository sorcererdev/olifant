module Olifant.Server.LoopSpec (spec) where

import Control.Monad (forM_)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.App (Ports, Project)
import Olifant.LspSpec (withPython)
import Olifant.Plans (Anchor (..), Line (..), parsePlan)
import Olifant.Server (ServerState, newServerState, projectFor)
import Olifant.Server.Loop
import Olifant.Server.Plans (Resolved (..), resolveAnchor)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.Process (callProcess, readProcess)
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Server.Loop (the work set)" $ do
  it "(a) on main the baseline is the plan's first commit; an uncommitted plan uses HEAD" $
    withPython [(table, tableText)] $ \_ dir -> do
      initRepo dir
      write dir "plans/t.md" "# T\n"
      git dir ["add", "plans"]
      git dir ["commit", "-q", "-m", "the plan"]
      planSha <- gitLine dir ["rev-parse", "HEAD"]
      write dir table labelEdited
      git dir ["commit", "-q", "-am", "an edit"]
      baseline dir "plans/t.md" `shouldReturn` Just planSha
      write dir "plans/u.md" "# U\n"
      headSha <- gitLine dir ["rev-parse", "HEAD"]
      headSha `shouldNotBe` planSha
      baseline dir "plans/u.md" `shouldReturn` Just headSha
  it "(b) on a branch the baseline is the merge-base with main" $
    withLoop "# T\n" [] $ \_ _ dir -> do
      first <- gitLine dir ["rev-parse", "HEAD"]
      write dir table labelEdited
      git dir ["commit", "-q", "-am", "more on main"]
      mainSha <- gitLine dir ["rev-parse", "HEAD"]
      git dir ["checkout", "-q", "-b", "close-table"]
      write dir table (T.replace "self.seats" "self.seats + 1" labelEdited)
      git dir ["commit", "-q", "-am", "on the branch"]
      mainSha `shouldNotBe` first
      baseline dir "plans/t.md" `shouldReturn` Just mainSha
  it "(c) a changed, an added and a removed unit, with fingerprints" $
    withLoop "# T\n" [(table, threeWays)] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" (parsePlan "# T\n")
      let at n = find ((== n) . cuName) (wsChanged ws)
      fmap cuChange (at "spare") `shouldBe` Just Added
      fmap cuChange (at "Table.other") `shouldBe` Just Removed
      fmap cuFingerprint (at "Table.other") `shouldBe` Just Nothing
      fmap cuHeader (at "Table.label") `shouldBe` Just "def label(self) -> str:"
      case at "Table.label" of
        Just ChangedUnit {cuChange = Changed old, cuFingerprint = Just now, cuNow = Just n, cuBase = Just b} -> do
          old `shouldNotBe` now
          map T.length [old, now] `shouldBe` [7, 7]
          n `shouldSatisfy` T.isInfixOf "\"t-\""
          b `shouldSatisfy` T.isInfixOf "return self.id"
        other -> expectationFailure ("Table.label: " <> show other)
      map cuPath (wsChanged ws) `shouldSatisfy` all (== table)
      wsBaseline ws `shouldSatisfy` (/= Nothing)
  it "(d) a moved unit is one Moved, not a Removed and an Added" $ do
    slotText <- TIO.readFile ("packs/python-htmx/fixtures/app" </> slot)
    withLoop "# T\n" [(table, T.replace helper "" tableText), (slot, slotText <> "\n\n" <> helper)] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" []
      map (\c -> (cuPath c, cuName c, cuChange c)) (wsChanged ws) `shouldBe` [(slot, "helper", Moved table "helper")]
  it "(e) accounted units are left out, across plans" $
    withLoopPorts "# T\n" [(table, labelEdited)] $ \ports dir -> do
      (s0, p0) <- loaded ports dir
      fp <- maybe (fail "Table.label does not resolve") pure . rsFingerprint =<< resolveAnchor s0 p0 (Anchor table "Table.label" 0 0)
      write dir "plans/other.md" ("# O\n\n- [x] x @" <> T.pack table <> ":Table.label ✓ " <> fp <> "\n")
      -- loaded again: a server sees a new file once its listing is stale
      (s, p) <- loaded ports dir
      ws <- workSet s p "plans/t.md" (parsePlan "# T\n")
      map cuName (wsChanged ws) `shouldContain` ["Table.label"]
      map cuName (wsUnaccounted ws) `shouldNotContain` ["Table.label"]
      map cuName (wsUnaccounted ws) `shouldContain` ["Table"]
  it "(f) a step's own changed anchor is touched, not unaccounted" $ do
    let plan = "# T\n\n- [ ] tidy the label @" <> T.pack table <> ":Table.label\n"
    withLoop plan [(table, labelEdited)] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" (parsePlan plan)
      map (\(l, c) -> (lnRow l, cuName c)) (wsTouched ws) `shouldBe` [(2, "Table.label")]
      map cuName (wsUnaccounted ws) `shouldNotContain` ["Table.label"]
      map cuName (wsUnaccounted ws) `shouldContain` ["Table"]
  it "(g) files without a language are skipped" $ do
    pyproject <- TIO.readFile "packs/python-htmx/fixtures/app/pyproject.toml"
    withLoop "# T\n" [("pyproject.toml", pyproject <> "\n# edited\n"), (table, labelEdited)] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" []
      wsChanged ws `shouldSatisfy` not . null
      map cuPath (wsChanged ws) `shouldSatisfy` all (== table)
  it "(h) no git: Nothing baseline, empty sets" $
    withPython [("plans/t.md", "# T\n")] $ \ports dir -> do
      (s, p) <- loaded ports dir
      baseline dir "plans/t.md" `shouldReturn` Nothing
      workSet s p "plans/t.md" [] `shouldReturn` WorkSet Nothing [] [] 0 []
  it "(i) the cap" $
    withLoop "# T\n" [("src/bistro/many.py", T.concat ["def f" <> tshow i <> "() -> int:\n    return " <> tshow i <> "\n\n\n" | i <- [1 .. 45 :: Int]])] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" []
      length (wsChanged ws) `shouldBe` 45
      length (wsUnaccounted ws) `shouldBe` unaccountedCap
      unaccountedCap `shouldBe` 40
      wsMore ws `shouldBe` 5

-- | The python fixture with TABLE's units and the plan, committed on main,
-- then EDITS written, and the project loaded.
withLoop :: Text -> [(FilePath, Text)] -> (ServerState -> Project -> FilePath -> IO a) -> IO a
withLoop plan edits act = withLoopPorts plan edits $ \ports dir -> loaded ports dir >>= \(s, p) -> act s p dir

-- | 'withLoop' before the project is loaded.
withLoopPorts :: Text -> [(FilePath, Text)] -> (Ports -> FilePath -> IO a) -> IO a
withLoopPorts plan edits act =
  withPython [("plans/t.md", plan), (table, tableText)] $ \ports dir -> do
    initRepo dir
    forM_ edits (uncurry (write dir))
    act ports dir

loaded :: Ports -> FilePath -> IO (ServerState, Project)
loaded ports dir = do
  s0 <- newServerState ports
  projectFor (dir </> "plans/t.md") s0 >>= \case
    (s, Just (_, p)) -> pure (s, p)
    _ -> fail "no project"

initRepo :: FilePath -> IO ()
initRepo dir = do
  git dir ["init", "-q", "-b", "main"]
  git dir ["add", "."]
  git dir ["commit", "-q", "-m", "the fixture"]

git :: FilePath -> [String] -> IO ()
git dir args = callProcess "git" (["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t"] ++ args)

gitLine :: FilePath -> [String] -> IO Text
gitLine dir args = T.strip . T.pack <$> readProcess "git" (["-C", dir] ++ args) ""

write :: FilePath -> FilePath -> Text -> IO ()
write dir path text = createDirectoryIfMissing True (takeDirectory (dir </> path)) >> TIO.writeFile (dir </> path) text

table, slot :: FilePath
table = "src/bistro/booking/models/table.py"
slot = "src/bistro/booking/models/slot.py"

tableText, helper, labelEdited, threeWays :: Text
tableText =
  T.unlines
    [ "from dataclasses import dataclass", "", "", "@dataclass(frozen=True)", "class Table:", "    id: str", "    seats: int", ""
    , "    def label(self) -> str:", "        return self.id", "", "    def other(self) -> int:", "        return self.seats", "", "" ]
    <> helper
helper = "def helper() -> int:\n    return 1\n"
labelEdited = T.replace "return self.id" "return \"t-\" + self.id" tableText
threeWays = T.replace "    def other(self) -> int:\n        return self.seats\n" "" labelEdited <> "\n\ndef spare() -> None:\n    pass\n"

tshow :: Show a => a -> Text
tshow = T.pack . show
