module Olifant.Server.ResolveSpec (spec) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime (..), fromGregorian)
import Control.Monad (forM)
import Olifant.App (Ports (..), Project)
import Olifant.Doctor (DoctorPorts (..))
import Olifant.Language (Match (..), noChecker, noRunner, querying)
import Olifant.Language.Check (noVerifier)
import Olifant.Language.Grammar (GrammarFailure (NoSource))
import Olifant.Process (gitStateStamp)
import Olifant.Server (ServerState, newServerState, projectFor)
import Olifant.Server.Resolve
import Olifant.Server.Telemetry (Decision (..))
import Olifant.Server.Unit (Named (..))
import Olifant.Workspace (memoryWorkspace)
import System.Directory (canonicalizePath, doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, (</>))
import Test.Hspec
import Olifant.Decide (recordedDecider)
import Olifant.Plans (Line (..), parsePlan)
import Olifant.Process (neverCancel)
import System.Directory (doesFileExist)
import System.IO.Temp (withSystemTempDirectory)

-- | A small Python project's units, profiled from their text.
app, tests :: Text
app = "def read_tracker(path):\n    \"\"\"Read the 2018 tracker totals.\"\"\"\n    return load(path)\n\ndef write_totals(rows):\n    return rows\n"
tests = "def test_read_tracker():\n    assert read_tracker(\"x\")\n"

idx :: [UnitProfile]
idx =
  [ profileOf "app.py" app False (Named "read_tracker" (1, 3) 1)
  , profileOf "app.py" app False (Named "write_totals" (5, 6) 5)
  , profileOf "tests/test_app.py" tests True (Named "test_read_tracker" (1, 2) 1)
  ]

spec :: Spec
spec = describe "Olifant.Server.Resolve" $ do
 describe "Resolve rung" rung
 describe "Resolve candidates" $ do
  it "(a) splits camel and snake, keeps digits, drops stop words" $
    lineTokens "The 2018 loader reads read_tracker in Comparison2018Admin" `shouldBe` ["2018", "loader", "reads", "read", "tracker", "comparison", "2018", "admin"]
  it "(b) a backticked or snake identifier equal to a unit name ranks that unit first" $ do
    identifiers "The 2018 loader `read_tracker` keeps Comparison2018Admin and write_totals." `shouldBe` ["read_tracker", "2018", "Comparison2018Admin", "write_totals"]
    map (urName . upRef . fst) (take 1 (candidates "The 2018 loader `read_tracker` keeps totals" idx)) `shouldBe` ["read_tracker"]
  it "(c) a test unit drops below its subject unless the line says test" $ do
    map (urName . upRef . fst) (take 1 (candidates "the tracker is read" idx)) `shouldBe` ["read_tracker"]
    map (urName . upRef . fst) (take 1 (candidates "a test checks the tracker is read" idx)) `shouldBe` ["test_read_tracker"]
  it "(d) at most 8, none at zero" $ do
    length (candidates "zzz qqq" idx) `shouldBe` 0
    length (candidates "read" (concat (replicate 5 idx))) `shouldBe` 8
  it "(e) projectIndex reuses a file's profiles while its text is unchanged" $ do
    (s, proj, parses) <- indexServer
    first <- projectIndex s proj
    map (urName . upRef) first `shouldContain` ["Foo.bar"]
    n1 <- readIORef parses
    second <- projectIndex s proj
    map upRef second `shouldBe` map upRef first
    readIORef parses `shouldReturn` n1

-- | A project over the python-htmx pack with a syntax stub that counts its parses.
indexServer :: IO (ServerState, Project, IORefInt)
indexServer = do
  pack <- readTree =<< canonicalizePath "packs/python-htmx"
  (disk, _) <-
    memoryWorkspace $
      [("/pack" </> p, t) | (p, t) <- M.toList pack]
        ++ [ ("/r1/.olifant/config.toml", "[packs]\npython-htmx = { path = \"/pack\" }\n")
           , ("/r1/src/a.py", "class Foo:\n    def bar(self):\n        return 1\n")
           , ("/r1/src/b.py", "class Foo:\n    def bar(self):\n        return 1\n")
           , ("/r1/.venv/c.py", "class Foo:\n    def bar(self):\n        return 1\n")
           , ("/r1/notes.txt", "x\n")
           , ("/r1/plans/t.md", "# T\n") ]
  parses <- newIORef (0 :: Int)
  let syntax = querying (\_ _ _ -> atomicModifyIORef' parses (\n -> (n + 1, ())) >> pure (Right [Match (a, 0) (z, 0) "" (0, 0) | (a, z) <- [(0, 2), (1, 2)]]))
      ports = Ports disk syntax (const noChecker) (const noVerifier) (const noRunner) (\_ _ -> noRunner) (pure (UTCTime (fromGregorian 2026 10 25) 0)) Nothing Nothing (DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False) (\_ -> pure Nothing) (\_ -> pure []) (\_ -> pure (Left NoSource))) (\_ -> pure Nothing) (\_ _ _ -> pure (Left "no base in tests")) (\_ _ -> pure (Left "no base in tests")) gitStateStamp
  s0 <- newServerState ports
  projectFor "/r1/plans/t.md" s0 >>= \case
    (s, Just (_, proj)) -> pure (s, proj, parses)
    _ -> fail "no project at /r1"

type IORefInt = IORef Int

readTree :: FilePath -> IO (M.Map FilePath Text)
readTree dir = M.fromList <$> go dir
  where
    go d = do
      names <- listDirectory d
      fmap concat . forM names $ \n -> do
        let p = d </> n
        isDir <- doesDirectoryExist p
        if isDir then go p else (\t -> [(makeRelative dir p, t)]) <$> TIO.readFile p

-- | Ten units that all say tracker, ranked by the lexical rung.
many :: [UnitProfile]
many = map fst (candidates "the tracker" [profileOf "app.py" text False (Named n (i, i) i) | (i, n) <- zip [1 ..] names])
  where
    names = ["read_tracker", "write_tracker", "tracker", "tracker_rows", "tracker_totals", "tracker_cols", "tracker_x", "tracker_y", "tracker_z", "tracker_w"]
    text = T.unlines ["def " <> n <> "(): pass" | n <- names]

ctx :: LineContext
ctx = LineContext "the tracker is read" Nothing "Loader" ""

rung :: Spec
rung = do
  let withAnswers answers body = withSystemTempDirectory "olifant-resolve" $ \dir -> do
        let file = dir </> "answers.json"
        writeFile file answers
        body file (recordedDecider file neverCancel)
      asked file = doesFileExist (file <> ".asked") >>= \case
        True -> lines <$> readFile (file <> ".asked")
        False -> pure []
  it "(a) a sure first round resolves with one call" $
    withAnswers "{\"answers\": {\"plan-unit\": {\"choice\": \"app.py:read_tracker\", \"confidence\": 0.9}}}" $ \file d -> do
      (r, ds) <- resolveLine d (pure True) ctx many
      r `shouldBe` Resolved (UnitRef "app.py" "read_tracker") Headers 0.9
      map dcRung ds `shouldBe` ["jev-1"]
      asked file `shouldReturn` ["plan-unit"]
  it "(b) unsure on headers widens to code, then to the plan around" $
    withAnswers "{\"answers\": {\"plan-unit\": {\"choice\": \"app.py:read_tracker\", \"confidence\": 0.4}, \"plan-unit-code\": {\"choice\": \"app.py:read_tracker\", \"confidence\": 0.5}, \"plan-unit-around\": {\"choice\": \"app.py:tracker\", \"confidence\": 0.8}}}" $ \file d -> do
      (r, ds) <- resolveLine d (pure True) ctx many
      r `shouldBe` Resolved (UnitRef "app.py" "tracker") Around 0.8
      map dcRung ds `shouldBe` ["jev-1", "jev-2", "jev-3"]
      asked file `shouldReturn` ["plan-unit", "plan-unit-code", "plan-unit-around"]
  it "(c) unsure after three rounds keeps the candidates" $
    withAnswers "{\"answers\": {\"plan-unit\": {\"choice\": \"app.py:tracker\", \"confidence\": 0.4}, \"plan-unit-code\": {\"choice\": \"app.py:tracker\", \"confidence\": 0.4}, \"plan-unit-around\": {\"choice\": \"app.py:tracker\", \"confidence\": 0.4}}}" $ \_ d -> do
      (r, _) <- resolveLine d (pure True) ctx many
      r `shouldBe` Unresolved (map upRef (take 8 many)) 0.4
  it "(d) no budget left asks nothing" $
    withAnswers "{\"answers\": {}}" $ \file d -> do
      (r, ds) <- resolveLine d (pure False) ctx many
      r `shouldBe` Unresolved (map upRef (take 8 many)) 0
      ds `shouldBe` []
      doesFileExist (file <> ".asked") `shouldReturn` False
  it "(e) a line with no candidate asks nothing" $
    withAnswers "{\"answers\": {}}" $ \file d -> do
      fmap fst (resolveLine d (pure True) ctx []) `shouldReturn` Unresolved [] 0
      doesFileExist (file <> ".asked") `shouldReturn` False
  it "(f) lineContext strips anchors, names the question above and joins three lines either side" $ do
    let ls = parsePlan (T.unlines ["# Loader", "- one", "- two", "- ? How is the tracker read?", "  - The loader reads `read_tracker`   @app.py:read_tracker ✓ abc1234", "  - It keeps totals", "Some prose.", "- [ ] Add a step @app.py:write_totals", "- three", "- four"])
        target = ls !! 5
    lineContext ls target
      `shouldBe` LineContext "It keeps totals" (Just "How is the tracker read?") "Loader" (T.intercalate "\n" ["two", "? How is the tracker read?", "The loader reads `read_tracker`", "[ ] Add a step", "three", "four"])
    lcLine (lineContext ls (ls !! 4)) `shouldBe` "The loader reads `read_tracker`"
    lnRow target `shouldBe` 5
