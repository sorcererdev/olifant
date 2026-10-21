module Olifant.Server.AnticipateSpec (spec) where

import Control.Monad (forM)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.App (Ports (..))
import Olifant.Decide (Question (..), Shape (..), recordedDecider)
import Olifant.Doctor (DoctorPorts (..))
import Olifant.Generation (Plan (..))
import Olifant.Language (noChecker, noRunner)
import Olifant.Language.AstGrep (astGrepSyntax)
import Olifant.Language.Check (noVerifier)
import Olifant.Process (gitStateStamp, neverCancel)
import Olifant.Recognize (Band (..), Recognition (..))
import Olifant.Server.Anticipate
import Olifant.Server.State (ServerState, newServerState, openDoc)
import Olifant.Server.Suggest (Suggestion (..))
import Olifant.Workspace (memoryWorkspace)
import System.Directory (canonicalizePath, doesDirectoryExist, getTemporaryDirectory, listDirectory)
import System.FilePath (makeRelative, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Server.Anticipate (the one Jev question at the cursor)" $ do
  describe "prefixAt" $ do
    it "(a) the unit from the nearest column-0 line to the cursor, and the file's imports" $ do
      let text = T.unlines ["from bistro.booking.models.table import Table", "", "class CloseTable:", "    def __init__(self) -> None:", "        "]
      prefixAt text 5 `shouldBe` (T.intercalate "\n" ["class CloseTable:", "    def __init__(self) -> None:", "        "], "        ", ["from bistro.booking.models.table import Table"])
    it "(b) a unit of 200 lines is its last 120, to the cursor" $ do
      let text = T.unlines ("class Big:" : ["    x_" <> T.pack (show i) <> " = " <> T.pack (show i) | i <- [2 .. 200 :: Int]])
          (unit, cursor, imports) = prefixAt text 200
      length (T.lines unit) `shouldBe` 120
      take 1 (T.lines unit) `shouldBe` ["    x_81 = 81"]
      (cursor, imports) `shouldBe` ("    x_200 = 200", [])
  it "(c) the question: `anticipate`, the candidates and none, from the path, cursor line, unit and imports" $ do
    let q = anticipateQuestion Silence "src/bistro/booking/commands/x.py" ("class X:", "class X:", ["import os"]) [("python-htmx/command", "A command of a service, with its test")]
    qKind q `shouldBe` "anticipate"
    (case qShape q of Choice os -> map fst os; _ -> []) `shouldBe` ["python-htmx/command", "none"]
    map fst (qContext q) `shouldBe` ["path", "cursor_line", "unit", "imports"]
    lookup "imports" (qContext q) `shouldBe` Just "import os"
  describe "anticipateAt (Silence: nothing applies and recognition is unsure)" $ do
    let newCommand = "src/bistro/booking/commands/reschedule_booking.py"
        anticipating answer = withFixture [] $ \s dir -> do
          let answers = dir </> "answers.json"
          TIO.writeFile answers answer
          (_, found) <- anticipateAt (root </> newCommand) 1 (openDoc (root </> newCommand) 1 "class RescheduleBooking:\n" s) (recordedDecider answers neverCancel)
          asked <- readFile (answers <> ".asked")
          pure (found, asked)
    it "(d) a sure answer (0.97) for a convention whose inputs the place fills: its recognition, planned; asked once" $ do
      (found, asked) <- anticipating "{\"answers\": {\"anticipate\": {\"choice\": \"python-htmx/command\", \"confidence\": 0.97}}}"
      asked `shouldBe` "anticipate\n"
      Just an <- pure found
      (anGenerator an, anConfidence an, anMoment an) `shouldBe` ("python-htmx/command", 0.97, Silence)
      let sg = anSuggestion an
      sgKind sg `shouldBe` "recognition"
      fmap rcBand (sgRecognition sg) `shouldBe` Just Offer
      fmap rcReasons (sgRecognition sg) `shouldBe` Just ["anticipated at 0.97"]
      M.keys (planWrites (sgPlan sg)) `shouldSatisfy` elem "src/bistro/booking/commands/reschedule_booking__test.py"
      lookup "command" (sgSets sg) `shouldBe` Just "reschedule_booking"
    it "(e) a generator whose inputs the line cannot fill: a hint, nothing planned (Review Focus 3)" $ do
      (found, _) <- anticipating "{\"answers\": {\"anticipate\": {\"choice\": \"python-htmx/wire\", \"confidence\": 0.97}}}"
      Just an <- pure found
      let sg = anSuggestion an
      fmap rcBand (sgRecognition sg) `shouldBe` Just Hint
      (sgLines sg, M.null (planWrites (sgPlan sg))) `shouldBe` ([], True)
    it "(f) an answer without a confidence: nothing" $ do
      (found, asked) <- anticipating "{\"answers\": {\"anticipate\": {\"choice\": \"python-htmx/command\"}}}"
      asked `shouldBe` "anticipate\n"
      fmap anGenerator found `shouldBe` Nothing
    it "(g) 0.9 at Silence: nothing (below 0.95)" $ do
      (found, _) <- anticipating "{\"answers\": {\"anticipate\": {\"choice\": \"python-htmx/command\", \"confidence\": 0.9}}}"
      fmap anGenerator found `shouldBe` Nothing
  it "(h) a Tie: a blank line two snippets apply at, port-method chosen at 0.93, as a snippet" $ do
    let port = "src/bistro/booking/ports/booking_repository/booking_repository.py"
    withFixture [] $ \s0 dir -> do
      original <- TIO.readFile ("packs/python-htmx/fixtures/app" </> port)
      let text = original <> tieTail
          line = length (T.lines text) + 1
          answers = dir </> "answers.json"
          s = openDoc (root </> port) 1 (text <> "\n") s0
      TIO.writeFile answers "{\"answers\": {\"anticipate\": {\"choice\": \"python-htmx/port-method\", \"confidence\": 0.93}}}"
      (_, cands) <- anticipateCandidates (root </> port) line s
      fmap (\(m, cs) -> (m, map fst cs)) cands `shouldBe` Just (Tie, ["python-htmx/match-arm", "python-htmx/port-method"])
      (_, found) <- anticipateAt (root </> port) line s (recordedDecider answers neverCancel)
      Just an <- pure found
      (anGenerator an, anConfidence an, anMoment an) `shouldBe` ("python-htmx/port-method", 0.93, Tie)
      sgKind (anSuggestion an) `shouldBe` "snippet"

-- | A Protocol method with a default body ending in a match: the blank line
-- after it follows both a method of the Protocol and a match's last case.
tieTail :: Text
tieTail = T.unlines ["", "    def kind(self, booking: Booking) -> str:", "        match booking:", "            case Booking():", "                return \"booking\""]

root :: FilePath
root = "/r"

-- | The python-htmx fixture app at /r (the pack at /pack), served with the
-- real syntax, and a temporary directory for recorded answers.
withFixture :: [(FilePath, Text)] -> (ServerState -> FilePath -> IO a) -> IO a
withFixture extra act = do
  tmp <- getTemporaryDirectory
  pack <- readTree =<< canonicalizePath "packs/python-htmx"
  app <- readTree =<< canonicalizePath "packs/python-htmx/fixtures/app"
  (disk, _) <-
    memoryWorkspace $
      [("/pack" </> p, t) | (p, t) <- M.toList pack]
        ++ [(root </> p, t) | (p, t) <- M.toList app]
        ++ [(root </> ".olifant/config.toml", "[packs]\npython-htmx = { path = \"/pack\" }\n")]
        ++ [(root </> p, t) | (p, t) <- extra]
  let ports = Ports disk (astGrepSyntax (tmp </> "olifant-test-cache")) (const noChecker) (const noVerifier) (const noRunner) (\_ _ -> noRunner) (pure (UTCTime (fromGregorian 2026 10 20) 0)) Nothing Nothing (DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False)) (\_ -> pure Nothing) (\_ _ _ -> pure (Left "no base in tests")) (\_ _ -> pure (Left "no base in tests")) gitStateStamp
  s <- newServerState ports
  withSystemTempDirectory "olifant-anticipate" (act s)

readTree :: FilePath -> IO (M.Map FilePath Text)
readTree top = M.fromList <$> go top
  where
    go d = do
      names <- listDirectory d
      fmap concat . forM names $ \n -> do
        let p = d </> n
        isDir <- doesDirectoryExist p
        if isDir then go p else (\t -> [(makeRelative top p, t)]) <$> TIO.readFile p
