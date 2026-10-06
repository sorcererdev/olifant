module Olifant.AppSpec (spec) where

import Control.Monad (forM, forM_)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.Actions (Action (..))
import Olifant.App
import Olifant.Language (noChecker)
import Olifant.Language.AstGrep (astGrepSyntax)
import Olifant.Language.Check (noVerifier)
import Olifant.Rules (Finding (..), FixOffer (..))
import Olifant.Workspace (diskWorkspace)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, listDirectory)
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "Olifant.App" $ do
  it "finds a file's rule findings with their fixes' choices (the operation check and the server share)" $
    withPythonProject [(closeTable, closeTableText)] $ \ports project -> do
      (fs, _) <- findings ports project [closeTable]
      let ours = [f | f <- fs, fRule f == "commands-use-ports"]
      map fRule ours `shouldBe` ["commands-use-ports"]
      case concatMap fFixes ours of
        o : _ -> lookup "port" (foChoices o) `shouldSatisfy` maybe False (elem "booking_repository")
        [] -> expectationFailure "no fixes"
  it "lists the actions at a cursor (the operation ⚡ and the server share)" $ do
    given <- readTree "packs/python-htmx/generators/add-dependency/tests/inline/given"
    withPythonProject (M.toList given) $ \ports project -> do
      let file = "src/bistro/booking/commands/cancel_booking.py"
          line = 1 + length (takeWhile (not . T.isInfixOf "def __init__") (T.lines (given M.! file)))
      acts <- actionsAt' ports project file (line, 5)
      map actLabel acts `shouldSatisfy` elem "Add a port"
  where
    closeTable = "src/bistro/booking/commands/close_table.py"
    closeTableText = T.unlines ["import sqlite3", "", "", "class CloseTableInput:", "    pass", "", "", "class CloseTable:", "    def __init__(self) -> None:", "        \"\"\"Its ports.\"\"\"", "", "    def handle(self, input: CloseTableInput) -> None:", "        raise NotImplementedError(\"later\")"]

-- | The python-htmx fixture app with some files changed, as a project on
-- disk using the pack from this repository.
withPythonProject :: [(FilePath, Text)] -> (Ports -> Project -> IO a) -> IO a
withPythonProject changed act = do
  tmp <- getTemporaryDirectory
  packs <- canonicalizePath "packs/python-htmx"
  fixture <- readTree "packs/python-htmx/fixtures/app"
  withSystemTempDirectory "olifant-app" $ \dir0 -> do
    dir <- canonicalizePath dir0
    forM_ (M.toList (M.union (M.fromList changed) fixture)) $ \(p, t) ->
      createDirectoryIfMissing True (takeDirectory (dir </> p)) >> TIO.writeFile (dir </> p) t
    createDirectoryIfMissing True (dir </> ".olifant")
    TIO.writeFile (dir </> ".olifant/config.toml") ("[packs]\npython-htmx = { path = \"" <> T.pack packs <> "\" }\n")
    let ports = Ports
          { pFiles = diskWorkspace "/", pSyntax = astGrepSyntax (tmp </> "olifant-test-cache"), pChecker = noChecker
          , pVerifier = noVerifier, pNow = pure (UTCTime (fromGregorian 2026 10 6) 0), pUserConfigDir = Nothing, pHomeDir = Nothing }
    openProject ports dir >>= \case
      Left _ -> fail "the project's catalog did not load"
      Right project -> act ports project

readTree :: FilePath -> IO (M.Map FilePath Text)
readTree root = M.fromList <$> go root
  where
    go d = do
      names <- listDirectory d
      fmap concat . forM names $ \n -> do
        let p = d </> n
        isDir <- doesDirectoryExist p
        if isDir then go p else (\t -> [(makeRelative root p, t)]) <$> TIO.readFile p
