module Olifant.AppSpec (spec) where

import Control.Monad (forM, forM_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as M
import Olifant.Fill (FillRequest (..), Filler (..), ModelFiller (..), Stance (..))
import Olifant.Generation (Plan (..), Step (..))
import Olifant.Generator (replaceGenerator)
import Olifant.Snippet (Snippet (..))
import Olifant.Template (parseTemplate)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.Actions (Action (..))
import Olifant.App
import Olifant.Doctor (Check (..), DoctorPorts (..))
import Olifant.Language (Runner (..), TestRun (..), noChecker, noRunner)
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
  it "doctor --run passes a runner; plain doctor does not" $
    withPythonProject [] $ \ports project -> do
      let tests cs = head [checkDetail c | c <- cs, checkName c == "python: tests"]
          ran = ports {pTester = \_ -> Runner (\_ ids _ -> pure (Right (TestRun [i <> "::t" | i <- ids] [] Nothing)))}
      plain <- doctorChecks ports project False
      tests plain `shouldSatisfy` T.isInfixOf "(not run: olifant doctor --run)"
      units <- pure (head [checkDetail c | c <- plain, checkName c == "python: units"])
      units `shouldSatisfy` T.isInfixOf " found in "
      run <- doctorChecks ran project True
      tests run `shouldSatisfy` (\d -> "ran uv on " `T.isPrefixOf` d && "1 results parsed (pytest)" `T.isInfixOf` d)
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
  it "plans with the stance it is given (a conjure asks for the least code)" $
    withPythonProject [("src/x.py", "x = 1\n")] $ \ports project -> do
      seen <- newIORef []
      let filler = ModelFiller "fake" 0 (Filler (\req -> modifyIORef' seen (++ [frStance req]) >> pure (Right (M.fromList [("body", "x = 2")]))))
          template = either (error . show) id (parseTemplate "{= ? body: block | anything =}")
      Right _ <- planWith ports project planOptions {poFiller = Just filler, poStance = ConjureStance} [Step (replaceGenerator "src/x.py" (1, 1) "x = 1" template) M.empty]
      readIORef seen `shouldReturn` [ConjureStance]
  it "answers a snippet only when a step placed one: a replace that merely starts at the cursor's line is no snippet" $
    withPythonProject [("src/x.py", "x = 1\ny = 1\n")] $ \ports project -> do
      let template = either (error . show) id (parseTemplate "y = 2")
          steps = [Step (replaceGenerator "src/x.py" (2, 2) "y = 1" template) M.empty]
      Right plain <- planWith ports project planOptions {poAt = Just ("src/x.py", 2)} steps
      planSnippet plain `shouldBe` Nothing
      Right placed <- planWith ports project planOptions {poAt = Just ("src/x.py", 2), poSnippet = True} steps
      fmap snBody (planSnippet placed) `shouldBe` Just "y = 2$0"
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
          { pFiles = diskWorkspace "/", pSyntax = astGrepSyntax (tmp </> "olifant-test-cache"), pChecker = const noChecker
          , pVerifier = const noVerifier, pTester = const noRunner, pTesterWithin = \_ _ -> noRunner, pNow = pure (UTCTime (fromGregorian 2026 10 6) 0), pUserConfigDir = Nothing, pHomeDir = Nothing
          , pDoctor = DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False), pTracked = \_ -> pure Nothing, pBaseText = \_ _ _ -> pure (Left "no base in tests"), pBaseFiles = \_ _ -> pure (Left "no base in tests"), pBaseStamp = \_ _ -> pure "" }
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
