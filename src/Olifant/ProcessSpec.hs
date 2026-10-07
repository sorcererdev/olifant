module Olifant.ProcessSpec (spec) where

import Data.Set qualified as S
import Olifant.Process (gitTracked)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Process" $
  describe "gitTracked" $ do
    it "lists the files git tracks under a folder (relative to it), not the untracked ones" $
      withSystemTempDirectory "olifant-tracked" $ \dir -> do
        createDirectoryIfMissing True (dir </> "app/src")
        writeFile (dir </> "app/src/old.py") "x = 1\n"
        writeFile (dir </> "top.txt") "t\n"
        callProcess "git" ["-C", dir, "init", "-q"]
        callProcess "git" ["-C", dir, "add", "."]
        callProcess "git" ["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "first"]
        writeFile (dir </> "app/src/new.py") "y = 2\n"
        gitTracked (dir </> "app") `shouldReturn` Just (S.fromList ["src/old.py"])
    it "says nothing for a folder under no git repository" $
      withSystemTempDirectory "olifant-untracked" $ \dir ->
        gitTracked dir `shouldReturn` Nothing
