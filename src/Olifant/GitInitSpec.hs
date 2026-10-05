module Olifant.GitInitSpec (spec) where

import Data.Text qualified as T
import Olifant.GitInit (gitInit)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, findExecutable)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "olifant init's first commit (gitInit)" $ do
  let withGit k = findExecutable "git" >>= maybe (pendingWith "git is not installed") (const k)
      git dir args = readProcessWithExitCode "git" (["-C", dir, "-c", "user.email=t@t", "-c", "user.name=t"] ++ args) ""
  it "makes a repository and its first commit in a new folder" $ withGit $
    withSystemTempDirectory "olifant-git" $ \dir -> do
      writeFile (dir </> "a.txt") "a"
      _ <- git dir ["init", "-q"] -- (a person's config gives the identity; here, the repo's)
      _ <- git dir ["config", "user.email", "t@t"] >> git dir ["config", "user.name", "t"]
      gitInit dir "olifant init: p" `shouldReturn` Right ()
      (_, out, _) <- git dir ["log", "--format=%s"]
      lines out `shouldBe` ["olifant init: p"]
  it "inside another repository: no nested repository, and says why there is no commit (review I1)" $ withGit $
    withSystemTempDirectory "olifant-git" $ \parent -> do
      _ <- git parent ["init", "-q"]
      let app = parent </> "apps" </> "bistro"
      createDirectoryIfMissing True app
      writeFile (app </> "a.txt") "a"
      r <- gitInit app "olifant init: p"
      doesDirectoryExist (app </> ".git") `shouldReturn` False
      r `shouldSatisfy` either (T.isInfixOf "inside the git repository at") (const False)
