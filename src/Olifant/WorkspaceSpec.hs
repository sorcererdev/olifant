module Olifant.WorkspaceSpec (spec) where

import Olifant.Workspace
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

contract :: SpecWith (Workspace IO)
contract = do
  it "reads back what it wrote, including non-ASCII text, creating parent dirs" $ \ws -> do
    writeText ws "notes/deep/hello.txt" "héllo ✓\n"
    readText ws "notes/deep/hello.txt" `shouldReturn` Just "héllo ✓\n"
    fileExists ws "notes/deep/hello.txt" `shouldReturn` True
  it "reports missing files" $ \ws -> do
    readText ws "missing.txt" `shouldReturn` Nothing
    fileExists ws "missing.txt" `shouldReturn` False
  it "lists files recursively, relative to the directory, sorted" $ \ws -> do
    writeText ws "gen/b/x.toml" "1"
    writeText ws "gen/a.toml" "2"
    writeText ws "other.txt" "3"
    listFiles ws "gen" `shouldReturn` ["a.toml", "b/x.toml"]
  it "lists nothing for a missing directory" $ \ws ->
    listFiles ws "nope" `shouldReturn` []
  it "scopes paths with `within`" $ \ws -> do
    writeText (within "proj" ws) "a.txt" "x"
    readText ws "proj/a.txt" `shouldReturn` Just "x"
    readText (within "proj/sub/.." ws) "a.txt" `shouldReturn` Just "x"

spec :: Spec
spec = do
  describe "memory adapter" $ around (\act -> memoryWorkspace [] >>= act . fst) contract
  describe "disk adapter" $ around (\act -> withSystemTempDirectory "olifant" (act . diskWorkspace)) contract
  describe "paths" $ do
    it "normalises . and .." $
      map normalisePath ["a/./b", "a/../b", "a/b/..", "/x/../y", "", "../a"] `shouldBe` ["a/b", "b", "a", "/y", ".", "../a"]
    it "treats only relative paths inside the root as safe" $
      map isSafeRelative ["app/x.rb", "a/../b", "../x", "a/../../x", "/etc/passwd", ""] `shouldBe` [True, True, False, False, False, False]
