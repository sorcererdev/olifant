module Olifant.WorkspaceSpec (spec) where

import Data.ByteString qualified as BS
import Olifant.Workspace
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (findExecutable)
import System.Process (readProcess)
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
  it "inspects paths as absent, readable text, or a directory" $ \ws -> do
    writeText ws "d/f.txt" "hi"
    inspectPath ws "d/f.txt" `shouldReturn` Readable "hi"
    inspectPath ws "d" `shouldReturn` Directory
    inspectPath ws "nope" `shouldReturn` Absent
  it "scopes paths with `within`" $ \ws -> do
    writeText (within "proj" ws) "a.txt" "x"
    readText ws "proj/a.txt" `shouldReturn` Just "x"
    readText (within "proj/sub/.." ws) "a.txt" `shouldReturn` Just "x"

spec :: Spec
spec = do
  describe "overlay (spec §4.8: plan against an unsaved buffer)" $ do
    it "reads the overlaid file's text, delegates the rest, and lists it even if absent" $ do
      (ws, _) <- memoryWorkspace [("a.txt", "disk"), ("b.txt", "other")]
      let o = overlay "a.txt" "unsaved" ws
          new = overlay "c.txt" "fresh" ws
      readText o "a.txt" `shouldReturn` Just "unsaved"
      readText o "b.txt" `shouldReturn` Just "other"
      inspectPath o "a.txt" `shouldReturn` Readable "unsaved"
      listFiles new "." >>= (`shouldSatisfy` elem "c.txt")
      fileExists new "c.txt" `shouldReturn` True
  describe "memory adapter" $ around (\act -> memoryWorkspace [] >>= act . fst) contract
  describe "disk adapter" $ around (\act -> withSystemTempDirectory "olifant" (act . diskWorkspace)) contract
  describe "disk adapter (bytes that are not UTF-8)" $
    it "reports them as unreadable, never as absent" $
      withSystemTempDirectory "olifant" $ \dir -> do
        BS.writeFile (dir </> "latin1.txt") "caf\xe9 precious\n"
        inspectPath (diskWorkspace dir) "latin1.txt" `shouldReturn` Unreadable
  describe "disk adapter (listing a project)" $ do
    it "never descends into dependency and build directories" $
      withSystemTempDirectory "olifant" $ \dir -> do
        let ws = diskWorkspace dir
        mapM_ (\p -> writeText ws p "x") ["src/a.hs", "node_modules/pkg/index.js", "target/debug/x", "elm-stuff/0.19/y", "dist-newstyle/z", ".git/HEAD"]
        listFiles ws "." `shouldReturn` ["src/a.hs"]
    it "lists what git considers part of the project, honouring .gitignore" $
      findExecutable "git" >>= \case
        Nothing -> pendingWith "git is not installed"
        Just _ -> withSystemTempDirectory "olifant" $ \dir -> do
          let ws = diskWorkspace dir
          _ <- readProcess "git" ["-C", dir, "init", "-q"] ""
          mapM_ (\(p, t) -> writeText ws p t) [(".gitignore", "build/\n*.log\n"), ("build/out.js", "x"), ("debug.log", "x"), ("src/b.hs", "x"), ("src/new.hs", "untracked but not ignored")]
          listFiles ws "." `shouldReturn` [".gitignore", "src/b.hs", "src/new.hs"]
          listFiles ws "src" `shouldReturn` ["b.hs", "new.hs"]
  describe "paths" $ do
    it "normalises . and .." $
      map normalisePath ["a/./b", "a/../b", "a/b/..", "/x/../y", "", "../a"] `shouldBe` ["a/b", "b", "a", "/y", ".", "../a"]
    it "treats only relative paths inside the root as safe" $
      map isSafeRelative ["app/x.rb", "a/../b", "../x", "a/../../x", "/etc/passwd", ""] `shouldBe` [True, True, False, False, False, False]
