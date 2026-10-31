module Olifant.WorkspaceSpec (spec) where

import Control.Applicative ((<|>))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as M
import Hedgehog (evalIO, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec.Hedgehog (hedgehog)
import Olifant.Workspace
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (emptyPermissions, findExecutable, setOwnerExecutable, setOwnerReadable, setOwnerWritable, setPermissions)
import Control.Exception (SomeException, try)
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
  describe "overlayMany (the server's open documents over disk)" $ do
    it "reads open documents first and disk for the rest" $ do
      (ws, _) <- memoryWorkspace [("src/a.py", "disk a"), ("src/b.py", "disk b")]
      let o = overlayMany (M.fromList [("src/a.py", "unsaved a")]) ws
      readText o "src/a.py" `shouldReturn` Just "unsaved a"
      readText o "./src/a.py" `shouldReturn` Just "unsaved a"
      readText o "src/b.py" `shouldReturn` Just "disk b"
      inspectPath o "src/a.py" `shouldReturn` Readable "unsaved a"
    it "lists an open document that is not on disk yet, once" $ do
      (ws, _) <- memoryWorkspace [("src/a.py", "disk a")]
      let o = overlayMany (M.fromList [("src/new.py", "fresh"), ("src/a.py", "unsaved a")]) ws
      listFiles o "." >>= (`shouldMatchList` ["src/a.py", "src/new.py"])
      listFiles o "src" >>= (`shouldMatchList` ["a.py", "new.py"])
      listFiles o "other" `shouldReturn` []
      fileExists o "src/new.py" `shouldReturn` True
    it "reads disk again once a document is closed (Review Focus 5)" $ do
      (ws, _) <- memoryWorkspace [("src/a.py", "disk a")]
      let docs = M.fromList [("src/a.py", "unsaved a")]
      writeText ws "src/a.py" "changed on disk"
      readText (overlayMany (M.delete "src/a.py" docs) ws) "src/a.py" `shouldReturn` Just "changed on disk"
    it "reads what is open, else what is on disk (any documents, any disk)" $ hedgehog $ do
      let path = Gen.element ["a.py", "src/b.py", "src/c/d.py", "e.txt"]
          text = Gen.text (Range.linear 0 8) Gen.alpha
      disk <- forAll (Gen.map (Range.linear 0 4) ((,) <$> path <*> text))
      docs <- forAll (Gen.map (Range.linear 0 4) ((,) <$> path <*> text))
      p <- forAll path
      (ws, _) <- evalIO (memoryWorkspace (M.toList disk))
      r <- evalIO (readText (overlayMany docs ws) p)
      r === (M.lookup p docs <|> M.lookup p disk)
  describe "memory adapter" $ around (\act -> memoryWorkspace [] >>= act . fst) contract
  describe "disk adapter" $ around (\act -> withSystemTempDirectory "olifant" (act . diskWorkspace)) contract
  describe "disk adapter (bytes that are not UTF-8)" $
    it "reports them as unreadable, never as absent" $
      withSystemTempDirectory "olifant" $ \dir -> do
        BS.writeFile (dir </> "latin1.txt") "caf\xe9 precious\n"
        inspectPath (diskWorkspace dir) "latin1.txt" `shouldReturn` Unreadable
  describe "disk adapter (listing a project)" $ do
    it "never descends into .git or a dot-folder; a language's folders are its `ignore` (4b m2)" $
      withSystemTempDirectory "olifant" $ \dir -> do
        let ws = diskWorkspace dir
        mapM_ (\p -> writeText ws p "x") ["src/a.hs", "node_modules/pkg/index.js", "target/debug/x", ".git/HEAD", ".venv/lib/x.py", ".pytest_cache/v", ".gitignore"]
        listFiles ws "." `shouldReturn` [".gitignore", "node_modules/pkg/index.js", "src/a.hs", "target/debug/x"]
        listFiles (ignoring ["target", "node_modules"] ws) "." `shouldReturn` [".gitignore", "src/a.hs"]
    it "never walks into an ignored folder (an unreadable one inside it does not fail the listing; review I4)" $
      withSystemTempDirectory "olifant" $ \dir -> do
        let ws = diskWorkspace dir
        mapM_ (\p -> writeText ws p "x") ["src/a.rs", "target/debug/locked/x"]
        let locked = dir </> "target/debug/locked"
        setPermissions locked emptyPermissions
        r <- try (listFiles (ignoring ["target"] ws) ".") :: IO (Either SomeException [FilePath])
        setPermissions locked (setOwnerExecutable True (setOwnerReadable True (setOwnerWritable True emptyPermissions)))
        either (Left . show) Right r `shouldBe` Right ["src/a.rs"]
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
