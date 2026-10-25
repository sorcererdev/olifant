module Olifant.Server.PlansSpec (spec) where

import Control.Lens ((^.))
import Control.Monad (forM)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime (..), fromGregorian)
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Types (DiagnosticSeverity (..))
import Olifant.App (Ports (..), Project)
import Olifant.Doctor (DoctorPorts (..))
import Olifant.Language (Match (..), noChecker, noRunner, querying)
import Olifant.Language.Check (noVerifier)
import Olifant.Plans (Anchor (..))
import Olifant.Process (gitStateStamp)
import Olifant.Server (ServerState, newServerState, projectFor)
import Olifant.Server.Plans
import Olifant.Server.Unit (Named (..))
import Olifant.Workspace (memoryWorkspace)
import System.Directory (canonicalizePath, doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, (</>))
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Server.Plans (anchors in the server)" $ do
  it "a plan is a markdown file under the plans dir" $ do
    (_, proj) <- planServer
    isPlanPath proj "/r1/plans/t.md" `shouldBe` True
    isPlanPath proj "/r1/plans/notes.txt" `shouldBe` False
    isPlanPath proj "/r1/src/a.md" `shouldBe` False
  it "resolves Foo.bar, misses Gone, fingerprints the unit" $ do
    (s, proj) <- planServer
    r <- resolveAnchor s proj (Anchor "src/a.py" "Foo.bar" 0 0)
    fmap namedName (rsUnit r) `shouldBe` Just "Foo.bar"
    fmap T.length (rsFingerprint r) `shouldBe` Just 7
    rsUnit <$> resolveAnchor s proj (Anchor "src/a.py" "Gone" 0 0) `shouldReturn` Nothing
    rsUnit <$> resolveAnchor s proj (Anchor "src/nope.py" "X" 0 0) `shouldReturn` Nothing
    rsUnit <$> resolveAnchor s proj (Anchor "notes.txt" "X" 0 0) `shouldReturn` Nothing
  it "diagnoses stale and missing anchors on their columns" $ do
    (s, proj) <- planServer
    ds <- planDiagnostics s proj "/r1/plans/t.md" "- a @src/a.py:Foo.bar ✓ 0000000\n- b @src/a.py:Gone\n- c @src/a.py:Foo.bar\n"
    map (\d -> (d ^. L.message, d ^. L.range . L.start . L.line, d ^. L.severity)) ds
      `shouldBe` [("✗ changed since 0000000", 0, Just DiagnosticSeverity_Information), ("? not found", 1, Just DiagnosticSeverity_Hint)]
  it "a diagnostic's range is the anchor's UTF-16 columns" $ do
    (s, proj) <- planServer
    ds <- planDiagnostics s proj "/r1/plans/t.md" "- 𝔸 é @src/a.py:Gone\n"
    map (\d -> (d ^. L.range . L.start . L.character, d ^. L.range . L.end . L.character)) ds `shouldBe` [(7, 21)]
  it "completes paths after @ and units after @path:" $ do
    (s, proj) <- planServer
    map (^. L.label) <$> anchorItems s proj "- x @src/" `shouldReturn` ["src/a.py"]
    map (^. L.label) <$> anchorItems s proj "- x @src/a.py:" `shouldReturn` ["Foo", "Foo.bar"]
    map (^. L.label) <$> anchorItems s proj "- x @src/a.py:Foo.b" `shouldReturn` ["Foo.bar"]
    map (^. L.label) <$> anchorItems s proj "- x no anchor" `shouldReturn` []
  it "lists the plan's anchored lines by section" $ do
    (s, proj) <- planServer
    fs <- planFiles s proj "## A\n- a @src/a.py:Foo.bar\n## B\n- [ ] b @src/a.py:Gone\n"
    map (\f -> (fiSection f, fiRow f, fiLine f)) fs `shouldBe` [("A", 1, Just 2), ("B", 3, Nothing)]
    map fiAbs fs `shouldBe` [Just "/r1/src/a.py", Just "/r1/src/a.py"]

-- | /r1 with the python pack, `src/a.py` (class Foo with def bar) and a
-- plan; a syntax whose units are Foo (lines 0-2) and Foo.bar (1-2).
planServer :: IO (ServerState, Project)
planServer = do
  pack <- readTree =<< canonicalizePath "packs/python-htmx"
  (disk, _) <-
    memoryWorkspace $
      [("/pack" </> p, t) | (p, t) <- M.toList pack]
        ++ [ ("/r1/.olifant/config.toml", "[packs]\npython-htmx = { path = \"/pack\" }\n")
           , ("/r1/src/a.py", "class Foo:\n    def bar(self):\n        return 1\n")
           , ("/r1/notes.txt", "x\n")
           , ("/r1/plans/t.md", "# T\n") ]
  let syntax = querying (\_ _ _ -> pure (Right [Match (a, 0) (z, 0) "" (0, 0) | (a, z) <- [(0, 2), (1, 2)]]))
      ports = Ports disk syntax (const noChecker) (const noVerifier) (const noRunner) (\_ _ -> noRunner) (pure (UTCTime (fromGregorian 2026 10 25) 0)) Nothing Nothing (DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False)) (\_ -> pure Nothing) (\_ _ _ -> pure (Left "no base in tests")) (\_ _ -> pure (Left "no base in tests")) gitStateStamp
  s0 <- newServerState ports
  projectFor "/r1/plans/t.md" s0 >>= \case
    (s, Just (_, proj)) -> pure (s, proj)
    _ -> fail "no project at /r1"

readTree :: FilePath -> IO (M.Map FilePath Text)
readTree dir = M.fromList <$> go dir
  where
    go d = do
      names <- listDirectory d
      fmap concat . forM names $ \n -> do
        let p = d </> n
        isDir <- doesDirectoryExist p
        if isDir then go p else (\t -> [(makeRelative dir p, t)]) <$> TIO.readFile p
