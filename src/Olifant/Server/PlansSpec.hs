module Olifant.Server.PlansSpec (spec) where

import Olifant.Language.Grammar (GrammarFailure (NoSource))
import Control.Lens ((^.))
import Control.Monad (forM)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
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
import Olifant.Plans (Anchor (..), parsePlan)
import Olifant.Process (gitStateStamp)
import Olifant.Server (ServerState, newServerState, projectFor)
import Olifant.Server.Plans
import Olifant.Server.Claims (CheckRecord (..), SentenceEntry (..))
import Olifant.Server.Triage (Route (..), Triage (..))
import Olifant.Server.Errand (Belief (..), BeliefCheck (..))
import Olifant.Server.Checker (Citation (..), Confirmed (..), Judgement (..), UnitRef (..))
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
    (ds, _) <- planDiagnostics s proj "/r1/plans/t.md" "- a @src/a.py:Foo.bar ✓ 0000000\n- b @src/a.py:Gone\n- c @src/a.py:Foo.bar\n"
    map (\d -> (d ^. L.message, d ^. L.range . L.start . L.line, d ^. L.severity)) ds
      `shouldBe` [("changed since this was verified", 0, Just DiagnosticSeverity_Information), ("? not found", 1, Just DiagnosticSeverity_Hint)]
  it "evidence under a question raises no changed-since" $ do
    (s, proj) <- planServer
    (ds, _) <- planDiagnostics s proj "/r1/plans/t.md" "- ? q\n  - grounded @src/a.py:Foo.bar ✓ 0000000\n  - ✓ fine @src/a.py:Foo.bar ✓ 0000000\n"
    ds `shouldBe` []
  it "a claim outside any question raises changed since this was verified" $ do
    (s, proj) <- planServer
    (ds, _) <- planDiagnostics s proj "/r1/plans/t.md" "- a @src/a.py:Foo.bar ✓ 0000000\n"
    map (\d -> (d ^. L.message, d ^. L.severity)) ds `shouldBe` [("changed since this was verified", Just DiagnosticSeverity_Information)]
  it "a step's changed anchor raises nothing" $ do
    (s, proj) <- planServer
    (ds, _) <- planDiagnostics s proj "/r1/plans/t.md" "- [ ] a @src/a.py:Foo.bar ✓ 0000000\n- [x] b @src/a.py:Foo.bar ✓ 0000000\n"
    ds `shouldBe` []
  it "inside a ticked step only not found is raised" $ do
    (s, proj) <- planServer
    (ds, _) <- planDiagnostics s proj "/r1/plans/t.md" "- [x] a\n  - claim @src/a.py:Foo.bar ✓ 0000000\n  - claim @src/a.py:Gone\n"
    map (\d -> (d ^. L.message, d ^. L.range . L.start . L.line)) ds `shouldBe` [("? not found", 2)]
  it "returns the fingerprints it read" $ do
    (s, proj) <- planServer
    (_, prints) <- planDiagnostics s proj "/r1/plans/t.md" "- a @src/a.py:Foo.bar\n- b @src/a.py:Gone\n"
    fmap (fmap T.length) (M.lookup ("src/a.py", "Foo.bar") prints) `shouldBe` Just (Just 7)
    M.lookup ("src/a.py", "Gone") prints `shouldBe` Just Nothing
  it "a diagnostic's range is the anchor's UTF-16 columns" $ do
    (s, proj) <- planServer
    (ds, _) <- planDiagnostics s proj "/r1/plans/t.md" "- 𝔸 é @src/a.py:Gone\n"
    map (\d -> (d ^. L.range . L.start . L.character, d ^. L.range . L.end . L.character)) ds `shouldBe` [(7, 21)]
  it "an anchor outside the root, or absolute, is never read: not found" $ do
    (s, proj) <- planServer
    out <- resolveAnchor s proj (Anchor "../a.py" "Foo" 0 0)
    (rsUnit out, rsText out) `shouldBe` (Nothing, Nothing)
    abs' <- resolveAnchor s proj (Anchor "/r1/src/a.py" "Foo.bar" 0 0)
    (rsUnit abs', rsText abs') `shouldBe` (Nothing, Nothing)
    fmap namedName . rsUnit <$> resolveAnchor s proj (Anchor "src/../src/a.py" "Foo.bar" 0 0) `shouldReturn` Just "Foo.bar"
    (ds, _) <- planDiagnostics s proj "/r1/plans/t.md" "- a @../a.py:Foo\n- b @/r1/src/a.py:Foo.bar\n- c @src/../src/a.py:Foo.bar\n"
    map (\d -> (d ^. L.message, d ^. L.range . L.start . L.line)) ds `shouldBe` [("? not found", 0), ("? not found", 1)]
    map (^. L.label) <$> anchorItems s proj "- x @../a.py:" `shouldReturn` []
    map (^. L.label) <$> anchorItems s proj "- x @/r1/src/a.py:" `shouldReturn` []
  it "completes paths after @ and units after @path:" $ do
    (s, proj) <- planServer
    map (^. L.label) <$> anchorItems s proj "- x @src/" `shouldReturn` ["src/a.py"]
    map (^. L.label) <$> anchorItems s proj "- x @src/a.py:" `shouldReturn` ["Foo", "Foo.bar"]
    map (^. L.label) <$> anchorItems s proj "- x @src/a.py:Foo.b" `shouldReturn` ["Foo.bar"]
    map (^. L.label) <$> anchorItems s proj "- x no anchor" `shouldReturn` []
  it "completes fuzzily after @: units first, then files, with filterText" $ do
    (s, proj) <- planServer
    is <- anchorItems s proj "- x @table"
    map (^. L.label) is `shouldBe` ["app/models/table.py:Table", "app/models/table.py:Table.x", "app/models/table.py", "docs/table.txt"]
    map (^. L.insertText) (take 1 is) `shouldBe` [Just "app/models/table.py:Table"]
    map (^. L.filterText) (take 1 is) `shouldBe` [Just "table.py Table app/models/table.py"]
    map (^. L.insertText) (drop 2 is) `shouldBe` [Just "app/models/table.py:", Just "docs/table.txt"]
    map (fmap (^. L.command) . (^. L.command)) (drop 2 is) `shouldBe` [Just "editor.action.triggerSuggest", Nothing]
    map (^. L.label) <$> anchorItems s proj "- x @tbl" `shouldReturn` ["app/models/table.py:Table", "app/models/table.py:Table.x", "app/models/table.py", "docs/table.txt"]
    map (^. L.label) <$> anchorItems s proj "- x @zzz" `shouldReturn` []
  it "bounds the list" $ do
    (s, proj) <- planServer
    is <- anchorItems s proj "- x @"
    length is `shouldSatisfy` (<= 50)
    "src/a.py" `elem` map (^. L.label) is `shouldBe` True
  it "offers a new step as a snippet and as plain text" $ do
    newStepSnippet `shouldBe` "- [ ] ${1:step} @${2:anchor}"
    newStepText `shouldBe` "- [ ] step @anchor"
  it "lists the plan's anchored lines by section" $ do
    (s, proj) <- planServer
    fs <- planFiles s proj "## A\n- a @src/a.py:Foo.bar\n## B\n- [ ] b @src/a.py:Gone\n"
    map (\f -> (fiSection f, fiRow f, fiLine f)) fs `shouldBe` [("A", 1, Just 2), ("B", 3, Nothing)]
    map fiAbs fs `shouldBe` [Just "/r1/src/a.py", Just "/r1/src/a.py"]

  describe "planTokens" $ do
    let fixture = T.unlines
          [ "## Plan"                                              -- 0
          , "- [ ] open step @src/a.py:Foo"                          -- 1
          , "  - test: it works @src/a.py:Foo.bar ○"                -- 2
          , "  - also @src/a.py:Foo.bar"                             -- 3
          , "- [x] ticked @src/a.py:Foo ✓ 0123456"                  -- 4
          , "  - 25 Oct 21:31 · do (sonnet) · answered"            -- 5
          , "  - ? nested question"                                 -- 6
          , "    - the answer"                                      -- 7
          , "- ? a question"                                        -- 8
          , "  - grounded @src/a.py:Foo.bar ✓ 0123456"              -- 9
          , "  - plain words"                                       -- 10
          , "  - ✓ fine @src/a.py:Gone ? not found"                 -- 11
          , "prose here"                                            -- 12
          , "```"                                                   -- 13
          , "- [ ] in a fence"                                      -- 14
          , "```" ]                                                 -- 15
        ls = parsePlan fixture
        cst = [(ptCol t, ptLen t, ptType t) | t <- planTokens M.empty (parsePlan "- [x] DB migration @src/a.py:Comparison2018 ✓ 01849a0")]
    it "classifies every line kind" $
      map (\t -> (ptRow t, ptType t)) (planTokens M.empty ls)
        `shouldBe` [ (1, "planMarker"), (1, "planStep"), (1, "planAnchorPath"), (1, "planAnchorUnit")
                   , (2, "planMarker"), (2, "planTest"), (2, "planAnchorPath"), (2, "planAnchorUnit"), (2, "planVerdict")
                   , (3, "planMarker"), (3, "planAnchorPath"), (3, "planAnchorUnit")
                   , (4, "planMarker"), (4, "planStepDone"), (4, "planAnchorPath"), (4, "planAnchorUnit"), (4, "planVerdict"), (4, "planFingerprint")
                   , (5, "planMarker"), (5, "planJournal")
                   , (6, "planMarker"), (6, "planQuestion")
                   , (7, "planMarker"), (7, "planClaim")
                   , (8, "planMarker"), (8, "planQuestion")
                   , (9, "planMarker"), (9, "planEvidence"), (9, "planAnchorPath"), (9, "planAnchorUnit"), (9, "planVerdict"), (9, "planFingerprint")
                   , (10, "planMarker"), (10, "planClaim")
                   , (11, "planMarker"), (11, "planVerdict"), (11, "planEvidence"), (11, "planAnchorPath"), (11, "planAnchorUnit"), (11, "planVerdict") ]
    it "marker, path, unit, verdict and fingerprint columns" $
      cst `shouldBe` [(0, 6, "planMarker"), (6, 12, "planStepDone"), (19, 10, "planAnchorPath"), (29, 14, "planAnchorUnit"), (44, 1, "planVerdict"), (46, 7, "planFingerprint")]
    it "columns are UTF-16 with a wide character before the anchor" $
      [(ptCol t, ptLen t, ptType t) | t <- planTokens M.empty (parsePlan "- 𝔸 · x @src/a.py:F ✓ abcdef0")]
        `shouldBe` [(0, 2, "planMarker"), (2, 6, "planClaim"), (9, 10, "planAnchorPath"), (19, 1, "planAnchorUnit"), (21, 1, "planVerdict"), (23, 7, "planFingerprint")]
    it "done on the subtree of a ticked step, not on the step" $
      [(ptRow t, ptMods t) | t <- planTokens M.empty ls, ptRow t `elem` [4, 5, 6, 7, 8], ptType t `elem` ["planStepDone", "planJournal", "planQuestion", "planClaim"]]
        `shouldBe` [(4, []), (5, ["done"]), (6, ["done"]), (7, ["done"]), (8, [])]
    it "stale only when the map's fingerprint differs; notFound when the map says Nothing; neither when absent" $ do
      let mods prints = [(ptRow t, ptType t, ptMods t) | t <- planTokens prints (parsePlan "- [x] a @src/a.py:Foo ✓ 0123456\n"), ptType t `elem` ["planVerdict", "planFingerprint", "planAnchorUnit"]]
          key = ("src/a.py", "Foo")
      mods M.empty `shouldBe` [(0, "planAnchorUnit", []), (0, "planVerdict", []), (0, "planFingerprint", [])]
      mods (M.fromList [(key, Just "0123456")]) `shouldBe` [(0, "planAnchorUnit", []), (0, "planVerdict", []), (0, "planFingerprint", [])]
      mods (M.fromList [(key, Just "9999999")]) `shouldBe` [(0, "planAnchorUnit", []), (0, "planVerdict", ["stale"]), (0, "planFingerprint", ["stale"])]
      mods (M.fromList [(key, Nothing)]) `shouldBe` [(0, "planAnchorUnit", ["notFound"]), (0, "planVerdict", []), (0, "planFingerprint", [])]
    it "tokens never overlap and are ascending" $ do
      let ts = planTokens M.empty ls
          ends = [(ptRow t, ptCol t, ptCol t + ptLen t) | t <- ts]
      and [(r, c) < (r', c') && (r < r' || e <= c') | ((r, c, e), (r', c', _)) <- zip ends (drop 1 ends)] `shouldBe` True

  describe "planTokensWith (claim modifiers)" $
    it "a bare line's text carries checked or wrong from its check" $ do
      let e j = verdictEntry j "no"
          store = M.fromList [("right one", e JRight), ("wrong one", e JWrong), ("open one", queuedEntry)]
          mods = [(ptRow t, ptMods t) | t <- planTokensWith store M.empty (parsePlan "- right one\n- wrong one\n- open one\n- other\n"), ptType t == "planClaim"]
      mods `shouldBe` [(0, ["checked"]), (1, ["wrong"]), (2, []), (3, [])]
  describe "beliefsUnder" $
    it "an ask's beliefs are its question's bare claims, once one of them has a verdict" $ do
      let ls = parsePlan "- ? how\n  - it reads D\n  - grounded @src/a.py:Foo ✓ 0123456\n  - it is fast\n- other\n"
          q = ls !! 0
          checked = M.fromList [("it reads D", verdictEntry JWrong "it drops D")]
      beliefsUnder checked ls q `shouldBe` [Belief "it reads D" (BelievedWrong "it drops D" "src/a.py:Foo"), Belief "it is fast" NotChecked]
      beliefsUnder M.empty ls q `shouldBe` []
      beliefsUnder checked ls (ls !! 4) `shouldBe` []
  describe "planFolds" $ do
    let folds = planFolds S.empty . parsePlan . T.unlines
    it "a ticked step with children folds to its last descendant" $
      folds ["- [x] a", "  - 25 Oct 21:31 · did", "  - also @src/a.py:Foo", "- [ ] b"] `shouldBe` [(0, 2)]
    it "a ticked step without children, and an open step, do not fold" $
      folds ["- [x] a", "- [ ] b", "  - 25 Oct 21:31 · did"] `shouldBe` []
    it "an answered question folds: a verified claim or evidence under it; a bare question does not" $ do
      folds ["- ? q", "  - ev @src/a.py:Foo ✓ 0123456", "  - the answer", "- ? r", "  - ev @src/a.py:Foo ✓ 0123456"] `shouldBe` [(0, 2), (3, 4)]
      folds ["- ? q", "  - ✓ it is @src/a.py:Foo ✓ 0123456"] `shouldBe` [(0, 1)]
      folds ["- ? q"] `shouldBe` []
    it "a bare belief keeps its question open until it is checked" $ do
      folds ["- ? q", "  - I think x"] `shouldBe` []
      planFolds (S.fromList ["I think x"]) (parsePlan (T.unlines ["- ? q", "  - I think x"])) `shouldBe` [(0, 1)]
    it "an ask's answer folds whatever its anchors' verdicts: anchored claims plus a bare conclusion" $ do
      folds ["- ? q", "  - grounded @src/a.py:Foo", "  - so it keeps D"] `shouldBe` [(0, 2)]
      folds ["- ? q", "  - grounded @src/a.py:Foo"] `shouldBe` []
      folds ["- ? q", "  - I think x", "  - so it keeps D"] `shouldBe` []
    it "a question under a ticked step folds inside it" $
      planFolds (S.fromList ["the answer"]) (parsePlan (T.unlines ["- [x] a", "  - ? q", "    - the answer", "  - 25 Oct 21:31 · did"])) `shouldBe` [(0, 3), (1, 2)]

at0 :: UTCTime
at0 = UTCTime (fromGregorian 2026 11 7) 0

-- | A verify sentence with no check yet.
queuedEntry :: SentenceEntry
queuedEntry = SentenceEntry (Triage "claim" 0.9 0.9 "jev") Verify Nothing Nothing at0

-- | A verify sentence checked J, with WHY, citing src/a.py:Foo.
verdictEntry :: Judgement -> Text -> SentenceEntry
verdictEntry j why = queuedEntry {seCheck = Just (CheckRecord (Confirmed j why [Citation "src/a.py" (Just "Foo") Nothing] (Just (UnitRef "src/a.py" "Foo")) (Just "abc1234")) "claude-sonnet-5-5" (Just 0.1) 12 at0)}

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
           , ("/r1/app/models/table.py", "class Table:\n    def x(self):\n        return 1\n")
           , ("/r1/docs/table.txt", "t\n")
           , -- outside the root: there, so a refusal is not a miss
             ("/a.py", "class Foo:\n    def bar(self):\n        return 1\n")
           , ("/r1/plans/t.md", "# T\n") ]
  let syntax = querying (\_ _ _ -> pure (Right [Match (a, 0) (z, 0) "" (0, 0) | (a, z) <- [(0, 2), (1, 2)]]))
      ports = Ports disk syntax (const noChecker) (const noVerifier) (const noRunner) (\_ _ -> noRunner) (pure (UTCTime (fromGregorian 2026 10 25) 0)) Nothing Nothing (DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False) (\_ -> pure Nothing) (\_ -> pure []) (\_ -> pure (Left NoSource))) (\_ -> pure Nothing) (\_ _ _ -> pure (Left "no base in tests")) (\_ _ -> pure (Left "no base in tests")) gitStateStamp
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
