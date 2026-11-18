{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.ServerSpec (spec) where

import Olifant.Language.Grammar (GrammarFailure (NoSource))
import Control.Concurrent (threadDelay)
import Control.Monad (forM, void)
import Data.Aeson (Value (..), (.:))
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as BL
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Map.Strict qualified as M
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Olifant.Mutate (Mutant (..))
import Olifant.TestName (stubNameFor)
import Olifant.Language (Failure (..), Match (..), Rewrite (..), Runner (..), Syntax (..), TestRun (..))
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)
import System.Directory qualified as Dir
import Olifant.Server.ConjureStep qualified as CS
import Olifant.Server.Step (TestDelta (..), stepSystem)
import Olifant.Inspect.Store (InspectFinding (..), InspectStore (..), Place (..), Status (..), emptyInspectStore, encodeStore)
import Olifant.Inspect.StoreSpec (finding)
import Olifant.Server.Edits (FileEdit (..))
import Olifant.Model (ChatReply (..))
import Olifant.Generation.Steer (noSeed)
import System.FilePath (makeRelative, (</>))
import Control.Concurrent.Async (replicateConcurrently_, wait)
import Data.Set qualified as S
import Olifant.Server.State qualified as St
import Olifant.Server.Site (Site (..), importOf, importedContext, importedModules, siteAt)
import Olifant.Server.Stub (testNameIn, unitWordsOf)
import Olifant.Server.Unit (Named (..), unitAround)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Olifant.Decide qualified as De
import Olifant.Fill (FillHole (..), FillRequest (..), Filler (..), ModelFiller (..))
import System.IO.Temp (withSystemTempDirectory)
import Data.List (isSuffixOf)
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (catalogRulesConfig)
import Olifant.Doctor (DoctorPorts (..))
import Olifant.Language (ImportsDef (..), Language (..), noAnchors, noChecker, noRunner, noSyntax, parseLanguage, shippedLanguage, shippedLanguageNow)
import Olifant.Language.TestUnits (testCaseAtIn)
import Olifant.Language.Check (noVerifier)
import Olifant.Process (cancelled, gitStateStamp, newCancel)
import Olifant.Rules (RulesConfig (..), Severity (..))
import Olifant.Server
import Olifant.Workspace (Workspace (..), memoryWorkspace)
import Data.Text qualified as T
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = describe "Olifant.Server (projects across roots, open documents, jobs)" $ do
  describe "TestName (Olifant.TestName)" $ do
    let mutant op from text = Mutant 1 op from "\"\"" text False
    it "a literal's letter prefix is dropped before its quote, any letters" $ do
      stubNameFor (mutant "string" "rb\"hello world\"" "x = rb\"hello world\"") ("test_", "") "#" [] [] `shouldBe` "test_hello_world"
      stubNameFor (mutant "string" "br'hello world'" "x = br'hello world'") ("test_", "") "#" [] [] `shouldBe` "test_hello_world"
      stubNameFor (mutant "string" "\"hello world\"" "x = \"hello world\"") ("test_", "") "#" [] [] `shouldBe` "test_hello_world"
      -- an identifier then a string reads as a prefix too: the rule cannot tell them apart (ruling)
      stubNameFor (mutant "string" "x\"y\"" "z = x\"y\"") ("test_", "") "#" [] [] `shouldBe` "test_y"
    it "self is a keyword, not a hard-coded word" $ do
      stubNameFor (mutant "arith" "+" "total = self + 1") ("test_", "") "#" [] [] `shouldBe` "test_arith_total_self"
      stubNameFor (mutant "arith" "+" "total = self + 1") ("test_", "") "#" [] ["self"] `shouldBe` "test_arith_total"
  it "(a) routes each file to its nearest project; a nested project wins over its parent" $ do
    (s0, _) <- server
    (s1, a) <- projectFor "/r1/src/x.py" s0
    (s2, b) <- projectFor "/r2/a/src/y.py" s1
    (_, c) <- projectFor "/r2/src/z.py" s2
    (fmap fst a, fmap fst b, fmap fst c) `shouldBe` (Just "/r1", Just "/r2/a", Just "/r2")
  it "(b) shows a project the unsaved text of its own open documents, not another project's" $ do
    (s0, _) <- server
    let s1 = changeDoc "/r1/src/x.py" 2 "unsaved" (openDoc "/r1/src/x.py" 1 "x = 1\n" s0)
    readText (workspaceOf s1 "/r1") "src/x.py" `shouldReturn` Just "unsaved"
    readText (workspaceOf s1 "/r2") "../r1/src/x.py" `shouldReturn` Just "x = 1\n"
    (_, Just (_, p)) <- projectFor "/r1/src/x.py" s1
    readText (prWorkspace p) "src/x.py" `shouldReturn` Just "unsaved"
  it "(c) loads a project only when one of its files is asked about" $ do
    (s0, configReads) <- server
    let s1 = openDoc "/r1/src/x.py" 1 "x" s0
    configReads `shouldReturn` 0
    (s2, _) <- projectFor "/r1/src/x.py" s1
    loaded <- configReads
    loaded `shouldSatisfy` (> 0)
    _ <- projectFor "/r1/src/other.py" s2
    configReads `shouldReturn` loaded
  it "(d) keeps each project's own conventions: a rule's severity differs per project" $ do
    (s0, _) <- server
    (s1, Just (_, p1)) <- projectFor "/r1/src/x.py" s0
    (_, Just (_, p2)) <- projectFor "/r2/src/z.py" s1
    lookup "commands-use-ports" (rcSeverity (catalogRulesConfig (prCatalog p1))) `shouldBe` Just Off
    lookup "commands-use-ports" (rcSeverity (catalogRulesConfig (prCatalog p2))) `shouldBe` Just Error
  it "(e) gives a file in no project nothing (Review Focus 2)" $ do
    (s0, _) <- server
    (_, none) <- projectFor "/elsewhere/README.md" s0
    fmap fst none `shouldBe` Nothing
  it "(f) keeps the last good project when its config breaks, and recovers (Review Focus 3)" $ do
    (s0, _) <- serverWith (\ws -> pure ws)
    (s1, Just _) <- projectFor "/r1/src/x.py" s0
    writeText (portsFiles s1) "/r1/.olifant/config.toml" "[rules\nbroken"
    s2 <- catalogChanged "/r1" s1
    case slotOf "/r1" s2 of
      Just (Broken why (Just _)) -> why `shouldSatisfy` (/= "")
      other -> expectationFailure ("expected Broken with the last good project, got " <> describeSlot other)
    (_, stillThere) <- projectFor "/r1/src/x.py" s2
    fmap fst stillThere `shouldBe` Just "/r1"
    writeText (portsFiles s2) "/r1/.olifant/config.toml" "[rules]\ncommands-use-ports = \"warn\"\n"
    s3 <- catalogChanged "/r1" s2
    case slotOf "/r1" s3 of
      Just (Loaded p) -> lookup "commands-use-ports" (rcSeverity (catalogRulesConfig (prCatalog p))) `shouldBe` Just Warn
      other -> expectationFailure ("expected Loaded, got " <> describeSlot other)
  it "(g) reads disk again once a document is closed (Review Focus 5)" $ do
    (s0, _) <- server
    let s1 = closeDoc "/r1/src/x.py" (openDoc "/r1/src/x.py" 1 "unsaved" s0)
    writeText (portsFiles s1) "/r1/src/x.py" "changed on disk"
    readText (workspaceOf s1 "/r1") "src/x.py" `shouldReturn` Just "changed on disk"
  it "(h) a newer job with the same key cancels the older; other keys run on" $ do
    jobs <- newJobs
    ran <- newIORef ([] :: [Text])
    let slow name token = do
          let loop n = cancelled token >>= \stop -> if stop || n > (100 :: Int) then pure stop else threadDelay 10000 >> loop (n + 1)
          stop <- loop 0
          record (name <> if stop then " cancelled" else " finished")
        -- jobs run on other threads at once: append atomically
        record x = atomicModifyIORef' ran (\xs -> (xs ++ [x], ()))
    first' <- submit jobs "check /r1/x.py" 0 (slow "first")
    threadDelay 20000
    second <- submit jobs "check /r1/x.py" 0 (\_ -> record "second")
    other <- submit jobs "check /r2/y.py" 0 (\_ -> record "other")
    mapM_ wait [first', second, other]
    readIORef ran >>= (`shouldMatchList` ["first cancelled", "second", "other"])
  it "turns a plan's file into the smallest change of whole lines, which gives the new text (any texts)" $ hedgehog $ do
    let line = Gen.element ["", "a", "b", "def f():", "    x = 1", "é ✓"]
        text = T.intercalate "\n" <$> Gen.list (Range.linear 0 8) line
    old <- forAll text
    new <- forAll text
    let (range, replacement) = smallestChange old new
    applyChange range replacement old === new
  it "keeps the lines a change shares at both ends" $
    smallestChange "a\nb\nc\n" "a\nB\nc\n" `shouldBe` (((1, 0), (2, 0)), "B\n")
  it "(h) a debounced job superseded before it starts never runs" $ do
    jobs <- newJobs
    ran <- newIORef ([] :: [Text])
    a <- submit jobs "suggest /r1/x.py" 50 (\_ -> modifyIORef' ran (++ ["a"]))
    b <- submit jobs "suggest /r1/x.py" 50 (\_ -> modifyIORef' ran (++ ["b"]))
    (,) <$> wait a <*> wait b `shouldReturn` (Nothing, Just ())
    readIORef ran `shouldReturn` ["b"]
  orderSpecs
  siteSpecs
  testsSpecs
  stubSpecs
  unitSpecs
  stepSpecs
  mutateSpecs

-- | `olifant/conjure` as one step (design 2026-11-16): the writer answers
-- whole files once; the step is shown, then accepted, rejected or taken again
-- by the harder writer.
stepSpecs :: Spec
stepSpecs = describe "a conjure is one step (olifant/conjure)" $ do
  let writer asked replies = Just $ \name system user -> do
        modifyIORef' asked (++ [(name, system, user)])
        pure (replies name)
      reply t = Right (ChatReply t (Just 0.001) 1.5)
      moduleWith body = T.replace "raise NotImplementedError(\"later\")" body stubbed
      fileBlock rel t = "<file path=\"" <> T.pack rel <> "\">\n" <> t <> "</file>"
      filled = "<goal>fill handle so it closes a table</goal>\n" <> fileBlock moduleRel written
      serving replies extra = do
        asked <- newIORef []
        (s0, _) <- blocksOnSyntax unitSyntax (Right . redOnStub . M.findWithDefault "" moduleRel) extra
        let s = (openDoc (root </> moduleRel) 1 stubbed s0) {St.ssTestWriter = writer asked replies}
        pure (s, asked)
      step s line = snd <$> CS.conjureAt (const (pure ())) (root </> moduleRel) line Nothing s
      answer s token choice = snd <$> CS.answerStep (const (pure ())) (root </> moduleRel) token choice s
      shown = \case
        Right (CS.StepShown sh) -> sh
        other -> error ("not shown: " <> show other)
      records s = maybe [] T.lines <$> readText (pFiles (ssPorts s)) (root </> ".olifant/state/conjure-steps.jsonl")
      stepsLog s = maybe [] T.lines <$> readText (pFiles (ssPorts s)) (root </> ".olifant/state/steps.log")
      held s = M.size . snd <$> readIORef (St.ssShown s)
  it "on a red test: the writer is asked once with the situation; the step is shown with its goal, one edit and the test now passing" $ do
    (s, asked) <- serving (const (reply filled)) []
    sh <- shown <$> step s 6
    calls <- readIORef asked
    length calls `shouldBe` 1
    let [(_, system, user)] = calls
    system `shouldBe` stepSystem
    mapM_ (\k -> user `shouldSatisfy` T.isInfixOf k) ["`handle`", "def test_closes_a_table", "Tests before the step:"]
    CS.shGoal sh `shouldBe` Just "fill handle so it closes a table"
    map (\e -> (fePath e, feVersion e)) (CS.shEdits sh) `shouldBe` [(root </> moduleRel, Just 1)]
    tdPassing (CS.shDelta sh) `shouldBe` [testId]
    held s `shouldReturn` 1
  it "the situation holds inspect's open findings near the unit and the project's rules; a dismissed finding is left out" $ do
    let placed i st statement = (finding i "bug" st 0.9) {vfStatement = statement, vfPlace = Place moduleRel (Just "handle") (5, 6) Nothing (Just 5), vfEvidence = [(moduleRel, (6, 6))]}
        store = emptyInspectStore {vsFindings = [placed "a" Open "handle never closes a table.", placed "b" DismissedHere "handle is too terse."]}
        rules = "# Inspect\n\n## Rules\n\n### Seats are counted\nA table closes when its seats are counted.\n\n## A nitpick is\nstyle.\n"
    (s, asked) <- serving (const (reply filled)) [(".olifant/state/inspect/findings.json", encodeStore store), (".olifant/inspect.md", rules)]
    _ <- shown <$> step s 6
    [(_, _, user)] <- readIORef asked
    mapM_ (\k -> user `shouldSatisfy` T.isInfixOf k) ["Inspect's open findings here", "handle never closes a table.", "### Seats are counted"]
    user `shouldNotSatisfy` T.isInfixOf "handle is too terse."
    user `shouldNotSatisfy` T.isInfixOf "A nitpick is"
  it "no findings file and no inspect.md: neither block" $ do
    (s, asked) <- serving (const (reply filled)) []
    _ <- shown <$> step s 6
    [(_, _, user)] <- readIORef asked
    user `shouldNotSatisfy` T.isInfixOf "findings"
    user `shouldNotSatisfy` T.isInfixOf "The project's rules"
  it "a module with no test file is no refusal: the writer is told where its tests go and may create the file" $ do
    let other = "src/shop/open_table.py"
        otherTest = "src/shop/open_table__test.py"
    (s, asked) <- serving (const (reply ("<goal>write its first test</goal>\n" <> fileBlock otherTest "def test_opens() -> None:\n    assert True\n"))) [(other, "def open_table() -> int:\n    return 1\n")]
    sh <- shown <$> snd <$> CS.conjureAt (const (pure ())) (root </> other) 2 Nothing s
    [(_, _, user)] <- readIORef asked
    user `shouldSatisfy` T.isInfixOf ("There is no test file for this module yet (its tests would go in " <> T.pack otherTest <> ")")
    map (\e -> (fePath e, feCreate e)) (CS.shEdits sh) `shouldBe` [(root </> otherTest, True)]
  it "a step that leaves the test red is still shown, with the test still red" $ do
    (s, _) <- serving (const (reply ("<goal>name the seats</goal>\n" <> fileBlock moduleRel (moduleWith "seats = seats\n        raise NotImplementedError(\"later\")")))) []
    sh <- shown <$> step s 6
    tdStillRed (CS.shDelta sh) `shouldBe` [testId]
  it "a path outside the project is dropped and said, never an edit" $ do
    (s, _) <- serving (const (reply (filled <> "\n" <> fileBlock "../evil.py" "x = 1\n"))) []
    sh <- shown <$> step s 6
    CS.shDropped sh `shouldBe` ["../evil.py"]
    map fePath (CS.shEdits sh) `shouldBe` [root </> moduleRel]
  it "nothing to do is a step that takes none: shown with no edits, held under a token, its reply kept" $ do
    (s, _) <- serving (const (reply "<goal>nothing</goal>")) []
    sh <- shown <$> step s 6
    CS.shTook sh `shouldBe` False
    CS.shEdits sh `shouldBe` []
    CS.shGoal sh `shouldBe` Just "nothing"
    CS.shReply sh `shouldBe` "<goal>nothing</goal>"
    CS.shSituation sh `shouldSatisfy` T.isInfixOf "`handle`"
    held s `shouldReturn` 1
  it "a goal with no files is a step that takes none, held like any step" $ do
    (s, _) <- serving (const (reply "<goal>fill handle</goal>\nI would change handle.")) []
    sh <- shown <$> step s 6
    CS.shTook sh `shouldBe` False
    CS.shEdits sh `shouldBe` []
    held s `shouldReturn` 1
  it "a step with files took one" $ do
    (s, _) <- serving (const (reply filled)) []
    sh <- shown <$> step s 6
    CS.shTook sh `shouldBe` True
  it "the editor's unsaved text is what the step is diffed against, at its version" $ do
    (s0, _) <- serving (const (reply filled)) []
    let unsaved = T.replace "self.closed = 0" "self.closed = 0  # mine" stubbed
        s = openDoc (root </> moduleRel) 3 unsaved s0
    sh <- shown <$> step s 6
    map feVersion (CS.shEdits sh) `shouldBe` [Just 3]
  it "the writer failing is one sentence, and nothing is kept" $ do
    (s, _) <- serving (const (Left "claude is not installed")) []
    step s 6 `shouldReturn` Left "claude is not installed"
    held s `shouldReturn` 0
  it "no model and no writer: the no-model sentence" $ do
    (s0, _) <- serving (const (reply filled)) []
    r <- step s0 {St.ssTestWriter = Nothing} 6
    r `shouldSatisfy` either ("no model" `T.isInfixOf`) (const False)
  describe "the person's answer" $ do
    it "accept lands the step's edits, records it and says it in the step log" $ do
      (s, _) <- serving (const (reply filled)) []
      sh <- shown <$> step s 6
      r <- answer s (CS.shToken sh) CS.StepAccept
      case r of
        Right (CS.StepLanded l) -> CS.lsEdits l `shouldBe` CS.shEdits sh
        other -> expectationFailure (show other)
      recs <- records s
      length recs `shouldBe` 1
      head recs `shouldSatisfy` T.isInfixOf "\"choice\":\"accepted\""
      logs <- stepsLog s
      logs `shouldSatisfy` any ("conjure step" `T.isInfixOf`)
      held s `shouldReturn` 0
    it "reject lands nothing and records it" $ do
      (s, _) <- serving (const (reply filled)) []
      sh <- shown <$> step s 6
      r <- answer s (CS.shToken sh) CS.StepReject
      r `shouldSatisfy` either (const False) (\case CS.StepRejected -> True; _ -> False)
      recs <- records s
      head recs `shouldSatisfy` T.isInfixOf "\"choice\":\"rejected\""
    it "harder asks the harder writer over the same situation; the first is recorded as replaced" $ do
      (s0, asked) <- serving (const (reply filled)) [(".olifant/config.toml", "[packs]\npython-htmx = { path = \"/pack\" }\n[model]\ndefault = \"haiku\"\n[model.backends.haiku]\nkind = \"claude\"\nmodel = \"claude-haiku-5-5\"\n[model.backends.sonnet]\nkind = \"claude\"\nmodel = \"claude-sonnet-5-5\"\n[conjure]\nharder = \"sonnet\"\n")]
      let s = s0
      sh <- shown <$> step s 6
      CS.shHarder sh `shouldBe` True
      sh2 <- shown <$> answer s (CS.shToken sh) CS.StepHarder
      calls <- readIORef asked
      map (\(n, _, _) -> n) calls `shouldBe` ["haiku", "sonnet"]
      let [(_, _, u1), (_, _, u2)] = calls
      u2 `shouldBe` u1
      CS.shToken sh2 `shouldNotBe` CS.shToken sh
      recs <- records s
      head recs `shouldSatisfy` T.isInfixOf "\"choice\":\"replaced\""
    it "the harder writer failing keeps the step shown: said, and it can still be accepted" $ do
      (s, _) <- serving (\n -> if n == "sonnet" then Left "timed out after 120 s" else reply filled) [(".olifant/config.toml", "[packs]\npython-htmx = { path = \"/pack\" }\n[model]\ndefault = \"haiku\"\n[model.backends.haiku]\nkind = \"claude\"\nmodel = \"claude-haiku-5-5\"\n[model.backends.sonnet]\nkind = \"claude\"\nmodel = \"claude-sonnet-5-5\"\n[conjure]\nharder = \"sonnet\"\n")]
      sh <- shown <$> step s 6
      answer s (CS.shToken sh) CS.StepHarder `shouldReturn` Left "timed out after 120 s"
      records s `shouldReturn` []
      r <- answer s (CS.shToken sh) CS.StepAccept
      r `shouldSatisfy` either (const False) (\case CS.StepLanded _ -> True; _ -> False)
    it "no harder writer configured: the step says so and harder is refused" $ do
      (s, _) <- serving (const (reply filled)) []
      sh <- shown <$> step s 6
      CS.shHarder sh `shouldBe` False
      answer s (CS.shToken sh) CS.StepHarder `shouldReturn` Left "no harder writer: name one with [conjure] harder"
    it "accept on a step that took none lands nothing and records it as no step" $ do
      (s, _) <- serving (const (reply "<goal>nothing</goal>")) []
      sh <- shown <$> step s 6
      r <- answer s (CS.shToken sh) CS.StepAccept
      r `shouldSatisfy` either (const False) (\case CS.StepRejected -> True; _ -> False)
      recs <- records s
      head recs `shouldSatisfy` T.isInfixOf "\"choice\":\"no step\""
      held s `shouldReturn` 0
    it "harder on a step that took none: the harder writer's step replaces it" $ do
      (s, _) <- serving (\n -> if n == "sonnet" then reply filled else reply "<goal>nothing</goal>") [(".olifant/config.toml", "[packs]\npython-htmx = { path = \"/pack\" }\n[model]\ndefault = \"haiku\"\n[model.backends.haiku]\nkind = \"claude\"\nmodel = \"claude-haiku-5-5\"\n[model.backends.sonnet]\nkind = \"claude\"\nmodel = \"claude-sonnet-5-5\"\n[conjure]\nharder = \"sonnet\"\n")]
      sh <- shown <$> step s 6
      CS.shTook sh `shouldBe` False
      sh2 <- shown <$> answer s (CS.shToken sh) CS.StepHarder
      CS.shTook sh2 `shouldBe` True
      recs <- records s
      head recs `shouldSatisfy` T.isInfixOf "\"choice\":\"replaced\""
      held s `shouldReturn` 1
    it "harder answering with no step replaces the shown one too" $ do
      (s, _) <- serving (\n -> if n == "sonnet" then reply "<goal>nothing</goal>" else reply filled) [(".olifant/config.toml", "[packs]\npython-htmx = { path = \"/pack\" }\n[model]\ndefault = \"haiku\"\n[model.backends.haiku]\nkind = \"claude\"\nmodel = \"claude-haiku-5-5\"\n[model.backends.sonnet]\nkind = \"claude\"\nmodel = \"claude-sonnet-5-5\"\n[conjure]\nharder = \"sonnet\"\n")]
      sh <- shown <$> step s 6
      sh2 <- shown <$> answer s (CS.shToken sh) CS.StepHarder
      CS.shTook sh2 `shouldBe` False
      CS.shToken sh2 `shouldNotBe` CS.shToken sh
      held s `shouldReturn` 1
    it "a token answers once" $ do
      (s, _) <- serving (const (reply filled)) []
      sh <- shown <$> step s 6
      _ <- answer s (CS.shToken sh) CS.StepReject
      answer s (CS.shToken sh) CS.StepAccept `shouldReturn` Left CS.stepGone
    it "a new conjure in the file drops the step shown there, recorded as dropped" $ do
      (s, _) <- serving (const (reply filled)) []
      sh <- shown <$> step s 6
      _ <- step s 6
      answer s (CS.shToken sh) CS.StepAccept `shouldReturn` Left CS.stepGone
      recs <- records s
      head recs `shouldSatisfy` T.isInfixOf "\"choice\":\"dropped\""
    it "the wire: a shown step's keys, and the resume choices accept, reject and harder only" $ do
      (s, _) <- serving (const (reply filled)) []
      sh <- shown <$> step s 6
      let Object o = CS.stepJson (const (J.String "edits")) (CS.StepShown sh)
          Just (Object w) = KM.lookup "shown" o
      map K.toText (KM.keys w) `shouldMatchList` ["token", "file", "unit", "writer", "goal", "edits", "dropped", "parses", "tests", "after", "cost", "seconds", "harder", "diffs", "situation", "reply", "took"]
      case KM.lookup "diffs" w of
        Just (Array ds) | [Object d] <- toList ds -> do
          KM.lookup "file" d `shouldBe` Just (J.String (T.pack moduleRel))
          (KM.lookup "diff" d >>= \case J.String t -> Just t; _ -> Nothing) `shouldSatisfy` maybe False (T.isInfixOf "+        return int(seats > 2)")
        other -> expectationFailure (show other)
      KM.lookup "reply" w `shouldBe` Just (J.String filled)
      (KM.lookup "tests" w >>= \case Object t -> KM.lookup "passing" t; _ -> Nothing) `shouldBe` Just (J.toJSON [testId])
      KM.lookup "took" w `shouldBe` Just (J.Bool True)
      CS.stepResumeOf (J.object ["resume" J..= J.object ["token" J..= ("step-1" :: Text), "choice" J..= ("harder" :: Text)]]) `shouldBe` Right (Just ("step-1", CS.StepHarder))
      CS.stepResumeOf (J.object ["resume" J..= J.object ["token" J..= ("step-1" :: Text), "choice" J..= ("climb" :: Text)]]) `shouldSatisfy` either (const True) (const False)
      CS.stepResumeOf (J.object []) `shouldBe` Right Nothing
    it "the file moved on before accept: nothing lands, said, and the step is dropped" $ do
      (s, _) <- serving (const (reply filled)) []
      sh <- shown <$> step s 6
      let s' = openDoc (root </> moduleRel) 2 (stubbed <> "\n# more\n") s
      r <- answer s' (CS.shToken sh) CS.StepAccept
      r `shouldSatisfy` either ("changed" `T.isInfixOf`) (const False)
      held s `shouldReturn` 0

-- | `olifant/stub`: a new test's stub for the unit at a line.
stubSpecs :: Spec
stubSpecs = describe "a test's stub (olifant/stub)" $ do
  describe "the unit's test name and the unit at a line" $ do
    let named n = do
          py <- shippedLanguage "python" []
          pure (testNameIn py (unitWordsOf n))
    it "a function's name, snake case" $ named "handle" `shouldReturn` "test_handle"
    it "a class's name, snake case" $ named "CloseTable" `shouldReturn` "test_close_table"
    it "underscores at its ends dropped" $ named "__init__" `shouldReturn` "test_init"
    it "a digit before a capital splits too" $ named "V2Table" `shouldReturn` "test_v2_table"
    -- `stubbed`: 1 class CloseTable, 2 def __init__, 4 blank, 5 def handle, 7-8 blank, 9 def unrelated
    let around line = do
          (s, _) <- blocksOnSyntax unitSyntax (const (Right (TestRun [] [] Nothing))) []
          py <- pythonOf s
          fmap (\n -> (namedName n, fst (namedLines n))) <$> unitAround unitSyntax py Nothing stubbed line
        aroundIn src line = do
          (s, _) <- blocksOnSyntax unitSyntax (const (Right (TestRun [] [] Nothing))) []
          py <- pythonOf s
          fmap (\n -> (namedName n, namedDef n)) <$> unitAround unitSyntax py Nothing src line
    it "the class line is the class" $ around 1 `shouldReturn` Right ("CloseTable", 1)
    it "a blank line between its methods is the class" $ around 4 `shouldReturn` Right ("CloseTable", 1)
    it "a line in a method is the method" $ around 6 `shouldReturn` Right ("CloseTable.handle", 5)
    it "a blank line after the class, before a top-level def, is no unit" $ around 8 `shouldReturn` Left "no function or class at line 8: put the cursor in one to stub its test"
    it "a top-level function" $ around 10 `shouldReturn` Right ("unrelated", 9)
    it "a line past the end is nothing" $ fmap (either (const Nothing) Just) (around 99) `shouldReturn` Nothing
    let decorated = T.unlines ["import functools", "", "", "@functools.cache", "@other", "def dish() -> int:", "    return 1"]
    it "a decorator line is its def's" $ aroundIn decorated 4 `shouldReturn` Right ("dish", 6)
    it "a line in the def stays" $ aroundIn decorated 7 `shouldReturn` Right ("dish", 6)
  describe "stubAt" $ do
    let quiet = blocksOnSyntax unitSyntax (const (Right (TestRun [] [] Nothing)))
        stub = "def test_handle() -> None:\n    ...\n    assert ...\n"
        sentence st path line = either id (const "stubbed") . snd <$> stubAt path line Nothing st
    it "in a method: test_<method> after the test file's last test, its import already there" $ do
      (s, _) <- quiet []
      (_, Right a) <- stubAt (root </> moduleRel) 6 Nothing s
      (saFile a, saLine a, saName a) `shouldBe` (root </> testRel, 9, "test_handle")
      feCreate (saEdit a) `shouldBe` False
      applyEdits [saEdit a] testText `shouldBe` T.stripEnd testText <> "\n\n\n" <> stub
    it "in a function the test file does not import: the import joins the file's" $ do
      (s, _) <- quiet []
      (_, Right a) <- stubAt (root </> moduleRel) 10 Nothing s
      saName a `shouldBe` "test_unrelated"
      let edited = applyEdits [saEdit a] testText
      T.lines edited !! 0 `shouldBe` "from shop.close_table import CloseTable, unrelated"
      T.lines edited !! (saLine a - 1) `shouldBe` "def test_unrelated() -> None:"
    it "on the class line, and on a blank line between its methods: the class" $ do
      (s, _) <- quiet []
      (_, Right a) <- stubAt (root </> moduleRel) 1 Nothing s
      (_, Right b) <- stubAt (root </> moduleRel) 4 Nothing s
      (saName a, saName b) `shouldBe` ("test_close_table", "test_close_table")
    it "a name the file has already: test_<unit>_2" $ do
      (s, _) <- quiet [(testRel, testText <> T.unlines ["", "", "def test_handle() -> None:", "    assert True"])]
      (_, Right a) <- stubAt (root </> moduleRel) 6 Nothing s
      saName a `shouldBe` "test_handle_2"
    it "a new test file: the edit creates it, the import first" $ do
      (s, _) <- quiet [("src/shop/menu.py", T.unlines ["import functools", "", "", "@functools.cache", "def dish() -> int:", "    return 1"])]
      (_, Right a) <- stubAt (root </> "src/shop/menu.py") 4 Nothing s
      (saFile a, saLine a, saName a) `shouldBe` (root </> "src/shop/menu__test.py", 4, "test_dish")
      (feCreate (saEdit a), feRange (saEdit a)) `shouldBe` (True, ((0, 0), (0, 0)))
      feText (saEdit a) `shouldBe` "from shop.menu import dish\n\n\ndef test_dish() -> None:\n    ...\n    assert ...\n"
    it "a method whose name another class has: imported by its own class" $ do
      let repo = T.unlines ["class Memory:", "    def save(self, n: int) -> int:", "        return n", "", "", "class Sql:", "    def save(self, n: int) -> int:", "        return n + 1"]
      (s, _) <- quiet [("src/shop/repo.py", repo)]
      (_, Right a) <- stubAt (root </> "src/shop/repo.py") 8 Nothing s
      T.lines (feText (saEdit a)) !! 0 `shouldBe` "from shop.repo import Sql"
    it "the editor's text of the test file: the edit is built on it and carries its version" $ do
      (s0, _) <- quiet []
      let s = openDoc (root </> testRel) 3 siblingText s0
      (_, Right a) <- stubAt (root </> moduleRel) 6 Nothing s
      feVersion (saEdit a) `shouldBe` Just 3
      applyEdits [saEdit a] siblingText `shouldBe` T.stripEnd siblingText <> "\n\n\n" <> stub
    it "outside any unit, and on a test file: one sentence each" $ do
      (s, _) <- quiet []
      sentence s (root </> moduleRel) 8 `shouldReturn` "no function or class at line 8: put the cursor in one to stub its test"
      sentence s (root </> testRel) 5 `shouldReturn` "on a test: stub writes a new test for a function or class, from its module"
    -- a survivor on line 6 of handle, `seats > 2` for `seats >= 2`
    let survivor = Mutant 6 "boundary" "seats > 2" "seats >= 2" (T.replace "return int(seats > 2)" "return int(seats >= 2)" written) False
        survivorStub name = "def " <> name <> "() -> None:\n    # kills: seats > 2 \8594 seats >= 2 on line 6\n    ...\n    assert ...\n"
    it "a survivor's stub: named from its mutant, the mutant as a comment above the ..." $ do
      (s, _) <- quiet [(moduleRel, written)]
      (_, Right a) <- stubAt (root </> moduleRel) 6 (Just survivor) s
      saName a `shouldBe` "test_boundary_int_seats"
      applyEdits [saEdit a] testText `shouldBe` T.stripEnd testText <> "\n\n\n" <> survivorStub "test_boundary_int_seats"
      T.lines (applyEdits [saEdit a] testText) !! (saLine a - 1) `shouldBe` "def test_boundary_int_seats() -> None:"
    it "a survivor's stub whose name the file has: _2" $ do
      (s, _) <- quiet [(moduleRel, written), (testRel, testText <> T.unlines ["", "", "def test_boundary_int_seats() -> None:", "    assert True"])]
      (_, Right a) <- stubAt (root </> moduleRel) 6 (Just survivor) s
      saName a `shouldBe` "test_boundary_int_seats_2"
    it "a survivor's stub is logged with the line it kills" $ do
      (s, _) <- quiet [(moduleRel, written)]
      (s', Right _) <- stubAt (root </> moduleRel) 6 (Just survivor) s
      logged <- fromMaybe "" <$> readText (pFiles (ssPorts s')) (root </> ".olifant/state/steps.log")
      logged `shouldSatisfy` T.isInfixOf ("\tstub\tperson\t" <> T.pack moduleRel <> "::handle\tstubbed test_boundary_int_seats (kills line 6)\n")
    it "each stub is one line in the step log: stub, person, the unit, the name" $ do
      (s, _) <- quiet []
      (s', Right _) <- stubAt (root </> moduleRel) 6 Nothing s
      logged <- fromMaybe "" <$> readText (pFiles (ssPorts s')) (root </> ".olifant/state/steps.log")
      logged `shouldSatisfy` T.isInfixOf ("\tstub\tperson\t" <> T.pack moduleRel <> "::handle\tstubbed test_handle\n")

-- | The unit at a line: the innermost definition, by the adapter's kinds.
unitSpecs :: Spec
unitSpecs = describe "the unit at a line" $ do
  let unit line = do
        (s, _) <- blocksOnSyntax unitSyntax (const (Right (TestRun [] [] Nothing))) []
        py <- pythonOf s
        unitAt unitSyntax py Nothing stubbed line
      withPy k = do
        (s, _) <- blocksOnSyntax unitSyntax (const (Right (TestRun [] [] Nothing))) []
        pythonOf s >>= k
  -- `stubbed`: 1 class CloseTable, 2 def __init__, 3 its body, 4 blank, 5 def handle, 6 its body, 7-8 blank, 9 def unrelated, 10 its body
  it "a line in a method's body: the method, its header and body" $
    unit 6 `shouldReturn` Right (Unit "handle" 5 (5, 6) (Just (6, 6)))
  it "a method's def line: the method" $ fmap unitName <$> unit 5 `shouldReturn` Right "handle"
  it "a blank line between two methods: the class, its body all its lines after its header" $
    unit 4 `shouldReturn` Right (Unit "CloseTable" 1 (1, 6) (Just (2, 6)))
  it "a top-level function" $ fmap unitName <$> unit 10 `shouldReturn` Right "unrelated"
  it "a blank line between two top-level definitions: none, one sentence" $
    unit 8 `shouldReturn` Left "no function or class at line 8"
  it "a decorator line: the def it decorates" $ withPy $ \py -> do
    let src = T.unlines ["import functools", "", "", "@functools.cache", "def dish() -> int:", "    return 1"]
    unitAt unitSyntax py Nothing src 4 `shouldReturn` Right (Unit "dish" 5 (4, 6) (Just (6, 6)))
  it "a header over several lines: the body starts after its colon" $ withPy $ \py -> do
    let src = T.unlines ["def book(", "    seats: int,", ") -> int:  # the party", "    return seats"]
    fmap unitBody <$> unitAt unitSyntax py Nothing src 4 `shouldReturn` Right (Just (4, 4))
  it "a one-line def: no body to conjure" $ withPy $ \py ->
    fmap unitBody <$> unitAt unitSyntax py Nothing "def one() -> int: return 1\n" 1 `shouldReturn` Right Nothing

pythonOf :: ServerState -> IO Language
pythonOf s = siteAt (root </> moduleRel) 1 s >>= \case
  (_, Right site) -> pure (siteLang site)
  (_, Left why) -> fail (T.unpack why)

-- | `olifant/mutate`: the survivors of the unit at the cursor.
mutateSpecs :: Spec
mutateSpecs = describe "a mutate (olifant/mutate)" $ do
  -- two statements in handle, a boundary on each: 6 `n = seats > 2`, 7 `return int(n and seats > 1)`
  let two = T.replace "raise NotImplementedError(\"later\")" "n = seats > 2\n        return int(n and seats > 1)" stubbed
      passing _ = Right (TestRun [testId] [] Nothing)
      -- every mutant of line 6 is caught, those of line 7 survive
      catching files = Right (if "n = seats >= 2" `T.isInfixOf` M.findWithDefault "" moduleRel files then TestRun [] [(testId, Failure (Just 6) "assert 0 == 1" "")] Nothing else TestRun [testId] [] Nothing)
      serving verdict extra doc = do
        (s0, runs) <- blocksOnSyntax unitSyntax verdict extra
        pure (openDoc (root </> moduleRel) 1 doc s0, runs)
      mutate s line = snd <$> mutateAt (root </> moduleRel) line s
      sentence s path line = either id (const "mutated") . snd <$> mutateAt path line s
      mutatedLine m = fromMaybe "" (lookup (muLine m) (zip [1 :: Int ..] (T.lines (muText m))))
  it "a method with a survivor: its lines, the survivor's line and mutated line, the count, the summary" $ do
    (s, _) <- serving catching [] two
    Right a <- mutate s 6
    (maFile a, maUnit a, maLines a, maKilled a) `shouldBe` (root </> moduleRel, "handle", (5, 7), 1)
    map muLine (maSurvivors a) `shouldBe` [7]
    map mutatedLine (maSurvivors a) `shouldBe` ["        return int(n and seats >= 1)"]
    maSummary a `shouldBe` "1 survivor \183 1/2 killed"
  it "a mutable site in another function of the module is not a mutant" $ do
    (s, _) <- serving passing [] written
    Right a <- mutate s 6
    -- `unrelated`'s `n > 0` (line 10) is outside handle
    map muLine (maSurvivors a) `shouldBe` [6]
    maSummary a `shouldBe` "1 survivor \183 0/1 killed"
  it "the test file red before mutation: one sentence, and only the baseline ran" $ do
    (s, runs) <- serving (const (Right (TestRun [] [(testId, Failure (Just 6) "boom" "")] Nothing))) [] written
    mutate s 6 `shouldReturn` Left "could not judge: test_closes_a_table fails before mutation"
    runs `shouldReturn` 1
  it "a baseline that cannot run: one sentence" $ do
    (s, _) <- serving (const (Right (TestRun [] [] (Just "no pytest")))) [] written
    mutate s 6 `shouldReturn` Left "could not run: no pytest"
  it "on a test file, outside any unit, with no test file: one sentence each" $ do
    (s, _) <- serving passing [("src/shop/menu.py", "def dish() -> int:\n    return 1\n")] written
    sentence s (root </> testRel) 5 `shouldReturn` "on a test: mutate a function or class, from its module"
    sentence s (root </> moduleRel) 8 `shouldReturn` "no function or class at line 8: put the cursor in one to mutate it"
    sentence s (root </> "src/shop/menu.py") 2 `shouldReturn` "no test file for this module yet: src/shop/menu__test.py; stub one first (:Durandal stub)"
  it "every mutant killed: no survivors" $ do
    let killing files = Right (if ">=" `T.isInfixOf` M.findWithDefault "" moduleRel files then TestRun [] [(testId, Failure (Just 6) "x" "")] Nothing else TestRun [testId] [] Nothing)
    (s, _) <- serving killing [] written
    Right a <- mutate s 6
    (maKilled a, maSurvivors a, maSummary a) `shouldBe` (1, [], "no survivors \183 1/1 killed")
  it "no mutable site in the unit: nothing to mutate" $ do
    (s, runs) <- serving passing [] stubbed
    Right a <- mutate s 6
    (maKilled a, maSurvivors a, maSummary a) `shouldBe` (0, [], "nothing to mutate in handle")
    runs `shouldReturn` 0
  it "the editor's unsaved text is the one mutated" $ do
    -- on disk the module is the stub (nothing to mutate); the editor has written it
    (s, _) <- serving passing [] written
    Right a <- mutate s 6
    map muLine (maSurvivors a) `shouldBe` [6]
    all (T.isInfixOf "return int(seats >= 2)" . muText) (maSurvivors a) `shouldBe` True
  it "the decision model's answers known: the equivalent one dim and last" $
    withSystemTempDirectory "olifant-mutate" $ \dir -> do
      let answers = dir </> "answers.json"
      TIO.writeFile answers "{\"answers\": {}}"
      (s0, _) <- serving passing [(".olifant/config.toml", "[packs]\npython-htmx = { path = \"/pack\" }\n\n[decide]\nmodel = \"jev\"\n[model.backends.jev]\nkind = \"jev\"\n")] two
      (s1, Right first') <- mutateAt (root </> moduleRel) 6 s0
      map muLine (maSurvivors first') `shouldBe` [6, 7]
      let m : _ = maSurvivors first'
          s = s1 {ssConfig = defaultServerConfig {scDecideAnswers = Just answers}}
          line' = T.strip (mutatedLine m)
          key = (root </> moduleRel, T.intercalate " " ["mutant-equivalent", muOperator m, T.pack (show (muLine m)), muFrom m, muTo m, line'])
      writeIORef (ssDecisions s) (M.singleton key (line', De.Yes De.Unknown), mempty)
      Right a <- mutate s 6
      map (\x -> (muLine x, muDim x)) (maSurvivors a) `shouldBe` [(7, False), (6, True)]
  it "each mutate is one line in the step log: mutate, person, the unit, the summary" $ do
    (s, _) <- serving catching [] two
    (s', Right _) <- mutateAt (root </> moduleRel) 6 s
    logged <- fromMaybe "" <$> readText (pFiles (ssPorts s')) (root </> ".olifant/state/steps.log")
    logged `shouldSatisfy` T.isInfixOf ("\tmutate\tperson\t" <> T.pack moduleRel <> "::handle\t1 survivor \183 1/2 killed\n")
  describe "a waiver (olifant/waive)" $ do
    let waiveFirst s reason = do
          Right a <- mutate s 6
          let m : _ = maSurvivors a
          (s', r) <- waiveAt (root </> moduleRel) reason m s
          pure (s', r, m)
    it "a waived survivor is left out of the next mutate, which counts it" $ do
      (s0, _) <- serving catching [] two
      (s, r, _) <- waiveFirst s0 "n only gates the return"
      r `shouldBe` Right ()
      Right a <- mutate s 6
      (maSurvivors a, maWaived a, maKilled a) `shouldBe` ([], 1, 1)
      maSummary a `shouldBe` "no survivors \183 1/2 killed \183 1 waived"
    it "a waiver is a line of .olifant/state/waived.log: the time, file, unit, mutant, line text and reason" $ do
      (s0, _) <- serving catching [] two
      (s, _, _) <- waiveFirst s0 "n only   gates the return"
      logged <- fromMaybe "" <$> readText (pFiles (ssPorts s)) (root </> ".olifant/state/waived.log")
      logged `shouldSatisfy` T.isInfixOf ("\t" <> T.pack moduleRel <> "\thandle\t>\t>=\treturn int(n and seats >= 1)\tn only gates the return\n")
    it "an empty reason is refused, and nothing is written" $ do
      (s0, _) <- serving catching [] two
      (s, r, _) <- waiveFirst s0 "  "
      r `shouldBe` Left "a waiver needs a reason: why is this mutant equivalent, or not worth a test?"
      readText (pFiles (ssPorts s)) (root </> ".olifant/state/waived.log") `shouldReturn` Nothing
    it "a waiver is not about the line number: the unit moved down, the mutant is still waived" $ do
      (s0, _) <- serving catching [] two
      (s, _, _) <- waiveFirst s0 "equivalent"
      let moved = openDoc (root </> moduleRel) 2 ("# a note\n" <> two) s
      Right a <- mutate moved 7
      (map muLine (maSurvivors a), maWaived a) `shouldBe` ([], 1)
    it "a waiver of another mutant, unit or file does not match" $ do
      (s0, _) <- serving catching [] two
      Right a0 <- mutate s0 6
      let m : _ = maSurvivors a0
      (s, Right ()) <- waiveAt (root </> moduleRel) "equivalent" m {muTo = ">= 9"} s0
      Right a <- mutate s 6
      (map muLine (maSurvivors a), maWaived a) `shouldBe` ([7], 0)

-- | `olifant/tests/run`: a file's tests, run whole over the editor's text.
testsSpecs :: Spec
testsSpecs = describe "a file's tests (olifant/tests/run)" $ do
  describe "the test at a line (testCaseAtIn)" $ do
    let file = T.unlines
          [ "from shop.close_table import CloseTable"   -- 1
          , ""                                          -- 2
          , ""                                          -- 3
          , "def make_table() -> CloseTable:"           -- 4
          , "    return CloseTable()"                   -- 5
          , ""                                          -- 6
          , ""                                          -- 7
          , "@pytest.mark.parametrize(\"n\", [1, 2])"   -- 8
          , "def test_closes(n: int) -> None:"          -- 9
          , "    table = make_table()"                  -- 10
          , ""                                          -- 11
          , "    assert table.handle(n) == 1"           -- 12
          , ""                                          -- 13
          , ""                                          -- 14
          , "class TestSeats:"                          -- 15
          , "    def test_two(self) -> None:"           -- 16
          , "        assert make_table().handle(2) == 0" -- 17
          ]
    it "a line in a test's body is that test" $ testCaseAtIn pyNow Nothing file 10 `shouldBe` Just "test_closes"
    it "a blank line inside the body is still that test" $ testCaseAtIn pyNow Nothing file 11 `shouldBe` Just "test_closes"
    it "the def line is that test" $ testCaseAtIn pyNow Nothing file 9 `shouldBe` Just "test_closes"
    it "a decorator line is its def's test" $ testCaseAtIn pyNow Nothing file 8 `shouldBe` Just "test_closes"
    it "a method of a test class is Class::method" $ testCaseAtIn pyNow Nothing file 17 `shouldBe` Just "TestSeats::test_two"
    it "outside any test, nothing: imports" $ testCaseAtIn pyNow Nothing file 1 `shouldBe` Nothing
    it "outside any test, nothing: a helper that is not a test" $ testCaseAtIn pyNow Nothing file 5 `shouldBe` Nothing
    it "the class line itself is no test" $ testCaseAtIn pyNow Nothing file 15 `shouldBe` Nothing
    it "a line past the file's end is nothing" $ testCaseAtIn pyNow Nothing file 99 `shouldBe` Nothing
  let recording verdict = blocksRun blockSyntax (pure ()) (const verdict)
      passing _ = Right (TestRun [testId] [] Nothing)
      -- a language of the pack's with no [test] table
      plain = ("/pack/languages/plain.toml", "files = [\"**/*.txt\"]\ncomment = \"#\"\ngrammar = { git = \"https://github.com/tree-sitter/tree-sitter-python\", rev = \"293fdc02038ee2bf0e2e206711b69c90ac0d413f\" }\n")
  it "on a module, runs its test file, as one id" $ do
    seen <- newIORef ([] :: [[Text]])
    (s, _) <- recording passing []
    let s' = s {ssPorts = (ssPorts s) {pTester = \c -> Runner (\l ids f -> modifyIORef' seen (++ [ids]) >> runTests (pTester (ssPorts s) c) l ids f)}}
    (_, Right a) <- testsRun ScopeFile (root </> moduleRel) s'
    taFile a `shouldBe` root </> testRel
    readIORef seen `shouldReturn` [[T.pack testRel]]
    trPassed (taRun a) `shouldBe` [testId]
    taSeconds a `shouldSatisfy` (>= 0)
  it "answers the run's wall time in seconds" $ do
    (s, _) <- blocksRun blockSyntax (threadDelay 50000) (\_ _ -> Right (TestRun [testId] [] Nothing)) []
    (_, Right a) <- testsRun ScopeFile (root </> moduleRel) s
    taSeconds a `shouldSatisfy` (>= 0.05)
  it "on a test file, runs that file" $ do
    (s, _) <- recording passing []
    (_, Right a) <- testsRun ScopeFile (root </> testRel) s
    taFile a `shouldBe` root </> testRel
  it "runs the editor's text: an open, unsaved test file is what runs" $ do
    (s, _) <- recording (\files -> if "unsaved" `T.isInfixOf` M.findWithDefault "" testRel files then Right (TestRun [testId] [] Nothing) else Right (failing "saved text ran")) []
    (_, Right a) <- testsRun ScopeFile (root </> testRel) (openDoc (root </> testRel) 1 (testText <> "# unsaved\n") s)
    trFailed (taRun a) `shouldBe` []
  it "runs the editor's text: an open, unsaved module is what its tests run over" $ do
    (s, _) <- recording (\files -> if "unsaved" `T.isInfixOf` M.findWithDefault "" moduleRel files then Right (TestRun [testId] [] Nothing) else Right (failing "saved text ran")) []
    (_, Right a) <- testsRun ScopeFile (root </> moduleRel) (openDoc (root </> moduleRel) 1 (stubbed <> "# unsaved\n") s)
    trFailed (taRun a) `shouldBe` []
  it "on a test file whose module is gone, runs that file" $ do
    seen <- newIORef ([] :: [[Text]])
    (s, _) <- blocksRun blockSyntax (pure ()) (\ids _ -> Right (TestRun ids [] Nothing)) [("src/shop/orphan__test.py", "def test_alone() -> None:\n    assert True\n")]
    let s' = s {ssPorts = (ssPorts s) {pTester = \c -> Runner (\l ids f -> modifyIORef' seen (++ [ids]) >> runTests (pTester (ssPorts s) c) l ids f)}}
    (_, Right a) <- testsRun ScopeFile (root </> "src/shop/orphan__test.py") s'
    taFile a `shouldBe` root </> "src/shop/orphan__test.py"
    readIORef seen `shouldReturn` [["src/shop/orphan__test.py"]]
  it "refuses a language whose tests_for names no file, in one sentence" $ do
    (s, _) <- recording passing [("/pack/languages/blank.toml", "files = [\"**/*.md\"]\ncomment = \"#\"\ngrammar = { git = \"https://github.com/tree-sitter/tree-sitter-python\", rev = \"293fdc02038ee2bf0e2e206711b69c90ac0d413f\" }\n[test]\npreset = \"pytest\"\ntests_for = \"\"\n"), ("notes/a.md", "a\n")]
    (_, answer) <- testsRun ScopeFile (root </> "notes/a.md") s
    answer `shouldSatisfy` either (== "blank's tests_for names no test file: nothing to run") (const False)
  it "refuses a language with no [test] table, in one sentence" $ do
    (s, _) <- recording passing [plain, ("notes/a.txt", "a\n")]
    (_, answer) <- testsRun ScopeFile (root </> "notes/a.txt") s
    answer `shouldSatisfy` either (== "plain has no [test] table: nothing to run") (const False)
  it "refuses a module with no test file yet, naming the file it would be" $ do
    (s, _) <- recording passing [("src/shop/lonely.py", "def alone() -> int:\n    return 1\n")]
    (_, answer) <- testsRun ScopeFile (root </> "src/shop/lonely.py") s
    answer `shouldSatisfy` either (== "no test file for this module yet: src/shop/lonely__test.py") (const False)
  it "refuses a file in no project" $ do
    (s, _) <- recording passing []
    (_, answer) <- testsRun ScopeFile "/elsewhere/x.py" s
    answer `shouldSatisfy` either ("is in no Olifant project" `T.isInfixOf`) (const False)
  it "answers a failure with its line, message and output, as the editor reads it" $ do
    (s, _) <- recording (\_ -> Right (TestRun [] [(testId, Failure (Just 6) "assert 0 == 1" "E   assert 0 == 1")] Nothing)) []
    (_, Right a) <- testsRun ScopeFile (root </> moduleRel) s
    let json = testsJson a
        failed = case json of
          Object o | Just (Array xs) <- KM.lookup "failed" o -> toList xs
          _ -> []
    lookupIn "file" json `shouldBe` Just (String (T.pack (root </> testRel)))
    lookupIn "passed" json `shouldBe` Just (Array mempty)
    lookupIn "broken" json `shouldBe` Just Null
    failed `shouldBe` [J.object ["id" J..= testId, "file" J..= (root </> testRel), "line" J..= (6 :: Int), "message" J..= ("assert 0 == 1" :: Text), "output" J..= ("E   assert 0 == 1" :: Text)]]
    lookupIn "seconds" json `shouldSatisfy` \case Just (Number _) -> True; _ -> False
  it "answers a run whose tests could not run as broken, with the reason" $ do
    (s, _) <- recording (\_ -> Right (TestRun [] [] (Just "ImportError: no module named shop"))) []
    (_, Right a) <- testsRun ScopeFile (root </> moduleRel) s
    lookupIn "broken" (testsJson a) `shouldBe` Just (String "ImportError: no module named shop")
  it "answers a runner that cannot start as broken, with why" $ do
    (s, _) <- recording (\_ -> Left "uv is not installed") []
    (_, Right a) <- testsRun ScopeFile (root </> moduleRel) s
    trBroken (taRun a) `shouldBe` Just "uv is not installed"
  let recordingIds files = do
        seen <- newIORef ([] :: [[Text]])
        (s, _) <- blocksRun blockSyntax (pure ()) (\ids _ -> Right (TestRun ids [] Nothing)) files
        pure (s {ssPorts = (ssPorts s) {pTester = \c -> Runner (\l ids f -> modifyIORef' seen (++ [ids]) >> runTests (pTester (ssPorts s) c) l ids f)}}, seen)
  it "a line in a test runs that test alone" $ do
    (s, seen) <- recordingIds []
    (_, Right a) <- testsRun (ScopeAt 5) (root </> testRel) s
    readIORef seen `shouldReturn` [[testId]]
    taTest a `shouldBe` Just testId
    lookupIn "scope" (testsJson a) `shouldBe` Just (String "test")
  it "outside any test, the whole file" $ do
    (s, seen) <- recordingIds []
    (_, Right a) <- testsRun (ScopeAt 1) (root </> testRel) s
    readIORef seen `shouldReturn` [[T.pack testRel]]
    lookupIn "scope" (testsJson a) `shouldBe` Just (String "file")
  it "a line in a module runs its test file" $ do
    (s, seen) <- recordingIds []
    (_, Right _) <- testsRun (ScopeAt 5) (root </> moduleRel) s
    readIORef seen `shouldReturn` [[T.pack testRel]]
  it "all runs every test file of the project, from any file in it" $ do
    (s, seen) <- recordingIds [("src/shop/menu.py", "def dish() -> int:\n    return 1\n"), ("src/shop/menu__test.py", "def test_dish() -> None:\n    assert True\n"), ("src/shop/orphan__test.py", "def test_alone() -> None:\n    assert True\n")]
    (_, Right a) <- testsRun ScopeAll (root </> moduleRel) s
    readIORef seen `shouldReturn` [["src/shop/close_table__test.py", "src/shop/menu__test.py", "src/shop/orphan__test.py"]]
    taFile a `shouldBe` root
    lookupIn "scope" (testsJson a) `shouldBe` Just (String "all")
  it "run all on inline tests lists the files holding tests" $ do
    let files = [("Cargo.toml", "[package]\nname = \"a\"\n"), ("src/a.rs", "pub fn one() -> i32 {\n    1\n}\n\n#[cfg(test)]\nmod tests {\n    use super::*;\n\n    #[test]\n    fn it_is_one() {\n        assert_eq!(one(), 1);\n    }\n}\n"), ("src/b.rs", "pub fn two() -> i32 {\n    2\n}\n"), ("notes/c.rs.txt", "#[test]\nfn not_rust() {}\n")]
    (disk, _) <- memoryWorkspace ((root </> ".olifant/config.toml", "languages = [\"rust\"]\n") : [(root </> p, t) | (p, t) <- files])
    seen <- newIORef ([] :: [[Text]])
    let runner = Runner $ \_ ids _ -> modifyIORef' seen (++ [ids]) >> pure (Right (TestRun ids [] Nothing))
        ports = Ports disk blockSyntax (const noChecker) (const noVerifier) (const runner) (\_ _ -> runner) (pure (UTCTime (fromGregorian 2026 10 12) 0)) Nothing Nothing (DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False) (\_ -> pure Nothing) (\_ -> pure []) (\_ -> pure (Left NoSource))) (\_ -> pure Nothing) (\_ _ _ -> pure (Left "no base in tests")) (\_ _ -> pure (Left "no base in tests")) gitStateStamp
    s <- newServerState ports
    (_, r) <- testsRun ScopeAll (root </> "src/b.rs") s
    fmap (const ()) r `shouldBe` Right ()
    readIORef seen `shouldReturn` [["src/a.rs"]]
  it "all with failures in two files: each failure names its own file" $ do
    let other = "src/shop/menu__test.py::test_dish"
    (s, _) <- blocksRun blockSyntax (pure ()) (\_ _ -> Right (TestRun [] [(testId, Failure (Just 6) "a" ""), (other, Failure (Just 2) "b" "")] Nothing)) [("src/shop/menu.py", "def dish() -> int:\n    return 1\n"), ("src/shop/menu__test.py", "def test_dish() -> None:\n    assert False\n")]
    (_, Right a) <- testsRun ScopeAll (root </> moduleRel) s
    let files = case lookupIn "failed" (testsJson a) of
          Just (Array xs) -> [f | Object o <- toList xs, Just (String f) <- [KM.lookup "file" o]]
          _ -> []
    files `shouldBe` [T.pack (root </> testRel), T.pack (root </> "src/shop/menu__test.py")]
  it "a test the run does not report is broken, by name" $ do
    (s, _) <- blocksRun blockSyntax (pure ()) (\_ _ -> Right (TestRun [] [] Nothing)) []
    (_, Right a) <- testsRun (ScopeAt 5) (root </> testRel) s
    trBroken (taRun a) `shouldBe` Just "test_closes_a_table did not run: is it still in its test file?"
  it "a parametrized test's cases count as that test" $ do
    (s, _) <- blocksRun blockSyntax (pure ()) (\_ _ -> Right (TestRun [testId <> "[1]", testId <> "[2]"] [] Nothing)) []
    (_, Right a) <- testsRun (ScopeAt 5) (root </> testRel) s
    trBroken (taRun a) `shouldBe` Nothing
  it "each run is one line in the step log: test, person, what ran, the outcome" $ do
    (s, _) <- blocksRun blockSyntax (pure ()) (\_ _ -> Right (TestRun [] [(testId, Failure (Just 6) "assert 0 == 1" "")] Nothing)) []
    (s', Right _) <- testsRun (ScopeAt 5) (root </> testRel) s
    logged <- fromMaybe "" <$> readText (pFiles (ssPorts s')) (root </> ".olifant/state/steps.log")
    logged `shouldSatisfy` T.isInfixOf ("\ttest\tperson\t" <> testId <> "\t0 passed, 1 failed: test_closes_a_table\n")
  describe "a file's counterpart (olifant/tests/file)" $ do
    it "on a module, answers its test file, kind test" $ do
      (s, _) <- recording passing []
      (_, answer) <- testsFile (root </> moduleRel) s
      answer `shouldBe` Right (TestsFile (root </> testRel) True True)
    it "on a test file, answers the module it tests, kind module" $ do
      (s, _) <- recording passing []
      (_, answer) <- testsFile (root </> testRel) s
      answer `shouldBe` Right (TestsFile (root </> moduleRel) False True)
    it "on a module with no test file yet, answers the path it would be, which does not exist" $ do
      (s, _) <- recording passing [("src/shop/lonely.py", "def alone() -> int:\n    return 1\n")]
      (_, answer) <- testsFile (root </> "src/shop/lonely.py") s
      answer `shouldBe` Right (TestsFile (root </> "src/shop/lonely__test.py") True False)
    it "answers as the editor reads it: file, kind, exists" $ do
      (s, _) <- recording passing []
      (_, Right a) <- testsFile (root </> moduleRel) s
      testsFileJson a `shouldBe` J.object ["file" J..= (root </> testRel), "kind" J..= ("test" :: Text), "exists" J..= True]
      (_, Right m) <- testsFile (root </> testRel) s
      lookupIn "kind" (testsFileJson m) `shouldBe` Just (String "module")
    it "refuses a language with no [test] table, in one sentence" $ do
      (s, _) <- recording passing [plain, ("notes/a.txt", "a\n")]
      (_, answer) <- testsFile (root </> "notes/a.txt") s
      answer `shouldSatisfy` either (== "plain has no [test] table: no test file to jump to") (const False)
    it "refuses a test file whose module cannot be named, in one sentence" $ do
      (s, _) <- recording passing [("src/shop/orphan__test.py", "def test_alone() -> None:\n    assert True\n")]
      (_, answer) <- testsFile (root </> "src/shop/orphan__test.py") s
      answer `shouldSatisfy` either (== "no module's test file is src/shop/orphan__test.py: its module cannot be named") (const False)
    it "refuses a file that is its own test file (tests_for names the file itself), in one sentence" $ do
      (s, _) <- recording passing [("/pack/languages/inline.toml", "files = [\"**/*.txt\"]\ncomment = \"#\"\ngrammar = { git = \"https://github.com/tree-sitter/tree-sitter-python\", rev = \"293fdc02038ee2bf0e2e206711b69c90ac0d413f\" }\n[test]\npreset = \"pytest\"\ntests_for = \"{= dir =}/{= stem =}.txt\"\n"), ("notes/a.txt", "a\n")]
      (_, answer) <- testsFile (root </> "notes/a.txt") s
      answer `shouldSatisfy` either (== "notes/a.txt is its own test file (inline's tests_for): no other file to jump to") (const False)
    it "refuses a file in no project" $ do
      (s, _) <- recording passing []
      (_, answer) <- testsFile "/elsewhere/x.py" s
      answer `shouldSatisfy` either ("is in no Olifant project" `T.isInfixOf`) (const False)
  where
    lookupIn k = \case
      Object o -> KM.lookup (K.fromText k) o
      _ -> Nothing

-- | The decision model orders a mutate's survivors ('orderSurvivors').
orderSpecs :: Spec
orderSpecs = describe "the decision model's order" $ do
  it "(k) Jev: survivors it thinks equivalent are dimmed and last, the rest by how much they matter; with no decider, file order" $ do
    (s, _) <- blocks redOnStub []
    let lineOf n = T.lines stubbed !! (n - 1)
        mutant n op to = Mutant n op "" "" (T.unlines [if i == n then to else l | (i, l) <- zip [1 ..] (T.lines stubbed)]) False
        ms = [mutant 3 "number" "        self.closed = 1", mutant 6 "string" (T.replace "later" "" (lineOf 6)), mutant 10 "boundary" "    return n >= 0"]
        jev = De.decider $ \q -> pure $ case (De.qKind q, lookup "mutant" (De.qContext q)) of
          ("mutant-equivalent", Just m) | "\"\"" `T.isInfixOf` m -> De.Yes De.Unknown
          ("mutant-equivalent", _) -> De.No De.Unknown
          ("mutant-matters", Just m) | ">=" `T.isInfixOf` m -> De.Level 2 2 De.Unknown
          _ -> De.Unsure
    orderSurvivors s Nothing pyNow (root </> moduleRel) stubbed testText (pure ()) ms >>= (`shouldBe` ms)
    answered <- newIORef (0 :: Int)
    first' <- orderSurvivors s (Just jev) pyNow (root </> moduleRel) stubbed testText (atomicModifyIORef' answered (\n -> (n + 1, ()))) ms
    first' `shouldBe` ms -- nothing known yet: as given
    waitUntil ((>= 6) <$> readIORef answered)
    ordered <- orderSurvivors s (Just jev) pyNow (root </> moduleRel) stubbed testText (pure ()) ms
    map (\m -> (muLine m, muDim m)) ordered `shouldBe` [(10, False), (3, False), (6, True)]
  it "(k2) Jev asks about two mutants of one operator on one line once each, not without end" $ do
    (s, _) <- blocks redOnStub []
    asked <- newIORef (0 :: Int)
    let twin from to line = Mutant 10 "boundary" from to (T.unlines [if i == 10 then line else l | (i, l) <- zip [1 :: Int ..] (T.lines stubbed)]) False
        ms = [twin "a > b" "a >= b" "    return a >= b and c > d", twin "c > d" "c >= d" "    return a > b and c >= d"]
        counting = De.decider $ \_ -> atomicModifyIORef' asked (\n -> (n + 1, ())) >> pure De.Unsure
        -- as the server does: each answer orders again
        again = void (orderSurvivors s (Just counting) pyNow (root </> moduleRel) stubbed testText again ms)
    again
    -- the four questions answered and none in flight: nothing more is asked
    waitUntil (settled s 4)
    readIORef asked `shouldReturn` 4
  it "(k2) a decision asked by many at once is asked once" $ do
    (s, _) <- blocks redOnStub []
    asked <- newIORef (0 :: Int)
    let counting = De.decider $ \_ -> atomicModifyIORef' asked (\n -> (n + 1, ())) >> pure De.Unsure
        q = De.score "mutant-matters" "" [] []
        keys = [(root </> moduleRel, "matters:" <> T.pack (show i)) | i <- [1 .. 300 :: Int]]
    mapM_ (\key -> replicateConcurrently_ 8 (St.decision s counting key "fp" q (pure ()))) keys
    waitUntil (settled s (length keys))
    readIORef asked `shouldReturn` length keys

-- | The modules a conjure's context follows ('importedModules').
siteSpecs :: Spec
siteSpecs = describe "a conjure's imported modules" $ do
  it "reads Python's absolute imports: aliases, several names, brackets, a star and relative imports" $ do
    pythonResolve <- maybe [] imResolve . langImportsDef <$> shippedLanguage "python" []
    importedModules pyNow pythonResolve "import a.b as c, d\nfrom .x import y\nfrom p.q import (\n    r,\n)\nfrom m import n as k, *  # c\n" `shouldBe` ["a.b", "d", "p.q", "m", "m.n"]
  it "importOf writes [imports] write over the module and the unit's owner" $ do
    py <- shippedLanguage "python" []
    hs <- shippedLanguage "haskell" []
    rust <- shippedLanguage "rust" []
    importOf py "src/a/b.py" "Table.seats" `shouldBe` Just "from a.b import Table"
    importOf hs "src/Olifant/Plans.hs" "parsePlan" `shouldBe` Just "import Olifant.Plans"
    importOf rust "src/money.rs" "convert" `shouldBe` Nothing
  it "resolves imports from data: a Haskell language's resolve and paths" $ do
    let Right hs =
          parseLanguage "haskell" "/h" . T.unlines $
            [ "files = [\"**/*.hs\"]", "comment = \"--\"", "grammar = { git = \"https://github.com/tree-sitter/tree-sitter-haskell\", rev = \"c30d812bc90827f1a54106a25bc9a6307f5cdcec\" }"
            , "[imports]", "module_of = { strip = [\"^src/\"], separator = \".\" }", "paths = [\"src/{= path =}.hs\"]"
            , "[[imports.resolve]]", "pattern = \"^import +(qualified +)?([A-Z][A-Za-z0-9_.]*)\"", "module = 2" ]
    (ws, _) <- memoryWorkspace [("src/A.hs", "module A where\nimport qualified Data.Map as M\nimport B.C\n"), ("src/B/C.hs", "module B.C where\nc = 1\n")]
    importedContext ws hs [("src/A.hs", "module A where\nimport qualified Data.Map as M\nimport B.C\n")] `shouldReturn` [("src/B/C.hs", "module B.C where\nc = 1\n")]
  it "Rust: use crate:: names its crate's files" $ do
    rust <- shippedLanguage "rust" []
    let service = "use crate::ports::{Invoices, Clock};\nuse std::sync::Arc;\n\npub struct Billing;\n"
        portsText = "pub trait Invoices {}\npub trait Clock {}\n"
    (ws, _) <- memoryWorkspace [("billing/billing-service/src/service.rs", service), ("billing/billing-service/src/ports.rs", portsText), ("other/src/ports.rs", "pub trait Other {}\n")]
    importedContext ws rust [("billing/billing-service/src/service.rs", service)] `shouldReturn` [("billing/billing-service/src/ports.rs", portsText)]
  it "Haskell: import [qualified] M names src/M.hs" $ do
    hs <- shippedLanguage "haskell" []
    let app = "module Olifant.App where\nimport qualified Olifant.Language as L\nimport Data.Text (Text)\n"
    (ws, _) <- memoryWorkspace [("src/Olifant/App.hs", app), ("src/Olifant/Language.hs", "module Olifant.Language where\nx = 1\n")]
    importedContext ws hs [("src/Olifant/App.hs", app)] `shouldReturn` [("src/Olifant/Language.hs", "module Olifant.Language where\nx = 1\n")]

root, moduleRel, testRel :: FilePath
root = "/r"
moduleRel = "src/shop/close_table.py"
testRel = "src/shop/close_table__test.py"

testId :: Text
testId = "src/shop/close_table__test.py::test_closes_a_table"

stubbed, written, testText :: Text
stubbed = T.unlines ["class CloseTable:", "    def __init__(self) -> None:", "        self.closed = 0", "", "    def handle(self, seats: int) -> int:", "        raise NotImplementedError(\"later\")", "", "", "def unrelated(n: int) -> bool:", "    return n > 0"]
written = T.replace "raise NotImplementedError(\"later\")" "return int(seats > 2)" stubbed
testText = T.unlines ["from shop.close_table import CloseTable", "", "", "def test_closes_a_table() -> None:", "    table = CloseTable()", "    assert table.handle(4) == 1"]

failing :: Text -> TestRun
failing why = TestRun [] [(testId, Failure (Just 6) why "")] Nothing

-- | The test file with a sibling test after the block's, and its id.
siblingText :: Text
siblingText = testText <> T.unlines ["", "", "def test_sibling() -> None:", "    assert CloseTable().handle(1) == 0"]

-- | The module's test file run test by test (by the names it defines):
-- `test_closes_a_table` red on the stub; `test_kills_the_boundary` (or any
-- test asserting `table.handle(2) == 0`) red on the boundary mutant
-- (`seats >= 2`); `test_next` red always (the next test,
-- still being written); any other red at its assertion. A test whose
-- arrangement is still the stub's placeholder fails there.
killedBy :: M.Map FilePath Text -> Either Text TestRun
killedBy files = Right (TestRun [i | (i, Nothing) <- results] [(i, f) | (i, Just f) <- results] Nothing)
  where
    (m, t) = (M.findWithDefault "" moduleRel files, M.findWithDefault "" testRel files)
    numbered = zip [1 :: Int ..] (T.splitOn "\n" t)
    results = [(T.pack testRel <> "::" <> n, verdict n body) | (i, l) <- numbered, Just n <- [T.stripPrefix "def " (T.strip l) >>= named], let body = takeWhile (\(_, x) -> not ("def " `T.isPrefixOf` x)) (drop i numbered)]
    named rest = let n = T.takeWhile (\c -> isAlphaNum c || c == '_') rest in if "test" `T.isPrefixOf` n && "(" `T.isPrefixOf` T.drop (T.length n) rest then Just n else Nothing
    lineOf p body = listToMaybe [i | (i, x) <- body, p x]
    verdict n body
      | Just at <- lineOf ("olifant: arrange" `T.isInfixOf`) body = Just (Failure (Just at) "NotImplementedError" "")
      | n == "test_closes_a_table" = if "NotImplementedError" `T.isInfixOf` m then Just (Failure (Just 6) "NotImplementedError: later" "") else Nothing
      | n == "test_kills_the_boundary" || any (("table.handle(2) == 0" `T.isInfixOf`) . snd) body = if "NotImplementedError" `T.isInfixOf` m || "seats >= 2" `T.isInfixOf` m then Just (Failure (lineOf ("assert" `T.isInfixOf`) body) "assert 1 == 0" "") else Nothing
      | otherwise = Just (Failure (lineOf (("assert" `T.isPrefixOf`) . T.strip) body) "AssertionError" "")

-- | Edits of one text, applied as the editor does (each over the last).
applyEdits :: [FileEdit] -> Text -> Text
applyEdits edits text = foldl (\t e -> applyChange (feRange e) (feText e) t) text edits

-- | The decisions: N answered, and none being asked.
settled :: ServerState -> Int -> IO Bool
settled s n = (\(known, asking) -> M.size known == n && S.null asking) <$> St.decisionsNow s

-- | Until a condition holds (five seconds at most).
waitUntil :: IO Bool -> IO ()
waitUntil ready = go (250 :: Int)
  where
    go 0 = expectationFailure "waited five seconds"
    go n = ready >>= \done -> if done then pure () else threadDelay 20000 >> go (n - 1)

-- | Red while the body is the stub, green once written.
redOnStub :: Text -> TestRun
redOnStub t
  | "NotImplementedError" `T.isInfixOf` t = failing "NotImplementedError: later"
  | otherwise = TestRun [testId] [] Nothing

-- | A server over /r (the module, its test, recorded fills; the python-htmx
-- pack at /pack) whose runner answers by the module's text, and how many
-- runs it made.
blocks :: (Text -> TestRun) -> [(FilePath, Text)] -> IO (ServerState, IO Int)
blocks verdict = blocksWith (Right . verdict)

-- | 'blocks' whose runner may fail to run at all (Left).
blocksWith :: (Text -> Either Text TestRun) -> [(FilePath, Text)] -> IO (ServerState, IO Int)
blocksWith verdict = blocksOn (verdict . M.findWithDefault "" moduleRel)

-- | 'blocksWith' whose runner answers by every file it is given.
blocksOn :: (M.Map FilePath Text -> Either Text TestRun) -> [(FilePath, Text)] -> IO (ServerState, IO Int)
blocksOn = blocksOnSyntax blockSyntax

-- | 'blocksOn' over another syntax.
blocksOnSyntax :: Syntax IO -> (M.Map FilePath Text -> Either Text TestRun) -> [(FilePath, Text)] -> IO (ServerState, IO Int)
blocksOnSyntax syntax = blocksHooked syntax (pure ())

blocksHooked :: Syntax IO -> IO () -> (M.Map FilePath Text -> Either Text TestRun) -> [(FilePath, Text)] -> IO (ServerState, IO Int)
blocksHooked syntax hook verdict = blocksRun syntax hook (const verdict)

-- | 'blocksHooked' whose runner answers by the ids it is asked to run too.
blocksRun :: Syntax IO -> IO () -> ([Text] -> M.Map FilePath Text -> Either Text TestRun) -> [(FilePath, Text)] -> IO (ServerState, IO Int)
blocksRun syntax hook verdict extra = do
  pack <- readTree =<< canonicalizePath "packs/python-htmx"
  (disk, _) <-
    memoryWorkspace $
      [("/pack" </> p, t) | (p, t) <- M.toList pack]
        ++ [ (root </> ".olifant/config.toml", "[packs]\npython-htmx = { path = \"/pack\" }\n")
           , (root </> moduleRel, stubbed)
           , (root </> testRel, testText)
           , (root </> "fills.toml", "[fills]\nbody = \"return int(seats > 2)\"\narrange = \"table = CloseTable()\"\nassertion = \"assert table.handle(2) == 0\"\n") ]
        ++ [(root </> p, t) | (p, t) <- extra]
  runs <- newIORef (0 :: Int)
  let runner = Runner $ \_ ids files -> do
        modifyIORef' runs (+ 1)
        hook
        pure (narrow ids <$> verdict ids files)
      -- as pytest does: tests named by id run alone (a parametrized one's cases too)
      narrow ids run
        | not (null ids), all ("::" `T.isInfixOf`) ids = run {trPassed = filter named (trPassed run), trFailed = filter (named . fst) (trFailed run)}
        | otherwise = run
        where named t = any (\i -> t == i || (i <> "[") `T.isPrefixOf` t) ids
      ports = Ports disk syntax (const noChecker) (const noVerifier) (const runner) (\_ _ -> runner) (pure (UTCTime (fromGregorian 2026 10 12) 0)) Nothing Nothing (DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False) (\_ -> pure Nothing) (\_ -> pure []) (\_ -> pure (Left NoSource))) (\_ -> pure Nothing) (\_ _ _ -> pure (Left "no base in tests")) (\_ _ -> pure (Left "no base in tests")) gitStateStamp
  s <- newServerState ports
  pure (s, readIORef runs)

-- | `function handle`'s body (the lines under its def, deeper than it), and
-- a boundary rewrite (`a > b` to `a >= b`) at every `>`; nothing else.
blockSyntax :: Syntax IO
blockSyntax = Syntax query rewrite (\_ _ -> pure (Left "no provider runner")) noAnchors
  where
    query _ rule src = case T.breakOn "\"^" (TE.decodeUtf8 (BL.toStrict (J.encode rule))) of
      (_, r) | not (T.null r), "$\"" `T.isInfixOf` r -> pure (Right (body (T.takeWhile (/= '$') (T.drop 2 r)) src))
      _ -> pure (Right [])
    rewrite _ rule _ src
      | parseMaybe (J.withObject "rule" (.: "pattern")) rule == Just ("$A > $B" :: Text) = pure (Right (boundaries src))
      | otherwise = pure (Right [])
    indentOf = T.length . T.takeWhile (== ' ')
    -- every def of the name, as ast-grep finds each
    body name src =
      let ls = zip [0 ..] (T.splitOn "\n" src)
       in [ Match (first, 8) (end, T.length l) "" (0, 0)
          | (i, d) <- ls, ("def " <> name <> "(") `T.isInfixOf` d
          , under@((first, _) : _) <- [takeWhile (\(_, l) -> not (T.null (T.strip l)) && indentOf l > indentOf d) (drop (i + 1) ls)]
          , let (end, l) = last under ]
    boundaries src =
      [ Rewrite (i, col) (i, col + T.length w) w (T.replace " > " " >= " w) (offset + col, offset + col + T.length w)
      | (i, l, offset) <- zip3 [0 ..] (T.splitOn "\n" src) (scanl (\o x -> o + T.length x + 1) 0 (T.splitOn "\n" src))
      , " > " `T.isInfixOf` l
      , let (pre, post) = T.breakOn " > " l
            a = T.takeWhileEnd isAlphaNum pre
            w = a <> " > " <> T.takeWhile isAlphaNum (T.drop 3 post)
            col = T.length pre - T.length a
      ]

-- | 'blockSyntax', and the [unit] rule (an `any` of kinds) answered as
-- tree-sitter does for Python: each def or class from its line to its last
-- line deeper than it; a decorated one also from its first decorator, as
-- its `decorated_definition`; a header from its line to its colon. Lines
-- and columns 0-based.
unitSyntax :: Syntax IO
unitSyntax = blockSyntax {querySyntax = query}
  where
    query lang rule src
      | "\"function_definition\"" `T.isInfixOf` encoded rule = pure (Right (units src))
      | otherwise = querySyntax blockSyntax lang rule src
    encoded = TE.decodeUtf8 . BL.toStrict . J.encode
    indentOf = T.length . T.takeWhile (== ' ')
    blank = T.null . T.strip
    units src =
      let ls = zip [0 :: Int ..] (T.splitOn "\n" src)
          lineAt j = maybe "" snd (listToMaybe (drop j ls))
          -- a header over several lines runs to its colon
          colonOf i = fromMaybe i (listToMaybe [j | (j, x) <- drop i ls, ":" `T.isSuffixOf` T.stripEnd (fst (T.breakOn " #" x))])
          endOf i l = last (colonOf i : [j | (j, x) <- takeWhile (\(_, x) -> blank x || indentOf x > indentOf l) (drop (colonOf i + 1) ls), not (blank x)])
          at i j = Match (i, indentOf (lineAt i)) (j, T.length (lineAt j)) "" (0, 0)
          isHead l = any (`T.isPrefixOf` T.strip l) ["def ", "async def ", "class "]
          isDecorator l = "@" `T.isPrefixOf` T.strip l
          heads = [at i (endOf i l) | (i, l) <- ls, isHead l]
          decorated =
            [ at i (endOf d dl)
            | (i, l) <- ls, isDecorator l, i == 0 || not (isDecorator (lineAt (i - 1)))
            , (d, dl) <- take 1 [x | x@(_, y) <- drop (i + 1) ls, not (isDecorator y)], isHead dl ]
       in heads ++ decorated

readTree :: FilePath -> IO (M.Map FilePath Text)
readTree dir = M.fromList <$> go dir
  where
    go d = do
      names <- listDirectory d
      fmap concat . forM names $ \n -> do
        let p = d </> n
        isDir <- doesDirectoryExist p
        if isDir then go p else (\t -> [(makeRelative dir p, t)]) <$> TIO.readFile p

-- | Two roots: /r1 (commands-use-ports off), /r2 (error) holding a nested
-- project /r2/a; and how often a project config was read.
server :: IO (ServerState, IO Int)
server = serverWith pure

serverWith :: (Workspace IO -> IO (Workspace IO)) -> IO (ServerState, IO Int)
serverWith wrap = do
  (disk, _) <-
    memoryWorkspace
      [ ("/r1/.olifant/config.toml", "[rules]\ncommands-use-ports = \"off\"\n")
      , ("/r1/src/x.py", "x = 1\n")
      , ("/r2/.olifant/config.toml", "[rules]\ncommands-use-ports = \"error\"\n")
      , ("/r2/src/z.py", "z = 1\n")
      , ("/r2/a/.olifant/config.toml", "")
      , ("/r2/a/src/y.py", "y = 1\n") ]
  reads <- newIORef (0 :: Int)
  counted <- wrap disk {readText = \p -> (if "/.olifant/config.toml" `isSuffixOf` p then modifyIORef' reads (+ 1) else pure ()) >> readText disk p}
  let ports = Ports counted noSyntax (const noChecker) (const noVerifier) (const noRunner) (\_ _ -> noRunner) (pure (UTCTime (fromGregorian 2026 10 6) 0)) Nothing Nothing (DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False) (\_ -> pure Nothing) (\_ -> pure []) (\_ -> pure (Left NoSource))) (\_ -> pure Nothing) (\_ _ _ -> pure (Left "no base in tests")) (\_ _ -> pure (Left "no base in tests")) gitStateStamp
  s <- newServerState ports
  pure (s, readIORef reads)

portsFiles :: ServerState -> Workspace IO
portsFiles = pFiles . ssPorts

describeSlot :: Maybe ProjectSlot -> String
describeSlot = \case
  Nothing -> "nothing"
  Just (Loaded _) -> "Loaded"
  Just (Broken why p) -> "Broken " <> show why <> maybe " (no project)" (const " (last good)") p

-- | Apply a replacement of a span (0-based line, UTF-16 column) to a text.
applyChange :: ((Int, Int), (Int, Int)) -> Text -> Text -> Text
applyChange ((l1, c1), (l2, c2)) replacement text = before <> replacement <> after
  where
    offset (l, c) = sum [T.length x + 1 | x <- take l (T.splitOn "\n" text)] + c
    before = T.take (offset (l1, c1)) text
    after = T.drop (offset (l2, c2)) text

-- | The shipped Python.
pyNow :: Language
pyNow = shippedLanguageNow "python"
