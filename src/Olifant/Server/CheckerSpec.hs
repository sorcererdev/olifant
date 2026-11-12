{-# LANGUAGE OverloadedStrings #-}
module Olifant.Server.CheckerSpec (spec) where

import Control.Monad (forM, forM_)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.App (Ports (..), Project)
import Olifant.Catalog (PlansConfig (..), defaultPlans)
import Olifant.Doctor (DoctorPorts (..))
import Olifant.Language (Match (..), noChecker, noRunner, querying)
import Olifant.Language.Check (noVerifier)
import Olifant.Language.Grammar (GrammarFailure (NoSource))
import Olifant.Plans (parsePlan)
import Olifant.Plans.Sentences (Sentence (..), sentences)
import Olifant.Process (gitStateStamp, neverCancel)
import Olifant.Server (ServerState, newServerState, projectFor)
import Olifant.Server.State (ServerState (..))
import Olifant.Server.Checker
import Olifant.Server.Errand (RunResult (..))
import Olifant.Server.Triage (Triage (..))
import Olifant.Workspace (diskWorkspace, memoryWorkspace)
import System.Directory (canonicalizePath, createDirectoryIfMissing, createDirectoryLink, doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

one :: Text -> Sentence
one t = case reverse (sentences (parsePlan t)) of
  (s : _) -> s
  [] -> error "no sentence"

spec :: Spec
spec = describe "Olifant.Server.Checker" $ do
  it "a step is checked for progress, anything else as a claim" $ do
    checkModeOf (Triage "step" 0.9 0.9 "jev") `shouldBe` CheckProgress
    checkModeOf (Triage "claim" 0.9 0.9 "jev") `shouldBe` CheckClaim
    map checkModeName [CheckClaim, CheckProgress] `shouldBe` ["verify", "progress"]
  it "fileHash is stable and changes with one character" $ do
    fileHash "a\nb\n" `shouldBe` fileHash "a\nb\n"
    T.length (fileHash "a\nb\n") `shouldBe` 7
    fileHash "a\nb\n" `shouldNotBe` fileHash "a\nc\n"
  it "every verdict has a JSON word that reads back" $
    map (judgementNamed . judgementName) [JRight, JWrong, JCannotTell, JDone, JOpen, JMisfit]
      `shouldBe` map Just [JRight, JWrong, JCannotTell, JDone, JOpen, JMisfit]
  it "fences the plans dir, .olifant and not_evidence" $
    fences defaultPlans {pcDir = "plans", pcNotEvidence = ["docs", "book"]} `shouldBe` ["plans", ".olifant", "docs", "book"]

  it "asks about the sentence in its context, judged as it stands now" $ do
    let s = one (T.unlines ["## Retries", "- ? Do we retry?", "  - The collector retries three times."])
        p = checkPrompt s
    p `shouldSatisfy` T.isInfixOf "Retries"
    p `shouldSatisfy` T.isInfixOf "Do we retry?"
    p `shouldSatisfy` T.isInfixOf "The collector retries three times."
    p `shouldSatisfy` T.isInfixOf "judge what the sentence says is true of the code now; if it also wishes or asks for something, ignore that part"
    p `shouldSatisfy` T.isInfixOf "```json"
    p `shouldSatisfy` T.isInfixOf "right | wrong | cannot_tell"
    p `shouldSatisfy` (not . T.isInfixOf "the writer pointed at")
    checkPrompt (one "- slugify lowercases @src/slug.py:slugify") `shouldSatisfy` T.isInfixOf "the writer pointed at `src/slug.py:slugify`"

  it "reads the last json block, or a bare trailing object" $ do
    let reply v = "{\"verdict\": \"" <> v <> "\", \"why\": \"w\", \"evidence\": [{\"path\": \"src/a.py\", \"unit\": \"Foo\", \"line\": 2}]}"
    fmap crJudgement (parseCheckReply ("First\n```json\n" <> reply "right" <> "\n```\nthen\n```json\n" <> reply "wrong" <> "\n```\n")) `shouldBe` Right JWrong
    fmap crJudgement (parseCheckReply ("I looked.\n" <> reply "cannot_tell")) `shouldBe` Right JCannotTell
    fmap crEvidence (parseCheckReply (reply "right")) `shouldBe` Right [Citation "src/a.py" (Just "Foo") (Just 2)]
    parseCheckReply "It is right, I think." `shouldSatisfy` either (const True) (const False)
    parseCheckReply "{\"verdict\": \"right\", \"why\": \"w\", \"evidence\": []}" `shouldSatisfy` either (const True) (const False)
    fmap crEvidence (parseCheckReply "{\"verdict\": \"wrong\", \"why\": \"w\", \"evidence\": [{\"path\": \"src/a.py:3\"}]}") `shouldBe` Right [Citation "src/a.py" Nothing (Just 3)]

  it "fenced or missing citations cannot tell" $ do
    (s, proj) <- checkerServer
    let fs = fences defaultPlans
        reply cs = CheckReply JRight "it does" cs
        notFound c = do
          r <- confirm s proj fs (reply [c])
          (cfJudgement r, cfWhy r) `shouldBe` (JCannotTell, "cited code not found")
    notFound (Citation "plans/t.md" Nothing (Just 1))
    notFound (Citation "docs/table.txt" Nothing Nothing)
    notFound (Citation "../a.py" (Just "Foo") Nothing)
    notFound (Citation "src/nope.py" (Just "Foo") Nothing)
    notFound (Citation "src/a.py" (Just "Gone") Nothing)
    -- a unit named but not in the file is a guess, whatever its line
    notFound (Citation "src/a.py" (Just "Nope") (Just 3))
    good <- confirm s proj fs (reply [Citation "src/a.py" (Just "Foo.bar") Nothing])
    (cfJudgement good, cfUnit good, fmap T.length (cfFingerprint good)) `shouldBe` (JRight, Just (UnitRef "src/a.py" "Foo.bar"), Just 7)
    byLine <- confirm s proj fs (reply [Citation "/r1/src/a.py" Nothing (Just 3)])
    (cfJudgement byLine, cfUnit byLine) `shouldBe` (JRight, Just (UnitRef "src/a.py" "Foo.bar"))
    file <- confirm s proj fs (reply [Citation "notes.txt" Nothing Nothing])
    (cfJudgement file, cfUnit file) `shouldBe` (JRight, Nothing)

  it "a citation through a symlink into a fence or out of the root cannot tell" $
    withSystemTempDirectory "olifant-checker" $ \tmp0 -> do
      tmp <- canonicalizePath tmp0
      pack <- canonicalizePath "packs/python-htmx"
      let root = tmp </> "r"
          put rel t = createDirectoryIfMissing True (takeDirectory (root </> rel)) >> TIO.writeFile (root </> rel) t
          foo = "class Foo:\n    def bar(self):\n        return 1\n"
      put ".olifant/config.toml" ("[packs]\npython-htmx = { path = \"" <> T.pack pack <> "\" }\n")
      put "src/a.py" foo
      put "plans/a.py" foo
      createDirectoryIfMissing True (tmp </> "other")
      TIO.writeFile (tmp </> "other/a.py") foo
      createDirectoryLink (root </> "plans") (root </> "notes")
      createDirectoryLink (tmp </> "other") (root </> "out")
      (s0, _) <- checkerServer
      s1 <- newServerState (ssPorts s0) {pFiles = diskWorkspace "/"}
      proj <- projectFor (root </> "plans/t.md") s1 >>= \case
        (_, Just (_, p)) -> pure p
        _ -> fail "no project"
      let reply cs = CheckReply JRight "it does" cs
      forM_ [Citation "notes/a.py" (Just "Foo") Nothing, Citation "out/a.py" (Just "Foo") Nothing, Citation (root </> "out/a.py") Nothing (Just 1)] $ \c -> do
        r <- confirm s1 proj (fences defaultPlans) (reply [c])
        (cfJudgement r, cfWhy r) `shouldBe` (JCannotTell, "cited code not found")
      good <- confirm s1 proj (fences defaultPlans) (reply [Citation "src/a.py" (Just "Foo") Nothing])
      cfJudgement good `shouldBe` JRight

  it "runs the checker through the runner" $ do
    d <- getCurrentDirectory
    let cfg = defaultPlans {pcRunner = d </> "src/Olifant/Server/testdata/claude-shim.sh"}
    r <- liveChecker neverCancel cfg "/tmp" (one "- SHIM:CHECK the collector retries")
    fmap rrText r `shouldBe` Right "{\"verdict\": \"right\", \"why\": \"shim\", \"evidence\": [{\"path\": \"src/a.py\", \"unit\": \"Foo\"}]}"

  it "replays recorded replies by sentence key; a missing key is a failure" $
    withSystemTempDirectory "olifant-checker" $ \dir -> do
      let file = dir </> "checks.json"
      writeFile file "{\"replies\": {\"The collector retries.\": {\"text\": \"x\", \"cost\": 0.1}}}"
      r <- recordedChecker file (one "- The collector retries.")
      (fmap rrText r, fmap rrCost r) `shouldBe` (Right "x", Right (Just 0.1))
      m <- recordedChecker file (one "- Something else.")
      either (const True) (const False) m `shouldBe` True

-- | /r1 with the python pack, `src/a.py` (class Foo with def bar), a plan,
-- docs and a file outside the root; units Foo (lines 1-3) and Foo.bar (2-3).
checkerServer :: IO (ServerState, Project)
checkerServer = do
  pack <- readTree =<< canonicalizePath "packs/python-htmx"
  (disk, _) <-
    memoryWorkspace $
      [("/pack" </> p, t) | (p, t) <- M.toList pack]
        ++ [ ("/r1/.olifant/config.toml", "[packs]\npython-htmx = { path = \"/pack\" }\n")
           , ("/r1/src/a.py", "class Foo:\n    def bar(self):\n        return 1\n")
           , ("/r1/notes.txt", "x\n")
           , ("/r1/docs/table.txt", "t\n")
           , ("/a.py", "class Foo:\n    def bar(self):\n        return 1\n")
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
