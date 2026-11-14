{-# LANGUAGE OverloadedStrings #-}
module Olifant.Server.CheckerSpec (spec, checkerServer) where

import Control.Monad (forM, forM_)
import Data.List (isPrefixOf)
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
import Olifant.Server.Errand (Belief (..), BeliefCheck (..), RunResult (..))
import Olifant.Server.Triage (Triage (..))
import Olifant.Workspace (diskWorkspace, memoryWorkspace)
import System.Directory (canonicalizePath, createDirectoryIfMissing, createDirectoryLink, doesDirectoryExist, getCurrentDirectory, getPermissions, listDirectory, setOwnerExecutable, setPermissions)
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

one :: Text -> Sentence
one t = case reverse (sentences (parsePlan t)) of
  (s : _) -> s
  [] -> error "no sentence"

spec :: Spec
spec = describe "Olifant.Server.Checker" $ do
  it "a step is checked for progress, a question answered, anything else as a claim" $ do
    checkModeOf (Triage "step" 0.9 0.9 "jev" Nothing Nothing) `shouldBe` CheckProgress
    checkModeOf (Triage "claim" 0.9 0.9 "jev" Nothing Nothing) `shouldBe` CheckClaim
    checkModeOf (Triage "question" 0.9 0.9 "jev" (Just 0.9) Nothing) `shouldBe` CheckAnswer
    map checkModeName [CheckClaim, CheckProgress, CheckAnswer] `shouldBe` ["verify", "progress", "answer"]
  it "fileHash is stable and changes with one character" $ do
    fileHash "a\nb\n" `shouldBe` fileHash "a\nb\n"
    T.length (fileHash "a\nb\n") `shouldBe` 7
    fileHash "a\nb\n" `shouldNotBe` fileHash "a\nc\n"
  it "every verdict has a JSON word that reads back" $
    map (judgementNamed . judgementName) [JRight, JWrong, JCannotTell, JDone, JOpen, JMisfit, JAnswered]
      `shouldBe` map Just [JRight, JWrong, JCannotTell, JDone, JOpen, JMisfit, JAnswered]
  it "fences the plans dir, .olifant and not_evidence" $
    fences defaultPlans {pcDir = "plans", pcNotEvidence = ["docs", "book"]} `shouldBe` ["plans", ".olifant", "docs", "book"]

  it "asks about the sentence in its context, judged as it stands now" $ do
    let s = one (T.unlines ["## Retries", "- ? Do we retry?", "  - The collector retries three times."])
        p = checkPrompt (CheckAsk CheckClaim False []) s
    p `shouldSatisfy` T.isInfixOf "Retries"
    p `shouldSatisfy` T.isInfixOf "Do we retry?"
    p `shouldSatisfy` T.isInfixOf "The collector retries three times."
    p `shouldSatisfy` T.isInfixOf "judge what the sentence says is true of the code now; if it also wishes or asks for something, ignore that part"
    p `shouldSatisfy` T.isInfixOf "```json"
    p `shouldSatisfy` T.isInfixOf "right | wrong | cannot_tell"
    p `shouldSatisfy` (not . T.isInfixOf "the writer pointed at")
    checkPrompt (CheckAsk CheckClaim False []) (one "- slugify lowercases @src/slug.py:slugify") `shouldSatisfy` T.isInfixOf "the writer pointed at `src/slug.py:slugify`"

  it "asks for an answer: the question, the beliefs under it, three sentences (2b)" $ do
    let s = one (T.unlines ["## Retries", "- ? Do we retry?"])
        p = checkPrompt (CheckAsk CheckAnswer False [Belief "it is kept" NotChecked, Belief "it retries" (BelievedRight "src/a.py:Foo")]) s
    p `shouldSatisfy` T.isInfixOf "Question: Do we retry?"
    p `shouldSatisfy` T.isInfixOf "answered | cannot_tell"
    p `shouldSatisfy` T.isInfixOf "three sentences"
    p `shouldSatisfy` T.isInfixOf "THE WRITER'S BELIEFS UNDER IT:"
    p `shouldSatisfy` T.isInfixOf "it is kept — not checked"
    p `shouldSatisfy` T.isInfixOf "it retries — right, about src/a.py:Foo"
    p `shouldSatisfy` T.isInfixOf "If neither the code nor a public source settles it"
    p `shouldSatisfy` (not . T.isInfixOf "WebSearch")
    p `shouldSatisfy` (not . T.isInfixOf "\"sources\"")
    checkPrompt (CheckAsk CheckAnswer False []) s `shouldSatisfy` (not . T.isInfixOf "BELIEFS")
    let w = checkPromptWith ["docs.djangoproject.com"] (CheckAsk CheckAnswer True []) s
    w `shouldSatisfy` T.isInfixOf "WebSearch"
    w `shouldSatisfy` T.isInfixOf "read pages on docs.djangoproject.com (WebFetch)"
    w `shouldSatisfy` T.isInfixOf "\"sources\""
    checkPrompt (CheckAsk CheckAnswer True []) s `shouldSatisfy` (not . T.isInfixOf "WebFetch")

  it "a web check's prompt holds the sentence and its heading, and nothing of the repository (user ruling 2026-11-14)" $ do
    let s = one (T.unlines ["## Stack", "Lead line about the app.", "- Django 5.1 added the querystring tag. @src/a.py:Foo"])
        q = one (T.unlines ["## Stack", "- ? Does Django 5.2 support composite primary keys?"])
        bel = [Belief "We use a surrogate key." (BelievedRight "src/a.py:Foo")]
    forM_ [checkPrompt (CheckAsk CheckClaim True []) s, checkPrompt (CheckAsk CheckAnswer True bel) q] $ \w -> do
      w `shouldSatisfy` T.isInfixOf "Section: Stack"
      w `shouldSatisfy` T.isInfixOf "WebSearch"
      w `shouldSatisfy` T.isInfixOf "\"sources\""
      forM_ ["repository", "current directory", "Read, Grep", "\"evidence\"", "Lead", "src/a.py", "surrogate", "BELIEFS"] $ \x ->
        w `shouldSatisfy` (not . T.isInfixOf x)
    checkPrompt (CheckAsk CheckClaim True []) s `shouldSatisfy` T.isInfixOf "Sentence: Django 5.1 added the querystring tag."
    checkPrompt (CheckAsk CheckAnswer True bel) q `shouldSatisfy` T.isInfixOf "Question: Does Django 5.2 support composite primary keys?"

  it "the claim and progress prompts are 2a's byte for byte without web, and name the web with it" $ do
    let c = one "- The collector retries."
        old = T.unlines
          [ "Check one sentence of a software plan against this repository (the current directory)."
          , ""
          , "Section: (none)"
          , "Sentence: The collector retries."
          , ""
          , "Use Read, Grep and Glob to find the code the sentence is about, and read it."
          , "Behaviour can live in any layer: server code, templates, and front-end scripts and styles. Before you conclude that something does not exist, search for it across the whole repository."
          , "Rule: judge what the sentence says is true of the code now; if it also wishes or asks for something, ignore that part."
          , "If the code cannot settle it (it is about people, documents or outside services), the verdict is cannot_tell."
          , ""
          , "Answer with one fenced ```json block holding one object, after any notes:"
          , "```json"
          , "{\"verdict\": \"right | wrong | cannot_tell\", \"why\": \"<one sentence>\", \"evidence\": [{\"path\": \"<path relative to the current directory>\", \"unit\": \"<function or class name, or null>\", \"line\": <line number, or null>}]}"
          , "```"
          , "For right or wrong, give at least one evidence entry: the code the verdict rests on."
          , "In evidence, \"unit\" is the exact name of one function, class or method as the file defines it (Class.method for a method), or null. For anything else (a template, a heading, part of a function, several names) give null and the line. A unit the file does not define voids the verdict." ]
    checkPrompt (CheckAsk CheckClaim False []) c `shouldBe` old
    let w = checkPrompt (CheckAsk CheckClaim True []) c
    w `shouldSatisfy` T.isInfixOf "WebSearch"
    w `shouldSatisfy` T.isInfixOf "\"sources\""
    let st = one "- [ ] move the hint"
    checkPrompt (CheckAsk CheckProgress True []) st `shouldBe` checkPrompt (CheckAsk CheckProgress False []) st

  it "reads an answer: answered needs text and evidence or (with web) a source; only http(s) sources" $ do
    let ans v a ev src = "```json\n{\"verdict\": \"" <> v <> "\", \"answer\": \"" <> a <> "\", \"detail\": \"more\", \"why\": \"w\", \"evidence\": [" <> ev <> "], \"sources\": [" <> src <> "]}\n```"
        cite = "{\"path\": \"src/a.py\", \"unit\": \"Foo\"}"
        page = "{\"url\": \"https://docs.djangoproject.com/en/5.2/ref/\", \"title\": \"QuerySet API\"}"
        answer = CheckAsk CheckAnswer False []
        answerWeb = CheckAsk CheckAnswer True []
    fmap (\r -> (crJudgement r, crAnswer r, crDetail r)) (parseCheckReply answer (ans "answered" "Yes, three times." cite "")) `shouldBe` Right (JAnswered, Just "Yes, three times.", Just "more")
    parseCheckReply answer (ans "answered" " " cite "") `shouldBe` Left "an answer with no text"
    fmap crSources (parseCheckReply answerWeb (ans "answered" "Yes." "" page)) `shouldBe` Right [Source "https://docs.djangoproject.com/en/5.2/ref/" "QuerySet API"]
    parseCheckReply answer (ans "answered" "Yes." "" page) `shouldBe` Left "a verdict with no evidence"
    parseCheckReply answer (ans "right" "Yes." cite "") `shouldSatisfy` either (const True) (const False)
    fmap crJudgement (parseCheckReply answer (ans "cannot_tell" "" "" "")) `shouldBe` Right JCannotTell
    -- a claim resting only on a page, with web
    fmap crJudgement (parseCheckReply (CheckAsk CheckClaim True []) (ans "right" "" "" page)) `shouldBe` Right JRight
    parseCheckReply (CheckAsk CheckClaim False []) (ans "right" "" "" page) `shouldSatisfy` either (const True) (const False)
    fmap crAnswer (parseCheckReply (CheckAsk CheckClaim True []) (ans "right" "x" cite page)) `shouldBe` Right Nothing
    -- a web check rests on pages only: its code citations are dropped, and none is no source
    fmap crEvidence (parseCheckReply (CheckAsk CheckClaim True []) (ans "right" "" cite page)) `shouldBe` Right []
    parseCheckReply (CheckAsk CheckClaim True []) (ans "right" "" cite "") `shouldBe` Left "a verdict with no source"
    parseCheckReply answerWeb (ans "answered" "Yes." cite "") `shouldBe` Left "a verdict with no source"

  it "a source that is not an http(s) address is dropped" $ do
    let srcs = T.intercalate ", " ["{\"url\": \"" <> u <> "\", \"title\": \"t\"}" | u <- ["javascript:alert(1)", "file:///etc/passwd", "src/a.py", "https://", "http://ok.example.org/a"]]
        reply = "{\"verdict\": \"answered\", \"answer\": \"a\", \"why\": \"w\", \"evidence\": [], \"sources\": [" <> srcs <> "]}"
    fmap (map soUrl . crSources) (parseCheckReply (CheckAsk CheckAnswer True []) reply) `shouldBe` Right ["http://ok.example.org/a"]
    let many = T.intercalate ", " ["{\"url\": \"https://x.org/" <> T.pack (show n) <> "\", \"title\": \"t\"}" | n <- [1 .. 7 :: Int]]
    fmap (length . crSources) (parseCheckReply (CheckAsk CheckAnswer True []) ("{\"verdict\": \"answered\", \"answer\": \"a\", \"why\": \"w\", \"evidence\": [], \"sources\": [" <> many <> "]}")) `shouldBe` Right 5

  it "sourceDomain is the host without www." $ do
    sourceDomain "https://www.djangoproject.com/x" `shouldBe` "djangoproject.com"
    sourceDomain "https://docs.djangoproject.com:443/en/?q=1" `shouldBe` "docs.djangoproject.com"
    sourceDomain "http://HTMX.org" `shouldBe` "htmx.org"

  it "confirms an answer: a missing citation makes it cannot_tell with no answer; a source alone passes through" $ do
    (s, proj) <- checkerServer
    let fs = fences defaultPlans
        src = [Source "https://htmx.org/x" "hx-on"]
    missing <- confirm s proj fs (CheckReply JAnswered "w" [Citation "src/nope.py" (Just "Foo") Nothing] (Just "yes") (Just "d") src)
    (cfJudgement missing, cfWhy missing, cfAnswer missing, cfSources missing) `shouldBe` (JCannotTell, "cited code not found", Nothing, [])
    onlySource <- confirm s proj fs (CheckReply JAnswered "w" [] (Just "yes") Nothing src)
    (cfJudgement onlySource, cfAnswer onlySource, cfSources onlySource, cfPrints onlySource) `shouldBe` (JAnswered, Just "yes", src, [])
    both <- confirm s proj fs (CheckReply JAnswered "w" [Citation "src/a.py" (Just "Foo") Nothing] (Just "yes") (Just "d") src)
    (cfJudgement both, cfAnswer both, cfDetail both, cfSources both, length (cfPrints both)) `shouldBe` (JAnswered, Just "yes", Just "d", src, 1)

  it "asks about a step's progress: the whole step, whether it is ticked, four words" $ do
    let s = one (T.unlines ["## Hints", "Hints move the text.", "- [ ] move the hint @src/a.py:Foo", "  so it does not shift"])
        p = checkPrompt (CheckAsk CheckProgress False []) s
    p `shouldSatisfy` T.isInfixOf "Hints"
    p `shouldSatisfy` T.isInfixOf "Hints move the text."
    p `shouldSatisfy` T.isInfixOf "move the hint so it does not shift"
    p `shouldSatisfy` T.isInfixOf "The step is not ticked"
    p `shouldSatisfy` T.isInfixOf "the writer pointed at"
    p `shouldSatisfy` T.isInfixOf "done | open | misfit | cannot_tell"
    checkPrompt (CheckAsk CheckProgress False []) (one "- [x] move the hint") `shouldSatisfy` T.isInfixOf "The writer ticked this step"

  it "reads a progress reply: done and misfit need evidence, open does not, claim words are refused" $ do
    let reply v ev = "{\"verdict\": \"" <> v <> "\", \"why\": \"w\", \"evidence\": [" <> ev <> "]}"
        cite = "{\"path\": \"src/a.py\", \"unit\": \"Foo\"}"
    parseCheckReply (CheckAsk CheckProgress False []) (reply "done" "") `shouldBe` Left "a verdict with no evidence"
    parseCheckReply (CheckAsk CheckProgress False []) (reply "misfit" "") `shouldBe` Left "a verdict with no evidence"
    fmap crJudgement (parseCheckReply (CheckAsk CheckProgress False []) (reply "open" "")) `shouldBe` Right JOpen
    fmap crJudgement (parseCheckReply (CheckAsk CheckProgress False []) (reply "done" cite)) `shouldBe` Right JDone
    parseCheckReply (CheckAsk CheckProgress False []) (reply "right" cite) `shouldSatisfy` either (const True) (const False)
    parseCheckReply (CheckAsk CheckClaim False []) (reply "done" cite) `shouldSatisfy` either (const True) (const False)
    fmap crJudgement (parseCheckReply (CheckAsk CheckProgress False []) (reply "cannot_tell" "")) `shouldBe` Right JCannotTell
    fmap crJudgement (parseCheckReply (CheckAsk CheckClaim False []) (reply "cannot_tell" "")) `shouldBe` Right JCannotTell

  it "confirms a progress verdict: done on missing code cannot tell, open drops it; a print per citation" $ do
    (s, proj) <- checkerServer
    let fs = fences defaultPlans
    doneMissing <- confirm s proj fs (CheckReply JDone "it does" [Citation "src/nope.py" (Just "Foo") Nothing] Nothing Nothing [])
    (cfJudgement doneMissing, cfWhy doneMissing) `shouldBe` (JCannotTell, "cited code not found")
    doneFenced <- confirm s proj fs (CheckReply JDone "it does" [Citation "plans/t.md" Nothing Nothing] Nothing Nothing [])
    cfJudgement doneFenced `shouldBe` JCannotTell
    open <- confirm s proj fs (CheckReply JOpen "not yet" [Citation "src/nope.py" Nothing Nothing, Citation "src/a.py" (Just "Foo") Nothing] Nothing Nothing [])
    cfJudgement open `shouldBe` JOpen
    cfEvidence open `shouldBe` [Citation "src/a.py" (Just "Foo") Nothing]
    map ptPath (cfPrints open) `shouldBe` ["src/a.py"]
    c <- confirm s proj fs (CheckReply JDone "it does" [Citation "src/a.py" (Just "Foo") Nothing, Citation "src/a.py" Nothing (Just 3), Citation "notes.txt" Nothing Nothing, Citation "notes.txt" Nothing (Just 1)] Nothing Nothing [])
    cfJudgement c `shouldBe` JDone
    cfUnit c `shouldBe` Just (UnitRef "src/a.py" "Foo")
    map (\x -> (ptPath x, ptUnit x)) (cfPrints c) `shouldBe` [("src/a.py", Just "Foo"), ("src/a.py", Just "Foo.bar"), ("notes.txt", Nothing), ("notes.txt", Nothing)]
    map ptHash (drop 2 (cfPrints c)) `shouldBe` [fileHash "x\n", fileHash "x\n"]
    Just (ptHash (head (cfPrints c))) `shouldBe` cfFingerprint c

  it "reads the last json block, or a bare trailing object" $ do
    let reply v = "{\"verdict\": \"" <> v <> "\", \"why\": \"w\", \"evidence\": [{\"path\": \"src/a.py\", \"unit\": \"Foo\", \"line\": 2}]}"
    fmap crJudgement (parseCheckReply (CheckAsk CheckClaim False []) ("First\n```json\n" <> reply "right" <> "\n```\nthen\n```json\n" <> reply "wrong" <> "\n```\n")) `shouldBe` Right JWrong
    fmap crJudgement (parseCheckReply (CheckAsk CheckClaim False []) ("I looked.\n" <> reply "cannot_tell")) `shouldBe` Right JCannotTell
    fmap crEvidence (parseCheckReply (CheckAsk CheckClaim False []) (reply "right")) `shouldBe` Right [Citation "src/a.py" (Just "Foo") (Just 2)]
    parseCheckReply (CheckAsk CheckClaim False []) "It is right, I think." `shouldSatisfy` either (const True) (const False)
    parseCheckReply (CheckAsk CheckClaim False []) "{\"verdict\": \"right\", \"why\": \"w\", \"evidence\": []}" `shouldSatisfy` either (const True) (const False)
    fmap crEvidence (parseCheckReply (CheckAsk CheckClaim False []) "{\"verdict\": \"wrong\", \"why\": \"w\", \"evidence\": [{\"path\": \"src/a.py:3\"}]}") `shouldBe` Right [Citation "src/a.py" Nothing (Just 3)]

  it "fenced or missing citations cannot tell" $ do
    (s, proj) <- checkerServer
    let fs = fences defaultPlans
        reply cs = CheckReply JRight "it does" cs Nothing Nothing []
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

  it ".env files are fenced with web on" $ do
    -- a secret is never evidence, in any mode, with or without web
    (s, proj) <- checkerServer
    forM_ [JRight, JWrong, JDone, JMisfit] $ \j ->
      forM_ [Citation ".env" Nothing Nothing, Citation "sub/.env.local" Nothing (Just 1), Citation "/r1/.env" Nothing Nothing] $ \c -> do
        r <- confirm s proj (fences defaultPlans) (CheckReply j "it does" [c] Nothing Nothing [])
        (cfJudgement r, cfWhy r) `shouldBe` (JCannotTell, "cited code not found")

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
      let reply cs = CheckReply JRight "it does" cs Nothing Nothing []
      forM_ [Citation "notes/a.py" (Just "Foo") Nothing, Citation "out/a.py" (Just "Foo") Nothing, Citation (root </> "out/a.py") Nothing (Just 1)] $ \c -> do
        r <- confirm s1 proj (fences defaultPlans) (reply [c])
        (cfJudgement r, cfWhy r) `shouldBe` (JCannotTell, "cited code not found")
      good <- confirm s1 proj (fences defaultPlans) (reply [Citation "src/a.py" (Just "Foo") Nothing])
      cfJudgement good `shouldBe` JRight

  it "runs the checker through the runner" $ do
    d <- getCurrentDirectory
    let cfg = defaultPlans {pcRunner = d </> "src/Olifant/Server/testdata/claude-shim.sh"}
    r <- liveChecker neverCancel cfg "/tmp" (CheckAsk CheckClaim False []) (one "- SHIM:CHECK the collector retries")
    fmap rrText r `shouldBe` Right "{\"verdict\": \"right\", \"why\": \"shim\", \"evidence\": [{\"path\": \"src/a.py\", \"unit\": \"Foo\"}]}"

  it "runs a web check outside the repository, with no file tools (user ruling 2026-11-14)" $
    withSystemTempDirectory "olifant-webcheck" $ \dir -> do
      let runner = dir </> "runner.sh"
          root = dir </> "repo"
      createDirectoryIfMissing True root
      writeFile runner $ unlines
        [ "#!/bin/sh"
        , "cat > /dev/null"
        , "pwd -P > " <> show (dir </> "pwd")
        , "printf '%s\\n' \"$@\" > " <> show (dir </> "args")
        , "echo '{\"result\":\"ok\",\"session_id\":\"s\"}'" ]
      p <- getPermissions runner
      setPermissions runner (setOwnerExecutable True p)
      let cfg = defaultPlans {pcRunner = runner}
          ran = do
            cwd <- filter (/= '\n') <$> readFile (dir </> "pwd")
            as <- lines <$> readFile (dir </> "args")
            pure (cwd, as)
      rootP <- canonicalizePath root
      _ <- liveChecker neverCancel cfg root (CheckAsk CheckClaim True []) (one "- Django 5.1 added the querystring tag.")
      (cwd, webArgs) <- ran
      cwd `shouldNotBe` rootP
      (rootP `isPrefixOf` cwd) `shouldBe` False
      filter (`elem` ["Read", "Grep", "Glob"]) (concatMap (map T.unpack . T.splitOn "," . T.pack) webArgs) `shouldBe` []
      _ <- liveChecker neverCancel cfg root (CheckAsk CheckClaim False []) (one "- The collector retries.")
      (cwd', codeArgs) <- ran
      cwd' `shouldBe` rootP
      filter (\a -> "WebSearch" `isPrefixOf` a || "WebFetch" `isPrefixOf` a) (concatMap (map T.unpack . T.splitOn "," . T.pack) codeArgs) `shouldBe` []

  it "replays recorded replies by sentence key; a missing key is a failure" $
    withSystemTempDirectory "olifant-checker" $ \dir -> do
      let file = dir </> "checks.json"
      writeFile file "{\"replies\": {\"The collector retries.\": {\"text\": \"x\", \"cost\": 0.1}}}"
      r <- recordedChecker file (CheckAsk CheckClaim False []) (one "- The collector retries.")
      (fmap rrText r, fmap rrCost r) `shouldBe` (Right "x", Right (Just 0.1))
      m <- recordedChecker file (CheckAsk CheckClaim False []) (one "- Something else.")
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
           , ("/r1/.env", "SECRET=x\n")
           , ("/r1/sub/.env.local", "LOCAL_SECRET=y\n")
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
