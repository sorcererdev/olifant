module Olifant.Inspect.PassSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.Key qualified as K
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, utc)
import Data.Time.Format.ISO8601 (iso8601Show)
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..))
import Olifant.Decide (recordedDecider)
import Olifant.LspSpec (withPython)
import Olifant.Process (neverCancel)
import Olifant.Server.Checker (recordedChecker)
import Olifant.Server.State (ServerState (..), newServerState, projectFor)
import Olifant.Server.Unit (fingerprint)
import Olifant.Inspect.Pass
import Olifant.Decide (Decider (..), Question (..))
import Olifant.Server.Errand (RunFailure (..))
import Olifant.Server.Errand qualified as E
import Olifant.Inspect.Find (recordedFinder)
import Olifant.Inspect.Store
import Olifant.Workspace (Workspace)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Process (callProcess)
import Test.Hspec

-- | The fixture file inspect looks at: f and g.
baseText :: Text
baseText = "def f(x):\n    return x + 1\n\n\ndef g(y):\n    return y * 2\n"

file :: FilePath
file = "src/inspectme.py"

git :: FilePath -> [String] -> IO ()
git dir args = callProcess "git" (["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t"] ++ args)

-- | Recorded Jev answers by kind: yes or no, at 0.9.
answers :: [(Text, Bool)] -> Text
answers kvs = TE.decodeUtf8 (BL.toStrict (encode (object ["answers" .= object [K.fromText k .= object ["choice" .= b, "confidence" .= (0.9 :: Double)] | (k, b) <- kvs]])))

-- | Recorded replies by key.
replies :: [(Text, Value)] -> Text
replies kvs = TE.decodeUtf8 (BL.toStrict (encode (object ["replies" .= object [K.fromText k .= v | (k, v) <- kvs]])))

-- | A find reply with findings in other files: (file, (kind, lines, statement, rule, evidence lines)).
findReplyIn :: [(FilePath, (Text, (Int, Int), Text, Text, (Int, Int)))] -> Value
findReplyIn fs =
  String . TE.decodeUtf8 . BL.toStrict . encode $
    object ["findings" .= [object ["kind" .= k, "file" .= f, "lines" .= [a, z], "statement" .= st, "rule" .= r, "evidence" .= [object ["file" .= f, "lines" .= [ea, ez]]]] | (f, (k, (a, z), st, r, (ea, ez))) <- fs], "open" .= ([] :: [Value])]

-- | A find reply with findings: (kind, lines, statement, rule, evidence lines in the file).
findReply :: [(Text, (Int, Int), Text, Text, (Int, Int))] -> [(Text, Text)] -> Value
findReply fs opens =
  String . TE.decodeUtf8 . BL.toStrict . encode $
    object
      [ "findings" .= [object ["kind" .= k, "file" .= file, "lines" .= [a, z], "statement" .= st, "rule" .= r, "evidence" .= [object ["file" .= file, "lines" .= [ea, ez]]]] | (k, (a, z), st, r, (ea, ez)) <- fs]
      , "open" .= [object ["id" .= i, "holds" .= h, "why" .= ("x" :: Text)] | (i, h) <- opens]
      ]

checkReply :: Text -> Value
checkReply verdict = object ["cost" .= (0.01 :: Double), "text" .= ("```json\n{\"verdict\": \"" <> verdict <> "\", \"why\": \"w\", \"evidence\": [{\"path\": \"src/inspectme.py\", \"line\": 1}]}\n```" :: Text)]

data Env = Env {eDir :: FilePath, eAnswers :: FilePath, eFinds :: FilePath, eChecks :: FilePath, ePassWith :: (InspectDeps -> InspectDeps) -> FilePath -> IO InspectOutcome, eWs :: Workspace IO, eStore :: IO InspectStore}

ePass :: Env -> IO InspectOutcome
ePass e = ePassOn e (eDir e </> file)

ePassOn :: Env -> FilePath -> IO InspectOutcome
ePassOn e = ePassWith e id

-- | A committed fixture with inspect on (Jev "configured", recorded), the file
-- then edited; CONFIG is appended to config.toml.
withInspect :: Text -> Text -> (Env -> IO a) -> IO a
withInspect config edited act =
  withPython [(file, baseText)] $ \ports dir -> do
    TIO.writeFile (dir </> ".olifant/inspect.md") "## Rules\n### A\nNo twos.\n"
    old <- TIO.readFile (dir </> ".olifant/config.toml")
    TIO.writeFile (dir </> ".olifant/config.toml") (old <> "[plans]\ncheck_model = \"sonnet\"\n[inspect]\nenabled = true\n" <> config)
    git dir ["init", "-q", "-b", "main"] >> git dir ["add", "."] >> git dir ["commit", "-q", "-m", "base"]
    TIO.writeFile (dir </> file) edited
    let env =
          Env
            { eDir = dir
            , eAnswers = dir </> "answers.json"
            , eFinds = dir </> "finds.json"
            , eChecks = dir </> "checks.json"
            , ePassWith = \tweak path -> do
                s0 <- newServerState ports
                projectFor path s0 >>= \case
                  (s, Just (_, p)) -> do
                    let deps = InspectDeps (recordedDecider (dir </> "answers.json") neverCancel) (recordedFinder (dir </> "finds.json")) (const (recordedChecker (dir </> "checks.json"))) utc S.empty (const (pure ()))
                    inspectPass neverCancel s p (tweak deps) path
                  _ -> fail "no project"
            , eWs = pFiles ports
            , eStore = either (error . T.unpack) id <$> readInspectStore (pFiles ports) dir
            }
    TIO.writeFile (eAnswers env) (answers [("stops-early", False), ("nitpick", False), ("same", True)])
    TIO.writeFile (eFinds env) (replies [])
    TIO.writeFile (eChecks env) (replies [])
    act env

edit1 :: Text
edit1 = "def f(x):\n    return x + 2\n\n\ndef g(y):\n    return y * 2\n"

edit2 :: Text
edit2 = "def f(x):\n    return x + 22\n\n\ndef g(y):\n    return y * 2\n"

conv :: (Text, (Int, Int), Text, Text, (Int, Int))
conv = ("convention", (2, 2), "f adds two.", "A", (1, 2))

asked :: FilePath -> IO [Text]
asked f = doesFileExist (f <> ".asked") >>= \there -> if there then T.lines <$> TIO.readFile (f <> ".asked") else pure []

statuses :: InspectStore -> [Status]
statuses = map vfStatus . vsFindings

spec :: Spec
spec = describe "Olifant.Inspect.Pass" $ do
  it "the cap message is due once a local day" $ do
    capNoticeDue Nothing (fromGregorian 2026 11 15) `shouldBe` True
    capNoticeDue (Just (fromGregorian 2026 11 15)) (fromGregorian 2026 11 15) `shouldBe` False
    capNoticeDue (Just (fromGregorian 2026 11 14)) (fromGregorian 2026 11 15) `shouldBe` True
  it "in-flight units are left out, and the find call is announced" $ withInspect "" edit1 $ \e -> do
    TIO.writeFile (eFinds e) (replies [("src/inspectme.py:f", findReply [conv] [])])
    sent <- newIORef []
    _ <- ePassWith e (\d -> d {vdSkip = S.fromList ["f"]}) (eDir e </> file)
    asked (eFinds e) >>= (`shouldBe` [])
    _ <- ePassWith e (\d -> d {vdOnFind = \xs -> modifyIORef' sent (++ xs)}) (eDir e </> file)
    readIORef sent >>= (`shouldBe` [("f", fingerprint edit1 (1, 2))])
  it "inspects a changed unit" $ withInspect "" edit1 $ \e -> do
    TIO.writeFile (eFinds e) (replies [("src/inspectme.py:f", findReply [conv] [])])
    o <- ePass e
    o `shouldSatisfy` \case InspectDone {} -> True; _ -> False
    vs <- eStore e
    statuses vs `shouldBe` [Open]
    map (null . vfPrints) (vsFindings vs) `shouldBe` [False]
    M.lookup file (vsInspectted vs) `shouldBe` Just (S.fromList [fingerprint edit1 (1, 2)])
  it "an unchanged unit is not inspected" $ withInspect "" edit1 $ \e -> do
    _ <- ePass e
    asked (eFinds e) >>= (`shouldBe` ["src/inspectme.py:f"])
  it "untracked file: every unit changed" $ withInspect "" baseText $ \e -> do
    TIO.writeFile (eDir e </> "src/new.py") "def a():\n    return 1\n\n\ndef b():\n    return 2\n"
    _ <- ePassOn e (eDir e </> "src/new.py")
    asked (eFinds e) >>= (`shouldBe` ["src/new.py:a,src/new.py:b"])
  it "a repository with no commit: every unit changed" $ withInspect "" edit1 $ \e -> do
    removeDirectoryRecursive (eDir e </> ".git")
    git (eDir e) ["init", "-q", "-b", "main"]
    _ <- ePass e
    asked (eFinds e) >>= (`shouldBe` ["src/inspectme.py:f,src/inspectme.py:g"])
  it "skips a file with no units" $ withInspect "" baseText $ \e -> do
    TIO.writeFile (eDir e </> "notes.txt") "x\n"
    TIO.writeFile (eDir e </> "src/flat.py") "x = 1\n"
    o1 <- ePassOn e (eDir e </> "notes.txt")
    o2 <- ePassOn e (eDir e </> "src/flat.py")
    [o1, o2] `shouldSatisfy` all (\case InspectSkipped _ -> True; _ -> False)
    doesFileExist (eFinds e <> ".asked") >>= (`shouldBe` False)
    doesFileExist (eDir e </> inspectStorePath) >>= (`shouldBe` False)
  it "an unfinished unit is silent" $ withInspect "" edit1 $ \e -> do
    TIO.writeFile (eAnswers e) (answers [("stops-early", True)])
    _ <- ePass e
    asked (eFinds e) >>= (`shouldBe` [])
    vsInspectted <$> eStore e >>= (`shouldBe` M.empty)
  it "a nitpick is withheld" $ withInspect "" edit1 $ \e -> do
    TIO.writeFile (eAnswers e) (answers [("stops-early", False), ("nitpick", True)])
    TIO.writeFile (eFinds e) (replies [("src/inspectme.py:f", findReply [conv] [])])
    _ <- ePass e
    statuses <$> eStore e >>= (`shouldBe` [Withheld])
  it "a wrong bug is refuted" $ withInspect "" edit1 $ \e -> do
    TIO.writeFile (eFinds e) (replies [("src/inspectme.py:f", findReply [("bug", (2, 2), "f is off by one.", "", (1, 2))] [])])
    TIO.writeFile (eChecks e) (replies [("f is off by one.", checkReply "wrong")])
    _ <- ePass e
    statuses <$> eStore e >>= (`shouldBe` [Refuted])
  it "the same problem merges" $ withInspect "" edit1 $ \e -> do
    TIO.writeFile (eFinds e) (replies [("src/inspectme.py:f", findReply [conv] [])])
    _ <- ePass e
    TIO.writeFile (eDir e </> file) edit2
    _ <- ePass e
    map (\f -> (vfStatus f, vfSeen f)) . vsFindings <$> eStore e >>= (`shouldBe` [(Open, 2)])
  it "dismissed here stays gone" $ withInspect "" edit1 $ \e -> do
    TIO.writeFile (eFinds e) (replies [("src/inspectme.py:f", findReply [conv] [])])
    _ <- ePass e
    [i] <- map vfId . vsFindings <$> eStore e
    _ <- updateInspectStore (eWs e) (eDir e) (\vs -> (dismiss False i vs, ()))
    TIO.writeFile (eDir e </> file) edit2
    _ <- ePass e
    statuses <$> eStore e >>= (`shouldBe` [DismissedHere])
  it "no longer holds resolves" $ withInspect "" edit1 $ \e -> do
    TIO.writeFile (eFinds e) (replies [("src/inspectme.py:f", findReply [conv] [])])
    _ <- ePass e
    [i] <- map vfId . vsFindings <$> eStore e
    TIO.writeFile (eFinds e) (replies [("src/inspectme.py:f", findReply [] [(i, "no longer holds")])])
    TIO.writeFile (eDir e </> file) edit2
    _ <- ePass e
    statuses <$> eStore e >>= (`shouldBe` [Resolved])
  it "withheld is re-asked only on change" $ withInspect "" edit1 $ \e -> do
    TIO.writeFile (eAnswers e) (answers [("stops-early", False), ("nitpick", True), ("same", True)])
    -- the finding sits in f; later saves change only g, so f's fingerprint stays
    TIO.writeFile (eFinds e) (replies [(k, findReply [conv] []) | k <- ["src/inspectme.py:f", "src/inspectme.py:g"]])
    _ <- ePass e
    let nitpicks = length . filter (== "nitpick") <$> asked (eAnswers e)
    nitpicks >>= (`shouldBe` 1)
    TIO.writeFile (eDir e </> file) (T.replace "y * 2" "y * 3" edit1)
    _ <- ePass e
    nitpicks >>= (`shouldBe` 1)
    TIO.writeFile (eDir e </> ".olifant/inspect.md") "## Rules\n### A\nNo twos, ever.\n"
    TIO.writeFile (eDir e </> file) (T.replace "y * 2" "y * 4" edit1)
    _ <- ePass e
    nitpicks >>= (`shouldBe` 2)
    statuses <$> eStore e >>= (`shouldBe` [Withheld])
  it "refuted stands until its prints move" $ withInspect "" edit1 $ \e -> do
    -- a bug in f whose evidence is g (line 5)
    let bug = ("bug", (2, 2), "f is off by one.", "", (5, 6))
    TIO.writeFile (eFinds e) (replies [(k, findReply [bug] []) | k <- ["src/inspectme.py:f", "src/inspectme.py:f,src/inspectme.py:g"]])
    TIO.writeFile (eChecks e) (replies [("f is off by one.", checkReply "wrong")])
    _ <- ePass e
    length <$> asked (eChecks e) >>= (`shouldBe` 1)
    TIO.writeFile (eDir e </> file) edit2
    _ <- ePass e
    length <$> asked (eChecks e) >>= (`shouldBe` 1)
    TIO.writeFile (eDir e </> file) (T.replace "y * 2" "y * 5" (T.replace "x + 22" "x + 23" edit2))
    _ <- ePass e
    length <$> asked (eChecks e) >>= (`shouldBe` 2)
    statuses <$> eStore e >>= (`shouldBe` [Refuted])
  it "the pass cap stops the find call" $ withInspect "pass_cost = 0.01\n" edit1 $ \e -> do
    o <- ePass e
    o `shouldSatisfy` \case InspectDone {voCapped = True} -> True; _ -> False
    asked (eFinds e) >>= (`shouldBe` [])
    M.lookup file . vsCapped <$> eStore e >>= (`shouldBe` Just (S.fromList ["f"]))
  it "the day cap counts today's inspect spend, local day" $ withInspect "" edit1 $ \e -> do
    let now = UTCTime (fromGregorian 2026 10 6) 0
        record at = "{\"kind\":\"errand\",\"id\":\"x\",\"at\":\"" <> T.pack (iso8601Show at) <> "\",\"verb\":\"inspect-check\",\"plan\":\"src/inspectme.py\",\"row\":0,\"section\":\"\",\"model\":\"sonnet\",\"status\":\"ok\",\"models\":{},\"prompt_chars\":0,\"grounded\":0,\"blocks\":0,\"lines\":0,\"dropped\":0,\"unresolved\":0,\"changed\":0,\"denials\":0,\"cost\":3.0}\n"
    createDirectoryIfMissing True (eDir e </> ".olifant/state")
    TIO.writeFile (eDir e </> ".olifant/state/errands.jsonl") (record now)
    _ <- ePass e
    asked (eFinds e) >>= (`shouldBe` [])
    TIO.writeFile (eDir e </> ".olifant/state/errands.jsonl") (record (addUTCTime (-86400) now))
    _ <- ePass e
    asked (eFinds e) >>= (`shouldBe` ["src/inspectme.py:f"])
  it "max_shown: three verified, the rest wait" $ withInspect "" edit1 $ \e -> do
    let bug n = ("bug", (2, 2), "f is wrong in way " <> n <> ".", "", (1, 2))
    TIO.writeFile (eAnswers e) (answers [("stops-early", False), ("nitpick", False), ("same", False)])
    TIO.writeFile (eFinds e) (replies [("src/inspectme.py:f", findReply (map bug ["one", "two", "three", "four"]) [])])
    TIO.writeFile (eChecks e) (replies [("f is wrong in way " <> n <> ".", checkReply "right") | n <- ["one", "two", "three", "four"]])
    _ <- ePass e
    vs <- eStore e
    (length (filter (== Open) (statuses vs)), length (filter (== Waiting) (statuses vs))) `shouldBe` (3, 1)
    length <$> asked (eChecks e) >>= (`shouldBe` 3)
  it "room in the margin: the best waiting finding is verified with no find call" $ withInspect "" edit1 $ \e -> do
    let bug n = ("bug", (2, 2), "f is wrong in way " <> n <> ".", "", (1, 2))
    TIO.writeFile (eAnswers e) (answers [("stops-early", False), ("nitpick", False), ("same", False)])
    TIO.writeFile (eFinds e) (replies [("src/inspectme.py:f", findReply (map bug ["one", "two", "three", "four"]) [])])
    TIO.writeFile (eChecks e) (replies [("f is wrong in way " <> n <> ".", checkReply "right") | n <- ["one", "two", "three", "four"]])
    _ <- ePass e
    [i] <- map vfId . filter ((== Open) . vfStatus) . vsFindings <$> eStore e >>= pure . take 1
    _ <- updateInspectStore (eWs e) (eDir e) (\vs -> (dismiss False i vs, ()))
    _ <- ePass e
    length <$> asked (eFinds e) >>= (`shouldBe` 1)
    vs <- eStore e
    (length (filter (== Open) (statuses vs)), length (filter (== Waiting) (statuses vs))) `shouldBe` (3, 0)
  it "(C1) a finding naming .env or a fenced file never sends that file's text to Jev" $ withInspect "" edit1 $ \e -> do
    TIO.writeFile (eDir e </> ".env") "SECRET=hunter2\n"
    createDirectoryIfMissing True (eDir e </> "docs")
    TIO.writeFile (eDir e </> "docs/notes.txt") "FENCED=swordfish\n"
    seen <- newIORef []
    let capture d = d {vdDecider = (vdDecider d) {weigh = \q -> modifyIORef' seen (++ map snd (qContext q)) >> weigh (vdDecider d) q}}
        envFinding f = ("assumption", (1, 1), "It relies on a value in " <> T.pack f <> ".", "", (1, 1))
    TIO.writeFile (eFinds e) (replies [("src/inspectme.py:f", findReplyIn [(f, envFinding f) | f <- [".env", "docs/notes.txt"]])])
    _ <- ePassWith e capture (eDir e </> file)
    TIO.writeFile (eDir e </> file) edit2
    _ <- ePassWith e capture (eDir e </> file)
    sent <- T.concat <$> readIORef seen
    (T.isInfixOf "hunter2" sent, T.isInfixOf "swordfish" sent) `shouldBe` (False, False)
  it "(M3) a file under the plans' fences is not inspected" $ withInspect "" edit1 $ \e -> do
    createDirectoryIfMissing True (eDir e </> "docs")
    TIO.writeFile (eDir e </> "docs/tool.py") "def h():\n    return 1\n"
    o <- ePassOn e (eDir e </> "docs/tool.py")
    o `shouldSatisfy` \case InspectSkipped _ -> True; _ -> False
  it "(I1) Jev not answering: the pass records nothing, the units stay uninspected" $ withInspect "" edit1 $ \e -> do
    TIO.writeFile (eAnswers e) "{\"answers\": {}}"
    TIO.writeFile (eFinds e) (replies [("src/inspectme.py:f", findReply [conv] [])])
    o <- ePass e
    o `shouldSatisfy` \case InspectSkipped _ -> True; _ -> False
    doesFileExist (eDir e </> inspectStorePath) >>= (`shouldBe` False)
  it "(I1) Jev silent on the same-problem question: no new finding beside the stored one" $ withInspect "" edit1 $ \e -> do
    TIO.writeFile (eFinds e) (replies [("src/inspectme.py:f", findReply [conv] [])])
    _ <- ePass e
    TIO.writeFile (eAnswers e) (answers [("stops-early", False), ("nitpick", False)])
    TIO.writeFile (eFinds e) (replies [("src/inspectme.py:f", findReply [("convention", (2, 2), "f adds two, which the rule forbids.", "A", (1, 2))] [])])
    TIO.writeFile (eDir e </> file) edit2
    _ <- ePass e
    length . vsFindings <$> eStore e >>= (`shouldBe` 1)
  it "(I3) a find call that fails is charged its estimate, and the day counts it" $ withInspect "" edit1 $ \e -> do
    o <- ePassWith e (\d -> d {vdFinder = \_ -> pure (Left (RunFailure E.Timeout "timed out" Nothing))}) (eDir e </> file)
    o `shouldSatisfy` \case InspectDone {voSpent = x} -> x == 0.04; _ -> False
    t <- TIO.readFile (eDir e </> ".olifant/state/errands.jsonl")
    t `shouldSatisfy` T.isInfixOf "\"cost\":4.0e-2"
  it "(I6) a deleted unit's findings are gone on the next save, even with nothing else changed" $ withInspect "" edit1 $ \e -> do
    TIO.writeFile (eFinds e) (replies [("src/inspectme.py:f", findReply [conv] [])])
    _ <- ePass e
    TIO.writeFile (eDir e </> file) "def g(y):\n    return y * 2\n"
    _ <- ePass e
    statuses <$> eStore e >>= (`shouldBe` [Gone])
  it "a fingerprint that moves during the pass drops its findings" $ withInspect "" edit1 $ \e -> do
    TIO.writeFile (eFinds e) (replies [("src/inspectme.py:f", object ["text" .= findReply [conv] [], "delay" .= (1 :: Double)])])
    -- the edit lands while the find call is out: after it was asked, before its reply
    let waitAsked = doesFileExist (eFinds e <> ".asked") >>= \there -> if there then pure () else threadDelay 20000 >> waitAsked
    void . forkIO $ waitAsked >> threadDelay 100000 >> TIO.writeFile (eDir e </> file) edit2
    _ <- ePass e
    vs <- eStore e
    statuses vs `shouldBe` []
    vsInspectted vs `shouldBe` M.empty
