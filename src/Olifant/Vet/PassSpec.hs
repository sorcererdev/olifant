module Olifant.Vet.PassSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
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
import Olifant.Vet.Pass
import Olifant.Vet.Find (recordedFinder)
import Olifant.Vet.Store
import Olifant.Workspace (Workspace)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Process (callProcess)
import Test.Hspec

-- | The fixture file vet looks at: f and g.
baseText :: Text
baseText = "def f(x):\n    return x + 1\n\n\ndef g(y):\n    return y * 2\n"

file :: FilePath
file = "src/vetme.py"

git :: FilePath -> [String] -> IO ()
git dir args = callProcess "git" (["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t"] ++ args)

-- | Recorded Jev answers by kind: yes or no, at 0.9.
answers :: [(Text, Bool)] -> Text
answers kvs = TE.decodeUtf8 (BL.toStrict (encode (object ["answers" .= object [K.fromText k .= object ["choice" .= b, "confidence" .= (0.9 :: Double)] | (k, b) <- kvs]])))

-- | Recorded replies by key.
replies :: [(Text, Value)] -> Text
replies kvs = TE.decodeUtf8 (BL.toStrict (encode (object ["replies" .= object [K.fromText k .= v | (k, v) <- kvs]])))

-- | A find reply with findings: (kind, lines, statement, rule, evidence lines in the file).
findReply :: [(Text, (Int, Int), Text, Text, (Int, Int))] -> [(Text, Text)] -> Value
findReply fs opens =
  String . TE.decodeUtf8 . BL.toStrict . encode $
    object
      [ "findings" .= [object ["kind" .= k, "file" .= file, "lines" .= [a, z], "statement" .= st, "rule" .= r, "evidence" .= [object ["file" .= file, "lines" .= [ea, ez]]]] | (k, (a, z), st, r, (ea, ez)) <- fs]
      , "open" .= [object ["id" .= i, "holds" .= h, "why" .= ("x" :: Text)] | (i, h) <- opens]
      ]

checkReply :: Text -> Value
checkReply verdict = object ["cost" .= (0.01 :: Double), "text" .= ("```json\n{\"verdict\": \"" <> verdict <> "\", \"why\": \"w\", \"evidence\": [{\"path\": \"src/vetme.py\", \"line\": 1}]}\n```" :: Text)]

data Env = Env {eDir :: FilePath, eAnswers :: FilePath, eFinds :: FilePath, eChecks :: FilePath, ePassOn :: FilePath -> IO VetOutcome, eWs :: Workspace IO, eStore :: IO VetStore}

ePass :: Env -> IO VetOutcome
ePass e = ePassOn e (eDir e </> file)

-- | A committed fixture with vet on (Jev "configured", recorded), the file
-- then edited; CONFIG is appended to config.toml.
withVet :: Text -> Text -> (Env -> IO a) -> IO a
withVet config edited act =
  withPython [(file, baseText)] $ \ports dir -> do
    TIO.writeFile (dir </> ".olifant/vet.md") "## Rules\n### A\nNo twos.\n"
    old <- TIO.readFile (dir </> ".olifant/config.toml")
    TIO.writeFile (dir </> ".olifant/config.toml") (old <> "[plans]\ncheck_model = \"sonnet\"\n[vet]\nenabled = true\n" <> config)
    git dir ["init", "-q", "-b", "main"] >> git dir ["add", "."] >> git dir ["commit", "-q", "-m", "base"]
    TIO.writeFile (dir </> file) edited
    let env =
          Env
            { eDir = dir
            , eAnswers = dir </> "answers.json"
            , eFinds = dir </> "finds.json"
            , eChecks = dir </> "checks.json"
            , ePassOn = \path -> do
                s0 <- newServerState ports
                projectFor path s0 >>= \case
                  (s, Just (_, p)) -> do
                    let deps = VetDeps (recordedDecider (dir </> "answers.json") neverCancel) (recordedFinder (dir </> "finds.json")) (const (recordedChecker (dir </> "checks.json"))) utc
                    vetPass neverCancel s p deps path
                  _ -> fail "no project"
            , eWs = pFiles ports
            , eStore = either (error . T.unpack) id <$> readVetStore (pFiles ports) dir
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

statuses :: VetStore -> [Status]
statuses = map vfStatus . vsFindings

spec :: Spec
spec = describe "Olifant.Vet.Pass" $ do
  it "vets a changed unit" $ withVet "" edit1 $ \e -> do
    TIO.writeFile (eFinds e) (replies [("src/vetme.py:f", findReply [conv] [])])
    o <- ePass e
    o `shouldSatisfy` \case VetDone {} -> True; _ -> False
    vs <- eStore e
    statuses vs `shouldBe` [Open]
    map (null . vfPrints) (vsFindings vs) `shouldBe` [False]
    M.lookup file (vsVetted vs) `shouldBe` Just (S.fromList [fingerprint edit1 (1, 2)])
  it "an unchanged unit is not vetted" $ withVet "" edit1 $ \e -> do
    _ <- ePass e
    asked (eFinds e) >>= (`shouldBe` ["src/vetme.py:f"])
  it "untracked file: every unit changed" $ withVet "" baseText $ \e -> do
    TIO.writeFile (eDir e </> "src/new.py") "def a():\n    return 1\n\n\ndef b():\n    return 2\n"
    _ <- ePassOn e (eDir e </> "src/new.py")
    asked (eFinds e) >>= (`shouldBe` ["src/new.py:a,src/new.py:b"])
  it "a repository with no commit: every unit changed" $ withVet "" edit1 $ \e -> do
    removeDirectoryRecursive (eDir e </> ".git")
    git (eDir e) ["init", "-q", "-b", "main"]
    _ <- ePass e
    asked (eFinds e) >>= (`shouldBe` ["src/vetme.py:f,src/vetme.py:g"])
  it "skips a file with no units" $ withVet "" baseText $ \e -> do
    TIO.writeFile (eDir e </> "notes.txt") "x\n"
    TIO.writeFile (eDir e </> "src/flat.py") "x = 1\n"
    o1 <- ePassOn e (eDir e </> "notes.txt")
    o2 <- ePassOn e (eDir e </> "src/flat.py")
    [o1, o2] `shouldSatisfy` all (\case VetSkipped _ -> True; _ -> False)
    doesFileExist (eFinds e <> ".asked") >>= (`shouldBe` False)
    doesFileExist (eDir e </> vetStorePath) >>= (`shouldBe` False)
  it "an unfinished unit is silent" $ withVet "" edit1 $ \e -> do
    TIO.writeFile (eAnswers e) (answers [("stops-early", True)])
    _ <- ePass e
    asked (eFinds e) >>= (`shouldBe` [])
    vsVetted <$> eStore e >>= (`shouldBe` M.empty)
  it "a nitpick is withheld" $ withVet "" edit1 $ \e -> do
    TIO.writeFile (eAnswers e) (answers [("stops-early", False), ("nitpick", True)])
    TIO.writeFile (eFinds e) (replies [("src/vetme.py:f", findReply [conv] [])])
    _ <- ePass e
    statuses <$> eStore e >>= (`shouldBe` [Withheld])
  it "a wrong bug is refuted" $ withVet "" edit1 $ \e -> do
    TIO.writeFile (eFinds e) (replies [("src/vetme.py:f", findReply [("bug", (2, 2), "f is off by one.", "", (1, 2))] [])])
    TIO.writeFile (eChecks e) (replies [("f is off by one.", checkReply "wrong")])
    _ <- ePass e
    statuses <$> eStore e >>= (`shouldBe` [Refuted])
  it "the same problem merges" $ withVet "" edit1 $ \e -> do
    TIO.writeFile (eFinds e) (replies [("src/vetme.py:f", findReply [conv] [])])
    _ <- ePass e
    TIO.writeFile (eDir e </> file) edit2
    _ <- ePass e
    map (\f -> (vfStatus f, vfSeen f)) . vsFindings <$> eStore e >>= (`shouldBe` [(Open, 2)])
  it "dismissed here stays gone" $ withVet "" edit1 $ \e -> do
    TIO.writeFile (eFinds e) (replies [("src/vetme.py:f", findReply [conv] [])])
    _ <- ePass e
    [i] <- map vfId . vsFindings <$> eStore e
    _ <- updateVetStore (eWs e) (eDir e) (\vs -> (dismiss False i vs, ()))
    TIO.writeFile (eDir e </> file) edit2
    _ <- ePass e
    statuses <$> eStore e >>= (`shouldBe` [DismissedHere])
  it "no longer holds resolves" $ withVet "" edit1 $ \e -> do
    TIO.writeFile (eFinds e) (replies [("src/vetme.py:f", findReply [conv] [])])
    _ <- ePass e
    [i] <- map vfId . vsFindings <$> eStore e
    TIO.writeFile (eFinds e) (replies [("src/vetme.py:f", findReply [] [(i, "no longer holds")])])
    TIO.writeFile (eDir e </> file) edit2
    _ <- ePass e
    statuses <$> eStore e >>= (`shouldBe` [Resolved])
  it "withheld is re-asked only on change" $ withVet "" edit1 $ \e -> do
    TIO.writeFile (eAnswers e) (answers [("stops-early", False), ("nitpick", True), ("same", True)])
    -- the finding sits in f; later saves change only g, so f's fingerprint stays
    TIO.writeFile (eFinds e) (replies [(k, findReply [conv] []) | k <- ["src/vetme.py:f", "src/vetme.py:g"]])
    _ <- ePass e
    let nitpicks = length . filter (== "nitpick") <$> asked (eAnswers e)
    nitpicks >>= (`shouldBe` 1)
    TIO.writeFile (eDir e </> file) (T.replace "y * 2" "y * 3" edit1)
    _ <- ePass e
    nitpicks >>= (`shouldBe` 1)
    TIO.writeFile (eDir e </> ".olifant/vet.md") "## Rules\n### A\nNo twos, ever.\n"
    TIO.writeFile (eDir e </> file) (T.replace "y * 2" "y * 4" edit1)
    _ <- ePass e
    nitpicks >>= (`shouldBe` 2)
    statuses <$> eStore e >>= (`shouldBe` [Withheld])
  it "refuted stands until its prints move" $ withVet "" edit1 $ \e -> do
    -- a bug in f whose evidence is g (line 5)
    let bug = ("bug", (2, 2), "f is off by one.", "", (5, 6))
    TIO.writeFile (eFinds e) (replies [(k, findReply [bug] []) | k <- ["src/vetme.py:f", "src/vetme.py:f,src/vetme.py:g"]])
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
  it "the pass cap stops the find call" $ withVet "pass_cost = 0.01\n" edit1 $ \e -> do
    o <- ePass e
    o `shouldSatisfy` \case VetDone {voCapped = True} -> True; _ -> False
    asked (eFinds e) >>= (`shouldBe` [])
    M.lookup file . vsCapped <$> eStore e >>= (`shouldBe` Just (S.fromList ["f"]))
  it "the day cap counts today's vet spend, local day" $ withVet "" edit1 $ \e -> do
    let now = UTCTime (fromGregorian 2026 10 6) 0
        record at = "{\"kind\":\"errand\",\"id\":\"x\",\"at\":\"" <> T.pack (iso8601Show at) <> "\",\"verb\":\"vet-check\",\"plan\":\"src/vetme.py\",\"row\":0,\"section\":\"\",\"model\":\"sonnet\",\"status\":\"ok\",\"models\":{},\"prompt_chars\":0,\"grounded\":0,\"blocks\":0,\"lines\":0,\"dropped\":0,\"unresolved\":0,\"changed\":0,\"denials\":0,\"cost\":3.0}\n"
    createDirectoryIfMissing True (eDir e </> ".olifant/state")
    TIO.writeFile (eDir e </> ".olifant/state/errands.jsonl") (record now)
    _ <- ePass e
    asked (eFinds e) >>= (`shouldBe` [])
    TIO.writeFile (eDir e </> ".olifant/state/errands.jsonl") (record (addUTCTime (-86400) now))
    _ <- ePass e
    asked (eFinds e) >>= (`shouldBe` ["src/vetme.py:f"])
  it "max_shown: three verified, the rest wait" $ withVet "" edit1 $ \e -> do
    let bug n = ("bug", (2, 2), "f is wrong in way " <> n <> ".", "", (1, 2))
    TIO.writeFile (eAnswers e) (answers [("stops-early", False), ("nitpick", False), ("same", False)])
    TIO.writeFile (eFinds e) (replies [("src/vetme.py:f", findReply (map bug ["one", "two", "three", "four"]) [])])
    TIO.writeFile (eChecks e) (replies [("f is wrong in way " <> n <> ".", checkReply "right") | n <- ["one", "two", "three", "four"]])
    _ <- ePass e
    vs <- eStore e
    (length (filter (== Open) (statuses vs)), length (filter (== Waiting) (statuses vs))) `shouldBe` (3, 1)
    length <$> asked (eChecks e) >>= (`shouldBe` 3)
  it "a fingerprint that moves during the pass drops its findings" $ withVet "" edit1 $ \e -> do
    TIO.writeFile (eFinds e) (replies [("src/vetme.py:f", object ["text" .= findReply [conv] [], "delay" .= (1 :: Double)])])
    void . forkIO $ threadDelay 300000 >> TIO.writeFile (eDir e </> file) edit2
    _ <- ePass e
    vs <- eStore e
    statuses vs `shouldBe` []
    vsVetted vs `shouldBe` M.empty
