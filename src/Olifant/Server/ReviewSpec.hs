module Olifant.Server.ReviewSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.Aeson (encode)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.LspSpec (plainPorts)
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.Plans (lnRow, parsePlan)
import Olifant.Server.Review
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Server.Review (the do review)" $ do
  describe "the tree reference" $ do
    it "is the change since it: tracked edits, new files, changed untracked files; the plan, the state and the config left out" $
      inRepo $ \dir -> do
        TIO.writeFile (dir </> "notes.txt") "mine\n"
        TIO.writeFile (dir </> "a.py") "x = 2\n" -- an edit not yet committed is part of the reference
        Just ref <- takeRef dir
        TIO.writeFile (dir </> "a.py") "x = 3\n"
        TIO.writeFile (dir </> "b.py") "y = 1\n"
        TIO.writeFile (dir </> "notes.txt") "theirs\n"
        TIO.writeFile (dir </> "plans/t.md") "- [x] done\n"
        createDirectoryIfMissing True (dir </> ".olifant/state")
        TIO.writeFile (dir </> ".olifant/state/x.log") "noise\n"
        TIO.writeFile (dir </> ".olifant/config.toml") "[plans]\ndo_tools = [\"Bash(make*)\"]\n"
        cs <- changeSince dir ["plans/t.md"] ref
        [(fcPath c, fcChange c) | c <- cs] `shouldBe` [("a.py", Modified), ("b.py", Added), ("notes.txt", Modified)]
        [(fcBefore c, fcAfter c) | c <- cs] `shouldBe` [(Just "x = 2\n", Just "x = 3\n"), (Nothing, Just "y = 1\n"), (Just "mine\n", Just "theirs\n")]
        map fcDiff (take 1 cs) `shouldBe` ["--- a/a.py\n+++ b/a.py\n@@ -1,1 +1,1 @@\n-x = 2\n+x = 3\n"]
    it "revert restores the files, removes the new ones and leaves the index alone" $
      inRepo $ \dir -> do
        TIO.writeFile (dir </> "notes.txt") "mine\n"
        Just ref <- takeRef dir
        TIO.writeFile (dir </> "a.py") "x = 3\n"
        TIO.writeFile (dir </> "b.py") "y = 1\n"
        TIO.writeFile (dir </> "notes.txt") "theirs\n"
        cs <- changeSince dir [] ref
        revertChange dir ref cs `shouldReturn` Right ()
        TIO.readFile (dir </> "a.py") `shouldReturn` "x = 1\n"
        TIO.readFile (dir </> "notes.txt") `shouldReturn` "mine\n"
        doesFileExist (dir </> "b.py") `shouldReturn` False
        changeSince dir [] ref `shouldReturn` []
    it "is none outside a repository" $
      withSystemTempDirectory "olifant-review" $ \dir -> takeRef dir `shouldReturn` Nothing
  describe "denials" $ do
    let bash c = Denial "Bash" c
    it "are read from the runner's permission_denials" $
      denialsIn (toArray [object ["tool_name" .= ("Bash" :: Text), "tool_use_id" .= ("x" :: Text), "tool_input" .= object ["command" .= ("python -m pytest t.py" :: Text)]], object ["tool_name" .= ("WebFetch" :: Text), "tool_input" .= object ["url" .= ("https://x" :: Text)]]])
        `shouldBe` [bash "python -m pytest t.py", Denial "WebFetch" "https://x"]
    it "are said in words, each command once with its count, then what is allowed" $
      deniedWords [bash "python -m pytest t.py", bash "python -m pytest t.py", bash "python -m pytest t.py"] ["Bash(uv run*)"]
        `shouldBe` ["tried `python -m pytest t.py` 3 times; not allowed.", "Allowed: `uv run …`."]
    it "allow a command's first words, three when the second is a flag" $ do
      allowPattern (bash "python -m pytest t.py") `shouldBe` "Bash(python -m pytest*)"
      allowPattern (bash "uv run pytest x") `shouldBe` "Bash(uv run*)"
      allowPattern (Denial "WebFetch" "https://x") `shouldBe` "WebFetch"
  describe "[plans] do_tools in the config" $ do
    it "is appended with its table when there is none" $
      allowInConfig ["Bash(make*)"] "[test]\ncommand = \"x\"\n" `shouldBe` "[test]\ncommand = \"x\"\n\n[plans]\ndo_tools = [\"Bash(make*)\"]\n"
    it "is made under [plans] when the key is not there" $
      allowInConfig ["Bash(make*)"] "[plans]\nmodel = \"sonnet\"\n[other]\nx = 1\n" `shouldBe` "[plans]\ndo_tools = [\"Bash(make*)\"]\nmodel = \"sonnet\"\n[other]\nx = 1\n"
    it "is extended where it is, once per pattern, a list over lines made one" $
      allowInConfig ["Bash(make*)", "Bash(gh*)"] "[plans]\ndo_tools = [\n  \"Bash(gh*)\",\n]\nmodel = \"opus\"\n"
        `shouldBe` "[plans]\ndo_tools = [\"Bash(gh*)\", \"Bash(make*)\"]\nmodel = \"opus\"\n"
  describe "a review" $ do
    it "round-trips through its file's JSON" $
      parseReview (BL.toStrict (encode (reviewRecord sample))) `shouldBe` Just sample
    it "finds its step again after the tick landed, or after lines moved it" $ do
      let ticked = parsePlan "# T\n\n- [x] add a label @a.py:Table.label ✓ abc1234\n"
          moved = parsePlan "# T\nnew line\n\n- [ ] other\n- [ ] add a label @a.py:Table.label\n"
      fmap lnRow (locateStep ticked sample) `shouldBe` Just 2
      fmap lnRow (locateStep moved sample) `shouldBe` Just 4
      locateStep (parsePlan "# T\n- [ ] another @a.py:Table\n") sample `shouldBe` Nothing
    it "finds an unanchored step again after keep gave it an anchor (by its words)" $ do
      let bare = sample {rvStep = "- [ ] add a label"}
          kept = parsePlan "# T\n\n- [ ] add a label   @a.py:Table.label\n- [ ] add a label too\n"
      fmap lnRow (locateStep kept bare) `shouldBe` Just 2
      sameStep "- [ ] add a label" "- [x] add a label @a.py:T ✓ abc1234" `shouldBe` True
      sameStep "- [ ] add a label @a.py:U" "- [ ] add a label @a.py:T" `shouldBe` False
    it "keeps the step line it proposes as a replace, the rest as lines after the step" $ do
      let unanchored = sample {rvStep = "- [ ] add a label", rvLines = ["- [ ] add a label   @a.py:Table.label", "  - also @a.py:Table"]}
      fmap (\(l, rep, rest) -> (lnRow l, rep, rest)) (keepLines (parsePlan "# T\n\n- [ ] add a label\n") unanchored)
        `shouldBe` Just (2, Just "- [ ] add a label   @a.py:Table.label", ["  - also @a.py:Table"])
    it "is drawn as a pause: title, summary, the tests, the denials in words, the landing and the offers" $ do
      let change = FileChange "a.py" Modified (Just "x = 1\n") (Just "x = 2\n") "--- a/a.py\n+++ b/a.py\n"
          v = reviewPayload sample (Just (2, 1)) [change] Null
      at ["summary"] v `shouldBe` Just (String "The do changed 1 file.")
      at ["title", "badge"] v `shouldBe` Just (String "◆ answered")
      at ["title", "unit"] v `shouldBe` Just (String "Table.label")
      at ["title", "step"] v `shouldBe` Just (Number 1)
      at ["tests", "passed"] v `shouldBe` Just (Number 1)
      at ["denied"] v `shouldBe` Just (toArray ["tried `python -m pytest a__test.py`; not allowed.", "Allowed: `uv run …`."])
      at ["landing", "diff"] v `shouldBe` Just (String "--- a/a.py\n+++ b/a.py\n")
      fmap (map (\o -> lookupKey "key" o)) (arrayAt ["offers"] v) `shouldBe` Just [Just "a", Just "r", Just "o", Just "t", Just "p"]
    it "offers no revert without a reference or a change, and no allow once allowed" $ do
      let v = reviewPayload sample {rvRef = Nothing, rvAllowed = ["Bash(python -m pytest*)"]} Nothing [] Null
      fmap (map (lookupKey "key")) (arrayAt ["offers"] v) `shouldBe` Just [Just "a", Just "o", Just "t"]
      at ["summary"] v `shouldBe` Just (String "The do answered and changed no file.")
    it "parses a review file from before the archive (no parent, diff or choice)" $ do
      let old = KM.delete "parent" (KM.delete "diff" (KM.delete "choice" (case reviewRecord sample of Object o -> o; _ -> KM.empty)))
      parseReview (BL.toStrict (encode (Object old))) `shouldBe` Just sample
  describe "the errand archive" $ do
    it "keeps every do of a plan, newest first, and finds a step's by its words" $ withSystemTempDirectory "archive" $ \root -> do
      let ports = plainPorts
          later = sample {rvId = "s2", rvAt = UTCTime (fromGregorian 2026 10 26) 0, rvStep = "- [x] add a label @a.py:Table.label ✓ abc1234"}
          other = sample {rvId = "s3", rvAt = UTCTime (fromGregorian 2026 10 24) 0, rvStep = "- [ ] another @a.py:Table"}
      mapM_ (archiveErrand ports root) [sample, later, other]
      map rvId <$> loadArchive ports root "plans/t.md" `shouldReturn` ["s2", "shim-1", "s3"]
      map rvId <$> archivedFor ports root "plans/t.md" (rvStep sample) `shouldReturn` ["s2", "shim-1"]
      fmap rvId <$> loadArchived ports root "plans/t.md" "s3" `shouldReturn` Just "s3"
      loadArchived ports root "plans/t.md" "nope" `shouldReturn` Nothing
    it "(M8) counts a past do on one step only: its recorded row when two steps share its words" $ do
      let ls = parsePlan "# T\n- [ ] write tests @a.py:F\n- [ ] write tests @a.py:G\n"
          bare = sample {rvStep = "- [ ] write tests", rvRow = 2}
      map rvId (pastDosAt ls [bare] 2) `shouldBe` ["shim-1"]
      map rvId (pastDosAt ls [bare] 1) `shouldBe` []
    it "(M5) reopens only with a runner that is claude (or a claude wrapper)" $ do
      map reopenable ["claude", "/usr/local/bin/claude", "/x/claude-shim.sh", "/x/codex", "/x/my-runner"] `shouldBe` [True, True, True, False, False]
    it "cuts the diff it keeps to 2000 lines in all" $ do
      let big n f = FileChange f Modified Nothing Nothing (T.unlines (replicate n "+x"))
          kept = keptDiff [big 1500 "a.py", big 1500 "b.py", big 10 "c.py"]
      sum (map (length . T.lines . fcDiff) kept) `shouldSatisfy` (<= 2001)
      map fcPath kept `shouldBe` ["a.py", "b.py"]
      last (T.lines (fcDiff (last kept))) `shouldBe` "… (cut)"
    it "is drawn read-only, with its outcome and one offer: reopen, when it has a session" $ do
      let change = FileChange "a.py" Modified Nothing Nothing "--- a/a.py\n+++ b/a.py\n"
          v = errandPayload (Just "/h/.claude/projects/x/s1.jsonl") sample {rvDiff = [change], rvChoice = Just "kept"}
      at ["kind"] v `shouldBe` Just (String "errand")
      at ["title", "badge"] v `shouldBe` Just (String "◆ answered · kept")
      at ["landing", "diff"] v `shouldBe` Just (String "--- a/a.py\n+++ b/a.py\n")
      fmap (map (lookupKey "key")) (arrayAt ["offers"] v) `shouldBe` Just [Just "o"]
      fmap (map (lookupKey "title")) (arrayAt ["details"] v) `shouldSatisfy` maybe False (elem (Just (String "transcript")))
      fmap (map (lookupKey "key")) (arrayAt ["offers"] (errandPayload Nothing sample {rvSession = Nothing})) `shouldBe` Just []
    it "offers reopen on a live do review only when the do has a session" $ do
      fmap (map (lookupKey "key")) (arrayAt ["offers"] (reviewPayload sample {rvRef = Nothing} Nothing [] Null)) `shouldBe` Just [Just "a", Just "o", Just "t", Just "p"]
      fmap (map (lookupKey "key")) (arrayAt ["offers"] (reviewPayload sample {rvRef = Nothing, rvSession = Nothing} Nothing [] Null)) `shouldBe` Just [Just "a", Just "t", Just "p"]
    it "reads the tests from the facts' test children" $
      testsOfFacts ["  - test: it labels @a__test.py:test_label ●", "- [x] s @a.py:T ✓ abc1234", "  - test: other @a__test.py:test_other ✗"]
        `shouldBe` [ReviewTest "test_label" "passed", ReviewTest "test_other" "failed"]
  where
    toArray :: [Value] -> Value
    toArray = Array . foldr (\x acc -> pure x <> acc) mempty
    lookupKey k = \case
      Object o -> KM.lookup k o
      _ -> Nothing
    at [] v = Just v
    at (k : ks) v = lookupKey k v >>= at ks
    arrayAt ks v = at ks v >>= \case
      Array xs -> Just (foldr (:) [] xs)
      _ -> Nothing

sample :: Review
sample =
  Review
    { rvId = "shim-1", rvErrand = "shim-1", rvPlan = "plans/t.md", rvRow = 2, rvStep = "- [ ] add a label @a.py:Table.label"
    , rvAt = UTCTime (fromGregorian 2026 10 25) 0, rvModel = "sonnet", rvSeconds = 42, rvCost = Just 0.06
    , rvStatus = "answered", rvWhy = Nothing, rvSaid = ["DONE @a.py:Table.label"], rvLines = ["  - also @a.py:Table"]
    , rvFacts = [(2, "- [ ] add a label @a.py:Table.label", "- [x] add a label @a.py:Table.label ✓ abc1234")]
    , rvTests = [ReviewTest "test_label" "passed"], rvDenials = [Denial "Bash" "python -m pytest a__test.py"]
    , rvTools = ["Bash(uv run*)"], rvAllowed = [], rvRef = Just (TreeRef "abc" [("notes.txt", "def")])
    , rvPrompt = "the prompt", rvAnswer = "DONE @a.py:Table.label", rvSession = Just "shim-1"
    , rvParent = Nothing, rvDiff = [], rvChoice = Nothing
    }

-- | A repository with one commit: a.py, plans/t.md.
inRepo :: (FilePath -> IO a) -> IO a
inRepo act = withSystemTempDirectory "olifant-review" $ \dir -> do
  let g args = callProcess "git" (["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t"] ++ args)
  createDirectoryIfMissing True (dir </> "plans")
  TIO.writeFile (dir </> "a.py") "x = 1\n"
  TIO.writeFile (dir </> "plans/t.md") "- [ ] s\n"
  TIO.writeFile (dir </> ".gitignore") ".olifant/state/\n"
  createDirectoryIfMissing True (dir </> ".olifant")
  TIO.writeFile (dir </> ".olifant/config.toml") ""
  g ["init", "-q", "-b", "main"]
  g ["add", "."]
  g ["commit", "-q", "-m", "init"]
  act dir
