module Olifant.Server.ClaimsSpec (spec) where

import Control.Monad (forM)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.App (Ports (..), Project)
import Olifant.Decide (recordedDecider)
import Olifant.Doctor (DoctorPorts (..))
import Olifant.Language (Match (..), noChecker, noRunner, querying)
import Olifant.Language.Check (noVerifier)
import Olifant.Language.Grammar (GrammarFailure (NoSource))
import Olifant.Model (Backend (..), Escalate (..), Rung (..))
import Olifant.Plans (Line (..), parsePlan)
import Olifant.Process (gitStateStamp, neverCancel)
import Olifant.Server (ServerState, newServerState, projectFor)
import Olifant.Server.Claims
import Olifant.Server.LoopQuestions (Rungs (..), rungsFor)
import Olifant.Server.Resolve (LineContext (..), UnitRef (..))
import Olifant.Server.State (ServerConfig (..), ServerState (..))
import Olifant.Server.Telemetry (Decision (..))
import Olifant.Workspace (Workspace (..), memoryWorkspace)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (makeRelative, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

at0 :: UTCTime
at0 = UTCTime (fromGregorian 2026 11 7) 0

ctx :: LineContext
ctx = LineContext "the loader keeps D" Nothing "Loader" ""

unit :: UnitRef
unit = UnitRef "app/x.py" "f"

spec :: Spec
spec = describe "Olifant.Server.Claims" $ describe "Claims" $ do
  describe "passWork" $ do
    let entry check = ClaimEntry (Just unit) "jev-1" [unit] 0.9 check (Just "abc1234") at0
        work store = map summary . passWork Nothing (parsePlan (T.unlines plan)) . M.fromList $ store
        summary = \case
          ResolveAndCheck l -> ("check", lnRow l)
          ResolveOnly l -> ("resolve", lnRow l)
          WriteAnchor l _ -> ("write", lnRow l)
          CheckAgain l _ -> ("again", lnRow l)
        plan =
          [ "# T"
          , "- the loader keeps D"
          , "- [ ] add a column"
          , "- anchored @app/x.py:f"
          , "- ? how is it read"
          , "  - I think it reads D"
          , "- ? what does the ask say"
          , "  - grounded @app/x.py:f ✓ abc1234"
          , "  - so it keeps D"
          , "- ? evidence only"
          , "  - ✓ it is @app/x.py:f ✓ abc1234"
          , "  - the conclusion"
          , "Some prose."
          , "- 25 Oct 21:31 · did"
          , "- [x] done step" ]
    it "(g) an unanchored claim not in the store is resolved and checked; a belief under a bare question too" $
      work [] `shouldBe` [("check", 1), ("resolve", 2), ("check", 5), ("resolve", 14)]
    it "(h) a claim or step already in the store is not work again" $
      work [("the loader keeps D", entry Nothing), ("add a column", entry Nothing)] `shouldBe` [("check", 5), ("resolve", 14)]
    it "(i) an entry checked right with a unit and a fingerprint, its line still bare, is a write" $
      work [("the loader keeps D", entry (Just CheckRight)), ("add a column", entry (Just CheckRight)), ("I think it reads D", entry (Just (CheckWrong "no"))), ("done step", entry Nothing)]
        `shouldBe` [("write", 1), ("write", 2)]
    it "(j) pruneClaims drops a renamed line's old key" $ do
      let store = M.fromList [("the loader keeps D", entry Nothing), ("the loader kept D", entry Nothing)]
      M.keys (pruneClaims (parsePlan "- the loader keeps D\n") store) `shouldBe` ["the loader keeps D"]
    it "(k) lines sharing a key are one piece of work: the first" $
      map summary (passWork Nothing (parsePlan "# T\n- same words\n- [ ] same words\n  - same words\n- other\n") M.empty)
        `shouldBe` [("check", 1), ("check", 4)]
    it "(l) a resolved, unchecked claim is checked again once its entry is older than the cutoff, and no sooner" $ do
      let store = [("the loader keeps D", entry Nothing)]
          ls = parsePlan "# T\n- the loader keeps D\n"
          cutoff = UTCTime (fromGregorian 2026 11 7) 600
      map summary (passWork (Just cutoff) ls (M.fromList store)) `shouldBe` [("again", 1)]
      map summary (passWork (Just at0) ls (M.fromList store)) `shouldBe` []
      map summary (passWork Nothing ls (M.fromList store)) `shouldBe` []
      -- a step is only resolved, never checked
      map summary (passWork (Just cutoff) (parsePlan "# T\n- [ ] add a column\n") (M.fromList [("add a column", entry Nothing)])) `shouldBe` []
      -- one without a unit is unresolved, not unchecked
      map summary (passWork (Just cutoff) ls (M.fromList [("the loader keeps D", (entry Nothing) {ceUnit = Nothing})])) `shouldBe` []
  it "(a) a store round-trips through writeClaims and readClaims" $ do
    (s, _) <- claimsServer "" Nothing
    let store =
          M.fromList
            [ ("the loader keeps D", ClaimEntry (Just unit) "jev-2" [unit, UnitRef "app/y.py" "g"] 0.8 (Just (CheckWrong "it drops D")) (Just "abc1234") at0)
            , ("a step", ClaimEntry Nothing "none" [] 0 Nothing Nothing at0)
            , ("elsewhere", ClaimEntry Nothing "elsewhere" [unit] 0.7 (Just CheckElsewhere) Nothing at0)
            , ("right", ClaimEntry (Just unit) "jev-1" [unit] 0.9 (Just CheckRight) (Just "abc1234") at0) ]
    writeClaims (ssPorts s) "/r1" "plans/t.md" store
    readClaims (ssPorts s) "/r1" "plans/t.md" `shouldReturn` store
    claimsPath "/r1" "plans/t.md" `shouldBe` "/r1/.olifant/state/claims/plans/t.json"
  it "(b) a corrupt or missing file reads as empty" $ do
    (s, _) <- claimsServer "" Nothing
    readClaims (ssPorts s) "/r1" "plans/none.md" `shouldReturn` M.empty
    writeText (pFiles (ssPorts s)) (claimsPath "/r1" "plans/t.md") "{not json"
    readClaims (ssPorts s) "/r1" "plans/t.md" `shouldReturn` M.empty
  it "(g) a store is written whole or not at all: to a temporary file, then renamed over" $ do
    (s, _) <- claimsServer "" Nothing
    wrote <- newIORef []
    let ws = pFiles (ssPorts s)
        ports = (ssPorts s) {pFiles = ws {writeText = \p x -> modifyIORef wrote (p :) >> writeText ws p x}}
        store = M.fromList [("a", ClaimEntry Nothing "none" [] 0 Nothing Nothing at0)]
    writeClaims ports "/r1" "plans/t.md" store
    readIORef wrote `shouldReturn` [claimsPath "/r1" "plans/t.md" <> ".tmp"]
    readClaims ports "/r1" "plans/t.md" `shouldReturn` store
    fileExists ws (claimsPath "/r1" "plans/t.md" <> ".tmp") `shouldReturn` False
  it "(h) pruneOrphanClaims removes the stores of plans that no longer exist" $ do
    (s, _) <- claimsServer "" Nothing
    let store = M.fromList [("a", ClaimEntry Nothing "none" [] 0 Nothing Nothing at0)]
    writeClaims (ssPorts s) "/r1" "plans/t.md" store
    writeClaims (ssPorts s) "/r1" "plans/gone.md" store
    writeClaims (ssPorts s) "/r1" "plans/sub/deeper.md" store
    pruneOrphanClaims (ssPorts s) "/r1"
    fileExists (pFiles (ssPorts s)) (claimsPath "/r1" "plans/t.md") `shouldReturn` True
    fileExists (pFiles (ssPorts s)) (claimsPath "/r1" "plans/gone.md") `shouldReturn` False
    fileExists (pFiles (ssPorts s)) (claimsPath "/r1" "plans/sub/deeper.md") `shouldReturn` False
  it "(c) claimKey drops indentation, the dash, a checkbox, the anchor and its verdict" $ do
    map claimKey (parsePlan "- foo   @a.py:f ✓ abc1234\n  - foo\n- [ ] foo @a.py:f\n") `shouldBe` ["foo", "foo", "foo"]
  it "(d) a recorded wrong check on an auto rung is CheckWrong with its why" $
    withAnswers "{\"answers\": {\"plan-check\": {\"choice\": \"wrong\", \"fields\": {\"why\": \"it keeps D\"}, \"confidence\": 0.8}}}" $ \file -> do
      let rungs = Rungs Nothing [(Rung "local" EscalateAuto, OpenAICompatible "http://127.0.0.1:1/v1" "x")] "jev" (Just (\_ -> recordedDecider file neverCancel))
      (check, ds) <- checkClaim neverCancel 5 rungs ctx unit "def f():\n    return 1\n"
      check `shouldBe` Just (CheckWrong "it keeps D")
      length ds `shouldBe` 1
  it "(e) a recorded elsewhere check is CheckElsewhere; an empty why is no reason given" $ do
    withAnswers "{\"answers\": {\"plan-check\": {\"choice\": \"elsewhere\", \"confidence\": 0.9}}}" $ \file -> do
      let rungs = Rungs Nothing [(Rung "local" EscalateAuto, OpenAICompatible "http://127.0.0.1:1/v1" "x")] "jev" (Just (\_ -> recordedDecider file neverCancel))
      fst <$> checkClaim neverCancel 5 rungs ctx unit "def f(): pass\n" `shouldReturn` Just CheckElsewhere
    withAnswers "{\"answers\": {\"plan-check\": {\"choice\": \"wrong\", \"confidence\": 0.9}}}" $ \file -> do
      let rungs = Rungs Nothing [(Rung "local" EscalateAuto, OpenAICompatible "http://127.0.0.1:1/v1" "x")] "jev" (Just (\_ -> recordedDecider file neverCancel))
      fst <$> checkClaim neverCancel 5 rungs ctx unit "def f(): pass\n" `shouldReturn` Just (CheckWrong "no reason given")
  it "(i) every chat rung asked is a decision; only the last can have chosen" $
    withAnswers "{\"answers\": {\"plan-check\": {\"choice\": \"right\", \"confidence\": 0.1}}}" $ \file -> do
      let rungs = Rungs Nothing [(Rung "local" EscalateAuto, OpenAICompatible "http://127.0.0.1:1/v1" "x"), (Rung "big" EscalateAuto, OpenAICompatible "http://127.0.0.1:1/v1" "y")] "jev" (Just (\_ -> recordedDecider file neverCancel))
      (check, ds) <- checkClaim neverCancel 5 rungs ctx unit "def f(): pass\n"
      check `shouldBe` Nothing
      map dcRung ds `shouldBe` ["local", "big"]
      map dcChosen ds `shouldBe` [Nothing, Just "right"]
  it "(f) an ask rung, unforced, asks nothing" $
    withAnswers "{\"answers\": {\"plan-check\": {\"choice\": \"right\", \"confidence\": 0.9}}}" $ \file -> do
      (s, p) <- claimsServer "[model.backends.local]\nkind = \"openai-compatible\"\nurl = \"http://127.0.0.1:1/v1\"\nmodel = \"x\"\nescalate = \"ask\"\n[plans]\nladder = [\"local\"]\n" (Just file)
      let rungs = rungsFor s p False
      length (rgChat rungs) `shouldBe` 0
      checkClaim neverCancel 5 rungs ctx unit "def f(): pass\n" `shouldReturn` (Nothing, [])
      doesFileExist (file <> ".asked") `shouldReturn` False
      length (rgChat (rungsFor s p True)) `shouldBe` 1

withAnswers :: String -> (FilePath -> IO a) -> IO a
withAnswers answers body = withSystemTempDirectory "olifant-claims" $ \dir -> do
  let file = dir </> "answers.json"
  writeFile file answers
  body file

-- | A project over the python-htmx pack in memory, its config extended by
-- EXTRA, chat rungs answering from ANSWERS.
claimsServer :: Text -> Maybe FilePath -> IO (ServerState, Project)
claimsServer extra answers = do
  pack <- readTree =<< canonicalizePath "packs/python-htmx"
  (disk, _) <-
    memoryWorkspace $
      [("/pack" </> p, t) | (p, t) <- M.toList pack]
        ++ [ ("/r1/.olifant/config.toml", "[packs]\npython-htmx = { path = \"/pack\" }\n" <> extra)
           , ("/r1/app/x.py", "def f():\n    return 1\n")
           , ("/r1/plans/t.md", "# T\n") ]
  let syntax = querying (\_ _ _ -> pure (Right [Match (0, 0) (1, 0) "" (0, 0)]))
      ports = Ports disk syntax (const noChecker) (const noVerifier) (const noRunner) (\_ _ -> noRunner) (pure at0) Nothing Nothing (DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False) (\_ -> pure Nothing) (\_ -> pure []) (\_ -> pure (Left NoSource))) (\_ -> pure Nothing) (\_ _ _ -> pure (Left "no base in tests")) (\_ _ -> pure (Left "no base in tests")) gitStateStamp
  s0 <- newServerState ports
  projectFor "/r1/plans/t.md" s0 {ssConfig = (ssConfig s0) {scChatAnswers = answers}} >>= \case
    (s, Just (_, p)) -> pure (s, p)
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
