module Olifant.Server.ClaimsSpec (spec) where

import Control.Concurrent.Async (async, wait)
import Control.Monad (forM)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Olifant.App (Ports (..), Project)
import Olifant.Doctor (DoctorPorts (..))
import Olifant.Language (Match (..), noChecker, noRunner, querying)
import Olifant.Language.Check (noVerifier)
import Olifant.Language.Grammar (GrammarFailure (NoSource))
import Olifant.Plans (parsePlan)
import Olifant.Plans.Sentences (Sentence (..), sentences)
import Olifant.Process (gitStateStamp)
import Olifant.Server (ServerState, newServerState, projectFor)
import Olifant.Server.Checker (CheckMode (..), Citation (..), Confirmed (..), Judgement (..), Print (..), UnitRef (..))
import Olifant.Server.Claims
import Olifant.Server.State (ServerState (..))
import Olifant.Server.Triage (Route (..), Triage (..))
import Olifant.Workspace (Workspace (..), memoryWorkspace)
import System.Directory (canonicalizePath, doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, (</>))
import Test.Hspec

at0 :: UTCTime
at0 = UTCTime (fromGregorian 2026 11 7) 0

claim, aside :: SentenceEntry
claim = SentenceEntry (Triage "claim" 0.9 0.9 "jev") Verify Nothing Nothing at0 Nothing False
aside = SentenceEntry (Triage "prose" 0.9 0.1 "jev") Aside Nothing Nothing at0 Nothing False

checked :: Judgement -> SentenceEntry
checked j = claim {seCheck = Just (CheckRecord (Confirmed j "it drops D" [Citation "app/x.py" (Just "f") (Just 2)] (Just (UnitRef "app/x.py" "f")) (Just "abc1234") []) "claude-sonnet-5-5" (Just 0.1) 17.5 at0 CheckClaim)}

texts :: [Sentence] -> [Text]
texts = map snText

spec :: Spec
spec = describe "Olifant.Server.Claims" $ do
  it "(a) a store round-trips through writeClaims and readClaims, as version 2" $ do
    (s, _) <- claimsServer
    let store =
          M.fromList
            [ ("The loader keeps D.", checked JWrong)
            , ("A step", SentenceEntry (Triage "step" 1 1 "grammar") Progress Nothing Nothing at0 Nothing False)
            , ("Failed one.", claim {seFailure = Just (Failure "timed out after 120 s" at0)})
            , ("Prose here.", aside)
            , ("Edited away.", claim {seGone = Just at0}) ]
    writeClaims (ssPorts s) "/r1" "plans/t.md" store
    readClaims (ssPorts s) "/r1" "plans/t.md" `shouldReturn` store
    Just body <- readText (pFiles (ssPorts s)) (claimsPath "/r1" "plans/t.md")
    body `shouldSatisfy` T.isInfixOf "\"version\":2"
    claimsPath "/r1" "plans/t.md" `shouldBe` "/r1/.olifant/state/claims/plans/t.json"
  it "(a2) an owed entry and a progress check with prints round-trip" $ do
    (s, _) <- claimsServer
    let prints = [Print "app/x.py" (Just "f") "abc1234", Print "app/y.py" Nothing "0f0f0f0"]
        misfit = CheckRecord (Confirmed JMisfit "is_staff misses the Staff role" [Citation "app/x.py" (Just "f") Nothing, Citation "app/y.py" Nothing Nothing] (Just (UnitRef "app/x.py" "f")) (Just "abc1234") prints) "sonnet" (Just 0.1) 12 at0 CheckProgress
        store = M.fromList
          [ ("Owed one.", claim {seOwed = True})
          , ("A step", (SentenceEntry (Triage "step" 1 1 "grammar") Progress Nothing Nothing at0 Nothing False) {seCheck = Just misfit}) ]
    writeClaims (ssPorts s) "/r1" "plans/t.md" store
    readClaims (ssPorts s) "/r1" "plans/t.md" `shouldReturn` store
    Just body <- readText (pFiles (ssPorts s)) (claimsPath "/r1" "plans/t.md")
    body `shouldSatisfy` T.isInfixOf "\"owed\":true"
    body `shouldSatisfy` T.isInfixOf "\"mode\":\"progress\""
    body `shouldSatisfy` T.isInfixOf "\"unit\":null"
  it "(a3) a milestone 1 store loads: not owed, its checks claims with no prints" $ do
    (s, _) <- claimsServer
    writeText (pFiles (ssPorts s)) (claimsPath "/r1" "plans/t.md") "{\"version\": 2, \"claims\": {\"x\": {\"triage\": {\"kind\": \"claim\", \"p\": 0.9, \"about\": 0.9, \"by\": \"jev\"}, \"route\": \"verify\", \"check\": {\"verdict\": \"right\", \"why\": \"w\", \"evidence\": [{\"path\": \"a.py\", \"unit\": \"f\", \"line\": null}], \"unit\": {\"path\": \"a.py\", \"name\": \"f\"}, \"fingerprint\": \"abc1234\", \"model\": \"sonnet\", \"cost\": 0.1, \"seconds\": 3, \"at\": \"2026-11-07T00:00:00Z\"}, \"failure\": null, \"at\": \"2026-11-07T00:00:00Z\", \"gone\": null}}}"
    store <- readClaims (ssPorts s) "/r1" "plans/t.md"
    let Just e = M.lookup "x" store
        Just c = seCheck e
    seOwed e `shouldBe` False
    cjMode c `shouldBe` CheckClaim
    cfPrints (cjConfirmed c) `shouldBe` []
    cfJudgement (cjConfirmed c) `shouldBe` JRight
  it "(b) a corrupt, missing or version-1 file reads as empty" $ do
    (s, _) <- claimsServer
    readClaims (ssPorts s) "/r1" "plans/none.md" `shouldReturn` M.empty
    writeText (pFiles (ssPorts s)) (claimsPath "/r1" "plans/t.md") "{not json"
    readClaims (ssPorts s) "/r1" "plans/t.md" `shouldReturn` M.empty
    writeText (pFiles (ssPorts s)) (claimsPath "/r1" "plans/t.md") "{\"version\": 1, \"claims\": {\"the loader keeps D\": {\"unit\": null, \"rung\": \"none\", \"candidates\": [], \"top\": 0, \"check\": null, \"fingerprint\": null, \"at\": \"2026-11-07T00:00:00Z\"}}}"
    readClaims (ssPorts s) "/r1" "plans/t.md" `shouldReturn` M.empty
  it "(o) the joins ride in the same file, optional, still version 2; each writer keeps the other's" $ do
    (s, _) <- claimsServer
    let store = M.fromList [("a", aside)]
        joins = M.fromList [("We would have to ⏎ redeploy everything.", True), ("x ⏎ y", False)]
    writeClaimsWith (ssPorts s) "/r1" "plans/t.md" store joins
    readClaims (ssPorts s) "/r1" "plans/t.md" `shouldReturn` store
    readJoins (ssPorts s) "/r1" "plans/t.md" `shouldReturn` joins
    Just body <- readText (pFiles (ssPorts s)) (claimsPath "/r1" "plans/t.md")
    body `shouldSatisfy` T.isInfixOf "\"version\":2"
    -- a store written without joins reads as no joins
    writeClaims (ssPorts s) "/r1" "plans/u.md" store
    readJoins (ssPorts s) "/r1" "plans/u.md" `shouldReturn` M.empty
    -- the held store and joins: updating one writes the other unchanged
    _ <- updateJoins s "/r1" "plans/t.md" (M.insert "p ⏎ q" True)
    _ <- updateClaims s "/r1" "plans/t.md" (M.insert "b" aside)
    readJoins (ssPorts s) "/r1" "plans/t.md" `shouldReturn` M.insert "p ⏎ q" True joins
    M.keys <$> readClaims (ssPorts s) "/r1" "plans/t.md" `shouldReturn` ["a", "b"]
    joinsOf s "/r1" "plans/t.md" `shouldReturn` M.insert "p ⏎ q" True joins
  it "(g) a store is written whole or not at all: to a temporary file, then renamed over" $ do
    (s, _) <- claimsServer
    wrote <- newIORef []
    let ws = pFiles (ssPorts s)
        ports = (ssPorts s) {pFiles = ws {writeText = \p x -> modifyIORef wrote (p :) >> writeText ws p x}}
        store = M.fromList [("a", aside)]
    writeClaims ports "/r1" "plans/t.md" store
    readIORef wrote `shouldReturn` [claimsPath "/r1" "plans/t.md" <> ".tmp"]
    readClaims ports "/r1" "plans/t.md" `shouldReturn` store
    fileExists ws (claimsPath "/r1" "plans/t.md" <> ".tmp") `shouldReturn` False
  it "(n) updateClaims changes the store as it is now: concurrent writers lose nothing" $ do
    (s, _) <- claimsServer
    let keys = [T.pack ("k" <> show i) | i <- [1 .. 40 :: Int]]
    done <- mapM (\k -> async (updateClaims s "/r1" "plans/t.md" (M.insert k aside))) keys
    mapM_ wait done
    M.keys <$> claimsOf s "/r1" "plans/t.md" `shouldReturn` M.keys (M.fromList [(k, ()) | k <- keys])
    M.size <$> readClaims (ssPorts s) "/r1" "plans/t.md" `shouldReturn` 40
  it "(h) pruneOrphanClaims removes the stores of plans that no longer exist" $ do
    (s, _) <- claimsServer
    let store = M.fromList [("a", aside)]
    writeClaims (ssPorts s) "/r1" "plans/t.md" store
    writeClaims (ssPorts s) "/r1" "plans/gone.md" store
    writeClaims (ssPorts s) "/r1" "plans/sub/deeper.md" store
    pruneOrphanClaims (ssPorts s) "/r1"
    fileExists (pFiles (ssPorts s)) (claimsPath "/r1" "plans/t.md") `shouldReturn` True
    fileExists (pFiles (ssPorts s)) (claimsPath "/r1" "plans/gone.md") `shouldReturn` False
    fileExists (pFiles (ssPorts s)) (claimsPath "/r1" "plans/sub/deeper.md") `shouldReturn` False
  it "(j) pruneClaims keeps a sentence gone from the plan for a day, marked gone; a heading rename keeps it" $ do
    let store = M.fromList [("The loader keeps D.", claim), ("Gone now.", checked JRight), ("Gone long ago.", claim {seGone = Just (addUTCTime (-86401) at0)})]
        ss = sentences (parsePlan "## Renamed\nThe loader keeps D.\n")
        pruned = pruneClaims at0 ss store
    M.keys pruned `shouldBe` ["Gone now.", "The loader keeps D."]
    fmap seGone (M.lookup "Gone now." pruned) `shouldBe` Just (Just at0)
    fmap seGone (M.lookup "The loader keeps D." pruned) `shouldBe` Just Nothing
    -- back in the plan an hour later (an undo): no longer gone, its check kept
    let back = pruneClaims (addUTCTime 3600 at0) (sentences (parsePlan "Gone now.\n")) pruned
    fmap (\e -> (seGone e, seCheck e == seCheck (checked JRight))) (M.lookup "Gone now." back) `shouldBe` Just (Nothing, True)
  it "(k) queued: verify sentences unchecked and not failed lately, in plan order, one per key" $ do
    let ss = sentences (parsePlan (T.unlines ["- Prose here.", "- Done one.", "- New one.", "- Failed now.", "- Failed long ago.", "- Asked one.", "- A step", "- Two.", "- Two.", "- Unknown."]))
        old = addUTCTime (-660) at0
        store =
          M.fromList
            [ ("Prose here.", aside), ("Done one.", checked JRight), ("New one.", claim)
            , ("Failed now.", claim {seFailure = Just (Failure "x" (addUTCTime (-60) at0))})
            , ("Failed long ago.", claim {seFailure = Just (Failure "x" old)})
            , ("Asked one.", aside {seRoute = Asked}), ("A step", aside {seRoute = Progress}), ("Two.", claim) ]
    texts (queued at0 ss store) `shouldBe` ["New one.", "Failed long ago.", "Two."]
  it "(l) checkedKeys holds right and wrong, not cannot tell" $
    checkedKeys (M.fromList [("a", checked JRight), ("b", checked JWrong), ("c", checked JCannotTell), ("d", claim)]) `shouldBe` M.keysSet (M.fromList [("a", ()), ("b", ())])
  it "(m) lineKey is a bullet's words after its marker" $
    map lineKey (parsePlan "- [ ] Do it @a.py:F\n  - The  loader keeps D.\n") `shouldBe` ["Do it", "The loader keeps D."]

-- | A project over the python-htmx pack in memory.
claimsServer :: IO (ServerState, Project)
claimsServer = do
  pack <- readTree =<< canonicalizePath "packs/python-htmx"
  (disk, _) <-
    memoryWorkspace $
      [("/pack" </> p, t) | (p, t) <- M.toList pack]
        ++ [ ("/r1/.olifant/config.toml", "[packs]\npython-htmx = { path = \"/pack\" }\n")
           , ("/r1/app/x.py", "def f():\n    return 1\n")
           , ("/r1/plans/t.md", "# T\n") ]
  let syntax = querying (\_ _ _ -> pure (Right [Match (0, 0) (1, 0) "" (0, 0)]))
      ports = Ports disk syntax (const noChecker) (const noVerifier) (const noRunner) (\_ _ -> noRunner) (pure at0) Nothing Nothing (DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False) (\_ -> pure Nothing) (\_ -> pure []) (\_ -> pure (Left NoSource))) (\_ -> pure Nothing) (\_ _ _ -> pure (Left "no base in tests")) (\_ _ -> pure (Left "no base in tests")) gitStateStamp
  s0 <- newServerState ports
  projectFor "/r1/plans/t.md" s0 >>= \case
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
