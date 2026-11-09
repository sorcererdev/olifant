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
import Olifant.Server.Checker (Citation (..), Confirmed (..), Judgement (..), UnitRef (..))
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
claim = SentenceEntry (Triage "claim" 0.9 0.9 "jev") Verify Nothing Nothing at0
aside = SentenceEntry (Triage "prose" 0.9 0.1 "jev") Aside Nothing Nothing at0

checked :: Judgement -> SentenceEntry
checked j = claim {seCheck = Just (CheckRecord (Confirmed j "it drops D" [Citation "app/x.py" (Just "f") (Just 2)] (Just (UnitRef "app/x.py" "f")) (Just "abc1234")) "claude-sonnet-5-5" (Just 0.1) 17.5 at0)}

texts :: [Sentence] -> [Text]
texts = map snText

spec :: Spec
spec = describe "Olifant.Server.Claims" $ do
  it "(a) a store round-trips through writeClaims and readClaims, as version 2" $ do
    (s, _) <- claimsServer
    let store =
          M.fromList
            [ ("The loader keeps D.", checked JWrong)
            , ("A step", SentenceEntry (Triage "step" 1 1 "grammar") Progress Nothing Nothing at0)
            , ("Failed one.", claim {seFailure = Just (Failure "timed out after 120 s" at0)})
            , ("Prose here.", aside) ]
    writeClaims (ssPorts s) "/r1" "plans/t.md" store
    readClaims (ssPorts s) "/r1" "plans/t.md" `shouldReturn` store
    Just body <- readText (pFiles (ssPorts s)) (claimsPath "/r1" "plans/t.md")
    body `shouldSatisfy` T.isInfixOf "\"version\":2"
    claimsPath "/r1" "plans/t.md" `shouldBe` "/r1/.olifant/state/claims/plans/t.json"
  it "(b) a corrupt, missing or version-1 file reads as empty" $ do
    (s, _) <- claimsServer
    readClaims (ssPorts s) "/r1" "plans/none.md" `shouldReturn` M.empty
    writeText (pFiles (ssPorts s)) (claimsPath "/r1" "plans/t.md") "{not json"
    readClaims (ssPorts s) "/r1" "plans/t.md" `shouldReturn` M.empty
    writeText (pFiles (ssPorts s)) (claimsPath "/r1" "plans/t.md") "{\"version\": 1, \"claims\": {\"the loader keeps D\": {\"unit\": null, \"rung\": \"none\", \"candidates\": [], \"top\": 0, \"check\": null, \"fingerprint\": null, \"at\": \"2026-11-07T00:00:00Z\"}}}"
    readClaims (ssPorts s) "/r1" "plans/t.md" `shouldReturn` M.empty
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
  it "(j) pruneClaims keeps the keys of sentences still there; a heading rename keeps them" $ do
    let store = M.fromList [("The loader keeps D.", claim), ("Gone now.", claim)]
        ss = sentences (parsePlan "## Renamed\nThe loader keeps D.\n")
    M.keys (pruneClaims ss store) `shouldBe` ["The loader keeps D."]
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
