module Olifant.Server.TelemetrySpec (spec) where

import Olifant.Language.Grammar (GrammarFailure (NoSource))
import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (throwIO)
import Data.Aeson (Value (..), encode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.App (Ports (..))
import Olifant.Doctor (DoctorPorts (..))
import Olifant.Language (noChecker, noRunner, querying)
import Olifant.Language.Check (noVerifier)
import Olifant.Process (gitStateStamp)
import Olifant.Server.Errand (ModelUse (..), Status (..), Tokens (..))
import Olifant.Server.Telemetry
import Olifant.Workspace (Workspace (..), memoryWorkspace)
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Server.Telemetry (errand records)" $ do
  it "round-trips an errand record through JSON with the spec's names" $ do
    let r = AnErrand sampleErrand
    o <- objectOf (recordJson r)
    sort (KM.keys o) `shouldBe` sort ["kind", "id", "at", "verb", "plan", "row", "section", "branch", "model", "status", "session", "seconds", "turns", "tokens", "cost", "models", "prompt_chars", "grounded", "blocks", "lines", "dropped", "unresolved", "changed", "denials"]
    KM.lookup "why" o `shouldBe` Nothing
    KM.lookup "kind" o `shouldBe` Just "errand"
    KM.lookup "tokens" o `shouldBe` Just (Object (KM.fromList [("input", Number 10), ("output", Number 20), ("cache_write", Number 30), ("cache_read", Number 40)]))
    parseRecord (recordJson r) `shouldBe` Just r
  it "a reopened errand names its parent; one without has no parent key" $ do
    let r = AnErrand sampleErrand {erParent = Just "s0"}
    o <- objectOf (recordJson r)
    KM.lookup "parent" o `shouldBe` Just "s0"
    parseRecord (recordJson r) `shouldBe` Just r
    o' <- objectOf (recordJson (AnErrand sampleErrand))
    KM.lookup "parent" o' `shouldBe` Nothing
  it "a failed errand carries why and no session" $ do
    o <- objectOf (recordJson (AnErrand sampleErrand {erStatus = Timeout, erWhy = Just "timed out after 300 s", erSession = Nothing, erCost = Nothing}))
    (KM.lookup "status" o, KM.lookup "why" o, KM.member "session" o, KM.member "cost" o) `shouldBe` (Just "timeout", Just "timed out after 300 s", False, False)
  it "an errand with every Maybe absent round-trips" $ do
    let r = AnErrand sampleErrand {erBranch = Nothing, erStatus = Cancelled, erWhy = Just "cancelled", erSession = Nothing, erSeconds = Nothing, erTurns = Nothing, erTokens = Nothing, erCost = Nothing, erModels = []}
    parseRecord (recordJson r) `shouldBe` Just r
  it "a reconcile record round-trips with the spec's names" $ do
    let r = AReconcile sampleReconcile
    o <- objectOf (recordJson r)
    sort (KM.keys o) `shouldBe` sort ["kind", "id", "at", "plan", "branch", "baseline", "forced", "changed", "unaccounted", "more", "facts", "seconds", "climbs", "decisions", "blocks"]
    KM.lookup "kind" o `shouldBe` Just "reconcile"
    parseRecord (recordJson r) `shouldBe` Just r
    let bare = AReconcile sampleReconcile {rcBranch = Nothing, rcBaseline = Nothing, rcClimbs = [], rcDecisions = [], rcBlocks = []}
    parseRecord (recordJson bare) `shouldBe` Just bare
  it "a resolve record round-trips" $ do
    let r = AResolve sampleResolve
    o <- objectOf (recordJson r)
    KM.lookup "kind" o `shouldBe` Just "resolve"
    parseRecord (recordJson r) `shouldBe` Just r
  it "a pass record round-trips; an automatic check errand says auto" $ do
    let r = APass (PassRecord "pass-plans/x.md:0@x" (UTCTime (fromGregorian 2026 11 8) 0) "plans/x.md" "save" 12 3 [("aside", 7), ("verify", 5)] 2 1 0 1 1 3 34.5 3 0.21)
    o <- objectOf (recordJson r)
    KM.lookup "kind" o `shouldBe` Just "pass"
    parseRecord (recordJson r) `shouldBe` Just r
    e <- objectOf (recordJson (AnErrand sampleErrand {erVerb = "check", erAuto = True}))
    KM.lookup "auto" e `shouldBe` Just (Bool True)
    parseRecord (recordJson (AnErrand sampleErrand {erAuto = True})) `shouldBe` Just (AnErrand sampleErrand {erAuto = True})
  it "a check errand's mode and verdict round-trip, and are left out when unknown" $ do
    let r = AnErrand sampleErrand {erVerb = "check", erMode = Just "progress", erVerdict = Just "misfit"}
    o <- objectOf (recordJson r)
    KM.lookup "mode" o `shouldBe` Just "progress"
    KM.lookup "verdict" o `shouldBe` Just "misfit"
    parseRecord (recordJson r) `shouldBe` Just r
    e <- objectOf (recordJson (AnErrand sampleErrand))
    KM.lookup "mode" e `shouldBe` Nothing
  it "an outcome record round-trips" $
    parseRecord (recordJson (AnOutcome (OutcomeRecord "s1" 0 t0 "accepted"))) `shouldBe` Just (AnOutcome (OutcomeRecord "s1" 0 t0 "accepted"))
  it "a record's at is whole-second ISO-8601 UTC, and parses back" $ do
    let at = UTCTime (fromGregorian 2026 10 25) 3600.25
    e <- objectOf (recordJson (AnErrand sampleErrand {erAt = at}))
    o <- objectOf (recordJson (AnOutcome (OutcomeRecord "s1" 0 at "accepted")))
    (KM.lookup "at" e, KM.lookup "at" o) `shouldBe` (Just "2026-10-25T01:00:00Z", Just "2026-10-25T01:00:00Z")
    (orAt <$> (parseRecord (Object o) >>= \case AnOutcome r -> Just r; _ -> Nothing)) `shouldBe` Just (UTCTime (fromGregorian 2026 10 25) 3600)
  it "an unknown kind or status does not parse" $ do
    parseRecord (Object (KM.fromList [("kind", "other"), ("id", "x")])) `shouldBe` Nothing
    o <- objectOf (recordJson (AnErrand sampleErrand))
    parseRecord (Object (KM.insert "status" "weird" o)) `shouldBe` Nothing
  it "readRecords skips bad lines and counts them" $
    let (rs, bad) = readRecords (T.unlines [enc (AnOutcome o1), "{\"kind\":\"errand\"", "not json", enc (AnOutcome o2)])
     in (rs, bad) `shouldBe` ([AnOutcome o1, AnOutcome o2], 2)
  it "appends one line per record under .olifant/state" $ do
    (ports, snap) <- memoryPorts
    appendRecord ports "/proj" (AnOutcome o1)
    appendRecord ports "/proj" (AnErrand sampleErrand)
    files <- snap
    fmap readRecords (M.lookup (recordsPath "/proj") files) `shouldBe` Just ([AnOutcome o1, AnErrand sampleErrand], 0)
    recordsPath "/proj" `shouldBe` "/proj/.olifant/state/errands.jsonl"
  it "twenty concurrent appends all land" $ do
    (ports, snap) <- memoryPorts
    mapConcurrently_ (\i -> appendRecord ports "/proj" (AnOutcome (OutcomeRecord "s" i t0 "accepted"))) [0 .. 19]
    Just txt <- M.lookup "/proj/.olifant/state/errands.jsonl" <$> snap
    length (T.lines txt) `shouldBe` 20
  it "a workspace that cannot write does not throw" $ do
    (ports, _) <- memoryPorts
    let ws = pFiles ports
    appendRecord ports {pFiles = ws {writeText = \_ _ -> throwIO (userError "disk full")}} "/proj" (AnOutcome o1)
    appendRecord ports {pFiles = ws {readText = \_ -> throwIO (userError "unreadable")}} "/proj" (AnOutcome o1)
  it "errandId is the session when known, else verb-plan:row@time" $ do
    errandId t0 "ask" "plans/x.md" 5 (Just "abc") `shouldBe` "abc"
    errandId t0 "ask" "plans/x.md" 5 Nothing `shouldBe` "ask-plans/x.md:5@2026-10-25T00:00:00Z"

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 10 25) 0

o1, o2 :: OutcomeRecord
o1 = OutcomeRecord "s1" 0 t0 "accepted"
o2 = OutcomeRecord "s1" 1 t0 "rejected"

sampleErrand :: ErrandRecord
sampleErrand =
  ErrandRecord
    { erId = "sess-1", erAt = UTCTime (fromGregorian 2026 10 25) 3600, erVerb = "ask", erPlan = "plans/x.md", erRow = 5
    , erSection = "Steps", erBranch = Just "errand-telemetry", erModel = "sonnet", erStatus = Ok, erWhy = Nothing
    , erSession = Just "sess-1", erSeconds = Just 4.25, erTurns = Just 3, erTokens = Just (Tokens 10 20 30 40)
    , erCost = Just 0.054, erModels = [("claude-haiku-5-5", ModelUse (Tokens 1 2 3 4) (Just 0.001)), ("claude-sonnet-5-5", ModelUse (Tokens 9 18 27 36) Nothing)]
    , erPromptChars = 1234, erGrounded = 2, erBlocks = 2, erLines = 7, erDropped = 1, erUnresolved = 0, erChanged = 0, erDenials = 0, erParent = Nothing, erAuto = False, erMode = Nothing, erVerdict = Nothing }

sampleReconcile :: ReconcileRecord
sampleReconcile =
  ReconcileRecord
    { rcId = "reconcile-plans/x.md:0@2026-10-25T01:00:00Z", rcAt = UTCTime (fromGregorian 2026 10 25) 3600, rcPlan = "plans/x.md"
    , rcBranch = Just "plans-loop", rcBaseline = Just "abc", rcForced = True, rcChanged = 3, rcUnaccounted = 2, rcMore = 1, rcFacts = 4
    , rcSeconds = 1.5, rcClimbs = [("jev", 0.25), ("local", 1.0)]
    , rcDecisions = [Decision "plan-step" ["s7", "none"] (Just "s7") (Just 0.9) "jev", Decision "plan-state" ["done", "in_progress", "diverged"] Nothing Nothing "local"]
    , rcBlocks = [Just ("src/a.py", "F", "abc1234"), Nothing] }

sampleResolve :: ResolveRecord
sampleResolve =
  ResolveRecord
    { rvId = "resolve-plans/x.md:0@2026-11-07T01:00:00Z", rvAt = UTCTime (fromGregorian 2026 11 7) 3600, rvPlan = "plans/x.md"
    , rvSeen = 4, rvByRung = [("jev-1", 2), ("jev-3", 1), ("none", 1)], rvChecked = 2, rvRight = 1, rvWrong = 1, rvElsewhere = 0
    , rvMore = 1, rvSeconds = 2.5, rvCalls = 6
    , rvDecisions = [Decision "plan-unit" ["a.py:f", "a.py:g"] (Just "a.py:f") (Just 0.9) "jev-1"] }

enc :: Record -> Text
enc = TE.decodeUtf8 . BL.toStrict . encode . recordJson

objectOf :: Value -> IO (KM.KeyMap Value)
objectOf (Object o) = pure o
objectOf v = expectationFailure ("not an object: " <> show v) >> fail "not an object"

memoryPorts :: IO (Ports, IO (Map FilePath Text))
memoryPorts = do
  (disk, snap) <- memoryWorkspace []
  let ports = Ports disk (querying (\_ _ _ -> pure (Right []))) (const noChecker) (const noVerifier) (const noRunner) (\_ _ -> noRunner) (pure t0) Nothing Nothing (DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False) (\_ -> pure Nothing) (\_ -> pure []) (\_ -> pure (Left NoSource))) (\_ -> pure Nothing) (\_ _ _ -> pure (Left "no base in tests")) (\_ _ -> pure (Left "no base in tests")) gitStateStamp
  pure (ports, snap)
