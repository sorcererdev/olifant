module Olifant.Server.ConjureNextSpec
  ( spec
  , cb
  , succeedsId
  , notFoundId
  , cancelTestText
  , pyHtmx
  , cancelHeader
  , cancelBefore
  , cancelRecords
  , cancelState
  , round1Run
  , round2Run
  , cancelModule
  ) where

import Data.Aeson (Value (..), eitherDecodeFileStrict, encode, toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (toList)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Olifant.Server.Edits (unifiedDiff)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import Olifant.Decide
import Olifant.Generation.Steer
import Olifant.Language
import Olifant.Model (Escalate (..), Rung (..))
import Olifant.Server.ConjureNext
import Olifant.Server.ConjureScan (Fake (..))
import Olifant.Generation.Tools (ProgressEvent (..))
import Olifant.Server.Unit (ownerHeader)
import System.IO.Unsafe (unsafePerformIO)
import Olifant.Language.TestUnits (testSourcesIn)
import Test.Hspec

spec :: Spec
spec = describe "conjure-next (Olifant.Server.ConjureNext)" $ do
  let run = TestRun ["t.py::test_b"] [("t.py::test_a", Failure (Just 6) "assert 0 == 1\nmore" "")] Nothing
      hole = RoundHole "body" "the tests failed:" [("return 0", "w1")] (Just run)
      state = RoundState 1 "qwen" [hole]
      ctx = RoundContext "def test_a():\n    assert 0 == 1\n" "class C:\n    def __init__(self):" ["x: X"] False [] (TestRun [] [] Nothing) "t" (testSourcesIn pyNow "def test_a():\n    assert 0 == 1\n")
      -- the same attempt again, a round later
      state2 = RoundState 2 "qwen" [hole]
      viewAgain = roundView ctx {rcHistory = [roundRecord state]} (Just view) state2
      view = roundView ctx Nothing state
      viewB = roundView ctx Nothing (RoundState 1 "qwen" [hole {rhRun = Nothing, rhWhy = "does not parse as block"}])
      nextRung e = Just (Rung "claude" e)
      standing e = Standing 3 50 (nextRung e) Nothing False
      keys = ["round", "rung", "rounds_left", "attempts", "failing", "passing", "trade", "repeat", "tests", "unit", "hint"]
  it "(a) roundView lists failures, counts passes and spots a repeat" $ do
    rvFailing view `shouldBe` [("t.py::test_a", "assert 0 == 1")]
    rvPassing view `shouldBe` 1
    rvCheck view `shouldBe` Nothing
    rvRepeat view `shouldBe` False
    rvRepeat viewAgain `shouldBe` True
    rvPassingDelta viewAgain `shouldBe` Just 0
  it "(b) a round with no verdict carries the check" $ do
    rvCheck viewB `shouldBe` Just "does not parse as block"
    rvFailing viewB `shouldBe` []
    let broken = roundView ctx Nothing (RoundState 1 "qwen" [hole {rhRun = Just (TestRun [] [] (Just "ImportError: x"))}])
    rvCheck broken `shouldBe` Just "ImportError: x"
  it "(c) attemptDiff drops the common lines" $ do
    attemptDiff (Just "a\nb\nc") "a\nx\nc" `shouldBe` "@@ line 2\n- b\n+ x"
    attemptDiff Nothing "a\nb" `shouldBe` "+ a\n+ b"
    attemptDiff (Just "a") "a" `shouldBe` "(the same as the previous attempt)"
  it "(d) undefinedNames reads python and pyright" $ do
    undefinedNames "E   NameError: name 'date' is not defined" `shouldBe` ["date"]
    undefinedNames "src/a.py: \"Booking\" is not defined; src/a.py: \"Booking\" is not defined" `shouldBe` ["Booking"]
    undefinedNames "assert 0 == 1" `shouldBe` []
  it "(e) the question offers again, climb and ask, with eleven context keys" $ do
    let q = conjureNextQuestion (standing EscalateAsk) view
    qKind q `shouldBe` "conjure-next"
    map fst (qOptions' q) `shouldBe` ["again", "climb", "ask"]
    map fst (qContext q) `shouldBe` keys
    lookup "hint" (qContext q) `shouldBe` Just "none"
    lookup "repeat" (qContext q) `shouldBe` Just "no"
    lookup "repeat" (qContext (conjureNextQuestion (standing EscalateAsk) viewAgain)) `shouldBe` Just "yes"
    map fst (qOptions' (conjureNextQuestion (standing EscalateNever) view)) `shouldBe` ["again", "ask"]
    map fst (qOptions' (conjureNextQuestion (Standing 3 50 Nothing Nothing False) view)) `shouldBe` ["again", "ask"]
  it "(f) spent when rounds or seconds run out" $ do
    spent (Standing 0 50 Nothing Nothing False) `shouldBe` True
    spent (Standing 3 (-1) (Just (Rung "claude" EscalateNever)) Nothing False) `shouldBe` True
    spent (Standing 3 50 Nothing Nothing False) `shouldBe` False
  it "(g) nextFrom follows a confident answer and otherwise goes again" $ do
    let st = standing EscalateAsk
    nextFrom st 2 (Chosen "again" [] (Confidence 0.9)) `shouldBe` NextAgain
    nextFrom st 2 (Chosen "climb" [] (Confidence 0.9)) `shouldBe` NextPause (CauseClimb "claude")
    nextFrom (standing EscalateAuto) 2 (Chosen "climb" [] (Confidence 0.9)) `shouldBe` NextClimb "claude"
    nextFrom st 2 (Chosen "ask" [] (Confidence 0.9)) `shouldBe` NextPause CauseAsk
    nextFrom st 2 (Chosen "ask" [] Unknown) `shouldBe` NextAgain
    nextFrom st 2 Unsure `shouldBe` NextAgain
    -- one round cannot show convergence: an ask after round 1 goes on
    nextFrom st 1 (Chosen "ask" [] (Confidence 0.9)) `shouldBe` NextAgain
    nextFrom st 1 (Chosen "climb" [] (Confidence 0.9)) `shouldBe` NextPause (CauseClimb "claude")
  it "(h0) roundSummary and offerText" $ do
    roundSummary view `shouldBe` "red: test_a \183 assert 0 == 1"
    roundSummary viewB `shouldBe` "red: does not parse as block"
    offerText (OfferClimb "claude") `shouldBe` "climb:claude"
  it "(i0) withHint adds the person's words to each hole's last attempt" $
    withHint "use the repository" (Seed 2 (M.singleton "body" [("f1", "w1"), ("f2", "w2")]))
      `shouldBe` Seed 2 (M.singleton "body" [("f1", "w1"), ("f2", "w2\nThe person says: use the repository")])
  it "(h) roundView carries each failing test's source, the unit's header and what is wired" $ do
    rvTests view `shouldBe` [("t.py::test_a", "def test_a():\n    assert 0 == 1")]
    let other = RoundHole "body" "x" [] (Just (TestRun [] [("t.py::test_zz", Failure Nothing "m" "")] Nothing))
        v2 = roundView ctx Nothing (RoundState 1 "qwen" [other])
    rvTests v2 `shouldBe` [("t.py::test_zz", "(source not found)")]
    rvUnit view `shouldBe` rcUnit ctx
    rvWired view `shouldBe` rcWired ctx
  it "(i) unitHeader gives the class and __init__ lines for a method, the def for a function" $ do
    ownerHeader pyNow "class C:\n    def __init__(self, a: A) -> None:\n        pass\n    def handle(self):\n        pass\n" 5
      `shouldBe` "class C:\n    def __init__(self, a: A) -> None:"
    ownerHeader pyNow "def f():\n    pass\n" 2 `shouldBe` "def f():"
    ownerHeader pyNow "x = 1\n" 1 `shouldBe` ""
  it "(m) the cancel_booking rounds: rows, involved tests, a repeat and a trade" $ do
    roundRows "cancel_booking" cancelRecords
      `shouldBe` [ RoundRow 1 "local" "return Ok(None)" False ["not_found"]
                 , RoundRow 2 "local" "return NotFound(reason=\"Booking not found\")" False ["succeeds"]
                 , RoundRow 3 "local" "(the same as round 2)" False ["succeeds"]
                 ]
    involved cancelBefore cancelRecords `shouldBe` [TestStatus succeedsId True (Just 1), TestStatus notFoundId False (Just 1)]
    isRepeat cancelRecords `shouldBe` True
    isTrade cancelBefore cancelRecords `shouldBe` True
    isTrade cancelBefore (take 1 cancelRecords) `shouldBe` False
    isRepeat (take 2 cancelRecords) `shouldBe` False
  it "(n) the view holds every involved test's source and the attempts" $ do
    let v = roundView (RoundContext cancelTestText cancelHeader [] False cancelRecords cancelBefore "cancel_booking" (testSourcesIn pyNow cancelTestText)) Nothing (cancelState 3)
    map fst (rvTests v) `shouldBe` [succeedsId, notFoundId]
    rvPassingTests v `shouldBe` [notFoundId]
    rvAttempts v
      `shouldBe` [ "round 1: return Ok(None) \183 red: not_found"
                 , "round 2: return NotFound(reason=\"Booking not found\") \183 red: succeeds"
                 , "round 3: (the same as round 2) \183 red: succeeds"
                 ]
    rvTrade v `shouldBe` True
    rvRepeat v `shouldBe` True
  let nf = Fake "repository" "Mock"
      cancelFacts =
        DiagnosisFacts
          { dfRung = "local"
          , dfNext = Nothing
          , dfStem = "cancel_booking"
          , dfMethod = "handle"
          , dfClass = "CancelBooking"
          , dfHeader = cancelHeader
          , dfTestText = cancelTestText
          , dfBefore = cancelBefore
          , dfHistory = cancelRecords
          , dfFlagged = [(notFoundId, nf)]
          , dfWired = []
          , dfCandidates = []
          , dfRounds = 3
          , dfSeconds = 14
          , dfWireFailed = Nothing
          , dfFailure = Nothing
          , dfSources = testSourcesIn pyNow cancelTestText
          , dfAssertion = langAssertion pyNow
          , dfConjure = langConjure pyNow
          }
  it "(o) the policy table, in order" $ do
    let base = PolicyFacts [(notFoundId, nf)] [notFoundId] [notFoundId] True True [] (Just ("bookings", "BookingRepository")) False False False Nothing
    policy base `shouldBe` PolicyWire "bookings" "BookingRepository"
    policy base {pfDerived = Nothing} `shouldBe` PolicyPause CausePort
    policy base {pfWired = ["bookings: BookingRepository"]} `shouldBe` PolicyPause (CauseTest notFoundId)
    policy base {pfWireApplies = False} `shouldBe` PolicyPause (CauseTest notFoundId)
    -- Review Focus 2: the class already takes the port (wired by hand): no wire, unsatisfiable
    policy base {pfLacksPort = False} `shouldBe` PolicyPause (CauseTest notFoundId)
    policy base {pfFailingNow = [], pfWired = ["x: X"], pfRepeat = True} `shouldBe` PolicyPause CauseRepeat
    policy base {pfFlagged = [], pfTrade = True} `shouldBe` PolicyPause CauseTrade
    policy base {pfFlagged = [], pfSpent = True} `shouldBe` PolicyPause CauseBudget
    policy base {pfFlagged = []} `shouldBe` PolicyAsk
  it "(p) the cancel_booking run as it happened: repeat, trade and the same call" $
    diagnosis cancelFacts CauseRepeat
      `shouldBe` [ "local wrote the same body in rounds 2 and 3."
                 , "Every body so far passes one test and fails the other."
                 , "Both tests call handle(CancelBookingInput(id=\"1\")) and expect different results."
                 ]
  it "(q) after the wire, round 1 red on not_found: unsatisfiable" $ do
    diagnosis cancelFacts {dfHistory = take 1 cancelRecords, dfWired = ["bookings: BookingRepository"], dfRounds = 1} (CauseTest notFoundId)
      `shouldBe` ["No body can pass not_found: it builds repository = Mock() and never hands it to command(), so handle never sees it. CancelBooking takes bookings: BookingRepository since the wire; the test should pass its fake as command(bookings=\8230)."]
    diagnosis cancelFacts {dfHistory = take 1 cancelRecords, dfRounds = 1} (CauseTest notFoundId)
      `shouldBe` ["No body can pass not_found: it builds repository = Mock() and never hands it to command(), so handle never sees it. CancelBooking() takes no ports."]
    diagnosis cancelFacts {dfHistory = take 1 cancelRecords, dfCandidates = ["bookings: BookingRepository", "ids: IdGenerator"]} CausePort
      `shouldBe` [ "not_found builds repository = Mock() for a port CancelBooking lacks; it could be bookings: BookingRepository or ids: IdGenerator."
                 , "No body can pass not_found: it builds repository = Mock() and never hands it to command(), so handle never sees it. CancelBooking() takes no ports."
                 ]
    diagnosis cancelFacts {dfHistory = take 1 cancelRecords, dfFlagged = [], dfRounds = 6, dfSeconds = 180} CauseBudget `shouldBe` ["6 rounds used (180 s); nothing passed."]
    diagnosis cancelFacts {dfHistory = take 1 cancelRecords, dfFlagged = [], dfWireFailed = Just "wire writes nothing here"} CauseAsk
      `shouldBe` ["local is not converging.", "The wire failed: wire writes nothing here."]
  it "(r) the offers, suspect first" $ do
    let st = Standing 6 180 Nothing Nothing False
    offersAt st CauseRepeat [notFoundId, succeedsId] [] 3 `shouldBe` [OfferContinue, OfferHint, OfferReconjure notFoundId, OfferReconjure succeedsId, OfferStop, OfferKeep 3]
    offersAt st {stNext = Just (Rung "claude" EscalateAsk)} CausePort [notFoundId] ["bookings: BookingRepository", "ids: IdGenerator"] 1
      `shouldBe` [OfferContinue, OfferHint, OfferClimb "claude", OfferWire "bookings" "BookingRepository", OfferWire "ids" "IdGenerator", OfferReconjure notFoundId, OfferStop, OfferKeep 1]
    offersAt st CauseRepeat [] ["bookings: BookingRepository"] 2 `shouldBe` [OfferContinue, OfferHint, OfferStop, OfferKeep 2]
    suspectFirst [(notFoundId, nf)] (involved cancelBefore cancelRecords) `shouldBe` [notFoundId, succeedsId]
    suspectFirst [] (involved cancelBefore cancelRecords) `shouldBe` [succeedsId, notFoundId]
    map offerText [OfferHint, OfferWire "bookings" "BookingRepository", OfferKeep 3] `shouldBe` ["hint", "wire:bookings: BookingRepository", "keep:3"]
    causeWords (CauseTest notFoundId) `shouldBe` ["test", notFoundId]
  it "(s) the narrowed question and its gates" $ do
    let v = roundView (RoundContext cancelTestText cancelHeader [] False cancelRecords cancelBefore "cancel_booking" (testSourcesIn pyNow cancelTestText)) Nothing (cancelState 3)
        st = Standing 3 50 Nothing Nothing False
        q = conjureNextQuestion st v
    map fst (qOptions' q) `shouldBe` ["again", "ask"]
    map fst (qOptions' (conjureNextQuestion st {stNext = Just (Rung "claude" EscalateAuto)} v)) `shouldBe` ["again", "climb", "ask"]
    qFields q `shouldBe` []
    map fst (qContext q) `shouldBe` ["round", "rung", "rounds_left", "attempts", "failing", "passing", "trade", "repeat", "tests", "unit", "hint"]
    lookup "failing" (qContext q) `shouldBe` Just (succeedsId <> ": assert NotFound(reason='Booking not found') == Ok(value=None)")
    lookup "passing" (qContext q) `shouldBe` Just notFoundId
    lookup "trade" (qContext q) `shouldBe` Just "yes"
    nextFrom st 2 (Chosen "ask" [] (Confidence 0.5)) `shouldBe` NextPause CauseAsk
    nextFrom st 2 (Chosen "ask" [] (Confidence 0.49)) `shouldBe` NextAgain
    nextFrom st {stNext = Just (Rung "claude" EscalateAuto)} 2 (Chosen "climb" [] (Confidence 0.6)) `shouldBe` NextClimb "claude"
    nextFrom st {stNext = Just (Rung "claude" EscalateAsk)} 2 (Chosen "climb" [] (Confidence 0.6)) `shouldBe` NextPause (CauseClimb "claude")
    nextFrom st {stNext = Just (Rung "claude" EscalateAuto)} 2 (Chosen "climb" [] (Confidence 0.59)) `shouldBe` NextAgain
    trigger CauseAsk (Just (Decided "ask" (Just 0.68) [("ask", 0.68), ("again", 0.29), ("test", 0.02)])) Nothing `shouldBe` "Jev: ask .68 \183 again .29 \183 test .02"
    trigger CauseRepeat Nothing (Just "x writes nothing here") `shouldBe` "repeat; wire failed: x writes nothing here"
    trigger (CauseTest notFoundId) Nothing Nothing `shouldBe` "unsatisfiable test: " <> notFoundId
  it "(u) the errors a test already has are not a re-conjure's: matched by file and message, once each" $ do
    let lambda = "Type of parameter \"id\" is unknown"
        report = Report [] [("booking/commands/cancel_booking__test.py", lambda), ("booking/commands/cancel_booking__test.py", lambda), ("booking/x.py", "BAD")] [] [(("booking/commands/cancel_booking__test.py", lambda), 23), (("booking/commands/cancel_booking__test.py", lambda), 30), (("booking/x.py", "BAD"), 2)]
        known = [("src/booking/commands/cancel_booking__test.py", lambda)]
    -- one known lambda error takes one of the two; the other is the re-conjure's
    reportBroken (withoutKnown known report) `shouldBe` [("booking/commands/cancel_booking__test.py", lambda), ("booking/x.py", "BAD")]
    reportBroken (withoutKnown [] report) `shouldBe` reportBroken report
    reportBroken (withoutKnown [("src/other.py", lambda)] report) `shouldBe` reportBroken report
    knownNote [(Just 23, lambda)] `shouldBe` "the test already has these errors; fix them if you touch those lines:\nline 23: Type of parameter \"id\" is unknown"
    knownNote [(Nothing, "x")] `shouldBe` "the test already has these errors; fix them if you touch those lines:\nx"
  it "(v) a refused re-conjure: the sentence, its rows, and the keep row's round" $ do
    refusedSentence "not_found" (addsReason "\"BAD\" is not defined" (Just 26)) `shouldBe` "Your re-conjure of not_found was refused: it adds `\"BAD\" is not defined` at line 26."
    addsReason "x" Nothing `shouldBe` "it adds `x`"
    let row = offerRow (OfferFacts 6 "local" Nothing 3 "CancelBooking" Nothing False "cancel_booking" [] [] pyMethod)
        json = pausedJson cancelPaused {paOffers = map row [OfferRefused, OfferReconjureAgain notFoundId, OfferKeep 2]}
    map (\r -> (orLabel r, orDetail r)) (map row [OfferRefused, OfferReconjureAgain notFoundId, OfferKeep 2])
      `shouldBe` [ ("see the refused proposal", "the diff it wrote and why it was refused; a accepts it anyway")
                 , ("fix the test: not_found, again", "re-conjure it once more; you see its diff")
                 , ("keep round 2", "lands the diff on the right, red (saved; u undoes it)")
                 ]
    at ["offers", "0", "choice"] json `shouldBe` Just (String "proposal")
    at ["offers", "1", "choice"] json `shouldBe` Just (String "reconjure")
    at ["offers", "1", "arg"] json `shouldBe` Just (String notFoundId)
    at ["offers", "2", "arg"] json `shouldBe` Just (String "2")
    at ["outcome"] (pausedJson cancelPaused {paOutcome = Just "Your re-conjure of not_found was refused: x."}) `shouldBe` Just (String "Your re-conjure of not_found was refused: x.")
    at ["proposal", "refused"] (pausedJson cancelPaused {paProposal = Just (Proposal "t.py" notFoundId "a" "b" "d" (Just "it adds `x`") "" "test" [])}) `shouldBe` Just (String "it adds `x`")
  it "(t) the cancel_booking pause as it happened, as JSON (the plugin's fixture)" $ do
    let v = pausedJson cancelPaused
        labels = [x | Just (Array os) <- [at ["offers"] v], Object o <- toList os, Just (String x) <- [KM.lookup "label" o]]
    at ["title", "unit"] v `shouldBe` Just (String "CancelBooking.handle")
    at ["diagnosis"] v `shouldBe` Just (toJSON (diagnosis cancelFacts CauseRepeat))
    at ["trigger"] v `shouldBe` Just (String "repeat")
    at ["rounds", "2", "line"] v `shouldBe` Just (String "(the same as round 2)")
    at ["rounds", "1", "failed", "0", "got"] v `shouldBe` Just (String "NotFound(reason='Booking not found')")
    at ["rounds", "1", "failed", "0", "expected"] v `shouldBe` Just (String "Ok(value=None)")
    at ["rounds", "1", "failed", "0", "assertion"] v `shouldBe` Just (String "assert result == Ok(None)")
    at ["tests", "0", "test"] v `shouldBe` Just (String succeedsId)
    at ["tests", "0", "status"] v `shouldBe` Just (String "failing")
    at ["tests", "0", "flipped"] v `shouldBe` Just (Number 1)
    at ["tests", "0", "note"] v `shouldBe` Just Null
    at ["tests", "1", "test"] v `shouldBe` Just (String notFoundId)
    at ["tests", "1", "status"] v `shouldBe` Just (String "passing")
    at ["tests", "1", "flipped"] v `shouldBe` Just (Number 1)
    at ["tests", "1", "assertion"] v `shouldBe` Just (String "assert isinstance(result, NotFound)")
    at ["tests", "1", "note"] v `shouldBe` Just (String "builds repository = Mock() and never hands it to command(); CancelBooking() takes no ports")
    labels `shouldBe` ["continue", "continue with a hint\8230", "fix the test: not_found", "fix the test: succeeds", "stop", "keep round 3"]
    at ["offers", "0", "detail"] v `shouldBe` Just (String "6 more rounds with local, from round 4")
    at ["offers", "1", "detail"] v `shouldBe` Just (String "your sentence goes to local with round 4")
    at ["offers", "2", "detail"] v `shouldBe` Just (String "re-conjure it from its name and the unit; you see its diff, then the rounds go on over it")
    at ["offers", "4", "detail"] v `shouldBe` Just (String "nothing lands; your text stands")
    at ["offers", "5", "detail"] v `shouldBe` Just (String "lands the diff on the right, red (saved; u undoes it)")
    at ["wired"] v `shouldBe` Just (Array mempty)
    at ["offers", "5", "land"] v `shouldBe` Just (Bool True)
    at ["offers", "4", "land"] v `shouldBe` Just (Bool False)
    at ["offers", "1", "hint"] v `shouldBe` Just (Bool True)
    at ["offers", "2", "arg"] v `shouldBe` Just (String notFoundId)
    at ["landing", "diff"] v `shouldSatisfy` \case Just (String d) -> "--- a/src/bistro/booking/commands/cancel_booking.py" `T.isPrefixOf` d; _ -> False
    at ["offers", "5", "arg"] v `shouldBe` Just (String "3")
    at ["outcome"] v `shouldBe` Just Null
    -- each round's change: the unit's diff, and every file keeping it lands (the unit's first)
    at ["rounds", "0", "diff"] v `shouldBe` Just (String "")
    at ["rounds", "1", "diff"] v `shouldSatisfy` \case Just (String d) -> "+        return NotFound(reason=\"Booking not found\")" `elem` T.lines d; _ -> False
    at ["rounds", "1", "files", "0", "file"] v `shouldBe` Just (String "src/bistro/booking/commands/cancel_booking.py")
    at ["rounds", "1", "files", "0", "diff"] v `shouldBe` at ["rounds", "1", "diff"] v
    -- OLIFANT_BLESS=1 writes the golden instead of comparing it
    let golden = "src/Olifant/Server/testdata/cancel-booking-paused.json"
    bless <- (== Just "1") <$> lookupEnv "OLIFANT_BLESS"
    if bless then TIO.writeFile golden (pretty v) else pure ()
    eitherDecodeFileStrict golden `shouldReturn` Right v
  it "(x) the re-conjure is told the test's note; progress names the round" $ do
    reconjureWhy cancelWiredPaused notFoundId `shouldBe` "builds repository = Mock() and never hands it to command(); CancelBooking takes bookings: BookingRepository since the wire"
    reconjureWhy cancelWiredPaused succeedsId `shouldBe` "local wrote the same body in rounds 1 and 2."
    let modelOf r = if r == "local" then "qwen3-coder:30b" else ""
    roundWords 8 modelOf 0 (Filling ["body"] 3 "local") `shouldBe` Just "round 3 of 8 \183 asking local (qwen3-coder:30b)\8230"
    roundWords 8 modelOf 0 (Filling ["body"] 3 "claude") `shouldBe` Just "round 3 of 8 \183 asking claude\8230"
    roundWords 8 modelOf 3 (Verifying "body") `shouldBe` Just "round 3 \183 testing\8230"
    roundWords 8 modelOf 3 (Checking "python") `shouldBe` Nothing
    fixWords "not_found" (Filling ["body"] 1 "local") `shouldBe` Just "fix the test: not_found \183 asking local\8230"
  it "(w) the cancel_booking pause after the wire, as design §10 draws it (the plugin's wired fixture)" $ do
    let v = pausedJson cancelWiredPaused
        labels = [x | Just (Array os) <- [at ["offers"] v], Object o <- toList os, Just (String x) <- [KM.lookup "label" o]]
        files i = [f | Just (Array fs) <- [at ["rounds", i, "files"] v], Object o <- toList fs, Just (String f) <- [KM.lookup "file" o]]
    at ["title", "round"] v `shouldBe` Just (Number 2)
    at ["diagnosis"] v
      `shouldBe` Just
        ( toJSON
            [ "local wrote the same body in rounds 1 and 2." :: Text
            , "No body can pass not_found: it builds repository = Mock() and never hands it to command(), so handle never sees it. CancelBooking takes bookings: BookingRepository since the wire; the test should pass its fake as command(bookings=\8230)."
            ]
        )
    -- succeeds passes before and after every round: not involved, so no row of its own
    labels `shouldBe` ["continue", "continue with a hint\8230", "fix the test: not_found", "stop", "keep round 2"]
    -- the scan knows the fix: the row says the edit itself (made without a model)
    at ["offers", "2", "detail"] v `shouldBe` Just (String "hand repository to command(bookings=repository); you see the diff")
    at ["offers", "3", "detail"] v `shouldBe` Just (String "nothing lands, not the wire either; your text stands")
    at ["wired", "0", "param"] v `shouldBe` Just (String "bookings")
    at ["wired", "0", "test"] v `shouldBe` Just (String notFoundId)
    at ["rounds", "1", "line"] v `shouldBe` Just (String "(the same as round 1)")
    files "1" `shouldBe` ["src/bistro/booking/commands/cancel_booking.py", cb]
    let golden = "src/Olifant/Server/testdata/cancel-booking-wired-paused.json"
    bless <- (== Just "1") <$> lookupEnv "OLIFANT_BLESS"
    if bless then TIO.writeFile golden (pretty v) else pure ()
    eitherDecodeFileStrict golden `shouldReturn` Right v
  it "(y) steps: each accepted step an entry after its round; a fixed test is offered again only for a new failure" $ do
    let fixed = ConjureStep 2 (StepTest notFoundId (Just "False = isinstance(Ok(value=None), NotFound)")) "test fixed: not_found (command(bookings=repository))" [(T.unpack cb, "--- a/x\n+++ b/x\n")]
        v = pausedJson cancelWiredPaused {paSteps = [ConjureStep 0 StepWire "wire bookings: BookingRepository" [], fixed]}
    at ["steps", "1", "after"] v `shouldBe` Just (Number 2)
    at ["steps", "1", "kind"] v `shouldBe` Just (String "test")
    at ["steps", "1", "test"] v `shouldBe` Just (String notFoundId)
    at ["steps", "1", "text"] v `shouldBe` Just (String "test fixed: not_found (command(bookings=repository))")
    at ["steps", "1", "files", "0", "file"] v `shouldBe` Just (String cb)
    at ["steps", "0", "kind"] v `shouldBe` Just (String "wire")
    -- the step's words: the line it changed, less what it assigns to
    proposalStep (langImports pyNow) "not_found" cancelTestText (T.replace "    cancel_booking = command()\n    result = cancel_booking.handle(CancelBookingInput(id=\"1\"))\n    assert isinstance" "    cancel_booking = command(bookings=repository)\n    result = cancel_booking.handle(CancelBookingInput(id=\"1\"))\n    assert isinstance" cancelTestText)
      `shouldBe` "test fixed: not_found (command(bookings=repository))"
    -- no run since the fix: not offered; a run failing the same way: not offered; failing anew: offered, saying so
    let checked = RoundRecord 3 "local" "x" Nothing (Just "pyright") Nothing 2
        same = checked {rrRun = Just round1Run}
        anew = checked {rrRun = Just (TestRun [] [(notFoundId, Failure (Just 26) "AttributeError: 'Mock' object has no attribute 'cancel'" "")] Nothing)}
        before2 = take 2 cancelRecords
    fixAgain [fixed] (before2 ++ [checked]) notFoundId `shouldBe` Nothing
    fixAgain [fixed] (before2 ++ [same]) notFoundId `shouldBe` Nothing
    fixAgain [fixed] (before2 ++ [anew]) notFoundId `shouldBe` Just (Just "AttributeError: 'Mock' object has no attribute 'cancel'")
    fixAgain [fixed] before2 succeedsId `shouldBe` Just Nothing
    let row = offerRow (OfferFacts 6 "local" Nothing 3 "CancelBooking" Nothing True "cancel_booking" [] [(notFoundId, "AttributeError: 'Mock' object has no attribute 'cancel'")] pyMethod)
    orDetail (row (OfferReconjure notFoundId)) `shouldBe` "fails anew: AttributeError: 'Mock' object has no attribute 'cancel'; you see its diff"
  it "(z) the failing section is the latest round's real failure: a check's refusal, with its lines, leads the diagnosis' others" $ do
    let refusal = LatestCheck 3 "pyright" [("src/bistro/booking/commands/cancel_booking.py", Just 30, "Cannot access attribute \"find_by_id\" for class \"BookingRepository\"")]
        checked = RoundRecord 3 "local" "x" Nothing (Just "src/bistro/booking/commands/cancel_booking.py: Cannot access attribute \"find_by_id\" for class \"BookingRepository\"") Nothing 2
        facts = cancelPausedFacts {dfHistory = take 2 cancelRecords ++ [checked], dfFlagged = [], dfFailure = Just refusal}
    take 2 (diagnosis facts CauseAsk)
      `shouldBe` ["local is not converging.", "Round 3 did not type-check: pyright refused it before the tests ran (cancel_booking.py:30: Cannot access attribute \"find_by_id\" for class \"BookingRepository\")."]
    let v = pausedJson cancelPaused {paFailure = Just refusal, paRounds = take 2 cancelRecords ++ [checked]}
    at ["failure", "kind"] v `shouldBe` Just (String "check")
    at ["checker"] (pausedJson cancelPaused {paChecker = Just "pyright"}) `shouldBe` Just (String "pyright")
    at ["failure", "round"] v `shouldBe` Just (Number 3)
    at ["failure", "tool"] v `shouldBe` Just (String "pyright")
    at ["failure", "problems", "0", "line"] v `shouldBe` Just (Number 30)
    at ["failure", "problems", "0", "file"] v `shouldBe` Just (String "src/bistro/booking/commands/cancel_booking.py")
    at ["rounds", "2", "check"] v `shouldBe` Just (String "src/bistro/booking/commands/cancel_booking.py: Cannot access attribute \"find_by_id\" for class \"BookingRepository\"")
    at ["rounds", "1", "check"] v `shouldBe` Just Null
    at ["failure", "tests"] (pausedJson cancelPaused {paFailure = Just (LatestTests 3 [succeedsId])}) `shouldBe` Just (toJSON [succeedsId])
    -- the root cause first: what the checker says is unknown because of it, after
    rootFirst [("a.py", Just 30, "Type of \"booking\" is unknown"), ("a.py", Just 30, "Cannot access attribute \"x\" for class \"P\"")]
      `shouldBe` [("a.py", Just 30, "Cannot access attribute \"x\" for class \"P\""), ("a.py", Just 30, "Type of \"booking\" is unknown")]
  it "(aa) a repeat means nothing changed: the same body after a step is a new round" $ do
    let r2 = cancelRecords !! 1
        r3 = (cancelRecords !! 2) {rrSteps = 1}
    isRepeat [r2, r3] `shouldBe` False
    isRepeat [r2, r3 {rrSteps = 0}] `shouldBe` True
    map rwLine (roundRows "cancel_booking" [r2, r3]) `shouldBe` ["return NotFound(reason=\"Booking not found\")", "return NotFound(reason=\"Booking not found\")"]
  it "(bb) a port missing a method pauses at once and offers it: p adds it, the diagnosis says where the check refused" $ do
    let base = PolicyFacts [] [] [] False True ["bookings: BookingRepository"] Nothing True False False (Just ("BookingRepository", "find_by_id"))
        sig = Just (", id: str", "Booking | None")
        cause = CauseMethod "BookingRepository" "find_by_id" sig
    -- before the repeat: the same body cannot pass while the port lacks it
    policy base `shouldBe` PolicyPause (CauseMethod "BookingRepository" "find_by_id" Nothing)
    policy base {pfFlagged = [(notFoundId, nf)], pfFailingNow = [notFoundId]} `shouldBe` PolicyPause (CauseTest notFoundId)
    causeWords cause `shouldBe` ["method", "BookingRepository.find_by_id"]
    trigger cause Nothing Nothing `shouldBe` "a port lacks a method: BookingRepository.find_by_id"
    let st = Standing 6 180 Nothing Nothing False
    offersAt st cause [notFoundId] [] 1 `shouldBe` [OfferContinue, OfferHint, OfferPort "BookingRepository" "find_by_id" ", id: str" "Booking | None", OfferReconjure notFoundId, OfferStop, OfferKeep 1]
    offersAt st (CauseMethod "BookingRepository" "find_by_id" Nothing) [] [] 1 `shouldBe` [OfferContinue, OfferHint, OfferStop, OfferKeep 1]
    let row = offerRow (OfferFacts 6 "local" Nothing 1 "CancelBooking" Nothing True "cancel_booking" [] [] pyMethod) (OfferPort "BookingRepository" "find_by_id" ", id: str" "Booking | None")
    (orLabel row, orDetail row) `shouldBe` ("add find_by_id to BookingRepository", "find_by_id(self, id: str) -> Booking | None in the port and its adapters, then round 2")
    -- every detail under 90 cells (conjure-pause-ux-pass-2.md §5): the window wraps it, never cuts it
    let facts = OfferFacts 6 "local" (Just "claude") 1 "CancelBooking" (Just notFoundId) True "cancel_booking" [(notFoundId, "hand repository to command(bookings=repository)")] [] pyMethod
        everyOffer = [OfferContinue, OfferHint, OfferClimb "claude", OfferWire "bookings" "BookingRepository", OfferReconjure notFoundId, OfferStop, OfferKeep 12, OfferAccept, OfferReject, OfferRefused, OfferReconjureAgain notFoundId, OfferPort "BookingRepository" "find_by_id" ", id: str" "Booking | None"]
    [d | d <- map (orDetail . offerRow facts) everyOffer, T.length d >= 90] `shouldBe` []
    -- a detail whose facts run long is cut at 89 cells and ends in …
    let long = orDetail (offerRow facts (OfferPort "BookingRepository" "find_every_booking_between" ", start: datetime, end: datetime, venue: VenueId" "list[Booking]"))
    T.length long `shouldBe` 89
    T.takeEnd 1 long `shouldBe` "\8230"
    let refusal = LatestCheck 1 "pyright" [("src/bistro/booking/commands/cancel_booking.py", Just 30, "Cannot access attribute \"find_by_id\" for class \"BookingRepository\"")]
        checked = RoundRecord 1 "local" "x" Nothing (Just "pyright") Nothing 1
        facts = cancelPausedFacts {dfHistory = [checked], dfFailure = Just refusal}
    take 2 (diagnosis facts cause)
      `shouldBe` [ "BookingRepository has no find_by_id: pyright refuses round 1's body before the tests run (cancel_booking.py:30)."
                 , "No body can pass not_found: it builds repository = Mock() and never hands it to command(), so handle never sees it. CancelBooking() takes no ports."
                 ]
    take 1 (diagnosis facts (CauseMethod "BookingRepository" "find_by_id" Nothing))
      `shouldBe` ["BookingRepository has no find_by_id: pyright refuses round 1's body before the tests run (cancel_booking.py:30). Its signature does not follow from the call and the test's fake: add it to BookingRepository yourself."]
    let v = pausedJson cancelPaused {paOffers = [row]}
    at ["offers", "0", "choice"] v `shouldBe` Just (String "port")
    at ["offers", "0", "arg"] v `shouldBe` Just (String "BookingRepository.find_by_id")
  where
    qOptions' q = case qShape q of
      Choice os -> os
      _ -> []

-- | The cancel_booking run as it happened (experiments/conjure-second-pass-2026-10-23.md):
-- the demo's morning test file, the unit's header, the before-run and three red rounds.
cb :: Text
cb = "src/bistro/booking/commands/cancel_booking__test.py"

succeedsId, notFoundId :: Text
succeedsId = cb <> "::test_cancel_booking_succeeds"
notFoundId = cb <> "::test_cancel_booking_not_found"

cancelTestText :: Text
cancelTestText =
  T.unlines
    [ "from unittest.mock import Mock"
    , ""
    , "from bistro.booking.commands.cancel_booking import ("
    , "    CancelBooking,"
    , "    CancelBookingInput,"
    , "    NotFound,"
    , ")"
    , "from bistro.shared.result import Ok"
    , ""
    , ""
    , "def command() -> CancelBooking:"
    , "    return CancelBooking()"
    , ""
    , ""
    , "def test_cancel_booking_succeeds() -> None:"
    , "    cancel_booking = command()"
    , "    result = cancel_booking.handle(CancelBookingInput(id=\"1\"))"
    , "    assert result == Ok(None)"
    , ""
    , ""
    , "def test_cancel_booking_not_found() -> None:"
    , "    repository = Mock()"
    , "    repository.find_by_id = lambda id: None"
    , "    cancel_booking = command()"
    , "    result = cancel_booking.handle(CancelBookingInput(id=\"1\"))"
    , "    assert isinstance(result, NotFound)"
    , ""
    ]

cancelHeader :: Text
cancelHeader = "class CancelBooking:\n    def __init__(self) -> None:"

-- | pytest's report of a round (the fixtures under Language/testdata/pytest).
pytestReport :: Bool -> Text
pytestReport notFoundFails =
  T.unlines $
    [if notFoundFails then ".F                                                                       [100%]" else "F.                                                                       [100%]", "=================================== FAILURES ==================================="]
      ++ ( if notFoundFails
             then
               [ "________________________ test_cancel_booking_not_found _________________________"
               , "src/bistro/booking/commands/cancel_booking__test.py:26: in test_cancel_booking_not_found"
               , "    assert isinstance(result, NotFound)"
               , "E   assert False"
               , "E    +  where False = isinstance(Ok(value=None), NotFound)"
               ]
             else
               [ "_________________________ test_cancel_booking_succeeds _________________________"
               , "src/bistro/booking/commands/cancel_booking__test.py:18: in test_cancel_booking_succeeds"
               , "    assert result == Ok(None)"
               , "E   AssertionError: assert NotFound(reason='Booking not found') == Ok(value=None)"
               , "E    +  where Ok(value=None) = Ok(None)"
               ]
         )
      ++ [ "==================================== PASSES ===================================="
         , "=========================== short test summary info ============================"
         , "PASSED " <> (if notFoundFails then succeedsId else notFoundId)
         , "FAILED " <> (if notFoundFails then notFoundId else succeedsId)
         , "1 failed, 1 passed in 0.02s"
         ]

round1Run, round2Run, cancelBefore :: TestRun
round1Run = parsePytest (ExitFailure 1) (pytestReport True)
round2Run = parsePytest (ExitFailure 1) (pytestReport False)
cancelBefore = round1Run

cancelBody2 :: Text
cancelBody2 = "# This is a mock implementation that always returns NotFound\n# In a real implementation, this would interact with a repository\nreturn NotFound(reason=\"Booking not found\")"

cancelRecords :: [RoundRecord]
cancelRecords =
  [ RoundRecord 1 "local" "return Ok(None)" (Just round1Run) Nothing (Just (Decided "again" (Just 0.21) [])) 0
  , RoundRecord 2 "local" cancelBody2 (Just round2Run) Nothing (Just (Decided "ask" (Just 0.19) [])) 0
  , RoundRecord 3 "local" cancelBody2 (Just round2Run) Nothing Nothing 0
  ]

-- | Round N's state (rounds 2 and 3 wrote body 2).
cancelState :: Int -> RoundState
cancelState n = RoundState n "local" [RoundHole "body" "the tests failed:" [(b, "w") | b <- take n ["return Ok(None)", cancelBody2, cancelBody2]] (Just (if n == 1 then round1Run else round2Run))]

-- | The cancel_booking pause at round 3 as it happened (no wire ran), with the morning
-- module as the buffer and round 3's body as what keep lands.
cancelPaused :: PausedAnswer
cancelPaused =
  PausedAnswer
    { paToken = "conjure-1"
    , paFile = "/demo/src/bistro/booking/commands/cancel_booking.py"
    , paUnit = "handle"
    , paLines = (27, 28)
    , paRound = 3
    , paRung = "local"
    , paSummary = "red: test_cancel_booking_succeeds \183 assert NotFound(reason='Booking not found') == Ok(value=None)"
    , paWhy = T.concat (take 1 sentences)
    , paTitle = Title "CancelBooking.handle" 3 6 "local" "qwen3-coder:30b" 14
    , paDiagnosis = sentences
    , paTrigger = trigger CauseRepeat Nothing Nothing
    , paRounds = cancelRecords
    , paTests = testEntries cancelPausedFacts
    , paHeader = cancelHeader
    , paPorts = []
    , paLanding = unifiedDiff rel cancelModule landing
    , paWired = []
    , paOffers = map (offerRow (OfferFacts 6 "local" Nothing 3 "CancelBooking" (listToMaybe' suspects) False "cancel_booking" (fixMoves cancelPausedFacts) [] pyMethod)) (offersAt (Standing 6 180 Nothing Nothing False) CauseRepeat suspects [] 3)
    , paExpires = 600
    , paStem = "cancel_booking"
    , paProposal = Nothing
    , paOutcome = Nothing
    , paRoundFiles = [(n, [(rel, unifiedDiff rel cancelModule l)]) | (n, l) <- [(1, cancelModule), (2, landing), (3, landing)]]
    , paWiredFor = Nothing
    , paHeld = []
    , paSteps = []
    , paFailure = Nothing
    , paChecker = Nothing
    }
  where
    rel = "src/bistro/booking/commands/cancel_booking.py"
    sentences = diagnosis cancelPausedFacts CauseRepeat
    suspects = suspectFirst (dfFlagged cancelPausedFacts) (involved cancelBefore cancelRecords)
    landing = T.replace "        return Ok(None)" (T.intercalate "\n" (map ("        " <>) (T.lines cancelBody2))) cancelModule
    listToMaybe' = \case
      x : _ -> Just x
      [] -> Nothing

cancelPausedFacts :: DiagnosisFacts
cancelPausedFacts =
  DiagnosisFacts "local" Nothing "cancel_booking" "handle" "CancelBooking" cancelHeader cancelTestText cancelBefore cancelRecords [(notFoundId, Fake "repository" "Mock")] [] [] 3 14 Nothing Nothing (testSourcesIn pyNow cancelTestText) (langAssertion pyNow) (langConjure pyNow)

-- | The cancel_booking pause as the by-hand run of 2026-10-24 had it (design §10): the wire
-- before round 1 gave CancelBooking bookings: BookingRepository and the test's helper the
-- param; rounds 1 and 2 wrote the same body; not_found still fails (it never hands its fake
-- in). The body is illustrative: the run recorded only its first line.
cancelWiredPaused :: PausedAnswer
cancelWiredPaused =
  PausedAnswer
    { paToken = "conjure-1"
    , paFile = "/demo/src/bistro/booking/commands/cancel_booking.py"
    , paUnit = "handle"
    , paLines = (31, 32)
    , paRound = 2
    , paRung = "local"
    , paSummary = roundSummaryText
    , paWhy = T.concat (take 1 sentences)
    , paTitle = Title "CancelBooking.handle" 2 6 "local" "qwen3-coder:30b" 16
    , paDiagnosis = sentences
    , paTrigger = trigger CauseRepeat Nothing Nothing
    , paRounds = records
    , paTests = testEntries facts
    , paHeader = wiredHeader
    , paPorts = ["bookings: BookingRepository"]
    , paLanding = unifiedDiff rel cancelModule landing
    , paWired = [("bookings: BookingRepository", [(rel, unifiedDiff rel cancelModule wiredModule), (T.unpack cb, unifiedDiff (T.unpack cb) cancelTestText wiredTest)])]
    , paOffers = map (offerRow (OfferFacts 6 "local" Nothing 2 "CancelBooking" (listToMaybe' suspects) True "cancel_booking" (fixMoves facts) [] pyMethod)) (offersAt (Standing 6 180 Nothing Nothing False) CauseRepeat suspects [] 2)
    , paExpires = 600
    , paStem = "cancel_booking"
    , paProposal = Nothing
    , paOutcome = Nothing
    , paRoundFiles = [(n, [(rel, unifiedDiff rel cancelModule landing), (T.unpack cb, unifiedDiff (T.unpack cb) cancelTestText wiredTest)]) | n <- [1, 2]]
    , paWiredFor = Just notFoundId
    , paHeld = []
    , paSteps = []
    , paFailure = Nothing
    , paChecker = Nothing
    }
  where
    rel = "src/bistro/booking/commands/cancel_booking.py"
    body = "booking = self.bookings.find_by_id(input.id)\nif booking is not None:\n    self.bookings.save(booking)\nreturn Ok(None)"
    records =
      [ RoundRecord 1 "local" body (Just round1Run) Nothing (Just (Decided "again" (Just 0.31) [])) 0
      , RoundRecord 2 "local" body (Just round1Run) Nothing Nothing 0
      ]
    roundSummaryText = "red: test_cancel_booking_not_found \183 False = isinstance(Ok(value=None), NotFound)"
    facts =
      DiagnosisFacts "local" Nothing "cancel_booking" "handle" "CancelBooking" wiredHeader wiredTest cancelBefore records [(notFoundId, Fake "repository" "Mock")] ["bookings: BookingRepository"] [] 2 16 Nothing Nothing (testSourcesIn pyNow wiredTest) (langAssertion pyNow) (langConjure pyNow)
    sentences = diagnosis facts CauseRepeat
    suspects = suspectFirst (dfFlagged facts) (involved cancelBefore records)
    landing = T.replace "        return Ok(None)" (T.intercalate "\n" (map ("        " <>) (T.lines body))) wiredModule
    listToMaybe' = \case
      x : _ -> Just x
      [] -> Nothing

wiredHeader :: Text
wiredHeader = "class CancelBooking:\n    def __init__(self, bookings: BookingRepository) -> None:"

-- | The morning module as the wire wrote it: the port imported, taken and kept.
wiredModule :: Text
wiredModule =
  T.replace "        \"\"\"Takes the ports it uses; main.py passes their adapters.\"\"\"\n" "        \"\"\"Takes the ports it uses; main.py passes their adapters.\"\"\"\n        self.bookings = bookings\n"
    . T.replace "    def __init__(self) -> None:" "    def __init__(self, bookings: BookingRepository) -> None:"
    . T.replace "from bistro.shared.result import Ok\n" "from bistro.booking.ports.booking_repository.booking_repository import (\n    BookingRepository,\n)\nfrom bistro.shared.result import Ok\n"
    $ cancelModule

-- | The morning test file as the wire wrote it: its helper takes the port, an in-memory one by default.
wiredTest :: Text
wiredTest =
  T.replace "def command() -> CancelBooking:\n    return CancelBooking()" "def command(bookings: BookingRepository | None = None) -> CancelBooking:\n    return CancelBooking(bookings or InMemoryBookingRepository())"
    . T.replace "from unittest.mock import Mock\n\n" "from unittest.mock import Mock\n\nfrom bistro.booking.adapters.in_memory_booking_repository import (\n    InMemoryBookingRepository,\n)\n"
    . T.replace ")\nfrom bistro.shared.result import Ok" ")\nfrom bistro.booking.ports.booking_repository.booking_repository import (\n    BookingRepository,\n)\nfrom bistro.shared.result import Ok"
    $ cancelTestText

-- | The demo's morning cancel_booking.py.
cancelModule :: Text
cancelModule =
  T.unlines
    [ "\"\"\"Cancel booking.\"\"\""
    , ""
    , "from dataclasses import dataclass"
    , ""
    , "from bistro.shared.result import Ok"
    , ""
    , ""
    , "@dataclass(frozen=True)"
    , "class CancelBookingInput:"
    , "    id: str"
    , ""
    , ""
    , "@dataclass(frozen=True)"
    , "class NotFound:"
    , "    reason: str"
    , ""
    , ""
    , "type CancelBookingFailure = NotFound"
    , ""
    , "type CancelBookingOutput = Ok[None] | CancelBookingFailure"
    , ""
    , ""
    , "class CancelBooking:"
    , "    def __init__(self) -> None:"
    , "        \"\"\"Takes the ports it uses; main.py passes their adapters.\"\"\""
    , ""
    , "    def handle(self, input: CancelBookingInput) -> CancelBookingOutput:"
    , "        return Ok(None)"
    ]

-- | A JSON value along a path of keys (an array's by index).
at :: [Text] -> Value -> Maybe Value
at [] v = Just v
at (k : ks) v = case v of
  Object o -> KM.lookup (Key.fromText k) o >>= at ks
  Array xs | [(i, "")] <- reads (T.unpack k) -> case drop i (toList xs) of
    x : _ -> at ks x
    [] -> Nothing
  _ -> Nothing

-- | A JSON value, two spaces an indent, keys in order (the golden file).
pretty :: Value -> Text
pretty v0 = go 0 v0 <> "\n"
  where
    go :: Int -> Value -> Text
    go n = \case
      Object o | not (KM.null o) -> "{\n" <> T.intercalate ",\n" [pad (n + 1) <> scalar (String (Key.toText k)) <> ": " <> go (n + 1) x | (k, x) <- KM.toAscList o] <> "\n" <> pad n <> "}"
      Array xs | not (null xs) -> "[\n" <> T.intercalate ",\n" [pad (n + 1) <> go (n + 1) x | x <- toList xs] <> "\n" <> pad n <> "]"
      x -> scalar x
    pad n = T.replicate (2 * n) " "
    scalar = TE.decodeUtf8 . BSL.toStrict . encode

-- | Python as conjure reads it here: the python-htmx pack's.
pyNow :: Language
pyNow = pyHtmx

-- | Python as the python-htmx pack has it (its `[conjure]` shapes over the
-- shipped language), read once for the pure fixtures here.
pyHtmx :: Language
pyHtmx = unsafePerformIO (shippedLanguage "python" [("pack python-htmx", "packs/python-htmx")])
{-# NOINLINE pyHtmx #-}

-- | Python's method signature template (`[types] method`).
pyMethod :: Maybe Text
pyMethod = langTypes pyNow >>= tyMethod
