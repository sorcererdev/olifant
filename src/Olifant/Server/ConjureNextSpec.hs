module Olifant.Server.ConjureNextSpec
  ( spec
  , cb
  , succeedsId
  , notFoundId
  , cancelTestText
  , cancelHeader
  , cancelBefore
  , cancelRecords
  , cancelState
  , round1Run
  , round2Run
  ) where

import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode (..))
import Olifant.Decide
import Olifant.Generation.Steer
import Olifant.Language
import Olifant.Model (Escalate (..), Rung (..))
import Olifant.Server.ConjureNext
import Olifant.Server.ConjureScan (Fake (..))
import Olifant.Server.Unit (unitHeader)
import Test.Hspec

spec :: Spec
spec = describe "conjure-next (Olifant.Server.ConjureNext)" $ do
  let run = TestRun ["t.py::test_b"] [("t.py::test_a", Failure (Just 6) "assert 0 == 1\nmore" "")] Nothing
      hole = RoundHole "body" "the tests failed:" [("return 0", "w1")] (Just run)
      state = RoundState 1 "qwen" [hole]
      ctx = RoundContext "def test_a():\n    assert 0 == 1\n" "class C:\n    def __init__(self):" ["x: X"] False [] (TestRun [] [] Nothing) "t"
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
    nextFrom st (Chosen "again" [] (Confidence 0.9)) `shouldBe` NextAgain
    nextFrom st (Chosen "climb" [] (Confidence 0.9)) `shouldBe` NextPause (CauseClimb "claude")
    nextFrom (standing EscalateAuto) (Chosen "climb" [] (Confidence 0.9)) `shouldBe` NextClimb "claude"
    nextFrom st (Chosen "ask" [] (Confidence 0.9)) `shouldBe` NextPause CauseAsk
    nextFrom st (Chosen "ask" [] Unknown) `shouldBe` NextAgain
    nextFrom st Unsure `shouldBe` NextAgain
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
    unitHeader "class C:\n    def __init__(self, a: A) -> None:\n        pass\n    def handle(self):\n        pass\n" 5
      `shouldBe` "class C:\n    def __init__(self, a: A) -> None:"
    unitHeader "def f():\n    pass\n" 2 `shouldBe` "def f():"
    unitHeader "x = 1\n" 1 `shouldBe` ""
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
    let v = roundView (RoundContext cancelTestText cancelHeader [] False cancelRecords cancelBefore "cancel_booking") Nothing (cancelState 3)
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
          }
  it "(o) the policy table, in order" $ do
    let base = PolicyFacts [(notFoundId, nf)] [notFoundId] [notFoundId] True True [] (Just ("bookings", "BookingRepository")) False False False
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
    let v = roundView (RoundContext cancelTestText cancelHeader [] False cancelRecords cancelBefore "cancel_booking") Nothing (cancelState 3)
        st = Standing 3 50 Nothing Nothing False
        q = conjureNextQuestion st v
    map fst (qOptions' q) `shouldBe` ["again", "ask"]
    map fst (qOptions' (conjureNextQuestion st {stNext = Just (Rung "claude" EscalateAuto)} v)) `shouldBe` ["again", "climb", "ask"]
    qFields q `shouldBe` []
    map fst (qContext q) `shouldBe` ["round", "rung", "rounds_left", "attempts", "failing", "passing", "trade", "repeat", "tests", "unit", "hint"]
    lookup "failing" (qContext q) `shouldBe` Just (succeedsId <> ": assert NotFound(reason='Booking not found') == Ok(value=None)")
    lookup "passing" (qContext q) `shouldBe` Just notFoundId
    lookup "trade" (qContext q) `shouldBe` Just "yes"
    nextFrom st (Chosen "ask" [] (Confidence 0.5)) `shouldBe` NextPause CauseAsk
    nextFrom st (Chosen "ask" [] (Confidence 0.49)) `shouldBe` NextAgain
    nextFrom st {stNext = Just (Rung "claude" EscalateAuto)} (Chosen "climb" [] (Confidence 0.6)) `shouldBe` NextClimb "claude"
    nextFrom st {stNext = Just (Rung "claude" EscalateAsk)} (Chosen "climb" [] (Confidence 0.6)) `shouldBe` NextPause (CauseClimb "claude")
    nextFrom st {stNext = Just (Rung "claude" EscalateAuto)} (Chosen "climb" [] (Confidence 0.59)) `shouldBe` NextAgain
    trigger CauseAsk (Just (Decided "ask" (Just 0.68) [("ask", 0.68), ("again", 0.29), ("test", 0.02)])) Nothing `shouldBe` "Jev: ask .68 \183 again .29 \183 test .02"
    trigger CauseRepeat Nothing (Just "x writes nothing here") `shouldBe` "repeat; wire failed: x writes nothing here"
    trigger (CauseTest notFoundId) Nothing Nothing `shouldBe` "unsatisfiable test: " <> notFoundId
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
  [ RoundRecord 1 "local" "return Ok(None)" (Just round1Run) Nothing (Just (Decided "again" (Just 0.21) []))
  , RoundRecord 2 "local" cancelBody2 (Just round2Run) Nothing (Just (Decided "ask" (Just 0.19) []))
  , RoundRecord 3 "local" cancelBody2 (Just round2Run) Nothing Nothing
  ]

-- | Round N's state (rounds 2 and 3 wrote body 2).
cancelState :: Int -> RoundState
cancelState n = RoundState n "local" [RoundHole "body" "the tests failed:" [(b, "w") | b <- take n ["return Ok(None)", cancelBody2, cancelBody2]] (Just (if n == 1 then round1Run else round2Run))]
