module Olifant.Server.ConjureNextSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode (..))
import Olifant.Decide
import Olifant.Generation.Steer
import Olifant.Language
import Olifant.Model (Escalate (..), Rung (..))
import Olifant.Server.ConjureNext
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
      keys = ["round", "rung", "attempt", "failing", "passing", "check", "tests", "unit", "wired", "repeat", "rounds_left", "seconds_left", "hint"]
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
  it "(e) the question offers again, climb and ask, with thirteen context keys" $ do
    let q = conjureNextQuestion (standing EscalateAsk) view
    qKind q `shouldBe` "conjure-next"
    map fst (qOptions' q) `shouldBe` ["again", "climb", "ask", "port", "test"]
    map fst (qContext q) `shouldBe` keys
    lookup "hint" (qContext q) `shouldBe` Just "none"
    lookup "repeat" (qContext q) `shouldBe` Just "no"
    lookup "repeat" (qContext (conjureNextQuestion (standing EscalateAsk) viewAgain)) `shouldBe` Just "yes"
    map fst (qOptions' (conjureNextQuestion (standing EscalateNever) view)) `shouldBe` ["again", "ask", "port", "test"]
    map fst (qOptions' (conjureNextQuestion (Standing 3 50 Nothing Nothing False) view)) `shouldBe` ["again", "ask", "port", "test"]
  it "(f) spent pauses when rounds or seconds run out" $ do
    spent (Standing 0 50 Nothing Nothing False) `shouldBe` Just (NextPause "budget spent" [OfferContinue, OfferStop])
    spent (Standing 3 (-1) (Just (Rung "claude" EscalateNever)) Nothing False)
      `shouldBe` Just (NextPause "budget spent" [OfferContinue, OfferClimb "claude", OfferStop])
    spent (Standing 3 50 Nothing Nothing False) `shouldBe` Nothing
  it "(g) nextFrom follows a confident answer and otherwise goes again" $ do
    let st = standing EscalateAsk
        offers = [OfferContinue, OfferClimb "claude", OfferStop]
    nextFrom st view (Chosen "again" [] (Confidence 0.9)) `shouldBe` NextAgain
    nextFrom st view (Chosen "climb" [] (Confidence 0.9)) `shouldBe` NextPause "Jev would climb to claude" offers
    nextFrom (standing EscalateAuto) view (Chosen "climb" [] (Confidence 0.9)) `shouldBe` NextClimb "claude"
    nextFrom st viewAgain (Chosen "ask" [] (Confidence 0.9)) `shouldBe` NextPause "the same failure twice" offers
    nextFrom st view (Chosen "ask" [] (Confidence 0.9)) `shouldBe` NextPause "Jev asks you to look" offers
    nextFrom st view (Chosen "climb" [] (Confidence 0.5)) `shouldBe` NextAgain
    nextFrom st view (Chosen "ask" [] Unknown) `shouldBe` NextAgain
    nextFrom st view Unsure `shouldBe` NextAgain
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
  it "(j) the question offers port, wireable or not (an unwireable one is explained), and names its fields" $ do
    let w = (standing EscalateAsk) {stWireable = True}
        q = conjureNextQuestion w view
    map fst (qOptions' q) `shouldBe` ["again", "climb", "ask", "port", "test"]
    qFields q `shouldBe` [("param", "ident"), ("port", "ident"), ("test", "text"), ("why", "text")]
    map fst (qOptions' (conjureNextQuestion (standing EscalateAsk) view)) `shouldBe` ["again", "climb", "ask", "port", "test"]
    map fst (qContext q) `shouldBe` keys
    lookup "wired" (qContext q) `shouldBe` Just "x: X"
  it "(k) nextFrom wires on port, pauses with a reconjure offer on test, explains on ask" $ do
    let w = (standing EscalateAsk) {stWireable = True}
        st = standing EscalateAsk
        offers = [OfferContinue, OfferClimb "claude", OfferStop]
        portA c = Chosen "port" [("param", "bookings"), ("port", "BookingRepository")] (Confidence c)
    nextFrom w view (portA 0.9) `shouldBe` NextWire "bookings" "BookingRepository"
    nextFrom st view (portA 0.9) `shouldBe` NextPause "needs a port the class lacks: bookings: BookingRepository; no wire generator applies here" offers
    nextFrom w view (portA 0.5) `shouldBe` NextAgain
    nextFrom st view (Chosen "test" [("test", "t.py::test_a"), ("why", "it builds a Mock it never passes in")] (Confidence 0.9))
      `shouldBe` NextPause "it builds a Mock it never passes in" (offers ++ [OfferReconjure "t.py::test_a"])
    nextFrom st view (Chosen "test" [("test", "t.py::nope")] (Confidence 0.9)) `shouldBe` NextPause "Jev asks you to look" offers
    nextFrom st view (Chosen "ask" [("why", "the tests disagree")] (Confidence 0.9)) `shouldBe` NextPause "the tests disagree" offers
    offerText (OfferReconjure "t.py::test_a") `shouldBe` "reconjure:t.py::test_a"
    offerText OfferAccept `shouldBe` "accept"
    offerText OfferReject `shouldBe` "reject"
  it "(l) conjure-test asks ok or port and reads a confident port" $ do
    let q = conjureTestQuestion "def test_a():\n    assert f() == 1" "class C:\n    def __init__(self) -> None:"
    qKind q `shouldBe` "conjure-test"
    map fst (qOptions' q) `shouldBe` ["ok", "port"]
    qFields q `shouldBe` [("param", "ident"), ("port", "ident")]
    map fst (qContext q) `shouldBe` ["test", "unit"]
    testCheckFrom (Chosen "port" [("param", "bookings"), ("port", "BookingRepository")] (Confidence 0.8)) `shouldBe` TestPort "bookings" "BookingRepository"
    testCheckFrom (Chosen "port" [] (Confidence 0.9)) `shouldBe` TestOk
    testCheckFrom (Chosen "port" [("param", "bookings"), ("port", "BookingRepository")] (Confidence 0.5)) `shouldBe` TestOk
    testCheckFrom (Chosen "ok" [] (Confidence 0.9)) `shouldBe` TestOk
    testCheckFrom Unsure `shouldBe` TestOk
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
