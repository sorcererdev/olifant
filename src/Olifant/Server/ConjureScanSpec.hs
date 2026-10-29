{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.Server.ConjureScanSpec (spec) where

import Data.Maybe (fromJust)
import Data.Text qualified as T
import Olifant.Decide
import Olifant.Language (ConjureDef (..), Language (..), PortShape (..))
import Olifant.Language.TestUnits (testSourceIn)
import Olifant.Server.ConjureNextSpec (cancelTestText, pyHtmx)
import Olifant.Server.ConjureScan
import Test.Hspec

spec :: Spec
spec = describe "ConjureScan (Olifant.Server.ConjureScan)" $ do
  let py = pyHtmx
      Just cj = langConjure py
      Just fs = cjFake cj
      Just ct = cjConstructor cj
      Just ps = cjPort cj
      -- the test NAME's fake in TEXT, by the test's source
      unpassedIn text name = testSourceIn py name text >>= unpassedFake fs
      -- the test NAME in TEXT with its fake handed in
      handIn name helper param var text = testSourceIn py name text >>= \src -> (\src' -> T.replace src src' text) <$> handFake fs helper param var src
      succeedsSource = fromJust (testSourceIn py "test_cancel_booking_succeeds" cancelTestText)
      notFoundSource = fromJust (testSourceIn py "test_cancel_booking_not_found" cancelTestText)
      ports = [Port "BookingRepository" ["def table(self, table_id: str) -> Table | None: ..."], Port "IdGenerator" ["def next_id(self) -> str: ..."]]
  it "(a) the demo's not_found builds a Mock it never passes in; succeeds builds none" $ do
    unpassedIn cancelTestText "test_cancel_booking_not_found" `shouldBe` Just (Fake "repository" "Mock")
    unpassedIn cancelTestText "test_cancel_booking_succeeds" `shouldBe` Nothing
    fakeCalls notFoundSource (Fake "repository" "Mock") `shouldBe` ["find_by_id"]
  it "(b) a fake passed by keyword, positionally, or to another call is not flagged; one only assigned to is" $ do
    unpassedIn "def test_a():\n    bookings = InMemoryBookingRepository()\n    c = command(bookings=bookings)\n" "test_a" `shouldBe` Nothing
    unpassedIn "def test_b():\n    repo = FakeBookingRepository()\n    c = CancelBooking(repo)\n" "test_b" `shouldBe` Nothing
    unpassedIn "def test_c():\n    repository = Mock()\n    foo(bar, repository)\n" "test_c" `shouldBe` Nothing
    unpassedIn "def test_d():\n    m = MagicMock()\n    m.x = 1\n" "test_d" `shouldBe` Just (Fake "m" "MagicMock")
    unpassedIn "def test_e():\n    repo = Mock()\n    c = command(\n        bookings=repo,\n    )\n" "test_e" `shouldBe` Nothing
  it "(c) the act call and the helper" $ do
    actCall "handle" succeedsSource `shouldBe` Just "handle(CancelBookingInput(id=\"1\"))"
    actCall "handle" notFoundSource `shouldBe` Just "handle(CancelBookingInput(id=\"1\"))"
    helperOf "handle" notFoundSource `shouldBe` Just "command"
  it "(d) the port: by the fake's class, by the variable's last word, the only one; else none" $ do
    derivePort fs (Fake "repository" "Mock") ports `shouldBe` Just "BookingRepository"
    derivePort fs (Fake "x" "InMemoryBookingRepository") ports `shouldBe` Just "BookingRepository"
    derivePort fs (Fake "x" "Mock") ports `shouldBe` Nothing
    derivePort fs (Fake "x" "Mock") (take 1 ports) `shouldBe` Just "BookingRepository"
  it "(e) the param: the siblings' commonest, else the variable" $ do
    deriveParam "BookingRepository" [[("bookings", "BookingRepository"), ("ids", "IdGenerator")], [("booking_repository", "BookingRepository")], [("bookings", "BookingRepository")]] (Fake "repository" "Mock") `shouldBe` "bookings"
    deriveParam "BookingRepository" [] (Fake "repository" "Mock") `shouldBe` "repository"
    ctorParams ct "class C:\n    def __init__(self, bookings: BookingRepository, ids: IdGenerator) -> None:\n" `shouldBe` [("bookings", "BookingRepository"), ("ids", "IdGenerator")]
  it "(f) takesPort, and the port question and its reading" $ do
    takesPort ct "class CancelBooking:\n    def __init__(self) -> None:" "BookingRepository" `shouldBe` False
    takesPort ct "class C:\n    def __init__(self, bookings: BookingRepository) -> None:" "BookingRepository" `shouldBe` True
    let q = conjurePortQuestion notFoundSource ["find_by_id"] ports
    qKind q `shouldBe` "conjure-port"
    qOptions q `shouldBe` ["BookingRepository", "IdGenerator"]
    map fst (qContext q) `shouldBe` ["test", "uses", "ports"]
    lookup "uses" (qContext q) `shouldBe` Just "find_by_id"
    portFrom ports (Chosen "IdGenerator" [] (Confidence 0.7)) `shouldBe` Just "IdGenerator"
    portFrom ports (Chosen "IdGenerator" [] (Confidence 0.5)) `shouldBe` Nothing
    portFrom ports (Chosen "Nope" [] (Confidence 0.9)) `shouldBe` Nothing
    portCandidates [("IdGenerator", 0.5), ("BookingRepository", 0.3)] ports `shouldBe` ["IdGenerator", "BookingRepository"]
    portCandidates [] ports `shouldBe` ["BookingRepository", "IdGenerator"]
  it "(g) portsIn reads Protocol classes and their methods" $
    portsIn ps
      [ ("p/booking_repository.py", "class BookingRepository(Protocol):\n    def table(self, table_id: str) -> Table | None: ...\n")
      , ("p/in_memory.py", "class InMemoryBookingRepository:\n    def table(self, table_id: str) -> Table | None:\n        return None\n")
      ]
      `shouldBe` [Port "BookingRepository" ["def table(self, table_id: str) -> Table | None: ..."]]
  it "(h) handFake hands the fake to the helper's call in that test alone" $ do
    let fixed = handIn "test_cancel_booking_not_found" "command" "bookings" "repository" cancelTestText
        changed t = [(a, b) | (a, b) <- zip (T.lines cancelTestText) (T.lines t), a /= b]
    fmap changed fixed `shouldBe` Just [("    cancel_booking = command()", "    cancel_booking = command(bookings=repository)")]
    handIn "test_b" "command" "bookings" "repo" "def test_b():\n    repo = Mock()\n    c = command(1)\n" `shouldBe` Just "def test_b():\n    repo = Mock()\n    c = command(1, bookings=repo)\n"
    handIn "test_c" "command" "bookings" "repo" "def test_c():\n    repo = Mock()\n    c = CancelBooking()\n" `shouldBe` Nothing
    handIn "test_nope" "command" "bookings" "repository" cancelTestText `shouldBe` Nothing
    handIn "test_d" "command" "bookings" "repo" "def test_d():\n    repo = Mock()\n    c = command(bookings=repo)\n" `shouldBe` Nothing
  it "(i) a port missing a method: the check's words, on a class the unit takes" $ do
    let why = "src/bistro/booking/commands/cancel_booking.py: Type of \"booking\" is unknown; src/bistro/booking/commands/cancel_booking.py: Cannot access attribute \"find_by_id\" for class \"BookingRepository\"\n  Attribute \"find_by_id\" is unknown"
    missingMethod ["BookingRepository"] why `shouldBe` Just ("BookingRepository", "find_by_id")
    missingMethod ["IdGenerator"] why `shouldBe` Nothing
    missingMethod ["BookingRepository"] "src/x.py: \"y\" is not defined" `shouldBe` Nothing
  it "(j) the missing method's signature: names from the test's fake, types from the call, the result from its use" $ do
    let port = Port "BookingRepository" ["def table(self, table_id: str) -> Table | None: ...", "def booking_at(self, table_id: str, slot: Slot) -> Booking | None: ...", "def bookings_on(self, day: date) -> list[Booking]: ..."]
        body = "booking = self.bookings.find_by_id(input.id)\nif booking is None:\n    return NotFound(\"Booking not found\")\nreturn Ok(None)"
        moduleText = "@dataclass(frozen=True)\nclass CancelBookingInput:\n    id: str\n\n\nclass CancelBooking:\n    def handle(self, input: CancelBookingInput) -> CancelBookingOutput:\n        return Ok(None)\n"
    methodSignature "find_by_id" port body moduleText cancelTestText `shouldBe` Just (", id: str", "Booking | None")
    -- the call as a statement returns nothing
    methodSignature "cancel" port "self.bookings.cancel(input.id)\nreturn Ok(None)" moduleText "" `shouldBe` Just (", id: str", "None")
    -- an argument whose type is not known: none
    methodSignature "find_by_id" port "booking = self.bookings.find_by_id(thing)\nreturn Ok(None)" moduleText "" `shouldBe` Nothing
  it "(k) where a method goes in a port's file: the line after its Protocol's last" $ do
    portEnd ps "BookingRepository" "class BookingRepository(Protocol):\n    def table(self) -> None: ...\n\n    def save(self) -> None: ...\n" `shouldBe` Just 5
    portEnd ps "BookingRepository" "class BookingRepository(Protocol):\n    def save(self) -> None: ...\n\n\nclass Other:\n    pass\n" `shouldBe` Just 3
    portEnd ps "Nope" "class BookingRepository(Protocol):\n    def save(self) -> None: ...\n" `shouldBe` Nothing
  it "(n) the shapes are the pack's: a Rust-shaped trait reads as a port; no fake shape, no fake" $ do
    let rustPorts = PortShape ["ports/*.rs"] "^pub trait ([[:alnum:]_]+)" "^(async )?fn "
    portsIn rustPorts [("ports/invoice_repository.rs", "#[async_trait]\npub trait InvoiceRepository: Send + Sync + 'static {\n    async fn get_invoice(&self, id: String) -> Result<Invoice, RepositoryFailure>;\n}\n")]
      `shouldBe` [Port "InvoiceRepository" ["async fn get_invoice(&self, id: String) -> Result<Invoice, RepositoryFailure>;"]]
    fakeIn Nothing "def test_d():\n    m = MagicMock()\n" `shouldBe` Nothing
    ctorParamsIn Nothing "class C:\n    def __init__(self, a: A) -> None:\n" `shouldBe` []
