{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.Server.ConjureScanSpec (spec) where

import Control.Monad (forM_)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Olifant.Decide
import Olifant.Language (Advice (..), ConjureDef (..), CtorShape (..), Language (..), PortShape (..), shippedLanguageNow)
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
      unpassedIn text name = testSourceIn py Nothing name text >>= unpassedFake fs
      -- the test NAME in TEXT with its fake handed in
      handIn name helper param var text = testSourceIn py Nothing name text >>= \src -> (\src' -> T.replace src src' text) <$> handFake fs helper param var src
      succeedsSource = fromJust (testSourceIn py Nothing "test_cancel_booking_succeeds" cancelTestText)
      notFoundSource = fromJust (testSourceIn py Nothing "test_cancel_booking_not_found" cancelTestText)
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
    missingMemberIn (shippedLanguageNow "python") ["BookingRepository"] why `shouldBe` Just ("BookingRepository", "find_by_id")
    missingMemberIn (shippedLanguageNow "python") ["IdGenerator"] why `shouldBe` Nothing
    missingMemberIn (shippedLanguageNow "python") ["BookingRepository"] "src/x.py: \"y\" is not defined" `shouldBe` Nothing
  it "(j) a model's signature line read through the snippet's marked line" $ do
    let snippet = "def \10214olifant-hole name|ident|the method\10215(self\10214olifant-hole params|text|, name: type, \8230\10215) -> \10214olifant-hole returns|text|what it returns\10215: ..."
    readSignature snippet "find_by_id" "def find_by_id(self, id: str) -> Booking | None: ..." `shouldBe` Just (", id: str", "Booking | None")
    readSignature snippet "cancel" "def cancel(self, id: str) -> None" `shouldBe` Just (", id: str", "None")
    -- another name, a fence, two lines, the snippet's prefix missing: none
    readSignature snippet "find_by_id" "def find(self, id: str) -> Booking | None: ..." `shouldBe` Nothing
    readSignature snippet "find_by_id" "```python\ndef find_by_id(self, id: str) -> Booking | None: ...\n```" `shouldBe` Nothing
    readSignature snippet "find_by_id" "def find_by_id(self, id: str) -> Booking | None: ...\ndef other(self) -> None: ..." `shouldBe` Nothing
    readSignature snippet "find_by_id" "find_by_id(self, id: str)" `shouldBe` Nothing
    signatureTemplate snippet `shouldBe` "{= name =}(self{= params =}) -> {= returns =}"
    snippetLine ("\n    " <> snippet <> "\n") `shouldBe` Just snippet
    callLines "find_by_id" "x = self.bookings.find_by_id(input.id)\ny = 1" `shouldBe` ["x = self.bookings.find_by_id(input.id)"]
  it "(j3) the siblings: test files (an orphan's too) and the unit's own file left out" $ do
    let files = ["src/b/s/commands/x.py", "src/b/s/commands/x__test.py", "src/b/s/commands/y__test.py", "src/b/s/commands/z.py"]
    siblingsOf py "src/b/s/commands/z.py" files `shouldBe` ["src/b/s/commands/x.py"]
    -- a Rust module that holds its own tests is a sibling
    siblingsOf (shippedLanguageNow "rust") "src/a.rs" ["src/a.rs", "src/b.rs", "src/c.rs"] `shouldBe` ["src/b.rs", "src/c.rs"]
  it "(j4) a module's service sets by its path: in the service's folder or below it" $ do
    let cj = langConjure py
    servicePaths cj "src/bistro/booking/commands/x.py" `shouldBe` [("app", "bistro"), ("service", "booking")]
    servicePaths cj "src/bistro/booking/x.py" `shouldBe` [("app", "bistro"), ("service", "booking")]
    servicePaths cj "src/bistro/booking" `shouldBe` []
    servicePaths cj "lib/bistro/booking/x.py" `shouldBe` []
  it "(k) where a method goes in a port's file: the line after its Protocol's last" $ do
    portEnd ps "BookingRepository" "class BookingRepository(Protocol):\n    def table(self) -> None: ...\n\n    def save(self) -> None: ...\n" `shouldBe` Just 5
    portEnd ps "BookingRepository" "class BookingRepository(Protocol):\n    def save(self) -> None: ...\n\n\nclass Other:\n    pass\n" `shouldBe` Just 3
    portEnd ps "Nope" "class BookingRepository(Protocol):\n    def save(self) -> None: ...\n" `shouldBe` Nothing
    -- no blank line after it (the end of the file, the next class at once): the line after
    -- its last all the same, which 'blankAt' makes blank for the snippet
    portEnd ps "BookingRepository" "class BookingRepository(Protocol):\n    def save(self) -> None: ..." `shouldBe` Just 3
    portEnd ps "BookingRepository" "class BookingRepository(Protocol):\n    def save(self) -> None: ...\nclass Other:\n    pass\n" `shouldBe` Just 3
  it "(k2) blankAt: a blank line stays; else one is put there, at the end too" $ do
    blankAt 2 "a\n\nb" `shouldBe` "a\n\nb"
    blankAt 2 "a\nb" `shouldBe` "a\n\nb"
    blankAt 2 "a" `shouldBe` "a\n"
  it "(k3) advisedIn: the first advice to match a test file's line adds its sentence" $ do
    let advice = [Advice "^def command\\(" "use command()", Advice "^def query\\(" "use query()"]
    advisedIn advice ["import x", "def query():", "def command():"] "write it" `shouldBe` "write it; use query()"
    advisedIn advice ["def other():"] "write it" `shouldBe` "write it"
    advisedIn [] ["def command():"] "write it" `shouldBe` "write it"
  it "(n) the shapes are the pack's: a Rust-shaped trait reads as a port; no fake shape, no fake" $ do
    let rustPorts = PortShape ["ports/*.rs"] "^pub trait ([[:alnum:]_]+)" "^(async )?fn "
    portsIn rustPorts [("ports/invoice_repository.rs", "#[async_trait]\npub trait InvoiceRepository: Send + Sync + 'static {\n    async fn get_invoice(&self, id: String) -> Result<Invoice, RepositoryFailure>;\n}\n")]
      `shouldBe` [Port "InvoiceRepository" ["async fn get_invoice(&self, id: String) -> Result<Invoice, RepositoryFailure>;"]]
    fakeIn Nothing "def test_d():\n    m = MagicMock()\n" `shouldBe` Nothing
    ctorParamsIn Nothing "class C:\n    def __init__(self, a: A) -> None:\n" `shouldBe` []
  it "(r) Rust's annotation reads Arc<dyn X>, Box<dyn X>, &dyn X and a plain type" $ do
    let Just ann = langUnitAnnotation (shippedLanguageNow "rust")
        ct = CtorShape [] "" "pub fn new\\(" ("^([[:alnum:]_]+)" <> T.replace "{= name =}" "" ann)
    forM_ ["Arc<dyn BookingRepository>", "Box<dyn BookingRepository>", "&dyn BookingRepository", "BookingRepository"] $ \ty ->
      ctorParams ct ("pub fn new(bookings: " <> ty <> ") -> Self {\n") `shouldBe` [("bookings", "BookingRepository")]
