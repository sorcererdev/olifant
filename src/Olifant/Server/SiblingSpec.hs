{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.Server.SiblingSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Language (CtorShape (..), FakeShape (..), Language (..), shippedLanguageNow)
import Olifant.Server.ConjureNextSpec (cancelTestText, pyHtmx)
import Olifant.Server.ConjureScan (Port (..))
import Olifant.Server.Sibling
import Olifant.Workspace (diskWorkspace)
import Test.Hspec

commands :: FilePath
commands = "packs/python-htmx/fixtures/app/src/bistro/booking/commands/"

ports :: [Port]
ports = [Port "BookingRepository" ["def table(self, table_id: str) -> Table | None: ..."], Port "IdGenerator" ["def next_id(self) -> str: ..."]]

-- | The pack's old `[conjure.fake] bound`, character for character.
oldBound :: Text
oldBound = "^[[:space:]]+([[:alnum:]_]+)[[:space:]]*=[[:space:]]*([[:alnum:]_]+)\\("

spec :: Spec
spec = describe "Olifant.Server.Sibling" $ do
  let py = pyHtmx
  it "a sibling teaches the owner, fakes, constructor and binding the pack used to declare" $ do
    tt <- TIO.readFile (commands <> "create_booking__test.py")
    st <- TIO.readFile (commands <> "create_booking.py")
    let Just sb = siblingShapes py ports "handle" ("create_booking__test.py", tt, st)
    sbOwner sb `shouldBe` "CreateBooking"
    sbFakes sb `shouldBe` [("BookingRepository", "InMemoryBookingRepository", "bookings"), ("IdGenerator", "InMemoryIdGenerator", "ids")]
    ctStart (sbCtor sb) `shouldBe` "def __init__\\("
    ctOwner (sbCtor sb) `shouldBe` "^class ([[:alnum:]_]+)"
    fsBound (sbFake sb) `shouldBe` oldBound
    fsKeyword (sbFake sb) `shouldBe` Just "{= param =}={= var =}"
    fsPrefixes (sbFake sb) `shouldBe` ["InMemory"]
    sbAct sb `shouldBe` "plain"
  it "an own test file that builds the owner with no arguments teaches nothing" $
    siblingShapes py ports "handle" ("cancel_booking__test.py", cancelTestText, "class CancelBooking:\n    def __init__(self) -> None:\n        pass\n\n    def handle(self, input: CancelBookingInput) -> CancelBookingOutput:\n        return Ok(None)\n")
      `shouldBe` Nothing
  it "conjuredFakes: Mock by a port's word, Mock by configuration, an unconfigured thing not" $ do
    conjuredFakes oldBound ports cancelTestText `shouldBe` ["Mock"]
    conjuredFakes oldBound ports (T.replace "repository" "thing" cancelTestText) `shouldBe` ["Mock"]
    conjuredFakes oldBound ports "def test_x():\n    thing = Mock()\n    assert thing\n" `shouldBe` []
  it "a sibling whose owner does not hold the unit and is not called is skipped" $ do
    let st = "class Notify:\n    def __init__(self, bookings: BookingRepository) -> None:\n        self.bookings = bookings\n\n    def send(self) -> None:\n        pass\n"
        tt = "def test_notifies() -> None:\n    bookings = InMemoryBookingRepository()\n    notify = Notify(bookings=bookings)\n    assert notify\n"
    siblingShapes py ports "handle" ("notify__test.py", tt, st) `shouldBe` Nothing
  it "candidates: own file, folder by name, then beside folders by distance" $ do
    let Just td = langTest py
    siblingCandidates td "a/b/x.py" ["a/b/x.py", "a/b/w.py", "a/c/y.py", "a/c/d/z.py", "a/b/x__test.py"]
      `shouldBe` ["a/b/x.py", "a/b/w.py", "a/c/y.py", "a/c/d/z.py"]
  it "no [unit] annotation, no sibling" $ do
    tt <- TIO.readFile (commands <> "create_booking__test.py")
    st <- TIO.readFile (commands <> "create_booking.py")
    siblingShapes py {langUnitAnnotation = Nothing} ports "handle" ("create_booking__test.py", tt, st) `shouldBe` Nothing
  it "findSibling reads the service's ports and the nearest sibling, its constructor files under the service" $ do
    let ws = diskWorkspace "packs/python-htmx/fixtures/app"
        rel = "src/bistro/booking/queries/get_day_schedule.py"
    st <- TIO.readFile ("packs/python-htmx/fixtures/app/" <> rel)
    found <- servicePorts ws py rel
    map ptName found `shouldBe` ["BookingRepository", "IdGenerator"]
    Just sb <- findSibling ws py found (rel, st, Nothing) "handle"
    sbTest sb `shouldSatisfy` (`elem` ["src/bistro/booking/queries/get_day_schedule__test.py", "src/bistro/booking/commands/create_booking__test.py"])
    ctFiles (sbCtor sb) `shouldSatisfy` (not . null)
  it "Rust: a sibling in mod tests reads Arc<dyn X> ports" $ do
    let rust = shippedLanguageNow "rust"
        st = T.unlines
          [ "pub struct Billing {", "    invoices: Arc<dyn Invoices>,", "}", ""
          , "impl Billing {", "    pub fn new(invoices: Arc<dyn Invoices>) -> Self {", "        Self { invoices }", "    }", ""
          , "    pub fn charge(&self) {}", "}", ""
          , "#[cfg(test)]", "mod tests {", "    use super::*;", ""
          , "    #[test]", "    fn charges() {", "        let billing = Billing::new(Arc::new(InMemoryInvoices::default()));", "        billing.charge();", "    }", "}"
          ]
    case siblingShapes rust [Port "Invoices" []] "charge" ("billing.rs", st, st) of
      Just sb -> sbFakes sb `shouldBe` [("Invoices", "InMemoryInvoices", "invoices")]
      Nothing -> pendingWith "finding: a Rust sibling does not read (`Billing::new(` is a path call, not `Billing(`); rust-htmx shapes are deferred"
