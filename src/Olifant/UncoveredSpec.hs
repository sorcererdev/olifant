module Olifant.UncoveredSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language.Diagnostics (CheckReport (..), CompilerSupport (..), MissingPatterns (..))
import Olifant.Language.Support (supportFor)
import Olifant.Uncovered
import Test.Hspec

report :: CheckReport
report =
  CheckReport
    [ MissingPatterns "src/a.rs" (39, 47) ["RefundInvoiceFailure::AlreadyRefunded", "RefundInvoiceFailure::Expired"] Nothing
    , MissingPatterns "src/a.rs" (9, 14) ["Other::X"] Nothing
    , MissingPatterns "src/b.rs" (39, 47) ["RefundInvoiceFailure::Elsewhere"] Nothing
    ]
    []
    []

-- | The file's 1-based lines: all blank (as after a match's last arm).
blankAfter :: Int -> Text
blankAfter _ = ""

-- | Patterns as each compiler's support writes them at the site.
rust, python :: MissingPatterns -> Text -> Text -> Text
rust = writer "rustc-json"
python = writer "pyright-json"

writer :: Text -> MissingPatterns -> Text -> Text -> Text
writer format d content p = maybe p (\s -> supportPattern s d content p p) (supportFor format)

spec :: Spec
spec = describe "Olifant.Uncovered (the cases a match at the cursor lacks, spec §4.9)" $ do
  it "(a) the match the cursor is in: a site whose arms reach the cursor, or only blank lines lie between" $ do
    uncoveredAt rust "Failure::" "src/a.rs" 45 blankAfter report `shouldBe` ["RefundInvoiceFailure::AlreadyRefunded", "RefundInvoiceFailure::Expired"]
    -- blank lines just after the last arm are still in it
    uncoveredAt rust "Failure::" "src/a.rs" 50 blankAfter report `shouldBe` ["RefundInvoiceFailure::AlreadyRefunded", "RefundInvoiceFailure::Expired"]
  it "(review I2) not an earlier match's gap: code between its last arm and the cursor means another match" $
    uncoveredAt rust "Failure::" "src/a.rs" 50 (\n -> if n == 49 then "    }" else "") report `shouldBe` []
  it "drops a reference the compiler puts on a pattern (a match on &self): arms are written without it" $
    uncoveredAt rust "Failure::" "src/a.rs" 5 blankAfter (CheckReport [MissingPatterns "src/a.rs" (1, 3) ["&E::Failure::A", "&mut E::Failure::B"] Nothing] [] [])
      `shouldBe` ["E::Failure::A", "E::Failure::B"]
  it "writes a pattern as the site does (python: through the command's module, like its other arms)" $ do
    let site = T.unlines ["def status(failure: F) -> int:", "    match failure:", "        case cancel_booking.NotFound():", "            return 404", ""]
        lineAt n = T.splitOn "\n" site !! (n - 1)
        r = CheckReport [MissingPatterns "src/f.py" (1, 3) ["AlreadyCancelled()"] Nothing] [] []
    uncoveredAt python "." "src/f.py" 5 lineAt r `shouldBe` ["cancel_booking.AlreadyCancelled()"]
  it "(a) filters by the regex, and is empty above every match or in another file" $ do
    uncoveredAt rust "Failure::" "src/a.rs" 12 blankAfter report `shouldBe` []
    uncoveredAt rust "Failure::" "src/a.rs" 3 blankAfter report `shouldBe` []
    uncoveredAt rust "Failure::" "src/c.rs" 45 blankAfter report `shouldBe` []
