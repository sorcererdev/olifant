module Olifant.UncoveredSpec (spec) where

import Olifant.Language.Diagnostics (CheckReport (..), MissingPatterns (..))
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

spec :: Spec
spec = describe "Olifant.Uncovered (the cases a match at the cursor lacks, spec §4.9)" $ do
  it "(a) the match the cursor is in: the latest one in this file that starts at or above it" $ do
    uncoveredAt "Failure::" "src/a.rs" 45 report `shouldBe` ["RefundInvoiceFailure::AlreadyRefunded", "RefundInvoiceFailure::Expired"]
    -- a blank line just after the last arm is still in it
    uncoveredAt "Failure::" "src/a.rs" 50 report `shouldBe` ["RefundInvoiceFailure::AlreadyRefunded", "RefundInvoiceFailure::Expired"]
  it "drops a reference the compiler puts on a pattern (a match on &self): arms are written without it" $
    uncoveredAt "Failure::" "src/a.rs" 5 (CheckReport [MissingPatterns "src/a.rs" (1, 3) ["&E::Failure::A", "&mut E::Failure::B"] Nothing] [] [])
      `shouldBe` ["E::Failure::A", "E::Failure::B"]
  it "(a) filters by the regex, and is empty above every match or in another file" $ do
    uncoveredAt "Failure::" "src/a.rs" 12 report `shouldBe` []
    uncoveredAt "Failure::" "src/a.rs" 3 report `shouldBe` []
    uncoveredAt "Failure::" "src/c.rs" 45 report `shouldBe` []
