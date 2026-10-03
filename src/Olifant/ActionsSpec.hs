{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.ActionsSpec (spec) where

import Data.Functor.Identity (Identity (..), runIdentity)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Actions
import Olifant.Generator
import Olifant.Language
import Olifant.Query (noQueries)
import Olifant.Template (Template)
import Test.Hspec

rust :: Language
rust = l
  where
    Right l = parseLanguage "rust" "/p" (T.unlines ["files = [\"**/*.rs\"]", "comment = \"//\"", "grammar = \"rust\""])

generator :: Text -> Generator
generator src = either (error . T.unpack) (fmap inline) (parseGenerator (GeneratorId "p/g") src)
  where
    inline :: SourceRef -> Template
    inline (InlineText t) = t
    inline other = error (show other)

failureArms :: Generator
failureArms = generator . T.unlines $
  [ "description = \"Add arms\"", "language = \"rust\""
  , "[inputs.service]", "type = \"ident\"", "[inputs.command]", "type = \"ident\"", "[inputs.failure]", "type = \"ident\""
  , "[applies_at]", "path = \"{service}/*-service/src/{command}.rs\""
  , "node = { kind = \"enum_variant\" }"
  , "bind = { failure = \"^[[:space:]]*([A-Za-z0-9_]+)\" }"
  , "label = \"Add {= failure =} to every match\"" ]

-- An outer enum (lines 0–3) and a variant (line 2, cols 4–19).
matches :: Syntax Identity
matches = Syntax $ \_ _ _ -> Identity (Right [Match (0, 0) (3, 1) "pub enum RefundInvoiceFailure {…}", Match (2, 4) (2, 19) "AlreadyRefunded"])

file :: FilePath
file = "billing/billing-service/src/refund_invoice.rs"

at :: Syntax Identity -> [(Text, Generator)] -> FilePath -> (Int, Int) -> [Action]
at syn gens path pos = runIdentity (actionsAt syn [rust] noQueries [] (const (pure Nothing)) gens path pos "text")

spec :: Spec
spec = describe "olifant actions (spec §4.6)" $ do
  it "(a) binds path slots" $ do
    matchPathSlots "{service}/*-service/src/{command}.rs" file `shouldBe` Just [("service", "billing"), ("command", "refund_invoice")]
    matchPathSlots "{service}/*-service/src/{command}.rs" "billing/lib.rs" `shouldBe` Nothing
    matchPathSlots "**/src/{command}.rs" file `shouldBe` Just [("command", "refund_invoice")]
  it "(b) offers a generator whose path and node match, with inputs bound and the label rendered" $
    at matches [("p/failure-arms", failureArms)] file (3, 6)
      `shouldBe` [Action "p/failure-arms" "Add AlreadyRefunded to every match" [("command", "refund_invoice"), ("failure", "AlreadyRefunded"), ("service", "billing")] []]
  it "(c) matches at a node's first and just past its last character; binds from the innermost node (Review Focus 1)" $ do
    map actSets (at matches [("g", failureArms)] file (3, 5)) `shouldBe` [[("command", "refund_invoice"), ("failure", "AlreadyRefunded"), ("service", "billing")]]
    map actSets (at matches [("g", failureArms)] file (3, 20)) `shouldBe` [[("command", "refund_invoice"), ("failure", "AlreadyRefunded"), ("service", "billing")]]
  it "(d) offers nothing outside every match, and nothing for another path (Review Focus 1)" $ do
    at matches [("g", failureArms)] file (9, 1) `shouldBe` []
    at matches [("g", failureArms)] "notes/readme.md" (1, 1) `shouldBe` []
  it "(e) parses the text it is given, not the file (Review Focus 2)" $ do
    seen <- newIORef []
    let recording = Syntax $ \_ _ content -> modifyIORef' seen (content :) >> pure (Right [])
    _ <- actionsAt recording [rust] noQueries [] (const (pure Nothing)) [("g", failureArms)] file (1, 1) "unsaved text"
    readIORef seen `shouldReturn` ["unsaved text"]
  it "(f) moves a bound value that is not a valid choice into missing (Review Focus 3)" $ do
    let withChoices' = generator . T.unlines $
          [ "description = \"d\"", "language = \"rust\""
          , "[inputs.service]", "type = \"enum\"", "one_of = \"services\""
          , "[inputs.command]", "type = \"ident\""
          , "[query.services]", "glob = \"*/*-service\"", "value = \"parent folder\""
          , "[applies_at]", "path = \"{service}/*-service/src/{command}.rs\"" ]
        acts = runIdentity (actionsAt matches [rust] noQueries ["shipping/shipping-service/lib.rs"] (const (pure Nothing)) [("g", withChoices')] file (1, 1) "text")
    map (\a -> (actSets a, actMissing a)) acts `shouldBe` [([("command", "refund_invoice")], ["service"])]
