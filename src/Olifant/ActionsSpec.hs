{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.ActionsSpec (spec) where

import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Text.IO qualified as TIO
import Olifant.GeneratorTesting (preparePack)
import Olifant.Generation (Tools (..))
import Olifant.Language.AstGrep (astGrepSyntax)
import Olifant.Workspace (Workspace (..), diskWorkspace)
import System.Directory (findExecutable, getTemporaryDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
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
  it "(g) renders path slots into the node rule (a command's own struct)" $ do
    seen <- newIORef []
    let recording = Syntax $ \_ rule _ -> modifyIORef' seen (rule :) >> pure (Right [])
        own = generator . T.unlines $
          [ "description = \"d\"", "language = \"rust\""
          , "[inputs.service]", "type = \"ident\"", "[inputs.command]", "type = \"ident\""
          , "[applies_at]", "path = \"{service}/*-service/src/{command}.rs\""
          , "node = { kind = \"field_declaration\", inside = { kind = \"struct_item\", regex = \"struct {= command|pascal =} \" } }" ]
    _ <- actionsAt recording [rust] noQueries [] (const (pure Nothing)) [("g", own)] file (1, 1) "text"
    (T.pack . show <$> readIORef seen) >>= (`shouldSatisfy` T.isInfixOf "struct RefundInvoice ")
  it "(i) reads COL as a UTF-8 byte column (Neovim's), not a character column (review I1)" $ do
    -- "éé x yz": x is character 3 (0-based) but byte 5; ast-grep reports characters.
    let content = "\233\233 x yz\n"
        onX = Syntax $ \_ _ _ -> Identity (Right [Match (0, 3) (0, 4) "x"])
        acts c = runIdentity (actionsAt onX [rust] noQueries [] (const (pure Nothing)) [("g", failureArms)] file (1, c) content)
    map actGenerator (acts 6) `shouldBe` ["g"]
    acts 4 `shouldBe` []
  it "(h) binds the file's own path to an input with `file`" $ do
    let errorCase = generator . T.unlines $
          [ "description = \"d\"", "language = \"rust\""
          , "[inputs.path]", "type = \"text\"", "[inputs.name]", "type = \"ident\""
          , "[applies_at]", "path = \"**/*.rs\"", "file = \"path\"", "label = \"Add a case\"" ]
    map (\a -> (actSets a, actMissing a)) (at matches [("g", errorCase)] file (1, 1)) `shouldBe` [([("path", T.pack file)], ["name"])]
  describe "the rust-htmx demo generators (real ast-grep)" $ do
    strict <- runIO ((== Just "1") <$> lookupEnv "OLIFANT_REQUIRE_TOOLS")
    let real gen given path pos = findExecutable "ast-grep" >>= \case
          Nothing -> if strict then expectationFailure "ast-grep is required" >> pure [] else pendingWith "ast-grep is not installed" >> pure []
          Just _ -> do
            tmp <- getTemporaryDirectory
            content <- TIO.readFile given
            Right (ws, tools, _, g) <- preparePack (diskWorkspace ".") (astGrepSyntax (tmp </> "olifant-test-cache")) noChecker "packs/rust-htmx" "rust-htmx" gen [(path, content)]
            paths <- listFiles ws "."
            actionsAt (toolSyntax tools) (toolLanguages tools) (toolQueries tools) paths (readText ws) [("rust-htmx/" <> gen, g)] path pos content
        cancel = "packs/rust-htmx/generators/wire-command/tests/cancel-invoice/given/billing/billing-service/src/cancel_invoice.rs"
        refund = "packs/rust-htmx/generators/failure-arms/tests/already-refunded/given/billing/billing-service/src/refund_invoice.rs"
    it "offers wire-command on a field of the command's struct, not on another struct" $ do
      acts <- real "wire-command" cancel "billing/billing-service/src/cancel_invoice.rs" (9, 9)
      map (\a -> (actLabel a, actSets a, actMissing a)) acts `shouldBe` [("Wire cancel_invoice's dependencies", [("command", "cancel_invoice"), ("service", "billing")], [])]
      real "wire-command" cancel "billing/billing-service/src/cancel_invoice.rs" (13, 20) >>= (`shouldBe` [])
    it "offers failure-arms on a Failure variant, binding the variant" $ do
      acts <- real "failure-arms" refund "billing/billing-service/src/refund_invoice.rs" (28, 7)
      map (\a -> (actLabel a, actSets a, actMissing a)) acts `shouldBe` [("Add AlreadyRefunded to every match", [("command", "refund_invoice"), ("failure", "AlreadyRefunded"), ("service", "billing")], [])]
