{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.RulesSpec (spec) where

import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language (Language, Match (..), Syntax (..), SyntaxFailure (..), parseLanguage)
import Olifant.Rules
import Test.Hspec

rust :: Language
rust = l
  where
    Right l = parseLanguage "rust" "/p" (T.unlines ["files = [\"**/*.rs\"]", "comment = \"//\"", "grammar = \"rust\""])

usePorts :: Text
usePorts =
  T.unlines
    [ "[rules.commands-use-ports]"
    , "files    = \"{service}/*-service/src/{command}.rs\""
    , "exclude  = [\"**/lib.rs\"]"
    , "match    = { kind = \"use_declaration\", regex = \"diesel\" }"
    , "message  = \"Commands talk to ports, not the database\""
    , "severity = \"warn\""
    , "fix      = \"rust-htmx/service-repository\""
    , "[rules.ports-have-fakes]"
    , "files   = \"*/src/ports/*.rs\""
    , "missing = \"{= dir =}/../fakes/{= stem =}.rs\""
    , "message = \"Every port has a fake\""
    ]

rules :: [RuleDef]
rules = r
  where
    Right r = parseRules usePorts

-- Every line containing `use diesel` is a use_declaration match (0-based).
diesel :: Syntax Identity
diesel = Syntax $ \_ _ content ->
  Identity (Right [Match (i, 0) (i, T.length l) l | (i, l) <- zip [0 ..] (T.lines content), "use diesel" `T.isPrefixOf` T.stripStart l])

cmd :: FilePath
cmd = "billing/billing-service/src/cancel_invoice.rs"

run :: RulesConfig -> [(FilePath, Text)] -> [FilePath] -> ([Finding], [Exception])
run config files toCheck =
  runIdentity (checkFiles diesel [rust] config rules (map fst files) (\p -> Identity (lookup p files)) toCheck)

none :: RulesConfig
none = RulesConfig [] []

findings :: RulesConfig -> Text -> [(Text, Maybe Int, Severity)]
findings config content = [(fRule f, fmap (fst . fst) (fRange f), fSeverity f) | f <- fst (run config [(cmd, content)] [cmd])]

spec :: Spec
spec = describe "rules (spec §5.9)" $ do
  it "(a) a match rule finds its node, with message, fix and the path's slots" $ do
    let [f] = fst (run none [(cmd, "use std::sync::Arc;\nuse diesel::prelude::*;\n")] [cmd])
    (fRule f, fRange f, fSeverity f, fMessage f, fFixes f)
      `shouldBe` ("commands-use-ports", Just ((2, 1), (2, 24)), Warn, "Commands talk to ports, not the database", [FixOffer "rust-htmx/service-repository" Nothing [("command", "cancel_invoice"), ("service", "billing")]])
    fst (run none [("billing/billing-service/src/lib.rs", "use diesel::x;\n")] ["billing/billing-service/src/lib.rs"]) `shouldBe` []
  it "(b) a missing rule finds a file that should exist" $ do
    let port = "billing/src/ports/email_sender.rs"
        fake = "billing/src/fakes/email_sender.rs"
    map fRule (fst (run none [(port, "")] [port])) `shouldBe` ["ports-have-fakes"]
    fst (run none [(port, ""), (fake, "")] [port]) `shouldBe` []
  it "(c) an allow comment on the line or the line above suppresses, with its reason" $ do
    findings none "use diesel::x; // olifant:allow commands-use-ports — migration script\n" `shouldBe` []
    let (fs, exs) = run none [(cmd, "// olifant:allow commands-use-ports — migration script\nuse diesel::x;\n")] [cmd]
    fs `shouldBe` []
    map (\e -> (exRule e, exReason e, exStale e)) exs `shouldBe` [("commands-use-ports", Just "migration script", False)]
  it "(d) a file allow in the first lines suppresses the whole file" $ do
    let (fs, exs) = run none [(cmd, "// olifant:allow-file commands-use-ports — legacy\n\nuse diesel::a;\nuse diesel::b;\n")] [cmd]
    fs `shouldBe` []
    map exRule exs `shouldBe` ["commands-use-ports"]
  it "(e) an allow without a reason still suppresses, but warns (Review Focus 2)" $
    findings none "// olifant:allow commands-use-ports\nuse diesel::x;\n" `shouldBe` [("allow-without-reason", Just 1, Warn)]
  it "(f) an allow naming an unknown rule warns (Review Focus 2)" $
    findings none "// olifant:allow comands-use-ports — typo\nuse diesel::x;\n" `shouldBe` [("unknown-rule", Just 1, Warn), ("commands-use-ports", Just 2, Warn)]
  it "(g) an allow that suppresses nothing is stale (Review Focus 2)" $
    map (\e -> (exRule e, exStale e)) (snd (run none [(cmd, "// olifant:allow commands-use-ports — old\nfn x() {}\n")] [cmd])) `shouldBe` [("commands-use-ports", True)]
  it "(h) only a real comment for the exact rule suppresses (Review Focus 5)" $ do
    findings none "let s = \"// olifant:allow commands-use-ports — x\";\nuse diesel::y;\n" `shouldSatisfy` any (\(r, _, _) -> r == "commands-use-ports")
    findings none "// olifant:allow ports-have-fakes — x\nuse diesel::y;\n" `shouldSatisfy` any (\(r, _, _) -> r == "commands-use-ports")
  it "(i) the project's [rules] set severity, turn rules off and allow paths with a reason" $ do
    findings (RulesConfig [("commands-use-ports", Off)] []) "use diesel::x;\n" `shouldBe` []
    findings (RulesConfig [("commands-use-ports", Error)] []) "use diesel::x;\n" `shouldBe` [("commands-use-ports", Just 1, Error)]
    let (fs, exs) = run (RulesConfig [] [("commands-use-ports", ["billing/**"], "billing migrates later")]) [(cmd, "use diesel::x;\n")] [cmd]
    fs `shouldBe` []
    map exReason exs `shouldBe` [Just "billing migrates later"]
  describe "ranked fixes and one finding per line (beat 4 feedback)" $ do
    it "offers several fixes, each with its label and inputs from the finding" $ do
      let Right [r] = parseRules (T.unlines
            [ "[rules.p]", "files = \"{service}/*-service/src/*.rs\"", "message = \"m\"", "match = { kind = \"use_declaration\" }"
            , "[[rules.p.fixes]]", "generator = \"rust-htmx/command-dependency\"", "label = \"Use an existing port\"", "with = { service = \"{= service =}\", command = \"{= stem =}\" }"
            , "[[rules.p.fixes]]", "generator = \"rust-htmx/service-repository\"", "label = \"Add a new repository port\"", "with = { service = \"{= service =}\" }" ])
          (fs, _) = runIdentity (checkFiles diesel [rust] none [r] [cmd] (\_ -> Identity (Just "use diesel::x;\n")) [cmd])
      map fFixes fs `shouldBe`
        [ [ FixOffer "rust-htmx/command-dependency" (Just "Use an existing port") [("command", "cancel_invoice"), ("service", "billing")]
          , FixOffer "rust-htmx/service-repository" (Just "Add a new repository port") [("service", "billing")] ] ]
    it "keeps one finding where matches nest (the outermost)" $ do
      let nested = Syntax $ \_ _ _ -> Identity (Right [Match (0, 4) (0, 10) "diesel", Match (0, 0) (0, 20) "use diesel::prelude"])
          (fs, _) = runIdentity (checkFiles nested [rust] none rules [cmd] (\_ -> Identity (Just "use diesel::prelude;\n")) [cmd])
      map fRange fs `shouldBe` [Just ((1, 1), (1, 21))]
    it "pre-chooses the choice whose words appear in the code" $ do
      suggestChoice "let rows = invoices::table.filter(x);" ["email_sender", "invoice_repository"] `shouldBe` Just "invoice_repository"
      suggestChoice "diesel::insert_into(things)" ["email_sender", "invoice_repository"] `shouldBe` Nothing
      suggestChoice "invoice_emails::table" ["email_sender", "invoice_repository"] `shouldBe` Nothing
  describe "rules that cannot run (review C2)" $ do
    it "a syntax failure is an error finding, not silence" $ do
      let broken = Syntax $ \_ _ _ -> Identity (Left (SyntaxToolMissing "ast-grep is not installed"))
          (fs, _) = runIdentity (checkFiles broken [rust] none rules [cmd] (\_ -> Identity (Just "use diesel::x;\n")) [cmd])
      map (\f -> (fRule f, fSeverity f)) fs `shouldBe` [("rule-failed", Error)]
      map fMessage fs `shouldSatisfy` all ("ast-grep is not installed" `T.isInfixOf`)
    it "a missing template may only use dir, stem and file" $
      parseRules "[rules.x]\nfiles = \"*\"\nmessage = \"m\"\nmissing = \"{= dir =}/fakes/{= file_name =}.rs\"\n" `shouldSatisfy` either ("file_name" `T.isInfixOf`) (const False)
  it "I11: an escaped quote does not hide a string (allow inside a string never suppresses)" $
    findings none "const S: &str = \"say \\\"hi // olifant:allow commands-use-ports - x\";\nuse diesel::y;\n" `shouldSatisfy` any (\(r, _, _) -> r == "commands-use-ports")
  describe "ratchet (spec §5.10)" $ do
    it "(b) a finding's fingerprint survives moving lines, and differs for different code" $ do
      let fp content = map fFingerprint (fst (run none [(cmd, content)] [cmd]))
      fp "use diesel::prelude::*;\n" `shouldBe` fp "\n\n// moved\nuse diesel::prelude::*;\n"
      fingerprint ["r", "use   diesel::x;"] `shouldBe` fingerprint ["r", "use diesel::x;"]
      fp "use diesel::prelude::*;\n" `shouldNotBe` fp "use diesel::r2d2;\n"
    it "(e) since and why are part of the message" $ do
      let Right [r] = parseRules "[rules.x]\nfiles = \"*.rs\"\nmessage = \"No.\"\nmatch = { kind = \"a\" }\nsince = \"2026-10\"\nwhy = \"ADR-12\"\n"
      ruleMessage r `shouldBe` "No. (since 2026-10: ADR-12)"
  it "I9: the baseline counts: a second identical finding is new" $ do
    let fs = fst (run none [(cmd, "use diesel::x;\nuse diesel::x;\n")] [cmd])
        key f = (fRule f, T.pack (fFile f), fFingerprint f)
    map snd (markBaselined [(key (head' fs), 1)] fs) `shouldBe` [True, False]
  describe "loading" $ do
    it "reads [rules] config: severities and allows" $
      parseRulesConfig (M.fromList []) `shouldBe` Right (RulesConfig [] [])
    it "refuses a rule with both match and missing, or neither, or an unknown severity" $ do
      parseRules "[rules.x]\nfiles = \"*\"\nmessage = \"m\"\n" `shouldSatisfy` either ("match" `T.isInfixOf`) (const False)
      parseRules "[rules.x]\nfiles = \"*\"\nmessage = \"m\"\nmatch = { kind = \"a\" }\nmissing = \"b\"\n" `shouldSatisfy` either ("match" `T.isInfixOf`) (const False)
      parseRules "[rules.x]\nfiles = \"*\"\nmessage = \"m\"\nmatch = { kind = \"a\" }\nseverity = \"loud\"\n" `shouldSatisfy` either ("hint, warn, error" `T.isInfixOf`) (const False)
    it "suggests the right key for a typo" $
      parseRules "[rules.x]\nfiles = \"*\"\nmesage = \"m\"\nmatch = { kind = \"a\" }\n" `shouldSatisfy` either ("message" `T.isInfixOf`) (const False)

head' :: [a] -> a
head' = \case
  x : _ -> x
  [] -> error "empty"
