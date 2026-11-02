{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.RulesSpec (spec) where

import Data.Functor.Identity (Identity (..), runIdentity)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language (Language, Match (..), Syntax (..), querying, SyntaxFailure (..), parseLanguage)
import Olifant.Newness (NewLines (..))
import Data.Set qualified as S
import Olifant.Rules
import Olifant.Template (templateSource)
import Test.Hspec

rust :: Language
rust = l
  where
    Right l = parseLanguage "rust" "/p" (T.unlines ["files = [\"**/*.rs\"]", "comment = \"//\"", "grammar = { git = \"https://github.com/tree-sitter/tree-sitter-rust\", rev = \"e2bee853694a1d3e0f6ef308fe3674542fec95d7\" }"])

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
diesel = querying $ \_ _ content ->
  Identity (Right [Match (i, 0) (i, T.length l) l (0, 0) | (i, l) <- zip [0 ..] (T.lines content), "use diesel" `T.isPrefixOf` T.stripStart l])

cmd :: FilePath
cmd = "billing/billing-service/src/cancel_invoice.rs"

run :: RulesConfig -> [(FilePath, Text)] -> [FilePath] -> ([Finding], [Exception])
run config files toCheck =
  runIdentity (checkFiles diesel [rust] config [] (\_ -> Identity AllNew) rules (map fst files) (\p -> Identity (lookup p files)) toCheck)

none :: RulesConfig
none = RulesConfig [] []

findings :: RulesConfig -> Text -> [(Text, Maybe Int, Severity)]
findings config content = [(fRule f, fmap (fst . fst) (fRange f), fSeverity f) | f <- fst (run config [(cmd, content)] [cmd])]

-- A python adapter for `.py` files, and a fake syntax where each `def` is a
-- function_definition: its line, through its last non-blank line before
-- the next line at column 0 (0-based, as ast-grep reports).
python :: Language
python = either (error . T.unpack) id (parseLanguage "python" "/p" "files = [\"**/*.py\"]\ncomment = \"#\"\ngrammar = { git = \"https://github.com/tree-sitter/tree-sitter-python\", rev = \"293fdc02038ee2bf0e2e206711b69c90ac0d413f\" }\n")

defs :: Syntax Identity
defs = querying $ \_ _ content ->
  let ls = zip [0 ..] (T.lines content)
      starts = [i | (i, l) <- ls, "def " `T.isPrefixOf` l]
      end i = last (i : [j | (j, l) <- takeWhile (\(j, l) -> j == i || T.null l || " " `T.isPrefixOf` l) (drop i ls), not (T.null (T.strip l))])
   in Identity (Right [Match (i, 0) (end i, T.length (snd (ls !! end i))) (T.unlines [l | (j, l) <- ls, j >= i, j <= end i]) (0, 0) | i <- starts])

-- | `only = "new"` rules over a legacy file with two functions (lines 1–2, 4–5).
legacyRule :: Text
legacyRule = "[rules.no-new-defs]\nfiles = \"{legacy}\"\nonly = \"new\"\nmatch = { kind = \"function_definition\" }\nmessage = \"new work goes elsewhere\"\n"

legacyFile :: Text
legacyFile = T.unlines ["def a():", "    return 1", "", "def b(x):", "    return x"]

checkLegacy :: [Text] -> NewLines -> [Finding]
checkLegacy legacy newness =
  let Right rs = parseRules legacyRule
      f = "legacy/views.py"
   in fst (runIdentity (checkFiles defs [python] none legacy (\_ -> Identity newness) rs [f] (\_ -> Identity (Just legacyFile)) [f]))

spec :: Spec
spec = describe "rules (spec §5.9)" $ do
  describe "only = \"new\", {legacy}" $ do
    it "(a) reports a definition that is wholly new" $
      map fRule (checkLegacy ["legacy/**"] (NewIn (S.fromList [4, 5]) S.empty)) `shouldBe` ["no-new-defs"]
    it "(b) not a definition with any old line" $
      checkLegacy ["legacy/**"] (NewIn (S.fromList [5]) S.empty) `shouldBe` []
    it "(c) not a definition whose def line alone changed (Review Focus 1)" $
      checkLegacy ["legacy/**"] (NewIn (S.fromList [4]) S.empty) `shouldBe` []
    it "(d) without a base: one note that the rule is inactive, saying why" $
      [(fRule f, fSeverity f, "no ref `main`" `T.isInfixOf` fMessage f) | f <- checkLegacy ["legacy/**"] (NoBase "no ref `main`")] `shouldBe` [("rule-inactive", Hint, True)]
    it "(e) {legacy} with no [migrate] legacy is a rule error" $
      map fRule (checkLegacy [] AllNew) `shouldBe` ["rule-failed"]
    it "(g) a fix's `with` may use finding_name: the found definition's name" $ do
      let ruleText = legacyRule <> "[[rules.no-new-defs.fixes]]\ngenerator = \"p/delegate\"\nwith = { name = \"{= finding_name =}\" }\n"
          Right rs = parseRules ruleText
          f = "legacy/views.py"
          (fs, _) = runIdentity (checkFiles defs [python] none ["legacy/**"] (\_ -> Identity (NewIn (S.fromList [4, 5]) S.empty)) rs [f] (\_ -> Identity (Just legacyFile)) [f])
      [foSets o | fd <- fs, o <- fFixes fd] `shouldBe` [[("name", "b")]]
    it "(h) an old definition costs no query for its fix's code (only new ones are fixed)" $ do
      -- every query counted: one for the matches, then one per kept finding's replacement
      calls <- newIORef (0 :: Int)
      let counting = querying $ \l r t -> modifyIORef' calls (+ 1) >> pure (runIdentity (querySyntax defs l r t))
          ruleText = legacyRule <> "[[rules.no-new-defs.fixes]]\ngenerator = \"p/delegate\"\nreplace = { enclosing = [\"function_definition\"], text = \"pass\" }\n"
          Right rs = parseRules ruleText
          f = "legacy/views.py"
      (fs, _) <- checkFiles counting [python] none ["legacy/**"] (\_ -> pure (NewIn (S.fromList [4, 5]) S.empty)) rs [f] (\_ -> pure (Just legacyFile)) [f]
      map fRule fs `shouldBe` ["no-new-defs"]
      readIORef calls `shouldReturn` 2
    it "(k) `bind` takes values from the found code; `when` keeps a finding only where another file says so (review #5)" $ do
      let ruleText = "[rules.flag-on]\nfiles = \"legacy/*.py\"\nmatch = { kind = \"function_definition\" }\nbind = { fn = \"^def ([a-z]+)\" }\nwhen = { files = \"flags/*.txt\", matches = \"^{= fn =} = on$\" }\nmessage = \"its flag is on\"\n"
          Right rs = parseRules ruleText
          f = "legacy/views.py"
          files = M.fromList [(f, legacyFile), ("flags/x.txt", "a = off\nb = on\n")]
          (fs, _) = runIdentity (checkFiles defs [python] none [] (\_ -> Identity AllNew) rs (M.keys files) (\p -> Identity (M.lookup p files)) [f])
      [fText fd | fd <- fs] `shouldBe` ["def b(x):\n    return x\n"]
    it "(l) a bug fix that rewrites every line of a small function is not new work: its name was there (review minor #11)" $
      checkLegacy ["legacy/**"] (NewIn (S.fromList [4, 5]) (S.fromList ["a", "b"])) `shouldBe` []
    it "(f) a file outside the legacy globs is not checked" $ do
      let Right rs = parseRules legacyRule
      fst (runIdentity (checkFiles defs [python] none ["legacy/**"] (\_ -> Identity AllNew) rs ["src/x.py"] (\_ -> Identity (Just legacyFile)) ["src/x.py"])) `shouldBe` []

  it "(a) a match rule finds its node, with message, fix and the path's slots" $ do
    let [f] = fst (run none [(cmd, "use std::sync::Arc;\nuse diesel::prelude::*;\n")] [cmd])
    (fRule f, fRange f, fSeverity f, fMessage f, fFixes f)
      `shouldBe` ("commands-use-ports", Just ((2, 1), (2, 24)), Warn, "Commands talk to ports, not the database", [FixOffer "rust-htmx/service-repository" Nothing [("command", "cancel_invoice"), ("service", "billing")] Nothing []])
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
          (fs, _) = runIdentity (checkFiles diesel [rust] none [] (\_ -> Identity AllNew) [r] [cmd] (\_ -> Identity (Just "use diesel::x;\n")) [cmd])
      map fFixes fs `shouldBe`
        [ [ FixOffer "rust-htmx/command-dependency" (Just "Use an existing port") [("command", "cancel_invoice"), ("service", "billing")] Nothing []
          , FixOffer "rust-htmx/service-repository" (Just "Add a new repository port") [("service", "billing")] Nothing [] ] ]
    it "keeps one finding where matches nest (the outermost)" $ do
      let nested = querying $ \_ _ _ -> Identity (Right [Match (0, 4) (0, 10) "diesel" (0, 0), Match (0, 0) (0, 20) "use diesel::prelude" (0, 0)])
          (fs, _) = runIdentity (checkFiles nested [rust] none [] (\_ -> Identity AllNew) rules [cmd] (\_ -> Identity (Just "use diesel::prelude;\n")) [cmd])
      map fRange fs `shouldBe` [Just ((1, 1), (1, 21))]
    it "pre-chooses the choice whose words appear in the code" $ do
      suggestChoice "let rows = invoices::table.filter(x);" ["email_sender", "invoice_repository"] `shouldBe` Just "invoice_repository"
      suggestChoice "diesel::insert_into(things)" ["email_sender", "invoice_repository"] `shouldBe` Nothing
      suggestChoice "invoice_emails::table" ["email_sender", "invoice_repository"] `shouldBe` Nothing
  describe "fixes that replace the code they found (beat 4 feedback)" $ do
    let withReplace = T.unlines
          [ "[rules.p]", "files = \"{service}/*-service/src/*.rs\"", "message = \"m\"", "match = { kind = \"scoped_identifier\" }"
          , "[[rules.p.fixes]]", "generator = \"g\"", "with = { service = \"{= service =}\" }"
          , "replace = [ { enclosing = [\"use_declaration\"], text = \"\" }, { enclosing = [\"let_declaration\", \"expression_statement\"], text = \"{= ? call: stmt | do {= finding =} through a port =}\" } ]" ]
        Right [r] = parseRules withReplace
        -- the rule matches `diesel::x`; enclosing queries see the import or the let
        syn = querying $ \_ rule content ->
          let txt = T.pack (show rule)
              hits = [(i, l) | (i, l) <- zip [0 ..] (T.lines content), "diesel::x" `T.isInfixOf` l]
              col l = T.length (fst (T.breakOn "diesel::x" l))
           in Identity . Right $
                if "use_declaration" `T.isInfixOf` txt && not ("let_declaration" `T.isInfixOf` txt)
                  then [Match (i, 0) (i, T.length l) (T.strip l) (0, 0) | (i, l) <- hits, "use " `T.isPrefixOf` T.strip l]
                  else if "let_declaration" `T.isInfixOf` txt
                    then [Match (i, 4) (i, T.length l) (T.strip l) (0, 0) | (i, l) <- hits, "let " `T.isPrefixOf` T.strip l]
                    else [Match (i, col l) (i, col l + 9) "diesel::x" (0, 0) | (i, l) <- hits]
        offersFor content = map (map foReplace . fFixes) (fst (runIdentity (checkFiles syn [rust] none [] (\_ -> Identity AllNew) [r] [cmd] (\_ -> Identity (Just content)) [cmd])))
    it "replaces the enclosing statement (lines, old text, indent, the node's code)" $
      map (fmap (fmap (\rp -> (rpLines rp, rpOld rp, rpIndent rp, rpFinding rp)))) (offersFor "fn f() {\n    let rows = diesel::x;\n}\n")
        `shouldBe` [[Just ((2, 2), "    let rows = diesel::x;", "    ", "let rows = diesel::x;")]]
    it "deletes an import: the first case whose node encloses the finding wins" $
      map (fmap (fmap (\rp -> (rpLines rp, templateSource (rpText rp))))) (offersFor "use diesel::x;\nfn f() {}\n")
        `shouldBe` [[Just ((1, 1), "")]]
  describe "rules that cannot run (review C2)" $ do
    it "a syntax failure is an error finding, not silence" $ do
      let broken = querying $ \_ _ _ -> Identity (Left (SyntaxToolMissing "ast-grep is not installed"))
          (fs, _) = runIdentity (checkFiles broken [rust] none [] (\_ -> Identity AllNew) rules [cmd] (\_ -> Identity (Just "use diesel::x;\n")) [cmd])
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
