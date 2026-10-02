{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.LanguageSpec (spec) where

import Data.Aeson (object, (.=))
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.ByteString qualified as BS
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Text.IO qualified as TIO
import Olifant.Language.Elm (parseElmReport, qualifyPattern, visibleAt)
import Olifant.Anchor (RenderedAnchor (..), locIndent, locLine, locOpen, resolveLayers)
import Olifant.Language
import Olifant.Language.AstGrep (astGrepSyntax)
import System.Directory (findExecutable, getTemporaryDirectory)
import System.FilePath ((</>))
import Olifant.Template (Value (..))
import Control.Monad (forM_)
import Olifant.Language.Diagnostics (CompilerSupport (..))
import Olifant.Language.Support (supportFor)
import Data.Text.Encoding qualified as TE
import Olifant.Language.Ghc (parseGhcReport, typeOf)
import Test.Hspec

elmToml :: Text
elmToml =
  T.unlines
    [ "files = [\"src/**/*.elm\"]"
    , "comment = \"--\""
    , "grammar = { source = \"grammar/elm\" }"
    , "placeholder = 'Debug.todo \"{= message =}\"'"
    , "branch = \"{= pattern =} ->\\n    {= body =}\""
    , "[check]"
    , "command = [\"elm\", \"make\", \"{= files =}\", \"--report=json\", \"--output=/dev/null\"]"
    , "context = [\"elm.json\"]"
    , "format = \"elm-json\""
    , "[targets.case_in]"
    , "phrase = \"case in FUNCTION\""
    , "container = { kind = \"case_of_expr\", inside = { kind = \"value_declaration\", regex = \"^{= FUNCTION =}\\\\b\", stopBy = \"end\" } }"
    , "items = { kind = \"case_of_branch\" }"
    , "example = \"end of case in update\""
    ]

spec :: Spec
spec = do
  describe "parseLanguage" $ do
    it "reads an adapter, resolving the grammar source against the pack" $ do
      let Right lang = parseLanguage "elm" "/packs/elm" elmToml
      langName lang `shouldBe` "elm"
      langGrammar lang `shouldBe` GrammarSource "/packs/elm/languages/grammar/elm"
      fmap checkFormat (langCheck lang) `shouldBe` Just "elm-json"
      map targetPhrase (langTargets lang) `shouldBe` ["case in FUNCTION"]
    it "accepts a built-in grammar name" $
      fmap langGrammar (parseLanguage "rust" "/p" "files = [\"**/*.rs\"]\ncomment = \"//\"\ngrammar = \"rust\"\n") `shouldBe` Right (BuiltInGrammar "rust")
    it "rejects unknown keys" $
      parseLanguage "x" "/p" "files = []\ncomment = \"#\"\ngrammar = \"x\"\ncolour = 1\n" `shouldSatisfy` either ("colour" `T.isInfixOf`) (const False)
    it "refuses the retired `qualify` key, saying what replaced it" $
      parseLanguage "x" "/p" "files = [\"**/*.x\"]\ncomment = \"--\"\ngrammar = \"x\"\nqualify = \"elm\"\n"
        `shouldSatisfy` either ("the [check] format decides" `T.isInfixOf`) (const False)
    it "refuses a check format with no compiler support, listing the known ones" $
      parseLanguage "x" "/p" "files = [\"**/*.x\"]\ncomment = \"--\"\ngrammar = \"x\"\n[check]\ncommand = [\"x\"]\nformat = \"cobol-json\"\n"
        `shouldSatisfy` either (\e -> "unknown format `cobol-json`" `T.isInfixOf` e && "elm-json" `T.isInfixOf` e) (const False)
    it "renders a target's rule with the phrase's parameters" $ do
      let Right lang = parseLanguage "elm" "/p" elmToml
          [t] = langTargets lang
          Just binds = matchShape t ["case", "in", "update"]
      renderRule (M.fromList [(k, VText v) | (k, v) <- binds]) (targetContainer t)
        `shouldBe` Right (object ["kind" .= ("case_of_expr" :: Text), "inside" .= object ["kind" .= ("value_declaration" :: Text), "regex" .= ("^update\\b" :: Text), "stopBy" .= ("end" :: Text)]])
  describe "matchShape" $ do
    it "binds parameters and requires literal words" $ do
      let Right lang = parseLanguage "elm" "/p" elmToml
          [t] = langTargets lang
      matchShape t ["case", "in", "view"] `shouldBe` Just [("FUNCTION", "view")]
      matchShape t ["case", "of", "view"] `shouldBe` Nothing
      matchShape t ["case", "in"] `shouldBe` Nothing
  describe "languageFor" $
    it "picks the first adapter whose globs match" $ do
      let Right elm = parseLanguage "elm" "/p" elmToml
      fmap langName (languageFor [elm] "src/Main.elm") `shouldBe` Just "elm"
      fmap langName (languageFor [elm] "src/lib.rs") `shouldBe` Nothing
  describe "ports without tools" $ do
    it "noSyntax reports ast-grep as missing, with the install command" $ do
      let Right lang = parseLanguage "elm" "/p" elmToml
      r <- querySyntax noSyntax lang (object []) "x"
      either (const True) (const False) r `shouldBe` True
      either describeSyntaxFailure (const "") r `shouldSatisfy` ("brew install ast-grep" `T.isInfixOf`)
    it "noChecker reports that no check is available" $ do
      let Right lang = parseLanguage "elm" "/p" elmToml
      r <- runCheck noChecker lang M.empty
      either describeCheckFailure (const "") r `shouldSatisfy` ("no checker" `T.isInfixOf`)
  describe "the Elm adapter (ast-grep, fixture Main.elm)" $ do
    let at syn elm file p = fmap (fmap locLine) (resolveLayers syn (Just elm) [PhraseAnchor p] file)
        lineOf file needle = length (takeWhile (not . (needle `T.isInfixOf`)) (T.lines file))
    it "end of type Page goes after the last variant" $ withAstGrep $ \syn elm file ->
      at syn elm file "end of type Page" `shouldReturn` Right (lineOf file "| SamplePage SamplePage.Model" + 1)
    it "end of case in update skips the catch-all and the nested case" $ withAstGrep $ \syn elm file ->
      at syn elm file "end of case in update" `shouldReturn` Right (lineOf file "SamplePage.update subMsg page" + 1)
    it "end of case in view goes after the last branch" $ withAstGrep $ \syn elm file ->
      at syn elm file "end of case in view" `shouldReturn` Right (lineOf file "viewPage GotSamplePageMsg (SamplePage.view page)" + 1)
    it "end of imports goes after the last import" $ withAstGrep $ \syn elm file ->
      at syn elm file "end of imports" `shouldReturn` Right (lineOf file "import Url exposing (Url)" + 1)
    it "end of list in parser goes after the last element (Route.elm)" $ withAstGrepOn "src/Route.elm" $ \syn elm file ->
      at syn elm file "end of list in parser" `shouldReturn` Right (lineOf file ", Parser.map SamplePage (s \"sample-page\")" + 1)
    it "end of type Route goes after the last variant (Route.elm)" $ withAstGrepOn "src/Route.elm" $ \syn elm file ->
      at syn elm file "end of type Route" `shouldReturn` Right (lineOf file "| SamplePage" + 1)
    it "end of case skips a case nested in the catch-all branch (review)" $ withAstGrep $ \syn elm _ -> do
      let src = T.unlines ["module A exposing (..)", "", "update msg model =", "    case msg of", "        Foo ->", "            model", "", "        _ ->", "            case model of", "                X ->", "                    1", "", "                Y ->", "                    2"]
      at syn elm src "end of case in update" `shouldReturn` Right 6
    it "a rule ast-grep rejects on a large file is a failure, not a crash (review)" $ withAstGrep $ \syn elm _ -> do
      let big = T.replicate 20000 "x = 1\n\n"
      r <- querySyntax syn elm (object ["kind" .= ("no_such_kind" :: Text)]) big
      either (const True) (const False) r `shouldBe` True
    it "an unknown function is a miss, not a crash" $ withAstGrep $ \syn elm file ->
      fmap (either (const "miss") (const "hit")) (at syn elm file "end of case in nope") `shouldReturn` ("miss" :: Text)

  describe "the Rust adapter (ast-grep, fixture service.rs)" $ do
    let at syn lang file p = fmap (fmap locLine) (resolveLayers syn (Just lang) [PhraseAnchor p] file)
        lineOf file needle = length (takeWhile (not . (needle `T.isInfixOf`)) (T.lines file))
    it "end of data goes after the last constructor (haskell adapter)" $ withHaskell $ \syn lang file ->
      fmap (fmap (\l -> (locLine l, locIndent l))) (resolveLayers syn (Just lang) [PhraseAnchor "end of data SampleError"] file)
        `shouldReturn` Right (6, "  ")
    it "end of parameters in new goes after the last parameter" $ withRustFile "billing/billing-service/src/service.rs" $ \syn lang file ->
      at syn lang file "end of parameters in new" `shouldReturn` Right (lineOf file "invoice_repository: Arc<dyn InvoiceRepository>," + 1)
    it "end of literal VoidInvoice in new opens the empty literal" $ withRustFile "billing/billing-service/src/service.rs" $ \syn lang file ->
      fmap (fmap locOpen) (resolveLayers syn (Just lang) [PhraseAnchor "end of literal VoidInvoice in new"] file) >>= (`shouldSatisfy` either (const False) isJust)
    it "end of literal RefundInvoice in new goes after its last field" $ withRustFile "billing/billing-service/src/service.rs" $ \syn lang file ->
      at syn lang file "end of literal RefundInvoice in new" `shouldReturn` Right (lineOf file "invoice_repository: invoice_repository.clone()," + 1)
    it "end of call … BillingService::new in main goes after the last argument" $ withRustFile "main/src/main.rs" $ \syn lang file ->
      at syn lang file "end of call billing_service::service::BillingService::new in main" `shouldReturn` Right (lineOf file "invoice_repository.clone()," + 1)
    it "end of call … NotificationsService::new in main opens the empty argument list" $ withRustFile "main/src/main.rs" $ \syn lang file ->
      fmap (fmap locOpen) (resolveLayers syn (Just lang) [PhraseAnchor "end of call notifications_service::service::NotificationsService::new in main"] file) >>= (`shouldSatisfy` either (const False) isJust)
    it "end of struct goes after the last field" $ withRust $ \syn lang file ->
      at syn lang file "end of struct BillingService" `shouldReturn` Right (lineOf file "pub refund_invoice: RefundInvoice," + 1)
    it "end of impl goes after the last method" $ withRust $ \syn lang file ->
      at syn lang file "end of impl BillingService" `shouldReturn` Right (lineOf file "self.refund_invoice.refund_invoice(input).await" + 2)
    it "end of literal in new goes after the last initializer" $ withRust $ \syn lang file ->
      at syn lang file "end of literal in new" `shouldReturn` Right (lineOf file "refund_invoice: RefundInvoice {}," + 1)
    it "end of crate imports goes after the last imported item" $ withRust $ \syn lang file ->
      at syn lang file "end of crate imports" `shouldReturn` Right (lineOf file "refund_invoice::{RefundInvoice" + 1)
  describe "compiler support" $ do
    it "maps the elm-json format to Elm's support" $
      fmap supportFormat (supportFor "elm-json") `shouldBe` Just "elm-json"
    it "has no support for an unknown format" $
      fmap supportFormat (supportFor "cobol-json") `shouldBe` Nothing
    it "keeps the language-neutral modules free of Elm" $
      forM_ ["src/Olifant/Generation/Complete.hs", "src/Olifant/Language/Check.hs"] $ \f -> do
        src <- TIO.readFile f
        (f, "Olifant.Language.Elm" `T.isInfixOf` src) `shouldBe` (f, False)
  describe "GHC support (fixtures captured from GHC 9.10.3)" $ do
    let real = BS.readFile "src/Olifant/Language/testdata/ghc-62161.jsonl"
    it "reads one pattern per line, the full type, and only case alternatives as sites (review C1, C2, I1)" $ do
      Right r <- parseGhcReport <$> real
      [(missingPatterns m, missingType m) | m <- reportMissing r]
        `shouldBe` [ (["SampleBroken _ _", "SampleLocked _"], Just "SampleError")
                   , (["((SampleLocked _), _)"], Just "(SampleError, Bool)")
                   , (["Just (SampleLocked _)"], Just "Maybe SampleError")
                   ]
      map (fst . missingCase) (reportMissing r) `shouldBe` [8, 12, 17]
    it "puts equations, lambdas and pattern bindings in reportUnplaced, described (review I1)" $ do
      Right r <- parseGhcReport <$> real
      [(missingPatterns m, d) | (m, d) <- reportUnplaced r]
        `shouldBe` [ (["SampleLocked _"], "an equation for `label`")
                   , (["SampleBroken _ _", "SampleLocked _"], "a lambda abstraction")
                   , (["SampleBroken _ _", "SampleLocked _"], "a pattern binding")
                   ]
    it "reads a missing-pattern site even when -Werror makes it an error (review I2)" $ do
      Right r <- parseGhcReport <$> BS.readFile "src/Olifant/Language/testdata/ghc-62161-werror.jsonl"
      (map missingPatterns (reportMissing r), reportBroken r) `shouldBe` ([["SampleBroken _ _", "SampleLocked _"]], [])
    it "drops GHC's truncation marker" $
      fmap (map missingPatterns . reportMissing) (parseGhcReport (TE.encodeUtf8 ("{\"span\":{\"file\":\"src/A.hs\",\"start\":{\"line\":2,\"column\":1},\"end\":{\"line\":4,\"column\":1}},\"severity\":\"Warning\",\"code\":62161,\"message\":[\"Pattern match(es) are non-exhaustive\\nIn a case alternative:\\n    Patterns of type `T' not matched:\\n        A\\n        B\\n        ...\"]}")))
        `shouldBe` Right [["A", "B"]]
    it "reports errors as broken files and ignores other warnings" $ do
      let err = "{\"span\":{\"file\":\"src/B.hs\",\"start\":{\"line\":1,\"column\":1},\"end\":{\"line\":1,\"column\":2}},\"severity\":\"Error\",\"code\":88464,\"message\":[\"Variable not in scope: x\"]}"
          other = "{\"span\":{\"file\":\"src/C.hs\",\"start\":{\"line\":1,\"column\":1},\"end\":{\"line\":1,\"column\":2}},\"severity\":\"Warning\",\"code\":40910,\"message\":[\"Defined but not used\"]}"
      parseGhcReport (TE.encodeUtf8 (err <> "\n" <> other)) `shouldBe` Right (CheckReport [] [("src/B.hs", "Variable not in scope: x")] [])
    it "reads the type with ASCII or unicode quotes (Review Focus 3)" $ do
      typeOf "Patterns of type \8216CatalogError\8217 not matched: PackLocked _" `shouldBe` Just "CatalogError"
      typeOf "Patterns of type `Olifant.Catalog.CatalogError' not matched: PackLocked _" `shouldBe` Just "Olifant.Catalog.CatalogError"
    it "is visible where the type appears anywhere in GHC's type: tuples, Maybe, qualified (review C2)" $ do
      let Just g = supportFor "ghc-json"
          at ty = MissingPatterns "src/A.hs" (4, 8) ["x"] (Just ty)
      supportVisible g (at "(SampleError, Bool)") "" "SampleError.SampleLocked" `shouldBe` True
      supportVisible g (at "Maybe SampleError") "" "SampleError.SampleLocked" `shouldBe` True
      supportVisible g (at "Olifant.Catalog.CatalogError") "" "CatalogError.PackLocked" `shouldBe` True
      supportVisible g (at "SampleErrorish") "" "SampleError.SampleLocked" `shouldBe` False
      supportVisible g (at "RegionError") "" "SampleError.SampleLocked" `shouldBe` False
      supportVisible g (at "RegionError") "" "SampleLocked" `shouldBe` True
      supportPattern g "" "CatalogError.PackLocked" "PackLocked _" `shouldBe` "PackLocked _"
    it "asks GHC not to truncate pattern lists (review M2)" $ do
      src <- TIO.readFile "packs/olifant-dev/languages/haskell.toml"
      src `shouldSatisfy` T.isInfixOf "-fmax-uncovered-patterns="
  describe "parseElmReport" $ do
    it "reads each MISSING PATTERNS problem: file, 0-based case lines, patterns" $ do
      report <- BS.readFile "src/Olifant/Language/testdata/elm-missing-patterns.json"
      parseElmReport report
        `shouldBe` Right
          ( CheckReport
              [ missing "/tmp/elmcheck/src/Page/SamplePage.elm" (30, 32) ["SetName _"]
              , missing "/tmp/elmcheck/src/Route.elm" (29, 34) ["Settings"]
              ]
              []
              []
          )
    it "is empty for a clean compile" $ parseElmReport "" `shouldBe` Right (CheckReport [] [] [])
    it "lists files with other problems as broken (review)" $
      parseElmReport "{\"type\":\"compile-errors\",\"errors\":[{\"path\":\"/x/src/A.elm\",\"name\":\"A\",\"problems\":[{\"title\":\"NAMING ERROR\",\"region\":{\"start\":{\"line\":1,\"column\":0},\"end\":{\"line\":1,\"column\":3}},\"message\":[\"x\"]}]}]}"
        `shouldBe` Right (CheckReport [] [("/x/src/A.elm", "NAMING ERROR")] [])
    it "reports a non-compile error as a failure" $
      parseElmReport "{\"type\":\"error\",\"path\":\"elm.json\",\"title\":\"BAD JSON\",\"message\":[\"oops\"]}" `shouldSatisfy` either ("BAD JSON" `T.isInfixOf`) (const False)
  describe "qualifyPattern" $ do
    let mainElm = "module Main exposing (main)\nimport Route exposing (Route)\nimport Page.Home as Home\n"
    it "qualifies a constructor from another module" $
      qualifyPattern mainElm "Route.Settings" "Just Settings" `shouldBe` "Just Route.Settings"
    it "leaves it bare in its own module or when exposed" $ do
      qualifyPattern "module Route exposing (..)\n" "Route.Settings" "Settings" `shouldBe` "Settings"
      qualifyPattern "module Main exposing (main)\nimport Route exposing (..)\n" "Route.Settings" "Just Settings" `shouldBe` "Just Settings"
    it "uses an import alias" $
      qualifyPattern "module Main exposing (main)\nimport Page.Home as Home\n" "Page.Home.Reset" "Reset" `shouldBe` "Home.Reset"
    it "leaves unqualified constructors and other words alone" $
      qualifyPattern mainElm "SetName" "SetName _" `shouldBe` "SetName _"
  describe "visibleAt" $
    it "sees a qualified constructor only in its module or a module importing it" $ do
      let route = "module Route exposing (..)\nimport Url\n"
          mainElm = "module Main exposing (main)\nimport Route exposing (Route)\n"
      visibleAt route "Route.Settings" `shouldBe` True
      visibleAt route "Main.Settings" `shouldBe` False
      visibleAt mainElm "Route.Settings" `shouldBe` True
      visibleAt mainElm "Main.Settings" `shouldBe` True
      visibleAt route "Settings" `shouldBe` True
  describe "declarationAt" $
    it "finds the top-level declaration above a line" $
      declarationAt "toPath : Route -> String\ntoPath route =\n    case route of\n        Home ->\n" 2 `shouldBe` "toPath"
  describe "processChecker (real elm)" $ do
    it "reports the missing branch in a page's update" $
      findExecutable "elm" >>= \case
        Nothing -> pendingWith "elm is not installed"
        Just _ -> do
          src <- TIO.readFile "packs/elm/languages/elm.toml"
          let Right elm = parseLanguage "elm" "packs/elm" src
          files <- traverse (\p -> (p,) <$> TIO.readFile ("packs/elm/fixtures/app" </> p)) ["elm.json", "src/Main.elm", "src/Route.elm", "src/Page/Home.elm", "src/Page/SamplePage.elm"]
          let withReset = [(p, if p == "src/Page/SamplePage.elm" then T.replace "    = NoOp\n" "    = NoOp\n    | Reset\n" t else t) | (p, t) <- files]
          r <- runCheck processChecker elm (M.fromList withReset)
          fmap (map (\m -> (missingFile m, missingPatterns m)) . reportMissing) r `shouldBe` Right [("src/Page/SamplePage.elm", ["Reset"])]
    it "reports a file that does not compile as broken, relative to the project (review)" $
      findExecutable "elm" >>= \case
        Nothing -> pendingWith "elm is not installed"
        Just _ -> do
          src <- TIO.readFile "packs/elm/languages/elm.toml"
          let Right elm = parseLanguage "elm" "packs/elm" src
          files <- traverse (\p -> (p,) <$> TIO.readFile ("packs/elm/fixtures/app" </> p)) ["elm.json", "src/Main.elm", "src/Route.elm", "src/Page/Home.elm", "src/Page/SamplePage.elm"]
          let typo = [(p, if p == "src/Page/Home.elm" then T.replace "Cmd.none" "Cmd.nonee" t else t) | (p, t) <- files]
          r <- runCheck processChecker elm (M.fromList typo)
          fmap (nub . map fst . reportBroken) r `shouldBe` Right ["src/Page/Home.elm"]

-- | Run with the real ast-grep and the pack's Elm adapter, or mark pending.
-- | The rust-htmx pack's adapter on the fixture's service.rs, or pending.
withRust :: (Syntax IO -> Language -> Text -> Expectation) -> Expectation
withRust k =
  findExecutable "ast-grep" >>= \case
    Nothing -> pendingWith "ast-grep is not installed (brew install ast-grep)"
    Just _ -> do
      src <- TIO.readFile "packs/rust-htmx/languages/rust.toml"
      tmp <- getTemporaryDirectory
      let Right lang = parseLanguage "rust" "packs/rust-htmx" src
      file <- TIO.readFile "packs/rust-htmx/fixtures/app/billing/billing-service/src/service.rs"
      k (astGrepSyntax (tmp </> "olifant-test-cache")) lang file

-- | The olifant-dev pack's Haskell adapter on the fixture's SampleErrors.hs, or pending.
withHaskell :: (Syntax IO -> Language -> Text -> Expectation) -> Expectation
withHaskell k =
  findExecutable "ast-grep" >>= \case
    Nothing -> pendingWith "ast-grep is not installed (brew install ast-grep)"
    Just _ -> do
      src <- TIO.readFile "packs/olifant-dev/languages/haskell.toml"
      tmp <- getTemporaryDirectory
      let Right lang = parseLanguage "haskell" "packs/olifant-dev" src
      file <- TIO.readFile "packs/olifant-dev/fixtures/app/src/Olifant/SampleErrors.hs"
      k (astGrepSyntax (tmp </> "olifant-test-cache")) lang file

-- | The rust-htmx pack's adapter on one of its fixture's files, or pending.
withRustFile :: FilePath -> (Syntax IO -> Language -> Text -> Expectation) -> Expectation
withRustFile rel k =
  findExecutable "ast-grep" >>= \case
    Nothing -> pendingWith "ast-grep is not installed (brew install ast-grep)"
    Just _ -> do
      src <- TIO.readFile "packs/rust-htmx/languages/rust.toml"
      tmp <- getTemporaryDirectory
      let Right lang = parseLanguage "rust" "packs/rust-htmx" src
      file <- TIO.readFile ("packs/rust-htmx/fixtures/app" </> rel)
      k (astGrepSyntax (tmp </> "olifant-test-cache")) lang file

withAstGrep :: (Syntax IO -> Language -> Text -> Expectation) -> Expectation
withAstGrep = withAstGrepOn "src/Main.elm"

withAstGrepOn :: FilePath -> (Syntax IO -> Language -> Text -> Expectation) -> Expectation
withAstGrepOn fixtureFile k =
  findExecutable "ast-grep" >>= \case
    Nothing -> pendingWith "ast-grep is not installed (brew install ast-grep)"
    Just _ -> do
      src <- TIO.readFile "packs/elm/languages/elm.toml"
      tmp <- getTemporaryDirectory
      let Right elm = parseLanguage "elm" "packs/elm" src
      file <- TIO.readFile ("packs/elm/fixtures/app" </> fixtureFile)
      k (astGrepSyntax (tmp </> "olifant-test-cache")) elm file
