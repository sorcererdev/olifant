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
import Olifant.Anchor (RenderedAnchor (..), Spacing (..), locIndent, locLine, locOpen, locSpacing, resolveLayers)
import Olifant.Language
import Olifant.Language.AstGrep (astGrepSyntax)
import System.Directory (findExecutable, getTemporaryDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Olifant.Template (Value (..))
import Control.Monad (forM_)
import Data.Text.Encoding qualified as TE
import Olifant.Language.Ghc (parseGhcReport, typeOf)
import Olifant.Language.Pyright (parsePyrightReport, pyrightSupport)
import Olifant.Language.Rustc (parseRustcReport, rustFunctionAt, rustcSupport)
import Olifant.Cli.Output (anchorsHuman)
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
  describe "hole kinds (spec §6.3)" $ do
    it "reads [holes.KIND] with a placeholder and a wrap" $ do
      let Right l = parseLanguage "r" "/p" (T.unlines ["files = [\"**/*.rs\"]", "comment = \"//\"", "grammar = \"rust\"", "[holes.expr]", "placeholder = 'todo!(\"olifant: {= hole =}\")'", "wrap = \"fn __olifant() { let _ = {= fill =}; }\""])
      map fst (langHoles l) `shouldBe` ["expr"]
    it "refuses a placeholder that does not name its hole (review I2)" $
      parseLanguage "r" "/p" (T.unlines ["files = [\"**/*.rs\"]", "comment = \"//\"", "grammar = \"rust\"", "[holes.type]", "placeholder = '()'", "wrap = \"type T = {= fill =};\""])
        `shouldSatisfy` either ("{= hole =}" `T.isInfixOf`) (const False)
    it "refuses a wrap without {= fill =}" $
      parseLanguage "r" "/p" (T.unlines ["files = [\"**/*.rs\"]", "comment = \"//\"", "grammar = \"rust\"", "[holes.expr]", "placeholder = 'x({= hole =})'", "wrap = \"fn f() {}\""])
        `shouldSatisfy` either ("{= fill =}" `T.isInfixOf`) (const False)
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

  describe "the Python adapter (python-htmx)" $ do
    let at syn lang file p = fmap (fmap (\l -> (locLine l, locIndent l))) (resolveLayers syn (Just lang) [PhraseAnchor p] file)
        lineOf needle = length (takeWhile (not . (needle `T.isInfixOf`)) (T.lines pythonModule))
    it "(b) loads: hole kinds, a four-space indent, pyright-json" $ withPython $ \_ lang _ -> do
      map fst (langHoles lang) `shouldMatchList` ["expr", "stmt", "block", "ident", "text"]
      langIndent lang `shouldBe` "    "
      fmap checkFormat (langCheck lang) `shouldBe` Just "pyright-json"
    it "(c) end of union NAME goes after its last member, at its indentation" $ withPython $ \syn lang file ->
      at syn lang file "end of union CreateBookingFailure" `shouldReturn` Right (lineOf "| PartyTooLarge" + 1, "    ")
    it "(c) end of class NAME goes after the class's last statement" $ withPython $ \syn lang file ->
      at syn lang file "end of class BookingService" `shouldReturn` Right (lineOf "return 1" + 1, "    ")
    it "(c) end of function NAME goes after its last statement" $ withPython $ \syn lang file ->
      at syn lang file "end of function status" `shouldReturn` Right (lineOf "return 422" + 1, "    ")
    it "(c) end of match on NAME goes after the last case, whole" $ withPython $ \syn lang file ->
      at syn lang file "end of match on f" `shouldReturn` Right (lineOf "return 422" + 1, "        ")
    it "(c) end of imports goes after the last import" $ withPython $ \syn lang file ->
      at syn lang file "end of imports" `shouldReturn` Right (lineOf "from dataclasses import dataclass" + 1, "")
    it "(c) end of call NAME in FUNCTION goes after its last argument (python-pack: wire)" $ withPython $ \syn lang file ->
      at syn lang file "end of call CreateBooking in create" `shouldReturn` Right (lineOf "bookings=booking_repository," + 1, "        ")
    it "(c) end of imports adds no blank line: one between import groups is grouping, not spacing" $ withPython $ \syn lang _ -> do
      let grouped = T.unlines ["from pathlib import Path", "", "from app.x import y", "", "", "def f() -> None:", "    pass"]
      fmap (fmap locSpacing) (resolveLayers syn (Just lang) [PhraseAnchor "end of imports"] grouped) `shouldReturn` Right NoSpacing
    it "(c) end of imports in a file with none goes after its docstring, a blank line before" $ withPython $ \syn lang _ -> do
      let bare = T.unlines ["\"\"\"Clock in memory.\"\"\"", "", "", "class InMemoryClock:", "    pass"]
      fmap (fmap (\l -> (locLine l, locSpacing l))) (resolveLayers syn (Just lang) [PhraseAnchor "end of imports"] bare) `shouldReturn` Right (1, BlankBefore)
    it "(c) end of parameters in FUNCTION goes after its last parameter (python-pack: web-command)" $ withPython $ \syn lang file ->
      at syn lang file "end of parameters in floor_routes" `shouldReturn` Right (lineOf "find_free_tables: FindFreeTables," + 1, "    ")
    it "(c) end of call NAME (anywhere in the file) goes after its last argument" $ withPython $ \syn lang file ->
      at syn lang file "end of call CreateBooking" `shouldReturn` Right (lineOf "bookings=booking_repository," + 1, "        ")
    it "(c) end of call NAME in FUNCTION opens an empty argument list" $ withPython $ \syn lang file ->
      fmap (fmap locOpen) (resolveLayers syn (Just lang) [PhraseAnchor "end of call CancelBooking in create"] file) >>= (`shouldSatisfy` either (const False) isJust)

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
    it "anchors lines up every target's example, however long the phrase" $ do
      src <- TIO.readFile "packs/rust-htmx/languages/rust.toml"
      let Right lang = parseLanguage "rust" "packs/rust-htmx" src
          rows = [l | l <- T.lines (anchorsHuman lang), "e.g. " `T.isInfixOf` l]
      rows `shouldSatisfy` all (\l -> "  e.g. " `T.isInfixOf` l)
      length (nub [T.length (fst (T.breakOn "e.g. " l)) | l <- rows]) `shouldBe` 1
      anchorsHuman lang `shouldSatisfy` T.isInfixOf "start of|end of file"
    it "end of struct goes after the last field" $ withRust $ \syn lang file ->
      at syn lang file "end of struct BillingService" `shouldReturn` Right (lineOf file "pub cancel_invoice: CancelInvoice," + 1)
    it "end of impl goes after the last method" $ withRust $ \syn lang file ->
      at syn lang file "end of impl BillingService" `shouldReturn` Right (lineOf file "self.cancel_invoice.cancel_invoice(input).await" + 2)
    it "end of literal in new goes after the last initializer" $ withRust $ \syn lang file ->
      at syn lang file "end of literal in new" `shouldReturn` Right (lineOf file "cancel_invoice: CancelInvoice {}," + 1)
    it "end of crate imports goes after the last imported item" $ withRust $ \syn lang file ->
      at syn lang file "end of crate imports" `shouldReturn` Right (lineOf file "cancel_invoice::{CancelInvoice" + 1)
  describe "compiler support" $ do
    it "maps the elm-json format to Elm's support" $
      fmap supportFormat (supportFor "elm-json") `shouldBe` Just "elm-json"
    it "has no support for an unknown format" $
      fmap supportFormat (supportFor "cobol-json") `shouldBe` Nothing
    it "keeps the language-neutral modules free of Elm" $
      forM_ ["src/Olifant/Generation/Complete.hs", "src/Olifant/Language/Check.hs"] $ \f -> do
        src <- TIO.readFile f
        (f, "Olifant.Language.Elm" `T.isInfixOf` src) `shouldBe` (f, False)
  describe "rustc-json" $ do
    it "reads errors as broken files, relative to the check directory" $
      fmap reportBroken (supportParse rustcSupport "{\"reason\":\"compiler-message\",\"message\":{\"level\":\"error\",\"code\":{\"code\":\"E0063\"},\"message\":\"missing field `email_sender` in initializer of `CancelInvoice`\",\"spans\":[{\"file_name\":\"billing/billing-service/src/service.rs\",\"line_start\":26,\"line_end\":26,\"is_primary\":true}],\"children\":[]}}\n{\"reason\":\"build-finished\",\"success\":false}\n")
        `shouldBe` Right [("billing/billing-service/src/service.rs", "missing field `email_sender` in initializer of `CancelInvoice`")]
    it "is a known format" $ fmap supportFormat (supportFor "rustc-json") `shouldBe` Just "rustc-json"
  describe "pyright-json (Python, recorded from pyright)" $ do
    it "reads the undefined name from a message (auto-import)" $ do
      supportUndefined pyrightSupport "\"date\" is not defined" `shouldBe` Just "date"
      supportUndefined pyrightSupport "Type of \"date\" is unknown" `shouldBe` Nothing
    let sample name = BS.readFile ("src/Olifant/Language/testdata/pyright/" <> name <> "/report.json")
        sites r = [(missingFile m, fst (missingCase m), missingPatterns m) | m <- reportMissing r]
    it "is a known format, read from stdout" $ do
      fmap supportFormat (supportFor "pyright-json") `shouldBe` Just "pyright-json"
      fmap supportReadsStdout (supportFor "pyright-json") `shouldBe` Just True
    it "(a) a non-exhaustive match is a site at its `match` line, the class as a pattern" $ do
      Right r <- parsePyrightReport <$> sample "union"
      sites r `shouldBe` [("src/failures.py", 17, ["PartyTooLarge()"])]
    it "(a) the function's \"must return on all code paths\" is the missing case's doing, not a broken file" $ do
      Right r <- parsePyrightReport <$> sample "union"
      reportBroken r `shouldBe` []
    it "(b) an enum member is written as Enum.MEMBER" $ do
      Right r <- parsePyrightReport <$> sample "enum"
      sites r `shouldBe` [("src/colors.py", 9, ["Color.BLUE"])]
    it "(c) any other error is a broken file" $ do
      Right r <- parsePyrightReport <$> sample "broken"
      (sites r, map fst (reportBroken r)) `shouldBe` ([], ["src/broken.py"])
    it "(d) a pattern is written as the site spells the others: qualified when they are (Review Focus 1)" $ do
      site <- TIO.readFile "src/Olifant/Language/testdata/pyright/qualified/src/web.py"
      supportPattern pyrightSupport (missing "src/web.py" (4, 6) []) site "PartyTooLarge" "PartyTooLarge()" `shouldBe` "fx.PartyTooLarge()"
      supportPattern pyrightSupport (missing "a.py" (0, 1) []) "match f:\n    case SlotTaken():\n" "PartyTooLarge" "PartyTooLarge()" `shouldBe` "PartyTooLarge()"
    it "(python review I2) the qualifier is the site's own match's, not the first in the file" $ do
      let file = T.unlines
            [ "def is_ok(r):", "    match r:", "        case result.Ok():", "            return True", ""
            , "def status(f):", "    match f:", "        case create_booking.SlotTaken():", "            return 409" ]
      supportPattern pyrightSupport (missing "a.py" (6, 8) []) file "Cancelled" "Cancelled()" `shouldBe` "create_booking.Cancelled()"
    it "(e) a clean run is an empty report" $
      (parsePyrightReport <$> sample "clean") `shouldReturn` Right (CheckReport [] [] [])
    it "(f) two matches lacking the same case are two sites" $ do
      Right r <- parsePyrightReport <$> sample "two-matches"
      sites r `shouldBe` [("src/failures.py", 17, ["PartyTooLarge()"]), ("src/failures.py", 23, ["PartyTooLarge()"])]
      reportBroken r `shouldBe` []
    it "(g) output that is not pyright's JSON is an error" $
      parsePyrightReport "Traceback: oops" `shouldSatisfy` either (const True) (const False)
    it "names the enclosing function: the nearest def at or above the line" $
      supportFunction pyrightSupport "def a():\n    pass\n\ndef status(f):\n    match f:\n" 4 `shouldBe` "status"
  describe "the program a check runs" $ do
    it "skips `env` and its VAR=value arguments" $ do
      checkProgram ["env", "UV_PROJECT_ENVIRONMENT=/c/venv", "uv", "run", "pyright"] `shouldBe` Just "uv"
      checkProgram ["cargo", "check"] `shouldBe` Just "cargo"
      checkProgram ["env", "A=1"] `shouldBe` Nothing
    it "(Review Focus 3) a missing program behind `env` is a missing tool, not a failed check" $ do
      let Right lang = parseLanguage "python" "/p" (T.unlines ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\"", "[check]", "command = [\"env\", \"X=1\", \"olifant-no-such-tool\"]", "format = \"pyright-json\""])
      r <- runCheck processChecker lang (M.fromList [("a.py", "x = 1\n")])
      case r of
        Left (CheckToolMissing why) -> why `shouldSatisfy` T.isInfixOf "olifant-no-such-tool"
        other -> expectationFailure (show other)
  describe "contextKey: one check environment per project and lockfile" $ do
    let Right py = parseLanguage "python" "/p" (T.unlines ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\"", "[check]", "command = [\"uv\"]", "context = [\"pyproject.toml\", \"uv.lock\"]", "format = \"pyright-json\""])
        Just check = langCheck py
        project lock src = M.fromList [("pyproject.toml", "[project]\nname = \"bistro\"\n"), ("uv.lock", lock), ("src/a.py", src)]
    it "is the same for the same project, whatever its sources say" $
      contextKey check (project "v1" "x = 1\n") `shouldBe` contextKey check (project "v1" "x = 2\n")
    it "changes with the lockfile (or the project file)" $
      contextKey check (project "v1" "x = 1\n") `shouldNotBe` contextKey check (project "v2" "x = 1\n")
  describe "compileClean with pyright (a site but no broken file)" $
    it "says which cases are missing, not an empty reason" $ do
      let report = "{\"generalDiagnostics\": [{\"file\": \"a.py\", \"severity\": \"error\", \"rule\": \"reportMatchNotExhaustive\", \"message\": \"Cases within match statement do not exhaustively handle all values\\n  Unhandled type: \\\"Gone\\\"\", \"range\": {\"start\": {\"line\": 3, \"character\": 4}, \"end\": {\"line\": 3, \"character\": 9}}}]}"
          Right lang = parseLanguage "python" "/p" (T.unlines ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\"", "[check]", "command = [\"sh\", \"-c\", " <> T.pack (show ("printf '%s' '" <> T.unpack report <> "'; exit 1")) <> "]", "format = \"pyright-json\""])
      r <- compileClean lang (M.fromList [("a.py", "x = 1\n")])
      r `shouldSatisfy` either ("Gone()" `T.isInfixOf`) (const False)
  describe "a check whose report cannot be read (python review I4)" $
    it "says what the tool printed on stderr (uv offline, no lockfile)" $ do
      let Right lang = parseLanguage "python" "/p" (T.unlines ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\"", "[check]", "command = [\"sh\", \"-c\", \"echo 'error: No `uv.lock` found; run uv lock' >&2; exit 2\"]", "format = \"pyright-json\""])
      r <- runCheck processChecker lang (M.fromList [("a.py", "x = 1\n")])
      case r of
        Left f -> describeCheckFailure f `shouldSatisfy` T.isInfixOf "No `uv.lock` found"
        other -> expectationFailure (show other)
  describe "a match's extent from its indentation (Python)" $ do
    it "(a) the last line of the block under a line: blank lines don't extend it, a line back at its indentation ends it" $
      indentBlockEnd ["def f(x):", "    match x:", "        case A():", "            return 1", "", "    return 2"] 1 `shouldBe` 3
    it "a column-0 line inside a triple-quoted string does not end the block" $
      indentBlockEnd ["    match x:", "        case A():", "            return \"\"\"one", "at column 0", "            \"\"\"", "    y = 1"] 0 `shouldBe` 4
    it "(b) a last arm whose body spans lines (nested if, a multi-line call) ends at its last line (Review Focus 2)" $ do
      src <- TIO.readFile "src/Olifant/Language/testdata/pyright/two-matches/src/failures.py"
      indentBlockEnd (T.splitOn "\n" src) 23 `shouldBe` 30
      indentBlockEnd (T.splitOn "\n" src) 17 `shouldBe` 19
    it "(c) through the checker: each pyright site spans its match to the last arm (real pyright via uvx)" $
      findExecutable "uvx" >>= \case
        Nothing -> lookupEnv "OLIFANT_REQUIRE_TOOLS" >>= \case
          Just "1" -> expectationFailure "required tool missing: uv (brew install uv)"
          _ -> pendingWith "uv is not installed (brew install uv)"
        Just _ -> do
          let Right py = parseLanguage "python" "/p" (T.unlines ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\"", "[check]", "command = [\"uvx\", \"--quiet\", \"pyright==1.1.414\", \"--outputjson\"]", "context = [\"pyproject.toml\"]", "format = \"pyright-json\""])
          files <- traverse (\f -> (f,) <$> TIO.readFile ("src/Olifant/Language/testdata/pyright/two-matches/" <> f)) ["pyproject.toml", "src/failures.py"]
          r <- runCheck processChecker py (M.fromList files)
          fmap (map (\m -> (missingFile m, missingCase m)) . reportMissing) r `shouldBe` Right [("src/failures.py", (17, 19)), ("src/failures.py", (23, 30))]
  describe "rustc-json sites (spec §5.6, fixtures captured from cargo 1.99)" $ do
    report <- runIO (BS.readFile "src/Olifant/Language/testdata/rustc-e0004.jsonl")
    let Right r = parseRustcReport report
        site line = [m | m <- reportMissing r, fst (missingCase m) == line - 1]
    it "reads each E0004 as a site from the scrutinee to the last arm, with patterns and type" $ do
      map (\m -> (missingFile m, missingCase m, missingPatterns m, missingType m)) (site 11)
        `shouldBe` [("src/lib.rs", (10, 14), ["&Failure::D"], Just "&Failure")]
      map missingPatterns (site 4) `shouldBe` [["&Failure::Locked { .. }", "&Failure::A", "&Failure::B"]]
      map missingCase (site 20) `shouldBe` [(19, 25)]
    it "places both nested sites (Review Focus 4)" $
      map missingCase (site 31 ++ site 32) `shouldBe` [(30, 35), (31, 33)]
    it "reports a refutable let (E0005) as a site it cannot extend" $
      map snd (reportUnplaced r) `shouldBe` ["a `let` binding"]
    it "has no broken files: E0004/E0005 are sites, not breakage" $
      reportBroken r `shouldBe` []
    it "writes patterns without the reference (match ergonomics)" $
      supportPattern rustcSupport (missing "a.rs" (0, 0) []) "" "Failure::D" "&Failure::D" `shouldBe` "Failure::D"
    it "sees Type::Variant (and Type.Variant) where that type is matched" $ do
      let [d] = site 11
      supportVisible rustcSupport d "" "Failure::D" `shouldBe` True
      supportVisible rustcSupport d "" "Other::D" `shouldBe` False
      supportVisible rustcSupport d "" "Failure.D" `shouldBe` True
    it "names the enclosing fn" $
      rustFunctionAt "pub fn one(f: &Failure) -> u16 {\n    match f {\n" 1 `shouldBe` "one"
    it "appends a comma to the last arm before a new one" $
      supportArmSeparator rustcSupport `shouldBe` ","
  describe "check commands" $
    it "expands {= cache =} to an existing per-language cache directory" $ do
      let Right lang = parseLanguage "cachey" "/p" (T.unlines ["files = [\"**/*.x\"]", "comment = \"--\"", "grammar = \"x\"", "[check]", "command = [\"sh\", \"-c\", \"test -d \\\"$0\\\"\", \"{= cache =}\"]", "format = \"rustc-json\""])
      compileClean lang (M.fromList [("a.x", "")]) `shouldReturn` Right ()
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
      supportPattern g (missing "a.hs" (0, 0) []) "" "CatalogError.PackLocked" "PackLocked _" `shouldBe` "PackLocked _"
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
-- | A module with each Python target in it.
pythonModule :: Text
pythonModule =
  T.unlines
    [ "import fx"
    , "from dataclasses import dataclass"
    , ""
    , ""
    , "@dataclass(frozen=True)"
    , "class SlotTaken:"
    , "    reason: str"
    , ""
    , ""
    , "type CreateBookingFailure = ("
    , "    SlotTaken"
    , "    | PartyTooLarge"
    , ")"
    , ""
    , ""
    , "class BookingService:"
    , "    def __init__(self) -> None:"
    , "        self.x = 1"
    , ""
    , "    def run(self) -> int:"
    , "        return 1"
    , ""
    , ""
    , "def floor_routes("
    , "    find_free_tables: FindFreeTables,"
    , ") -> int:"
    , "    return 1"
    , ""
    , ""
    , "def create(db_path: str) -> int:"
    , "    create_booking = CreateBooking("
    , "        bookings=booking_repository,"
    , "    )"
    , "    cancel_booking = CancelBooking()"
    , "    return 1"
    , ""
    , ""
    , "def status(f: CreateBookingFailure) -> int:"
    , "    match f:"
    , "        case SlotTaken():"
    , "            return 409"
    , "        case PartyTooLarge():"
    , "            return 422"
    ]

-- | The python-htmx pack's adapter on 'pythonModule', or pending.
withPython :: (Syntax IO -> Language -> Text -> Expectation) -> Expectation
withPython k =
  findExecutable "ast-grep" >>= \case
    Nothing -> pendingWith "ast-grep is not installed (brew install ast-grep)"
    Just _ -> do
      src <- TIO.readFile "packs/python-htmx/languages/python.toml"
      tmp <- getTemporaryDirectory
      let Right lang = parseLanguage "python" "packs/python-htmx" src
      k (astGrepSyntax (tmp </> "olifant-test-cache")) lang pythonModule

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
