{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.GeneratorTestingSpec (spec) where

import Control.Monad (filterM, forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.GeneratorTesting
import Olifant.Generation.Regions (RegionMode (..), refreshRegions)
import Olifant.Generation (describeConflict, PlannedEdit (..), HoleStatus (..), Mode (..), Plan (..), PlannedHole (..), Step (..), describeGenerationError, generate)
import Data.Time (UTCTime (..), fromGregorian)
import Data.Map.Strict qualified as M
import Data.Text.IO qualified as TIO
import Olifant.Generation.Tools (Verifier (..))
import Olifant.Language.Check (noVerifier, processVerifierWith)
import Olifant.Language (HoleKind (..), Language (..), Syntax (..), SyntaxFailure (..), compileClean, noChecker, noSyntax, parseLanguage, processChecker)
import Olifant.Template (Value (..), render)
import Olifant.Workspace (Workspace (..))
import System.Directory (doesFileExist, findExecutable)
import Olifant.Language.AstGrep (astGrepSyntax)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import System.FilePath ((</>))
import Olifant.Workspace (diskWorkspace, memoryWorkspace)
import System.Environment (lookupEnv)
import Olifant.Rules (FixOffer (..), Finding (..), RulesConfig (..), Severity (..), checkFiles, parseRules)
import Olifant.Cli (DoctorPorts (..), Runtime (..), runCli)
import System.Directory (canonicalizePath)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Test.Hspec

tinyPack :: Text -> [(FilePath, Text)]
tinyPack snapshotContent =
  [ ("/packs/p/generators/g/generator.toml", "description = \"g\"\nlanguage = \"x\"\n[inputs.name]\ntype = \"ident\"\n[[edits]]\ncreate = \"out/{= name|snake =}.txt\"\ntext = \"hello {= name|pascal =}\\n\"\n")
  , ("/packs/p/fixtures/app/README", "fixture\n")
  , ("/packs/p/generators/g/tests/basic/inputs.toml", "now = \"2026-09-27T15:30:00Z\"\n[inputs]\nname = \"blog_post\"\n")
  , ("/packs/p/generators/g/tests/basic/snapshot/out/blog_post.txt", snapshotContent)
  ]

runTiny :: [(FilePath, Text)] -> IO CaseResult
runTiny files = do
  (root, _) <- memoryWorkspace files
  [c] <- discoverCases root "/packs"
  runCase root noSyntax noChecker False c

spec :: Spec
spec = do
  describe "runCase" $ do
    it "passes when output matches the snapshot and re-running changes nothing" $
      runTiny (tinyPack "hello BlogPost\n") `shouldReturn` CasePassed
    it "fails with the differing file when output differs" $
      runTiny (tinyPack "hello Wrong\n") >>= \case
        CaseFailed msgs -> T.unlines msgs `shouldSatisfy` T.isInfixOf "out/blog_post.txt"
        other -> expectationFailure ("expected a failure, got " <> show other)
    it "fails when a generated file is missing from the snapshot" $
      runTiny (take 3 (tinyPack "")) >>= \case
        CaseFailed msgs -> T.unlines msgs `shouldSatisfy` T.isInfixOf "not in snapshot"
        other -> expectationFailure ("expected a failure, got " <> show other)

    it "reports a generation error in words, not as a Haskell value" $
      runTiny (map (\(p, c) -> (p, T.replace "hello {= name|pascal =}" "hello {= nope =}" c)) (tinyPack "")) >>= \case
        CaseFailed msgs -> do
          T.unlines msgs `shouldSatisfy` T.isInfixOf "undefined variable `nope`"
          T.unlines msgs `shouldNotSatisfy` T.isInfixOf "TemplateFailed"
        other -> expectationFailure ("expected a failure, got " <> show other)
    it "lays a case's `given/` files over the fixture, for that case only" $ do
      let gen = "description = \"g\"\nlanguage = \"x\"\n[[edits]]\ninsert = \"README\"\nat = \"end of file\"\ntext = \"added\"\n"
          files =
            [ ("/packs/p/generators/g/generator.toml", gen)
            , ("/packs/p/fixtures/app/README", "fixture\n")
            , ("/packs/p/generators/g/tests/basic/inputs.toml", "now = \"2026-09-27T15:30:00Z\"\n")
            , ("/packs/p/generators/g/tests/basic/given/README", "given\n")
            , ("/packs/p/generators/g/tests/basic/snapshot/README", "given\nadded\n")
            ]
      runTiny files `shouldReturn` CasePassed
    it "leaves a case pending when a query's tool is missing" $ do
      let gen = "description = \"g\"\nlanguage = \"x\"\n[query.items]\nfiles = \"a.x\"\nitems_of = \"type T\"\n[[edits]]\ncreate = \"b\"\ntext = \"b\"\n"
          lang = "files = [\"**/*.x\"]\ncomment = \"--\"\ngrammar = \"x\"\n[targets.type]\nphrase = \"type NAME\"\ncontainer = { kind = \"t\" }\nitems = { kind = \"i\" }\nexample = \"end of type T\"\n"
          files =
            [ ("/packs/p/generators/g/generator.toml", gen)
            , ("/packs/p/languages/x.toml", lang)
            , ("/packs/p/fixtures/app/a.x", "x\n")
            , ("/packs/p/generators/g/tests/basic/inputs.toml", "now = \"2026-09-27T15:30:00Z\"\n")
            ]
          missingAstGrep = Syntax (\_ _ _ -> pure (Left (SyntaxToolMissing "ast-grep is not installed")))
      (root, _) <- memoryWorkspace files
      [c] <- discoverCases root "/packs"
      runCase root missingAstGrep noChecker False c >>= \case
        CaseSkipped why -> why `shouldSatisfy` T.isInfixOf "ast-grep is not installed"
        other -> expectationFailure ("expected pending, got " <> show other)
    it "replays a case's fills.toml as the model (spec §9)" $ do
      let gen = "description = \"g\"\nlanguage = \"x\"\n[[edits]]\ncreate = \"out.x\"\ntext = \"v = {= ? val: expr | a value =}\\n\"\n"
          lang = "files = [\"**/*.x\"]\ncomment = \"--\"\ngrammar = \"x\"\n[holes.expr]\nplaceholder = 'todo(\"{= hole =}\")'\nwrap = \"h = {= fill =}\"\n"
          base fills =
            [ ("/packs/p/generators/g/generator.toml", gen)
            , ("/packs/p/languages/x.toml", lang)
            , ("/packs/p/fixtures/app/README", "fixture\n")
            , ("/packs/p/generators/g/tests/basic/inputs.toml", "now = \"2026-09-27T15:30:00Z\"\n")
            , ("/packs/p/generators/g/tests/basic/fills.toml", fills)
            , ("/packs/p/generators/g/tests/basic/snapshot/out.x", "v = 42\n")
            ]
      runTiny (base "[fills]\nval = \"42\"\n") `shouldReturn` CasePassed
      runTiny (base "[fills]\nother = \"1\"\n") >>= \case
        CaseFailed msgs -> T.unlines msgs `shouldSatisfy` T.isInfixOf "no recorded fill for `val`"
        other -> expectationFailure ("expected a failure, got " <> show other)
    it "blessing removes snapshot files the case no longer generates" $ do
      (root, snap) <- memoryWorkspace (tinyPack "old\n" ++ [("/packs/p/generators/g/tests/basic/snapshot/out/stale.txt", "x\n")])
      [c] <- discoverCases root "/packs"
      runCase root noSyntax noChecker True c `shouldReturn` CasePassed
      files <- snap
      M.member "/packs/p/generators/g/tests/basic/snapshot/out/stale.txt" files `shouldBe` False
      M.lookup "/packs/p/generators/g/tests/basic/snapshot/out/blog_post.txt" files `shouldBe` Just "hello BlogPost\n"
    it "resolves one_of in a [[uses]] generator, from the fixture" $ do
      let files =
            [ ("/packs/p/generators/outer/generator.toml", "description = \"o\"\nlanguage = \"x\"\n[inputs.svc]\ntype = \"text\"\n[[uses]]\ngenerator = \"inner\"\nwith = { svc = \"{= svc =}\" }\n")
            , ("/packs/p/generators/inner/generator.toml", "description = \"i\"\nlanguage = \"x\"\n[inputs.svc]\ntype = \"enum\"\none_of = \"svcs\"\n[query.svcs]\nglob = \"svc/*\"\n[[edits]]\ncreate = \"out.txt\"\ntext = \"{= svc =}\\n\"\n")
            , ("/packs/p/fixtures/app/svc/billing", "")
            , ("/packs/p/generators/outer/tests/basic/inputs.toml", "now = \"2026-09-27T15:30:00Z\"\n[inputs]\nsvc = \"billing\"\n")
            , ("/packs/p/generators/outer/tests/basic/snapshot/out.txt", "billing\n")
            ]
      (root, _) <- memoryWorkspace files
      [c] <- discoverCases root "/packs"
      runCase root noSyntax noChecker False c `shouldReturn` CasePassed
    it "runs command queries in the fixture directory" $ do
      tmp <- getTemporaryDirectory
      -- Rooted at / with absolute paths, like the real pack runs (rooted at .).
      let dir = tmp </> "olifant-command-case"
          ws = diskWorkspace "/"
      forM_
        [ ("p/generators/g/generator.toml", "description = \"g\"\nlanguage = \"x\"\n[inputs.svc]\ntype = \"enum\"\none_of = \"svcs\"\n[query.svcs]\ncommand = [\"cat\", \"svcs.json\"]\nvalue = \"name\"\n[[edits]]\ncreate = \"out.txt\"\ntext = \"{= svc =}\\n\"\n")
        , ("p/fixtures/app/svcs.json", "[{\"name\": \"billing\"}]")
        , ("p/generators/g/tests/basic/inputs.toml", "now = \"2026-09-27T15:30:00Z\"\n[inputs]\nsvc = \"billing\"\n")
        , ("p/generators/g/tests/basic/snapshot/out.txt", "billing\n")
        ]
        (\(p, t) -> writeText ws (dir </> p) t)
      [c] <- discoverCases ws dir
      runCase ws noSyntax noChecker False c `shouldReturn` CasePassed

  describe "packs" $ do
    cases <- runIO (discoverCases (diskWorkspace ".") "packs")
    bless <- runIO ((== Just "1") <$> lookupEnv "OLIFANT_BLESS")
    strict <- runIO ((== Just "1") <$> lookupEnv "OLIFANT_REQUIRE_TOOLS")
    it "has pack cases to run" $ cases `shouldNotBe` []
    tmp <- runIO getTemporaryDirectory
    forM_ cases $ \c ->
      it (T.unpack (caseName c)) $
        runCase (diskWorkspace ".") (astGrepSyntax (tmp </> "olifant-test-cache")) processChecker bless c >>= \case
          CaseSkipped why -> missingTool strict (T.unpack why)
          other -> other `shouldBe` CasePassed

  describe "rust-htmx rules (spec §5.9)" $
    it "flags diesel in a command (with the repository fix) and unwrap as a hint" $ do
      tmp <- getTemporaryDirectory
      rulesSrc <- TIO.readFile "packs/rust-htmx/rules.toml"
      langSrc <- TIO.readFile "packs/rust-htmx/languages/rust.toml"
      let Right rules = parseRules rulesSrc
          Right rust = parseLanguage "rust" "packs/rust-htmx" langSrc
          path = "billing/billing-service/src/cancel_invoice.rs"
          code = "use std::sync::Arc;\nuse diesel::prelude::*;\n\nfn f() { let x = g().unwrap(); }\n"
      (fs, _) <- checkFiles (astGrepSyntax (tmp </> "olifant-test-cache")) [rust] (RulesConfig [] []) rules [path] (\_ -> pure (Just code)) [path]
      [(fRule f, fmap (fst . fst) (fRange f), fSeverity f, map foGenerator (fFixes f)) | f <- fs]
        `shouldBe` [ ("commands-use-ports", Just 2, Warn, ["rust-htmx/command-dependency", "rust-htmx/service-repository"])
                   , ("commands-no-unwrap", Just 4, Hint, []) ]
      -- the realistic flow: Diesel code, no import yet; one finding per line
      let usage = "fn f(conn: &mut PgConnection) {\n    let rows = invoices::table.filter(invoices::id.eq(1)).load::<Invoice>(conn);\n    diesel::insert_into(invoices::table).values(&x).execute(conn);\n}\n"
      (uses, _) <- checkFiles (astGrepSyntax (tmp </> "olifant-test-cache")) [rust] (RulesConfig [] []) rules [path] (\_ -> pure (Just usage)) [path]
      [(fRule f, fmap (fst . fst) (fRange f)) | f <- uses, fRule f == "commands-use-ports"] `shouldBe` [("commands-use-ports", Just 2), ("commands-use-ports", Just 3)]
      -- the service's own modules are not commands (review: upgraded minor)
      (others, _) <- checkFiles (astGrepSyntax (tmp </> "olifant-test-cache")) [rust] (RulesConfig [] []) rules ["billing/billing-service/src/service.rs"] (\_ -> pure (Just "fn f() { let x = g().unwrap(); }\n")) ["billing/billing-service/src/service.rs"]
      others `shouldBe` []
  describe "python-htmx rules (spec §5.9)" $
    it "flags sqlite3 in a command, ports in the web, adapters outside main, raised failures; the fixture holds" $ do
      tmp <- getTemporaryDirectory
      rulesSrc <- TIO.readFile "packs/python-htmx/rules.toml"
      langSrc <- TIO.readFile "packs/python-htmx/languages/python.toml"
      let Right rules = parseRules rulesSrc
          Right python = parseLanguage "python" "packs/python-htmx" langSrc
          check path code = do
            (fs, _) <- checkFiles (astGrepSyntax (tmp </> "olifant-test-cache")) [python] (RulesConfig [] []) rules [path] (\_ -> pure (Just code)) [path]
            pure [(fRule f, fmap (fst . fst) (fRange f), fSeverity f, map foGenerator (fFixes f)) | f <- fs]
          command = "src/bistro/booking/commands/close_table.py"
      check command "import sqlite3\n\n\ndef f() -> None:\n    db = sqlite3.connect(\"x\")\n    raise TableNotFound(\"t9\")\n"
        `shouldReturn` [ ("commands-use-ports", Just 1, Warn, ["python-htmx/add-dependency", "python-htmx/repository"])
                       , ("commands-use-ports", Just 5, Warn, ["python-htmx/add-dependency", "python-htmx/repository"])
                       , ("failures-returned", Just 6, Hint, []) ]
      check command "def f() -> None:\n    raise NotImplementedError(\"later\")\n" `shouldReturn` []
      check "src/bistro/web/floor/routes.py" "from bistro.booking.ports.booking_repository.booking_repository import BookingRepository\n"
        `shouldReturn` [("web-uses-commands", Just 1, Warn, [])]
      check "src/bistro/booking/queries/find_free_tables.py" "from bistro.booking.ports.table_repository.sqlite_table_repository import SqliteTableRepository\n"
        `shouldReturn` [("adapters-in-main", Just 1, Warn, [])]
      check "src/bistro/main.py" "from bistro.booking.ports.table_repository.sqlite_table_repository import SqliteTableRepository\n" `shouldReturn` []
      check "src/bistro/booking/queries/find_free_tables__test.py" "from bistro.booking.ports.table_repository.in_memory_table_repository import InMemoryTableRepository\n" `shouldReturn` []
      -- the fixture is the convention written down: it holds
      fixture <- readTree "packs/python-htmx/fixtures/app"
      let pys = [p | p <- M.keys fixture, ".py" `T.isSuffixOf` T.pack p]
      (all', _) <- checkFiles (astGrepSyntax (tmp </> "olifant-test-cache")) [python] (RulesConfig [] []) rules pys (\p -> pure (M.lookup p fixture)) pys
      map fRule all' `shouldBe` []
  describe "rust-htmx regions (spec §5.9)" $
    it "a command file created by hand gets its `pub mod` line; ports.rs lists the ports" $ do
      tmp <- getTemporaryDirectory
      prepared <- preparePack (diskWorkspace ".") (astGrepSyntax (tmp </> "olifant-test-cache")) noChecker "packs/rust-htmx" "rust-htmx" "wire-command" [("billing/billing-service/src/void_payment.rs", "")]
      case prepared of
        Left why -> expectationFailure (T.unpack why)
        Right (ws, tools, _, _) -> do
          (edits, conflicts, current) <- refreshRegions ReportUnset tools ws 1 M.empty
          map describeConflict conflicts `shouldBe` []
          [editPath e | e <- edits] `shouldBe` ["billing/billing-service/src/lib.rs"]
          fmap (fmap (T.isInfixOf "pub mod refund_invoice;\npub mod service;\npub mod void_invoice;\npub mod void_payment;\n")) (M.lookup "billing/billing-service/src/lib.rs" current) `shouldBe` Just (Just True)
  describe "rust-htmx dependent choices" $
    it "wire-command refuses a field that is not one of the service's ports, naming the service" $ do
      tmp <- getTemporaryDirectory
      let given = "packs/rust-htmx/generators/wire-command/tests/cancel-invoice/given/billing/billing-service/src/cancel_invoice.rs"
          root = diskWorkspace "."
      src <- TIO.readFile given
      let withClock = T.replace "pub email_sender: Arc<dyn EmailSender>," "pub email_sender: Arc<dyn EmailSender>,\n    pub clock: Arc<dyn Clock>," src
      prepared <- preparePack root (astGrepSyntax (tmp </> "olifant-test-cache")) noChecker "packs/rust-htmx" "rust-htmx" "wire-command" [("billing/billing-service/src/cancel_invoice.rs", withClock)]
      case prepared of
        Left why -> expectationFailure (T.unpack why)
        Right (ws, tools, resolve, gen) -> do
          result <- generate ws resolve tools [Step gen (M.fromList [("service", "billing"), ("command", "cancel_invoice")])] (UTCTime (fromGregorian 2026 10 2) 0) PlanOnly
          either describeGenerationError (const "") result `shouldSatisfy` T.isInfixOf "expected one of email_sender, invoice_repository (the ports of service=billing); got `clock`"

  -- Structure-placed code must still build: fixture + each Elm case's output.
  describe "elm pack output compiles" $ do
    cases <- runIO (discoverCases (diskWorkspace ".") "packs")
    strict <- runIO ((== Just "1") <$> lookupEnv "OLIFANT_REQUIRE_TOOLS")
    elmSrc <- runIO (TIO.readFile "packs/elm/languages/elm.toml")
    let Right elm = parseLanguage "elm" "packs/elm" elmSrc
    forM_ [c | c <- cases, casePack c == "elm"] $ \c ->
      it (T.unpack (caseName c)) $
        findExecutable "elm" >>= \case
          Nothing -> missingTool strict "elm is not installed"
          Just _ -> do
            fixture <- readTree "packs/elm/fixtures/app"
            given <- readTree (caseDir c </> "given")
            snapshot <- readTree (caseDir c </> "snapshot")
            compileClean elm (M.unions [snapshot, given, fixture]) `shouldReturn` Right ()

  -- Generated Rust must still build: each rust-htmx case's service crates
  -- (fixture + given/ + snapshot), `cargo check`ed with minimal manifests (the
  -- template's heavy workspace dependencies are not what generators write).
  -- The python-htmx fixture is the architecture written down: it must stay
  -- pyright-strict clean and its own tests green; and every case's output
  -- (fixture + given/ + snapshot) must type-check, as Rust's must build.
  describe "python pack" $ do
    cases <- runIO (discoverCases (diskWorkspace ".") "packs")
    strict <- runIO ((== Just "1") <$> lookupEnv "OLIFANT_REQUIRE_TOOLS")
    pySrc <- runIO (TIO.readFile "packs/python-htmx/languages/python.toml")
    let Right python = parseLanguage "python" "packs/python-htmx" pySrc
        withUv k = findExecutable "uv" >>= \case
          Nothing -> missingTool strict "uv is not installed (brew install uv)"
          Just _ -> k
    it "olifant init makes a project where pytest and pyright are green (north-star project 2 §1)" $ withUv $ do
      tmp <- getTemporaryDirectory
      withSystemTempDirectory "olifant-init" $ \dir0 -> do
        dir <- canonicalizePath dir0
        packs <- canonicalizePath "packs/python-htmx"
        let rt = Runtime
              { rtRoot = diskWorkspace "/", rtProjectDir = dir, rtUserConfigDir = Nothing, rtHomeDir = Nothing
              , rtReadStdin = pure "", rtNow = pure (UTCTime (fromGregorian 2026 10 4) 0), rtStdoutIsTty = False
              , rtOut = \_ -> pure (), rtErr = \_ -> pure ()
              , rtSyntax = astGrepSyntax (tmp </> "olifant-test-cache"), rtChecker = processChecker
              , rtGitChanged = \_ _ -> pure (Right ([], [])), rtDoctor = DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False)
              , rtGitInit = \_ _ -> pure (Right ()), rtVerifier = noVerifier }
        runCli rt ["init", "--pack", packs, "demo"] `shouldReturn` ExitSuccess
        env <- getEnvironment
        let project = dir </> "demo"
            uv args = readCreateProcessWithExitCode ((proc "uv" args) {cwd = Just project, env = Just (("UV_PROJECT_ENVIRONMENT", tmp </> "olifant-test-cache" </> "init-venv") : env)}) ""
        (synced, _, syncErr) <- uv ["sync", "--quiet"]
        (synced, syncErr) `shouldSatisfy` ((== ExitSuccess) . fst)
        (tested, out, _) <- uv ["run", "--quiet", "pytest", "-q"]
        (tested, T.isInfixOf "3 passed" (T.pack out)) `shouldBe` (ExitSuccess, True)
        (typed, typedOut, _) <- uv ["run", "--quiet", "pyright"]
        (typed, typedOut) `shouldSatisfy` ((== ExitSuccess) . fst)
    -- An unfilled expr hole must not hide the code after it: a placeholder
    -- typed Never made pyright skip the rest as unreachable, so a second
    -- hole's fill went unchecked (python-pack Task 5).
    it "an unfilled expr hole leaves the code after it checked" $ withUv $ do
      fixture <- readTree "packs/python-htmx/fixtures/app"
      let Just k = lookup "expr" (langHoles python)
          Right ph = render (M.fromList [("hole", VText "a"), ("instruction", VText "the first")]) (kindPlaceholder k)
          file = "def pair() -> tuple[int, int]:\n    return (" <> ph <> ", undefined_name)\n"
          withHole = M.insert "src/bistro/hole_probe.py" file fixture
      compileClean python withHole >>= (`shouldSatisfy` either (T.isInfixOf "undefined_name") (const False))
      -- and the placeholder alone compiles (it stands for any type)
      compileClean python (M.insert "src/bistro/hole_probe.py" (T.replace ", undefined_name" ", 2" file) fixture) `shouldReturn` Right ()
    -- The slice from an empty folder (north-star project 2, Task 11): init,
    -- the generators and the recorded typing (scripts/slice-python.sh).
    it "the python slice: from an empty folder, the stubs red, then green, check holding, and served" $ withUv $ do
      (built, binOut, _) <- readCreateProcessWithExitCode (proc "cabal" ["list-bin", "-v0", "olifant"]) ""
      if built /= ExitSuccess then pendingWith "cabal list-bin olifant failed" else do
        let bin = T.unpack (T.strip (T.pack binOut))
        withSystemTempDirectory "olifant-slice" $ \dir -> do
          env <- getEnvironment
          let stage s = readCreateProcessWithExitCode ((proc "scripts/slice-python.sh" [dir, s]) {env = Just (("OLIFANT", bin) : env)}) ""
              app = dir </> "bistro"
              uv args = readCreateProcessWithExitCode ((proc "uv" args) {cwd = Just app}) ""
              failing out = [T.takeWhile (/= ' ') (T.drop 7 l) | l <- T.lines (T.pack out), "FAILED " `T.isPrefixOf` l]
          (gens, _, gensErr) <- stage "gens"
          (gens, gensErr) `shouldSatisfy` ((== ExitSuccess) . fst)
          (_, _, _) <- uv ["sync", "--quiet"]
          (typed, typedOut, _) <- uv ["run", "--quiet", "pyright"]
          (typed, typedOut) `shouldSatisfy` ((== ExitSuccess) . fst)
          (_, red, _) <- uv ["run", "--quiet", "pytest", "-q"]
          failing red `shouldBe`
            [ "src/bistro/booking/commands/cancel_booking__test.py::test_cancel_booking_succeeds"
            , "src/bistro/booking/commands/create_booking__test.py::test_create_booking_succeeds"
            , "src/bistro/booking/ports/booking_repository/booking_repository__test.py::test_remove[in_memory]"
            , "src/bistro/booking/ports/booking_repository/booking_repository__test.py::test_remove[sqlite]"
            , "src/bistro/booking/ports/id_generator/id_generator__test.py::test_next_id[InMemoryIdGenerator]"
            , "src/bistro/booking/queries/get_day_schedule__test.py::test_get_day_schedule_answers"
            , "src/bistro/web/bookings/routes__test.py::test_the_bookings_page_shows_it" ]
          (behaviour, _, behaviourErr) <- stage "behaviour"
          (behaviour, behaviourErr) `shouldSatisfy` ((== ExitSuccess) . fst)
          (_, cancelRed, _) <- uv ["run", "--quiet", "pytest", "-q"]
          failing cancelRed `shouldBe`
            [ "src/bistro/booking/commands/cancel_booking__test.py::test_cancels_a_booking"
            , "src/bistro/booking/commands/cancel_booking__test.py::test_refuses_a_booking_that_does_not_exist" ]
          -- handle-body writes it (a recorded fill), verified by its tests
          (filled, _, fillErr) <- stage "fill"
          (filled, fillErr) `shouldSatisfy` ((== ExitSuccess) . fst)
          (green, greenOut, _) <- uv ["run", "--quiet", "pytest", "-q"]
          (green, failing greenOut) `shouldBe` (ExitSuccess, [])
          (typed2, typedOut2, _) <- uv ["run", "--quiet", "pyright"]
          (typed2, typedOut2) `shouldSatisfy` ((== ExitSuccess) . fst)
          (checked, checkOut, _) <- readCreateProcessWithExitCode ((proc bin ["check", "--human"]) {cwd = Just app}) ""
          (checked, T.isInfixOf "conventions hold (0 findings" (T.pack checkOut)) `shouldBe` (ExitSuccess, True)
          (served, _, serveErr) <- stage "serve"
          (served, serveErr) `shouldSatisfy` ((== ExitSuccess) . fst)
    it "a fix that needs a choice offers it, and fix --set takes it (review I2: Use an existing port)" $ withUv $ do
      tmp <- getTemporaryDirectory
      withSystemTempDirectory "olifant-fix" $ \dir0 -> do
        dir <- canonicalizePath dir0
        packs <- canonicalizePath "packs/python-htmx"
        fixture <- readTree "packs/python-htmx/fixtures/app"
        forM_ (M.toList fixture) $ \(p, t) -> createDirectoryIfMissing True (takeDirectory (dir </> p)) >> TIO.writeFile (dir </> p) t
        createDirectoryIfMissing True (dir </> ".olifant")
        TIO.writeFile (dir </> ".olifant/config.toml") ("[packs]\npython-htmx = { path = \"" <> T.pack packs <> "\" }\n")
        let close = "src/bistro/booking/commands/close_table.py"
        TIO.writeFile (dir </> close) (T.unlines ["import sqlite3", "", "", "class CloseTableInput:", "    pass", "", "", "class CloseTable:", "    def __init__(self) -> None:", "        \"\"\"Its ports.\"\"\"", "", "    def handle(self, input: CloseTableInput) -> None:", "        raise NotImplementedError(\"later\")"])
        out <- newIORef ""
        let rt = Runtime
              { rtRoot = diskWorkspace "/", rtProjectDir = dir, rtUserConfigDir = Nothing, rtHomeDir = Nothing
              , rtReadStdin = pure "", rtNow = pure (UTCTime (fromGregorian 2026 10 6) 0), rtStdoutIsTty = False
              , rtOut = \t -> modifyIORef' out (<> t), rtErr = \_ -> pure ()
              , rtSyntax = astGrepSyntax (tmp </> "olifant-test-cache"), rtChecker = noChecker
              , rtGitChanged = \_ _ -> pure (Right ([], [])), rtDoctor = DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False)
              , rtGitInit = \_ _ -> pure (Right ()), rtVerifier = noVerifier }
        _ <- runCli rt ["check", "--file", close, "--json"]
        checked <- readIORef out
        checked `shouldSatisfy` T.isInfixOf "\"missing\":[\"port\"]"
        checked `shouldSatisfy` T.isInfixOf "booking_repository"
        writeIORef out ""
        runCli rt ["fix", "commands-use-ports", "--at", close <> ":1:1", "--set", "port=booking_repository", "--plan", "--json"] `shouldReturn` ExitSuccess
        planned <- readIORef out
        planned `shouldSatisfy` T.isInfixOf "booking_repository: BookingRepository"
    it "web-command refuses a command with no Failure union (pack-opens)" $ do
      tmp <- getTemporaryDirectory
      let path = "src/bistro/booking/commands/close_table.py"
          command = T.unlines ["from dataclasses import dataclass", "", "from bistro.shared.result import Ok", "", "", "@dataclass(frozen=True)", "class CloseTableInput:", "    id: str", "", "", "class CloseTable:", "    def handle(self, input: CloseTableInput) -> Ok[None]:", "        return Ok(None)"]
      Right (ws, tools, resolve, g) <- preparePack (diskWorkspace ".") (astGrepSyntax (tmp </> "olifant-test-cache")) noChecker "packs/python-htmx" "python-htmx" "web-command" [(path, command)]
      Right plan <- generate ws resolve tools [Step g (M.fromList [("app", "bistro"), ("service", "booking"), ("resource", "bookings"), ("command", "close_table")])] (UTCTime (fromGregorian 2026 10 6) 0) PlanOnly
      map describeConflict (planConflicts plan) `shouldSatisfy` any ("give close_table a Failure union first" `T.isInfixOf`)
    it "handle-body refuses a command with no __test.py beside it (Review Focus 5)" $ do
      tmp <- getTemporaryDirectory
      given <- TIO.readFile "packs/python-htmx/generators/handle-body/tests/create-booking/given/src/bistro/booking/commands/create_booking.py"
      let moved = "src/bistro/booking/commands/close_table.py"
      Right (ws, tools0, resolve, g0) <- preparePack (diskWorkspace ".") (astGrepSyntax (tmp </> "olifant-test-cache")) noChecker "packs/python-htmx" "python-htmx" "handle-body" [(moved, given)]
      let line = 1 + length (takeWhile (not . T.isInfixOf "raise NotImplementedError") (T.lines given))
      Right (tools, g) <- atCaseCursor ws noChecker (Just (moved, line)) tools0 g0
      Right plan <- generate ws resolve tools [Step g (M.fromList [("app", "bistro"), ("service", "booking"), ("kind", "commands"), ("command", "close_table")])] (UTCTime (fromGregorian 2026 10 6) 0) PlanOnly
      map describeConflict (planConflicts plan) `shouldSatisfy` any ("write close_table__test.py first" `T.isInfixOf`)
    it "verify: a command that runs too long is stopped and says so (Review Focus 4)" $ do
      r <- runVerify (processVerifierWith 1) python ["sleep", "30"] (M.fromList [("src/a.py", "x = 1\n")])
      r `shouldBe` Right (False, "the tests timed out (1 s)")
      ok <- runVerify (processVerifierWith 10) python ["true"] (M.fromList [("src/a.py", "x = 1\n")])
      fmap fst ok `shouldBe` Right True
    it "its fixture is pyright-strict clean" $ withUv $ do
      fixture <- readTree "packs/python-htmx/fixtures/app"
      compileClean python fixture `shouldReturn` Right ()
    it "its fixture's own tests pass" $ withUv $ do
      tmp <- getTemporaryDirectory
      fixture <- readTree "packs/python-htmx/fixtures/app"
      withSystemTempDirectory "olifant-py" $ \dir -> do
        forM_ (M.toList fixture) $ \(p, t) -> createDirectoryIfMissing True (takeDirectory (dir </> p)) >> TIO.writeFile (dir </> p) t
        env <- getEnvironment
        (code, out, err) <- readCreateProcessWithExitCode ((proc "uv" ["run", "--quiet", "--frozen", "pytest", "-q"]) {cwd = Just dir, env = Just (("UV_PROJECT_ENVIRONMENT", tmp </> "olifant-test-cache" </> "python-fixture-venv") : env)}) ""
        (code, out <> err) `shouldSatisfy` ((== ExitSuccess) . fst)
    -- the test policy: a case marked `run_tests = true` runs its project's
    -- tests; exactly `expect_failing = N` of them fail (0: boilerplate passes;
    -- a stub of behaviour fails until written)
    testedCases <- runIO (filterM (\c -> T.isInfixOf "\nrun_tests = true" . ("\n" <>) <$> TIO.readFile (caseDir c </> "inputs.toml")) [c | c <- cases, casePack c == "python-htmx"])
    forM_ testedCases $ \c ->
      it ("tests keep the test policy: " <> T.unpack (caseName c)) $ withUv $ do
        inputs <- TIO.readFile (caseDir c </> "inputs.toml")
        let expected = case [n | l <- T.lines inputs, Just rest <- [T.stripPrefix "expect_failing = " l], [(n, "")] <- [reads (T.unpack rest)]] of
              n : _ -> n
              [] -> 0 :: Int
        starter <- T.isInfixOf "\nstarter = true" <$> TIO.readFile (casePackRoot c </> "generators" </> T.unpack (caseGenerator c) </> "generator.toml")
        fixture <- if starter then pure M.empty else readTree "packs/python-htmx/fixtures/app"
        given <- readTree (caseDir c </> "given")
        snapshot <- readTree (caseDir c </> "snapshot")
        tmp <- getTemporaryDirectory
        withSystemTempDirectory "olifant-py-case" $ \dir -> do
          forM_ (M.toList (M.unions [snapshot, given, fixture])) $ \(p, t) -> createDirectoryIfMissing True (takeDirectory (dir </> p)) >> TIO.writeFile (dir </> p) t
          env <- getEnvironment
          (_, out, _) <- readCreateProcessWithExitCode ((proc "uv" ["run", "--quiet", "--frozen", "pytest", "-q", "-p", "no:cacheprovider"]) {cwd = Just dir, env = Just (("UV_PROJECT_ENVIRONMENT", tmp </> "olifant-test-cache" </> "python-case-venv") : env)}) ""
          -- pytest's short summary has one FAILED line per failing test
          let failedCount = length [() | l <- T.lines (T.pack out), "FAILED " `T.isPrefixOf` l]
          (failedCount, T.takeEnd 600 (T.pack out)) `shouldSatisfy` ((== expected) . fst)
    snippetCases <- runIO (filterM (\c -> T.isInfixOf "\nat = " . ("\n" <>) <$> TIO.readFile (caseDir c </> "inputs.toml")) cases)
    forM_ [c | c <- cases, casePack c == "python-htmx", c `notElem` snippetCases] $ \c ->
      it ("output type-checks: " <> T.unpack (caseName c)) $ withUv $ do
        -- a starter's project stands alone: no fixture under it
        starter <- T.isInfixOf "\nstarter = true" <$> TIO.readFile (casePackRoot c </> "generators" </> T.unpack (caseGenerator c) </> "generator.toml")
        fixture <- if starter then pure M.empty else readTree "packs/python-htmx/fixtures/app"
        given <- readTree (caseDir c </> "given")
        snapshot <- readTree (caseDir c </> "snapshot")
        compileClean python (M.unions [snapshot, given, fixture]) `shouldReturn` Right ()
  describe "rust pack output compiles" $ do
    cases <- runIO (discoverCases (diskWorkspace ".") "packs")
    strict <- runIO ((== Just "1") <$> lookupEnv "OLIFANT_REQUIRE_TOOLS")
    rustSrc <- runIO (TIO.readFile "packs/rust-htmx/languages/rust.toml")
    let Right rust = parseLanguage "rust" "packs/rust-htmx" rustSrc
    -- A snippet's case (it has a cursor, `at`) is unfinished by design: the
    -- follow-up it raises completes it (spec §4.9), so it is not compiled alone.
    snippetCases <- runIO (filterM (\c -> T.isInfixOf "\nat = " . ("\n" <>) <$> TIO.readFile (caseDir c </> "inputs.toml")) cases)
    forM_ [c | c <- cases, casePack c == "rust-htmx", c `notElem` snippetCases] $ \c ->
      it (T.unpack (caseName c)) $
        findExecutable "cargo" >>= \case
          Nothing -> missingTool strict "cargo is not installed (brew install rustup; rustup default stable)"
          Just _ -> do
            fixture <- readTree "packs/rust-htmx/fixtures/app"
            given <- readTree (caseDir c </> "given")
            snapshot <- readTree (caseDir c </> "snapshot")
            compileClean rust (M.unions [snapshot, given, fixture]) `shouldReturn` Right ()
  -- A case with recorded fills must also compile with every hole left as its
  -- placeholder (spec §9 step 4): AI is optional.
  describe "placeholder output compiles" $ do
    cases <- runIO (discoverCases (diskWorkspace ".") "packs")
    strict <- runIO ((== Just "1") <$> lookupEnv "OLIFANT_REQUIRE_TOOLS")
    tmp <- runIO getTemporaryDirectory
    withFills <- runIO (filterM (\c -> doesFileExist (caseDir c </> "fills.toml")) cases)
    forM_ withFills $ \c ->
      it (T.unpack (caseName c)) $ do
        let root = diskWorkspace "."
        given <- readTree (caseDir c </> "given")
        inputs <- TIO.readFile (caseDir c </> "inputs.toml")
        prepared <- preparePack root (astGrepSyntax (tmp </> "olifant-test-cache")) processChecker (casePackRoot c) (casePack c) (caseGenerator c) (M.toList given)
        case (prepared, parseCaseInputs inputs) of
          (Left why, _) -> expectationFailure (T.unpack why)
          (_, Left why) -> expectationFailure (T.unpack why)
          (Right (ws, tools0, resolve, gen0), Right (now, sets)) -> do
            -- the pack's language, and the program its check runs: without it
            -- the case is pending (planning would find no sites to fill)
            let (langName', tool) = case casePack c of
                  "elm" -> ("elm", "elm")
                  "python-htmx" -> ("python", "uv")
                  _ -> ("rust", "cargo")
            findExecutable tool >>= \case
              Nothing -> missingTool strict "the pack's compiler is not installed"
              Just _ -> do
                Right (tools, gen) <- atCaseCursor ws processChecker (either (const Nothing) id (parseCaseCursor inputs)) tools0 gen0
                Right plan <- generate ws resolve tools [Step gen sets] now PlanOnly
                [() | PlannedHole {phStatus = HoleUnfilled _} <- planHoles plan] `shouldNotBe` []
                src <- TIO.readFile (casePackRoot c </> "languages" </> (langName' <> ".toml"))
                let Right lang = parseLanguage (T.pack langName') (casePackRoot c) src
                fixture <- readTree (casePackRoot c </> "fixtures" </> "app")
                compileClean lang (M.unions [planWrites plan, given, fixture]) `shouldReturn` Right ()
  where
    -- Pending locally; a failure where tools are required (the pre-merge hook
    -- sets OLIFANT_REQUIRE_TOOLS=1), so a missing tool cannot look green.
    missingTool strict why = if strict then expectationFailure ("required tool missing: " <> why) else pendingWith why
    readTree dir = do
      let ws = diskWorkspace "."
      paths <- filter (\p -> takeWhile (/= '/') p /= "elm-stuff") <$> listFiles ws dir
      M.fromList <$> traverse (\p -> (p,) . maybe "" id <$> readText ws (dir </> p)) paths
