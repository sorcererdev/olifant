{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.GeneratorTestingSpec (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.GeneratorTesting
import Data.Map.Strict qualified as M
import Data.Text.IO qualified as TIO
import Olifant.Language (Syntax (..), SyntaxFailure (..), compileClean, noChecker, noSyntax, parseLanguage, processChecker)
import Olifant.Workspace (Workspace (..))
import System.Directory (findExecutable)
import Olifant.Language.AstGrep (astGrepSyntax)
import System.Directory (getTemporaryDirectory)
import System.FilePath (splitDirectories, takeDirectory, takeFileName, (</>))
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Data.List (isSuffixOf)
import Data.Containers.ListUtils (nubOrd)
import Olifant.Workspace (diskWorkspace, memoryWorkspace)
import System.Environment (lookupEnv)
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
    it "blessing removes snapshot files the case no longer generates" $ do
      (root, snap) <- memoryWorkspace (tinyPack "old\n" ++ [("/packs/p/generators/g/tests/basic/snapshot/out/stale.txt", "x\n")])
      [c] <- discoverCases root "/packs"
      runCase root noSyntax noChecker True c `shouldReturn` CasePassed
      files <- snap
      M.member "/packs/p/generators/g/tests/basic/snapshot/out/stale.txt" files `shouldBe` False
      M.lookup "/packs/p/generators/g/tests/basic/snapshot/out/blog_post.txt" files `shouldBe` Just "hello BlogPost\n"
    it "resolves choices_from in a [[uses]] generator, from the fixture" $ do
      let files =
            [ ("/packs/p/generators/outer/generator.toml", "description = \"o\"\nlanguage = \"x\"\n[inputs.svc]\ntype = \"text\"\n[[uses]]\ngenerator = \"inner\"\nwith = { svc = \"{= svc =}\" }\n")
            , ("/packs/p/generators/inner/generator.toml", "description = \"i\"\nlanguage = \"x\"\n[inputs.svc]\ntype = \"enum\"\nchoices_from = { glob = \"svc/*\", value = \"{= stem =}\" }\n[[edits]]\ncreate = \"out.txt\"\ntext = \"{= svc =}\\n\"\n")
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
        [ ("p/generators/g/generator.toml", "description = \"g\"\nlanguage = \"x\"\n[inputs.svc]\ntype = \"enum\"\nchoices_from = { command = [\"cat\", \"svcs.json\"], value = \"{= name =}\" }\n[[edits]]\ncreate = \"out.txt\"\ntext = \"{= svc =}\\n\"\n")
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
  describe "rust pack output compiles" $ do
    cases <- runIO (discoverCases (diskWorkspace ".") "packs")
    strict <- runIO ((== Just "1") <$> lookupEnv "OLIFANT_REQUIRE_TOOLS")
    tmp <- runIO getTemporaryDirectory
    forM_ [c | c <- cases, casePack c == "rust-htmx"] $ \c ->
      it (T.unpack (caseName c)) $
        findExecutable "cargo" >>= \case
          Nothing -> missingTool strict "cargo is not installed (brew install rustup; rustup default stable)"
          Just cargo -> do
            fixture <- readTree "packs/rust-htmx/fixtures/app"
            given <- readTree (caseDir c </> "given")
            snapshot <- readTree (caseDir c </> "snapshot")
            cargoCheck cargo (tmp </> "olifant-test-cache" </> "cargo-target") (M.unions [snapshot, given, fixture]) `shouldReturn` Right ()
  where
    -- Pending locally; a failure where tools are required (the pre-merge hook
    -- sets OLIFANT_REQUIRE_TOOLS=1), so a missing tool cannot look green.
    missingTool strict why = if strict then expectationFailure ("required tool missing: " <> why) else pendingWith why
    readTree dir = do
      let ws = diskWorkspace "."
      paths <- filter (\p -> takeWhile (/= '/') p /= "elm-stuff") <$> listFiles ws dir
      M.fromList <$> traverse (\p -> (p,) . maybe "" id <$> readText ws (dir </> p)) paths

-- | `cargo check` every @*/*-service@ crate in @files@, as one workspace.
cargoCheck :: FilePath -> FilePath -> M.Map FilePath Text -> IO (Either Text ())
cargoCheck cargo target files = withSystemTempDirectory "olifant-cargo" $ \dir -> do
  let crate p = case splitDirectories p of
        (a : b : "src" : _) | "-service" `isSuffixOf` b -> Just (a </> b)
        _ -> Nothing
      crates = nubOrd [k | Just k <- map crate (M.keys files)]
      manifest k = "[package]\nname = \"" <> T.pack (takeFileName k) <> "\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n[dependencies]\nthiserror = \"1\"\nasync-trait = \"0.1\"\n"
      write p t = createDirectoryIfMissing True (takeDirectory (dir </> p)) >> TIO.writeFile (dir </> p) t
  forM_ [(p, t) | (p, t) <- M.toList files, maybe False (const True) (crate p)] (uncurry write)
  forM_ crates $ \k -> write (k </> "Cargo.toml") (manifest k)
  write "Cargo.toml" ("[workspace]\nresolver = \"2\"\nmembers = [" <> T.intercalate ", " ["\"" <> T.pack k <> "\"" | k <- crates] <> "]\n")
  env <- getEnvironment
  (code, _, err) <- readCreateProcessWithExitCode ((proc cargo ["check", "--quiet", "--message-format=short"]) {cwd = Just dir, env = Just (("CARGO_TARGET_DIR", target) : env)}) ""
  pure $ case code of
    ExitSuccess -> Right ()
    ExitFailure _ -> Left (T.unlines [l | l <- T.lines (T.pack err), "error" `T.isInfixOf` l])
