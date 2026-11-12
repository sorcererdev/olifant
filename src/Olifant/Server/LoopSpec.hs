module Olifant.Server.LoopSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.List (find, isSuffixOf, sort)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, getCurrentTime)
import Olifant.Decide (Question (..), Shape (..))
import Olifant.Process (cancelAfter, newCancel, neverCancel)
import Olifant.Server.Edits (FileEdit (..))
import Olifant.Server.Errand (Block (..), Ghost (..))
import Olifant.Server.Stub (StubAnswer (..))
import Olifant.Server.State (ServerConfig (..), ServerState (..), filesChanged)
import Olifant.Server.Telemetry (ErrandRecord (..), OutcomeRecord (..), PassRecord (..), ReconcileRecord (..), Record (..), appendRecord, readRecords, recordsPath)
import Olifant.Server.Claims (CheckRecord (..), SentenceEntry (..), readClaims, readJoins, storeClaims)
import Olifant.Server.Claims qualified as C
import Olifant.Server.Checker (CheckMode (..), Confirmed (..), Judgement (..), UnitRef (..))
import Olifant.Server.Triage (Route (..), Triage (..))
import Control.Concurrent (threadDelay)
import Data.Maybe (isJust)
import System.Directory (doesDirectoryExist)
import System.Directory (doesFileExist)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.App (Ports (..), Project)
import Olifant.Language (processRunner, scriptedRunner, shippedLanguage)
import Olifant.Language.AstGrep (astGrepSyntax)
import System.Directory (canonicalizePath, getTemporaryDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Olifant.LspSpec (needsUv, plainPorts, withPython)
import Olifant.Plans (Anchor (..), Line (..), TestState (..), parsePlan)
import Olifant.Server (ConjureAnswer (..), ConjureOutcome (..), PausedAnswer (..), TestsFile (..), conjureAt, newServerState, projectFor, stubAt, testsFile)
import Olifant.Fill (FillHole (..), FillRequest (..), Filler (..), ModelFiller (..))
import Data.IORef (modifyIORef, newIORef, readIORef)
import Olifant.Language (Failure (..), Runner (..), TestRun (..))
import Olifant.Server.Loop
import Olifant.Server.Plans (Resolved (..), claimHover, resolveAnchor)
import Olifant.Plans.Sentences (sentences)
import Olifant.Server.ReviewReadings (applyFactsText)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import Control.Concurrent.Async (async, waitCatch)
import Control.Exception (SomeException, bracket, throwIO, try)
import System.IO (hGetLine)
import System.Process (CreateProcess (..), StdStream (..), callProcess, createProcess, proc, readProcess, terminateProcess)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = claimPassSpec >> factsSpec >> workSetSpec >> questionsSpec >> reconcileSpec >> stepTestsSpec >> haskellStubSpec >> rustStubSpec >> conjureFloorSpec >> excerptsSpec

workSetSpec :: Spec
workSetSpec = describe "Olifant.Server.Loop (the work set)" $ do
  it "(a) on main the baseline is the plan's first commit; an uncommitted plan uses HEAD" $
    withPython [(table, tableText)] $ \_ dir -> do
      initRepo dir
      write dir "plans/t.md" "# T\n"
      git dir ["add", "plans"]
      git dir ["commit", "-q", "-m", "the plan"]
      planSha <- gitLine dir ["rev-parse", "HEAD"]
      write dir table labelEdited
      git dir ["commit", "-q", "-am", "an edit"]
      baseline dir "plans/t.md" `shouldReturn` Just planSha
      write dir "plans/u.md" "# U\n"
      headSha <- gitLine dir ["rev-parse", "HEAD"]
      headSha `shouldNotBe` planSha
      baseline dir "plans/u.md" `shouldReturn` Just headSha
  it "(b) on a branch the baseline is the merge-base with main" $
    withLoop "# T\n" [] $ \_ _ dir -> do
      first <- gitLine dir ["rev-parse", "HEAD"]
      write dir table labelEdited
      git dir ["commit", "-q", "-am", "more on main"]
      mainSha <- gitLine dir ["rev-parse", "HEAD"]
      git dir ["checkout", "-q", "-b", "close-table"]
      write dir table (T.replace "self.seats" "self.seats + 1" labelEdited)
      git dir ["commit", "-q", "-am", "on the branch"]
      mainSha `shouldNotBe` first
      baseline dir "plans/t.md" `shouldReturn` Just mainSha
  it "(b2) with only origin's default branch, the merge-base is with origin/<default>" $
    withLoop "# T\n" [] $ \_ _ dir -> do
      first <- gitLine dir ["rev-parse", "HEAD"]
      git dir ["update-ref", "refs/remotes/origin/main", "HEAD"]
      git dir ["symbolic-ref", "refs/remotes/origin/HEAD", "refs/remotes/origin/main"]
      git dir ["branch", "-q", "-m", "main", "close-table"]
      write dir table labelEdited
      git dir ["commit", "-q", "-am", "on the branch"]
      baseline dir "plans/t.md" `shouldReturn` Just first
  it "(c) a changed, an added and a removed unit, with fingerprints" $
    withLoop "# T\n" [(table, threeWays)] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" (parsePlan "# T\n")
      let at n = find ((== n) . cuName) (wsChanged ws)
      fmap cuChange (at "spare") `shouldBe` Just Added
      fmap cuChange (at "Table.other") `shouldBe` Just Removed
      fmap cuFingerprint (at "Table.other") `shouldBe` Just Nothing
      fmap cuHeader (at "Table.label") `shouldBe` Just "def label(self) -> str:"
      case at "Table.label" of
        Just ChangedUnit {cuChange = Changed old, cuFingerprint = Just now, cuNow = Just n, cuBase = Just b} -> do
          old `shouldNotBe` now
          map T.length [old, now] `shouldBe` [7, 7]
          n `shouldSatisfy` T.isInfixOf "\"t-\""
          b `shouldSatisfy` T.isInfixOf "return self.id"
        other -> expectationFailure ("Table.label: " <> show other)
      map cuPath (wsChanged ws) `shouldSatisfy` all (== table)
      wsBaseline ws `shouldSatisfy` (/= Nothing)
  it "(d) a moved unit is one Moved, not a Removed and an Added" $ do
    slotText <- TIO.readFile ("packs/python-htmx/fixtures/app" </> slot)
    withLoop "# T\n" [(table, T.replace helper "" tableText), (slot, slotText <> "\n\n" <> helper)] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" []
      map (\c -> (cuPath c, cuName c, cuChange c)) (wsChanged ws) `shouldBe` [(slot, "helper", Moved table "helper")]
  it "(d2) a git-mv'd module's units are Moved, and a step at the old path is touched" $ do
    let plan = "# T\n\n- [ ] keep the helper @" <> T.pack table <> ":helper\n"
        renamed = "src/bistro/booking/models/tables.py"
    withLoopPorts plan [] $ \ports dir -> do
      git dir ["mv", table, renamed]
      git dir ["commit", "-q", "-m", "a rename"]
      (s, p) <- loaded ports dir
      ws <- workSet s p "plans/t.md" (parsePlan plan)
      map (\c -> (cuPath c, cuChange c)) (wsChanged ws) `shouldSatisfy` all (\(f, c) -> f == renamed && case c of Moved old _ -> old == table; _ -> False)
      map cuName (wsChanged ws) `shouldSatisfy` (\ns -> all (`elem` ns) ["Table.label", "helper"])
      map (\(l, c) -> (lnRow l, cuName c)) (wsTouched ws) `shouldBe` [(2, "helper")]
  it "(d3) a path with non-ASCII characters is not lost" $
    withLoop "# T\n" [("src/bistro/café.py", "def crème() -> int:\n    return 1\n")] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" []
      map (\c -> (cuPath c, cuName c, cuChange c)) (wsChanged ws) `shouldBe` [("src/bistro/café.py", "crème", Added)]
  it "(e) accounted units are left out, across plans" $
    withLoopPorts "# T\n" [(table, labelEdited)] $ \ports dir -> do
      (s0, p0) <- loaded ports dir
      fp <- maybe (fail "Table.label does not resolve") pure . rsFingerprint =<< resolveAnchor s0 p0 (Anchor table "Table.label" 0 0)
      write dir "plans/other.md" ("# O\n\n- [x] x @" <> T.pack table <> ":Table.label ✓ " <> fp <> "\n")
      -- loaded again: a server sees a new file once its listing is stale
      (s, p) <- loaded ports dir
      ws <- workSet s p "plans/t.md" (parsePlan "# T\n")
      map cuName (wsChanged ws) `shouldContain` ["Table.label"]
      map cuName (wsUnaccounted ws) `shouldNotContain` ["Table.label"]
      -- its class changed with it, and is not unaccounted on its own
      map cuName (wsChanged ws) `shouldContain` ["Table"]
      map cuName (wsUnaccounted ws) `shouldNotContain` ["Table"]
  it "(f) a step's own changed anchor is touched, not unaccounted" $ do
    let plan = "# T\n\n- [ ] tidy the label @" <> T.pack table <> ":Table.label\n"
    withLoop plan [(table, labelEdited)] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" (parsePlan plan)
      map (\(l, c) -> (lnRow l, cuName c)) (wsTouched ws) `shouldBe` [(2, "Table.label")]
      map cuName (wsUnaccounted ws) `shouldNotContain` ["Table.label"]
      map cuName (wsUnaccounted ws) `shouldNotContain` ["Table"]
  it "(f3) a step anchored at a bare method name is touched, and its unit not unaccounted" $ do
    let plan = "# T\n\n- [ ] tidy the label @" <> T.pack table <> ":label\n"
    withLoop plan [(table, labelEdited)] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" (parsePlan plan)
      map (\(l, c) -> (lnRow l, cuName c)) (wsTouched ws) `shouldBe` [(2, "Table.label")]
      map cuName (wsUnaccounted ws) `shouldNotContain` ["Table.label"]
  it "(f2) a changed class is not unaccounted when one of its methods is" $
    withLoop "# T\n" [(table, labelEdited)] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" (parsePlan "# T\n")
      map cuName (wsChanged ws) `shouldContain` ["Table", "Table.label"]
      map cuName (wsUnaccounted ws) `shouldBe` ["Table.label"]
  it "(g) files without a language are skipped" $ do
    pyproject <- TIO.readFile "packs/python-htmx/fixtures/app/pyproject.toml"
    withLoop "# T\n" [("pyproject.toml", pyproject <> "\n# edited\n"), (table, labelEdited)] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" []
      wsChanged ws `shouldSatisfy` not . null
      map cuPath (wsChanged ws) `shouldSatisfy` all (== table)
  it "(h) no git: Nothing baseline, empty sets" $
    withPython [("plans/t.md", "# T\n")] $ \ports dir -> do
      (s, p) <- loaded ports dir
      baseline dir "plans/t.md" `shouldReturn` Nothing
      workSet s p "plans/t.md" [] `shouldReturn` WorkSet Nothing [] [] 0 []
  it "(i) the cap" $
    withLoop "# T\n" [("src/bistro/many.py", T.concat ["def f" <> tshow i <> "() -> int:\n    return " <> tshow i <> "\n\n\n" | i <- [1 .. 45 :: Int]])] $ \s p _ -> do
      ws <- workSet s p "plans/t.md" []
      length (wsChanged ws) `shouldBe` 45
      length (wsUnaccounted ws) `shouldBe` unaccountedCap
      unaccountedCap `shouldBe` 40
      wsMore ws `shouldBe` 5

-- | The python fixture with TABLE's units and the plan, committed on main,
-- then EDITS written, and the project loaded.
withLoop :: Text -> [(FilePath, Text)] -> (ServerState -> Project -> FilePath -> IO a) -> IO a
withLoop plan edits act = withLoopPorts plan edits $ \ports dir -> loaded ports dir >>= \(s, p) -> act s p dir

-- | A Haskell project (olifant-dev's language, its [test] the hspec preset)
-- with a plan whose steps test a describe, under the scripted
-- runner SCRIPT; a fresh state.
withHaskellLoop :: M.Map Text Value -> (FilePath -> ServerState -> IO ()) -> IO ()
withHaskellLoop script act = do
  tmp <- getTemporaryDirectory
  lang <- TIO.readFile ".olifant/languages/haskell.toml"
  withSystemTempDirectory "olifant-loop-hs" $ \dir0 -> do
    dir <- canonicalizePath dir0
    -- olifant's own adapter: its [test] (the hspec preset) gives unit,
    -- file_group and tests_for; the scripted runner stands for cabal
    let files =
          [ ("pack/languages/haskell.toml", lang)
          , (".olifant/config.toml", "[packs]\nhs = { path = \"pack\" }\n")
          , ("src/Olifant/Plans.hs", "module Olifant.Plans where\n\nparsePlan :: Int -> Int\nparsePlan n = n\n")
          , ("src/Olifant/PlansSpec.hs", T.unlines ["module Olifant.PlansSpec (spec) where", "spec :: Spec", "spec = describe \"Olifant.Plans\" $ do", "  it \"(a) reads a step\" $", "    1 `shouldBe` 1"])
          , ("plans/t.md", T.unlines ["# T", "", "## Plans", "", "- [ ] parse a plan @src/Olifant/Plans.hs:parsePlan", "  - test: plans parse @src/Olifant/PlansSpec.hs:Olifant.Plans", "", "- [ ] nope @src/Olifant/Plans.hs:parsePlan", "  - test: nope @src/Olifant/PlansSpec.hs:Nope"])
          ]
    forM_ files (uncurry (write dir))
    initRepo dir
    s0 <- newServerState plainPorts {pSyntax = astGrepSyntax (tmp </> "olifant-test-cache"), pTester = const (scriptedRunner script)}
    act dir s0

-- | A project of the shipped haskell language (hspec's stub, ast-grep's
-- syntax) holding FILES; a fresh state.
withHaskellProject :: [(FilePath, Text)] -> (FilePath -> ServerState -> IO ()) -> IO ()
withHaskellProject files act = do
  tmp <- getTemporaryDirectory
  withSystemTempDirectory "olifant-stub-hs" $ \dir0 -> do
    dir <- canonicalizePath dir0
    forM_ ((".olifant/config.toml", "languages = [\"haskell\"]\n") : files) (uncurry (write dir))
    initRepo dir
    s0 <- newServerState plainPorts {pSyntax = astGrepSyntax (tmp </> "olifant-test-cache")}
    act dir s0

barText :: Text
barText = T.unlines ["module Olifant.Foo.Bar (baz) where", "", "baz :: Int -> Int", "baz n = n + 1"]

-- | A Haskell stub (hspec's [test.stub]): a new Spec module, an existing one
-- without the import, a duplicate name, and a plan step's test.
haskellStubSpec :: Spec
haskellStubSpec = describe "a Haskell test's stub (hspec)" $ do
  let bar = "src/Olifant/Foo/Bar.hs"
      barSpec = "src/Olifant/Foo/BarSpec.hs"
      stubbedIn dir s = stubAt (dir </> bar) 4 Nothing s
  it "a module with no Spec yet: the Spec module, its import, its describe and the it" $
    withHaskellProject [(bar, barText)] $ \dir s -> do
      (_, r) <- stubbedIn dir s
      a <- either (fail . T.unpack) pure r
      (saFile a, saLine a, saName a) `shouldBe` (dir </> barSpec, 8, "Olifant.Foo.Bar.baz")
      feCreate (saEdit a) `shouldBe` True
      feText (saEdit a)
        `shouldBe` T.unlines
          [ "module Olifant.Foo.BarSpec (spec) where"
          , ""
          , "import Olifant.Foo.Bar"
          , "import Test.Hspec"
          , ""
          , "spec :: Spec"
          , "spec = describe \"Olifant.Foo.Bar\" $ do"
          , "  it \"baz\" $ do"
          , "    expectationFailure \"write this test\""
          ]
  let existing = T.unlines ["module Olifant.Foo.BarSpec (spec) where", "", "import Test.Hspec", "", "spec :: Spec", "spec = describe \"Olifant.Foo.Bar\" $ do", "  it \"other\" $ do", "    1 `shouldBe` 1"]
  it "a Spec without the module's import: the import joins its imports, the it after its last line, a blank line before" $
    withHaskellProject [(bar, barText), (barSpec, existing)] $ \dir s -> do
      (_, r) <- stubbedIn dir s
      a <- either (fail . T.unpack) pure r
      feCreate (saEdit a) `shouldBe` False
      let edited = applyEdit (saEdit a) existing
      edited
        `shouldBe` T.unlines
          [ "module Olifant.Foo.BarSpec (spec) where", "", "import Test.Hspec", "import Olifant.Foo.Bar", "", "spec :: Spec", "spec = describe \"Olifant.Foo.Bar\" $ do", "  it \"other\" $ do", "    1 `shouldBe` 1"
          , "", "  it \"baz\" $ do", "    expectationFailure \"write this test\"" ]
      (saLine a, saName a) `shouldBe` (11, "Olifant.Foo.Bar.baz")
  it "a Spec that ends with a helper: the it at the end of spec's do, not inside the helper" $ do
    let helped = existing <> "\nhelper :: IO ()\nhelper = do\n  pure ()\n"
    withHaskellProject [(bar, barText), (barSpec, helped)] $ \dir s -> do
      (_, r) <- stubbedIn dir s
      a <- either (fail . T.unpack) pure r
      applyEdit (saEdit a) helped
        `shouldBe` T.unlines
          [ "module Olifant.Foo.BarSpec (spec) where", "", "import Test.Hspec", "import Olifant.Foo.Bar", "", "spec :: Spec", "spec = describe \"Olifant.Foo.Bar\" $ do", "  it \"other\" $ do", "    1 `shouldBe` 1"
          , "", "  it \"baz\" $ do", "    expectationFailure \"write this test\""
          , "", "helper :: IO ()", "helper = do", "  pure ()" ]
      (saLine a, saName a) `shouldBe` (11, "Olifant.Foo.Bar.baz")
  it "a name the Spec has already: baz_2" $
    withHaskellProject [(bar, barText), (barSpec, existing <> "\n  it \"baz\" $ do\n    pure ()\n")] $ \dir s -> do
      (_, r) <- stubbedIn dir s
      a <- either (fail . T.unpack) pure r
      saName a `shouldBe` "Olifant.Foo.Bar.baz_2"
      applyEdit (saEdit a) (existing <> "\n  it \"baz\" $ do\n    pure ()\n") `shouldSatisfy` T.isSuffixOf "\n\n  it \"baz_2\" $ do\n    expectationFailure \"write this test\"\n"
  it "a step's test on Haskell: named by its words, its child anchored at the describe path" $
    withHaskellProject [(bar, barText), (barSpec, existing), ("plans/t.md", "# T\n\n## S\n\n- [ ] answers zero @src/Olifant/Foo/Bar.hs:baz\n")] $ \dir s -> do
      (_, r) <- stepStub (dir </> "plans/t.md") 4 s
      st <- either (fail . T.unpack) pure r
      stChild st `shouldBe` "  - test: answers zero @src/Olifant/Foo/BarSpec.hs:Olifant.Foo.Bar.answers_zero \9675"
      applyEdit (saEdit (stAnswer st)) existing `shouldSatisfy` T.isInfixOf "  it \"answers zero\" $ do"

-- | Conjure where no pack declares `[conjure]` (milestone 4a's floor): a test
-- whose body is its stub gets the body filled, with no port asked and
-- nothing wired. The runner says every test it runs fails; the model fake
-- answers the body.
conjureFloorSpec :: Spec
conjureFloorSpec = describe "conjure at the floor (no [conjure])" $ do
  let red = Runner (\_ ids _ -> pure (Right (TestRun [] [(i, Failure Nothing "expected: 2\n but got: 1" "") | i <- ids] Nothing)))
      ready fill s = s {ssTestFiller = Just (ModelFiller "conjurer" 0 (Filler (\_ -> pure (Right (M.fromList [("body", fill)]))))), ssPorts = (ssPorts s) {pTester = const red, pTesterWithin = \_ _ -> red}}
      conjured path line fill s = do
        (_, r) <- conjureAt (const (pure ())) path line Nothing (ready fill s)
        case r of
          Right (Landed a) -> pure a
          Right (Paused p) -> fail (T.unpack ("paused: " <> paWhy p))
          Left why -> fail (T.unpack why)
      edited a text = foldl (flip applyEdit) text (daEdits a)
  it "Haskell: a hspec it whose body is the stub: its body filled, no port, no wire" $ do
    let bar = "src/Olifant/Foo/Bar.hs"
        barSpec = "src/Olifant/Foo/BarSpec.hs"
        specText = T.unlines ["module Olifant.Foo.BarSpec (spec) where", "", "import Olifant.Foo.Bar", "import Test.Hspec", "", "spec :: Spec", "spec = describe \"Olifant.Foo.Bar\" $ do", "  it \"baz\" $ do", "    expectationFailure \"write this test\""]
    withHaskellProject [(bar, barText), (barSpec, specText)] $ \dir s -> do
      a <- conjured (dir </> barSpec) 9 "baz 1 `shouldBe` 3" s
      (daFile a, daUnit a) `shouldBe` (dir </> barSpec, "baz")
      map fePath (daEdits a) `shouldBe` [dir </> barSpec]
      edited a specText `shouldBe` T.replace "expectationFailure \"write this test\"" "baz 1 `shouldBe` 3" specText
  it "Rust: a #[test] fn in mod tests whose body is todo!(): its body filled, no port, no wire" $ do
    let rates = "src/rates.rs"
        text = ratesText <> "\n#[cfg(test)]\nmod tests {\n    use super::*;\n\n    #[test]\n    fn convert() {\n        todo!(\"write this test\");\n    }\n}\n"
    withRustProject [(rates, text)] $ \dir s -> do
      a <- conjured (dir </> rates) 11 "assert_eq!(convert(2, 3), 7);" s
      (daFile a, daUnit a) `shouldBe` (dir </> rates, "convert")
      map fePath (daEdits a) `shouldBe` [dir </> rates]
      edited a text `shouldBe` T.replace "todo!(\"write this test\");" "assert_eq!(convert(2, 3), 7);" text

  it "Rust: a test in its module's own mod tests is conjured as a test (its instruction, not a function's)" $ do
    let rates = "src/rates.rs"
        text = ratesText <> "\n#[cfg(test)]\nmod tests {\n    use super::*;\n\n    #[test]\n    fn convert() {\n        todo!(\"write this test\");\n    }\n}\n"
    asked <- newIORef []
    withRustProject [(rates, text)] $ \dir s -> do
      let s' = (ready "assert_eq!(convert(2, 3), 7);" s) {ssTestFiller = Just (ModelFiller "conjurer" 0 (Filler (\r -> modifyIORef asked (map fhInstruction (frHoles r) ++) >> pure (Right (M.fromList [("body", "assert_eq!(convert(2, 3), 7);")])))))}
      (_, r) <- conjureAt (const (pure ())) (dir </> rates) 11 Nothing s'
      either (fail . T.unpack) (const (pure ())) r
    instructions <- readIORef asked
    instructions `shouldSatisfy` \is -> not (null is) && all ("write the body of the test" `T.isInfixOf`) is

-- | A project of the shipped rust language (cargo-test's stub, ast-grep's
-- syntax) holding FILES; a fresh state.
withRustProject :: [(FilePath, Text)] -> (FilePath -> ServerState -> IO ()) -> IO ()
withRustProject files act = do
  tmp <- getTemporaryDirectory
  withSystemTempDirectory "olifant-stub-rs" $ \dir0 -> do
    dir <- canonicalizePath dir0
    forM_ ((".olifant/config.toml", "languages = [\"rust\"]\n") : ("Cargo.toml", "[package]\nname = \"money\"\nversion = \"0.1.0\"\nedition = \"2021\"\n") : files) (uncurry (write dir))
    initRepo dir
    s0 <- newServerState plainPorts {pSyntax = astGrepSyntax (tmp </> "olifant-test-cache")}
    act dir s0

ratesText :: Text
ratesText = T.unlines ["pub fn convert(amount: i64, rate: i64) -> i64 {", "    amount * rate", "}"]

-- | A Rust test's stub (cargo-test): into the file's own `mod tests`,
-- opened when there is none; refused when the tests live in tests.rs.
rustStubSpec :: Spec
rustStubSpec = describe "a Rust test's stub (cargo-test, tests in the source file)" $ do
  let rates = "src/rates.rs"
  it "no mod tests: one is opened at the file's end, the test in it" $
    withRustProject [(rates, ratesText)] $ \dir s -> do
      (_, r) <- stubAt (dir </> rates) 2 Nothing s
      a <- either (fail . T.unpack) pure r
      (saFile a, saName a) `shouldBe` (dir </> rates, "tests.convert")
      let edited = applyEdit (saEdit a) ratesText
      edited `shouldBe` ratesText <> "\n#[cfg(test)]\nmod tests {\n    use super::*;\n\n    #[test]\n    fn convert() {\n        todo!(\"write this test\");\n    }\n}\n"
      T.lines edited !! (saLine a - 1) `shouldBe` "    #[test]"
  it "a mod tests in the middle of the file: the test at its end, a blank line before" $ do
    let middle = T.unlines
          [ "pub fn convert(amount: i64, rate: i64) -> i64 {", "    amount * rate", "}", ""
          , "#[cfg(test)]", "mod tests {", "    use super::*;", ""
          , "    #[test]", "    fn doubles() {", "        assert_eq!(convert(2, 2), 4);", "    }", "}", ""
          , "pub fn later() -> i64 {", "    1", "}" ]
    withRustProject [(rates, middle)] $ \dir s -> do
      (_, r) <- stubAt (dir </> rates) 1 Nothing s
      a <- either (fail . T.unpack) pure r
      saName a `shouldBe` "tests.convert"
      applyEdit (saEdit a) middle
        `shouldBe` T.unlines
          [ "pub fn convert(amount: i64, rate: i64) -> i64 {", "    amount * rate", "}", ""
          , "#[cfg(test)]", "mod tests {", "    use super::*;", ""
          , "    #[test]", "    fn doubles() {", "        assert_eq!(convert(2, 2), 4);", "    }", ""
          , "    #[test]", "    fn convert() {", "        todo!(\"write this test\");", "    }", "}", ""
          , "pub fn later() -> i64 {", "    1", "}" ]
      saLine a `shouldBe` 14
  it "mod tests; (tests in another file): refused, naming it" $
    withRustProject [(rates, ratesText <> "\n#[cfg(test)]\nmod tests;\n")] $ \dir s -> do
      (_, r) <- stubAt (dir </> rates) 2 Nothing s
      either id (const "stubbed") r `shouldBe` "tests live in another file: tests.rs"
  it "tests_for both ways: a Rust file is its own test file; a Haskell module and its Spec name each other" $ do
    withRustProject [(rates, ratesText)] $ \dir s -> do
      (_, r) <- testsFile (dir </> rates) s
      either id (const "a file") r `shouldBe` "src/rates.rs is its own test file (rust's tests_for): no other file to jump to"
    withHaskellProject [("src/Olifant/Plans.hs", "module Olifant.Plans where\n"), ("src/Olifant/PlansSpec.hs", "module Olifant.PlansSpec (spec) where\n")] $ \dir s -> do
      (s1, r1) <- testsFile (dir </> "src/Olifant/PlansSpec.hs") s
      fmap (\f -> (tfFile f, tfIsTest f)) r1 `shouldBe` Right (dir </> "src/Olifant/Plans.hs", False)
      (_, r2) <- testsFile (dir </> "src/Olifant/Plans.hs") s1
      fmap (\f -> (tfFile f, tfIsTest f, tfExists f)) r2 `shouldBe` Right (dir </> "src/Olifant/PlansSpec.hs", True, True)

-- | A file edit applied to TEXT (its span replaced; ASCII columns).
applyEdit :: FileEdit -> Text -> Text
applyEdit e text = T.take (offset l1 c1) text <> feText e <> T.drop (offset l2 c2) text
  where
    ((l1, c1), (l2, c2)) = feRange e
    offset l c = sum [T.length x + 1 | x <- take l (T.splitOn "\n" text)] + c

-- | 'withLoop' before the project is loaded.
withLoopPorts :: Text -> [(FilePath, Text)] -> (Ports -> FilePath -> IO a) -> IO a
withLoopPorts plan edits act =
  withPython [("plans/t.md", plan), (table, tableText)] $ \ports dir -> do
    initRepo dir
    forM_ edits (uncurry (write dir))
    act ports dir

loaded :: Ports -> FilePath -> IO (ServerState, Project)
loaded ports dir = do
  s0 <- newServerState ports
  projectFor (dir </> "plans/t.md") s0 >>= \case
    (s, Just (_, p)) -> pure (s, p)
    _ -> fail "no project"

initRepo :: FilePath -> IO ()
initRepo dir = do
  git dir ["init", "-q", "-b", "main"]
  git dir ["add", "."]
  git dir ["commit", "-q", "-m", "the fixture"]

git :: FilePath -> [String] -> IO ()
git dir args = callProcess "git" (["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t"] ++ args)

gitLine :: FilePath -> [String] -> IO Text
gitLine dir args = T.strip . T.pack <$> readProcess "git" (["-C", dir] ++ args) ""

write :: FilePath -> FilePath -> Text -> IO ()
write dir path text = createDirectoryIfMissing True (takeDirectory (dir </> path)) >> TIO.writeFile (dir </> path) text

table, slot :: FilePath
table = "src/bistro/booking/models/table.py"
slot = "src/bistro/booking/models/slot.py"

tableText, helper, labelEdited, threeWays :: Text
tableText =
  T.unlines
    [ "from dataclasses import dataclass", "", "", "@dataclass(frozen=True)", "class Table:", "    id: str", "    seats: int", ""
    , "    def label(self) -> str:", "        return self.id", "", "    def other(self) -> int:", "        return self.seats", "", "" ]
    <> helper
helper = "def helper() -> int:\n    return 1\n"
labelEdited = T.replace "return self.id" "return \"t-\" + self.id" tableText
threeWays = T.replace "    def other(self) -> int:\n        return self.seats\n" "" labelEdited <> "\n\ndef spare() -> None:\n    pass\n"

tshow :: Show a => a -> Text
tshow = T.pack . show

factsSpec :: Spec
factsSpec = describe "Olifant.Server.Loop (facts)" $ do
  let ls = parsePlan "- [ ] s @src/a.py:F\n  - test: it works @tests/t.py:test_it ●\n  - test: new @tests/t.py:test_new"
      done = parsePlan "- [x] s @src/a.py:F ✓ abc1234\n  - test: it works @tests/t.py:test_it ●"
      step0 = case ls of (l : _) -> l; [] -> error "empty plan"
      cu = ChangedUnit "src/a.py" "F" (Just "abc1234") (Changed "x") "def F" Nothing Nothing
      st n x = (("tests/t.py", n), x)
  it "(a) a moved unit is re-anchored wherever it is named" $
    reanchorFacts (parsePlan "- [ ] s @src/a.py:F\n  - also @src/a.py:F ✓ abc1234\n- claim @src/b.py:G ✓ 1111111") [ChangedUnit "src/c.py" "F" (Just "abc1234") (Moved "src/a.py" "F") "def F" Nothing Nothing]
      `shouldBe` [Replace 0 "- [ ] s @src/c.py:F", Replace 1 "  - also @src/c.py:F ✓ abc1234"]
  it "(b) a touched step's green children go pending" $
    pendingFacts ls [(step0, cu)] `shouldBe` [Replace 1 "  - test: it works @tests/t.py:test_it ○"]
  it "(b2) a step whose fingerprint is its unit's now keeps its children's marks, done or open" $ do
    let at fp box = parsePlan ("- " <> box <> " s @src/a.py:F ✓ " <> fp <> "\n  - test: a @tests/t.py:test_a ●\n  - test: b @tests/t.py:test_b ✗")
        first l = case l of (x : _) -> x; [] -> error "empty plan"
    forM_ ["[x]", "[ ]"] $ \box -> pendingFacts (at "abc1234" box) [(first (at "abc1234" box), cu)] `shouldBe` []
    pendingFacts (at "0000000" "[ ]") [(first (at "0000000" "[ ]"), cu)]
      `shouldBe` [Replace 1 "  - test: a @tests/t.py:test_a ○", Replace 2 "  - test: b @tests/t.py:test_b ○"]
  it "(c) all green ticks the step at its fingerprint" $
    testFacts ls 0 [st "test_it" TestGreen, st "test_new" TestGreen] (Just "abc1234")
      `shouldBe` [ Replace 1 "  - test: it works @tests/t.py:test_it ●", Replace 2 "  - test: new @tests/t.py:test_new ●"
                 , Replace 0 "- [x] s @src/a.py:F ✓ abc1234" ]
  it "(d) one unmarked child keeps the step open" $
    testFacts ls 0 [st "test_it" TestGreen] (Just "abc1234")
      `shouldBe` [Replace 1 "  - test: it works @tests/t.py:test_it ●"]
  it "(e) a red test on a done step un-ticks it and says so once" $ do
    let facts = testFacts done 0 [st "test_it" TestRed] (Just "abc1234")
    facts `shouldBe` [Replace 1 "  - test: it works @tests/t.py:test_it ✗", Replace 0 "- [ ] s @src/a.py:F ✓ abc1234", Insert 1 ["  - ✗ test_it red again"]]
    let applied = parsePlan "- [x] s @src/a.py:F ✓ abc1234\n  - test: it works @tests/t.py:test_it ✗\n  - ✗ test_it red again"
        inserts = filter (\f -> case f of Insert _ _ -> True; _ -> False)
    inserts (testFacts applied 0 [st "test_it" TestRed] (Just "abc1234")) `shouldBe` []
  it "(e2) a note for test_it does not hide test_item's; two reds, one noted, insert only the other" $ do
    let p = parsePlan "- [x] s @src/a.py:F ✓ abc1234\n  - test: a @tests/t.py:test_it ✗\n  - test: b @tests/t.py:test_item ●\n  - ✗ test_it red again"
    testFacts p 0 [st "test_it" TestRed, st "test_item" TestRed] Nothing
      `shouldSatisfy` elem (Insert 3 ["  - ✗ test_item red again"])
  it "(e3) only direct test children count; a title with a checkbox is kept; a nested step is skipped" $ do
    let p = parsePlan "- [ ] fix \"- [ ]\" @src/a.py:F\n  - test: a @tests/t.py:test_a ○\n  - [ ] inner @src/a.py:G\n    - test: g @tests/t.py:test_g ○"
    testFacts p 0 [st "test_a" TestGreen, st "test_g" TestRed] Nothing
      `shouldBe` [Replace 1 "  - test: a @tests/t.py:test_a ●", Replace 0 "- [x] fix \"- [ ]\" @src/a.py:F"]
  it "(f) no fingerprint ticks without a mark; factJson shapes" $ do
    testFacts ls 0 [st "test_it" TestGreen, st "test_new" TestGreen] Nothing
      `shouldSatisfy` (elem (Replace 0 "- [x] s @src/a.py:F"))
    factJson (Replace 2 "t") `shouldBe` object ["row" .= (2 :: Int), "text" .= ("t" :: Text)]
    factJson (Insert 2 ["a"]) `shouldBe` object ["after" .= (2 :: Int), "lines" .= (["a"] :: [Text])]
  it "(g) a run of one of two children rewrites that child only: no tick, no stamp" $ do
    let two = parsePlan "- [ ] s @src/a.py:F\n  - test: a @tests/t.py:test_a ●\n  - test: b @tests/t.py:test_b ●"
    testFacts two 0 [st "test_a" TestGreen] (Just "abc1234") `shouldBe` [Replace 1 "  - test: a @tests/t.py:test_a ●"]
  it "(h) children with one test name in two files get their own states; a red run stamps the open step" $ do
    let twin = parsePlan "- [ ] s @src/a.py:F\n  - test: a @tests/a.py:test_init ○\n  - test: b @tests/b.py:test_init ○"
    testFacts twin 0 [(("tests/a.py", "test_init"), TestGreen), (("tests/b.py", "test_init"), TestRed)] (Just "abc1234")
      `shouldBe` [Replace 1 "  - test: a @tests/a.py:test_init ●", Replace 2 "  - test: b @tests/b.py:test_init ✗", Replace 0 "- [ ] s @src/a.py:F ✓ abc1234"]

questionsSpec :: Spec
questionsSpec = describe "Olifant.Server.Loop (questions)" $ do
  let ls = parsePlan "# T\n\n## Other\n\n- [ ] elsewhere\n- [x] finished @src/a.py:G ✓ abc1234\n\n## Mine · branch b\n\nWhat this branch does.\n\n- [ ] here @src/a.py:F\n  - test: it works @tests/t.py:test_it ●\n"
      cu = ChangedUnit "src/a.py" "F" (Just "abc1234") (Changed "x") "def F():" (Just "def F():\n    return 2") (Just "def F():\n    return 1")
      opts q = case qShape q of Choice os -> map fst os; _ -> []
      at r = case drop r ls of (l : _) -> l; [] -> error "no row"
  it "(m) plan-step offers the open steps, the branch's section first, then none" $ do
    let q = planStepQuestion (Just "b") ls ("src/a.py", [cu])
    qKind q `shouldBe` "plan-step"
    opts q `shouldBe` ["s11", "s4", "none"]
    map fst (qContext q) `shouldBe` ["file", "units", "diff", "sections"]
    lookup "diff" (qContext q) `shouldSatisfy` maybe False (\d -> T.isInfixOf "-     return 1" d && T.isInfixOf "+     return 2" d)
    lookup "sections" (qContext q) `shouldSatisfy` maybe False (T.isInfixOf "What this branch does.")
    opts (planStepQuestion Nothing ls ("src/a.py", [cu])) `shouldBe` ["s4", "s11", "none"]
  it "(n) plan-state offers done, in progress, diverged for an open step; still done, reopened, diverged for a done one" $ do
    let q = planStateQuestion (at 11) ls [cu]
    (qKind q, opts q) `shouldBe` ("plan-state", ["done", "in_progress", "diverged"])
    map fst (qContext q) `shouldBe` ["step", "children", "diff", "tests"]
    lookup "tests" (qContext q) `shouldSatisfy` maybe False (T.isInfixOf "green")
    opts (planStateQuestion (at 5) ls [cu]) `shouldBe` ["still_done", "reopened", "diverged"]
  it "(n2) plan-state leaves journal lines out of a step's children" $ do
    let jls = parsePlan "- [ ] tidy @a.py:helper\n  - test: t @t.py:test_t ●\n  - 25 Oct 21:31 · do (sonnet) · cancelled"
        q = planStateQuestion (head jls) jls []
    lookup "children" (qContext q) `shouldBe` Just "- test: t @t.py:test_t ●"
  it "(l) parseWords reads numbered lines and leaves gaps" $
    parseWords 3 "1: the table answers label\n3: ok" `shouldBe` [Just "the table answers label", Nothing, Just "ok"]
  it "(l2) parseWords ignores numbers out of range and blank answers; gate is 0.6" $ do
    parseWords 2 "0: no\n2:   \n4: no\n1. dotted\n" `shouldBe` [Just "dotted", Nothing]
    gate `shouldBe` 0.6

reconcileSpec :: Spec
reconcileSpec = describe "Olifant.Server.Loop (reconcile)" $ do
  it "(a) nothing changed → no record, empty answer" $
    withReconcile tidyPlan [] jevConfig (Just (answersOf [("plan-step", "s7")])) $ \dir s -> do
      (_, a) <- run False dir s
      (raFacts a, length (raBlocks a)) `shouldBe` ([], 0)
      reconciles dir `shouldReturn` []
      asked dir `shouldReturn` []
  it "(b) a changed unit placed under a step is an also ghost with its unit" $
    withReconcile tidyPlan [(table, helperEdited)] jevConfig (Just (answersOf [("plan-step", "s7"), ("plan-state", "in_progress")])) $ \dir s -> do
      (_, a) <- run False dir s
      fp <- fpNow dir s "helper"
      map summary (raBlocks a) `shouldBe` [(7, Just "- [ ] tidy the helper", ["  - also @src/bistro/booking/models/table.py:helper"], False, Just (table, "helper", fp))]
      map rbKind (raBlocks a) `shouldBe` [Also]
      -- the new step it could be instead, and the unit it is about
      map (fmap (map ghText . blLines) . rbNewStep) (raBlocks a) `shouldBe` [Just ["- [ ] def helper() -> int @src/bistro/booking/models/table.py:helper"]]
      map (fmap cuName . rbChange) (raBlocks a) `shouldBe` [Just "helper"]
      asked dir `shouldReturn` ["plan-step", "plan-state"]
  it "(c) a touched open step with tests answered done is a replacing ticked ghost" $ do
    let plan = "# T\n\n## S\n\n- [ ] tidy the helper @src/bistro/booking/models/table.py:helper\n  - test: it works @tests/t.py:test_it ○\n"
    withReconcile plan [(table, helperEdited)] jevConfig (Just (answersOf [("plan-state", "done")])) $ \dir s -> do
      (_, a) <- run False dir s
      fp <- fpNow dir s "helper"
      map summary (raBlocks a) `shouldBe` [(4, Just "- [ ] tidy the helper @src/bistro/booking/models/table.py:helper", ["- [x] tidy the helper @src/bistro/booking/models/table.py:helper ✓ " <> fp], True, Just (table, "helper", fp))]
      map rbKind (raBlocks a) `shouldBe` [Done]
  it "(c0) a reading never ticks a step with no tests: done is taken as in progress, no ghost" $ do
    let plan = "# T\n\n## S\n\n- [ ] tidy the helper @src/bistro/booking/models/table.py:helper\n"
    withReconcile plan [(table, helperEdited)] jevConfig (Just (answersOf [("plan-state", "done")])) $ \dir s -> do
      (_, a) <- run False dir s
      raBlocks a `shouldBe` []
      asked dir `shouldReturn` ["plan-state"]
  it "(c2) a diverged step is a changed child, the unit named when no chat rung gives words" $ do
    let plan = "# T\n\n## S\n\n- [ ] tidy the helper @src/bistro/booking/models/table.py:helper\n"
    withReconcile plan [(table, helperEdited)] jevConfig (Just (answersOf [("plan-state", "diverged")])) $ \dir s -> do
      (_, a) <- run False dir s
      map (\(r, _, ls, rep, _) -> (r, ls, rep)) (map summary (raBlocks a)) `shouldBe` [(4, ["  - ✗ changed: helper changed @src/bistro/booking/models/table.py:helper"], False)]
      fp <- fpNow dir s "helper"
      map rbUnit (raBlocks a) `shouldBe` [Just (table, "helper", fp)]
      map rbKind (raBlocks a) `shouldBe` [Diverged]
  it "(c3) an accepted changed child settles the step at the unit's fingerprint, until the unit changes" $ do
    let step = "- [ ] tidy the helper @src/bistro/booking/models/table.py:helper"
    withReconcile "# T\n" [(table, helperEdited)] jevConfig (Just (answersOf [("plan-state", "diverged")])) $ \dir s -> do
      fp <- fpNow dir s "helper"
      write dir "plans/t.md" ("# T\n\n## S\n\n" <> step <> "\n  - ✗ changed: helper changed @src/bistro/booking/models/table.py:helper ✓ " <> fp <> "\n")
      (_, a) <- run False dir s
      raBlocks a `shouldBe` []
      asked dir `shouldReturn` []
      write dir table (T.replace "return 1" "return 3" tableText)
      (_, a2) <- run False dir s
      asked dir `shouldReturn` ["plan-state"]
      map (\(r, _, ls, _, _) -> (r, ls)) (map summary (raBlocks a2)) `shouldBe` [(4, ["  - ✗ changed: helper changed @src/bistro/booking/models/table.py:helper"])]
  it "(d) a touched done step answered reopened is a replacing open line" $
    withReconcile "# T\n" [] jevConfig (Just (answersOf [("plan-state", "reopened")])) $ \dir s -> do
      old <- fpNow dir s "helper"
      let step = "- [x] tidy the helper @src/bistro/booking/models/table.py:helper ✓ " <> old
      write dir "plans/t.md" ("# T\n\n## S\n\n" <> step <> "\n")
      write dir table helperEdited
      (_, a) <- run False dir s
      map summary (raBlocks a) `shouldSatisfy` \case
        [(4, Just t, ["- [ ] tidy the helper @src/bistro/booking/models/table.py:helper"], True, Just (_, "helper", _))] -> t == step
        _ -> False
  it "(e) a removed anchored unit is a gone ghost without a question" $ do
    let plan = "# T\n\n## S\n\n- [ ] keep the helper @src/bistro/booking/models/table.py:helper\n"
    withReconcile plan [(table, T.replace helper "" tableText)] jevConfig (Just (answersOf [("plan-state", "done")])) $ \dir s -> do
      (_, a) <- run False dir s
      map summary (raBlocks a) `shouldBe` [(4, Just "- [ ] keep the helper @src/bistro/booking/models/table.py:helper", ["  - ✗ gone: @src/bistro/booking/models/table.py:helper no longer exists"], False, Just (table, "helper", ""))]
      map rbKind (raBlocks a) `shouldBe` [Gone]
      asked dir `shouldReturn` []
      map (map dcKind . rcDecisions) <$> reconciles dir `shouldReturn` [[]]
  it "(e2) a rejected gone ghost does not return; a done step's gone unit has none" $ do
    let plan = "# T\n\n## S\n\n- [ ] keep the helper @src/bistro/booking/models/table.py:helper\n"
    withReconcile plan [(table, T.replace helper "" tableText)] jevConfig (Just (answersOf [])) $ \dir s -> do
      (s1, a1) <- run False dir s
      map rbUnit (raBlocks a1) `shouldBe` [Just (table, "helper", "")]
      now <- getCurrentTime
      appendRecord (ssPorts s) dir (AnOutcome (OutcomeRecord (raId a1) 0 now "rejected"))
      (_, a2) <- run False dir s1
      raBlocks a2 `shouldBe` []
    withReconcile (T.replace "- [ ] keep" "- [x] keep" plan) [(table, T.replace helper "" tableText)] jevConfig (Just (answersOf [])) $ \dir s -> do
      (_, a) <- run False dir s
      raBlocks a `shouldBe` []
  it "(e3) with other work to place, a rejected gone ghost is not replayed from the cache" $ do
    let plan = "# T\n\n## S\n\n- [ ] keep the helper @src/bistro/booking/models/table.py:helper\n"
        gone b = case blLines (rbBlock b) of [g] -> "gone:" `T.isInfixOf` ghText g; _ -> False
    withReconcile plan [(table, T.replace helper "" tableText <> "\n\ndef spare() -> str:\n    return \"spare\"\n")] jevConfig (Just (answersOf [("plan-step", "none")])) $ \dir s -> do
      (s1, a1) <- run False dir s
      i <- maybe (fail "no gone block") pure (lookup True (zip (map gone (raBlocks a1)) [0 ..]))
      length (raBlocks a1) `shouldBe` 2
      now <- getCurrentTime
      appendRecord (ssPorts s) dir (AnOutcome (OutcomeRecord (raId a1) i now "rejected"))
      (_, a2) <- run False dir s1
      map gone (raBlocks a2) `shouldBe` [False]
      length <$> reconciles dir `shouldReturn` 2
  it "(f) none → a new step after the section's last step, with the unit header when no chat rung" $ do
    let planned branch = "# T\n\n## Tables" <> branch <> "\n\n- [ ] count the seats\n  - test: counts @tests/test_t.py:test_count ○\n\n## Later\n\n- [ ] later\n"
        ghost = "- [ ] def spare() -> str @src/bistro/booking/models/table.py:spare"
    withReconcile (planned " · branch main") [(table, withSpare)] jevConfig (Just (answersOf [("plan-step", "none")])) $ \dir s -> do
      (_, a) <- run False dir s
      fp <- fpNow dir s "spare"
      map summary (raBlocks a) `shouldBe` [(5, Just "  - test: counts @tests/test_t.py:test_count ○", [ghost], False, Just (table, "spare", fp))]
    withReconcile (planned "") [(table, withSpare)] jevConfig (Just (answersOf [("plan-step", "none")])) $ \dir s -> do
      (_, a) <- run False dir s
      map (\(r, t, ls, _, _) -> (r, t, ls)) (map summary (raBlocks a)) `shouldBe` [(9, Just "- [ ] later", [ghost])]
      map rbKind (raBlocks a) `shouldBe` [NewStep]
  it "(f2) a new step never lands under a sub-step" $
    withReconcile "# T\n\n## Tables\n\n- [ ] count the seats\n  - [ ] a sub-step\n" [(table, withSpare)] jevConfig (Just (answersOf [("plan-step", "none")])) $ \dir s -> do
      (_, a) <- run False dir s
      map (\(r, t, ls, _, _) -> (r, t, ls)) (map summary (raBlocks a)) `shouldBe` [(5, Just "  - [ ] a sub-step", ["- [ ] def spare() -> str @src/bistro/booking/models/table.py:spare"])]
  it "(g) no decider, no ladder → a new-step reading per unit, from its header" $
    withReconcile "# T\n\n- [ ] x\n" [(table, T.replace "return 1" "return 2" withSpare)] "" Nothing $ \dir s -> do
      (_, a) <- run False dir s
      map rbKind (raBlocks a) `shouldBe` [NewStep, NewStep]
      map (\(r, _, ls, _, _) -> (r, ls)) (map summary (raBlocks a)) `shouldBe` [(2, ["- [ ] def helper() -> int @src/bistro/booking/models/table.py:helper"]), (2, ["- [ ] def spare() -> str @src/bistro/booking/models/table.py:spare"])]
  it "(h) an unsure Jev climbs an auto rung; ask rungs only when forced" $
    withReconcile tidyPlan [(table, helperEdited)] (jevConfig <> climbConfig) (Just (answersOf [("plan-step", "unsure")])) $ \dir s -> do
      _ <- run False dir s
      _ <- run True dir s
      climbs <- map (map fst . rcClimbs) <$> reconciles dir
      case climbs of
        [auto, forced] -> do
          take 2 auto `shouldBe` ["jev", "local"]
          auto `shouldNotContain` ["remote"]
          take 3 forced `shouldBe` ["jev", "local", "remote"]
        other -> expectationFailure ("two reconcile records, got " <> show other)
      map (map dcRung . rcDecisions) <$> reconciles dir `shouldReturn` [["local"], ["remote"]]
  it "(i) the same situation answers from the cache" $
    withReconcile tidyPlan [(table, helperEdited)] jevConfig (Just (answersOf [("plan-step", "s7"), ("plan-state", "in_progress")])) $ \dir s -> do
      (s1, a1) <- run False dir s
      (_, a2) <- run False dir s1
      map summary (raBlocks a2) `shouldBe` map summary (raBlocks a1)
      raId a2 `shouldBe` raId a1
      length <$> reconciles dir `shouldReturn` 1
      length <$> asked dir `shouldReturn` 2
  it "(i2) the plan with the facts applied answers from the cache" $ do
    let plan = "# T\n\n## S\n\n- [ ] tidy the helper @src/bistro/booking/models/table.py:helper\n  - test: it works @tests/t.py:test_it ●\n"
    withReconcile plan [(table, helperEdited)] jevConfig (Just (answersOf [("plan-state", "done")])) $ \dir s -> do
      (s1, a1) <- run False dir s
      raFacts a1 `shouldBe` [Replace 5 "  - test: it works @tests/t.py:test_it ○"]
      write dir "plans/t.md" (T.replace "test_it ●" "test_it ○" plan)
      (_, a2) <- run False dir s1
      (raId a2, raFacts a2, map summary (raBlocks a2)) `shouldBe` (raId a1, [], map summary (raBlocks a1))
      length <$> reconciles dir `shouldReturn` 1
      asked dir `shouldReturn` ["plan-state"]
  it "(i3) a cancelled reconcile answers Left, records nothing, caches nothing" $
    withReconcile tidyPlan [(table, helperEdited)] jevConfig (Just (answersOf [("plan-step", "s7")])) $ \dir s -> do
      (token, trip) <- newCancel
      trip
      (s1, r) <- reconcile token False (dir </> "plans/t.md") s
      either id (const "answered") r `shouldBe` "cancelled"
      reconciles dir `shouldReturn` []
      M.null (ssReconciled s1) `shouldBe` True
  it "(i4) a reconcile cancelled during the words call answers Left, records nothing, caches nothing" $
    withHangingServer $ \port wait -> do
      let config = jevConfig <> T.unlines ["[model.backends.local]", "kind = \"openai-compatible\"", "url = \"http://127.0.0.1:" <> port <> "/v1\"", "model = \"x\"", "escalate = \"auto\"", "[plans]", "ladder = [\"local\"]"]
      withReconcile tidyPlan [(table, helperEdited)] config (Just (answersOf [("plan-step", "none")])) $ \dir s -> do
        (token, trip) <- newCancel
        running <- async (reconcile token False (dir </> "plans/t.md") s)
        -- the words call has reached the server: trip the token under it
        wait >>= (`shouldBe` True)
        trip
        (s1, r) <- either throwIO pure =<< waitCatch running
        either id (const "answered") r `shouldBe` "cancelled"
        reconciles dir `shouldReturn` []
        M.null (ssReconciled s1) `shouldBe` True
        asked dir `shouldReturn` ["plan-step"]
  it "(j) a rejected reading is not proposed again at the same fingerprint" $
    withReconcile tidyPlan [(table, helperEdited)] jevConfig (Just (answersOf [("plan-step", "s7"), ("plan-state", "in_progress")])) $ \dir s -> do
      (s1, a1) <- run False dir s
      length (raBlocks a1) `shouldBe` 1
      now <- getCurrentTime
      appendRecord (ssPorts s) dir (AnOutcome (OutcomeRecord (raId a1) 0 now "rejected"))
      (s2, a2) <- run False dir s1
      raBlocks a2 `shouldBe` []
      write dir table (T.replace "return 1" "return 3" tableText)
      (_, a3) <- run False dir s2
      map (\(_, _, _, _, u) -> fmap (\(_, n, _) -> n) u) (map summary (raBlocks a3)) `shouldBe` [Just "helper"]
  it "(l) a forced reconcile runs a touched step's tests and ticks it; an automatic one marks them pending" $ do
    let plan = "# T\n\n## S\n\n- [ ] tidy the helper @src/bistro/booking/models/table.py:helper\n  - test: it helps @" <> T.pack tableTest <> ":test_helper \9679\n"
    withReconcileTested plan [(table, T.replace "return 1" "return 0 + 1" tableText), (tableTest, T.unlines (take 5 (T.lines tableTestText)))] jevConfig (Just (answersOf [("plan-state", "in_progress")])) $ \dir s -> do
      (s1, auto) <- run False dir s
      raFacts auto `shouldBe` [Replace 5 ("  - test: it helps @" <> T.pack tableTest <> ":test_helper \9675")]
      (_, forced) <- run True dir s1
      fp <- fpNow dir s "helper"
      raFacts forced `shouldBe` [Replace 4 ("- [x] tidy the helper @src/bistro/booking/models/table.py:helper \10003 " <> fp), Replace 5 ("  - test: it helps @" <> T.pack tableTest <> ":test_helper \9679")]
      raBlocks forced `shouldBe` []
      -- the tick applied: the next automatic reconcile leaves its marks
      write dir "plans/t.md" ("# T\n\n## S\n\n- [x] tidy the helper @src/bistro/booking/models/table.py:helper \10003 " <> fp <> "\n  - test: it helps @" <> T.pack tableTest <> ":test_helper \9679\n")
      (_, again) <- run False dir s1
      raFacts again `shouldBe` []
  it "(l3) an open step's marks at its unit now are left; an edit of the unit makes them pending" $ do
    let child n m = "  - test: " <> n <> " @" <> T.pack tableTest <> ":test_" <> n <> " " <> m
        planAt fp = "# T\n\n## S\n\n- [ ] tidy the helper @src/bistro/booking/models/table.py:helper \10003 " <> fp <> "\n" <> child "a" "\9679" <> "\n" <> child "b" "\10007" <> "\n"
    withReconcile "# T\n" [(table, helperEdited)] jevConfig (Just (answersOf [("plan-state", "in_progress")])) $ \dir s -> do
      fp <- fpNow dir s "helper"
      write dir "plans/t.md" (planAt fp)
      (s1, a) <- run False dir s
      raFacts a `shouldBe` []
      asked dir `shouldReturn` []
      write dir table (T.replace "return 1" "return 3" tableText)
      (_, a2) <- run False dir s1
      raFacts a2 `shouldBe` [Replace 5 (child "a" "\9675"), Replace 6 (child "b" "\9675")]
  it "(l2) a forced reconcile whose tests do not run leaves them pending and the step alone" $ do
    let child n = "  - test: " <> n <> " @src/bistro/nowhere__test.py:test_" <> n
        plan = "# T\n\n## S\n\n- [ ] tidy the helper @src/bistro/booking/models/table.py:helper\n" <> child "a" <> " \9679\n" <> child "b" <> " \10007\n"
    withReconcile plan [(table, helperEdited)] jevConfig (Just (answersOf [("plan-state", "in_progress")])) $ \dir s -> do
      (_, a) <- run True dir s
      raFacts a `shouldBe` [Replace 5 (child "a" <> " \9675"), Replace 6 (child "b" <> " \9675")]
  it "(k) the record carries decisions and per-block units; the step log has one plan-reconcile line" $
    withReconcile tidyPlan [(table, helperEdited)] jevConfig (Just (answersOf [("plan-step", "s7"), ("plan-state", "in_progress")])) $ \dir s -> do
      (_, a) <- run False dir s
      rs <- reconciles dir
      case rs of
        [r] -> do
          rcId r `shouldBe` raId a
          rcBlocks r `shouldBe` map rbUnit (raBlocks a)
          (rcPlan r, rcForced r, rcChanged r, rcUnaccounted r) `shouldBe` ("plans/t.md", False, 1, 1)
          [(dcKind d, dcChosen d, dcRung d) | d <- rcDecisions r] `shouldBe` [("plan-step", Just "s7", "jev"), ("plan-state", Just "in_progress", "jev")]
          map dcOptions (rcDecisions r) `shouldBe` [["s6", "s7", "none"], ["done", "in_progress", "diverged"]]
        other -> expectationFailure ("one reconcile record, got " <> show (length other))
      logged <- filter (T.isInfixOf "plan-reconcile") . T.lines <$> TIO.readFile (dir </> ".olifant/state/steps.log")
      map (drop 1 . T.splitOn "\t") logged `shouldSatisfy` \case
        [["plan-reconcile", "plans/t.md", "0 facts", "1 readings", _, "jev"]] -> True
        _ -> False
      case reconcileJson a of
        Object o -> do
          sort (KM.keys o) `shouldBe` sort ["id", "baseline", "facts", "blocks", "changed", "more", "seconds", "rungs"]
          case KM.lookup "blocks" o of
            Just (Array bs) | [Object b] <- foldr (:) [] bs -> do
              sort (KM.keys b) `shouldBe` sort ["after", "after_text", "lines", "replace", "unit"]
              KM.lookup "after" b `shouldBe` Just (Number 7)
            other -> expectationFailure ("one block, got " <> show other)
        v -> expectationFailure ("not an object: " <> show v)

-- | 'withLoopPorts' with CONFIG appended to the project's config and, when
-- given, recorded ANSWERS the decider answers from; a fresh state.
withReconcile :: Text -> [(FilePath, Text)] -> Text -> Maybe Text -> (FilePath -> ServerState -> IO a) -> IO a
withReconcile = withReconcileOn id

-- | 'withReconcile' with the real pytest (through uv): pending without uv,
-- failing with OLIFANT_REQUIRE_TOOLS=1.
withReconcileTested :: Text -> [(FilePath, Text)] -> Text -> Maybe Text -> (FilePath -> ServerState -> IO ()) -> IO ()
withReconcileTested plan edits config answers act = needsUv (withReconcileOn tested plan edits config answers act)

-- | 'withLoop' with the real pytest (through uv).
withLoopTested :: Text -> [(FilePath, Text)] -> (ServerState -> Project -> FilePath -> IO ()) -> IO ()
withLoopTested plan edits act = needsUv (withLoopPorts plan edits $ \ports dir -> loaded (tested ports) dir >>= \(s, p) -> act s p dir)

tested :: Ports -> Ports
tested ports = ports {pTester = (`processRunner` 120)}

withReconcileOn :: (Ports -> Ports) -> Text -> [(FilePath, Text)] -> Text -> Maybe Text -> (FilePath -> ServerState -> IO a) -> IO a
withReconcileOn withPorts plan edits config answers act =
  withLoopPorts plan edits $ \ports0 dir -> do
    let ports = withPorts ports0
    old <- TIO.readFile (dir </> ".olifant/config.toml")
    TIO.writeFile (dir </> ".olifant/config.toml") (old <> "\n" <> config)
    forM_ answers (TIO.writeFile (answersFile dir))
    s0 <- newServerState ports
    act dir s0 {ssConfig = (ssConfig s0) {scDecideAnswers = answersFile dir <$ answers}}

answersFile :: FilePath -> FilePath
answersFile dir = dir </> ".olifant/answers.json"

-- | Recorded answers by question kind: an option at 0.9, or a bare "unsure".
answersOf :: [(Text, Text)] -> Text
answersOf kvs = TE.decodeUtf8 (BL.toStrict (encode (object ["answers" .= object [K.fromText k .= answer v | (k, v) <- kvs]])))
  where
    answer "unsure" = String "unsure"
    answer v = object ["choice" .= v, "confidence" .= (0.9 :: Double)]

-- | The question kinds the recorded decider was asked, in order.
asked :: FilePath -> IO [Text]
asked dir = do
  let f = answersFile dir <> ".asked"
  there <- doesFileExist f
  if there then T.lines <$> TIO.readFile f else pure []

reconciles :: FilePath -> IO [ReconcileRecord]
reconciles dir = do
  there <- doesFileExist (recordsPath dir)
  rs <- if there then fst . readRecords <$> TIO.readFile (recordsPath dir) else pure []
  pure [r | AReconcile r <- rs]

run :: Bool -> FilePath -> ServerState -> IO (ServerState, ReconcileAnswer)
run forced dir s =
  reconcile neverCancel forced (dir </> "plans/t.md") s >>= \case
    (s', Right a) -> pure (s', a)
    (_, Left e) -> fail (T.unpack e)

-- | A block as (after, after text, lines, replace, unit).
summary :: ReconcileBlock -> (Int, Maybe Text, [Text], Bool, Maybe (FilePath, Text, Text))
summary b = (blAfterRow (rbBlock b), blAfterText (rbBlock b), map ghText (blLines (rbBlock b)), rbReplace b, rbUnit b)

-- | The fingerprint of a unit of the table module now.
fpNow :: FilePath -> ServerState -> Text -> IO Text
fpNow dir s unit =
  projectFor (dir </> "plans/t.md") s >>= \case
    (s', Just (_, p)) -> fromMaybe "" . rsFingerprint <$> resolveAnchor s' p (Anchor table unit 0 0)
    _ -> fail "no project"

-- | The table module's test file, with a passing and a failing test.
tableTest :: FilePath
tableTest = "src/bistro/booking/models/table__test.py"

tableTestText :: Text
tableTestText =
  T.unlines
    [ "from bistro.booking.models.table import Table, helper", "", ""
    , "def test_helper() -> None:", "    assert helper() == 1", "", ""
    , "def test_label() -> None:", "    assert Table(\"a\", 2).label() == \"a\"", "", ""
    , "def test_wrong() -> None:", "    assert Table(\"a\", 2).label() == \"b\"" ]

stepTestsSpec :: Spec
stepTestsSpec = describe "Olifant.Server.Loop (a step's tests)" $ do
  let plan = T.unlines
        [ "# T", "", "## S", ""
        , "- [ ] the table labels itself @" <> T.pack table <> ":Table.label"
        , "  - test: it labels @" <> T.pack tableTest <> ":test_label \9675"
        , "- [ ] the table is wrong @" <> T.pack table <> ":Table.other"
        , "  - test: it is wrong @" <> T.pack tableTest <> ":test_wrong \9679"
        , "- [ ] no anchor here", "- [ ] gone @" <> T.pack table <> ":Nope" ]
      planPath dir = dir </> "plans/t.md"
  it "(a) testNameFor: the step's words in snake case, past its anchor and marks" $ do
    py <- shippedLanguage "python" []
    testNameFor py "the repository answers closed_on" `shouldBe` "test_the_repository_answers_closed_on"
    testNameFor py "- [ ] Count the seats!  @src/a.py:F \10003 abc1234" `shouldBe` "test_count_the_seats"
    testNameFor py "close -- the   table" `shouldBe` "test_close_the_table"
    testNameFor py "- [ ] mail user@host about it @src/a.py:F" `shouldBe` "test_mail_user_host_about_it"
  it "(b) stepStub: a test named for the step in the unit's test file, and its child line" $
    withLoop plan [] $ \s _ dir -> do
      (s1, r) <- stepStub (planPath dir) 4 s
      st <- either (fail . T.unpack) pure r
      let name = "test_the_table_labels_itself"
          child = "  - test: the table labels itself @" <> T.pack tableTest <> ":" <> name <> " \9675"
      saName (stAnswer st) `shouldBe` name
      stChild st `shouldBe` child
      stFact st `shouldBe` Insert 5 [child]
      let edit = saEdit (stAnswer st)
      fePath edit `shouldSatisfy` (tableTest `isSuffixOf`)
      feCreate edit `shouldBe` True
      TIO.writeFile (fePath edit) (feText edit)
      doesFileExist (dir </> tableTest) `shouldReturn` True
      (feText edit) `shouldSatisfy` T.isInfixOf ("def " <> name <> "()")
      -- again, the name is made unique against the test file
      (_, r2) <- stepStub (planPath dir) 4 s1
      fmap (saName . stAnswer) r2 `shouldSatisfy` either (const False) (\n -> n /= name && name `T.isPrefixOf` n)
  it "(c) stepStub refuses a step without an anchor, and one whose unit is not found" $
    withLoop plan [] $ \s _ dir -> do
      (_, r1) <- stepStub (planPath dir) 8 s
      either id (const "stubbed") r1 `shouldBe` "the step has no anchor"
      (_, r2) <- stepStub (planPath dir) 9 s
      either id (const "stubbed") r2 `shouldBe` "the step's unit was not found"
  it "(d) stepTest on a step runs its tests: green ticks it at its fingerprint; on a test child, red keeps it open" $
    withLoopTested plan [(tableTest, tableTestText)] $ \s _ dir -> do
      fp <- fpNow dir s "Table.label"
      (s1, r) <- stepTest (planPath dir) 4 s
      t <- either (fail . T.unpack) pure r
      ttFacts t `shouldBe` [Replace 5 ("  - test: it labels @" <> T.pack tableTest <> ":test_label \9679"), Replace 4 ("- [x] the table labels itself @" <> T.pack table <> ":Table.label \10003 " <> fp)]
      (ttPassed t, ttFailed t) `shouldBe` (1, 0)
      ttSeconds t `shouldSatisfy` (> 0)
      (_, r2) <- stepTest (planPath dir) 7 s1
      t2 <- either (fail . T.unpack) pure r2
      other <- fpNow dir s "Table.other"
      -- the step's only test: its mark, and the step stamped at its unit now
      ttFacts t2 `shouldBe` [Replace 7 ("  - test: it is wrong @" <> T.pack tableTest <> ":test_wrong \10007"), Replace 6 ("- [ ] the table is wrong @" <> T.pack table <> ":Table.other \10003 " <> other)]
      (ttPassed t2, ttFailed t2) `shouldBe` (0, 1)
      (_, r3) <- stepTest (planPath dir) 8 s1
      either id (const "ran") r3 `shouldBe` "the step has no tests"

  it "(e) a step's test child can name a Haskell describe: run through [test] unit, green ticks the step" $
    withHaskellLoop (M.fromList [("src/Olifant/PlansSpec.hs::Olifant.Plans::(a) reads a step", String "pass")]) $ \dir s -> do
      (_, r) <- stepTest (dir </> "plans/t.md") 4 s
      t <- either (fail . T.unpack) pure r
      take 1 (ttFacts t) `shouldBe` [Replace 5 "  - test: plans parse @src/Olifant/PlansSpec.hs:Olifant.Plans \9679"]
      map (\case Replace 4 l -> "- [x] " `T.isPrefixOf` l; _ -> False) (drop 1 (ttFacts t)) `shouldBe` [True]
      (_, r2) <- stepTest (dir </> "plans/t.md") 7 s
      either id (const "ran") r2 `shouldBe` "Nope is not in src/Olifant/PlansSpec.hs"

tidyPlan, jevConfig, climbConfig, helperEdited, withSpare :: Text
tidyPlan = "# T\n\n## Tables\n\nThe tables a bistro seats.\n\n- [ ] count the seats\n- [ ] tidy the helper\n"
jevConfig = "[decide]\nmodel = \"jev\"\n\n[model.backends.jev]\nkind = \"jev\"\n"
climbConfig =
  T.unlines
    [ "[model.backends.local]", "kind = \"openai-compatible\"", "url = \"http://127.0.0.1:1/v1\"", "model = \"x\"", "escalate = \"auto\""
    , "[model.backends.remote]", "kind = \"openai-compatible\"", "url = \"http://127.0.0.1:1/v1\"", "model = \"y\"", "escalate = \"ask\""
    , "[plans]", "ladder = [\"local\", \"remote\"]" ]
helperEdited = T.replace "return 1" "return 2" tableText
withSpare = tableText <> "\n\ndef spare() -> str:\n    return \"spare\"\n"

-- | A local server that accepts one connection and never answers: its port,
-- and an action that waits (30 s at most) for that connection.
withHangingServer :: (Text -> IO Bool -> IO a) -> IO a
withHangingServer act = do
  let script = "import socket,sys,time\ns=socket.socket()\ns.bind(('127.0.0.1',0))\ns.listen(1)\nprint(s.getsockname()[1],flush=True)\nc,_=s.accept()\nprint('accepted',flush=True)\ntime.sleep(60)\n"
  bracket (createProcess (proc "python3" ["-I", "-c", script]) {std_out = CreatePipe}) (\(_, _, _, h) -> terminateProcess h) $ \case
    (_, Just out, _, _) -> do
      port <- T.strip . T.pack <$> hGetLine out
      act port (maybe False (== "accepted") <$> timeout 30000000 (hGetLine out))
    _ -> fail "no server"

excerptsSpec :: Spec
excerptsSpec = describe "Olifant.Server.Loop (excerpts)" $ do
  let step box = "- " <> box <> " tidy the helper @src/bistro/booking/models/table.py:helper"
      fence done body = ["  ```python @src/bistro/booking/models/table.py:helper" <> (if done then " · done" else "")] ++ map ("  " <>) body ++ ["  ```"]
      planOf box done body = T.unlines (["# T", "", "## S", "", step box] ++ fence done body ++ ["- [ ] count the seats"])
      helperNow = ["def helper() -> int:", "    return 1"]
      readPlan dir = TIO.readFile (dir </> "plans/t.md")
      reconcileOnce dir s = do
        (s', a) <- run False dir s
        text <- readPlan dir
        TIO.writeFile (dir </> "plans/t.md") (applyFactsText (raFacts a) text)
        pure (s', raFacts a)
      answers = Just (answersOf [("plan-state", "in_progress"), ("plan-step", "none")])
  it "applyFactsText: a range grown and one shrunk, beside an insert below" $
    applyFactsText [ReplaceRows 1 2 ["B1", "B2", "B3"], ReplaceRows 4 6 ["E"], Insert 6 ["after"], Insert 2 ["below B"]] (T.intercalate "\n" ["a", "b1", "b2", "c", "e1", "e2", "e3", "f"])
      `shouldBe` T.intercalate "\n" ["a", "B1", "B2", "B3", "below B", "c", "E", "after", "f"]
  it "the unit changed on disk: the open step's excerpt has the new code" $
    withReconcile (planOf "[ ]" False helperNow) [(table, helperEdited)] jevConfig answers $ \dir s -> do
      (_, facts) <- reconcileOnce dir s
      length [() | ReplaceRows {} <- facts] `shouldBe` 1
      readPlan dir `shouldReturn` planOf "[ ]" False ["def helper() -> int:", "    return 2"]
  it "the same code: no fact" $
    withReconcile (planOf "[ ]" False helperNow) [] jevConfig answers $ \dir s ->
      (raFacts . snd <$> run False dir s) `shouldReturn` []
  it "ticked with the unit changed in the same pass: frozen with the final code" $
    withReconcile (planOf "[x]" False helperNow) [(table, helperEdited)] jevConfig answers $ \dir s -> do
      (_, facts) <- reconcileOnce dir s
      length facts `shouldBe` 1
      readPlan dir `shouldReturn` planOf "[x]" True ["def helper() -> int:", "    return 2"]
  it "ticked: frozen; then never written again, the unit changed or the step open again" $
    withReconcile (planOf "[x]" False helperNow) [] jevConfig answers $ \dir s -> do
      (s1, facts) <- reconcileOnce dir s
      facts `shouldBe` [ReplaceRows 5 8 (fence True helperNow)]
      readPlan dir `shouldReturn` planOf "[x]" True helperNow
      (s2, again) <- reconcileOnce dir s1
      again `shouldBe` []
      write dir table helperEdited
      (s3, edited) <- reconcileOnce dir s2
      [() | ReplaceRows {} <- edited] `shouldBe` []
      readPlan dir `shouldReturn` planOf "[x]" True helperNow
      TIO.writeFile (dir </> "plans/t.md") (planOf "[ ]" True helperNow)
      (_, reopened) <- reconcileOnce dir s3
      [() | ReplaceRows {} <- reopened] `shouldBe` []
  it "a unit gone leaves the excerpt as it was" $
    withReconcile (planOf "[ ]" False helperNow) [(table, T.replace helper "" tableText)] jevConfig answers $ \dir s -> do
      (_, facts) <- reconcileOnce dir s
      [() | ReplaceRows {} <- facts] `shouldBe` []

claimPassSpec :: Spec
claimPassSpec = describe "Olifant.Server.Loop (claimPass)" $ describe "claimPass" $ do
  let plan = "The helper returns one.\n"
      -- every sentence: a claim about the code, sure on both counts
      claims = TE.decodeUtf8 (BL.toStrict (encode (object ["answers" .= object
        [ "sentence-kind" .= object ["choice" .= ("claim" :: Text), "confidence" .= (0.9 :: Double), "probabilities" .= object ["claim" .= (0.9 :: Double), "prose" .= (0.1 :: Double)]]
        , "about-code" .= object ["choice" .= True, "confidence" .= (0.9 :: Double)] ]])))
      checksFile dir = dir </> ".olifant/checks.json"
      reply :: Text -> Value
      reply verdict = object ["text" .= ("I read it.\n```json\n{\"verdict\": \"" <> verdict <> "\", \"why\": \"it returns 1\", \"evidence\": [{\"path\": \"" <> T.pack table <> "\", \"unit\": \"helper\"}]}\n```"), "cost" .= (0.1 :: Double)]
      replies :: [(Text, Value)] -> Text
      replies kvs = TE.decodeUtf8 (BL.toStrict (encode (object ["replies" .= object [K.fromText k .= v | (k, v) <- kvs]])))
      withPass text extra checks act =
        withReconcile text [] (jevConfig <> extra) (Just claims) $ \dir s -> do
          TIO.writeFile (checksFile dir) (replies checks)
          act dir s {ssConfig = (ssConfig s) {scCheckAnswers = Just (checksFile dir)}}
      pass mode dir s =
        claimPass neverCancel mode (dir </> "plans/t.md") s >>= \case
          (s', Right a) -> pure (s', a)
          (_, Left e) -> fail (T.unpack e)
      checksAsked dir = do
        let f = checksFile dir <> ".asked"
        there <- doesFileExist f
        if there then T.lines <$> TIO.readFile f else pure []
      entry a k = M.lookup k (paStore a)
      verdictOf a k = cfJudgement . cjConfirmed <$> (entry a k >>= seCheck)
      passes dir = do
        there <- doesFileExist (recordsPath dir)
        rs <- if there then fst . readRecords <$> TIO.readFile (recordsPath dir) else pure []
        pure ([r | APass r <- rs], [e | AnErrand e <- rs])
  describe "joins" $ do
    let wrapped = "The helper returns\none.\n"
        claimsJoining yes = TE.decodeUtf8 (BL.toStrict (encode (object ["answers" .= object
          [ "sentence-kind" .= object ["choice" .= ("claim" :: Text), "confidence" .= (0.9 :: Double), "probabilities" .= object ["claim" .= (0.9 :: Double), "prose" .= (0.1 :: Double)]]
          , "about-code" .= object ["choice" .= True, "confidence" .= (0.9 :: Double)]
          , "line-continues" .= object ["choice" .= yes, "confidence" .= (0.9 :: Double)] ]])))
        withJoining yes act =
          withReconcile wrapped [] jevConfig (Just (claimsJoining yes)) $ \dir s -> do
            TIO.writeFile (checksFile dir) (replies [("The helper returns one.", reply "right")])
            act dir s {ssConfig = (ssConfig s) {scCheckAnswers = Just (checksFile dir)}}
    it "a wrapped row Jev says continues is one sentence, checked whole; the answer is kept" $
      withJoining True $ \dir s -> do
        (s1, a) <- pass SavePass dir s
        verdictOf a "The helper returns one." `shouldBe` Just JRight
        M.member "The helper returns" (paStore a) `shouldBe` False
        readJoins (ssPorts s) dir "plans/t.md" `shouldReturn` M.fromList [("The helper returns ⏎ one.", True)]
        _ <- pass SavePass dir s1
        kinds <- asked dir
        length (filter (== "line-continues") kinds) `shouldBe` 1
        fmap prJevCalls (paRecord a) `shouldBe` Just 2
    it "rows Jev says do not continue are two sentences" $
      withJoining False $ \dir s -> do
        (_, a) <- pass IdlePass dir s
        M.keys (M.filter ((== Nothing) . seGone) (paStore a)) `shouldBe` ["The helper returns", "one."]
        readJoins (ssPorts s) dir "plans/t.md" `shouldReturn` M.fromList [("The helper returns ⏎ one.", False)]
    it "with no Jev every row is its own sentence and nothing is stored for joins" $
      withReconcile wrapped [] "" Nothing $ \dir s -> do
        (_, a) <- pass IdlePass dir s
        M.keys (paStore a) `shouldBe` ["The helper returns", "one."]
        readJoins (ssPorts s) dir "plans/t.md" `shouldReturn` M.empty
  it "Check this on a step records a progress check, and its errand line says so" $
    withPass "- [ ] Make the helper return one\n" "" [("Make the helper return one", reply "done")] $ \dir s -> do
      (_, r) <- checkOne neverCancel (dir </> "plans/t.md") "Make the helper return one" s
      store <- either (fail . T.unpack) pure r
      let ch = M.lookup "Make the helper return one" store >>= seCheck
      fmap (\c -> (cjMode c, cfJudgement (cjConfirmed c))) ch `shouldBe` Just (CheckProgress, JDone)
      (_, es) <- passes dir
      map (\e -> (erVerb e, erMode e, erVerdict e)) es `shouldBe` [("check", Just "progress", Just "done")]
      prompts <- TIO.readFile (checksFile dir <> ".prompts")
      prompts `shouldSatisfy` T.isInfixOf "done | open | misfit | cannot_tell"
  it "idle triages and checks nothing" $
    withPass plan "" [("The helper returns one.", reply "right")] $ \dir s -> do
      (_, a) <- pass IdlePass dir s
      fmap seRoute (entry a "The helper returns one.") `shouldBe` Just Verify
      fmap (trBy . seTriage) (entry a "The helper returns one.") `shouldBe` Just "jev"
      checksAsked dir `shouldReturn` []
      fmap prTriaged (paRecord a) `shouldBe` Just 1
  it "save checks a claim and stores right, with its unit and fingerprint" $
    withPass plan "" [("The helper returns one.", reply "right")] $ \dir s -> do
      (_, a) <- pass SavePass dir s
      fp <- fpNow dir s "helper"
      let cf = cjConfirmed <$> (entry a "The helper returns one." >>= seCheck)
      fmap (\c -> (cfJudgement c, cfUnit c, cfFingerprint c)) cf `shouldBe` Just (JRight, Just (UnitRef table "helper"), Just fp)
      (ps, es) <- passes dir
      map (\r -> (prMode r, prChecked r, prRight r, prQueued r)) ps `shouldBe` [("save", 1, 1, 0)]
      map (\e -> (erVerb e, erAuto e, erCost e)) es `shouldBe` [("check", True, Just 0.1)]
      stored <- readClaims (ssPorts s) dir "plans/t.md"
      stored `shouldBe` paStore a
  it "open with no store triages only" $
    withPass plan "" [("The helper returns one.", reply "right")] $ \dir s -> do
      (_, a) <- pass OpenPass dir s
      verdictOf a "The helper returns one." `shouldBe` Nothing
      checksAsked dir `shouldReturn` []
  describe "recheck" $ do
    let editHelper dir s = do
          TIO.writeFile (dir </> table) (T.replace "return 1" "return 2" tableText)
          filesChanged [dir </> table] s
    it "with recheck = \"save\", a save re-checks a stale verdict" $
      withPass plan "[plans]\nrecheck = \"save\"\n" [("The helper returns one.", reply "right")] $ \dir s -> do
        (s1, _) <- pass SavePass dir s
        s2 <- editHelper dir s1
        (_, a) <- pass SavePass dir s2
        checksAsked dir `shouldReturn` ["The helper returns one.", "The helper returns one."]
        fmap prChecked (paRecord a) `shouldBe` Just 1
    it "by default a stale verdict waits to be asked" $
      withPass plan "" [("The helper returns one.", reply "right")] $ \dir s -> do
        (s1, _) <- pass SavePass dir s
        s2 <- editHelper dir s1
        _ <- pass SavePass dir s2
        checksAsked dir `shouldReturn` ["The helper returns one."]
  it "an m1 store opened and saved checks nothing" $
    withPass plan "" [("The helper returns one.", reply "wrong")] $ \dir s -> do
      -- a milestone 1 store: version 2, no "owed"
      createDirectoryIfMissing True (dir </> ".olifant/state/claims/plans")
      TIO.writeFile (dir </> ".olifant/state/claims/plans/t.json") "{\"version\": 2, \"claims\": {\"The helper returns one.\": {\"triage\": {\"kind\": \"claim\", \"p\": 0.9, \"about\": 0.9, \"by\": \"jev\"}, \"route\": \"verify\", \"check\": null, \"failure\": null, \"at\": \"2026-11-07T00:00:00Z\", \"gone\": null}}}"
      (s1, _) <- pass OpenPass dir s
      (_, a) <- pass SavePass dir s1
      checksAsked dir `shouldReturn` []
      claimHover (paStore a) S.empty (sentences (parsePlan plan)) `shouldBe` Just "Unchecked · Check this, or Check the plan"
  it "opening a plan with a store checks nothing" $
    withPass plan "" [("The helper returns one.", reply "wrong")] $ \dir s -> do
      (s1, a1) <- pass IdlePass dir s
      fmap seOwed (entry a1 "The helper returns one.") `shouldBe` Just True
      (_, a) <- pass OpenPass dir s1
      verdictOf a "The helper returns one." `shouldBe` Nothing
      checksAsked dir `shouldReturn` []
  it "a sentence written then saved is checked once" $
    withPass plan "" [("The helper returns one.", reply "right")] $ \dir s -> do
      (s1, _) <- pass IdlePass dir s
      (s2, _) <- pass SavePass dir s1
      _ <- pass SavePass dir s2
      checksAsked dir `shouldReturn` ["The helper returns one."]
  it "a step written then saved gets a progress check" $
    withPass "- [ ] Make the helper return one\n" "" [("Make the helper return one", reply "done")] $ \dir s -> do
      (s1, _) <- pass IdlePass dir s
      (_, a) <- pass SavePass dir s1
      fmap (\c -> (cjMode c, cfJudgement (cjConfirmed c))) (entry a "Make the helper return one" >>= seCheck) `shouldBe` Just (CheckProgress, JDone)
  it "a ticked step is never checked automatically" $
    withPass "- [x] Make the helper return one\n" "" [("Make the helper return one", reply "done")] $ \dir s -> do
      (s1, _) <- pass IdlePass dir s
      _ <- pass SavePass dir s1
      checksAsked dir `shouldReturn` []
  it "what the cost cap leaves is backlog" $ do
    let many = T.unlines ["Claim number " <> tshow n <> " holds." | n <- [1 .. 6 :: Int]]
        keys = ["Claim number " <> tshow n <> " holds." | n <- [1 .. 6 :: Int]]
    withPass many "[plans]\npass_cost = 0.25\n" [(k, reply "right") | k <- keys] $ \dir s -> do
      (s1, a1) <- pass SavePass dir s
      length [k | k <- keys, verdictOf a1 k == Just JRight] `shouldBe` 2
      stored <- readClaims (ssPorts s) dir "plans/t.md"
      [k | k <- keys, Just e <- [M.lookup k stored], seOwed e] `shouldBe` []
      fmap prQueued (paRecord a1) `shouldBe` Just 0
      _ <- pass SavePass dir s1
      length <$> checksAsked dir `shouldReturn` 2
  it "a failed check is retried once" $
    withPass plan "" [] $ \dir s -> do
      (s1, a1) <- pass SavePass dir s
      fmap seOwed (entry a1 "The helper returns one.") `shouldBe` Just True
      let aged st = do
            now <- pNow (ssPorts s)
            stored <- readClaims (ssPorts s) dir "plans/t.md"
            storeClaims st dir (dir </> "plans/t.md") (M.adjust (\e -> e {seFailure = fmap (\f -> f {C.fuAt = addUTCTime (-660) now}) (seFailure e)}) "The helper returns one." stored)
      aged s1
      (s2, a2) <- pass SavePass dir s1
      length <$> checksAsked dir `shouldReturn` 2
      fmap seOwed (entry a2 "The helper returns one.") `shouldBe` Just False
      aged s2
      _ <- pass SavePass dir s2
      length <$> checksAsked dir `shouldReturn` 2
  it "failures that cost nothing do not lower the estimate: the cap holds" $ do
    let plan6 = T.unlines ["Fails number " <> tshow n <> " here." | n <- [1 .. 3 :: Int]] <> T.unlines ["Claim number " <> tshow n <> " holds." | n <- [1 .. 3 :: Int]]
        dear = object ["text" .= ("I read it.\n```json\n{\"verdict\": \"right\", \"why\": \"it returns 1\", \"evidence\": [{\"path\": \"" <> T.pack table <> "\", \"unit\": \"helper\"}]}\n```" :: Text), "cost" .= (0.2 :: Double)]
    withPass plan6 "" [("Claim number " <> tshow n <> " holds.", dear) | n <- [1 .. 3 :: Int]] $ \dir s -> do
      (_, a) <- pass SavePass dir s
      fmap (\r -> (prFailed r, prChecked r)) (paRecord a) `shouldBe` Just (3, 2)
      fmap ((<= 0.5 + 1e-9) . prCost) (paRecord a) `shouldBe` Just True
  it "a check that throws leaves no sentence marked as being checked" $
    withPass plan "" [("The helper returns one.", reply "right")] $ \dir s -> do
      -- the recorded checker cannot note what it was asked: it throws
      let s' = s {ssConfig = (ssConfig s) {scCheckAnswers = Just (dir </> "no-such-dir/checks.json")}}
      r <- try (claimPass neverCancel SavePass (dir </> "plans/t.md") s') :: IO (Either SomeException (ServerState, Either Text PassAnswer))
      either (const True) (const False) r `shouldBe` True
      readIORef (ssChecking s) `shouldReturn` M.empty
  it "a failed check waits ten minutes" $
    withPass plan "" [] $ \dir s -> do
      (s1, a1) <- pass SavePass dir s
      fmap C.fuWhy (entry a1 "The helper returns one." >>= seFailure) `shouldBe` Just "no recorded reply"
      verdictOf a1 "The helper returns one." `shouldBe` Nothing
      length <$> checksAsked dir `shouldReturn` 1
      (s2, _) <- pass SavePass dir s1
      length <$> checksAsked dir `shouldReturn` 1
      now <- pNow (ssPorts s2)
      let old = M.adjust (\e -> e {seFailure = Just (C.Failure "timed out" (addUTCTime (-660) now))}) "The helper returns one." (paStore a1)
      storeClaims s2 dir (dir </> "plans/t.md") old
      _ <- pass SavePass dir s2
      length <$> checksAsked dir `shouldReturn` 2
  it "a cancel lets the check in flight finish: stored while its sentence is in the plan, and counted" $ do
    let three = "First claim holds.\nSecond claim holds.\nThird claim holds.\n"
        slow = object ["text" .= ("I read it.\n```json\n{\"verdict\": \"right\", \"why\": \"it returns 1\", \"evidence\": [{\"path\": \"" <> T.pack table <> "\", \"unit\": \"helper\"}]}\n```" :: Text), "cost" .= (0.1 :: Double), "delay" .= (1 :: Double)]
    withPass three "" [("First claim holds.", reply "right"), ("Second claim holds.", slow), ("Third claim holds.", reply "right")] $ \dir s -> do
      (token, trip) <- newCancel
      -- trip while the second check is in flight
      _ <- async (let wait = checksAsked dir >>= \ks -> if length ks >= 2 then trip else threadDelay 10000 >> wait in wait)
      (_, r) <- claimPass token SavePass (dir </> "plans/t.md") s
      either id (const "answered") r `shouldBe` "cancelled"
      stored <- readClaims (ssPorts s) dir "plans/t.md"
      let checkedOf k = cfJudgement . cjConfirmed <$> (M.lookup k stored >>= seCheck)
      map checkedOf ["First claim holds.", "Second claim holds.", "Third claim holds."] `shouldBe` [Just JRight, Just JRight, Nothing]
      [k | (k, e) <- M.toList stored, isJust (seFailure e)] `shouldBe` []
      (ps, _) <- passes dir
      map (\p -> (prChecked p, prCost p)) ps `shouldBe` [(2, 0.2)]
  it "a check whose sentence changed while in flight is dropped, its cost still counted" $ do
    let two = "First claim holds.\nSecond claim holds.\n"
        slow = object ["text" .= ("x" :: Text), "cost" .= (0.1 :: Double), "delay" .= (1 :: Double)]
    withPass two "" [("First claim holds.", slow), ("Second claim holds.", reply "right")] $ \dir s -> do
      live <- newIORef two
      let s' = s {ssLiveDoc = Just (\_ -> Just . (2,) <$> readIORef live)}
      -- the first sentence is edited while its check runs
      _ <- async (let wait = checksAsked dir >>= \ks -> if length ks >= 1 then modifyIORef live (T.replace "First" "One" ) else threadDelay 10000 >> wait in wait)
      (_, a) <- pass SavePass dir s'
      stored <- readClaims (ssPorts s) dir "plans/t.md"
      (M.lookup "First claim holds." stored >>= seCheck) `shouldBe` Nothing
      (M.lookup "First claim holds." stored >>= seFailure) `shouldBe` Nothing
      fmap prCost (paRecord a) `shouldBe` Just 0.2
  it "a sentence another pass is checking is not checked again" $
    withPass plan "" [("The helper returns one.", reply "right")] $ \dir s -> do
      modifyIORef (ssChecking s) (M.insert (dir </> "plans/t.md") (S.singleton "The helper returns one."))
      (_, a) <- pass SavePass dir s
      checksAsked dir `shouldReturn` []
      fmap prChecked (paRecord a) `shouldBe` Just 0
  it "Check this during a pass: neither write loses the other's verdict, nothing checked twice" $ do
    let two = "First claim holds.\nSecond claim holds.\n"
        slow = object ["text" .= ("I read it.\n```json\n{\"verdict\": \"right\", \"why\": \"it returns 1\", \"evidence\": [{\"path\": \"" <> T.pack table <> "\", \"unit\": \"helper\"}]}\n```" :: Text), "cost" .= (0.1 :: Double), "delay" .= (1 :: Double)]
    withPass two "" [("First claim holds.", slow), ("Second claim holds.", reply "wrong")] $ \dir s -> do
      (s1, _) <- pass IdlePass dir s
      -- Check this on the second while the pass checks the first
      one <- async (let go = checksAsked dir >>= \ks -> if length ks >= 1 then checkOne neverCancel (dir </> "plans/t.md") "Second claim holds." s1 else threadDelay 10000 >> go in go)
      _ <- pass SavePass dir s1
      _ <- waitCatch one
      stored <- readClaims (ssPorts s) dir "plans/t.md"
      let checkedOf k = cfJudgement . cjConfirmed <$> (M.lookup k stored >>= seCheck)
      map checkedOf ["First claim holds.", "Second claim holds."] `shouldBe` [Just JRight, Just JWrong]
      checksAsked dir `shouldReturn` ["First claim holds.", "Second claim holds."]
  it "a one-word edit and its undo keep the paid check: nothing is checked again" $
    withPass plan "" [("The helper returns one.", reply "right"), ("The helper returns two.", reply "wrong")] $ \dir s -> do
      (s1, _) <- pass SavePass dir s
      TIO.writeFile (dir </> "plans/t.md") "The helper returns two.\n"
      (s2, _) <- pass IdlePass dir s1
      TIO.writeFile (dir </> "plans/t.md") plan
      (s3, _) <- pass IdlePass dir s2
      (_, a) <- pass SavePass dir s3
      verdictOf a "The helper returns one." `shouldBe` Just JRight
      checksAsked dir `shouldReturn` ["The helper returns one."]
  it "the pass writes nothing to the plan" $
    withPass plan "" [("The helper returns one.", reply "right")] $ \dir s -> do
      before <- TIO.readFile (dir </> "plans/t.md")
      _ <- pass SavePass dir s
      TIO.readFile (dir </> "plans/t.md") `shouldReturn` before
  it "a hand anchor goes into the prompt" $
    withPass ("- The helper returns one @" <> T.pack table <> ":helper\n") "" [("The helper returns one", reply "right")] $ \dir s -> do
      _ <- pass SavePass dir s
      prompts <- TIO.readFile (checksFile dir <> ".prompts")
      prompts `shouldSatisfy` T.isInfixOf ("the writer pointed at `" <> T.pack table <> ":helper`")
  it "pass_budget = 0 runs nothing" $
    withPass plan "[plans]\npass_budget = 0\n" [("The helper returns one.", reply "right")] $ \dir s -> do
      (_, a) <- pass SavePass dir s
      paRecord a `shouldBe` Nothing
      asked dir `shouldReturn` []
      checksAsked dir `shouldReturn` []
  it "no [decide] model: bullets are claims by the grammar and still checked" $
    withReconcile "- The helper returns one.\n" [] "" Nothing $ \dir s0 -> do
      TIO.writeFile (checksFile dir) (replies [("The helper returns one.", reply "right")])
      let s = s0 {ssConfig = (ssConfig s0) {scCheckAnswers = Just (checksFile dir)}}
      (_, a) <- pass SavePass dir s
      fmap (trBy . seTriage) (entry a "The helper returns one.") `shouldBe` Just "grammar"
      verdictOf a "The helper returns one." `shouldBe` Just JRight

