{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.CatalogSpec (spec) where

import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Olifant.Language.Grammar (GrammarSettings (..), grammarSettings)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing, emptyPermissions, setOwnerExecutable, setOwnerReadable, setOwnerWritable, setPermissions)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.Catalog
import Olifant.Generation
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Olifant.Generator (EditWith (..), GeneratorId (..), GeneratorWith (..))
import Olifant.Naming (parseName)
import Olifant.Template (Value (..), render)
import Olifant.Language (Language (..), Match (..), Provider (..), Syntax (..), TestDef (..), originOf, querying, noSyntax)
import Olifant.Region (RegionDefWith (..))
import Olifant.Workspace
import Test.Hspec

files :: [(FilePath, Text)]
files =
  [ ("/proj/.olifant/config.toml", "[packs]\nrails = { path = \"../packs/rails\" }\n")
  , ("/proj/.olifant/generators/model/generator.toml", "description = \"local model\"\nlanguage = \"ruby\"\n[inputs.name]\ntype = \"ident\"\n[[edits]]\ncreate = \"local/{= name|snake =}.rb\"\ntext = \"local\"\n")
  , ("/packs/rails/generators/model/generator.toml", "description = \"pack model\"\nlanguage = \"ruby\"\n[inputs.name]\ntype = \"ident\"\n[[edits]]\ncreate = \"app/models/{= name|snake =}.rb\"\ntemplate = \"model.rb.dt\"\n")
  , ("/packs/rails/generators/model/templates/model.rb.dt", "class {= name|pascal =}\nend\n")
  , ("/packs/rails/generators/cell/generator.toml", "description = \"cell\"\nlanguage = \"ts\"\n[inputs.name]\ntype = \"ident\"\n[[edits]]\ncreate = \"src/{= name|snake =}.ts\"\nexemplar = \"fixture:src/blog_post.ts\"\nbind = { name = \"blog_post\" }\n")
  , ("/packs/rails/fixtures/app/src/blog_post.ts", "export const BlogPost = 'blog_posts'\n")
  , ("/packs/rails/generators/broken/generator.toml", "description = \"broken\"\nlanguage = \"x\"\n[[edits]]\ncreate = \"a\"\ntemplate = \"missing.dt\"\n")
  ]

now :: UTCTime
now = UTCTime (fromGregorian 2026 9 27) 0

projectOnly :: CatalogPaths
projectOnly = CatalogPaths {catalogProjectDir = "/proj", catalogUserConfigDir = Nothing, catalogHomeDir = Nothing}

withUser :: CatalogPaths
withUser = projectOnly {catalogUserConfigDir = Just "/home/u/.config/olifant", catalogHomeDir = Just "/home/u"}

withCatalog :: [(FilePath, Text)] -> (Workspace IO -> Catalog -> IO a) -> IO a
withCatalog = withCatalogAt projectOnly

-- | The project catalog of these files, or why it does not open.
openWith :: [(FilePath, Text)] -> IO (Either CatalogError Catalog)
openWith fs = memoryWorkspace fs >>= \(root, _) -> openProjectCatalog root projectOnly

withCatalogAt :: CatalogPaths -> [(FilePath, Text)] -> (Workspace IO -> Catalog -> IO a) -> IO a
withCatalogAt paths fs k = do
  (root, _) <- memoryWorkspace fs
  openProjectCatalog root paths >>= \case
    Left err -> error (show err)
    Right cat -> k root cat

genInto :: Workspace IO -> Catalog -> Text -> IO (M.Map FilePath Text)
genInto root cat ref = do
  let Right entry = findEntry cat ref
  Right gen <- loadGenerator root noSyntax cat entry
  Right plan <- generate (within "/proj" root) noResolve noTools [Step gen (M.fromList [("name", "cancel_subscription")])] now PlanOnly
  pure (planWrites plan)

spec :: Spec
spec = describe "catalog" $ do
  describe "[recognize] (convention recognition)" $ do
    it "reads enabled and model; on, and no model, by default" $ do
      withCatalog [("/proj/.olifant/config.toml", "[recognize]\nenabled = false\nmodel = \"local\"\n")] $ \_ cat ->
        catalogRecognize cat `shouldBe` RecognizeConfig False (Just "local")
      withCatalog [("/proj/.olifant/config.toml", "")] $ \_ cat ->
        catalogRecognize cat `shouldBe` RecognizeConfig True Nothing
  describe "[decide] (typed decisions: Jev, progressively)" $ do
    it "reads the model; [recognize] model is its alias" $ do
      withCatalog [("/proj/.olifant/config.toml", "[decide]\nmodel = \"jev\"\n[model.backends.jev]\nkind = \"jev\"\n")] $ \_ cat -> catalogDecideModel cat `shouldBe` Just "jev"
      withCatalog [("/proj/.olifant/config.toml", "[recognize]\nmodel = \"local\"\n")] $ \_ cat -> catalogDecideModel cat `shouldBe` Just "local"
      withCatalog [("/proj/.olifant/config.toml", "")] $ \_ cat -> catalogDecideModel cat `shouldBe` Nothing
    it "refuses a [decide] model that names no backend, saying where the version goes" $ do
      let backends = "[model.backends.local]\nkind = \"openai-compatible\"\nurl = \"http://127.0.0.1:1/v1\"\nmodel = \"x\"\n[model.backends.jev]\nkind = \"jev\"\n"
      r <- openWith [("/proj/.olifant/config.toml", "[decide]\nmodel = \"jev-1.14.0\"\n" <> backends)]
      case r of
        Left (ConfigInvalid _ e) -> e `shouldBe` "[decide] model = \"jev-1.14.0\" names no backend (backends: jev, local). It takes a backend's name; pin the version in that backend's model key."
        _ -> expectationFailure "expected ConfigInvalid for a [decide] model naming no backend"
      withCatalog [("/proj/.olifant/config.toml", "[decide]\nmodel = \"jev\"\n" <> backends)] $ \_ cat -> catalogDecideModel cat `shouldBe` Just "jev"
  describe "[conjure] (conjure, second pass)" $
    it "reads [conjure] wire and refuses other keys" $ do
      withCatalog [("/proj/.olifant/config.toml", "[conjure]\nwire = \"wire\"\n")] $ \_ cat -> catalogConjureWire cat `shouldBe` Just "wire"
      withCatalog [("/proj/.olifant/config.toml", "")] $ \_ cat -> catalogConjureWire cat `shouldBe` Nothing
      r <- openWith [("/proj/.olifant/config.toml", "[conjure]\nwire = \"wire\"\nother = 1\n")]
      case r of
        Left (ConfigInvalid _ e) -> e `shouldSatisfy` T.isInfixOf "conjure"
        _ -> expectationFailure "expected ConfigInvalid naming conjure"
  describe "[plans] (plans, stage 1)" $ do
    it "defaults when absent" $
      withCatalog [("/proj/.olifant/config.toml", "")] $ \_ cat -> catalogPlans cat `shouldBe` defaultPlans
    it "reads every key" $
      withCatalog [("/proj/.olifant/config.toml", "[plans]\ndir = \"docs/plans\"\nrunner = \"/tmp/shim\"\nmodel = \"opus\"\ntimeout = 900\ndo_tools = [\"Bash(cabal test*)\"]\n")] $ \_ cat ->
        catalogPlans cat `shouldBe` defaultPlans {pcDir = "docs/plans", pcRunner = "/tmp/shim", pcModel = "opus", pcTimeout = 900, pcDoTools = ["Bash(cabal test*)"]}
    it "reads the loop's ladder and budget (stage 2); absent, neither" $ do
      withCatalog [("/proj/.olifant/config.toml", "[plans]\nladder = [\"local\"]\nbudget = 60\n[model.backends.local]\nkind = \"openai-compatible\"\nurl = \"http://x/v1\"\nmodel = \"q\"\n")] $ \_ cat ->
        (pcLadder (catalogPlans cat), pcBudget (catalogPlans cat)) `shouldBe` (Just ["local"], Just 60)
      withCatalog [("/proj/.olifant/config.toml", "[plans]\nmodel = \"opus\"\n")] $ \_ cat ->
        (pcLadder (catalogPlans cat), pcBudget (catalogPlans cat)) `shouldBe` (Nothing, Nothing)
    it "reads the claim pass's budget; absent, none; below 0, a config error" $ do
      withCatalog [("/proj/.olifant/config.toml", "[plans]\npass_budget = 0\n")] $ \_ cat -> pcPassBudget (catalogPlans cat) `shouldBe` Just 0
      withCatalog [("/proj/.olifant/config.toml", "")] $ \_ cat -> pcPassBudget (catalogPlans cat) `shouldBe` Nothing
      r <- openWith [("/proj/.olifant/config.toml", "[plans]\npass_budget = -1\n")]
      case r of
        Left (ConfigInvalid _ e) -> e `shouldSatisfy` T.isInfixOf "pass_budget"
        _ -> expectationFailure "expected ConfigInvalid naming pass_budget"
    it "reads the checker's keys; bad values and unknown keys are errors" $ do
      withCatalog [("/proj/.olifant/config.toml", "[plans]\npass_cost = 1.5\ncheck_model = \"haiku\"\ncheck_timeout = 60\nnot_evidence = [\"docs\", \"book\"]\n")] $ \_ cat ->
        let c = catalogPlans cat in (pcPassCost c, pcCheckModel c, pcCheckTimeout c, pcNotEvidence c) `shouldBe` (Just 1.5, "haiku", 60, ["docs", "book"])
      withCatalog [("/proj/.olifant/config.toml", "")] $ \_ cat ->
        let c = catalogPlans cat in (pcPassCost c, pcCheckModel c, pcCheckTimeout c, pcNotEvidence c) `shouldBe` (Nothing, "sonnet", 120, ["docs"])
      let bad toml word = openWith [("/proj/.olifant/config.toml", toml)] >>= \case
            Left (ConfigInvalid _ e) -> e `shouldSatisfy` T.isInfixOf word
            _ -> expectationFailure ("expected ConfigInvalid naming " <> T.unpack word)
      bad "[plans]\npass_cost = -1\n" "pass_cost"
      bad "[plans]\ncheck_timeout = 0\n" "check_timeout"
      bad "[plans]\ncheck_cost = -1\n" "check_cost"
    it "reads check_cost: 5.00 when absent" $ do
      withCatalog [("/proj/.olifant/config.toml", "")] $ \_ cat -> pcCheckCost (catalogPlans cat) `shouldBe` 5.00
      withCatalog [("/proj/.olifant/config.toml", "[plans]\ncheck_cost = 0.25\n")] $ \_ cat -> pcCheckCost (catalogPlans cat) `shouldBe` 0.25
    it "reads recheck: asked by default, or save; anything else names the key" $ do
      withCatalog [("/proj/.olifant/config.toml", "[plans]\nrecheck = \"save\"\n")] $ \_ cat -> pcRecheck (catalogPlans cat) `shouldBe` RecheckSave
      withCatalog [("/proj/.olifant/config.toml", "")] $ \_ cat -> pcRecheck (catalogPlans cat) `shouldBe` RecheckAsked
      openWith [("/proj/.olifant/config.toml", "[plans]\nrecheck = \"later\"\n")] >>= \case
        Left (ConfigInvalid _ e) -> e `shouldSatisfy` T.isInfixOf "recheck"
        _ -> expectationFailure "expected ConfigInvalid naming recheck"
    it "a ladder name with no backend is a config error, as [model] ladder's" $ do
      r <- openWith [("/proj/.olifant/config.toml", "[plans]\nladder = [\"gpt\"]\n")]
      case r of
        Left (ConfigInvalid _ e) -> e `shouldSatisfy` (\m -> T.isInfixOf "plans" m && T.isInfixOf "no such backend `gpt`" m)
        _ -> expectationFailure "expected ConfigInvalid naming the ladder's backend"
    it "refuses other keys, naming the section" $ do
      r <- openWith [("/proj/.olifant/config.toml", "[plans]\nother = 1\n")]
      case r of
        Left (ConfigInvalid _ e) -> e `shouldSatisfy` T.isInfixOf "plans"
        _ -> expectationFailure "expected ConfigInvalid naming plans"
    it "prices default to the four current models at list price" $
      withCatalog [("/proj/.olifant/config.toml", "")] $ \_ cat -> do
        map fst (pcPrices (catalogPlans cat)) `shouldMatchList` ["claude-opus-5-5", "claude-sonnet-5-5", "claude-haiku-5-5", "claude-fable-5-1"]
        lookup "claude-opus-5-5" (pcPrices (catalogPlans cat)) `shouldBe` Just (Price 4 20 0.2 5 8)
        lookup "claude-sonnet-5-5" (pcPrices (catalogPlans cat)) `shouldBe` Just (Price 2 10 0.2 2.5 4)
        lookup "claude-haiku-5-5" (pcPrices (catalogPlans cat)) `shouldBe` Just (Price 0.1 0.5 0.01 0.125 0.2)
        lookup "claude-fable-5-1" (pcPrices (catalogPlans cat)) `shouldBe` Just (Price 10 50 0.25 12.5 20)
    it "[plans.prices.<model>] overlays a row and adds new models" $
      withCatalog [("/proj/.olifant/config.toml", "[plans.prices.\"claude-sonnet-5-5\"]\ninput = 1.0\noutput = 2\ncache_read = 0.1\ncache_write_5m = 1.25\ncache_write_1h = 2.0\n[plans.prices.\"claude-x\"]\ninput = 3.0\noutput = 4.0\ncache_read = 0.3\ncache_write_5m = 3.75\ncache_write_1h = 6.0\n")] $ \_ cat -> do
        lookup "claude-sonnet-5-5" (pcPrices (catalogPlans cat)) `shouldBe` Just (Price 1 2 0.1 1.25 2)
        lookup "claude-x" (pcPrices (catalogPlans cat)) `shouldBe` Just (Price 3 4 0.3 3.75 6)
        lookup "claude-opus-5-5" (pcPrices (catalogPlans cat)) `shouldBe` Just (Price 4 20 0.2 5 8)
        length (pcPrices (catalogPlans cat)) `shouldBe` 5
    it "a price row missing a key is a config error naming it" $ do
      r <- openWith [("/proj/.olifant/config.toml", "[plans.prices.\"m\"]\ninput = 1.0\n")]
      case r of
        Left (ConfigInvalid _ e) -> e `shouldSatisfy` T.isInfixOf "cache_read"
        _ -> expectationFailure "expected ConfigInvalid naming cache_read"
    it "a price row with an unknown key or a non-number is a config error" $ do
      r <- openWith [("/proj/.olifant/config.toml", "[plans.prices.\"m\"]\ninput = 1.0\noutput = 2.0\ncache_read = 0.1\ncache_write_5m = 1.25\ncache_write_1h = 2.0\ncache_write = 1.0\n")]
      case r of
        Left (ConfigInvalid _ e) -> e `shouldSatisfy` T.isInfixOf "cache_write"
        _ -> expectationFailure "expected ConfigInvalid naming the unknown key"
      r' <- openWith [("/proj/.olifant/config.toml", "[plans.prices.\"m\"]\ninput = \"1\"\noutput = 2.0\ncache_read = 0.1\ncache_write_5m = 1.25\ncache_write_1h = 2.0\n")]
      case r' of
        Left (ConfigInvalid _ e) -> e `shouldSatisfy` T.isInfixOf "input"
        _ -> expectationFailure "expected ConfigInvalid naming input"
  describe "[migrate] (strangling a legacy codebase)" $ do
    it "reads legacy, base, switches, new_work_lines and count, with defaults" $ do
      withCatalog [("/proj/.olifant/config.toml", "[migrate]\nlegacy = [\"legacy/**\"]\n")] $ \_ cat ->
        catalogMigrate cat `shouldBe` Just (MigrateConfig ["legacy/**"] "main" Nothing 15 [])
      withCatalog [("/proj/.olifant/config.toml", "[migrate]\nlegacy = [\"old/**\"]\nbase = \"trunk\"\nswitches = \"src/app/switches.py\"\nnew_work_lines = 30\ncount = [\"function_item\"]\n")] $ \_ cat ->
        catalogMigrate cat `shouldBe` Just (MigrateConfig ["old/**"] "trunk" (Just "src/app/switches.py") 30 ["function_item"])
    it "is nothing without [migrate]" $
      withCatalog [("/proj/.olifant/config.toml", "")] $ \_ cat -> catalogMigrate cat `shouldBe` Nothing
    it "names an unknown key, and needs legacy" $ do
      bad <- openWith [("/proj/.olifant/config.toml", "[migrate]\nlegacy = [\"a/**\"]\ntypo = 1\n")]
      either describeCatalogError (const "") bad `shouldSatisfy` T.isInfixOf "typo"
      none <- openWith [("/proj/.olifant/config.toml", "[migrate]\nbase = \"main\"\n")]
      either describeCatalogError (const "") none `shouldSatisfy` T.isInfixOf "legacy"
  describe "languages: shipped, then packs, then the project (language-neutral m2)" $ do
    let gen l = "description = \"d\"\nlanguage = \"" <> l <> "\"\n"
        cfg = ("/proj/.olifant/config.toml", "[packs]\np = { path = \"/packs/p\" }\n")
    it "a generator's language is active, from the shipped file" $ do
      Right cat <- openWith [cfg, ("/packs/p/generators/g/generator.toml", gen "python")]
      map langName (catalogLanguages cat) `shouldBe` ["python"]
      map langLayers (catalogLanguages cat) `shouldBe` [["shipped"]]
    it "the config's languages are active" $ do
      Right cat <- openWith [("/proj/.olifant/config.toml", "languages = [\"rust\"]\n")]
      map langName (catalogLanguages cat) `shouldBe` ["rust"]
    it "one language per name, its layers in pack order" $ do
      Right cat <- openWith [("/proj/.olifant/config.toml", "[packs]\na = { path = \"/packs/a\" }\nb = { path = \"/packs/b\" }\n"), ("/packs/a/languages/python.toml", "indent = \"  \"\n"), ("/packs/b/languages/python.toml", "line_length = 100\n")]
      map langLayers (catalogLanguages cat) `shouldBe` [["shipped", "pack a", "pack b"]]
      map langIndent (catalogLanguages cat) `shouldBe` ["  "]
    it "a project's units provider wins over the shipped kinds, its origin the project, its root the project root (Review Focus 5, 2)" $ do
      Right cat <- openWith [cfg, ("/packs/p/generators/g/generator.toml", gen "python"), ("/packs/p/languages/python.toml", "anchors = { provider = [\"./tools/anchor.sh\"] }\n"), ("/proj/.olifant/languages/python.toml", "[unit]\nkinds = { provider = [\"sh\", \"tools/units.sh\"] }\n")]
      let [l] = catalogLanguages cat
      langUnitProvider l `shouldBe` Just (Provider ["sh", "tools/units.sh"] (Just "/proj"))
      langUnitKinds l `shouldBe` []
      originOf l "unit.kinds" `shouldBe` Just "project"
      langAnchorProvider l `shouldBe` Just (Provider ["./tools/anchor.sh"] (Just "/proj"))
      originOf l "anchors" `shouldBe` Just "pack p"
    it "a project's .olifant/queries/<grammar>/ file is the language's, over the shipped one (4b m2)" $ do
      Right cat <- openWith [("/proj/.olifant/config.toml", "languages = [\"rust\"]\n"), ("/proj/.olifant/queries/rust/imports.scm", "(extern_crate_declaration) @import\n")]
      map langQueries (catalogLanguages cat) `shouldBe` [[("imports.scm", "(extern_crate_declaration) @import\n", "project")]]
      catalogBrokenLanguages cat `shouldBe` []
    it "a project query that does not compile is a load error naming the file and byte" $ do
      Right cat <- openWith [("/proj/.olifant/config.toml", "languages = [\"rust\"]\n"), ("/proj/.olifant/queries/rust/imports.scm", "(no_such_node) @import\n")]
      map fst (catalogBrokenLanguages cat) `shouldBe` ["/proj/.olifant/queries/rust/imports.scm"]
      map snd (catalogBrokenLanguages cat) `shouldSatisfy` all (\e -> "imports.scm" `T.isInfixOf` e && "byte" `T.isInfixOf` e)
    it "reading config never fetches or builds a grammar: a project's git grammar is left for its first use" $ do
      Right cat <- openWith [("/proj/.olifant/config.toml", "languages = [\"trustprobe\"]\n"), ("/proj/.olifant/languages/trustprobe.toml", "files = [\"**/*.tp\"]\ncomment = \"#\"\ngrammar = { git = \"file:///nonexistent/trustprobe\", rev = \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\" }\n"), ("/proj/.olifant/queries/trustprobe/tags.scm", "(x) @name\n")]
      catalogBrokenLanguages cat `shouldBe` []
      settings <- grammarSettings
      doesDirectoryExist (gsCache settings </> "grammars" </> "src" </> "trustprobe-aaaaaaa") `shouldReturn` False
    it "a project query file nobody reads is a load error, not silence: a subfolder's, an unused grammar's (m2b M3)" $ do
      Right cat <- openWith [("/proj/.olifant/config.toml", "languages = [\"rust\"]\n"), ("/proj/.olifant/queries/rust/more/x.scm", "(identifier) @name\n"), ("/proj/.olifant/queries/python/imports.scm", "(import_statement) @import\n")]
      map fst (catalogBrokenLanguages cat) `shouldBe` ["/proj/.olifant/queries/rust/more/x.scm", "/proj/.olifant/queries/python"]
      map langQueries (catalogLanguages cat) `shouldBe` [[]]
    it "a project query file that is not UTF-8 is a load error naming it" $ do
      withSystemTempDirectory "olifant-queries" $ \dir -> do
        createDirectoryIfMissing True (dir </> ".olifant/queries/rust")
        BS.writeFile (dir </> ".olifant/config.toml") "languages = [\"rust\"]\n"
        BS.writeFile (dir </> ".olifant/queries/rust/imports.scm") (BS.pack [0xff, 0xfe, 0x28])
        Right cat <- openProjectCatalog (diskWorkspace dir) projectOnly {catalogProjectDir = dir}
        map fst (catalogBrokenLanguages cat) `shouldSatisfy` ((== 1) . length)
        map (T.isInfixOf "UTF-8" . snd) (catalogBrokenLanguages cat) `shouldBe` [True]
    it "`ignore` reaches the walk: a Rust project lists no target/ file, a Python-only one does (4b m2)" $ do
      let tree = [("/proj/target/x.rs", "fn x() {}\n"), ("/proj/src/a.rs", "fn a() {}\n")]
          listed langs = do
            (root, _) <- memoryWorkspace (("/proj/.olifant/config.toml", "languages = [" <> langs <> "]\n") : tree)
            Right cat <- openProjectCatalog root projectOnly
            listFiles (ignoring (catalogIgnores cat) (within "/proj" root)) "."
      listed "\"rust\"" >>= (`shouldSatisfy` (\ps -> "src/a.rs" `elem` ps && all (not . ("target/" `isPrefixOf`)) ps))
      listed "\"python\"" >>= (`shouldSatisfy` ("target/x.rs" `elem`))
    it "`ignore` reaches the disk walk: a non-git Rust project's target/ is never entered, even unreadable (m2b M7)" $
      withSystemTempDirectory "olifant-walk" $ \dir -> do
        createDirectoryIfMissing True (dir </> "target/deep")
        createDirectoryIfMissing True (dir </> "src")
        createDirectoryIfMissing True (dir </> ".olifant")
        BS.writeFile (dir </> ".olifant/config.toml") "languages = [\"rust\"]\n"
        BS.writeFile (dir </> "src/a.rs") "fn a() {}\n"
        BS.writeFile (dir </> "target/deep/x.rs") "fn x() {}\n"
        setPermissions (dir </> "target/deep") emptyPermissions
        Right cat <- openProjectCatalog (diskWorkspace dir) projectOnly {catalogProjectDir = dir}
        files <- listFiles (ignoring (catalogIgnores cat) (diskWorkspace dir)) "."
        setPermissions (dir </> "target/deep") (setOwnerReadable True (setOwnerWritable True (setOwnerExecutable True emptyPermissions)))
        files `shouldSatisfy` ("src/a.rs" `elem`)
        files `shouldSatisfy` all (not . ("target" `isPrefixOf`))
    it "a project's selection provider replaces the shipped [test.select] whole" $ do
      Right cat <- openWith [cfg, ("/packs/p/generators/g/generator.toml", gen "python"), ("/proj/.olifant/languages/python.toml", "[test]\nselect = { provider = [\"sh\", \"tools/select.sh\"] }\n")]
      let [l] = catalogLanguages cat
      (langTest l >>= testSelectProvider) `shouldBe` Just (Provider ["sh", "tools/select.sh"] (Just "/proj"))
      (langTest l >>= testSelect) `shouldBe` Nothing
    it "a project override wins" $ do
      Right cat <- openWith [cfg, ("/packs/p/generators/g/generator.toml", gen "python"), ("/proj/.olifant/languages/python.toml", "indent = \"  \"\n")]
      let [l] = catalogLanguages cat
      langIndent l `shouldBe` "  "
      originOf l "indent" `shouldBe` Just "project"
      langLayers l `shouldBe` ["shipped", "project"]
    it "a broken override is set aside with its file and key; the rest load (Review Focus 4)" $ do
      Right cat <- openWith [("/proj/.olifant/config.toml", "languages = [\"rust\"]\n"), ("/proj/.olifant/languages/python.toml", "bogus = 1\n")]
      map langName (catalogLanguages cat) `shouldBe` ["rust"]
      let [(path, why)] = catalogBrokenLanguages cat
      path `shouldBe` "/proj/.olifant/languages/python.toml"
      why `shouldSatisfy` T.isInfixOf "bogus"
    it "a broken key is blamed on the layer that holds it, not the last layer" $ do
      Right cat <- openWith [cfg, ("/packs/p/languages/python.toml", "bogus = 1\n"), ("/proj/.olifant/languages/python.toml", "indent = \"  \"\n")]
      map langName (catalogLanguages cat) `shouldBe` []
      let [(path, why)] = catalogBrokenLanguages cat
      path `shouldBe` "/packs/p/languages/python.toml"
      why `shouldSatisfy` T.isInfixOf "bogus"
    it "a layer that is not valid TOML sets the whole language aside, with that layer's path" $ do
      Right cat <- openWith [("/proj/.olifant/config.toml", "languages = [\"python\", \"rust\"]\n[packs]\np = { path = \"/packs/p\" }\n"), ("/packs/p/languages/python.toml", "tests_for = \"x\n")]
      map langName (catalogLanguages cat) `shouldBe` ["rust"]
      map fst (catalogBrokenLanguages cat) `shouldBe` ["/packs/p/languages/python.toml"]
    it "a pack's toolchain overrides the shipped preset" $ do
      Right cat <- openWith [cfg, ("/packs/p/languages/haskell.toml", ""), ("/packs/p/toolchains/hspec.toml", "[test]\ncommand = [\"x\"]\n")]
      map (fmap testCommand . langTest) (catalogLanguages cat) `shouldBe` [Just ["x"]]
    it "elm without its pack is broken, naming the grammar" $ do
      Right cat <- openWith [("/proj/.olifant/config.toml", "languages = [\"elm\"]\n")]
      map langName (catalogLanguages cat) `shouldBe` []
      map snd (catalogBrokenLanguages cat) `shouldSatisfy` any (T.isInfixOf "grammar")
    it "a generator naming elm without the elm pack: no elm, and nothing broken" $ do
      Right cat <- openWith [cfg, ("/packs/p/generators/g/generator.toml", gen "elm")]
      (map langName (catalogLanguages cat), catalogBrokenLanguages cat) `shouldBe` ([], [])
    it "a shipped language no layer gives a grammar is named as such, asked for or not; a pack's grammar takes it off" $ do
      Right asked <- openWith [("/proj/.olifant/config.toml", "languages = [\"elm\"]\n")]
      Right quiet <- openWith [("/proj/.olifant/config.toml", "")]
      Right given <- openWith [cfg, ("/packs/p/languages/elm.toml", "grammar = { source = \"grammar/elm\" }\n")]
      (catalogNoGrammar asked, catalogNoGrammar quiet, catalogNoGrammar given) `shouldBe` (["elm"], ["elm"], [])
    it "edits to the project's override and to a pack's toolchain show on the next open (Review Focus 5)" $ do
      (root, _) <- memoryWorkspace [cfg, ("/packs/p/languages/python.toml", ""), ("/packs/p/toolchains/pytest.toml", "[test]\ncommand = [\"a\"]\n")]
      cache <- newPackCache
      let open = openProjectCatalogCached cache root projectOnly
          seen = either (const []) (map (\l -> (langIndent l, fmap testCommand (langTest l))) . catalogLanguages)
      seen <$> open `shouldReturn` [("    ", Just ["a"])]
      writeText root "/proj/.olifant/languages/python.toml" "indent = \"  \"\n"
      writeText root "/packs/p/toolchains/pytest.toml" "[test]\ncommand = [\"b\"]\n"
      seen <$> open `shouldReturn` [("  ", Just ["b"])]
  describe "the pack cache (olifant lsp: many projects, packs parsed once)" $ do
    let gen desc = "description = \"" <> desc <> "\"\nlanguage = \"x\"\n[[edits]]\ncreate = \"a.x\"\ntext = \"a\"\n"
        config pack = "[packs]\npython-htmx = { path = \"" <> pack <> "\" }\n"
        files =
          [ ("/packs/a/generators/command/generator.toml", gen "the original")
          , ("/packs/fork/generators/command/generator.toml", gen "the fork")
          , ("/p1/.olifant/config.toml", config "/packs/a")
          , ("/p2/.olifant/config.toml", config "/packs/fork")
          , ("/p3/.olifant/config.toml", config "/packs/a") ]
        open cache root dir = openProjectCatalogCached cache root (CatalogPaths dir Nothing Nothing)
        described = either (const []) (map (genDescription . entryDefinition) . catalogEntries)
        -- how often a pack's generators were discovered (parsed)
        counting root = do
          n <- newIORef (0 :: Int)
          let ws = root {listFiles = \d -> (if "/generators" `isSuffixOf` d && "/packs/" `isPrefixOf` d then modifyIORef' n (+ 1) else pure ()) >> listFiles root d}
          pure (ws, readIORef n)
    it "keeps two copies of a pack apart: same name, different paths" $ do
      (root, _) <- memoryWorkspace files
      cache <- newPackCache
      described <$> open cache root "/p1" `shouldReturn` ["the original"]
      described <$> open cache root "/p2" `shouldReturn` ["the fork"]
    it "parses a pack once for every project that uses it" $ do
      (root0, _) <- memoryWorkspace files
      (root, parsed) <- counting root0
      cache <- newPackCache
      _ <- open cache root "/p1"
      _ <- open cache root "/p3"
      parsed `shouldReturn` 1
    it "parses it again when its files change" $ do
      (root0, _) <- memoryWorkspace files
      (root, parsed) <- counting root0
      cache <- newPackCache
      _ <- open cache root "/p1"
      writeText root0 "/packs/a/generators/command/generator.toml" (gen "changed")
      described <$> open cache root "/p3" `shouldReturn` ["changed"]
      parsed `shouldReturn` 2
    it "fingerprints a pack by its files' paths and contents" $ do
      (root, _) <- memoryWorkspace files
      a <- packFingerprint root "/packs/a"
      a' <- packFingerprint root "/packs/a"
      fork <- packFingerprint root "/packs/fork"
      (a == a', a == fork) `shouldBe` (True, False)
  describe "fragment exemplars" $ do
    let lang = "files = [\"**/*.x\"]\ncomment = \"--\"\ngrammar = \"x\"\n[targets.case_in]\nphrase = \"case in FUNCTION\"\ncontainer = { kind = \"c\" }\nitems = { kind = \"b\" }\nexample = \"end of case in update\"\n"
        gen matching = "description = \"d\"\nlanguage = \"x\"\n[inputs.page]\ntype = \"ident\"\n[[edits]]\ninsert = \"Main.x\"\nat = \"end of case in update\"\nfragment = { file = \"fixture:Main.x\", items_of = \"case in update\", matching = \"" <> matching <> "\" }\nbind = { page = \"sample_page\" }\n"
        -- The container, then two items; the second spans two lines, its body indented 12 (the node starts at column 8).
        branches = [Match (3, 8) (3, 20) "Home -> home" (0, 0), Match (5, 8) (6, 29) "( GotSamplePageMsg sub, SamplePage page ) ->\n            SamplePage.update sub" (0, 0)]
        fakeWith items = querying $ \_ rule _ -> pure . Right $
          if "\"all\"" `isInfixOf` BL.unpack (encode rule)
            then items
            else [Match (2, 4) (9, 29) "case" (0, 0)]
        load = loadWith branches
        loadWith items matching = do
          (root, _) <- memoryWorkspace [("/packs/p/languages/x.toml", lang), ("/packs/p/fixtures/app/Main.x", "x\n"), ("/packs/p/generators/g/generator.toml", gen matching)]
          Right cat <- openPackCatalog root [("p", "/packs/p")]
          either (pure . Left) (loadGenerator root (fakeWith items) cat) (findEntry cat "p/g")
    it "turns the one matching item into a template, dedented and renamed" $ do
      Right g <- load "GotSamplePageMsg"
      let [Insert _ _ src _] = genEdits g
          Right name = parseName "settings"
      render (M.fromList [("page", VName name)]) src `shouldBe` Right "( GotSettingsMsg sub, Settings page ) ->\n    Settings.update sub"
    it "refuses a fragment that matches no item, or several (Review Focus 3)" $ do
      none <- load "Nothing"
      either describeCatalogError (const "") none `shouldSatisfy` ("matched 0 items" `T.isInfixOf`)
      both <- load "."
      either describeCatalogError (const "") both `shouldSatisfy` ("matched 2 items" `T.isInfixOf`)
    it "refuses a fragment or exemplar path that leaves the fixture" $ do
      (root, _) <- memoryWorkspace [("/packs/p/languages/x.toml", lang), ("/packs/p/fixtures/app/Main.x", "x\n"), ("/packs/secret.x", "s\n"), ("/packs/p/generators/g/generator.toml", T.replace "fixture:Main.x" "fixture:../../../secret.x" (gen "GotSamplePageMsg")), ("/packs/p/generators/h/generator.toml", "description = \"d\"\nlanguage = \"x\"\n[inputs.page]\ntype = \"ident\"\n[[edits]]\ncreate = \"a.x\"\nexemplar = \"fixture:/packs/secret.x\"\nbind = { page = \"s\" }\n")]
      Right cat <- openPackCatalog root [("p", "/packs/p")]
      forM_ ["p/g", "p/h"] $ \g -> do
        let Right entry = findEntry cat g
        r <- loadGenerator root (fakeWith branches) cat entry
        either describeCatalogError (const "") r `shouldSatisfy` ("outside" `T.isInfixOf`)
    it "drops the CR of a CRLF file's lines" $ do
      Right g <- loadWith [Match (5, 8) (6, 29) "( GotSamplePageMsg sub, SamplePage page ) ->\r\n            SamplePage.update sub" (0, 0)] "GotSamplePageMsg"
      let [Insert _ _ src _] = genEdits g
          Right name = parseName "settings"
      render (M.fromList [("page", VName name)]) src `shouldBe` Right "( GotSettingsMsg sub, Settings page ) ->\n    Settings.update sub"
    it "dedents tab-indented lines by the node's column" $ do
      Right g <- loadWith [Match (5, 1) (7, 2) "fn sample_page() {\n\t\tbody();\n\t}" (0, 0)] "sample_page"
      let [Insert _ _ src _] = genEdits g
          Right name = parseName "settings"
      render (M.fromList [("page", VName name)]) src `shouldBe` Right "fn settings() {\n\tbody();\n}"
    it "rejects an invalid `matching` regex when the generator is read" $ do
      bad <- load "GotSamplePageMsg("
      either describeCatalogError (const "") bad `shouldSatisfy` ("`fragment.matching`" `T.isInfixOf`)
  it "loads regions from packs and the project, the project overriding by name" $ do
    (root, _) <-
      memoryWorkspace
        [ ("/packs/p/regions.toml", "[regions.mods]\nfile = \"a.cabal\"\nquery = { glob = \"src/**/*.hs\" }\nitem = \"{= dotted =}\"\n")
        , ("/packs/p/generators/g/generator.toml", "description = \"d\"\nlanguage = \"x\"\n")
        , ("/proj/.olifant/config.toml", "[packs]\np = { path = \"/packs/p\" }\n")
        , ("/proj/.olifant/regions.toml", "[regions.mods]\nfile = \"b.cabal\"\nquery = { glob = \"src/**/*.hs\" }\nitem = \"{= dotted =}\"\n[regions.pages]\nfile = \"Pages.elm\"\nwhole_file = true\nquery = { glob = \"src/Page/*.elm\" }\ntemplate = \"pages.dt\"\n")
        , ("/proj/.olifant/pages.dt", "{= for p in items =}{= p.stem =}\n{= end =}")
        ]
    Right cat <- openProjectCatalog root (CatalogPaths "/proj" Nothing Nothing)
    map (\r -> (regionName r, regionFile r)) (catalogRegions cat) `shouldBe` [("mods", "b.cabal"), ("pages", "Pages.elm")]
    catalogBrokenRegions cat `shouldBe` []
  describe "checkEntry (what list and show report as INVALID)" $ do
    let lang = "files = [\"**/*.x\"]\ncomment = \"--\"\ngrammar = \"x\"\n[targets.type]\nphrase = \"type NAME\"\ncontainer = { kind = \"t\" }\nitems = { kind = \"v\" }\nexample = \"end of type Msg\"\n"
        insertGen at = "description = \"d\"\nlanguage = \"x\"\n[inputs.p]\ntype = \"ident\"\n[[edits]]\ninsert = \"src/{= p =}.x\"\nat = \"" <> at <> "\"\ntext = \"| X\"\n"
        check gen = do
          (root, _) <- memoryWorkspace [("/packs/p/languages/x.toml", lang), ("/packs/p/generators/g/generator.toml", gen)]
          Right cat <- openPackCatalog root [("p", "/packs/p")]
          let Right entry = findEntry cat "p/g"
          checkEntry root noSyntax cat entry
    it "checks a `new`: it names a generator of the pack, its template only that generator's values" $ do
      let withNew name tpl = "description = \"d\"\nlanguage = \"x\"\n[inputs.port]\ntype = \"enum\"\nchoices = [\"a\"]\nnew = { " <> name <> " = \"" <> tpl <> "\" }\n"
          checkTwo gen = do
            (root, _) <- memoryWorkspace [("/packs/p/languages/x.toml", lang), ("/packs/p/generators/g/generator.toml", gen), ("/packs/p/generators/b/generator.toml", "description = \"d\"\nlanguage = \"x\"\n[inputs.port]\ntype = \"ident\"\n[derive]\nfull = \"{= port =}_x\"\n")]
            Right cat <- openPackCatalog root [("p", "/packs/p")]
            let Right entry = findEntry cat "p/g"
            checkEntry root noSyntax cat entry
      checkTwo (withNew "nope" "{= port =}") >>= (`shouldSatisfy` maybe False ("new names no generator `nope`" `T.isInfixOf`))
      checkTwo (withNew "b" "{= nothing =}") >>= (`shouldSatisfy` maybe False ("{= nothing =} is not an input or derived value of b" `T.isInfixOf`))
      checkTwo (withNew "b" "{= full =}") `shouldReturn` Nothing
    it "reports a missing template file" $
      check "description = \"d\"\nlanguage = \"x\"\n[[edits]]\ncreate = \"a.x\"\ntemplate = \"nope.x.dt\"\n" >>= (`shouldSatisfy` maybe False ("nope.x.dt" `T.isInfixOf`))
    it "reports a fixed `at` target the file's adapter does not have, with a suggestion" $
      check (insertGen "end of typo Msg") >>= (`shouldSatisfy` maybe False (\e -> "unknown target `typo Msg`" `T.isInfixOf` e && "end of type Msg" `T.isInfixOf` e))
    it "accepts known targets, and leaves targets with variables to run time" $ do
      check (insertGen "end of type Msg") `shouldReturn` Nothing
      check (insertGen "end of typo {= p =}") `shouldReturn` Nothing
      check (insertGen "end of file") `shouldReturn` Nothing
  it "is an error when a configured pack path does not exist" $ do
    (root, _) <- memoryWorkspace [("/packs/p/generators/g/generator.toml", "description = \"d\"\nlanguage = \"x\"\n")]
    r <- openPackCatalog root [("p", "/packs/p"), ("q", "/packs/typo")]
    either describeCatalogError (const "") r `shouldSatisfy` ("pack `q`" `T.isInfixOf`)
  it "keeps a broken regions.toml aside" $ do
    (root, _) <- memoryWorkspace [("/packs/p/regions.toml", "[regions.x]\nfile = 1\n"), ("/packs/p/generators/g/generator.toml", "description = \"d\"\nlanguage = \"x\"\n")]
    Right cat <- openPackCatalog root [("p", "/packs/p")]
    map fst (catalogBrokenRegions cat) `shouldBe` ["/packs/p/regions.toml"]
  it "loads a pack's language adapters and keeps broken ones aside" $ do
    (root, _) <-
      memoryWorkspace
        [ ("/packs/elm/languages/elm.toml", "files = [\"**/*.elm\"]\ncomment = \"--\"\ngrammar = { source = \"grammar/elm\" }\n")
        , ("/packs/elm/languages/bad.toml", "files = 1\n")
        , ("/packs/elm/languages/grammar/elm/parser.c", "/* not an adapter */\n")
        , ("/packs/elm/generators/g/generator.toml", "description = \"d\"\nlanguage = \"elm\"\n")
        ]
    Right cat <- openPackCatalog root [("elm", "/packs/elm")]
    map langName (catalogLanguages cat) `shouldBe` ["elm"]
    map fst (catalogBrokenLanguages cat) `shouldBe` ["/packs/elm/languages/bad.toml"]
  it "lists local and pack generators, sorted by id" $
    withCatalog files $ \_ cat ->
      map entryId (catalogEntries cat) `shouldBe` map GeneratorId ["model", "rails/broken", "rails/cell", "rails/model"]
  it "resolves a bare name to the local generator first (shadowing), and pack/name to the pack" $
    withCatalog files $ \_ cat -> do
      fmap entryId (findEntry cat "model") `shouldBe` Right (GeneratorId "model")
      fmap entryId (findEntry cat "rails/model") `shouldBe` Right (GeneratorId "rails/model")
  it "resolves a bare name to a unique pack generator" $
    withCatalog files $ \_ cat -> fmap entryId (findEntry cat "cell") `shouldBe` Right (GeneratorId "rails/cell")
  it "reports unknown generators with what is available" $
    withCatalog files $ \_ cat ->
      findEntry cat "nope" `shouldSatisfy` \case
        Left (GeneratorNotFound "nope" available) -> GeneratorId "rails/model" `elem` available
        _ -> False
  it "loads template files from the generator's templates/ dir" $
    withCatalog files $ \root cat ->
      genInto root cat "rails/model" `shouldReturn` M.fromList [("app/models/cancel_subscription.rb", "class CancelSubscription\nend\n")]
  it "loads fixture: exemplars from the pack's fixture app" $
    withCatalog files $ \root cat ->
      genInto root cat "rails/cell" `shouldReturn` M.fromList [("src/cancel_subscription.ts", "export const CancelSubscription = 'cancel_subscriptions'\n")]
  it "reports a missing template file" $
    withCatalog files $ \root cat -> do
      let Right entry = findEntry cat "rails/broken"
      loadGenerator root noSyntax cat entry >>= \case
        Left err -> err `shouldBe` SourceMissing (GeneratorId "rails/broken") "/packs/rails/generators/broken/templates/missing.dt"
        Right _ -> expectationFailure "expected a missing template"
  it "works without a config file (local generators only)" $
    withCatalog (drop 1 files) $ \_ cat -> map entryId (catalogEntries cat) `shouldBe` [GeneratorId "model"]
  it "reports an invalid config" $ do
    (root, _) <- memoryWorkspace [("/proj/.olifant/config.toml", "[packs]\nrails = { git = \"x\" }\n")]
    openProjectCatalog root projectOnly >>= \case
      Left err -> err `shouldBe` ConfigInvalid "/proj/.olifant/config.toml" "`packs`: `rails`: unknown key(s) `git`; expected one of `path`"
      Right _ -> expectationFailure "expected an invalid config"

  it "isolates a broken generator.toml: others still list and load" $ do
    let broken = ("/proj/.olifant/generators/two/generator.toml", "description = \"two\"\nlanguage = \"x\"\ntemplete = \"oops\"\n")
    withCatalog (broken : files) $ \root cat -> do
      map entryId (catalogEntries cat) `shouldBe` map GeneratorId ["model", "rails/broken", "rails/cell", "rails/model"]
      map (\(gid, _, _) -> gid) (catalogBroken cat) `shouldBe` [GeneratorId "two"]
      genInto root cat "rails/model" `shouldReturn` M.fromList [("app/models/cancel_subscription.rb", "class CancelSubscription\nend\n")]
      findEntry cat "two" `shouldSatisfy` \case
        Left (GeneratorInvalid _ msg) -> "templete" `T.isInfixOf` msg
        _ -> False

  describe "where packs come from" $ do
    let gen desc = "description = \"" <> desc <> "\"\nlanguage = \"x\"\n[[edits]]\ncreate = \"a.txt\"\ntext = \"a\"\n"
        packIds cat = map entryId (catalogEntries cat)
        descriptions cat = [(entryId e, genDescription (entryDefinition e)) | e <- catalogEntries cat]
    it "expands ~ in pack paths" $
      withCatalogAt
        withUser
        [ ("/proj/.olifant/config.toml", "[packs]\nelm = { path = \"~/packs/elm\" }\n")
        , ("/home/u/packs/elm/generators/page/generator.toml", gen "page")
        ]
        $ \_ cat -> packIds cat `shouldBe` [GeneratorId "elm/page"]
    it "adds packs from the user config (relative to it), and the project config wins on a clash" $
      withCatalogAt
        withUser
        [ ("/home/u/.config/olifant/config.toml", "[packs]\nelm = { path = \"packs/elm\" }\nrails = { path = \"~/user-rails\" }\n")
        , ("/home/u/.config/olifant/packs/elm/generators/page/generator.toml", gen "user elm page")
        , ("/home/u/user-rails/generators/model/generator.toml", gen "user rails model")
        , ("/proj/.olifant/config.toml", "[packs]\nrails = { path = \"../packs/rails\" }\n")
        , ("/packs/rails/generators/model/generator.toml", gen "project rails model")
        ]
        $ \_ cat ->
          descriptions cat `shouldBe` [(GeneratorId "elm/page", "user elm page"), (GeneratorId "rails/model", "project rails model")]
    it "says when a pack is not configured, and how to configure it" $
      withCatalog files $ \_ cat -> case findEntry cat "elm/page" of
        Left err@(PackNotConfigured "elm" "elm/page") -> do
          describeCatalogError err `shouldSatisfy` T.isInfixOf "no pack named `elm` is configured"
          describeCatalogError err `shouldSatisfy` T.isInfixOf "[packs]"
        other -> expectationFailure ("expected PackNotConfigured, got " <> show (fmap entryId other))
  it "resolves [[uses]] refs pack-locally first, then as written" $ do
    let fs =
          [ ("/proj/.olifant/config.toml", "[packs]\nelm = { path = \"../packs/elm\" }\n")
          , ("/proj/.olifant/generators/msg/generator.toml", "description = \"local msg\"\nlanguage = \"x\"\n")
          , ("/packs/elm/generators/msg/generator.toml", "description = \"elm msg\"\nlanguage = \"x\"\n")
          ]
    withCatalog fs $ \root cat -> do
      fmap (fmap genDescription) (resolver root noSyntax cat (Just "elm") "msg") `shouldReturn` Right "elm msg"
      fmap (fmap genDescription) (resolver root noSyntax cat Nothing "msg") `shouldReturn` Right "local msg"
      fmap (either (const "err") genDescription) (resolver root noSyntax cat (Just "elm") "nope") `shouldReturn` "err"
  it "does not fall through to another generator when the pack-local one is broken" $ do
    let fs =
          [ ("/proj/.olifant/config.toml", "[packs]\nelm = { path = \"../packs/elm\" }\n")
          , ("/proj/.olifant/generators/msg/generator.toml", "description = \"local msg\"\nlanguage = \"x\"\n")
          , ("/packs/elm/generators/msg/generator.toml", "description = \"broken\"\nlanguage = \"x\"\nbogus = 1\n")
          ]
    withCatalog fs $ \root cat -> do
      r <- resolver root noSyntax cat (Just "elm") "msg"
      r `shouldSatisfy` \case
        Left (ResolveInvalid msg) -> "bogus" `T.isInfixOf` msg
        _ -> False
      r2 <- resolver root noSyntax cat (Just "elm") "nope"
      r2 `shouldSatisfy` \case
        Left (ResolveNotFound _) -> True
        _ -> False
