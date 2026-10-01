{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.CatalogSpec (spec) where

import Control.Monad (forM_)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.Catalog
import Olifant.Generation
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.List (isInfixOf)
import Olifant.Generator (EditWith (..), GeneratorId (..), GeneratorWith (..))
import Olifant.Naming (parseName)
import Olifant.Template (Value (..), render)
import Olifant.Language (Language (..), Match (..), Syntax (..), noSyntax)
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
  describe "fragment exemplars" $ do
    let lang = "files = [\"**/*.x\"]\ncomment = \"--\"\ngrammar = \"x\"\n[targets.case_in]\nphrase = \"case in FUNCTION\"\ncontainer = { kind = \"c\" }\nitems = { kind = \"b\" }\nexample = \"end of case in update\"\n"
        gen matching = "description = \"d\"\nlanguage = \"x\"\n[inputs.page]\ntype = \"ident\"\n[[edits]]\ninsert = \"Main.x\"\nat = \"end of case in update\"\nfragment = { file = \"fixture:Main.x\", items_of = \"case in update\", matching = \"" <> matching <> "\" }\nbind = { page = \"sample_page\" }\n"
        -- The container, then two items; the second spans two lines, its body indented 12 (the node starts at column 8).
        branches = [Match (3, 8) (3, 20) "Home -> home", Match (5, 8) (6, 29) "( GotSamplePageMsg sub, SamplePage page ) ->\n            SamplePage.update sub"]
        fakeWith items = Syntax $ \_ rule _ -> pure . Right $
          if "\"all\"" `isInfixOf` BL.unpack (encode rule)
            then items
            else [Match (2, 4) (9, 29) "case"]
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
      Right g <- loadWith [Match (5, 8) (6, 29) "( GotSamplePageMsg sub, SamplePage page ) ->\r\n            SamplePage.update sub"] "GotSamplePageMsg"
      let [Insert _ _ src _] = genEdits g
          Right name = parseName "settings"
      render (M.fromList [("page", VName name)]) src `shouldBe` Right "( GotSettingsMsg sub, Settings page ) ->\n    Settings.update sub"
    it "dedents tab-indented lines by the node's column" $ do
      Right g <- loadWith [Match (5, 1) (7, 2) "fn sample_page() {\n\t\tbody();\n\t}"] "sample_page"
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
    it "reports a missing template file" $
      check "description = \"d\"\nlanguage = \"x\"\n[[edits]]\ncreate = \"a.x\"\ntemplate = \"nope.x.dt\"\n" >>= (`shouldSatisfy` maybe False ("nope.x.dt" `T.isInfixOf`))
    it "reports a fixed `at` target the file's adapter does not have, with a suggestion" $
      check (insertGen "end of typo Msg") >>= (`shouldSatisfy` maybe False (\e -> "unknown target `typo Msg`" `T.isInfixOf` e && "end of type Msg" `T.isInfixOf` e))
    it "accepts known targets, and leaves targets with variables to run time" $ do
      check (insertGen "end of type Msg") `shouldReturn` Nothing
      check (insertGen "end of typo {= p =}") `shouldReturn` Nothing
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
