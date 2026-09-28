{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.CatalogSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.Catalog
import Olifant.Generation
import Olifant.Generator (GeneratorId (..), GeneratorWith (..))
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
  Right gen <- loadGenerator root cat entry
  Right plan <- generate (within "/proj" root) gen (M.fromList [("name", "cancel_subscription")]) now PlanOnly
  pure (planWrites plan)

spec :: Spec
spec = describe "catalog" $ do
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
      loadGenerator root cat entry >>= \case
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
