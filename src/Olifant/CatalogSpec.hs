{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.CatalogSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.Catalog
import Olifant.Generation
import Olifant.Generator (GeneratorId (..))
import Olifant.Workspace
import Test.Hspec

files :: [(FilePath, Text)]
files =
  [ ("/proj/.olifant/config.toml", "[packs]\nrails = { path = \"../packs/rails\" }\n")
  , ("/proj/.olifant/generators/model/generator.toml", "description = \"local model\"\nlanguage = \"ruby\"\n[inputs.name]\ntype = \"ident\"\n[[edits]]\ncreate = \"local/{{ name|snake }}.rb\"\ntext = \"local\"\n")
  , ("/packs/rails/generators/model/generator.toml", "description = \"pack model\"\nlanguage = \"ruby\"\n[inputs.name]\ntype = \"ident\"\n[[edits]]\ncreate = \"app/models/{{ name|snake }}.rb\"\ntemplate = \"model.rb.dt\"\n")
  , ("/packs/rails/generators/model/templates/model.rb.dt", "class {{ name|pascal }}\nend\n")
  , ("/packs/rails/generators/cell/generator.toml", "description = \"cell\"\nlanguage = \"ts\"\n[inputs.name]\ntype = \"ident\"\n[[edits]]\ncreate = \"src/{{ name|snake }}.ts\"\nexemplar = \"fixture:src/blog_post.ts\"\nbind = { name = \"blog_post\" }\n")
  , ("/packs/rails/fixtures/app/src/blog_post.ts", "export const BlogPost = 'blog_posts'\n")
  , ("/packs/rails/generators/broken/generator.toml", "description = \"broken\"\nlanguage = \"x\"\n[[edits]]\ncreate = \"a\"\ntemplate = \"missing.dt\"\n")
  ]

now :: UTCTime
now = UTCTime (fromGregorian 2026 9 27) 0

withCatalog :: [(FilePath, Text)] -> (Workspace IO -> Catalog -> IO a) -> IO a
withCatalog fs k = do
  (root, _) <- memoryWorkspace fs
  openProjectCatalog root "/proj" >>= \case
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
    openProjectCatalog root "/proj" >>= \case
      Left err -> err `shouldBe` ConfigInvalid "`packs`: `rails`: unknown key(s) `git`; expected one of `path`"
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
