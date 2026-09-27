{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.GenerationSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.Generation
import Olifant.Generator
import Olifant.Template (Template)
import Olifant.Workspace (memoryWorkspace)
import Test.Hspec

now :: UTCTime
now = UTCTime (fromGregorian 2026 9 27) 0

-- | Build a generator from TOML whose edits use inline `text` only.
generatorFrom :: Text -> Generator
generatorFrom src = either (error . T.unpack) (fmap inlineOnly) (parseGenerator (GeneratorId "test/gen") src)
  where
    inlineOnly :: SourceRef -> Template
    inlineOnly (InlineText t) = t
    inlineOnly other = error ("test generators use inline text only: " <> show other)

header :: Text
header = "description = \"d\"\nlanguage = \"x\"\n[inputs.name]\ntype = \"ident\"\n"

createGen :: Generator
createGen =
  generatorFrom . (header <>) . T.unlines $
    [ "[[edits]]"
    , "create = \"app/{{ name|snake }}.rb\""
    , "text = \"class {{ name|pascal }}\\nend\\n\""
    , "[[edits]]"
    , "create = \"test/{{ name|snake }}_test.rb\""
    , "text = \"test {{ name|pascal }}\\n\""
    ]

run :: [(FilePath, Text)] -> Generator -> [(Text, Text)] -> Mode -> IO (Either GenerationError Plan, M.Map FilePath Text)
run files gen sets mode = do
  (ws, snapshot) <- memoryWorkspace files
  result <- generate ws gen (M.fromList sets) now mode
  (result,) <$> snapshot

spec :: Spec
spec = describe "generate (create edits)" $ do
  it "creates rendered files when applied" $ do
    (Right plan, files) <- run [] createGen [("name", "blog_post")] Apply
    files `shouldBe` M.fromList [("app/blog_post.rb", "class BlogPost\nend\n"), ("test/blog_post_test.rb", "test BlogPost\n")]
    map editAction (planEdits plan) `shouldBe` [WillCreate "class BlogPost\nend\n", WillCreate "test BlogPost\n"]
  it "writes nothing in plan-only mode, but plans the same edits" $ do
    (Right plan, files) <- run [] createGen [("name", "blog_post")] PlanOnly
    files `shouldBe` M.empty
    M.keys (planWrites plan) `shouldBe` ["app/blog_post.rb", "test/blog_post_test.rb"]
  it "is all-or-nothing: a conflict means nothing is written" $ do
    (Right plan, files) <- run [("test/blog_post_test.rb", "mine\n")] createGen [("name", "blog_post")] Apply
    planConflicts plan `shouldBe` [TargetExists 2 "test/blog_post_test.rb"]
    files `shouldBe` M.fromList [("test/blog_post_test.rb", "mine\n")]
  it "treats an identical existing file as a no-op (re-running is safe)" $ do
    (Right plan, _) <- run [("app/post.rb", "class Post\nend\n"), ("test/post_test.rb", "test Post\n")] createGen [("name", "post")] Apply
    map editAction (planEdits plan) `shouldBe` [Skipped UnchangedContent, Skipped UnchangedContent]
    planWrites plan `shouldBe` M.empty
  it "honours on_exists = skip and overwrite" $ do
    let gen mode' = generatorFrom (header <> "[[edits]]\ncreate = \"a.txt\"\ntext = \"new\"\non_exists = \"" <> mode' <> "\"\n")
    (Right skipped, _) <- run [("a.txt", "old")] (gen "skip") [("name", "x_y")] Apply
    map editAction (planEdits skipped) `shouldBe` [Skipped AlreadyExists]
    (_, files) <- run [("a.txt", "old")] (gen "overwrite") [("name", "x_y")] Apply
    M.lookup "a.txt" files `shouldBe` Just "new"
  it "refuses paths outside the project and writes nothing" $ do
    let gen = generatorFrom "description = \"d\"\nlanguage = \"x\"\n[inputs.dir]\ntype = \"text\"\n[[edits]]\ncreate = \"{{ dir }}/x.txt\"\ntext = \"x\"\n"
    (Right upward, files1) <- run [] gen [("dir", "../../etc")] Apply
    planConflicts upward `shouldBe` [UnsafePath 1 "../../etc/x.txt"]
    files1 `shouldBe` M.empty
    (Right absolute, files2) <- run [] gen [("dir", "/etc")] Apply
    planConflicts absolute `shouldBe` [UnsafePath 1 "/etc/x.txt"]
    files2 `shouldBe` M.empty
  it "reports unmet requirements with the rendered hint" $ do
    let gen = generatorFrom (header <> "[requires]\nexists = [\"{{ name|snake }}/service.rs\"]\nhint = \"run service-skeleton for {{ name }}\"\n[[edits]]\ncreate = \"a.txt\"\ntext = \"x\"\n")
    (Right plan, files) <- run [] gen [("name", "billing")] Apply
    planConflicts plan `shouldBe` [RequirementUnmet ["billing/service.rs"] (Just "run service-skeleton for billing")]
    files `shouldBe` M.empty
  it "returns input problems instead of planning" $ do
    (result, _) <- run [] createGen [] Apply
    result `shouldSatisfy` \case
      Left (InvalidInputs (InputProblems [MissingInput _])) -> True
      _ -> False
