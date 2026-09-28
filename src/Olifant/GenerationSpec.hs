{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.GenerationSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Hedgehog (evalIO, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Olifant.Generation
import Olifant.Generator
import Olifant.Template (Template)
import Data.ByteString qualified as BS
import Olifant.Workspace (diskWorkspace, memoryWorkspace)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

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
    , "create = \"app/{= name|snake =}.rb\""
    , "text = \"class {= name|pascal =}\\nend\\n\""
    , "[[edits]]"
    , "create = \"test/{= name|snake =}_test.rb\""
    , "text = \"test {= name|pascal =}\\n\""
    ]

run :: [(FilePath, Text)] -> Generator -> [(Text, Text)] -> Mode -> IO (Either GenerationError Plan, M.Map FilePath Text)
run files gen sets mode = do
  (ws, snapshot) <- memoryWorkspace files
  result <- generate ws gen (M.fromList sets) now mode
  (result,) <$> snapshot

spec :: Spec
spec = do
  createSpec
  insertSpec
  unusableSpec

createSpec :: Spec
createSpec = describe "generate (create edits)" $ do
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
    let gen = generatorFrom "description = \"d\"\nlanguage = \"x\"\n[inputs.dir]\ntype = \"text\"\n[[edits]]\ncreate = \"{= dir =}/x.txt\"\ntext = \"x\"\n"
    (Right upward, files1) <- run [] gen [("dir", "../../etc")] Apply
    planConflicts upward `shouldBe` [UnsafePath 1 "../../etc/x.txt"]
    files1 `shouldBe` M.empty
    (Right absolute, files2) <- run [] gen [("dir", "/etc")] Apply
    planConflicts absolute `shouldBe` [UnsafePath 1 "/etc/x.txt"]
    files2 `shouldBe` M.empty
  it "reports unmet requirements with the rendered hint" $ do
    let gen = generatorFrom (header <> "[requires]\nexists = [\"{= name|snake =}/service.rs\"]\nhint = \"run service-skeleton for {= name =}\"\n[[edits]]\ncreate = \"a.txt\"\ntext = \"x\"\n")
    (Right plan, files) <- run [] gen [("name", "billing")] Apply
    planConflicts plan `shouldBe` [RequirementUnmet ["billing/service.rs"] (Just "run service-skeleton for billing")]
    files `shouldBe` M.empty
  it "returns input problems instead of planning" $ do
    (result, _) <- run [] createGen [] Apply
    result `shouldSatisfy` \case
      Left (InvalidInputs (InputProblems [MissingInput _])) -> True
      _ -> False

insertSpec :: Spec
insertSpec = describe "generate (marker inserts)" $ do
  let insertGen =
        generatorFrom . T.unlines $
          [ "description = \"d\""
          , "language = \"rust\""
          , "[inputs.cmd]"
          , "type = \"ident\""
          , "[[edits]]"
          , "insert = \"svc/lib.rs\""
          , "anchor = { marker = \"// olifant:modules\" }"
          , "text = \"pub mod {= cmd|snake =};\""
          , "[[edits]]"
          , "insert = \"svc/service.rs\""
          , "anchor = { marker = \"// olifant:delegates\" }"
          , "text = \"\\npub fn {= cmd|snake =}(&self) {\\n    todo!()\\n}\\n\""
          ]
      lib = "pub mod a;\n// olifant:modules\n"
      service = "impl S {\n    pub fn a(&self) {}\n    // olifant:delegates\n}\n"
  it "inserts before the marker, using its indentation, keeping blank lines unindented" $ do
    (Right plan, files) <- run [("svc/lib.rs", lib), ("svc/service.rs", service)] insertGen [("cmd", "refund_invoice")] Apply
    M.lookup "svc/lib.rs" files `shouldBe` Just "pub mod a;\npub mod refund_invoice;\n// olifant:modules\n"
    M.lookup "svc/service.rs" files
      `shouldBe` Just "impl S {\n    pub fn a(&self) {}\n\n    pub fn refund_invoice(&self) {\n        todo!()\n    }\n    // olifant:delegates\n}\n"
    map editAction (planEdits plan) `shouldBe` [WillInsert 2 "pub mod refund_invoice;", WillInsert 3 "\npub fn refund_invoice(&self) {\n    todo!()\n}\n"]
  it "keeps earlier inserts in order (the marker stays last)" $ do
    (ws, snapshot) <- memoryWorkspace [("svc/lib.rs", lib), ("svc/service.rs", service)]
    _ <- generate ws insertGen (M.fromList [("cmd", "b_b")]) now Apply
    _ <- generate ws insertGen (M.fromList [("cmd", "c_c")]) now Apply
    M.lookup "svc/lib.rs" <$> snapshot `shouldReturn` Just "pub mod a;\npub mod b_b;\npub mod c_c;\n// olifant:modules\n"
  it "skips text that is already present, so re-running is a no-op" $ do
    (ws, _) <- memoryWorkspace [("svc/lib.rs", lib), ("svc/service.rs", service)]
    _ <- generate ws insertGen (M.fromList [("cmd", "b_b")]) now Apply
    Right again <- generate ws insertGen (M.fromList [("cmd", "b_b")]) now Apply
    map editAction (planEdits again) `shouldBe` [Skipped AlreadyPresent, Skipped AlreadyPresent]
    planWrites again `shouldBe` M.empty
  it "does not mistake a longer identifier for the snippet (token-bounded already-present check)" $ do
    let modGen = generatorFrom "description = \"d\"\nlanguage = \"cabal\"\n[inputs.m]\ntype = \"ident\"\n[[edits]]\ninsert = \"x.cabal\"\nanchor = { marker = \"-- olifant:modules\" }\ntext = \"Olifant.{= m|pascal =}\"\n"
        cabal = "    Olifant.Generation\n    -- olifant:modules\n"
    (_, files) <- run [("x.cabal", cabal)] modGen [("m", "gen")] Apply
    M.lookup "x.cabal" files `shouldBe` Just "    Olifant.Generation\n    Olifant.Gen\n    -- olifant:modules\n"
    (Right again, _) <- run [("x.cabal", "    Olifant.Gen\n    -- olifant:modules\n")] modGen [("m", "gen")] Apply
    map editAction (planEdits again) `shouldBe` [Skipped AlreadyPresent]
  it "checks already-present only within the marker's block, so one line can go into two lists" $ do
    let twoLists = generatorFrom "description = \"d\"\nlanguage = \"cabal\"\n[inputs.m]\ntype = \"ident\"\n[[edits]]\ninsert = \"x.cabal\"\nanchor = { marker = \"-- olifant:lib\" }\ntext = \"Olifant.{= m|pascal =}\"\n[[edits]]\ninsert = \"x.cabal\"\nanchor = { marker = \"-- olifant:test\" }\ntext = \"Olifant.{= m|pascal =}\"\n"
        cabal = "library\n  exposed-modules:\n    Olifant.A\n    -- olifant:lib\n\ntest-suite spec\n  other-modules:\n    Olifant.A\n    -- olifant:test\n"
    (_, files) <- run [("x.cabal", cabal)] twoLists [("m", "b_b")] Apply
    M.lookup "x.cabal" files
      `shouldBe` Just "library\n  exposed-modules:\n    Olifant.A\n    Olifant.BB\n    -- olifant:lib\n\ntest-suite spec\n  other-modules:\n    Olifant.A\n    Olifant.BB\n    -- olifant:test\n"
  it "reports a missing or repeated marker, and a missing file, as conflicts" $ do
    (Right plan, files) <- run [("svc/lib.rs", "pub mod a;\n"), ("svc/service.rs", service <> "// olifant:delegates\n")] insertGen [("cmd", "b_b")] Apply
    planConflicts plan `shouldBe` [MarkerNotFound 1 "svc/lib.rs" "// olifant:modules", MarkerRepeated 2 "svc/service.rs" "// olifant:delegates" 2]
    M.lookup "svc/lib.rs" files `shouldBe` Just "pub mod a;\n"
    (Right plan2, _) <- run [] insertGen [("cmd", "b_b")] Apply
    planConflicts plan2 `shouldBe` [InsertTargetMissing 1 "svc/lib.rs", InsertTargetMissing 2 "svc/service.rs"]
  it "preserves CRLF line endings and a missing final newline" $ do
    (_, files) <- run [("svc/lib.rs", "pub mod a;\r\n// olifant:modules"), ("svc/service.rs", service)] insertGen [("cmd", "b_b")] Apply
    M.lookup "svc/lib.rs" files `shouldBe` Just "pub mod a;\r\npub mod b_b;\r\n// olifant:modules"
  it "re-running with any command name changes nothing (property)" $ hedgehog $ do
    ws' <- forAll (Gen.list (Range.linear 2 3) (Gen.text (Range.linear 2 6) Gen.lower))
    let sets = M.fromList [("cmd", T.intercalate "_" ws')]
    (ws, _) <- evalIO (memoryWorkspace [("svc/lib.rs", lib), ("svc/service.rs", service)])
    _ <- evalIO (generate ws insertGen sets now Apply)
    second <- evalIO (generate ws insertGen sets now Apply)
    fmap planWrites second === Right M.empty

unusableSpec :: Spec
unusableSpec = describe "generate (targets that cannot be written)" $ do
  let pathGen = generatorFrom "description = \"d\"\nlanguage = \"x\"\n[inputs.dir]\ntype = \"text\"\n[[edits]]\ncreate = \"a.txt\"\ntext = \"a\"\n[[edits]]\ncreate = \"{= dir =}\"\ntext = \"X\\n\"\n"
      isUnusable n p = \case
        TargetUnusable n' p' _ -> n == n' && p == p'
        _ -> False
  it "never overwrites an existing file it cannot read as text (all-or-nothing)" $
    withSystemTempDirectory "olifant" $ \dir -> do
      BS.writeFile (dir </> "mine.txt") "caf\xe9 precious\n"
      Right plan <- generate (diskWorkspace dir) pathGen (M.fromList [("dir", "mine.txt")]) now Apply
      planConflicts plan `shouldSatisfy` \case [c] -> isUnusable 2 "mine.txt" c; _ -> False
      BS.readFile (dir </> "mine.txt") `shouldReturn` "caf\xe9 precious\n"
      BS.readFile (dir </> "a.txt") `shouldThrow` anyIOException
  it "reports a directory target as a conflict before writing anything" $ do
    (Right plan, files) <- run [("src/m.rs", "")] pathGen [("dir", "src")] Apply
    planConflicts plan `shouldSatisfy` \case [c] -> isUnusable 2 "src" c; _ -> False
    M.member "a.txt" files `shouldBe` False
  it "reports a file standing where a parent directory must be" $ do
    (Right plan, files) <- run [("z", "x")] pathGen [("dir", "z/x.txt")] Apply
    planConflicts plan `shouldSatisfy` \case [c] -> isUnusable 2 "z/x.txt" c; _ -> False
    M.member "a.txt" files `shouldBe` False
