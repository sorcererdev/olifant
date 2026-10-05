{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.GenerationSpec (spec) where

import Data.Map.Strict qualified as M
import Data.List (isInfixOf)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Aeson (encode, object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Hedgehog (evalIO, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Olifant.Anchor (astQueries)
import Olifant.Generation
import Olifant.Holes (HoleSite (..))
import Olifant.Fill (FillHole (..), FillRequest (..), Filler (..), ModelFiller (..))
import Olifant.Generation.Complete (insertBranch)
import Olifant.Generator
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Olifant.Language
import Olifant.Query (AstQuery (..), AstSelect (..), Query (..), Queries (..), QueryFailure (..), failed, noQueries)
import Olifant.Region
import Olifant.Template (Template, Value (..), parseTemplate)
import Data.ByteString qualified as BS
import Olifant.Workspace (Workspace (..), diskWorkspace, memoryWorkspace)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

now :: UTCTime
now = UTCTime (fromGregorian 2026 9 27) 0

-- | Build a generator from TOML whose edits use inline `text` only.
generatorFrom :: Text -> Generator
generatorFrom = generatorNamed "test/gen"

-- | Like 'generatorFrom', with a chosen id (composition tests need distinct ids).
generatorNamed :: Text -> Text -> Generator
generatorNamed gid src = either (error . T.unpack) (fmap inlineOnly) (parseGenerator (GeneratorId gid) src)
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
  result <- generate ws noResolve noTools [Step gen (M.fromList sets)] now mode
  (result,) <$> snapshot

spec :: Spec
spec = do
  createSpec
  insertSpec
  atSpec
  completeSpec
  holesSpec
  fillSpec
  parseCheckSpec
  replaceSpec
  regionSpec
  unusableSpec
  composeSpec

createSpec :: Spec
createSpec = describe "generate (create edits)" $ do
  it "binds a generator query after derived values, for templates (Review Focus 5)" $ do
    let gen =
          generatorFrom . (header <>) . T.unlines $
            [ "[derive]", "dir = \"src/{= name|snake =}\""
            , "[query.mods]", "glob = \"{= dir =}/*.hs\""
            , "[[edits]]", "create = \"index.txt\"", "text = \"{= for m in mods =}{= m.stem =}\\n{= end =}\"" ]
    (_, files) <- run [("src/core/A.hs", ""), ("src/core/B.hs", ""), ("src/other/C.hs", "")] gen [("name", "core")] Apply
    M.lookup "index.txt" files `shouldBe` Just "A\nB\n"
  it "stops with QueryFailed when a generator query fails" $ do
    let gen = generatorFrom . (header <>) . T.unlines $ ["[query.types]", "files = \"a.x\"", "items_of = \"type T\"", "[[edits]]", "create = \"b\"", "text = \"b\""]
    (result, files) <- run [("a.x", "")] gen [("name", "x")] Apply
    either (\case QueryFailed _ f -> "query `types`" `T.isInfixOf` failureWhy f; _ -> False) (const False) result `shouldBe` True
    M.keys files `shouldBe` ["a.x"]
  it "renders every query with inputs and derived values only, so query order does not matter" $ do
    let gen = generatorFrom . (header <>) . T.unlines $ ["[query.a]", "glob = \"*.hs\"", "[query.b]", "glob = \"{= a =}\"", "[[edits]]", "create = \"b\"", "text = \"b\""]
    (result, _) <- run [("A.hs", "")] gen [("name", "x")] Apply
    either (\case QueryFailed _ f -> "undefined variable `a`" `T.isInfixOf` failureWhy f; _ -> False) (const False) result `shouldBe` True
  it "skips a create when `unless` finds a match, on disk or created earlier in the plan" $ do
    let migration =
          generatorFrom . (header <>) . T.unlines $
            [ "[[edits]]", "create = \"db/migrate/{= now|timestamp =}_create_{= name|plural|snake =}.rb\""
            , "text = \"class Create\\n\"", "unless = { exists = \"db/migrate/*_create_{= name|plural|snake =}.rb\" }" ]
    (_, once) <- run [] migration [("name", "post")] Apply
    M.keys once `shouldBe` ["db/migrate/20260927000000_create_posts.rb"]
    (ws, snap) <- memoryWorkspace (M.toList once)
    Right plan <- generate ws noResolve noTools [Step migration (M.fromList [("name", "post")])] (UTCTime (fromGregorian 2026 9 28) 0) Apply
    map editAction (planEdits plan) `shouldBe` [Skipped (UnlessMatched "db/migrate/20260927000000_create_posts.rb")]
    (M.keys <$> snap) `shouldReturn` ["db/migrate/20260927000000_create_posts.rb"]
  it "skips an insert when `unless matches` finds a line in its target, on disk or added earlier in the plan" $ do
    let letA =
          generatorFrom . (header <>) . T.unlines $
            [ "[[edits]]", "insert = \"main.rs\"", "at = \"end of file\"", "text = \"let a = todo!();\""
            , "unless = { matches = \"^ *let (mut )?{= name =}[:= ]\" }"
            , "[[edits]]", "insert = \"main.rs\"", "at = \"start of file\"", "text = \"let a: A = todo!();\""
            , "unless = { matches = \"^ *let (mut )?{= name =}[:= ]\" }" ]
    (_, once) <- run [("main.rs", "fn main() {}\n")] letA [("name", "a")] Apply
    M.lookup "main.rs" once `shouldBe` Just "fn main() {}\nlet a = todo!();\n"
    (ws, _) <- memoryWorkspace [("main.rs", "    let mut a = Real::new();\n")]
    Right plan <- generate ws noResolve noTools [Step letA (M.fromList [("name", "a")])] (UTCTime (fromGregorian 2026 9 28) 0) Apply
    map editAction (planEdits plan) `shouldBe` [Skipped (UnlessFound "main.rs" 1), Skipped (UnlessFound "main.rs" 1)]
  it "re-running a create that later edits changed is a no-op (fix pass)" $ do
    let grow =
          generatorFrom . (header <>) . T.unlines $
            [ "[[edits]]", "create = \"a.txt\"", "text = \"start\\n# more\\n\""
            , "[[edits]]", "insert = \"a.txt\"", "at = \"before marker # more\"", "text = \"{= name =}\"" ]
    (_, once) <- run [] grow [("name", "x")] Apply
    M.lookup "a.txt" once `shouldBe` Just "start\nx\n# more\n"
    (Right again, twice) <- run (M.toList once) grow [("name", "x")] Apply
    planConflicts again `shouldBe` []
    planWrites again `shouldBe` M.empty
    map editAction (planEdits again) `shouldBe` [Skipped UnchangedContent, Skipped UnchangedContent]
    twice `shouldBe` once
  it "still refuses to replace a created file that was changed by hand" $ do
    let grow =
          generatorFrom . (header <>) . T.unlines $
            [ "[[edits]]", "create = \"a.txt\"", "text = \"start\\n# more\\n\""
            , "[[edits]]", "insert = \"a.txt\"", "at = \"before marker # more\"", "text = \"{= name =}\"" ]
    (Right plan, _) <- run [("a.txt", "start\nhand edit\n# more\n")] grow [("name", "x")] Apply
    map describeConflict (planConflicts plan) `shouldSatisfy` any ("already exists" `T.isInfixOf`)
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
    planConflicts plan `shouldBe` [RequirementUnmet ["test/gen"] ["billing/service.rs"] (Just "run service-skeleton for billing")]
    files `shouldBe` M.empty
  it "returns input problems instead of planning" $ do
    (result, _) <- run [] createGen [] Apply
    result `shouldSatisfy` \case
      Left (InvalidInputs _ (InputProblems [MissingInput _])) -> True
      _ -> False

insertSpec :: Spec
insertSpec = describe "generate (marker inserts)" $ do
  let insertGen = generatorFrom insertToml
      insertToml =
        T.unlines
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
    map editAction (planEdits plan) `shouldBe` [WillInsert 2 "pub mod refund_invoice;", WillInsert 3 "\npub fn refund_invoice(&self) {\n    todo!()\n}"]
  it "shows in the plan the text as written: dedented, final newline dropped" $ do
    (Right plan, _) <- run [("svc/lib.rs", lib), ("svc/service.rs", service)] (generatorFrom (T.replace "text = \"pub mod" "text = \"        pub mod" insertToml)) [("cmd", "refund_invoice")] PlanOnly
    take 1 (map editAction (planEdits plan)) `shouldBe` [WillInsert 2 "pub mod refund_invoice;"]
  it "keeps earlier inserts in order (the marker stays last)" $ do
    (ws, snapshot) <- memoryWorkspace [("svc/lib.rs", lib), ("svc/service.rs", service)]
    _ <- generate ws noResolve noTools [Step insertGen (M.fromList [("cmd", "b_b")])] now Apply
    _ <- generate ws noResolve noTools [Step insertGen (M.fromList [("cmd", "c_c")])] now Apply
    M.lookup "svc/lib.rs" <$> snapshot `shouldReturn` Just "pub mod a;\npub mod b_b;\npub mod c_c;\n// olifant:modules\n"
  it "skips text that is already present, so re-running is a no-op" $ do
    (ws, _) <- memoryWorkspace [("svc/lib.rs", lib), ("svc/service.rs", service)]
    _ <- generate ws noResolve noTools [Step insertGen (M.fromList [("cmd", "b_b")])] now Apply
    Right again <- generate ws noResolve noTools [Step insertGen (M.fromList [("cmd", "b_b")])] now Apply
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
    map describeConflict (planConflicts plan)
      `shouldBe` [ "edit 1: cannot place text in svc/lib.rs: `before marker // olifant:modules`: marker not found"
                 , "edit 2: cannot place text in svc/service.rs: `before marker // olifant:delegates`: marker appears 2 times; it must appear once"
                 ]
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
    _ <- evalIO (generate ws noResolve noTools [Step insertGen sets] now Apply)
    second <- evalIO (generate ws noResolve noTools [Step insertGen sets] now Apply)
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
      Right plan <- generate (diskWorkspace dir) noResolve noTools [Step pathGen (M.fromList [("dir", "mine.txt")])] now Apply
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

composeSpec :: Spec
composeSpec = describe "generate (composition)" $ do
  let pageGen =
        generatorNamed "page" . T.unlines $
          [ "description = \"page\"", "language = \"x\""
          , "[inputs.page]", "type = \"ident\""
          , "[inputs.msgs]", "type = \"list\"", "optional = true", "of = \"ident\""
          , "[[edits]]", "create = \"src/{= page|pascal =}.txt\"", "text = \"page {= page|pascal =}\\n-- msgs\\n\""
          , "[[uses]]", "generator = \"msg\"", "for_each = \"msgs\""
          , "with = { page = \"{= page =}\", msg = \"{= item =}\" }"
          ]
      msgGen =
        generatorNamed "msg" . T.unlines $
          [ "description = \"msg\"", "language = \"x\""
          , "[inputs.page]", "type = \"ident\""
          , "[inputs.msg]", "type = \"ident\""
          , "[requires]", "exists = [\"src/{= page|pascal =}.txt\"]"
          , "[[edits]]", "insert = \"src/{= page|pascal =}.txt\"", "anchor = { marker = \"-- msgs\" }", "text = \"| {= msg|pascal =}\""
          ]
      resolveTo gens _ ref = pure (maybe (Left (ResolveNotFound ("no generator `" <> ref <> "`"))) Right (lookup ref gens))
      resolve = resolveTo [("msg", msgGen), ("page", pageGen)]
      gen1 steps = do
        (ws, snapshot) <- memoryWorkspace []
        result <- generate ws resolve noTools steps now Apply
        (result,) <$> snapshot
  it "runs a generator's [[uses]] once per for_each item, in one plan, with requires met by earlier steps" $ do
    (Right plan, files) <- gen1 [Step pageGen (M.fromList [("page", "settings"), ("msgs", "toggle_dark_mode, reset")])]
    M.lookup "src/Settings.txt" files `shouldBe` Just "page Settings\n| ToggleDarkMode\n| Reset\n-- msgs\n"
    map editStep (planEdits plan) `shouldBe` [["page"], ["page", "msg[1]"], ["page", "msg[2]"]]
  it "runs [[uses]] once per record of a generator query" $ do
    let fromPages =
          generatorNamed "all" . T.unlines $
            [ "description = \"all\"", "language = \"x\""
            , "[query.pages]", "glob = \"pages/*.txt\""
            , "[[uses]]", "generator = \"page\"", "for_each = \"pages\""
            , "with = { page = \"{= item.stem =}\" }"
            ]
    (ws, snapshot) <- memoryWorkspace [("pages/about.txt", ""), ("pages/contact_us.txt", "")]
    Right _ <- generate ws resolve noTools [Step fromPages M.empty] now Apply
    files <- snapshot
    [k | k <- M.keys files, "src/" `T.isPrefixOf` T.pack k] `shouldBe` ["src/About.txt", "src/ContactUs.txt"]
  describe "insertBranch (spec §5.6)" $ do
    it "adds the separator to a last arm that lacks it (Review Focus 4)" $
      insertBranch "," False (0, 2) "B => todo!()," "match f {\n    A => 1,\n    C => 2\n}\n"
        `shouldBe` (4, "match f {\n    A => 1,\n    C => 2,\n    B => todo!(),\n}\n")
    it "leaves a block last arm as it is" $
      insertBranch "," False (0, 3) "B => todo!()," "match f {\n    A => {\n        1\n    }\n}\n"
        `shouldBe` (5, "match f {\n    A => {\n        1\n    }\n    B => todo!(),\n}\n")
    it "adds no blank line after a single arm in a comma-separated language" $
      insertBranch "," False (0, 1) "B => todo!()," "match f {\n    A => 1,\n}\n"
        `shouldBe` (3, "match f {\n    A => 1,\n    B => todo!(),\n}\n")
    it "adds no separator when the language has none" $
      insertBranch "" True (0, 2) "B -> 2" "case x of\n    A -> 1\n    C -> 3\n"
        `shouldBe` (4, "case x of\n    A -> 1\n    C -> 3\n    B -> 2\n")
    it "(python) no blank line after a single arm where the language's style has none" $
      insertBranch "" False (0, 2) "case B():\n    return 2" "match x:\n    case A():\n        return 1\n"
        `shouldBe` (4, "match x:\n    case A():\n        return 1\n    case B():\n        return 2\n")
  describe "dependent choices (spec §4.1)" $ do
    let wiring = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"x\""
          , "[inputs.service]", "type = \"enum\"", "one_of = \"services\""
          , "[inputs.dependency]", "type = \"enum\"", "one_of = \"ports\""
          , "[derive]", "service_dir = \"{= service =}/{= service =}-service\""
          , "[query.services]", "glob = \"*/*-service\"", "value = \"parent folder\""
          , "[query.ports]", "glob = \"{= service_dir =}/ports/*.rs\""
          , "[[edits]]", "create = \"out.txt\"", "text = \"{= service =} {= dependency =}\"" ]
        project =
          [ ("billing/billing-service/ports/email_sender.rs", ""), ("billing/billing-service/ports/invoice_repository.rs", "")
          , ("notifications/notifications-service/lib.rs", "") ]
        message sets = do
          (result, _) <- run project wiring sets Apply
          pure (either describeGenerationError (const "") result)
    it "(a) lists only the chosen service's ports, and names the context when the pick is wrong" $ do
      (_, files) <- run project wiring [("service", "billing"), ("dependency", "invoice_repository")] Apply
      M.lookup "out.txt" files `shouldBe` Just "billing invoice_repository"
      message [("service", "billing"), ("dependency", "clock")]
        >>= (`shouldSatisfy` T.isInfixOf "invalid inputs: `dependency`: expected one of email_sender, invoice_repository (the ports of service=billing); got `clock`")
    it "(b) says a dependent input waits for the one it depends on, not a template error (Review Focus 1)" $
      message [("dependency", "clock")] >>= (`shouldSatisfy` \m -> "missing `service`" `T.isInfixOf` m && "`dependency`: its choices depend on `service`" `T.isInfixOf` m)
    it "(c) does not run the query for an invalid earlier pick (Review Focus 1)" $
      message [("service", "nope"), ("dependency", "x")] >>= (`shouldSatisfy` \m -> "`service`: expected one of billing, notifications" `T.isInfixOf` m && not ("found nothing" `T.isInfixOf` m))
    it "(d) names the query and context when it finds nothing (Review Focus 2)" $
      message [("service", "notifications"), ("dependency", "x")] >>= (`shouldSatisfy` T.isInfixOf "the query `ports` found nothing for service=notifications")
    it "(f) a query may use a derived value built from `now`" $ do
      let dated = generatorFrom . T.unlines $
            [ "description = \"d\"", "language = \"x\""
            , "[inputs.thing]", "type = \"enum\"", "one_of = \"things\""
            , "[derive]", "year = \"{= now|timestamp =}\""
            , "[query.things]", "glob = \"log/*-{= year =}.txt\""
            , "[[edits]]", "create = \"out.txt\"", "text = \"{= thing =}\"" ]
      (r, files) <- run [("log/a-20260927000000.txt", "")] dated [("thing", "a-20260927000000")] Apply
      either describeGenerationError (const "ok") r `shouldBe` "ok"
      M.lookup "out.txt" files `shouldBe` Just "a-20260927000000"
    it "(g) a --set override of a derived value is what the query uses, as the edits do" $ do
      (r, files) <- run project wiring [("service", "notifications"), ("service_dir", "billing/billing-service"), ("dependency", "email_sender")] Apply
      either describeGenerationError (const "ok") r `shouldBe` "ok"
      M.lookup "out.txt" files `shouldBe` Just "notifications email_sender"
      message [("service", "notifications"), ("service_dir", "billing/billing-service"), ("dependency", "email_template_repository")]
        >>= (`shouldSatisfy` T.isInfixOf "expected one of email_sender, invoice_repository")
    it "(e) sees files an earlier step of the chain created (Review Focus 3)" $ do
      let maker = generatorNamed "maker" . T.unlines $ ["description = \"m\"", "language = \"x\"", "[[edits]]", "create = \"svc/a/x.txt\"", "text = \"x\""]
          picker = generatorNamed "picker" . T.unlines $
            [ "description = \"p\"", "language = \"x\""
            , "[inputs.dir]", "type = \"enum\"", "one_of = \"dirs\""
            , "[inputs.file]", "type = \"enum\"", "one_of = \"files\""
            , "[query.dirs]", "glob = \"svc/*\""
            , "[query.files]", "glob = \"svc/{= dir =}/*.txt\""
            , "[[edits]]", "create = \"picked.txt\"", "text = \"{= dir =}/{= file =}\"" ]
      (ws, snap) <- memoryWorkspace []
      r <- generate ws noResolve noTools [Step maker M.empty, Step picker (M.fromList [("dir", "a"), ("file", "x")])] now Apply
      either describeGenerationError (const "ok") r `shouldBe` "ok"
      (M.lookup "picked.txt" <$> snap) `shouldReturn` Just "a/x"
  describe "one_of in a chain (spec §4.1: a later step can choose what an earlier one creates)" $ do
    let skeleton =
          generatorNamed "skeleton" . T.unlines $
            [ "description = \"s\"", "language = \"x\""
            , "[inputs.service]", "type = \"ident\""
            , "[[edits]]", "create = \"{= service =}/{= service =}-service/lib.rs\"", "text = \"// {= service =}\\n\"" ]
        command =
          generatorNamed "command" . T.unlines $
            [ "description = \"c\"", "language = \"x\""
            , "[inputs.service]", "type = \"enum\"", "one_of = \"services\"", "[query.services]", "glob = \"*/*-service\""
            , "[[edits]]", "create = \"{= service =}.txt\"", "text = \"cmd\\n\"" ]
        withSkeleton =
          generatorNamed "with-skeleton" . T.unlines $
            [ "description = \"w\"", "language = \"x\""
            , "[inputs.service]", "type = \"ident\""
            , "[[edits]]", "create = \"{= service =}/{= service =}-service/lib.rs\"", "text = \"x\\n\""
            , "[[uses]]", "generator = \"command\"", "with = { service = \"{= service =}-service\" }" ]
        resolveChain = resolveTo [("command", command)]
        runChain files steps = do
          (ws, snapshot) <- memoryWorkspace files
          r <- generate ws resolveChain noTools steps now Apply
          (r,) <$> snapshot
    it "lets a step choose a service an earlier step of the chain creates" $ do
      (r, files) <- runChain [("billing/billing-service/lib.rs", "")] [Step skeleton (M.fromList [("service", "payments")]), Step command (M.fromList [("service", "payments-service")])]
      either (Left . describeGenerationError) (const (Right ())) r `shouldBe` Right ()
      M.member "payments-service.txt" files `shouldBe` True
    it "lets a [[uses]] step choose what its parent's own edits create" $ do
      (r, files) <- runChain [] [Step withSkeleton (M.fromList [("service", "orders")])]
      either (Left . describeGenerationError) (const (Right ())) r `shouldBe` Right ()
      M.member "orders-service.txt" files `shouldBe` True
    it "still refuses a value in neither the project nor the plan" $ do
      (r, _) <- runChain [("billing/billing-service/lib.rs", "")] [Step command (M.fromList [("service", "nowhere-service")])]
      either describeGenerationError (const "") r `shouldSatisfy` T.isInfixOf "expected one of billing-service"
  it "treats an empty for_each list as no steps" $ do
    (Right plan, files) <- gen1 [Step pageGen (M.fromList [("page", "settings")])]
    M.keys files `shouldBe` ["src/Settings.txt"]
    length (planEdits plan) `shouldBe` 1
  it "chains top-level steps into one plan with explicit inputs only" $ do
    (Right plan, files) <-
      gen1 [Step pageGen (M.fromList [("page", "profile")]), Step msgGen (M.fromList [("page", "profile"), ("msg", "follow")])]
    M.lookup "src/Profile.txt" files `shouldBe` Just "page Profile\n| Follow\n-- msgs\n"
    map editStep (planEdits plan) `shouldBe` [["page (step 1)"], ["msg (step 2)"]]
  it "does not inherit inputs: a step missing an input fails even if an earlier step set it" $ do
    (result, files) <- gen1 [Step pageGen (M.fromList [("page", "profile")]), Step msgGen (M.fromList [("msg", "follow")])]
    result `shouldSatisfy` \case
      Left (InvalidInputs ["msg (step 2)"] (InputProblems [MissingInput _])) -> True
      _ -> False
    files `shouldBe` M.empty
  it "is idempotent when the same step appears twice" $ do
    (Right plan, _) <-
      gen1
        [ Step pageGen (M.fromList [("page", "a_b")])
        , Step msgGen (M.fromList [("page", "a_b"), ("msg", "x_y")])
        , Step msgGen (M.fromList [("page", "a_b"), ("msg", "x_y")])
        ]
    planConflicts plan `shouldBe` []
    map editAction (drop 2 (planEdits plan)) `shouldBe` [Skipped AlreadyPresent]
  it "reports an unknown nested generator and writes nothing" $ do
    (ws, snapshot) <- memoryWorkspace []
    result <- generate ws noResolve noTools [Step pageGen (M.fromList [("page", "a_b"), ("msgs", "c_d")])] now Apply
    result `shouldSatisfy` \case
      Left (UnknownGenerator ["page"] "msg" _) -> True
      _ -> False
    snapshot `shouldReturn` M.empty
  it "detects composition cycles instead of looping" $ do
    let loopGen = generatorNamed "loop" "description = \"loop\"\nlanguage = \"x\"\n[[uses]]\ngenerator = \"loop\"\n"
    (ws, _) <- memoryWorkspace []
    result <- generate ws (resolveTo [("loop", loopGen)]) noTools [Step loopGen M.empty] now Apply
    result `shouldSatisfy` \case
      Left (CompositionCycle _) -> True
      _ -> False

  it "reports a cycle even behind an empty for_each" $ do
    let loopGen = generatorNamed "loop" "description = \"loop\"\nlanguage = \"x\"\n[inputs.xs]\ntype = \"list\"\noptional = true\nof = \"ident\"\n[[uses]]\ngenerator = \"loop\"\nfor_each = \"xs\"\n"
    (ws, _) <- memoryWorkspace []
    result <- generate ws (resolveTo [("loop", loopGen)]) noTools [Step loopGen M.empty] now Apply
    result `shouldSatisfy` \case
      Left (CompositionCycle _) -> True
      _ -> False
  it "labels repeated uses of the same generator distinctly" $ do
    let twice = generatorNamed "page" . T.unlines $
          [ "description = \"page\"", "language = \"x\""
          , "[[edits]]", "create = \"src/P.txt\"", "text = \"-- msgs\\n\""
          , "[[uses]]", "generator = \"msg\"", "with = { page = \"p\", msg = \"a_a\" }"
          , "[[uses]]", "generator = \"msg\"", "with = { page = \"p\", msg = \"b_b\" }"
          ]
    (Right plan, _) <- gen1 [Step twice M.empty]
    map editStep (planEdits plan) `shouldBe` [["page"], ["page", "msg (use 1)"], ["page", "msg (use 2)"]]
  it "does not let a later step satisfy an earlier step's requires" $ do
    (Right plan, files) <- gen1 [Step msgGen (M.fromList [("page", "x_y"), ("msg", "a_b")]), Step pageGen (M.fromList [("page", "x_y")])]
    planConflicts plan `shouldSatisfy` any (\case RequirementUnmet ["msg (step 1)"] _ _ -> True; _ -> False)
    files `shouldBe` M.empty
  it "lists a conflicting edit with its step, so conflicts point at something" $ do
    (ws, snapshot) <- memoryWorkspace [("src/B.txt", "someone else's\n")]
    Right plan <- generate ws noResolve noTools [Step (pageOnly "page") (M.fromList [("page", "a")]), Step (pageOnly "page") (M.fromList [("page", "b")])] now Apply
    map (\e -> (editStep e, editAction e)) (planEdits plan)
      `shouldBe` [(["page (step 1)"], WillCreate "page A\n"), (["page (step 2)"], Conflicted)]
    snapshot `shouldReturn` M.fromList [("src/B.txt", "someone else's\n")]
  it "counts an existing binary file as present for requires" $
    withSystemTempDirectory "olifant" $ \dir -> do
      BS.writeFile (dir </> "logo.png") "\x89PNG\xff"
      let gen = generatorFrom "description = \"d\"\nlanguage = \"x\"\n[requires]\nexists = [\"logo.png\"]\n[[edits]]\ncreate = \"a.txt\"\ntext = \"a\"\n"
      Right plan <- generate (diskWorkspace dir) noResolve noTools [Step gen M.empty] now Apply
      planConflicts plan `shouldBe` []
  where
    pageOnly gid = generatorNamed gid "description = \"p\"\nlanguage = \"x\"\n[inputs.page]\ntype = \"ident\"\n[[edits]]\ncreate = \"src/{= page|pascal =}.txt\"\ntext = \"page {= page|pascal =}\\n\"\n"

atSpec :: Spec
atSpec = describe "generate (at phrases)" $ do
  let gen =
        generatorFrom . (header <>) . T.unlines $
          [ "[[edits]]"
          , "insert = \"lib.rs\""
          , "at = [\"end of mods in lib\", \"after last line matching ^pub mod \"]"
          , "text = \"pub mod {= name|snake =};\""
          ]
  it "falls back to a line target when no adapter/tool is available, and says which anchor it used" $ do
    (Right plan, files) <- run [("lib.rs", "pub mod a;\npub mod b;\n\nfn x() {}\n")] gen [("name", "c")] Apply
    M.lookup "lib.rs" files `shouldBe` Just "pub mod a;\npub mod b;\npub mod c;\n\nfn x() {}\n"
    map editAnchor (planEdits plan) `shouldBe` [Just "after last line matching ^pub mod "]
  it "is a conflict listing every layer when none resolves" $ do
    (Right plan, _) <- run [("lib.rs", "fn x() {}\n")] gen [("name", "c")] PlanOnly
    map describeConflict (planConflicts plan) `shouldSatisfy` any ("`after last line matching ^pub mod `: no line matches" `T.isInfixOf`)
  it "keeps `anchor = { marker = … }` working" $ do
    let old = generatorFrom . (header <>) . T.unlines $ ["[[edits]]", "insert = \"a.txt\"", "anchor = { marker = \"# here\" }", "text = \"{= name =}\""]
    (_, files) <- run [("a.txt", "x\n# here\n")] old [("name", "y")] Apply
    M.lookup "a.txt" files `shouldBe` Just "x\ny\n# here\n"
  it "uses a marker fallback when the syntax tool is missing" $ do
    let Right elm =
          parseLanguage "elm" "/p" . T.unlines $
            [ "files = [\"**/*.elm\"]", "comment = \"--\"", "grammar = \"elm\""
            , "[targets.type]", "phrase = \"type NAME\"", "container = { kind = \"type_declaration\" }"
            , "items = { kind = \"union_variant\" }", "example = \"end of type Msg\""
            ]
        gen' = generatorFrom . (header <>) . T.unlines $ ["[[edits]]", "insert = \"Main.elm\"", "at = [\"end of type Page\", \"before marker -- olifant:pages\"]", "text = \"| {= name|pascal =}\""]
    (ws, snapshot) <- memoryWorkspace [("Main.elm", "type Page\n    = A\n    -- olifant:pages\n")]
    Right plan <- generate ws noResolve (basicTools noSyntax noChecker [elm]) [Step gen' (M.fromList [("name", "b")])] now Apply
    map editAnchor (planEdits plan) `shouldBe` [Just "before marker -- olifant:pages"]
    (M.lookup "Main.elm" <$> snapshot) `shouldReturn` Just "type Page\n    = A\n    | B\n    -- olifant:pages\n"
  it "re-running before/after inserts is a no-op (review)" $ do
    let beside =
          generatorFrom . (header <>) . T.unlines $
            [ "[[edits]]", "insert = \"a.elm\"", "at = \"after marker -- here\"", "text = \"after {= name =}\""
            , "[[edits]]", "insert = \"a.elm\"", "at = \"before first line matching ^init \"", "text = \"before {= name =}\\n\""
            ]
        start = "x = 1\n-- here\n\ninit a = a\n"
    (_, once) <- run [("a.elm", start)] beside [("name", "b")] Apply
    M.lookup "a.elm" once `shouldBe` Just "x = 1\n-- here\nafter b\n\nbefore b\ninit a = a\n"
    (Right again, twice) <- run (M.toList once) beside [("name", "b")] Apply
    map editAction (planEdits again) `shouldBe` [Skipped AlreadyPresent, Skipped AlreadyPresent]
    twice `shouldBe` once
  it "re-running an at-phrase insert is a no-op" $ hedgehog $ do
    n <- forAll (Gen.element ["c", "d_e", "f"])
    (_, once) <- evalIO (run [("lib.rs", "pub mod a;\n")] gen [("name", n)] Apply)
    (_, twice) <- evalIO (run (M.toList once) gen [("name", n)] Apply)
    twice === once

-- | A checker that returns scripted rounds, then clean.
scriptedChecker :: IORef [[MissingPatterns]] -> Checker IO
scriptedChecker ref = Checker $ \_ _ -> atomicModifyIORef' ref (\case [] -> ([], Right (CheckReport [] [] [])); (r : rs) -> (rs, Right (CheckReport r [] [])))

-- | A checker that always reports the given broken files and nothing missing.
brokenChecker :: [(FilePath, Text)] -> Checker IO
brokenChecker broken = Checker $ \_ _ -> pure (Right (CheckReport [] broken []))

elmLang :: Language
elmLang = l
  where
    Right l = parseLanguage "elm" "/p" $ T.unlines
      [ "files = [\"**/*.elm\"]", "comment = \"--\"", "grammar = \"elm\""
      , "placeholder = 'Debug.todo \"{= message =}\"'", "branch = \"{= pattern =} ->\\n    {= body =}\""
      , "[check]", "command = [\"true\"]", "format = \"elm-json\"" ]

holeLang :: Language
holeLang = l
  where
    Right l = parseLanguage "elm" "/p" $ T.unlines
      [ "files = [\"**/*.elm\"]", "comment = \"--\"", "grammar = \"elm\""
      , "placeholder = 'Debug.todo \"{= message =}\"'", "branch = \"{= pattern =} ->\\n    {= body =}\""
      , "[check]", "command = [\"true\"]", "format = \"elm-json\""
      , "[holes.expr]", "placeholder = 'Debug.todo \"olifant: {= hole =}\"'", "wrap = \"h = {= fill =}\"" ]

holesSpec :: Spec
holesSpec = describe "holes in plans (spec §6.4)" $ do
  let valGen = generatorFrom . T.unlines $
        [ "description = \"d\"", "language = \"elm\""
        , "[[edits]]", "create = \"src/V.elm\"", "text = \"v = {= ? val: expr | the value =}\\n\""
        , "[[edits]]", "insert = \"src/V.elm\"", "at = \"end of file\"", "text = \"w = {= ? other: expr | another =}\"" ]
      tools checker = basicTools noSyntax checker [holeLang]
  it "(a) writes the kind's placeholder and lists the hole, unfilled without a model" $ do
    (ws, snap) <- memoryWorkspace []
    Right plan <- generate ws noResolve (tools noChecker) [Step valGen M.empty] now Apply
    (M.lookup "src/V.elm" <$> snap) `shouldReturn` Just "v = Debug.todo \"olifant: val\"\nw = Debug.todo \"olifant: other\"\n"
    map (\h -> (holeName (phSite h), holeLine (phSite h), phStatus h)) (planHoles plan)
      `shouldBe` [("val", 1, HoleUnfilled "no model is configured"), ("other", 2, HoleUnfilled "no model is configured")]
  it "(b) names a complete body's hole per site (Review Focus 5)" $ do
    let page = "module Page.A exposing (..)\n\nupdate msg model =\n    case msg of\n        NoOp ->\n            ( model, Cmd.none )\n"
        gen = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"elm\"", "[inputs.name]", "type = \"ident\""
          , "[[edits]]", "complete = \"{= name|pascal =}\"", "text = \"{= pattern =} ->\\n    {= ? body: expr | for {= pattern =} =}\"" ]
    ref <- newIORef [[missing "src/A.elm" (3, 5) ["Reset"], missing "src/B.elm" (3, 5) ["Reset"]]]
    (ws, _) <- memoryWorkspace [("src/A.elm", page), ("src/B.elm", page)]
    Right plan <- generate ws noResolve (tools (scriptedChecker ref)) [Step gen (M.fromList [("name", "reset")])] now Apply
    map (holeName . phSite) (planHoles plan) `shouldBe` ["body[src/A.elm:9]", "body[src/B.elm:9]"]
  it "a case the compiler still reports after its branch says why the compiler objects" $ do
    let page = "module Page.A exposing (..)\n\nupdate msg model =\n    case msg of\n        NoOp ->\n            ( model, Cmd.none )\n"
        gen = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"elm\"", "[inputs.name]", "type = \"ident\""
          , "[[edits]]", "complete = \"{= name|pascal =}\"", "text = \"{= pattern =} ->\\n    ( model, Cmd.none )\"" ]
        stubborn = Checker $ \_ files -> pure (Right (CheckReport [missing "src/A.elm" (3, 5) ["Reset"]] [(p, "Reset is not defined") | (p, t) <- M.toList files, "Reset ->" `T.isInfixOf` t] []))
    (ws, _) <- memoryWorkspace [("src/A.elm", page)]
    Right plan <- generate ws noResolve (tools stubborn) [Step gen (M.fromList [("name", "reset")])] now PlanOnly
    map describeConflict (planConflicts plan) `shouldSatisfy` any (\c -> "still reports Reset" `T.isInfixOf` c && "Reset is not defined" `T.isInfixOf` c)
  it "(python review I1) an error already in the file is not the generated code's: the branch stands" $ do
    let page = "module Page.A exposing (..)\n\nbad = OLD_ERROR\n\nupdate msg model =\n    case msg of\n        NoOp ->\n            ( model, Cmd.none )\n"
        gen = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"elm\"", "[inputs.name]", "type = \"ident\""
          , "[[edits]]", "complete = \"{= name|pascal =}\"", "text = \"{= pattern =} ->\\n    ( model, Cmd.none )\"" ]
    ref <- newIORef [[missing "src/A.elm" (5, 7) ["Reset"]]]
    let checker = Checker $ \_ files -> do
          ms <- atomicModifyIORef' ref (\case [] -> ([], []); (r : rs) -> (rs, r))
          pure (Right (CheckReport ms [(p, "OLD_ERROR is not defined") | (p, t) <- M.toList files, "OLD_ERROR" `T.isInfixOf` t] []))
    (ws, _) <- memoryWorkspace [("src/A.elm", page)]
    Right plan <- generate ws noResolve (tools checker) [Step gen (M.fromList [("name", "reset")])] now PlanOnly
    map describeConflict (planConflicts plan) `shouldBe` []
    M.lookup "src/A.elm" (planWrites plan) `shouldSatisfy` maybe False ("Reset ->" `T.isInfixOf`)
    -- said as it is: the cases were completed; the error was there before
    planWarnings plan `shouldSatisfy` any (\w -> "already had errors before this plan" `T.isInfixOf` w && "OLD_ERROR" `T.isInfixOf` w)
  it "(c) refuses a kind the adapter does not declare (Review Focus 3)" $ do
    let gen = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"elm\"", "[[edits]]", "create = \"src/V.elm\"", "text = \"v = {= ? val: block | x =}\\n\"" ]
    (ws, _) <- memoryWorkspace []
    Right plan <- generate ws noResolve (tools noChecker) [Step gen M.empty] now Apply
    map describeConflict (planConflicts plan) `shouldSatisfy` any (T.isInfixOf "`block` is not a hole kind of the elm adapter (kinds: expr)")
  it "(d) re-running finds the placeholders already there (a hole matches whatever fills it)" $ do
    (ws, _) <- memoryWorkspace []
    Right _ <- generate ws noResolve (tools noChecker) [Step valGen M.empty] now Apply
    Right again <- generate ws noResolve (tools noChecker) [Step valGen M.empty] now Apply
    planWrites again `shouldBe` M.empty
    writeText ws "src/V.elm" "v = 42\nw = 7\n"
    Right filled <- generate ws noResolve (tools noChecker) [Step valGen M.empty] now Apply
    planWrites filled `shouldBe` M.empty

fillSpec :: Spec
fillSpec = describe "the fill phase (spec §6.4)" $ do
  let twoFiles = generatorFrom . T.unlines $
        [ "description = \"d\"", "language = \"elm\""
        , "[[edits]]", "create = \"src/A.elm\"", "text = \"a = {= ? a: expr | the a value =}\\n\""
        , "[[edits]]", "create = \"src/B.elm\"", "text = \"b = {= ? b: expr | the b value =}\\n\"" ]
      -- `!!` does not parse; a file containing BAD does not compile.
      parses = Syntax $ \_ _ content -> pure (Right [Match (0, 0) (0, 1) "!!" | "!!" `T.isInfixOf` content])
      compiles = Checker $ \_ files -> pure (Right (CheckReport [] [(p, "BAD is not defined") | (p, t) <- M.toList files, "BAD" `T.isInfixOf` t] []))
      withFiller replies = do
        calls <- newIORef []
        script <- newIORef replies
        let filler = Filler $ \req -> do
              modifyIORef' calls (++ [req])
              atomicModifyIORef' script (\case [] -> ([], Left "no more replies"); (r : rs) -> (rs, r))
            tools = (basicTools parses compiles [holeLang]) {toolFiller = Just (ModelFiller "fake" 2 filler)}
        pure (tools, calls)
      run1 tools = do
        (ws, snap) <- memoryWorkspace []
        Right plan <- generate ws noResolve tools [Step twoFiles M.empty] now Apply
        (plan,) <$> snap
      statuses plan = [(holeName (phSite h), phStatus h) | h <- planHoles plan]
  it "(a) fills every hole from one request" $ do
    (tools, calls) <- withFiller [Right (M.fromList [("a", "1"), ("b", "2")])]
    (plan, files) <- run1 tools
    M.lookup "src/A.elm" files `shouldBe` Just "a = 1\n"
    statuses plan `shouldBe` [("a", HoleFilled "fake" 1), ("b", HoleFilled "fake" 1)]
    length <$> readIORef calls `shouldReturn` 1
  it "(python review I1) a fill in a file with an error already there is judged by what it adds" $ do
    let withOld = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"elm\""
          , "[[edits]]", "create = \"src/A.elm\"", "text = \"x = OLD_ERROR\\na = {= ? a: expr | the a value =}\\n\"" ]
        oldOrBad = Checker $ \_ files -> pure (Right (CheckReport [] ([(p, "OLD_ERROR is not defined") | (p, t) <- M.toList files, "OLD_ERROR" `T.isInfixOf` t] ++ [(p, "BAD is not defined") | (p, t) <- M.toList files, "BAD" `T.isInfixOf` t]) []))
    script <- newIORef [Right (M.fromList [("a", "1")])]
    let filler = Filler $ \_ -> atomicModifyIORef' script (\case [] -> ([], Left "no more replies"); (r : rs) -> (rs, r))
        tools = (basicTools parses oldOrBad [holeLang]) {toolFiller = Just (ModelFiller "fake" 0 filler)}
    (ws, _) <- memoryWorkspace []
    Right plan <- generate ws noResolve tools [Step withOld M.empty] now PlanOnly
    statuses plan `shouldBe` [("a", HoleFilled "fake" 1)]
  it "(a2) shows the fills in the plan's edits, not the placeholders (review I1)" $ do
    (tools, _) <- withFiller [Right (M.fromList [("a", "1"), ("b", "2")])]
    (ws, _) <- memoryWorkspace []
    Right plan <- generate ws noResolve tools [Step twoFiles M.empty] now PlanOnly
    [t | e <- planEdits plan, WillCreate t <- [editAction e]] `shouldBe` ["a = 1\n", "b = 2\n"]
  it "(b) retries a fill that does not compile, with the compiler's message" $ do
    (tools, calls) <- withFiller [Right (M.fromList [("a", "BAD"), ("b", "2")]), Right (M.fromList [("a", "7")])]
    (plan, files) <- run1 tools
    M.lookup "src/A.elm" files `shouldBe` Just "a = 7\n"
    statuses plan `shouldBe` [("a", HoleFilled "fake" 2), ("b", HoleFilled "fake" 1)]
    reqs <- readIORef calls
    map (\h -> (fhName h, fhPrevious h)) (frHoles (reqs !! 1)) `shouldBe` [("a", Just ("BAD", "src/A.elm: BAD is not defined"))]
  it "(c) keeps the good fill and the bad hole's placeholder (Review Focus 1)" $ do
    (tools, _) <- withFiller (replicate 3 (Right (M.fromList [("a", "BAD"), ("b", "2")])))
    (plan, files) <- run1 tools
    M.lookup "src/A.elm" files `shouldBe` Just "a = Debug.todo \"olifant: a\"\n"
    M.lookup "src/B.elm" files `shouldBe` Just "b = 2\n"
    statuses plan `shouldBe` [("a", HoleUnfilled "src/A.elm: BAD is not defined"), ("b", HoleFilled "fake" 1)]
  it "(d) survives junk: a failed reply leaves placeholders; a missing name is a failed attempt (Review Focus 2)" $ do
    (tools, _) <- withFiller (replicate 3 (Left "the reply is not JSON"))
    (plan, files) <- run1 tools
    M.lookup "src/A.elm" files `shouldBe` Just "a = Debug.todo \"olifant: a\"\n"
    statuses plan `shouldBe` [("a", HoleUnfilled "the reply is not JSON"), ("b", HoleUnfilled "the reply is not JSON")]
    (tools2, _) <- withFiller (replicate 3 (Right (M.fromList [("b", "2"), ("zzz", "9")])))
    (plan2, _) <- run1 tools2
    statuses plan2 `shouldBe` [("a", HoleUnfilled "the model returned no fill for `a`"), ("b", HoleFilled "fake" 1)]
  it "(e) re-running over filled holes calls no model (Review Focus 4)" $ do
    (tools, calls) <- withFiller [Right (M.fromList [("a", "1"), ("b", "2")])]
    (ws, _) <- memoryWorkspace []
    Right _ <- generate ws noResolve tools [Step twoFiles M.empty] now Apply
    Right again <- generate ws noResolve tools [Step twoFiles M.empty] now Apply
    planWrites again `shouldBe` M.empty
    length <$> readIORef calls `shouldReturn` 1
  it "(g) puts each fill at its own placeholder, two holes on one line, multi-line fills included (review I2)" $ do
    let pair = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"elm\""
          , "[[edits]]", "create = \"src/P.elm\"", "text = \"p = ( {= ? x: expr | first =}, {= ? y: expr | second =} )\\n\"" ]
    calls <- newIORef []
    let filler = Filler $ \req -> modifyIORef' calls (++ [req]) >> pure (Right (M.fromList [("x", "if True then\n  1\nelse\n  2"), ("y", "3")]))
        tools = (basicTools parses compiles [holeLang]) {toolFiller = Just (ModelFiller "fake" 2 filler)}
    (ws, snap) <- memoryWorkspace []
    Right plan <- generate ws noResolve tools [Step pair M.empty] now Apply
    (M.lookup "src/P.elm" <$> snap) `shouldReturn` Just "p = ( if True then\n  1\nelse\n  2, 3 )\n"
    statuses plan `shouldBe` [("x", HoleFilled "fake" 1), ("y", HoleFilled "fake" 1)]
  it "(m) a fill's leading import lines join the file's imports, once each (python-pack: fills that need a name)" $ do
    let pyLang = l
          where
            Right l = parseLanguage "python" "/p" $ T.unlines
              [ "files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\""
              , "imports = \"^(from [A-Za-z0-9_.]+ import .+|import [A-Za-z0-9_.]+)$\""
              , "[check]", "command = [\"true\"]", "format = \"pyright-json\""
              , "[holes.expr]", "placeholder = 'todo(\"olifant: {= hole =}\")'", "wrap = \"h = {= fill =}\"" ]
        gen = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"python\""
          , "[[edits]]", "create = \"src/c.py\"", "text = \"\\\"\\\"\\\"Codec.\\\"\\\"\\\"\\n\\nfrom x import Slot\\n\\n\\ndef decode(t: str) -> Slot:\\n    return {= ? decode: expr | read it =}\\n\"" ]
    calls <- newIORef []
    let filler = Filler $ \req -> modifyIORef' calls (++ [req]) >> pure (Right (M.fromList [("decode", "from datetime import date\nfrom x import Slot\n\nSlot(day=date.fromisoformat(t))")]))
        sawCode = Syntax $ \_ _ content -> pure (Right [Match (0, 0) (0, 1) "import" | "import" `T.isInfixOf` content])
        tools = (basicTools sawCode compiles [pyLang]) {toolFiller = Just (ModelFiller "fake" 0 filler)}
    (ws, snap) <- memoryWorkspace []
    Right plan <- generate ws noResolve tools [Step gen M.empty] now Apply
    statuses plan `shouldBe` [("decode", HoleFilled "fake" 1)]
    (M.lookup "src/c.py" <$> snap) `shouldReturn` Just "\"\"\"Codec.\"\"\"\n\nfrom x import Slot\nfrom datetime import date\n\n\ndef decode(t: str) -> Slot:\n    return Slot(day=date.fromisoformat(t))\n"
    -- what the plan shows is what is written
    map Just [t | e <- planEdits plan, WillCreate t <- [editAction e]] `shouldBe` [M.lookup "src/c.py" (planWrites plan)]
    -- the model is told it may (the language allows it)
    reqs <- readIORef calls
    map fhImports (concatMap frHoles reqs) `shouldBe` [True]
  it "(n) shows the model the files its generator's `context` names (python-pack: a codec needs the field's type)" $ do
    let gen = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"elm\""
          , "context = [\"src/{= m =}/*.elm\"]"
          , "[inputs.m]", "type = \"ident\""
          , "[[edits]]", "create = \"src/V.elm\"", "text = \"v = {= ? val: expr | the value =}\\n\"" ]
    calls <- newIORef []
    let filler = Filler $ \req -> modifyIORef' calls (++ [req]) >> pure (Right (M.fromList [("val", "1")]))
        tools = (basicTools parses compiles [holeLang]) {toolFiller = Just (ModelFiller "fake" 0 filler)}
    (ws, _) <- memoryWorkspace [("src/models/Slot.elm", "type alias Slot = { day : Int }\n"), ("src/other/X.elm", "x = 1\n")]
    Right _ <- generate ws noResolve tools [Step gen (M.fromList [("m", "models")])] now Apply
    map frContext <$> readIORef calls `shouldReturn` [[("src/models/Slot.elm", "type alias Slot = { day : Int }\n")]]
  it "(o) shows repeated holes' fills in the plan's edits, as written (python-pack: a codec per value field)" $ do
    let twice = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"elm\""
          , "[[edits]]", "create = \"src/T.elm\"", "text = \"a = {= ? v: expr | the first =}\\nb = {= ? v: expr | the second =}\\n\"" ]
        filler = Filler $ \_ -> pure (Right (M.fromList [("v[src/T.elm:1]", "1"), ("v[src/T.elm:2]", "2")]))
        tools = (basicTools parses compiles [holeLang]) {toolFiller = Just (ModelFiller "fake" 0 filler)}
    (ws, _) <- memoryWorkspace []
    Right plan <- generate ws noResolve tools [Step twice M.empty] now PlanOnly
    [t | e <- planEdits plan, WillCreate t <- [editAction e]] `shouldBe` ["a = 1\nb = 2\n"]
  it "(m2) with no imports in the file, a fill's imports go after its leading docstring and comments" $ do
    let pyLang = l
          where
            Right l = parseLanguage "python" "/p" $ T.unlines
              [ "files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\""
              , "imports = \"^(from [A-Za-z0-9_.]+ import .+|import [A-Za-z0-9_.]+)$\""
              , "[check]", "command = [\"true\"]", "format = \"pyright-json\""
              , "[holes.expr]", "placeholder = 'todo(\"olifant: {= hole =}\")'", "wrap = \"h = {= fill =}\"" ]
        gen = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"python\""
          , "[[edits]]", "create = \"src/d.py\"", "text = \"# a header\\n\\\"\\\"\\\"Days.\\n\\nMore.\\n\\\"\\\"\\\"\\n\\nTODAY = {= ? today: expr | today =}\\n\"" ]
        filler = Filler $ \_ -> pure (Right (M.fromList [("today", "import datetime\ndatetime.date.today()")]))
        tools = (basicTools noSyntax compiles [pyLang]) {toolFiller = Just (ModelFiller "fake" 0 filler)}
    (ws, snap) <- memoryWorkspace []
    Right _ <- generate ws noResolve tools [Step gen M.empty] now Apply
    (M.lookup "src/d.py" <$> snap) `shouldReturn` Just "# a header\n\"\"\"Days.\n\nMore.\n\"\"\"\n\nimport datetime\n\nTODAY = datetime.date.today()\n"
  it "(h) refuses a fill that escapes its hole or is the placeholder itself (review I4)" $ do
    (tools, _) <- withFiller (replicate 3 (Right (M.fromList [("a", "1 } evil = { 2"), ("b", "Debug.todo \"olifant: b\"")])))
    (plan, files) <- run1 tools
    M.lookup "src/A.elm" files `shouldBe` Just "a = Debug.todo \"olifant: a\"\n"
    statuses plan `shouldBe` [("a", HoleUnfilled "the fill reaches outside its hole (unbalanced brackets)"), ("b", HoleUnfilled "the fill is the placeholder, not code")]
  it "(i) blames the fill that breaks another file, not the hole in that file (review I5)" $ do
    let lim = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"elm\""
          , "[[edits]]", "create = \"src/Lim.elm\"", "text = \"limit = {= ? lim: expr | the limit =}\\n\""
          , "[[edits]]", "create = \"src/UseLim.elm\"", "text = \"step = Lim.limit + {= ? step: expr | the step =}\\n\"" ]
        -- UseLim breaks when Lim's limit is a string.
        typed = Checker $ \_ files -> pure (Right (CheckReport [] [("src/UseLim.elm", "TYPE MISMATCH") | maybe False ("\"ten\"" `T.isInfixOf`) (M.lookup "src/Lim.elm" files)] []))
    script <- newIORef (replicate 3 (Right (M.fromList [("lim", "\"ten\""), ("step", "1")])))
    let filler = Filler (\_ -> atomicModifyIORef' script (\case [] -> ([], Left "no more"); (r : rs) -> (rs, r)))
        tools = (basicTools parses typed [holeLang]) {toolFiller = Just (ModelFiller "fake" 2 filler)}
    (ws, snap) <- memoryWorkspace []
    Right plan <- generate ws noResolve tools [Step lim M.empty] now Apply
    statuses plan `shouldBe` [("lim", HoleUnfilled "src/UseLim.elm: TYPE MISMATCH"), ("step", HoleFilled "fake" 1)]
    (M.lookup "src/Lim.elm" <$> snap) `shouldReturn` Just "limit = Debug.todo \"olifant: lim\"\n"
  it "(j) says so when a fill could not be compile-checked (review I6)" $ do
    let missingTool = Checker $ \_ _ -> pure (Left (CheckToolMissing "elm is not installed"))
    script <- newIORef [Right (M.fromList [("a", "1"), ("b", "2")])]
    let filler = Filler (\_ -> atomicModifyIORef' script (\case [] -> ([], Left "no more"); (r : rs) -> (rs, r)))
        tools = (basicTools parses missingTool [holeLang]) {toolFiller = Just (ModelFiller "fake" 2 filler)}
    (plan, _) <- run1 tools
    planWarnings plan `shouldSatisfy` any (T.isInfixOf "not compile-checked: elm is not installed")
  it "(k) retries a reply that could not be read (review I7)" $ do
    (tools, calls) <- withFiller [Left "the reply is not JSON", Right (M.fromList [("a", "1"), ("b", "2")])]
    (plan, _) <- run1 tools
    statuses plan `shouldBe` [("a", HoleFilled "fake" 2), ("b", HoleFilled "fake" 2)]
    length <$> readIORef calls `shouldReturn` 2
  it "(l) reports progress: each request, each fill, each check (for editors)" $ do
    (tools0, _) <- withFiller [Right (M.fromList [("a", "BAD"), ("b", "2")]), Right (M.fromList [("a", "7")])]
    events <- newIORef []
    let tools = tools0 {toolProgress = \e -> modifyIORef' events (++ [e])}
    _ <- run1 tools
    seen <- readIORef events
    [e | e <- seen, not (isChecking e)] `shouldBe` [Filling ["a", "b"] 1 "fake", Filled "b" 1, Filling ["a"] 2 "fake", Filled "a" 2]
    [l | Checking l <- seen] `shouldSatisfy` (\ls -> not (null ls) && all (== "elm") ls)
  it "(f) retries a fill that does not parse as its kind" $ do
    (tools, calls) <- withFiller [Right (M.fromList [("a", "!!"), ("b", "2")]), Right (M.fromList [("a", "3")])]
    (plan, _) <- run1 tools
    statuses plan `shouldBe` [("a", HoleFilled "fake" 2), ("b", HoleFilled "fake" 1)]
    reqs <- readIORef calls
    map (fmap snd . fhPrevious) (frHoles (reqs !! 1)) `shouldBe` [Just "does not parse as expr"]

completeSpec :: Spec
completeSpec = describe "generate (complete edits)" $ do
  let page = "module Page.A exposing (..)\n\nupdate msg model =\n    case msg of\n        NoOp ->\n            ( model, Cmd.none )\n"
      gen = generatorFrom . T.unlines $
        [ "description = \"d\"", "language = \"elm\"", "[inputs.name]", "type = \"ident\""
        , "[[edits]]", "complete = \"{= name|pascal =}\"", "text = \"{= pattern =} ->\\n    ( model, Cmd.none )\"" ]
      go files rounds = do
        ref <- newIORef rounds
        (ws, snapshot) <- memoryWorkspace files
        r <- generate ws noResolve (basicTools noSyntax (scriptedChecker ref) [elmLang]) [Step gen (M.fromList [("name", "reset")])] now Apply
        (r,) <$> snapshot
  it "adds a branch after the case the compiler reports" $ do
    (Right plan, files) <- go [("src/A.elm", page)] [[missing "src/A.elm" (3, 5) ["Reset"]]]
    M.lookup "src/A.elm" files `shouldBe` Just (page <> "\n        Reset ->\n            ( model, Cmd.none )\n")
    map editSite (planEdits plan) `shouldBe` [Just (Site "src/A.elm" "update" "Reset" False)]
  it "reports each compiler check as progress" $ do
    ref <- newIORef [[missing "src/A.elm" (3, 5) ["Reset"]]]
    events <- newIORef []
    (ws, _) <- memoryWorkspace [("src/A.elm", page)]
    let tools = (basicTools noSyntax (scriptedChecker ref) [elmLang]) {toolProgress = \e -> modifyIORef' events (++ [e])}
    _ <- generate ws noResolve tools [Step gen (M.fromList [("name", "reset")])] now Apply
    readIORef events >>= (`shouldSatisfy` elem (Checking "elm"))
  it "records the previous text of a file only the compiler found (for editors' previews)" $ do
    (Right plan, _) <- go [("src/A.elm", page)] [[missing "src/A.elm" (3, 5) ["Reset"]]]
    M.lookup "src/A.elm" (planBefore plan) `shouldBe` Just (Just page)
  it "warns about a site it cannot extend (a function equation) that needs one of its constructors" $ do
    let unplaced = Checker $ \_ _ -> pure (Right (CheckReport [] [] [(missing "src/A.elm" (7, 9) ["Reset"], "an equation for `reset`")]))
    (ws, _) <- memoryWorkspace [("src/A.elm", page)]
    Right plan <- generate ws noResolve (basicTools noSyntax unplaced [elmLang]) [Step gen (M.fromList [("name", "reset")])] now Apply
    planWarnings plan `shouldSatisfy` any (\w -> "src/A.elm" `T.isInfixOf` w && "reset" `T.isInfixOf` w && "Reset" `T.isInfixOf` w)
    planConflicts plan `shouldBe` []
  it "copies the spacing between the case's alternatives: none when they are adjacent" $ do
    let tight = "module Page.A exposing (..)\n\nupdate msg model =\n    case msg of\n        NoOp ->\n            ( model, Cmd.none )\n        Other ->\n            ( model, Cmd.none )\n"
    (Right _, files) <- go [("src/A.elm", tight)] [[missing "src/A.elm" (3, 7) ["Reset"]]]
    M.lookup "src/A.elm" files `shouldBe` Just (tight <> "        Reset ->\n            ( model, Cmd.none )\n")
  it "leaves todo branches at every site when the complete edit has no body" $ do
    let bodiless = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"elm\"", "[inputs.name]", "type = \"ident\""
          , "[[edits]]", "complete = \"{= name|pascal =}\"" ]
    ref <- newIORef [[missing "src/A.elm" (3, 5) ["Reset"]]]
    (ws, snapshot) <- memoryWorkspace [("src/A.elm", page)]
    Right plan <- generate ws noResolve (basicTools noSyntax (scriptedChecker ref) [elmLang]) [Step bodiless (M.fromList [("name", "reset")])] now Apply
    fmap (fmap siteTodo . editSite) (planEdits plan) `shouldBe` [Just True]
    (M.lookup "src/A.elm" <$> snapshot) `shouldReturn` Just (page <> "\n        Reset ->\n            Debug.todo \"olifant: update Reset\"\n")
  it "warns, never drops silently, when a site needs our constructor but none of our edits is visible there (review C2)" $ do
    let mainGen = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"elm\"", "[inputs.name]", "type = \"ident\""
          , "[[edits]]", "complete = \"Main.{= name|pascal =}\"", "text = \"{= pattern =} -> 1\"" ]
    ref <- newIORef [[missing "src/A.elm" (3, 5) ["Reset"]]]
    (ws, _) <- memoryWorkspace [("src/A.elm", page)]
    Right plan <- generate ws noResolve (basicTools noSyntax (scriptedChecker ref) [elmLang]) [Step mainGen (M.fromList [("name", "reset")])] now Apply
    planEdits plan `shouldBe` []
    planWarnings plan `shouldSatisfy` any (\w -> "src/A.elm" `T.isInfixOf` w && "Reset" `T.isInfixOf` w && "Main.Reset" `T.isInfixOf` w)
  it "leaves a placeholder todo where no `where` matches" $ do
    let gen' = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"elm\"", "[inputs.name]", "type = \"ident\""
          , "[[edits]]", "complete = \"{= name|pascal =}\"", "where = \"case in view\"", "text = \"{= pattern =} -> 1\"" ]
    ref <- newIORef [[missing "src/A.elm" (3, 5) ["Reset"]]]
    (ws, snapshot) <- memoryWorkspace [("src/A.elm", page)]
    Right plan <- generate ws noResolve (basicTools noSyntax (scriptedChecker ref) [elmLang]) [Step gen' (M.fromList [("name", "reset")])] now Apply
    fmap (fmap siteTodo . editSite) (planEdits plan) `shouldBe` [Just True]
    (M.lookup "src/A.elm" <$> snapshot) `shouldReturn` Just (page <> "\n        Reset ->\n            Debug.todo \"olifant: update Reset\"\n")
  it "ignores diagnostics about constructors it did not introduce" $ do
    (Right plan, files) <- go [("src/A.elm", page)] [[missing "src/A.elm" (3, 5) ["Other"]]]
    (planEdits plan, M.lookup "src/A.elm" files) `shouldBe` ([], Just page)
  it "is a conflict when the compiler keeps reporting the same site (Review Focus 3)" $ do
    let same = [missing "src/A.elm" (3, 5) ["Reset"]]
    (Right plan, _) <- go [("src/A.elm", page)] [same, same]
    map describeConflict (planConflicts plan) `shouldSatisfy` any ("still reports Reset" `T.isInfixOf`)
  it "is a conflict naming the tool when the check cannot run" $ do
    (ws, _) <- memoryWorkspace [("src/A.elm", page)]
    Right plan <- generate ws noResolve (basicTools noSyntax (Checker (\_ _ -> pure (Left (CheckToolMissing "`elm` is not installed")))) [elmLang]) [Step gen (M.fromList [("name", "reset")])] now Apply
    map conflictToolMissing (planConflicts plan) `shouldBe` [True]
  it "chooses the body by `where` when two edits complete the same bare name (Review Focus 2)" $ do
    -- `where` needs Syntax; a fake one maps each target phrase to the case it names.
    let mainElm = "module Main exposing (main)\nimport Route exposing (Route)\n\nchange r =\n    case r of\n        Nothing ->\n            0\n\nview p =\n    case p of\n        Home ->\n            1\n"
        syntax = Syntax $ \_ rule _ -> pure . Right $
          if "change" `T.isInfixOf` T.pack (show rule) then [Match (4, 4) (6, 13) "case r of"] else [Match (9, 4) (11, 13) "case p of"]
        two = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"elm\""
          , "[[edits]]", "complete = \"Route.Settings\"", "where = \"case in change\"", "text = \"{= pattern =} ->\\n    2\""
          , "[[edits]]", "complete = \"Settings\"", "where = \"case in view\"", "text = \"Settings _ ->\\n    3\"" ]
        Right withTargets = parseLanguage "elm" "/p" $ T.unlines
          [ "files = [\"**/*.elm\"]", "comment = \"--\"", "grammar = \"elm\""
          , "placeholder = 'Debug.todo \"{= message =}\"'", "branch = \"{= pattern =} ->\\n    {= body =}\""
          , "[check]", "command = [\"true\"]", "format = \"elm-json\""
          , "[targets.case_in]", "phrase = \"case in FUNCTION\"", "container = { kind = \"case_of_expr\", regex = \"{= FUNCTION =}\" }", "items = { kind = \"case_of_branch\" }", "example = \"end of case in update\"" ]
    ref <- newIORef [[missing "src/Main.elm" (9, 11) ["Settings _"], missing "src/Main.elm" (4, 6) ["Just Settings"]]]
    (ws, snapshot) <- memoryWorkspace [("src/Main.elm", mainElm)]
    Right _ <- generate ws noResolve (basicTools syntax (scriptedChecker ref) [withTargets]) [Step two M.empty] now Apply
    Just out <- M.lookup "src/Main.elm" <$> snapshot
    out `shouldSatisfy` ("        Just Route.Settings ->\n            2" `T.isInfixOf`)
    out `shouldSatisfy` ("        Settings _ ->\n            3" `T.isInfixOf`)
  it "is a conflict, not a guess, when an unmatched site could be either constructor (review)" $ do
    let mainElm = "module Main exposing (main)\nimport Route exposing (Route)\n\npageName p =\n    case p of\n        Home ->\n            \"home\"\n"
        syntax = Syntax $ \_ _ _ -> pure (Right [])
        two =
          generatorFrom . T.unlines $
            [ "description = \"d\"", "language = \"elm\""
            , "[[edits]]", "complete = \"Route.Settings\"", "where = \"case in change\"", "text = \"{= pattern =} ->\\n    2\""
            , "[[edits]]", "complete = \"Settings\"", "where = \"case in view\"", "text = \"Settings _ ->\\n    3\""
            ]
    ref <- newIORef [[missing "src/Main.elm" (4, 6) ["Settings _"]]]
    (ws, snapshot) <- memoryWorkspace [("src/Main.elm", mainElm)]
    Right plan <- generate ws noResolve (basicTools syntax (scriptedChecker ref) [elmLang]) [Step two M.empty] now Apply
    map describeConflict (planConflicts plan) `shouldSatisfy` any (\c -> "Route.Settings" `T.isInfixOf` c && "pageName" `T.isInfixOf` c)
    (M.lookup "src/Main.elm" <$> snapshot) `shouldReturn` Just mainElm
  it "warns when the project does not compile, so cases could not be checked (review)" $ do
    (ws, _) <- memoryWorkspace [("src/A.elm", page), ("src/B.elm", "module B\n")]
    Right plan <- generate ws noResolve (basicTools noSyntax (brokenChecker [("src/B.elm", "NAMING ERROR")]) [elmLang]) [Step gen (M.fromList [("name", "reset")])] now Apply
    planConflicts plan `shouldBe` []
    planWarnings plan `shouldSatisfy` any ("src/B.elm" `T.isInfixOf`)
  it "is a conflict when a file the plan changed does not compile (review)" $ do
    let broken = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"elm\"", "[inputs.name]", "type = \"ident\""
          , "[[edits]]", "insert = \"src/A.elm\"", "at = \"after first line matching ^module\"", "text = \"oops\""
          , "[[edits]]", "complete = \"{= name|pascal =}\"", "text = \"{= pattern =} -> 1\"" ]
    (ws, snapshot) <- memoryWorkspace [("src/A.elm", page)]
    -- the file breaks with the plan's `oops`, not before it
    let oops = Checker $ \_ files -> pure (Right (CheckReport [] [("src/A.elm", "SYNTAX PROBLEM") | maybe False ("oops" `T.isInfixOf`) (M.lookup "src/A.elm" files)] []))
    Right plan <- generate ws noResolve (basicTools noSyntax oops [elmLang]) [Step broken (M.fromList [("name", "reset")])] now Apply
    map describeConflict (planConflicts plan) `shouldSatisfy` any (\c -> "src/A.elm" `T.isInfixOf` c && "SYNTAX PROBLEM" `T.isInfixOf` c)
    (M.lookup "src/A.elm" <$> snapshot) `shouldReturn` Just page
  it "narrows candidates to constructors visible at the site (Route cannot see Main)" $ do
    let routeElm = "module Route exposing (..)\n\ntoPath r =\n    case r of\n        Home ->\n            \"/\"\n"
        syntax = Syntax $ \_ _ _ -> pure (Right [])
        two =
          generatorFrom . T.unlines $
            [ "description = \"d\"", "language = \"elm\""
            , "[[edits]]", "complete = \"Route.Settings\"", "where = \"case in change\"", "text = \"{= pattern =} ->\\n    2\""
            , "[[edits]]", "complete = \"Main.Settings\"", "where = \"case in view\"", "text = \"Settings _ ->\\n    3\""
            ]
    ref <- newIORef [[missing "src/Route.elm" (3, 5) ["Settings"]]]
    (ws, snapshot) <- memoryWorkspace [("src/Route.elm", routeElm)]
    Right plan <- generate ws noResolve (basicTools syntax (scriptedChecker ref) [elmLang]) [Step two M.empty] now Apply
    planConflicts plan `shouldBe` []
    map (fmap siteTodo . editSite) (planEdits plan) `shouldBe` [Just True]
    (M.lookup "src/Route.elm" <$> snapshot) `shouldReturn` Just (routeElm <> "\n        Settings ->\n            Debug.todo \"olifant: toPath Settings\"\n")
  it "numbers compiler branches in the order they are applied, so lines replay (fix pass)" $ do
    let two = "module A exposing (..)\n\nf m =\n    case m of\n        A ->\n            1\n\ng m =\n    case m of\n        A ->\n            2\n"
    ref <- newIORef [[missing "src/A.elm" (3, 5) ["Reset"], missing "src/A.elm" (8, 10) ["Reset"]]]
    (ws, _) <- memoryWorkspace [("src/A.elm", two)]
    Right plan <- generate ws noResolve (basicTools noSyntax (scriptedChecker ref) [elmLang]) [Step gen (M.fromList [("name", "reset")])] now Apply
    [(editNumber e, siteFunction s', l) | e <- planEdits plan, Just s' <- [editSite e], WillInsert l _ <- [editAction e]]
      `shouldBe` [(1, "g", 13), (2, "f", 8)]
  it "re-running a create whose cases the compiler completed is a no-op (fix pass)" $ do
    let pageGen =
          generatorFrom . T.unlines $
            [ "description = \"d\"", "language = \"elm\"", "[inputs.name]", "type = \"ident\""
            , "[[edits]]", "create = \"src/A.elm\"", "text = \"update msg =\\n    case msg of\\n        NoOp ->\\n            1\\n\""
            , "[[edits]]", "complete = \"{= name|pascal =}\"", "text = \"{= pattern =} ->\\n    2\"" ]
        -- Reports the case in src/A.elm as missing Reset until a Reset branch exists.
        honest = Checker $ \_ files -> pure . Right $
          CheckReport [missing "src/A.elm" (1, 3) ["Reset"] | Just t <- [M.lookup "src/A.elm" files], not ("Reset ->" `T.isInfixOf` t)] [] []
        runTwice files = do
          (ws, snap) <- memoryWorkspace files
          Right plan <- generate ws noResolve (basicTools noSyntax honest [elmLang]) [Step pageGen (M.fromList [("name", "reset")])] now Apply
          (plan,) <$> snap
    (_, once) <- runTwice []
    M.lookup "src/A.elm" once `shouldBe` Just "update msg =\n    case msg of\n        NoOp ->\n            1\n\n        Reset ->\n            2\n"
    (again, twice) <- runTwice (M.toList once)
    planConflicts again `shouldBe` []
    twice `shouldBe` once
    -- The compiler's label is not an anchor, and must not leak as one (JSON anchor_used).
    [a | e <- planEdits again, Just a <- [editAnchor e], "compiler:" `T.isPrefixOf` a] `shouldBe` []

regionSpec :: Spec
regionSpec = describe "generate (owned regions)" $ do
  let Right item = parseTemplate "{= dotted =}"
      mods = RegionDef "mods" "x.cabal" (GlobQuery "src/**/*.hs" []) (ItemTemplate item) False (Just "--") Nothing Nothing
      tools = (noTools :: Tools IO) {toolRegions = [mods]}
      cabal = "  exposed-modules:\n    -- olifant:region mods\n    -- olifant:end mods\n"
      unit = generatorFrom . (header <>) . T.unlines $ ["[[edits]]", "create = \"src/Olifant/{= name|pascal =}.hs\"", "text = \"module X\\n\""]
  it "lists a module the same run creates, and re-running is a no-op (Review Focus 1)" $ do
    (ws, snap) <- memoryWorkspace [("x.cabal", cabal), ("src/Olifant/A.hs", "")]
    Right plan <- generate ws noResolve tools [Step unit (M.fromList [("name", "b")])] now Apply
    [(r, a) | WillRefresh r a _ _ <- map editAction (planEdits plan)] `shouldBe` [("mods", 2)]
    Just out <- M.lookup "x.cabal" <$> snap
    out `shouldSatisfy` ("    Olifant.A\n    Olifant.B\n    -- olifant:end mods" `T.isInfixOf`)
    Right again <- generate ws noResolve tools [Step unit (M.fromList [("name", "b")])] now Apply
    planWrites again `shouldBe` M.empty
  it "re-runs an AST query for a later region after an earlier one rewrote the file it reads" $ do
    let Right n = parseTemplate "{= n =}"
        q = AstQueryQ (AstQuery "f.txt" (ItemsOf "type T") [])
        lineCount = Queries (\_ -> pure (Left (failed "no"))) (\_ _ view -> fmap (\c -> Right [M.fromList [("n", VText (maybe "?" (T.pack . show . length . T.lines) c))]]) (view "f.txt")) (\_ -> pure (Right []))
        region name = RegionDef name "f.txt" q (ItemTemplate n) False (Just "--") Nothing Nothing
        both = (noTools :: Tools IO) {toolRegions = [region "a", region "b"], toolQueries = lineCount}
    (ws, snap) <- memoryWorkspace [("f.txt", "-- olifant:region a\n-- olifant:end a\n-- olifant:region b\n-- olifant:end b\n")]
    Right _ <- generate ws noResolve both [Step unit (M.fromList [("name", "b")])] now Apply
    Just out <- M.lookup "f.txt" <$> snap
    out `shouldSatisfy` ("\n4\n-- olifant:end a\n" `T.isInfixOf`)
    out `shouldSatisfy` ("\n5\n-- olifant:end b\n" `T.isInfixOf`)
  it "stays fast with many files outside the region's glob (review #1)" $ do
    (ws, _) <- memoryWorkspace (("x.cabal", cabal) : [("node_modules/pkg" <> show i <> "/index.js", "") | i <- [1 :: Int .. 30000]])
    result <- timeout 5000000 (generate ws noResolve tools [Step unit (M.fromList [("name", "b")])] now Apply)
    fmap (either (const False) (not . hasConflicts)) result `shouldBe` Just True
  describe "families: one region per matching file (spec §5.9)" $ do
    let Right modItem = parseTemplate "pub mod {= stem =};"
        family = RegionDef "modules" "*/*-service/src/lib.rs" (GlobQuery "{= dir =}/*.rs" ["{= dir =}/lib.rs"]) (ItemTemplate modItem) False (Just "//") Nothing Nothing
        famTools = (noTools :: Tools IO) {toolRegions = [family]}
        lib = "// olifant:region modules\n// olifant:end modules\n"
        files = [ ("billing/billing-service/src/lib.rs", lib), ("billing/billing-service/src/cancel.rs", ""), ("billing/billing-service/src/void.rs", "")
                , ("mail/mail-service/src/lib.rs", lib), ("mail/mail-service/src/send.rs", "") ]
        noop = generatorFrom (header <> "[[edits]]\ncreate = \"x/{= name =}.txt\"\ntext = \"x\"\n")
    it "(a, b) refreshes each matching file from its own directory" $ do
      (ws, snap) <- memoryWorkspace files
      Right _ <- generate ws noResolve famTools [Step noop (M.fromList [("name", "n")])] now Apply
      out <- snap
      let body f = fmap (T.unlines . drop 1 . T.lines) (M.lookup f out)
      body "billing/billing-service/src/lib.rs" `shouldBe` Just "pub mod cancel;\npub mod void;\n// olifant:end modules\n"
      body "mail/mail-service/src/lib.rs" `shouldBe` Just "pub mod send;\n// olifant:end modules\n"
      fmap (T.isInfixOf "generated from glob mail/mail-service/src/*.rs") (M.lookup "mail/mail-service/src/lib.rs" out) `shouldBe` Just True
    it "I5: gen warns when it creates a file next to a list that is not set up" $ do
      (ws, _) <- memoryWorkspace [("billing/billing-service/src/lib.rs", "pub mod cancel;\n"), ("billing/billing-service/src/cancel.rs", "")]
      let creator = generatorFrom (header <> "[[edits]]\ncreate = \"billing/billing-service/src/{= name =}.rs\"\ntext = \"x\"\n")
      Right plan <- generate ws noResolve famTools [Step creator (M.fromList [("name", "pay")])] now PlanOnly
      planWarnings plan `shouldSatisfy` any (\w -> "not set up in billing/billing-service/src/lib.rs" `T.isInfixOf` w && "sync --init modules" `T.isInfixOf` w)
    it "(c) reports drift per file" $ do
      (ws, _) <- memoryWorkspace files
      (edits, _, _) <- refreshRegions ReportUnset famTools ws 1 M.empty
      [(editPath e, r) | e <- edits, WillRefresh r _ _ _ <- [editAction e]] `shouldBe` [("billing/billing-service/src/lib.rs", "modules"), ("mail/mail-service/src/lib.rs", "modules")]
    it "(d) a matching file without fences is not set up — never rewritten (Review Focus 3)" $ do
      (ws, _) <- memoryWorkspace [("billing/billing-service/src/lib.rs", "pub mod cancel;\n"), ("billing/billing-service/src/cancel.rs", "")]
      (edits, conflicts, _) <- refreshRegions ReportUnset famTools ws 1 M.empty
      edits `shouldBe` []
      map describeConflict conflicts `shouldSatisfy` any (\c -> "billing/billing-service/src/lib.rs" `T.isInfixOf` c && "sync --init modules" `T.isInfixOf` c)
  it "never writes a region outside the project (review #3)" $ do
    let Right t = parseTemplate "{= dotted =}"
        escape = RegionDef "esc" "../escaped.txt" (GlobQuery "src/**/*.hs" []) (ItemTemplate t) True (Just "#") Nothing Nothing
    (ws, snap) <- memoryWorkspace [("src/Olifant/A.hs", "")]
    Right plan <- generate ws noResolve (noTools {toolRegions = [escape]}) [Step unit (M.fromList [("name", "b")])] now Apply
    map describeConflict (planConflicts plan) `shouldSatisfy` any ("outside the project" `T.isInfixOf`)
    (M.member "../escaped.txt" <$> snap) `shouldReturn` False
  it "does not create a whole-file region's file on an unrelated gen (review #4)" $ do
    let Right t = parseTemplate "{= dotted =}"
        whole = RegionDef "pages" "Pages.txt" (GlobQuery "src/**/*.hs" []) (ItemTemplate t) True (Just "#") Nothing Nothing
    (ws, snap) <- memoryWorkspace [("src/Olifant/A.hs", "")]
    Right plan <- generate ws noResolve (noTools {toolRegions = [whole]}) [Step unit (M.fromList [("name", "b")])] now Apply
    planConflicts plan `shouldBe` []
    (M.member "Pages.txt" <$> snap) `shouldReturn` False
  it "refreshes a region from an AST query" $ do
    let Right t = parseTemplate "{= head =}"
        Right caseLang =
          parseLanguage "elm" "/p" . T.unlines $
            [ "files = [\"**/*.elm\"]", "comment = \"--\"", "grammar = \"elm\""
            , "[targets.case_in]", "phrase = \"case in FUNCTION\"", "container = { kind = \"case_of_expr\" }"
            , "items = { kind = \"case_of_branch\" }", "example = \"end of case in update\""
            ]
        variants = RegionDef "variants" "list.txt" (AstQueryQ (AstQuery "src/A.elm" (ItemsOf "case in update") [("head", "^([A-Z]+)")])) (ItemTemplate t) False (Just "#") Nothing Nothing
        fake = Syntax $ \_ rule _ -> pure . Right $
          if "\"all\"" `isInfixOf` BL.unpack (encode rule) then [Match (3, 8) (4, 1) "A -> 1", Match (7, 8) (8, 1) "B -> 2"] else [Match (2, 4) (8, 1) "case"]
    (ws, snap) <- memoryWorkspace [("src/A.elm", "x"), ("list.txt", "# olifant:region variants\n# olifant:end variants\n")]
    let queries = noQueries {queryAst = astQueries fake [caseLang]}
    Right _ <- generate ws noResolve (noTools {toolRegions = [variants], toolQueries = queries}) [Step unit (M.fromList [("name", "b")])] now Apply
    (M.lookup "list.txt" <$> snap) `shouldReturn` Just "# olifant:region variants (generated from ast src/A.elm (items of case in update); edit the declaration, not this block)\nA\nB\n# olifant:end variants\n"
  it "an AST region sees what the same plan inserted" $ do
    let Right t = parseTemplate "{= head =}"
        Right lang = parseLanguage "x" "/p" "files = [\"**/*.x\"]\ncomment = \"--\"\ngrammar = \"x\"\n[targets.type]\nphrase = \"type NAME\"\ncontainer = { kind = \"t\" }\nitems = { kind = \"v\" }\nexample = \"end of type T\"\n"
        -- The container is the whole text; items are the lines holding "| ".
        bars = Syntax $ \_ rule content -> pure . Right $
          if "\"all\"" `isInfixOf` BL.unpack (encode rule)
            then [Match (i, 2) (i, T.length l) (T.strip l) | (i, l) <- zip [0 ..] (T.lines content), "| " `T.isInfixOf` l]
            else [Match (0, 0) (length (T.lines content), 0) content]
        variants = RegionDef "variants" "list.txt" (AstQueryQ (AstQuery "a.x" (ItemsOf "type T") [("head", "[|] ([A-Z])")])) (ItemTemplate t) False (Just "#") Nothing Nothing
        addC = generatorFrom . (header <>) . T.unlines $ ["[[edits]]", "insert = \"a.x\"", "at = \"after last line matching [|] \"", "text = \"| C\""]
    (ws, snap) <- memoryWorkspace [("a.x", "type T\n  = A\n  | B\n"), ("list.txt", "# olifant:region variants\n# olifant:end variants\n")]
    let barTools = noTools {toolRegions = [variants], toolLanguages = [lang], toolQueries = noQueries {queryAst = astQueries bars [lang]}}
    Right _ <- generate ws noResolve barTools [Step addC (M.fromList [("name", "b")])] now Apply
    (M.lookup "list.txt" <$> snap) `shouldReturn` Just "# olifant:region variants (generated from ast a.x (items of type T); edit the declaration, not this block)\nB\nC\n# olifant:end variants\n"
  it "skips a region whose file or fences are not set up here" $ do
    (ws, _) <- memoryWorkspace [("src/Olifant/A.hs", "")]
    Right plan <- generate ws noResolve tools [Step unit (M.fromList [("name", "b")])] now Apply
    planConflicts plan `shouldBe` []
  it "is a conflict when a fence is broken" $ do
    (ws, _) <- memoryWorkspace [("x.cabal", "    -- olifant:region mods\n")]
    Right plan <- generate ws noResolve tools [Step unit (M.fromList [("name", "b")])] now Apply
    map describeConflict (planConflicts plan) `shouldSatisfy` any ("has no end" `T.isInfixOf`)

isChecking :: ProgressEvent -> Bool
isChecking = \case
  Checking _ -> True
  _ -> False

parseCheckSpec :: Spec
parseCheckSpec = describe "parse pre-check (spec §4.6)" $ do
  let -- `;;` is a syntax error; the ERROR node is the `;;` itself.
      errors = Syntax $ \_ rule content -> pure . Right $
        if rule == object ["kind" .= ("ERROR" :: Text)]
          then [Match (i, c) (i, c + 2) ";;" | (i, l) <- zip [0 ..] (T.lines content), Just c <- [T.length . fst <$> nonEmpty (T.breakOn ";;" l)]]
          else []
      nonEmpty (a, b) = if T.null b then Nothing else Just (a, b)
      tools checker = basicTools errors checker [elmLang]
      insertGen = generatorFrom . T.unlines $
        [ "description = \"d\"", "language = \"elm\"", "[[edits]]", "insert = \"src/A.elm\"", "at = \"end of file\"", "text = \"y = 2\"" ]
      warned = "src/A.elm:2 does not parse (near `;;`) — this plan may place code wrongly, and checks will fail"
      planOver checker files gen = do
        (ws, _) <- memoryWorkspace files
        Right plan <- generate ws noResolve (tools checker) [Step gen M.empty] now PlanOnly
        pure plan
  it "(a) warns, first, about a touched file that does not parse; the plan still stands" $ do
    plan <- planOver noChecker [("src/A.elm", "module A exposing (..)\nx = 1;;\n")] insertGen
    take 1 (planWarnings plan) `shouldBe` [warned]
    planConflicts plan `shouldBe` []
    length (planEdits plan) `shouldBe` 1
  it "(b) says nothing about a broken file the plan does not touch (Review Focus 3)" $ do
    plan <- planOver noChecker [("src/A.elm", "module A exposing (..)\n"), ("src/B.elm", "b = 1;;\n")] insertGen
    planWarnings plan `shouldBe` []
  it "(c) does not pre-check a file the plan creates (Review Focus 3)" $ do
    let createBroken = generatorFrom . T.unlines $ ["description = \"d\"", "language = \"elm\"", "[[edits]]", "create = \"src/C.elm\"", "text = \"c = 1;;\\n\""]
    plan <- planOver noChecker [] createBroken
    planWarnings plan `shouldBe` []
  it "(d) checks a file only the compiler found" $ do
    let page = "module Page.A exposing (..)\n\nupdate msg model =;;\n    case msg of\n        NoOp ->\n            ( model, Cmd.none )\n"
        gen = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"elm\"", "[[edits]]", "complete = \"Reset\"", "text = \"{= pattern =} ->\\n    ( model, Cmd.none )\"" ]
    ref <- newIORef [[missing "src/P.elm" (3, 5) ["Reset"]]]
    plan <- planOver (scriptedChecker ref) [("src/P.elm", page)] gen
    take 1 (planWarnings plan) `shouldBe` ["src/P.elm:3 does not parse (near `;;`) — this plan may place code wrongly, and checks will fail"]
  it "(g) warns about a named file even when every edit to it conflicted (review I7)" $ do
    let anchored = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"elm\"", "[[edits]]", "insert = \"src/A.elm\"", "at = \"end of type Msg\"", "text = \"| Reset\"" ]
    plan <- planOver noChecker [("src/A.elm", "module A exposing (..)\nx = 1;;\n")] anchored
    planConflicts plan `shouldNotBe` []
    take 1 (planWarnings plan) `shouldBe` [warned]
  it "(f) a missing check tool's conflict suggests olifant doctor (spec §4.7)" $
    describeConflict (CompleteFailed ["g"] "`cargo` is not installed (needed to check rust)" True) `shouldSatisfy` T.isSuffixOf "; run `olifant doctor`"
  it "(e) points the compile warning for that file back at the parse warning" $ do
    let both = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"elm\""
          , "[[edits]]", "insert = \"src/A.elm\"", "at = \"end of file\"", "text = \"y = 2\""
          , "[[edits]]", "complete = \"Reset\"", "text = \"{= pattern =} -> 0\"" ]
        broken = Checker $ \_ _ -> pure (Right (CheckReport [] [("src/A.elm", "unexpected ;;")] []))
    plan <- planOver broken [("src/A.elm", "module A exposing (..)\nx = 1;;\n")] both
    take 1 (planWarnings plan) `shouldBe` [warned]
    -- The touched file was broken before the plan: not the plan's doing
    -- (python review I1), so a warning that the cases could not be checked,
    -- pointing back at the parse warning — never a guess, never a block.
    map describeConflict (planConflicts plan) `shouldBe` []
    planWarnings plan `shouldSatisfy` any (\w -> "could not check for missing cases" `T.isInfixOf` w && "(see the parse warning above)" `T.isSuffixOf` w)

replaceSpec :: Spec
replaceSpec = describe "replace edits (rule fixes, spec §5.9)" $ do
  let tools = basicTools noSyntax noChecker [holeLang]
      file = "src/A.elm"
      replacing old new = replaceGenerator file (2, 2) old (either (error . show) id (parseTemplate new))
      runReplace files gen = do
        (ws, snap) <- memoryWorkspace files
        Right plan <- generate ws noResolve tools [Step gen M.empty] now Apply
        (plan,) . M.lookup file <$> snap
  it "replaces the lines with the text, holes included" $ do
    (plan, out) <- runReplace [(file, "a\nold line\nc\n")] (replacing "old line" "  x = {= ? fixed: expr | the new value =}")
    out `shouldBe` Just "a\n  x = Debug.todo \"olifant: fixed\"\nc\n"
    [(l, n) | WillReplace l n _ <- map editAction (planEdits plan)] `shouldBe` [(2, 1)]
  it "deletes the lines when the text is empty" $ do
    (_, out) <- runReplace [(file, "a\nold line\nc\n")] (replacing "old line" "")
    out `shouldBe` Just "a\nc\n"
  it "refuses when the lines are no longer what the fix saw" $ do
    (ws, _) <- memoryWorkspace [(file, "a\nsomething else\nc\n")]
    Right plan <- generate ws noResolve tools [Step (replacing "old line" "y") M.empty] now PlanOnly
    map describeConflict (planConflicts plan) `shouldSatisfy` any ("changed since the rule saw it" `T.isInfixOf`)
