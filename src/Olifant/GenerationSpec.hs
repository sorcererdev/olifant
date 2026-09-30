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
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Olifant.Language
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
  unusableSpec
  composeSpec

createSpec :: Spec
createSpec = describe "generate (create edits)" $ do
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
    Right plan <- generate ws noResolve (Tools noSyntax noChecker [elm]) [Step gen' (M.fromList [("name", "b")])] now Apply
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
scriptedChecker ref = Checker $ \_ _ -> atomicModifyIORef' ref (\case [] -> ([], Right (CheckReport [] [])); (r : rs) -> (rs, Right (CheckReport r [])))

-- | A checker that always reports the given broken files and nothing missing.
brokenChecker :: [(FilePath, Text)] -> Checker IO
brokenChecker broken = Checker $ \_ _ -> pure (Right (CheckReport [] broken))

elmLang :: Language
elmLang = l
  where
    Right l = parseLanguage "elm" "/p" $ T.unlines
      [ "files = [\"**/*.elm\"]", "comment = \"--\"", "grammar = \"elm\"", "qualify = \"elm\""
      , "placeholder = 'Debug.todo \"{= message =}\"'", "branch = \"{= pattern =} ->\\n    {= body =}\""
      , "[check]", "command = [\"true\"]", "format = \"elm-json\"" ]

completeSpec :: Spec
completeSpec = describe "generate (complete edits)" $ do
  let page = "module Page.A exposing (..)\n\nupdate msg model =\n    case msg of\n        NoOp ->\n            ( model, Cmd.none )\n"
      gen = generatorFrom . T.unlines $
        [ "description = \"d\"", "language = \"elm\"", "[inputs.name]", "type = \"ident\""
        , "[[edits]]", "complete = \"{= name|pascal =}\"", "text = \"{= pattern =} ->\\n    ( model, Cmd.none )\"" ]
      go files rounds = do
        ref <- newIORef rounds
        (ws, snapshot) <- memoryWorkspace files
        r <- generate ws noResolve (Tools noSyntax (scriptedChecker ref) [elmLang]) [Step gen (M.fromList [("name", "reset")])] now Apply
        (r,) <$> snapshot
  it "adds a branch after the case the compiler reports" $ do
    (Right plan, files) <- go [("src/A.elm", page)] [[MissingPatterns "src/A.elm" (3, 5) ["Reset"]]]
    M.lookup "src/A.elm" files `shouldBe` Just (page <> "\n        Reset ->\n            ( model, Cmd.none )\n")
    map editSite (planEdits plan) `shouldBe` [Just (Site "src/A.elm" "update" "Reset" False)]
  it "leaves a placeholder todo where no `where` matches" $ do
    let gen' = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"elm\"", "[inputs.name]", "type = \"ident\""
          , "[[edits]]", "complete = \"{= name|pascal =}\"", "where = \"case in view\"", "text = \"{= pattern =} -> 1\"" ]
    ref <- newIORef [[MissingPatterns "src/A.elm" (3, 5) ["Reset"]]]
    (ws, snapshot) <- memoryWorkspace [("src/A.elm", page)]
    Right plan <- generate ws noResolve (Tools noSyntax (scriptedChecker ref) [elmLang]) [Step gen' (M.fromList [("name", "reset")])] now Apply
    fmap (fmap siteTodo . editSite) (planEdits plan) `shouldBe` [Just True]
    (M.lookup "src/A.elm" <$> snapshot) `shouldReturn` Just (page <> "\n        Reset ->\n            Debug.todo \"olifant: update Reset\"\n")
  it "ignores diagnostics about constructors it did not introduce" $ do
    (Right plan, files) <- go [("src/A.elm", page)] [[MissingPatterns "src/A.elm" (3, 5) ["Other"]]]
    (planEdits plan, M.lookup "src/A.elm" files) `shouldBe` ([], Just page)
  it "is a conflict when the compiler keeps reporting the same site (Review Focus 3)" $ do
    let same = [MissingPatterns "src/A.elm" (3, 5) ["Reset"]]
    (Right plan, _) <- go [("src/A.elm", page)] [same, same]
    map describeConflict (planConflicts plan) `shouldSatisfy` any ("still reports Reset" `T.isInfixOf`)
  it "is a conflict naming the tool when the check cannot run" $ do
    (ws, _) <- memoryWorkspace [("src/A.elm", page)]
    Right plan <- generate ws noResolve (Tools noSyntax (Checker (\_ _ -> pure (Left (CheckToolMissing "`elm` is not installed")))) [elmLang]) [Step gen (M.fromList [("name", "reset")])] now Apply
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
          [ "files = [\"**/*.elm\"]", "comment = \"--\"", "grammar = \"elm\"", "qualify = \"elm\""
          , "placeholder = 'Debug.todo \"{= message =}\"'", "branch = \"{= pattern =} ->\\n    {= body =}\""
          , "[check]", "command = [\"true\"]", "format = \"elm-json\""
          , "[targets.case_in]", "phrase = \"case in FUNCTION\"", "container = { kind = \"case_of_expr\", regex = \"{= FUNCTION =}\" }", "items = { kind = \"case_of_branch\" }", "example = \"end of case in update\"" ]
    ref <- newIORef [[MissingPatterns "src/Main.elm" (9, 11) ["Settings _"], MissingPatterns "src/Main.elm" (4, 6) ["Just Settings"]]]
    (ws, snapshot) <- memoryWorkspace [("src/Main.elm", mainElm)]
    Right _ <- generate ws noResolve (Tools syntax (scriptedChecker ref) [withTargets]) [Step two M.empty] now Apply
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
    ref <- newIORef [[MissingPatterns "src/Main.elm" (4, 6) ["Settings _"]]]
    (ws, snapshot) <- memoryWorkspace [("src/Main.elm", mainElm)]
    Right plan <- generate ws noResolve (Tools syntax (scriptedChecker ref) [elmLang]) [Step two M.empty] now Apply
    map describeConflict (planConflicts plan) `shouldSatisfy` any (\c -> "Route.Settings" `T.isInfixOf` c && "pageName" `T.isInfixOf` c)
    (M.lookup "src/Main.elm" <$> snapshot) `shouldReturn` Just mainElm
  it "warns when the project does not compile, so cases could not be checked (review)" $ do
    (ws, _) <- memoryWorkspace [("src/A.elm", page), ("src/B.elm", "module B\n")]
    Right plan <- generate ws noResolve (Tools noSyntax (brokenChecker [("src/B.elm", "NAMING ERROR")]) [elmLang]) [Step gen (M.fromList [("name", "reset")])] now Apply
    planConflicts plan `shouldBe` []
    planWarnings plan `shouldSatisfy` any ("src/B.elm" `T.isInfixOf`)
  it "is a conflict when a file the plan changed does not compile (review)" $ do
    let broken = generatorFrom . T.unlines $
          [ "description = \"d\"", "language = \"elm\"", "[inputs.name]", "type = \"ident\""
          , "[[edits]]", "insert = \"src/A.elm\"", "at = \"after first line matching ^module\"", "text = \"oops\""
          , "[[edits]]", "complete = \"{= name|pascal =}\"", "text = \"{= pattern =} -> 1\"" ]
    (ws, snapshot) <- memoryWorkspace [("src/A.elm", page)]
    Right plan <- generate ws noResolve (Tools noSyntax (brokenChecker [("src/A.elm", "SYNTAX PROBLEM")]) [elmLang]) [Step broken (M.fromList [("name", "reset")])] now Apply
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
    ref <- newIORef [[MissingPatterns "src/Route.elm" (3, 5) ["Settings"]]]
    (ws, snapshot) <- memoryWorkspace [("src/Route.elm", routeElm)]
    Right plan <- generate ws noResolve (Tools syntax (scriptedChecker ref) [elmLang]) [Step two M.empty] now Apply
    planConflicts plan `shouldBe` []
    map (fmap siteTodo . editSite) (planEdits plan) `shouldBe` [Just True]
    (M.lookup "src/Route.elm" <$> snapshot) `shouldReturn` Just (routeElm <> "\n        Settings ->\n            Debug.todo \"olifant: toPath Settings\"\n")
  it "numbers compiler branches in the order they are applied, so lines replay (fix pass)" $ do
    let two = "module A exposing (..)\n\nf m =\n    case m of\n        A ->\n            1\n\ng m =\n    case m of\n        A ->\n            2\n"
    ref <- newIORef [[MissingPatterns "src/A.elm" (3, 5) ["Reset"], MissingPatterns "src/A.elm" (8, 10) ["Reset"]]]
    (ws, _) <- memoryWorkspace [("src/A.elm", two)]
    Right plan <- generate ws noResolve (Tools noSyntax (scriptedChecker ref) [elmLang]) [Step gen (M.fromList [("name", "reset")])] now Apply
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
          CheckReport [MissingPatterns "src/A.elm" (1, 3) ["Reset"] | Just t <- [M.lookup "src/A.elm" files], not ("Reset ->" `T.isInfixOf` t)] []
        runTwice files = do
          (ws, snap) <- memoryWorkspace files
          Right plan <- generate ws noResolve (Tools noSyntax honest [elmLang]) [Step pageGen (M.fromList [("name", "reset")])] now Apply
          (plan,) <$> snap
    (_, once) <- runTwice []
    M.lookup "src/A.elm" once `shouldBe` Just "update msg =\n    case msg of\n        NoOp ->\n            1\n\n        Reset ->\n            2\n"
    (again, twice) <- runTwice (M.toList once)
    planConflicts again `shouldBe` []
    twice `shouldBe` once
