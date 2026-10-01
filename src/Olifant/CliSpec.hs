module Olifant.CliSpec (spec) where

import Data.Aeson (Value (..), decodeStrict)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Olifant.Cli
import Olifant.Cli.Output (planHuman)
import Olifant.Generation (EditAction (..), Plan (..), PlannedEdit (..), Site (..))
import Olifant.Generator (GeneratorId (..))
import Olifant.Language (noChecker, noSyntax)
import Olifant.Workspace (memoryWorkspace)
import System.Exit (ExitCode (..))
import Test.Hspec

project :: [(FilePath, Text)]
project =
  [
    ( "/proj/.olifant/generators/model/generator.toml"
    , T.unlines
        [ "description = \"A model\""
        , "language = \"ruby\""
        , "examples = [\"olifant gen model --set name=blog_post\"]"
        , "[inputs.name]"
        , "type = \"ident\""
        , "prompt = \"Model name, singular (e.g. blog_post)\""
        , "[[edits]]"
        , "create = \"db/{= now|timestamp =}_create_{= name|plural|snake =}.rb\""
        , "text = \"# {= name|pascal =}\\n\""
        , "[[edits]]"
        , "create = \"app/models/{= name|snake =}.rb\""
        , "text = \"class {= name|pascal =}\\nend\\n\""
        ]
    )
  , ( "/proj/.olifant/generators/note/generator.toml"
    , "description = \"note\"\nlanguage = \"x\"\n[inputs.name]\ntype = \"ident\"\n[inputs.text]\ntype = \"text\"\n[[edits]]\ncreate = \"notes/{= name|snake =}.txt\"\ntext = \"{= text =}\\n\"\n"
    )
  ]

fixedNow :: UTCTime
fixedNow = UTCTime (fromGregorian 2026 9 27) (secondsToDiffTime 55800)

runWith :: Text -> [(FilePath, Text)] -> Bool -> [String] -> IO (ExitCode, Text, Text, Map FilePath Text)
runWith stdinText files tty args = do
  (root, snapshot) <- memoryWorkspace files
  out <- newIORef ""
  err <- newIORef ""
  code <-
    runCli
      Runtime
        { rtRoot = root
        , rtProjectDir = "/proj"
        , rtUserConfigDir = Nothing
        , rtHomeDir = Nothing
        , rtReadStdin = pure stdinText
        , rtNow = pure fixedNow
        , rtStdoutIsTty = tty
        , rtOut = \t -> modifyIORef' out (<> t)
        , rtErr = \t -> modifyIORef' err (<> t)
        , rtSyntax = noSyntax
        , rtChecker = noChecker
        }
      args
  (,,,) code <$> readIORef out <*> readIORef err <*> snapshot

jsonAt :: [Text] -> Text -> Maybe Value
jsonAt path out = decodeStrict (TE.encodeUtf8 out) >>= go path
  where
    go [] v = Just v
    go (k : ks) (Object o) = KM.lookup (Key.fromText k) o >>= go ks
    go _ _ = Nothing

spec :: Spec
spec = planOutputSpec >> runCliSpec

planOutputSpec :: Spec
planOutputSpec = describe "planHuman" $ do
  let at0 = UTCTime (fromGregorian 2026 9 29) 0
      edit n p a site = PlannedEdit ["g"] n p a Nothing site
      plan =
        Plan (GeneratorId "g") [] at0
          [ edit 1 "a.elm" (WillInsert 3 "x") Nothing
          , edit 2 "b.elm" (WillInsert 9 "Settings ->\n    1") (Just (Site "b.elm" "view" "Settings" False))
          , edit 3 "c.elm" (WillInsert 4 "T ->\n    Debug.todo \"\"") (Just (Site "c.elm" "toPath" "T" True))
          ]
          [] [] M.empty
  it "marks edits the compiler found, and todos (fix pass)" $ do
    let out = planHuman True plan
    out `shouldSatisfy` ("(+2 lines at line 9, compiler: view)" `T.isInfixOf`)
    out `shouldSatisfy` ("(+2 lines at line 4, compiler: toPath, todo)" `T.isInfixOf`)
  it "ends every file's contents with a newline in --plan output (fix pass)" $
    planHuman False plan `shouldSatisfy` ("Debug.todo \"\"\n" `T.isSuffixOf`)

runCliSpec :: Spec
runCliSpec = describe "runCli" $ do
  it "sync --check fails when a regions.toml is broken, and gen warns (review #2)" $ do
    let broken = ("/proj/.olifant/regions.toml", "[regions.mods]\nbogus = 1\n") : [(p, t) | (p, t) <- regionProject "  exposed-modules:\n", p /= "/proj/.olifant/regions.toml"]
    (code, out, _, _) <- runWith "" broken False ["sync", "--check"]
    code `shouldBe` ExitFailure 5
    out `shouldSatisfy` ("regions.toml" `T.isInfixOf`)
    (_, genOut, _, _) <- runWith "" broken False ["gen", "model", "--set", "name=post"]
    genOut `shouldSatisfy` ("regions.toml" `T.isInfixOf`)
  it "sync --init refuses a region file outside the project (review #3)" $ do
    let outside = ("/proj/.olifant/regions.toml", "[regions.esc]\nfile = \"../x.txt\"\ncomment = \"#\"\nquery = { glob = \"*\" }\nitem = \"{= path =}\"\nwhole_file = true\n") : project
    (code, _, _, files) <- runWith "" outside False ["sync", "--init", "esc"]
    code `shouldBe` ExitFailure 5
    M.member "/x.txt" files `shouldBe` False
  it "sync --init creates a whole-file region's file (review #4)" $ do
    let whole = ("/proj/.olifant/regions.toml", "[regions.idx]\nfile = \"index.txt\"\ncomment = \"#\"\nquery = { glob = \"src/**/*.hs\" }\nitem = \"{= dotted =}\"\nwhole_file = true\n") : ("/proj/src/Olifant/A.hs", "") : project
    (code, _, _, files) <- runWith "" whole True ["sync", "--init", "idx"]
    code `shouldBe` ExitSuccess
    M.lookup "/proj/index.txt" files `shouldSatisfy` maybe False ("# Generated by olifant (region idx" `T.isPrefixOf`)
    M.lookup "/proj/index.txt" files `shouldSatisfy` maybe False ("\nOlifant.A\n" `T.isInfixOf`)
  it "list --detail prints live choices, like show (review #6)" $ do
    let svc = ("/proj/.olifant/generators/svc/generator.toml", "description = \"d\"\nlanguage = \"x\"\n[inputs.service]\ntype = \"enum\"\nchoices_from = { glob = \"*/*-service\", value = \"{= dir =}\" }\n")
    (_, out, _, _) <- runWith "" (svc : ("/proj/billing/billing-service/Cargo.toml", "") : project) False ["list", "--detail"]
    out `shouldSatisfy` ("enum(billing)" `T.isInfixOf`)
  it "sync --init creates the fences, and fills them" $ do
    (code, _, _, files) <- runWith "" (regionProject "  exposed-modules:\n") True ["sync", "--init", "mods"]
    code `shouldBe` ExitSuccess
    M.lookup "/proj/x.cabal" files `shouldSatisfy` maybe False ("  exposed-modules:\n    -- olifant:region mods" `T.isInfixOf`)
    M.lookup "/proj/x.cabal" files `shouldSatisfy` maybe False ("    Olifant.A\n    -- olifant:end mods" `T.isInfixOf`)
  it "sync --check reports drift without writing, and passes once synced (Review Focus 2)" $ do
    let stale = "  exposed-modules:\n    -- olifant:region mods\n    Olifant.Handwritten\n    -- olifant:end mods\n"
    (code, out, _, files) <- runWith "" (regionProject stale) False ["sync", "--check"]
    code `shouldBe` ExitFailure 5
    out `shouldSatisfy` ("mods" `T.isInfixOf`)
    M.lookup "/proj/x.cabal" files `shouldBe` Just stale
    (code2, _, _, files2) <- runWith "" (regionProject stale) True ["sync"]
    code2 `shouldBe` ExitSuccess
    let synced = [(p, t) | (p, t) <- regionProject stale, p /= "/proj/x.cabal"] ++ [("/proj/x.cabal", files2 M.! "/proj/x.cabal")]
    (code3, _, _, _) <- runWith "" synced True ["sync", "--check"]
    code3 `shouldBe` ExitSuccess
  it "sync --check reads as a lint result for people" $ do
    let stale = "  exposed-modules:\n    -- olifant:region mods\n    Olifant.Handwritten\n    -- olifant:end mods\n"
    (_, out, _, _) <- runWith "" (regionProject stale) True ["sync", "--check"]
    out `shouldSatisfy` ("1 region out of date in x.cabal: mods (+1 −1); nothing written, run `olifant sync`" `T.isInfixOf`)
    (_, _, _, files) <- runWith "" (regionProject stale) True ["sync"]
    (_, clean, _, _) <- runWith "" ([(p, t) | (p, t) <- regionProject stale, p /= "/proj/x.cabal"] ++ [("/proj/x.cabal", files M.! "/proj/x.cabal")]) True ["sync", "--check"]
    clean `shouldSatisfy` ("regions up to date" `T.isInfixOf`)
  it "sync --check reports a region that is not set up" $ do
    (code, out, _, _) <- runWith "" (regionProject "  exposed-modules:\n") False ["sync", "--check"]
    code `shouldBe` ExitFailure 5
    out `shouldSatisfy` ("sync --init mods" `T.isInfixOf`)
  it "shows choices_from values found in the project" $ do
    let svc = ("/proj/.olifant/generators/svc/generator.toml", "description = \"d\"\nlanguage = \"x\"\n[inputs.service]\ntype = \"enum\"\nchoices_from = { glob = \"*/*-service\", value = \"{= dir =}\" }\n")
    (code, out, _, _) <- runWith "" (svc : ("/proj/billing/billing-service/Cargo.toml", "") : project) True ["show", "svc"]
    code `shouldBe` ExitSuccess
    out `shouldSatisfy` ("--set service=<enum(billing)>" `T.isInfixOf`)
  it "suggests `gen` when the first word looks like a generator" $ do
    (code, _, err, _) <- runWith "" project True ["elm/page", "--set", "page=settings"]
    code `shouldBe` ExitFailure 2
    err `shouldSatisfy` ("did you mean `olifant gen elm/page --set page=settings`?" `T.isInfixOf`)
  it "lists a language's anchor targets" $ do
    (code, out, _, _) <- runWith "" elmPack False ["anchors", "elm"]
    code `shouldBe` ExitSuccess
    out `shouldSatisfy` ("\"phrase\":\"case in FUNCTION\"" `T.isInfixOf`)
  it "reports an unknown language" $ do
    (code, out, _, _) <- runWith "" elmPack False ["anchors", "cobol"]
    code `shouldBe` ExitFailure 3
    out `shouldSatisfy` ("language_not_found" `T.isInfixOf`)
  it "shows where a line phrase resolves" $ do
    (code, out, _, _) <- runWith "" (("/proj/lib.rs", "pub mod a;\nfn x() {}\n") : elmPack) False ["where", "lib.rs", "after last line matching ^pub mod "]
    code `shouldBe` ExitSuccess
    out `shouldSatisfy` ("\"line\":2" `T.isInfixOf`)
  it "explains a phrase that does not resolve" $ do
    (code, out, _, _) <- runWith "" (("/proj/lib.rs", "fn x() {}\n") : elmPack) False ["where", "lib.rs", "before marker // m"]
    code `shouldBe` ExitFailure 5
    out `shouldSatisfy` ("marker not found" `T.isInfixOf`)
  it "rejects a phrase that does not parse as a usage error" $ do
    (code, _, _, _) <- runWith "" (("/proj/lib.rs", "fn x() {}\n") : elmPack) False ["where", "lib.rs", "bottom of lib"]
    code `shouldBe` ExitFailure 2
  it "gen applies by default and prints a human summary on a TTY" $ do
    (code, out, _, files) <- runWith "" project True ["gen", "model", "--set", "name=blog_post"]
    code `shouldBe` ExitSuccess
    M.lookup "/proj/app/models/blog_post.rb" files `shouldBe` Just "class BlogPost\nend\n"
    out `shouldSatisfy` T.isInfixOf "+ app/models/blog_post.rb"
  it "uses the clock for `now`, and --now pins it" $ do
    (_, _, _, files1) <- runWith "" project True ["gen", "model", "--set", "name=post"]
    M.member "/proj/db/20260927153000_create_posts.rb" files1 `shouldBe` True
    (_, _, _, files2) <- runWith "" project True ["gen", "model", "--set", "name=post", "--now", "2030-01-02T03:04:05Z"]
    M.member "/proj/db/20300102030405_create_posts.rb" files2 `shouldBe` True
  it "--plan writes nothing and prints the plan as JSON when not on a TTY" $ do
    (code, out, _, files) <- runWith "" project False ["gen", "model", "--set", "name=post", "--plan"]
    code `shouldBe` ExitSuccess
    files `shouldBe` M.fromList project
    jsonAt ["applied"] out `shouldBe` Just (Bool False)
    jsonAt ["generator"] out `shouldBe` Just (String "model")
  it "lists every input problem as JSON with exit 4" $ do
    (code, out, _, _) <- runWith "" project False ["gen", "model", "--set", "colour=red"]
    code `shouldBe` ExitFailure 4
    jsonAt ["error", "code"] out `shouldBe` Just (String "invalid_inputs")
    jsonAt ["error", "unknown"] out `shouldBe` Just (Array (pure (String "colour")))
    fmap (T.isInfixOf "--set name=<ident>") (jsonAt ["error", "hint"] out >>= \case String s -> Just s; _ -> Nothing) `shouldBe` Just True
  it "exits 3 for an unknown generator" $ do
    (code, out, _, _) <- runWith "" project False ["gen", "nope"]
    code `shouldBe` ExitFailure 3
    jsonAt ["error", "code"] out `shouldBe` Just (String "generator_not_found")
  it "exits 5 on a conflict and writes nothing" $ do
    let existing = ("/proj/app/models/post.rb", "mine\n")
    (code, out, _, files) <- runWith "" (existing : project) False ["gen", "model", "--set", "name=post"]
    code `shouldBe` ExitFailure 5
    M.lookup "/proj/app/models/post.rb" files `shouldBe` Just "mine\n"
    M.size files `shouldBe` length project + 1
    fmap (\case Array xs -> length xs; _ -> 0) (jsonAt ["conflicts"] out) `shouldBe` Just 1
  it "exits 3 with a pack_not_configured hint for pack/generator of an unknown pack" $ do
    (code, out, _, _) <- runWith "" project False ["gen", "elm/page"]
    code `shouldBe` ExitFailure 3
    jsonAt ["error", "code"] out `shouldBe` Just (String "pack_not_configured")
  it "exits 2 on bad usage" $ do
    (code, _, _, _) <- runWith "" project False ["gen"]
    code `shouldBe` ExitFailure 2
  it "lists generators as JSON" $ do
    (code, out, _, _) <- runWith "" project False ["list"]
    code `shouldBe` ExitSuccess
    out `shouldSatisfy` T.isInfixOf "\"id\":\"model\""
  it "shows a generator's inputs" $ do
    (_, out, _, _) <- runWith "" project False ["show", "model", "--json"]
    out `shouldSatisfy` T.isInfixOf "\"type\":\"ident\""
  it "shows each input's prompt, so agents can use generators without reading code" $ do
    (_, json, _, _) <- runWith "" project False ["show", "model", "--json"]
    json `shouldSatisfy` T.isInfixOf "\"prompt\":\"Model name, singular (e.g. blog_post)\""
    (_, human, _, _) <- runWith "" project True ["show", "model"]
    human `shouldSatisfy` T.isInfixOf "Model name, singular (e.g. blog_post)"
  it "chains generators with + into one plan" $ do
    (code, out, _, files) <- runWith "" project False ["gen", "model", "--set", "name=post", "+", "note", "--set", "name=post", "--set", "text=a+b", "--plan"]
    code `shouldBe` ExitSuccess
    files `shouldBe` M.fromList project
    fmap (\case Array xs -> length xs; _ -> 0) (jsonAt ["edits"] out) `shouldBe` Just 3
    out `shouldSatisfy` T.isInfixOf "note (step 2)"
  it "applies a chain atomically and keeps a + inside a value" $ do
    (code, _, _, files) <- runWith "" project False ["gen", "model", "--set", "name=post", "+", "note", "--set", "name=post", "--set", "text=a+b"]
    code `shouldBe` ExitSuccess
    M.lookup "/proj/notes/post.txt" files `shouldBe` Just "a+b\n"
  it "reads the same chain as JSON with --steps -" $ do
    let steps = "[{\"gen\":\"model\",\"set\":{\"name\":\"post\"}},{\"gen\":\"note\",\"set\":{\"name\":\"post\",\"text\":\"hi\"}}]"
    (code, _, _, files) <- runWith steps project False ["gen", "--steps", "-"]
    code `shouldBe` ExitSuccess
    M.lookup "/proj/notes/post.txt" files `shouldBe` Just "hi\n"
  it "reports an unknown generator mid-chain with exit 3 and writes nothing" $ do
    (code, out, _, files) <- runWith "" project False ["gen", "model", "--set", "name=post", "+", "nope", "--set", "x=y"]
    code `shouldBe` ExitFailure 3
    jsonAt ["error", "code"] out `shouldBe` Just (String "generator_not_found")
    jsonAt ["error", "step"] out `shouldBe` Just (String "nope (step 2)")
    files `shouldBe` M.fromList project
  it "rejects an empty step and mixing --steps with positional steps" $ do
    (c1, _, _, _) <- runWith "" project False ["gen", "model", "--set", "name=post", "+"]
    c1 `shouldBe` ExitFailure 2
    (c2, _, _, _) <- runWith "[]" project False ["gen", "model", "--steps", "-"]
    c2 `shouldBe` ExitFailure 2
  it "reports a missing --steps file as not found (exit 3), without crashing" $ do
    (code, out, _, _) <- runWith "" project False ["gen", "--steps", "missing.json"]
    code `shouldBe` ExitFailure 3
    jsonAt ["error", "code"] out `shouldBe` Just (String "steps_not_found")
  it "accepts numbers, booleans and lists in --steps values" $ do
    let steps = "[{\"gen\":\"note\",\"set\":{\"name\":\"n1\",\"text\":3}},{\"gen\":\"note\",\"set\":{\"name\":\"n2\",\"text\":[\"a\",\"b\"]}}]"
    (code, _, _, files) <- runWith steps project False ["gen", "--steps", "-"]
    code `shouldBe` ExitSuccess
    M.lookup "/proj/notes/n1.txt" files `shouldBe` Just "3\n"
    M.lookup "/proj/notes/n2.txt" files `shouldBe` Just "a,b\n"
  it "treats a broken nested generator as a definition error (exit 1)" $ do
    let fs =
          project
            ++ [ ("/proj/.olifant/generators/outer/generator.toml", "description = \"o\"\nlanguage = \"x\"\n[[uses]]\ngenerator = \"inner\"\n")
               , ("/proj/.olifant/generators/inner/generator.toml", "description = \"i\"\nlanguage = \"x\"\nbogus = 1\n")
               ]
    (code, out, _, _) <- runWith "" fs False ["gen", "outer"]
    code `shouldBe` ExitFailure 1
    jsonAt ["error", "code"] out `shouldBe` Just (String "generator_invalid")
  it "keeps applied output compact (no file contents); --plan still shows them" $ do
    (_, applied, _, _) <- runWith "" project False ["gen", "model", "--set", "name=post"]
    (_, planned, _, _) <- runWith "" project False ["gen", "model", "--set", "name=post", "--plan"]
    let firstEdit out = case jsonAt ["edits"] out of
          Just (Array xs) | (Object o : _) <- toList xs -> Just o
          _ -> Nothing
    fmap (KM.member "content") (firstEdit applied) `shouldBe` Just False
    fmap (KM.member "content") (firstEdit planned) `shouldBe` Just True
    jsonAt ["plan_version"] applied `shouldBe` Just (Number 2)
  it "shows examples and lists a composition tip" $ do
    (_, shown, _, _) <- runWith "" project False ["show", "model"]
    shown `shouldSatisfy` T.isInfixOf "olifant gen model --set name=blog_post"
    (_, listed, _, _) <- runWith "" project False ["list"]
    fmap (\case String s -> "+" `T.isInfixOf` s; _ -> False) (jsonAt ["tip"] listed) `shouldBe` Just True
    (_, human, _, _) <- runWith "" project True ["list"]
    human `shouldSatisfy` T.isInfixOf "Tip:"
  it "shows where a generator is defined, and what a complete body can use" $ do
    let withRoute = project ++ [("/proj/.olifant/generators/route/generator.toml", "description = \"r\"\nlanguage = \"elm\"\n[inputs.name]\ntype = \"ident\"\n[[edits]]\ncomplete = \"Route.{= name|pascal =}\"\ntext = \"{= pattern =} -> x\"\n")]
    (_, human, _, _) <- runWith "" withRoute True ["show", "route"]
    human `shouldSatisfy` T.isInfixOf "defined in /proj/.olifant/generators/route/generator.toml"
    human `shouldSatisfy` T.isInfixOf "{= pattern =}"
    (_, json, _, _) <- runWith "" withRoute False ["show", "route"]
    jsonAt ["source"] json `shouldBe` Just (String "/proj/.olifant/generators/route/generator.toml")
    (_, model, _, _) <- runWith "" project True ["show", "model"]
    model `shouldNotSatisfy` T.isInfixOf "{= pattern =}"
  it "indents a multi-line load error under its generator in list" $ do
    let broken = project ++ [("/proj/.olifant/generators/broken/generator.toml", "description = \"b\"\nlanguage = \"x\"\n[[edits]]\ncreate = \"x\"\ntext = \"{= if =}\"\n")]
    (_, out, _, _) <- runWith "" broken True ["list"]
    let after = drop 1 (dropWhile (not . T.isInfixOf "INVALID") (T.lines out))
        continuation = takeWhile (\l -> not (T.null (T.strip l)) && not ("Tip:" `T.isPrefixOf` l)) after
    continuation `shouldNotBe` []
    continuation `shouldSatisfy` all ("        " `T.isPrefixOf`)
  it "accepts --flag=value forms" $ do
    (code, _, _, files) <- runWith "" project False ["gen", "model", "--set=name=post", "--now=2030-01-02T03:04:05Z"]
    code `shouldBe` ExitSuccess
    M.member "/proj/db/20300102030405_create_posts.rb" files `shouldBe` True
  it "rejects unknown keys in --steps and hints at gen/set" $ do
    (code, out, _, _) <- runWith "[{\"generator\":\"model\",\"with\":{\"name\":\"post\"}}]" project False ["gen", "--steps", "-"]
    code `shouldBe` ExitFailure 2
    fmap (\case String s -> "\"gen\" and \"set\"" `T.isInfixOf` s; _ -> False) (jsonAt ["error", "message"] out) `shouldBe` Just True
  it "reports usage errors as JSON when output is JSON" $ do
    (code, out, _, _) <- runWith "" project False ["gen", "model", "+"]
    code `shouldBe` ExitFailure 2
    jsonAt ["error", "code"] out `shouldBe` Just (String "usage")
  it "says a generator is missing when gen has none" $ do
    (code, _, err, _) <- runWith "" project True ["gen", "--plan"]
    code `shouldBe` ExitFailure 2
    err `shouldSatisfy` T.isInfixOf "missing GENERATOR"
  it "shows a composite's sub-generators and their inputs, so one show is enough" $ do
    let fs = project ++ [("/proj/.olifant/generators/outer/generator.toml", "description = \"o\"\nlanguage = \"x\"\n[inputs.names]\ntype = \"list\"\nof = \"ident\"\n[[uses]]\ngenerator = \"note\"\nfor_each = \"names\"\nwith = { name = \"{= item =}\", text = \"hi\" }\n")]
    (_, json, _, _) <- runWith "" fs False ["show", "outer"]
    json `shouldSatisfy` T.isInfixOf "\"uses\""
    json `shouldSatisfy` T.isInfixOf "\"for_each\":\"names\""
    json `shouldSatisfy` T.isInfixOf "\"name\":\"text\""
    (_, human, _, _) <- runWith "" fs True ["show", "outer"]
    human `shouldSatisfy` T.isInfixOf "uses:"
    human `shouldSatisfy` T.isInfixOf "note (for each item of names)"
  it "lists every generator with its inputs and examples via list --detail" $ do
    (code, out, _, _) <- runWith "" project False ["list", "--detail"]
    code `shouldBe` ExitSuccess
    out `shouldSatisfy` T.isInfixOf "\"prompt\":\"Model name, singular (e.g. blog_post)\""
    out `shouldSatisfy` T.isInfixOf "olifant gen model --set name=blog_post"
    (_, human, _, _) <- runWith "" project True ["list", "--detail"]
    human `shouldSatisfy` T.isInfixOf "--set name=<ident>"

-- | A project whose config points at a pack with one Elm adapter.
elmPack :: [(FilePath, Text)]
elmPack =
  [ ("/proj/.olifant/config.toml", "[packs]\nelm = { path = \"/packs/elm\" }\n")
  , ("/packs/elm/generators/g/generator.toml", "description = \"d\"\nlanguage = \"elm\"\n")
  , ( "/packs/elm/languages/elm.toml"
    , T.unlines
        [ "files = [\"**/*.elm\"]", "comment = \"--\"", "grammar = \"elm\""
        , "[targets.case_in]", "phrase = \"case in FUNCTION\""
        , "container = { kind = \"case_of_expr\" }", "items = { kind = \"case_of_branch\" }"
        , "example = \"end of case in update\""
        ]
    )
  ]

-- | A project with one fenced region over src/**/*.hs in x.cabal.
regionProject :: Text -> [(FilePath, Text)]
regionProject cabalText =
  [ ( "/proj/.olifant/regions.toml"
    , T.unlines
        [ "[regions.mods]", "file = \"x.cabal\"", "comment = \"--\"", "query = { glob = \"src/**/*.hs\" }", "item = \"{= dotted =}\""
        , "init_at = \"after first line matching ^  exposed-modules:\"", "init_indent = \"    \"" ]
    )
  , ("/proj/x.cabal", cabalText)
  , ("/proj/src/Olifant/A.hs", "module Olifant.A where\n")
  ]
    ++ project
