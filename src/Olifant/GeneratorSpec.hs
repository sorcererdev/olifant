{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.GeneratorSpec (spec) where

import Data.Functor.Identity (runIdentity)
import Olifant.Query (noQueries)

import Data.Either (isLeft)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.Generator
import Olifant.Naming (parseName)
import Olifant.Template (Value (..), templateSource)
import Test.Hspec

modelToml :: Text
modelToml =
  T.unlines
    [ "description = \"Model\""
    , "language = \"ruby\""
    , "[inputs.name]"
    , "type = \"ident\""
    , "[inputs.fields]"
    , "type = \"list\""
    , "optional = true"
    , "of = ["
    , "  { name = \"name\", type = \"ident\" },"
    , "  { name = \"type\", type = \"enum\", choices = [\"string\", \"text\", \"references\"], default = \"string\" },"
    , "]"
    , "[derive]"
    , "table = \"{= name|plural|snake =}\""
    , "Model = \"{= name|pascal =}\""
    , "[[edits]]"
    , "create = \"app/models/{= name|snake =}.rb\""
    , "template = \"model.rb.dt\""
    , "[[edits]]"
    , "insert = \"config/routes.rb\""
    , "anchor = { marker = \"# olifant:routes\" }"
    , "text = \"resources :{= table =}\""
    ]

load :: Text -> Either Text GeneratorDef
load = parseGenerator (GeneratorId "rails/model")

spec :: Spec
spec = do
  parseSpec
  envSpec

parseSpec :: Spec
parseSpec = describe "parseGenerator" $ do
  it "reads metadata, defaulting version to 1" $
    fmap (\g -> (genDescription g, genLanguage g, genVersion g)) (load modelToml) `shouldBe` Right ("Model", "ruby", 1)
  it "reads inputs, including ordered record fields" $
    fmap (map (\i -> (inputName i, inputType i, inputOptional i)) . genInputs) (load modelToml)
      `shouldBe` Right
        [ ("name", ScalarInput IdentS, False)
        , ("fields", ListInput (RecordItems [FieldSpec "name" IdentS Nothing, FieldSpec "type" (EnumS ["string", "text", "references"]) (Just "string")]), True)
        ]
  it "reads derived values as templates" $
    fmap (map (fmap templateSource) . genDerived) (load modelToml)
      `shouldBe` Right [("Model", "{= name|pascal =}"), ("table", "{= name|plural|snake =}")]
  it "reads create and insert edits" $
    fmap (map describeEdit . genEdits) (load modelToml)
      `shouldBe` Right ["create app/models/{= name|snake =}.rb from model.rb.dt", "insert config/routes.rb at before marker # olifant:routes"]
  it "reads exemplar sources with their bindings" $ do
    let toml = "description = \"d\"\nlanguage = \"rust\"\n[[edits]]\ncreate = \"x.rs\"\nexemplar = \"fixture:src/refund_invoice.rs\"\nbind = { command_name = \"refund_invoice\" }\n"
    fmap (map (\e -> case editSource e of ExemplarFile ref bs -> (ref, length bs); _ -> ("?", 0)) . genEdits) (load toml)
      `shouldBe` Right [("fixture:src/refund_invoice.rs", 1)]
  it "reads [[uses]] steps with for_each and with templates" $ do
    let toml =
          T.unlines
            [ "description = \"page\""
            , "language = \"elm\""
            , "[inputs.page]"
            , "type = \"ident\""
            , "[[uses]]"
            , "generator = \"msg\""
            , "for_each = \"msgs\""
            , "with = { page = \"{= page =}\", msg = \"{= item.name =}\" }"
            , "[[uses]]"
            , "generator = \"elm/route\""
            ]
    fmap (map (\u -> (useGenerator u, useForEach u, map (fmap templateSource) (useWith u))) . genUses) (load toml)
      `shouldBe` Right
        [ ("msg", Just "msgs", [("msg", "{= item.name =}"), ("page", "{= page =}")])
        , ("elm/route", Nothing, [])
        ]
  it "reads examples" $
    fmap genExamples (load "description = \"d\"\nlanguage = \"x\"\nexamples = [\"olifant gen x --set a=b\"]\n") `shouldBe` Right ["olifant gen x --set a=b"]
  it "rejects unknown keys in a [[uses]] step" $
    load "description = \"d\"\nlanguage = \"x\"\n[[uses]]\ngenerator = \"g\"\nforeach = \"xs\"\n" `shouldSatisfy` isLeft
  it "rejects misspelled keys, naming them" $ do
    let result = load (T.replace "template = " "templete = " modelToml)
    result `shouldSatisfy` isLeft
    either id (const "") result `shouldSatisfy` T.isInfixOf "templete"
  it "reads `at` as one phrase or a list, and `complete` edits" $ do
    let src =
          T.unlines
            [ "description = \"d\"", "language = \"elm\""
            , "[[edits]]", "insert = \"a.elm\"", "at = [\"end of type Msg\", \"before marker -- m\"]", "text = \"x\""
            , "[[edits]]", "complete = \"Route.X\"", "where = \"case in update\"", "text = \"{= pattern =} -> 1\""
            ]
    fmap (map describeEdit . genEdits) (load src) `shouldBe` Right ["insert a.elm at end of type Msg | before marker -- m", "complete Route.X"]
  it "rejects a phrase without variables that does not parse, at load time" $
    load (T.unlines ["description = \"d\"", "language = \"x\"", "[[edits]]", "insert = \"a\"", "at = \"bottom of type Msg\"", "text = \"x\""])
      `shouldSatisfy` either ("start of, end of, before or after" `T.isInfixOf`) (const False)
  it "rejects a `where` that is not a target" $
    load (T.unlines ["description = \"d\"", "language = \"x\"", "[[edits]]", "complete = \"X\"", "where = \"marker\"", "text = \"x\""])
      `shouldSatisfy` isLeft
  it "needs exactly one of `at` or `anchor`" $
    load (T.unlines ["description = \"d\"", "language = \"x\"", "[[edits]]", "insert = \"a\"", "text = \"x\""])
      `shouldSatisfy` either ("`at`" `T.isInfixOf`) (const False)
  describe "hints for the mistakes models make (v9)" $ do
    let top extra = T.unlines (["description = \"d\"", "language = \"x\""] ++ extra)
        err src = either id (const "") (load src)
    it "explains a generator name or id key" $
      err (top ["name = \"route\""]) `shouldSatisfy` T.isInfixOf "named by its directory"
    it "points `path` and `content` on an edit at `insert`/`create` and `text`" $ do
      let e = err (top ["[[edits]]", "insert = \"a.x\"", "at = \"end of type T\"", "path = \"a.x\"", "content = \"x\""])
      e `shouldSatisfy` T.isInfixOf "`content`: did you mean `text`?"
      e `shouldSatisfy` T.isInfixOf "`path`: the file is the value of `insert`"
    it "points `description` and `required` on an input at `prompt` and `optional`" $ do
      let e = err (top ["[inputs.name]", "type = \"ident\"", "description = \"d\"", "required = true"])
      e `shouldSatisfy` T.isInfixOf "`description`: did you mean `prompt`?"
      e `shouldSatisfy` T.isInfixOf "required unless `optional = true`"
    it "suggests the nearest key for a typo" $
      err (top ["[[edits]]", "insert = \"a.x\"", "att = \"end of type T\"", "text = \"x\""]) `shouldSatisfy` T.isInfixOf "`att`: did you mean `at`?"
    it "explains `insert` written as a table, and [edits] written as one table" $ do
      err (top ["[[edits]]", "insert = { file = \"a.x\" }", "at = \"end of type T\"", "text = \"x\""]) `shouldSatisfy` T.isInfixOf "insert = \"src/Main.elm\""
      err (top ["[edits]", "insert = \"a.x\""]) `shouldSatisfy` T.isInfixOf "[[edits]]"
    it "shows the three edit forms when an edit has none of them" $
      err (top ["[[edits]]", "text = \"x\""]) `shouldSatisfy` T.isInfixOf "insert = \""
    it "explains `derive` written as an array" $
      err (top ["derive = [\"x\"]"]) `shouldSatisfy` T.isInfixOf "[derive]"
  it "keeps inputs in the order they are declared" $ do
    let src = T.unlines ["description = \"d\"", "language = \"x\"", "[inputs.zeta]", "type = \"ident\"", "[inputs.alpha]", "type = \"text\"", "[inputs.mid]", "type = \"text\""]
    fmap (map inputName . genInputs) (load src) `shouldBe` Right ["zeta", "alpha", "mid"]
  it "refuses a query named like an input or derived value" $ do
    load (T.unlines ["description = \"d\"", "language = \"x\"", "[inputs.pages]", "type = \"text\"", "[query.pages]", "glob = \"*\""])
      `shouldSatisfy` either ("`query.pages`" `T.isInfixOf`) (const False)
    load (T.unlines ["description = \"d\"", "language = \"x\"", "[derive]", "pages = \"x\"", "[query.pages]", "glob = \"*\""])
      `shouldSatisfy` either ("`query.pages`" `T.isInfixOf`) (const False)
  it "refuses a fragment alongside text or template" $
    load (T.unlines ["description = \"d\"", "language = \"x\"", "[[edits]]", "insert = \"a\"", "at = \"end of type T\"", "text = \"x\"", "fragment = { file = \"a\", items_of = \"type T\", matching = \"X\" }", "bind = { x = \"a_b\" }"])
      `shouldSatisfy` either ("exactly one of `template`, `text` or `fragment`" `T.isInfixOf`) (const False)
  it "refuses `bind` without an exemplar or fragment" $ do
    load (T.unlines ["description = \"d\"", "language = \"x\"", "[[edits]]", "insert = \"a\"", "at = \"end of type T\"", "text = \"x\"", "bind = { x = \"a_b\" }"])
      `shouldSatisfy` either ("`bind`" `T.isInfixOf`) (const False)
    load (T.unlines ["description = \"d\"", "language = \"x\"", "[[edits]]", "create = \"a\"", "text = \"x\"", "bind = { x = \"a_b\" }"])
      `shouldSatisfy` either ("`bind`" `T.isInfixOf`) (const False)
    load (T.unlines ["description = \"d\"", "language = \"x\"", "[[edits]]", "create = \"a\"", "template = \"x.dt\"", "bind = { x = \"a_b\" }"])
      `shouldSatisfy` either ("`bind`" `T.isInfixOf`) (const False)
  it "reads choices_from and resolves it from the repo (Review Focus 5)" $ do
    let src = T.unlines ["description = \"d\"", "language = \"x\"", "[inputs.service]", "type = \"enum\"", "choices_from = { glob = \"*/*-service\", value = \"{= dir =}\" }"]
        Right g = load src
        at0 = UTCTime (fromGregorian 2026 9 29) 0
        Right resolved = runIdentity (withChoices noQueries ["billing/billing-service", "accounts/accounts-service"] (const (pure Nothing)) g)
    map (describeInputType . inputType) (genInputs resolved) `shouldBe` ["enum(accounts|billing)"]
    either describeEnvError (const "") (buildEnv resolved (M.fromList [("service", "biling")]) at0) `shouldSatisfy` ("did you mean billing" `T.isInfixOf`)
    fmap (M.lookup "service") (buildEnv resolved (M.fromList [("service", "billing")]) at0) `shouldBe` Right (Just (VText "billing"))
  it "says so when a choices_from query finds nothing" $ do
    let Right g = load (T.unlines ["description = \"d\"", "language = \"x\"", "[inputs.service]", "type = \"enum\"", "choices_from = { glob = \"*/*-service\" }"])
        Right resolved = runIdentity (withChoices noQueries [] (const (pure Nothing)) g)
    either describeEnvError (const "") (buildEnv resolved (M.fromList [("service", "x")]) (UTCTime (fromGregorian 2026 9 29) 0)) `shouldSatisfy` ("found nothing" `T.isInfixOf`)
  it "rejects an edit that is both create and insert" $
    load "description = \"d\"\nlanguage = \"x\"\n[[edits]]\ncreate = \"a\"\ninsert = \"b\"\ntext = \"t\"\n" `shouldSatisfy` isLeft
  it "reports template syntax errors with the key" $ do
    let result = load (T.replace "{= name|pascal =}" "{= name|shout =}" modelToml)
    either id (const "") result `shouldSatisfy` T.isInfixOf "Model"
  where
    describeEdit = \case
      Create p (TemplateFile f) _ _ -> "create " <> templateSource p <> " from " <> T.pack f
      Insert p anchors _ _ -> "insert " <> templateSource p <> " at " <> T.intercalate " | " [case a of AtPhrase t -> templateSource t; AtRule _ _ -> "<ast rule>" | a <- anchors]
      Complete c _ _ -> "complete " <> templateSource c
      _ -> "unexpected"

envSpec :: Spec
envSpec = do
  describe "buildEnv" $ do
    let now = UTCTime (fromGregorian 2026 9 27) 0
        gen = either (error . T.unpack) id (load modelToml)
        nameV = VName . either (error . T.unpack) id . parseName
        env sets = buildEnv gen (M.fromList sets) now
        problemKey = \case
          MissingInput s -> "missing:" <> inputName s
          InvalidInput n _ -> "invalid:" <> n
          UnknownInput n -> "unknown:" <> n
    it "resolves idents, record lists with defaults, derived values and now" $ do
      let Right e = env [("name", "blog_post"), ("fields", "title, body:text")]
          field n t = VRecord (M.fromList [("name", nameV n), ("type", VText t)])
      M.lookup "table" e `shouldBe` Just (VText "blog_posts")
      M.lookup "Model" e `shouldBe` Just (VText "BlogPost")
      M.lookup "fields" e `shouldBe` Just (VList [field "title" "string", field "body" "text"])
      M.lookup "now" e `shouldBe` Just (VTime now)
    it "rejects an ident that looks like a name followed by a type" $
      case env [("name", "SetDisplayName String")] of
        Left (InputProblems [InvalidInput "name" reason]) -> reason `shouldSatisfy` T.isInfixOf "looks like"
        other -> expectationFailure ("expected an invalid ident, got " <> show other)
    it "still accepts title-case and space-separated names" $
      fmap (M.lookup "table") (env [("name", "Blog Post")]) `shouldBe` Right (Just (VText "blog_posts"))
    it "splits list items and record fields outside brackets only" $ do
      let toml = "description = \"d\"\nlanguage = \"x\"\n[inputs.msgs]\ntype = \"list\"\nof = [{ name = \"name\", type = \"ident\" }, { name = \"arg\", type = \"text\", default = \"\" }]\n"
          g = either (error . T.unpack) id (load toml)
          rec n a = VRecord (M.fromList [("name", nameV n), ("arg", VText a)])
      fmap (M.lookup "msgs") (buildEnv g (M.fromList [("msgs", "set_pair:(Int, String), set_rec:{ a : Int }, plain, got:Dict String (List Int)")]) now)
        `shouldBe` Right (Just (VList [rec "set_pair" "(Int, String)", rec "set_rec" "{ a : Int }", rec "plain" "", rec "got" "Dict String (List Int)"]))
    it "refuses an empty value for a required list (an optional one may be empty)" $ do
      let toml opt = "description = \"d\"\nlanguage = \"x\"\n[inputs.msgs]\ntype = \"list\"\nof = \"ident\"\n" <> (if opt then "optional = true\n" else "")
          run opt v = buildEnv (either (error . T.unpack) id (load (toml opt))) (M.fromList [("msgs", v)]) now
      either (\case InputProblems ps -> map problemKey ps; _ -> []) (const []) (run False " , ") `shouldBe` ["invalid:msgs"]
      fmap (M.lookup "msgs") (run True "") `shouldBe` Right (Just (VList []))
    it "gives optional lists an empty default" $
      fmap (M.lookup "fields") (env [("name", "post")]) `shouldBe` Right (Just (VList []))
    it "reports missing, invalid and unknown inputs all at once" $
      case env [("fields", "title:bogus"), ("colour", "red")] of
        Left (InputProblems ps) -> map problemKey ps `shouldMatchList` ["missing:name", "invalid:fields", "unknown:colour"]
        other -> expectationFailure ("expected input problems, got " <> show other)
    it "lets --set override a derived value (irregular plurals)" $
      fmap (M.lookup "table") (env [("name", "octopus"), ("table", "octopodes")]) `shouldBe` Right (Just (VText "octopodes"))
    it "evaluates derived values in dependency order, keeping single-expression types" $ do
      let toml = "description = \"d\"\nlanguage = \"x\"\n[inputs.name]\ntype = \"ident\"\n[derive]\na_second = \"{= z_first =}!\"\nz_first = \"{= name != name|singular =}\"\n"
          g = either (error . T.unpack) id (load toml)
      fmap (\e -> (M.lookup "z_first" e, M.lookup "a_second" e)) (buildEnv g (M.fromList [("name", "posts")]) now)
        `shouldBe` Right (Just (VBool True), Just (VText "true!"))
    it "parses a --set override as the type of the derived value it replaces" $ do
      let toml = "description = \"d\"\nlanguage = \"x\"\n[inputs.name]\ntype = \"ident\"\n[derive]\nis_list = \"{= name != name|singular =}\"\nplural = \"{= name|plural =}\"\nlabel = \"{= if is_list =}many{= else =}one{= end =}\"\n"
          g = either (error . T.unpack) id (load toml)
          envWith sets = buildEnv g (M.fromList sets) now
      fmap (\e -> (M.lookup "is_list" e, M.lookup "label" e)) (envWith [("name", "blog_post"), ("is_list", "false")])
        `shouldBe` Right (Just (VBool False), Just (VText "one"))
      fmap (M.lookup "plural") (envWith [("name", "octopus"), ("plural", "octopodes")]) `shouldBe` Right (Just (nameV "octopodes"))
      case envWith [("name", "post"), ("is_list", "maybe")] of
        Left (InputProblems ps) -> map problemKey ps `shouldBe` ["invalid:is_list"]
        other -> expectationFailure ("expected an invalid override, got " <> show other)
    it "detects cycles between derived values" $ do
      let toml = "description = \"d\"\nlanguage = \"x\"\n[derive]\na = \"{= b =}\"\nb = \"{= a =}\"\n"
          g = either (error . T.unpack) id (load toml)
      buildEnv g M.empty now `shouldBe` Left (DeriveCycle ["a", "b"])
  describe "describeInputType" $
    it "shows the CLI shape of each type" $
      map describeInputType [ScalarInput IdentS, ScalarInput (EnumS ["a", "b"]), ListInput (ScalarItems IdentS), ListInput (RecordItems [FieldSpec "name" IdentS Nothing, FieldSpec "type" TextS Nothing])]
        `shouldBe` ["ident", "enum(a|b)", "list<ident>", "list<name:type>"]
