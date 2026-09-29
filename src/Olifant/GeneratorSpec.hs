{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.GeneratorSpec (spec) where

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
        [ ("fields", ListInput (RecordItems [FieldSpec "name" IdentS Nothing, FieldSpec "type" (EnumS ["string", "text", "references"]) (Just "string")]), True)
        , ("name", ScalarInput IdentS, False)
        ]
  it "reads derived values as templates" $
    fmap (map (fmap templateSource) . genDerived) (load modelToml)
      `shouldBe` Right [("Model", "{= name|pascal =}"), ("table", "{= name|plural|snake =}")]
  it "reads create and insert edits" $
    fmap (map describeEdit . genEdits) (load modelToml)
      `shouldBe` Right ["create app/models/{= name|snake =}.rb from model.rb.dt", "insert config/routes.rb at # olifant:routes"]
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
  it "rejects unknown keys in a [[uses]] step" $
    load "description = \"d\"\nlanguage = \"x\"\n[[uses]]\ngenerator = \"g\"\nforeach = \"xs\"\n" `shouldSatisfy` isLeft
  it "rejects misspelled keys, naming them" $ do
    let result = load (T.replace "template = " "templete = " modelToml)
    result `shouldSatisfy` isLeft
    either id (const "") result `shouldSatisfy` T.isInfixOf "templete"
  it "rejects an edit that is both create and insert" $
    load "description = \"d\"\nlanguage = \"x\"\n[[edits]]\ncreate = \"a\"\ninsert = \"b\"\ntext = \"t\"\n" `shouldSatisfy` isLeft
  it "reports template syntax errors with the key" $ do
    let result = load (T.replace "{= name|pascal =}" "{= name|shout =}" modelToml)
    either id (const "") result `shouldSatisfy` T.isInfixOf "Model"
  where
    describeEdit = \case
      Create p (TemplateFile f) _ -> "create " <> templateSource p <> " from " <> T.pack f
      Insert p (Marker m) _ -> "insert " <> templateSource p <> " at " <> m
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
