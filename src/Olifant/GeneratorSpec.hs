module Olifant.GeneratorSpec (spec) where

import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Generator
import Olifant.Template (templateSource)
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
    , "table = \"{{ name|plural|snake }}\""
    , "Model = \"{{ name|pascal }}\""
    , "[[edits]]"
    , "create = \"app/models/{{ name|snake }}.rb\""
    , "template = \"model.rb.dt\""
    , "[[edits]]"
    , "insert = \"config/routes.rb\""
    , "anchor = { marker = \"# olifant:routes\" }"
    , "text = \"resources :{{ table }}\""
    ]

load :: Text -> Either Text GeneratorDef
load = parseGenerator (GeneratorId "rails/model")

spec :: Spec
spec = describe "parseGenerator" $ do
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
      `shouldBe` Right [("Model", "{{ name|pascal }}"), ("table", "{{ name|plural|snake }}")]
  it "reads create and insert edits" $
    fmap (map describeEdit . genEdits) (load modelToml)
      `shouldBe` Right ["create app/models/{{ name|snake }}.rb from model.rb.dt", "insert config/routes.rb at # olifant:routes"]
  it "reads exemplar sources with their bindings" $ do
    let toml = "description = \"d\"\nlanguage = \"rust\"\n[[edits]]\ncreate = \"x.rs\"\nexemplar = \"fixture:src/refund_invoice.rs\"\nbind = { command_name = \"refund_invoice\" }\n"
    fmap (map (\e -> case editSource e of ExemplarFile ref bs -> (ref, length bs); _ -> ("?", 0)) . genEdits) (load toml)
      `shouldBe` Right [("fixture:src/refund_invoice.rs", 1)]
  it "rejects misspelled keys, naming them" $ do
    let result = load (T.replace "template = " "templete = " modelToml)
    result `shouldSatisfy` isLeft
    either id (const "") result `shouldSatisfy` T.isInfixOf "templete"
  it "rejects an edit that is both create and insert" $
    load "description = \"d\"\nlanguage = \"x\"\n[[edits]]\ncreate = \"a\"\ninsert = \"b\"\ntext = \"t\"\n" `shouldSatisfy` isLeft
  it "reports template syntax errors with the key" $ do
    let result = load (T.replace "{{ name|pascal }}" "{{ name|shout }}" modelToml)
    either id (const "") result `shouldSatisfy` T.isInfixOf "Model"
  where
    describeEdit = \case
      Create p (TemplateFile f) _ -> "create " <> templateSource p <> " from " <> T.pack f
      Insert p (Marker m) _ -> "insert " <> templateSource p <> " at " <> m
      _ -> "unexpected"
