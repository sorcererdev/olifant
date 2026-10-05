-- | `olifant schema`: a JSON Schema for generator.toml (TOML maps onto JSON),
-- so an author (or a model) can see and check the whole format at once. Key
-- lists come from the loader, so the schema cannot drift from what loads.
module Olifant.Generator.Schema (generatorSchema) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Key qualified as K
import Data.Text (Text)
import Olifant.Generator.Load (completeKeys, createKeys, inputKeys, insertKeys, requiresKeys, topKeys, useKeys)

generatorSchema :: Value
generatorSchema =
  object
    [ "$schema" .= ("https://json-schema.org/draft/2020-12/schema" :: Text)
    , "title" .= ("generator.toml" :: Text)
    , "description" .= ("A generator lives at .olifant/generators/NAME/generator.toml (or a pack's generators/NAME/); NAME is its name. Templates use {= expr =} with filters after a bar: {= name|pascal =}. Filters: snake, kebab, pascal, camel, constant, title, lower_words, plural, singular, timestamp, parens. `olifant new NAME` writes a valid skeleton." :: Text)
    , "type" .= str "object"
    , "required" .= (["description", "language"] :: [Text])
    , "additionalProperties" .= False
    , "properties" .= props topKeys topProp
    ]
  where
    topProp = \case
      "description" -> desc "string" "One line, shown by list and show."
      "language" -> desc "string" "The language adapter (e.g. elm): its compiler checks `complete` edits."
      "version" -> desc "integer" "Defaults to 1."
      "inputs" ->
        object
          [ "type" .= str "object"
          , "description" .= str "One [inputs.NAME] table per input. Inputs are required unless optional = true."
          , "additionalProperties" .= object ["type" .= str "object", "additionalProperties" .= False, "required" .= (["type"] :: [Text]), "properties" .= props inputKeys inputProp]
          ]
      "derive" -> object ["type" .= str "object", "description" .= str "[derive] NAME = \"{= expr =}\": values computed from inputs, in order.", "additionalProperties" .= object ["type" .= str "string"]]
      "requires" -> object ["type" .= str "object", "additionalProperties" .= False, "properties" .= props requiresKeys requiresProp]
      "edits" ->
        object
          [ "type" .= str "array"
          , "description" .= str "[[edits]] tables (double brackets), applied in order. Each has exactly one of create, insert or complete."
          , "items" .= object ["oneOf" .= [edit "create" createKeys, edit "insert" insertKeys, edit "complete" completeKeys]]
          ]
      "uses" -> object ["type" .= str "array", "description" .= str "[[uses]]: run other generators as part of this one.", "items" .= object ["type" .= str "object", "additionalProperties" .= False, "required" .= (["generator"] :: [Text]), "properties" .= props useKeys useProp]]
      "examples" -> object ["type" .= str "array", "items" .= object ["type" .= str "string"], "description" .= str "Real commands, printed by show (agents copy them)."]
      "context" -> object ["type" .= str "array", "items" .= object ["type" .= str "string"], "description" .= str "Globs (templates over the inputs) of files the model sees when it fills this generator's holes, e.g. the models a codec reads."]
      "name" -> object ["type" .= str "string", "description" .= str "What editors show for this generator (\"Add Command Dependency\"); default: from the id."]
      "applies_at" -> object ["type" .= str "object", "description" .= str "Where this generator applies, for editors (olifant actions): path = \"{slot}/…\" (slots bind inputs), node = an ast-grep rule the cursor must be inside, bind = { input = \"regex\" } over that node's text, label = a template, where = what the rule looks for in words (shown when nothing applies), suggest = true lets editors offer it unasked (ghost text or a follow-up), trigger = a POSIX regex the text before the cursor must end with (a snippet's anticipation), after = an ast-grep rule the cursor's line must follow (only blank lines between; instead of node)."]
      "query" -> object ["type" .= str "object", "description" .= str "[query.NAME]: bind NAME to records from a glob, command or AST query; value = \"file name\" | \"parent folder\" | \"path\" | a field | a template, for an enum that is one_of it (see the user book, chapter 6).", "additionalProperties" .= object ["type" .= str "object"]]
      _ -> object []
    inputProp = \case
      "type" -> object ["enum" .= (["ident", "text", "bool", "enum", "list"] :: [Text]), "description" .= str "ident: a name (case filters work); text; bool; enum (needs choices or one_of); list (needs of)."]
      "optional" -> desc "boolean" "Default false: the input is required."
      "default" -> object ["description" .= str "A default value (makes --set optional)."]
      "prompt" -> desc "string" "Help text shown by show."
      "choices" -> object ["type" .= str "array", "items" .= object ["type" .= str "string"]]
      "one_of" -> desc "string" "The name of a [query.NAME] whose records are the choices; the query's value (file name | parent folder | path | a field | a {= … =} template) picks each choice. The query may use inputs declared above this one."
      "choices_from" -> desc "object" "Retired: use one_of and a [query.NAME]."
      "of" -> object ["description" .= str "List items: a scalar type name, or an array of { name, type } fields."]
      _ -> object []
    edit kind keys =
      object
        [ "type" .= str "object"
        , "required" .= [kind]
        , "additionalProperties" .= False
        , "properties" .= props keys (editProp kind)
        ]
    editProp kind = \case
      k | k == kind -> desc "string" (case kind of
            "create" -> "The new file's path (a template), e.g. \"src/Page/{= name|pascal =}.elm\"."
            "insert" -> "The existing file to insert into, e.g. \"src/Main.elm\". Needs `at`."
            _ -> "The constructor this generator adds, e.g. \"Route.{= name|pascal =}\"; the compiler finds the case expressions missing it.")
      "at" -> object ["description" .= str "Where, as a phrase: start of / end of / before / after + a target (`olifant anchors LANGUAGE` lists them; `olifant where FILE \"PHRASE\"` tests one), or a list of phrases tried in order."]
      "where" -> desc "string" "Which case this body is for, e.g. \"case in changeRouteTo\"."
      "text" -> desc "string" "The text (a template). In complete bodies, {= pattern =} is the missing case."
      "template" -> desc "string" "A file in the generator's templates/ directory, instead of text."
      "exemplar" -> desc "string" "A real file copied with names renamed (needs bind)."
      "fragment" -> desc "object" "One item of a real file's target: { file, items_of, matching } (needs bind)."
      "bind" -> desc "object" "For exemplars and fragments: { input = \"name_in_the_file\" }."
      "on_exists" -> object ["enum" .= (["error", "skip", "overwrite"] :: [Text])]
      "unless" -> desc "object" "{ exists = \"glob\" }: skip the edit when a file matches; { matches = \"regex\" }: skip it when a line of its own file matches (already there by name)."
      "anchor" -> desc "object" "Older spelling of at = \"before marker …\"; use at."
      _ -> object []
    requiresProp = \case
      "exists" -> object ["type" .= str "array", "items" .= object ["type" .= str "string"]]
      _ -> object ["type" .= str "string"]
    useProp = \case
      "generator" -> desc "string" "The generator to run (pack/name, or a name in the same pack)."
      "for_each" -> desc "string" "A list value: run once per item (bound as item)."
      _ -> desc "object" "Its inputs, as templates."
    props keys f = object [K.fromText k .= f k | k <- keys]
    desc t d = object ["type" .= str t, "description" .= str d]
    str :: Text -> Text
    str = id
