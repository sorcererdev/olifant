# 7. Writing your own generator

A generator is a directory with a `generator.toml` in it. Put one in your project's
`.olifant/generators/<name>/` and `olifant list` shows it, with no pack and no prefix. This
chapter builds three, from the quickest kind to the most structural, in an Elm sandbox:

```sh
scripts/try-pack.sh elm /tmp/olifant-gen && cd /tmp/olifant-gen
```

## Start from a skeleton

`olifant new NAME` writes `.olifant/generators/NAME/generator.toml` with the parts every
generator has (a description, the language, one `ident` input, a `[derive]`d PascalCase
name) and the three kinds of edit as commented examples to adapt. It loads as written, so
`olifant list` shows it at once:

```
$ olifant new route
created .olifant/generators/route/generator.toml
fill in description and [[edits]]; `olifant anchors elm` lists placement phrases, `olifant where FILE "PHRASE"` tests one, `olifant gen route --set name=example --plan` previews
```

With more than one language adapter, pass `--language`. `olifant schema` prints the whole
format as a JSON Schema (every key, what it takes, the filters), built from the same key
lists the loader checks, so the two cannot disagree.

When a key is wrong, the error says what was probably meant: `path` on an edit ("the file
is the value of `insert`"), `content` ("did you mean `text`?"), `required` on an input
("inputs are required unless `optional = true`"), a typo like `att` ("did you mean
`at`?"), `[edits]` with single brackets, or a filter written `pascalCase(name)` or
`name|capitalize` (both point at `{= name|pascal =}`), or `{{ name|pascal }}` written in
another template language (a template with `{{ … }}` and no `{=` tag is refused rather than
copied through as text). These are the mistakes models made
most when writing generators (token experiment v9).

## The quickest generator: copy a real file (an exemplar)

Most conventions start life as a file someone already wrote. Write one by hand, and make
sure its name has **at least two words**:

```elm
-- src/View/UserCard.elm
module View.UserCard exposing (userCard)

import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (class)


userCard : Html msg
userCard =
    div [ class "user-card" ]
        [ h2 [] [ text "User Card" ] ]
```

Then point a generator at it:

```toml
# .olifant/generators/view/generator.toml
description = "A reusable view function in src/View/"
language = "elm"

[inputs.name]
type = "ident"
prompt = "View name (e.g. order_summary)"

[[edits]]
create   = "src/View/{= name|pascal =}.elm"
exemplar = "src/View/UserCard.elm"
bind     = { name = "user_card" }
```

```
$ olifant gen view --set name=order_summary --plan
Plan: view (1 edit, 0 conflicts)
  + src/View/OrderSummary.elm (10 lines)

--- src/View/OrderSummary.elm
module View.OrderSummary exposing (orderSummary)

import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (class)


orderSummary : Html msg
orderSummary =
    div [ class "order-summary" ]
        [ h2 [] [ text "Order Summary" ] ]
```

`bind = { name = "user_card" }` says "the example value of the input `name` in this file is
`user_card`". Olifant replaces it in every case form (`UserCard`, `userCard`, `user-card`,
`User Card`, `USER_CARD`, `user card`), singular and plural (`UserCards`), wherever it
stands as a whole word or a camel hump. That is why the example needs two words: with
`bind = { name = "card" }`, `card` and `Card` would be the only forms, and Olifant could not
tell the snake form from the lower-words form, so it refuses:

```
error: view: …/src/View/UserCard.elm: bind `name`: example `card` needs at least two words so that every case form is distinct
```

Rules for exemplars:

- Only `create` edits take an `exemplar`; it needs `bind`. Each bound input is replaced by
  the value you pass for it.
- In a project generator the path is relative to the project root. In a pack, write
  `fixture:src/Page/SamplePage.elm`, relative to the pack's `fixtures/app/` (`elm/page`
  does this, so the fixture app is the pack's documentation and its exemplar at once).
- Only whole forms are replaced: a lone `user` or `Card` elsewhere in the file stays as it
  is. If two bindings would produce the same text (say `user_card` and `user_cards`), the
  load fails with "comes from more than one binding".
- The exemplar is copied literally apart from those names, so `{=` in it is just text.
- A broken exemplar (missing file, or an example value that is not two words) shows up in
  `olifant list` as `INVALID`, and `show` refuses the generator — not only when you run it.

When the file needs to vary by more than a name (a list of fields, an optional section), move
to a template.

## A generator with a template

Goal: `olifant gen record --set name=blog_post --set fields="title, views:Int, published:Bool"`
creates `src/Data/BlogPost.elm` with a record type and an `empty` value.

```toml
# .olifant/generators/record/generator.toml
description = "A record type alias with an `empty` value, in src/Data/"
language = "elm"
version = 1
examples = [
  "olifant gen record --set name=blog_post --set fields=\"title, views:Int, published:Bool\"",
]

[inputs.name]
type = "ident"
prompt = "Record name (e.g. blog_post)"

[inputs.fields]
type = "list"
prompt = "Fields as name or name:Type, comma separated (Type: String, Int, Float or Bool; default String)"
of = [
  { name = "name", type = "ident" },
  { name = "type", type = "enum", choices = ["String", "Int", "Float", "Bool"], default = "String" },
]

[inputs.doc]
type = "text"
optional = true
prompt = "A one-line description for the doc comment"

[derive]
Name = "{= name|pascal =}"
module = "Data.{= Name =}"

[requires]
exists = ["elm.json"]
hint = "Run this from the root of an Elm app."

[[edits]]
create   = "src/Data/{= Name =}.elm"
template = "record.elm.dt"
```

Templates live in the generator's `templates/` directory, so this one is
`.olifant/generators/record/templates/record.elm.dt`:

```
module {= module =} exposing ({= Name =}, empty)
{=- if doc =}

{-| {= doc =}
-}
{=- end =}


type alias {= Name =} =
{=- for f in fields =}
    {= if loop.first =}{{= else =},{= end =} {= f.name|camel =} : {= f.type =}
{=- end =}
    }


empty : {= Name =}
empty =
{=- for f in fields =}
    {= if loop.first =}{{= else =},{= end =} {= f.name|camel =} = {= if f.type == "String" =}""{= elif f.type == "Bool" =}False{= else =}0{= end =}
{=- end =}
    }
```

(The template language has no comments; everything outside `{= … =}` is output.)

`show` reads it back as a user would see it:

```
$ olifant show record
record: A record type alias with an `empty` value, in src/Data/

inputs:
  --set doc=<text>  (optional)
      A one-line description for the doc comment
  --set fields=<list<name:type>>
      Fields as name or name:Type, comma separated (Type: String, Int, Float or Bool; default String)
  --set name=<ident>
      Record name (e.g. blog_post)

edits:
  create src/Data/{= Name =}.elm

examples:
  olifant gen record --set name=blog_post --set fields="title, views:Int, published:Bool"
```

```
$ olifant gen record --set name=blog_post --set fields="title, views:Int, published:Bool" --set doc="A post on the blog." --plan
Plan: record (1 edit, 0 conflicts)
  + src/Data/BlogPost.elm (19 lines)

--- src/Data/BlogPost.elm
module Data.BlogPost exposing (BlogPost, empty)

{-| A post on the blog.
-}


type alias BlogPost =
    { title : String
    , views : Int
    , published : Bool
    }


empty : BlogPost
empty =
    { title = ""
    , views = 0
    , published = False
    }
```

Without `--set doc`, the `if` renders nothing and the module line is followed directly by the
two blank lines. The output compiles (`elm make src/Data/BlogPost.elm`). Bad inputs are
refused before anything is planned, all at once, with exit code 4:

```
$ olifant gen record --set name=blog_post --set fields="title, views:Integer"
error: record: invalid inputs: `fields`: expected one of String, Int, Float, Bool; got `Integer`

$ olifant gen record --set fields="title" --set colour=red
error: record: invalid inputs: unknown input `colour`; missing `name` (ident)
```

The rest of this chapter is the reference for each part of `generator.toml`.

## Top-level keys

| key | required | what it does |
|---|---|---|
| `name` | no | what editors show ("Add Command Dependency"); without it, made from the id (`command-dependency` → "Command Dependency"). The id is still the directory name |
| `description` | yes | one line, shown by `list` and `show` |
| `language` | yes | names the language adapter whose `[check]` runs this generator's `complete` edits (chapter 4); otherwise informational |
| `version` | no (1) | shown in `show --json`; nothing else reads it yet |
| `examples` | no | example commands, printed by `show`. Agents copy these, so make them real |
| `[inputs.*]`, `[derive]`, `[requires]`, `[[edits]]`, `[[uses]]` | no | below; `[[uses]]` is [chapter 8](08-composing-generators.md) |
| `[query.*]` | no | binds a query's records to a name, after inputs and `[derive]` ([chapter 6](06-queries-and-choices.md#queries-in-generators-queryname)); an enum that is `one_of` it takes its choices from it, with `value` saying which part of each record ([chapter 6](06-queries-and-choices.md#one_of-inputs-whose-values-come-from-the-project)) |
| `[applies_at]` | no | where in the code the generator applies, for editors: `path` (a pattern whose `{slot}`s bind inputs), `node` (an ast-grep rule the cursor must be inside), `bind` (inputs from regexes over that node's text), `file` (an input that takes the file's path) and `label`. `olifant actions` reads it ([chapter 14](14-editors-and-actions.md#applies_at-where-a-generator-applies)) |

Any other key is an error that names the key, so typos fail at load (`list` marks the
generator `INVALID`, `gen` exits 1).

## Inputs

```toml
[inputs.page]
type = "ident"             # ident | text | bool | enum | list
optional = true            # default false
default = "home"           # used when --set does not give one
prompt = "Page name (e.g. counter, article_list)"   # shown by `show`
```

| type | accepts | in templates |
|---|---|---|
| `ident` | a name in any case: `blog_post`, `BlogPost`, `blog post`, `blog-post` (letters, digits, `_`, `-`, space; not starting with a digit) | a name; filters give its case forms; printed bare it is snake_case |
| `text` | anything | text |
| `bool` | `true` / `false` (any capitalisation) | a bool |
| `enum` | one of `choices = [...]`, or `one_of = "NAME"`: one of the values a `[query.NAME]` finds in the project, which may depend on the inputs above it ([chapter 6](06-queries-and-choices.md#one_of-inputs-whose-values-come-from-the-project)) | text |
| `list` | comma-separated items, `of` one of the types above, or a record | a list |

An `ident` that looks like a name followed by a type is refused, because it is almost always
a mistake: `--set name="SetName String"` gets "looks like a name followed by other words (e.g.
a type); pass only the name, like SetName". Enum choices are exact and case-sensitive; a near
miss gets "did you mean".

`olifant schema` describes `one_of` (and still lists `choices_from`, as retired, so a
schema-aware editor flags it):

```
$ olifant schema --json | jq -c '.properties.inputs.additionalProperties.properties.one_of'
{"description":"The name of a [query.NAME] whose records are the choices; the query's value (file name | parent folder | path | a field | a {= … =} template) picks each choice. The query may use inputs declared above this one.","type":"string"}
$ olifant schema --json | jq -c '.properties.inputs.additionalProperties.properties.choices_from'
{"description":"Retired: use one_of and a [query.NAME].","type":"object"}
```

An optional input that is not given is an empty list, `false`, empty text, or, for `ident`
and `enum`, *nothing*: it prints as the empty string and is false in an `if`, but a filter on
it fails ("`pascal` needs a name, got nothing"). Guard it with `{= if x =}`.

### Lists and record lists

`of = "ident"` makes a plain list: `--set tags="red, big_blue, x"`. Items are split on commas
that are not inside `()`, `[]` or `{}`, and trimmed; empty items are dropped.

`of = [ … ]` makes each item a record, written `field1:field2:…`:

```toml
[inputs.msgs]
type = "list"
optional = true
of = [
  { name = "name", type = "ident" },
  { name = "arg",  type = "text", default = "" },
]
```

`--set msgs="toggle_dark_mode, set_display_name:String"` gives two records; the first has
`arg = ""` from its default. Fields are split on `:`, and the **last field takes the rest**,
so `got_scores:Dict String Int` or `pair:(Int, String)` stay whole. A field can be an
`enum` with its own `choices` and `default` (`rails/model` does this for column types). In a
template, read fields with a dot: `{= item.name =}`, `{= f.type =}`.

A required list refuses an empty value: `--set fields=` fails invalid inputs, "needs at
least one item (it is not optional)". An optional list may be empty (`--set tags=` is an
empty list, `{= if tags =}` sees it as false); make a list optional if an empty one makes
sense for the template.

## Derived values: `[derive]`

```toml
[derive]
Name   = "{= name|pascal =}"
module = "Data.{= Name =}"
```

Derived values are templates over the inputs and other derived values, evaluated in
dependency order (a cycle is an error naming them). Two things are worth knowing:

- A derived value that is exactly one `{= … =}` keeps that expression's type. Redwood's
  `is_list = "{= name != name|singular =}"` is a real bool, so `{= if not is_list =}` works.
- `--set` can override a derived value. `--set Name=Post` makes the file
  `src/Data/Post.elm`; an override of a bool-valued derived value must be `true`/`false`.

`now` is always defined: the current time, or the time given with `--now 2026-09-28T10:00:00Z`
(pin it in tests and anywhere output must be reproducible).

## Preconditions: `[requires]`

```toml
[requires]
exists = ["src/Page/{= page|pascal =}.elm"]
hint = "Create the page first: olifant gen elm/page --set page={= page =}"
```

Each path must exist on disk or be created earlier in the same plan. If not, the run is a
conflict that prints the hint, so the hint should say how to fix it.

## Edits

Every `[[edits]]` entry is exactly one of `create`, `insert` or `complete`. Paths and `at`
phrases are templates too.

### `create`

```toml
[[edits]]
create    = "src/Data/{= Name =}.elm"
template  = "record.elm.dt"     # or: text = "…", or: exemplar = "…" + bind = {…}
on_exists = "error"             # error (default) | skip | overwrite
unless    = { exists = "db/migrate/*_create_{= table =}.rb" }   # or: { matches = "REGEX" }
```

If the file already exists with exactly the generated content, the edit is skipped
(`unchanged`) whatever `on_exists` says. Otherwise `error` is a conflict ("already exists
(on_exists = "error")"), `skip` leaves the file alone (`skipped: already exists`), and
`overwrite` replaces it (`~ path (overwritten, N lines)`). Prefer `error`: it is what keeps
hand edits safe.

`unless` takes exactly one key. `exists` skips the edit when its glob matches a file on
disk or one created earlier in the plan; `matches` skips it when any line of the edit's own
target, as the plan has it so far, matches a POSIX extended regex (chapter 6).

### `insert`

```toml
[[edits]]
insert = "src/Main.elm"
at     = "end of type Page"            # or a list of phrases, tried in order
text   = "| {= P =} {= P =}.Model"     # or: template = "…"
unless = { matches = "^[[:space:]]*\\| {= P =}( |$)" }   # or: { exists = "…" }
```

`at` is covered in [chapter 3](03-anchors.md): a phrase, or a list of phrases where the
first that resolves wins (the JSON says which, in `anchor_used`). Two other forms exist:

- `anchor = { marker = "-- MAIN" }` is an older spelling of `at = "before marker -- MAIN"`.
  Use `at`.
- `at = { ast = { kind = "type_declaration", regex = "^type Msg" }, pos = "after" }` is a raw
  ast-grep rule with a position (`start`, `end`, `before`, `after`), for a one-off spot no
  adapter target covers. The plan reports it as `anchor_used: "ast rule (after)"`. If you
  need it twice, add a target to the language adapter instead ([chapter 9](09-packs-and-language-adapters.md)).

Test a phrase before you write it into a generator: `olifant where src/Main.elm "end of
record Model"` shows the spot or says why not (for `Page/Home.elm`, whose `Model` is `{}`,
it opens the record instead of refusing — [chapter 3](03-anchors.md)). A phrase with no `{= … =}` in it has both its shape and its
target name checked against the language adapter when the generator loads — `list` marks it
`INVALID` and `show`/`gen` refuse it — so a typo like `end of typo Msg` is caught before
anyone runs the generator, with a suggestion:

```
$ olifant show badtarget
error: badtarget: `end of typo Msg` in src/Page/{= page|pascal =}.elm: unknown target `typo Msg` for elm; did you mean `end of type Msg`? (targets: case in FUNCTION, imports, list in FUNCTION, record NAME, type NAME)
```

A phrase with a `{= … =}` in it — the target name itself varies, like `end of cases in
{= page =}` — cannot be checked until the value is known, so it stays a conflict at run
time.

### Fragments: insert text copied from a real file

An exemplar copies a whole file. A **fragment** copies one item out of a file, for an
`insert`, so the wiring code is also real code that compiles. `elm/page`'s `update` branch
is the fixture's own `SamplePage` branch:

```toml
[[edits]]
insert   = "src/Main.elm"
at       = "end of case in update"
fragment = { file = "fixture:src/Main.elm", items_of = "case in update", matching = "GotSamplePageMsg" }
bind     = { page = "sample_page" }
```

- `items_of` is a target, the same vocabulary as `at` (`case in update`, `type Route`,
  `record Model`). Its items are the candidates.
- `matching` is a POSIX regular expression, checked when the generator is read (so `list`
  marks a bad one `INVALID`). Exactly one item must match it.
- The item's text is dedented and then renamed like an exemplar: every case form of each
  `bind` value (`sample_page`, `SamplePage`, `samplePage`, …) becomes the input.
- `file = "fixture:…"` reads from the pack's fixture; a plain path reads the project's own
  file, so a project generator can copy "a branch like the ones already there". Either way
  the path must stay inside (no absolute paths, no `..`).
- A fragment replaces `text`/`template`: an insert with both is a load error, and so is
  `bind` on an edit with neither an exemplar nor a fragment.

With `--set page=settings` the branch comes out as:

```elm
        ( GotSettingsMsg subMsg, Settings page ) ->
            Settings.update subMsg page |> updateWith Settings GotSettingsMsg model
```

Only the bound names change. In a fragment of `( GotProfileMsg subMsg, Profile page ) -> …`
bound on `got_profile_msg` alone, `Profile page` stays `Profile page`, so bind every name
that varies.

A `matching` that fits several items is caught as soon as the generator loads — `list` marks
it `INVALID`, and `show` and `gen` all refuse it the same way:

```
$ olifant gen fragtest --set page=x --plan
error: fragtest: …/src/Main.elm: fragment `case in update` matching `Got` matched 2 items (expected 1)
```

The blank line before the new branch is not in the fragment: `end of` copies the spacing
between the existing branches ([chapter 3](03-anchors.md#spacing)).

### `complete`

```toml
[[edits]]
complete = "Route.{= R =}"             # the constructor this generator adds
where    = "case in changeRouteTo"     # optional: which case this body is for
text     = """
{= pattern =} ->
    {= page =}.init |> updateWith {= page =} Got{= page =}Msg model"""
```

See [chapter 4](04-compiler-branches-and-todos.md). The body can also use `{= site.file =}`
and `{= site.function =}`. It needs the adapter named by the generator's `language` to have a
`[check]`.

The body can hold a typed hole, so that a model writes each new arm's value:
`rust-htmx/command-failure`'s body is `{= pattern =} => {= ? status: expr | … =},`. Each site
the compiler finds gets its own hole, named `status[file:line]` when there is more than one
([chapter 12](12-holes-and-models.md#where-holes-may-go)). The hole sees the site's
`{= pattern =}` but not the payload's fields, which the compiler's pattern leaves as `_`.

`text`/`template` is optional. Leave both out and every site the compiler finds gets a todo
branch — there is no body to try, so nothing is ever matched or chosen. That is how
`olifant-dev/error-case` works: it only adds a constructor and leaves every `case` for a
person (or a model) to fill in, each one listed as a todo in the plan.

How you write the constructor depends on the adapter: Elm's `[check] format` (`elm-json`)
names it `Module.Ctor` (`Route.{= R =}`, visible by module import); GHC's (`ghc-json`) names
it `Type.Ctor` (`{= T =}.{= C =}`, e.g. `CatalogError.PackLocked`, visible where that type
appears in the type GHC reports: `T`, `Maybe T`, `(T, Bool)`); rustc's (`rustc-json`) names it
`Type::Variant` (`{= Failure =}::{= failure|pascal =}`, e.g.
`RefundInvoiceFailure::AlreadyRefunded`; `Type.Variant` also works), visible where rustc
reports a `match` on that type.

## Putting inserts and `complete` together

The third generator gives an existing page a second URL. It has no file of its own: it edits
`Route.elm` in two places and lets the compiler find the third.

```toml
# .olifant/generators/alias/generator.toml
description = "Another URL for an existing page (e.g. /start shows Home)"
language = "elm"
version = 1
examples = [
  "olifant gen alias --set name=start --set page=Home",
]

[inputs.name]
type = "ident"
prompt = "Route name; the URL is its kebab-case form (start_here → /start-here)"

[inputs.page]
type = "enum"
one_of = "pages"
prompt = "The page it shows"

[derive]
R = "{= name|pascal =}"

# Every page module: src/Page/Home.elm → Home.
[query.pages]
glob = "src/Page/*.elm"

[requires]
exists = ["src/Route.elm", "src/Main.elm"]

[[edits]]
insert = "src/Route.elm"
at     = "end of type Route"
text   = "| {= R =}"

[[edits]]
insert = "src/Route.elm"
at     = "end of list in parser"
text   = ', Parser.map {= R =} (s "{= name|kebab =}")'

[[edits]]
complete = "Route.{= R =}"
where    = "case in changeRouteTo"
text     = """
{= pattern =} ->
    {= page =}.init |> updateWith {= page =} Got{= page =}Msg model"""
```

```
$ olifant show alias
…
inputs:
  --set name=<ident>
      Route name; the URL is its kebab-case form (start_here → /start-here)
  --set page=<enum(Home|SamplePage)>
      The page it shows
…
$ olifant gen alias --set name=start --set page=home
error: alias: invalid inputs: `page`: expected one of Home, SamplePage; got `home` (did you mean Home?)

$ olifant gen alias --set name=start --set page=Home
alias (3 edits, 0 conflicts)
  ~ src/Route.elm (+1 line at line 10)
  ~ src/Route.elm (+1 line at line 18)
  ~ src/Main.elm (+2 lines at line 56, compiler: changeRouteTo)
```

```diff
         Just Route.SamplePage ->
             SamplePage.init |> updateWith SamplePage GotSamplePageMsg model

+        Just Route.Start ->
+            Home.init |> updateWith Home GotHomeMsg model
+
```

The app compiles. The page list came from the project (`one_of = "pages"`; a glob's `value`
defaults to `"file name"`, so `src/Page/Home.elm` gives `Home`), and nothing in the generator
says where `changeRouteTo` is.

## The template language

Everything between `{=` and `=}` is template code; everything else is copied as is.

### Expressions

| form | example |
|---|---|
| variable | `{= page =}` |
| field of a record | `{= item.name =}`, `{= loop.index =}` |
| string, integer, bool literal | `{= "List String"|parens =}`, `{= 1 =}`, `true` |
| filters, left to right | `{= name|plural|pascal =}` |
| comparison | `==`, `!=` (a name equals text spelling it in snake_case) |
| logic | `and`, `or`, `not`, parentheses |

An undefined variable is an error when the template is rendered ("undefined variable `nme`"),
not when it loads: run the generator once with `--plan` after every edit to it.

### Filters

The case filters, from `src/Olifant/Naming.hs`, applied to `BlogPost`:

| filter | result |
|---|---|
| `snake` | `blog_post` |
| `kebab` | `blog-post` |
| `pascal` | `BlogPost` |
| `camel` | `blogPost` |
| `constant` | `BLOG_POST` |
| `title` | `Blog Post` |
| `lower_words` | `blog post` |

The others, from `src/Olifant/Template/Render.hs`:

| filter | does | example |
|---|---|---|
| `plural` | pluralises the last word; the result is still a name | `{= name|plural =}` → `blog_posts`, `{= name|plural|pascal =}` → `BlogPosts` |
| `singular` | the reverse | `blog_posts` → `blog_post` |
| `timestamp` | formats a time as `YYYYMMDDhhmmss` | `{= now|timestamp =}` → `20260929100000` |
| `parens` | wraps text with spaces in parentheses, unless it is already one group | `List String` → `(List String)`, `Int` → `Int`, `(Maybe Int)` unchanged |
| `rust_string` | escapes text for the inside of a Rust format-string literal (`todo!("…")`, `#[error("…")]`): `\` → `\\`, `"` → `\"`, `{` → `{{`, `}` → `}}` | `Too "late" for {refunds}` → `Too \"late\" for {{refunds}}` |
| `escaped` | escapes text for the inside of an Elm or Haskell string literal: `\` → `\\`, `"` → `\"` | `say "hi" to C:\tmp` → `say \"hi\" to C:\\tmp` |

`rust_string` is what the rust-htmx pack writes a free-text message with, in `command-failure`'s
`#[error("{= message|rust_string =}")]` and in its adapter's `placeholder`
(`todo!("{= message|rust_string =}")`, whose message holds a pattern that may contain `{ .. }`):
a quote ends the string early, and a brace starts a format argument. A scratch generator,
`rs`, whose one edit creates `out.rs` from the line `todo!("{= message|rust_string =}")`:

```
$ olifant gen rs --set message='Too "late" for {refunds}, C:\tmp' --plan --human
Plan: rs (1 edit, 0 conflicts)
  + out.rs (1 line)

--- out.rs
todo!("Too \"late\" for {{refunds}}, C:\\tmp")
```

Case filters also accept text that parses as a name, so `{= "blog post"|pascal =}` works.
`plural` knows common irregulars (`person` → `people`, `index` → `indices`) and uncountables
(`news`, `metadata`); anything else follows English suffix rules. An unknown filter fails at
load with its position (`unknown filter \`upper\``).

### Tags

```
{= if cond =} … {= elif other =} … {= else =} … {= end =}
{= for f in fields =} … {= end =}
```

Inside `for`, `loop.first`, `loop.last` and `loop.index` (from 1) are defined. Truthiness:
`false`, empty text, an empty list, `0` and *nothing* are false; everything else is true.

### Whitespace control

`{=-` removes all whitespace (newlines included) before the tag; `-=}` removes all whitespace
after it. The usual pattern for a line per item is to trim before both the `for` and the
`end`, and let the body start with its own newline:

```
type alias {= Name =} =
{=- for f in fields =}
    {= f.name|camel =} : {= f.type =}
{=- end =}
    }
```

Here is a small generator's output with every filter, a bool and a trimmed loop, taken from a
scratch generator run with `--set name=BlogPost --set tags="red,big_blue , x" --set
loud=true --now 2026-09-29T10:00:00Z`:

```
blog_post blog-post BlogPost blogPost BLOG_POST Blog Post blog post
blog_posts BlogPosts blog_post
LOUD now=2026-09-29T10:00:00Z
1:Red, 2:BigBlue, 3:X
parens: (List String) / Int / (Maybe Int)
```

`escaped` is what the Elm and Haskell adapters write a hole's instruction with, inside their
placeholders (`Debug.todo "olifant: {= hole =}: {= instruction|escaped =}"`). A scratch
generator, `esc`, whose one edit creates `Out.elm` from the line
`message = "{= t|escaped =}"`:

```
$ olifant gen esc --set t='say "hi" to C:\tmp' --plan --human
Plan: esc (1 edit, 0 conflicts)
  + Out.elm (1 line)

--- Out.elm
message = "say \"hi\" to C:\\tmp"
```

### Holes

```
{= ? name: kind | instruction =}
```

is a **typed hole**: a gap a model fills when the generator runs, written as a placeholder
that compiles when no model fills it. `kind` is one of the language adapter's hole kinds
(`expr` in Elm; `expr`, `block` or `type` in Rust). The instruction may use outputs
(`{= pattern =}`, `{= message =}`) but not `if` or `for`, and it may contain `=}` inside
its own tags: the hole ends at the `=}` that matches its `{=`. A hole can sit inside an `if`,
as in `elm/msg`, so it only exists when an input asks for it:

```
{= if intent =}{= ? update: expr | the update branch for {= pattern =}: {= intent =}; a ( Model, Cmd Msg ) =}{= else =}( model, Cmd.none ){= end =}
```

A hole without a `name:` and a `kind` is a load error ("a hole is `{= ? name: kind |
instruction =}`"). [Chapter 12](12-holes-and-models.md) covers how holes are filled, and
[chapter 13](13-tutorial-holes.md) writes a generator with one.

### A literal `{` before a tag

`{{= else =}` is a literal `{` followed by a tag, because Olifant looks for `{=`. There is no
escape for a literal `{=` in a template; if you need one, put it in an exemplar (which is
copied literally) or print it from a string (`{= "{=" =}`).

## Multi-line `text` in TOML

TOML drops the newline right after an opening `"""`. For `end of` and `start of` inserts
this no longer matters: the blank lines between items are copied ([chapter 3](03-anchors.md#spacing)),
so leave them out of the text. A `before` or `after` insert that needs a leading blank line
needs an empty line after the `"""`. `complete` edits add their separator themselves.

## Working habits

- Build in a sandbox that is a git repo; after each `gen`, `git add -A && git diff --cached`
  is the real review, and `git reset --hard && git clean -fd` starts over.
- Use `--plan` constantly: it prints every created file and every inserted snippet.
- Probe every `at` phrase with `olifant where` before it goes in the TOML, and an
  `[applies_at]` with `olifant actions --at FILE:LINE:COL` at a few cursor positions
  ([chapter 14](14-editors-and-actions.md#tutorial-make-your-generator-an-action)).
- Re-run the generator with the same inputs: it must be a no-op (`= … (skipped: …)`).
  Re-running with *different* inputs that would create the same file is a conflict, by design.
- When it works, give it a test case ([chapter 10](10-testing-generators.md)), or move it into
  a pack ([chapter 9](09-packs-and-language-adapters.md)).

## Rough edges

- `olifant anchors` still sorts an adapter's targets alphabetically (unlike a generator's
  own inputs, which `show` now lists in the order they are declared). Edits keep their order
  too.
- An undeclared hole kind (`{= ? x: block | … =}` in an Elm generator) is found only when
  the generator runs, as a `hole` conflict, not when it loads.
- Undefined variables in a template are found only when the generator runs, not when it
  loads — a `{= nope =}` with no such input or derived value is "undefined variable
  `nope`" at `gen` time, not an `INVALID` in `list`.
