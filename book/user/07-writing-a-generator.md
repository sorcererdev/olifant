# 7. Writing your own generator

A generator is a directory with a `generator.toml` in it. Put one in your project's
`.olifant/generators/<name>/` and `olifant list` shows it, with no pack and no prefix. This
chapter builds three, from the quickest kind to the most structural, in an Elm sandbox:

```sh
scripts/try-pack.sh elm /tmp/olifant-gen && cd /tmp/olifant-gen
```

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
- A broken exemplar shows up when you run the generator, not in `olifant list`.

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
| `description` | yes | one line, shown by `list` and `show` |
| `language` | yes | names the language adapter whose `[check]` runs this generator's `complete` edits (chapter 4); otherwise informational |
| `version` | no (1) | shown in `show --json`; nothing else reads it yet |
| `examples` | no | example commands, printed by `show`. Agents copy these, so make them real |
| `[inputs.*]`, `[derive]`, `[requires]`, `[[edits]]`, `[[uses]]` | no | below; `[[uses]]` is [chapter 8](08-composing-generators.md) |

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
| `enum` | one of `choices = [...]`, or of the values found by `choices_from` (chapter 6) | text |
| `list` | comma-separated items, `of` one of the types above, or a record | a list |

An `ident` that looks like a name followed by a type is refused, because it is almost always
a mistake: `--set name="SetName String"` gets "looks like a name followed by other words (e.g.
a type); pass only the name, like SetName". Enum choices are exact and case-sensitive; a near
miss gets "did you mean".

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

A required list still accepts an empty value: `--set fields=` is an empty list, not a
missing input, and the `record` template above would render `type alias X =` followed by a
bare `}`. If an empty list makes no sense, guard the template (`{= if fields =}…{= end =}`).

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
unless    = { exists = "db/migrate/*_create_{= table =}.rb" }
```

If the file already exists with exactly the generated content, the edit is skipped
(`unchanged`) whatever `on_exists` says. Otherwise `error` is a conflict ("already exists
(on_exists = "error")"), `skip` leaves the file alone (`skipped: already exists`), and
`overwrite` replaces it (`~ path (overwritten, N lines)`). Prefer `error`: it is what keeps
hand edits safe.

`unless` skips the edit when its glob matches a file on disk or one created earlier in the
plan (chapter 6).

### `insert`

```toml
[[edits]]
insert = "src/Main.elm"
at     = "end of type Page"            # or a list of phrases, tried in order
text   = "| {= P =} {= P =}.Model"     # or: template = "…"
unless = { exists = "…" }
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
record Model"` shows the spot or says why not (for `Page/Home.elm`, whose `Model` is `{}`:
"has no items to insert beside"). A phrase with no `{= … =}` in it has its shape checked
when the generator loads (a missing or misspelt `end of` fails at once), but the target name
is only checked against the language adapter when the edit runs, so `end of cases in update`
is a conflict at run time, not a load error.

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
choices_from = { glob = "src/Page/*.elm", value = "{= stem =}" }
prompt = "The page it shows"

[derive]
R = "{= name|pascal =}"

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

The app compiles. The page list came from the project (`choices_from`), and nothing in the
generator says where `changeRouteTo` is.

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

`{=? … =}` is reserved for typed holes (the AI-filled gaps in the spec) and currently fails
with "holes ({= ? … =}) arrive in step 3".

### A literal `{` before a tag

`{{= else =}` is a literal `{` followed by a tag, because Olifant looks for `{=`. There is no
escape for a literal `{=` in a template; if you need one, put it in an exemplar (which is
copied literally) or print it from a string (`{= "{=" =}`).

## Multi-line `text` in TOML

Covered in [chapter 3](03-anchors.md#multi-line-text), and worth repeating because it is the
most common authoring snag: TOML drops the newline right after an opening `"""`. A
separating blank line before an `at` insert needs an empty line after the `"""`. `complete`
edits add that separator themselves.

## Working habits

- Build in a sandbox that is a git repo; after each `gen`, `git add -A && git diff --cached`
  is the real review, and `git reset --hard && git clean -fd` starts over.
- Use `--plan` constantly: it prints every created file and every inserted snippet.
- Probe every `at` phrase with `olifant where` before it goes in the TOML.
- Re-run the generator with the same inputs: it must be a no-op (`= … (skipped: …)`).
  Re-running with *different* inputs that would create the same file is a conflict, by design.
- When it works, give it a test case ([chapter 10](10-testing-generators.md)), or move it into
  a pack ([chapter 9](09-packs-and-language-adapters.md)).

## Rough edges

- `show` lists inputs in alphabetical order, not file order (TOML tables are read as maps;
  `olifant anchors` sorts an adapter's targets the same way). Edits keep their order.
- `olifant list` prints only the first line of a multi-line load error (`record  INVALID:
  \`derive.Name\`: line 1: 1:11:`); `olifant gen record` or `show` prints all of it.
- Undefined variables and broken exemplars are found when the generator runs, not when it
  loads.
