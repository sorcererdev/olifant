# 3. Where code goes: `at` phrases

An `insert` edit says where its text goes with a short phrase:

```toml
[[edits]]
insert = "src/Main.elm"
at     = "end of type Page"
text   = "| {= P =} {= P =}.Model"
```

Olifant parses `src/Main.elm`, finds `type Page`, and puts the new line after its last
variant, indented like that variant. Nothing in your app marks the spot.

## Try a phrase: `olifant where`

```
$ olifant where src/Main.elm "end of case in update"
src/Main.elm:76

            ( GotSamplePageMsg subMsg, SamplePage page ) ->
                SamplePage.update subMsg page |> updateWith SamplePage GotSamplePageMsg model
  ▶         (inserted here)

            ( _, _ ) ->
                ( model, Cmd.none )
```

Note what it did: it went after the last *real* branch, before the catch-all `( _, _ )`,
and it skipped the `case urlRequest of` nested inside the first branch. That is the
difference between a structural anchor and "the last line matching some text".

## The grammar: `<position> <target>`

| position | means |
|---|---|
| `end of X` | after X's last item |
| `start of X` | before X's first item |
| `after X` | after the whole of X |
| `before X` | before the whole of X |

Targets come from the file's **language adapter**. `olifant anchors elm` lists them:

```
$ olifant anchors elm
elm: at = "<start of|end of|before|after> <target>"

targets:
  case in FUNCTION        e.g. end of case in update
  list in FUNCTION        e.g. end of list in parser

from queries (elm):
  function NAME           e.g. end of function main
  class NAME              e.g. end of class BillingService
  KIND NAME               e.g. end of struct BillingService
  parameters in FUNCTION  e.g. end of parameters in new
  call NAME               e.g. end of call BillingService::new
  call NAME in FUNCTION   e.g. end of call BillingService::new in main
  imports                 e.g. end of imports

built in (any file):
  before|after marker TEXT
  before|after first line matching REGEX
  before|after last line matching REGEX
  start of|end of file
```

For a language with a `grammar` source and its queries (the shipped Python, Rust and Haskell,
and Elm with the elm pack), the listing has a second section, `from queries (elm):` above
(`from queries (tree-sitter-rust @77a3747):` for Rust), with the generic phrases below: Elm's
`end of type Msg`, `end of record Model` and `end of imports` are generic phrases (`KIND NAME`
with the kind words `type` and `record`), and its two `targets` are what no query capture
names. And
`olifant anchors rust --json` gives each target an `origin`: `targets` for a declared one,
`queries` for a generic phrase.

Words in capitals are filled in by you (`case in update`, `type Route`). The built-in
targets work in any file, even one with no adapter:

- `after last line matching ^import Page\.` (a POSIX regular expression per line)
- `before marker -- TODO: routes` (an exact comment line, if you have one)
- `start of file` / `end of file` (works on an empty file too; the whole file counts for
  "already there", so re-running is a no-op)

Mistakes are caught with a suggestion:

```
$ olifant where src/Main.elm "end of cases in update"
error: `end of cases in update`: unknown target `cases in update` for elm; did you mean `end of case in update`? (targets: case in FUNCTION, list in FUNCTION, function NAME, class NAME, KIND NAME, parameters in FUNCTION, call NAME, call NAME in FUNCTION, imports)
```

### The generic phrases

A language with a `grammar` source (the shipped Python, Rust and Haskell; Elm from its pack; `olifant doctor`'s `grammar` and `queries`
rows say which) knows these phrases with no `[targets.*]` table. They are read from tree-sitter's
community queries for the grammar, shipped inside olifant:

| phrase | the container | its items |
|---|---|---|
| `function NAME` | the function's body | its statements (the last one may be a tail expression) |
| `class NAME` | the body of a class-like definition: anything the grammar's queries tag as a class, interface, module or type, and (in Rust) an inherent `impl` | its members |
| `KIND NAME` | the same, narrowed to definitions whose node type is `KIND` or starts with `KIND_`: `struct BillingService` (a `struct_item`), `enum`, `mod tests`, `impl BillingService`, `trait` | its fields, variants, items or methods |
| `parameters in FUNCTION` | the function's parameter list | its parameters (an empty `()` is opened) |
| `call NAME` / `call NAME in FUNCTION` | the call's argument list, the call named by its text up to the arguments (`BillingService::new`) | its arguments |
| `imports` | the file | its top-level import statements, whole (`end of imports`: after the last one; in a file with none, after its leading comments and docstring where the language's `[imports] when_empty` says so, as Python's does) |

A name (`NAME`, `FUNCTION`) is the name itself or, as in a declared target, a POSIX expression that matches it whole: `parameters in [a-z_]+_routes` is the parameter list of the one function whose name ends in `_routes`.

`class` alone is often ambiguous: in Rust a struct and its `impl` share a name, so
`end of class BillingService` matches two places and is refused. Say `struct` or `impl`.
`impl NAME` is the type's inherent impl (`impl<T> Store<T>` is `impl Store`); an
`impl Trait for Type` cannot be named by a phrase. Comments are never items: `end of enum E`
goes after the last variant even when a commented-out variant follows it.

A phrase is looked up in this order: a declared `[targets.*]` whose shape matches (so a pack
can still override a generic phrase); the generic phrases; the language's anchors provider;
then "unknown target", whose suggestion also offers the generic phrases. The generic answer
is final: `end of struct Nope` in a file with no `struct Nope` misses with "no `struct Nope`
in this file", and is not sent to a provider. A two-word phrase whose first word is no node
type of the grammar (`end of the imports`) is not a generic phrase.

A language can also hand phrases its targets do not know to a program: `anchors = { provider
= [...] }` in its language file ([chapter 9](09-packs-and-language-adapters.md#providers-answering-a-slot-with-a-program)).
Such a phrase (the whole `at` text) goes to the provider after the generic phrases and before "unknown target"; it answers
the line the new lines start at and their indentation, or `null` ("the anchors provider does
not know `…`"), and the text is inserted as at any target, with no blank line added. A phrase a
declared target matches never goes to the provider.

## Fallbacks

`at` can be a list, tried in order; the first that resolves wins and the plan records
which one (`anchor_used`):

```toml
at = ["end of type Page", "before marker -- pages"]
```

Use this when a structural target needs a tool (below) that might not be installed.

## Rules worth knowing

- **Whole lines.** Olifant inserts whole lines, so the spot must own its line. With
  `oneOf [ a, b ]` written on one line, `end of list in parser` refuses ("the last item
  shares its line with other code") instead of producing broken code, unless the
  language says how its items are separated. With a `separator` (Python's and Rust's
  `,`), an item joins a one-line container: inline when the line stays within the
  language's `line_length` (`def __init__(self, bookings: BookingRepository) -> None:`), else
  the container goes one item per line, the closing bracket and what followed it on a line
  of their own. The plan shows that line replaced; a separator already at the end is not
  doubled. A separator such as
  `,` or `|` may share the line, so Elm's leading-comma lists work.
- **Exactly one.** A target must match once. If `update` had two top-level `case`s,
  `case in update` would be ambiguous and the edit a conflict. The same holds for a generic
  phrase: `end of class X` where a struct and an impl are both named `X` "matches 2 places;
  it must match once".
- **Already there.** If the text is already at that spot (or just beside it), the edit is
  skipped, so re-runs are no-ops. Only the target and a few lines around the spot are
  checked: for text that may already be elsewhere in the file, or spelled differently, add
  `unless = { matches = "REGEX" }` ([chapter 6](06-queries-and-choices.md)). Commented-out
  lines (using the adapter's `comment`) are left out on both sides, so a template's hint
  such as `// pub worksite_repository: Arc<dyn WorksiteRepository>,` does not count as the
  field being there.
- **Tools.** Declared language targets need `ast-grep`. Every grammar needs a C compiler once
  per machine and revision, the first time it is used, to build it (and the network, or
  `OLIFANT_GRAMMAR_SOURCES`, to fetch its source; [chapter 9](09-packs-and-language-adapters.md#grammars-built-once-loaded-at-run-time));
  after that the generic phrases need nothing more, the parser runs inside olifant. Without them the edit misses with "ast-grep is not installed (brew
  install ast-grep)" and the next fallback is tried. Line and marker targets need nothing.

## Opening an empty container

`end of`/`start of` put the new line beside an existing item. A container with none — an
Elm page's `Model` before its first field, a Rust `new()` before its first parameter —
used to refuse. Now Olifant opens it instead. Only a bracketed container (`()`, `{}`, `[]`)
is opened; any other empty target (a whole file, say) still refuses with "has no items to
insert beside" (use `end of file` there).

On one line, the line is split around the brackets, and the new text lands one level
inside — a notch deeper than the container's own indentation:

```toml
[[edits]]
insert = "src/Page/Home.elm"
at     = "end of record Model"
text   = "count : Int"
```

```diff
 type alias Model =
-    {}
+    {
+        count : Int
+    }
```

Whatever followed the brackets on that line stays on the closing line — a Rust parameter
list keeps its `-> Self {`, a call's arguments keep a trailing `;`:

```diff
-    pub fn new() -> Self {
+    pub fn new(
+        email_template_repository: Arc<dyn EmailTemplateRepository>,
+    ) -> Self {
```

A container already spread over several lines, with only comments inside, gets the text
before its closing line, at the comments' indentation — so a commented-out hint in a
template is also where the real field lands:

```diff
 pub struct SendEmail {
     // Put infra dependencies in this struct
     // Below is an example of a repo dependency
     // pub worksite_repository: Arc<dyn WorksiteRepository>,
+    pub email_template_repository: Arc<dyn EmailTemplateRepository>,
 }
```

With nothing inside at all, not even a comment, the text goes one level past the
container's own indentation, same as the one-line case.

"One level" is the adapter's `indent` key (`indent = "    "`, the default, in
`languages/<name>.toml`); set it to match a language's own convention.

"Empty" means *blank or comment-only*, where a comment is a line starting with the adapter's
`comment` (`//` for Rust, `--` for Elm). A container with no items but something else inside
(an argument whose kind is not in the target's `items`) is refused, not opened, because
opening it would delete that text or join the new item onto it badly. With `main.rs`'s call
edited to `NotificationsService::new(true)` (a `boolean_literal` is not one of the call
target's items):

```
$ olifant where main/src/main.rs "end of call notifications_service::service::NotificationsService::new in main"
error: `end of call notifications_service::service::NotificationsService::new in main`: has content that is not one of the target's items (add its kind to the target's `items`)
hint: `olifant anchors <language>` lists the targets
```

The fix is in the adapter: add the kind to the target's `items`
([chapter 9](09-packs-and-language-adapters.md)). A block comment counts as content too:
`new(/* deps */)` is refused the same way, because only lines that start with `comment` are
skipped.

When a container is opened, the text becomes its *first* item, so a leading `,` is dropped.
`elm/page` adds a route as `, Parser.map {= P =} (s "{= page|kebab =}")`, because Elm writes
lists with leading commas. Against a `Route.elm` whose parser is `oneOf []`:

```diff
-    oneOf []
+    oneOf [
+        Parser.map Settings (s "settings")
+    ]
```

The "already there" check tries the text both with and without that `,`, so running the
same `elm/page` again skips the edit (`skipped: already present`).

## Spacing

`end of` and `start of` copy the spacing of the items already there. In `update`, the
`case` branches are separated by a blank line, so a new branch gets one too, with no blank
line in your `text`:

```toml
[[edits]]
insert = "src/Main.elm"
at     = "end of case in update"
text   = """
( Got{= P =}Msg subMsg, {= P =} page ) ->
    {= P =}.update subMsg page |> updateWith {= P =} Got{= P =}Msg model"""
```

```diff
         ( GotSamplePageMsg subMsg, SamplePage page ) ->
             SamplePage.update subMsg page |> updateWith SamplePage GotSamplePageMsg model
 
+        ( GotSettingsMsg subMsg, Settings page ) ->
+            Settings.update subMsg page |> updateWith Settings GotSettingsMsg model
+
         ( _, _ ) ->
```

The branch still goes before the catch-all `( _, _ )`, which is not an item. The rule:

- The blank lines between the last two items (for `end of`) or the first two (for
  `start of`) are copied to the new item. Items written tightly together (`type Page`'s
  `| Home Home.Model` lines, an import block) stay tight.
- A container with one item has nothing to copy, so nothing is added.
- If your text already starts (or ends) with the blank line, it is not doubled.

Indentation works the same way: Olifant removes the text's common indentation and
indents it like the spot, so a branch written at its real column (`"""\n        Just
Route.About ->\n            …"""`) and one written flush left come out the same.

`before` and `after` add no blank lines of their own. For those, TOML is the snag:
it drops the newline right after an opening `"""`, so a leading blank line needs an empty
line after the `"""`:

```toml
text = """

-- the text starts with this blank line"""
```
