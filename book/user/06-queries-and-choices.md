# 6. Queries, `choices_from` and `unless`

A **query** reads facts out of the project. Regions use one to know what to list; enum
inputs use one to know their choices; a generator can bind one to a name and use it in
its templates.

## Glob queries

```toml
query = { glob = "src/**/*.hs", exclude = ["**/*Spec.hs"] }
```

`**` spans directories, `*` stays within one path segment. Globs see **the project's
files**: in a git repository, exactly what git considers part of it (tracked and new files,
minus anything `.gitignore` excludes); elsewhere, every file except those under
`node_modules`, `.git`, `elm-stuff`, `dist-newstyle` and `target`, which are never even
walked. Either way, dependencies never slow Olifant down or show up in a query. A glob can match
directories as well as files (`*/*-service`). Each match is a **record**:

| field | for `src/Olifant/Anchor/Phrase.hs`, glob `src/**/*.hs` |
|---|---|
| `path` | `src/Olifant/Anchor/Phrase.hs` |
| `dir` | `src/Olifant/Anchor` |
| `stem` | `Phrase` |
| `name` | `Phrase`, as a name, so `{= name|kebab =}` works |
| `rel` | `Olifant/Anchor/Phrase` (relative to the glob's fixed prefix `src/`, no extension) |
| `dotted` | `Olifant.Anchor.Phrase` |

Results are sorted by path.

## Command queries

For what a glob cannot see (Cargo or npm workspaces, a framework's routes, a schema), run a
command that prints JSON:

```toml
query = { command = ["cargo", "metadata", "--format-version=1", "--no-deps"], select = "packages", sort = "name" }
```

- The command is an argument list, run in the project root, with no shell. A program
  with a `/` in its name (`bin/rails`, `./scripts/list-routes`) is the project's own; a
  bare name (`cargo`) is looked up on PATH.
- It prints a JSON array of objects, or an object plus `select` naming the array. Each
  object becomes a record (string, number and boolean fields).
- `sort` names a field to sort by, so the output is stable.
- A command that fails or prints something else is a conflict carrying its error output.
- Commands come from packs and project config, which you trust the way you trust any
  plugin you install.

## AST queries: facts from the code itself

A glob sees files; an AST query sees what is *in* them. This region keeps
`src/RouteNames.elm` equal to the constructors of `type Route`:

```toml
# .olifant/regions.toml
[regions.route-names]
file       = "src/RouteNames.elm"
query      = { files = "src/Route.elm", items_of = "type Route", each = { ctor = "([A-Z][A-Za-z0-9_]*)" } }
whole_file = true
template   = "route-names.elm.dt"
```

```
-- .olifant/route-names.elm.dt
module RouteNames exposing (all)


all : List String
all =
    [ {= for r in items =}"{= r.ctor =}"{= if not loop.last =}, {= end =}{= end =} ]
```

```
$ olifant sync --init route-names
$ olifant gen elm/page --set page=profile
elm/page (10 edits, 0 conflicts)
  + src/Page/Profile.elm (42 lines)  [elm/page]
  …
  ~ src/Route.elm (+1 line at line 11)  [elm/page]
  …
  ~ src/RouteNames.elm (region route-names: +1 −1)  [region route-names]
```

```diff
-    [ "Home", "SamplePage", "Settings" ]
+    [ "Home", "SamplePage", "Settings", "Profile" ]
```

The page generator knows nothing about `RouteNames`. The region reads `Route.elm` as the
plan leaves it, with the new `| Profile`, so the list is right in the same run.

- **`files`** is a glob (and a template), so one query can span many files. Each file's
  language adapter supplies the grammar and the targets.
- **`items_of = "<target>"`** selects the items of a target, exactly as `end of` sees them:
  the vocabulary is the one `at` uses (`olifant anchors elm` lists it), and `olifant where`
  tests it. A file without the target gives no records, so a misspelt *name*
  (`type Rout`) gives an empty list rather than an error. A misspelt target *kind* is an
  error even when no file matches ("unknown target `typo Route` in any language adapter
  (targets: …)"), and so is a target that appears twice in one file. Files the glob
  matches that have no language adapter (`src/*` catching `elm.json`) are skipped; a file
  named outright must have one.
- **`ast = { … }`** is a raw ast-grep rule, for nodes no target covers. Every matching node
  is a record:

  ```toml
  query = { files = "src/Page/*.elm", ast = { kind = "module_declaration" }, each = { module = "^module ([A-Za-z.]+)" } }
  ```

  gives `Page.Home (src/Page/Home.elm:1)`, `Page.Profile (src/Page/Profile.elm:1)`, … when
  rendered as `{= m.module =} ({= m.path =}:{= m.line =})`.
- Every record has **`text`** (the node's source), **`path`** and **`line`** (1-based).
- **`each`** adds fields. Each is a regular expression over the node's text; the field is
  its first capture group (the whole match if it has no group). No match gives an empty
  field, never a dropped record.
- The expressions are POSIX, not Perl: `[[:space:]]` rather than `\s`, `[A-Za-z0-9_]`
  rather than `\w`, and no lazy `*?`. An invalid expression fails when the query is read,
  naming the field, and so does a Perl class like `\s` or `\w` (POSIX would read it as a
  plain letter and silently match nothing).
- AST queries need `ast-grep`; without it the query fails with the install hint.

## Queries in generators: `[query.NAME]`

A generator can bind a query's records to a name:

```toml
[query.routes]
files    = "src/Route.elm"
items_of = "type Route"
each     = { ctor = "([A-Z][A-Za-z0-9_]*)" }

[[edits]]
create = "src/RouteNames.elm"
text = """
module RouteNames exposing (all)


all : List String
all =
    [ {= for r in routes =}"{= r.ctor =}"{= if not loop.last =}, {= end =}{= end =} ]
"""
```

```
$ olifant gen route-names
route-names (1 edit, 0 conflicts)
  + src/RouteNames.elm (6 lines)
```

`routes` is a list like any other: templates, `for_each` and `[[uses]]` can use it. Any
query works here (`glob`, `command`, `items_of`, `ast`). Queries run after inputs and
`[derive]`, so their strings can use them (`files = "{= service_dir =}/src/{= cmd =}.rs"`),
but not each other's records. A query named like an input or derived value is an error.
A query that fails stops the generator with `query_failed` (exit 1).

Two limits. Generator queries read the files on disk, so in a chain of steps they do not
see what earlier steps planned (regions do). And a generator that *creates* a list, like the
one above, is stale as soon as a route is added; for that, the region is the better tool.

## `choices_from`: inputs whose values come from the project

`rust-htmx/service-command` adds a command to an existing service. Instead of making you
remember the service names, it asks the project:

```toml
[inputs.service]
type = "enum"
choices_from = { glob = "*/*-service", value = "{= dir =}" }   # billing/billing-service → billing
```

```
$ olifant show rust-htmx/service-command
inputs:
  --set command_name=<ident>
  --set service=<enum(billing)>
      The service to add the command to

$ olifant gen rust-htmx/service-command --set service=biling --set command_name=cancel_subscription
error: rust-htmx/service-command: invalid inputs: `service`: expected one of billing; got `biling` (did you mean billing?)
```

`value` is a template over each record (default `{= name =}`). If the query finds nothing,
the error says so ("the query glob */*-service found nothing"), so an empty project is not
a mystery. For agents this matters: the valid values are in `show`, so there is no need to
list directories first.

## `unless`: skip an edit when a file exists, or a line matches

`unless` takes exactly one of two keys: `exists` (a glob over the project's files) or
`matches` (a regular expression over the lines of the edit's own target). Writing both, or
neither, is refused when the generator loads ("write exactly one of `exists` (a glob) or
`matches` (a regular expression)"). `unless` works on `create` and `insert` edits.

### `exists`

Rails refuses two migrations that create the same table. `rails/model` guards against a
re-run with a different time:

```toml
[[edits]]
create = "db/migrate/{= now|timestamp =}_create_{= table =}.rb"
template = "migration.rb.dt"
unless = { exists = "db/migrate/*_create_{= table =}.rb" }
```

```
$ olifant gen rails/model --set name=post --set fields="title:string" --now 2026-09-28T10:00:00Z
rails/model (4 edits, 0 conflicts)
  = db/migrate/20260928100000_create_posts.rb (skipped: unless: db/migrate/20260927100000_create_posts.rb exists)
  = app/models/post.rb (skipped: unchanged)
```

The glob is rendered first, and it sees files on disk as well as files created earlier in
the same run.

### `matches`

An insert already skips itself when its text is at the spot it would go, or just beside it
("Already there", [chapter 3](03-anchors.md)). That check looks only at the anchor's region
and a small window around the site. Text that is already in the file *by name*, but
somewhere else or spelled differently, is not found, and used to be added a second time:

- an import already present inside a grouped `use crate::ports::{a::A, b::B};`;
- a field written by hand as `Arc<dyn X + Send + Sync>` where the generator writes
  `Arc<dyn X>`;
- a `let x = Arc::new(Real…)` that replaced the `let x: … = todo!(…)` the generator wrote.

`unless = { matches = "REGEX" }` skips the edit when **any line** of the edit's own target
file matches. `command-dependency` (rust-htmx) puts one on each edit that names the
dependency:

```toml
# packs/rust-htmx/generators/command-dependency/generator.toml (excerpt)
[[edits]]
insert = "{= service_dir =}/src/{= cmd =}.rs"
at     = "end of struct {= Cmd =}"
text   = "pub {= d =}: Arc<dyn {= D =}>,"
unless = { matches = "^[[:space:]]*pub {= d =}[[:space:]]*:" }

[[edits]]
insert = "{= service_dir =}/src/{= cmd =}.rs"
at     = ["end of uses", "start of file"]
text   = "use crate::ports::{= d =}::{= D =};"
unless = { matches = "{= d =}::{= D =}([^[:alnum:]_]|$)" }

[[edits]]
insert = "main/src/main.rs"
at     = "before first line matching ^[[:space:]]*let (mut )?{= svc =} = "
text   = "let {= d =}: Arc<dyn {= svc =}::ports::{= d =}::{= D =}> = todo!(\"an adapter for {= D =}\");"
unless = { matches = "^[[:space:]]*let (mut )?{= d =}[[:space:]]*[:=]" }
```

The field is "there" if any line declares `pub email_sender:`, whatever its type; the import
if `email_sender::EmailSender` appears anywhere, grouped or not; the `let` if any line binds
`email_sender`, whether to a `todo!()` or a real adapter. With a hand-written command whose
import is grouped and whose second field has extra bounds:

```rust
use crate::ports::{email_sender::EmailSender, invoice_repository::InvoiceRepository};

#[derive(Clone)]
pub struct CancelInvoice {
    pub email_sender: Arc<dyn EmailSender>,
    pub invoice_repository: Arc<dyn InvoiceRepository + Send + Sync>,
}
```

`wire-command` ([chapter 8](08-composing-generators.md)) leaves the file alone, and says
which line stopped each edit (the excerpt keeps the second dependency's run):

```
$ olifant gen rust-htmx/wire-command --set service=billing --set command=cancel_invoice
rust-htmx/wire-command (14 edits, 0 conflicts)
  …
  = billing/billing-service/src/cancel_invoice.rs (skipped: unless: line 10 of billing/billing-service/src/cancel_invoice.rs matches)  [rust-htmx/wire-command › rust-htmx/command-dependency[2]]
  = billing/billing-service/src/cancel_invoice.rs (skipped: unless: line 5 of billing/billing-service/src/cancel_invoice.rs matches)  [rust-htmx/wire-command › rust-htmx/command-dependency[2]]
  = billing/billing-service/src/service.rs (skipped: unless: line 2 of billing/billing-service/src/service.rs matches)  [rust-htmx/wire-command › rust-htmx/command-dependency[2]]
  = billing/billing-service/src/service.rs (skipped: already present)  [rust-htmx/wire-command › rust-htmx/command-dependency[2]]
  ~ billing/billing-service/src/service.rs (+1 line at line 31)  [rust-htmx/wire-command › rust-htmx/command-dependency[2]]
  = main/src/main.rs (skipped: unless: line 21 of main/src/main.rs matches)  [rust-htmx/wire-command › rust-htmx/command-dependency[2]]
  = main/src/main.rs (skipped: already present)  [rust-htmx/wire-command › rust-htmx/command-dependency[2]]
```

The details:

- **The file as the plan has it so far.** The target is read with every earlier edit and
  step of this run applied, so a line an earlier edit added counts, not only what is on
  disk. A second step that would add the same `let` is skipped.
- **The regex is POSIX extended** (the same dialect as `first line matching`), matched
  against each line on its own; `^` and `$` are the line's ends. `{= … =}` placeholders are
  allowed and are rendered first; the regex is checked when the generator loads, with each
  placeholder standing in for a plain name. Perl classes are refused with a hint:

  ```
  error: …/generator.toml: `edits`: [1] `unless`: `matches`: `let\s+x` is not a valid regular expression (POSIX extended: use [[:space:]] and [a-z_0-9], not \s or \w); …
  ```

- **Rendered values are not escaped.** A placeholder's value goes into the regex as is.
  Identifiers (`ident` inputs and their case filters) are safe; free text with `.` or `(` in
  it is not.
- **Comments count.** `unless.matches` reads every line, a commented-out one included, so
  write the regex so a comment cannot match. `command-dependency`'s import regex starts
  `^[^/]*`: the template's hint `// use crate::ports::worksite_repository::WorksiteRepository;`
  has a `/` before the name, so a real `worksite_repository` dependency still gets its
  import. (An anchor's own "already present" check leaves comment lines out by itself; see
  [chapter 3](03-anchors.md).)
- **The plan says which line.** `skipped: unless: line N of PATH matches`, 1-based, in the
  file as the plan had it at that point; in JSON the same text is the edit's `reason`.
