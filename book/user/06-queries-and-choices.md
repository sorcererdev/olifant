# 6. Queries, `one_of` and `unless`

A **query** reads facts out of the project. Regions use one to know what to list; enum
inputs use one (`one_of`) to know their choices; a generator can bind one to a name and use it in
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
query works here (`glob`, `command`, `items_of`, `ast`, and `uncovered`,
[below](#uncovered-the-cases-a-match-at-the-cursor-lacks)). Queries run after inputs and
`[derive]`, so their strings can use them (`files = "{= service_dir =}/src/{= cmd =}.rs"`),
but not each other's records. A query named like an input or derived value is an error.
A query that fails stops the generator with `query_failed` (exit 1).

Two limits. Generator queries read the files on disk, so in a chain of steps they do not
see what earlier steps planned (regions do, and so does a query that gives an input its
choices, below). And a generator that *creates* a list, like the
one above, is stale as soon as a route is added; for that, the region is the better tool.

## `one_of`: inputs whose values come from the project

`rust-htmx/command-dependency` adds a port to a command. All three of its inputs name
something that is already in the project: a service, one of that service's commands, one of
that service's ports. Instead of making you remember them, each input is `one_of` a query,
and each query sits on its own lines with a comment saying what it finds:

```toml
# packs/rust-htmx/generators/command-dependency/generator.toml (excerpt)
[inputs.service]
type = "enum"
one_of = "services"
prompt = "The service the command belongs to"

[inputs.command]
type = "enum"
one_of = "commands"
prompt = "The command (use case) that needs the dependency"

[inputs.dependency]
type = "enum"
one_of = "ports"
prompt = "The port it depends on (a file in the service's ports/)"

[derive]
service_dir = "{= service|kebab =}/{= service|kebab =}-service"
…

# Every service: folders like billing/billing-service. The choice is the
# parent folder's name (billing).
[query.services]
glob  = "*/*-service"
value = "parent folder"

# The chosen service's commands: its src/*.rs files, less the service's own modules.
[query.commands]
glob    = "{= service_dir =}/src/*.rs"
exclude = ["**/lib.rs", "**/service.rs", "**/models.rs", "**/ports.rs"]
value   = "file name"

# The chosen service's ports.
[query.ports]
glob  = "{= service_dir =}/src/ports/*.rs"
value = "file name"
```

`one_of = "NAME"` names a `[query.NAME]` in the same generator: any query works (glob,
command, `items_of`, `ast`), and `value` says which part of each record is the choice.
Choices are sorted, duplicates removed, empty values dropped.

This replaced `choices_from`, which put the whole query inline on the input:

```toml
choices_from = { glob = "*/*-service", value = "{= dir =}" }   # billing/billing-service → billing
```

That line needed its trailing comment, because `{= dir =}` (a record field, rendered by a
template) does not say what it picks. In the new form the input says what it is (one of the
services), the query says in words where the choice comes from (`"parent folder"`), the
comment above it can be a sentence, and a query that more than one generator needs reads the
same in each. A generator that still has `choices_from` does not load, and the error says
what to write instead (here a project generator, `old`, with the line above):

```
$ olifant show old --human
error: …/.olifant/generators/old/generator.toml: `inputs.service`: `choices_from` is retired: use `one_of` (one_of = "NAME") and a [query.NAME] table (spec §4.1)
hint: `olifant schema` prints the generator.toml format; `olifant new NAME` writes a skeleton that loads
```

### `value`, in words

| `value` | the choice | for the match `billing/billing-service/src/ports/email_sender.rs` |
|---|---|---|
| `"file name"` | the name without its extension (a folder's own name); the default for a glob | `email_sender` |
| `"parent folder"` | the name of the folder the match is in | `ports` |
| `"path"` | the whole path | `billing/billing-service/src/ports/email_sender.rs` |
| a field name | that field of the record; `name` is the default for AST and command queries. A glob's `name` is a name, so the choice is its snake_case form, as `{= name =}` prints it | `"name"`: `email_sender`; on `src/Page/*.elm`, `home`, `sample_page` |
| a `{= … =}` template | the template rendered over the record (the escape hatch) | `"{= rel =}"` |

So `services` (`*/*-service`, a folder) gives `billing` with `"parent folder"` and would give
`billing-service` with `"file name"`. Anything else is refused when the generator loads:

```
error: …/generator.toml: `query.services`: `value`: expected "file name", "parent folder", "path", a field name or a {= … =} template; got `folder name`
```

and so is a `one_of` that names no query:

```
error: …/generator.toml: `inputs.service`: one_of = "service" but there is no [query.service]
```

### Dependent choices

`commands` and `ports` use `service_dir`, which is derived from `service`: a billing command
is one of billing's commands, not any service's. A query that serves an input may use the
inputs declared **above** that input, and derived values built from them. Choices resolve
top to bottom: `service` first, then `commands` and `ports` with that service's
`service_dir`. `show` prints a dependent input's choices grouped by the values of the inputs
it uses:

```
$ olifant show rust-htmx/command-dependency --human
rust-htmx/command-dependency: Add one dependency (a port) to a command: the command, the service's new() and main.rs (spec §7.1)

inputs:
  --set service=<enum(billing|notifications)>
      The service the command belongs to
  --set command=<enum, depends on service>
      The command (use case) that needs the dependency
      service=billing:        cancel_invoice | refund_invoice | void_invoice
      service=notifications:  send_email
  --set dependency=<enum, depends on service>
      The port it depends on (a file in the service's ports/)
      service=billing:        email_sender | invoice_repository
      service=notifications:  email_template_repository
…
```

`show --set` narrows to the lists for the values you give:

```
$ olifant show rust-htmx/command-dependency --set service=billing --human
…
inputs:
  --set service=<enum(billing|notifications)>
      The service the command belongs to
  --set command=<enum(cancel_invoice|refund_invoice|void_invoice)>
      The command (use case) that needs the dependency
  --set dependency=<enum(email_sender|invoice_repository)>
      The port it depends on (a file in the service's ports/)
…
```

In JSON (here each input on its own line, from `jq -c '.inputs[]'`), an enum whose choices
are known has `choices`; a dependent one has `depends_on` and `choices_by`, keyed by the
value it depends on (`a=x, b=y` when it depends on several):

```
$ olifant show rust-htmx/command-dependency --json | jq -c '.inputs[]'
{"choices":["billing","notifications"],"default":null,"name":"service","optional":false,"prompt":"The service the command belongs to","type":"enum(billing|notifications)"}
{"choices_by":{"billing":["cancel_invoice","refund_invoice","void_invoice"],"notifications":["send_email"]},"default":null,"depends_on":["service"],"name":"command","optional":false,"prompt":"The command (use case) that needs the dependency","type":"enum, depends on service"}
{"choices_by":{"billing":["email_sender","invoice_repository"],"notifications":["email_template_repository"]},"default":null,"depends_on":["service"],"name":"dependency","optional":false,"prompt":"The port it depends on (a file in the service's ports/)","type":"enum, depends on service"}

$ olifant show rust-htmx/command-dependency --set service=billing --json | jq -c '.inputs[]'
{"choices":["billing","notifications"],"default":null,"name":"service","optional":false,"prompt":"The service the command belongs to","type":"enum(billing|notifications)"}
{"choices":["cancel_invoice","refund_invoice","void_invoice"],"default":null,"name":"command","optional":false,"prompt":"The command (use case) that needs the dependency","type":"enum(cancel_invoice|refund_invoice|void_invoice)"}
{"choices":["email_sender","invoice_repository"],"default":null,"name":"dependency","optional":false,"prompt":"The port it depends on (a file in the service's ports/)","type":"enum(email_sender|invoice_repository)"}
```

`show` lists one group per value (per combination, for several inputs) of the enums an
input depends on, up to 30 groups. Past that, or when an input it uses is not an enum, it
says what to set instead. A project generator whose `service` is a plain `ident` (and whose
`commands` query has no `value`, so the file name is the choice):

```
$ olifant show by-name --human
by-name: a command of a service named by hand

inputs:
  --set service=<ident>
  --set command=<enum, depends on service>
      depends on service; set it to see the choices
…
$ olifant show by-name --set service=notifications --human
…
inputs:
  --set service=<ident>
  --set command=<enum(send_email)>
…
```

(JSON: `"depends_on":["service"]` and no `choices_by`.)

**Errors name the context.** A value that is not one of the choices says whose choices they
were, with the usual "did you mean"; exit code 4:

```
$ olifant gen rust-htmx/command-dependency --set service=billing --set command=refund_invoice --set dependency=clock --human
error: rust-htmx/command-dependency: invalid inputs: `dependency`: expected one of email_sender, invoice_repository (the ports of service=billing); got `clock`

$ olifant gen rust-htmx/command-dependency --set service=billing --set command=refund_invoice --set dependency=email_sendr --human
error: rust-htmx/command-dependency: invalid inputs: `dependency`: expected one of email_sender, invoice_repository (the ports of service=billing); got `email_sendr` (did you mean email_sender?)
```

Leave out the input the choices depend on, and the dependent inputs say so instead of
listing every service's values:

```
$ olifant gen rust-htmx/command-dependency --set command=refund_invoice --set dependency=email_sender --human
error: rust-htmx/command-dependency: invalid inputs: missing `service` (enum(billing|notifications)); `command`: its choices depend on `service`; set it first; `dependency`: its choices depend on `service`; set it first
```

A `service` that is set but wrong (`--set service=biling`) gets only its own error
(`` expected one of billing, notifications; got `biling` (did you mean billing?) ``): the
inputs below it have nothing to add. A query that finds nothing is an error naming the
query and what it was looking for, so an empty directory is not a mystery. With
`notifications`' only port deleted:

```
$ olifant gen rust-htmx/command-dependency --set service=notifications --set command=send_email --set dependency=email_template_repository --human
error: rust-htmx/command-dependency: invalid inputs: `dependency`: the query `ports` found nothing for service=notifications, so there is nothing to choose
```

(`show` prints that group as `service=notifications:  (none)`.) A query that fails names
its context too, and stops the generator with exit code 1. Here `tables` is a command query
that prints to stderr and exits 3, given `{= service =}` as an argument:

```
$ olifant gen cmdfail --set service=billing --set table=x --plan --human
error: cmdfail: `table`: query tables (for service=billing) failed: command sh -c echo no database for $0 >&2; exit 3 billing failed: exited 3: no database for billing
```

`show` prints each failed group's reason where its choices would be, rather than an empty
list:

```
$ olifant show cmdfail --human
…
  --set table=<enum, depends on service>
      service=billing:        query failed: `table`: query tables (for service=billing) failed: command sh -c echo no database for $0 >&2; exit 3 billing failed: exited 3: no database for billing
      service=notifications:  query failed: `table`: query tables (for service=notifications) failed: command sh -c echo no database for $0 >&2; exit 3 notifications failed: exited 3: no database for notifications
```

In JSON the reasons go in an `errors` object, keyed like `choices_by` (which keeps only the
groups that worked):

```
$ olifant show cmdfail --json | jq -c '.inputs[1]'
{"choices_by":{},"default":null,"depends_on":["service"],"errors":{"billing":"`table`: query tables (for service=billing) failed: command sh -c echo no database for $0 >&2; exit 3 billing failed: exited 3: no database for billing","notifications":"`table`: query tables (for service=notifications) failed: command sh -c echo no database for $0 >&2; exit 3 notifications failed: exited 3: no database for notifications"},"name":"table","optional":false,"prompt":null,"type":"enum, depends on service"}
```

**Order matters, and is checked when the generator loads.** Declare `dependency` above
`service` and `ports` would need a value that does not exist yet:

```
error: …/generator.toml: `[query.ports]` uses `service`, which is declared after `dependency`
```

A query that serves no input (one only edits, templates or `[[uses]]` use, like
`wire-command`'s `deps` below) sees every input, as before.

**Derived values, `now` and overrides.** A derived value counts as "above" when everything it
is built from is: `service_dir` from `service`, or a value built from `now` alone, which every
query can use. A project generator that publishes one of the drafts stamped with the run's
time:

```toml
[derive]
stamp = "{= now|timestamp =}"

# Drafts stamped with the run's time (--now).
[query.drafts]
glob  = "drafts/{= stamp =}_*.md"
value = "file name"
```

```
$ olifant gen publish --set draft=x --now 2026-10-02T12:00:00Z --plan --human
error: publish: invalid inputs: `draft`: expected one of 20261002120000_intro, 20261002120000_outro; got `x`
```

(`drafts/20261001090000_old.md` is there too, and is not a choice.) A `--set` of a derived
value overrides it for the queries as it does for the edits, and the error says so. With
`service_dir` pointed at billing's folder, notifications' own command and port are refused:

```
$ olifant gen rust-htmx/command-dependency --set service=notifications --set service_dir=billing/billing-service --set command=send_email --set dependency=email_template_repository --plan --human
error: rust-htmx/command-dependency: invalid inputs: `command`: expected one of cancel_invoice, refund_invoice, void_invoice (the commands of service=notifications, service_dir=billing/billing-service); got `send_email`; `dependency`: expected one of email_sender, invoice_repository (the ports of service=notifications, service_dir=billing/billing-service); got `email_template_repository`
```

For agents this matters: the valid values are in `show`, so there is no need to list
directories first, and `show --set service=billing` gives exactly the values for the
service at hand. In a chain of steps, each step's choices also see the files the steps
before it create ([chapter 8](08-composing-generators.md)).

## `uncovered`: the cases a `match` at the cursor lacks

One more kind of query asks the compiler instead of the files. In a generator,

```toml
# packs/rust-htmx/generators/match-arm/generator.toml (excerpt)
[inputs.pattern]
type   = "enum"
one_of = "uncovered"
prompt = "The case to add an arm for"

[query.uncovered]
uncovered = "Failure::"
value     = "name"
```

finds the cases the `match` at the editor's cursor is missing: it runs the file's language
check (the adapter's `[check]`, as a `complete` edit does, [chapter 4](04-compiler-branches-and-todos.md))
and reads the latest missing-case error in the cursor's file that starts at or above the
cursor's line. Each missing pattern that matches the regex (POSIX extended, and a template)
is a record with one field, `name`; a leading `&` (rustc's way of writing a case of a
`match` on a reference) is left out. On a blank line after `status()`'s last arm, with an
`AlreadyRefunded` variant that has no arm yet, the one record is
`{name = "RefundInvoiceFailure::AlreadyRefunded"}`, so `pattern`'s one choice is that.

It only makes sense with a cursor, and it only runs with one: `olifant actions --at`,
`olifant complete`, `gen --at` and a test case with `at`. Anywhere else it finds nothing (an
enum `one_of` it has no choices: `show` lists it as `<enum()>`). The generator it serves is
a snippet, `rust-htmx/match-arm`
([chapter 14](14-editors-and-actions.md#what-the-compiler-says-is-missing-uncovered-queries)).
An `uncovered` query has no other keys, and a file whose adapter has no `[check]` makes it
fail.

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
unless = { matches = "^[^/]*{= d =}::{= D =}([^[:alnum:]_]|$)" }

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
