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
  error ("unknown target `typo Route` for elm (targets: …)"), and so is a target that
  appears twice in one file.
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
  naming the field.
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
`[derive]`, so their strings can use them (`files = "{= service_dir =}/src/{= cmd =}.rs"`).
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

## `unless`: skip an edit when a file exists

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
the same run. `unless` works on `create` and `insert` edits.
