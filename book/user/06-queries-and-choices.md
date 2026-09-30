# 6. Queries, `choices_from` and `unless`

A **query** reads facts out of the project. Regions use one to know what to list; enum
inputs use one to know their choices.

## Glob queries

```toml
query = { glob = "src/**/*.hs", exclude = ["**/*Spec.hs"] }
```

`**` spans directories, `*` stays within one path segment. Build directories (`elm-stuff`,
`node_modules`, `.git`, `dist-newstyle`, `target`, `.olifant`) are skipped. A glob can match
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

- The command is an argument list, run in the project root, with no shell.
- It prints a JSON array of objects, or an object plus `select` naming the array. Each
  object becomes a record (string, number and boolean fields).
- `sort` names a field to sort by, so the output is stable.
- A command that fails or prints something else is a conflict carrying its error output.
- Commands come from packs and project config, which you trust the way you trust any
  plugin you install.

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
