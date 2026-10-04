# 2. Running generators

Four commands cover almost everything: `list`, `show`, `gen`, and `gen --plan`. A fifth,
`doctor`, says whether this machine has what they need.

## What can I run? `olifant list`

```
$ olifant list
elm/msg   A new Msg for an Elm page: the variant plus an update branch returning ( model, Cmd.none )
elm/page  A page in the Elm architecture (Model/Msg/init/update/view), wired into Main and Route like elm-spa-example

Tip: run a whole task in one call: olifant gen A --set k=v + B --set k=v (or --steps FILE with …)
```

Generators from packs are named `pack/name`; your project's own generators (in
`.olifant/generators/`) have no prefix. `olifant list --detail` also prints every
generator's inputs, which saves a round of `show` calls.

## What does it need? `olifant show`

```
$ olifant show elm/page
elm/page: A page in the Elm architecture (Model/Msg/init/update/view), wired into Main and Route like elm-spa-example

inputs:
  --set page=<ident>
      Page name (e.g. counter, article_list)
  --set msgs=<list<name:arg>>  (optional)
      Messages to add to the page, as name or name:PayloadType, comma separated (e.g. toggle_dark_mode, set_display_name:String)

edits:
  create src/Page/{= P =}.elm
  insert src/Main.elm at after last line matching ^import Page\.
  insert src/Main.elm at end of type Page
  …
  complete Route.{= P =} in case in changeRouteTo
  complete Main.{= P =} in case in view

uses:
  elm/msg (for each item of msgs)
    …

examples:
  olifant gen elm/page --set page=settings --set msgs="toggle_dark_mode, set_display_name:String"
```

- **inputs** are what you pass with `--set`. An `ident` is a name in any case
  (`article_list`, `ArticleList` and `article list` are the same name); Olifant renders it
  in whatever case each place needs. An `enum` lists its choices, often found in your project; when
  they depend on an input above (a service's commands), `show` lists them per value, and
  `olifant show GEN --set service=billing` narrows them to that one
  ([chapter 6](06-queries-and-choices.md#dependent-choices)).
- **edits** are what it will do, with `{= … =}` placeholders filled from your inputs.
  `insert … at …` places text by structure ([chapter 3](03-anchors.md)); `complete …`
  adds `case` branches wherever the compiler asks for them ([chapter 4](04-compiler-branches-and-todos.md)).
- **uses** lists the other generators it runs for you: `elm/page` runs `elm/msg` once per
  message.

## Run it: `olifant gen`

```
$ olifant gen elm/page --set page=settings --set msgs="toggle_dark_mode"
elm/page (11 edits, 0 conflicts)
  + src/Page/Settings.elm (42 lines)  [elm/page]
  ~ src/Main.elm (+1 line at line 8)  [elm/page]
  …
  ~ src/Page/Settings.elm (+1 line at line 25)  [elm/page › elm/msg[1]]
  ~ src/Page/Settings.elm (+2 lines at line 34)  [elm/page › elm/msg[1]]
  ~ src/Main.elm (+2 lines at line 108)  [elm/page]
  ~ src/Main.elm (+2 lines at line 59)  [elm/page]
```

`+` is a new file, `~` an insert, `=` an edit skipped because its text is already there,
`!` a conflict. `compiler: view` means the Elm compiler found that spot
([chapter 4](04-compiler-branches-and-todos.md)). `region library-modules: +1 −0` is an owned region being
refreshed ([chapter 5](05-owned-regions.md)); `skipped: unless: …` is an edit whose `unless`
matched ([chapter 6](06-queries-and-choices.md)). The tag on the right says which generator made the edit;
`elm/page › elm/msg[1]` is the first message's `elm/msg` run inside `elm/page`.

`gen` applies by default. Add `--plan` to see the same plan, with the full text of every
edit, without writing anything — unless the plan has a conflict, in which case nothing would
be written anyway, so neither `--plan` nor a plain `gen` prints any edit's text.

## One task, one command: `+`

Chain steps with a standalone `+`. The whole chain is one plan: all of it is written, or
none of it.

```sh
olifant gen elm/page --set page=settings --set msgs="toggle_dark_mode, set_display_name:String" \
          + elm/page --set page=profile --set msgs="follow, got_bio:String"
```

For many steps, or steps built by a program, put them in JSON and pass `--steps FILE`
(`-` reads stdin): `[{"gen": "elm/page", "set": {"page": "settings"}}, …]`.

List inputs take commas; a message's payload type follows a colon. Multi-word types are
parenthesised for you (`got_articles:List String` becomes `GotArticles (List String)`).

## When it says no

Olifant writes nothing unless the whole plan is sound, and every refusal says why:

```
$ olifant gen elm/page
error: elm/page: invalid inputs: missing `page` (ident)
```

```
$ olifant gen elm/page --set page=profile          # in an app whose parser function was renamed
Plan: elm/page (9 edits, 1 conflict): nothing written
  …
  ! src/Route.elm (conflict, see below)
  …
  ! edit 7: cannot place text in src/Route.elm: `end of list in parser`: not found
```

A conflict shows the edits that *would* have worked, so you can see how close it came.

```
$ olifant elm/page --set page=x
error: unknown command `elm/page`; did you mean `olifant gen elm/page --set page=x`?
```

Warnings don't stop a plan. The first one to read, when there is one, is about your code, not
the generator's: `gen` parses every existing file the plan changes, and names the first error
in one that doesn't parse:

```
$ olifant gen rust-htmx/wire-command --set service=billing --set command=cancel_invoice --plan --human
Plan: rust-htmx/wire-command (7 edits, 0 conflicts)
  …
  ? warning: billing/billing-service/src/cancel_invoice.rs:7 does not parse (near `;`) — this plan may place code wrongly, and checks will fail
```

(a `;` typed after a struct field instead of `,`). It is listed before any other warning, and
a compile error later in the same plan about that file ends with `(see the parse warning
above)`. Fix the file and run again; [chapter
14](14-editors-and-actions.md#code-that-does-not-parse-in-a-plan) has the details.

| exit code | meaning |
|---|---|
| 0 | done (including "nothing to do") |
| 1 | a generator or pack is broken |
| 2 | the command line is wrong |
| 3 | no such generator, file or pack |
| 4 | inputs missing or invalid (the JSON lists each one) |
| 5 | conflict: nothing was written |

When a `complete` edit cannot run its check because the compiler is not on your `PATH`, the
conflict says so and ends with `` ; run `olifant doctor` ``:

```
! rust-htmx/failure-arms: cannot complete cases: `cargo` is not installed (needed to check rust); run `olifant doctor`
```

([Below](#is-this-machine-ready-olifant-doctor).)

## For scripts and agents: JSON

When stdout is not a terminal, output is JSON (force either with `--json` / `--human`).
Each edit says how it was placed:

```json
{"id": "e2", "path": "src/Main.elm", "status": "insert", "line": 9, "anchor_used": "after last line matching ^import Page\\."}
{"id": "e9", "path": "src/Main.elm", "status": "insert", "line": 65, "found_by": "compiler", "site": "changeRouteTo", "todo": false}
```

The plan also has `conflicts`, `warnings`, `todos` and `holes` arrays. An applied run leaves
out file contents (you have the files); `--plan` includes them, both per edit (`content`) and
per file: `files` is `[{"path", "before", "after"}]`, each written file's whole text before
and after the plan (`before` is `null` for a new file), which is what an editor applies as
buffer changes ([chapter 14](14-editors-and-actions.md#plans-for-editors-files)). An
applied run has no `files`.

## Holes and models: `--model`, `--no-fill`

Some generators leave a small typed gap, a **hole**, for a model to fill: `elm/msg` does
when you give it an `intent`. With a model configured under `[model]` in
`.olifant/config.toml` ([chapter 12](12-holes-and-models.md)), `gen` fills it and lists it
under `holes:`:

```
$ olifant gen elm/msg --set page=home --set msg=reset --set intent="reset count to 0" --human
elm/msg (2 edits, 0 conflicts)
  ~ src/Page/Home.elm (+1 line at line 25)
  ~ src/Page/Home.elm (+2 lines at line 34, compiler: update)
holes:
  update (expr) src/Page/Home.elm:35  filled by qwen3-coder:30b: ( { count = 0 }, Cmd.none )
```

(run on a `Home.elm` whose `Model` is `{ count : Int }`). Each hole's line says where it is
and how it ended: `filled by MODEL: CODE` (with `(attempt N)` after a retry), `unfilled:
WHY` (it was left as a placeholder that compiles: no model is configured, or the model
failed), or `skipped: --no-fill`. In JSON, `holes` has `name`, `kind`, `file`, `line`,
`status`, `model`, `attempts`, `why` and `fill`.

- `--model NAME` fills holes with `[model.backends.NAME]` instead of the default.
- `--no-fill` leaves every hole as its placeholder, and asks no model.

Both apply to the whole run, like `--plan`. `--plan` does run the model, so it shows the
fills it would write. [Chapter 13](13-tutorial-holes.md) is a tutorial with a local
model.

## Progress: `--progress`

`--progress` writes what `gen` is doing to stderr, one JSON object per line, while it works:
a compiler check (`{"event":"check","language":"rust"}`), a request to the model (`fill`,
with `holes`, `attempt` and `model`), and each hole's outcome (`filled` with its `attempt`,
or `unfilled` with `why`). Stdout is unchanged. It is for editors that show a spinner during
model calls and checks; like `--plan`, it applies to the whole run.
[Chapter 14](14-editors-and-actions.md#progress-gen---progress) has the table and a real run.

## What applies here? `olifant actions`

Editors ask which generators apply at a cursor, with the inputs the code there already
gives:

```
$ olifant actions --at billing/billing-service/src/refund_invoice.rs:28:5 --human
Add AlreadyRefunded to every match  (rust-htmx/failure-arms)
$ olifant actions --at src/Olifant/Catalog.hs:84:3 --json        # in the Olifant repo
[{"generator":"olifant-dev/error-case","label":"Add a constructor to CatalogError (GHC finds every case)","missing":["name"],"sets":{"path":"src/Olifant/Catalog.hs","type":"CatalogError"}}]
$ olifant actions --file billing/billing-service/src/cancel_invoice.rs --human
22:5  Add Unknown to every match  (rust-htmx/failure-arms)
nearby: rust-htmx/wire-command — on a field of the CancelInvoice struct
```

(The first is in a rust-htmx sandbox with an `AlreadyRefunded` variant typed into the
`RefundInvoiceFailure` enum.) `LINE` and `COL` are 1-based; `COL` counts UTF-8 bytes (as Neovim does). `sets` are the inputs bound from
the path and the code, and `missing` the required inputs still to ask for. `--file FILE`
answers for the whole file at once: every place a generator applies (`LINE:COL`, or a
`range` in JSON), and, under `nearby`, the generators whose path matched but which found
nothing to apply to yet, with what they look for. `--content -` reads the file's current,
possibly unsaved, text from stdin. Only generators with an `[applies_at]` are offered;
[chapter 14](14-editors-and-actions.md) covers both, and the Neovim plugin that uses them.

## Is this machine ready? `olifant doctor`

Generators need more than Olifant: `ast-grep` to place code, each language's compiler for
`complete` edits, and a model for holes. `olifant doctor`, run in a project, checks each one
and says how to fix what is missing:

```
$ olifant doctor
✓ config — project at /tmp/demo
✓ pack rust-htmx — loaded
✓ language rust — loaded
✓ ast-grep — /opt/homebrew/bin/ast-grep
✗ check tool cargo (rust) — `cargo` is not on this PATH: /Users/you/.local/bin:/opt/homebrew/bin:/usr/bin:/bin
  fix: install Rust with rustup; cargo lives in ~/.cargo/bin, or /opt/homebrew/opt/rustup/bin with Homebrew's rustup (keg-only: add it to PATH, then restart your editor from a new shell)
✓ model — qwen3-coder:30b answers at http://localhost:11434/v1
$ echo $?
1
```

| check | what it looks at |
|---|---|
| `config` | the project root: the nearest directory with a `.olifant/`, from here upwards |
| `pack NAME`, `language NAME` | each pack and language adapter that loaded; an adapter or generator that failed to load is an `error`, with why |
| `ast-grep` | `ast-grep` on this process's `PATH` |
| `check tool EXE (LANGUAGE)` | the first word of each adapter's `[check] command` (`cargo`, `elm`, `cabal`) on this process's `PATH`, which is printed when it is missing |
| `model` | `[model]` ([chapter 12](12-holes-and-models.md#configuring-a-model)): none is a `warn`, with the snippet to add; a `default` with no such backend is an `error`. An `openai-compatible` default must list its model at `URL/models` within 3 seconds (else `warn`: `curl: (7) Failed to connect …`, or `` the server answers but has no model `NAME` ``); for `anthropic`, only that the key's environment variable is set, never its value and never a request |

`✓` is ok, `!` a warning, `✗` an error, each followed by `fix:` when there is one. The exit
code is 1 if any check is an error, 0 otherwise (warnings included). `--json` prints
`{"checks": [{"name", "status", "detail", "fix"}]}`, with `status` one of `ok`, `warn`,
`error` and `fix` null when there is none.

"This process's `PATH`" is the point: an editor that runs `olifant` gives it the editor's
`PATH`, which may not be your shell's. Run doctor the way the editor does: Durandal's
`:checkhealth durandal` runs it from Neovim
([chapter 14](14-editors-and-actions.md#troubleshooting)).

A config that does not parse, or a pack that is missing, stops doctor as it stops every
command: with the error (`error: …/.olifant/config.toml: 10:1: parse error: …`) and exit 1,
before any check.

## Running it again

Re-running a generator with the same inputs is a no-op (exit 0, nothing written): inserts
whose text is already in place are skipped (`=`), and a file the run would create is left
alone when planning it from scratch reproduces it exactly, even when later steps (a
page's messages) changed it after creation. A file that was created and then edited by
hand still stops the run (`already exists`), so your edits are never overwritten.
