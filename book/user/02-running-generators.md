# 2. Running generators

Four commands cover almost everything: `list`, `show`, `gen`, and `gen --plan`. A fifth,
`doctor`, says whether this machine has what they need, and a sixth, `check`, whether the
code still follows the project's conventions.

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

## Start a project: `olifant init`

`olifant init --pack PACK NAME` makes a new project from nothing: a running skeleton, its
tests green, ready for the pack's generators.

```
$ olifant init --pack ~/projects/olifant/packs/python-htmx bistro --human
python-htmx/project (14 edits, 0 conflicts)
  + pyproject.toml (32 lines)
  + uv.lock (525 lines)
  + .gitignore (4 lines)
  …
  + src/bistro/main.py (17 lines)
  + src/bistro/main__test.py (18 lines)
  + src/bistro/web/app.py (23 lines)
  + src/bistro/web/app__test.py (9 lines)
  + src/bistro/web/resources.py (4 lines)
  + src/bistro/web/templates/base.html (19 lines)
  + src/bistro/web/templates/home.html (7 lines)

bistro is ready. Next:
  cd bistro
  uv sync
  uv run pytest
  uv run uvicorn --factory --app-dir src bistro.main:create --reload
```

In order, it:

1. **Checks the folder.** `NAME` (or `--dir DIR`) must not exist, or be empty; a folder
   holding only `.git` counts as empty. Otherwise it stops before writing anything:
   `error: full is not empty (notes.txt)`.
2. **Finds the pack.** `--pack` is a pack's directory, or a name under `[packs]` in
   `~/.config/olifant/config.toml`. An unknown one: `error: no pack \`nope\`` with how to
   add it. The pack must have exactly one **starter** (a generator with `starter = true`,
   [chapter 7](07-writing-a-generator.md)); it is found before anything is written.
3. **Writes `.olifant/config.toml`**: the pack, and your user config's `[model]` tables, so
   holes are filled the way you set up ([chapter 12](12-holes-and-models.md)).
4. **Runs the starter** with `name`, then sets up every owned region it made (as `sync
   --init`; python-htmx's `resources` list).
5. **Commits**: `git init`, then a first commit `olifant init: PACK`. Inside another
   repository (a monorepo's `apps/`), it neither nests a repository nor commits: `note: no
   first commit: it is inside the git repository at …; add and commit it there`.
6. **Prints the next steps**, the starter's `next` lines.

`--plan` shows the starter's plan and writes nothing; `--json` gives the plan as JSON.

### An existing repo: `olifant init --migrate`

A codebase you already have, to move to a new architecture a little at a time
([chapter 17](17-strangling-a-legacy-codebase.md)):

```
$ olifant init --migrate legacy --pack ~/projects/olifant/packs/python-htmx --human
baselined 0 findings in .olifant/baseline.toml
migrating legacy: legacy is bug-fix-only; new work goes to the new architecture
next: olifant check, and open the project in your editor
```

`PATH…` are the legacy folders (`legacy` becomes the glob `legacy/**`). It writes into
`.olifant/config.toml`, keeping what is there:

```toml
[packs]
python-htmx = { path = "/Users/you/projects/olifant/packs/python-htmx" }
python-strangler = { path = "/Users/you/projects/olifant/packs/python-strangler" }

[migrate]
legacy = ["legacy/**"]
```

The target pack (`--pack`, a directory or a configured name; without it, one already in the
config) and `python-strangler` (configured, else beside the `--pack` directory). Then it adds
`.olifant/state/` to `.gitignore` and records a baseline (`check --baseline`), so the code as it
is today passes. A project already migrating is refused: `error: already migrating: edit
[migrate] in .olifant/config.toml`. `olifant init` with neither `--pack PACK NAME` nor
`--migrate` is a usage error.

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

## Holes and models: `--model`, `--no-fill`, `--fills`

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
failed), or `skipped: --no-fill`. In JSON, `holes` has `name`, `kind`, `source`, `file`,
`line`, `placeholder`, `status`, `model`, `attempts`, `why` and `fill`. (`source` is `model`
except for a snippet's tab stops, [below](#a-snippet-at-the-cursor---at).)

- `--model NAME` fills holes with `[model.backends.NAME]` instead of the default.
- `--no-fill` leaves every hole as its placeholder, and asks no model.

Both apply to the whole run, like `--plan`. `--plan` does run the model, so it shows the
fills it would write. [Chapter 13](13-tutorial-holes.md) is a tutorial with a local
model.

`--fills FILE` stands a recorded `[fills]` table (hole name = code, as a case's
`fills.toml`) in for the model: every hole the plan asks for must be in it. It still goes
through the checks, `verify` included, so a recorded fill is judged like a live one. The
python slice uses it to replay what qwen3-coder:30b wrote, without Ollama.

## Progress: `--progress`

`--progress` writes what `gen` is doing to stderr, one JSON object per line, while it works:
a compiler check (`{"event":"check","language":"rust"}`), a request to the model (`fill`,
with `holes`, `attempt` and `model`), the repairs made on the way (`auto_import`,
`reindent`), a fill's tests (`verify`), and each hole's outcome (`filled` with its `attempt`,
or `unfilled` with `why`). Stdout is unchanged. It is for editors that show a spinner during
model calls and checks; like `--plan`, it applies to the whole run.
[Chapter 14](14-editors-and-actions.md#progress-gen---progress) has the table and a real run.

## An unsaved buffer: `--overlay FILE`

`gen` reads the disk. An editor's buffer is often ahead of it, and an editor that plans in
the background, while you type, should not save your file to find out what a generator
would do. `--overlay FILE` plans against FILE's current text, read from stdin, instead of
the disk's. In an Elm sandbox with the `msg-branch` generator of [chapter
14](14-editors-and-actions.md#tutorial-make-your-generator-an-action), and an `Increment`
variant typed into `type Msg` but not saved:

```
$ sed 's/    | Reset/    | Reset\n    | Increment/' src/Page/Home.elm \
    | olifant gen msg-branch --set page=Home --set msg=Increment --plan --human --no-fill --overlay src/Page/Home.elm
Plan: msg-branch (1 edit, 0 conflicts)
  ~ src/Page/Home.elm (+2 lines at line 35, compiler: update)

--- src/Page/Home.elm
Increment ->
    ( model, Cmd.none )
```

The same command without `--overlay` plans against the saved file, where there is no
`Increment` yet, so there is nothing to add: `msg-branch (0 edits, 0 conflicts)`.

- **Everything sees the overlay**: placement, `unless`, queries (in rust-htmx, a command
  file you have never saved is already one of the service's `commands`), the compiler
  checks of `complete` edits and of fills, and the parse pre-check. Every other file is
  read from the disk.
- **It never writes.** `--overlay` needs `--plan`; without it the command is a usage error,
  exit 2:

  ```
  $ olifant gen msg-branch --set page=Home --set msg=Increment --overlay src/Page/Home.elm --human
  error: --overlay needs --plan (an unsaved buffer is planned, never written)
  ```

- **`files` starts from the buffer.** The overlaid file's `before` is the text you sent,
  so an editor applies `after` to the buffer it read, and can tell if it changed since.
- One file per run. `FILE` is relative to where you run `olifant` (or absolute), as for
  `olifant actions`. Like `--plan` and `--progress`, it applies to the whole run.

This is how Durandal plans ghost text and follow-ups without saving
([chapter 14](14-editors-and-actions.md#ghost-text-and-follow-ups)).

## A snippet at the cursor: `--at`

A **snippet** is a generator whose one edit goes at the cursor
([chapter 14](14-editors-and-actions.md#snippets)): a new variant in an enum, the next arm of
a `match`, a field. `olifant show` lists its edit as `at the cursor`. It needs to know where
the cursor is, so `gen` takes `--at FILE:LINE:COL` (1-based, as for `olifant actions`). In a
rust-htmx sandbox with a blank line 27 typed below `Unknown(String),` and saved:

```
$ olifant gen rust-htmx/failure-variant --at billing/billing-service/src/refund_invoice.rs:27:1 --plan --human
Plan: rust-htmx/failure-variant (1 edit, 0 conflicts)
  ~ billing/billing-service/src/refund_invoice.rs (replaced 1 line with 2 lines at line 27)
holes:
  message (text@yours) billing/billing-service/src/refund_invoice.rs:27  skipped: a tab stop (yours)
  Variant (ident@yours) billing/billing-service/src/refund_invoice.rs:28  skipped: a tab stop (yours)

--- billing/billing-service/src/refund_invoice.rs
    #[error("message")]
    Variant,
```

- **The snippet replaces the cursor's line**, indented to fit: a line with text keeps its
  own indentation, and an empty one takes the line above's, one level deeper after a line
  that opens a block. Only `LINE` decides where; `COL` is the cursor an editor reports, for
  the `trigger` and for queries that ask about the cursor.
- **Its holes are tab stops.** `message` and `Variant` are the snippet's, not the model's
  (`skipped: a tab stop (yours)`), so a plain `gen --at` writes their names. That is for
  scripts and tests; an editor takes the plan's `snippet` (with `--json`) and expands it with
  the holes as tab stops ([chapter 14](14-editors-and-actions.md#the-plans-snippet)). A hole
  that is the model's is filled as usual ([below](#holes-and-models---model---no-fill)).
- **With `--overlay FILE`**, the line and everything else are read from the buffer on stdin,
  which is how an editor plans a snippet as you type.
- **A snippet without `--at` is a usage error**, exit 2:

  ```
  $ olifant gen rust-htmx/failure-variant --plan --human
  error: `rust-htmx/failure-variant` is a snippet: it needs --at FILE:LINE:COL
  ```

  `--at` on a generator that is not a snippet changes nothing about where it writes.

## What applies here? `olifant actions`

Editors ask which generators apply at a cursor, with the inputs the code there already
gives:

```
$ olifant actions --at billing/billing-service/src/refund_invoice.rs:28:5 --human
Add AlreadyRefunded to every match  (rust-htmx/failure-arms)
Failure variant  (rust-htmx/failure-variant)
$ olifant actions --at src/Olifant/Catalog.hs:94:3 --json        # in the Olifant repo
[{"choices":{},"generator":"olifant-dev/error-case","label":"Add a constructor to CatalogError (GHC finds every case)","missing":["name"],"name":"Add Error Case","sets":{"path":"src/Olifant/Catalog.hs","type":"CatalogError"},"snippet":false,"suggest":false,"trigger":false}]
$ olifant actions --file billing/billing-service/src/cancel_invoice.rs --human
6:26  A port as a field of a command (a snippet, spec §4.9); wiring it in follows up (wire-command)  (rust-htmx/add-port)  asks: port
20:31  Failure variant  (rust-htmx/failure-variant)
22:5  Add Unknown to every match  (rust-htmx/failure-arms)
nearby: rust-htmx/match-arm — on a blank line inside a match
nearby: rust-htmx/wire-command — on a field of the CancelInvoice struct
```

(The first is in a rust-htmx sandbox with an `AlreadyRefunded` variant typed into the
`RefundInvoiceFailure` enum; the `Failure variant` snippet is listed after it, as
[chapter 14](14-editors-and-actions.md#the-demo-from-the-command-line) explains.) `LINE`
and `COL` are 1-based; `COL` counts UTF-8 bytes (as Neovim does). `sets` are the inputs
bound from the path and the code, `missing` the required inputs still to ask for, `choices`
the values a missing `enum` can take, `name` the generator's display name, `suggest` whether
an editor may offer it unasked
([chapter 14](14-editors-and-actions.md#ghost-text-and-follow-ups)), and `snippet` and
`trigger` whether it is a snippet and whether it waits for something typed
([chapter 14](14-editors-and-actions.md#snippets)). `--file FILE`
answers for the whole file at once: every place a generator applies (`LINE:COL`, or a
`range` in JSON), and, under `nearby`, the generators whose path matched but which found
nothing to apply to yet, with what they look for. `--content -` reads the file's current,
possibly unsaved, text from stdin. Only generators with an `[applies_at]` are offered;
[chapter 14](14-editors-and-actions.md) covers both, and the Neovim plugin that uses them.

`olifant complete --at FILE:LINE:COL [--content -]` is the same question for a completion
menu: only the snippets, with one item per choice. With `pub struct CancelInvoice {}` opened
onto three lines and saved, on the blank line 7 between the braces:

```
$ olifant complete --at billing/billing-service/src/cancel_invoice.rs:7:1 --human
⚡ Add port EmailSender  (rust-htmx/add-port)
⚡ Add port InvoiceRepository  (rust-htmx/add-port)
```

Each item's `sets` (in JSON) and `--at` make its `gen` command
([chapter 14](14-editors-and-actions.md#olifant-complete)).

## Do the conventions hold? `olifant check`

`olifant check` checks the two kinds of convention a project keeps
([chapter 15](15-conventions-that-hold.md)): its owned regions are up to date
([chapter 5](05-owned-regions.md)), and its **rules** are kept. In a rust-htmx sandbox
with Diesel code typed into a command (`invoices::table.filter(…)`):

```
$ olifant check
! billing/billing-service/src/cancel_invoice.rs:15  commands-use-ports — Commands talk to ports, not the database (rust-htmx: ports & adapters)  (fixes: Use an existing port: InvoiceRepository; Add a new repository port)
conventions hold (1 finding, none blocking)
$ echo $?
0
```

A finding is `✗` (error), `!` (warn) or `·` (hint, or a finding already in the baseline,
marked `(existing)`), then the file and line, the rule, its message and, when it has one, the
generator that fixes it. The exit code is 1 when a region drifted or is broken, a rule is
broken, or a finding is an `error` not in the baseline; warnings and hints never fail.

| command | what it does |
|---|---|
| `olifant check` | regions and every rule over the project |
| `olifant check --report` | also every exception (allow comments and `[rules]` allows, stale ones marked) and the debt per rule; in JSON, each rule's severity too |
| `olifant check --file FILE [--content -]` | only FILE's rule findings, from stdin with `--content -` (for editors); no regions, always exit 0 |
| `olifant check --baseline` | record today's findings in `.olifant/baseline.toml`: from then on only new ones fail |
| `olifant check --changed-since REF` | only findings on lines changed since the git ref REF, and in untracked files; no regions |

`--json` prints `{"regions", "findings", "exceptions", "errors"}` (and `rules` with
`--report`). Each finding has `rule`, `file`, `range` (`{"start": [LINE, COL], "end": [LINE,
COL]}`, or `null` for a finding about the whole file), `severity`, `message`, `fix` (a
generator id) and `fix_name`, `sets` (the fix's inputs, from the path) and `baselined`:

```
$ olifant check --json
{"errors":[],"exceptions":[],"findings":[{"baselined":false,"file":"billing/billing-service/src/cancel_invoice.rs","fix":"rust-htmx/service-repository","fix_name":"Add Repository Port","message":"Commands talk to ports, not the database (rust-htmx: ports & adapters)","range":{"end":[2,24],"start":[2,1]},"rule":"commands-use-ports","sets":{"command":"cancel_invoice","service":"billing"},"severity":"warn"}],"regions":[]}
```

## Is this machine ready? `olifant doctor`

Generators need more than Olifant: `ast-grep` to place code, each language's compiler for
`complete` edits, and a model for holes. `olifant doctor`, run in a project, checks each one
and says how to fix what is missing:

```
$ olifant doctor
✓ config — project at /tmp/demo
✓ pack rust-htmx — loaded
✓ language rust — loaded (shipped, pack rust-htmx)
✓ ast-grep — /opt/homebrew/bin/ast-grep
✗ check tool cargo (rust) — `cargo` is not on this PATH: /Users/you/.local/bin:/opt/homebrew/bin:/usr/bin:/bin
  fix: install Rust with rustup; cargo lives in ~/.cargo/bin, or /opt/homebrew/opt/rustup/bin with Homebrew's rustup (keg-only: add it to PATH, then restart your editor from a new shell)
✗ test runner cargo (rust) — `cargo` is not on this PATH: /Users/you/.local/bin:/opt/homebrew/bin:/usr/bin:/bin
  fix: install Rust with rustup; cargo lives in ~/.cargo/bin, or /opt/homebrew/opt/rustup/bin with Homebrew's rustup (keg-only: add it to PATH, then restart your editor from a new shell)
✓ model — qwen3-coder:30b answers at http://localhost:11434/v1
✓ rust: units — [unit] kinds (ast-grep), named by [unit] name · from shipped
✓ rust: diagnostics — [check] format rustc-json · from toolchain cargo
✓ rust: missing cases — [check] format rustc-json · from toolchain cargo
✓ rust: tests — [test] format lines, tests_for {= dir =}/{= stem =}.rs · from toolchain cargo-test
✓ rust: test units — [test] unit, above · from shipped
✓ rust: test selection — [test] select · from toolchain cargo-test
! rust: mutation — absent
  fix: add [[mutate]] to .olifant/languages/rust.toml
✓ rust: stub — [test] stub at end of mod tests · from toolchain cargo-test
! rust: conjure shapes — absent
  fix: add [conjure] to .olifant/languages/rust.toml (conjure works at the floor: the test and its fill, no ports)
✓ rust: messages — [check] undefined · from shipped
! rust: test messages — absent
  fix: add [test] undefined, [test.failure] to .olifant/languages/rust.toml (the panel shows the test's last assertion)
✓ rust: stub bodies — stub_lines (2) · from shipped
✓ rust: signature facts — [types] string, integer, nothing, optional, param, annotation, returns, assigned, record, method, checks_none · from shipped
✓ plans runner claude — /opt/homebrew/bin/claude; timeout 300 s
$ echo $?
1
```

A language row names the layers it came from ([chapter 9](09-packs-and-language-adapters.md#where-a-language-comes-from)),
and each capability the layer that set its key (`toolchain cargo`: the preset filled it in).

| check | what it looks at |
|---|---|
| `config` | the project root: the nearest directory with a `.olifant/`, from here upwards |
| `pack NAME`, `language NAME` | each pack and language adapter that loaded; an adapter or generator that failed to load is an `error`, with why |
| `ast-grep` | `ast-grep` on this process's `PATH` |
| `check tool EXE (LANGUAGE)` | the first word of each adapter's `[check] command` (`cargo`, `elm`, `cabal`) on this process's `PATH`, which is printed when it is missing |
| `test runner EXE (LANGUAGE)` | the first word of each adapter's `[test] command`, as for the check tool; its detail ends with the language's `tests_for` |
| `LANGUAGE: CAPABILITY` | what each language can do, and the key that would enable what it cannot (a `warn`, never an error), thirteen rows in this order: **units** (`[unit] kinds`, and `name`, `join`), **diagnostics** (`[check]`, its format, and `convert`), **missing cases** (a `[check] format` that reads them: `ghc-json`, `pyright-json`, `rustc-json`, `elm-json`, `olifant-json`), **tests** (`[test]`, its format and `tests_for`), **test units** (`[test] unit`; pytest's rules come with the `pytest` preset), **test selection** (`[test] select`; pytest ids for `pytest`; without it a run is whole files), **mutation** (`[[mutate]]`, with the number of operators), **stub** (`[test] stub`), **conjure shapes** (the parts of a pack's `[conjure]`; absent means conjure works at the floor), **messages** (`[check] undefined`, `missing_member`), **test messages** (`[test] undefined`, `[test.failure]`), **stub bodies** (`stub_lines`) and **signature facts** (`[types]`). Each row ends with the layer that set its key. [Chapter 9](09-packs-and-language-adapters.md) has the keys |
| `recognize model` | only with `[recognize] model` set ([chapter 16](16-the-language-server.md#recognition-this-looks-like-a-command)): ok when it names a backend under `[model.backends]`, an `error` when it does not |
| `plans runner RUNNER` | `[plans] runner` ([chapter 19](19-plans.md#plans)), `claude` by default, on this process's `PATH`, with the errand timeout and `do_tools` when set; missing is an `error`: install Claude Code, or set `[plans] runner` |
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

## Fixing a finding: `olifant fix`

`olifant fix RULE --at FILE:LINE:COL [--fix N] [--plan] [--model NAME] [--no-fill] [--progress]`
runs a rule's fix where its finding is: the code it replaces (an import deleted, a statement
rewritten through a hole) and the fix's generator, as one plan — what an editor's "Fix: …" does
([chapter 15](15-conventions-that-hold.md#fixes-that-remove-the-code-they-found)). `--fix N`
picks the rule's Nth fix (1, the default, is the usual answer). No finding of that rule at that
line is exit 3.
