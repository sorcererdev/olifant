# 14. Olifant in your editor

Engineers meet Olifant through **Durandal**, the editor side of the product. Olifant is the
conventions engine underneath it. The first editor is Neovim, with the `durandal.nvim`
plugin. It runs `olifant` for everything, so this chapter covers both halves: what the
plugin does, and the three things Olifant gives an editor:

- **`[applies_at]`** in a `generator.toml`: where in the code a generator applies, and which
  of its inputs the code there already gives;
- **`olifant actions --at FILE:LINE:COL`**: the generators that apply at a cursor;
- a plan an editor can apply as buffer edits (**`files`**), and progress while it is built
  (**`gen --progress`**).

## What Durandal does

You write something by hand: a field on a command's struct, a variant on a `Failure` enum, a
constructor on a Haskell `data` type. With the cursor on it, press `<leader>ma` ("m" for
magic). Durandal offers the generators that apply there. The inputs it can read from the code
are already filled in, so it asks only for what is missing. Then it shows the whole change,
with any holes a model filled ([chapter 12](12-holes-and-models.md)), and nothing lands until
you press `<CR>`. The change goes in as ordinary buffer edits, so `u` undoes it.

`<leader>mg` is the other way in: pick any generator, then its inputs one at a time, with
`one_of` choices narrowing as you go ([chapter 6](06-queries-and-choices.md#dependent-choices)).

| key | command | what it does |
|---|---|---|
| `<leader>ma` | `:DurandalActions` | generators that apply at the cursor (`olifant actions`) |
| `<leader>mg` | `:DurandalGenerate` | any generator, inputs from a picker (`olifant show`) |

In the preview, `j`/`k` move between files, `<Tab>` switches to the diff, `<CR>` applies and
`q`/`<Esc>` cancels. A plan with conflicts shows why, and cannot be applied. While a model is
filling holes, a spinner says so (that is `--progress`, [below](#progress-gen---progress)).

The plugin's README (in the `durandal.nvim` repository) has the install steps and a
three-beat demo on the rust-htmx fixture. It needs Neovim 0.11+, `nui.nvim`, and `olifant`
and `ast-grep` on your `PATH`.

Two things the plugin does that Olifant doesn't:

- **It saves modified buffers in the project before it plans.** `gen` reads the disk.
  (`olifant actions` can read an unsaved buffer, [below](#unsaved-text---content--).)
- **It logs** which actions it offered and which you picked, to
  `~/.cache/durandal/actions.jsonl`, on your machine only, so that where generators apply can
  be tuned. Olifant logs nothing.

## The demo, from the command line

Everything Durandal does is a command you can run. In a rust-htmx sandbox
(`scripts/try-pack.sh rust-htmx /tmp/demo && cd /tmp/demo`, with a `[model]` as in
[chapter 12](12-holes-and-models.md#configuring-a-model)), add a failure to `refund_invoice`
by hand:

```rust
#[derive(Error, Debug, PartialEq)]
pub enum RefundInvoiceFailure {
    #[error("Something went wrong")]
    Unknown(String),
    #[error("The invoice was already refunded")]
    AlreadyRefunded,
}
```

`AlreadyRefunded` is line 28, and its name starts at column 5. Ask what applies there:

```
$ olifant actions --at billing/billing-service/src/refund_invoice.rs:28:5 --human
Add AlreadyRefunded to every match  (rust-htmx/failure-arms)
```

```
$ olifant actions --at billing/billing-service/src/refund_invoice.rs:28:5 --json
[{"generator":"rust-htmx/failure-arms","label":"Add AlreadyRefunded to every match","missing":[],"sets":{"command":"refund_invoice","failure":"AlreadyRefunded","service":"billing"}}]
```

`service` and `command` came from the file's path, and `failure` from the variant under the
cursor. Nothing is `missing`, so Durandal goes straight to the plan, which is the `gen`
command those `sets` spell:

```
$ olifant gen rust-htmx/failure-arms --set service=billing --set command=refund_invoice --set failure=AlreadyRefunded --progress --human
{"event":"check","language":"rust"}
{"event":"check","language":"rust"}
{"attempt":1,"event":"fill","holes":["status"],"model":"qwen3-coder:30b"}
{"event":"check","language":"rust"}
{"attempt":1,"event":"filled","hole":"status"}
rust-htmx/failure-arms (1 edit, 0 conflicts)
  ~ billing/billing-service/src/refund_invoice.rs (+1 line at line 36, compiler: status)
holes:
  status (expr) billing/billing-service/src/refund_invoice.rs:35  filled by qwen3-coder:30b: 409
```

```diff
         match self {
             RefundInvoiceFailure::Unknown(_) => 500,
+            RefundInvoiceFailure::AlreadyRefunded => 409,
         }
```

(The JSON lines are the progress events, on stderr. The run took under two seconds.)

`rust-htmx/failure-arms` is the editor-shaped half of `rust-htmx/command-failure`
([chapter 4](04-compiler-branches-and-todos.md#rust-cargo)): it has no `insert`, because you
already wrote the variant, and only the `complete` edit, whose arm's value is the `status`
hole. With the cursor on a field of the `RefundInvoice` struct instead (line 9), the same
command offers `rust-htmx/wire-command`:

```
$ olifant actions --at billing/billing-service/src/refund_invoice.rs:9:5 --json
[{"generator":"rust-htmx/wire-command","label":"Wire refund_invoice's dependencies","missing":[],"sets":{"command":"refund_invoice","service":"billing"}}]
```

and in the `match` (line 34), nothing:

```
$ olifant actions --at billing/billing-service/src/refund_invoice.rs:34:5 --json
[]
```

## `[applies_at]`: where a generator applies

Here is `failure-arms`'s:

```toml
# packs/rust-htmx/generators/failure-arms/generator.toml (excerpt)
# In an editor (olifant actions): on a variant of a command's Failure enum.
[applies_at]
path  = "{service}/*-service/src/{command}.rs"
node  = { kind = "enum_variant", inside = { kind = "enum_item", regex = "Failure", stopBy = "end" } }
bind  = { failure = "^([A-Za-z0-9_]+)" }
label = "Add {= failure =} to every match"
```

A generator applies at a cursor when the file's path matches `path` and, if there is a
`node`, the cursor is inside a node the rule matches. Then every input the path and the node
give is set, and the rest are asked for.

| key | required | what it does |
|---|---|---|
| `path` | yes | a pattern over the file's project-relative path. `{name}` matches one path segment (or the part of one between literals, as in `{command}.rs`) and binds the input `name`; `*` matches within a segment; `**` matches across segments (`src/**.hs` is every `.hs` file under `src/`) |
| `node` | no | an ast-grep rule, written as in an AST query ([chapter 6](06-queries-and-choices.md)). The cursor must be inside a node it matches. Without `node`, the path alone decides |
| `bind` | no | `{ input = "regex" }`: an input set from the matched node's text, the regex's first capture group. POSIX extended, like `each` in AST queries |
| `file` | no | the name of an input that gets the file's own project-relative path (`src/Olifant/Catalog.hs`), which a one-segment `{slot}` cannot bind |
| `label` | no | a template: what the editor shows. Defaults to the generator's `description` |

The details:

- **The rule is a template over the path's slots.** `wire-command` offers itself only on
  fields of its own command's struct, not on `RefundInvoiceInput`'s:

  ```toml
  # packs/rust-htmx/generators/wire-command/generator.toml (excerpt)
  [applies_at]
  path  = "{service}/*-service/src/{command}.rs"
  node  = { kind = "field_declaration", inside = { kind = "struct_item", regex = "^pub struct {= command|pascal =} ", stopBy = "end" } }
  label = "Wire {= command =}'s dependencies"
  ```

  Only path slots are in scope there, not `bind` values or `[derive]`.
- **The innermost node wins.** When several matches contain the cursor, `bind` reads the
  smallest. A cursor just past a node's last character still counts as inside it; a cursor on
  the `|` before an Elm variant does not.
- **A `bind` whose regex doesn't match** leaves its input unset, so it is asked for.
- **The label sees only the bound inputs.** If it uses anything else (a derived value, an
  input that wasn't bound), it falls back to the `description`.
- **Bound values must be valid choices.** A value bound to an `enum` input that isn't one of
  its choices (a `one_of` resolved against the project, with the other bound inputs) is
  dropped and listed in `missing` instead, so the editor asks. On a struct typed into
  `lib.rs`, which the `commands` query excludes:

  ```
  $ printf 'pub struct Lib {\n    pub x: u8,\n}\n' | olifant actions --at billing/billing-service/src/lib.rs:2:5 --content - --json
  [{"generator":"rust-htmx/wire-command","label":"Wire a command's dependencies after you add fields to its struct by hand (spec §7.1: reconcile)","missing":["command"],"sets":{"service":"billing"}}]
  ```

  (`command=lib` was dropped, so the label fell back to the description.) Other input types
  are not checked here; `gen` checks them as usual.

`olifant-dev/error-case`, which Olifant uses on its own source, shows `file` and a `bind`
over a declaration:

```toml
# packs/olifant-dev/generators/error-case/generator.toml (excerpt)
# In an editor (olifant actions): on a data type's declaration. The module is
# the file you are in; the type comes from the declaration.
[applies_at]
path  = "src/**.hs"
file  = "path"
node  = { kind = "data_type" }
bind  = { type = "^data[[:space:]]+([A-Za-z0-9_]+)" }
label = "Add a constructor to {= type =} (GHC finds every case)"
```

In the Olifant repository, on `data CatalogError` (`src/Olifant/Catalog.hs`, line 82):

```
$ olifant actions --at src/Olifant/Catalog.hs:82:3 --human
Add a constructor to CatalogError (GHC finds every case)  (olifant-dev/error-case)  asks: name
$ olifant actions --at src/Olifant/Catalog.hs:82:3 --json
[{"generator":"olifant-dev/error-case","label":"Add a constructor to CatalogError (GHC finds every case)","missing":["name"],"sets":{"path":"src/Olifant/Catalog.hs","type":"CatalogError"}}]
```

`name`, the new constructor, is the one thing the code can't say, so it is the one thing
asked.

Mistakes in `[applies_at]` are load errors, like any other key: `olifant list` marks the
generator `INVALID`, `gen` exits 1, and `olifant actions` leaves it out.

| mistake | error |
|---|---|
| a `{slot}` that isn't an input | `` `{pages}` in applies_at.path is not an input (inputs: page, msg) `` |
| a `bind` key that isn't an input | `` `bind.message`: `message` is not an input `` |
| `file` naming something that isn't an input | `` file: `where` is not an input `` |
| a Perl class in a `bind` regex | `` `bind.msg`: `^(\w+)` is not a valid regular expression (POSIX extended: use [[:space:]] and [a-z_0-9], not \s or \w) `` |
| an unknown key | `` unknown key(s) `colour`; expected one of `path`, `node`, `bind`, `file`, `label` `` |

(Each is prefixed with the file and `` `applies_at`: ``.)

## `olifant actions`

```
olifant actions --at FILE:LINE:COL [--content -]
```

- **`FILE`** is relative to where you run `olifant` (or absolute); it is matched as a
  project-relative path, so `cd src && olifant actions --at Olifant/Catalog.hs:82:3` gives the
  same answer as above.
- **`LINE` and `COL` are 1-based**, as editors show them. `COL` counts UTF-8 bytes, as
  Neovim's cursor and LSP's `utf-8` position encoding do: on `"Café" x`, `x` is column 9, not
  8. A malformed position is a usage error (exit 2): `option --at: expected FILE:LINE:COL
  (1-based)`.
- The JSON is a list, one object per generator that applies, in catalog order:
  `generator` (its id), `label`, `sets` (the inputs bound, as `--set` would take them) and
  `missing` (the required inputs, with no default, that are still unset).
- The human form is one line per action: the label, the generator, and `asks: …` when
  something is missing. When nothing applies, JSON is `[]`, the human form prints nothing,
  and the exit code is 0 either way.

### Unsaved text: `--content -`

An editor's buffer is often ahead of the disk. `--content -` reads the file's text from
stdin and matches against that, not the file. With a variant typed but not saved:

```
$ sed 's/    Unknown(String),/    Unknown(String),\n    NotPaid,/' billing/billing-service/src/refund_invoice.rs \
    | olifant actions --at billing/billing-service/src/refund_invoice.rs:27:6 --content - --human
Add NotPaid to every match  (rust-htmx/failure-arms)
```

The `one_of` choices are still read from the project on disk.

## Plans for editors: `files`

An editor wants the result, not a list of inserts: it shows a diff and applies whole files as
buffer changes, so each buffer has its own undo. `gen --plan --json` has a `files` array, one
entry per file the plan writes:

```json
"files": [{"path": "src/Page/Home.elm",
           "before": "module Page.Home exposing (Model, Msg, init, update, view)\n…",
           "after":  "module Page.Home exposing (Model, Msg, init, update, view)\n…"}]
```

- `before` is the file's text before the plan, `null` for a file the plan creates — including
  files only the compiler found (a `complete` edit's sites), which are read for this. `after`
  is its text with the plan applied, fills included.
- Like each edit's `content`, `files` is there only when nothing was written: an applied run
  has none.
- A plan with conflicts still exits 5 with the plan on stdout. An editor shows it as a plan
  with conflicts, not as an error.

## Progress: `gen --progress`

A `gen` with `complete` edits or holes can take seconds: compiler checks, then model calls.
`--progress` writes what it is doing to **stderr**, one JSON object per line, while stdout
gets the plan as usual. Like `--plan`, it applies to the whole run, wherever it appears in a
`+` chain.

| event | fields | when |
|---|---|---|
| `check` | `language` | before each compiler check: each round of the `complete` loop, and each check of fills |
| `fill` | `holes`, `attempt`, `model` | before each request to the model: the holes in it, the round (1 first), and the model's name |
| `filled` | `hole`, `attempt` | a hole's fill was accepted, on that attempt |
| `unfilled` | `hole`, `why` | a hole was given up on, after the last attempt |

With a backend whose `url` nothing listens on, all three attempts fail:

```
$ olifant gen rust-htmx/failure-arms --set service=billing --set command=refund_invoice --set failure=AlreadyRefunded --plan --human --progress --model down
{"event":"check","language":"rust"}
{"event":"check","language":"rust"}
{"attempt":1,"event":"fill","holes":["status"],"model":"qwen3-coder:30b"}
{"attempt":2,"event":"fill","holes":["status"],"model":"qwen3-coder:30b"}
{"attempt":3,"event":"fill","holes":["status"],"model":"qwen3-coder:30b"}
{"event":"unfilled","hole":"status","why":"curl exited 7: curl: (7) Failed to connect to localhost port 11999 after 0 ms: Couldn't connect to server"}
Plan: rust-htmx/failure-arms (1 edit, 0 conflicts)
  ~ billing/billing-service/src/refund_invoice.rs (+1 line at line 36, compiler: status)
holes:
  status (expr) billing/billing-service/src/refund_invoice.rs:35  unfilled: curl exited 7: …
```

A plan with no checks and no holes (most `insert`-only generators) emits no events.

## Tutorial: make your generator an action

This adds `[applies_at]` to a small project generator and tests it with `olifant actions`,
before any editor is involved. It runs in an Elm sandbox:

```sh
scripts/try-pack.sh elm /tmp/olifant-act && cd /tmp/olifant-act
```

### Step 1: a generator for a Msg you typed

Adding a message by hand to `src/Page/Home.elm`'s `type Msg` leaves `update` without a
branch. This generator adds the branch, wherever the compiler says one is missing
([chapter 4](04-compiler-branches-and-todos.md)). Start with `olifant new msg-branch` and
replace the file with:

```toml
# .olifant/generators/msg-branch/generator.toml
description = "The update branch for a Msg you added by hand: ( model, Cmd.none )"
language = "elm"
version = 1
examples = ["olifant gen msg-branch --set page=home --set msg=reset"]

[inputs.page]
type = "ident"
prompt = "The page (e.g. home)"

[inputs.msg]
type = "ident"
prompt = "The Msg variant you added (e.g. reset)"

[requires]
exists = ["src/Page/{= page|pascal =}.elm"]

[[edits]]
complete = "{= msg|pascal =}"
where    = "case in update"
text     = """
{= pattern =} ->
    ( model, Cmd.none )"""
```

Add the variant by hand:

```elm
type Msg
    = NoOp
    | Reset
```

and check that the generator works from the command line first:

```
$ olifant gen msg-branch --set page=home --set msg=reset --plan --human
Plan: msg-branch (1 edit, 0 conflicts)
  ~ src/Page/Home.elm (+2 lines at line 34, compiler: update)

--- src/Page/Home.elm
Reset ->
    ( model, Cmd.none )
```

### Step 2: the path

Every page module is `src/Page/NAME.elm`, and `NAME` is the page. Append:

```toml
[applies_at]
path = "src/Page/{page}.elm"
```

Anywhere in a page module, the generator now applies, with `page` set:

```
$ olifant actions --at src/Page/Home.elm:25:7 --human
The update branch for a Msg you added by hand: ( model, Cmd.none )  (msg-branch)  asks: msg
$ olifant actions --at src/Page/Home.elm:1:1 --json
[{"generator":"msg-branch","label":"The update branch for a Msg you added by hand: ( model, Cmd.none )","missing":["msg"],"sets":{"page":"Home"}}]
$ olifant actions --at src/Main.elm:1:1 --json
[]
```

That is too broad: line 1 is the module header, and nothing there suggests a message.

### Step 3: the node, and an input from it

Narrow it to a variant of `type Msg`, and read the variant's name from it:

```toml
[applies_at]
path  = "src/Page/{page}.elm"
node  = { kind = "union_variant", inside = { kind = "type_declaration", regex = "^type Msg", stopBy = "end" } }
bind  = { msg = "^([A-Za-z0-9_]+)" }
label = "Add the update branch for {= msg =}"
```

`union_variant` and `type_declaration` are the node kinds of the Elm grammar the pack vendors;
the rule language is the one in [chapter 6](06-queries-and-choices.md). Probe a few positions
(line 23 is `type Msg`, 24 `= NoOp`, 25 `| Reset`, 31 the `NoOp ->` branch):

```
$ olifant actions --at src/Page/Home.elm:25:7 --json
[{"generator":"msg-branch","label":"Add the update branch for Reset","missing":[],"sets":{"msg":"Reset","page":"Home"}}]
$ olifant actions --at src/Page/Home.elm:24:7 --json
[{"generator":"msg-branch","label":"Add the update branch for NoOp","missing":[],"sets":{"msg":"NoOp","page":"Home"}}]
$ olifant actions --at src/Page/Home.elm:25:5 --json
[]
$ olifant actions --at src/Page/Home.elm:23:6 --json
[]
$ olifant actions --at src/Page/Home.elm:31:9 --json
[]
```

Column 5 of line 25 is the `|`, which is not part of the variant. Nothing is missing on
`Reset`, so Durandal would go straight to the preview. On `NoOp`, which already has its
branch, the run finds nothing to add:

```
$ olifant gen msg-branch --set page=Home --set msg=NoOp --human
msg-branch (0 edits, 0 conflicts)
```

### Step 4: before saving

Type another variant without saving, and pass the buffer on stdin:

```
$ sed 's/    | Reset/    | Reset\n    | Increment/' src/Page/Home.elm | olifant actions --at src/Page/Home.elm:26:7 --content - --human
Add the update branch for Increment  (msg-branch)
```

### Step 5: what the editor will apply

```
$ olifant gen msg-branch --set page=Home --set msg=Reset --plan --json --progress 2>progress.txt | jq -c '.files[] | {path, new: (.before == null)}'
{"path":"src/Page/Home.elm","new":false}
$ cat progress.txt
{"event":"check","language":"elm"}
{"event":"check","language":"elm"}
```

Two checks: the first finds the missing `Reset` branch, the second confirms the plan
compiles. `new` is false: the page existed, so its `before` is its old text.

When `olifant actions` gives the right answer at the right places, the generator is ready
for `<leader>ma`. Give it a test case as usual ([chapter 10](10-testing-generators.md)).

## Habits for `[applies_at]`

- **Bind what the code says; ask for what it doesn't.** A generator that only reshapes code
  you just wrote (`failure-arms`, `wire-command`) can often bind everything, so the action
  runs with no questions.
- **Make `node` specific.** A rule that matches whole files, or every struct, offers the
  generator where it makes no sense. Use `inside` and the path's slots, as `wire-command`
  does.
- **Probe positions with `olifant actions`** the way you probe `at` phrases with
  `olifant where`: on the node, just outside it, and in a place it should not apply.
- **Write the label as the action.** "Add AlreadyRefunded to every match" says what will
  happen; the description is the fallback.
