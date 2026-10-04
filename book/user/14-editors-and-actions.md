# 14. Olifant in your editor

Engineers meet Olifant through **Durandal**, the editor side of the product. Olifant is the
conventions engine underneath it. The first editor is Neovim, with the `durandal.nvim`
plugin. It runs `olifant` for everything, so this chapter covers both halves: what the
plugin does, and what Olifant gives an editor:

- **`[applies_at]`** in a `generator.toml`: where in the code a generator applies, which
  of its inputs the code there already gives, (`where`) what it looks for, in words, and
  (`suggest`) whether an editor may offer it unasked;
- **`olifant actions --at FILE:LINE:COL`**: the generators that apply at a cursor;
- **`olifant actions --file FILE`**: every place in a file where one applies, and the
  generators that could apply but found nothing yet;
- a plan an editor can apply as buffer edits (**`files`**), and progress while it is built
  (**`gen --progress`**), with a warning first when a file it touches does not parse;
- **`gen --plan --overlay FILE`**: a plan against a buffer you have not saved, which is how
  Durandal shows ghost text and follow-ups ([below](#ghost-text-and-follow-ups));
- **`olifant sync --plan`** and **`olifant check --file FILE`**: the conventions on save,
  regions refreshed as buffer edits and rule findings as diagnostics
  ([below](#conventions-on-save));
- **`olifant doctor`**: what generators need on this machine, and how to fix what is missing
  ([Troubleshooting](#troubleshooting)).

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
| | `:DurandalHints [toggle\|on\|off]` | the ⚡ hints ([below](#hints-on-the-cursor-line)); no argument toggles |
| `<Tab>` | | while a suggestion is shown, and only then: apply the ghost text, or review the follow-up ([below](#ghost-text-and-follow-ups)) |
| | `:DurandalGhosts [toggle\|on\|off]` | ghost text and follow-ups; no argument toggles |
| | `:checkhealth durandal` | Neovim, `nui.nvim`, `olifant`, then `olifant doctor` ([Troubleshooting](#troubleshooting)) |

In the preview, `j`/`k` move between files, `<Tab>` switches to the diff, `<CR>` applies and
`q`/`<Esc>` cancels. A plan with conflicts shows why, and cannot be applied. While a model is
filling holes, a spinner says so (that is `--progress`, [below](#progress-gen---progress)).

The plugin's README (in the `durandal.nvim` repository) has the install steps and a
three-beat demo on the rust-htmx fixture. It needs Neovim 0.11+, `nui.nvim`, and `olifant`
and `ast-grep` on your `PATH` — Neovim's `PATH`, which is not always your shell's
([Troubleshooting](#troubleshooting)).

Two things the plugin does that Olifant doesn't:

- **It saves modified buffers in the project before it plans** from `<leader>ma` or
  `<leader>mg`. `gen` reads the disk. (`olifant actions` can read an unsaved buffer,
  [below](#unsaved-text---content--), and so can `gen --plan --overlay`, which suggestions
  use instead of saving.)
- **It logs** which actions it hinted, offered and suggested, and which you picked, applied
  or dismissed, to `~/.cache/durandal/actions.jsonl`, on your machine only, so that where
  generators apply can be tuned. Olifant logs nothing.

### Hints on the cursor line

When a generator applies on the line the cursor is on (anywhere in a node that spans several
lines), a dim hint appears at the end of it:

```
    Unknown(String), ⚡ Add Unknown to every match
```

`+N` follows the label when N more apply on that line. The hint is only ever on the cursor
line, never on the rest of the file, and never for a generator whose `[applies_at]` has no
`node`: one that applies to the whole file would hint on every line, so it is in the
`<leader>ma` menu only.

The hints come from one `olifant actions --file` call per buffer
([below](#every-place-in-a-file---file)), sent the buffer's unsaved text, not from a call per
cursor move. Durandal makes that call when you enter the buffer and when you save it, and
after a change once the buffer has been quiet for 300 ms (a change in normal mode, or leaving
insert mode). Moving the cursor only redraws. If a call fails, the last good hints stay.

Turn them off for the session with `:DurandalHints off` (`on` and `toggle` as you'd expect),
or for good in the setup:

```lua
opts = { hints = false },
```

The hint's highlight group is `DurandalHint`, linked to `Comment`; link it to something else
to make it louder.

### Ghost text and follow-ups

Some generators are worth running the moment you write the line they react to: wiring a
field you just added to a command's struct, or giving a new `Failure` variant its arm in
every `match`. For those, the generators whose `[applies_at]` says `suggest = true`
([below](#applies_at-where-a-generator-applies)), Durandal doesn't wait for `<leader>ma`.
Type the field into `CancelInvoice` (beat 1 of the demo) and pause; next to the line's ⚡
hint, a follow-up appears:

```
⚡ Tab: review Wire Command Dependencies (+2 lines here, +7 in service.rs, main.rs)
```

`Tab` opens the review: the same preview as `<leader>ma`'s, titled `Wire Command Dependencies
Tab apply · Esc dismiss`. There `Tab` (or `<CR>`) applies, `Esc` dismisses, and `q` closes the
review and leaves the follow-up where it was. Nothing changes that you have not seen.

Whether a suggestion is **ghost text** or a **follow-up** depends on its plan, not on the
generator. It is ghost text only when the whole plan is one contiguous insert into the file
you are in, starting on the cursor's line or the line after it, and at most 8 lines. Those
lines are drawn dim where they would go, each hole's placeholder shown as `⟨name⟩`, with `⚡
Tab: NAME` at the end of the cursor's line, and `Tab` applies them. Anything else is a
follow-up: a plan that touches another file, or more lines, or lines further away. Its
summary counts the lines added in this file (`here`) and in the others, by file name.

The rule comes from research on code suggestions (spec §4.8 has the references). Ghost text
helps when you already know what comes next and want it sooner (Barke et al., 2023).
Checking suggestions already takes about a fifth of a session (Mozannar et al., 2024), long
ones are hard to take in at a glance (Vaithilingam et al., 2022), and a quarter of
developers find them distracting (Liang et al., 2024). The editors that suggest edits away
from the cursor, Copilot's next edit suggestions and Cursor's Tab, point to them and let you
jump there rather than drawing them where you type. So a convention's next edit is ghost
text only when it is small and right where you are, and a follow-up you review otherwise.

Today both of rust-htmx's suggestions are follow-ups. `wire-command` changes `service.rs`
and `main.rs` too. `failure-arms`'s arm goes into the `match` seven lines below the variant
(`⚡ Tab: review Add Failure Arms (+1 line here)`); were it on the cursor's line or the next,
it would be the ghost line

```
            RefundInvoiceFailure::AlreadyRefunded => ⟨status⟩,
```

Ghost text proper arrives with snippets (spec §4.9), which are small inserts at the cursor.

**When Durandal plans.** Once the cursor has settled (300 ms) on a line where a `suggest`
action applies, according to the hints' `--file` answer, and nothing is `missing`: a
generator that would have to ask you something is only ever in `<leader>ma`. It runs the
action's `gen` with `--plan --json --no-fill --overlay FILE`, the buffer's text on stdin
([below](#from-the-command-line)), so it saves nothing and asks no model. One plan at a time
per buffer; if the buffer changed while it ran, the answer is dropped; a plan that fails, has
a conflict or changes nothing shows nothing.

**`Tab` is Durandal's only while a suggestion is shown**, as a buffer-local mapping in normal
and insert mode that is removed with the suggestion. A completion menu (the built-in one,
`blink.cmp` or `nvim-cmp`) clears the suggestion when it opens, and none is drawn while one is
open, so its `Tab` is never taken.

**Accepting plans again, for real**: the same `gen` against the buffer as it is now, without
`--no-fill`, so the model fills the holes (with a spinner, `--progress`), and the plan's
`files` go in as buffer edits, one undo step per file, and are written, your buffer
included. If a file changed between that plan and applying it, nothing is applied:
`FILE changed since planning; nothing was applied — plan again`. A plan with a conflict
says `not applied:` and why. What lands has the model's fills where the suggestion showed
`⟨name⟩`; filling only on accept keeps the background plan fast and free.

**Dismissing.** `Esc` in normal mode, or `Esc` in a follow-up's review, dismisses it, and it
is not offered again for that generator, those inputs and that line's text: edit the line,
and it may come back. Moving the cursor off the line only clears it; come back and it is
planned again.

Each step goes to the action log as an event with the generator: `ghost-shown`,
`ghost-applied`, `ghost-dismissed`, `followup-shown`, `followup-reviewed`, `followup-applied`
and `followup-dismissed`. They are what will say whether the split is right.

Turn suggestions off for the session with `:DurandalGhosts off`, or for good:

```lua
opts = { ghosts = false },
```

Ghost lines use the `DurandalGhost` highlight group and the `⚡ Tab:` text uses
`DurandalHint`, both linked to `Comment`.

#### From the command line

The background plan is an ordinary command. In the rust-htmx sandbox, with
`pub email_sender: Arc<dyn EmailSender>,` typed into `CancelInvoice` and not saved (`sed`
plays the buffer here):

```
$ F=billing/billing-service/src/cancel_invoice.rs
$ buffer() { sed 's/^pub struct CancelInvoice {}/pub struct CancelInvoice {\n    pub email_sender: Arc<dyn EmailSender>,\n}/' $F; }
$ buffer | olifant actions --at $F:7:5 --content - --json
[{"generator":"rust-htmx/wire-command","label":"Wire cancel_invoice's dependencies","missing":[],"name":"Wire Command Dependencies","sets":{"command":"cancel_invoice","service":"billing"},"suggest":true}]
$ buffer | olifant gen rust-htmx/wire-command --set command=cancel_invoice --set service=billing --plan --human --no-fill --overlay $F
Plan: rust-htmx/wire-command (7 edits, 0 conflicts)
  = billing/billing-service/src/cancel_invoice.rs (skipped: unless: line 7 of billing/billing-service/src/cancel_invoice.rs matches)
  ~ billing/billing-service/src/cancel_invoice.rs (+2 lines at line 4)
  ~ billing/billing-service/src/service.rs (+2 lines at line 9)
  ~ billing/billing-service/src/service.rs (+1 line at line 22)
  ~ billing/billing-service/src/service.rs (+1 line at line 30)
  ~ main/src/main.rs (+1 line at line 22)
  ~ main/src/main.rs (+1 line at line 25)
```

(Each edit's text is left out here.) It took 0.16 seconds, and `git status` is clean
afterwards. `failure-arms`'s plan for a new variant takes a little over a second, since
`cargo` checks the buffer's text to find the `match`.

### When nothing applies

`<leader>ma` where nothing applies says why, from the same `--file` answer:

- **Something applies elsewhere in the file.** A menu titled "Nothing at the cursor. In this
  file:" lists each place, as `⚡ Add Unknown to every match — line 22`. Picking one moves
  the cursor there and goes on as if you had asked there.
- **Nothing applies anywhere in the file, but a generator could.** Each generator whose path
  matched says what it is waiting for, from its `where`
  ([below](#near-misses-where-and-nearby)). In a new `pay_invoice.rs` with only its struct
  typed so far:

  ```
  rust-htmx/failure-arms applies on a variant of the PayInvoiceFailure enum — none yet
  rust-htmx/wire-command applies on a field of the PayInvoice struct — none yet
  ```

- **No generator's path matches the file**: `No Olifant generators apply to Cargo.toml`.

The second kind shows only when the first doesn't. In `cancel_invoice.rs`, which always has
its `Unknown` variant, a miss lists that variant's line, not what `wire-command` is waiting
for.

### Code that does not parse

You are often mid-edit when you ask for an action. If a file the plan touches does not parse
(a `;` where a struct field wants `,`), the preview's notes start with Olifant's warning
([below](#code-that-does-not-parse-in-a-plan)), before holes and other warnings:

```
⚠ billing/billing-service/src/cancel_invoice.rs:7 does not parse (near `;`) — this plan may place code wrongly, and checks will fail
```

and its title ends with `gd go to the error`. `gd` closes the preview and puts the cursor on
that line, so you can fix the code and ask again. The warning alone doesn't stop you
applying the plan: it is a warning, not a conflict.

## Conventions on save

Generators keep conventions when they write code; Durandal keeps them when you do
([chapter 15](15-conventions-that-hold.md)). Every time you save a buffer in an Olifant
project, it does two things, and nothing while you type.

### Regions, as buffer edits

First it runs `olifant sync --plan --json` ([chapter 5](05-owned-regions.md#--plan-the-refresh-for-editors))
and applies the plan's `files` the way it applies a generator's: as ordinary buffer edits,
written, each an undo step in its buffer. Save a new `billing/billing-service/src/void_payment.rs`
and `lib.rs` gains its line, with a notice:

```
synced billing/billing-service/src/lib.rs: modules +1
```

(`modules +0 −1` when a file went away.) The refresh covers the whole project, not only the
file you saved.

It never writes over your unsaved work. If a file the refresh would change is open with
unsaved changes, nothing is applied, and the notice says so:

```
billing/billing-service/src/lib.rs has unsaved changes; region modules not refreshed (save it, then save again)
```

A refresh with a conflict (a broken fence) is not applied either; `olifant sync --check` in
a terminal says why.

### Rule findings as diagnostics

Then it checks the saved buffer's rules, with `olifant check --file FILE --content -` and the
buffer's text on stdin, and shows each finding as a Neovim diagnostic (source `durandal`, the
rule's id as its code). It does the same when you enter a buffer.

- The diagnostic has the rule's severity: `hint`, `warn` or `error`. With `use
  diesel::prelude::*;` in a command, that is a warning under the line: "Commands talk to
  ports, not the database (rust-htmx: ports & adapters)".
- A finding in the baseline ([chapter 15](15-conventions-that-hold.md#a-new-rule-the-ratchet))
  is always a hint, its message ending "(existing)": known debt, not news.
- A finding about a whole file (a `missing` rule) is on line 1.
- A check that fails (no `olifant`, a broken `rules.toml`) changes nothing and says nothing;
  run `olifant check` in a terminal to see why.

Add `// olifant:allow commands-use-ports — spike` to the line, save, and the diagnostic goes.

### A finding's fix, first in `<leader>ma`

When the cursor is on a finding whose rule has a `fix`, `<leader>ma` lists the fix first,
before the actions that apply there:

```
Fix: Commands talk to ports, not the database (rust-htmx: ports & adapters) (Add Repository Port)
```

Picking it goes on as `<leader>mg` would, with the finding's `sets` (the slots of the rule's
`files`) already set: it asks for the rest (`aggregate`), then shows the plan.

### Turning it off

```lua
opts = { conventions = false },
```

turns off both, region refreshes and diagnostics. The diagnostics are in their own
namespace, `require("durandal.conventions").ns`, so Neovim's diagnostic settings apply to
them as to any other: `vim.diagnostic.enable(false, { ns_id = require("durandal.conventions").ns })`
hides them and keeps the refreshes.

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
[{"generator":"rust-htmx/failure-arms","label":"Add AlreadyRefunded to every match","missing":[],"name":"Add Failure Arms","sets":{"command":"refund_invoice","failure":"AlreadyRefunded","service":"billing"},"suggest":true}]
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
[{"generator":"rust-htmx/wire-command","label":"Wire refund_invoice's dependencies","missing":[],"name":"Wire Command Dependencies","sets":{"command":"refund_invoice","service":"billing"},"suggest":true}]
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
where = "on a variant of the {= command|pascal =}Failure enum"
# Offered unasked (spec §4.8): a follow-up you Tab to review, Tab to apply.
suggest = true
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
| `where` | no | a template over the path's slots: what `node` looks for, in words, finishing "`GENERATOR` applies …" (`on a field of the {= command\|pascal =} struct`). Shown when the file has no such node yet ([below](#near-misses-where-and-nearby)). Defaults to a mechanical description of the rule |
| `suggest` | no | `true` lets an editor offer the generator unasked, as ghost text or a follow-up ([above](#ghost-text-and-follow-ups)), wherever it applies with nothing `missing`. `actions` reports it as `"suggest"`. Default `false`: the generator is only in the menu |

The details:

- **The rule is a template over the path's slots.** `wire-command` offers itself only on
  fields of its own command's struct, not on `RefundInvoiceInput`'s:

  ```toml
  # packs/rust-htmx/generators/wire-command/generator.toml (excerpt)
  [applies_at]
  path  = "{service}/*-service/src/{command}.rs"
  node  = { kind = "field_declaration", inside = { kind = "struct_item", regex = "^pub struct {= command|pascal =} ", stopBy = "end" } }
  where = "on a field of the {= command|pascal =} struct"
  suggest = true
  label = "Wire {= command =}'s dependencies"
  ```

  Only path slots are in scope there, not `bind` values or `[derive]`. The same goes for
  `where`, which is why it can name the struct.
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
  [{"generator":"rust-htmx/wire-command","label":"Wire a command's dependencies after you add fields to its struct by hand (spec §7.1: reconcile)","missing":["command"],"name":"Wire Command Dependencies","sets":{"service":"billing"},"suggest":true}]
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
where = "on a data declaration"
label = "Add a constructor to {= type =} (GHC finds every case)"
```

In the Olifant repository, on `data CatalogError` (`src/Olifant/Catalog.hs`, line 93):

```
$ olifant actions --at src/Olifant/Catalog.hs:93:3 --human
Add a constructor to CatalogError (GHC finds every case)  (olifant-dev/error-case)  asks: name
$ olifant actions --at src/Olifant/Catalog.hs:93:3 --json
[{"generator":"olifant-dev/error-case","label":"Add a constructor to CatalogError (GHC finds every case)","missing":["name"],"name":"Add Error Case","sets":{"path":"src/Olifant/Catalog.hs","type":"CatalogError"},"suggest":false}]
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
| a `where` that isn't a template | `` `where`: line 1: unclosed `{=`: expected `=}` `` |
| a Perl class in a `bind` regex | `` `bind.msg`: `^(\w+)` is not a valid regular expression (POSIX extended: use [[:space:]] and [a-z_0-9], not \s or \w) `` |
| a `suggest` that isn't a boolean | `` `suggest`: expected true or false, got a string `` |
| an unknown key | `` unknown key(s) `colour`; expected one of `path`, `node`, `bind`, `file`, `label`, `where`, `suggest` `` |

(Each is prefixed with the file and `` `applies_at`: ``.)

## `olifant actions`

```
olifant actions --at FILE:LINE:COL [--content -]
olifant actions --file FILE [--content -]
```

`--at` asks about one position, `--file` about the whole file
([below](#every-place-in-a-file---file)); exactly one of them is required (`Missing: (--at
FILE:LINE:COL | --file FILE)`, exit 2).

- **`FILE`** is relative to where you run `olifant` (or absolute); it is matched as a
  project-relative path, so `cd src && olifant actions --at Olifant/Catalog.hs:93:3` gives the
  same answer as above.
- **`LINE` and `COL` are 1-based**, as editors show them. `COL` counts UTF-8 bytes, as
  Neovim's cursor and LSP's `utf-8` position encoding do: on `"Café" x`, `x` is column 9, not
  8. A malformed position is a usage error (exit 2): `option --at: expected FILE:LINE:COL
  (1-based)`.
- For `--at`, the JSON is a list, one object per generator that applies, in catalog order:
  `generator` (its id), `name` (its display name), `label`, `sets` (the inputs bound, as
  `--set` would take them), `missing` (the required inputs, with no default, that are still
  unset) and `suggest` (its `[applies_at] suggest`, `false` when not set).
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

### Every place in a file: `--file`

An editor that shows hints wants every place at once, not a process per cursor move.
`--file` answers for the whole file. In the rust-htmx sandbox:

```
$ olifant actions --file billing/billing-service/src/refund_invoice.rs --json
{"actions":[{"generator":"rust-htmx/wire-command","label":"Wire refund_invoice's dependencies","missing":[],"name":"Wire Command Dependencies","range":{"end":[9,55],"start":[9,5]},"sets":{"command":"refund_invoice","service":"billing"},"suggest":true},{"generator":"rust-htmx/failure-arms","label":"Add Unknown to every match","missing":[],"name":"Add Failure Arms","range":{"end":[26,20],"start":[26,5]},"sets":{"command":"refund_invoice","failure":"Unknown","service":"billing"},"suggest":true},{"generator":"rust-htmx/failure-arms","label":"Add AlreadyRefunded to every match","missing":[],"name":"Add Failure Arms","range":{"end":[28,20],"start":[28,5]},"sets":{"command":"refund_invoice","failure":"AlreadyRefunded","service":"billing"},"suggest":true}],"nearby":[]}
```

and in the Olifant repository, where `olifant-dev/error-case` applies on each `data`
declaration:

```
$ olifant actions --file src/Olifant/Catalog.hs --human
52:1  Add a constructor to CatalogEntry (GHC finds every case)  (olifant-dev/error-case)  asks: name
63:1  Add a constructor to Catalog (GHC finds every case)  (olifant-dev/error-case)  asks: name
87:1  Add a constructor to CatalogPaths (GHC finds every case)  (olifant-dev/error-case)  asks: name
93:1  Add a constructor to CatalogError (GHC finds every case)  (olifant-dev/error-case)  asks: name
```

- The JSON is an object with two lists, `actions` and `nearby` (next section).
- **`actions`** has one entry per node a generator's `node` rule matches, each with that
  node's `bind` values, in file order: `generator`, `name`, `label`, `range`, `sets`,
  `missing` and `suggest`, as for `--at`. `range` is `{"start": [LINE, COL], "end": [LINE, COL]}`, with 1-based lines and
  UTF-8 byte columns as `--at` takes them; `end` is just past the node (`Unknown(String)` on
  line 26 runs from column 5 to 20, the `,` after it).
- **A generator without `node`** applies to the whole file: one entry with `"range": null`,
  listed first. Its human line starts with `file` instead of `LINE:COL`:

  ```
  file  Add a test module for cancel_invoice  (command-test)
  22:5  Add Unknown to every match  (rust-htmx/failure-arms)
  ```

  (`command-test` is a project generator with only `path = "{service}/*-service/src/{command}.rs"`.)
  Editors list such an action in their menu but don't hint it.
- The human form is one line per action, `LINE:COL` of the node's start, then as `--at`,
  then one `nearby:` line per near miss.
- `--content -` works as for `--at`. A file no generator's path matches, or one that doesn't
  exist, is `{"actions":[],"nearby":[]}`, exit 0.

### Near misses: `where` and `nearby`

**`nearby`** lists each generator whose `path` matched the file but whose `node` rule found
nothing in it: `generator`, `name`, `label` and `why`, what it looks for. In `cancel_invoice.rs`,
whose struct has no fields yet:

```
$ olifant actions --file billing/billing-service/src/cancel_invoice.rs --human
22:5  Add Unknown to every match  (rust-htmx/failure-arms)
nearby: rust-htmx/wire-command — on a field of the CancelInvoice struct
$ olifant actions --file billing/billing-service/src/cancel_invoice.rs --json
{"actions":[{"generator":"rust-htmx/failure-arms","label":"Add Unknown to every match","missing":[],"name":"Add Failure Arms","range":{"end":[22,20],"start":[22,5]},"sets":{"command":"cancel_invoice","failure":"Unknown","service":"billing"},"suggest":true}],"nearby":[{"generator":"rust-htmx/wire-command","label":"Wire cancel_invoice's dependencies","name":"Wire Command Dependencies","why":"on a field of the CancelInvoice struct"}]}
```

`why` is the generator's `[applies_at] where`, rendered with the path's slots: `on a field of
the {= command|pascal =} struct` became `on a field of the CancelInvoice struct`. Write it to
finish the sentence "`wire-command` applies …", since that is how Durandal shows it
([above](#when-nothing-applies)).

Without a `where` (or with one that doesn't render), `why` describes the rule mechanically.
The same rule as `wire-command`'s, in a project generator with no `where`:

```
nearby: plain-field — a `field_declaration` inside a `struct_item` matching /^pub struct CancelInvoice /
```

It is accurate, and it is why `where` exists. The `label` in `nearby` is rendered with the
path's slots only, since there is no node to `bind` from; a label that needs a bound value
falls back to the description, as `failure-arms`'s does in a new file:

```
$ printf '#[derive(Clone)]\npub struct PayInvoice {}\n' | olifant actions --file billing/billing-service/src/pay_invoice.rs --content - --json
{"actions":[],"nearby":[{"generator":"rust-htmx/failure-arms","label":"Add an arm for a Failure variant you just wrote to every match on its enum (spec §5.6); the arm's value is a hole","why":"on a variant of the PayInvoiceFailure enum"},{"generator":"rust-htmx/wire-command","label":"Wire pay_invoice's dependencies","why":"on a field of the PayInvoice struct"}]}
```

A generator with no `node` is never in `nearby`: it applies whenever its path matches.

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
- With `--overlay FILE` ([chapter 2](02-running-generators.md#an-unsaved-buffer---overlay-file)),
  FILE's `before` is the text sent on stdin, the buffer, not the disk's. An editor checks
  that each file still has its `before` text when it applies `after`, and refuses if not.

## Code that does not parse, in a plan

When it plans, `gen` also parses every existing file the plan changes, as it was before the
plan, with the file's language adapter (the ast-grep check the fill phase uses on fills,
[chapter 12](12-holes-and-models.md)). A file that doesn't parse gets a warning, listed
before any other warning. It is a warning, not a conflict: you may be mid-edit, and the plan
may well be right.

Beat 1 of the demo, typed with a `;` where the field wants `,`:

```rust
#[derive(Clone)]
pub struct CancelInvoice {
    pub email_sender: Arc<dyn EmailSender>;
}
```

```
$ olifant gen rust-htmx/wire-command --set service=billing --set command=cancel_invoice --plan --human
Plan: rust-htmx/wire-command (7 edits, 0 conflicts)
  = billing/billing-service/src/cancel_invoice.rs (skipped: unless: line 7 of billing/billing-service/src/cancel_invoice.rs matches)
  ~ billing/billing-service/src/cancel_invoice.rs (+2 lines at line 4)
  ~ billing/billing-service/src/service.rs (+2 lines at line 9)
  ~ billing/billing-service/src/service.rs (+1 line at line 22)
  ~ billing/billing-service/src/service.rs (+1 line at line 30)
  ~ main/src/main.rs (+1 line at line 22)
  ~ main/src/main.rs (+1 line at line 25)
  ? warning: billing/billing-service/src/cancel_invoice.rs:7 does not parse (near `;`) — this plan may place code wrongly, and checks will fail
```

(`--plan` also prints each edit's text, left out here.) The warning names the first parse
error's line and up to 20 characters of the text there. Tree-sitter recovers from most
errors, so placement often still works, as it did here; but `cargo check` on the result will
fail, so the warning says so before you find out. Some mistakes the parser repairs silently
(a missing `;`, or a `}` missing at the very end of the file); those get no warning, and the
compiler is the first to say so.

When a compiler check in the same plan does fail in that file, its message points back. Chain
beat 1 with an arm for a new `AlreadyPaid` variant in the same broken file:

```
$ olifant gen rust-htmx/wire-command --set service=billing --set command=cancel_invoice \
    + rust-htmx/failure-arms --set service=billing --set command=cancel_invoice --set failure=AlreadyPaid --plan --human --no-fill
Plan: rust-htmx/wire-command + rust-htmx/failure-arms (7 edits, 1 conflict): nothing written
  …
  ! rust-htmx/failure-arms (step 2): cannot complete cases: the generated code does not compile: billing/billing-service/src/cancel_invoice.rs (struct fields are separated by `,`) (see the parse warning above)
  ? warning: billing/billing-service/src/cancel_invoice.rs:7 does not parse (near `;`) — this plan may place code wrongly, and checks will fail
```

Without the parse warning, "the generated code does not compile" would blame the plan for
your `;`. The same suffix goes on a warning that a check could not run because the project
does not compile, when it names that file. (In the human form, conflicts print before warnings, so the warning is
below, not above; Durandal's preview puts it first.)

Only files that exist and that the plan changes are checked: files it creates are its own
text, already checked, and files it only reads are not checked. A `complete` edit's
files count once the compiler has found them and the plan adds a branch there. In JSON the
warning is a string in `warnings`, like any other; editors find it by its shape,
`FILE:LINE does not parse`.

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
[{"generator":"msg-branch","label":"The update branch for a Msg you added by hand: ( model, Cmd.none )","missing":["msg"],"name":"Msg Branch","sets":{"page":"Home"},"suggest":false}]
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
[{"generator":"msg-branch","label":"Add the update branch for Reset","missing":[],"name":"Msg Branch","sets":{"msg":"Reset","page":"Home"},"suggest":false}]
$ olifant actions --at src/Page/Home.elm:24:7 --json
[{"generator":"msg-branch","label":"Add the update branch for NoOp","missing":[],"name":"Msg Branch","sets":{"msg":"NoOp","page":"Home"},"suggest":false}]
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

### Step 6: offer it unasked

A branch for a variant you just typed is always wanted, and the code gives every input, so
let Durandal suggest it. Add to the `[applies_at]`:

```toml
suggest = true
```

`actions` now says `"suggest":true`, and Durandal, once the cursor rests on a new variant,
plans the branch against the unsaved buffer, as this does:

```
$ buffer() { sed 's/    | Reset/    | Reset\n    | Increment/' src/Page/Home.elm; }
$ buffer | olifant actions --at src/Page/Home.elm:26:7 --content - --json
[{"generator":"msg-branch","label":"Add the update branch for Increment","missing":[],"name":"Msg Branch","sets":{"msg":"Increment","page":"Home"},"suggest":true}]
$ buffer | olifant gen msg-branch --set msg=Increment --set page=Home --plan --json --no-fill --overlay src/Page/Home.elm | jq -c '.files[] | {path, new: (.before == null)}'
{"path":"src/Page/Home.elm","new":false}
```

(Pipe the buffer rather than saving a copy in the project: a stray `buf.elm` that also
says `module Page.Home` makes the Elm check fail with `MODULE NAME CLASH`.)

One file, the one you are in, but the branch goes into `update`, nine lines below the
variant, so it is a follow-up, `⚡ Tab: review Msg Branch (+3 lines here)` (the blank line
before the branch counts), not ghost text.

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
- **Suggest only what is always wanted.** `suggest = true` puts the generator in front of
  everyone who writes its node, unasked. It suits a generator that finishes what you just
  wrote and binds every input (`failure-arms`, `wire-command`, `msg-branch`); one that needs
  a name or a choice is never suggested anyway, and one that is only sometimes right belongs
  in the menu. Dismissals in the action log say when it is wrong.
- **Write `where` as the place.** "on a field of the CancelInvoice struct" tells someone
  where to put the cursor; "a `field_declaration` inside a `struct_item` matching /…/" tells
  them how the rule works. Check it with `olifant actions --file` on a file that has no such
  node yet.

## Troubleshooting

### `:checkhealth durandal` and `olifant doctor`

When an action does nothing useful, start with `:checkhealth durandal`. It checks Neovim
(0.11+), `nui.nvim` and that `olifant` is on Neovim's `PATH`, then, from the project of the
current buffer, runs `olifant doctor --json` and shows each of its checks:

```
durandal ~
- ✅ OK Neovim 0.12.5
- ✅ OK nui.nvim
- ✅ OK olifant: /Users/you/.local/bin/olifant

durandal: olifant doctor in /tmp/demo ~
- ✅ OK config — project at /tmp/demo
- ✅ OK pack rust-htmx — loaded
- ✅ OK language rust — loaded
- ✅ OK ast-grep — /opt/homebrew/bin/ast-grep
- ❌ ERROR check tool cargo (rust) — `cargo` is not on this PATH: /Users/you/.local/bin:/opt/homebrew/bin:/usr/bin:/bin
  - ADVICE:
    - install Rust with rustup; cargo lives in ~/.cargo/bin, or /opt/homebrew/opt/rustup/bin with Homebrew's rustup (keg-only: add it to PATH, then restart your editor from a new shell)
- ✅ OK model — qwen3-coder:30b answers at http://localhost:11434/v1
```

Outside an Olifant project it stops after `olifant`, with a note to open a file in one.
`olifant doctor` is an ordinary command too
([chapter 2](02-running-generators.md#is-this-machine-ready-olifant-doctor)): run it in a
terminal for the same checks, against your shell's `PATH`.

### Neovim's `PATH` is not your shell's

Olifant runs the tools a generator needs (`ast-grep` always; `cargo`, `elm` or `cabal` for a
`complete` edit's checks) from the `PATH` of the process that started it. From Durandal,
that is Neovim's `PATH`, which is whatever Neovim was started with: a GUI Neovim, or one
started before you edited your shell profile, may not have what your terminal has. Homebrew's
`rustup` is the usual case: it is keg-only, so `cargo` is in `/opt/homebrew/opt/rustup/bin`
and on no `PATH` until you add it.

The symptom is a plan that cannot complete:

```
! rust-htmx/failure-arms: cannot complete cases: `cargo` is not installed (needed to check rust); run `olifant doctor`
```

A `complete` edit whose compiler is missing always ends that way. Run `olifant doctor` the
way Durandal does, with `:checkhealth durandal`, and you see Neovim's `PATH` in the error. Add
the directory to `PATH` in your shell profile, then restart Neovim from a new shell. (Or,
for a GUI Neovim, set `vim.env.PATH` in your config.)

### No `[model]`

Without a `[model]` in `.olifant/config.toml`, holes stay placeholders (`todo!(…)` in Rust,
`Debug.todo` in Elm): the plan applies and compiles, but you write that code yourself. Doctor
says so as a warning, with the snippet to add:

```
! model — no [model] configured: holes stay placeholders
  fix: add to .olifant/config.toml:
[model]
default = "local"
[model.backends.local]
kind = "openai-compatible"
url = "http://localhost:11434/v1"
model = "qwen3-coder:30b"
```

With one configured, doctor asks an `openai-compatible` server for its model list (3 seconds
at most) and warns if it doesn't answer or doesn't have the model; for `anthropic` it checks
only that the key's environment variable is set, in Neovim's environment when run from
`:checkhealth`. [Chapter 12](12-holes-and-models.md#configuring-a-model) has the details.

### Errors in your code

Durandal doesn't underline errors as you type; your language server does. For Rust that is
`rust-analyzer`, which runs `cargo check` itself, so it needs `cargo` on Neovim's `PATH`
too: if `rust-analyzer` shows no errors at all, the `PATH` above is the likely cause. Olifant
only reports a broken file when you ask for a plan ([above](#code-that-does-not-parse-in-a-plan)).
The diagnostics Durandal does show, on save, are the project's rules
([above](#rule-findings-as-diagnostics)), not compile errors.
