# 14. Olifant in your editor

Engineers meet Olifant through **Durandal**, the editor side of the product. Olifant is the
conventions engine underneath it. The first editor is Neovim, with the `durandal.nvim`
plugin. The plugin is a thin client of Olifant's language server, `olifant lsp`
([chapter 16](16-the-language-server.md)), which answers from the same engine as the CLI. This
chapter covers both halves: what the plugin does, and what Olifant gives an editor — as the
server answers it, and as the CLI prints it, for scripts, agents and probing by hand:

- **`[applies_at]`** in a `generator.toml`: where in the code a generator applies (inside
  a node, or `after` one), which of its inputs the code there already gives, (`where`) what
  it looks for, in words, (`suggest`) whether an editor may offer it unasked, and
  (`trigger`) what a snippet waits for you to type;
- **`olifant actions --at FILE:LINE:COL`**: the generators that apply at a cursor;
- **`olifant actions --file FILE`**: every place in a file where one applies, and the
  generators that could apply but found nothing yet;
- **snippets**: generators whose one edit goes at the cursor (`cursor = true`), planned with
  **`gen --at FILE:LINE:COL`** and offered by **`olifant complete`** to a completion menu
  ([below](#snippets));
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
| `<leader>ma` | `:DurandalActions` | generators that apply at the cursor (the server's code actions; `olifant actions` on the command line) |
| `<leader>mg` | `:DurandalGenerate` | any generator, inputs from a picker (`olifant/generators`, `olifant/choices`; `olifant show`) |
| | `:DurandalHints [toggle\|on\|off]` | the ⚡ hints ([below](#hints-on-the-cursor-line)); no argument toggles |
| `<Tab>` | | while a suggestion is shown, and only then: apply the ghost text, review the follow-up ([below](#ghost-text-and-follow-ups)), or expand the snippet ([below](#snippets)) |
| | `:DurandalGhosts [toggle\|on\|off]` | ghost text and follow-ups; no argument toggles |
| | `:checkhealth durandal` | Neovim, `nui.nvim`, `olifant`, the server, then the doctor's checks ([Troubleshooting](#troubleshooting)) |

In the preview, `j`/`k` move between files, `<Tab>` switches to the diff, `<CR>` applies and
`q`/`<Esc>` cancels. A plan with conflicts shows why, and cannot be applied. While a model is
filling holes, a spinner says so (that is `--progress`, [below](#progress-gen---progress)).

The plugin's README (in the `durandal.nvim` repository) has the install steps and a
three-beat demo on the rust-htmx fixture. It needs Neovim 0.11+, `nui.nvim`, and `olifant`
and `ast-grep` on your `PATH` — Neovim's `PATH`, which is not always your shell's
([Troubleshooting](#troubleshooting)).

Two things worth knowing:

- **Nothing is saved before Olifant plans.** The server holds every open buffer's text, so it
  plans against what you see, unsaved. (On the command line, `--content -` and
  `--overlay FILE` do the same for one file, [below](#unsaved-text---content--).)
- **The plugin logs** which actions it hinted, offered and suggested, and which you picked, applied
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
after a change once the buffer has been quiet for 300 ms (a change in normal or insert mode,
or leaving insert mode; insert mode counts so that a snippet can appear while you type,
[below](#snippets)). Moving the cursor only redraws. If a call fails, the last good hints
stay.

Snippets are never hinted. They show as ghost text after their trigger, or in the completion
menu, so the ⚡ hint is only ever for the other generators that apply on the line.

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
Tab apply · Esc back`. There `Tab` (or `<CR>`) applies, `Shift-Tab` switches between the file
list and the diff, and `Esc` or `q` closes the review and leaves the follow-up where it was, so
`Tab` opens it again. Nothing changes that you have not seen. While a suggestion is shown it
takes the ⚡ hint's place on its line.

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

rust-htmx's two generators that react to what you wrote are follow-ups. `wire-command`
changes `service.rs` and `main.rs` too. `failure-arms`'s arm goes into the `match` seven
lines below the variant (`⚡ Tab: review Add Failure Arms (+1 line here)`); were it on the
cursor's line or the next, it would be the ghost line

```
            RefundInvoiceFailure::AlreadyRefunded => ⟨status⟩,
```

The ghost text you will see most is a **snippet's** (spec §4.9): a small block at the cursor
for what you are about to write, not a consequence of what you wrote. Snippets are the next
section; they are suggested the same way, and drawn the same way, but `Tab` expands them with
their holes as tab stops ([below](#snippets)).

**When Olifant plans.** The server plans once typing pauses (300 ms, `debounce_ms`), on the
line you edited, when a `suggest` action applies there and nothing is `missing`: a generator
that would have to ask you something is only ever in `<leader>ma`. It plans without asking a
model (the CLI's `gen --plan --no-fill --overlay FILE`, [below](#from-the-command-line)) and
pushes the result (`olifant/suggestion`); when the cursor settles on another line, Durandal
asks for that line's (`olifant/suggest`). Newer edits supersede older work; a suggestion for
an older version of the buffer is dropped; a plan that fails, has a conflict or changes nothing
shows nothing. A follow-up's review is fetched with `olifant/plan`, and `Tab` applies it, its
holes filled by the model then.

When several apply on the line, **follow-ups come before snippets**: what you just wrote is
finished before what comes next is offered. The first one that offers something here is
shown. One that offers nothing (a plan with nothing to suggest, one you dismissed, a snippet
whose trigger does not match) gives way to the next.

**`Tab` is Durandal's only while a suggestion is shown**, as a buffer-local mapping in normal
and insert mode that is removed with the suggestion. A completion menu (the built-in one,
`blink.cmp` or `nvim-cmp`) clears the suggestion when it opens, and none is drawn while one is
open, so its `Tab` is never taken. Nor is anything drawn while a snippet is being filled in
(`vim.snippet.active()`): there `Tab` moves to the next tab stop.

**Accepting plans again, for real**: the same `gen` against the buffer as it is now, without
`--no-fill`, so the model fills the holes (with a spinner, `--progress`), and the plan's
`files` go in as buffer edits, one undo step per file, and are written, your buffer
included. If a file changed between that plan and applying it, nothing is applied:
`FILE changed since planning; nothing was applied — plan again`. A plan with a conflict
says `not applied:` and why. What lands has the model's fills where the suggestion showed
`⟨name⟩`; filling only on accept keeps the background plan fast and free.

**Dismissing.** `Esc` in normal mode on the line dismisses it (backing out of a review does
not), and it is not offered again for that generator, those inputs and that line's text: edit the line,
and it may come back. Moving the cursor off the line only clears it; come back and it is
planned again.

Each step goes to the action log as an event with the generator: `ghost-shown`,
`ghost-applied`, `ghost-dismissed`, `followup-shown`, `followup-reviewed`, `followup-applied`
and `followup-dismissed` (snippets have their own, [below](#snippets)). They are what will
say whether the split is right.

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
[{"choices":{},"generator":"rust-htmx/wire-command","label":"Wire cancel_invoice's dependencies","missing":[],"name":"Wire Command Dependencies","sets":{"command":"cancel_invoice","service":"billing"},"snippet":false,"suggest":true,"trigger":false},{"choices":{"port":["email_sender","invoice_repository"]},"generator":"rust-htmx/add-port","label":"A port as a field of a command (a snippet, spec §4.9); wiring it in follows up (wire-command)","missing":["port"],"name":"Add port","sets":{"command":"cancel_invoice","service":"billing"},"snippet":true,"suggest":false,"trigger":false}]
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
`cargo` checks the buffer's text to find the `match`. `actions` also lists
`rust-htmx/add-port` on that line: a snippet, offered in the menu only (`"suggest":false`),
so it is never planned in the background ([next](#snippets)).

### Snippets

A **snippet** is a generator whose one edit goes at the cursor (spec §4.9). It is still a
generator: typed inputs, `one_of` choices, queries over the code, holes, checks. What makes
it a snippet is that it writes the line you are on, and that its holes become **tab stops**
you fill in, or the model does. It is offered two ways:

- **Anticipated**: when the code and what you just typed point to it, it is ghost text on
  your line, and `Tab` takes it.
- **Chosen**: it is a ⚡ item in your completion menu (nvim-cmp) and in `<leader>ma`.

The packs in this repository have five:

| snippet | where | offered | its holes |
|---|---|---|---|
| `rust-htmx/failure-variant` | a blank line in a command's `Failure` enum | anticipated | `message` and `Variant`, yours |
| `python-htmx/failure-variant` | a blank line after a failure class ([below](#the-python-chain)) | anticipated | `Name`, yours |
| `rust-htmx/match-arm` | a blank line in a `match` the compiler says lacks a case | anticipated (the first missing case) and in the menu (one item per case) | `status`, the model's |
| `rust-htmx/add-port` | inside a command's struct | in the menu only, one item per port | none |
| `olifant-dev/spec-case` | just after typing `it "` in a `*Spec.hs` | anticipated | `behaviour`, yours; `body`, derived (`pending`) |

#### Anticipated: ghost text, then tab stops

In `refund_invoice.rs`, open a line below `Unknown(String),` (`o`, then `Esc`, which strips
the autoindent and leaves it empty). Durandal draws the snippet's first line over your line
and the rest below it, dim, each hole as `⟨name⟩`:

```
    #[error("⟨message⟩")]  ⚡ Tab: Failure variant
    ⟨Variant⟩,
```

`Tab` plans it for real, with the model for any model holes (a spinner while it fills), and
expands it with Neovim's `vim.snippet`: the line becomes

```
    #[error("message")]
    Variant,
```

with `message` selected. Type the message, `Tab` to `Variant`, type the name, `Tab` once more
to leave the snippet at its end. Expanding changes only the buffer: nothing is saved, and the
emptied line and the expansion are one undo step, so one `u` takes it all back. If you edited
the buffer while it was planning, nothing is expanded: `not expanded: the buffer changed
while planning`.

The snippet does only the line you are on. What it implies elsewhere is a **follow-up**:
with the cursor on the variant you just named, `AlreadyRefunded,`, `failure-arms` offers
`⚡ Tab: review Add Failure Arms (+1 line here)`, as [above](#ghost-text-and-follow-ups).
That is the whole convention, in two `Tab`s and a review: the variant at the cursor, its arm
in every `match` reviewed.

`match-arm` works the same way on a blank line after a `match`'s last arm. Its ghost is the
arm for the first case the compiler says is missing, with the value as a model hole:

```
            RefundInvoiceFailure::AlreadyRefunded => ⟨status⟩,  ⚡ Tab: Next match arm
```

`Tab` asks the model, and the expansion has its fill as the tab stop's text,
`RefundInvoiceFailure::AlreadyRefunded => 409,` with `409` selected: keep it with `Tab`, or
type over it.

**When it is drawn.** Durandal plans a snippet as it does a follow-up (once the cursor has
settled, against the unsaved buffer, `--no-fill`), after one more question: before planning,
it asks the engine whether the snippet applies at the cursor itself (`olifant actions --at`
with the buffer). That is where the snippet's **trigger** is checked: the text before the
cursor must end with it ([below](#writing-a-snippet)). `failure-variant`'s trigger is
"nothing but whitespace", so it waits for a blank line; `spec-case`'s is `it "`. The same
answer lists the choices of anything still `missing`, and Durandal takes the first:
`match-arm`'s `pattern` is the first case the compiler says the `match` lacks. A snippet with
a missing input that has no choices offers nothing, and gives way to the next candidate. A
ghost is at most 8 lines, like other ghost text.

#### Chosen: the completion menu, and `<leader>ma`

The snippets are the server's completion items, so any LSP completion lists them: nvim-cmp
with `cmp-nvim-lsp`, `blink.cmp`, or Neovim's own:

```lua
sources = { { name = "nvim_lsp" } },
```

Its items are the snippets that apply at the cursor (what `olifant complete` prints,
[below](#olifant-complete)), with the Snippet kind and the generator's `description` as the
detail. A snippet missing one choice is one item per choice. On a blank
line inside `pub struct CancelInvoice { … }`, the menu has

```
Add port EmailSender
Add port InvoiceRepository
```

(The trigger does not matter here: you opened the menu, so you are asking.) Picking one
resolves it (`completionItem/resolve`: the model fills model holes, under progress) and
expands it, as `Tab` does for a ghost: the line becomes
`    pub email_sender: Arc<dyn EmailSender>,`. With the cursor on that field, `wire-command`'s
follow-up appears, `⚡ Tab: review Wire Command Dependencies (+2 lines here, +7 in service.rs,
main.rs)`: the import, `new()`'s parameter and `main.rs`'s construction, reviewed.

`<leader>ma` lists snippets too, after the other actions on the line. Picking one asks for
what is missing as usual (for `add-port`, the port), then expands the snippet at the cursor
instead of opening the preview.

**Events.** Snippets go to the action log as `snippet-shown`, `snippet-applied` and
`snippet-dismissed` (a ghost shown, taken with `Tab`, dismissed with `Esc`),
`snippet-picked` (expanded from `<leader>ma`) and `snippet-chosen` (from the completion
menu). Whether an anticipated snippet is taken or dismissed is what will say if its trigger
is precise enough (spec §4.9).

#### From the command line

Every step is a command. In the rust-htmx sandbox, with the blank line typed below
`Unknown(String),` and not saved:

```
$ F=billing/billing-service/src/refund_invoice.rs
$ buffer() { sed 's/^    Unknown(String),$/    Unknown(String),\n/' $F; }
$ buffer | olifant actions --at $F:27:1 --content - --json
[{"choices":{},"generator":"rust-htmx/failure-variant","label":"Failure variant","missing":[],"name":"Failure variant","sets":{},"snippet":true,"suggest":true,"trigger":true}]
$ buffer | olifant gen rust-htmx/failure-variant --at $F:27:1 --plan --json --no-fill --overlay $F | jq -c .snippet
{"body":"    #[error(\"${1:message}\")]\n    ${2:Variant},$0","file":"billing/billing-service/src/refund_invoice.rs","ghost":["    #[error(\"⟨message⟩\")]","    ⟨Variant⟩,"],"line":27}
```

The first is the gate, the second the ghost. `ghost` is what Durandal draws and `body` what
`vim.snippet` expands, both indented as placed ([below](#the-plans-snippet)). The
plan took 0.07 seconds. `Tab` runs the same `gen` without `--no-fill` and with `--progress`.
For `match-arm`, `buffer` now prints the file with an `AlreadyRefunded` variant added to the
enum, and a blank line 35 after the last arm of `status()`'s `match`, which has no arm for
it yet:

```
$ buffer | olifant actions --at $F:35:1 --content - --json
[{"choices":{"pattern":["RefundInvoiceFailure::AlreadyRefunded"]},"generator":"rust-htmx/match-arm","label":"An arm for a case the compiler says this match lacks; its value is a hole the model fills (a snippet, spec §4.9)","missing":["pattern"],"name":"Next match arm","sets":{},"snippet":true,"suggest":true,"trigger":true}]
$ buffer | olifant gen rust-htmx/match-arm --set pattern=RefundInvoiceFailure::AlreadyRefunded --at $F:35:1 --plan --json --overlay $F | jq -c .snippet
{"body":"            RefundInvoiceFailure::AlreadyRefunded => ${1:409},$0","file":"billing/billing-service/src/refund_invoice.rs","ghost":["            RefundInvoiceFailure::AlreadyRefunded => 409,"],"line":35}
```

The `actions` call took 1.4 seconds, most of it `cargo` saying which cases are missing; the
filled plan took 2.4, with `qwen3-coder:30b`.

### When nothing applies

`<leader>ma` where nothing applies says why, from the same `--file` answer:

- **Something applies elsewhere in the file.** A menu titled "Nothing at the cursor. In this
  file:" lists each place, as `⚡ Add Unknown to every match — line 22`. Picking one moves
  the cursor there and goes on as if you had asked there.
- **Nothing applies anywhere in the file, but a generator could.** Each generator whose path
  matched says what it is waiting for, from its `where`
  ([below](#near-misses-where-and-nearby)), one line each:

  ```
  rust-htmx/failure-arms applies on a variant of the PayInvoiceFailure enum — none yet
  rust-htmx/wire-command applies on a field of the PayInvoice struct — none yet
  ```

  (In rust-htmx today a new, saved `pay_invoice.rs` with only its struct gets the first kind
  of answer instead: the `add-port` snippet applies on the struct's empty `{}`, line 2.)

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
([chapter 15](15-conventions-that-hold.md)). Both halves come from the server.

### Regions, as buffer edits

As a buffer saves, the server refreshes the project's owned regions (what `olifant sync --plan`
prints, [chapter 5](05-owned-regions.md#--plan-the-refresh-for-editors)):

- the saved file's region edits go into the same write (`textDocument/willSaveWaitUntil`);
- other files' come as `workspace/applyEdit`, which Durandal applies as ordinary buffer edits
  (each an undo step) and saves, with a notice.

Save a new `billing/billing-service/src/void_payment.rs` and `lib.rs` gains its line:

```
synced billing/billing-service/src/lib.rs: modules +1
```

The refresh covers the whole project, not only the file you saved. A file with unsaved changes
gets the edit and keeps its other changes unsaved; save it when you are ready. A refresh with a
conflict (a broken fence) is not applied; `olifant sync --check` in a terminal says why.

### Rule findings as diagnostics

The project's rules are the server's diagnostics (pulled, `textDocument/diagnostic`), there as
you type, unsaved text included: source `olifant`, the rule's id as the code.

- The diagnostic has the rule's severity: `hint`, `warn` or `error`. With `use
  diesel::prelude::*;` in a command, that is a warning under the line: "Commands talk to
  ports, not the database (rust-htmx: ports & adapters)".
- A finding in the baseline ([chapter 15](15-conventions-that-hold.md#a-new-rule-the-ratchet))
  is always a hint, its message ending "(existing)": known debt, not news.
- A finding about a whole file (a `missing` rule) is on line 1.

Add `// olifant:allow commands-use-ports — spike` to the line and the diagnostic goes.

### A finding's fix, first in `<leader>ma`

When the cursor is on a finding whose rule has a `fix`, `<leader>ma` lists the fix first
(a `quickfix` code action, preferred), before the actions that apply there:

```
Use an existing port: InvoiceRepository
Add a new repository port
```

Picking one asks for any choice still missing, then shows the plan, with the line the rule
flagged rewritten by the model through the port where one is configured.

### Turning them off

The diagnostics are ordinary LSP diagnostics: Neovim's settings apply to them as to any other
(`vim.diagnostic.config`, or hide one client's with its namespace,
`vim.lsp.diagnostic.get_namespace(require("durandal.lsp").client().id)`). Regions follow the
project's `regions.toml`: a project with none has nothing to refresh.

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
Failure variant  (rust-htmx/failure-variant)
```

```
$ olifant actions --at billing/billing-service/src/refund_invoice.rs:28:5 --json
[{"choices":{},"generator":"rust-htmx/failure-arms","label":"Add AlreadyRefunded to every match","missing":[],"name":"Add Failure Arms","sets":{"command":"refund_invoice","failure":"AlreadyRefunded","service":"billing"},"snippet":false,"suggest":true,"trigger":false},{"choices":{},"generator":"rust-htmx/failure-variant","label":"Failure variant","missing":[],"name":"Failure variant","sets":{},"snippet":true,"suggest":true,"trigger":true}]
```

`service` and `command` came from the file's path, and `failure` from the variant under the
cursor. Nothing is `missing`, so Durandal goes straight to the plan, which is the `gen`
command those `sets` spell. (`failure-variant`, a snippet, is listed after it: column 5 is
the start of the line's text, so the text before the cursor is only whitespace and its
trigger matches. In Durandal the follow-up comes first, [above](#ghost-text-and-follow-ups).)

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
[{"choices":{},"generator":"rust-htmx/wire-command","label":"Wire refund_invoice's dependencies","missing":[],"name":"Wire Command Dependencies","sets":{"command":"refund_invoice","service":"billing"},"snippet":false,"suggest":true,"trigger":false},{"choices":{"port":["email_sender","invoice_repository"]},"generator":"rust-htmx/add-port","label":"A port as a field of a command (a snippet, spec §4.9); wiring it in follows up (wire-command)","missing":["port"],"name":"Add port","sets":{"command":"refund_invoice","service":"billing"},"snippet":true,"suggest":false,"trigger":false}]
```

(with the `add-port` snippet after it), and in the `match` (line 34), only the `match-arm`
snippet, with the case the compiler says is missing as its one choice:

```
$ olifant actions --at billing/billing-service/src/refund_invoice.rs:34:5 --json
[{"choices":{"pattern":["RefundInvoiceFailure::AlreadyRefunded"]},"generator":"rust-htmx/match-arm","label":"An arm for a case the compiler says this match lacks; its value is a hole the model fills (a snippet, spec §4.9)","missing":["pattern"],"name":"Next match arm","sets":{},"snippet":true,"suggest":true,"trigger":true}]
```

Line 34 is the existing arm, `RefundInvoiceFailure::Unknown(_) => 500,`; at column 5 the text
before the cursor is whitespace, so the trigger matches here too. Taken, the snippet would
replace that line, not add one ([Rough edges](#rough-edges-of-snippets)).

## The Python chain

The same convention, in Python: a new failure for a command, then an arm for it in every
`match` on the command's failures. In the python-htmx pack it is two generators, the
`failure-variant` snippet ([below](#after-a-node-after)) and `failure-arms`, its follow-up,
and Durandal chains them with no change of its own. Make a sandbox; `try-pack.sh` runs `uv
sync` in it, so pyright and pytest are ready
([chapter 10](10-testing-generators.md#trying-a-pack-by-hand-scriptstry-packsh)):

```sh
scripts/try-pack.sh python-htmx /tmp/olifant-py && cd /tmp/olifant-py
```

and give it a `[model]` ([chapter 12](12-holes-and-models.md#configuring-a-model)). The
fixture is a restaurant's table bookings. `src/bistro/booking/commands/create_booking.py` is a
command: its input, its failures as frozen dataclasses with a `reason`, and their union,
one member per line:

```python
@dataclass(frozen=True)
class TableNotFound:
    reason: str


type CreateBookingFailure = (
    SlotTaken
    | PartyTooLarge
    | TableNotFound
)
```

and `src/bistro/web/bookings/failures.py` maps each failure to an HTTP status with a `match` that
pyright, in strict mode, proves exhaustive. It names each failure through the command's
module (`case create_booking.SlotTaken():`), so a new failure needs no import there.

**In Neovim.** Open `create_booking.py`, put the cursor on `TableNotFound`'s `reason: str`,
and press `o`, then `Esc`. The ghost is the new class, PEP 8-spaced: two blank lines (the
first is your line, with `⚡ Tab: Failure variant` at its end), then

```
@dataclass(frozen=True)
class ⟨Name⟩:
    reason: str
```

`Tab` expands it with `Name` selected. Type `Cancelled` and `Tab` once more to leave the
snippet. The cursor is still in the class, so the follow-up appears:

```
⚡ Tab: review Add Failure Arms (+1 line here, +2 in failures.py)
```

`Tab` opens the review: `| Cancelled` at the end of the union, and in `failures.py` the new
arm, its value a model hole. `Tab` again applies it, the model fills the value, and pyright
checks the fill with the rest:

```diff
         case create_booking.TableNotFound():
             return 404
+        case create_booking.Cancelled():
+            return 503
```

`uv run pyright` reports `0 errors, 0 warnings, 0 informations`, and `uv run pytest -q`
`13 passed`. As in Rust, the whole convention is a snippet, two `Tab`s and a review.

**From the command line.** Each step is a command, as in Rust. With the blank line typed
below `reason: str` (line 31) and not saved:

```
$ F=src/bistro/booking/commands/create_booking.py
$ buffer() { sed '30a\
' $F; }
$ buffer | olifant actions --at $F:31:1 --content - --json
[{"choices":{},"generator":"python-htmx/failure-variant","label":"Failure variant","missing":[],"name":"Failure variant","sets":{},"snippet":true,"suggest":true,"trigger":true}]
$ buffer | olifant gen python-htmx/failure-variant --at $F:31:1 --plan --json --no-fill --overlay $F | jq -c .snippet
{"body":"\n\n@dataclass(frozen=True)\nclass ${1:Name}:\n    reason: str$0","file":"src/bistro/booking/commands/create_booking.py","ghost":["","","@dataclass(frozen=True)","class ⟨Name⟩:","    reason: str"],"line":31}
```

Each took 0.06 seconds. With `Cancelled` named (the class on lines 33 to 35 of the buffer),
the follow-up's background plan:

```
$ buffer | olifant actions --at $F:34:7 --content - --json
[{"choices":{},"generator":"python-htmx/failure-arms","label":"Add Cancelled to every match","missing":[],"name":"Add Failure Arms","sets":{"command":"create_booking","failure":"Cancelled","service":"booking"},"snippet":false,"suggest":true,"trigger":false}]
$ buffer | olifant gen python-htmx/failure-arms --set service=booking --set command=create_booking --set failure=Cancelled --plan --human --no-fill --overlay $F
Plan: python-htmx/failure-arms (2 edits, 0 conflicts)
  ~ src/bistro/booking/commands/create_booking.py (+1 line at line 42)
  ~ src/bistro/web/bookings/failures.py (+2 lines at line 19, compiler: create_booking_status)
holes:
  value (expr) src/bistro/web/bookings/failures.py:19  skipped: --no-fill

--- src/bistro/booking/commands/create_booking.py
| Cancelled

--- src/bistro/web/bookings/failures.py
case create_booking.Cancelled():
    return getattr(NotImplemented, "olifant: value: the value this match returns for create_booking.Cancelled(), following the other arms")
```

(`buffer` here prints the file with the named class in it.) The plan took 1.7 seconds, most
of it pyright finding the `match`. Saved and applied with the model:

```
$ olifant gen python-htmx/failure-arms --set service=booking --set command=create_booking --set failure=Cancelled --progress --human
{"event":"check","language":"python"}
{"event":"check","language":"python"}
{"attempt":1,"event":"fill","holes":["value"],"model":"qwen3-coder:30b"}
{"event":"check","language":"python"}
{"attempt":1,"event":"filled","hole":"value"}
python-htmx/failure-arms (2 edits, 0 conflicts)
  ~ src/bistro/booking/commands/create_booking.py (+1 line at line 42)
  ~ src/bistro/web/bookings/failures.py (+2 lines at line 19, compiler: create_booking_status)
holes:
  value (expr) src/bistro/web/bookings/failures.py:19  filled by qwen3-coder:30b: 503
```

in 2.8 seconds. Run again, it changes nothing: the union already names `Cancelled` (its
`unless`), and pyright finds no `match` without it:

```
$ olifant gen python-htmx/failure-arms --set service=booking --set command=create_booking --set failure=Cancelled --plan --human
Plan: python-htmx/failure-arms (1 edit, 0 conflicts)
  = src/bistro/booking/commands/create_booking.py (skipped: unless: line 42 of src/bistro/booking/commands/create_booking.py matches)
```

`failure-arms` differs from rust-htmx's in two ways. It has an `insert` as well as the
`complete` edit, since a Python union is written by hand apart from its classes, and you
wrote only the class. And its hole is `value`, not `status`, with the instruction "the value
this match returns for … following the other arms": a second `match` on the union might map
failures to messages, not statuses. Its `[applies_at]`:

```toml
# packs/python-htmx/generators/failure-arms/generator.toml (excerpt)
# In an editor (olifant actions): on a failure class in a command module.
[applies_at]
path  = "src/{app}/{service}/commands/{command}.py"
node  = { kind = "class_definition", inside = { kind = "decorated_definition", regex = "frozen=True" }, not = { has = { field = "name", regex = "Input$" } } }
bind  = { failure = "^class ([A-Za-z0-9_]+)" }
where = "on a failure class of the {= command|pascal =} command"
suggest = true
label = "Add {= failure =} to every match"
```

The node is a frozen dataclass that is not the command's `…Input`. The path is a command
module (`commands/NAME.py`), and `command` is a `one_of` query over the service's
`commands/` less its tests, so `web/bookings/failures.py` is not a command and offers
nothing.

`durandal.nvim` runs this chain end to end against a real sandbox in
`tests/e2e_python_spec.lua`: the ghost, the name, the follow-up, the model's fill, then
`uv run pyright` and `uv run pytest`. It is skipped unless `DURANDAL_E2E_PY` names the
sandbox: `DURANDAL_E2E_PY=/tmp/olifant-py make test`.

## The python-htmx snippets

Beside `failure-variant` and `failure-arms`, the pack anticipates the next piece wherever
its conventions make it predictable. Each is a ghost on a blank line (Tab to take it, Tab
to walk its stops) and in the ⚡ menu.

- **`test-case`**, on a blank line after a test in a `*__test.py`:

  ```python
  def test_⟨behaviour⟩() -> None:
      create_booking = command()
      assert False, "write this test"
  ```

  The arrange line is the file's first test's first statement, so a new test starts
  where the others do. The last line is a tab stop that starts red: TDD's first step is
  already taken.
- **`match-arm`**, on a blank line after a `match`'s last case: the case pyright says is
  missing, written as the other arms write theirs (`case cancel_booking.AlreadyCancelled():`,
  qualified through the command's module), its body a hole the model fills.
- **`port-method`**, on a blank line in a port's Protocol: `def ⟨name⟩(self⟨, params⟩) ->
  ⟨None⟩: ...`, valid as typed.
- **`port-method-adapters`**, the follow-up on a Protocol's method: each adapter beside the
  Protocol gets the method, its body a hole the model fills (with the Protocol's imports),
  and the contract test a red `test_NAME`.
- **`add-dependency`** ("Add a port"), chosen from the ⚡ menu on a command's or query's
  `__init__`: pick one of the service's ports and it joins the parameters, inline while the
  line fits 88 columns, else one per line; then `wire` (below) runs as part of it. It is
  also `commands-use-ports`' first fix, "Use an existing port".
- **`handle-body`** ("Write handle with the model (until its tests pass)"), chosen from the
  ⚡ menu on a `handle` whose body is still `raise NotImplementedError(…)`, never offered
  unasked. The model writes the body, seeing the module's tests, the service's models and
  ports; the fill stands only when pyright is clean and `NAME__test.py` passes (`verify`;
  editors show "Testing"). With no `__test.py` it refuses: "write NAME__test.py first". On
  the slice, qwen3-coder:30b wrote `create_booking`, `cancel_booking` and
  `get_day_schedule` green in one round each, 3–6 s.
- **`wire`**, the follow-up on a parameter of a command's or query's `__init__` (type
  `bookings: BookingRepository` and Tab): the module keeps the port, every test that builds
  the command passes its fake, and `main.py`, when it builds the command, passes the
  adapter.

A model's fill may start with import lines, which join the file's imports
([chapter 12](12-holes-and-models.md#what-is-sent-to-the-model)): a decode that needs
`date` brings `from datetime import date`.

The end-to-end test in durandal.nvim (`tests/e2e_python_spec.lua`) does this from nothing:
`olifant init`, a command and its red stub, the test-case ghost on the line below the stub
(arranged as `create_booking = command()`), the new test typed, both red, `handle`
written, then five green and pyright clean.

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
| `trigger` | no | a POSIX extended regex the text before the cursor must end with: what you type to anticipate a snippet (`'^[[:space:]]*it "'`). Only `actions --at` checks it; `--file` and `olifant complete` ignore it ([below](#writing-a-snippet)). `actions` reports whether there is one as `"trigger"` |
| `after` | no | instead of `node`: an ast-grep rule for a node the cursor's line **follows**, with only blank lines between. It is where "the next one" goes: a blank line below the last failure class, which only the module contains ([below](#after-a-node-after)). `bind` reads the node it follows |

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
  [{"choices":{"command":["cancel_invoice","refund_invoice","void_invoice"]},"generator":"rust-htmx/wire-command","label":"Wire a command's dependencies after you add fields to its struct by hand (spec §7.1: reconcile)","missing":["command"],"name":"Wire Command Dependencies","sets":{"service":"billing"},"snippet":false,"suggest":true,"trigger":false},{"choices":{"port":["email_sender","invoice_repository"]},"generator":"rust-htmx/add-port","label":"A port as a field of a command (a snippet, spec §4.9); wiring it in follows up (wire-command)","missing":["port"],"name":"Add port","sets":{"command":"lib","service":"billing"},"snippet":true,"suggest":false,"trigger":false}]
  ```

  (`command=lib` was dropped from `wire-command`, so its label fell back to the description,
  and `choices` lists the commands to ask from.) Other input types are not checked here;
  `gen` checks them as usual. `add-port`'s `command` is an `ident`, not an `enum`, so it
  keeps `lib`.

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

In the Olifant repository, on `data CatalogError` (`src/Olifant/Catalog.hs`, line 94):

```
$ olifant actions --at src/Olifant/Catalog.hs:94:3 --human
Add a constructor to CatalogError (GHC finds every case)  (olifant-dev/error-case)  asks: name
$ olifant actions --at src/Olifant/Catalog.hs:94:3 --json
[{"choices":{},"generator":"olifant-dev/error-case","label":"Add a constructor to CatalogError (GHC finds every case)","missing":["name"],"name":"Add Error Case","sets":{"path":"src/Olifant/Catalog.hs","type":"CatalogError"},"snippet":false,"suggest":false,"trigger":false}]
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
| a Perl class in a `trigger` | `` `trigger`: `\s+$` is not a valid regular expression (POSIX extended: use [[:space:]] and [a-z_0-9], not \s or \w) `` |
| both `node` and `after` | `` `applies_at` takes `after` or `node`, not both (`node`: the cursor is inside one; `after`: it follows one) `` |
| an unknown key | `` unknown key(s) `colour`; expected one of `path`, `node`, `after`, `bind`, `file`, `label`, `where`, `suggest`, `trigger` `` |

(Each is prefixed with the file and `` `applies_at`: ``.)

## Writing a snippet

A snippet is an ordinary generator with three differences: its one edit is a **cursor
edit**, its holes may say **where their value comes from**, and its `[applies_at]` may have a
**trigger**. Here is `failure-variant`, whole:

```toml
# packs/rust-htmx/generators/failure-variant/generator.toml
name = "Failure variant"
description = "A new variant for a command's Failure enum: its #[error] message and its name, as tab stops (a snippet, spec §4.9)"
language = "rust"
version = 1
examples = [
  "olifant gen rust-htmx/failure-variant --at billing/billing-service/src/refund_invoice.rs:27:1 --plan",
]

# At the cursor; once the variant is named, failure-arms follows up with an
# arm for it in every match (spec §4.8).
[[edits]]
cursor = true
text = "#[error(\"{= ? message: text | what went wrong, for a person =}\")]\n{= ? Variant: ident | the variant, PascalCase =},"

[holes.message]
source = "yours"

[holes.Variant]
source = "yours"

# Anticipated on a blank line inside a command's Failure enum.
[applies_at]
path    = "*/*-service/src/*.rs"
node    = { kind = "enum_variant_list", inside = { kind = "enum_item", regex = "Failure", stopBy = "end" } }
trigger = "^[[:space:]]*$"
where   = "on a blank line in a command's Failure enum"
suggest = true
label   = "Failure variant"
```

### The cursor edit: `cursor = true`

```toml
[[edits]]
cursor = true
text = "…"            # or: template = "…"
```

The text goes where the editor's cursor is, given as `gen --at FILE:LINE:COL`
([chapter 2](02-running-generators.md#a-snippet-at-the-cursor---at)). It **replaces the
cursor's line**: an editor sends the line as it is (blank, or holding the trigger you typed),
and the snippet takes its place. That makes it a replacement like a rule's fix, so it is
drift-checked: the line must still read what it read when the plan was made.

Every line is **indented to fit**. A line with anything on it keeps its own leading
whitespace. An empty line, which is what `Esc` leaves after autoindent, takes the indentation
of the nearest non-blank line above it, one level deeper when that line opens a block (ends
with `{`, `(`, `[` or `=`, or is or ends with the word `where` or `do`). One level is the
language adapter's `indent` ([chapter 9](09-packs-and-language-adapters.md#the-keys)): four
spaces by default, two in `olifant-dev`'s Haskell adapter. So on the blank line below
`    Unknown(String),` the variant is indented four spaces, and on the blank line inside
`pub struct CancelInvoice {` it is one level in.

The rules:

- **A cursor edit is the generator's only edit.** A generator with a cursor edit and any other
  edit fails to load: `a cursor edit (a snippet) must be the generator's only edit: what it
  implies elsewhere is a separate generator, offered as a follow-up`. The snippet is the part
  at the cursor; the distant consequences are another generator, with `suggest = true`, which
  applies where the snippet leaves the cursor (`failure-variant` → `failure-arms`, `add-port`
  → `wire-command`).
- **`cursor = false` is an error**, not a no-op: `` `cursor = true` makes the edit a snippet at
  the editor's cursor; leave `cursor` out otherwise ``.
- **A snippet needs a cursor.** `gen` without `--at` is a usage error, exit 2:
  `` `rust-htmx/failure-variant` is a snippet: it needs --at FILE:LINE:COL ``. `olifant show`
  lists the edit as `at the cursor (a snippet: gen --at FILE:LINE:COL)`, and `show --json` as
  `{"kind":"cursor"}`.

### Holes with a source: `[holes.NAME]`

A snippet's holes are its tab stops. Each one's value comes from somewhere, and
`[holes.NAME]` says where:

| `source` | the tab stop starts as | filled by |
|---|---|---|
| `"model"` (the default) | the model's fill, or `⟨name⟩` until there is one | the model, when the snippet is expanded ([chapter 12](12-holes-and-models.md)) |
| `"yours"` | the hole's name (`message`) | you |
| `"derived"`, with `value = "TEMPLATE"` | the template, rendered (`pending`) | you, if you want something else |

`failure-variant`'s two holes are yours: only you know the message and the name.
`spec-case`'s body is derived, so the case starts as `pending`, and its behaviour is yours:

```toml
# packs/olifant-dev/generators/spec-case/generator.toml (excerpt)
[[edits]]
cursor = true
text = "it \"{= ? behaviour: text | what it does, in words =}\" $ do\n  {= ? body: expr | the test =}"

[holes.behaviour]
source = "yours"

[holes.body]
source = "derived"
value  = "pending"
```

`match-arm`'s `status` has no `[holes.status]`, so it is the model's: the value of an arm is a
judgement, and the model makes it with the other arms in view.

A yours or derived hole is a tab stop and nothing else. The model is never asked about it,
and `gen` leaves its name (yours) or its value (derived) in the text, in the plan's files and
on disk: `#[error("message")]` / `Variant,`. Its kind still has to be one the file's adapter
declares, since that is how a hole is written down; the packs here add `ident` and `text` to
Rust and `text` to Haskell for tab stops ([chapter 12](12-holes-and-models.md#kinds-and-placeholders)).

Mistakes are load errors (prefixed with the file and `` `holes.NAME`: ``, except the first):

| mistake | error |
|---|---|
| a `[holes.NAME]` for a hole no template has | `[holes.mesage]: no hole called mesage in this generator's templates` |
| `source = "choice"` | `` `choice` holes are not built yet: make it an input with one_of = "QUERY" (editors list one item per choice) `` |
| another source | `` unknown source `mine`: a hole's source is yours, derived or model `` |
| `derived` without `value` | ``missing `value` `` |

There are no `choice` holes yet: a choice is an `enum` input whose choices come from a query
(`one_of`). An editor's menu has one item per choice, and an anticipated snippet takes the
first, as `match-arm` and `add-port` do.

### The trigger

```toml
[applies_at]
trigger = '^[[:space:]]*it "'
```

`trigger` is a POSIX extended regex the text before the cursor must end with; it is
anchored at the end for you (a trigger already ending in `$` is used as is). `actions --at`
checks it, so it decides when a snippet is **anticipated**: `failure-variant`'s
`^[[:space:]]*$` waits for a line with nothing before the cursor but whitespace, and
`spec-case`'s waits for `it "` typed at the start of a line. `actions --file` (the hints) and
`olifant complete` (the menu) ignore it: in the menu you are asking.

A trigger narrows `node`, it doesn't replace it: `failure-variant` also has a `node`, so a blank
line anywhere but in a `Failure` enum offers nothing. `spec-case` has none, so its `path`
(`**/*Spec.hs`) and its trigger decide. `add-port` has no trigger and no `suggest`: it is
only ever chosen. Anticipation should be precise, not frequent (spec §4.9); when in doubt,
leave a snippet in the menu.

### After a node: `after`

`trigger` narrows *where in a node* a snippet waits. Some snippets wait *after* a node
instead: the next failure class goes on a blank line below the last one, and at a blank
top-level line in Python, the only node containing the cursor is the module. So
`[applies_at]` takes `after`, an ast-grep rule like `node`, in its place. Here is
`python-htmx/failure-variant`, whole:

```toml
# packs/python-htmx/generators/failure-variant/generator.toml
name = "Failure variant"
description = "A new failure for a command: a frozen dataclass with a reason, its name a tab stop (a snippet, spec §4.9); failure-arms follows up"
language = "python"
version = 1
examples = [
  "olifant gen python-htmx/failure-variant --at src/bistro/booking/commands/create_booking.py:33:1 --plan",
]

# At the cursor, PEP 8-spaced from the class above; naming it raises the
# failure-arms follow-up (the union and every match).
[[edits]]
cursor = true
text = "\n\n@dataclass(frozen=True)\nclass {= ? Name: ident | the failure, PascalCase =}:\n    reason: str"

[holes.Name]
source = "yours"

# Anticipated on a blank line right after a failure (a frozen dataclass with
# a reason) in a service's module.
[applies_at]
path    = "src/bistro/*/*.py"
after   = { kind = "decorated_definition", regex = "reason: str" }
trigger = "^[[:space:]]*$"
where   = "on a blank line after a failure class"
suggest = true
label   = "Failure variant"
```

- **The cursor's line follows the node** when it is below the node's last line and every
  line between them is blank. So the line right after a failure class counts, and so does
  the one after that, but not the class's own lines, and not a line past the `type` that
  follows the classes. When several nodes qualify, the nearest one above is the one
  followed (`bind` reads it).
- **It is indented like the node it follows**, not like the line above the cursor: the line
  above is the class's last field, `    reason: str`, four spaces deeper than the class.
- **The snippet starts with two blank lines**, which PEP 8 asks for between top-level
  definitions. `o` on `reason: str`, then `Esc`, leaves one blank line plus the two the file
  already had below the class, so the new class ends up with exactly two above and two
  below.
- **`after` and `node` don't mix**: one says the cursor is inside such a node, the other
  that it follows one. A generator with both fails to load.

`after` is not Python's: any language's snippet, or any generator, can use it. In the
python-htmx sandbox (`scripts/try-pack.sh python-htmx`), each failure class is followed by
the `failure-variant` action:

```
$ olifant actions --file src/bistro/booking/commands/create_booking.py --human
21:1  Add SlotTaken to every match  (python-htmx/failure-arms)
23:1  Failure variant  (python-htmx/failure-variant)
26:1  Add PartyTooLarge to every match  (python-htmx/failure-arms)
28:1  Failure variant  (python-htmx/failure-variant)
31:1  Add TableNotFound to every match  (python-htmx/failure-arms)
33:1  Failure variant  (python-htmx/failure-variant)
45:24  Wire bookings (BookingRepository)  (python-htmx/wire)
45:53  Wire ids (IdGenerator)  (python-htmx/wire)
```

Each `Failure variant` range runs over the two blank lines below its class (`{"start":[33,1],
"end":[34,1]}` for the last). The `Wire` lines are `wire` on each `__init__` parameter
([above](#the-python-htmx-snippets)). In a file with no failure class, such as
`web/bookings/failures.py`, it
is a near miss: `nearby: python-htmx/failure-variant — on a blank line after a failure
class`.

### What the compiler says is missing: `uncovered` queries

`match-arm` offers an arm for each case the `match` at the cursor lacks. It asks the compiler
which ones:

```toml
# packs/rust-htmx/generators/match-arm/generator.toml (excerpt)
[inputs.pattern]
type   = "enum"
one_of = "uncovered"
prompt = "The case to add an arm for"

[query.uncovered]
uncovered = "Failure::"
value     = "name"

[[edits]]
cursor = true
text = "{= pattern =} => {= ? status: expr | the value this match returns for {= pattern =}, following the other arms =},"
```

An `uncovered` query runs the file's language check (the adapter's `[check]`, over the
buffer when there is one, holes as placeholders) and reads the missing-case errors. Of those
in the cursor's file, it takes the latest one that starts at or above the cursor's line, so
a blank line after a `match`'s last arm still counts as inside it. Its records are the
patterns that match the regex, each as `name`, with a leading `&` (or `&mut `) left out, since
rustc reports a `match` on a reference with `&RefundInvoiceFailure::AlreadyRefunded` and the
arm wants `RefundInvoiceFailure::AlreadyRefunded`. The regex is a template, like the other
query strings.

It needs a cursor, so it only runs where there is one: `actions --at`, `olifant complete`,
`gen --at`, and a test case with `at` ([below](#testing-a-snippet)). Everywhere else (`actions
--file`, `show`, a `gen` without `--at`) it finds nothing. So the hints know `match-arm`
applies in every `match`, and only the cursor's `actions --at` knows the cases. An
`uncovered` query takes no other key (in a generator, `value` as well), and a file whose
adapter has no `[check]` is a query failure (`the … adapter has no [check]: the compiler
cannot say which cases are missing`).

### The plan's `snippet`

`gen --at … --plan --json` on a snippet has a `snippet` object, which is all an editor needs to
show and expand it:

| field | what it is |
|---|---|
| `file`, `line` | the file and the 1-based line the snippet replaces |
| `ghost` | the lines to draw, each hole as `⟨name⟩`, a derived hole as its value, a filled model hole as its fill |
| `body` | the same lines in LSP snippet syntax: each hole a tab stop `${N:…}`, numbered in reading order and starting as its name, value or fill, then `$0` at the end. `\`, `$` and `}` in the text are escaped |

Both keep the indentation the snippet was placed with, so an editor empties the line and
expands the body at column 0. From `spec-case` in an `olifant-dev` sandbox
(`scripts/try-pack.sh olifant-dev`), with `  it "` typed as line 8 of the sample spec and
not saved:

```
$ F=src/Olifant/SampleUnitSpec.hs
$ buffer() { cat $F; printf '  it "\n'; }
$ buffer | olifant actions --at $F:8:7 --content - --json
[{"choices":{},"generator":"olifant-dev/spec-case","label":"Spec case","missing":[],"name":"Spec case","sets":{},"snippet":true,"suggest":true,"trigger":true}]
$ buffer | olifant gen olifant-dev/spec-case --at $F:8:7 --plan --json --overlay $F | jq -c .snippet
{"body":"  it \"${1:behaviour}\" \\$ do\n    ${2:pending}$0","file":"src/Olifant/SampleUnitSpec.hs","ghost":["  it \"⟨behaviour⟩\" $ do","    pending"],"line":8}
```

(The `$` of `$ do` is escaped as `\$`. Column 7 is just after the `"`; at column 6 the
trigger doesn't match and `actions` says `[]`. The plan also warns that the file does not
parse, `src/Olifant/SampleUnitSpec.hs:3 does not parse (near `import Test.Hspec`)`: the `"`
you just typed opened a string that nothing closes yet. The warning is harmless here.)

Each hole in `holes` has a `source` too, and the tab stops are `skipped`:

```
$ buffer | olifant gen olifant-dev/spec-case --at $F:8:7 --plan --json --overlay $F | jq -c '[.holes[] | {name, kind, source, status, why}]'
[{"name":"behaviour","kind":"text","source":"yours","status":"skipped","why":"a tab stop (yours)"},{"name":"body","kind":"expr","source":"derived","status":"skipped","why":"a tab stop (derived)"}]
```

In the human form the source follows the kind: `behaviour (text@yours)`.

### Testing a snippet

A snippet's test case says where the cursor is, with `at` in `inputs.toml`:

```toml
# packs/rust-htmx/generators/match-arm/tests/uncovered-variant/inputs.toml
now = "2026-10-04T12:00:00Z"
# The cursor: a blank line after the match's last arm. The compiler reports
# AlreadyRefunded as not covered there.
at = "billing/billing-service/src/refund_invoice.rs:35:1"

[inputs]
pattern = "RefundInvoiceFailure::AlreadyRefunded"
```

The case's `given/` has the cursor's state: the blank line, and the variant with no arm. The
rest is as for any case ([chapter 10](10-testing-generators.md#a-snippet-case-at)), with two
exceptions: a snippet case is not re-run, and not compiled on its own.

### Rough edges of snippets

- **The trigger sees only what is before the cursor.** `^[[:space:]]*$` matches at the
  start of a line's text as well as on a blank line, and the snippet replaces the whole line.
  `gen rust-htmx/failure-variant --at …refund_invoice.rs:26:5` replaces `Unknown(String),`
  with the new variant, and `match-arm` at the start of an existing arm replaces the arm.
  Durandal offers follow-ups first, so the variant line's `failure-arms` usually wins, but a
  dismissed follow-up lets the snippet through.
- **`add-port` offers ports the struct already has.** After `email_sender` is added, the menu
  still lists `⚡ Add port EmailSender`.
- **A `match` with nothing missing still lists `match-arm`** in `actions --at`, with
  `"choices":{"pattern":[]}`. Durandal skips it; `olifant complete` lists nothing.
- **`gen --plan` shows a model hole's placeholder in a snippet's edit text**, though the
  `snippet`, `files` and `holes` have the fill: `--- …` / `… => todo!("olifant: status: …"),`
  above `holes: status … filled by qwen3-coder:30b: 409`.
- **`show` lists an `uncovered` enum as `<enum()>`**: its choices exist only at a cursor.

## `olifant actions`

```
olifant actions --at FILE:LINE:COL [--content -]
olifant actions --file FILE [--content -]
```

`--at` asks about one position, `--file` about the whole file
([below](#every-place-in-a-file---file)); exactly one of them is required (`Missing: (--at
FILE:LINE:COL | --file FILE)`, exit 2).

- **`FILE`** is relative to where you run `olifant` (or absolute); it is matched as a
  project-relative path, so `cd src && olifant actions --at Olifant/Catalog.hs:94:3` gives the
  same answer as above.
- **`LINE` and `COL` are 1-based**, as editors show them. `COL` counts UTF-8 bytes, as
  Neovim's cursor and LSP's `utf-8` position encoding do: on `"Café" x`, `x` is column 9, not
  8. A malformed position is a usage error (exit 2): `option --at: expected FILE:LINE:COL
  (1-based)`.
- For `--at`, the JSON is a list, one object per generator that applies, in catalog order
  with snippets last: `generator` (its id), `name` (its display name), `label`, `sets` (the
  inputs bound, as `--set` would take them), `missing` (the required inputs, with no default,
  that are still unset), `choices` (for each missing `enum` input, its values, which is what
  an editor offers; `{}` when there are none), `suggest` (its `[applies_at] suggest`, `false`
  when not set), `snippet` (whether its edit is a cursor edit, [above](#writing-a-snippet))
  and `trigger` (whether its `[applies_at]` has a `trigger`).
- A snippet with a `trigger` is listed only when the text before the cursor ends with it.
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
{"actions":[{"generator":"rust-htmx/add-port","label":"A port as a field of a command (a snippet, spec §4.9); wiring it in follows up (wire-command)","missing":["port"],"name":"Add port","range":{"end":[10,2],"start":[8,26]},"sets":{"command":"refund_invoice","service":"billing"},"snippet":true,"suggest":false,"trigger":false},{"generator":"rust-htmx/wire-command","label":"Wire refund_invoice's dependencies","missing":[],"name":"Wire Command Dependencies","range":{"end":[9,55],"start":[9,5]},"sets":{"command":"refund_invoice","service":"billing"},"snippet":false,"suggest":true,"trigger":false},{"generator":"rust-htmx/failure-variant","label":"Failure variant","missing":[],"name":"Failure variant","range":{"end":[29,2],"start":[24,31]},"sets":{},"snippet":true,"suggest":true,"trigger":true},{"generator":"rust-htmx/failure-arms","label":"Add Unknown to every match","missing":[],"name":"Add Failure Arms","range":{"end":[26,20],"start":[26,5]},"sets":{"command":"refund_invoice","failure":"Unknown","service":"billing"},"snippet":false,"suggest":true,"trigger":false},{"generator":"rust-htmx/failure-arms","label":"Add AlreadyRefunded to every match","missing":[],"name":"Add Failure Arms","range":{"end":[28,20],"start":[28,5]},"sets":{"command":"refund_invoice","failure":"AlreadyRefunded","service":"billing"},"snippet":false,"suggest":true,"trigger":false},{"generator":"rust-htmx/match-arm","label":"An arm for a case the compiler says this match lacks; its value is a hole the model fills (a snippet, spec §4.9)","missing":["pattern"],"name":"Next match arm","range":{"end":[35,10],"start":[33,20]},"sets":{},"snippet":true,"suggest":true,"trigger":true}],"nearby":[]}
```

and in the Olifant repository, where `olifant-dev/error-case` applies on each `data`
declaration:

```
$ olifant actions --file src/Olifant/Catalog.hs --human
53:1  Add a constructor to CatalogEntry (GHC finds every case)  (olifant-dev/error-case)  asks: name
64:1  Add a constructor to Catalog (GHC finds every case)  (olifant-dev/error-case)  asks: name
88:1  Add a constructor to CatalogPaths (GHC finds every case)  (olifant-dev/error-case)  asks: name
94:1  Add a constructor to CatalogError (GHC finds every case)  (olifant-dev/error-case)  asks: name
```

- The JSON is an object with two lists, `actions` and `nearby` (next section).
- **`actions`** has one entry per node a generator's `node` rule matches, each with that
  node's `bind` values, in file order: `generator`, `name`, `label`, `range`, `sets`,
  `missing`, `suggest`, `snippet` and `trigger`, as for `--at` (but no `choices`, and no
  trigger is checked: a snippet's range is its whole `node`, such as the `Failure` enum's
  body, lines 24 to 29 above). `range` is `{"start": [LINE, COL], "end": [LINE, COL]}`, with 1-based lines and
  UTF-8 byte columns as `--at` takes them; `end` is just past the node (`Unknown(String)` on
  line 26 runs from column 5 to 20, the `,` after it).
- **An `after` rule's action** has one entry per node it matches too, but its `range` is
  where the cursor would follow the node: the blank lines after it, from the first to the
  last, each at column 1, which is where `--at` applies ([above](#after-a-node-after)).
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
6:26  A port as a field of a command (a snippet, spec §4.9); wiring it in follows up (wire-command)  (rust-htmx/add-port)  asks: port
20:31  Failure variant  (rust-htmx/failure-variant)
22:5  Add Unknown to every match  (rust-htmx/failure-arms)
nearby: rust-htmx/match-arm — on a blank line inside a match
nearby: rust-htmx/wire-command — on a field of the CancelInvoice struct
$ olifant actions --file billing/billing-service/src/cancel_invoice.rs --json
{"actions":[{"generator":"rust-htmx/add-port","label":"A port as a field of a command (a snippet, spec §4.9); wiring it in follows up (wire-command)","missing":["port"],"name":"Add port","range":{"end":[6,28],"start":[6,26]},"sets":{"command":"cancel_invoice","service":"billing"},"snippet":true,"suggest":false,"trigger":false},{"generator":"rust-htmx/failure-variant","label":"Failure variant","missing":[],"name":"Failure variant","range":{"end":[23,2],"start":[20,31]},"sets":{},"snippet":true,"suggest":true,"trigger":true},{"generator":"rust-htmx/failure-arms","label":"Add Unknown to every match","missing":[],"name":"Add Failure Arms","range":{"end":[22,20],"start":[22,5]},"sets":{"command":"cancel_invoice","failure":"Unknown","service":"billing"},"snippet":false,"suggest":true,"trigger":false}],"nearby":[{"generator":"rust-htmx/match-arm","label":"An arm for a case the compiler says this match lacks; its value is a hole the model fills (a snippet, spec §4.9)","name":"Next match arm","why":"on a blank line inside a match"},{"generator":"rust-htmx/wire-command","label":"Wire cancel_invoice's dependencies","name":"Wire Command Dependencies","why":"on a field of the CancelInvoice struct"}]}
```

(`cancel_invoice.rs` has no `status()`, so `match-arm` has no `match` to apply in. The
`add-port` snippet's node is the struct's empty body, `{}`.)

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
falls back to the description, as `failure-arms`'s does in a new file, saved with only its
struct:

```
$ printf '#[derive(Clone)]\npub struct PayInvoice {}\n' > billing/billing-service/src/pay_invoice.rs
$ olifant actions --file billing/billing-service/src/pay_invoice.rs --json
{"actions":[{"generator":"rust-htmx/add-port","label":"A port as a field of a command (a snippet, spec §4.9); wiring it in follows up (wire-command)","missing":["port"],"name":"Add port","range":{"end":[2,25],"start":[2,23]},"sets":{"command":"pay_invoice","service":"billing"},"snippet":true,"suggest":false,"trigger":false}],"nearby":[{"generator":"rust-htmx/failure-arms","label":"Add an arm for a Failure variant you just wrote to every match on its enum (spec §5.6); the arm's value is a hole","name":"Add Failure Arms","why":"on a variant of the PayInvoiceFailure enum"},{"generator":"rust-htmx/failure-variant","label":"Failure variant","name":"Failure variant","why":"on a blank line in a command's Failure enum"},{"generator":"rust-htmx/match-arm","label":"An arm for a case the compiler says this match lacks; its value is a hole the model fills (a snippet, spec §4.9)","name":"Next match arm","why":"on a blank line inside a match"},{"generator":"rust-htmx/wire-command","label":"Wire pay_invoice's dependencies","name":"Wire Command Dependencies","why":"on a field of the PayInvoice struct"}]}
```

A generator with no `node` is never in `nearby`: it applies whenever its path matches. Nor is
one whose path slots are not real choices: sent the same text with `--content -` before it
is saved, `pay_invoice` is not yet one of the service's commands, so `failure-arms` and
`wire-command` are left out and only the snippets with no inputs are near misses. (The
`add-port` snippet applies on the struct's empty body; its `command` is an `ident`, which is
not checked.)

## `olifant complete`

```
olifant complete --at FILE:LINE:COL [--content -]
```

The snippets for a completion menu at a position: what the server's completion lists
([above](#chosen-the-completion-menu-and-leaderma)). It is `actions --at` narrowed to
snippets, with two differences: a `trigger` is not checked (you opened the menu, so you are
asking), and a snippet missing exactly one input that has choices becomes one item per
choice, with that input set. A snippet missing anything else is left out, since a menu item
can't ask. On the blank line inside `CancelInvoice`:

```
$ F=billing/billing-service/src/cancel_invoice.rs
$ buffer() { sed 's/^pub struct CancelInvoice {}/pub struct CancelInvoice {\n\n}/' $F; }
$ buffer | olifant complete --at $F:7:1 --content - --human
⚡ Add port EmailSender  (rust-htmx/add-port)
⚡ Add port InvoiceRepository  (rust-htmx/add-port)
$ buffer | olifant complete --at $F:7:1 --content - --json
[{"detail":"A port as a field of a command (a snippet, spec §4.9); wiring it in follows up (wire-command)","generator":"rust-htmx/add-port","label":"Add port EmailSender","name":"Add port","sets":{"command":"cancel_invoice","port":"email_sender","service":"billing"}},{"detail":"A port as a field of a command (a snippet, spec §4.9); wiring it in follows up (wire-command)","generator":"rust-htmx/add-port","label":"Add port InvoiceRepository","name":"Add port","sets":{"command":"cancel_invoice","port":"invoice_repository","service":"billing"}}]
```

- The JSON is a list, one object per item: `generator`, `name`, `label` (the `[applies_at]
  label` rendered with the item's `sets`, so it can name the choice), `sets` (everything to
  pass as `--set`) and `detail` (the generator's `description`). The human form is one line
  per item, `⚡ LABEL  (GENERATOR)`.
- Each item is a `gen GENERATOR --set … --at FILE:LINE:COL` away from its plan.
- `--at` and `--content -` are as for `actions`; `--at` is required (`Missing: --at
  FILE:LINE:COL`, exit 2). Nothing applies: `[]`, exit 0.

With a `match` that lacks a case, `match-arm` is one item per case the compiler reports
(`⚡ RefundInvoiceFailure::AlreadyRefunded arm  (rust-htmx/match-arm)`), and none when
nothing is missing. In the rust-htmx sandbox each call takes well under a second; one with
an `uncovered` query also runs the language check, so `match-arm`'s took 0.5 seconds.

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
- A snippet's plan (`gen --at`) also has a `snippet` object, `{file, line, ghost, body}`
  ([above](#the-plans-snippet)). Durandal expands `body` instead of applying `files`: a
  snippet's `files` have its tab stops written as their names (`#[error("message")]`), which
  is what a plain `gen --at` writes to disk.

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
[{"choices":{},"generator":"msg-branch","label":"The update branch for a Msg you added by hand: ( model, Cmd.none )","missing":["msg"],"name":"Msg Branch","sets":{"page":"Home"},"snippet":false,"suggest":false,"trigger":false}]
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
[{"choices":{},"generator":"msg-branch","label":"Add the update branch for Reset","missing":[],"name":"Msg Branch","sets":{"msg":"Reset","page":"Home"},"snippet":false,"suggest":false,"trigger":false}]
$ olifant actions --at src/Page/Home.elm:24:7 --json
[{"choices":{},"generator":"msg-branch","label":"Add the update branch for NoOp","missing":[],"name":"Msg Branch","sets":{"msg":"NoOp","page":"Home"},"snippet":false,"suggest":false,"trigger":false}]
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
[{"choices":{},"generator":"msg-branch","label":"Add the update branch for Increment","missing":[],"name":"Msg Branch","sets":{"msg":"Increment","page":"Home"},"snippet":false,"suggest":true,"trigger":false}]
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
- **Make a snippet's trigger precise.** An anticipated snippet appears while you type, so it
  should appear only where it is almost always wanted: a `node` and a `trigger` together,
  as `failure-variant` has. Probe it with `actions --at` on the line you mean, on the line
  after you typed something else, and at the start of a line that already has code. When in
  doubt, leave out `suggest` and let the snippet wait in the menu, as `add-port` does.
- **Write `where` as the place.** "on a field of the CancelInvoice struct" tells someone
  where to put the cursor; "a `field_declaration` inside a `struct_item` matching /…/" tells
  them how the rule works. Check it with `olifant actions --file` on a file that has no such
  node yet.

## Troubleshooting

### `:checkhealth durandal` and `olifant doctor`

When an action does nothing useful, start with `:checkhealth durandal`. It checks Neovim
(0.11+), `nui.nvim` and that `olifant` is on Neovim's `PATH`, whether `olifant lsp` is running
and which projects it holds, then asks it for the doctor's checks (`olifant.doctor`, what
`olifant doctor --json` prints) for the project of the current buffer:

```
durandal ~
- ✅ OK Neovim 0.12.5
- ✅ OK nui.nvim
- ✅ OK olifant: /Users/you/.local/bin/olifant

durandal: olifant lsp ~
- ✅ OK olifant lsp: running
- ✅ OK project /tmp/demo (loaded 1×)

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
that is the server's, which Neovim started: Neovim's `PATH`, which is whatever Neovim was started with: a GUI Neovim, or one
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
