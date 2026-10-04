# 5. Owned regions and `olifant sync`

Some code is a list of everything of a kind: a cabal file's module list, a Rust `lib.rs`
full of `pub mod` lines, a page registry. Inserting into such lists works, but they drift:
delete a module by hand, rename one, or revert one commit out of ten, and the list is stale.
And when two branches (or two agents) each append to the end of the same list, git reports
a merge conflict.

An **owned region** is a block Olifant rewrites *in full* from what is on disk, every time.
Olifant's own `olifant.cabal` works this way:

```cabal
  exposed-modules:
    -- olifant:region library-modules (generated from glob src/**/*.hs; edit the declaration, not this block)
    Olifant.Anchor
    Olifant.Anchor.Phrase
    …
    -- olifant:end library-modules
```

The list is exactly the modules under `src/`, sorted. Add a file and it appears; delete one
and it disappears. Because the list is sorted and regenerated, two branches that each add a
module produce the same lines in the same places, and merge cleanly.

## Try it

```sh
scripts/try-pack.sh olifant-dev /tmp/olifant-dev && cd /tmp/olifant-dev
olifant gen olifant-dev/unit --set unit=workspace_sync
```

```
olifant-dev/unit (4 edits, 0 conflicts)
  + src/Olifant/WorkspaceSync.hs (2 lines)  [olifant-dev/unit]
  + src/Olifant/WorkspaceSyncSpec.hs (7 lines)  [olifant-dev/unit]
  ~ olifant.cabal (region library-modules: +1 −0)  [region library-modules]
  ~ olifant.cabal (region test-modules: +2 −0)  [region test-modules]
```

The generator only created the two files. The module lists were refreshed at the end of the
same run: every `gen` refreshes every region, in the same all-or-nothing plan.

## Declaring a region

Regions are declared by a pack (`regions.toml` at the pack root) or a project
(`.olifant/regions.toml`), not by a generator, because the list must be right however files
change. From `packs/olifant-dev/regions.toml`:

```toml
[regions.library-modules]
file        = "olifant.cabal"
comment     = "--"
query       = { glob = "src/**/*.hs", exclude = ["**/*Spec.hs", "src/TestDriver.hs"] }
item        = "{= dotted =}"
init_at     = "after first line matching ^  exposed-modules:"
init_indent = "    "
```

- `query` says what to list (chapter 6 describes queries and their fields).
- `item` is rendered once per result; lines are sorted and deduplicated.
- `comment` is the file's comment syntax for the fences (optional when the file's language
  adapter knows it).
- `init_at` and `init_indent` say where `olifant sync --init` puts the fences the first time.

A project region with the same name replaces a pack's.

## Families: one region per matching file

`olifant.cabal` is one file. A Rust service's `lib.rs` is one per service, and every one of
them is the same kind of list. A region whose `file` is a pattern (it has a `*`, `?` or `[`)
is a **family**: one region for every file in the project that matches, each listing what
is next to *that* file. rust-htmx declares two, in `packs/rust-htmx/regions.toml`:

```toml
[regions.modules]
file    = "*/*-service/src/lib.rs"
comment = "//"
query   = { glob = "{= dir =}/*.rs", exclude = ["{= dir =}/lib.rs"] }
item    = "pub mod {= stem =};"
init_at = "start of file"

[regions.ports]
file    = "*/*-service/src/ports.rs"
comment = "//"
query   = { glob = "{= dir =}/ports/*.rs" }
item    = "pub mod {= stem =};"
init_at = "start of file"
```

In the query, `{= dir =}` is the matched file's directory and `{= file =}` its path, so
`billing/billing-service/src/lib.rs` lists `billing/billing-service/src/*.rs` and
`notifications/notifications-service/src/lib.rs` lists its own. (Glob and AST queries take
them; a command query does not.) Each file's opening fence names its own glob:

```rust
// olifant:region modules (generated from glob billing/billing-service/src/*.rs; edit the declaration, not this block)
pub mod cancel_invoice;
pub mod models;
pub mod ports;
pub mod refund_invoice;
pub mod service;
pub mod void_invoice;
// olifant:end modules
```

So in a rust-htmx project, a command or port file is a module as soon as it exists. Create
one by hand and `sync` adds its line (in a rust-htmx sandbox:
`scripts/try-pack.sh rust-htmx /tmp/rh && cd /tmp/rh`):

```
$ printf 'pub fn hello() {}\n' > billing/billing-service/src/pay_invoice.rs
$ olifant sync --check
1 region out of date in billing/billing-service/src/lib.rs: modules (+1 −0); nothing written, run `olifant sync`
$ olifant sync
sync (1 edit, 0 conflicts)
  ~ billing/billing-service/src/lib.rs (region modules: +1 −0)
```

Delete the file, and the next `sync` takes the line out again (`region modules: +0 −1`).
The generators no longer write `pub mod` lines themselves: `service-command` creates the
command's file, `service-repository` the port's, and the refresh at the end of the same
`gen` lists them:

```
$ olifant gen rust-htmx/service-command --set service=billing --set command_name=void_payment
rust-htmx/service-command (6 edits, 0 conflicts)
  + billing/billing-service/src/void_payment.rs (36 lines)  [rust-htmx/service-command]
  ~ billing/billing-service/src/service.rs (+1 line at line 8)  [rust-htmx/service-command]
  …
  ~ billing/billing-service/src/lib.rs (region modules: +1 −0)  [region modules]
$ olifant gen rust-htmx/service-repository --set service=billing --set aggregate=payment
rust-htmx/service-repository (10 edits, 0 conflicts)
  + billing/billing-service/src/ports/payment_repository.rs (19 lines)  [rust-htmx/service-repository]
  …
  ~ billing/billing-service/src/ports.rs (region ports: +1 −0)  [region ports]
```

`service-skeleton` creates a new service's `lib.rs` and `ports.rs` with empty fences, and
the same run fills them.

A matching file without fences is not set up, as for any region: `gen` skips it,
`sync --check` reports it, and nothing rewrites it until you run `sync --init`
([below](#--init-for-a-family)).

For a file Olifant generates entirely (say, an Elm `Pages.elm` registry), use
`whole_file = true` and `template = "pages.elm.dt"` instead of `item`: the template gets
the records as `items`, and the file starts with a "generated by olifant" header instead of
fences. The header is how Olifant knows the file is its own: `olifant sync --init NAME`
creates the file, and a file of that name *without* the header is never overwritten (it is
a conflict: move it aside first).

## Commands

```
$ olifant sync --init library-modules     # create the fences once, at init_at, and fill them
$ olifant sync                            # refresh every region now (after a hand edit, rename, revert…)
$ olifant sync --check                    # write nothing; exit 5 if anything is stale
$ olifant sync --plan                     # write nothing; show the refresh as a plan (for editors)
```

### `--init` for a family

`sync --init NAME` on a family sets up every matching file that has no fences yet, and
fills them all in one run. With the fences taken out of both services' `lib.rs`:

```
$ olifant sync --check
  ! region `modules` has no fences in billing/billing-service/src/lib.rs (run `olifant sync --init modules`)
  ! region `modules` has no fences in notifications/notifications-service/src/lib.rs (run `olifant sync --init modules`)
$ olifant sync --init modules
sync (2 edits, 0 conflicts)
  ~ billing/billing-service/src/lib.rs (region modules: +6 −0)
  ~ notifications/notifications-service/src/lib.rs (region modules: +4 −0)
```

Files that already have fences are left alone; run it again and it is `sync (0 edits, 0
conflicts)`. `--init` adds the fences at `init_at` and does not touch what was there before,
so the hand-written `pub mod` lines are still there, below the region, and every module is
now declared twice. Delete them: from now on the region is the list.

### `--plan`: the refresh, for editors

`sync --plan` computes the same refresh as `sync` and writes nothing. It prints a plan, as
`gen --plan` does, with each region's new text:

```
$ olifant sync --plan
Plan: sync (1 edit, 0 conflicts)
  ~ billing/billing-service/src/lib.rs (region modules: +1 −0)

--- billing/billing-service/src/lib.rs
// olifant:region modules (generated from glob billing/billing-service/src/*.rs; edit the declaration, not this block)
pub mod cancel_invoice;
pub mod models;
pub mod pay_invoice;
…
// olifant:end modules
```

In JSON it is a `gen` plan with `"generator": "sync"`. Each edit has `status` `"region"`,
with `region`, `added` and `removed`, and `files` has each file's whole text `before` and
`after`, which an editor applies as buffer edits
([chapter 14](14-editors-and-actions.md#conventions-on-save)):

```
$ olifant sync --plan --json | jq -c '.edits[0] | {path, status, region, added, removed}'
{"path":"billing/billing-service/src/lib.rs","status":"region","region":"modules","added":1,"removed":0}
```

Nothing to refresh is `Plan: sync (0 edits, 0 conflicts)` (`"edits": []`, `"files": []`),
exit 0. A broken region is a conflict, exit 5, as for `sync`.

`sync --check` is a linter for hooks and CI:

```
$ olifant sync --check
regions up to date

$ olifant sync --check                    # after someone hand-edited a list
2 regions out of date in olifant.cabal: library-modules (+1 −1), test-modules (+2 −1); nothing written, run `olifant sync`
```

`olifant check` ([chapter 15](15-conventions-that-hold.md)) checks the regions the same way,
together with the project's rules, and exits 1 instead of 5. This repository's pre-merge
hook runs it, so a drifted module list cannot reach `main`.

## Rules

- **Hand edits inside a region are overwritten.** That is the contract, and the fence says
  so. Edit the declaration (or the files it lists), not the block.
- **Not set up here? Skipped.** A pack's region whose file or fences are missing in this
  project is silently skipped by `gen`; `sync --check` reports it ("run `olifant sync
  --init NAME`").
- **A broken `regions.toml` is loud.** `sync` and `sync --check` fail naming it, and every
  `gen` carries a warning that regions are not being refreshed.
- **Regions stay inside the project.** A region whose `file` points outside it is a
  conflict.
- **Broken fences stop everything.** An opening fence without its end, a duplicate, or an
  end before its start is a conflict: nothing is written until it is fixed.
- Region lines take the opening fence's indentation, and the file keeps its line endings.
- **A family is set up file by file.** One service's `lib.rs` without fences is skipped (and
  reported by `sync --check`) while the others are refreshed.
- **In the editor, on save.** Durandal runs `sync --plan` after every save and applies it as
  buffer edits, with a notice ([chapter 14](14-editors-and-actions.md#conventions-on-save)).
