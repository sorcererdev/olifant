# 9. Packs and language adapters

A project's own generators live in `.olifant/generators/`. When a convention is worth sharing
between projects, it goes in a **pack**: a directory Olifant is told about in config.

## What is in a pack

```
packs/elm/
  generators/
    page/
      generator.toml
      tests/counter/{inputs.toml, snapshot/…}
    msg/
      generator.toml
      tests/…
  fixtures/app/          the small app the generators are tested against (and copy from)
  languages/
    elm.toml             the Elm language adapter
    grammar/elm/         the tree-sitter grammar it needs, vendored as C source
  regions.toml           owned regions (optional; olifant-dev and rust-htmx have one)
  rules.toml             rules: conventions as checks (optional; rust-htmx has one)
  SKILL.md               instructions for AI agents (optional; Olifant itself never reads it)
```

| part | read by | covered in |
|---|---|---|
| `generators/<name>/generator.toml` | every command; the id is `<pack>/<name>` | [chapter 7](07-writing-a-generator.md) |
| `generators/<name>/templates/` | `template = "…"` edits | chapter 7 |
| `generators/<name>/tests/<case>/` | the test suite | [chapter 10](10-testing-generators.md) |
| `fixtures/app/` | `exemplar = "fixture:…"`, `fragment = { file = "fixture:…" }`, tests, `scripts/try-pack.sh` | chapters 7 and 10 |
| `languages/<name>.toml` | `at` phrases, `complete`, `olifant anchors`/`where`, region comments | below |
| `regions.toml` | every `gen`, `sync` and `check` | [chapter 5](05-owned-regions.md) |
| `rules.toml` | `check` (and Durandal, on save) | [chapter 15](15-conventions-that-hold.md) |
| `SKILL.md` | agent harnesses | [chapter 11](11-olifant-and-agents.md) |

Only directories exactly one level under `generators/` with a `generator.toml` count; the same
is true of `.olifant/generators/` in a project.

The packs in this repository are `elm`, `rails`, `redwood`, `rust-htmx`, `olifant-dev` (the
one Olifant uses on its own source) and `python-htmx` ([below](#a-fourth-example-python)).

## Configuring packs

A pack is used by a project when a config file names it:

```toml
# <project>/.olifant/config.toml   (paths relative to the project)
[packs]
elm = { path = "../olifant/packs/elm" }
```

```toml
# ~/.config/olifant/config.toml    (every project; paths relative to that directory)
[packs]
elm = { path = "~/projects/olifant/packs/elm" }
```

- Both files are read; when both name the same pack, **the project's wins**.
- `~/` at the start of a path is your home directory, in either file.
- `$XDG_CONFIG_HOME/olifant/config.toml` replaces `~/.config/olifant/config.toml` when that
  variable is set.
- `path` is the only key. Anything else is an error that names the file:

```
error: …/.olifant/config.toml: `packs`: `elm`: unknown key(s) `ref`; expected one of `path`
```

Asking for a generator from a pack that is not configured says how to fix it (exit 3, code
`pack_not_configured` in JSON):

```
$ olifant gen elm/page --set page=x
error: no generator named `elm/page`: no pack named `elm` is configured (add it under [packs] in .olifant/config.toml for this project, or ~/.config/olifant/config.toml for every project)
hint: elm = { path = "/path/to/elm" }
```

### The project root

Olifant walks up from wherever it runs to the nearest directory with a `.olifant/`, and treats
that as the project. Run it from a subdirectory and it still finds the project's config and
generators:

```
$ cd src/Page && olifant list
elm/msg   A new Msg for an Elm page: the variant plus an update branch returning ( model, Cmd.none )
elm/page  A page in the Elm architecture (Model/Msg/init/update/view), wired into Main and Route like elm-spa-example
```

The search stops below your home directory, so it never wanders into `~/.config/olifant/`'s
territory by accident. With no `.olifant/` anywhere above it, Olifant uses the directory it
ran from, same as always.

File arguments (`where FILE`, `--steps FILE`) stay relative to where you ran Olifant, not to
the project root it found — `olifant where ../lib.rs "end of mods"` from a subdirectory reads
that path relative to the subdirectory.

A pack path that does not exist is an error (exit 1, code `config_invalid`), naming the pack
and the path:

```
$ olifant list
error: pack `elm`: /private/tmp/claude-501/nowhere is not a directory (check its path under [packs])
```

## Names

| you type | resolves to |
|---|---|
| `elm/page` | the pack `elm`'s `page` |
| `section` | the project's `.olifant/generators/section`, if there is one |
| `page` | otherwise, the one pack generator named `page`, if exactly one pack has one (`olifant gen page …` runs `elm/page`); if several do, exit 2, "use the full pack/name" |

Inside a pack's generator, `[[uses]] generator = "msg"` looks in the same pack first, so packs
can refer to their own generators without knowing what they will be called in someone's
config.

## Language adapters

A language adapter teaches Olifant a language's structure: which files it covers, how to find
"the end of `type Msg`", and how to ask the compiler about missing cases. It is data, in
`languages/<name>.toml`; the file's name is the language's name.

```
$ olifant anchors rust
rust: at = "<start of|end of|before|after> <target>"

targets:
  call NAME in FUNCTION     e.g. end of call BillingService::new in main
  crate imports             e.g. end of crate imports
  impl NAME                 e.g. end of impl BillingService
  literal in FUNCTION       e.g. end of literal in new
  literal NAME in FUNCTION  e.g. end of literal RefundInvoice in new
  parameters in FUNCTION    e.g. end of parameters in new
  struct NAME               e.g. end of struct BillingService
  uses                      e.g. end of uses

built in (any file):
  before|after marker TEXT
  before|after first line matching REGEX
  before|after last line matching REGEX
  start of|end of file
```

Three of these exist to wire a command's dependencies (spec §7.1, chapter 8): `parameters in
FUNCTION` is a function's parameter list; `literal NAME in FUNCTION` is the struct literal
named `NAME` inside `FUNCTION` (`SendEmail` inside `new`, as opposed to `literal in FUNCTION`,
which is whichever one literal sits there); `call NAME in FUNCTION` is the arguments of a call
to `NAME` inside `FUNCTION` (`BillingService::new` inside `main`). All three can target an
empty `()`/`{}` — see "Opening an empty container" in [chapter 3](03-anchors.md).

`uses` is a file's top-level `use` declarations (container `source_file`, items
`use_declaration`). `end of uses` goes after the last one, whole, so a new import lands after
a multi-line `use crate::{ … };` block and never inside it. A line pattern cannot see the
block: `after last line matching ^use ` lands just after `use crate::{`, inside it:

```
$ olifant where billing/billing-service/src/service.rs "after last line matching ^use "
billing/billing-service/src/service.rs:5
    use crate::ports::invoice_repository::InvoiceRepository;
    
    use crate::{
  ▶ (inserted here)
        refund_invoice::{RefundInvoice, RefundInvoiceInput, RefundInvoiceOutput},
        void_invoice::{VoidInvoice, VoidInvoiceInput, VoidInvoiceOutput},
        cancel_invoice::{CancelInvoice, CancelInvoiceInput, CancelInvoiceOutput},

$ olifant where billing/billing-service/src/service.rs "end of uses"
billing/billing-service/src/service.rs:9
        void_invoice::{VoidInvoice, VoidInvoiceInput, VoidInvoiceOutput},
        cancel_invoice::{CancelInvoice, CancelInvoiceInput, CancelInvoiceOutput},
    };
  ▶ (inserted here)
    
    #[derive(Clone)]
    pub struct BillingService {
```

A file is not a bracketed container, so one with no `use` at all is not opened; the phrase
misses (`end of uses` in `billing-service/src/lib.rs`, which only has `pub mod`s: "has no
items to insert beside"). Give `start of file` as the fallback, as
`command-dependency` does: `at = ["end of uses", "start of file"]`.

Adapters come only from configured packs. In a project that only configures `rust-htmx`,
`olifant anchors elm` says "no language adapter named `elm`", and `.elm` files get only the
built-in line and marker targets. A project cannot declare an adapter of its own; put it in
a pack (a pack can be nothing but a `languages/` directory).

### The keys

From `packs/rust-htmx/languages/rust.toml` and `packs/elm/languages/elm.toml`:

```toml
files       = ["**/*.rs"]                         # which files this adapter covers
comment     = "//"                                # line comment, for region fences
grammar     = "rust"                              # a grammar ast-grep has built in…
# grammar   = { source = "grammar/elm" }          # …or C sources vendored in the pack
placeholder = 'todo!("{= message|rust_string =}")'  # the body of a todo branch
branch      = "{= pattern =} => {= body =},"      # how a case branch is written
indent      = "    "                              # one level of indentation, for opening an empty container (shown: the default)

[check]                                           # optional: enables `complete` edits
command = ["elm", "make", "{= files =}", "--report=json", "--output=/dev/null"]
context = ["elm.json"]
format  = "elm-json"

[holes.expr]                                      # optional: a kind of typed hole (chapter 12)
placeholder = 'todo!("olifant: {= hole =}: {= instruction|rust_string =}")'
wrap        = "fn __olifant() { let _ = {= fill =}; }"

[targets.struct]
phrase    = "struct NAME"
container = { kind = "field_declaration_list", inside = { kind = "struct_item", has = { field = "name", regex = "^{= NAME =}$" } } }
items     = { kind = "field_declaration" }
example   = "end of struct BillingService"
```

| key | what it is for |
|---|---|
| `files` | globs; a file uses the first adapter (in pack name order) whose globs match it |
| `comment` | region fences in this language's files (chapter 5) |
| `grammar` | a built-in ast-grep language name (`rust`, `typescript`, …), or `{ source = "dir" }` relative to `languages/` holding `parser.c` (and `scanner.c` if the grammar has one) |
| `placeholder`, `branch` | what `complete` writes when the compiler finds a case no edit covers: `branch` with `pattern` and `body`, where `body` is the `placeholder` with `message` = `olifant: <function> <pattern>`. Without both, such a case is a conflict instead of a todo |
| `indent` | one level of indentation; default four spaces. Used when `end of`/`start of` opens an empty container ([chapter 3](03-anchors.md)), and when a snippet goes on an empty line after one that opens a block ([chapter 14](14-editors-and-actions.md#the-cursor-edit-cursor--true)). `olifant-dev`'s Haskell adapter sets two spaces |
| `[check]` | `command` runs in a scratch copy of the project's files (those matching `files`, plus `context`). An argument that is exactly `{= files =}` becomes the list of source files; `{= cache =}`, anywhere in an argument, becomes a cache directory for this language that outlives the scratch copy (`~/.cache/olifant/check/<language>`). `format` picks a *compiler support* record — the code that reads that compiler's report (from stderr, or stdout for cargo and pyright), decides whether a candidate constructor is visible at a site, writes a reported pattern the way the site should, and names the declaration a line sits in. Known formats: `elm-json` (Elm), `ghc-json` (GHC), `pyright-json` (pyright) and `rustc-json` (cargo). A command that starts with `env NAME=value …` runs the program after the settings, and that program is the one that must be installed (`env UV_PROJECT_ENVIRONMENT=… uv run …` needs `uv`; `olifant doctor` checks it) |
| `[holes.KIND]` | the kinds of typed hole this language has: what an unfilled one is written as (`placeholder`) and where a fill must parse (`wrap`). See [hole kinds](#hole-kinds-holeskind), below |
| `imports` | a POSIX expression matching one import line (Python's: `^(from [A-Za-z0-9_.]+ import [A-Za-z0-9_., ]+\|import …)$`). With it, a model's fill may **begin with import lines**: they are not code for the hole but join the file's imports, after its last top-level import (or, in a file with none, after its leading comments and docstring), each only once. The model is told it may; the fill's parse check sees only its code; `--plan` shows a created file with the imports where they went. Without the key a fill is code only, so a fill that needs a name the file lacks fails its check |
| `[import_merge]` | with `imports`: `pattern` (an expression whose first group is the module and second the comma-separated names) and `write` (a template of `module` and `names`). An import of names from a module the file already imports from adds the new names to that line instead of writing a second one: `from datetime import date` + a fill's `from datetime import date, time` → `from datetime import date, time`. Python's: `pattern = "^from ([A-Za-z0-9_.]+) import ([A-Za-z0-9_, ]+)$"`, `write = "from {= module =} import {= names =}"` |
| `[targets.*]` | the `at` targets (below) |

There used to be a `qualify` key (`qualify = "elm"`) for Elm's module rules. It is retired:
loading an adapter that still has it is now a load error saying the `[check] format`
decides that instead. An unknown `format` is also a load error, listing the known ones:
`` `format`: unknown format `cobol-json`; expected one of elm-json, ghc-json, pyright-json, rustc-json ``. Like any
broken adapter, this keeps the pack's language aside instead of failing the whole command
(`catalogBrokenLanguages`); it bites only commands that need that language, such as `olifant
anchors` on it or a `complete` edit whose `language` names it.

### A second example: Haskell

`packs/olifant-dev/languages/haskell.toml` uses ast-grep's grammar built in for Haskell, and
checks with GHC through `cabal repl` instead of `elm make`:

```toml
files       = ["**/*.hs"]
comment     = "--"
grammar     = "haskell"
placeholder = 'error "{= message =}"'
branch      = "{= pattern =} -> {= body =}"

[targets.data]
phrase    = "data NAME"
container = { kind = "data_constructors", inside = { kind = "data_type", regex = "^data {= NAME =}\\b" } }
items     = { kind = "data_constructor" }
example   = "end of data CatalogError"

[check]
command = ["cabal", "repl", "-v0", "lib:olifant", "--repl-options=-fno-code", "--repl-options=-fdiagnostics-as-json", "--repl-options=-Wincomplete-patterns", "--repl-options=-fmax-uncovered-patterns=1000"]
context = ["*.cabal", "cabal.project"]
format  = "ghc-json"
```

`repl`, not `build`: with `-fno-code`, `cabal build` always exits 1 at its link step, while
`repl` exits 0 after type-checking, even when the code has errors (a compile error arrives
as a JSON diagnostic, which is how Olifant sees it; only a broken setup, such as an
unresolvable dependency, makes it exit non-zero). `-fdiagnostics-as-json` and `-Wincomplete-patterns` are both needed; GHC's
warning 62161 is what carries the missing patterns. A `complete` edit for this adapter
names its constructor `Type.Ctor` rather than Elm's `Module.Ctor` — chapter 4 covers what
that changes.

### A third example: Rust

`packs/rust-htmx/languages/rust.toml` checks the whole cargo workspace:

```toml
placeholder = 'todo!("{= message|rust_string =}")'
branch = "{= pattern =} => {= body =},"

# cargo, over the whole workspace (spec §5.6, §9). Diesel adapters need libpq
# and the template's real db crate, so they are left out of the build.
[check]
command = ["cargo", "check", "--workspace", "--exclude", "diesel-*", "--message-format=json", "--target-dir", "{= cache =}"]
context = ["**/Cargo.toml", "Cargo.lock"]
format  = "rustc-json"

[targets.enum]
phrase = "enum NAME"
container = { kind = "enum_variant_list", inside = { kind = "enum_item", has = { field = "name", regex = "^{= NAME =}$" } } }
items = { kind = "enum_variant" }
example = "end of enum RefundInvoiceFailure"
```

- **No `{= files =}`.** cargo finds the sources itself, from the manifests, so `context`
  copies every `Cargo.toml` (and a `Cargo.lock`, when the project has one) beside the `.rs`
  files.
- **`--target-dir {= cache =}`.** The scratch copy is new each round; the build directory is
  not, so dependencies compile once per machine (`~/.cache/olifant/check/rust`).
- **`--exclude diesel-*`.** The rust-htmx template's Diesel adapters need libpq and its real
  `db` crate; they are left out of every check, and the in-memory adapters are compiled.
- **`rustc-json`** reads cargo's `compiler-message` lines from stdout: error E0004 is a
  `match` missing arms, a site `complete` extends; E0005 (a refutable `let`) is a warning;
  any other error is a broken file. A `complete` edit names its variant `Type::Variant`.
  [Chapter 4](04-compiler-branches-and-todos.md#rust-cargo) walks through
  `rust-htmx/command-failure`.
- **`enum NAME`** is the target `command-failure` inserts its variant with:

  ```
  $ olifant where billing/billing-service/src/refund_invoice.rs "end of enum RefundInvoiceFailure"
  billing/billing-service/src/refund_invoice.rs:27
      pub enum RefundInvoiceFailure {
          #[error("Something went wrong")]
          Unknown(String),
    ▶     (inserted here)
      }
      
      impl RefundInvoiceFailure {
  ```

### A fourth example: Python

`packs/python-htmx/languages/python.toml` is the Python adapter. It uses ast-grep's built-in
grammar and checks with pyright, in strict mode, through `uv`:

```toml
files = ["**/*.py"]
comment = "#"
grammar = "python"
indent = "    "
placeholder = 'raise NotImplementedError("{= message|python_string =}")'
branch = "case {= pattern =}:\n    {= body =}"

[check]
command = ["env", "UV_PROJECT_ENVIRONMENT={= cache =}/venv", "uv", "run", "--quiet", "--frozen", "pyright", "--outputjson"]
context = ["pyproject.toml", "uv.lock"]
format  = "pyright-json"
```

- **`uv run pyright`, not a `pyright` on your PATH.** pyright is a dev dependency of the
  project, so its version is pinned in `uv.lock`, and strict mode comes from the project's own
  `pyproject.toml` (`[tool.pyright] typeCheckingMode = "strict"`). `--frozen` uses the lock
  as it is and never rewrites it.
- **`env UV_PROJECT_ENVIRONMENT={= cache =}/venv`.** The scratch copy is new each round, so
  a `.venv` inside it would be built again every time. The environment lives in the cache
  instead (`~/.cache/olifant/check/python/venv`), as cargo's build directory does, so only
  the first check on a machine installs anything.
- **The program is `uv`.** The command starts with `env`, so Olifant looks past it and its
  `NAME=value` settings to what it runs. Without `uv`, a `complete` edit says
  `` `uv` is not installed (needed to check python) ``, and `olifant doctor` says how to
  fix it:

  ```
  ✗ check tool uv (python) — `uv` is not on this PATH: /usr/bin:/bin
    fix: brew install uv (pyright comes with the project: `uv sync`)
  ```

- **`branch`** is a `case` arm on two lines; `complete` indents both like the `match`'s other
  arms. `placeholder` is what an arm with no body gets, a `raise` that names the function and
  the pattern.

**`pyright-json`** reads the JSON pyright prints on stdout. A non-exhaustive `match`
(`reportMatchNotExhaustive`, on in strict mode) is a site. With a `Cancelled` failure added
to the union and no arm for it, pyright says, on the `match` line of `failures.py`:

```
Cases within match statement do not exhaustively handle all values
  Unhandled type: "Cancelled"
  If exhaustive handling is not intended, add "case _: pass"
```

- **The patterns** come from `Unhandled type: "…"`: a class becomes a class pattern
  (`Cancelled` → `Cancelled()`, `Ok[int]` → `Ok()`), and an enum's `Literal[Color.RED,
  Color.BLUE]` becomes `Color.RED` and `Color.BLUE`.
- **Written as the site writes the others.** pyright names the class bare, but at a site
  whose arms say `case create_booking.SlotTaken():` the new arm says `case
  create_booking.Cancelled():`, and at one that imports the module as `cb`, `case
  cb.Cancelled():`. A pattern that already has a `.` is left alone.
- **Where the site ends.** pyright gives only the `match` line. The site runs from there to
  the last line indented deeper than it, so the new arm goes after the last `case`, however
  many lines that `case` has.
- **One error belongs to the missing case.** A function whose `match` lacks a case also gets
  "must return value on all code paths" from pyright. In a file with a non-exhaustive
  `match`, that error is the missing case's consequence, not a broken file. Any other error
  is a broken file; warnings and information are ignored.
- **No visibility rule.** pyright reports only real sites, so any of the plan's classes may
  fill any of them, and a `complete` edit names the class bare (`complete = "{=
  failure|pascal =}"`).

The adapter's targets:

```
$ olifant anchors python --human
python: at = "<start of|end of|before|after> <target>"

targets:
  class NAME     e.g. end of class BookingService
  function NAME  e.g. end of function create_app
  imports        e.g. end of imports
  match on NAME  e.g. end of match on failure
  union NAME     e.g. end of union CreateBookingFailure

built in (any file):
  before|after marker TEXT
  before|after first line matching REGEX
  before|after last line matching REGEX
  start of|end of file
```

`union NAME` is a failure union written one member per line, in parentheses, which is the
pack's convention:

```python
type CreateBookingFailure = (
    SlotTaken
    | PartyTooLarge
    | TableNotFound
)
```

The whole union is the target's one item, so `end of` goes after its last line, at its
indentation:

```
$ olifant where src/bistro/booking/create_booking.py "end of union CreateBookingFailure" --human
src/bistro/booking/create_booking.py:37
        SlotTaken
        | PartyTooLarge
        | TableNotFound
  ▶     (inserted here)
    )
    
    type CreateBookingOutput = Ok[Booking] | CreateBookingFailure
```

A one-line union (`type F = A | B`) has no parentheses, so it is not a `union NAME`: write
unions the long way. `match on NAME` is a `match` on the subject `NAME`, its items the
`case`s. `function NAME` is a function's body, even when the body is one `match`, as in the
fixture's `failures.py`:

```
$ olifant where src/bistro/web/failures.py "end of match on failure" --human
src/bistro/web/failures.py:18
                return 422
            case create_booking.TableNotFound():
                return 404
  ▶         (inserted here)
$ olifant where src/bistro/web/failures.py "end of function create_booking_status" --human
src/bistro/web/failures.py:18
                return 422
            case create_booking.TableNotFound():
                return 404
  ▶     (inserted here)
```

There is no `module` target: `start of file` and `end of file` are built in.

Olifant never looks inside `.venv`, `__pycache__` or `.pytest_cache`, as it never looks
inside `node_modules` or `target` ([chapter 6](06-queries-and-choices.md)). The pack's
fixture and tests are in [chapter 10](10-testing-generators.md#the-python-pack), and its two
generators, chained in an editor, in [chapter 14](14-editors-and-actions.md#the-python-chain).

### Hole kinds: `[holes.KIND]`

A typed hole (`{= ? name: kind | instruction =}`, [chapter 12](12-holes-and-models.md)) can
use only the kinds its file's adapter declares. Each kind is a table with two keys, both
required:

```toml
# packs/rust-htmx/languages/rust.toml
# Hole kinds (spec §6.3): the unfilled placeholder (must compile) and the code
# a fill must parse in.
[holes.expr]
placeholder = 'todo!("olifant: {= hole =}: {= instruction|rust_string =}")'
wrap = "fn __olifant() { let _ = {= fill =}; }"

[holes.block]
placeholder = 'todo!("olifant: {= hole =}: {= instruction|rust_string =}")'
wrap = "fn __olifant() { {= fill =} }"

[holes.type]
placeholder = "() /* olifant: {= hole =} */"
wrap = "type __Olifant = {= fill =};"
```

```toml
# packs/elm/languages/elm.toml
[holes.expr]
placeholder = 'Debug.todo "olifant: {= hole =}: {= instruction|escaped =}"'
wrap = """
olifantHole =
    {= fill =}
"""
```

- **`placeholder`** is a template with two values, `hole` (the hole's name, with
  `[file:line]` when the name repeats) and `instruction` (the rendered instruction). It is
  what an unfilled hole is written as, so **it must compile** wherever a hole of that kind
  can stand. **It must use `{= hole =}`**; an adapter whose placeholder doesn't fails to
  load ("`placeholder` must use {= hole =} (the hole's name), so every placeholder is
  unique"). Olifant finds each hole's placeholder by its text, so two holes must never share
  one, which is why the Rust `type` placeholder carries the name in a comment. Put
  `olifant:` before the name, as these do, so that a search finds every unfilled hole and a
  reader knows what is missing. Escape the instruction for
  the string it lands in: `rust_string` in Rust, `python_string` in Python, `escaped` in Elm and Haskell
  ([chapter 7](07-writing-a-generator.md#filters)).
- **`wrap`** is plain text (not a template) containing `{= fill =}` exactly once, where the
  fill goes. With the fill in place, it must be a whole file the adapter's grammar parses
  with no error. That is the "parses as its kind" check: `409` passes in
  `fn __olifant() { let _ = 409; }`, and `409; x` does not. A `wrap` without `{= fill =}`
  is a load error for the adapter.

A pack's test cases check the first rule for you: the "placeholder output compiles" group
compiles every case that has recorded fills with its holes left unfilled
([chapter 10](10-testing-generators.md#holes-fillstoml-and-placeholder-output)).

The kinds in this repository are Rust `expr`, `block`, `type`, `ident` and `text`; Elm
`expr`; and Haskell `expr` (`placeholder = 'error "olifant: {= hole =}: {= instruction|escaped =}"'`,
`wrap = "olifantHole = {= fill =}"`) and `text`; Python's are below. `ident` and `text` are for snippets' tab
stops ([chapter 12](12-holes-and-models.md#where-a-holes-value-comes-from-holesname)), a name
and text inside a string:

```toml
# packs/rust-htmx/languages/rust.toml
# Tab stops in snippets (spec §4.9): a name, or text inside a string.
[holes.ident]
placeholder = "{= hole =}"
wrap = "struct {= fill =};"

[holes.text]
placeholder = "{= hole =}"
wrap = "const _: &str = \"{= fill =}\";"
```

Python's placeholders are the hardest: one must stand anywhere an expression can, in strict
pyright, with no import. A `raise` is a statement, so the `expr` placeholder is a generator
expression's `throw`:

```toml
# packs/python-htmx/languages/python.toml
# Hole kinds (spec §6.3). An expression placeholder is typed Any (getattr's
# result), so it fits any type under pyright strict and needs no import; it
# raises with the hole's message if it is ever run. Not Never: pyright skips
# code after a Never as unreachable, which would leave a later fill unchecked.
[holes.expr]
placeholder = 'getattr(NotImplemented, "olifant: {= hole =}: {= instruction|python_string =}")'
wrap = "def __olifant():\n    return {= fill =}"

[holes.stmt]
placeholder = 'raise NotImplementedError("olifant: {= hole =}: {= instruction|python_string =}")'
wrap = "def __olifant():\n{= fill =}"

[holes.block]
placeholder = 'raise NotImplementedError("olifant: {= hole =}: {= instruction|python_string =}")'
wrap = "def __olifant():\n{= fill =}"
```

`getattr(NotImplemented, "…")` has type `Any`, which pyright accepts as any type: as a
`return` value, an argument or a dataclass field's default. Run, it raises
`AttributeError` naming the hole (`'NotImplementedType' object has no attribute 'olifant: value: …'`).
It is deliberately not `Never` (a `raise` inside an expression, such as
`(_ for _ in ()).throw(…)`, the first version): pyright treats the code after a `Never` as
unreachable and checks none of it, so in `Visit(day=<hole>, slot=<fill>)` a wrong fill for
`slot` passed its check. A placeholder must stand for any type *and* leave the code around
it checked. `stmt` and `block` are a plain `raise` on a
line of its own. Python also has `ident` (`wrap = "class {= fill =}: ..."`) and `text`
(`wrap = "__olifant = \"{= fill =}\""`), for snippets' tab stops. In a plan, an unfilled
`value` hole of `failure-arms` reads:

```python
case create_booking.Cancelled():
    return getattr(NotImplemented, "olifant: value: the value this match returns for create_booking.Cancelled(), following the other arms")
```

A tab stop that is yours or derived is written as its name or value, not its placeholder, so
the `ident` and `text` placeholders matter only for a model hole of that kind. Using a kind the adapter doesn't declare is a conflict
that names the ones it has:

```
  ! hole: src/Page/Home.elm: `block` is not a hole kind of the elm adapter (kinds: expr)
```

A fill is also compiled with the plan when the adapter has a `[check]`. A language with
holes but no `[check]` validates fills by parsing alone.

A vendored grammar is compiled once with `cc` into `~/.cache/olifant/grammars/`, keyed by a
hash of `parser.c`, so an updated grammar is rebuilt automatically. The first Elm run on a
machine therefore needs a C compiler (on macOS, `xcode-select --install`).

### Targets

A target is a phrase shape and two ast-grep rules:

- `phrase`: words, where words in CAPITALS are parameters (`case in FUNCTION`). The key after
  `targets.` is only a label.
- `container`: a rule matching exactly one node, the thing being inserted into. Its strings
  are templates over the parameters (`regex = "^{= NAME =}$"`).
- `items`: a rule for the container's items. `start of` goes before the first, `end of` after
  the last, with the last item's indentation. `before`/`after` use the container itself.
- `example`: shown by `olifant anchors`.

The rules are ast-grep's rule language (`kind`, `regex`, `inside`, `has`, `not`, `any`,
`stopBy`, `field`). Two idioms from the Elm adapter are worth copying:

- **"the outermost one"**: `not = { inside = { kind = "case_of_expr", stopBy = "end" } }`
  is how `case in update` ignores the `case urlRequest of` nested inside a branch.
- **"not the catch-all"**: `items = { kind = "case_of_branch", not = { regex = "^(_|…)\\s*->" } }`
  is how `end of case in update` lands before `( _, _ ) ->`.

ast-grep needs a `kind` on every item rule, which is why `list in FUNCTION` lists every
expression kind a list element can be.

## Adding a target, and testing it with `olifant where`

`lib.rs` in the rust-htmx fixture is a list of `pub mod` lines. Today it is an owned region
([chapter 5](05-owned-regions.md#families-one-region-per-matching-file)), but a generator that
placed a line there used `at = "after last line matching ^pub mod "`, and a structural target
is better. Work on a copy of the pack, so nothing in the repo changes:

```sh
cp -R ~/projects/olifant/packs/rust-htmx /tmp/my-rust-pack
scripts/try-pack.sh rust-htmx /tmp/olifant-rust && cd /tmp/olifant-rust
printf '[packs]\nrust-htmx = { path = "/tmp/my-rust-pack" }\n' > .olifant/config.toml
```

First find the node kinds. ast-grep prints the tree of a pattern:

```
$ ast-grep run -l rust -p 'pub mod refund_invoice;' --debug-query=ast billing/billing-service/src/lib.rs
Debug AST:
source_file (0,0)-(0,23)
  mod_item (0,0)-(0,23)
    visibility_modifier (0,0)-(0,3)
    name: identifier (0,8)-(0,22)
```

(For Elm, point ast-grep at the config Olifant generated: `ast-grep run -c
~/.cache/olifant/sg/elm-*/sgconfig.yml -l elm -p 'type Route = Home | About'
--debug-query=ast src/Route.elm`.)

So the container is the `source_file` and the items are `mod_item`s. Add to
`/tmp/my-rust-pack/languages/rust.toml`:

```toml
[targets.mods]
phrase = "mods"
container = { kind = "source_file" }
items = { kind = "mod_item" }
example = "end of mods"
```

and try it:

```
$ olifant anchors rust
…
  mods                    e.g. end of mods
…
$ olifant where billing/billing-service/src/lib.rs "end of mods"
billing/billing-service/src/lib.rs:8
    pub mod refund_invoice;
    pub mod service;
    pub mod void_invoice;
  ▶ (inserted here)
    // olifant:end modules

$ olifant where billing/billing-service/src/lib.rs "start of mods"
billing/billing-service/src/lib.rs:2
    // olifant:region modules (generated from glob billing/billing-service/src/*.rs; edit the declaration, not this block)
  ▶ (inserted here)
    pub mod cancel_invoice;
    pub mod models;
    pub mod ports;
```

`service.rs` has no `mod_item` at all, so `end of mods` has nothing to go beside, and Olifant
refuses rather than guess:

```
$ olifant where billing/billing-service/src/service.rs "end of mods"
error: `end of mods`: has no items to insert beside
hint: `olifant anchors <language>` lists the targets
```

Only a bracketed container (`()`, `{}`, `[]`) is opened when empty ([chapter 3](03-anchors.md));
a whole file is not one. For a file that may be empty, use the built-in `end of file`.

A wrong kind is reported by ast-grep, through Olifant:

```
$ olifant where billing/billing-service/src/lib.rs "end of mods"     # with kind = "mod_itm"
error: `end of mods`: ast-grep failed: Error: Cannot parse rule INLINE_RULES
…
╰▻ Kind `mod_itm` is invalid.
```

Check the cases that should *not* match as carefully as the ones that should: a nested
container, a catch-all branch, and — since an empty bracketed container now opens instead
of refusing — a container with no items, where "resolves" no longer means "is a sane place
to insert". A container that has something inside that is not one of `items` is refused
("has content that is not one of the target's items"); that is usually a sign the target's
`items` is missing a kind. `where` exits 5 when the phrase does not resolve at all, so a genuinely wrong
target is still easy to script against.

When the target works, commit it to the pack with a generator test that uses it
([chapter 10](10-testing-generators.md)); the adapter's own unit tests are in
`src/Olifant/LanguageSpec.hs` and `src/Olifant/AnchorSpec.hs`.

## A pack of your own

Nothing about a pack is special to this repository. A directory with a `generators/` and a
`[packs]` entry pointing at it is a pack. To start one:

```sh
mkdir -p ~/packs/mine/generators ~/packs/mine/fixtures/app
```

then configure it, move a project generator's directory into `generators/`, and change any
exemplar path to `fixture:…` (copying the file into `fixtures/app/`). The olifant-dev pack
has a generator that scaffolds a new pack generator with a test case to bless:
`olifant gen olifant-dev/generator --set pack=rails --set name=controller`, run from the root
of this repository (chapter 10).
