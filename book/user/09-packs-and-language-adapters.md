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
  model.toml             how the pack's conventions map onto the project model (optional)
  tests/recognize/       labelled cases of hand-written code, for recognition (optional)
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
| `model.toml` | the language server's project model | [below](#the-project-model-modeltoml) |
| `tests/recognize/*.toml` | Olifant's own spec (recognition's precision and recall) | [below](#the-recognition-corpus-testsrecognize) |
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
| `[check]` | `preset` names a check Olifant knows (`pyright`, `cargo`; [below](#presets-check-and-test)), and its keys are filled in from it; a key you write wins. Without a preset: `command` runs in a scratch copy of the project's files (those matching `files`, plus `context`). An argument that is exactly `{= files =}` becomes the list of source files; `{= cache =}`, anywhere in an argument, becomes a cache directory for this language that outlives the scratch copy (`~/.cache/olifant/check/<language>`). `root` (optional) lists files that mark a package's root: a check, an `uncovered` query and a hole's `verify` then run only in the judged files' package — the nearest folder above them holding one of these files (`root_pick = "outermost"`: the outermost) — with `files` and `context` read from that folder, and the report's paths made the project's again. Files judged in different packages, or no marker, check the whole project. `exclusive = true` is for a tool that keeps a build cache it cannot share between runs at once (cargo's target dir): each check gets a cache no other check uses while it runs (`{= cache =}` is then one of a few slots per workspace, each kept warm), so checks running together — the language server's, two projects' — never read each other's builds. `format` picks a *compiler support* record — the code that reads that compiler's report (from stderr, or stdout for cargo and pyright), decides whether a candidate constructor is visible at a site, writes a reported pattern the way the site should, and names the declaration a line sits in. Known formats: `elm-json` (Elm), `ghc-json` (GHC), `pyright-json` (pyright) and `rustc-json` (cargo), and the generic `sarif`, `errorformat` and `olifant-json`; `convert` runs a command that turns any tool's output into olifant-json ([check formats](#check-formats)). A command that starts with `env NAME=value …` runs the program after the settings, and that program is the one that must be installed (`env UV_PROJECT_ENVIRONMENT=… uv run …` needs `uv`; `olifant doctor` checks it) |
| `[test]` | how this language's tests run, for the editor's test, conjure and mutate ([chapter 18](18-tdd-in-the-editor.md)) and `verify = "tests"`: `preset` (`pytest`) or `command` (`{= tests =}` becomes the test ids), `format` (`pytest`, `lines`, `tap`, `junit-xml`, `olifant-json`) or `convert`, `context`, `root`; and `tests_for`, a module's test file (`{= dir =}/{= stem =}__test.py`). `[test.select]`, `file_group`, `unit`, `exclusive` and `timeout` say how one test is run and found. See [presets](#presets-check-and-test) and [test formats](#test-formats) |
| `[unit]` | `kinds`: the tree-sitter node kinds of a definition (a function, method or class) in this language, for `:Durandal conjure` ([chapter 18](18-tdd-in-the-editor.md#conjuring)): the innermost node of these kinds around the cursor is the unit conjured. Python's are `function_definition`, `class_definition` and `decorated_definition` (written inline, `unit = { kinds = [...] }`); Rust, Elm and Haskell list theirs. Without it, `conjure` says the language has no unit kinds. `name` (optional): a list of POSIX expressions over a unit's line; the first that matches names the unit by its **last group** (the whole match when it has none). A unit is named by the first line of its span that has a name (Python's decorated def by its `def` line). Without it, a unit is named by its line's first identifier. `join = true` (default false): units of one name, at one indentation, separated only by blank lines (or touching), are one unit — a Haskell signature and its equations. See [the Haskell example](#a-second-example-haskell) |
| `[holes.KIND]` | the kinds of typed hole this language has: what an unfilled one is written as (`placeholder`) and where a fill must parse (`wrap`). See [hole kinds](#hole-kinds-holeskind), below |
| `keywords` | the language's reserved words (optional). Recognition keeps them as they are when it reads code by shape; every other name becomes its case shape ([chapter 16](16-the-language-server.md#recognition-this-looks-like-a-command)). The words in the pack's templates count too |
| `separator`, `line_length` | what follows each item in brackets (`","`), and the longest line (default 88). With a separator an item can join a one-line container: inline when it fits, else the container goes one item per line ([chapter 3](03-anchors.md#rules-worth-knowing)). Python: `,`/88; Rust: `,`/100; Elm has none (its commas lead), so a one-line Elm list still refuses |
| `imports` | a POSIX expression matching one import line (Python's: `^(from [A-Za-z0-9_.]+ import [A-Za-z0-9_., ]+\|import …)$`). With it, a model's fill may **begin with import lines**: they are not code for the hole but join the file's imports, after its last top-level import (or, in a file with none, after its leading comments and docstring), each only once. The model is told it may; the fill's parse check sees only its code; `--plan` shows a created file with the imports where they went. Without the key a fill is code only, so a fill that needs a name the file lacks fails its check |
| `[import_merge]` | with `imports`: `pattern` (an expression whose first group is the module and second the comma-separated names) and `write` (a template of `module` and `names`). An import of names from a module the file already imports from adds the new names to that line instead of writing a second one: `from datetime import date` + a fill's `from datetime import date, time` → `from datetime import date, time`. Python's: `pattern = "^from ([A-Za-z0-9_.]+) import ([A-Za-z0-9_, ]+)$"`, `write = "from {= module =} import {= names =}"` |
| `[targets.*]` | the `at` targets (below) |

There used to be a `qualify` key (`qualify = "elm"`) for Elm's module rules. It is retired:
loading an adapter that still has it is now a load error saying the `[check] format`
decides that instead. An unknown `format` is also a load error, listing the known ones:
`` `format`: unknown format `cobol-json`; expected one of elm-json, ghc-json, pyright-json, rustc-json, sarif, olifant-json, errorformat ``. Like any
broken adapter, this keeps the pack's language aside instead of failing the whole command
(`catalogBrokenLanguages`); it bites only commands that need that language, such as `olifant
anchors` on it or a `complete` edit whose `language` names it.

### A second example: Haskell

`packs/olifant-dev/languages/haskell.toml` uses ast-grep's grammar built in for Haskell,
checks with GHC through `cabal repl` instead of `elm make`, and runs its tests with hspec
through `cabal test`. Both are presets ([below](#presets-check-and-test)):

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
preset = "cabal-repl"

[test]
preset = "hspec"
```

The `cabal-repl` preset runs `cabal repl -v0 --builddir={= cache =}/dist-repl-{= context =}
--repl-options=-fno-code --repl-options=-fdiagnostics-as-json --repl-options=-Wincomplete-patterns
--repl-options=-fmax-uncovered-patterns=1000` with no target: cabal then loads the package's
library, so the preset is for a package with one library (a package with several names its
target in its own `command`). The build directory lives in Olifant's cache, one per slot
(`exclusive`), so the second check of a scratch copy is incremental although the copy's path is
new each time.

`repl`, not `build`: with `-fno-code`, `cabal build` always exits 1 at its link step, while
`repl` exits 0 after type-checking, even when the code has errors (a compile error arrives
as a JSON diagnostic, which is how Olifant sees it; only a broken setup, such as an
unresolvable dependency, makes it exit non-zero). `-fdiagnostics-as-json` and `-Wincomplete-patterns` are both needed; GHC's
warning 62161 is what carries the missing patterns. A `complete` edit for this adapter
names its constructor `Type.Ctor` rather than Elm's `Module.Ctor` — chapter 4 covers what
that changes.

Its `[unit]` table makes a function its signature and its equations, so a plan's
excerpt of `parsePlan` or a step anchored to it reads the whole definition:

```toml
[unit]
kinds = ["signature", "function", "bind", "data_type", "newtype", "class", "instance"]
join = true
name = [
  "^(data|newtype|type|class)[[:space:]]+([^[:space:]]+)",          # a type: its second word
  "^instance[[:space:]]+(.*[^[:space:]])[[:space:]]+where[[:space:]]*$", # an instance: its head
  "^instance[[:space:]]+(.*[^[:space:]])",
  "^[[:space:]]*(\\([^)]*\\))",                                     # an operator: (<+>)
  "^[[:space:]]*([[:alpha:]_][[:alnum:]_']*)",                       # else the first word
]
```

`parsePlan :: Text -> [Line]` and the equations under it are one unit named `parsePlan`.
A signature split over lines before `::` is named by its first line. An infix equation
(`a <+> b = b`) is named by its first word, as before, so it is not joined to its
signature. Expressions are POSIX as regex-tdfa reads them: `[[:alnum:]]` is ASCII only,
which is why Python's name is `[^[:space:](:[]+` (a `def crème` keeps its name). Rust's
names an `impl` block by its type (`impl<T> Display for Wrapper<T>` → `Wrapper`) and an
item by the word after its keyword (`pub(crate) async fn load` → `load`); Elm's names a
`type alias` by its alias.

### A third example: Rust

`packs/rust-htmx/languages/rust.toml` checks the whole cargo workspace:

```toml
placeholder = 'todo!("{= message|rust_string =}")'
branch = "{= pattern =} => {= body =},"

# cargo, over the whole workspace (spec §5.6, §9). Diesel adapters need libpq
# and the template's real db crate, so they are left out of the build.
[check]
preset = "cargo"

[targets.enum]
phrase = "enum NAME"
container = { kind = "enum_variant_list", inside = { kind = "enum_item", has = { field = "name", regex = "^{= NAME =}$" } } }
items = { kind = "enum_variant" }
example = "end of enum RefundInvoiceFailure"
```

The `cargo` preset stands for

```toml
command   = ["cargo", "check", "--workspace", "--exclude", "diesel-*", "--message-format=json", "--target-dir", "{= cache =}/target-{= context =}"]
context   = ["**/Cargo.toml", "Cargo.lock"]
format    = "rustc-json"
exclusive = true
root      = ["Cargo.toml"]
root_pick = "outermost"
```

- **No `{= files =}`.** cargo finds the sources itself, from the manifests, so `context`
  copies every `Cargo.toml` (and a `Cargo.lock`, when the project has one) beside the `.rs`
  files.
- **`--target-dir {= cache =}/target-{= context =}`.** The scratch copy is new each round; the
  build directory is not, so dependencies compile once per machine and workspace shape
  (`~/.cache/olifant/check/rust`). `exclusive`: cargo's build directory is not safe to share
  between checks running at once.
- **`root`, `root_pick = "outermost"`.** In a repository holding several Rust projects, a
  check runs in the Cargo workspace around the files it judges: the outermost folder with a
  `Cargo.toml` (a crate's own `Cargo.toml` sits inside the workspace's).
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
preset = "pyright"

[test]
preset    = "pytest"
tests_for = "{= dir =}/{= stem =}__test.py"
```

The `pyright` preset stands for

```toml
command = ["env", "UV_PROJECT_ENVIRONMENT={= cache =}/venv-{= context =}", "uv", "run", "--quiet", "--frozen", "pyright", "--outputjson"]
context = ["pyproject.toml", "uv.lock"]
format  = "pyright-json"
root    = ["pyproject.toml"]
```

- **`root = ["pyproject.toml"]`.** In a monorepo of Python apps, a check (and a hole's
  `verify`) runs in the judged file's app only: the nearest folder above it with a
  `pyproject.toml`. Its `context` globs are read from there, so that app's `pyproject.toml`
  and `uv.lock` are the ones used.

- **`uv run pyright`, not a `pyright` on your PATH.** pyright is a dev dependency of the
  project, so its version is pinned in `uv.lock`, and strict mode comes from the project's own
  `pyproject.toml` (`[tool.pyright] typeCheckingMode = "strict"`). `--frozen` uses the lock
  as it is and never rewrites it.
- **`UV_PROJECT_ENVIRONMENT={= cache =}/venv-{= context =}`.** The scratch copy is new each
  round, so a `.venv` inside it would be built again every time. The environment lives in the
  cache instead (`~/.cache/olifant/check/python/venv-…`, one per `pyproject.toml` and
  `uv.lock`), as cargo's build directory does, so only the first check on a machine installs
  anything. The preset says it; a pack never does.
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
  call NAME               e.g. end of call CancelBooking
  call NAME in FUNCTION   e.g. end of call CreateBooking in create
  class NAME              e.g. end of class BookingService
  function NAME           e.g. end of function create_app
  imports                 e.g. end of imports
  match on NAME           e.g. end of match on failure
  parameters in FUNCTION  e.g. end of parameters in floor_routes
  union NAME              e.g. end of union CreateBookingFailure

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
$ olifant where src/bistro/booking/commands/create_booking.py "end of union CreateBookingFailure" --human
src/bistro/booking/commands/create_booking.py:39
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
$ olifant where src/bistro/web/bookings/failures.py "end of match on failure" --human
src/bistro/web/bookings/failures.py:18
                return 422
            case create_booking.TableNotFound():
                return 404
  ▶         (inserted here)
$ olifant where src/bistro/web/bookings/failures.py "end of function create_booking_status" --human
src/bistro/web/bookings/failures.py:18
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

### Presets: `[check]` and `[test]`

A check or a test run needs Olifant's plumbing (its cache, a venv kept there, the tool's output
format). A **preset** says it once, in Olifant, so a pack writes only what is its own. The presets
ship inside the binary as TOML files, `library/toolchains/<name>.toml` in Olifant's source, each
holding one `[check]` or `[test]` table with the keys it fills in:

| table | preset | what it runs |
|---|---|---|
| `[check]` | `pyright` | `uv run pyright --outputjson` in the project's venv in Olifant's cache; `pyright-json`; rooted at `pyproject.toml` |
| `[check]` | `cargo` | `cargo check --workspace --exclude diesel-* --message-format=json`, a target dir per workspace in the cache; `rustc-json`; `exclusive`; the outermost `Cargo.toml` |
| `[test]` | `pytest` | `uv run pytest -q -rA --tb=short -p no:cacheprovider` over the test ids, in the same venv; `format = "pytest"` reads which test failed, at which line, and why; rooted at `pyproject.toml`; `tests_for = "{= dir =}/test_{= stem =}.py"` |
| `[check]` | `cabal-repl` | `cabal repl -v0 --builddir={= cache =}/dist-repl-{= context =}` with `-fno-code`, GHC's JSON diagnostics and its missing-pattern warning; `ghc-json`; `exclusive`; context `*.cabal`, `cabal.project`, `cabal.project.freeze`. Loads the package's one library |
| `[test]` | `hspec` | `cabal test -v0 --builddir={= cache =}/dist-test-{= context =} --test-show-details=direct --test-option=--format=checks --test-option=--no-color`, then the selected tests; `format = "lines"` over hspec's tree ([test formats](#test-formats)); one test by `--match=/{= group =}/{= name =}/`, a file by its module (`file_group`); `unit` finds `describe`/`it`; `exclusive`, `timeout = 900`; `tests_for = "{= dir =}/{= stem =}Spec.hs"` |

Any key you write beside `preset` wins over the preset's: `[check] preset = "pyright"` with your
own `root = ["apps/web/pyproject.toml"]` keeps the rest. An unknown preset is a load error naming
the known ones (`` `preset`: unknown test preset `jest`; expected one of hspec, pytest ``).

A `[check]` or `[test]` may say how to install its tool, `install = "…"` (a sentence): `olifant
doctor` gives it as the fix when the tool is not on the PATH. Every shipped preset carries one;
without it doctor says "install <program> and make sure it is on the PATH your editor starts
with".

`[test]` also says where a module's tests live: `tests_for`, a template of the module's folder
(`{= dir =}`) and name without its extension (`{= stem =}`). The editor's commands find a module's test file
by it, and a test file's module by it backwards.

Tests run like checks: in a scratch copy of the project's files as the editor has them (nothing
needs saving), rooted at the package around the tests (`root`). A run that cannot start (a test
file that does not import, a collection error) is said apart from a red test.

### Check formats

`[check] format` names who reads the check's report. Four belong to one compiler each
(`elm-json`, `ghc-json`, `pyright-json`, `rustc-json`) and can say which cases a `case` lacks,
which is what `complete` and `uncovered` need. Three are generic: any tool that writes one of
them works without Olifant code, though only for broken files (a red file and its line).

**`sarif`** — SARIF 2.1, which most linters and analysers can write. Every
`runs[].results[]` whose `level` is `"error"` is a broken file: its path is
`locations[0].physicalLocation.artifactLocation.uri` (a leading `file://` dropped), its line
`region.startLine`, its text `ruleId: message.text` (just the message without a rule). Other
levels are ignored; a report with no results is clean.

```toml
[check]
command = ["semgrep", "scan", "--sarif", "--quiet", "."]
format  = "sarif"
```

**`errorformat`** — a regex over the tool's lines, stdout then stderr. `[check.errorformat]`
gives the `pattern` and the numbers of its groups holding the `file`, the `line` and the
`message`; with `severity` (a group) and `error` (an expression), a line counts only when that
group matches `error`. Without `error` every matching line counts.

```toml
[check]
command = ["make", "-k"]
format  = "errorformat"

[check.errorformat]
pattern  = '^([^:]+):([0-9]+):[0-9]+: (error|warning): (.*)$'
file     = 1
line     = 2
severity = 3
message  = 4
error    = '^error$'
```

`src/a.c:3:5: error: boom` makes `src/a.c` broken at line 3 with `boom`; the warning line
after it does not count.

**`olifant-json`** — Olifant's own report, for a `convert` command (below) or a tool that
writes it directly. Either key may be absent; `{}` is clean. `case` lines are 0-based, as
GHC's and Elm's are read:

```json
{
  "broken":  [{"file": "src/A.hs", "message": "Variable not in scope: nope", "line": 12}],
  "missing": [{"file": "src/B.hs", "case": [40, 44], "patterns": ["Nothing"], "type": "Maybe Int"}]
}
```

**`convert`** — for a tool Olifant has no format for: a command that turns the check's output
into olifant-json. It runs in the same scratch copy after the check, with
`{"exit": N, "stdout": "…", "stderr": "…"}` on its stdin and 60 seconds to finish; what it
prints is the report. A non-zero exit fails the check with what it said on stderr. With
`convert`, `format` is absent or `"olifant-json"`; anything else is a load error naming both
keys. The converted report is the verdict: a check that exits non-zero and converts to `{}` is
clean.

```toml
[check]
command = ["mytool", "--all"]
convert = ["python3", "tools/mytool-to-olifant.py"]
```

### Test formats

`[test] format` names who reads a run's output (stdout, then stderr). `pytest` is Python's,
built in. The others are generic: a framework that prints one of them needs no Olifant code.

**`lines`** — a regex per line, for runners that print one line per test (hspec's
`--format=checks`, `cargo test`). `[test.lines]`:

| key | |
|---|---|
| `pass`, `fail` | a line matching one is a test that passed or failed, named by the last group |
| `nest` | (default false) a test's name has the names of the lines above it at smaller indentation in front: hspec's `describe` tree. Without it, the line's name is the whole name |
| `failure` | a line that opens a failure block: group 1 the file, group 2 the line. The block runs to the next one, or to an `end` line; it is the k-th failed test's message, dedented |
| `end` | where a block (and the tree) ends |
| `drop` | lines left out of a block's message (hspec's `1) Module.group test`) |

hspec through cabal, as the [`hspec` preset](#presets-check-and-test) reads it:

```toml
[test.lines]
pass    = '^[[:space:]]*(.*[^[:space:]]) \[✔\]$'
fail    = '^[[:space:]]*(.*[^[:space:]]) \[✘\]$'
nest    = true
failure = '^  ([^[:space:]].*):([0-9]+):[0-9]+: *$'
end     = '^  To rerun use|^Randomized with seed|^Finished in'
drop    = '^  [0-9]+\) '
```

**`tap`** — `ok N - name` passes, `not ok N - name` fails with its YAML block (between `---`
and `...`) as the message; a `# SKIP` or `# TODO` is neither; `Bail out!` is a run that
could not judge, with its reason.

**`junit-xml`** — each `testcase` is a test named by its `name`, in its `file`; a `failure`
or `error` child fails it (its `message`, its text, its `line`); `skipped` is neither. A
runner that writes the XML to a file is read through `convert` (`cat report.xml`).

**`olifant-json`** — for `convert` (as in `[check]`: the command gets
`{"exit": N, "stdout": "…", "stderr": "…"}` on stdin and prints this) or a runner that
writes it:

```json
{"passed": ["src/a_test.x::adds"], "failed": [{"id": "src/a_test.x::subtracts", "line": 9, "message": "expected 1", "output": "…"}], "broken": null}
```

An id with no `::` is a test's name in the requested file. `broken` (a string) is a run
that could not judge the tests.

**Which file a test is in.** A test id is `path::group::…::name`. A report's test goes to
the file its report names (hspec's failure line, JUnit's `file`), when that is a file the
run asked for; else to the file whose `file_group` is its first name (hspec's outermost
`describe` is the module); else to the only file asked for. That first name is then dropped:
`Olifant.Plans` / `parsePlan` / `reads a step` in `src/Olifant/PlansSpec.hs` is
`src/Olifant/PlansSpec.hs::parsePlan::reads a step`. A run with no tests is broken: with a
non-zero exit, by the first 40 lines it printed (GHC's error when the spec does not
compile); with exit 0, "no tests ran".

**The other `[test]` keys.**

| key | |
|---|---|
| `file_group` | an expression over a test file's text; its last group is the file's outermost group. hspec's: `'^module[[:space:]]+([[:alnum:]_.]+)Spec'` |
| `[test.select]` | how an id becomes an argument of `{= tests =}`. `test` for a test's id, `file` for a file's. `{= group =}` is the file's `file_group` (it needs that key), `{= name =}` the test's names below its file joined by `/`. hspec's: `test = "--test-option=--match=/{= group =}/{= name =}/"`. Without it, the ids themselves are the arguments |
| `unit` | expressions whose line starts a test or a group, the last group its name: how a plan step and the editor find a test by name ([chapter 19](19-plans.md)) |
| `exclusive` | the runner keeps a build cache: each run gets a slot (`{= cache =}` is the slot's), as `[check] exclusive` |
| `timeout` | seconds a run may take, over the editor's default |

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
- `spaced` (default `true`): when the items next to the new one are separated by a blank
  line, the new one gets one too (Elm case branches, Rust methods). `false` where a blank
  line means something else: Python's `imports` sets it, because a blank line there
  separates import groups, and a new import joins the last group.

The rules are ast-grep's rule language (`kind`, `regex`, `inside`, `has`, `not`, `any`,
`stopBy`, `field`). Two idioms from the Elm adapter are worth copying:

- **"the outermost one"**: `not = { inside = { kind = "case_of_expr", stopBy = "end" } }`
  is how `case in update` ignores the `case urlRequest of` nested inside a branch.
- **"not the catch-all"**: `items = { kind = "case_of_branch", not = { regex = "^(_|…)\\s*->" } }`
  is how `end of case in update` lands before `( _, _ ) ->`.

ast-grep needs a `kind` on every item rule, which is why `list in FUNCTION` lists every
expression kind a list element can be.

`call NAME` and `call NAME in FUNCTION` are a call's arguments (`wire` passes a command
its adapter in `main.py`, a fake in a test); `parameters in FUNCTION` a function's
parameters (`web-command` gives a router factory its command). All three want one item per
line, and an empty `CancelBooking()` opens. `imports` sets `spaced = false` (a blank line
between imports is a group, so a new import joins the last group) and `when_empty = "after
header"` (in a file with no imports, the first goes under the docstring).

## The project model: `model.toml`

The language server keeps a model of each project: its services, commands, queries, ports,
adapters, web resources, data models, tests and pages, and how they relate. Every pack
describes it with the same words, so a Python project and a Rust project read the same way
(and can be put side by side later, as one system). A pack's `model.toml` says which of its
files are which:

```toml
# packs/python-htmx/model.toml (abridged)
[[elements]]
kind  = "command"
query = { glob = "src/*/*/commands/*.py", exclude = ["**/__init__.py", "**/*__test.py"] }
id    = "{= path|dir|dir|basename =}/commands/{= stem =}"

[[elements]]
kind   = "adapter"
query  = { glob = "src/*/*/ports/*/*.py", exclude = ["**/__init__.py", "**/*__test.py"] }
id     = "{= path|dir|dir|dir|basename =}/ports/{= path|dir|basename =}/{= stem =}"
differ = ["{= stem =}", "{= path|dir|basename =}"]   # not the port's own Protocol file

[[relations]]
kind     = "uses"
query    = { files = "src/*/*/commands/*.py", items_of = "parameters in __init__", each = { type = ":[[:space:]]*([A-Za-z_]+)" } }
from     = "{= path|dir|dir|basename =}/commands/{= path|stem =}"
to_name  = "{= type|snake =}"
to_kinds = ["port"]

[[relations]]
kind    = "covers"
from    = "test"
to_path = "{= path =}"
rewrite = [["__test\\.py$", ".py"]]
```

- **`[[elements]]`**: a `kind`, a `query` (the [query language](06-queries-and-choices.md):
  a glob, an AST query, or a command) and an `id` template over each record; `name` defaults
  to the id's last segment. `differ = [a, b]` keeps only records where the two templates
  render differently.
- **Kinds**: `service`, `command`, `query`, `port`, `adapter`, `resource`, `model`, `test`,
  `page`, or the pack's own, namespaced (`elm:route`, `haskell:unit`). Any other word is a
  load error.
- **Ids** are the project's name, a colon, and the rendered id:
  `bistro:booking/commands/create_booking`. They do not depend on the order files are listed in.
  The name is `name` at the top of `.olifant/config.toml` (`name = "bistro"`), else the root
  folder's name; give one when two projects' folders have the same name, or when the folder's
  name is not the project's.
- **`[[relations]]`**: a `kind` (`uses`, `implements`, `calls`, `covers`), from each element
  of a kind (`from = "adapter"`, the templates read its record), or from each record of a
  `query` (`from` renders the element's id). The target is one of `to` (an id), `to_path` (an
  element's path) or `to_name` (an element's name, among `to_kinds`), each rewritten by
  `rewrite` (regex, replacement) pairs in order. A relation whose ends are not both elements is
  dropped.
- Two template filters exist for this: `dir` (a path's folder) and `stem` (its file name
  without the extension).

`python-htmx`, `rust-htmx`, `elm` and `olifant-dev` have one.

To see a project's model, ask the language server for it: `olifant/model` with a document of the
project answers its `elements` (id, kind, name, path, line), its `relations` (from, kind, to)
and its `problems` (a query that failed, and why), [chapter 16](16-the-language-server.md#the-project-model).
Nothing draws it yet: it is the seam a view of the whole system builds on.

## The recognition corpus: `tests/recognize/`

How well recognition knows a pack's conventions is measured, not argued. A pack may keep small
labelled cases of hand-written code, one file each:

```toml
# packs/python-htmx/tests/recognize/command-own-way.toml
path   = "src/bistro/booking/commands/close_table.py"   # where it is written
expect = "command"          # the truth: a generator (or "repository|port"), or "nothing"
band   = "offer"            # what recognition gives today: offer | hint | nothing
inputs = { app = "bistro", service = "booking", command = "close_table" }
# as = "query"; known = "why"   — a known miss: what it gives instead, and why
text = '''
class CloseTable:
    def run(self, table_id):
        ...
'''
```

Olifant's spec pins every case's outcome and reports each corpus's offer precision and recall;
a change that lowers either fails. The cases worth having are the hard ones: the convention
written its own way, a copy renamed, the same shape in the wrong folder, and code that only looks
like it (a data class in `shared/`, an enum beside the commands). python-htmx has 28 (offer
precision 0.92, recall 0.88); rust-htmx 17 (1.00 and 1.00).

## The python-htmx pack

`python-htmx` builds a ports-and-adapters web app: Python 3.12+, FastAPI and htmx with
Jinja, uv, pytest, pyright strict, SQLite through the standard library. `olifant init
--pack python-htmx NAME` starts one ([chapter 2](02-running-generators.md#start-a-project-olifant-init));
its fixture is the layout written down ([chapter 10](10-testing-generators.md#the-python-pack)).

**The test policy.** A generator that writes something known and purely boilerplate ships
passing tests; one that writes behaviour you decide ships a failing stub test, already
wired up, ready for the first case. Snippets add the next case.

| generator | writes | its tests |
|---|---|---|
| `project` (the starter) | `pyproject.toml`, `uv.lock`, `main.py`, `web/app.py`, base and home templates | 3 passing |
| `service` | a service's packages: `models/`, `ports/`, `commands/`, `queries/` | none |
| `model` | `models/NAME.py`, a frozen dataclass; its first field a tab stop | none |
| `port` | `ports/PORT/`: the Protocol, an in-memory adapter, a contract test | passing |
| `repository` | `ports/E_repository/`: Protocol (`get`, `save`, `all`), in-memory and SQLite adapters from the model's fields (a value field such as `date` goes through encode/decode holes the model fills), contract test over both | passing |
| `command` | `commands/NAME.py`: Input, a first failure in a one-per-line union, `handle` | a red stub |
| `query` | `queries/NAME.py`: Input, Result, `handle` | a red stub |
| `add-dependency` (menu) | on `__init__`: one of the service's ports joins its parameters, then `wire` | the existing ones |
| `handle-body` (menu) | on `handle`'s `raise NotImplementedError`: the model writes the body until the module's tests pass | must make them pass |
| `wire` (follow-up) | on an `__init__` parameter: the module keeps the port; the test's helper (`def command(…)`) takes it, its fake the default (`bookings: BookingRepository \| None = None`, passing `bookings or InMemoryBookingRepository()`), and every other test that builds it passes the fake; `main.py`, if it builds it, passes the adapter, added once | the existing ones |
| `web-resource` | `web/RESOURCE/`: the page from a query, a route per command (`web-command`), templates, `failures.py`; `main.py` builds and routes them; the home page links it | a red stub |
| `failure-arms`, `failure-variant`, `match-arm`, `test-case`, `port-method`, `port-method-adapters` | snippets and follow-ups ([chapter 14](14-editors-and-actions.md#the-python-htmx-snippets)) | a red stub where they add behaviour |

`build-in-main`, `wire-main`, `wire-test`, `wire-helper`, `web-command`, `port-method-adapter` and
`add-import` are the pieces the others use; each runs on its own too.

**`main.py` builds what is routed.** A command or query is built in `main.py` when a web
resource routes it (an adapter or a command nothing uses would be an unused variable, which
pyright strict rejects). `wire` then keeps `main.py` in step as ports are added. The
adapter is the port's SQLite one (given the database path) when it has one, else another
real one beside it, else the fake.

**The region.** `web/resources.py`'s `RESOURCES` lists every `web/*/routes.py`; the home
page links each. (It lives in Python, not in `home.html`, because a region's fences are
comments and an HTML comment cannot end a line the way `#` does.)

**The rules** (`packs/python-htmx/rules.toml`, [chapter 15](15-conventions-that-hold.md)):
`commands-use-ports` (no `sqlite3` in a command or query; the fixes use an existing port, or add a repository),
`web-uses-commands` (no ports or `sqlite3` in `web/`), `adapters-in-main` (only `main.py`
and tests import adapters) and `failures-returned` (a hint: return a failure, don't raise
it).

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
