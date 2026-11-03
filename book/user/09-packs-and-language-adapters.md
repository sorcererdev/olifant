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
    elm.toml             over the shipped elm.toml: the grammar below
    grammar/elm/         the tree-sitter grammar it needs, vendored as C source
  toolchains/            preset overrides, <name>.toml (optional)
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
  crate imports             e.g. end of crate imports
  literal in FUNCTION       e.g. end of literal in new
  literal NAME in FUNCTION  e.g. end of literal RefundInvoice in new

from queries (tree-sitter-rust @77a3747):
  function NAME             e.g. end of function main
  class NAME                e.g. end of class BillingService
  KIND NAME                 e.g. end of struct BillingService
  parameters in FUNCTION    e.g. end of parameters in new
  call NAME                 e.g. end of call BillingService::new
  call NAME in FUNCTION     e.g. end of call BillingService::new in main
  imports                   e.g. end of imports

built in (any file):
  before|after marker TEXT
  before|after first line matching REGEX
  before|after last line matching REGEX
  start of|end of file
```

Most of Rust's anchors are the generic phrases ([chapter 3](03-anchors.md#the-generic-phrases)),
read from tree-sitter queries: `end of struct BillingService`, `end of impl BillingService`,
`end of enum RefundInvoiceFailure`, `end of mod tests`. The three `targets` are what no query
capture expresses, declared in ast-grep's dialect in `library/languages/rust.toml`.

Four phrases wire a command's dependencies (spec §7.1, chapter 8): `parameters in FUNCTION` is
a function's parameter list; `literal NAME in FUNCTION` is the struct literal named `NAME`
inside `FUNCTION` (`SendEmail` inside `new`, as opposed to `literal in FUNCTION`, which is
whichever one literal sits there); `call NAME in FUNCTION` is the arguments of a call to `NAME`
inside `FUNCTION` (`BillingService::new` inside `main`). All of them can target an empty
`()`/`{}` — see "Opening an empty container" in [chapter 3](03-anchors.md).

`imports` is a file's top-level import statements (`use` and `extern crate` in Rust, from
olifant's own `imports.scm`). `end of imports` goes after the last one, whole, so a new import
lands after a multi-line `use crate::{ … };` block and never inside it; a `use` inside
`mod tests` is not top-level and does not count. A line pattern cannot see the block: `after
last line matching ^use ` lands just after `use crate::{`, inside it:

```
$ olifant where billing/billing-service/src/service.rs "after last line matching ^use "
billing/billing-service/src/service.rs:5
    use crate::ports::invoice_repository::InvoiceRepository;
    
    use crate::{
  ▶ (inserted here)
        refund_invoice::{RefundInvoice, RefundInvoiceInput, RefundInvoiceOutput},
        void_invoice::{VoidInvoice, VoidInvoiceInput, VoidInvoiceOutput},
        cancel_invoice::{CancelInvoice, CancelInvoiceInput, CancelInvoiceOutput},

$ olifant where billing/billing-service/src/service.rs "end of imports"
billing/billing-service/src/service.rs:9
        void_invoice::{VoidInvoice, VoidInvoiceInput, VoidInvoiceOutput},
        cancel_invoice::{CancelInvoice, CancelInvoiceInput, CancelInvoiceOutput},
    };
  ▶ (inserted here)
    
    #[derive(Clone)]
    pub struct BillingService {
```

A file with no import at all is not opened; the phrase misses (`end of imports` in
`billing-service/src/lib.rs`, which only has `pub mod`s: "no `imports` in this file"). Give
`start of file` as the fallback, as `command-dependency` does: `at = ["end of imports", "start
of file"]`. The phrase was `end of uses` before the generic phrases; a pack that still says
`uses` is told "unknown target `uses` … did you mean `end of imports`?".

### Where a language comes from

Olifant ships its languages: `python`, `rust`, `haskell` and `elm` are TOML files inside the
binary (`library/languages/<name>.toml` in Olifant's source), as are the toolchain presets
([below](#presets-check-and-test)). A language is the merge of up to three **layers**, each the
same file format, later winning:

1. the shipped file;
2. each configured pack's `languages/<name>.toml`, in pack order;
3. the project's own `.olifant/languages/<name>.toml`.

They merge key by key: a table merges with the table under the same key (so a pack's `[test]
tests_for` keeps the shipped `[test] preset`), and anything else — a string, a number, a list,
an array of tables like `[[mutate]]` — is replaced whole. Then the one preset the merged `[check]`
or `[test]` names fills in the keys still missing. So a project that names a different preset
swaps the whole preset, while the keys an earlier layer wrote stay. A `grammar = { source = "…" }`
is found beside the layer that set it (`<pack>/languages/…` or `.olifant/languages/…`); a shipped
file cannot name one, which is why the elm pack keeps `languages/elm.toml` with only its grammar.

Toolchains layer the same way: a pack's `toolchains/<name>.toml` and the project's
`.olifant/toolchains/<name>.toml` (one `[check]` or `[test]` table each) override the shipped
preset of that name.

A shipped language is not on in every project. A language is the project's when a pack or the
project has a `languages/<name>.toml` for it, when a generator says `language = "<name>"`, or
when the project's `.olifant/config.toml` lists it:

```toml
languages = ["rust"]
```

A language that does not load (an unknown key, a missing `grammar`) is reported with the path of
the layer that is at fault (`olifant doctor`, `olifant list`) and the others load; a layer that is not
valid TOML sets its whole language aside the same way; one that only a generator
names and that cannot load (elm without the elm pack) is left out quietly. `olifant doctor` says
which layers each language came from and which layer set each capability's key.

In a project with no Elm, `olifant anchors elm` says "no language adapter named `elm`", and
`.elm` files get only the built-in line and marker targets.

**Migrating a pack.** A pack's full `languages/<name>.toml` from before keeps working: it is now a
layer over the shipped file, and every key it sets wins. One rule keeps an old file whole: a
`[check]` or `[test]` that sets `command` or `convert` and no `preset` stops inheriting the shipped
`preset`, so the shipped preset's other keys (hspec's `select`, pytest's `root`) are not added to a
runner of its own. Write `preset` again in that section to get them. It can be thinned to what its convention
changes — python-htmx's is three lines, its `__test.py` test files:

```toml
# Over the shipped python.toml: only what this pack's convention changes.
[test]
tests_for = "{= dir =}/{= stem =}__test.py"
```

Adding a language to Olifant itself is a file in `library/languages/` (touch
`src/Olifant/Library.hs` so the embed sees it); adding one to a project is a file in
`.olifant/languages/`.

### The keys

From `library/languages/rust.toml` and `library/languages/elm.toml`:

```toml
files       = ["**/*.rs"]                         # which files this adapter covers
comment     = "//"                                # line comment, for region fences
grammar     = { git = "https://github.com/tree-sitter/tree-sitter-rust", rev = "77a3747266f4d621d0757825e6b11edcbf991ca5" }
                                                  # the grammar's source at a pinned commit…
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
| `grammar` | where the language's tree-sitter grammar comes from ([grammars](#grammars-built-once-loaded-at-run-time)). `{ git = URL, rev = SHA }`: a git repository at a pinned commit; `rev` is a full 40-character commit SHA, never a branch or a tag (a branch moves, and the build is cached by it), and `git` is an `https://`, `ssh://`, `git://` or `file://` URL. `{ source = "dir" }`: C sources beside the file, relative to `languages/`, holding `parser.c` (and `scanner.c` if the grammar has one); a pack or the project carries them, a shipped file cannot. Either table may add `name` (the grammar's name when it is not the language's: the queries folder, its `tree_sitter_<name>` symbol, ast-grep's language; letters, digits and `_`) and, with `git`, `path` (the folder inside the repository holding `src/`, as tree-sitter-typescript's `tsx`). A bare name (`"go"`) is ast-grep's built-in language only: `[[mutate]]` and `[targets.*]` work, units, generic anchors and the default migrate count do not. The shipped Python, Rust and Haskell files name their `git` and `rev` |
| `placeholder`, `branch` | what `complete` writes when the compiler finds a case no edit covers: `branch` with `pattern` and `body`, where `body` is the `placeholder` with `message` = `olifant: <function> <pattern>`. Without both, such a case is a conflict instead of a todo |
| `arm_separator`, `lone_arm_spaced` | how `complete` joins a new arm to the others: `arm_separator` (default none) is appended to the last arm first, unless it already ends with it or ends a block (Rust's `","`); `lone_arm_spaced = true` puts a blank line before the new arm when the match has only one (Elm's and Haskell's style; default false). With two or more arms, the existing spacing is copied |
| `indent` | one level of indentation; default four spaces. Used when `end of`/`start of` opens an empty container ([chapter 3](03-anchors.md)), and when a snippet goes on an empty line after one that opens a block ([chapter 14](14-editors-and-actions.md#the-cursor-edit-cursor--true)). the shipped Haskell file sets two spaces |
| `[check]` | `preset` names a check Olifant knows (`pyright`, `cargo`; [below](#presets-check-and-test)), and its keys are filled in from it; a key you write wins. Without a preset: `command` runs in a scratch copy of the project's files (those matching `files`, plus `context`). An argument that is exactly `{= files =}` becomes the list of source files; `{= cache =}`, anywhere in an argument, becomes a cache directory for this language that outlives the scratch copy (`~/.cache/olifant/check/<language>`); `{= root =}`, anywhere in an argument or the program, the package folder on disk, as in `[test]` (a tool in the project's venv is `{= root =}/.venv/bin/ruff`). `command` may be a list of candidates, the first that fits, as a `[test] command` is ([command candidates](#test-command-candidates)). `root` (optional) lists files that mark a package's root: a check, an `uncovered` query and a hole's `verify` then run only in the judged files' package — the nearest folder above them holding one of these files (`root_pick = "outermost"`: the outermost) — with `files` and `context` read from that folder, and the report's paths made the project's again. Files judged in different packages, or no marker, check the whole project. `exclusive = true` is for a tool that keeps a build cache it cannot share between runs at once (cargo's target dir): each check gets a cache no other check uses while it runs (`{= cache =}` is then one of a few slots per workspace, each kept warm), so checks running together — the language server's, two projects' — never read each other's builds. `format` picks a *compiler support* record — the code that reads that compiler's report (from stderr, or stdout for cargo and pyright), decides whether a candidate constructor is visible at a site, writes a reported pattern the way the site should, and names the declaration a line sits in. Known formats: `elm-json` (Elm), `ghc-json` (GHC), `pyright-json` (pyright) and `rustc-json` (cargo), and the generic `sarif`, `errorformat` and `olifant-json`; `convert` runs a command that turns any tool's output into olifant-json ([check formats](#check-formats)). A command that starts with `env NAME=value …` runs the program after the settings, and that program is the one that must be installed (`env UV_PROJECT_ENVIRONMENT=… uv run …` needs `uv`; `olifant doctor` checks it) |
| `[test]` | how this language's tests run, for the editor's test, conjure and mutate ([chapter 18](18-tdd-in-the-editor.md)) and `verify = "tests"`: `preset` (`pytest`) or `command` (`{= tests =}` becomes the test ids; `{= root =}`, anywhere in an argument or the program, the package folder on disk, the one `root`'s markers find, else the project's: the run itself is in a scratch copy, so a venv of the project is `{= root =}/.venv/bin/pytest`), `format` (`pytest`, `lines`, `tap`, `junit-xml`, `olifant-json`; `[test.lines]` and `[test.junit]` say how to read the first and the fourth) or `convert`, `context`, `root`, `files` (globs of ignored files a run needs; the rest of the project is copied anyway, [below](#test-formats)); and `tests_for`, a module's test file (`{= dir =}/{= stem =}__test.py`). `[test.select]`, `file_group`, `unit`, `exclusive` and `timeout` say how one test is run and found. See [presets](#presets-check-and-test) and [test formats](#test-formats). `report` (optional): where the runner writes its report, a template over `{= cache =}`, `{= context =}` and `{= root =}`; `{= report =}` in the command is that path, it is deleted before the run and read after, whatever the format. A relative path is under the run's copy. Without it, `{= report =}` is a file in the copy |
| `[[mutate]]` | mutation operators, for `mutate` ([chapter 18](18-tdd-in-the-editor.md)): each a `name`, a `pattern` or ast-grep `rule`, and a `fix` or `transform = "increment"`. Python ships seventeen |
| `ignore` | folder names (not paths: a name with a `/`, or an empty one, is a load error) a project's listings never enter (dependencies, build output, caches): Rust's `["target"]`, Python's `[".venv", "__pycache__", ".pytest_cache", "node_modules"]`, Haskell's `["dist-newstyle"]`, Elm's `["elm-stuff", "node_modules"]`. The project's languages' lists are joined; olifant itself keeps out only `.git` and, in a walk outside git, dot-folders |
| `unit_words` | what a unit is in this language, as the plan prompts say it: "<Unit> is <unit_words> defined in that file". Python's is "a top-level class or function name (Name) or a method (Class.method)"; a project whose languages disagree, or one without the key, gets "a top-level function or type name (Name), or a method (Type.method)" |
| `[unit]` | `kinds`: the tree-sitter node kinds of a definition (a function, method or class) in this language, for `:Durandal conjure` ([chapter 18](18-tdd-in-the-editor.md#conjuring)): the innermost node of these kinds around the cursor is the unit conjured. A language whose grammar loads (Rust, Python, Haskell, and Elm with its pack) needs none: its units are every function and class-like definition the grammar's queries capture, named by its `@name` there (`[unit] name` then only finds a unit's definition line), a decorated definition with its decorators, a Haskell signature with its equations. A Rust trait impl is a unit named by its type (its methods are `Type.method`, as before), and a Haskell instance by its head (`C Int`, its methods `C Int.m`). A plan anchor naming a type and its inherent impl (both `A`) is the type, as before; any other name several units share is reported as ambiguous, never resolved to the first. Without `kinds`, a provider or a grammar that loads, `conjure` says the language has no unit kinds. A test's span, where its line starts a definition, is that definition's too (a string's outdented line, a Rust test's closing brace and a multi-line signature are the test's). `name` (optional): a list of POSIX expressions over a unit's line; the first that matches names the unit by its **last group** (the whole match when it has none). A unit is named by the first line of its span that has a name (Python's decorated def by its `def` line). Without it, a unit is named by its line's first identifier. `join = true` (default false): units of one name, at one indentation, separated only by blank lines (or touching), are one unit — a Haskell signature and its equations. `annotation` (optional): an expression over `{= name =}` matching that name's declared type, the type its **last non-empty group**, read on a unit's header line for a parameter, on a `record`'s lines for a field, and by conjure on a sibling test's constructor (Python's `'{= name =}:[[:space:]]*([^,)=]+)'`; Haskell's `'{= name =}[[:space:]]*::[[:space:]]*([^,}]+)'`; Rust's reads `Arc<dyn X>`, `Box<dyn X>`, `&dyn X`, `impl X` and a plain `X` as `X`). See [the Haskell example](#a-second-example-haskell). `kinds = { provider = [...] }` in place of the list: a program names the units instead ([providers](#providers-answering-a-slot-with-a-program)); its answers are qualified (`Outer.Inner`) by containment, and a unit's definition line is still the first of its lines that `name` names (else its first) |
| `[holes.KIND]` | the kinds of typed hole this language has: what an unfilled one is written as (`placeholder`) and where a fill must parse (`wrap`). See [hole kinds](#hole-kinds-holeskind), below |
| `keywords` | the language's reserved words (optional). Recognition keeps them as they are when it reads code by shape; every other name becomes its case shape ([chapter 16](16-the-language-server.md#recognition-this-looks-like-a-command)). The words in the pack's templates count too |
| `separator`, `line_length` | what follows each item in brackets (`","`), and the longest line (default 88). With a separator an item can join a one-line container: inline when it fits, else the container goes one item per line ([chapter 3](03-anchors.md#rules-worth-knowing)). Python: `,`/88; Rust: `,`/100; Elm has none (its commas lead), so a one-line Elm list still refuses |
| `[imports]` | how this language imports. `line`: a POSIX expression matching one import line (Python's: `^(from [A-Za-z0-9_.]+ import [A-Za-z0-9_., ]+\|import …)$`; `imports = "…"`, a string, is the same key on its own). With it, a model's fill may **begin with import lines**: they are not code for the hole but join the file's imports, after its last top-level import (or, in a file with none, after its leading comments and docstring), each only once. The model is told it may; the fill's parse check sees only its code; `--plan` shows a created file with the imports where they went. Without the key a fill is code only, so a fill that needs a name the file lacks fails its check. `example = { needs, fill }` (optional): the example the model is shown, a fill that starts with the import of the name it `needs`; Python's `{ needs = "date", fill = "from datetime import date\n\ndate.fromisoformat(text)" }`. Without it the model is told the rule with no example. `spaced = false` (default `true`): a blank line between imports groups them, so `end of imports` (the generic phrase) joins the last import with no blank line; Python sets it. `when_empty = "after header"`: in a file with no import, `end of imports` goes after its leading comments and docstring (else it is "no `imports` in this file"); Python sets it |
| `[imports]` (2) | `write`: the line a test file imports a unit with, a template over `{= module =}` and `{= owner =}` (the unit's first dotted segment: a method's class). Python's `from {= module =} import {= owner =}`, Haskell's `import {= module =}`; Rust has none (its tests sit in the file's `mod tests`, under `use super::*;`). Without it a stub writes no import. `module_of = { strip, drop, separator }`: how a path becomes a module — strip the first `strip` expression that matches, drop the extension, drop trailing segments that `drop` lists, join with `separator` (default `.`). Python's `{ strip = ["^src/"], drop = ["__init__"] }` (`src/a/b.py` is `a.b`, `pkg/__init__.py` is `pkg`); Haskell's strips `^src/` or `^test/`; Rust's `{ strip = ["^(.*/)?src/"], drop = ["lib", "main", "mod"], separator = "::" }` (`crates/m/src/money/rates.rs` is `money::rates`, `src/lib.rs` the crate root) |
| `[imports]` (3) | `[[imports.resolve]]` and `paths`: which project files an import line names, for a conjure's context. Each resolve is `{ pattern, module, names }`: an expression over a stripped line, the group holding its modules (comma-separated, each less its alias) and, optionally, the group holding names imported from it, each of which may be a module too (`module.name`). The first resolve that matches a line counts. `paths` are templates over `{= path =}` (the module, its separator as `/`); the first that is a file is the module's. Python's two resolves read `from a.b import x, y` (`a.b`, `a.b.x`, `a.b.y`) and `import a.b, c`; its paths are `src/{= path =}.py` and `src/{= path =}/__init__.py`. Without `resolve`, a conjure sees no imported files |
| `opens_block` | an expression over a line (its comment cut) that opens a block: Python's `':[[:space:]]*(#.*)?$'`, Rust's `'[{][[:space:]]*$'`, Haskell's a line ending in `do`, `=` or `$`. A model that answers a hole of lines with the whole file is cut back to the hole's lines, from the block that opens above it . Without it, such a fill is not cut |
| `empty_statement` | a statement that does nothing (Python's `pass`, Haskell's `pure ()`): what a test's arrangement becomes when the model put nothing before the assertion. Without it (Rust), that side is left empty |
| `stub_lines` | expressions over a stripped line of a body that is still a stub, so conjure treats it as unwritten rather than as a body the person rejected: Python's `...`, `pass`, `assert ...`, `raise NotImplementedError`; Haskell's `expectationFailure "write this test"`, `error "`, `pure ()`; Rust's `todo!(`, `unimplemented!(`. A comment (the language's `comment`) and a hole's line (`olifant:`) count in every language |
| `[types]` | what a new method's signature is made of, when conjure adds a method a test calls to a port: `string` and `integer` (a literal argument's type: Python `str`, `int`; Rust `&str`, `i64`), `nothing` (a call whose result is unused: `None`, `()`), `optional` (a template over `{= type =}` for a result the code tests for absence: `{= type =} \| None`, `Option<{= type =}>`), `checks_none` (expressions over `{= name =}` that test it: `{= name =} is None`, `{= name =}\.is_none\(\)`), `record` (an expression over `{= name =}` starting a type with fields), `returns` (an expression over a port's method line, group 1 what it returns: the port's entity is a type there whose name starts the port's), `assigned` (an expression over a line assigning a call's result, its last group the variable), `param` (a template over `{= name =}` and `{= type =}` writing one parameter) and `method` (a template over `{= name =}`, `{= params =}` and `{= returns =}` writing the signature: Python `{= name =}(self{= params =}) -> {= returns =}`). A declared type is read by `[unit] annotation`. The method itself is written by the generator `[conjure.port] add` names (python-htmx's `port-method`). When a fact is missing, conjure does not guess the signature: the pause asks the person to add the method |
| `[import_merge]` | with `[imports] line`: `pattern` (an expression whose first group is the module and second the comma-separated names) and `write` (a template of `module` and `names`). An import of names from a module the file already imports from adds the new names to that line instead of writing a second one: `from datetime import date` + a fill's `from datetime import date, time` → `from datetime import date, time`. Python's: `pattern = "^from ([A-Za-z0-9_.]+) import ([A-Za-z0-9_, ]+)$"`, `write = "from {= module =} import {= names =}"` |
| `[targets.*]` | the `at` targets (below) |

There used to be a `qualify` key (`qualify = "elm"`) for Elm's module rules. It is retired:
loading an adapter that still has it is now a load error saying the `[check] format`
decides that instead. An unknown `format` is also a load error, listing the known ones:
`` `format`: unknown format `cobol-json`; expected one of elm-json, ghc-json, pyright-json, rustc-json, sarif, olifant-json, errorformat ``. Like any
broken adapter, this keeps the pack's language aside instead of failing the whole command
(`catalogBrokenLanguages`); it bites only commands that need that language, such as `olifant
anchors` on it or a `complete` edit whose `language` names it.

### A second example: Haskell

`library/languages/haskell.toml` pins tree-sitter-haskell (ast-grep has it built in too),
checks with GHC through `cabal repl` instead of `elm make`, and runs its tests with hspec
through `cabal test`. Both are presets ([below](#presets-check-and-test)):

```toml
files       = ["**/*.hs"]
comment     = "--"
grammar     = { git = "https://github.com/tree-sitter/tree-sitter-haskell", rev = "c30d812bc90827f1a54106a25bc9a6307f5cdcec" }
placeholder = 'error "{= message =}"'
branch      = "{= pattern =} -> {= body =}"

[check]
preset = "cabal-repl"

[test]
preset = "hspec"
```

Its anchors are the generic phrases ([chapter 3](03-anchors.md#the-generic-phrases)) read
from olifant's own Haskell queries (tree-sitter-haskell ships no tags), plus one declared
target, `do in FUNCTION` (a function's outermost `do` block). `end of data SampleError` goes
after the last constructor; `end of function f` after f's last equation (a function is its
signature and its equations, so `f` is one place, not three); `end of class C` (the class, never
its instances) and `end of instance C` (C's instances, named by the class: one instance is
one place) after the last declaration in its `where`; `end of imports` after the last
import declaration.

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

`library/languages/rust.toml` checks the whole cargo workspace:

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

`library/languages/python.toml` is the Python adapter. It pins tree-sitter-python
and checks with pyright, in strict mode, through `uv`:

```toml
files = ["**/*.py"]
comment = "#"
grammar = { git = "https://github.com/tree-sitter/tree-sitter-python", rev = "293fdc02038ee2bf0e2e206711b69c90ac0d413f" }
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

The adapter's anchors: two declared targets, and the generic phrases from tree-sitter
queries ([chapter 3](03-anchors.md#the-generic-phrases); `library/queries/python/`):

```
$ olifant anchors python --human
python: at = "<start of|end of|before|after> <target>"

targets:
  match on NAME           e.g. end of match on failure
  union NAME              e.g. end of union CreateBookingFailure

from queries (tree-sitter-python @293fdc0):
  function NAME           e.g. end of function main
  class NAME              e.g. end of class BillingService
  KIND NAME               e.g. end of struct BillingService
  parameters in FUNCTION  e.g. end of parameters in new
  call NAME               e.g. end of call BillingService::new
  call NAME in FUNCTION   e.g. end of call BillingService::new in main
  imports                 e.g. end of imports

built in (any file):
  before|after marker TEXT
  before|after first line matching REGEX
  before|after last line matching REGEX
  start of|end of file
```

In Python, `end of class BookingService` goes after the class's last statement (a docstring,
a method, a decorated method whole), at its indentation; `end of function create_app` the
same for a function. `call NAME` and `parameters in FUNCTION` take every argument or
parameter as an item, so a one-line call grows keeping them all. `end of imports` joins the
last import with no blank line (`[imports] spaced = false`) and, in a file with none, goes
after its leading comments and docstring. The examples the listing shows are the engine's,
the same for every language.

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

Olifant never looks inside `.venv`, `__pycache__`, `.pytest_cache` or `node_modules`: they
are the Python language's `ignore` ([chapter 6](06-queries-and-choices.md)). The pack's
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
| `[test]` | `pytest` | `pytest -q --tb=short -p no:cacheprovider` over the test ids, then `-o junit_family=xunit1 --junitxml={= report =}`, run as the project runs it, the first that fits under the package root: with a `uv.lock`, `uv run pytest` in the same venv as pyright's (context `pyproject.toml`, `uv.lock`); else the project's own `.venv/bin/pytest` or `venv/bin/pytest` (poetry's, pip's; `{= root =}/.venv/bin/pytest`); else `pytest` from PATH ([command candidates](#test-command-candidates)); `format = "junit-xml"` (`[test.junit] join = "."`; xunit1 because only it gives each testcase its `file`) reads which test failed, at which line, and why from pytest's JUnit report. A project `command` of your own that names no `{= report =}` keeps reading pytest's console output as before (doctor says so); rooted at `pyproject.toml`; `tests_for = "{= dir =}/test_{= stem =}.py"`; `unit` finds `def test…`, `above` its decorators; a test's class, whatever its name (`ElectionTests(TestCase)` too), comes from the grammar |
| `[check]` | `cabal-repl` | `cabal repl -v0 --builddir={= cache =}/dist-repl-{= context =}` with `-fno-code`, GHC's JSON diagnostics and its missing-pattern warning; `ghc-json`; `exclusive`; context `*.cabal`, `cabal.project`, `cabal.project.freeze`. Loads the package's one library |
| `[test]` | `cargo-test` | `cargo test --target-dir {= cache =}/test-target-{= context =} --`, then the selected tests, in the file's crate (`root = ["Cargo.toml"]`); `format = "lines"` over libtest's lines ([test formats](#test-formats)); one test by `--exact <module>::tests::<name>`, a file by `<module>::`; `exclusive`, `timeout = 900`; `tests_for = "{= dir =}/{= stem =}.rs"`: tests live in the source file. Its stub writes a `#[test] fn` with `todo!("write this test")` at `end of mod tests`, opening `#[cfg(test)] mod tests { use super::*; … }` at the file's end when there is none, and refuses (`elsewhere`) when the file says `mod tests;` |
| `[test]` | `cargo-nextest` | opt-in (`preset = "cargo-nextest"`; `rust.toml` keeps `cargo-test`): `cargo nextest run --profile olifant --no-fail-fast --target-dir {= cache =}/nextest-target-{= context =} --`, then the selected tests, with `format = "junit-xml"` and `[test.junit] split = "::"` (nextest's classname is the binary, its name the test path). nextest writes JUnit only when a profile asks, so the one candidate needs `.config/nextest.toml` to hold `[profile.olifant.junit]` and `path = "junit.xml"`; until it does, doctor's runner row is Bad with `no [profile.olifant.junit] in .config/nextest.toml`. `report` names where nextest puts the file. Ids are cargo-test's (`src/rates.rs::tests::adds`); a failure has no line (nextest's report has no file). Not yet tried against a real nextest |
| `[test]` | `hspec` | `cabal test -v0 --builddir={= cache =}/dist-test-{= context =} --test-show-details=direct --test-option=--no-color`, then the selected tests, a list of two [candidates](#test-command-candidates): when a `*.cabal` or `package.yaml` mentions `hspec-junit-formatter`, `env JUNIT_ENABLED=1 JUNIT_OUTPUT_FILE={= report =} cabal test …` with `format = "junit-xml"` ([`[test.junit] join = "/"`](#test-formats), hspec's describe path); else the same with `--test-option=--format=checks` and `format = "lines"` over hspec's tree ([test formats](#test-formats)). One test by `--match=/{= group =}/{= name =}/`, a file by its module (`file_group`); `unit` finds `describe`/`it`; `exclusive`, `timeout = 900`; `tests_for = "{= dir =}/{= stem =}Spec.hs"` |

#### hspec through JUnit

A Haskell project that depends on `hspec-junit-formatter` gets hspec's JUnit report instead of its
console tree: doctor's runner row says `hspec-junit-formatter found in p.cabal; format junit-xml`
(and `no hspec-junit-formatter in *.cabal or package.yaml` when the project has none, in which case
the console tree is read as before). The run sets `JUNIT_ENABLED=1` and `JUNIT_OUTPUT_FILE` to the
run's `{= report =}`, which the formatter reads only through its `Env` module, so the project hooks
it in `test/SpecHook.hs`:

```haskell
module SpecHook (hook) where

import Test.Hspec.Core.Spec
import qualified Test.Hspec.JUnit.Formatter.Env as FormatterEnv

hook :: Spec -> Spec
hook = FormatterEnv.whenEnabled FormatterEnv.add
```

(with `hspec-discover` picking `SpecHook.hs` up). A project that hooks the formatter some other way
gets `the tests wrote no report at {= report =}`. A describe whose text holds a `/` splits into
groups in the id, as with hspec's own `--match` path.

#### Test command candidates

A `[test] command` (and a `[check] command` alike) is a command, or a list of **candidates** to
choose from, the first that fits:

```toml
[test]
command = [
  { when = ["uv.lock"], context = ["pyproject.toml", "uv.lock"], command = ["env", "UV_PROJECT_ENVIRONMENT={= cache =}/venv-{= context =}", "uv", "run", "--quiet", "--frozen", "pytest", "{= tests =}"] },
  { when = [".venv/bin/pytest"], command = ["{= root =}/.venv/bin/pytest", "{= tests =}"] },
  { command = ["pytest", "{= tests =}"] },
]
context = ["pyproject.toml"]
```

A candidate fits when one of its `when` files exists, a path from the package root (the folder
`root`'s markers find, else the project's), among the files a run copies or on disk; one without
`when` always fits. Its `context` replaces the table's for that run (so only the uv run is keyed
by `uv.lock`). Two more keys make a candidate look inside files. A `when` entry with a `*` is a glob over the files directly in that entry's own folder (`*.cabal`), and `has = "TEXT"` makes the candidate fit only when one of its `when` files contains that text; doctor then says `hspec-junit-formatter found in x.cabal`, and when no candidate fits the text, `no hspec-junit-formatter in *.cabal or package.yaml`. `format` (in a `[test]` candidate only; a `[check]` candidate refuses it) replaces the table's `format` for that candidate, and doctor's runner row adds `; format junit-xml` when the chosen candidate set it. No candidate fitting is a run's error, `no [test] command fits here: no uv.lock or
…` (`no [check] command fits here: …` for a check). `olifant doctor`'s runner row (and its
check tool row) names the chosen one and why, from the project root:

```
✓ test runner .venv/bin/pytest (python) — /…/nightvote/.venv/bin/pytest; .venv/bin/pytest found; tests_for = {= dir =}/{= stem =}__test.py
✓ test runner uv (python) — /opt/homebrew/bin/uv; uv.lock found; tests_for = …
✓ check tool .venv/bin/ruff (python) — /…/nightvote/.venv/bin/ruff; .venv/bin/ruff found
```

A project whose check tool lives in its venv names it without an absolute path:

```toml
[check]
command = [
  { when = [".venv/bin/ruff"], command = ["{= root =}/.venv/bin/ruff", "check", "--isolated", "--select", "E9,F", "--output-format=concise", "{= files =}"] },
  { command = ["ruff", "check", "--isolated", "--select", "E9,F", "--output-format=concise", "{= files =}"] },
]
```

A layer's own `command`, bare or a list, replaces the preset's whole list. A plan's `do` is told
the chosen command, with `{= root =}` the project's folder.

Any key you write beside `preset` wins over the preset's: `[check] preset = "pyright"` with your
own `root = ["apps/web/pyproject.toml"]` keeps the rest. An unknown preset is a load error naming
the known ones (`` `preset`: unknown test preset `jest`; expected one of cargo-nextest, cargo-test, hspec, pytest ``; for `[check]`: `cabal-repl, cargo, pyright`).

A `[check]` or `[test]` may say how to install its tool, `install = "…"` (a sentence): `olifant
doctor` gives it as the fix when the tool is not on the PATH. Every shipped preset carries one;
without it doctor says "install <program> and make sure it is on the PATH your editor starts
with".

**Message keys.** A check's and a runner's words are read by expressions, not by code per tool,
so conjure reads them the same way in every language. Each is optional; without it that reading
is off.

| key | | shipped |
|---|---|---|
| `[check] undefined` | expressions for a name the check says is not defined; the name is the last group (an entry `{ pattern, name = N }` picks group N) | pyright's `'"([^"]+)" is not defined'`, cabal-repl's `"Variable not in scope: ([[:alnum:]_']+)"`, cargo's `` 'cannot find (value|function) `([^`]+)`' `` |
| `[check] missing_member` | `{ pattern, member = N, type = M }`: the check's words that a type lacks a member. Conjure offers the member to a port the unit takes | pyright's `'Cannot access attribute "([^"]+)" for class "([^"]+)"'`, member 1, type 2 |
| `[test] undefined` | as `[check] undefined`, over a failure's message; an entry's `label` is how a summary names them (`red: NameError date`; default `undefined:`) | pytest's `"name '([^']+)' is not defined"`, label `NameError` |
| `[test.failure] line_after` | an expression over `{= name =}` (the test's short name) for the output line after which the failing source line comes; the pause panel shows it | pytest's `': in {= name =}$'` |
| `[test.failure] head_strip` | an expression taken off the front of a message's first line | pytest's `'^AssertionError: '` |
| `[test.failure] got_expected` | `[{ pattern, got = N, expected = M }]` over a message's lines (the first less `head_strip`): what came back and what was expected | pytest's `assert A == B`, and `+  where False = isinstance(A, T)` |

Undefined names are taken from both tables, in the order the text gives them. Without
`line_after` the panel shows the test's last assertion line (by `[test] assertion`). Doctor's
"messages" row lists the keys a language has.

`[test]` also says where a module's tests live: `tests_for`, a template of the module's folder
(`{= dir =}`) and name without its extension (`{= stem =}`). The editor's commands find a module's test file
by it, and a test file's module by it backwards.

Tests run like checks: in a scratch copy of the project's files as the editor has them (nothing
needs saving), rooted at the package around the tests (`root`). The copy holds the rest of the
package from disk too, byte for byte: every file git tracks, and new files it does not ignore
(outside git, every file a walk finds, dot-folders left out), but none under the language's
`ignore` folders (`.venv`, `node_modules`). So the templates, fixtures and data files a test
reads are there without being listed; on APFS each is a clone, so no bytes are copied. `files`
(globs) adds files that listing leaves out, an ignored generated file a test needs, say. A run
that cannot start (a test file that does not import, a collection error) is said apart from a
red test.

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
`{"v": 1, "exit": N, "stdout": "…", "stderr": "…"}` on its stdin and 60 seconds to finish; what it
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
| `split` | a separator splitting a line's name into segments: cargo's `rates::tests::adds` with `split = "::"` is `rates` / `tests` / `adds`, so its module can be its file's group |

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

`cargo test` (libtest), as the [`cargo-test` preset](#presets-check-and-test) reads it:

```toml
[test.lines]
pass    = '^test (.+) \.\.\. ok$'
fail    = '^test (.+) \.\.\. FAILED$'
split   = "::"
failure = 'panicked at ([^ ]+):([0-9]+):[0-9]+:$'
end     = '^failures:$'
drop    = '^---- .* stdout ----$|^note: run with'
```

**`tap`** — `ok N - name` passes, `not ok N - name` fails with its YAML block (between `---`
and `...`) as the message; a `# SKIP` or `# TODO` is neither; `Bail out!` is a run that
could not judge, with its reason.

**`junit-xml`** — each `testcase` is a test. Its id is its `file` (else its `testsuite`'s), then
the `classname`'s groups, then its `name`. `[test.junit]` says how the names split:

| key | |
|---|---|
| `join` | a separator splitting the `classname` into groups (pytest's `tests.test_x.TestGroup` with `join = "."`; hspec's describe path with `join = "/"`). Without it the classname is no part of the id |
| `split` | a separator splitting the `name` into segments (`rates::tests::adds` with `split = "::"`). Without it the name is one segment |

The leading groups that spell the file's path are dropped (`tests.test_x` in `tests/test_x.py`),
so pytest's ids are its own, `tests/test_x.py::TestGroup::test_param[2.5]`. A `failure` or
`error` child fails the test. Its message is the `message` attribute (when that is one line the
text lacks, the attribute and then the text). Its line is the last `FILE:N` in the text for the
test's file, else the failure's `line`, else the testcase's; a `skipped` child is neither. A
testcase with no `classname` and an `error` is a collection failure, not a test: the run is
broken, with its message and the first lines of its text. A runner that writes the XML to a
file names it as `{= report =}`: the run is in a scratch copy, so `{= report =}` is an absolute
path in it (`.olifant-report`), expanded in the command, the program and `convert`, and deleted
before the run. When the `command` names it, that file's text is the report, for every format;
without it, the runner's stdout then stderr is. A report the runner did not write is a broken
run: by its console output when it exited non-zero, else "the tests wrote no report at …". A
`junit-xml` run whose output is not XML says the `[test]` command names no `{= report =}`: name
it, or set `format`.

**`olifant-json`** — for `convert` (as in `[check]`: the command gets
`{"v": 1, "exit": N, "stdout": "…", "stderr": "…"}` on stdin and prints this) or a runner that
writes it:

```json
{"passed": ["src/a_test.x::adds"], "failed": [{"id": "src/a_test.x::subtracts", "line": 9, "message": "expected 1", "output": "…"}], "broken": null}
```

An id with no `::` is a test's name in the requested file. `broken` (a string) is a run
that could not judge the tests.

**Which file a test is in.** A test id is `path::group::…::name`. A report's test goes to
the file its report names (hspec's failure line, JUnit's `file`), when that is a file the
run asked for; else to the file whose group leads its names (hspec's outermost `describe`
is the module, its `file_group`; a Rust file's module, `money::rates`, by `[imports]
module_of` when there is no `file_group`); else to the only file asked for. The group is then
dropped, so `rates::tests::adds` in `src/rates.rs` is `src/rates.rs::tests::adds`:
`Olifant.Plans` / `parsePlan` / `reads a step` in `src/Olifant/PlansSpec.hs` is
`src/Olifant/PlansSpec.hs::parsePlan::reads a step`. A run with no tests is broken: with a
non-zero exit, by the first 40 lines it printed (GHC's error when the spec does not
compile); with exit 0, "no tests ran".

**The other `[test]` keys.**

| key | |
|---|---|
| `file_group` | an expression over a test file's text; its last group is the file's outermost group. hspec's: `'^module[[:space:]]+([[:alnum:]_.]+)Spec'` |
| `[test.select]` | how an id becomes arguments of `{= tests =}`. `test` for a test's id, `file` for a file's: each a template, or a list of them (several arguments; one repeated by several ids is given once). `{= group =}` is the file's group (its `file_group`, else its module by `[imports] module_of`; empty for a Rust crate's root), `{= name =}` the test's names below its file joined by `join` (default `/`), `{= id =}` the whole id (`path::name`), for a runner that reads ids as they are: pytest's `test = "{= id =}"`, `file = "{= id =}"`. hspec's: `test = "--test-option=--match=/{= group =}/{= name =}/"`. cargo-test's: `test = ["--exact", "{= if group =}{= group =}::{= end =}{= name =}"]`, `file = "{= if group =}{= group =}::{= end =}"`, `join = "::"`. Without it, the ids themselves are the arguments. `select = { provider = [...] }` in place of the table: a program gets `{"v": 1, "tests": [ids]}` and answers the arguments (`["-k", "adds"]`); when it fails, the run fails with its reason ([providers](#providers-answering-a-slot-with-a-program)) |
| `unit` | rules whose line starts a test or a group: how the editor finds the test at the cursor, and a plan step a test by name ([chapter 19](19-plans.md)). An entry is an expression (its last group the name) or a table `{ pattern, after, name }`: `after` is an expression a line above must match, looking past `above` lines (Rust's `{ pattern = '^[[:space:]]*fn[[:space:]]+([[:alnum:]_]+)', after = '#\[test\]' }`: a `fn` is a test only under `#[test]`, with `#[should_panic]` between allowed); `name` is the group holding the name when it is not the last (hspec's string with escapes: group 3; `\"` in it reads `"`). A unit runs from its line to the line before the next non-blank line at its indentation or less, so a nested unit's path is its parents' names, then its own. With a grammar that loads, a class (its queries' `definition.class`) that holds a test is a group too, whatever it is called: pytest's `class TestX` / `def test_y` is `TestX::test_y`, and a Django `class ElectionTests(TestCase)` method `ElectionTests::test_y`; a class with no test in it is none. pytest's rule is `pytest.toml`'s (`def test…`, async too); hspec's find `describe`/`context`/`it`/`specify`/`prop`. A language with no `unit` has no test at a line: a cursor runs the whole file, and a plan step's test cannot be found. `unit = { provider = [...] }` in place of the rules: a program finds the tests ([providers](#providers-answering-a-slot-with-a-program)); a test's groups are the answers that hold it, so a `describe` answered over lines 1–9 and an `it` over 2–4 make `describe::it` |
| `assertion` | expressions over a line where a test's assertion starts: Python's `'^[[:space:]]*assert([[:space:]]|\(|$)'`, Rust's `assert!`/`assert_eq!`/`assert_ne!`, hspec's `` `shouldBe` `` and its kin or `expectationFailure`. A key of the language, not of the runner (presets do not fill it); a `[test]` table holding only it is no runner. It says where a new test must fail (red at its assertion, not before), and where a fill of a test's two holes (arrangement, assertion) is split |
| `name` | a new test's name (a stub's, a plan step's test), a template over `{= words =}`: the step's words, or the unit's name split into words (`CloseTable` is `close table`). pytest's `test_{= words|snake =}`, hspec's `{= words|lower_words =}`, cargo-test's `{= words|snake =}`. Without it, the words joined by `_` |
| `[test.stub]` | what a new test looks like (`:Durandal stub`, a plan step's test; [chapter 18](18-tdd-in-the-editor.md#stubbing-a-test)). `test`: its text, a template over `{= name =}`, `{= unit =}`, `{= module =}` (the unit's module, by `[imports] module_of`), `{= indent =}` (one level) and `{= note =}` (a survivor's line, its comment the language's, or empty). `file`: a new test file's head, over the same and `{= import =}` (the unit's import, by `[imports] write`); the test follows it directly. `gap` (default 1): blank lines before a test added to a file that has some. `at` (optional): a target phrase (`end of mod tests`) the test goes at instead of the file's end, and `open`: the text appended at the file's end when that target is not there, a template over the same and `{= test =}` (the test, inside it). `elsewhere` (optional): an expression for a line that says the tests live in another file, its last group that file's name (cargo-test's `mod tests;`): the stub refuses, "tests live in another file: tests.rs". pytest's writes `def test_x() -> None:` with `...` and `assert ...`, two blank lines before; hspec's an `it` at one indent, its body `expectationFailure "write this test"`, one blank line before, at `end of do in spec` (Haskell's `do in FUNCTION` target: before any helper after `spec`; a `spec` without a `do`, composed with `>>`, takes the file's end), and a new Spec module's head. A runner without it has no stub: "no `[test] stub` for …" |
| `above` | an expression for lines that belong to the unit below them: decorators (pytest's `'^[[:space:]]*@'`), attributes (Rust's `'^[[:space:]]*#\['`). A cursor on one is in that unit, and the unit's first line is the topmost of them (with a decorator's continuation lines) |

**Anchor names.** A plan anchor names a test by its path joined by `.`, each run of characters
outside `[A-Za-z0-9_.]` written as one `_`: `it "parses \"x\" twice"` under `describe
"Olifant.Plans"` is `@src/Olifant/PlansSpec.hs:Olifant.Plans.parses_x_twice`, and pytest's
`TestX::test_y` is `TestX.test_y`. Two tests with the same anchor name: the first is found.

**Known limits of rules.** Test units are found by line rules, so a pattern inside a string
or a comment counts as a test, and a `fn` whose name is on another line than `fn` is missed.
Where the language's grammar loads (Rust, Python, Haskell), a unit whose line starts a
definition ends where the definition ends: at a Rust test's closing brace, past a string's
outdented line or a signature's `) -> None:` at column 0. Elsewhere (hspec's `it`, a language
with no grammar) a unit ends at an outdent.
| `exclusive` | the runner keeps a build cache: each run gets a slot (`{= cache =}` is the slot's), as `[check] exclusive` |
| `timeout` | seconds a run may take, over the editor's default |

### Providers: answering a slot with a program

A **slot** is one question Olifant asks of a language: where its units are, where its tests
are, where an anchor phrase lands, which arguments run some tests, what the compiler and the
runner said. Each has a rule in the language file. In place of the rule you can write
`{ provider = [...] }`, a program that answers the same question; everything after the answer
is the same code, so conjure, mutate, stubs, plans and the editor behave the same either way.

| Slot | Key | Without a provider |
|---|---|---|
| units | `[unit] kinds = { provider = [...] }` | `[unit] kinds` (ast-grep node kinds) |
| test units | `[test] unit = { provider = [...] }` | the `[test] unit` rules |
| anchor | `anchors = { provider = [...] }` (top level) | none: asked only for a phrase no `[targets.*]` shape matches and the generic phrases do not know (a loaded grammar's `function NAME`, `struct NAME`, …: [chapter 3](03-anchors.md#the-generic-phrases)), before "unknown target" |
| test selection | `[test] select = { provider = [...] }` | `[test.select]` |
| diagnostics | `[check] convert` | the `[check] format` parser |
| test results | `[test] convert` | the `[test] format` parser |

`{ provider = [...] }` stands alone: an empty list, or another key beside `provider`, is a load
error naming the key. It takes the rule's place in the layer merge too: a project's
`kinds = { provider = [...] }` replaces the shipped `kinds = [...]` whole (and a provider is
replaced whole by a later rule), and `olifant doctor` names the layer it came from.

```toml
# .olifant/languages/go.toml
[unit]
kinds = { provider = ["./tools/go-units"] }

[test]
unit = { provider = ["go", "run", "./tools/testunits"] }
```

**The protocol.**

- **Requests** are a JSON object on stdin, and every one carries `"v": 1`. A units, test-units
  or anchor request is `{"v": 1, "text": "…", "file": "src/a.go"}`. `file` is the path from
  the project root, sent whenever Olifant is reading a file. It is missing only when the
  text is no file: a model's answer that Olifant checks for tests before placing it. An anchor request adds `"phrase"`, the whole `at` text. A
  selection request is `{"v": 1, "tests": ["path::name", …]}`.
- **Answers** are JSON on stdout:
  - units and test units: `[{"name": "add", "line": 3, "end": 5, "kind": "function"}]`
    (`kind` optional). Olifant works out the rest by containment, as it does for rules: a
    unit inside another is `Outer.Inner`, and a test unit's groups are the answers around it.
    The answer is checked whole: a `line` before 1, an `end` before its `line` or past the
    text, or a span repeated rejects all of it, naming the first bad entry;
  - an anchor: `{"line": 12, "column": 5}` or `null` (not known here). `line` is where the
    inserted lines start (one past the last line is the end of the file), `column` their
    indentation, written as that many spaces (a tab-indented language gets spaces here);
  - a selection: `["arg", …]`, which takes the place of `{= tests =}`.
- **Positions:** lines are 1-based and inclusive, as the editor and olifant-json's
  `broken.line` show them; columns are 1-based and counted in characters.
- **Where it runs:** from the project root, whether olifant runs from a subfolder or under the
  language server. `argv[0]` with a `/` in it (`./tools/x`) is a path under the root; a bare
  name is looked up on `PATH`. A pack's provider runs from the project root too; a pack tried
  outside any project runs its providers from the current folder.
- **Time:** a provider is stopped after 10 seconds, counted from its start, whether or not it
  has read its request.
- **Failure:** a non-zero exit, a timeout, or an answer that does not decode leaves the slot
  unavailable. The reason is stderr's first line, or why the answer did not decode; `olifant
  doctor` shows it. What the slot then does depends on the slot:
  - units: a command that needs the unit at the cursor (conjure, mutate, stub) fails with the
    reason; a plan step's unit is not found; a fill that only wanted a file's top-level names
    falls back to the lines that `[unit] name` names at column 0;
  - test units: none, as if the file had no tests;
  - an anchor: the phrase does not resolve, "the anchors provider: REASON";
  - a selection: the test run fails, "the test selection provider: REASON".
- **When it keeps failing:** a provider that times out, or whose program is missing or will
  not start, is not asked again for 60 seconds. Every request in that time gets the same
  reason at once, so a broken provider costs one timeout, not one per file.
- **Purity:** a provider must be pure: the same request gives the same answer. Olifant may
  remember an answer for the rest of the process (test units are read from many places). It
  asks again when the file's text or path changes. A timeout or a missing program is not
  remembered: after the 60 seconds, the provider is asked again.
- **Diagnostics and test results** are the existing `convert` commands
  ([check formats](#check-formats), [test formats](#test-formats)): stdin
  `{"v": 1, "exit": N, "stdout": "…", "stderr": "…"}`, stdout olifant-json.

### Hole kinds: `[holes.KIND]`

A typed hole (`{= ? name: kind | instruction =}`, [chapter 12](12-holes-and-models.md)) can
use only the kinds its file's adapter declares. Each kind is a table with two keys, both
required:

```toml
# library/languages/rust.toml
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
# library/languages/elm.toml
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
# library/languages/rust.toml
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
# library/languages/python.toml
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

### Grammars: built once, loaded at run time

No grammar is compiled into olifant. The first time a language's grammar is needed (a
parse for an anchor, a unit or a count; an ast-grep query on Elm; `olifant doctor`), olifant
fetches its `{ git, rev }` (a shallow fetch of that one commit) into
`~/.cache/olifant/grammars/src/`, or takes its `{ source }` folder, compiles `parser.c` and
`scanner.c` with `cc` into `~/.cache/olifant/grammars/<name>-<key>.dylib`, and loads it. That
costs under a second per grammar for Python, Rust and Elm, about two for Haskell (a large
grammar), once per machine and revision; afterwards a load is under a millisecond. The key
covers the revision (or the folder's `parser.c` and `scanner.c`), the tree-sitter version
inside olifant and the machine, so a changed `rev`, an edited `parser.c` or an olifant
upgrade rebuilds by itself; old libraries are left in the folder. So a fresh machine needs a C
compiler once (on macOS, `xcode-select --install`) and either the network or a local copy of
the sources: `OLIFANT_GRAMMAR_SOURCES` is a colon-separated list of folders, each holding
`<name>/parser.c`, looked in before fetching (olifant's repository has the shipped three in
`library/grammars/`). Run `olifant doctor` once to do it ahead of time: its `grammar` row says
`built now in N s`, or what failed and how to fix it.

**A grammar that cannot load stops the command.** A grammar needs `cc` or its library already
in the cache. With neither (or when its fetch, build or load fails), any command that parses
that language stops with the reason instead of falling back to ast-grep or the line rules,
because the answer would then depend on the machine:

```
olifant: needs a C compiler to build the python grammar: install it (xcode-select --install), or restore ~/.cache/olifant/grammars
```

(`apt install gcc` off macOS). With no compiler nothing is fetched either. A command that never
parses the language (`olifant check` with a `[check]` command, `[[mutate]]` on ast-grep's own
Python) runs as before, and `olifant doctor` still runs and shows why in its `grammar` row; the
language server answers the request with the same sentence and shows it once. A bare
`grammar = "NAME"` (no source) is configuration, the same on every machine, and is the one case
that keeps ast-grep and the line rules.

**Trust.** A language file, a project's or a pack's included, can name a grammar to fetch, and
olifant then compiles that C code and loads it into its own process, the language server
included when a file opens: a project's grammar source is trusted like its `[check]` command,
which olifant also runs as you. Olifant fetches only a full commit SHA (never a branch or tag),
with git's own transports, no redirects and no hooks, into its cache folder; it never fetches
or compiles while it only lists or reads configuration.

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
  line means something else, as between Python's import groups, where a new import joins the
  last group. (Python's `imports` is now a generic phrase; the same fact is its `[imports]
  spaced = false`.)

The rules are ast-grep's rule language (`kind`, `regex`, `inside`, `has`, `not`, `any`,
`stopBy`, `field`). Two idioms from the Elm adapter are worth copying:

- **"the outermost one"**: `not = { inside = { kind = "case_of_expr", stopBy = "end" } }`
  is how `case in update` ignores the `case urlRequest of` nested inside a branch.
- **"not the catch-all"**: `items = { kind = "case_of_branch", not = { regex = "^(_|…)\\s*->" } }`
  is how `end of case in update` lands before `( _, _ ) ->`.

ast-grep needs a `kind` on every item rule, which is why `list in FUNCTION` lists every
expression kind a list element can be.

Python's `call NAME`, `call NAME in FUNCTION` and `parameters in FUNCTION` are generic phrases
now, not targets: a call's arguments (`wire` passes a command its adapter in `main.py`, a fake
in a test) and a function's parameters (`web-command` gives a router factory its command).
All three want one item per line, and an empty `CancelBooking()` opens. Its `imports` is a
generic phrase too; the two facts its old target carried are the language's `[imports]
spaced = false` (a blank line between imports is a group, so a new import joins the last
group) and `[imports] when_empty = "after header"` (in a file with no imports, the first goes
under the docstring).

### Overriding a language's queries

The generic phrases, units and test spans of a language with a grammar and its queries (the
shipped Python, Rust and Haskell, and Elm with the elm pack) are read through tree-sitter query
files shipped inside olifant (`library/queries/<grammar>/`: `tags.scm`, `textobjects.scm`,
`imports.scm`, Rust's `impls.scm` and Elm's `types.scm`). A project can put its own beside them, in `.olifant/queries/<grammar>/`:

- a file with a shipped file's name **replaces** it (`.olifant/queries/rust/imports.scm`
  holding `(extern_crate_declaration) @import` makes `end of imports` go after the
  `extern crate` lines only);
- a file with a new name is **added** to the set;
- the predicates `#eq?`, `#not-eq?`, `#match?`, `#not-match?`, `#any-of?` and `#not-any-of?`
  work (`#match?` is a POSIX extended search); any other predicate or directive is an error.

The captures olifant reads are `@definition.*` with `@name` (a definition and its name),
`@function.inner`, `@class.inner` (a definition's items), `@function.outer`, `@class.outer`
(a unit's whole span), `@parameter.inner`, `@call.outer` and `@import`. A query set that does
not compile is a load error naming the file (and the byte, for a syntax error; an unknown
predicate is named instead), and the language is used with its shipped queries. A query file olifant
cannot use is a load error too, never silence: one that is not UTF-8 (the rest of the set is
used), one in a subfolder of the grammar's folder, and a folder for a grammar no active language
reads. `olifant doctor`'s `queries` row names each file's origin:

```
✓ rust: queries — tree-sitter-rust @77a3747; imports (project), impls, tags, textobjects (shipped)
```

### Conjure's shapes: `[conjure]`

A pack tells conjure how its architecture looks, in its own
`languages/<name>.toml`. Conjure uses these shapes to find the ports a test's
fake stands for, to wire a missing port, and to add a method to a port. Without
`[conjure]`, conjure works at the floor: it writes a test's body or a
function's from the test and the unit, and never asks about ports. Haskell
and Rust ship at the floor: on a hspec `it` or a `#[test] fn` whose body is
its stub (`stub_lines`), conjure fills that body (the language's
`[holes.block]`). On a test, the unit conjured is the `[test] unit` at the
cursor (hspec's `it`, which no `[unit]` kind is); its body is the lines
indented deeper than its first, so Rust's closing brace stays out of it. This
`[conjure]` is a language table in a pack. It is not the `[conjure] wire` key
of a project's `.olifant/config.toml`, which names the generator that wires a
port.

Shapes are line expressions, like `[test] unit`. A block is the lines after
its start that are blank or indented deeper.

| Key | python-htmx's value | What it says |
|---|---|---|
| `service` | `"src/{= app =}/{= service =}"` | where a service lives. A module's path that starts so (and goes on by a folder and a file) binds `app` and `service` |
| `[conjure.port] files` | `["ports/**/*.py"]` | globs under the service for the files holding ports |
| `[conjure.port] start` | `'^class ([[:alnum:]_]+)\(Protocol\):'` | a line starting a port, group 1 its name |
| `[conjure.port] method` | `'^def '` | a stripped line of the port's block that is one of its methods |
| `[conjure.port] add` | `"port-method"` | a generator id: the snippet that adds a method a port lacks. Conjure places it on the line after the port's block (made blank if it is not) and fills its holes `name`, `params` and `returns` with the signature `[types]` gives. Without it, conjure never offers to add a method: the pause asks you to add it |
| `[conjure.port] adapters` | `"port-method-adapters"` | a generator id (optional, needs `add`): run after `add`, to put the method in the port's adapters. It is given, of the inputs it declares: the slots of its `[applies_at] path` matched on the port's file (`app`, `service`, `port` for python-htmx's `src/{app}/{service}/ports/{port}/{port}.py`), the service's `app`/`service`, `name`, `params`, `returns` and `signature` (`[types] method` written out). The model fills the bodies it leaves |
| `[conjure.fake] bound` | `'^[[:space:]]+([[:alnum:]_]+)[[:space:]]*=[[:space:]]*([[:alnum:]_]+)\('` | a test's line binding a fake: group 1 the variable, group 2 the class |
| `[conjure.fake] classes` | `["Mock", "MagicMock"]` | classes that are fakes |
| `[conjure.fake] prefixes` | `["Fake", "InMemory", "Stub"]` | a class starting with one (and longer) is a fake; less the prefix, it names a port |
| `[conjure.fake] keyword` | `"{= param =}={= var =}"` | how a fake is handed to a call by name. Without it, only positional arguments count as handing it in, and conjure cannot hand a fake to a helper |
| `[conjure.fake] stub` | `'\.{= method =} = lambda[[:space:]]*([^:]*):(.*)'` | a fake's stand-in for a method: group 1 its parameters, group 2 what it returns |
| `[conjure.constructor] files` | `["commands/**/*.py", "queries/**/*.py"]` | globs under the service for the siblings whose constructors show what a port's parameter is called (test files left out) |
| `[conjure.constructor] owner` | `'^class ([[:alnum:]_]+)'` | a line starting what holds a constructor, group 1 its name |
| `[conjure.constructor] start` | `'def __init__\('` | where a constructor's parameters start, up to the bracket closing the one it opens |
| `[conjure.constructor] param` | `'^([[:alnum:]_]+)[[:space:]]*:[[:space:]]*([^=]*[^=[:space:]])'` | one parameter: group 1 its name, group 2 its type. One that does not match (`self`, an untyped one) is left out |
| `[[conjure.advice]] when`, `say` | `'^def command\('` and "build the command with the helper, passing the adapters you arranged (…)"; the same for `query` | when a line of the test file matches `when`, `say` is added to the model's instruction for a test's body (the first advice that matches, by line, then by order). python-htmx's tell the model to build a command or query through the stub's helper |

`olifant doctor` shows a row "conjure shapes" per language: the parts a pack
declares (`port, port.add, fake, constructor, advice` for python-htmx), or the
floor. A port without `add` is marked "(no [conjure.port] add: a method a port
lacks is yours to add)".

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
