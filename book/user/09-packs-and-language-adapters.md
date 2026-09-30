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
  regions.toml           owned regions (optional; olifant-dev has one)
  SKILL.md               instructions for AI agents (optional; Olifant itself never reads it)
```

| part | read by | covered in |
|---|---|---|
| `generators/<name>/generator.toml` | every command; the id is `<pack>/<name>` | [chapter 7](07-writing-a-generator.md) |
| `generators/<name>/templates/` | `template = "…"` edits | chapter 7 |
| `generators/<name>/tests/<case>/` | the test suite | [chapter 10](10-testing-generators.md) |
| `fixtures/app/` | `exemplar = "fixture:…"`, `fragment = { file = "fixture:…" }`, tests, `scripts/try-pack.sh` | chapters 7 and 10 |
| `languages/<name>.toml` | `at` phrases, `complete`, `olifant anchors`/`where`, region comments | below |
| `regions.toml` | every `gen` and `sync` | [chapter 5](05-owned-regions.md) |
| `SKILL.md` | agent harnesses | [chapter 11](11-olifant-and-agents.md) |

Only directories exactly one level under `generators/` with a `generator.toml` count; the same
is true of `.olifant/generators/` in a project.

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

### Rough edges

- **Olifant does not look for the project root.** The project is the current directory. Run
  it from `src/` and it finds no `.olifant/config.toml`, so `olifant list` shows only
  user-config packs (or nothing), with no warning.
- **A pack path that does not exist is silent.** `elm = { path = "../nowhere" }` gives an
  empty `list` and "no generator named `elm/page`", not "no such pack directory".

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
  crate imports           e.g. end of crate imports
  impl NAME               e.g. end of impl BillingService
  literal in FUNCTION     e.g. end of literal in new
  struct NAME             e.g. end of struct BillingService

built in (any file):
  before|after marker TEXT
  before|after first line matching REGEX
  before|after last line matching REGEX
```

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
placeholder = 'todo!("{= message =}")'            # the body of a todo branch
branch      = "{= pattern =} => {= body =},"      # how a case branch is written
# qualify   = "elm"                               # Elm's module rules for `complete`

[check]                                           # optional: enables `complete` edits
command = ["elm", "make", "{= files =}", "--report=json", "--output=/dev/null"]
context = ["elm.json"]
format  = "elm-json"

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
| `qualify` | only `"elm"` means anything: rule out constructors a module cannot import, and write patterns the way the file refers to them (`Just Route.Settings` in `Main.elm`) |
| `[check]` | `command` runs in a scratch copy of the project's files (those matching `files`, plus `context`). An argument that is exactly `{= files =}` becomes the list of source files. `format` says how to read the report; `elm-json` is the only format today, so `complete` works for Elm only (Rust's comment says rustc JSON is future work) |
| `[targets.*]` | the `at` targets (below) |

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

`lib.rs` in the rust-htmx fixture is a list of `pub mod` lines, and `service-command` places
its new line with `at = "after last line matching ^pub mod "`. A structural target is
better. Work on a copy of the pack, so nothing in the repo changes:

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
billing/billing-service/src/lib.rs:3
    pub mod refund_invoice;
    pub mod service;
  ▶ (inserted here)

$ olifant where billing/billing-service/src/lib.rs "start of mods"
billing/billing-service/src/lib.rs:1
  ▶ (inserted here)
    pub mod refund_invoice;
    pub mod service;

$ olifant where billing/billing-service/src/service.rs "end of mods"
error: `end of mods`: has no items to insert beside
hint: `olifant anchors <language>` lists the targets
```

A wrong kind is reported by ast-grep, through Olifant:

```
$ olifant where billing/billing-service/src/lib.rs "end of mods"     # with kind = "mod_itm"
error: `end of mods`: ast-grep failed: Error: Cannot parse rule INLINE_RULES
…
╰▻ Kind `mod_itm` is invalid.
```

Check the cases that should *not* match as carefully as the ones that should: a file with no
items, a nested container, a catch-all branch. `where` exits 5 when the phrase does not
resolve, so it is easy to script.

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
