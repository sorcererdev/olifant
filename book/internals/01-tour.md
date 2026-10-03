# 1. A tour of the codebase

This chapter follows one command from `main` to the files it writes:

```sh
scripts/try-pack.sh elm /tmp/olifant-elm && cd /tmp/olifant-elm
olifant gen elm/page --set page=settings --set msgs="toggle_dark_mode"
```

```
elm/page (11 edits, 0 conflicts)
  + src/Page/Settings.elm (42 lines)  [elm/page]
  ~ src/Main.elm (+1 line at line 8)  [elm/page]
  ~ src/Main.elm (+1 line at line 27)  [elm/page]
  ~ src/Main.elm (+1 line at line 44)  [elm/page]
  ~ src/Main.elm (+3 lines at line 79)  [elm/page]
  ~ src/Route.elm (+1 line at line 10)  [elm/page]
  ~ src/Route.elm (+1 line at line 18)  [elm/page]
  ~ src/Page/Settings.elm (+1 line at line 25)  [elm/page › elm/msg[1]]
  ~ src/Page/Settings.elm (+2 lines at line 34, compiler: update)  [elm/page › elm/msg[1]]
  ~ src/Main.elm (+2 lines at line 108, compiler: view)  [elm/page]
  ~ src/Main.elm (+2 lines at line 59, compiler: changeRouteTo)  [elm/page]
```

By the end you should be able to say which function produced each of those lines, and why the
last three come out in that odd order. First, a map of the repository, the units it is split
into, and the ports that keep the core testable.

Olifant is small: about 5,800 lines of library code in `src/`, 3,000 lines of tests next to
it, and a 42-line `app/Main.hs`. Line references (`src/Olifant/Cli.hs:46`) are to the code as
of this chapter; if one is off, the code has moved and the chapter needs fixing.

If the Haskell itself is in your way, read [chapter 2](02-haskell-for-this-codebase.md)
alongside this one. This chapter names the idioms as they come up and chapter 2 explains
them.

## The repository

```
olifant.cabal        the package: one library, one executable, one test suite
cabal.project        `packages: .` and `tests: True`
app/Main.hs          the executable: builds the real adapters, calls Olifant.Cli.runCli
src/Olifant/*.hs     the library, one unit per top-level module
src/Olifant/*Spec.hs the tests, next to the unit they test
src/TestDriver.hs    one line: asks hspec-discover to find every *Spec.hs
packs/               generator packs: elm, rails, redwood, rust-htmx, olifant-dev
book/                these books
scripts/             try-pack.sh, install-olifant.sh, test-watch.sh
.githooks/           post-commit, post-merge, pre-merge-commit
.olifant/config.toml Olifant's own project config: it uses the olifant-dev pack on itself
```

**`olifant.cabal`** declares three components. They share a `common shared` stanza: GHC2021,
the default extensions `OverloadedStrings`, `LambdaCase` and `DerivingStrategies`, `-Wall`
plus stricter warnings, and every dependency. The **library** (`hs-source-dirs: src`) lists
its modules under `exposed-modules`. The **test suite** `spec` also uses `hs-source-dirs:
src` and compiles the library's modules itself instead of depending on the library, so its
`other-modules` lists every module, specs included. The **executable** `olifant` is
`app/Main.hs` and depends on the library.

Both module lists are owned regions ([user book, chapter 5](../user/05-owned-regions.md)):

```cabal
  exposed-modules:
    -- olifant:region library-modules (generated from glob src/**/*.hs; edit the declaration, not this block)
    Olifant.Anchor
    …
    -- olifant:end library-modules
```

The declarations are in `packs/olifant-dev/regions.toml`. Add a module file under `src/`,
run any `olifant gen` or `olifant sync` in the repo, and both lists pick it up. You never
edit those lists by hand. The pre-merge hook runs `olifant sync --check`, so a stale list
can't reach `main`.

**`packs/`** holds real packs, and they are part of the test suite.
`src/Olifant/GeneratorTestingSpec.hs:116` finds every `packs/*/generators/*/tests/*/inputs.toml`,
runs that generator against the pack's `fixtures/app`, and compares the result with the
case's `snapshot/`. A case may also have a `given/` directory, whose files replace or add to
the fixture for that case only: `runCase` (`src/Olifant/GeneratorTesting.hs:51`) reads them
(`:59-60`) and hands them to `preparePack` (`:107`) as an overlay, which wins the `M.union`
(`:120`); command queries still run in the fixture directory on disk. `preparePack` is the
whole setup (open the pack's catalog, load the generator, lay the overlay over the fixture in
a `memoryWorkspace`, build the `Tools`) split out of `runCase` so a spec can run a pack
generator with its own overlay and inputs: the "rust-htmx dependent choices" spec (`:127`)
gives `wire-command` a hand-added `clock` field and expects the error naming billing's
ports. A generation error that is only a missing tool (`generationToolMissing`,
`src/Olifant/Generation.hs:59`: a query whose ast-grep or command program is absent) makes
the case `CaseSkipped` (`GeneratorTesting.hs:68`), like a conflict that is only a missing
tool.

Two more groups in the same spec check that pack output builds, on the fixture with the
case's `given/` and `snapshot/` laid over it (`M.unions [snapshot, given, fixture]`).
**"elm pack output compiles"** (`GeneratorTestingSpec.hs:142`) hands each Elm case's tree to
`compileClean` (`Language/Check.hs:64`), which runs the Elm adapter's `[check]` command and
wants exit 0. **"rust pack output compiles"** (`:160`) is the same code with the rust-htmx
adapter: `packs/rust-htmx/languages/rust.toml`'s `[check]` runs `cargo check --workspace
--exclude diesel-* --message-format=json --target-dir {= cache =}`. `{= cache =}` is expanded
by `runIn` (`Check.hs:82`) to `getXdgDirectory XdgCache "olifant/check/<language>"`
(`~/.cache/olifant/check/rust`), so builds are cached across cases and runs. For that the
fixture is a real cargo workspace: the root `Cargo.toml`'s `members` are the two service
crates (below the plop hook line), each with its real manifest, plus a stub `db` crate so the
Diesel adapters' manifests load; Diesel adapters are excluded, in-memory adapters compiled,
and `main` and `web-htmx` are not members. On failure `compileClean` (`Check.hs:64-77`)
parses the report through the format's `CompilerSupport` and lists the broken files
(`path: message`, one per line); if there are none (a `match` missing an arm is a site, not a
broken file), it falls back to the first 2,000 characters of stderr. A missing `elm` or
`cargo` goes through `missingTool` (`:177`): `pendingWith` locally, `expectationFailure` under
`OLIFANT_REQUIRE_TOOLS=1`. Because every case is checked against it, the rust-htmx fixture
itself compiles, and a case that needs an unwired hand edit (an `email_sender` field typed
into `cancel_invoice.rs`) carries it in `given/`.

**`packs/olifant-dev`** is the pack Olifant uses on its own source. Its
`unit` generator creates `src/Olifant/X.hs` plus `src/Olifant/XSpec.hs`, and its
`internal-module` generator creates `src/Olifant/X/Y.hs`.

**`scripts/`**:
- `try-pack.sh PACK DIR` copies a pack's fixture app into DIR, configures the pack and
  commits it to a fresh git repo. Every example in these books runs in a sandbox like that.
- `install-olifant.sh` runs `cabal install` into `~/.local/bin`.
- `test-watch.sh` runs `ghcid` over the test suite and re-runs it on every save.

**`.githooks/`** (enable with `git config core.hooksPath .githooks`):
- `post-commit` and `post-merge` reinstall `olifant` when you are on `main`.
- `pre-merge-commit` runs `cabal test` (with `OLIFANT_REQUIRE_TOOLS=1`, and
  `~/.ghcup/bin` and `/opt/homebrew/opt/rustup/bin` on `PATH`, so a missing `ast-grep`,
  `elm` or `cargo` fails the merge) and `olifant sync --check` before a merge into `main`
  completes.

## The units

Each top-level module in `src/Olifant/` is a **unit**: one job, named for what it does. A
unit is either a single file (`Naming.hs`) or a **facade** module plus internal modules in a
directory of the same name (`Generation.hs` plus `Generation/*.hs`). Other units import the
facade. The facade decides what is public, usually by re-exporting its internal modules
(`module Olifant.Generation.Plan` in an export list re-exports that module's exports). Each
unit has a colocated `XSpec.hs` that tests it through the facade.

From the bottom up:

| unit | what it is for | public face | imports units |
|---|---|---|---|
| **Naming** | Identifiers as lists of words, rendered in any case | `Naming.hs`: `Name`, `parseName`, `render`, `Case`, `pluralize`, `singularize` | none |
| **Toml** | TOML parsed into a small `Tree`, plus decoders with readable errors; the only module that knows the `toml-parser` API | `Toml.hs`: `Tree`, `Table`, `parseToml`, `keyOrder`, `required`, `optional`, `inKey`, `text`, `list`, `table`, `onlyKeys` | none |
| **Glob** | `**` / `*` path globs | `Glob.hs`: `matchesGlob`, `globPrefix` | none |
| **Template** | The `{= … =}` language: parse, render, exemplars | `Template.hs` over `Template/{Syntax,Parse,Render,Value,Exemplar,Error}`: `Template`, `parseTemplate`, `render`, `evaluate`, `Value`, `Env`, `exemplar`, `freeVariables` | Naming |
| **Language** | Language adapters from packs (`languages/*.toml`), ast-grep rules, and the **Syntax** and **Checker** ports | `Language.hs` re-exports `Adapter`, `Rule`, `Syntax`, `Check`, `Diagnostics`, `Support`: `Language`, `parseLanguage`, `languageFor`, `Rule`, `renderRule`, `Syntax`, `noSyntax`, `Checker`, `noChecker`, `processChecker`, `compileClean`, `CompilerSupport`, `typeNames`, `supportFor`. `Language.Elm`, `Language.Ghc` and `Language.Rustc` (one `CompilerSupport` record each, spec §5.6) and `Language.AstGrep` (the ast-grep adapter) are internal; only `AstGrep` is imported directly, by `Main` | Glob, Template, Toml |
| **Process** | Run an external program with byte stdin/stdout/stderr; never throws | `Process.hs`: `runBytes` | none |
| **Workspace** | The filesystem **port**, with disk and in-memory adapters | `Workspace.hs`: `Workspace`, `PathState`, `pruned`, `diskWorkspace`, `memoryWorkspace`, `within`, `normalisePath`, `isSafeRelative` | Process (to run `git`) |
| **Query** | Facts from the repo as records (globs, commands printing JSON, or AST queries selecting nodes), the **Queries** port, and what an enum's choice is | `Query.hs`: `Query`, `AstQuery`, `AstSelect`, `Record`, `runQuery`, `renderQuery`, `eachFields`, `Queries`, `QueryFailure`, `failed`, `noQueries`, `processCommand`, `ChoiceValue`, `choiceValue`, `queryVariables` | Glob, Language (`Rule`), Naming, Process, Template, Toml |
| **Anchor** | `at` phrases and where they put text, plus AST queries over a target's items | `Anchor.hs` re-exports `Anchor/Phrase`, `Anchor/Resolve` and `Anchor/Items`: `parsePhrase`, `resolveLayers`, `Located`, `Opening`, `Spacing`, `Miss`, `insertLines`, `insertedText`, `placedText`, `presentAt`, `locateContainer`, `itemsOfTarget`, `astQueries` | Language, Query, Template |
| **Generator** | A generator definition: the types, loading `generator.toml`, resolving `one_of` choices, turning `--set` values into a template environment | `Generator.hs` re-exports `Definition` (including `ResolveFailure`, why a `[[uses]]` ref or step didn't resolve, and `Choices`) and `Inputs` (including `envBefore`, `inputsUsed`), plus `parseGenerator`, `withChoices`, `needsChoices`, `ChoiceView`, `choiceViews` | Anchor, Language, Naming, Query, Template, Toml |
| **Region** | Owned regions: parse `regions.toml`, render a region body, splice it between fences. All pure | `Region.hs`: `RegionDefWith`, `parseRegions`, `renderRegion`, `applyRegion`, `fenceLines` | Query, Template, Toml |
| **Generation** | Running generators: expand `[[uses]]`, bind `[query.NAME]` queries, render edits, plan them, check, refresh regions, write | `Generation.hs`: `generate`, `Step`, `Mode`, `Resolve`, `describeGenerationError`, `refreshRegions`; re-exports `Generation/Plan` (the `Plan` type) and `Generation/Tools`. Internals: `Compose`, `Render`, `Planner`, `Complete`, `Regions` | Anchor, Generator, Glob, Language, Query, Region, Template, Workspace |
| **Catalog** | Finding packs and generators, checking a generator's sources and fixed `at` targets, and loading one into a runnable `Generator` | `Catalog.hs`: `openProjectCatalog`, `findEntry`, `loadGenerator`, `checkEntry`, `resolver`, `Catalog`, `CatalogEntry` | Anchor, Generator, Language, Region, Template, Toml, Workspace |
| **Cli** | The command line: parse arguments, call the use cases, print JSON or text, pick the exit code | `Cli.hs`: `Runtime`, `runCli`. Internals: `Cli/Options` (argument parsing), `Cli/Output` (rendering) | almost everything |
| **GeneratorTesting** | Running a pack's test cases against its fixture (the seed of `olifant test`) | `GeneratorTesting.hs`: `discoverCases`, `runCase`, `preparePack` | Anchor, Catalog, Generation, Generator, Language, Query, Toml, Workspace |

`GeneratorTesting` isn't used by the CLI yet. Only `GeneratorTestingSpec` calls it.

### Layering

This diagram is derived from the `import` lines (`grep '^import Olifant\.' src/Olifant/**/*.hs`).
Each row imports only from rows below it. There are no imports sideways within a row, and
none upwards.

```
  ┌────────────────────────────────────────────────────────────┐
  │ app/Main.hs                                                │  wires real adapters
  ├────────────────────────────────────────────────────────────┤
  │ Cli                         GeneratorTesting               │  entry points
  ├────────────────────────────────────────────────────────────┤
  │ Catalog                                                    │  finding and loading generators
  ├────────────────────────────────────────────────────────────┤
  │ Generation                                                 │  the use case: steps → plan → writes
  ├────────────────────────────────────────────────────────────┤
  │ Generator                   Region                         │  definitions (generator.toml, regions.toml)
  ├────────────────────────────────────────────────────────────┤
  │ Anchor                                                     │  placing text (`at` phrases, AST queries)
  ├────────────────────────────────────────────────────────────┤
  │ Query                       Workspace                      │  reading facts, files
  ├────────────────────────────────────────────────────────────┤
  │ Language                                                   │  adapters as data; Syntax and Checker ports
  ├────────────────────────────────────────────────────────────┤
  │ Template                                                   │  {= … =}
  ├────────────────────────────────────────────────────────────┤
  │ Naming         Toml         Glob                           │  no Olifant imports
  └────────────────────────────────────────────────────────────┘
```

The exact edges, one line per unit:

```
Naming, Toml, Glob, Process → (nothing)
Template             → Naming
Language             → Glob, Template, Toml
Workspace            → Process             (for `git ls-files`)
Query                → Glob, Language, Naming, Process, Template, Toml     (Language.Rule, Workspace.pruned)
Anchor               → Language, Query, Template
Region               → Query, Template, Toml
Generator            → Anchor, Language, Naming, Query, Template, Toml
Generation           → Anchor, Generator, Glob, Language, Query, Region, Template, Workspace
Catalog              → Anchor, Generator, Language, Region, Template, Toml, Workspace
Cli                  → Anchor, Catalog, Generation, Generator, Language, Query, Region, Template, Workspace
GeneratorTesting     → Anchor, Catalog, Generation, Generator, Language, Query, Toml, Workspace
app/Main             → Cli, Language, Language.AstGrep, Workspace
```

`Anchor` importing `Query` is the one edge worth pausing on: an `at` target and an `ast`
query both end up asking the same question ("what are this container's items?"), so
`itemsOfTarget` (`Anchor/Resolve.hs:190`) is shared, and `Anchor.Items.astQueries` builds
`Record`s (`Query`'s type) from what it finds. `Query` itself has no idea `Anchor` exists.

One thing to notice:

- **Generation does not import Catalog.** Generation has to load the generators named in
  `[[uses]]`, but it never looks them up itself. It takes a function
  (`type Resolve m = Maybe Text -> Text -> m (Either ResolveFailure Generator)`,
  `src/Olifant/Generation/Compose.hs:38`), and `Cli` passes in `Catalog.resolver`. This is the
  same dependency inversion as the ports below, applied to a single function. `ResolveFailure`
  itself lives with the generator definition (`Generator/Definition.hs`), so `Catalog` needs
  no import of `Generation` at all to produce one; `Generation.Compose` and the `Generation`
  facade just re-export it, for callers that already import them.

A module outside a unit normally imports only that unit's facade. These are the exceptions
(found with `grep -n 'import Olifant\.[A-Za-z]*\.[A-Z]'`):

```
app/Main.hs:9                          import Olifant.Language.AstGrep (astGrepSyntax)
src/Olifant/Cli.hs:23                  import Olifant.Generator.Schema (generatorSchema)
src/Olifant/Query.hs:46                import Olifant.Language.Rule (Rule (..), renderRule, ruleFromTree)
```

`Main` importing an adapter directly is fine: wiring adapters together is `Main`'s job.
`Cli` is the only user of the JSON Schema `olifant schema` prints, so the `Generator` facade
does not re-export it.
`Generation/Complete.hs` used to be a third exception here: it imported `Olifant.Language.Elm`
directly and branched on `langQualify lang == Just "elm"` to decide whether to qualify a
pattern. Step 2c (compiler support, below) closed that gap — the check loop now goes through
a `CompilerSupport` record the adapter's `[check] format` selects, and `Generation/Complete.hs`
imports nothing from `Language.Elm` or `Language.Ghc` at all.

## Ports and adapters

Olifant touches four things outside its own memory: the filesystem, ast-grep, a language's
compiler, and arbitrary commands (for queries; AST queries reuse ast-grep through the Syntax
port instead of touching anything new). Each is a **port**. A port is a record of
functions, parameterised by the monad `m` it runs in:

```haskell
-- src/Olifant/Workspace.hs:29
data Workspace m = Workspace
  { readText :: FilePath -> m (Maybe Text)
  , writeText :: FilePath -> Text -> m ()
  , fileExists :: FilePath -> m Bool
  , listFiles :: FilePath -> m [FilePath]
  , inspectPath :: FilePath -> m PathState
  , removeFile :: FilePath -> m ()
  }
```

`removeFile` is the newest field: delete a file if it exists. Generation never calls it —
writing is still additive, spec §5.8 aside — but blessing a pack test's snapshot does, so a
case that stops generating a file doesn't leave its old snapshot behind (chapter on testing
generators, and below).

```haskell
-- src/Olifant/Language/Syntax.hs:15
newtype Syntax m = Syntax {querySyntax :: Language -> Value -> Text -> m (Either SyntaxFailure [Match])}

-- src/Olifant/Language/Check.hs:36
newtype Checker m = Checker {runCheck :: Language -> Map FilePath Text -> m (Either CheckFailure CheckReport)}

-- src/Olifant/Query.hs:186
data Queries m = Queries
  { queryCommand :: [Text] -> m (Either QueryFailure ByteString)
  , queryAst :: AstQuery -> [FilePath] -> (FilePath -> m (Maybe Text)) -> m (Either QueryFailure [Record])
  }

-- src/Olifant/Query.hs:178
data QueryFailure = QueryFailure {failureWhy :: Text, failureToolMissing :: Bool}
```

In Rust you would write a trait (`trait Workspace { fn read_text(&self, p: &Path) -> … }`)
and pass `&dyn Workspace` or a generic `W: Workspace`. Here the "trait object" is a plain
value: a record whose fields are functions. You build one by filling in the fields, and you
call one with `readText ws path` (the field name is a function that takes the record
first). The `m` parameter is the part Rust has no direct equivalent for. The same code can
run in `IO` in production and in a pure monad in a test. [Chapter 2](02-haskell-for-this-codebase.md#monads-and-monad-m-)
explains how.

`Queries` is a `data`, not a `newtype`, because it has two fields instead of one: running a
`command` query and evaluating an `ast` query are different capabilities, but both need to
reach outside Olifant's memory, so they travel together in one port. Both fail with a
`QueryFailure`: the message, and whether the only problem is a missing tool. `failed why`
(`:181`) is the ordinary kind; `processCommand` sets `failureToolMissing` when the program is
not found (`` `cargo` is not installed ``), and `astQueries` when the Syntax port says
ast-grep is missing (`Anchor/Items.hs:44`, `:47`). Callers that add context update only the
message (``f {failureWhy = "query `" <> name <> "`: " <> failureWhy f}``), so the flag survives
to `generationToolMissing` (`Generation.hs:59`), which pack tests use to make such a case
pending, as a missing tool is everywhere else (spec §4.5, §5.7).

| port | defined at | real adapter | no-op / test adapters |
|---|---|---|---|
| `Workspace m` | `Workspace.hs:29` | `diskWorkspace` (`Workspace.hs:72`): reads and writes under a root; lists files with `git ls-files` when inside a git work tree, otherwise walks the tree. `within dir` (`:131`) views a workspace from a subdirectory | `memoryWorkspace` (`Workspace.hs:45`): a `Map` in an `IORef`, which also hands back an action that reads the final map, so tests can assert on "the disk" |
| `Syntax m` | `Language/Syntax.hs:15` | `astGrepSyntax` (`Language/AstGrep.hs:33`): runs `ast-grep scan --stdin --inline-rules … --json=compact`; compiles a pack's vendored grammar with `cc` into the cache on first use | `noSyntax` (`Language/Syntax.hs:17`) always says "ast-grep is not installed"; `fakeSyntax :: Syntax Identity` in `AnchorSpec.hs:58` returns canned matches |
| `Checker m` | `Language/Check.hs:36` | `processChecker` (`Check.hs:48`): copies the files into a temp dir, runs the adapter's `[check] command` there, and parses the report (stderr, or stdout when the support's `supportReadsStdout` says so, as cargo's does) through the `CompilerSupport` its `format` names (`elm-json`, `ghc-json`, `rustc-json`) | `noChecker` (`Check.hs:38`); `scriptedChecker` (`GenerationSpec.hs:541`) replays a list of reports, one per round |
| `Queries m` | `Query.hs:186` | `processCommand dir` (`Query.hs:200`): runs a program (a bare name from `PATH`, or a project path like `bin/list`) in the project dir, for `queryCommand`; `astQueries syntax langs` (`Anchor/Items.hs:20`): evaluates an `ast` query's target over every matching file, for `queryAst` | `noQueries` (`Query.hs:193`); `QuerySpec.hs:46`'s `fake out = noQueries {queryCommand = …}` overrides one field and leaves `queryAst` as the no-op |

The ports are bundled at two levels. `Tools m` (`src/Olifant/Generation/Tools.hs:10`) holds
Syntax, Checker and Queries, plus the catalog's language adapters and regions, and it is what
`generate` takes. `noTools` (`:18`) needs nothing installed. `Runtime`
(`src/Olifant/Cli.hs:32`) is the CLI's port: the root workspace, the project dir, stdin, the
clock, stdout, stderr, whether stdout is a terminal, and the Syntax and Checker adapters.
`app/Main.hs` is the only place that builds a real one:

```haskell
-- app/Main.hs:24
  result <-
    try . runCli
      Runtime
        { rtRoot = diskWorkspace "/"
        , rtProjectDir = cwd
        , rtUserConfigDir = Just userConfigDir
        , rtHomeDir = Just home
        , rtReadStdin = TIO.getContents
        , rtNow = getCurrentTime
        , rtStdoutIsTty = tty
        , rtOut = TIO.putStr
        , rtErr = TIO.hPutStr stderr
        , rtSyntax = astGrepSyntax cache
        , rtChecker = processChecker
        }
      $ args
```

`CliSpec.hs:50` (`runWith`) builds the test version: `memoryWorkspace` for the root, output
collected in `IORef`s, a fixed clock, `noSyntax` and `noChecker`. Every CLI behaviour, from
exit codes and JSON shapes to `--steps -` reading stdin, is tested without a process, a
terminal or a disk.

**Why tests use fakes.** Speed is one reason: the CLI spec's 57 examples run in under 10 ms.
The main reasons are that a fake can produce situations the real tool rarely does and that
the test says exactly what it assumes. `scriptedChecker` returns "missing `Reset`" and then
the same thing again, which is how `completeEdits` is tested to stop instead of adding the
same branch forever. `fakeSyntax` returns a nested match, which is how the "direct children
only" rule in anchor resolution is tested. The real adapters are still tested, but
separately: `LanguageSpec.hs:277` runs real `elm make`, `LanguageSpec.hs:93` real
`ast-grep`, and the pack snapshot tests run both (plus real `cargo check` on rust-htmx
output). Those tests become *pending*, not failed, when the tool isn't installed (`conflictToolMissing`, `src/Olifant/Generation/Plan.hs:93`).

## One `gen`, end to end

Back to:

```sh
olifant gen elm/page --set page=settings --set msgs="toggle_dark_mode"
```

Here is the `elm/page` generator in brief (`packs/elm/generators/page/generator.toml`):
- inputs `page` (an ident) and `msgs` (an optional list of `name:arg` records)
- a derived value `P = "{= page|pascal =}"`
- `[requires] exists = ["src/Main.elm", "src/Route.elm"]`
- one `create` from an exemplar (`fixture:src/Page/SamplePage.elm`, bound `page =
  "sample_page"`)
- six `insert`s: one `after last line matching ^import Page\.` and five structural ones
  such as `end of type Page` and `end of list in parser`
- two `complete` edits: `Route.{= P =}` in `case in changeRouteTo`, and `Main.{= P =}` in
  `case in view`
- one `[[uses]]`, which runs `msg` once per item of `msgs`

`elm/msg` has one `insert` (`end of type Msg`) and one `complete` (`{= Msg =}` in `case in
update`), and it requires the page file to exist.

### 1. `main` builds the world (`app/Main.hs:16`)

`main` reads the arguments, the current directory, whether stdout is a terminal, and the
home, XDG config and XDG cache directories. It builds the `Runtime` shown above and calls
`runCli`. `try` catches any exception that escapes. That should never happen, and it's
reported as `olifant: internal error: …` with exit 1 (`:42`). Otherwise `runCli`'s
`ExitCode` becomes the process exit code.

The workspace is rooted at `/` (`diskWorkspace "/"`), and paths inside Olifant are absolute
until something narrows them with `within`.

### 2. `runCli` finds the project, then parses and opens the catalog (`src/Olifant/Cli.hs:46`)

```haskell
-- src/Olifant/Cli.hs:46
runCli :: Runtime -> [String] -> IO ExitCode
runCli rt args = case parseOptions args of
  Left (text, ExitSuccess) -> rtOut rt text >> pure ExitSuccess
  Left (text, _)
    | wantsJson -> rtOut rt (encodeJson (errorJson "usage" (T.strip text) []) <> "\n") >> pure (ExitFailure 2)
    | otherwise -> rtErr rt text >> pure (ExitFailure 2)
  Right opts -> do
    let fmt = fromMaybe (if rtStdoutIsTty rt then Human else Json) (optFormat opts)
        cwd = rtProjectDir rt
    root <- projectRoot (rtRoot rt) (rtHomeDir rt) cwd
    let rt' = rt {rtProjectDir = root}
    openProjectCatalog (rtRoot rt') (CatalogPaths root (rtUserConfigDir rt') (rtHomeDir rt')) >>= \case
      Left err -> catalogFailure rt' fmt err
      Right cat -> runCommand rt' cwd fmt cat (optCommand opts)
```

**`parseOptions`** (`src/Olifant/Cli/Options.hs:75`) returns `Either (Text, ExitCode)
Options`: either text to print with an exit code, or parsed options. Most commands go
through `optparse-applicative` (`:83`). `gen` does not. It's matched first (`:76`) and
handed to the hand-written `parseGen` (`:115`), because optparse can't express "steps
separated by a standalone `+`". `parseGen` is a loop, `go`, over the remaining words. It
peels off the global flags (`--json`, `--human`, `--plan`, `--now`, `--steps`) wherever they
appear, and starts a new step at each `"+"` (`:131`). `finish` (`:133`) turns each step into
a `StepSpec`. The optparse definition of `gen` (`genP`, `:103`) exists only to render `gen
--help`. For our command the result is, in effect:

```
Options Nothing (GenCmd (GenOptions [StepSpec "elm/page" [("page","settings"),("msgs","toggle_dark_mode")]] Nothing False Nothing))
```

That is: no format flag, one step, no `--steps` file, not `--plan`, no `--now`.

The **output format** is decided once (`Cli.hs:53`): the flag if given, otherwise human
output for a terminal and JSON when piped.

**`projectRoot`** (`Cli.hs:66`) walks up from `rtProjectDir rt` — the directory Olifant was
actually run from — looking for a `.olifant`, stopping once it reaches the home directory
(or the filesystem root). `cwd` keeps the original directory: `runCommand` uses it to resolve
file arguments (`where FILE`, `--steps FILE`) exactly where you typed them, while `rt'`
carries the found root for everything else (the catalog, `gen`'s writes, `sync`). With no
`.olifant` anywhere above it, `projectRoot` returns `start`, so behaviour is unchanged from
before this existed.

**`openProjectCatalog`** (`src/Olifant/Catalog.hs:102`) runs for every command, rooted at
whatever `projectRoot` found. It reads `[packs]` from `~/.config/olifant/config.toml` and
`.olifant/config.toml`. The project's entry wins for the same name
(`M.fromList (fromUser ++ fromProject)`, `:112`: a later key overwrites an earlier one). Then
`openCatalogAt` (`:139`) checks every configured pack path exists (`inspectPath`, `:141`)
before doing anything else — the first one that is not a directory fails the whole command
with `PackMissing` (`config_invalid`, exit 1), rather than silently acting as if the pack had
no generators. Once every pack checks out, `openAt` (`:146`) goes through each one:

- `discover` (`:168`) lists the files and keeps `generators/<name>/generator.toml`, exactly
  two levels deep. `load` (`:172`) parses each one with `parseGenerator`
  (`src/Olifant/Generator/Load.hs:30`) into a `GeneratorDef`. `[inputs.*]` is a TOML table,
  which `Toml.parseToml` reads into a `Map` that forgets the order keys were written in — so
  `parseGenerator` separately asks `Toml.keyOrder src ["inputs"]` (`Toml.hs:52`) for that
  order straight from the parse tree, and sorts `genInputs` by it (`Load.hs:39-40`). That is
  the only reason `show` and a generator's prompts list inputs in declaration order rather
  than whatever order the `Map` happens to iterate in.
- `languagesIn` (`Catalog.hs:156`) parses `languages/*.toml` into `Language` values with
  `parseLanguage` (`src/Olifant/Language/Adapter.hs:50`).
- `regionsIn` (`Catalog.hs:300`) reads `regions.toml`.

A generator, language or regions file that fails to parse is kept aside (`catalogBroken`,
`catalogBrokenLanguages`, `catalogBrokenRegions`) instead of failing the command. One bad
generator never breaks unrelated ones. The project's own `.olifant/generators/` and
`.olifant/regions.toml` are added the same way.

So every invocation parses every generator in every configured pack. That's cheap at
today's sizes, but it is also why a typo in one pack's TOML shows up in `olifant list` for
all of them.

### 3. `runCommand` loads the generator (`src/Olifant/Cli.hs:122`)

`runCommand` now takes `cwd` (where Olifant actually ran) alongside `rt` (whose
`rtProjectDir` is the root `projectRoot` found). The `GenCmd` branch first gets its steps,
from the arguments or from `--steps FILE|-` (JSON, decoded by the `FromJSON StepSpec`
instance at `Options.hs:44`; `-` reads stdin, and a plain filename reads relative to `cwd`,
not the project root — a `--steps` file is as much "where you ran it" as the thing Olifant
reads for you). Then it loads each step:

```haskell
-- src/Olifant/Cli.hs:213
        loaded <- traverse (\(i, s) -> fmap (first (label i s,)) (either (pure . Left) (loadGenerator (rtRoot rt) (rtSyntax rt) cat) (findEntry cat (specGenerator s)))) (zip [1 :: Int ..] ss)
        case sequence loaded of
```

Read from the inside out:

1. **`findEntry cat "elm/page"`** (`Catalog.hs:183`) looks for an exact id first. Failing
   that, it looks for a bare name that is unique across packs (`page` would work). Then it
   checks the broken list, and only then reports "not found" or "pack not configured".
2. **`loadGenerator`** (`Catalog.hs:203`) turns the `GeneratorDef`, whose edit sources are
   still file references, into a runnable `Generator`, whose sources are parsed templates.
   It also needs the Syntax port now, because a fragment exemplar (below) has to search a
   file's syntax tree at load time, not just render text. It's still one line:

   ```haskell
   -- src/Olifant/Catalog.hs:203
   loadGenerator :: Monad m => Workspace m -> Syntax m -> Catalog -> CatalogEntry -> m (Either CatalogError Generator)
   loadGenerator root syntax cat entry = sequenceA <$> traverse resolve (entryDefinition entry)
   ```

   `resolve` (`:207`) handles each source:
   - `TemplateFile f` reads `templates/f` and parses it.
   - `ExemplarFile ref binds` reads the exemplar (`fixture:…` means the pack's
     `fixtures/app/`) and turns it into a template with `exemplar`
     (`src/Olifant/Template/Exemplar.hs:37`). That function replaces every case form of
     `sample_page` (`SamplePage`, `samplePage`, `SAMPLE_PAGE`, …) with the matching filter
     applied to `page`.
   - `FragmentFile ref target matching binds` (spec §4.3) is an exemplar cut out of a bigger
     file instead of read whole. It reads the same kind of file reference as `ExemplarFile`,
     finds the `target`'s items with `itemsOfTarget` (`Anchor/Resolve.hs:190`, the same
     function `end of`/`start of` anchors and `items_of` AST queries use), requires exactly
     one of them to match the regex `matching`, dedents it by the node's starting column
     (`dedent`, `Catalog.hs:229`, since only its first line sits at that column in the
     source), then turns the result into a template with `exemplar` just like `ExemplarFile`
     does.
   - `InlineText t` is already a template.

   `traverse` over a *generator* is possible because `GeneratorWith` derives `Traversable`.
   That trick gets its own section in [chapter 2](02-haskell-for-this-codebase.md#the-traversable-trick-same-shape-sources-then-templates).

   `loadGenerator` is also what `checkEntry` (`Catalog.hs:255`) calls first: `list` and `show`
   run it over every entry before anyone asks to `gen` it, so a missing template, a broken
   exemplar or fragment, or a fixed `at` target the language adapter doesn't have, shows up
   as `INVALID` (or fails `show`/`gen` the same way) instead of surfacing only at run time. A
   target phrase with a `{= … =}` in it can't be checked until its value is known, so those
   are still left to run time (`targetProblem`, `:264`).
3. `traverse` over the steps, then **`sequence loaded`**, turns "a list of results" into "the
   first error, or a list of generators", like `.collect::<Result<Vec<_>, _>>()`.

`one_of` enums (inputs whose choices come from a `[query.NAME]` over the project, spec
§4.1) are not resolved here: `generate` resolves them step by step during composition
(`Choose` in `Generation/Compose.hs:58`), against the project plus the files created by the
steps before, and with that step's own `--set` values, so a chain can choose what an earlier
step creates and a dependent input sees the service its step chose. `generate` lists the
project only if an input needs it (`needsChoices`). `elm/page` has none, so for this command
it is a no-op. (`show` and `list --detail` use the CLI's `resolveChoicesFor` and
`choiceViewsFor`, `Cli.hs:295` and `:305`, against the project as it is, which is what they
describe.)

#### How a `one_of` resolves

An enum's choices go through three states, in the input's type (`Generator/Definition.hs:112`):

```haskell
data Scalar = IdentS | TextS | BoolS | EnumS [Text] | EnumOf Text ChoiceValue Choices
data Choices = Unresolved | Resolved [Text] [(Text, Text)] | NeedsInputs [Text]
```

`EnumOf q cv c` is the query's name, which part of each record is a choice, and the state.
**Loading** (`parseGenerator`, `Generator/Load.hs:30`) builds `EnumOf q FileName Unresolved`
from `one_of` (`scalarFrom`, `:149`; a `choices_from` key is the "retired" error, `:146`),
then `fillChoice` (`:68`) copies the `value` from `[query.q]`, which `queryWithValue` (`:63`)
reads with `choiceValueFrom` (`:75`): `"file name"`, `"parent folder"`, `"path"`, a field
name, or a template, as a `ChoiceValue` (`Query.hs:145`), defaulting to `FileName` for a glob
and `FieldValue "name"` otherwise. A missing query is an error there too (`:70`). The order
rule is checked at load (`:52-60`): `queryVariables` (`Query.hs:168`) collects every variable
in the query's templates, `inputsUsed` (`Generator/Inputs.hs:211`) follows them through
`[derive]` to the inputs they are built from (`service_dir` → `service`), and an input
declared at or below the one the query serves is refused, naming both.

**Resolving** is `withChoices` (`Generator/Choices.hs:25`), a `foldM` over the input names in
declaration order, so each input sees the resolved choices of those above it. For an
`EnumOf q cv Unresolved` it builds the query's environment with `envBefore`
(`Inputs.hs:60`): the inputs above it that parse (an enum against its resolved choices), `now`,
any `--set` override of a derived value, and the derived values that then evaluate. It
works out which inputs (and overridden derived values) the query needs; if one has no value,
the input becomes `NeedsInputs` and waits. Otherwise `renderQuery` and `runQuery` run it,
`choiceValue` (`Query.hs:149`) turns each record into a choice, and the input becomes
`Resolved choices context`, where the context is the `(input, raw value)` pairs the query
used. A failed query's `QueryFailure` gets the input, the query and the context prefixed to
its message (`` `dependency`: query ports (for service=billing) failed: … ``); its
`failureToolMissing` flag is kept.

**Parsing a `--set`** against those states is `parseScalar` (`Inputs.hs:101`), and the
context is what makes its messages specific: `NeedsInputs` is "its choices depend on
`service`; set it first", `Resolved [] ctx` "the query `ports` found nothing for
service=notifications", and a wrong value "expected one of … (the ports of
service=billing)", rendered by `showContext` (`:219`). `buildEnv` (`:35`) drops a "depends on"
problem when the input it waits for was given but is itself invalid (`waitsOnTrouble`,
`:44`): that input's own error says it all.

**Showing** is `choiceViews` (`Choices.hs:65`). After one `withChoices` with the `--set`
values given to `show`, every enum gets a `ChoiceView` (`:59`):

```haskell
data ChoiceView = Flat [Text] | Grouped [Text] [([(Text, Text)], Either Text [Text])] | DependsOn [Text]
```

`Flat` when its choices resolved; for `NeedsInputs ns` where every `n` is an enum with known
choices and the product of their counts is at most `maxGroups` (`:92`, 30), `Grouped`: one
more `withChoices` per combination, laid over the `--set` values, each giving the choices or,
when its query failed, the reason; otherwise `DependsOn`. `Cli/Output.hs` prints them:
`choiceLines` (`:91`) as `service=billing:  a | b`, `(none)`, `query failed: …` or "depends
on service; set it to see the choices", and `inputsJson` (`:73`) as `choices`, or
`depends_on` with `choices_by` (keyed `billing`, or `a=x, b=y` for several inputs) and
`errors`.

Then the call into the core:

```haskell
-- src/Olifant/Cli.hs:220
            let mode = if genPlanOnly g then PlanOnly else Apply
                steps = zipWith (\gen s -> Step gen (M.fromList (specSets s))) gens ss
            generate (within (rtProjectDir rt) (rtRoot rt)) (resolver (rtRoot rt) (rtSyntax rt) cat) (projectTools rt cat) steps now mode >>= \case
```

The arguments, in order:
- the workspace narrowed to the project root (`rtProjectDir rt` here, since `rt` is the `rt'`
  `runCli` rebound — see step 2), so paths are now project-relative, regardless of `cwd`
- `Catalog.resolver`, used for `[[uses]]` (a used generator's `one_of` is resolved by
  `generate`'s `choose`, like a top-level step's)
- the `Tools`, built by `projectTools rt cat` (`Cli.hs:358`): the real Syntax and Checker,
  the catalog's languages and regions, and `projectQueries rt cat` (`:363`) — `queryCommand
  = processCommand (rtProjectDir rt)`, `queryAst = astQueries (rtSyntax rt) (catalogLanguages
  cat)`
- the steps
- the time (`--now` or the clock)
- `Apply`

### 4. `generate`: from steps to a plan (`src/Olifant/Generation.hs:76`)

```haskell
generate :: Monad m => Workspace m -> Resolve m -> Tools m -> [Step] -> UTCTime -> Mode -> m (Either GenerationError Plan)
```

`generate` is the one use case at the centre of Olifant. It has five stages.

#### 4a. Expand the steps into plan items (`Generation/Compose.hs`)

`expandSteps` (`Compose.hs:67`) calls `expand` for each top-level step. `expand` runs in
`ExceptT CompositionError m`, a monad in which any line can fail with a composition error
and stop the rest (chapter 2 has a section on it):

```haskell
-- src/Olifant/Generation/Compose.hs:81
expand resolve bindQ choose now prior ancestors path gen0 sets = do
  when (genId gen0 `elem` ancestors) $ throwE (StepCycle path)
  checkCycles resolve (genId gen0 : ancestors) path gen0
  gen <- ExceptT (first (StepQuery path) <$> choose prior sets gen0)
  env0 <- except (first (StepInputs path) (buildEnv gen sets now))
  env <- if null (genQueries gen) then pure env0 else ExceptT (first (StepQuery path) <$> bindQ env0 (genQueries gen))
  rendered <- except (first (StepTemplate path) (renderEdits (genLanguage gen) env (genEdits gen)))
  (required, hint) <- except (first (StepTemplate path) (renderRequires env (genRequires gen)))
  let own = map (PlanEdit path) rendered
  -- Each use sees what the run, this generator and earlier uses create.
  nested <- foldM (\acc (k, u) -> (acc ++) <$> expandUse env (prior ++ created (own ++ acc)) k u) [] (zip [1 :: Int ..] (genUses gen))
  pure ([PlanRequire path required hint | not (null required)] ++ own ++ nested)
```

- **`choose`** resolves the generator's `one_of` enums for this step (above), given `prior`,
  the files the run has created so far, and the step's `sets`. Only then can its inputs be
  checked against their choices.
- **`buildEnv`** (`src/Olifant/Generator/Inputs.hs:35`) checks the `--set` values against the
  declared inputs. It reports *every* missing, invalid or unknown input at once, which is
  exit code 4 with a list. It parses each value by type, adds `now`, and evaluates the
  `[derive]` values in dependency order (`deriveAll`, `:156`). Here the environment ends up
  with `page` as a name (`["settings"]`), `msgs` as a list of one record `{name =
  ["toggle","dark","mode"], arg = ""}`, `now`, and `P = "Settings"`.
- **`bindQ`** binds the generator's own `[query.NAME]` queries (`genQueries`, spec §4.5) into
  the environment, if it has any (`elm/page` doesn't, so this step is skipped here).
  `BindQueries m` (`Compose.hs:53`) is another dependency-inverted function, like `Resolve
  m`: `Cli`/`GeneratorTesting` pass in `generate`'s real `bindQueries` (`Generation.hs:129`),
  which renders each query's text against the environment so far (`renderQuery`,
  `Query.hs:254`) and runs it over the project's files (`runQuery`, `Query.hs:222`), reading
  through the same plan-aware view every phase uses. A query's records are bound as a
  `VList` under its name, so a template can loop over it. For example (`GenerationSpec.hs:83`):
  `[query.mods] glob = "{= dir =}/*.hs"` makes `mods` a list of records, one per matching
  file, and `{= for m in mods =}{= m.stem =}\n{= end =}` lists their stems. A failed query
  is `StepQuery`/`QueryFailed`, exit code 1; `QueryFailed` carries the whole `QueryFailure`,
  so `generationToolMissing` can tell a missing tool from a broken query.
- **`renderEdits`** (`src/Olifant/Generation/Render.hs:43`) renders every template in every
  edit: target path, anchors, contents. The results:
  - `RenderedCreate "src/Page/Settings.elm" …`
  - `RenderedInsert "src/Main.elm" [PhraseAnchor "end of type Page"] "| Settings Settings.Model" …`
  - two `RenderedComplete` specs. These carry their body *unrendered*, plus the env, because
    the `{= pattern =}` in the body is only known once the compiler has named a site.
- **`expandUse`** (`Compose.hs:95`) handles `[[uses]]`, passing each use the files created
  so far (`prior`, plus this generator's own and the earlier uses'). `renderUse` (`Compose.hs:131`) makes one input set per
  item of `msgs`. `resolve (Just "elm") "msg"` finds `elm/msg`: inside a pack, a bare name
  means that pack's generator first (`Catalog.resolveEntry`, `Catalog.hs:276`). Then
  `expand` recurses with the step path `["elm/page", "elm/msg[1]"]`. That path is the
  `[elm/page › elm/msg[1]]` tag in the output.

The result is one flat list of `PlanItem`s, a generator's own items before its children's:

```
PlanRequire ["elm/page"] ["src/Main.elm","src/Route.elm"] …
PlanEdit    ["elm/page"] (create, 6 inserts, 2 completes)
PlanRequire ["elm/page","elm/msg[1]"] ["src/Page/Settings.elm"] …
PlanEdit    ["elm/page","elm/msg[1]"] (1 insert, 1 complete)
```

#### 4b. Look at the files the plan touches (`Generation.hs:81`)

`generate` collects every target path and required path and calls `inspectTarget` (`:83`)
on each one. The result is a map, `originals :: Map FilePath (Maybe Text)`, where `Nothing`
means absent. A second map, `blocked`, holds paths that can't be written: a directory, a
file that isn't UTF-8, or a path whose parent is a file. The project's full file list is
fetched only if some edit has an `unless = { exists = … }` glob (`:89`).

From here on, "the files" means that map. Each phase takes the current contents and returns
new contents. Nothing touches the disk until the very end.

#### 4c. The phases (`Generation.hs:92`)

```haskell
-- src/Olifant/Generation.hs:92
          phases originals' = do
            (positional, pc, afterPositional, next) <- decideItems locate diskPaths items originals' blocked onDisk
            (compiled, cc, ws', afterComplete) <- completeEdits tools ws completes next afterPositional
            (refreshed, rc, current') <- refreshRegions SkipUnset tools ws (next + length compiled) afterComplete
            pure (positional ++ compiled ++ refreshed, pc ++ cc ++ rc, ws', current')
```

Each phase returns its edits, its conflicts, and the contents after it, and the next phase
starts from those contents. `ws'` here is the list of warnings, not a workspace.

**Phase 1: positional edits, `decideItems`** (`src/Olifant/Generation/Planner.hs:35`). A
`foldM` over the items threads `(current, edits, conflicts, n)` through, dispatching each
`PlanEdit` to one of two sibling functions, `decideCreate` or `decideInsert` (`:61`, `:71`) —
split out so neither has to pattern-match a branch it can never reach:

- A `PlanRequire` checks its paths against `current`. `elm/msg` requires
  `src/Page/Settings.elm`, which is not on disk, but the page's create has already put it
  in `current`, so the requirement holds.
- A `RenderedCreate` goes to `decideCreate`, which becomes `WillCreate`. If the file exists
  with different content, what happens depends on `on_exists`: error (a `TargetExists`
  conflict), skip, or overwrite. If it exists with the same content, it is `Skipped
  UnchangedContent`.
- A `RenderedInsert` goes to `decideInsert`, which calls `locate` — `resolveLayers` with the
  file's language:

  ```haskell
  -- src/Olifant/Generation/Planner.hs:71
    decideInsert path n p anchors snippet current
      | Just problem <- unwritable n p = pure (current, planned path n p Conflicted, Just problem)
      | otherwise = case M.findWithDefault Nothing p current of
            Nothing -> pure (current, planned path n p Conflicted, Just (InsertTargetMissing n p))
            Just old ->
              locate p anchors old <&> \case
                Left misses -> (current, planned path n p Conflicted, Just (AnchorUnresolved n p misses))
                Right loc
                  | presentAt loc snippet old -> (current, (planned path n p (Skipped AlreadyPresent)) {editAnchor = Just (locUsed loc)}, Nothing)
                  | otherwise ->
                      let (line, updated) = insertLines loc snippet old
                       in (M.insert p (Just updated) current, (planned path n p (WillInsert line (placedText loc snippet))) {editAnchor = Just (locUsed loc)}, Nothing)
  ```

- `RenderedComplete` items are skipped here (`:45`) and left for phase 2.
- Before either function, an edit with an `unless` is checked (`:47`; the guard is read in
  [chapter 2](02-haskell-for-this-codebase.md#guards-where-let)). `matchUnless` (`:55`) is a
  `case` on `Unless` (`Generator/Definition.hs:83`): `UnlessExists` globs the project's paths
  plus the files `current` has created, and becomes `Skipped (UnlessMatched path)`;
  `UnlessMatches` compiles the rendered regex with `regex-tdfa` and runs it over each line
  of the edit's own target *as `current` has it*, so text an earlier edit or step inserted
  counts, and becomes `Skipped (UnlessFound path line)` (`Plan.hs:59`), which `skipReason`
  (`Cli/Output.hs:233`) prints as `unless: line N of PATH matches`. A target that does not
  exist yet, or a regex that no longer compiles once rendered, is no match. The regex was
  already checked at load time, with each `{= … =}` standing in for a plain name
  (`unlessFrom`, `Generator/Load.hs:295`).

**Anchors, `resolveLayers`** (`src/Olifant/Anchor/Resolve.hs:60`), tries the edit's anchors in
order and returns the first that resolves, or every miss. `resolveOne` (`:69`) parses the
phrase with `parsePhrase` (`src/Olifant/Anchor/Phrase.hs:39`) and dispatches on the target:

- `after last line matching ^import Page\.` is a **line target**. It's resolved in Haskell
  with `regex-tdfa` (`lineAt`, `Resolve.hs:128`), with no ast-grep call.
- `end of type Page` is a **named target**, `["type", "Page"]`. `named` (`Resolve.hs:141`)
  hands it to `itemsOfTarget` (`:190`), a top-level function shared with two other callers:
  a fragment exemplar's `items_of` (`Catalog.loadGenerator`, above) and an `ast` query's
  `items_of` (`Anchor.Items.astQueries`). `itemsOfTarget` matches the target against the Elm
  adapter's targets (`packs/elm/languages/elm.toml`); the shape `type NAME` fits and binds
  `NAME = Page`. It renders that target's `container` rule (`{ kind = "type_declaration",
  regex = "^type Page\b" }`) and asks the **Syntax port** for matches (`querySyntax`,
  `:200`). Exactly one match is required. Then it asks for the `items` (`union_variant`)
  *inside* that container (`:209`). Back in `resolveOne`'s `where`, `place` (`:146`) picks
  the line after the last item, indented like it — and sets `locSpacing`: if the container's
  existing items are separated by a blank line, `place` copies that spacing onto the new one
  (`BlankAfter` for `start of`, `BlankBefore` for `end of`; spec §5.2).
- **An empty container.** `place` has a third case, for `end of`/`start of` against a target
  whose `items` come back `[]` (spec §7.1). It opens the container only when it is bracketed
  (`()`, `{}`, `[]`; anything else, a whole file say, misses with "has no items to insert
  beside") and blank or comment-only: every line between the brackets, stripped, is empty or
  starts with the adapter's `comment` (`:167`). Anything else inside — `Svc::new(true)` when
  `boolean_literal` is not one of the target's `items`, `{ ..Default::default() }` — would be
  deleted or mis-joined, so it misses with "has content that is not one of the target's items
  (add its kind to the target's `items`)" (`:168`). Opening returns a `Located` with
  `locOpen = Just o`, where `data Opening = OpenLine Int Int Int | OpenLines` (`:48`);
  `locOpen` is `Nothing` for every other case, so `insertLines` (`:231`) knows only these need
  special handling. On one line (the container's start and end share a line —
  `Service::new()`, `RefundInvoice {}`), `OpenLine line from to` carries the bracket columns,
  and `insertLines` splits that line in three: up to the opening bracket, the new text at one
  level past the container's own indentation (`langIndent`, the adapter's `indent` key,
  `Language/Adapter.hs:38`), then the closing bracket onwards, at the line's own indentation
  — so `-> Self {` after a Rust parameter list, or a trailing `;` after a call's arguments,
  survives on the closing line. Spread over several lines already (`OpenLines`), `place`
  looks for a non-blank (comment) line inside and uses its indentation, so a template's
  commented-out hint in a struct literal is where the real field lands; with nothing inside
  at all, it falls back to the same one-level-in indentation as the one-line case. Either way
  the text becomes the container's *first* item, so `placedText` (`:249`) passes it through
  `firstItem` (`:252`), which drops one leading `,`: Elm's `oneOf []` given `, Parser.map …`
  becomes `oneOf [` / `Parser.map …` / `]`.

Each `querySyntax` call against the real adapter is one `ast-grep` process fed the file on
stdin (`src/Olifant/Language/AstGrep.hs:34`). The Elm pack vendors its tree-sitter grammar,
so the first ever call compiles `packs/elm/languages/grammar/elm/parser.c` with `cc` into
`~/.cache/olifant/grammars/`. The key is the language name plus a hash of `parser.c`'s
path, size and modification time (`prepare`): a `stat` per query, since hashing a
megabyte-sized grammar on every call cost a third of an AST query's time.

`presentAt` (`Resolve.hs:283`) makes re-runs idempotent: if the snippet's tokens already sit
in the target region, or just next to the insertion line, the insert is `Skipped
AlreadyPresent`. It tries the snippet and its `firstItem` both, so text whose leading `,` was
dropped when it opened a container is still found on the next run. Lines starting with the
adapter's comment (`locComment`, set from `langComment`) are dropped from the region, the
window and the snippet before comparing, so a template's commented-out example of the field
is not "present"; a snippet that is only comments is compared whole. It looks no further than
the region and that window; text that is in the file by name but elsewhere is what
`unless = { matches = … }` is for (above). `insertLines` does the actual splice, keeping the
file's line endings — whole lines around `locLine` normally, or, for `OpenLine`, the
three-way split above. The lines it adds come from `placedText`: the snippet (less a leading
`,` when the site opens a container) passed to `insertedText`, which removes its common
indentation (so a snippet written at its absolute column is not indented twice) and adds,
per `locSpacing`, a blank line if one isn't already there. The plan's `WillInsert` holds that
same text, so what `--plan` shows is what is written, apart from the site's indentation.

**Phase 2: `complete` edits, the check loop** (`src/Olifant/Generation/Complete.hs:37`).
`perLanguage` (`:40`) first turns the adapter's `[check] format` into a `CompilerSupport`
record with `supportFor` (`Language/Support.hs:13`) — `elmSupport`, `ghcSupport` or
`rustcSupport`. A format
`supportFor` doesn't recognise fails every `complete` edit for that language with one message
(`:45`), rather than surfacing the problem deep inside the loop. For each language that has
`complete` edits, `gather` (`:54`) collects every file the check needs, meaning the adapter's
`files` globs plus its `context` (`elm.json`; a Haskell adapter's `*.cabal` and
`cabal.project`; Rust's `**/Cargo.toml` and `Cargo.lock`), *as they will be after phase 1*. Then `loop` (`:62`) runs, carrying that
`support` record alongside the language:

```haskell
-- src/Olifant/Generation/Complete.hs:62
    loop lang support step mine disk rnd seen n files current acc
      | rnd > maxRounds = pure (reverse acc, [CompleteFailed step ("the compiler still reports missing cases after " <> T.pack (show maxRounds) <> " rounds") False], [], current)
      | otherwise =
          runCheck (toolChecker tools) lang files >>= \case
```

`runCheck` goes through the **Checker port**. `processChecker` (`src/Olifant/Language/Check.hs:48`)
writes the files into a temp directory and runs the adapter's `[check] command` there
(`runIn`, `:82`, which expands `{= files =}` to the sources and `{= cache =}` to a
per-language XDG cache directory, `:93`). It looks up the same `CompilerSupport` by format
(`supportFor`, `Check.hs:54`) and hands the compiler's raw output, stdout if the record's
`supportReadsStdout` says so (cargo) and stderr otherwise (`:56`), to `supportParse` —
`parseElmReport` (`Language/Elm.hs:20`) for `elm-json`, `parseGhcReport`
(`Language/Ghc.hs:36`) for `ghc-json`, `parseRustcReport` (`Language/Rustc.hs:50`) for
`rustc-json` — into a `CheckReport`:
`MissingPatterns` (file, the `case`'s line range, the missing patterns, and the scrutinee's
type when the compiler names it), broken files, and now `reportUnplaced`: sites the loop
cannot extend (GHC's equations, lambdas and pattern bindings; rustc's refutable `let`s),
paired with a description of the context ("an equation for `label`", "a lambda
abstraction", "a `let` binding").

`Language/Rustc.hs` reads cargo's `--message-format=json` one line at a time, keeping
`compiler-message` lines. Error E0004 ("non-exhaustive patterns") is a site: its primary span
is the `match`'s scrutinee, and the last arm is the line of rustc's own suggestion (the child
span whose `suggested_replacement` starts with `,`), so the site runs from one to the other
(`add`, `:72-73`). The patterns are the backquoted names in the message (`patternsIn`,
`:88`): three at most, since rustc writes "and 2 more", and the loop's next round reports the
rest. The type comes from the note "the matched value is of type `&RefundInvoiceFailure`"
(`matchedType`, `:80`). E0005 (a refutable `let`) becomes an unplaced site (`:74-75`); any
other error is a broken file. `rustcSupport` (`:21`) strips the `&`/`&mut ` rustc prints for a
reference scrutinee (`supportPattern`), checks visibility by type name (`supportVisible`:
the qualifier of `Type::Variant` or `Type.Variant`, `qualifier`, `:92`, must be one of
`typeNames` of the reported type), names the enclosing function with `rustFunctionAt`
(`:98`, the nearest `fn NAME` above), and sets `supportArmSeparator = ","` and
`supportReadsStdout = True`. `typeNames` lives in `Language/Diagnostics.hs` (`:60`) now that
GHC and rustc both use it.

The loop narrows a reported pattern's candidates with `supportVisible` (`:71`): for Elm, by
module import (`visibleAt`); for GHC, by the edit's `Type.Ctor` qualifier appearing as a
word in the reported type (`Maybe T`, `(T, Bool)`). A pattern that names one of the run's
constructors but has no visible candidate becomes a warning (`addHidden`), never a silent
skip. For each missing pattern, `branchFor` (`Complete.hs:119`) picks the `complete`
spec whose constructor it names, from the candidates sharing the pattern's bare name — a
`NonEmpty` list, since the call site only builds `cands` when it's non-empty (`nonEmpty`,
`:72`), so `branchFor` itself needs no "this is never empty" comment or partial `head`. Specs
with a `where` must also match: `locateContainer` checks that the reported `case` is the one
named. A `complete` edit with no body is filtered out before `whereMatches` even runs (`:124`,
`isJust (completeBody s)`), so it never supplies text for a chosen branch — only ever a todo.
`branchFor` then renders the chosen spec's body with `pattern`, written the way the site
should by `supportPattern` (Elm qualifies it; GHC's names are already in scope at the site;
rustc's lose a leading `&`), and `site`. `applyBranches` (`:158`) inserts the branches bottom-up within each file, so
earlier line numbers stay valid, and leaves the inserted edit's `editAnchor` as `Nothing` — a
compiler-found branch has no anchor to report, so there is nothing for a re-run to leak into
JSON (below).

When a round has nothing left to add, `addUnplaced` (`:99`) turns any `reportUnplaced` site
that needs one of this plan's constructors into a plan warning — never a todo, since there is
nowhere to place a branch — naming the file, the function and the pattern. Otherwise the loop
checks again.

`insertBranch` (`Complete.hs:173`) places the new line after the `case`'s last line, indented
like its other alternatives, and copies their *spacing*: a blank line before the new branch
only if one already separates the case's last two alternatives (Elm's usual style), none if
they sit flush against each other (ordinary Haskell). With only one alternative there is
nothing to copy: a blank line when the language has no arm separator (Elm), none when it has
one (Rust's arms sit together). Its first argument is the support's `supportArmSeparator`,
which `applyBranches` passes down (`:86`): before inserting, `separated` (`:192`) gives the
last arm the separator unless the line already ends with it, or with `}` or `{` (a block
arm). Elm and GHC pass `""`, which leaves the text alone. `insertBranch` is exported so
`GenerationSpec` can test those cases directly.

With a logging shim in front of `elm`, this command runs `elm make` three times:

```
round 1: src/Page/Settings.elm  MISSING PATTERNS            (update lacks ToggleDarkMode)
round 2: src/Main.elm           MISSING PATTERNS × 2        (view, changeRouteTo lack Settings)
round 3: clean
```

Round 1 reports nothing about `Main.elm` because Elm doesn't check a module whose imports
don't compile, and `Main` imports the new page. That's why the loop exists. It also
explains the order at the end of the output: the `update` branch (edit 9) was found in
round 1, and the `view` and `changeRouteTo` branches were found together in round 2 and
applied bottom-up (line 108, then line 59). The loop gives up after `maxRounds = 5`
(`:31`), or when a round reports a site it already filled (`:77`). If the compiler reports
one of the plan's constructors missing from a `case` that no matching spec's `where`
names, that site gets the adapter's placeholder branch (`Debug.todo`, or GHC's `error "…"`)
and is listed as a todo; a `complete` edit with no body at all (`olifant-dev/error-case`)
gets this for every site, since it has no spec a `where` could ever match. Missing patterns
that don't mention any of the plan's constructors are ignored, and GHC's function-equation
sites that do are the plan warnings from `addUnplaced` above, never todos.

**Phase 3: regions, `refreshRegions`** (`src/Olifant/Generation/Regions.hs:30`). This phase
re-renders every owned region against the files as the plan leaves them. The Elm pack
declares no regions, so `toolRegions` is empty and the phase returns immediately (`:32`). In
this repository the phase is where `olifant.cabal`'s module lists are refreshed.

#### 4d. The re-run second pass (`Generation.hs:97`)

Run the same command a second time. The page's `create` now finds `src/Page/Settings.elm`
on disk, but with different content from its template, because `elm/msg` changed the file
after creating it. On its own that's a `TargetExists` conflict. So when the first pass
reports `TargetExists` for any path, `generate` runs `phases` again, pretending those files
are absent (`M.insert p Nothing`). If that second pass has no conflicts and reproduces each
such file exactly, the run is a no-op for them: their edits become `Skipped
UnchangedContent`. If not, the first pass's conflict stands. A file that was edited by hand
since it was created doesn't come out identical, so it still conflicts.

```
$ olifant gen elm/page --set page=settings --set msgs="toggle_dark_mode"   # again
elm/page (9 edits, 0 conflicts)
  = src/Page/Settings.elm (skipped: unchanged)  [elm/page]
  = src/Main.elm (skipped: already present)  [elm/page]
  …
  = src/Page/Settings.elm (skipped: unchanged)  [elm/page › elm/msg[1]]
  = src/Page/Settings.elm (skipped: unchanged)  [elm/page › elm/msg[1]]
```

That's 9 edits, not 11: in the second pass the compiler finds nothing missing in `Main.elm`,
so the two `Main` branches never appear. A re-run costs two full passes, including the
compiler.

#### 4e. Writes (`Generation.hs:111`)

```haskell
-- src/Olifant/Generation.hs:111
      let writes = M.fromList [(p, c) | (p, Just c) <- M.toList current, M.lookup p originals /= Just (Just c)]
          plan = Plan (GeneratorId (T.intercalate " + " ids)) (zip ids (map stepSets steps)) now edits conflicts warnings writes
      when (mode == Apply && not (hasConflicts plan)) $
        forM_ (M.toList writes) (uncurry (writeText ws))
      pure (Right plan)
```

`planWrites` holds only the files whose final text differs from the original. They're
written only in `Apply` mode, and only when there are no conflicts at all. That is the whole
of "all or nothing": everything before this line worked on in-memory text. The `Plan` type
is in `src/Olifant/Generation/Plan.hs:21`. Its `planEdits` are `PlannedEdit`s (`:36`), each
with an `EditAction` (`:54`): `WillCreate`, `WillOverwrite`, `WillInsert line text`,
`WillRefresh`, `Skipped reason` or `Conflicted`.

The writes happen here, before anything is printed. If rendering the output crashed, the
files would already be written.

### 5. Output and exit code (`src/Olifant/Cli.hs:224`)

```haskell
-- src/Olifant/Cli.hs:224
              Right plan0 -> do
                let plan =
                      plan0
                        { planWarnings =
                            planWarnings plan0
                              ++ ["regions are not refreshed from " <> T.pack path <> ": " <> err | (path, err) <- catalogBrokenRegions cat]
                              ++ ["language adapter " <> T.pack path <> " failed to load: " <> err | (path, err) <- catalogBrokenLanguages cat]
                        }
                    applied = mode == Apply && not (hasConflicts plan)
                emit rt fmt (planJson applied plan) (planHuman applied plan)
                pure (if hasConflicts plan then ExitFailure 5 else ExitSuccess)
```

`emit` (`:249`) prints one of two renderings. Both are built, but laziness means only the
one printed is computed.

- **`planHuman`** (`src/Olifant/Cli/Output.hs:195`) prints the header, one line per edit
  (`editLine`, `:211`: `+`, `~`, `=`, `!`), then the conflicts, the todos and the warnings.
  In `--plan` mode it also prints the full text of each edit — `details` (`:222`) — unless
  the plan `hasConflicts`, in which case nothing would be written anyway, so neither `--plan`
  nor a plain `gen` prints any edit's text. The `[step]` tag appears only when edits came
  from more than one step path (`multiStep`, `:208`).
- **`planJson`** (`:163`) produces the `plan_version: 2` object. Applied runs leave out file
  contents (`content`, `:186`) — this one is keyed only on `applied`, not on conflicts,
  because a conflicted edit never has a `WillCreate`/`WillInsert`/etc. action to hold
  content in the first place (`editAction` is `Conflicted`).

Errors that stop generation before there is a plan (`GenerationError`, `Generation.hs:49`)
go through `generationFailure` (`Cli.hs:281`), which calls `describeGenerationError`
(`Generation.hs:65`, naming the step the error happened in) and picks the exit code: 4 for
inputs, 3 for an unknown generator, 1 for a broken one. `GeneratorTesting.runCase` calls the
same function when a case fails, so a pack test's failure message is the CLI's own sentence,
not a dumped Haskell value (below).

## Rough spots

This section is where things worth knowing about, but not urgent, go when you work near
them. There are none right now: step 2c (compiler support, above) closed the one that was
here — `Generation/Complete.hs` importing `Olifant.Language.Elm` and branching on
`langQualify lang == Just "elm"` — by moving the Elm-specific behaviour behind
`CompilerSupport`, the same way GHC's now sits.

## Where to go next

- [Chapter 2](02-haskell-for-this-codebase.md) covers the Haskell in the excerpts above,
  with Rust comparisons.
- To change a unit, start with its `XSpec.hs`: it's the unit's documentation by example,
  and it's where the change starts (tests first).
- `cabal test --test-options='--match "Olifant.Generation"'` runs one unit's specs, and
  `scripts/test-watch.sh` re-runs on save.
