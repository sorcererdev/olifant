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

Olifant is small: about 4,600 lines of library code in `src/`, 2,200 lines of tests next to
it, and a 42-line `app/Main.hs`. Line references (`src/Olifant/Cli.hs:45`) are to the code as
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
`src/Olifant/GeneratorTestingSpec.hs:50` finds every `packs/*/generators/*/tests/*/inputs.toml`,
runs that generator against the pack's `fixtures/app`, and compares the result with the
case's `snapshot/`. **`packs/olifant-dev`** is the pack Olifant uses on its own source. Its
`unit` generator creates `src/Olifant/X.hs` plus `src/Olifant/XSpec.hs`, and its
`internal-module` generator creates `src/Olifant/X/Y.hs`.

**`scripts/`**:
- `try-pack.sh PACK DIR` copies a pack's fixture app into DIR, configures the pack and
  commits it to a fresh git repo. Every example in these books runs in a sandbox like that.
- `install-olifant.sh` runs `cabal install` into `~/.local/bin`.
- `test-watch.sh` runs `ghcid` over the test suite and re-runs it on every save.

**`.githooks/`** (enable with `git config core.hooksPath .githooks`):
- `post-commit` and `post-merge` reinstall `olifant` when you are on `main`.
- `pre-merge-commit` runs `cabal test` and `olifant sync --check` before a merge into `main`
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
| **Toml** | TOML parsed into a small `Tree`, plus decoders with readable errors; the only module that knows the `toml-parser` API | `Toml.hs`: `Tree`, `Table`, `parseToml`, `required`, `optional`, `inKey`, `text`, `list`, `table`, `onlyKeys` | none |
| **Glob** | `**` / `*` path globs | `Glob.hs`: `matchesGlob`, `globPrefix` | none |
| **Template** | The `{= … =}` language: parse, render, exemplars | `Template.hs` over `Template/{Syntax,Parse,Render,Value,Exemplar,Error}`: `Template`, `parseTemplate`, `render`, `evaluate`, `Value`, `Env`, `exemplar`, `freeVariables` | Naming |
| **Language** | Language adapters from packs (`languages/*.toml`), ast-grep rules, and the **Syntax** and **Checker** ports | `Language.hs` re-exports `Adapter`, `Rule`, `Syntax`, `Check`: `Language`, `parseLanguage`, `languageFor`, `Rule`, `renderRule`, `Syntax`, `noSyntax`, `Checker`, `noChecker`, `processChecker`. `Language.AstGrep` (the ast-grep adapter) is imported directly by `Main` | Glob, Template, Toml |
| **Workspace** | The filesystem **port**, with disk and in-memory adapters | `Workspace.hs`: `Workspace`, `PathState`, `diskWorkspace`, `memoryWorkspace`, `within`, `normalisePath`, `isSafeRelative` | Language (only `Language.Process`, to run `git`) |
| **Query** | Facts from the repo as records (globs, commands printing JSON, or AST queries selecting nodes), and the **Queries** port | `Query.hs`: `Query`, `AstQuery`, `AstSelect`, `Record`, `runQuery`, `renderQuery`, `eachFields`, `Queries`, `noQueries`, `processCommand` | Glob, Language (`Process`, `Rule`), Naming, Template, Toml |
| **Anchor** | `at` phrases and where they put text, plus AST queries over a target's items | `Anchor.hs` re-exports `Anchor/Phrase`, `Anchor/Resolve` and `Anchor/Items`: `parsePhrase`, `resolveLayers`, `Located`, `Spacing`, `Miss`, `insertLines`, `presentAt`, `locateContainer`, `itemsOfTarget`, `astQueries` | Language, Query, Template |
| **Generator** | A generator definition: the types, loading `generator.toml`, turning `--set` values into a template environment | `Generator.hs` re-exports `Definition` and `Inputs`, plus `parseGenerator`, `withChoices`, `needsChoices` | Anchor, Language, Naming, Query, Template, Toml |
| **Region** | Owned regions: parse `regions.toml`, render a region body, splice it between fences. All pure | `Region.hs`: `RegionDefWith`, `parseRegions`, `renderRegion`, `applyRegion`, `fenceLines` | Query, Template, Toml |
| **Generation** | Running generators: expand `[[uses]]`, bind `[query.NAME]` queries, render edits, plan them, check, refresh regions, write | `Generation.hs`: `generate`, `Step`, `Mode`, `Resolve`, `refreshRegions`; re-exports `Generation/Plan` (the `Plan` type) and `Generation/Tools`. Internals: `Compose`, `Render`, `Planner`, `Complete`, `Regions` | Anchor, Generator, Glob, Language, Query, Region, Template, Workspace |
| **Catalog** | Finding packs and generators, and loading one into a runnable `Generator` | `Catalog.hs`: `openProjectCatalog`, `findEntry`, `loadGenerator`, `resolver`, `Catalog`, `CatalogEntry` | Anchor, Generation (one type), Generator, Language, Region, Template, Toml, Workspace |
| **Cli** | The command line: parse arguments, call the use cases, print JSON or text, pick the exit code | `Cli.hs`: `Runtime`, `runCli`. Internals: `Cli/Options` (argument parsing), `Cli/Output` (rendering) | almost everything |
| **GeneratorTesting** | Running a pack's test cases against its fixture (the seed of `olifant test`) | `GeneratorTesting.hs`: `discoverCases`, `runCase` | Anchor, Catalog, Generation, Generator, Language, Query, Toml, Workspace |

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
Naming, Toml, Glob   → (nothing)
Template             → Naming
Language             → Glob, Template, Toml
Workspace            → Language            (Language.Process, for `git ls-files`)
Query                → Glob, Language, Naming, Template, Toml     (Language.Process, Language.Rule)
Anchor               → Language, Query, Template
Region               → Query, Template, Toml
Generator            → Anchor, Language, Naming, Query, Template, Toml
Generation           → Anchor, Generator, Glob, Language, Query, Region, Template, Workspace
Catalog              → Anchor, Generation, Generator, Language, Region, Template, Toml, Workspace
Cli                  → Anchor, Catalog, Generation, Generator, Language, Query, Region, Template, Workspace
GeneratorTesting     → Anchor, Catalog, Generation, Generator, Language, Query, Toml, Workspace
app/Main             → Cli, Language, Language.AstGrep, Workspace
```

`Anchor` importing `Query` is the one edge worth pausing on: an `at` target and an `ast`
query both end up asking the same question ("what are this container's items?"), so
`itemsOfTarget` (`Anchor/Resolve.hs:153`) is shared, and `Anchor.Items.astQueries` builds
`Record`s (`Query`'s type) from what it finds. `Query` itself has no idea `Anchor` exists.

Two things to notice:

- **Generation does not import Catalog.** Generation has to load the generators named in
  `[[uses]]`, but it never looks them up itself. It takes a function
  (`type Resolve m = Maybe Text -> Text -> m (Either ResolveFailure Generator)`,
  `src/Olifant/Generation/Compose.hs:37`), and `Cli` passes in `Catalog.resolver`. This is the
  same dependency inversion as the ports below, applied to a single function.
- **`Language.Process` is misfiled.** It holds `runBytes` (run a program with byte stdin,
  stdout and stderr, never throw). It is general plumbing, but it sits in `Language/`, so
  `Workspace` and `Query` import the Language unit just to reach it. `Query` also reaches
  into `Language.Rule` directly, for the `Rule` an `ast` query wraps.

A module outside a unit normally imports only that unit's facade. These are the exceptions
(found with `grep -n 'import Olifant\.[A-Za-z]*\.[A-Z]'`):

```
app/Main.hs:9                          import Olifant.Language.AstGrep (astGrepSyntax)
src/Olifant/Workspace.hs:22            import Olifant.Language.Process (runBytes)
src/Olifant/Query.hs:36                import Olifant.Language.Process (runBytes)
src/Olifant/Query.hs:38                import Olifant.Language.Rule (Rule (..), renderRule, ruleFromTree)
src/Olifant/Catalog.hs:32              import Olifant.Generation.Compose (ResolveFailure (..))
src/Olifant/Generation/Complete.hs:23  import Olifant.Language.Elm (enclosingFunction, qualifyPattern, visibleAt)
```

`Main` importing an adapter directly is fine: wiring adapters together is `Main`'s job. The
Catalog import is unnecessary, because `Olifant.Generation` re-exports `ResolveFailure`. The
`Elm` import is a real rough spot, covered at the end of this chapter.

## Ports and adapters

Olifant touches four things outside its own memory: the filesystem, ast-grep, a language's
compiler, and arbitrary commands (for queries; AST queries reuse ast-grep through the Syntax
port instead of touching anything new). Each is a **port**. A port is a record of
functions, parameterised by the monad `m` it runs in:

```haskell
-- src/Olifant/Workspace.hs:27
data Workspace m = Workspace
  { readText :: FilePath -> m (Maybe Text)
  , writeText :: FilePath -> Text -> m ()
  , fileExists :: FilePath -> m Bool
  , listFiles :: FilePath -> m [FilePath]
  , inspectPath :: FilePath -> m PathState
  }
```

```haskell
-- src/Olifant/Language/Syntax.hs:15
newtype Syntax m = Syntax {querySyntax :: Language -> Value -> Text -> m (Either SyntaxFailure [Match])}

-- src/Olifant/Language/Check.hs:36
newtype Checker m = Checker {runCheck :: Language -> Map FilePath Text -> m (Either CheckFailure CheckReport)}

-- src/Olifant/Query.hs:124
data Queries m = Queries
  { queryCommand :: [Text] -> m (Either Text ByteString)
  , queryAst :: AstQuery -> [FilePath] -> (FilePath -> m (Maybe Text)) -> m (Either Text [Record])
  }
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
reach outside Olifant's memory, so they travel together in one port.

| port | defined at | real adapter | no-op / test adapters |
|---|---|---|---|
| `Workspace m` | `Workspace.hs:27` | `diskWorkspace` (`Workspace.hs:66`): reads and writes under a root; lists files with `git ls-files` when inside a git work tree, otherwise walks the tree. `within dir` (`:121`) views a workspace from a subdirectory | `memoryWorkspace` (`Workspace.hs:40`): a `Map` in an `IORef`, which also hands back an action that reads the final map, so tests can assert on "the disk" |
| `Syntax m` | `Language/Syntax.hs:15` | `astGrepSyntax` (`Language/AstGrep.hs:33`): runs `ast-grep scan --stdin --inline-rules … --json=compact`; compiles a pack's vendored grammar with `cc` into the cache on first use | `noSyntax` (`Language/Syntax.hs:17`) always says "ast-grep is not installed"; `fakeSyntax :: Syntax Identity` in `AnchorSpec.hs:58` returns canned matches |
| `Checker m` | `Language/Check.hs:36` | `processChecker` (`Check.hs:48`): copies the files into a temp dir, runs the adapter's `[check] command` there, parses the report (`elm-json` only) | `noChecker` (`Check.hs:38`); `scriptedChecker` (`GenerationSpec.hs:402`) replays a list of reports, one per round |
| `Queries m` | `Query.hs:124` | `processCommand dir` (`Query.hs:138`): runs a program (a bare name from `PATH`, or a project path like `bin/list`) in the project dir, for `queryCommand`; `astQueries syntax langs` (`Anchor/Items.hs:17`): evaluates an `ast` query's target over every matching file, for `queryAst` | `noQueries` (`Query.hs:131`); `QuerySpec.hs:39`'s `fake out = noQueries {queryCommand = …}` overrides one field and leaves `queryAst` as the no-op |

The ports are bundled at two levels. `Tools m` (`src/Olifant/Generation/Tools.hs:10`) holds
Syntax, Checker and Queries, plus the catalog's language adapters and regions, and it is what
`generate` takes. `noTools` (`:18`) needs nothing installed. `Runtime`
(`src/Olifant/Cli.hs:31`) is the CLI's port: the root workspace, the project dir, stdin, the
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

**Why tests use fakes.** Speed is one reason: the CLI spec's 44 examples run in under 10 ms.
The main reasons are that a fake can produce situations the real tool rarely does and that
the test says exactly what it assumes. `scriptedChecker` returns "missing `Reset`" and then
the same thing again, which is how `completeEdits` is tested to stop instead of adding the
same branch forever. `fakeSyntax` returns a nested match, which is how the "direct children
only" rule in anchor resolution is tested. The real adapters are still tested, but
separately: `LanguageSpec.hs:158` runs real `elm make`, `LanguageSpec.hs:191` real
`ast-grep`, and the pack snapshot tests run both. Those tests become *pending*, not failed,
when the tool isn't installed (`conflictToolMissing`, `src/Olifant/Generation/Plan.hs:91`).

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

### 2. `runCli` parses and opens the catalog (`src/Olifant/Cli.hs:45`)

```haskell
-- src/Olifant/Cli.hs:45
runCli :: Runtime -> [String] -> IO ExitCode
runCli rt args = case parseOptions args of
  Left (text, ExitSuccess) -> rtOut rt text >> pure ExitSuccess
  Left (text, _)
    | wantsJson -> rtOut rt (encodeJson (errorJson "usage" (T.strip text) []) <> "\n") >> pure (ExitFailure 2)
    | otherwise -> rtErr rt text >> pure (ExitFailure 2)
  Right opts -> do
    let fmt = fromMaybe (if rtStdoutIsTty rt then Human else Json) (optFormat opts)
    openProjectCatalog (rtRoot rt) (CatalogPaths (rtProjectDir rt) (rtUserConfigDir rt) (rtHomeDir rt)) >>= \case
      Left err -> catalogFailure rt fmt err
      Right cat -> runCommand rt fmt cat (optCommand opts)
```

**`parseOptions`** (`src/Olifant/Cli/Options.hs:75`) returns `Either (Text, ExitCode)
Options`: either text to print with an exit code, or parsed options. Most commands go
through `optparse-applicative` (`:83`). `gen` does not. It's matched first (`:76`) and
handed to the hand-written `parseGen` (`:113`), because optparse can't express "steps
separated by a standalone `+`". `parseGen` is a loop, `go`, over the remaining words. It
peels off the global flags (`--json`, `--human`, `--plan`, `--now`, `--steps`) wherever they
appear, and starts a new step at each `"+"` (`:129`). `finish` (`:131`) turns each step into
a `StepSpec`. The optparse definition of `gen` (`genP`, `:101`) exists only to render `gen
--help`. For our command the result is, in effect:

```
Options Nothing (GenCmd (GenOptions [StepSpec "elm/page" [("page","settings"),("msgs","toggle_dark_mode")]] Nothing False Nothing))
```

That is: no format flag, one step, no `--steps` file, not `--plan`, no `--now`.

The **output format** is decided once (`Cli.hs:52`): the flag if given, otherwise human
output for a terminal and JSON when piped.

**`openProjectCatalog`** (`src/Olifant/Catalog.hs:99`) runs for every command. It reads
`[packs]` from `~/.config/olifant/config.toml` and `.olifant/config.toml`. The project's
entry wins for the same name (`M.fromList (fromUser ++ fromProject)`, `:109`: a later key
overwrites an earlier one). Then `openCatalogAt` (`:136`) goes through each pack:

- `discover` (`:158`) lists the files and keeps `generators/<name>/generator.toml`, exactly
  two levels deep. `load` (`:162`) parses each one with `parseGenerator`
  (`src/Olifant/Generator/Load.hs:17`) into a `GeneratorDef`.
- `languagesIn` (`Catalog.hs:146`) parses `languages/*.toml` into `Language` values with
  `parseLanguage` (`src/Olifant/Language/Adapter.hs:50`).
- `regionsIn` (`Catalog.hs:262`) reads `regions.toml`.

A generator, language or regions file that fails to parse is kept aside (`catalogBroken`,
`catalogBrokenLanguages`, `catalogBrokenRegions`) instead of failing the command. One bad
generator never breaks unrelated ones. The project's own `.olifant/generators/` and
`.olifant/regions.toml` are added the same way.

So every invocation parses every generator in every configured pack. That's cheap at
today's sizes, but it is also why a typo in one pack's TOML shows up in `olifant list` for
all of them.

### 3. `runCommand` loads the generator (`src/Olifant/Cli.hs:99`)

The `GenCmd` branch first gets its steps, from the arguments or from `--steps FILE|-` (JSON,
decoded by the `FromJSON StepSpec` instance at `Options.hs:44`). Then it loads each step:

```haskell
-- src/Olifant/Cli.hs:114
        loaded <- traverse (\(i, s) -> fmap (first (label i s,)) (either (pure . Left) (loadGenerator (rtRoot rt) (rtSyntax rt) cat) (findEntry cat (specGenerator s)))) (zip [1 :: Int ..] ss)
        case sequence loaded of
```

Read from the inside out:

1. **`findEntry cat "elm/page"`** (`Catalog.hs:172`) looks for an exact id first. Failing
   that, it looks for a bare name that is unique across packs (`page` would work). Then it
   checks the broken list, and only then reports "not found" or "pack not configured".
2. **`loadGenerator`** (`Catalog.hs:193`) turns the `GeneratorDef`, whose edit sources are
   still file references, into a runnable `Generator`, whose sources are parsed templates.
   It also needs the Syntax port now, because a fragment exemplar (below) has to search a
   file's syntax tree at load time, not just render text. It's still one line:

   ```haskell
   -- src/Olifant/Catalog.hs:193
   loadGenerator :: Monad m => Workspace m -> Syntax m -> Catalog -> CatalogEntry -> m (Either CatalogError Generator)
   loadGenerator root syntax cat entry = sequenceA <$> traverse resolve (entryDefinition entry)
   ```

   `resolve` (`:197`) handles each source:
   - `TemplateFile f` reads `templates/f` and parses it.
   - `ExemplarFile ref binds` reads the exemplar (`fixture:…` means the pack's
     `fixtures/app/`) and turns it into a template with `exemplar`
     (`src/Olifant/Template/Exemplar.hs:37`). That function replaces every case form of
     `sample_page` (`SamplePage`, `samplePage`, `SAMPLE_PAGE`, …) with the matching filter
     applied to `page`.
   - `FragmentFile ref target matching binds` (spec §4.3) is an exemplar cut out of a bigger
     file instead of read whole. It reads the same kind of file reference as `ExemplarFile`,
     finds the `target`'s items with `itemsOfTarget` (`Anchor/Resolve.hs:153`, the same
     function `end of`/`start of` anchors and `items_of` AST queries use), requires exactly
     one of them to match the regex `matching`, dedents it by the node's starting column
     (`dedent`, `Catalog.hs:219`, since only its first line sits at that column in the
     source), then turns the result into a template with `exemplar` just like `ExemplarFile`
     does.
   - `InlineText t` is already a template.

   `traverse` over a *generator* is possible because `GeneratorWith` derives `Traversable`.
   That trick gets its own section in [chapter 2](02-haskell-for-this-codebase.md#the-traversable-trick-same-shape-sources-then-templates).
3. `traverse` over the steps, then **`sequence loaded`**, turns "a list of results" into "the
   first error, or a list of generators", like `.collect::<Result<Vec<_>, _>>()`.

Next, **`resolveChoicesFor`** (`Cli.hs:193`) fills in `choices_from` enums, which are enum
inputs whose choices come from a query over the project. It only lists the project if some
input needs it (`needsChoices`, `src/Olifant/Generator/Choices.hs:31`). `elm/page` has none,
so this is a no-op here.

Then the call into the core:

```haskell
-- src/Olifant/Cli.hs:125
            let mode = if genPlanOnly g then PlanOnly else Apply
                steps = zipWith (\gen s -> Step gen (M.fromList (specSets s))) gens ss
            generate (within (rtProjectDir rt) (rtRoot rt)) resolveWithChoices (projectTools rt cat) steps now mode >>= \case
```

The arguments, in order:
- the workspace narrowed to the project, so paths are now project-relative
- `resolveWithChoices`, which is `Catalog.resolver` followed by the same choices step, used
  for `[[uses]]`
- the `Tools`, built by `projectTools rt cat` (`Cli.hs:245`): the real Syntax and Checker,
  the catalog's languages and regions, and `projectQueries rt cat` (`:250`) — `queryCommand
  = processCommand (rtProjectDir rt)`, `queryAst = astQueries (rtSyntax rt) (catalogLanguages
  cat)`
- the steps
- the time (`--now` or the clock)
- `Apply`

### 4. `generate`: from steps to a plan (`src/Olifant/Generation.hs:57`)

```haskell
generate :: Monad m => Workspace m -> Resolve m -> Tools m -> [Step] -> UTCTime -> Mode -> m (Either GenerationError Plan)
```

`generate` is the one use case at the centre of Olifant. It has five stages.

#### 4a. Expand the steps into plan items (`Generation/Compose.hs`)

`expandSteps` (`Compose.hs:66`) calls `expand` for each top-level step. `expand` runs in
`ExceptT CompositionError m`, a monad in which any line can fail with a composition error
and stop the rest (chapter 2 has a section on it):

```haskell
-- src/Olifant/Generation/Compose.hs:75
expand resolve bindQ now ancestors path gen sets = do
  when (genId gen `elem` ancestors) $ throwE (StepCycle path)
  checkCycles resolve (genId gen : ancestors) path gen
  env0 <- except (first (StepInputs path) (buildEnv gen sets now))
  env <- if null (genQueries gen) then pure env0 else ExceptT (first (StepQuery path) <$> bindQ env0 (genQueries gen))
  rendered <- except (first (StepTemplate path) (renderEdits (genLanguage gen) env (genEdits gen)))
  (required, hint) <- except (first (StepTemplate path) (renderRequires env (genRequires gen)))
  nested <- concat <$> zipWithM (expandUse env) [1 :: Int ..] (genUses gen)
  pure ([PlanRequire path required hint | not (null required)] ++ map (PlanEdit path) rendered ++ nested)
```

- **`buildEnv`** (`src/Olifant/Generator/Inputs.hs:33`) checks the `--set` values against the
  declared inputs. It reports *every* missing, invalid or unknown input at once, which is
  exit code 4 with a list. It parses each value by type, adds `now`, and evaluates the
  `[derive]` values in dependency order (`deriveAll`, `:125`). Here the environment ends up
  with `page` as a name (`["settings"]`), `msgs` as a list of one record `{name =
  ["toggle","dark","mode"], arg = ""}`, `now`, and `P = "Settings"`.
- **`bindQ`** binds the generator's own `[query.NAME]` queries (`genQueries`, spec §4.5) into
  the environment, if it has any (`elm/page` doesn't, so this step is skipped here).
  `BindQueries m` (`Compose.hs:57`) is another dependency-inverted function, like `Resolve
  m`: `Cli`/`GeneratorTesting` pass in `generate`'s real `bindQueries` (`Generation.hs:104`),
  which renders each query's text against the environment so far (`renderQuery`,
  `Query.hs:192`) and runs it over the project's files (`runQuery`, `Query.hs:160`), reading
  through the same plan-aware view every phase uses. A query's records are bound as a
  `VList` under its name, so a template can loop over it. For example (`GenerationSpec.hs:78`):
  `[query.mods] glob = "{= dir =}/*.hs"` makes `mods` a list of records, one per matching
  file, and `{= for m in mods =}{= m.stem =}\n{= end =}` lists their stems. A failed query
  is `StepQuery`/`QueryFailed`, exit code 1.
- **`renderEdits`** (`src/Olifant/Generation/Render.hs:43`) renders every template in every
  edit: target path, anchors, contents. The results:
  - `RenderedCreate "src/Page/Settings.elm" …`
  - `RenderedInsert "src/Main.elm" [PhraseAnchor "end of type Page"] "| Settings Settings.Model" …`
  - two `RenderedComplete` specs. These carry their body *unrendered*, plus the env, because
    the `{= pattern =}` in the body is only known once the compiler has named a site.
- **`expandUse`** (`Compose.hs:86`) handles `[[uses]]`. `renderUse` (`Compose.hs:122`) makes one input set per
  item of `msgs`. `resolve (Just "elm") "msg"` finds `elm/msg`: inside a pack, a bare name
  means that pack's generator first (`Catalog.resolveEntry`, `Catalog.hs:239`). Then
  `expand` recurses with the step path `["elm/page", "elm/msg[1]"]`. That path is the
  `[elm/page › elm/msg[1]]` tag in the output.

The result is one flat list of `PlanItem`s, a generator's own items before its children's:

```
PlanRequire ["elm/page"] ["src/Main.elm","src/Route.elm"] …
PlanEdit    ["elm/page"] (create, 6 inserts, 2 completes)
PlanRequire ["elm/page","elm/msg[1]"] ["src/Page/Settings.elm"] …
PlanEdit    ["elm/page","elm/msg[1]"] (1 insert, 1 complete)
```

#### 4b. Look at the files the plan touches (`Generation.hs:62`)

`generate` collects every target path and required path and calls `inspectTarget` (`:122`)
on each one. The result is a map, `originals :: Map FilePath (Maybe Text)`, where `Nothing`
means absent. A second map, `blocked`, holds paths that can't be written: a directory, a
file that isn't UTF-8, or a path whose parent is a file. The project's full file list is
fetched only if some edit has an `unless` glob (`:70`).

From here on, "the files" means that map. Each phase takes the current contents and returns
new contents. Nothing touches the disk until the very end.

#### 4c. The phases (`Generation.hs:73`)

```haskell
-- src/Olifant/Generation.hs:73
          phases originals' = do
            (positional, pc, afterPositional, next) <- decideItems locate diskPaths items originals' blocked onDisk
            (compiled, cc, ws', afterComplete) <- completeEdits tools ws completes next afterPositional
            (refreshed, rc, current') <- refreshRegions SkipUnset tools ws (next + length compiled) afterComplete
            pure (positional ++ compiled ++ refreshed, pc ++ cc ++ rc, ws', current')
```

Each phase returns its edits, its conflicts, and the contents after it, and the next phase
starts from those contents. `ws'` here is the list of warnings, not a workspace.

**Phase 1: positional edits, `decideItems`** (`src/Olifant/Generation/Planner.hs:33`). A
`foldM` over the items threads `(current, edits, conflicts, n)` through:

- A `PlanRequire` checks its paths against `current`. `elm/msg` requires
  `src/Page/Settings.elm`, which is not on disk, but the page's create has already put it
  in `current`, so the requirement holds.
- A `RenderedCreate` becomes `WillCreate`. If the file exists with different content, what
  happens depends on `on_exists`: error (a `TargetExists` conflict), skip, or overwrite. If
  it exists with the same content, it is `Skipped UnchangedContent`.
- A `RenderedInsert` calls `locate`, which is `resolveLayers` with the file's language:

  ```haskell
  -- src/Olifant/Generation/Planner.hs:63
      RenderedInsert p anchors snippet _
        | Just problem <- unwritable p -> pure (current, planned path n p Conflicted, Just problem)
        | otherwise -> case existing p of
            Nothing -> pure (current, planned path n p Conflicted, Just (InsertTargetMissing n p))
            Just old ->
              locate p anchors old <&> \case
                Left misses -> (current, planned path n p Conflicted, Just (AnchorUnresolved n p misses))
                Right loc
                  | presentAt loc snippet old -> (current, (planned path n p (Skipped AlreadyPresent)) {editAnchor = Just (locUsed loc)}, Nothing)
                  | otherwise ->
                      let (line, updated) = insertLines loc snippet old
                       in (M.insert p (Just updated) current, (planned path n p (WillInsert line snippet)) {editAnchor = Just (locUsed loc)}, Nothing)
  ```

- `RenderedComplete` items are skipped here (`:42`) and left for phase 2.

**Anchors, `resolveLayers`** (`src/Olifant/Anchor/Resolve.hs:48`), tries the edit's anchors in
order and returns the first that resolves, or every miss. `resolveOne` (`:57`) parses the
phrase with `parsePhrase` (`src/Olifant/Anchor/Phrase.hs:39`) and dispatches on the target:

- `after last line matching ^import Page\.` is a **line target**. It's resolved in Haskell
  with `regex-tdfa` (`lineAt`, `Resolve.hs:111`), with no ast-grep call.
- `end of type Page` is a **named target**, `["type", "Page"]`. `named` (`Resolve.hs:124`)
  hands it to `itemsOfTarget` (`:153`), a top-level function shared with two other callers:
  a fragment exemplar's `items_of` (`Catalog.loadGenerator`, above) and an `ast` query's
  `items_of` (`Anchor.Items.astQueries`). `itemsOfTarget` matches the target against the Elm
  adapter's targets (`packs/elm/languages/elm.toml`); the shape `type NAME` fits and binds
  `NAME = Page`. It renders that target's `container` rule (`{ kind = "type_declaration",
  regex = "^type Page\b" }`) and asks the **Syntax port** for matches (`querySyntax`,
  `:163`). Exactly one match is required. Then it asks for the `items` (`union_variant`)
  *inside* that container (`:172`). Back in `resolveOne`'s `where`, `place` (`:129`) picks
  the line after the last item, indented like it — and sets `locSpacing`: if the container's
  existing items are separated by a blank line, `place` copies that spacing onto the new one
  (`BlankAfter` for `start of`, `BlankBefore` for `end of`; spec §5.2).

Each `querySyntax` call against the real adapter is one `ast-grep` process fed the file on
stdin (`src/Olifant/Language/AstGrep.hs:34`). The Elm pack vendors its tree-sitter grammar,
so the first ever call compiles `packs/elm/languages/grammar/elm/parser.c` with `cc` into
`~/.cache/olifant/grammars/`. The key is a hash of `parser.c` (`prepare`, `:49`).

`presentAt` (`Resolve.hs:214`) makes re-runs idempotent: if the snippet's tokens already sit
in the target region, or just next to the insertion line, the insert is `Skipped
AlreadyPresent`. `insertLines` (`:194`) does the actual splice, keeping the file's line
endings and, per `locSpacing`, a blank line around the new text if one isn't already there.

**Phase 2: `complete` edits, the check loop** (`src/Olifant/Generation/Complete.hs:35`). For
each language that has `complete` edits, `gather` (`:51`) collects every file the check
needs, meaning the adapter's `files` globs plus its `context` (`elm.json`), *as they will be
after phase 1*. Then `loop` (`:59`) runs:

```haskell
-- src/Olifant/Generation/Complete.hs:59
    loop lang step mine disk rnd seen n files current acc
      | rnd > maxRounds = pure (reverse acc, [CompleteFailed step ("the compiler still reports missing cases after " <> T.pack (show maxRounds) <> " rounds") False], [], current)
      | otherwise =
          runCheck (toolChecker tools) lang files >>= \case
```

`runCheck` goes through the **Checker port**. `processChecker` (`src/Olifant/Language/Check.hs:48`)
writes the files into a temp directory and runs the command from the Elm adapter's `[check]`,
`elm make {= files =} --report=json --output=/dev/null`, in that directory. It then parses
the report with `parseElmReport` (`src/Olifant/Language/Elm.hs:20`) into `MissingPatterns`
(file, the `case`'s line range, the missing constructors), plus a list of files broken for
any other reason.

For each missing pattern, `branchFor` (`Complete.hs:98`) picks the `complete` spec whose
constructor it names. Specs with a `where` must also match: `locateContainer` checks that
the reported `case` is the one in `changeRouteTo`. `branchFor` then renders the spec's body
with `pattern` (written the way that module refers to the constructor, e.g. `Route.Settings`
vs `Settings`, by `qualifyPattern`) and `site`. `applyBranches` (`:139`) inserts the
branches bottom-up within each file, so earlier line numbers stay valid. Then the loop
checks again.

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
(`:29`), or when a round reports a site it already filled (`:71`). If the compiler reports
one of the plan's constructors missing from a `case` that no matching spec's `where`
names, that site gets the adapter's placeholder branch (`Debug.todo`) and is listed as a
todo. Missing patterns that don't mention any of the plan's constructors are ignored.

**Phase 3: regions, `refreshRegions`** (`src/Olifant/Generation/Regions.hs:29`). This phase
re-renders every owned region against the files as the plan leaves them. The Elm pack
declares no regions, so `toolRegions` is empty and the phase returns immediately (`:31`). In
this repository the phase is where `olifant.cabal`'s module lists are refreshed.

#### 4d. The re-run second pass (`Generation.hs:79`)

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

#### 4e. Writes (`Generation.hs:92`)

```haskell
-- src/Olifant/Generation.hs:92
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

### 5. Output and exit code (`src/Olifant/Cli.hs:129`)

```haskell
-- src/Olifant/Cli.hs:129
              Right plan0 -> do
                let plan = plan0 {planWarnings = planWarnings plan0 ++ ["regions are not refreshed from " <> T.pack path <> ": " <> err | (path, err) <- catalogBrokenRegions cat]}
                    applied = mode == Apply && not (hasConflicts plan)
                emit rt fmt (planJson applied plan) (planHuman applied plan)
                pure (if hasConflicts plan then ExitFailure 5 else ExitSuccess)
```

`emit` (`:148`) prints one of two renderings. Both are built, but laziness means only the
one printed is computed.

- **`planHuman`** (`src/Olifant/Cli/Output.hs:155`) prints the header, one line per edit
  (`editLine`, `:170`: `+`, `~`, `=`, `!`), then the conflicts, the todos and the warnings.
  In `--plan` mode it also prints the full text of each edit. The `[step]` tag appears only
  when edits came from more than one step path (`multiStep`, `:168`).
- **`planJson`** (`:123`) produces the `plan_version: 2` object. Applied runs leave out file
  contents (`:146`).

Errors that stop generation before there is a plan (`GenerationError`, `Generation.hs:48`)
go through `generationFailure` (`Cli.hs:179`), which picks the exit code: 4 for inputs, 3
for an unknown generator, 1 for a broken one.

## Rough spots

These are things in the code today that are worth knowing when you work near them. None of
them is urgent.

- **Elm knowledge inside the generic engine.** `Generation/Complete.hs` imports
  `Olifant.Language.Elm` (`:23`) and branches on `langQualify lang == Just "elm"` (`:68`,
  `:106`). `Language/Check.hs:55` dispatches on the report format `"elm-json"`. A second
  compiled language will need these behind the adapter.
- **`Language.Process` is shared plumbing filed under Language**, so Workspace and Query
  depend on the Language unit (`Workspace.hs:22`, `Query.hs:36`). `Query` also reaches into
  `Language.Rule` directly (`:38`), for the same reason.
- **Three lists of directories to ignore**: `pruned` (`Workspace.hs:150`), `ignored`
  (`Generation/Complete.hs:47`) and `ignoredDirs` (`Query.hs:95`). Only the last one
  includes `.olifant`.
- **A duplicated comment.** The re-run explanation at `Generation.hs:81-84` is repeated at
  `:98-101`, in the `where` block above `bindQueries`, where it describes nothing.
- **Stale comments.** `Template/Render.hs:18` says `{{ expr }}`, but the syntax is `{= … =}`.
  `Anchor/Resolve.hs:223` says "Moved from Generation.Marker", a module that no longer
  exists.
- **The re-run pass leaks a label into JSON.** A compiler-found branch stores
  `"compiler: missing X"` in `editAnchor` (`Generation/Complete.hs:147`). JSON prints
  `anchor_used` only when `editSite` is empty (`Cli/Output.hs:142`). The second pass clears
  `editSite` when it rewrites an edit to `Skipped UnchangedContent` (`Generation.hs:90`), so
  a re-run's JSON shows `"anchor_used": "compiler: missing ToggleDarkMode"`, which isn't an
  anchor.
- **`openCatalogAt` can't fail** (`Catalog.hs:144` always returns `Right`), so the `Either`
  in `openPackCatalog`'s type is never `Left`. Its helper `languagesIn _ r` (`:146`) ignores
  its first argument.
- **Pack tests can run AST queries but not command queries.** `runCase` builds `queries =
  noQueries {queryAst = astQueries syntax (catalogLanguages cat)}` (`GeneratorTesting.hs:68`)
  and passes it to both `withChoices` and `toolQueries` (`:69`, `:72`), so `items_of`/`ast`
  queries run for real against the fixture. `queryCommand` is left as `noQueries`'s no-op, so
  a pack's `choices_from` or `[query.NAME]` that runs a `command` is still untestable in
  `olifant test`.
- **A partial function.** `branchFor` uses `head'`, which calls `error` on an empty list
  (`Generation/Complete.hs:119`). The comment says the list is never empty, and today that's
  true by construction (`:69`).

## Where to go next

- [Chapter 2](02-haskell-for-this-codebase.md) covers the Haskell in the excerpts above,
  with Rust comparisons.
- To change a unit, start with its `XSpec.hs`: it's the unit's documentation by example,
  and it's where the change starts (tests first).
- `cabal test --test-options='--match "Olifant.Generation"'` runs one unit's specs, and
  `scripts/test-watch.sh` re-runs on save.
