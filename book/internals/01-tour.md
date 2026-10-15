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

Olifant is small: about 7,400 lines of library code in `src/`, 4,100 lines of tests next to
it, and an 89-line `app/Main.hs`. Line references (`src/Olifant/Cli.hs:48`) are to the code as
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
packs/               generator packs: elm, rails, redwood, rust-htmx, python-htmx, olifant-dev
book/                these books
scripts/             try-pack.sh, install-olifant.sh, test-watch.sh, record-pyright.sh
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
edit those lists by hand. The pre-merge hook runs `olifant check`, which checks the regions
and the project's rules ([the conventions](#the-fourth-use-case-conventions-olifant-check-and-sync---plan),
below), so a stale list can't reach `main`.

**`packs/`** holds real packs, and they are part of the test suite.
`src/Olifant/GeneratorTestingSpec.hs:131` finds every `packs/*/generators/*/tests/*/inputs.toml`,
runs that generator against the pack's `fixtures/app`, and compares the result with the
case's `snapshot/`. A case may also have a `given/` directory, whose files replace or add to
the fixture for that case only: `runCase` (`src/Olifant/GeneratorTesting.hs:54`) reads them
(`:62-63`) and hands them to `preparePack` (`:117`) as an overlay, which wins the `M.union`
(`:130`); command queries still run in the fixture directory on disk. `preparePack` is the
whole setup (open the pack's catalog, load the generator, lay the overlay over the fixture in
a `memoryWorkspace`, build the `Tools`) split out of `runCase` so a spec can run a pack
generator with its own overlay and inputs: the "rust-htmx dependent choices" spec (`GeneratorTestingSpec.hs:142`)
gives `wire-command` a hand-added `clock` field and expects the error naming billing's
ports. A generation error that is only a missing tool (`generationToolMissing`,
`src/Olifant/Generation.hs:62`: a query whose ast-grep or command program is absent) makes
the case `CaseSkipped` (`GeneratorTesting.hs:76`), like a conflict that is only a missing
tool.

Two more groups in the same spec check that pack output builds, on the fixture with the
case's `given/` and `snapshot/` laid over it (`M.unions [snapshot, given, fixture]`).
**"elm pack output compiles"** (`GeneratorTestingSpec.hs:157`) hands each Elm case's tree to
`compileClean` (`Language/Check.hs:64`), which runs the Elm adapter's `[check]` command and
wants exit 0. **"rust pack output compiles"** (`GeneratorTestingSpec.hs:175`) is the same code with the rust-htmx
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
`cargo` goes through `missingTool` (`GeneratorTestingSpec.hs:217`): `pendingWith` locally, `expectationFailure` under
`OLIFANT_REQUIRE_TOOLS=1`. Because every case is checked against it, the rust-htmx fixture
itself compiles, and a case that needs an unwired hand edit (an `email_sender` field typed
into `cancel_invoice.rs`) carries it in `given/`.

**"python pack"** does the same for python-htmx with the pack's Python adapter (pyright
through `uv`), and checks the fixture itself twice: `compileClean` on it alone (pyright
strict), and `uv run --frozen pytest -q` in a temp copy, its environment in the temp
directory (`olifant-test-cache/python-fixture-venv`). Snippet cases are left out of its
"output type-checks" cases, as they are of Rust's. **"placeholder output compiles"** picks
the pack's language and the program its check runs from the pack's name: `elm` and `elm`,
`python-htmx` and `python`/`uv`, anything else `rust`/`cargo`. Every Python check goes
through `missingTool` when `uv` is absent, except that the placeholder group looks for its
tool only after planning, which without the tool already failed
([Rough spots](#rough-spots)).

**`packs/olifant-dev`** is the pack Olifant uses on its own source. Its
`unit` generator creates `src/Olifant/X.hs` plus `src/Olifant/XSpec.hs`, and its
`internal-module` generator creates `src/Olifant/X/Y.hs`.

**`scripts/`**:
- `try-pack.sh PACK DIR` copies a pack's fixture app into DIR, configures the pack and
  commits it to a fresh git repo. Every example in these books runs in a sandbox like that.
- `install-olifant.sh` runs `cabal install` into `~/.local/bin`.
- `test-watch.sh` runs `ghcid` over the test suite and re-runs it on every save.
- `record-pyright.sh DIR` records pyright's JSON report for a sample project
  (`uvx pyright --outputjson`, paths made relative to `DIR`) as `DIR/report.json`: the
  testdata under `src/Olifant/Language/testdata/pyright/` (a union, an enum, a qualified
  import, two matches in one file, a broken file, a clean run).
- For a fixture with a `pyproject.toml`, `try-pack.sh` runs `uv sync` in the sandbox, and it
  accepts a pack that has a `languages/` directory and no generators yet.

**`.githooks/`** (enable with `git config core.hooksPath .githooks`):
- `post-commit` and `post-merge` reinstall `olifant` when you are on `main`.
- `pre-merge-commit` runs `cabal test` (with `OLIFANT_REQUIRE_TOOLS=1`, and
  `~/.ghcup/bin` and `/opt/homebrew/opt/rustup/bin` on `PATH`, so a missing `ast-grep`,
  `elm` or `cargo` fails the merge) and `olifant check` (regions and rules) before a merge
  into `main` completes.

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
| **Suggest** | "Did you mean …?": the closest of a list of names | `Suggest.hs`: `closest` | none |
| **Naming** | Identifiers as lists of words, rendered in any case | `Naming.hs`: `Name`, `parseName`, `render`, `Case`, `pluralize`, `singularize` | none |
| **Toml** | TOML parsed into a small `Tree`, plus decoders with readable errors; the only module that knows the `toml-parser` API | `Toml.hs`: `Tree`, `Table`, `parseToml`, `keyOrder`, `required`, `optional`, `inKey`, `text`, `list`, `table`, `onlyKeys` | Suggest |
| **Glob** | `**` / `*` path globs | `Glob.hs`: `matchesGlob`, `globPrefix` | none |
| **Template** | The `{= … =}` language, holes included: parse, render, exemplars | `Template.hs` over `Template/{Syntax,Parse,Render,Value,Exemplar,Error}`: `Template`, `parseTemplate`, `render`, `evaluate`, `Value`, `Env`, `exemplar`, `freeVariables` | Naming, Suggest |
| **Language** | Language adapters from packs (`languages/*.toml`), ast-grep rules, and the **Syntax** and **Checker** ports | `Language.hs` re-exports `Adapter`, `Rule`, `Syntax`, `Check`, `Diagnostics`, `Support` (`Language.Header`'s `headerEnd`, a file's leading comments and docstring, is used by the anchors and the fill phase): `Language`, `parseLanguage`, `languageFor`, `Rule`, `renderRule`, `Syntax`, `noSyntax`, `Checker`, `noChecker`, `processChecker`, `compileClean`, `CompilerSupport`, `typeNames`, `supportFor`. `checkProgram`, `indentBlockEnd`. `Language.Elm`, `Language.Ghc`, `Language.Pyright` and `Language.Rustc` (one `CompilerSupport` record each, spec §5.6) and `Language.AstGrep` (the ast-grep adapter) are internal; only `AstGrep` is imported directly, by `Main`. `HoleKind` is an adapter's `[holes.KIND]`. The **Runner** port is `Language.Test`'s (`Runner`, `TestRun`, `Failure`, `TestId`, `parsePytest`, `testsForFile`; an adapter's `[test]` is its `TestDef`), the process behind it `Check`'s `runTestsIn`; `Language.Presets` (internal) fills a `[check]` or `[test]` table from its `preset` | Glob, Process, Template, Toml |
| **Process** | Run an external program with byte stdin/stdout/stderr; never throws; stop it (and its process group) when a `Cancel` token trips | `Process.hs`: `runBytes`, `runBytesWithin`, `runBytesCancellable`, `Outcome`, and `Process/Cancel`'s `Cancel`, `newCancel`, `neverCancel`, `cancelled` | none |
| **Fill** | The **Filler** port: what a model is asked for typed holes, and what it answers | `Fill.hs`: `FillHole`, `FillRequest`, `Filler`, `ModelFiller` | none |
| **Model** | `[model]` config and the real fillers: request and reply formats per backend, called with `curl` | `Model.hs`: `Backend`, `ModelConfig`, `parseModelConfig`, `backendKinds`, `backendModel`, `systemPrompt`, `userPrompt`, `requestBody`, `decodeFills`, `curlFiller`, `probeBackend` | Fill, Process, Toml |
| **Holes** | Typed holes inside a plan's text: markers, placeholders, final names, matching a hole against anything | `Holes.hs`: `Marker`, `HoleSite`, `splitMarkers`, `hasMarkers`, `placeholderize`, `placeHoles`, `matchesWithHoles` | Language, Template |
| **Workspace** | The filesystem **port**, with disk and in-memory adapters | `Workspace.hs`: `Workspace`, `PathState`, `pruned`, `diskWorkspace`, `memoryWorkspace`, `within`, `overlay`, `normalisePath`, `isSafeRelative`, `overlayMany` | Process (to run `git`) |
| **Query** | Facts from the repo as records (globs, commands printing JSON, AST queries selecting nodes, or the cases the compiler says a `match` at the cursor lacks), the **Queries** port, and what an enum's choice is | `Query.hs`: `Query` (with `UncoveredQuery`), `AstQuery`, `AstSelect`, `Record`, `runQuery`, `renderQuery`, `eachFields`, `Queries`, `QueryFailure`, `failed`, `noQueries`, `processCommand`, `ChoiceValue`, `choiceValue`, `queryVariables` | Glob, Language (`Rule`), Naming, Process, Template, Toml |
| **Anchor** | `at` phrases and where they put text, plus AST queries over a target's items | `Anchor.hs` re-exports `Anchor/Phrase`, `Anchor/Resolve` and `Anchor/Items`: `parsePhrase`, `resolveLayers`, `Located`, `Opening`, `Spacing`, `Miss`, `insertLines`, `insertedText`, `placedText`, `presentAt`, `locateContainer`, `itemsOfTarget`, `astQueries` | Holes, Language, Query, Suggest, Template |
| **Generator** | A generator definition: the types, loading `generator.toml`, resolving `one_of` choices, turning `--set` values into a template environment | `Generator.hs` re-exports `Definition` (including `ResolveFailure`, why a `[[uses]]` ref or step didn't resolve, `Choices`, and a snippet's `Cursor` edit and `HoleSource`) and `Inputs` (including `envBefore`, `inputsUsed`), plus `parseGenerator`, `withChoices`, `needsChoices`, `ChoiceView`, `choiceViews` | Anchor, Language, Naming, Query, Template, Toml |
| **Cursor** | Snippets at the cursor (spec §5.3): a cursor edit placed as a replacement of the cursor's line, indented to fit, or like the node an `after` rule says it follows | `Cursor.hs`: `isSnippet`, `atCursor`, `atCursorIndented`, `cursorIndent`, `indentTemplate`, `afterIndent`, `afterRule` | Generator, Language, Template |
| **Snippet** | Typed holes with a source (spec §4.9), and what an editor expands: ghost lines and an LSP snippet body | `Snippet.hs`: `Snippet`, `withHoleSources`, `holeSource`, `snippetOf`, `lspEscape` | Generator, Holes, Template |
| **Uncovered** | The cases the compiler says the `match` at the cursor lacks: the `uncovered` query | `Uncovered.hs`: `uncoveredAt`, `uncoveredQuery` | Glob, Holes, Language, Query, Template, Workspace |
| **Region** | Owned regions: parse `regions.toml`, render a region body, splice it between fences. All pure | `Region.hs`: `RegionDefWith`, `parseRegions`, `renderRegion`, `applyRegion`, `fenceLines` | Query, Template, Toml |
| **Rules** | Rules (spec §5.9): parse `rules.toml` and the project's `[rules]`, check files against them, honour and report the exceptions, fingerprint findings for the baseline | `Rules.hs`: `RuleDef`, `RuleCheck`, `Severity`, `RulesConfig`, `Finding`, `Exception`, `parseRules`, `parseRulesConfig`, `parseSeverity`, `checkFiles`, `fingerprint` | Actions (`matchPathSlots`), Glob, Language, Template, Toml |
| **Newness** | Which lines of a file are new since a base text (spec strangler §2.1): Myers' O(ND) diff over lines (cost follows the edits, not the file; past 1,000 edits lines pair by text), common prefix and suffix trimmed, a moved line not new | `Newness.hs`: `NewLines` (`AllNew`, `NewIn`, `NoBase`), `newLinesOf`, `lcsPairs`, `wholeNew`, `newCount` | none |
| **Migration** | The strangler's report and its pure parts: the switch registry, `olifant:switch` markers, what a PR extracted and added, big changes, whether a function reads or writes, the tables its SQL names, a switched function's text | `Migration.hs`: `Switch`, `MigrationReport`, `readRegistry`, `switchMarkers`, `unregistered`, `reportLine`, `migrationJson`, `bigChanges`, `readsOrWrites`, `tablesTouched`, `functionText` | Newness |
| **Mutate** | Mutation testing's makings: operators (an ast-grep rule and its fix, or a transform of the matched text), the sites they fire on in the lines in scope, the waivers a line carries, and the judge that runs each mutant in its own shadow, N at a time. `Mutate/Python` holds Python's operators | `Mutate.hs`: `Operator`, `Mutant`, `mutantsOf`, `capMutants`, `waiverOn`, `waiverLine`, `judge`, `Verdict`; `Mutate/Python`'s `pythonOperators`. Internal: `Mutate/Run`, `Mutate/Types` | Language |
| **Block** | The incremental loop's block as a pure state machine (Red, Drafting (with the state it began from, which `DraftFailed` puts back), Green, Judged, Broken, Accepted, Rejected; `Accepted` only from a `Judged` with every survivor waived), the moves open in each state, the HUD line; and the draft templates (a function body or a test's arrangement as a hole, the instruction, a kill's stub) | `Block.hs`: `Block`, `BlockState`, `Kind`, `Draft`, `Move`, `begin`, `step`, `open`, `hud`, `survivors`, `killTarget`, `pending`, `draftTemplate`, `testDraftTemplate`, `existingTestTemplate`, `testStub`, `killStub`, `stubNameFor`, `uniqueTestName`, `instructionFor`. Internal: `Block/Draft` | Language, Mutate, Template |
| **Decide** | Typed decisions, progressively (Jev-shaped; today `curlDecider` asks any chat backend named by `[decide] model`, no Jev backend exists): a `Question` of a kind with its options, a `Decider` that answers one or is `Unsure`; validated, so an answer outside the options is `Unsure`. Nothing depends on an answer arriving | `Decide.hs`: `Question`, `Answer`, `Decider`, `noDecider`, `validated`, `decisionPrompt`, `decodeAnswer`, `curlDecider`, `recordedDecider` | Model, Process |
| **Generation** | Running generators: expand `[[uses]]`, bind `[query.NAME]` queries, render edits, plan them, check, refresh regions, fill holes, write | `Generation.hs`: `generate`, `Step`, `Mode`, `Resolve`, `describeGenerationError`, `refreshRegions`, `regionInstances`; re-exports `Generation/Plan` (the `Plan` type) and `Generation/Tools`. Internals: `Compose`, `Render`, `Planner`, `Complete`, `Regions`, `FillPhase` | Anchor, Fill, Generator, Glob, Holes, Language, Query, Region, Snippet, Template, Workspace |
| **Actions** | Code actions (spec §4.6): which generators' `[applies_at]` match a cursor or a whole file, with the inputs the path and the code there give, and the near misses; snippets for a completion menu (spec §4.9) | `Actions.hs`: `Action`, `Range`, `FileAction`, `Nearby`, `Completion`, `actionsAt`, `actionsInFile`, `completionsAt`, `describeRule`, `matchPathSlots` | Cursor, Generator, Language, Query, Template |
| **Doctor** | `olifant doctor` (spec §4.7): what generators need here, and how to fix what is missing | `Doctor.hs`: `Status`, `Check`, `DoctorInput`, `DoctorPorts`, `doctor` | Language, Model |
| **Catalog** | Finding packs and generators, checking a generator's sources and fixed `at` targets, loading one into a runnable `Generator` (with its `[holes.NAME]` sources in the hole markers), reading `[model]`, and loading regions and rules | `Catalog.hs`: `openProjectCatalog`, `findEntry`, `loadGenerator`, `checkEntry`, `resolver`, `Catalog` (with `catalogModel`, `catalogProject`, `catalogPacks`, `catalogRules`, `catalogBrokenRules`, `catalogRulesConfig`), `CatalogEntry` | Anchor, Generator, Language, Model, Region, Rules, Snippet, Template, Toml, Workspace |
| **ProjectModel** | The project in a shared vocabulary (services, commands, ports, adapters, … and `uses`, `implements`, `calls`, `covers`), by each pack's `model.toml` | `ProjectModel.hs`: `Kind`, `RelKind`, `Element`, `Relation`, `ProjectModel`, `ModelMap`, `parseModelMap`, `buildModel` | Query, Template, Toml |
| **App** | The operations the CLI and the language server share, over a **Ports** record (files, syntax, checker, verifier, test runner, clock, config dirs, doctor) and a loaded **Project** (root, catalog, workspace) | `App.hs`: `Ports`, `Project`, `nearestProject`, `openProject`, `listGenerators`, `showGenerator`, `actionsAt'`, `actionsIn`, `completeAt`, `findings`, `planWith`, `doctorChecks`, `projectModel`, … | Actions, Anchor, Catalog, Cursor, Doctor, Fill, Generation, Generator, Holes, Language, Model, ProjectModel, Query, Rules, Snippet, Template, Uncovered, Workspace |
| **Output** | What Olifant says, as JSON and text: plans, lists, `show`, file actions, doctor, errors. The CLI prints it; the language server sends the same JSON | `Output.hs`: `planJson`, `planHuman`, `listJson`, `showJson`, `fileActionsJson`, `doctorJson`, `errorJson`, … | Actions, Anchor, Catalog, Cursor, Doctor, Generation, Generator, Holes, Language, Snippet, Template |
| **Recognize** | Convention recognition: which generator hand-written code is, how sure, with which inputs — by place (output paths, inverted) and shape (token-shape shingles weighted by rarity), banded; an optional model for the unsure | `Recognize.hs` over `Recognize/{Shape,Weights,Paths,Index,Inputs,Types,Ask}`: `recognizeIn`, `Recognition`, `Band`, `buildIndex`, `Convention`, `Index`, `pathPattern`, `invertPath`, `shapes`, `shingles`, `weightsOf`, `containment`, `inputsFor`, `Recognizer`, `consult`, `curlRecognizer` | Generator, Glob, Model, Naming, Process, Template |
| **Preview** | The generate picker's form and placeholders: each input's value and where it came from (you, the file's folders, the cursor, a lone choice, a default, a `+ New` step), checked as `olifant gen` checks it; marks a missing name is planned with, and their renderings back to `⟨name⟩` | `Preview.hs` over `Preview/{Fields,Marks}`: `Field`, `Source`, `fieldsFor`, `markValue`, `unmark` | Generator, Naming |
| **Server** | The language server's state and answers, with no LSP types: open documents (overlays), projects per root, caches, debounced jobs, and each feature as a function of the state | `Server.hs` over `Server/{State,Jobs,Edits,Actions,Fixes,Hints,Completion,Suggest,Holes,Regions,Model,Recognize,Preview,Block,BlockMoves,BlockDecide,Tests}`: `ServerState`, `projectFor`, `memo`, `editsOfPlan`, `findingsFor`, `actionsFor`, `suggestionFor`, `regionEdits`, `holesFor`, `modelFor`, `recognitionFor`, `suggestOrRecognize`, `previewFor`, `stepsPlanWith`, `submit`, the loop's `blockBegin`, `blockDraft`, `blockDraftAt`, `blockTest`, `blockJudge`, `blockKill`, `blockStub`, `blockWaive`, `blockChanged`, `blockAccept`, `blockReject`, `blockJson`, `testsRun`, … | Actions, Anchor, App, Block, Catalog, Cursor, Decide, Fill, Generation, Generator, Holes, Language, Migration, Model, Mutate, Naming, Newness, Preview, Process, ProjectModel, Recognize, Rules, Snippet, Template, Workspace |
| **Lsp** | `olifant lsp`: the adapter between the protocol (the `lsp` library) and `Server` — handlers, positions, `WorkspaceEdit`s, progress, background work | `Lsp.hs`: `runLsp`, `runLspWithHandles` (specs), `serverDefinition` | Actions, App, Block, Catalog, Generation, Output, Rules, Server, Snippet, Workspace |
| **Cli** | The command line: parse arguments, call the use cases, print JSON or text, pick the exit code | `Cli.hs`: `Runtime`, `DoctorPorts` (re-exported for `Main`), `runCli`. Internal: `Cli/Options` (argument parsing) | almost everything, `Lsp` for `olifant lsp` |
| **GeneratorTesting** | Running a pack's test cases against its fixture (the seed of `olifant test`), replaying recorded fills, placing a snippet case's cursor | `GeneratorTesting.hs`: `discoverCases`, `runCase`, `preparePack`, `parseCaseInputs`, `parseCaseCursor`, `atCaseCursor` | Anchor, Catalog, Cursor, Fill, Generation, Generator, Language, Query, Toml, Uncovered, Workspace |

`GeneratorTesting` isn't used by the CLI yet. Only `GeneratorTestingSpec` calls it.
Fill and Suggest have no spec file of their own: Fill is types only, tested through
`GenerationSpec`'s fake fillers, and Suggest through the error messages that use it.

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
  │ Lsp                                                        │  the protocol adapter (`olifant lsp`)
  ├────────────────────────────────────────────────────────────┤
  │ Server                                                     │  documents, projects, caches, jobs; no LSP types
  ├────────────────────────────────────────────────────────────┤
  │ App                         Output                         │  shared operations; what Olifant says
  ├────────────────────────────────────────────────────────────┤
  │ Catalog                                                    │  finding and loading generators, regions, rules
  ├────────────────────────────────────────────────────────────┤
  │ Rules                                                      │  conventions as checks (uses Actions' path patterns)
  ├────────────────────────────────────────────────────────────┤
  │ Generation  Actions  Doctor  Recognize  Preview            │  the use cases: steps → plan → writes; actions; doctor
  ├────────────────────────────────────────────────────────────┤
  │ Cursor           Snippet          Uncovered                │  snippets: the cursor, tab stops, missing cases
  ├────────────────────────────────────────────────────────────┤
  │ Generator          Region          ProjectModel            │  definitions (generator.toml, regions.toml, model.toml)
  ├────────────────────────────────────────────────────────────┤
  │ Anchor                                                     │  placing text (`at` phrases, AST queries)
  ├────────────────────────────────────────────────────────────┤
  │ Query            Holes            Model                    │  reading facts; holes in a plan; models
  ├────────────────────────────────────────────────────────────┤
  │ Language                    Workspace                      │  adapters as data; Syntax and Checker ports; files
  ├────────────────────────────────────────────────────────────┤
  │ Template                                                   │  {= … =}
  ├────────────────────────────────────────────────────────────┤
  │ Naming     Toml     Glob     Process     Fill              │
  ├────────────────────────────────────────────────────────────┤
  │ Suggest                                                    │  no Olifant imports
  └────────────────────────────────────────────────────────────┘
```

The exact edges, one line per unit:

```
Naming, Glob, Process, Fill, Suggest → (nothing)
Toml                 → Suggest
Template             → Naming, Suggest
Language             → Glob, Process, Template, Toml
Workspace            → Process             (for `git ls-files`)
Query                → Glob, Language, Naming, Process, Template, Toml, Workspace     (Language.Rule, Workspace.pruned)
Holes                → Language, Template  (Language.Adapter directly)
Model                → Fill, Process, Toml
Anchor               → Holes, Language, Query, Suggest, Template
Region               → Query, Template, Toml
Generator            → Anchor, Language, Naming, Query, Template, Toml
Cursor               → Generator, Language, Template          (Generator.Definition, Template.Syntax directly)
Snippet              → Generator, Holes, Template   (Generator.Definition, Template.Syntax directly)
Uncovered            → Glob, Holes, Language, Query, Template, Workspace
Generation           → Anchor, Fill, Generator, Glob, Holes, Language, Query, Region, Snippet, Template, Workspace
Actions              → Cursor, Generator, Language, Query, Template
Doctor               → Language, Model
Rules                → Actions, Glob, Language, Template, Toml
ProjectModel         → Query, Template, Toml
Catalog              → Anchor, Generator, Language, Model, ProjectModel, Region, Rules, Snippet, Template, Toml, Workspace
App                  → Actions, Anchor, Catalog, Cursor, Doctor, Fill, Generation, Generator, Holes, Language, Model, ProjectModel, Query, Rules, Snippet, Template, Uncovered, Workspace
Output               → Actions, Anchor, Catalog, Cursor, Doctor, Generation, Generator, Holes, Language, Snippet, Template
Preview              → Generator, Naming
Recognize            → Generator, Glob, Model, Naming, Process, Template
Server               → Actions, App, Catalog, Cursor, Fill, Generation, Generator, Holes, Language, Model, Naming, Preview, Process, ProjectModel, Recognize, Rules, Snippet, Template, Workspace
Lsp                  → Actions, App, Catalog, Generation, Generator, Output, Preview, Process, Recognize, Rules, Server, Snippet, Workspace
Cli                  → Actions, Anchor, App, Catalog, Cursor, Doctor, Fill, Generation, Generator, Language, Lsp, Output, Region, Rules, Template, Toml, Workspace
GeneratorTesting     → Anchor, Catalog, Cursor, Fill, Generation, Generator, Language, Query, Toml, Uncovered, Workspace   (Generator.Definition directly, for apAfter)
app/Main             → Cli, GitInit, Language, Language.AstGrep, Model, Workspace
```

`Anchor` importing `Query` is the one edge worth pausing on: an `at` target and an `ast`
query both end up asking the same question ("what are this container's items?"), so
`itemsOfTarget` (`Anchor/Resolve.hs:191`) is shared, and `Anchor.Items.astQueries` builds
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
app/Main.hs:15                         import Olifant.Language.AstGrep (astGrepSyntax)
src/Olifant/Cli.hs:26                  import Olifant.Template.Syntax (Segment (..), Template (..))
src/Olifant/Cli.hs:36                  import Olifant.Generator.Schema (generatorSchema)
src/Olifant/Cursor.hs:23               import Olifant.Generator.Definition (EditWith (..), Generator, GeneratorWith (..))
src/Olifant/Cursor.hs:24               import Olifant.Template.Syntax (Segment (..), Template (..))
src/Olifant/GeneratorTesting.hs:25     import Olifant.Generator.Definition (AppliesAt (..), GeneratorWith (..))
src/Olifant/GeneratorTesting.hs:28     import Olifant.Language.Adapter (Language (..), languageFor)
src/Olifant/Holes.hs:20                import Olifant.Language.Adapter (HoleKind (..), Language (..), languageFor)
src/Olifant/Query.hs:46                import Olifant.Language.Rule (Rule (..), renderRule, ruleFromTree)
src/Olifant/Snippet.hs:19              import Olifant.Generator.Definition (GeneratorWith (..), HoleSource (..))
src/Olifant/Snippet.hs:21              import Olifant.Template.Syntax (Segment (..), Template (..))
src/Olifant/Uncovered.hs:19            import Olifant.Language.Adapter (CheckDef (..))
src/Olifant/Uncovered.hs:20            import Olifant.Language.Check (Checker (..), describeCheckFailure)
src/Olifant/Uncovered.hs:21            import Olifant.Language.Diagnostics (CheckReport (..), MissingPatterns (..))
```

`Main` importing an adapter directly is fine: wiring adapters together is `Main`'s job.
`Holes` imports `Language.Adapter` rather than the `Language` facade, which would do as well;
it is a small inconsistency, not a layering problem.
`Cli` is the only user of the JSON Schema `olifant schema` prints, so the `Generator` facade
does not re-export it.
The snippet units go around facades more than the rest. `Cursor` and `Snippet` build and
take apart `Template` segments (`Lit`, `Hole`), which the `Template` facade doesn't export,
and `Cli` does the same to build a fix's replacement. `Snippet` imports
`Generator.Definition` for `HoleSource`, which the facade does re-export, and `Uncovered` and
`GeneratorTesting` reach into `Language.*` for names the `Language` facade also exports. None
of these is a layering problem: `Template` could export its segments, and the rest could
import the facades they skip.
`Generation/Complete.hs` used to be a third exception here: it imported `Olifant.Language.Elm`
directly and branched on `langQualify lang == Just "elm"` to decide whether to qualify a
pattern. Step 2c (compiler support, below) closed that gap — the check loop now goes through
a `CompilerSupport` record the adapter's `[check] format` selects, and `Generation/Complete.hs`
imports nothing from `Language.Elm` or `Language.Ghc` at all.

## Ports and adapters

Olifant touches five things outside its own memory: the filesystem, ast-grep, a language's
compiler, arbitrary commands (for queries; AST queries reuse ast-grep through the Syntax
port instead of touching anything new), and a model that fills typed holes. `olifant doctor`
adds a sixth: the process's own environment (what is on its `PATH`, which variables are set)
and a quick probe of the model. `olifant check --changed-since` adds a seventh: git, for the
lines a branch changed. The strangler adds `pBaseText` (`Process.gitBaseText`: a file's text at
the merge-base of `HEAD` and a ref, `git merge-base` then `git show`), which the language server
caches per file until the files change on disk (`Server/State.cachedBase`); it follows a rename
(`git diff -M`) to the file's old path. `pBaseFiles` (`Process.gitBaseFiles`, `git ls-tree` at
the merge-base) lets the report count the base over the base's own files. Each is a **port**. A port is a record of functions,
parameterised by the monad `m` it runs in:

```haskell
-- src/Olifant/Workspace.hs:30
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

-- src/Olifant/Fill.hs:30
newtype Filler m = Filler {runFill :: FillRequest -> m (Either Text (Map Text Text))}

-- src/Olifant/Doctor.hs:36
data DoctorPorts m = DoctorPorts
  { dpFind :: String -> m (Maybe FilePath)   -- an executable on this process's PATH
  , dpPath :: m Text                         -- this process's PATH, to show
  , dpProbe :: Backend -> m (Either Text ())  -- does the model answer (a short timeout)
  , dpEnvSet :: Text -> m Bool               -- is an environment variable set (never its value)
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
reach outside Olifant's memory, so they travel together in one port. Both fail with a
`QueryFailure`: the message, and whether the only problem is a missing tool. `failed why`
(`:181`) is the ordinary kind; `processCommand` sets `failureToolMissing` when the program is
not found (`` `cargo` is not installed ``), and `astQueries` when the Syntax port says
ast-grep is missing (`Anchor/Items.hs:44`, `:47`). Callers that add context update only the
message (``f {failureWhy = "query `" <> name <> "`: " <> failureWhy f}``), so the flag survives
to `generationToolMissing` (`Generation.hs:62`), which pack tests use to make such a case
pending, as a missing tool is everywhere else (spec §4.5, §5.7).

| port | defined at | real adapter | no-op / test adapters |
|---|---|---|---|
| `Workspace m` | `Workspace.hs:30` | `diskWorkspace` (`Workspace.hs:72`): reads and writes under a root; lists files with `git ls-files` when inside a git work tree, otherwise walks the tree. `within dir` (`:152`) views a workspace from a subdirectory. `overlay path text` (`:134`) wraps any workspace so that one file reads as `text` (an editor's unsaved buffer, `gen --overlay`): `readText`, `fileExists` and `inspectPath` see it, `listFiles` includes it even when it is not on disk, and writes and removes go to the wrapped workspace untouched | `memoryWorkspace` (`Workspace.hs:45`): a `Map` in an `IORef`, which also hands back an action that reads the final map, so tests can assert on "the disk" |
| `Syntax m` | `Language/Syntax.hs:15` | `astGrepSyntax` (`Language/AstGrep.hs:33`): runs `ast-grep scan --stdin --inline-rules … --json=compact`; compiles a pack's vendored grammar with `cc` into the cache on first use | `noSyntax` (`Language/Syntax.hs:17`) always says "ast-grep is not installed"; `fakeSyntax :: Syntax Identity` in `AnchorSpec.hs:58` returns canned matches |
| `Checker m` | `Language/Check.hs:36` | `processChecker` (`Check.hs:49`): copies the files into a temp dir, runs the adapter's `[check] command` there, and parses the report (stderr, or stdout when the support's `supportReadsStdout` says so, as cargo's and pyright's do) through the `CompilerSupport` its `format` names (`elm-json`, `ghc-json`, `pyright-json`, `rustc-json`), then lets the support read the checked files for the sites' extents (`supportSpans`) | `noChecker` (`Check.hs:39`); `scriptedChecker` (`GenerationSpec.hs:545`) replays a list of reports, one per round |
| `Filler m` | `Fill.hs:30` | `curlFiller timeout backend` (`Model.hs:170`): posts the request with `curl` to an OpenAI-compatible server or to Anthropic's API, and decodes the reply into fills by hole name | `replayFiller` (`GeneratorTesting.hs:143`) answers from a pack case's `fills.toml`; `withFiller` (`GenerationSpec.hs:615`) replays a list of replies, one per request, and counts the requests, so a spec can say "one request, then a retry with only the failing hole" |
| `rtGitChanged` | `Cli.hs:54` | `gitChanged` (`app/Main.hs:58`): `git -C ROOT diff --relative --no-color -U0 REF --`, each `+++ b/FILE` then each `@@ -a,b +c,d @@` hunk's new lines `c .. c+d-1` (a `d` of 0, a pure deletion, adds no range), and `git ls-files --others --exclude-standard` for untracked files; either command failing is `Left` with git's message | `CliSpec.hs:76`: `\_ _ -> pure (Right ([], ["src/b.x"]))` |
| `DoctorPorts m` | `Doctor.hs:36` | built in `app/Main.hs:42` from `findExecutable`, `lookupEnv "PATH"`, `probeBackend` (`Model.hs:213`) and `isJust <$> lookupEnv` | `ports` in `DoctorSpec.hs:25` (these executables exist, the probe answers this, these variables are set); `CliSpec.hs:76` finds every executable except `nope…` |
| `Queries m` | `Query.hs:186` | `processCommand dir` (`Query.hs:200`): runs a program (a bare name from `PATH`, or a project path like `bin/list`) in the project dir, for `queryCommand`; `astQueries syntax langs` (`Anchor/Items.hs:20`): evaluates an `ast` query's target over every matching file, for `queryAst` | `noQueries` (`Query.hs:193`); `QuerySpec.hs:46`'s `fake out = noQueries {queryCommand = …}` overrides one field and leaves `queryAst` as the no-op |

The ports are bundled at two levels. `Tools m` (`src/Olifant/Generation/Tools.hs:11`) holds
Syntax, Checker and Queries, plus the catalog's language adapters and regions, and, since
holes, `toolFiller :: Maybe (ModelFiller m)` (`:18`): the model that fills holes, with its
name and retry count, or `Nothing`. Its last field, `toolProgress :: ProgressEvent -> m ()`
(`:22`), is where a generation reports what it is doing; `ProgressEvent` (`:27`) is `Checking
language | Filling holes attempt model | Filled hole attempt | Unfilled hole why`. `basicTools`
sets it to `const (pure ())`, so nothing reports unless the CLI asks (`gen --progress`, step 5
below). It is what `generate` takes. `noTools` (`:30`) needs nothing installed. `Runtime`
(`src/Olifant/Cli.hs:39`) is the CLI's port: the root workspace, the project dir, stdin, the
clock, stdout, stderr, whether stdout is a terminal, the Syntax and Checker adapters,
`rtDoctor`, the `DoctorPorts` only `olifant doctor` uses, and `rtGitChanged` (`:54`), the
git port only `check --changed-since` uses:

```haskell
-- src/Olifant/Cli.hs:52
  , -- | lines changed since a git ref (file, 1-based inclusive ranges) and
    -- untracked files, for `check --changed-since`
    rtGitChanged :: FilePath -> Text -> IO (Either Text ([(FilePath, [(Int, Int)])], [FilePath]))
```

It is a single function in `IO`, not a record parameterised by `m`, because only the CLI
uses it and the CLI runs in `IO`; a test replaces the function.
`app/Main.hs` is the only place that builds a real one:

```haskell
-- app/Main.hs:28 (excerpt)
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
        , rtGitChanged = gitChanged
        , rtDoctor =
            DoctorPorts
              findExecutable
              (T.pack . fromMaybe "" <$> lookupEnv "PATH")
              probeBackend
              (fmap isJust . lookupEnv . T.unpack)
        }
      $ args
```

`CliSpec.hs:54` (`runWith`) builds the test version: `memoryWorkspace` for the root, output
collected in `IORef`s, a fixed clock, `noSyntax`, `noChecker`, `DoctorPorts` that find
every executable not named `nope…`, answer every probe and see no environment variables, and
an `rtGitChanged` (`CliSpec.hs:76`) that reports no changed lines and one untracked file,
`src/b.x`. Every CLI behaviour, from
exit codes and JSON shapes to `--steps -` reading stdin, is tested without a process, a
terminal or a disk.

**Why tests use fakes.** Speed is one reason: the CLI spec's 57 examples run in under 10 ms.
The main reasons are that a fake can produce situations the real tool rarely does and that
the test says exactly what it assumes. `scriptedChecker` returns "missing `Reset`" and then
the same thing again, which is how `completeEdits` is tested to stop instead of adding the
same branch forever. `fakeSyntax` returns a nested match, which is how the "direct children
only" rule in anchor resolution is tested. The real adapters are still tested, but
separately: `LanguageSpec.hs:287` runs real `elm make`, `LanguageSpec.hs:103` real
`ast-grep`, and the pack snapshot tests run both (plus real `cargo check` on rust-htmx
output). Those tests become *pending*, not failed, when the tool isn't installed (`conflictToolMissing`, `src/Olifant/Generation/Plan.hs:109`).

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

### 1. `main` builds the world (`app/Main.hs:21`)

`main` reads the arguments, the current directory, whether stdout is a terminal, and the
home, XDG config and XDG cache directories. It builds the `Runtime` shown above and calls
`runCli`. `try` catches any exception that escapes. That should never happen, and it's
reported as `olifant: internal error: …` with exit 1 (`:52`). Otherwise `runCli`'s
`ExitCode` becomes the process exit code.

The workspace is rooted at `/` (`diskWorkspace "/"`), and paths inside Olifant are absolute
until something narrows them with `within`.

### 2. `runCli` finds the project, then parses and opens the catalog (`src/Olifant/Cli.hs:48`)

```haskell
-- src/Olifant/Cli.hs:48
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

**`parseOptions`** (`src/Olifant/Cli/Options.hs:79`) returns `Either (Text, ExitCode)
Options`: either text to print with an exit code, or parsed options. Most commands go
through `optparse-applicative` (`:87`). `gen` does not. It's matched first (`:80`) and
handed to the hand-written `parseGen` (`:121`), because optparse can't express "steps
separated by a standalone `+`". `parseGen` is a loop, `go`, over the remaining words. It
peels off the global flags (`--json`, `--human`, `--plan`, `--now`, `--steps`) wherever they
appear, and starts a new step at each `"+"` (`:149`). Before it runs, `pull` (`:130`, called
at `:124`) takes out `--model NAME` and `--no-fill`, which also apply to the whole run, and
they are put into the result's `genModel` and `genNoFill` (`:71`, `:73`). `finish` (`:151`) turns each step into
a `StepSpec`. The optparse definition of `gen` (`genP`, `:107`) exists only to render `gen
--help`. For our command the result is, in effect:

```
Options Nothing (GenCmd (GenOptions [StepSpec "elm/page" [("page","settings"),("msgs","toggle_dark_mode")]] Nothing False Nothing Nothing False))
```

That is: no format flag, one step, no `--steps` file, not `--plan`, no `--now`, no
`--model`, not `--no-fill`.

The **output format** is decided once (`Cli.hs:55`): the flag if given, otherwise human
output for a terminal and JSON when piped.

**`projectRoot`** (`Cli.hs:68`) walks up from `rtProjectDir rt` — the directory Olifant was
actually run from — looking for a `.olifant`, stopping once it reaches the home directory
(or the filesystem root). `cwd` keeps the original directory: `runCommand` uses it to resolve
file arguments (`where FILE`, `--steps FILE`) exactly where you typed them, while `rt'`
carries the found root for everything else (the catalog, `gen`'s writes, `sync`). With no
`.olifant` anywhere above it, `projectRoot` returns `start`, so behaviour is unchanged from
before this existed.

**`openProjectCatalog`** (`src/Olifant/Catalog.hs:106`) runs for every command, rooted at
whatever `projectRoot` found. It reads `[packs]` from `~/.config/olifant/config.toml` and
`.olifant/config.toml`. The project's entry wins for the same name
(`M.fromList (fromUser ++ fromProject)`, `:116`: a later key overwrites an earlier one). Then
`openCatalogAt` (`:156`) checks every configured pack path exists (`inspectPath`, `:158`)
before doing anything else — the first one that is not a directory fails the whole command
with `PackMissing` (`config_invalid`, exit 1), rather than silently acting as if the pack had
no generators. Once every pack checks out, `openAt` (`:163`) goes through each one:

- `discover` (`:185`) lists the files and keeps `generators/<name>/generator.toml`, exactly
  two levels deep. `load` (`:189`) parses each one with `parseGenerator`
  (`src/Olifant/Generator/Load.hs:30`) into a `GeneratorDef`. `[inputs.*]` is a TOML table,
  which `Toml.parseToml` reads into a `Map` that forgets the order keys were written in — so
  `parseGenerator` separately asks `Toml.keyOrder src ["inputs"]` (`Toml.hs:54`) for that
  order straight from the parse tree, and sorts `genInputs` by it (`Load.hs:39-40`). That is
  the only reason `show` and a generator's prompts list inputs in declaration order rather
  than whatever order the `Map` happens to iterate in.
- `languagesIn` (`Catalog.hs:173`) parses `languages/*.toml` into `Language` values with
  `parseLanguage` (`src/Olifant/Language/Adapter.hs:63`).
- `regionsIn` (`Catalog.hs:333`) reads `regions.toml`.
- `rulesIn` (`Catalog.hs:350`) reads `rules.toml` with `Rules.parseRules`, and `addRules`
  (`:358`) adds them; a rule replaces an earlier one with its id.

A generator, language, regions or rules file that fails to parse is kept aside
(`catalogBroken`, `catalogBrokenLanguages`, `catalogBrokenRegions`, `catalogBrokenRules`)
instead of failing the command. One bad generator never breaks unrelated ones. The
project's own `.olifant/generators/`, `.olifant/regions.toml` and `.olifant/rules.toml` are
added the same way, after the packs', so a project rule or region replaces a pack's with
the same name. The project config's `[rules]` table (severities and `[[rules.allow]]`) is
read by `Rules.parseRulesConfig` into `catalogRulesConfig` (`:131`); unlike a rules file, a
mistake there is a `ConfigInvalid` that stops every command, as a bad `[model]` does. Last,
`checkFixes` (`:364`) moves every rule whose `fix` names no catalog entry from
`catalogRules` to `catalogBrokenRules` (`"rule `X`: fix names no generator `Y`"`), so a
fix that is offered always exists.

So every invocation parses every generator in every configured pack. That's cheap at
today's sizes, but it is also why a typo in one pack's TOML shows up in `olifant list` for
all of them.

### 3. `runCommand` loads the generator (`src/Olifant/Cli.hs:124`)

`runCommand` now takes `cwd` (where Olifant actually ran) alongside `rt` (whose
`rtProjectDir` is the root `projectRoot` found). The `GenCmd` branch first gets its steps,
from the arguments or from `--steps FILE|-` (JSON, decoded by the `FromJSON StepSpec`
instance at `Options.hs:44`; `-` reads stdin, and a plain filename reads relative to `cwd`,
not the project root — a `--steps` file is as much "where you ran it" as the thing Olifant
reads for you). Then it loads each step:

```haskell
-- src/Olifant/Cli.hs:215
        loaded <- traverse (\(i, s) -> fmap (first (label i s,)) (either (pure . Left) (loadGenerator (rtRoot rt) (rtSyntax rt) cat) (findEntry cat (specGenerator s)))) (zip [1 :: Int ..] ss)
        case sequence loaded of
```

Read from the inside out:

1. **`findEntry cat "elm/page"`** (`Catalog.hs:200`) looks for an exact id first. Failing
   that, it looks for a bare name that is unique across packs (`page` would work). Then it
   checks the broken list, and only then reports "not found" or "pack not configured".
2. **`loadGenerator`** (`Catalog.hs:220`) turns the `GeneratorDef`, whose edit sources are
   still file references, into a runnable `Generator`, whose sources are parsed templates.
   It also needs the Syntax port now, because a fragment exemplar (below) has to search a
   file's syntax tree at load time, not just render text. It's still one line:

   ```haskell
   -- src/Olifant/Catalog.hs:220
   loadGenerator :: Monad m => Workspace m -> Syntax m -> Catalog -> CatalogEntry -> m (Either CatalogError Generator)
   loadGenerator root syntax cat entry = sequenceA <$> traverse resolve (entryDefinition entry)
   ```

   `resolve` (`:224`) handles each source:
   - `TemplateFile f` reads `templates/f` and parses it.
   - `ExemplarFile ref binds` reads the exemplar (`fixture:…` means the pack's
     `fixtures/app/`) and turns it into a template with `exemplar`
     (`src/Olifant/Template/Exemplar.hs:37`). That function replaces every case form of
     `sample_page` (`SamplePage`, `samplePage`, `SAMPLE_PAGE`, …) with the matching filter
     applied to `page`.
   - `FragmentFile ref target matching binds` (spec §4.3) is an exemplar cut out of a bigger
     file instead of read whole. It reads the same kind of file reference as `ExemplarFile`,
     finds the `target`'s items with `itemsOfTarget` (`Anchor/Resolve.hs:191`, the same
     function `end of`/`start of` anchors and `items_of` AST queries use), requires exactly
     one of them to match the regex `matching`, dedents it by the node's starting column
     (`dedent`, `Catalog.hs:246`, since only its first line sits at that column in the
     source), then turns the result into a template with `exemplar` just like `ExemplarFile`
     does.
   - `InlineText t` is already a template.

   `traverse` over a *generator* is possible because `GeneratorWith` derives `Traversable`.
   That trick gets its own section in [chapter 2](02-haskell-for-this-codebase.md#the-traversable-trick-same-shape-sources-then-templates).

   `loadGenerator` is also what `checkEntry` (`Catalog.hs:272`) calls first: `list` and `show`
   run it over every entry before anyone asks to `gen` it, so a missing template, a broken
   exemplar or fragment, or a fixed `at` target the language adapter doesn't have, shows up
   as `INVALID` (or fails `show`/`gen` the same way) instead of surfacing only at run time. A
   target phrase with a `{= … =}` in it can't be checked until its value is known, so those
   are still left to run time (`targetProblem`, `:281`).
3. `traverse` over the steps, then **`sequence loaded`**, turns "a list of results" into "the
   first error, or a list of generators", like `.collect::<Result<Vec<_>, _>>()`.

`one_of` enums (inputs whose choices come from a `[query.NAME]` over the project, spec
§4.1) are not resolved here: `generate` resolves them step by step during composition
(`Choose` in `Generation/Compose.hs:58`), against the project plus the files created by the
steps before, and with that step's own `--set` values, so a chain can choose what an earlier
step creates and a dependent input sees the service its step chose. `generate` lists the
project only if an input needs it (`needsChoices`). `elm/page` has none, so for this command
it is a no-op. (`show` and `list --detail` use the CLI's `resolveChoicesFor` and
`choiceViewsFor`, `Cli.hs:300` and `:310`, against the project as it is, which is what they
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
`choiceLines` (`:92`) as `service=billing:  a | b`, `(none)`, `query failed: …` or "depends
on service; set it to see the choices", and `inputsJson` (`:74`) as `choices`, or
`depends_on` with `choices_by` (keyed `billing`, or `a=x, b=y` for several inputs) and
`errors`.

Then the call into the core:

```haskell
-- src/Olifant/Cli.hs:373
          Right gens -> runSteps rt cwd fmt cat g [] (zipWith (\gen s -> Step gen (M.fromList (specSets s))) gens ss)
```

`runSteps` (`Cli.hs:409`) is shared with `olifant fix`, which passes a replacement step and
the fix's context files. It takes `cwd` so that it can resolve `--overlay FILE` the way every
other path argument is resolved:

```haskell
-- src/Olifant/Cli.hs:412
  let mode = if genPlanOnly g then PlanOnly else Apply
      project = within (rtProjectDir rt) (rtRoot rt)
  -- `--overlay FILE` (spec §4.8): plan against the editor's unsaved text.
  overlaid <- case genOverlay g of
    Nothing -> pure (Right project)
    Just _ | not (genPlanOnly g) -> pure (Left "--overlay needs --plan (an unsaved buffer is planned, never written)")
    Just f -> do
      text <- rtReadStdin rt
      pure (Right (overlay (makeRelative (rtProjectDir rt) (normalise (cwd </> f))) text project))
  case (overlaid, modelFor cat g) of
   (Left why, _) -> failWith rt fmt 2 "usage" why Nothing []
   (Right ws, filler0) -> case filler0 of
             Left why -> failWith rt fmt 2 "usage" why Nothing []
             Right filler ->
              generate ws (resolver (rtRoot rt) (rtSyntax rt) cat) ((projectTools rt cat) {toolFiller = filler, toolFillContext = context, toolProgress = …}) steps now mode >>= \case
```

The overlay is the whole feature on the engine's side. `generate` takes a `Workspace`, and
everything it does (placement, `unless`, queries, the parse pre-check, and the files handed to
the `Checker` for `complete` edits and fills) reads through it, so swapping one file's text
in the port is enough; no phase knows. The `--plan` guard is what makes that safe: with
`PlanOnly`, 4h writes nothing, so the overlaid text can never reach the disk. `parseGen`
(`Cli/Options.hs:162`) lifts `--overlay FILE` out of the arguments before the `+` split
(`takeOverlay`), like `--model` and `--progress`, so it applies to the whole run wherever it
appears; `fix` doesn't take it.

`modelFor` (`Cli.hs:587`) picks the model: `Nothing` with `--no-fill`, otherwise the backend
named by `--model`, else by `[model] default`, else `Nothing`. A name with no
`[model.backends.NAME]` is the `Left`, a usage error (exit 2) listing the backends there are.
The backend becomes a `ModelFiller` with the backend's model name (for the plan), `[model]
retries`, and `curlFiller (mcTimeout config) b`. `catalogModel` is the `[model]` the catalog
read (`Catalog.hs:124`): the project config's if it has one, else the user config's, else
none (`ModelConfig Nothing 2 120 []`).

The arguments, in order:
- the workspace narrowed to the project root (`rtProjectDir rt` here, since `rt` is the `rt'`
  `runCli` rebound — see step 2), so paths are now project-relative, regardless of `cwd`;
  with `--overlay`, wrapped in `overlay`
- `Catalog.resolver`, used for `[[uses]]` (a used generator's `one_of` is resolved by
  `generate`'s `choose`, like a top-level step's)
- the `Tools`, built by `projectTools rt cat` (`Cli.hs:604`) with `toolFiller` set to the
  model above: the real Syntax and Checker,
  the catalog's languages and regions, and `projectQueries rt cat` (`:609`) — `queryCommand
  = processCommand (rtProjectDir rt)`, `queryAst = astQueries (rtSyntax rt) (catalogLanguages
  cat)`
- the steps
- the time (`--now` or the clock)
- `Apply`

### 4. `generate`: from steps to a plan (`src/Olifant/Generation.hs:94`)

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
  context <- except (first (StepTemplate path) (traverse (render env) (genContext gen)))
  let own = map (PlanEdit path) rendered
  -- Each use sees what the run, this generator and earlier uses create.
  nested <- foldM (\acc (k, u) -> (acc ++) <$> expandUse env (prior ++ created (own ++ acc)) k u) [] (zip [1 :: Int ..] (genUses gen))
  pure ([PlanRequire path required hint | not (null required)] ++ [PlanContext context | not (null context)] ++ own ++ nested)
```

A `PlanContext` carries a generator's `context` globs, rendered with its inputs. The planner
skips it; `generate` collects every step's globs, reads the files they match (only when
some hole goes to the model) and appends them to the tools' `toolFillContext`, which the
fill phase sends as "Related file …".

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
  m`: `Cli`/`GeneratorTesting` pass in `generate`'s real `bindQueries` (`Generation.hs:187`),
  which renders each query's text against the environment so far (`renderQuery`,
  `Query.hs:254`) and runs it over the project's files (`runQuery`, `Query.hs:222`), reading
  through the same plan-aware view every phase uses. A query's records are bound as a
  `VList` under its name, so a template can loop over it. For example (`GenerationSpec.hs:87`):
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
  means that pack's generator first (`Catalog.resolveEntry`, `Catalog.hs:293`). Then
  `expand` recurses with the step path `["elm/page", "elm/msg[1]"]`. That path is the
  `[elm/page › elm/msg[1]]` tag in the output.

The result is one flat list of `PlanItem`s, a generator's own items before its children's:

```
PlanRequire ["elm/page"] ["src/Main.elm","src/Route.elm"] …
PlanEdit    ["elm/page"] (create, 6 inserts, 2 completes)
PlanRequire ["elm/page","elm/msg[1]"] ["src/Page/Settings.elm"] …
PlanEdit    ["elm/page","elm/msg[1]"] (1 insert, 1 complete)
```

#### 4b. Look at the files the plan touches (`Generation.hs:99`)

`generate` collects every target path and required path and calls `inspectTarget` (`:101`)
on each one. The result is a map, `originals :: Map FilePath (Maybe Text)`, where `Nothing`
means absent. A second map, `blocked`, holds paths that can't be written: a directory, a
file that isn't UTF-8, or a path whose parent is a file. The project's full file list is
fetched only if some edit has an `unless = { exists = … }` glob (`:107`).

From here on, "the files" means that map. Each phase takes the current contents and returns
new contents. Nothing touches the disk until the very end.

#### 4c. The phases (`Generation.hs:110`)

```haskell
-- src/Olifant/Generation.hs:110
          phases originals' = do
            (positional, pc, afterPositional, next) <- decideItems locate diskPaths items originals' blocked onDisk
            (compiled, cc, ws', afterComplete) <- completeEdits tools ws completes next afterPositional
            (refreshed, rc, current') <- refreshRegions SkipUnset tools ws (next + length compiled) afterComplete
            pure (positional ++ compiled ++ refreshed, pc ++ cc ++ rc, ws', current')
```

Each phase returns its edits, its conflicts, and the contents after it, and the next phase
starts from those contents. `ws'` here is the list of warnings, not a workspace.

**Phase 1: positional edits, `decideItems`** (`src/Olifant/Generation/Planner.hs:36`). A
`foldM` over the items threads `(current, edits, conflicts, n)` through, dispatching each
`PlanEdit` to one of two sibling functions, `decideCreate` or `decideInsert` (`:62`, `:72`) —
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
  -- src/Olifant/Generation/Planner.hs:72
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

- `RenderedComplete` items are skipped here (`:46`) and left for phase 2.
- Before either function, an edit with an `unless` is checked (`:48`; the guard is read in
  [chapter 2](02-haskell-for-this-codebase.md#guards-where-let)). `matchUnless` (`:56`) is a
  `case` on `Unless` (`Generator/Definition.hs:83`): `UnlessExists` globs the project's paths
  plus the files `current` has created, and becomes `Skipped (UnlessMatched path)`;
  `UnlessMatches` compiles the rendered regex with `regex-tdfa` and runs it over each line
  of the edit's own target *as `current` has it*, so text an earlier edit or step inserted
  counts, and becomes `Skipped (UnlessFound path line)` (`Plan.hs:72`), which `skipReason`
  (`Cli/Output.hs:255`) prints as `unless: line N of PATH matches`. A target that does not
  exist yet, or a regex that no longer compiles once rendered, is no match. The regex was
  already checked at load time, with each `{= … =}` standing in for a plain name
  (`unlessFrom`, `Generator/Load.hs:295`).

**Anchors, `resolveLayers`** (`src/Olifant/Anchor/Resolve.hs:61`), tries the edit's anchors in
order and returns the first that resolves, or every miss. `resolveOne` (`:70`) parses the
phrase with `parsePhrase` (`src/Olifant/Anchor/Phrase.hs:39`) and dispatches on the target:

- `after last line matching ^import Page\.` is a **line target**. It's resolved in Haskell
  with `regex-tdfa` (`lineAt`, `Resolve.hs:129`), with no ast-grep call.
- `end of type Page` is a **named target**, `["type", "Page"]`. `named` (`Resolve.hs:142`)
  hands it to `itemsOfTarget` (`:191`), a top-level function shared with two other callers:
  a fragment exemplar's `items_of` (`Catalog.loadGenerator`, above) and an `ast` query's
  `items_of` (`Anchor.Items.astQueries`). `itemsOfTarget` matches the target against the Elm
  adapter's targets (`packs/elm/languages/elm.toml`); the shape `type NAME` fits and binds
  `NAME = Page`. It renders that target's `container` rule (`{ kind = "type_declaration",
  regex = "^type Page\b" }`) and asks the **Syntax port** for matches (`querySyntax`,
  `:201`). Exactly one match is required. Then it asks for the `items` (`union_variant`)
  *inside* that container (`:209`), and keeps those whose range lies within the
  container's (`inside`, `:218`). An item may span the container exactly: a Python
  `block` whose one statement is a `match` has the same range as the `match`, and the test
  used to drop it, so `end of function create_booking_status` missed. The items query's
  own `inside` already asks for a strict ancestor, so the range test no longer excludes
  the container's own range. Back in `resolveOne`'s `where`, `place` (`:147`) picks
  the line after the last item, indented like it — and sets `locSpacing`: if the container's
  existing items are separated by a blank line, `place` copies that spacing onto the new one
  (`BlankAfter` for `start of`, `BlankBefore` for `end of`; spec §5.2). `named`, which
  calls `place`, drops that spacing for a target with `targetSpaced = False` (key `spaced`;
  Python's `imports`, where a blank line separates groups), and for a target with no items
  and `targetWhenEmpty = Just "after header"` (Python's `imports` again) it places the text
  at `headerEnd` (after the leading comments and docstring) with `BlankBefore`, instead of
  missing.
- **A one-line container with items.** When the language has a `separator`, `place`
  returns a `Grow` opening (the line, the bracket columns, the items' texts, the separator,
  `line_length`, the indent, start or end) instead of refusing. `grownLine` builds the
  replacement: inline if it fits, else one item per line; `insertLines` applies it, and the
  planner shows it as `WillReplace line 1 text`.
- **An empty container.** `place` has a third case, for `end of`/`start of` against a target
  whose `items` come back `[]` (spec §7.1). It opens the container only when it is bracketed
  (`()`, `{}`, `[]`; anything else, a whole file say, misses with "has no items to insert
  beside") and blank or comment-only: every line between the brackets, stripped, is empty or
  starts with the adapter's `comment` (`:168`). Anything else inside — `Svc::new(true)` when
  `boolean_literal` is not one of the target's `items`, `{ ..Default::default() }` — would be
  deleted or mis-joined, so it misses with "has content that is not one of the target's items
  (add its kind to the target's `items`)" (`:169`). Opening returns a `Located` with
  `locOpen = Just o`, where `data Opening = OpenLine Int Int Int | OpenLines` (`:49`);
  `locOpen` is `Nothing` for every other case, so `insertLines` (`:232`) knows only these need
  special handling. On one line (the container's start and end share a line —
  `Service::new()`, `RefundInvoice {}`), `OpenLine line from to` carries the bracket columns,
  and `insertLines` splits that line in three: up to the opening bracket, the new text at one
  level past the container's own indentation (`langIndent`, the adapter's `indent` key,
  `Language/Adapter.hs:40`), then the closing bracket onwards, at the line's own indentation
  — so `-> Self {` after a Rust parameter list, or a trailing `;` after a call's arguments,
  survives on the closing line. Spread over several lines already (`OpenLines`), `place`
  looks for a non-blank (comment) line inside and uses its indentation, so a template's
  commented-out hint in a struct literal is where the real field lands; with nothing inside
  at all, it falls back to the same one-level-in indentation as the one-line case. Either way
  the text becomes the container's *first* item, so `placedText` (`Resolve.hs:250`) passes it through
  `firstItem` (`:253`), which drops one leading `,`: Elm's `oneOf []` given `, Parser.map …`
  becomes `oneOf [` / `Parser.map …` / `]`.

Each `querySyntax` call against the real adapter is one `ast-grep` process fed the file on
stdin (`src/Olifant/Language/AstGrep.hs:34`). The Elm pack vendors its tree-sitter grammar,
so the first ever call compiles `packs/elm/languages/grammar/elm/parser.c` with `cc` into
`~/.cache/olifant/grammars/`. The key is the language name plus a hash of `parser.c`'s
path, size and modification time (`prepare`): a `stat` per query, since hashing a
megabyte-sized grammar on every call cost a third of an AST query's time.

`presentAt` (`Resolve.hs:284`) makes re-runs idempotent: if the snippet's tokens already sit
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

**Phase 2: `complete` edits, the check loop** (`src/Olifant/Generation/Complete.hs:38`).
`perLanguage` (`:41`) first turns the adapter's `[check] format` into a `CompilerSupport`
record with `supportFor` (`Language/Support.hs:14`) — `elmSupport`, `ghcSupport`,
`pyrightSupport` or `rustcSupport`. A format
`supportFor` doesn't recognise fails every `complete` edit for that language with one message
(`Complete.hs:46`), rather than surfacing the problem deep inside the loop. For each language that has
`complete` edits, `gather` (`:55`) collects every file the check needs, meaning the adapter's
`files` globs plus its `context` (`elm.json`; a Haskell adapter's `*.cabal` and
`cabal.project`; Rust's `**/Cargo.toml` and `Cargo.lock`; Python's `pyproject.toml` and
`uv.lock`), *as they will be after phase 1*, leaving out anything under a `Workspace.pruned`
directory. For Python that matters: `.venv` holds thousands of `.py` files, so `pruned` has
`.venv`, `__pycache__` and `.pytest_cache` beside `.git`, `node_modules`, `elm-stuff`,
`dist-newstyle` and `target` (`Workspace.hs:183`), which the disk workspace never walks
either. Then `loop` (`:63`) runs, carrying that
`support` record alongside the language:

```haskell
-- src/Olifant/Generation/Complete.hs:63
    loop lang support step mine disk rnd seen n files current acc
      | rnd > maxRounds = pure (reverse acc, [CompleteFailed step ("the compiler still reports missing cases after " <> T.pack (show maxRounds) <> " rounds") False], [], current)
      | otherwise =
          runCheck (toolChecker tools) lang (placeholderize (toolLanguages tools) files) >>= \case
```

`runCheck` goes through the **Checker port**. `processChecker` (`src/Olifant/Language/Check.hs:49`)
writes the files into a temp directory and runs the adapter's `[check] command` there
(`runIn`, `:93`, which expands `{= files =}` to the sources and `{= cache =}` to a
per-language XDG cache directory, `:108`). Before it runs anything, `runIn` asks
`checkProgram` (`:65`) which program the command runs: the first word, or, past a leading
`env` and its `NAME=value` settings, the word after them (`env UV_PROJECT_ENVIRONMENT=… uv
run pyright` runs `uv`). A missing program is `CheckToolMissing` naming it, so the message
says `` `uv` is not installed ``, never `` `env` ``. It looks up the same `CompilerSupport` by
format (`supportFor`, `Check.hs:55`) and hands the compiler's raw output, stdout if the
record's `supportReadsStdout` says so (cargo, pyright) and stderr otherwise (`:57`), to
`supportParse` — `parseElmReport` (`Language/Elm.hs:20`) for `elm-json`, `parseGhcReport`
(`Language/Ghc.hs:36`) for `ghc-json`, `parsePyrightReport` (`Language/Pyright.hs:43`) for
`pyright-json`, `parseRustcReport` (`Language/Rustc.hs:50`) for `rustc-json` — into a
`CheckReport`:
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
`supportReadsStdout = True`. `typeNames` lives in `Language/Diagnostics.hs` (`:77`) now that
GHC and rustc both use it.

`Language/Pyright.hs` reads `pyright --outputjson`: one JSON object whose
`generalDiagnostics` each have a `file`, a `severity`, an optional `rule`, a `message` and a
`range` with 0-based lines. A `reportMatchNotExhaustive` diagnostic is a site at the `match`
line; its patterns come from the message's `Unhandled type: "…"` (`unhandled`, `:60`), split
on ` | `, each class as a class pattern (`pythonPattern`, `:71`: `SlotTaken` → `SlotTaken()`,
type arguments dropped) and each `Literal[E.X, E.Y]` as its members. pyright puts a
non-breaking space in some messages, which `unhandled` turns back into a space first. A
`reportReturnType` "must return value on all code paths" in a file that has such a site is
the missing case's consequence (a function whose last statement is the `match`), so it is
not a broken file; every other `error` is (`:47-51`). `pyrightSupport` (`:19`) writes a
pattern as the site writes its others (`qualifyLike`, `:76`: the qualifier of the first
`case q.Class` arm, so `Cancelled()` becomes `create_booking.Cancelled()` or
`cb.Cancelled()`), has no visibility rule (`supportVisible` is always `True`: sites come
from pyright's own report), names the enclosing function with `pythonFunctionAt` (`:89`, the
nearest `def` or `async def` above), and sets `supportArmSeparator = ""`.

pyright gives only the `match` line, and the loop needs the site's last line to place an
arm after it. That is the one thing a report can't say, so `CompilerSupport` has an
optional, source-aware step, `supportSpans :: Maybe (Map FilePath Text -> CheckReport ->
CheckReport)` (`Language/Diagnostics.hs:49`). `processChecker` applies it last, to the
report with its paths already made project-relative (`relativeTo`), with the files it
checked (`Check.hs:61`). Pyright's is `extendSites` (`Pyright.hs:34`), which ends each site
at `indentBlockEnd` (`Diagnostics.hs:57`): the last non-blank line, after the `match`, of the
lines that are blank or indented deeper than it. It is pure and generic, the indentation
rule for any block; a trailing blank line is not part of the block. Elm, GHC and rustc set
`supportSpans = Nothing`. The recorded reports `LanguageSpec` parses are in
`src/Olifant/Language/testdata/pyright/`, made with `scripts/record-pyright.sh`, and one
spec runs real pyright through `processChecker` (with `uvx`, so it is pending without `uv`,
even under `OLIFANT_REQUIRE_TOOLS=1`).

The loop narrows a reported pattern's candidates with `supportVisible` (`Complete.hs:72`): for Elm, by
module import (`visibleAt`); for GHC, by the edit's `Type.Ctor` qualifier appearing as a
word in the reported type (`Maybe T`, `(T, Bool)`). A pattern that names one of the run's
constructors but has no visible candidate becomes a warning (`addHidden`), never a silent
skip. For each missing pattern, `branchFor` (`Complete.hs:120`) picks the `complete`
spec whose constructor it names, from the candidates sharing the pattern's bare name — a
`NonEmpty` list, since the call site only builds `cands` when it's non-empty (`nonEmpty`,
`:73`), so `branchFor` itself needs no "this is never empty" comment or partial `head`. Specs
with a `where` must also match: `locateContainer` checks that the reported `case` is the one
named. A `complete` edit with no body is filtered out before `whereMatches` even runs (`:125`,
`isJust (completeBody s)`), so it never supplies text for a chosen branch — only ever a todo.
`branchFor` then renders the chosen spec's body with `pattern`, written the way the site
should by `supportPattern` (Elm qualifies it; GHC's names are already in scope at the site;
rustc's lose a leading `&`), and `site`. `applyBranches` (`:159`) inserts the branches bottom-up within each file, so
earlier line numbers stay valid, and leaves the inserted edit's `editAnchor` as `Nothing` — a
compiler-found branch has no anchor to report, so there is nothing for a re-run to leak into
JSON (below).

When a round has nothing left to add, `addUnplaced` (`:100`) turns any `reportUnplaced` site
that needs one of this plan's constructors into a plan warning — never a todo, since there is
nowhere to place a branch — naming the file, the function and the pattern. Otherwise the loop
checks again.

`insertBranch` (`Complete.hs:174`) places the new line after the `case`'s last line, indented
like its other alternatives, and copies their *spacing*: a blank line before the new branch
only if one already separates the case's last two alternatives (Elm's usual style), none if
they sit flush against each other (ordinary Haskell). With only one alternative there is
nothing to copy: a blank line when the language has no arm separator (Elm), none when it has
one (Rust's arms sit together). Its first argument is the support's `supportArmSeparator`,
which `applyBranches` passes down (`:87`): before inserting, `separated` (`:193`) gives the
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
(`:32`), or when a round reports a site it already filled (`:78`). If the compiler reports
one of the plan's constructors missing from a `case` that no matching spec's `where`
names, that site gets the adapter's placeholder branch (`Debug.todo`, or GHC's `error "…"`)
and is listed as a todo; a `complete` edit with no body at all (`olifant-dev/error-case`)
gets this for every site, since it has no spec a `where` could ever match. Missing patterns
that don't mention any of the plan's constructors are ignored, and GHC's function-equation
sites that do are the plan warnings from `addUnplaced` above, never todos.

**Phase 3: regions, `refreshRegions`** (`src/Olifant/Generation/Regions.hs:32`). This phase
re-renders every owned region against the files as the plan leaves them. The Elm pack
declares no regions, so `toolRegions` is empty and the phase returns immediately. In
this repository the phase is where `olifant.cabal`'s module lists are refreshed; in a
rust-htmx project, every service's `lib.rs` and `ports.rs`.

Those are **families** (spec §5.9): a `file` that is a pattern. Before the fold, each
declaration goes through `regionInstances universe` (`:84`), where `universe` is the files
on disk plus the files the plan creates. A `file` with a `*`, `?` or `[` becomes one
`RegionDef` per matching path (`matchesGlob`), with `{= dir =}` and `{= file =}` replaced in
a glob query's pattern and excludes, or an AST query's `files`, by that path's directory and
the path itself (a plain text replace, before the query is rendered); any other `file` is
the declaration as it is. The instances are sorted by `(regionName, regionFile)` (`:39`), so
the edits come out in the same order on every run. From there each instance is an ordinary
region: its own fences, its own drift, and a file without fences is skipped (or, under
`ReportUnset`, reported) on its own.

#### 4d. The re-run second pass (`Generation.hs:115`)

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

#### 4e. Holes: markers, then placeholders (`Generation.hs:129`)

A **typed hole**, `{= ? name: kind | instruction =}` (user book, chapter 12), is the one
part of a plan Olifant doesn't write itself. It travels through all the phases above as a
**marker**, and only at the end becomes the adapter's placeholder.

**Lexing and parsing** (`Template/Parse.hs`). The lexer sees `{= ?` and closes the tag with
`closeNested` (`:130`) instead of `closeWith`: it counts `{=` and `=}`, so an instruction
can hold outputs (`… for {= pattern =} =}`). The tag becomes a `THole` piece (`:112`), and
`block` (`:153`) splits it into `name`, `kind` and the instruction, which it parses as a
template of its own and accepts only if it has nothing but literals and outputs. The result
is a `Hole name kind body` segment (`Template/Syntax.hs`). `freeVariables` counts the
instruction's variables as the template's, so an undefined one is the usual error.

**Rendering** (`Template/Render.hs:33`) turns a `Hole` into one line of text:

```
⟦olifant-hole status|expr|the value this match returns for RefundInvoiceFailure::AlreadyRefunded (…), following the other arms⟧
```

The instruction is rendered with the environment (so `{= pattern =}` is the real pattern in a
`complete` body), its whitespace collapsed to single spaces, and any `⟧` in it replaced
with `]`. The marker is always one line because holes are found line by line.

**Why markers and not placeholders.** Three things about a hole are not known until the plan
is done, and markers keep each of them open:

- *The final name.* A hole whose name appears more than once in the plan is named
  `name[file:line]`, and both the count and the line are only known when every edit, every
  `complete` round and every region refresh has run.
- *The placeholder's text.* That comes from the adapter of the file the hole lands in, and
  it includes the final name.
- *Where the holes are.* Every comparison of planned text with a file needs to know which
  spans are holes, because a hole must match whatever is there (below). The marker says so
  in the text itself; a placeholder would look like any other code.

So a marker is self-describing: `splitMarkers` (`Holes.hs:43`) recovers `Marker name kind
instruction` from any text, and `hasMarkers` (`:52`) is a substring test for the opening
`⟦olifant-hole ` (`open`/`close`, `:38`–`:40`). The brackets are U+27E6/U+27E7, which never
appear in generated code.

**Checks see placeholders.** A compiler would choke on a marker, so the `complete` loop's
check runs on `placeholderize (toolLanguages tools) files` (`Generation/Complete.hs:66`).
`placeholderize` (`Holes.hs:66`) replaces each marker with its kind's placeholder, by its
bare name (`placeholderFor`, `:56`, renders the adapter's `[holes.KIND] placeholder` with
`hole` and `instruction`). Only the check sees that text; the plan's working text keeps its
markers. A marker in a file with no adapter, or of a kind the adapter lacks, is left alone
here; the step below reports it.

**Matching with holes.** Three places compare planned text with a file, and each treats a
hole as "anything":

- `decideCreate` (`Generation/Planner.hs:67`) skips a create as `UnchangedContent` when
  `matchesWithHoles content old` (`Holes.hs:109`). It is anchored and linear: the text
  before the first hole must start the file, the text after the last must end it, and the
  pieces between must appear in that order. Adjacent holes act as one.
- `alreadyPresent` (`Anchor/Resolve.hs:303`), behind `presentAt`, gets a branch for snippets
  with markers (`:306`): the snippet's literal pieces, whitespace-normalised, must appear in
  order in the region.
- The re-run second pass (4d) and the "what changed" filter below use `sameWithHoles`
  (`Generation.hs:68`), which calls `matchesWithHoles` when both sides have text and
  compares the `Maybe (Maybe Text)` values otherwise.

That is what makes a re-run over a filled hole (or its placeholder, or your edit of either) a
no-op that calls no model.

**Placing the holes** (`Generation.hs:138`):

```haskell
-- src/Olifant/Generation.hs:138
      let replaced = [editPath e | e <- edits, WillReplace {} <- [editAction e]]
          changed = M.fromList [(p, c) | (p, Just c) <- M.toList current, if p `elem` replaced then Just (Just c) /= M.lookup p originals else not (sameWithHoles (Just (Just c)) (M.lookup p originals))]
          (placed, holeConflicts, holes0) = case placeHoles (toolLanguages tools) changed of
            Left problems -> (changed, map HoleProblem problems, [])
            -- a tab stop (yours, derived) is the person's, never the model's (spec §4.9)
            Right (files, hs) -> (files, [], [PlannedHole h (case snd (holeSource (holeKind h)) of "model" -> HoleUnfilled "no model is configured"; src -> HoleSkipped ("a tab stop (" <> src <> ")")) Nothing | h <- hs])
          shown = map (showHoles (toolLanguages tools)) edits
```

`changed` is every file whose final text differs from the original, holes matching anything
(a file a replacement rewrote, a fix's or a snippet's, is compared exactly instead).
`placeHoles` (`Holes.hs:76`) walks those files line by line, collects every marker with its
file and 1-based line, counts names, and gives each hole its final name: the bare name, or
`name[file:line]` when it repeats. Then it renders each placeholder with that name and
writes it in place of the marker, giving a `HoleSite` (`:28`) per hole: name, kind,
instruction, file, line and placeholder. A file with markers but no adapter, or a kind the
adapter doesn't declare, comes back as `Left` problems, and each becomes a `HoleProblem`
conflict (`Plan.hs:88`; code `hole_problem`). Every model hole starts as `PlannedHole site
(HoleUnfilled "no model is configured") Nothing` (`Plan.hs:62`); a snippet's tab stop
starts, and stays, `HoleSkipped "a tab stop (yours)"` (or `derived`).

**A hole's source rides in its kind.** `[holes.NAME] source = "yours"` or `"derived"`
(user book, chapter 12) has to reach `placeHoles`, the fill phase and the plan's JSON, and
the marker is what all of them see. So when `Catalog.loadGenerator` resolves a generator, it
runs `Snippet.withHoleSources` (`Snippet.hs:31`, called as `sourced`, `Catalog.hs:241`) over
every template: a `Hole n kind instr` named in `[holes]` becomes `Hole n "text@yours" instr`,
or, for a derived one, `Hole n "expr@derived" value` (the instruction replaced by the
`value` template, rendered like any instruction). A `[holes.NAME]` that names no hole is a
`SourceInvalid` load error there. `Snippet.holeSource` (`:53`) splits a kind back into
`(kind, source)`, `model` when there is no `@`. Downstream:

- `placeholderFor` (`Holes.hs:56`) looks up the bare kind, so the adapter's kinds are checked
  as usual, but writes a yours hole as its name and a derived one as its instruction, the
  rendered value, instead of the kind's placeholder.
- The excerpt above makes a non-model hole `HoleSkipped`, and the fill phase is given only
  the `HoleUnfilled` ones (`forModel`, `Generation.hs:153`); the filled ones are merged back
  by name, so `planHoles` keeps every hole in order.
- `holeJson` (`Cli/Output.hs:217`) splits the kind into `kind` and `source`. The human form
  prints the raw kind, `text@yours`.

The adapters' placeholders must use `{= hole =}` (`holeKindFrom`, `Language/Adapter.hs:83`,
refuses one that doesn't, `:86`), so every placeholder in a plan is unique text. Fills are
placed by finding that text, wherever earlier fills have moved it.

`showHoles` (`Generation.hs:73`) does the same for each edit's own text, so `--plan` and
the JSON `content` never show a marker.

#### 4f. The fill phase (`Generation/FillPhase.hs`)

```haskell
-- src/Olifant/Generation.hs:141
      (writes, holes, fillWarnings) <-
        if null (conflicts ++ holeConflicts)
          then fillPhase ws tools (concatMap (M.toList . stepSets) steps) placed holes0
          else pure (placed, holes0, [])
```

The fill phase runs only for a plan with no conflicts, since nothing would be written
anyway. `fillPhase` (`FillPhase.hs:26`) returns the files, the holes with their status, and
any warnings. With no `toolFiller`, or no holes, it returns its inputs unchanged.

The **port** is `Filler` (`Fill.hs:30`): a function from a `FillRequest` (`:26`: the holes
and the generator's inputs) to fills by hole name, or why there are none. Each `FillHole`
(`:16`) carries the name, kind, instruction, file, the **context** and, on a retry, the
previous fill and why it failed. `ModelFiller` (`:33`) wraps a `Filler` with the model's
name (reported in the plan) and the retry count.

**The loop** (`loop`, `FillPhase.hs:33`) threads the accepted fills, the notes on checks that
couldn't run, and the holes still pending, as `Try` values (`:24`: the hole, its attempts so
far, and its previous fill and reason). Each round:

1. **One request** for every pending hole. `context` (`:87`) is up to 100 lines either side
   of the hole in the placed text, with the hole's placeholder on its line replaced by
   `⟦name⟧`.
2. A `Left` reply (curl failed, the reply wasn't readable) is a failed attempt for every
   hole in it (`:41`), retried like any other.
3. A hole the reply has no fill for fails ("the model returned no fill for …").
4. **`escapes`** (`:134`) refuses, before any parse, a fill containing `olifant:` (the model
   copied the placeholder) or one whose brackets don't balance outside string literals (it
   would close the code around it).
5. **`parses`** (`:93`) puts the fill in the kind's `wrap` and asks the Syntax port for
   `ERROR` nodes. None means it parses. If ast-grep can't run, the fill passes and the check
   decides.
6. **`checkFiles`** (`:104`) applies the accepted fills plus every candidate and runs each
   affected language's `[check]` over the project as the plan has it: `checkLang` (`:108`)
   reads the files the adapter's globs and `context` want from disk and lays the plan's
   files over them. A check that can't run returns a note instead of a verdict.
7. If anything broke, each candidate is checked again **alone** beside the accepted fills
   (`verdicts`, `:55`–`:60`), so the fill that breaks a file is the one blamed, even when
   the broken file is another hole's. The fills that pass alone are then checked
   **together** (`:64`–`:65`). If they clash, none of them is accepted this round.
8. Accepted fills go into `accepted` with their attempt count (`:67`); failures get their
   `(fill, reason)` as the next `Try`'s previous (`:68`).

**Progress** goes out through `toolProgress` at four points: `Filling` before each request
(`:34`, with the pending holes' names, the round and `mfName`), `Filled` for each fill
accepted in a round (`:72`), `Unfilled` for each hole still failing when the rounds run out
(`:39`), and `Checking` before each `[check]` in `checkLang` (`:118`). The complete loop
(`Generation/Complete.hs:66`) sends `Checking` before each of its rounds too.

`next` (`:37`–`:41`) stops when nothing failed or after `1 + retries` rounds, and `finish`
(`:71`) writes every accepted fill into the files with `applyFills` (`:121`). It marks
those holes `HoleFilled model attempts` with their fill, marks the others `HoleUnfilled`
with their last reason, and turns the notes into the warning "holes were filled but not
compile-checked: …" (`:76`). `applyFills` finds each placeholder by its text and indents a
multi-line fill's continuation lines like the placeholder's line. When the file's language
has `imports`, `splitImports` takes the fill's leading import lines off first and
`addImports` puts each one the file lacks after its last top-level import (or after the
leading comments and docstring, `headerEnd`). `parses` checks only the code part, and
`FillHole`'s `fhImports` tells the model it may send imports (`Model.hs` adds the line to
that hole's prompt). After a round's check, `autoImport` looks at files whose every error
is an undefined name (the support's `supportUndefined`); `importFor` scans the project's
files of that language for the one import line binding the name (through `merging` and
`import M( as N)?`), and each candidate using the name is prefixed with it and judged again
in the same round (`AutoImported` progress). A hole with `verify` (`genVerify`, rendered
per step into `PlanVerify` items, handed to `fillPhase` as a map by bare hole name) is run
after the compile check: `projectFiles` gathers what a check sees, and the **Verifier** port
(`Language.Check`: `runVerify`; `processVerifierWith secs` runs the command in a scratch copy
through `Process.runBytesWithin`, which polls for exit so it can stop a long run under the
non-threaded runtime) decides; a failure is a retry reason "the tests failed: …". Before
any of this, layout is repaired: `applyFills` places `dedented` lines (common indent
dropped), and a candidate failing to compile whose first line sits less deep than the rest
(and opens no block) is tried `reindented`, kept when, with only it placed, its file then
compiles, or, the original carrying a syntax-class finding (`syntaxError`), has fewer
findings or loses that one; it is announced as `Reindented` (a block's draft
writes that text). `Snippet.snippetOf` places a
model fill the same way (shared `Holes.dedented`, continuation lines at the marker line's
indent), so the editor's snippet matches the written file. `gen --fills FILE` makes the
filler `replayFiller` over `parseFills` (both in `Olifant.Fill`, shared with the case
harness).

Then `generate` makes the shown edits match the writes:

```haskell
-- src/Olifant/Generation.hs
      let filled = [(phSite h, f) | h <- holes, Just f <- [phFill h]]
          importsOf p = langImports =<< languageFor (toolLanguages tools) p
          withFills imports p t = M.findWithDefault t p (applyFills imports (M.singleton p t) [(s {holeFile = p}, f) | (s, f) <- filled])
```

`shownFilled` applies `withFills` to every edit's text, so `--plan` shows the fill where
the placeholder was. `withFills` is `applyFills` on that one text, so a created or
overwritten file shows the fill's imports where they went and its continuation lines
indented as written; an inserted fragment (`fragment`) is shown with each fill's code
only, its import lines split off with `splitImports` (they appear in the file's imports in
`planWrites`, not in the fragment).

**The real filler** is in `Model.hs`. `parseModelConfig` (`:59`) reads `[model]`: `default`,
`retries` (2), `timeout` (120 seconds) and `[model.backends.NAME]`, each a `Backend`
(`:43`), `OpenAICompatible url model` or `Anthropic model keyEnv`. `systemPrompt` (`:77`)
and `userPrompt` (`:89`) are the text: they ask for one `<fill name="NAME">CODE</fill>` block
per hole, with no escaping. `requestBody` (`:108`) is the JSON for each backend (OpenAI-style
with `temperature: 0` and no `response_format`; Anthropic's with `max_tokens: 4096`). JSON
mode was dropped after token experiment v12, where it made a small thinking model fail
18 of 33 calls and tagged blocks failed none (user book, chapter 12). `curlFiller` (`:188`)
is the adapter: there is no HTTP library, so `post` (`:202`) runs `curl -sS -X POST URL … --max-time T --data-binary @-` through
`Process.runBytes` with the body on stdin. For Anthropic it first reads the key from the
environment variable the config names ("ANTHROPIC_API_KEY is not set" if it's missing),
writes `x-api-key` and `anthropic-version` to a temporary header file, and passes
`-H @file`, so the key is never on a command line. `decodeFills` (`:125`) reads either
backend's reply shape and drops a `<think>…</think>` prefix (`dropThinking`, `:163`). Then
`tagged` (`:151`) splits the text on `<fill name="`, takes each name up to `">` and each body
up to `</fill>`, and trims one newline from each end of the body; blocks may sit anywhere in
prose. Only when that finds no block does it fall back to JSON: the text as it is, then the
JSON inside a ``` fence or between the outer braces, reading `{"fills": {…}}` or a bare
object and taking strings (unfenced), numbers and booleans as code.

**In tests**, `GeneratorTesting.runCase` reads a case's `fills.toml` (`GeneratorTesting.hs:66`)
and sets `toolFiller` to `ModelFiller "recorded" 0 (replayFiller fills)` (`:71`).
`replayFiller` (`:143`) answers every request from the map and fails if a requested name is
missing. A case whose plan still has an unfilled hole fails (`:82`). "placeholder output
compiles" (`GeneratorTestingSpec.hs:191`) plans the same cases with no filler and compiles
the placeholders.

#### 4g. The parse pre-check (`Generation.hs:146`)

Before the plan is built, `generate` asks whether the files it is about to change parsed in
the first place (spec §4.6). A `;` typed where a Rust struct field wants `,` makes tree-sitter
recover in ways that can put an anchor somewhere odd, and it makes every compiler check in
the plan fail in a way that looks like the plan's fault. The user is often mid-edit, so this
is a warning, never a conflict:

```haskell
-- src/Olifant/Generation.hs:145
      before <- traverse (\p -> (p,) <$> previous p) (M.keys writes)
      -- Parse pre-check (spec §4.6): a touched file that does not parse is
      -- warned about first; placement and checks around it may go wrong.
      unparsed <- fmap concat . forM [(p, old) | (p, Just old) <- before] $ \(p, old) ->
        case languageFor (toolLanguages tools) p of
          Nothing -> pure []
          Just lang ->
            querySyntax (toolSyntax tools) lang (object ["kind" .= ("ERROR" :: Text)]) old <&> \case
              Right ms
                | m : _ <- sortOn matchStart ms ->
                let near = T.take 20 (T.takeWhile (/= '\n') (matchText m))
                 in [(p, T.pack p <> ":" <> T.pack (show (fst (matchStart m) + 1)) <> " does not parse (near `" <> near <> "`) — this plan may place code wrongly, and checks will fail")]
              _ -> []
```

- **Which files.** `before` is every file in `writes` with its text before the plan (`Just`
  for a file that existed; `Nothing`, and so skipped, for one the plan creates). `writes` is
  every file whose text the plan changes, including files only the compiler found for a
  `complete` edit, which `previous` (`:140`) reads from the workspace. So the check sees
  exactly the existing files the plan touches, and none it only reads (GenerationSpec's
  "parse pre-check" group, `GenerationSpec.hs:976`, has a case for each).
- **The check** is the same one the fill phase uses on a fill (`parses`, 4f): an ast-grep rule
  `{ kind = "ERROR" }` through the Syntax port, on the old text. The first `ERROR` node by
  position is named, by 1-based line and up to 20 characters of its first line. A file with no
  adapter, or a Syntax port that fails (ast-grep missing), gives no warning: the pre-check
  never stops a plan.
- **Pointing back.** A broken file the plan touches also breaks every compiler check in the
  plan. When the check loop finds a broken file that the plan changed, it gives up with
  `CompleteFailed step "the generated code does not compile: FILE (…)"`
  (`Generation/Complete.hs:117`), which blames the plan for the user's `;`.
  `pointBackConflict` (`:163`) appends " (see the parse warning above)" to that reason when it
  names one of the unparsed files. `pointBack` (`:158`), which it uses, does the same to any
  warning containing "does not compile" that names one, such as the loop's "could not check
  for missing cases because the project does not compile: …" (`Complete.hs:118`, for broken
  files the plan had not changed at that point, so it matches only when a later phase changes
  that file). Both are
  substring tests on rendered text, which work because every message names the file by the
  same project-relative path.
- **Order.** The parse warnings go first in `planWarnings`, before the phases' and the fill
  phase's. Editors rely on that only loosely: Durandal's preview finds them by their shape
  (`FILE:LINE does not parse`) and puts them first itself.

A missing-tool `CompleteFailed` gets a different suffix, at render time: `describeConflict`
(`Generation/Plan.hs:101`) appends `` ; run `olifant doctor` `` when its `Bool` (the conflict is
only a missing tool) is set, so the hint is in the CLI's human and JSON output and in pack
test failures alike.

#### 4h. Writes (`Generation.hs:200`)

```haskell
-- src/Olifant/Generation.hs:200
          plan = Plan (GeneratorId (T.intercalate " + " ids)) (zip ids (map stepSets steps)) now shownFilled (map pointBackConflict (conflicts ++ holeConflicts)) (map snd unparsed ++ map pointBack (warnings ++ fillWarnings) ++ unsetWarnings) writes holes (M.fromList before) (M.filter hasMarkers changed) Nothing
      when (mode == Apply && not (hasConflicts plan)) $
        forM_ (M.toList writes) (uncurry (writeText ws))
      pure (Right plan)
```

`planWrites` holds only the files whose final text differs from the original, with their
holes filled or left as placeholders. They're written only in `Apply` mode, and only when
there are no conflicts at all. That is the whole of "all or nothing": everything before
this line worked on in-memory text. `--plan` runs the fill phase too (it calls the model),
and simply doesn't write. The `Plan` type is in `src/Olifant/Generation/Plan.hs:24`. Its
`planEdits` are `PlannedEdit`s (`:41`), each with an `EditAction` (`:67`): `WillCreate`,
`WillOverwrite`, `WillInsert line text`, `WillRefresh`, `Skipped reason` or `Conflicted`.
`planHoles` (`:34`) lists every hole with its `HoleStatus` (`:61`):
`HoleFilled model attempts`, `HoleUnfilled why` or `HoleSkipped why`. The last field,
`planBefore` (`:36`), is each written file's text before the plan, `Nothing` for a new file,
for editors (the JSON `files`, step 5). It is `before` from 4g: `originals` (4b) where it
can; a file that only the compiler found, named by no edit and no `[requires]`, was never
inspected, so `generate` reads it from the workspace (`previous`). The last two fields are
for snippets: `planMarked` (`:39`) is every changed file that still has hole markers, as it
was before `placeHoles`, so the CLI can tell which text was a hole, and `planSnippet` (`:41`)
is always `Nothing` here; `gen --at` sets it in step 5
([below](#snippets-gen---at-olifant-complete-and-uncovered)).

The writes happen here, before anything is printed. If rendering the output crashed, the
files would already be written.

### 5. Output and exit code (`src/Olifant/Cli.hs:240`)

Just before this, `runCommand` built the `Tools` for `generate` (`Cli.hs:238`) with
`toolFiller` from `modelFor` and, with `--progress`, `toolProgress` writing each event to
`rtErr` as one line of JSON (`progressJson`, `:408`: `{"event":"check","language"}`,
`{"event":"fill","holes","attempt","model"}`, `{"event":"filled","hole","attempt"}`,
`{"event":"unfilled","hole","why"}`). Without the flag it is `const (pure ())`. Because the
events go through the `Tools` port, `GenerationSpec` can collect them in a list.

```haskell
-- src/Olifant/Cli.hs:467
              Right plan0 -> do
                let plan =
                      (if genNoFill g then noFill plan0 else plan0)
                        { planSnippet = snippetAt plan0
                        , planWarnings =
                            planWarnings plan0
                              ++ ["regions are not refreshed from " <> T.pack path <> ": " <> err | (path, err) <- catalogBrokenRegions cat]
                              ++ ["language adapter " <> T.pack path <> " failed to load: " <> err | (path, err) <- catalogBrokenLanguages cat]
                        }
                    applied = mode == Apply && not (hasConflicts plan)
                emit rt fmt (planJson applied plan) (planHuman applied plan)
                pure (if hasConflicts plan then ExitFailure 5 else ExitSuccess)
```

`emit` (`:265`) prints one of two renderings. Both are built, but laziness means only the
one printed is computed.

- **`planHuman`** (`src/Olifant/Cli/Output.hs:209`) prints the header, one line per edit
  (`editLine`, `:235`: `+`, `~`, `=`, `!`), then the conflicts, the todos, the warnings and
  the holes (`holeLines`, `:212`; `holeText`, `:215`: name, kind, file and line, then
  `filled by MODEL (attempt N): FILL`, `unfilled: WHY` or `skipped: WHY`; a multi-line fill
  is joined with ` ⏎ `).
  In `--plan` mode it also prints the full text of each edit — `details` (`:246`) — unless
  the plan `hasConflicts`, in which case nothing would be written anyway, so neither `--plan`
  nor a plain `gen` prints any edit's text. The `[step]` tag appears only when edits came
  from more than one step path (`multiStep`, `:232`).
- **`planJson`** (`:168`) produces the `plan_version: 2` object, with a `holes` array from
  `holeJson` (`:206`): `name`, `kind`, `source`, `file`, `line`, `placeholder`
  (`holePlaceholder` of the `HoleSite`, the text an editor replaces with `⟨name⟩` in ghost
  text), `status`, `model`, `attempts`, `why` and `fill`, and `snippet` (`:183`), the
  `planSnippet` as `{file, line, ghost, body}` or `null`. Applied runs leave out file
  contents (`content`, `:190`) — this one is keyed only on `applied`, not on conflicts,
  because a conflicted edit never has a `WillCreate`/`WillInsert`/etc. action to hold
  content in the first place (`editAction` is `Conflicted`). `files` (`:177`) follows the same
  rule: for an unapplied plan, one `{path, before, after}` per `planWrites` entry, `before`
  from `planBefore`; for an applied one, `[]`.

Errors that stop generation before there is a plan (`GenerationError`, `Generation.hs:52`)
go through `generationFailure` (`Cli.hs:297`), which calls `describeGenerationError`
(`Generation.hs:83`, naming the step the error happened in) and picks the exit code: 4 for
inputs, 3 for an unknown generator, 1 for a broken one. `GeneratorTesting.runCase` calls the
same function when a case fails, so a pack test's failure message is the CLI's own sentence,
not a dumped Haskell value (below).

## The other use case: `olifant actions`

`gen` is one use case; the other is what editors call before it (user book, chapter 14):

```sh
olifant actions --at src/Olifant/Catalog.hs:94:3     # in this repo: on `data CatalogError`
```

```
Add a constructor to CatalogError (GHC finds every case)  (olifant-dev/error-case)  asks: name
```

**Loading.** `[applies_at]` is one more optional top-level table. `parseGenerator` reads it
last (`Generator/Load.hs:61`), after the inputs, because `appliesAtFrom` (`:76`) checks every
name against them: each `{slot}` in `path`, each `bind` key and `file` must be an input, and
each `bind` regex goes through `posixRegex` (the same check as fragments and `each`), so `\w`
is a load error. `node` is parsed by `ruleFromTree` (`:87`), as an `at` rule is, and so is
`after` (`:90`); a table with both is refused (`` `applies_at` takes `after` or `node`, not
both ``). `label` and `where` are templates, and `suggest` is an optional boolean, `False`
when absent. The result is `genAppliesAt :: Maybe AppliesAt`
(`Generator/Definition.hs:56`; `AppliesAt` at `:124`: `apPath`, `apNode`, `apBind`, `apFile`,
`apLabel`, `apWhere`, `apSuggest`, a snippet's `apTrigger`, [below](#snippets-gen---at-olifant-complete-and-uncovered),
and `apAfter`). `applies_at` is in `topKeys`, so `olifant schema` describes it. Errors
are prefixed `` `applies_at`: `` once, by `optional`.

**The command.** `Cli/Options.hs` (`actionsP`, `:124`) takes exactly one of `--at
FILE:LINE:COL`, split on `:` from the right (a path may contain colons, `positionP`, `:131`),
and `--file FILE`, plus `--content -` as a flag, into `ActionsCmd file (Maybe (line, col))
fromStdin`: `Just` the position for `--at`, `Nothing` for `--file`. `runCommand`
(`Cli.hs:147`) makes the path project-relative (`makeRelative (rtProjectDir rt) (normalise
(cwd </> file))`), reads the text from stdin (`rtReadStdin`) or the disk, lists the project's
files for the queries, and pairs every catalog entry's id with its definition. Generators that
fail to load aren't catalog entries, so they are never offered. Then it branches on the
position:

- `--at` calls `actionsAt`. `actionJson` (`:738`) renders each `Action` as `{generator,
  name, label, sets, missing, choices, suggest, snippet, trigger}`; the human form is one
  line per action. `name`, `suggest`, `snippet` and `trigger` are not on the `Action`:
  `Actions` knows nothing of display names or editors, so `runCommand` looks them up by
  generator id in the catalog (`nameOf`, and `suggestOf`, `:171`, which gives `apSuggest`,
  `isSnippet` and whether there is an `apTrigger`) and passes them in.
- `--file` calls `actionsInFile` and renders `{"actions": [...], "nearby": [...]}` in place:
  each `FileAction` with its `range` (`{start: [l, c], end: [l, c]}`, or `null`) and the same
  `name`, `suggest`, `snippet` and `trigger` (no `choices`), each `Nearby` as `{generator, name, label, why}`. The human form puts `LINE:COL  ` (or `file  `)
  before the `--at` line, then a `nearby: GENERATOR — WHY` line per near miss.

**Matching** (`Actions.hs`). Both use cases take the ports they need as arguments (the
`Syntax` port, the languages, the `Queries` port, the file list and a reader) and share two
steps: `candidates` (`:113`), which says how each generator relates to the file, and `offer`
(`:154`), which turns a generator and a node into an `Action`.

```haskell
-- src/Olifant/Actions.hs:81
data Candidate = Whole | Nodes [Match] | After [Match] | Near Text
```

`candidates` keeps each generator whose `genAppliesAt` is set and whose path matches, with one
of these:

1. **The path.** `matchPathSlots` (`:39`) compiles the pattern to an anchored POSIX regex:
   `{name}` becomes `([^/]+)` and remembers `name`, `**` becomes `.*`, `*` becomes `[^/]*`,
   and regex metacharacters are escaped. The capture groups, zipped with the names, are the
   path's bindings. No match: the generator is dropped.
2. **No `node`**: `Whole`. The generator applies to the whole file.
3. **A `node`**: the rule is rendered as a template over the path's bindings (`renderRule`,
   `Language/Rule.hs:26`), which is how `wire-command`'s rule says `^pub struct {=
   command|pascal =} `. The file's language comes from `languageFor`, and `querySyntax` runs
   the rule over the given text (the buffer from `--content -`, or the file). At least one
   match: `Nodes` with all of them. None, an ast-grep failure, no language, or a rule that
   won't render: `Near why`, where `why` is `apWhere` rendered over the path's bindings, or,
   if there is none or it won't render, `describeRule` of the rendered rule (or "a matching
   node" when even that failed).
4. **An `after`** (and no `node`): the same search (`search`, which takes the constructor to
   wrap the matches in), giving `After` with the matches, or the same `Near`.

`describeRule` (`:135`) reads the rule's JSON: `kind` becomes "a `KIND`", `regex` "matching
/RE/", `inside` and `has` recurse as "inside …" and "containing …". Other keys are ignored, so
it is a description, not a translation.

**`actionsAt`** (`:85`) is the cursor's view. `Whole` offers the generator with no node.
`Nodes` keeps the matches whose range contains the cursor, sorted by size, lines then
columns, and offers the smallest. `After` keeps the matches the cursor's line *follows*
(`follows`, `:141`: the node ends above the cursor's line, and every line between them is
blank) and offers the nearest, the one that ends last. `Near`, or no match containing (or
followed by) the cursor: nothing. The cursor arrives as a 1-based line and a UTF-8 byte column (Neovim's); ast-grep's columns are
characters, so `charColumn` decodes that many bytes of the cursor's line and counts them, and
the 0-based `(line, char)` is compared with `matchStart <= cursor <= matchEnd`, so just past
the end counts.

**`actionsInFile`** (`:154`) is the file's view. It offers `Whole` once, with `range =
Nothing`, and `Nodes` once per match, each with its own node, so each has its own `bind`
values. `After` is also once per match, but its range is where `--at` would apply, not the
node: `lineAfter` (`:176`) runs from the line after the node to the last of the blank lines
that follow it, column 1 on both (at least that first line; durandal.nvim's e2e found the
range first covered only it, while `--at` accepted any of them). The `Range` (`:61`) converts back from ast-grep's 0-based character positions: the
line plus one, and the column as one plus the UTF-8 length of the line's first `c`
characters (`pos`). Actions are sorted by `fmap rangeStart . faRange`, which puts `Nothing`
(whole-file actions) first and the rest in file order. `Near why` becomes a `Nearby` with the
generator's label rendered over the path's bindings alone (`labelFor`, `:149`), since there
is no node to bind from.

**`offer`** (`:154`) does the rest for one generator and one node, or none:

1. It adds the `bind`s (each regex's first non-empty capture group over the node's
   `matchText`) and `file` (the path itself) to the path's bindings.
2. **Choice validation.** It calls `withChoices` (`Generator/Choices.hs:25`), the same
   resolver `gen` and `show` use, with the bindings as the sets and a fixed time. For each
   bound input that resolved to an enum (`EnumOf … (Resolved choices _)` or `EnumS`), a
   value not in its choices is dropped. Other types pass through: `gen` validates them.
3. **The result.** `missing` is every required input without a default that isn't bound;
   the label is `labelFor`: `apLabel` rendered over the kept bindings, or the description if
   there is no label or it fails to render (it can't see `[derive]`).

`--file` costs one ast-grep call per generator whose path matches, plus `withChoices` per
action, whose `one_of` queries may run commands or AST queries over the project. `--at` makes
the same ast-grep calls (it too queries the whole file, then picks the node) but calls
`offer` at most once per generator. So for hints, one `--file` per buffer change is cheaper
than an `--at` per cursor move.

`ActionsSpec.hs` tests each of these with a fake `Syntax` (path slots, innermost node,
boundaries, the given text rather than the file, invalid choices moved to `missing`, slots in
the rule, `file`, byte columns). Its "--file" group (`:107`) covers whole-file actions, one
entry per node in file order, byte-column ranges, `nearby` with and without `where`, and a
path that matches nothing. Its last group, "the rust-htmx demo generators (real ast-grep)",
runs `wire-command`'s and `failure-arms`'s real `[applies_at]` through `preparePack` and
ast-grep on their test cases' `given/` files, including the miss that motivated `nearby`: a
field typed into `CancelInvoiceInput` gives no `wire-command` action, and a reason. The
"after" group (`:112`) covers the line right after a node and after blank lines only, nothing
inside the node, before it or past code that follows it, the `--file` range over the blank
lines, and the load error for `after` with `node`.

### Suggestions: `suggest` and `--overlay` together

Ghost text and follow-ups (user book, chapter 14; spec §4.8) need nothing new from `Actions`
or `generate`. The editor already has the hints' `actions --file` answer; for the cursor's
line it takes an action with `"suggest": true` and nothing `missing`, and runs that action's
`gen` with `--plan --json --no-fill --overlay FILE`, the buffer on stdin. Whether the plan is
ghost text or a follow-up is decided in the editor, from `files` (one contiguous insert in
FILE near the cursor, at most 8 lines, or not), and holes are drawn from
`holes[].placeholder`. Accepting runs the same command again without `--no-fill`. So
Olifant's part is three small pieces: `Workspace.overlay`, the `--overlay` option in
`runSteps`, and two fields of JSON (`suggest`, `placeholder`). `WorkspaceSpec` tests
`overlay` (the overlaid text read, the rest delegated, a file that is not on disk listed and
existing); `CliSpec`'s "plans against an unsaved buffer" runs `gen --plan --overlay` on a
memory workspace and checks the plan saw the stdin text and the disk did not change, and that
`--overlay` without `--plan` is exit 2.

## Snippets: `gen --at`, `olifant complete` and `uncovered`

A snippet (user book, chapter 14; spec §4.9, §5.3) is a generator whose one edit goes at the
cursor. Most of it is the machinery above, reused: the cursor edit becomes a replacement, the
tab stops are holes, the menu is `actions`. Three small units carry what is new: **Cursor**
(where the text goes), **Snippet** (hole sources, and what an editor expands) and
**Uncovered** (the compiler's missing cases at the cursor).

**Loading.** `editFrom` (`Generator/Load.hs:230`) reads `cursor = true` with a `text` or
`template` (keys `cursorKeys`) into `Cursor {editSource}` (`Generator/Definition.hs:93`);
`cursor = false` is refused. `parseGenerator` refuses a cursor edit beside any other edit
(`:45`), reads `[holes.NAME]` into `genHoles :: [(Text, HoleSource)]` (`:65`;
`holeSourceFrom`, `:206`; `HoleSource = Yours | Derived Template | Model`,
`Definition.hs:129`), and `appliesAtFrom` reads `trigger` through `posixRegex` (`:95`) into
`apTrigger`. `queryFromTree` (`Query.hs:77`) reads `uncovered = REGEX` as `UncoveredQuery`.
`Catalog.loadGenerator` then writes the hole sources into the markers' kinds (4e).

**Placing it: `gen --at`.** `parseGen` pulls `--at FILE:LINE:COL` out of the arguments
before the steps are split (`takeAt`, `Cli/Options.hs:193`, as it does `--overlay`), and
`cursorP` (`:170`) splits it from the right into `genAt`. In `runCommand`, once the steps'
generators are loaded, `placeCursor` (`Cli.hs:434`) runs before `runSteps`:

1. No snippet among them: the generators are unchanged (`--at` still gives `uncovered`
   queries their cursor).
2. A snippet and no `--at`: `Left`, which `runCommand` turns into a usage failure, exit 2.
3. Otherwise it reads the cursor's file, from stdin when it is the `--overlay` file, and
   replaces each snippet's `Cursor src` with `atCursor unit path line lines gen`
   (`Cursor.hs:61`): a `Replace` of line `n`, `(n, n)`, whose old text is the line as read
   and whose new text is `indentTemplate (cursorIndent unit ls n) src`. `unit` is the
   adapter's `langIndent`.
4. A snippet whose `[applies_at]` has an `after` rule is placed with `atCursorIndented`
   (`Cursor.hs:65`, which `atCursor` is now a case of) and the indentation of the node the
   cursor follows: `afterIndent` (`:78`) runs the rule (`afterRule`, `:91`, renders it with
   no inputs) through the Syntax port, keeps the matches the line follows, as `actionsAt`
   does, and takes the leading whitespace of the nearest one's first line. The line above is
   no guide here: below a Python class it is the class's last field, one level deeper.
   When the line follows no such node, or the search fails, `cursorIndent` decides as
   before.

From there the planner sees a replacement, the kind a rule's fix makes (`Replace`,
spec §5.9): `decideItems` checks the line still reads `old` (so a buffer that changed since
is a drift conflict) and splices the new lines in. One fix was needed in `decideItems`
(`Generation/Planner.hs:97`): an `old` of `""` is one empty line, not none, so a blank
cursor line matches. `renderEdits` refuses a `Cursor` that reaches it (`Render.hs:53`); only a
caller that skipped `placeCursor` could do that.

`cursorIndent` (`Cursor.hs:21`) is the rule from the user book: a line with anything on it
keeps its own leading whitespace; an empty one takes the previous non-blank line's, plus
`unit` when that line ends with `{`, `(`, `[` or `=`, or is or ends with the word `where` or
`do`. `indentTemplate` (`:34`) indents the template rather than its output: a `Lit ind`
first, and `ind` after every newline inside literal text unless the line it starts is empty.
A line that starts with a hole or a value is not empty, so it is indented too.

**Reading stdin once.** `placeCursor` reads the overlay's text from stdin, and so does
`Workspace.overlay` later. `Main` now wraps `rtReadStdin` in an `IORef` (`app/Main.hs:31`),
so stdin is read at most once and every reader gets the same text.

**The snippet in the plan.** `runSteps` gives `generate` the cursor's queries (`queriesAt`,
below) and, after it, sets `planSnippet` with `snippetAtCursor` (`Cli.hs:482`). That takes
the cursor file's text from `planMarked`, the text with its hole markers still in (from
`planWrites` if it has none), takes as many lines from the cursor's line as the replacement
there has, and looks up each model hole's fill in `planHoles` by its bare name (the name
before any `[file:line]`). `snippetOf` (`Snippet.hs:61`) walks the lines with `splitMarkers`
and numbers the holes in reading order with `mapAccumL`. Each marker becomes, in the ghost,
`⟨name⟩` (yours, or a model hole with no fill), its value (derived) or its fill, and in the
body `${N:…}` holding the name, the value or the fill. Literal text goes through `lspEscape`
(`:78`), which escapes `\`, `$` and `}`, and the body ends with `$0`. Indentation stays as
placed.

**The menu: `actions` and `complete`.** `actionsAt` is now `actionsAtWith True`
(`Actions.hs:83`). `actionsAtWith` (`:103`) sorts the generators with snippets last (`ordered`,
`:108`, a stable `sortOn isSnippet`) and drops a candidate whose `trigger` the text before the
cursor doesn't end with (`triggered`, `:124`): the cursor line's first `COL - 1` bytes,
matched against the regex with `$` appended unless it ends with one. `offer` adds
`actChoices` (`:209`): each missing input whose resolved type is an enum, with its values.
`completionsAt` (`:91`) is `actionsAtWith False` (no trigger) over the snippets only; an
action with nothing missing is one `Completion`, one missing enum input with choices is one
per value (its label rendered with that value), and anything else is dropped.
`runCommand` renders actions with `choices` (`actionJson`, `Cli.hs:738`) and, from the
catalog, `suggest`, `snippet` and `trigger` (`suggestOf`, `:171`); `CompleteCmd` (`:191`)
renders completions with the generator's `name` and `description` (`detail`).

**`uncovered`.** The `Queries` port has a third field, `queryUncovered :: Text -> m (Either
QueryFailure [Record])` (`Query.hs:200`). Everywhere without a cursor it answers `[]`
(`projectQueries`, `noQueries`, `preparePack`). `queriesAt` (`Cli.hs:664`) swaps in
`uncoveredQuery checker langs ws file line` for `actions --at`, `complete` and `gen --at`,
where `ws` is the project with the overlay or `--content -` text laid over it. `uncoveredQuery`
(`Uncovered.hs:43`) finds the file's language and its `[check]`, takes the project files the
adapter and the check's `context` cover (pruned directories left out), placeholderizes their
holes and runs the check through the **Checker** port. `uncoveredAt` (`:31`) reads the
report's missing-case sites in that file that start at or above the cursor's line, takes the
latest, writes each pattern as the site writes it (its first argument: the compiler
support's `supportPattern`, given the site and the file's source; rustc's drops a leading
`&` or `&mut `, pyright's qualifies `A()` as `create_booking.A()` when the other arms do),
and keeps those matching the regex. Each becomes a record `{name = pattern}`.

**Tests.** `parseCase` reads a case's `at`. `runCase` passes it
to `atCaseCursor` (`GeneratorTesting.hs:145`), which places a snippet as `placeCursor` does,
`afterIndent` included, and sets the tools'
`queryUncovered` to the cursor's; a snippet case's second run is skipped (`:80`).
`GeneratorTestingSpec` leaves cases whose `inputs.toml` has an `at` line out of "rust pack
output compiles" and the "python pack" group's "output type-checks", and "placeholder output compiles" calls `atCaseCursor` too. The unit specs
are `CursorSpec` (indentation, and the replacement), `SnippetSpec` (ghost, body, escaping,
sources), `UncoveredSpec` (which site, `&`, the regex) and new cases in `GeneratorSpec`,
`QuerySpec`, `ActionsSpec` (order, triggers, choices, `completionsAt`) and `CliSpec` (`gen
--at` with and without `--overlay`, the `snippet` JSON, `complete`).

**The editor's side.** `durandal.nvim` gates an anticipated snippet with `actions --at`
(the trigger and the choices), plans it with `gen --at … --plan --json --no-fill --overlay`
for the ghost, and on `Tab` plans it again with the model and `--progress` and expands the
plan's `snippet.body` with `vim.snippet.expand` on the emptied line. Its nvim-cmp source calls
`complete`. None of that writes a file: the expansion is a buffer edit.

## The third use case: `olifant doctor`

`olifant doctor` (spec §4.7; user book, chapters 2 and 14) answers "what is missing here?" It
is a unit of its own, `Doctor.hs`, which imports only `Language` and `Model`: it is handed a
small `DoctorInput`, not the catalog, and the `DoctorPorts` shown in the ports section.

```haskell
-- src/Olifant/Doctor.hs:26
data DoctorInput = DoctorInput
  { diProject :: Maybe FilePath
  , diPacks :: [Text]
  , diLanguages :: [Language]
  , diBrokenLanguages :: [(Text, Text)]
  , diBrokenGenerators :: [(Text, Text)]
  , diModel :: ModelConfig
  }
```

`doctor` (`:47`) returns a list of `Check {checkName, checkStatus, checkDetail, checkFix}`,
with `Status` one of `Ok`, `Warn`, `Bad`, in this order:

- **`config`**: `Ok` with the project root, or `Bad` when `diProject` is `Nothing`.
- **`pack NAME`** and **`language NAME`**: `Ok` for each one loaded. **Broken** language
  adapters and generators are `Bad`, with the catalog's reason and "fix the adapter file" or
  "fix its generator.toml".
- **`ast-grep`**, then **`check tool EXE (LANGUAGE)`** for each adapter with a `[check]`,
  where `EXE` is the program its `command` runs, by `checkProgram` (`checkTool`, `:68`: the
  first word, or the word after a leading `env` and its settings, so Python's is `uv`):
  `tool` asks `dpFind`; when it is missing, the detail shows `dpPath`'s `PATH` and the fix is
  `installHint` (`:86`), which knows where `cargo`, `elm`, `ghc` and `cabal` usually live, and
  that `uv` comes from `brew install uv` (pyright then comes with the project, `uv sync`).
- **`model`**: no `[model]` is `Warn` with `modelSnippet` (`:94`) as the fix; backends with
  no `default` is `Warn`; a `default` with no such backend is `Bad`. An `OpenAICompatible`
  default is probed with `dpProbe`, and a failure is `Warn` with the reason. An `Anthropic`
  default is never probed: `dpEnvSet` says whether its `api_key_env` is set, and only that
  (a request would spend the user's key).

Missing tools are errors, the model only ever a warning: holes still work without a model,
they just stay placeholders.

**The real probe** is `probeBackend` (`Model.hs:213`), next to `curlFiller` because it uses
the same tools: `curl -sS --max-time 3 URL/models` through `Process.runBytes`. It wants exit
0, a JSON body, and the model's name among `data[].id` (the OpenAI list shape Ollama serves);
otherwise it returns why ("the server answers but has no model `NAME` (pull it, e.g. `ollama
pull NAME`)", or curl's own stderr). For `Anthropic` it returns `Right ()` without doing
anything; `doctor` never calls it for one.

**CLI wiring.** `doctor` is `DoctorCmd` (`Cli/Options.hs:105`). `runCommand` (`Cli.hs:170`)
builds the `DoctorInput` from the catalog (`catalogProject`, `catalogPacks`,
`catalogLanguages`, `catalogBrokenLanguages`, `catalogBroken`, `catalogModel`; the first two
are exported for this), runs `doctor (rtDoctor rt)`, and renders JSON (`{"checks": [{name,
status, detail, fix}]}`, with `Bad` as `"error"`) or one line per check (`✓`, `!`, `✗`)
followed by `  fix: …`. The exit code is 1 if any check is `Bad`. The catalog is opened before
any command runs (step 2), so a config that does not parse, or a missing pack, stops doctor
there, with the usual error, before any check.

`DoctorSpec.hs` runs `doctor` in `Identity` with fake ports (`ports`, `:25`): all present, a
missing check tool (its fix names both rustup locations), a model that doesn't answer, no
`[model]`, an Anthropic backend whose probe would fail if called, broken adapters and
generators, and no project. `CliSpec.hs:249` checks the CLI's rendering and exit codes.

## The fourth use case: conventions (`olifant check` and `sync --plan`)

Generators keep conventions when they write code; `olifant check` checks them when people
write it (spec §5.9, §5.10; user book, chapter 15). It joins two things: the owned regions
the regions phase already refreshes, and **rules**, a new unit.

```sh
olifant check          # in a rust-htmx sandbox, with `use diesel::prelude::*;` in a command
```

```
! billing/billing-service/src/cancel_invoice.rs:2  commands-use-ports — Commands talk to ports, not the database (rust-htmx: ports & adapters)  (fix: Add Repository Port)
conventions hold (1 finding, none blocking)
```

### The `Rules` unit (`src/Olifant/Rules.hs`)

`Rules` is pure apart from the `Syntax` port it is handed, and knows nothing about the
catalog or the CLI. Its types:

```haskell
-- src/Olifant/Rules.hs:44
data RuleCheck = RuleMatch Rule | RuleMissing Template

-- src/Olifant/Rules.hs:65 (fields abbreviated)
data Finding = Finding
  { fRule :: Text, fFile :: FilePath
  , fRange :: Maybe ((Int, Int), (Int, Int))   -- 1-based lines, UTF-8 byte columns; Nothing for a whole file
  , fSeverity :: Severity, fMessage :: Text
  , fFix :: Maybe Text, fSets :: [(Text, Text)]
  , fFingerprint :: Text }

-- src/Olifant/Rules.hs:80
data Exception = Exception {exRule :: Text, exWhere :: Text, exReason :: Maybe Text, exStale :: Bool}
```

**Parsing.** `parseRules` (`:84`) reads `[rules.ID]` tables with the usual `Toml` decoders
(`onlyKeys`, so a typo gets "did you mean"). A rule has exactly one of `match`, parsed by
`ruleFromTree` like an AST query's rule, and `missing`, parsed as a template. `severity`
defaults to `Warn`, and `off` is refused there: turning a rule off is the project's
decision, not the pack's. `since` and `why` are not kept as fields; they are appended to the
message (`" (since 2026-10: …)"`), so everything that shows a message shows them.
`parseRulesConfig` (`:127`) reads the project config's `[rules]`: every key but `allow` is a
rule id with a severity (`off` allowed), and `[[rules.allow]]` is `(rule, paths, reason)`,
all three required.

**`checkFiles`** (`:146`) is the whole check:

```haskell
checkFiles :: Monad m => Syntax m -> [Language] -> RulesConfig -> [RuleDef] -> [FilePath]
           -> (FilePath -> m (Maybe Text)) -> [FilePath] -> m ([Finding], [Exception])
```

It takes the project's file list (for `missing`), a reader (so the CLI can hand it a buffer
from stdin for one path, and the disk for the rest), and the files to check. For each file:

1. **Which rules apply.** A rule applies when the project's severity for it isn't `Off`, its
   `files` pattern matches (`matchPathSlots`, the same pattern language as `[applies_at]
   path`, `Actions.hs:39`, which is why `Rules` imports `Actions`), and no `exclude` glob
   does. The pattern's slots are kept: they become the finding's `fSets`, the fix's inputs.
2. **What each finds.** `RuleMatch`: the rule is rendered as a template over the slots
   (`renderRule`) and run over the file's text through `querySyntax`, one finding per match;
   ast-grep's 0-based character positions become 1-based lines and UTF-8 byte columns, as in
   `actionsInFile`. A file without a language, or a failed query, finds nothing.
   `RuleMissing`: the template is rendered with `dir`, `stem` and `file`, `..` is collapsed
   (`collapse`, `:212`), and if that path is not in the file list the file gets one finding
   with no range.
3. **Exceptions.** `parseAllows` (`:223`) finds `olifant:allow RULE` and
   `olifant:allow-file RULE` comments: every occurrence of the language's comment prefix
   (`langComment`) on a line, kept only when an even number of `"` come before it on that
   line, so `"// olifant:allow …"` in a string is not one. `allow-file` counts only in the
   first five lines. The reason is whatever follows the rule after a leading `—`, `-` or `:`.
   A finding is suppressed (`suppressedBy`) by a file allow for its rule, a line allow on its
   first line or the line above, or a config allow whose `paths` glob matches the file.
4. **Reporting the exceptions.** Allows naming no loaded rule become `unknown-rule`
   findings, and allows for a known rule without a reason `allow-without-reason`, both
   `Warn` with the allow's line. Every allow for a known rule is an `Exception`, stale when
   it suppressed nothing in its file. Config allows are exceptions where they were used;
   after all files, a config allow used nowhere is added once, stale (only when some file
   was checked, so `--file` on an unrelated path doesn't call everything stale).

Findings are sorted by line within a file (a whole-file finding, `Nothing`, first).

**Fingerprints** (`:204`). A baseline must recognise a finding after lines move, so a
finding's identity is not its line. `fingerprint` is 64-bit FNV-1a over the rule id and
what was found (the matched node's text, or the missing path), each with runs of whitespace
collapsed to one space, joined by `\0`. Moving code, re-indenting it or adding lines above
it keeps the fingerprint; changing the matched text, or the file's path, does not.

`RulesSpec.hs` runs `checkFiles` in `Identity` with a fake `Syntax` (`diesel`, `:40`: every
line starting `use diesel` is a match), covering matches with slots and fixes, `missing`,
line and file allows, reasons, unknown rules, stale allows, allows in strings, `[rules]`
severities and path allows, fingerprints across moved lines, `since`/`why`, and the loading
errors. `GeneratorTestingSpec`'s "rust-htmx rules" runs the pack's real `rules.toml` through
real ast-grep.

### Loading rules (`Catalog`)

As step 2 above describes, `openAt` adds each pack's `rules.toml` and
`openProjectCatalog` the project's `.olifant/rules.toml` (`addRules`: a later rule replaces
an earlier one with its id), reads `[rules]` into `catalogRulesConfig`, and runs
`checkFixes` once the generators are known. A file that doesn't parse is in
`catalogBrokenRules` with its path; a rule with a bad `fix` is there as `rules.toml` with
the rule's id.

### `olifant init` (`runInit`, `Cli.hs:92`)

`runCli` handles `InitCmd` before it looks for a project (there is none yet). `runInit`:

1. resolves `--pack`: a directory, or a name in the user config's `[packs]`
   (`packsFromConfig`, exported by `Catalog`);
2. refuses a target folder with anything but `.git` in it, through the root workspace
   (`rtRoot`), naming what is there;
3. opens the pack's catalog and finds its one starter (`genStarter`) **before** writing
   anything, so a pack with none, or two, leaves no `.olifant/` behind;
4. writes `.olifant/config.toml`: the `[packs]` line, then the user config's `[model]`
   tables copied as text (`modelTables`);
5. opens the new project's catalog and runs the starter through `runSteps` with `name`,
   like a `gen`; then `initFences` sets up every region instance the starter's files made;
6. calls the runtime's `rtGitInit dir "olifant init: PACK"` (`Main.gitInit`: `git init`,
   `git add -A`, `git commit`; in tests a recorder), and prints the starter's `genNext`
   lines rendered with `name`.

`--plan` stops after step 5's plan, writing nothing. `GeneratorTesting` skips the fixture
for a starter's case (`genStarter`): a starter makes a project from nothing.

### `olifant check` (`Cli.hs:177`)

`checkP` (`Cli/Options.hs:125`) parses `CheckCmd report file fromStdin baseline
changedSince`. `runCommand` reads `.olifant/baseline.toml` with `readBaseline` (`:538`) into
`(rule, file, fingerprint)` triples (a baseline that doesn't parse counts as empty), and a
finding is `baselined` when its triple is there. Then three branches:

- **`--file FILE`** (`:198`): the path is made project-relative as for `actions`, its text
  read from stdin with `--content -`, and `checkFiles` runs on that one file, with the
  reader returning the stdin text for it and the disk for every other path. No regions, and
  always `ExitSuccess`: an editor shows findings, it doesn't fail.
- **`--baseline`** (`:204`): `checkFiles` over every file some rule's `files` matches, and
  the findings (after allows) written as `[[baseline]]` tables to `.olifant/baseline.toml`,
  replacing the whole file. That is how debt goes down: an entry whose finding is gone is
  simply not written again.
- **Otherwise** (`:213`): `refreshRegions ReportUnset` with an empty plan, the same call as
  `sync --check`, so drift, broken fences and regions not set up come back as edits and
  conflicts. With `--changed-since REF`, regions are skipped and `rtGitChanged` gives the
  changed line ranges and untracked files; `touched` keeps a finding in an untracked file,
  or in a changed file whose range overlaps a changed range, or that has no range (a whole
  file). `failed` (`:234`) is any drift, region conflict, broken rule, git failure, or a
  finding that is `Error` and not baselined.

Both renderings are built and `emit` prints one. JSON: `{"regions", "findings",
"exceptions", "errors"}`, plus `rules` (`id`, project severity, `debt`) with `--report`;
`fix_name` is the fix generator's `name` (`nameOf`, as `actions` does). Human: drift and
conflicts, `✗` broken rules, one line per finding (`findingHuman`: `✗`/`!`/`·` by severity, `·`
and `(existing)` when baselined), with `--report` the exceptions and `debt:` lines, and the
verdict. `CliSpec`'s "olifant check" group runs all of this against `memoryWorkspace` and
`noSyntax`, using `missing` rules (which need no ast-grep) for findings.

### `sync --plan` and `sync --init` for a family

`sync`'s three outputs are one function: `syncRegions` (`Cli.hs:449`) takes a `SyncOutput`
(`:446`), `SyncWrite` (`sync`), `SyncReport` (`sync --check`) or `SyncShow` (`sync --plan`).
All three run `refreshRegions` and build a `Plan` named `sync`, now with `planBefore` read
from the files the refresh would write, so `planJson` gives `--plan` the same `files`
(`before`, `after`) a `gen --plan` has. Only `SyncWrite` writes, and only `SyncReport` exits 5
for drift.

`sync --init NAME` is `initRegion` (`:463`): it expands the declaration with
`regionInstances` and calls `initFences` (`:478`) on each instance, which returns `Right
True` (fences added), `Right False` (already there) or `Left (exit code, error code,
message)`. The first `Left` fails the command. A single region that was already set up says
so, as before; anything else runs `syncRegions … SyncWrite` with just that declaration, so
every instance is filled in one plan.

### The editor's side

Durandal calls `sync --plan --json` after each save and applies `files` as buffer edits,
then `check --file FILE --content - --json` for the saved buffer's diagnostics (user book,
chapter 14). Both are read-only, so the editor decides what is written, and when.

## The fifth use case: the language server (`olifant lsp`)

`olifant lsp` (`Cli` hands it to `Lsp.runLsp`) is one long-lived process per editor session.
It answers from the same operations the CLI calls, in four layers, each knowing less than the one
above:

```
Lsp      the protocol: handlers, URIs and positions, WorkspaceEdits, $/progress, threads
Server   the state: documents, projects, caches, jobs — and each feature as state → (state, answer)
App      the operations the CLI shares: open a project, actions at a cursor, plan, findings, doctor
engine   Catalog, Actions, Generation, Rules, ProjectModel, …
```

### `App`: one set of operations, two front ends

Before the server, `Cli` held the use cases' glue (find the project, open the catalog, build
the tools, run `generate`). `App` is that glue, lifted out so the server could call it too.
Everything it needs from the world comes through **`Ports`** (`App.hs`): the filesystem
(`pFiles`), syntax (ast-grep), the checker and verifier, the clock, the user's config and home
directories, and the doctor's probes. A **`Project`** is a loaded root: `prRoot`, its catalog
(`prCatalog`) and a workspace rooted there (`prWorkspace`). `Main` builds the real ports; specs
build in-memory ones. The CLI and the server never import each other's code: the CLI's
`olifant lsp` subcommand is the one edge, and it only starts the server.

### `Server`: the state, without the protocol

`Server/State.hs`'s `ServerState` is the whole server as a value:

- **`ssDocs`**: every open document's version and text. `workspaceOf` lays them over the disk
  (`overlayMany`), so every engine call reads unsaved buffers.
- **`ssProjects`**: one slot per project root, `Loaded Project` or `Broken why lastGood` — a
  project whose configuration stops loading keeps serving its last good catalog, and `brokenFor`
  gives the reason, which `Lsp` puts at the top of each of its files' diagnostics. `projectFor
  file` finds the nearest root (`nearestProject`) and loads it on first use. Packs come from
  a shared cache keyed by (pack name, resolved folder), with a fingerprint, so projects using
  the same pack folder parse it once and a project with its own copy has its own.
- **Caches.** `ssGeneration` is bumped by anything that can change an answer (an edit, a
  watched file, a reload); `memo request file where work` keeps one answer per (request,
  file, position) for the current generation. `ssListings` caches each project's file list
  until files may have come or gone (`ssFilesStamp`: saves and watched-file events).
- **`ssConfig`**: the client's `initializationOptions` (debounce, model, fills).
- **`ssCancel`**: the token of the work the state was handed to (below).

Each feature is a function `… -> ServerState -> IO (ServerState, answer)` in its own module
(`Server/Actions.hs`, `Fixes.hs`, `Hints.hs`, `Completion.hs`, `Suggest.hs`, `Holes.hs`,
`Regions.hs`, `Model.hs`), returning the state because answering may load a project or fill a
cache. `Server/Edits.hs` turns a `Plan` into `FileEdit`s: the smallest run of changed lines per
file, with positions in the editor's units (UTF-16; `bytePosition`/`editorPosition` convert),
so a cursor and folds stay put. A property spec pins that applying the edits yields the plan's
text.

`Server/Jobs.hs` runs debounced work: `submit jobs key delayMs work` cancels the key's previous
job and starts this one after the delay; a job superseded before it starts never runs.

### `Lsp`: the adapter

`Lsp.hs` holds an `Env` (the state in an `MVar`, the jobs, the ports, the running requests,
and the token of the work at hand) and one handler per method, each `guarded`: `lsp` runs
handlers on its message loop without catching, so an exception would end the process; a guarded
request answers an internal error instead. The pattern of most:

- **`withSnapshot env work`**: run `work` on the state as it is now, without holding the
  lock; afterwards merge back what the work loaded (projects, caches) and never undo documents
  that changed meanwhile. Projects it loaded are kept only if no file changed meanwhile
  (`ssFilesStamp`): a configuration changed under a load is read again when next asked. `withPlanned` also returns the state a plan was made on: its edits
  are stamped with that state's document versions, never the versions current when it answers,
  so an editor refuses them once the buffer moved on.
- **`inBackground`**: slow requests (`codeAction/resolve`, `completionItem/resolve`,
  `olifant/plan`, `olifant/suggest`, completion, inline completion, `olifant/holes`,
  `olifant/model`, `olifant/tests/run`, `olifant/debugState`) run in their own thread, under work-done progress, so the
  server's loop keeps reading (and other requests keep being answered) while a model fills a
  hole. Each runs under its own `Cancel`, kept by request id in `envRunning`; the client's
  `$/cancelRequest` trips it. A cancel on its progress (`lsp` throws into the thread) trips it
  too and answers `RequestCancelled`; any other exception becomes an error response.
- **Saves.** `willSaveWaitUntil` works out the region refresh once: the saved file's edits go in
  the write, the others wait in `envSaving` for the `didSave`, which sends them off the loop.
- **Suggestions are pushed.** An edit (`didChange`) submits a debounced job that plans the
  edited line; its result is pushed as `olifant/suggestion` with a `planId`, kept per document
  version, and fetched with `olifant/plan`. `olifant/suggest` asks the same for the cursor's
  line.
- **Plans reuse `Output`.** A resolved action carries `data.plan`, the exact JSON `gen --plan
  --json` prints, so a client renders the review it already knows.
- **Watching.** The server asks the client to watch `**/.olifant/**` at start, and, as each
  project loads, its root and its packs' folders as `RelativePattern`s (`**/*` under each):
  an editor scopes a plain glob to the workspace folders it had when it registered it, which
  would miss a second project and packs elsewhere. Any change bumps the listing stamp and
  the generation; a change under `.olifant/` or a pack reloads the projects that use it.
- **`initializationOptions`** are read in `configure`; `inlineCompletionProvider` is added to
  the initialize response by hand (`prepareOutwards`), since `lsp-types` 2.4 predates it.

### `ProjectModel`: the seam for the whole system

`ProjectModel.hs` reads a pack's `model.toml` (`parseModelMap`) and builds a project's model
(`buildModel`): elements with stable ids (`PROJECT:path-like-name`), kinds from a shared
vocabulary (or a pack's own, namespaced), and relations whose two ends are both elements. It
runs the same query language generators use, so a pack describes its conventions once. A
failed query becomes a `problem`, not a failure: a partial model beats none in an editor.
`App.projectModel` builds it for a project, `olifant/model` serves it. Nothing draws it yet.

### `Cancel`

`Process/Cancel.hs` is a token: `newCancel` gives the token and the action that trips it;
`neverCancel` is the CLI's. `runBytesCancellable` polls it while a child runs and, when it
trips, kills the child's process group (cargo's rustc, uv's python). If the thread itself is
interrupted (an asynchronous exception: a client's progress cancel), the group is killed and
reaped, and the interruption goes on; only synchronous failures become answers. The ports' checker and
verifier take one (`pChecker`, `pVerifier :: Cancel -> …`; `Main` passes `processCheckerWith`
and `processVerifierCancellable`), and so does a plan (`poCancel`): `planWith` gives it to the
checks, the verifies, the `uncovered` queries and the model filler (`modelFillerWith`, curl),
and the fill loop stops between holes.

In the server, the token travels with the state: `withSnapshot` hands the work its snapshot with
`ssCancel` set from the `Env` it was given, and the feature functions put it in their
`PlanOptions`. A suggestion job's token is the one `Jobs.submit` trips when the next edit
supersedes it; a background request's is the one `$/cancelRequest` trips.

### Running the specs

`src/SpecHook.hs` marks every spec `parallel`, and the suite runs with `-N`: most specs wait on
compilers, pyright and models, not the CPU, so the 1,000 run in about 35 s instead of three
minutes. A spec that measures time is marked `sequential` where it is. Specs share no global
state but the tools' caches, which are safe to share: ast-grep grammars are built under a name of
their own, then renamed; pyright's venv is uv's, which locks it; cargo's target dir is an
`exclusive` check's slot (`Check.inSlot`). A new spec that needs something process-wide (the
working directory, an environment variable) must be `sequential`.

### Recognition (`Recognize`, `Server/Recognize`)

Recognition answers "which convention is this hand-written code, if any", without a model:

- **Shapes** (`Recognize/Shape`): one pass over the text; keywords (the adapter's `keywords` and
  the words of the pack's templates) and punctuation kept, names as case shape and word count,
  literals as `str`/`num`, the language's comments dropped; overlapping windows of three tokens.
- **Weights** (`Recognize/Weights`): each window's rarity over the project's files of the
  language (a smoothed idf), and the boilerplate (windows in more than 30% of the files),
  ignored. `containment` is the weighted share of the change's windows a reference explains —
  not a symmetric similarity, because only the start may be typed.
- **Paths** (`Recognize/Paths`): a `create` path with its derived names followed down to inputs,
  as a glob (to find the project's own instances, its siblings) and inverted (a path gives back
  the inputs that would have rendered it, decoded through their case).
- **The index** (`Recognize/Index`): per project and language, each recognisable generator's file
  ("convention"): its path pattern, its template, and references — the template rendered with
  sample values (`sampleText`) and each sibling. The server caches it per file stamp.
- **Deciding** (`recognizeIn`): gates (3 lines; names beyond boilerplate), a convention whose
  template declares its main thing by a name needs code declaring that kind, scores (shape +
  place), the place-aware bands, a choice with a near runner-up, a rival for the model.
- **The model** (`Recognize/Ask`): the `Recognizer` port; `consult` asks only the unsure ones and
  validates the answer; `curlRecognizer` asks a configured backend through `Model.askBackend`.

`Server/Recognize` turns a recognition into a `Suggestion` (kind `recognition`): its plan is the
generator planned with the user's file hidden (`hidePath`), then converting that file (10
non-blank lines or fewer) or keeping only its companions. Guards: only new files — untracked
by git (`pTracked`, the `gitTracked` adapter), else not there when the project was first listed
(`ssEstablished`), never a file whose plan would only rewrite itself;
earned quietness (`ssQuiet`, `.olifant/state/recognition.json`) and dismissed shapes
(`ssDismissed`) from `olifant/recognitionFeedback`. The suggest job tries a follow-up first, a
recognition next, then (unsure, a model configured) `recognitionUpgrade` for the same version.
The corpora (`packs/*/tests/recognize/`) are the gate on any change to scoring.

### The generate picker's preview (`Preview`, `Server/Preview`)

The picker plans a generator before its inputs are all set, so the person sees exactly what
Enter will write while they type:

- **Fields** (`Preview/Fields`): each input's value from, in order: what you set, what the
  file's folders give back through the generator's create paths (recognition's
  `invertPath`, keeping only folder-named inputs, never the file's own name), the cursor's ⚡
  action, a lone choice, a default. Each value is checked by the engine's own `resolveInput`, so
  the reasons match `olifant gen`.
- **Marks** (`Preview/Marks`): a missing name is planned as `zqmark_NAME`, and every rendering
  of it (each case, plural, singular) comes back as `⟨name⟩`/`⟨Name⟩`/`⟨NAME⟩`. A missing
  choice previews with its first option, missing text as `⟨name⟩`.
- **Steps** (`Server/Preview.previewFor`, `stepsPlanWith`): a picker sends steps, a `+ New`
  step first with `makes = {step, input}`. The new step's value comes from the served input's
  `new` template, rendered in that step's resolved environment, and fills the input it makes
  (source `new`, valid though not yet among the choices). All the steps plan as one plan.
  `olifant/preview` runs through the job queue keyed per document, so a newer request cancels
  one not yet started. The plugin ignores an older answer by its sequence number, which a
  change advances at once.
- **What it shows:** holes as `⟨name⟩` (from the plan's marked text); a missing choice planned
  with a stand-in never blames that stand-in ("choose entity to see what it writes"); a
  dependent choice's value its inputs no longer offer comes back cleared, with a note; `new`
  names full generator ids. The plan for real takes the values you set as you set them.

### Strangling (`Newness`, `Migration`, `Decide`)

The strangler ([user book chapter 17](../user/17-strangling-a-legacy-codebase.md)) is general
engine features plus a pack (`python-strangler`):

- **`only = "new"`** (`Rules.checkFiles`): each file's `NewLines` against the base, through the
  `newIn` function the caller gives (`App.newnessWith`: the base port, cached in the server). A
  rule's matches are dropped unless wholly new *before* their fixes' replacements are looked up:
  each replacement is a syntax query, and every old function in a legacy file paid one per
  keystroke (206 ms a pull in the demo's `views.py`; 21 ms after).
- **`{legacy}`** (`Rules.ruleCovers`, `App.generatorsOf`): `[migrate] legacy`'s globs, for a
  rule's `files` and a generator's `applies_at.path` (one copy per glob).
- **The report** (`App.migrationFor`): every legacy file's definitions now and at the base,
  the wholly new ones without an allowance (added), the registry now and at the base
  (extracted: switches the base lacks), markers it lacks. The registry is `[migrate] switches`,
  or a `switches.py` outside the legacy code. `check` prints its line; `olifant/model` carries it.
- **Decisions** (`Decide`; `Server/State.deciderFor`, from `[decide] model` or the `[recognize]`
  model): asked once per question in the background, the answer cached in `ssDecisions`; the
  deterministic answer stands meanwhile. Three kinds: `bug-or-new` (a big change in a legacy
  function, `Server/Fixes.decided`), `command-or-query` (when `readsOrWrites` is unsure,
  `Server/Actions.extractOrder`) and `service` (when the SQL's tables name no single service's
  model, `Server/Preview`'s `suggestService`; the value's source is `suggested`). Each goes
  through `Server/State.decision`: one decision per (file, kind, function name), at most one ask
  in flight (the answers and the asks in flight share `ssDecisions`, so knowing and claiming
  are one atomic step), asked again once it lands if the function's text changed since.
- **Rules that look elsewhere** (`Rules`: `bind`, `when`): a slot captured from the found code,
  and a condition on another file's lines; retire is offered only once the registry says the
  flag is on.
- **Fix data names its generator** (`Lsp`, a fix's `data.generator`), so the editor fills a
  fix's inputs in the generate picker and resolves the fix with them.

### The loop's blocks (`Block`, `Mutate`, `Server/Block`)

The incremental loop ([user book, chapter 18](../user/18-the-incremental-loop.md)) is a pure
machine served by the server. The person's "magic circle" is a `Block` internally:

- **`Block`** is the machine: `step move block` is `Left why` (one sentence for the person) or
  the block after the move; `open` lists the moves a state allows and `hud` writes its line (ending on the person's next
  step: `· draft`, `· name it, then draft · reject drops it` on a begin's test stub not yet named
  and `· draft · reject drops it` on a named one or a survivor's (until a model
  drafts), none after a model's failed draft, `red ✓ accept · go make it pass` on a new
  test red at its assertion (`green ✓ accept` on one that passes already, `blockPasses`: a
  regression test, accepted with nothing begun), `red at the assertion · draft again to redraft
  it, or edit it` on an `ExistingTest` block whose run fails at its assertion
  (`blockAtAssertion`), `· stub a test or waive`; another draft is `draft`, never `next`); `killTarget` is the survivor a kill takes (the cursor
  line's, or the first unwaived in the stored order; the person's `stub` on a survivor is `kill` internally).
  A property spec pins that `Accepted` is reachable only through a `Judged` whose survivors are
  all waived. `Block/Draft` turns a span into a template with one `block` hole (or a test's
  placeholder into a `stmt` hole, and its assertion into another while it is still the stub's
  `assert ...`, worded from the test's name and a kill's mutant), with the instruction from the test's name, its assertions and
  why it fails; `stubNameFor` names a kill's stub from its mutant (a string's words, else the
  operator and the mutated line's names, unique in the test file: `uniqueTestName`, which names a
  function's stub `test_<function>` too).
- **`Server/Block`** keeps each file's open circle (a `Block` internally) (`ssBlocks`, keyed by the module's path, found
  from its test file too), runs the moves and writes each to the step log (`appendStep`,
  `.olifant/state/steps.log`). `blockBegin` runs the test file (`runOver`: the runner over the
  editor's text), finds the function under test (`underTest`: each `.NAME(` call the test makes,
  its assertions first, until the module defines one, located by the language's `function NAME`
  target) and begins Red, or judges a green function at once; on a function no test calls,
  `blockStart` begins a `TestFor Nothing` block on the stub of its first test (`openTestStub`
  over `testStub`) and answers the stub for the editor, with the function's import as an edit to
  apply first when the test file lacks it (`addImports`, the fill phase's), and accepting that
  block begins the function on it: the block keeps that function (`blockSubject`: its name and
  def line), so `beginOnNewTest` begins there (`startFor`) rather than by the test's calls.
  `siteAt` finds a cursor's module, test file and def for both a begin and a stub, and
  `functionAt` the def at the cursor while its body reaches it (`functionSpanAfter` over
  `locateContainers`: the body after that def line, whatever else shares its name). Every
  move lands through `settle`, a compare-and-swap on the stored block's number and version
  (`blockVersion`, bumped by every move stored and by the decision model's reorder: a move begun on an older version
  is stale, logged and dropped; a test of the newest text, none begun since, settles again on the
  block as it stands, each time another move landed first), and a new block through `claim`, one atomic check-and-insert.
  `test` is refused while a draft is in flight, and `waive` and `accept` while a test of a text
  other than the verdict's has begun, or an edit of the span awaits its test (`awaitingVerdict`:
  `ssBlockSeen` against `blockOn`, and `spanEdited`); a draft that fails puts back the state it began
  from, testing the file again when it is no longer the text that state was established on
  (`blockOn`). `blockDraft` stores and pushes `Drafting`, makes the span a hole and plans it
  with `planWith` under `DraftStance` (the test file, or for a test the module, in its context,
  then the project modules either file imports and, one level further, what those import:
  `importedContext`, capped at 8 files of 120 lines, 600 in all, direct imports first),
  verified by `VerifyKeep` (the block's test file runs whole, never a kept id: one gone from
  the file would stop pytest's run; its test must pass and each test it keeps that the run has,
  `keptVerdict`, a sibling red otherwise passed over; `blockKeep` is the test file's tests that
  passed at begin and a kill's tests once accepted (`rejudgeFor`), and a kept test a test run
  does not have is gone, dropped from it, `ranIn`; the `test` move and an edit's test run the
  file the same way); a kept test failing while its own passes
  is the block's red, `outcomeOf`, landed through `AtKept` so `blockRedAt` names it and the HUD
  says `no longer passes`; a failed draft whose fill failed its tests (`testsFailed`) is run once
  more to find a kept red, `draftRedOn`, and the next draft's instruction names that test), `VerifyRedAt` (a new test fails at its assertion,
  or passes: a regression test, unless its function's body is still only a `raise NotImplementedError`, `bodyRaises`)
  or `VerifyKills` (what passed before and the new test pass, and the new test fails on the
  mutant); an unfilled hole writes the model's last attempt as a red draft (as the fill
  phase read it: a `Reindented` fill as read), unless, in Python, it would not parse
  (`unparseable`: `ast.parse` through the verify port, `python3 -I`, a syntax error printed
  after `parseSentinel`) while the file without it does: then the draft is recorded red with
  Python's reason (`unparsedDraft`) and nothing is written. Any other outcome (no python3, a
  traceback, the file unparseable before) turns the gate off for that draft, logged as a
  `parse` step. Its edits are
  versioned, so the editor applies them only to the buffer it planned on. `judgeIn` runs the
  test file once unmutated (the baseline: its passing tests are the judges, its wall time sets
  each mutant's timeout, `judgeTimeout`, through `pTesterWithin`), mutates the span's lines new
  since `blockBase` (`newLinesOf`) with the language's operators, caps them at 20 and runs them
  through `Mutate.judge`: each mutant's file over the project in the runner's own temp tree,
  four at a time.
- **`blockDraftAt`** is `draft` at a cursor: the file's circle (`blockDraft`), or, with none, on
  a red test, an `ExistingTest` block (the third `Kind`: the code is right, the test is drafted)
  on the test's lines (`testAt`: a method's id names its class), keeping the file's other passing
  tests and claiming its module too; a test `existingParts` cannot shape (no assert line, a def
  over several lines) is refused before anything opens. It drafts at once: `existingTestTemplate`
  makes the arrangement (from the def, or a leading docstring, to the body's statement holding
  the first assert, a `with` around it the assertion's) a `stmt` hole, the assertion left as it is, verified by `VerifyKeep`
  as a code block's draft is, the test as it was in its context; a red `draftRedOn` runs again
  for the runner's line, and red at the assertion (`redAtTheAssertion`) sets
  `blockAtAssertion`, so the next `draft` turns the kind to `ExistingTest True` and drafts the
  assertion too (`existingAssertionInstruction`), its line carried as `blockAssertion`.
  Accepted, it closes (`green: test drafted`); nothing moves to the module. Rejected, only the
  test's lines go back (`blockReject` splices them into the file as it is).
- **`Server/BlockMoves`**: `blockKill` (a `TestFor` block in the test file for the `killTarget`
  mutant, its test id the stub's name until a test of the block finds the person's: `testNow`
  makes a test block's test its first new one, and the stub the editor expands), `blockStub`
  (the editor's `stub`: `blockKill` in a judged code circle with a survivor left; with no circle
  in the module, the cursor in one of its functions (`functionAt`), a new test's stub named
  `test_<function>` through `openTestStub`; any other open circle refused with a sentence for its
  state, and a test file sent to its module), `blockWaive` (a comment per operator on the line, as one edit) and `blockChanged`, the
  debounced reaction to an edit of a block's file: the draft the editor applied (`Applied`) or the
  person's edit inside the span (`Edited`) is tested and, green, judged; an edit elsewhere does
  nothing; a test block none of whose new lines (`newLinesOf`) is left non-blank from its span
  on (its stub deleted whole) is rejected through `settle` (`rejectDeleted`, again on the block
  as it stands if another move landed first) with no restoring edit, and pushed as gone. `ssBlockSeen` holds the text last tested, so the editor applying the server's own
  edit (a waiver) is no edit.
- **`Server/BlockDecide`** orders a judged block's survivors with two `Decider` questions per mutant
  (`mutant-equivalent`, `mutant-matters`), asked in the background through the server's
  decisions; each answer that lands reorders and pushes the block (`ssOnBlock`).
- **`Server/Tests`** runs a file's tests whole, outside any block (`olifant/tests/run`):
  `testsRun` takes a module to its test file (`testsForFile`) and a test file to itself, and runs
  that file as one id with `Server/Block.runOver` over the snapshot's documents, timed; a test file
  whose module is gone (`testShaped`: named as `tests_for` names test files) runs itself; a runner
  that cannot start is a broken run, and `Lsp` answers a run the editor cancelled `RequestCancelled`. `testsJson` is the answer the editor reads.
  `testsFile` (`olifant/tests/file`, answered at once: it reads only config and the file list)
  answers a file's other side for the editor's jump: a module's test file, which may not exist
  yet, or the module a test file tests; both read one `placeOf` (language, `[test]` table, the
  module it is the test file of), so the two never disagree on which file is the test file.
- **`Lsp`** has one `blockRequest` per move, in the background like the other slow requests; every
  block a move changed is pushed as `olifant/block`, and `didChange` that edits a block's span
  (`blockEdited`, against the text last tested) submits `blockLater` (the debounce; a newer such
  edit cancels it, an edit elsewhere in the file leaves the running test or judge to land).

The specs drive it with a scripted runner (`scriptedRunner`, `scriptFileRunner`: verdicts by
test id, a file's id answering each test the file defines by its id, as pytest does; or by the
files' text) and a syntax that knows the one container and site they need;
`packs/python-htmx/tests/mutate/` judges the real operators against the pack's fixture.

### Testing the server

`LspSpec` drives the real server in-process over pipes with `lsp-test`, on fixture projects
copied to temp folders (`withProjects`: several roots, several packs), so every method is tested
as an editor calls it. `ServerSpec` tests the state functions directly. durandal.nvim's own
suite runs against a fake server with canned answers (`tests/fake-lsp`), and its e2e suites
against the real one.

## Rough spots

This section is where things worth knowing about, but not urgent, go when you work near
them. Step 2c (compiler support, above) closed the one that used to be here, by moving the
Elm-specific behaviour behind `CompilerSupport`. Holes brought these:

- **A compile failure is a broken file, not a line.** `reportBroken` gives `(file, message)`,
  so the fill phase can only blame a fill by checking it alone (4f). That costs one more
  check per candidate whenever anything breaks.
- **Elm's reason is a title.** `parseElmReport` keeps the error's title (`NAMING ERROR`,
  `TYPE MISMATCH`), and that is all a retry tells the model. In measured runs, a model told
  only "TYPE MISMATCH" repeated its fill on both retries.
- **`alreadyPresent` with holes is loose.** It asks only that the literal pieces around the
  holes appear in order in the region, so a short piece (`,`) matches easily.
- **The complete loop's checks name placeholders by their bare name.** `placeholderize` runs
  before the final names exist. That's harmless for a compiler, but it is why `showHoles`
  and the final placeholders can differ for a repeated name.

The editor features brought these:

- **The parse pre-check sees only `ERROR` nodes.** Tree-sitter repairs some mistakes by
  inserting a `MISSING` node instead (a `let` without its `;`, a struct without its closing
  `}` at the end of the file), and `{ kind = "ERROR" }` matches none of them, so those files
  pass the pre-check (4g) and only the compiler finds them.
- **`nearby` doesn't validate choices.** `Near` skips `offer`, so a path slot that is not a
  valid choice is used as is: in rust-htmx, `lib.rs` matches `{command}.rs`, and
  `--file billing/billing-service/src/lib.rs` reports `wire-command` "on a field of the Lib
  struct", though `lib` is not a command and `--at` on such a field would drop it into
  `missing`.

Suggestions brought these:

- **`actions --content -` and `gen --overlay` see different projects.** `actions` matches
  the buffer's text, but resolves `one_of` choices against the disk (`offer` calls
  `withChoices` without the overlay), while `gen --overlay` lists the overlaid file. A
  command file that has never been saved has `command` in `missing` from `actions`, so the
  editor never suggests on it, though `gen --plan --overlay` accepts `command=pay_invoice`.
- **One overlay, one stdin.** Only one file can be overlaid, and it is read from stdin, which
  `--steps -` also reads. Since `Main` reads stdin once and shares it, the two together no
  longer crash: both get the same text, so the steps parse and the overlaid file is the JSON
  (`Plan: rust-htmx/wire-command (0 edits, 0 conflicts)`). It should be a usage error.

Snippets brought these:

- **A trigger sees only the text before the cursor.** `placeCursor` replaces the whole line,
  so a whitespace trigger (`^[[:space:]]*$`) at the start of a line that has code offers a
  snippet that would replace that code. Nothing checks the rest of the line.
- **`--at` is not range-checked.** A `LINE` past the end of the file "replaces 0 lines" and
  the snippet lands at the end.
- **`withFills` doesn't touch `WillReplace`.** `--plan`'s edit text (and JSON `content`) for a
  snippet or a fix shows a model hole's placeholder, though `files`, `snippet` and `holes`
  have the fill.
- **The source is in the kind.** The human hole line prints `text@yours`; JSON splits it. An
  adapter kind with an `@` in its name would be misread.
- **`--no-fill` overwrites a tab stop's `why`** with `--no-fill`, since `noFill` marks every
  hole skipped.
- **`olifant schema` doesn't know snippets.** The `edits` items' `oneOf` has no cursor edit and
  `holes` is described as `{}`, so a schema-aware editor flags a valid snippet.
- **An `uncovered` enum's choices exist only at a cursor**, so `show` prints `<enum()>`, and an
  `actions --at` in a complete `match` still lists `match-arm` with `"choices":{"pattern":[]}`.

Python brought these:

- **A missing return is forgiven in a file with a missing case.** `parsePyrightReport`
  treats "must return value on all code paths" as the missing case's consequence whenever
  the file has a `reportMatchNotExhaustive` site, so a genuine missing return in the same
  file is not a broken file to Olifant (pyright still reports it).
- **No visibility rule.** `supportVisible` is always `True`, so a class could be offered at
  a `match` on another union with a class of the same name. Sites come from pyright's own
  report, so in practice they are real.
- **A one-line union has no target.** `union NAME` wants the parenthesized, one-per-line
  form; `type F = A | B` misses.
- **One arm, then a blank line.** `insertBranch` copies the spacing between a `match`'s last
  two arms; with only one, a language with no arm separator gets a blank line, Elm's style.
  Python has none, so a `match` with one `case` gets a blank line before the new arm (and
  the plan still says `+2 lines`).
- **`compileClean` has nothing to show for pyright.** When pyright fails with no broken file
  (only a missing case), the message falls back to stderr, which pyright leaves empty, so
  "output type-checks" fails with a blank reason.
- **`after` without ast-grep is silent.** `afterIndent` treats a failed search as "follows
  nothing", so without ast-grep a snippet anticipated `after` a node is indented by
  `cursorIndent`, under the class's field, instead of reporting the missing tool: the
  `failure-variant` case fails rather than going pending. And `indentTemplate` indents a
  snippet's first line even when it is empty, so after an indented node the snippet's
  leading blank line would carry the indentation.
- **`stmt` and `block` fills are barely parse-checked.** Their `wrap` is
  `def __olifant():\n{= fill =}`, and tree-sitter accepts an unindented body there (an empty
  `block`, no `ERROR`), so a fill's indentation is checked only by pyright, with the plan.
- **"placeholder output compiles" fails without its compiler.** It plans first and checks for
  the tool after, and with no compiler a `complete` edit finds no site and an `uncovered`
  query no case, so "at least one hole is unfilled" fails, for every pack. The real-pyright
  spec in `LanguageSpec` is the opposite: it uses `pendingWith`, so `OLIFANT_REQUIRE_TOOLS=1`
  doesn't make it required.
- **The `after`-and-`node` error says `applies_at` twice**: `` `applies_at`: `applies_at`
  takes `after` or `node`, not both … ``.

The conventions brought these:

- **The baseline is a set.** An entry is `(rule, file, fingerprint)`, and two findings with
  the same matched text in the same file have the same fingerprint, so one entry covers
  both: a second `use diesel::prelude::*;` pasted into a baselined file is not new.
- **A `missing` finding doesn't say what is missing.** The path is in its fingerprint, not
  its message, so the message has to name the convention well enough ("Every command has a
  test file").
- **Rules see the disk's file list.** `check --file --content -` checks the buffer's text,
  but a `missing` rule still asks whether the other path exists on disk.

## Where to go next

- [Chapter 2](02-haskell-for-this-codebase.md) covers the Haskell in the excerpts above,
  with Rust comparisons.
- To change a unit, start with its `XSpec.hs`: it's the unit's documentation by example,
  and it's where the change starts (tests first).
- `cabal test --test-options='--match "Olifant.Generation"'` runs one unit's specs, and
  `scripts/test-watch.sh` re-runs on save.
