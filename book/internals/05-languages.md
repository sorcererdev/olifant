# 5. How a language is described

Olifant's engine knows no language. Python, Rust, Haskell and Elm are TOML files, and the code
that finds a unit, runs a test, mutates a line or conjures a body asks the language file how.
This chapter is the map of that data: where it comes from, how the layers merge, which key is
read by which code, and the rule (and the specs) that keep language names out of the engine.
The user-side reference is [chapter 9 of the user book](../user/09-packs-and-language-adapters.md);
this one is for the person adding a key or a language.

Start from what you can run:

```sh
scripts/try-pack.sh python-htmx /tmp/bistro && cd /tmp/bistro
olifant doctor
```

```
✓ language python — loaded (shipped, pack python-htmx)
✓ test runner uv (python) — /opt/homebrew/bin/uv; uv.lock found; tests_for = {= dir =}/{= stem =}__test.py
✓ python: units — [unit] kinds (ast-grep), named by [unit] name · from shipped
✓ python: diagnostics — [check] format pyright-json · from toolchain pyright
✓ python: tests — [test] format pytest, tests_for {= dir =}/{= stem =}__test.py · from toolchain pytest
✓ python: test selection — [test] select · from toolchain pytest
✓ python: mutation — [[mutate]] (17 operators) · from shipped
✓ python: conjure shapes — [conjure] port, port.add, fake, constructor, advice · from pack python-htmx
…
```

Each row is one `Capability` (`Capabilities.hs:24`) and its origin: the layer that last set the
key (`toolchain pytest` when only the preset supplied it). In a project with Rust, a row the
language lacks reads `! rust: mutation — absent` with `fix: add [[mutate]] to
.olifant/languages/rust.toml` (that is a warning, never an error). Everything below explains where those words come from.

## Three kinds of file, three layers

There are three kinds of TOML file, and they stack.

- A **language** (`library/languages/<name>.toml`, embedded in the binary): what files it covers,
  its grammar, how a unit and a test look, how it imports, how to break its code, and which
  toolchain checks and tests it. Four ship: `elm`, `haskell`, `python`, `rust`.
- A **toolchain** (`library/toolchains/<name>.toml`): one `[check]` or `[test]` table, the
  plumbing of running a tool: its command, its cache, the report format, how to install it.
  Six ship: `cabal-repl`, `cargo`, `pyright` (checks), `cargo-test`, `hspec`, `pytest` (tests).
  A language names one with `preset = "pytest"`.
- A **pack** (`packs/<name>/languages/<name>.toml`, `.../toolchains/…`) and the **project**
  (`.olifant/languages/…`, `.olifant/toolchains/…`): what one convention changes. python-htmx's
  `languages/python.toml` is the `tests_for` and the whole `[conjure]` table, over the shipped
  Python.

A language is the merge of its **layers in order, shipped ← pack ← project**: the shipped file,
each configured pack's file of that name in pack order, the project's file. The embedding is
`Library.hs` (`shippedLanguages`, `shippedToolchains`, via `embedDir`; touch the module after
adding a file, as its header says).

### Merging

`languageFromLayers` (`Language/Adapter.hs:431`) takes `[Layer]`, where `Layer` (`:407`) is
`{layerOrigin, layerDir, layerTable}` and the origin is `"shipped"`, `"pack NAME"` or `"project"`.
It folds the tables with `Olifant.Toml.mergeTables` (`Toml.hs:41`): where both sides hold a table
the two merge key by key; **anything else is replaced whole**, lists and arrays of tables
(`[[mutate]]`, `[[imports.resolve]]`) among them. A pack that writes `[test]
tests_for = "…"` keeps the shipped `[test] preset`; a pack that writes `[[mutate]]` replaces all
of Python's operators.

Then the one preset the merged `[check]` and `[test]` name fills what is still missing
(`applyPreset`, `Presets.hs:47`, called from `testFrom` and `checkFrom` in `Adapter.hs`): a
left-biased union of the table with the preset's, so a key you wrote wins. One rule keeps old
full files whole: `dropPresets` (in `languageFromLayers`) removes the inherited `preset` when a
later layer writes its own `command` or `convert` and names no preset.

`Presets` themselves layer the same way. `Catalog.openAt` (`:396`) folds
`overPresets shippedPresets` with each pack's `toolchains/*.toml` (`toolchainsIn`, `:458`), then
the project's; `overPresets` merges key by key as `mergeTables` does.

### Origins

While merging, `languageFromLayers` also builds `langOrigins :: Map Text Text`: each top-level
key and each key one level down (`check`, `check.format`) with the layer that set it last, and for
a `[check]`/`[test]` key that only the preset supplied, `toolchain NAME`. `originOf lang key`
(`:461`) looks a dotted key up, falling back to its parent. Doctor prints it after each
capability ("· from pack python-htmx"), and `langLayers` is the list of layers it names.

### Which languages a project has

`Catalog.openAt` (`Catalog.hs:396`) collects the layers of every pack (`lpLanguages`) and the
project (`layersIn`, `:443`), plus the names a generator asks for (`genLanguage`) and the
config's `languages = [...]` list. `assembleLanguages` (`:423`) builds one `Language` per name,
the shipped layer first if the name ships (`shippedLayer`, `Adapter.hs:411`).
A name that nothing provides is skipped. A language that does not parse is set aside in
`catalogBrokenLanguages` with the path of the **first layer in its stack that makes the merge
fail** (so a bad key in the pack is blamed on the pack, not on the project file after it); one
only a generator names (elm without the elm pack's grammar) is left out quietly. A file that is
not valid TOML sets its whole language aside under its own path.

`languageFor` (`Adapter.hs:792`) picks the language of a file: the first of the project's
languages, in the order they were first named, whose `files` globs match. `olifant-dev` (this repository's own pack) carries no language
file; it gets Haskell from the shipped one.

## The keys, and who reads them

`languageFrom` (`Adapter.hs:~470`) parses the merged table and starts with
`onlyKeysWith [...]`, a list that is the closed set of top-level keys: `files, comment,
grammar, targets, placeholder, branch, check, indent, holes, imports, import_merge, separator,
line_length, keywords, test, unit, mutate, arm_separator, lone_arm_spaced, unit_words,
opens_block, empty_statement, stub_lines, conjure, types`. An unknown key is a load error naming
it. Add a key there first.

The seven tables you will touch most, with the code that reads each field:

### `[unit]` and `unit_words`: what a definition is

`langUnitKinds` (`kinds`), `langUnitNames` (`name`), `langUnitJoin` (`join`).
`Server/Unit.hs:unitAt` (`:53`) finds the innermost ast-grep node of those kinds around a line;
`unitsOf` (`:154`) lists every unit in a file, naming each by the first of `name`'s POSIX
expressions that matches (its last group), `join` folding a Haskell signature and its equations
into one. This is what conjure, mutate, stub, plan anchors and the plan fingerprint stand on.

Both paths meet in `unitAnswers :: Syntax IO -> Language -> Maybe FilePath -> Text -> IO
(Either Text [UnitAnswer])`, the units slot. By rule it gives each `[unit] kinds` match as a
`UnitAnswer` (its 1-based span, its name by `[unit] name`); with `langUnitProvider` it gives the
program's answer (`Provider.providedUnits`, memoised by request). With neither (no `[unit]
kinds`, no provider) and a linked grammar, it gives every function and class-like definition the
grammar's queries capture (`TreeSitter.definitionLines`, over `Captures.definitionSpans`): named
by its `@name`, its span the definition's, widened to the largest `function.outer`/`class.outer`
capture that ends with it and holds no other definition (a Python decorated def, decorators
in), a Haskell signature and its equations already one (`adjacent`). Units read
`Captures.unitDefinitions`: a phrase's definitions plus `@unit.*` captures no phrase reads
(Rust's `impls.scm` tags a trait impl `@unit.impl`, named by its type, so its methods stay
`Type.method`), each named by its match's `@name` and `@unit.name` texts in order.
`Unit.unitLookup` reports a name several units share instead of taking the first; a plan
anchor reads `unitLookupIn` over `unitsWithKinds`, where a type and its inherent impl
(`definition.impl`) sharing a name are the type, and says why when it does not resolve
(`Resolved.rsWhy`). Rust, Python and Haskell
ship no `kinds` any more; they keep `name`, which `defLine`, `ownerHeader` and the other line
readers (`App.definesIn`, `ConjureScan`, `Recognize`, `functionText`, `FillPhase`'s top-level
names) still use. `hasUnits` (in `Olifant.Language`) is `hasUnitRule` or a linked grammar.
`unitsOf` sorts either by
start (longer first) and folds it the same way: `defLine` (the first line `name` names, else the
first), `dedupe`, `joined`, `qualify`. `unitAt` keeps its rule path; with a provider, or with the
queries' answers, it takes the innermost answer holding the line, its body from `bodyOf` as
before. `FillPhase`'s top-level
names (the answers no other answer holds) read a provider through `providerPure`, since the fill
phase is generic in its monad.
`unit_words` (`langUnitWords`) is a sentence for prompts (`Errand.projectUnitWords`).

### `[test]`: running tests and finding them

`[test]` is two things in one table. The **runner** keys (`command`, `context`, `format`, `root`,
`tests_for`, `exclusive`, `timeout`, `install`, `[test.select]`, `[test.lines]`, `file_group`,
`convert`) make a `TestDef`. They are read by `Language/Check.hs`: `processRunner` (`:142`) runs
a scratch copy of the project's files (`scratchRun`, `:107`), `readTestReport` (`:210`) picks
the parser (`parsePytest`, or `TestFormats.parseLines | parseTap | parseJunit | parseTestJson`),
`selected` (`:199`) turns a test id into arguments with `[test.select]`, and `runTestsIn`
(`:476`) is the entry the server uses. A run knows its folder on disk, `TestDef.testDir`: the
catalog sets it to the project root as it roots the providers (`Adapter.withProviderRoot`), and
`runTestsIn` narrows it to the package its `root` markers find; `processRunner` passes it to
`scratchRun` as `{= root =}` (made absolute), expanded in the program too, so a command can
name a venv in the project although it runs in a scratch copy. `processRunner` also hands
`scratchRun` that folder as the rest to copy: `copyRest` lists it with the workspace's own
listing (`Workspace.diskWorkspace` `listFilesSkipping`, the language's `ignore` skipped), and
clones (`cbits/olifant_clone.c`, APFS `clonefile`) or copies each file the run's map lacks, so
templates and binary fixtures come byte for byte while the map's (edited, mutated) texts win. A
check or a `verify` passes no folder and sees only its map. A `command` that is a list of
`{ when, command, context }` tables parses into `testCandidates` (`CommandCandidate`, the
parser's `commandFrom`), with `testCommand` the unconditional one's until a choice;
`Adapter.chooseCommand` takes an existence test and the candidates and returns the first fitting
one and why (`uv.lock found`), and `chooseTestCommand` the `TestDef` with its command and context. It is called where the folder is known: `processRunner` (a `when`
file among the run's files or under `testDir`), the doctor's runner row (`Doctor.doctor`'s
`testRunner`, through the `dpExists` port, from the project root) and `Server/Plans.projectTests` (a `do`'s printed command, `{= root =}`
filled). `checkScope` copies every candidate's `context`. The pytest preset is data only: its
four candidates live in `library/toolchains/pytest.toml`. `tests_for` is read by `Server/Tests.hs`
(`testsForFile`, `placeOf`) and `Language.Test.testsForFile`.

The **shape** keys are the language's own, parsed even when there is no runner (a table holding
only shapes makes no `TestDef`: `testShapeKeys`): `unit` (rules: `langTest … testUnitRules`,
with `after` and `above`, or `testUnitProvider`) is read by `Language/TestUnits.hs` for tests and groups, and by
`Server/Unit.testUnitAt`; `assertion` (`langAssertion`) by conjure (red at the assertion),
`Server/Order.hs` and `FillPhase` (the split of a test's two holes); `name`
(`testNaming`) by `Server/Stub.hs`; `[test.stub]` (`testStub`, `StubDef`) by `Stub.stubWith`;
`[test.failure]` (`FailureDef`) by `Language.Test.failureLineWith`/`gotExpectedWith`;
`undefined` (`testUndefined`) by `ConjureNext.undefinedIn`.

Every reader of test units goes through one door, `TestUnits.testUnitsOf :: Language -> Text
-> [TestUnit]` (with `testUnitsReport`, which keeps a provider's failure for doctor; `testAtIn`
and `testLineIn` are built on it). By rule it is `testUnitsIn` over `[test] unit` and `above`;
with `testUnitProvider` it is the program's answer, each unit's path the names of the answers
that strictly hold it, outermost first. With a linked grammar, `testUnitsReport` calls
`testUnitsWith` with a span function (from `TreeSitter.definitionLines`): a unit whose rule's
line starts a definition ends where that definition ends, so a test with a string's outdented
line, a Rust test's closing brace and a multi-line signature keep their whole span (m3's known
limits). A unit on a line that starts no definition (hspec's `it`) keeps the outdent rule, as
does every unit of an unlinked grammar. It also passes the grammar's class definitions (capture
`definition.class`, from `TreeSitter.definitionKinds`): a class holding a unit is a group of it,
whatever its name, unless a rule matched its line; a class holding none is no unit. So a pytest
method's id is `ElectionTests::test_x` for a Django `TestCase` subclass, as for `class TestX`,
with no class rule; Rust's structs and Haskell's type classes hold no tests, so their ids are
unchanged. The callers (stubs, the fill phase's assertion span and
whole-file test, the scripted runner, conjure through `testCaseAtIn`/`testSourcesIn`) are pure,
so a provider runs through `Provider.providerPure`, memoised by request; that rests on a
provider being pure by contract. `TestUnitsSpec` fails if `testUnitsIn` appears in any other
non-spec module.

`format` is the one place a name is code: `knownTestFormats` (`Language/Support.hs`) lists
`pytest, lines, tap, junit-xml, olifant-json`, and `readTestReport` dispatches on it. A format
is a *parser*; a language is never a branch.

### `[imports]`, `[import_merge]`

`langImports` (the `line` regexp) and `langImportMerge` are read by the fill phase
(`Generation/FillPhase.hs`, `Generation.hs`): a fill may begin with import lines, which join the
file's imports. Its auto-import (`importFor`) finds the one project line binding a name: one
importing names is rewritten for that name by `[import_merge] write`, one importing a module
whole is read by an `[[imports.resolve]]` without `names` (`regexGroup`). `Server/Anticipate.prefixAt`
takes `[imports] line` for the file's import lines it shows the decider. `langImportsDef`
(`ImportsDef`: `write`, `module_of`, `[[imports.resolve]]`, `paths`, `example`) is read by
`Server/Site.hs` (`importOf`, `importedContext`: which project files an import names, a
conjure's context), `Server/Stub.hs` (a stub's import line), `Language/Check.hs` (`moduleOf` for
a test file's group when `file_group` is absent) and the fill phase: `example` (`imExample`, a
name and a fill) becomes `FillHole`'s `fhImportExample`, the example `Model.userPrompt` shows
under its imports rule (none: the rule alone).
`moduleOf` (`Adapter.hs`) is the pure path-to-module function.

### `[check]`: the compiler's report

`langCheck` (`CheckDef`) is run by `processChecker` (`Check.hs:218`) and read through
`runCheckIn` (`:437`); its `format` selects a `CompilerSupport` (`Language/Support.hs`:
`elmSupport`, `ghcSupport`, `pyrightSupport`, `rustcSupport`, `sarifSupport`,
`olifantJsonSupport`, or the user's `errorformat`). A **support** is the code that reads one
tool's report, in `Language/{Elm,Ghc,Pyright,Rustc,Generic}.hs`, and `supportsMissing` is the
list of formats that can also say which `case` lacks which constructor, which is what
`complete` and `uncovered` need. `convert` runs a command that turns any tool's output into
olifant-json (`convertReport`, `Check.hs:254`). `undefined` and `missing_member` are the check's
words for a name or a member that is missing (`ConjureNext.undefinedIn`,
`ConjureScan.missingMemberIn`). `checkProgram` (`:290`) names the program doctor looks for.
A check knows its folder on disk as a test run does: `CheckDef.checkDir`, set by
`withProviderRoot` and narrowed to the package by `runCheckIn`. `runIn` makes it absolute,
picks the command with `Adapter.chooseCheckCommand` (the same `chooseCommand` over
`checkCandidates`, a `when` file among the files or under `checkDir`, `existsFor`) and fills
`{= root =}` with `{= cache =}` and `{= context =}` through `fillVars`, the helper `scratchRun`
uses, before `runChosenCheck` runs it. The doctor's `check tool` and `test runner` rows are one
`commandRow` over the chosen candidate (`Doctor.hs`).

### `[[mutate]]`: ways to break the code

`langMutate` is a list of `MutateDef {mutName, mutRule, mutFix}`; `Mutate.operatorsOf`
(`Mutate.hs:27`) turns each into an `Operator` (an ast-grep rule plus a fix or `transform =
"increment"`), `mutantsOf` (`:36`) finds the mutants in a unit's lines, and `Server/Mutate.hs`
runs, caps (`capMutants 20`) and orders them. Only `python.toml` has any (17 operators).

### `[types]`, `[conjure]`

`langTypes` (`TypesDef`: `string, integer, nothing, optional, param, annotation, returns,
assigned, checks_none, record, method`) and `langConjure` (`ConjureDef`: `service`, `port`
with its `add` and `adapters`, `fake`, `constructor`, `advice`) belong to the pack and are read
only by conjure, in `Server/Conjure.hs` and `Server/ConjureScan.hs`: [chapter 4](04-conjure.md#the-conjure-shapes-and-the-floor)
has the table of which field feeds which function. A language without them conjures at the
floor. What a pack *does* there is named here too: `[conjure.port] add` and `adapters` are the
ids of the pack's generators that add a method a port lacks, so the engine names no generator
of its own. The `conjure shapes` row lists `port.add` and `advice` when present, and says
"(no [conjure.port] add: a method a port lacks is yours to add)" for a port without `add`.

### The rest

| key | read by |
|---|---|
| `files`, `comment`, `grammar` | `languageFor`; region fences (`Region.hs`); `AstGrep.hs` (a built-in name, or a vendored grammar compiled once with `cc`) |
| `[targets.*]` | `Anchor/Items.hs`, `Anchor/Resolve.hs`: the `at` phrases (`targetPhrase`) |
| `placeholder`, `branch`, `arm_separator`, `lone_arm_spaced` | `Generation/Complete.hs` (`applyBranches`): the todo arm a `complete` edit writes and how it joins the others |
| `[holes.KIND]` | `Holes.hs`, `Generation/FillPhase.hs`: a hole's placeholder and the code a fill must parse in |
| `indent`, `separator`, `line_length` | opening empty containers and joining items |
| `keywords` | `Server/Recognize.hs` (recognition keeps them), `Server/Stub.hs` |
| `[unit] name` | besides conjure's units: `App.isDefinition` and `callersOf` (a fix that rewrites a definition names its callers, not lines that define it), `Migration.functionText` (a legacy function's text, for extracting), `Server/Recognize`'s declaration line (with `[test] above`) |
| `[test] name` | `Server/Stub.testAffixesIn`: the text before and after the words, which a survivor's stub name (`TestName.stubNameFor`) is put between; uniqueness is against the file's tests by `[test] unit` |
| `opens_block`, `empty_statement`, `stub_lines` | `Server/Unit.hs` (`bodyOf`, `isStubIn`), `FillPhase` (cutting a whole-file answer back to a hole) |

## Queries: tree-sitter in process

Besides the ast-grep shell-out (`Language/AstGrep.hs`, which keeps `[[mutate]]` and the
declared `[targets.*]`), olifant parses in process with tree-sitter, to read the generic
anchor phrases from community queries (design 4b §A).

- **The C.** The tree-sitter runtime 0.27.0 (its one-file amalgamation, `cbits/tree-sitter/src/lib.c`)
  and each linked grammar (`cbits/tree-sitter-rust/parser.c`, `scanner.c`; the same in
  `cbits/tree-sitter-python/` and `cbits/tree-sitter-haskell/`) are `c-sources` in
  `olifant.cabal`, in both the `library` and the `spec` stanzas. Nothing is installed: no
  Homebrew library, no dylib, no doctor row for it. Both are pinned to the versions ast-grep
  0.45.3's `Cargo.lock` locks (runtime 0.27.0, tree-sitter-rust 0.24.2, tree-sitter-python
  0.25.0, tree-sitter-haskell 0.23.1), so the two parsers
  read the same trees. Each directory has a `SOURCES` (version, git commit, crate sha256) and
  the upstream `LICENSE`.
- **The shim.** GHC's FFI cannot pass a `TSNode` (a 32-byte struct) by value, so
  `cbits/olifant_ts.c` wraps the calls olifant needs and holds nodes by pointer
  (`ots_parse`, `ots_facts`, `ots_field`, `ots_next` and `ots_captures`, `ots_prune`, …). The grammar is passed
  in: `TreeSitter.hs`'s `grammarTable` maps a language file's `grammar` to the compiled-in
  `tree_sitter_<grammar>` function. That table is data keyed by grammar, never by a language's
  name; `linkedGrammars` lists it.
- **The queries.** `library/queries/<grammar>/*.scm`, embedded with `embedDir`
  (`shippedQueries`, `queriesFor`). For Rust: `tags.scm` (the grammar's own, MIT: the
  `@definition.*` and `@name` captures), `textobjects.scm` (nvim-treesitter-textobjects,
  Apache-2.0: `@function.inner`, `@class.inner`, `@parameter.inner`, `@call.outer`) and two
  olifant writes: `imports.scm` (an import statement, whole, `@import`) and `impls.scm` (an
  inherent impl named by its type, generics included, as `@definition.impl`; a trait impl is
  not captured, so no phrase names it). For Python: the grammar's `tags.scm`,
  nvim-treesitter-textobjects' `textobjects.scm` less its two comment patterns (they use
  `#offset!` and `#lua-match?`), and olifant's `imports.scm` (`import`, `from … import` and
  `from __future__ import`). For Haskell all three files are olifant's own, because
  tree-sitter-haskell ships no `tags.scm` and nvim-treesitter-textobjects' Haskell file has no
  class bodies: `tags.scm` tags a `signature`, `function` and `bind` as `@definition.function`,
  `data_type` and `newtype` as `.type`, `class` as `.class`, and `instance` as
  `@definition.instance` (named by its class for a phrase, `@unit.name` adding its type for its
  unit: `C Int`);
  `textobjects.scm` makes a data type's constructors and a class's or instance's declarations
  `@class.inner`; `imports.scm` captures each `import` in the `imports` block (the block itself
  runs on to the next declaration under the grammar's layout). They carry no upstream
  licence. The place to upstream them is tree-sitter-haskell's `queries/tags.scm` (and
  nvim-treesitter-textobjects for the inner ranges), after which they would be vendored like
  Rust's. Haskell's `parser.c` is 19.8 MB; it compiles clean at `-O2` in under a second. `SOURCES` pins each vendored file to a commit and lists any line
  olifant removed under "changes".
- **The project's queries.** `Catalog.openAt` reads `.olifant/queries/<grammar>/*.scm` for
  each language whose grammar is linked (`projectQueries`) into `langQueries` (file, text,
  origin "project"). `TreeSitter.queriesOf` is the set every reader uses: the shipped files,
  a project file replacing the one of its name, new names added, in name order; `querySet`
  drops the origins for `capturesOf`, whose cache key already holds the whole set. The
  generic rule (`treeSitterAnchors`), units and test spans (`definitionLines`), doctor's tries
  (`genericNames`) and the `queries` row all read it. A set that does not compile is a
  `catalogBrokenLanguages` entry naming the file, and the language keeps its shipped set.
- **Predicates.** The C cursor returns a match without judging its predicates;
  `TreeSitter.patternPredicates` reads each pattern's (`ts_query_predicates_for_pattern`) when
  the query set compiles, and `passes` drops a match that fails them. Six are known
  (`knownPredicates`): `#eq?`, `#not-eq?` (against a string or another capture), `#match?`,
  `#not-match?` (a search, POSIX extended, the engine of the language files' expressions) and
  `#any-of?`, `#not-any-of?`. Every node a quantified capture holds must pass. Anything else,
  `#make-range!` (nvim's `@*.outer` ranges, which olifant does not read), `#set!`,
  `#lua-match?`, fails the file's compile with "unknown predicate", naming the file: a vendored
  file that uses one has those patterns removed and listed in its `SOURCES`, and the spec
  "every shipped query compiles and uses only known predicates" holds every linked grammar to
  it. A capture a predicate reads is kept in the pruned query even when the rule does not read
  it, and is not returned.
- **The capture data.** `capturesOf grammar queries text` returns the root node and every
  capture as plain data from `Language/Captures.hs`: a `Cap` (capture name, query file,
  pattern, match number, its `PNode`, the node's parent, its top-level ancestor for `@import`,
  and the `body`/`parameters`/`arguments` fields with their named, non-extra children). A
  `PNode` has its type, start and end as 0-based (line, column) with columns in characters (as
  ast-grep's `Match` counts them; tree-sitter counts bytes, `pnode` converts), its byte range,
  text, and whether it is named or extra. Each query set is compiled once per process
  (`compiled`, keyed by grammar and query set), as **one** query (the files joined), so the
  tree is walked once for all of them; each file is also compiled alone first, so a query
  that does not compile is a `Left` naming the file, the byte and the kind of error, and a
  capture's `capFile`/`capPattern` are recovered from each file's first pattern index. The
  query is pruned (`ots_prune`) to the captures the rule reads (`readCaptures` and every
  `definition.*`), and a pattern whose only kept capture is a bare `@name` (tags.scm's
  references) is disabled, so the others cost nothing at match time.
- **What is marshalled.** A node's facts come in one C call (`ots_facts`: bytes, rows, byte
  columns, symbol, flags); its type is the grammar's symbol name from a table read once
  (`symbolTable`). Its text and character columns are lazy, since most nodes are only compared.
  A capture's parent is read only for `.inner` captures, and fields only where the rule reads
  them (a definition's `body` and `parameters`, a call's `arguments`), with the children of a
  parameter or argument list but not of a body (a body's items are its inner captures). One
  exception: an `.inner` capture that is its parent's whole `body` (Python's textobjects
  capture the block, where Rust's capture each item) carries its named children as its own
  `body` field.
- **The cache.** `capturesOf` keeps the last 16 answers it computed (`answers`, `cacheBound`),
  newest first, keyed by (grammar, query set, text). It is first in, first out: a hit does
  not move its entry, so a text asked about constantly is still dropped after 16 newer ones
  (and parsed again on its next request). The LSP asks about one document again and
  again, and a generator's edits ask about one file several times: a request on the same text
  skips the parse and the marshalling. A changed text is another key, so a stale tree never
  answers; the oldest computed entry goes when a 17th text arrives (`cacheSize`, for the spec). Two
  threads that miss at once both compute the same pure answer; the list is updated with
  `atomicModifyIORef'`, and no lock is held while parsing.
- **What it costs.** Measured with `OLIFANT_TS_BENCH=<dir>` (the opt-in example in
  `TreeSitterSpec`, `<dir>` holding lock_api's `rwlock.rs` as `big.rs` and main's
  `rust.toml` with its declared targets as `rust-main.toml`), medians of 20 in the test build:
  `end of struct BillingService` on the fixture's `service.rs` 0.11 ms cold, 0.01 ms cached
  (ast-grep: 4.2 ms); `end of struct RwLock` on the 3,130-line `rwlock.rs` 12 ms cold, 0.9 ms
  cached (ast-grep: 15 ms). The C floor on that file is about 2.7 ms to parse and 4 ms to run
  the queries.
- **The rule.** `Captures.anchorFromCaptures root caps words` is the generic anchor rule, pure:
  `Nothing` for a phrase that is not generic (`genericPhrase` is its shape), else every
  container the phrase names, in the file's order, each with its items. A definition is a
  `definition.*` capture with the `@name` of its match; `functionCaps` and `classCaps` are the
  engine's table of which captures `function` and `class` mean (`kindOnlyCaps`,
  `definition.instance`, only a kind word reaches: `instance C`), and any other kind word
  narrows by node type (`struct` → `struct_item`, `mod` → `mod_item`). A definition's items are
  the `function.inner`/`class.inner` captures whose *nearest strictly enclosing* definition it
  is ("strictly": an impl with one method has the method as both item and definition), named
  and not extra (no punctuation, no comments); their parent is the container. An item that is
  no definition and ends on the line before a definition (or on its line) decorates it (a
  Rust attribute, a decorator): the two become one item (`decorated`), so `end of` spacing is
  measured between whole members and `start of` goes before the decoration. A match holds
  every capture of a `_+` however many (`ots_next` returns the count, `ots_captures` copies
  them), so a body of hundreds of children is read whole. When the inner capture is the whole
  body (a Python block), the body is the container and its named children are the items
  (`wholeBody`). Adjacent function definitions of one name with only white space between
  are one (`adjacent`: a Haskell signature and its equations, captured each on its own, are
  one function from the signature to the last equation); class-like ones are not joined. A
  definition with no inner range and no `body` field (that Haskell function) is its own
  container and its one item, so `end of` goes after it. With no inner
  capture (an empty body), the container is the definition's `body` field, empty, which
  placement opens. `parameters in F` reads the function's `parameters` field (type parameters
  are a direct child too, so a capture alone would not do), a call is named by its text up to
  its `arguments` field, and `imports` takes the top-level `@import` statements (`capTop`: a
  direct child of the file, or of a top-level node that holds no definition, Haskell's block);
  in a file with none, `Resolve.named` puts a first import after the file's header
  (`headerEnd`) when the language's `[imports] when_empty = "after header"` (Python's), and
  spaces `end of imports` unless its `[imports] spaced = false` (Python's: a blank line there
  separates import groups).
  A phrase's name matches exactly when any definition (or call) has that name; only when none
  does is it read as a POSIX expression matching a name whole, compiled once per phrase
  (`nameMatcher`, as the declared targets' `^{= NAME =}$` did: python-htmx's model reads
  `parameters in [a-z_]+_routes`), so `(<+>)` never means `<>`. `adjacent` is one map lookup
  per definition.
  `oneContainer` turns the list into "one, none, or matches N places".
- **Where the rule is asked.** It is the `Syntax` port's `anchorSyntax` field (after
  `runSlotProvider`); `AstGrep.astGrepSyntax` sets it to `TreeSitter.treeSitterAnchors`, which
  answers `Nothing` unless the language's `grammar` is linked, the phrase is generic, and its
  kind word names a node type of the grammar (`grammarNodeTypes`; so `end of the imports` is
  not the engine's). Fakes use `noAnchors`. In `Anchor/Resolve.hs`, `named` follows Decision 2:
  a declared target (`itemsOfTarget`'s declared branch), else the generic rule, whose answer
  is final ("no `struct Nope` in this file"), else the anchors provider, else
  `itemsOfTarget`'s "unknown target", whose "did you mean" and target list add
  `genericPhrases` when `grammarLinked`. `itemsOfTarget` (an `items of` query, a fragment, the
  catalog's target check) and `locateContainers` (every container; a name two definitions
  share gives both) ask the same field. `Anchor/Items.hs`'s up-front "unknown target in any
  language adapter" check lets a generic phrase through when any language's grammar is linked.
- **What else reads them.** The engine's language words went with m2 (the leftovers): a
  definition's name (`TreeSitter.definedName`: the `@name` of the definition a text starts
  with, decorators in; else its first word) for `finding_name`, `Rules`' "a name the base
  defined" (`Newness.newSince` takes the base's names from `definedNamesIn`) and callers notes;
  a signature (`signatureHead`: the text up to the definition's `body` field) for
  `finding_head`; `[migrate]`'s count without a `count` key (`App.countedIn` over
  `functionMatches`: the top-level `functionCaps` definitions: those inside no other function's byte span, so a method counts and a nested def, `let` or `where` binding does not; `MigrationReport.mrCountHint` says so when an unlinked language counted 0 for want of a `count` key, shown in `reportLine` and `count_hint`); and the folders a listing skips (each
  language's `ignore`, `Catalog.catalogIgnores`, `Workspace.ignoring`). `Newness`'
  `definitionKeywords`, `App`'s colon-scanning `signatureHead` and `Workspace.pruned`'s
  language folders are gone. `TestName`'s string prefixes and `self` move in m4.
- **Pure.** A parse and a query run are functions of (grammar, queries, text), so
  `capturesOf` is pure and wraps its IO in `unsafePerformIO` (Decision 6 of the m2 plan). Pure
  callers (the anchor rule; later, units and test units) read captures without threading IO.
  It is `unsafePerformIO`, not the dupable variant: the compile cache is behind an `MVar` and
  the tree and cursor are freed by `bracket`, which an abandoned duplicate evaluation would
  skip.

## Slots and providers

A **slot** is one question the engine asks of a language: the units in a text, its test units,
where an anchor phrase lands, the arguments that select some tests. A rule in the language file
answers each by default; `{ provider = [...] }` in the rule's place hands it to a program
([chapter 9](../user/09-packs-and-language-adapters.md), "Providers"). Either way the answer has
one type, so everything downstream is the same code.

`Olifant.Provider` (`Provider.hs`) is the protocol:

- `Provider` is the argv and the root it runs from (`provRoot`, set by the catalog).
  `runProvider :: Provider -> Value -> IO (Either Text Value)` adds `"v": 1` to the request
  (`withVersion`), resolves `argv[0]` (against the root when it holds a `/`, else on `PATH`) and
  runs it through `Process.runBytesCancellable` with a 10 s limit (`providerTimeout`), cwd the
  root. Its `Left` is one line: "`x` is not installed", "it timed out (10 s)", stderr's first
  line, "it exited N and said nothing", or "its answer is not JSON: …". The limit covers the
  whole exchange: `runBytesCancellable` writes stdin from a thread of its own, so a provider that
  never reads a large text still times out, and the kill of the process group wraps everything
  after the start, so an interruption (a cancel) leaves no process behind.
- Every run goes through a **circuit breaker** per provider (`runGuarded`, keyed by argv and
  root, in the `NOINLINE` `providerBreaker`). A run that ends in `Broken` (a timeout, a missing
  program, one that will not start) opens it: for `providerCoolOff` (60 s) every request to that
  provider gets the same `Left` at once, without a run. So a broken provider costs one timeout,
  not one per file: doctor's evidence, which asks it for each sampled file, stays bounded. A run
  is `Answered` (an answer, or a non-zero exit, or bad JSON), `Broken`, or `Stopped` (cancelled).
- The answer types: `UnitAnswer` (`name`, `line`, `end`, optional `kind`; units and test units),
  `AnchorAnswer` (`line`, `column`), and an argv for a selection. `decodeUnits n` checks a units
  answer whole against a text of `n` lines (no line before 1, no `end` before `line` or past
  `n`, no repeated span) and names the first bad entry; a half-good answer is never used.
  `decodeAnchor` allows `line` one past the end (the end of the file); `decodeArgs` wants a
  list of strings.
- `providerPure :: Provider -> Value -> Either Text Value` is `runProvider` from pure code,
  through `unsafePerformIO`, memoised (`runProviderMemo`) for the process in a `NOINLINE`
  `MVar` keyed by argv, root and the encoded request. The request holds the file's path
  (`textRequest (Just path)`, when the caller knows it) and its text, so the long-lived server
  asks again after an edit, and the same text at two paths is asked twice. Only `Answered` runs
  are kept: a timeout, a missing program or a cancel is asked again once the breaker lets it.
  The memo starts over past 512 answers. It is sound because a
  provider is pure by contract (the same request gives the same answer), which chapter 9's
  protocol section states. It exists for test units, which are read in pure code at many call
  sites; threading IO through conjure, the fill phase and stubs instead would be a large
  refactor for an escape hatch. The other slots are read in IO and call `runProvider`.
- Where each slot asks: units in `Server/Unit.unitAnswers`; test units in
  `TestUnits.testUnitsOf`; every reader of either takes the file's project-relative path as a
  `Maybe FilePath` after the language (`unitAt`, `unitsOf`, `unitAround`, `testUnitAt`,
  `testOrUnitAt`, `testUnitsOf`, `testUnitsReport`, `testAtIn`, `testLineIn`, `testCaseAtIn`,
  `testCaseLineIn`, `testSourcesIn`, `testSourceIn`, `FillPhase.assertionSpan`, and
  `resolveLayers` for anchors), and every caller that reads a file passes it; only
  `FillPhase.looksLikeFile`, which reads a model's code rather than a file, passes `Nothing`; an anchor in `Anchor/Resolve`'s `named`, when no `[targets.*]` shape
  matches the phrase, through the **Syntax** port's `runSlotProvider` (anchors resolve in any
  monad; `astGrepSyntax` runs `runProvider`, `noSyntax` and `querying` fakes answer "no
  provider runner"); a selection in `Check.processRunner` (request `{"tests": [ids]}`, the
  answer replacing `{= tests =}`). Diagnostics and test results are `convert`
  (`Check.convertReport`), whose stdin now carries `"v": 1` too.

## Doctor: capabilities and evidence

`Olifant.Capabilities.capabilities :: Language -> [Capability]` (`Capabilities.hs:24`) is a list
of fifteen rows, one function of the `Language` each: `units`, `diagnostics`, `missing cases`,
`tests`, `test units`, `anchors`, `holes`, `test selection`, `mutation`, `stub`, `conjure
shapes`, `messages`, `test messages`, `stub bodies`, `signature facts`. A `Capability` is its
name, how it is provided (`Right how`) or the key that would enable it (`Left key`), `capKey`
(the dotted key whose layer doctor names with `originOf`), and an optional `capFix` for a
capability no single key enables.

`Doctor.doctor` makes a row from a `Capability` plus, for five of them, a `Proof`
(`DoctorInput`'s `diEvidence`, by language, which `App.doctorChecks` fills with
`Evidence.evidenceFor` through `App.evidencePorts`). A `Left` is a `Warn` row with the fix: "add
KEY to .olifant/languages/NAME.toml" unless `capFix` says otherwise; no evidence is asked. A
`Right` with no evidence is an `Ok` row with `how · from <origin>`. With evidence (`units`, `test
units`, `tests`, `anchors`, `holes`): `Proven` is `Ok` with the evidence text in place of `how`;
`Unproven` is `Warn`, except the tests row when doctor did not run tests (`diRan`, from `doctor
--run`), which stays `Ok` with `how (not run: olifant doctor --run)`; `Disproven` is `Warn` with
the fix "regenerate the key or supply a provider (see chapter 9, Providers)". The origin is kept
on every row, and evidence never makes a row `Bad`, so doctor's exit code rule is unchanged.
After a language's capability rows, `Doctor.queriesRow` adds `LANGUAGE: queries`: for a linked
grammar (`linkedGrammarOf`), `Ok` with its crate and version (`grammarLabel`) and its query
files (`tree-sitter-rust 0.24.2; impls, imports, tags, textobjects (shipped)`); otherwise
`Warn` "no linked grammar (anchors from [targets.*] only)", with no fix (it is not a key to
add). The `anchors` capability's `how` adds "generic phrases (queries)" for a linked grammar,
which is never `absent` even with no `[targets.*]`.
The language server's `olifant.doctor` passes `False` for `run`, and answers through
`inBackground` (evidence runs tools on the sample, so it must not hold up the message loop). It
races `doctorChecks` against `untilCancelled` on the request's token: a `$/cancelRequest` ends
the race, which interrupts the evidence thread, and `runBytesCancellable` kills the processes it
had started.

To add a capability: add a row in `capabilities`, a `capKey` that `originOf` can resolve, and an
example to `CapabilitiesSpec` (its "a bare language" example counts the absences, so it changes
from fifteen). The user book's doctor table ([chapter 2](../user/02-running-generators.md)) lists
the rows, so it changes too.

### Evidence: each slot on a sample of the project

`Olifant.Evidence` (`Evidence.hs`) proves the slots on the project's own code.
`evidenceFor :: EvidencePorts -> Language -> IO Evidence` reads through `EvidencePorts`
(`epFiles`, the project's files; `epRead`; `epSyntax`; `epRun`, a test runner only with `doctor
--run`) and gives one `Proof` per slot: `Proven` (with the evidence), `Unproven` ("not seen",
"not run") or `Disproven` (the line or reason that failed).

- **The sample** (`sampleOf`): the first 40 files (`sampleSize`), in path order, that match the
  language's `files` (sorted through a set, so a large project costs n log n). Units and anchors
  run at most eight at a time (`pooled`). Its test files are its sources' `tests_for` files that exist, plus any
  sampled file that holds test units itself (a Rust module with its own tests).
- **units:** `unitsOf` on each sampled file (concurrently): "212 found in 38 files (e.g.
  src/a.py:14 add)"; with none, the first non-blank line, or the slot's failure.
- **test units:** `testUnitsReport` on each test file (each with its path). A sampled source
  that holds test units itself is found by asking every sampled file when the language has rules,
  but with a test-units provider the asking stops at the first failure; with none in an existing test file, the
  first unit the units slot finds there ("0 found; path:9 is `…`"); with no test file, "not seen
  (no test file in the 40 sampled)".
- **anchors:** for a linked grammar, the generic phrases first: `end of function N` and `end of
  class N`, N from the definitions the captures name in each sampled file (`genericNames`;
  names only one definition has first, since a struct and its impl share one), and `end of
  imports` on each sampled file until one resolves. Then each `[targets.*]` with at most one
  parameter, tried through `resolveLayers` with
  the position its `example` uses, on the sample's units' bare names (those whose unit text holds
  the target's words first; at most six tries each, targets in parallel; a target with no
  parameter is tried once per file). Each try is a `Try` (form, words, position) in `anchorsProof`. Targets with more
  parameters are "not tried". Proven when any target resolves; a target that never does is listed
  with `✗` and its last miss. When there was nothing to try them on (no unit in the sample) the
  row is `Unproven` "not seen (no unit in the 40 sampled)", not `Disproven`.
- **holes:** each `[holes.KIND]` placeholder (`Holes.placeholderFor`) put in its `wrap`, then the
  Syntax port's `ERROR` query, as `FillPhase.parses` does.
- **tests:** with `epRun`, the first test file runs: "ran uv on src/a__test.py, 3 results parsed
  (pytest)", or the run's broken reason.

## The rule, and the specs that hold it

The design rule is **no language branches in the engine**: a language's name appears in messages,
never in logic; behaviour comes from facts in the file (a regex, a template, a node kind) or from
a *format* (a parser written once for a tool's report). When a language needs something the
file cannot say, the fix is a new key with a default that does nothing, read by language-neutral
code, and the language's file sets it. The keys `arm_separator`, `lone_arm_spaced`, `opens_block`,
`empty_statement`, `stub_lines`, `unit_words` and `[test] assertion` were each added that way
(milestones 2, 3 and 4a in the git log: `git log 4c1fbc1..0e3c772`).

Two specs enforce it.

- **`NeutralSpec`** (`src/Olifant/NeutralSpec.hs`) scans every `.hs` file under `src/` except
  specs and `testdata`, and fails on a line that compares `langName` with a string literal
  (`langName … == "…"`) or contains `python ||`. Its `allowed` list is empty. It guards the shape
  the old code had (`langName lang == "python"`); it does not see a language reached some other
  way.
- **`ConjureNeutralSpec`** (`src/Olifant/ConjureNeutralSpec.hs`) is the stricter check for the
  conjure modules, banning Python's *syntax* (`"def "`, `__init__`, `"assert "`, `pytest`, …) in
  six files ([chapter 4](04-conjure.md#no-python-in-conjure)). Its second check bans, in every
  engine source, the shipped packs' names and hyphenated generator ids (read from `packs/`), so
  a pack's generator cannot be named by the engine; the migrate subsystem's lines are its
  allowlist.

Neither understands intent. `CapabilitiesSpec` and `DoctorSpec` ("a capability a language lacks
is a warning naming the key") and `CatalogSpec`'s layering examples ("one language per name, its
layers in pack order", "a broken key is blamed on the layer that holds it") complete the picture.

## Testing it

- `LanguageSpec`, `Language/PresetsSpec`, `Language/GenericSpec`, `Language/TestFormatsSpec`,
  `Language/TestUnitsSpec`: parse a language, apply a preset, read each report format and each
  test-unit rule, on text.
- `CatalogSpec`: layering, origins, broken-layer blame, toolchain overrides.
- `CapabilitiesSpec`, `DoctorSpec`: the table.
- Language-specific end-to-end: `LoopSpec`'s `haskellStubSpec`, `rustStubSpec` and
  `conjureFloorSpec` build real Haskell and Rust projects from the shipped files.
- Real tools: the python-htmx pack's own suite (`olifant test`, user chapter 10) needs `uv` and
  pyright; `OLIFANT_REQUIRE_TOOLS=1` turns "pending without the tool" into a failure.

To add a language: write `library/languages/<name>.toml`, touch `src/Olifant/Library.hs` so the
embed sees it, add a toolchain file if its tool has one, and run `olifant doctor` in a project:
the capability rows are the checklist. No Haskell is needed unless its report format is new.

## Rough spots

- **Presets fill by top-level key, so a nested table replaces the preset's whole.** `applyPreset`
  is `M.union (own table) preset`. A project's `[test.stub] gap = 1` over `preset = "pytest"`
  replaces pytest's whole `[test.stub]` (its `test` text with it); it does not just change the
  gap. This follows from the code and from the user book's "a key you write wins", and is not
  mentioned there for nested tables.
- **Layer merge and preset fill are two merges with different rules.** Layers merge
  recursively (`mergeTables`); a preset is a flat union. So a nested table that the shipped
  *language file* holds (`[imports]`, `[holes.expr]`, `[types]`) merges key by key with a
  pack's, while one that only the *preset* holds (pytest's `[test.stub]` and `[test.failure]`,
  in `pytest.toml`) is replaced whole by a layer that writes any of its keys.
- **Formats are named in code in three lists.** `knownFormats` (check), `knownTestFormats` (test)
  and `missingFormats` (behind `supportsMissing`, which doctor's `missing cases` row lists) are
  separate lists in `Language/Support.hs`, and `readTestReport` dispatches on the format string
  again. A new format touches all of them, and none is checked against the others. (The `test
  selection` row no longer special-cases pytest: the pytest toolchain sets `[test.select]` to
  `{= id =}`, the whole id, which `Check.selected` renders.)
- **`NeutralSpec` is a text match.** `langName lang == "python"` is caught, `testFormat t ==
  "pytest"` is not, nor is a branch on a file extension. `ConjureNeutralSpec` adds literal
  checks over named modules (conjure's; and strangling's, stubs', the fill prompt's,
  anticipation's, recognition's and doctor's) and bans every shipped pack's name, hyphenated
  generator and rule ids and `migrate.toml` values across `src/` and `app/`, with no allowlist.
- **A capability's origin falls back to its parent table.** `originOf` tries the dotted key
  (`test.select`), then its parent (`test`). The fallback gives "messages · from shipped" for
  `[check] undefined`, which the pyright preset actually supplied.
- **Keys exist in the language file that are about a pack, not a language.** `[conjure]` and
  `[types]` are the pack's convention but live in the language table so they layer with it.
  Doctor shows `conjure shapes` for every language, including Elm.
- **`olifant doctor`'s fix text names `.olifant/languages/<name>.toml`.** That is the project
  layer; for a key a *pack* should own, the user has to know to put it in the pack.
- **The layer for a `grammar = { source }` is found by directory.** `layerDir` is set only for
  pack and project layers, which is why the elm pack keeps a `languages/elm.toml` containing
  only its grammar.
