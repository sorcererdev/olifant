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
✓ test runner uv (python) — /opt/homebrew/bin/uv; tests_for = {= dir =}/{= stem =}__test.py
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
program's answer (`Provider.providedUnits`, memoised by request). `unitsOf` sorts either by
start (longer first) and folds it the same way: `defLine` (the first line `name` names, else the
first), `dedupe`, `joined`, `qualify`. `unitAt` keeps its rule path; with a provider it takes the
innermost answer holding the line, its body from `bodyOf` as before. `FillPhase`'s top-level
names (the answers no other answer holds) read a provider through `providerPure`, since the fill
phase is generic in its monad.
`unit_words` (`langUnitWords`) is a sentence for prompts (`Errand.projectUnitWords`).

### `[test]`: running tests and finding them

`[test]` is two things in one table. The **runner** keys (`command`, `context`, `format`, `root`,
`tests_for`, `exclusive`, `timeout`, `install`, `[test.select]`, `[test.lines]`, `file_group`,
`convert`) make a `TestDef`. They are read by `Language/Check.hs`: `processRunner` (`:135`) runs
a scratch copy of the project's files (`scratchRun`, `:105`), `readTestReport` (`:187`) picks
the parser (`parsePytest`, or `TestFormats.parseLines | parseTap | parseJunit | parseTestJson`),
`selected` (`:176`) turns a test id into arguments with `[test.select]`, and `runTestsIn`
(`:453`) is the entry the server uses. `tests_for` is read by `Server/Tests.hs`
(`testsForFile`, `placeOf`) and `Language.Test.testsForFile`.

The **shape** keys are the language's own, parsed even when there is no runner (a table holding
only shapes makes no `TestDef`: `testShapeKeys`): `unit` (rules: `langTest … testUnitRules`,
with `after` and `above`) is read by `Language/TestUnits.hs` for tests and groups, and by
`Server/Unit.testUnitAt`; `assertion` (`langAssertion`) by conjure (red at the assertion),
`Server/Order.hs` and `FillPhase` (the split of a test's two holes); `name`
(`testNaming`) by `Server/Stub.hs`; `[test.stub]` (`testStub`, `StubDef`) by `Stub.stubWith`;
`[test.failure]` (`FailureDef`) by `Language.Test.failureLineWith`/`gotExpectedWith`;
`undefined` (`testUndefined`) by `ConjureNext.undefinedIn`.

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
  line, "it exited N and said nothing", or "its answer is not JSON: …".
- The answer types: `UnitAnswer` (`name`, `line`, `end`, optional `kind`; units and test units),
  `AnchorAnswer` (`line`, `column`), and an argv for a selection. `decodeUnits n` checks a units
  answer whole against a text of `n` lines (no line before 1, no `end` before `line` or past
  `n`, no repeated span) and names the first bad entry; a half-good answer is never used.
  `decodeAnchor` allows `line` one past the end (the end of the file); `decodeArgs` wants a
  list of strings.
- `providerPure :: Provider -> Value -> Either Text Value` is `runProvider` from pure code,
  through `unsafePerformIO`, memoised for the process in a `NOINLINE` `MVar` keyed by argv,
  root and the encoded request. The request holds the file's path and text, so the long-lived
  server asks again after an edit; the memo starts over past 512 answers. It is sound because a
  provider is pure by contract (the same request gives the same answer), which chapter 9's
  protocol section states. It exists for test units, which are read in pure code at many call
  sites; threading IO through conjure, the fill phase and stubs instead would be a large
  refactor for an escape hatch. The other slots are read in IO and call `runProvider`.

## Doctor's capability table

`Olifant.Capabilities.capabilities :: Language -> [Capability]` (`Capabilities.hs:24`) is a list
of thirteen rows, one function of the `Language` each: `units`, `diagnostics`, `missing cases`,
`tests`, `test units`, `test selection`, `mutation`, `stub`, `conjure shapes`, `messages`,
`test messages`, `stub bodies`, `signature facts`. A `Capability` is its name, how it is
provided (`Right how`) or the key that would enable it (`Left key`), `capKey` (the dotted key
whose layer doctor names with `originOf`), and an optional `capFix` for a capability no single
key enables. `Doctor.doctor` (`Doctor.hs:~60`) turns a `Right` into an `Ok` row with `· from
<origin>` and a `Left` into a `Warn` row with the fix: "add KEY to
.olifant/languages/NAME.toml" unless `capFix` says otherwise.

To add a capability: add a row in `capabilities`, a `capKey` that `originOf` can resolve, and an
example to `CapabilitiesSpec` (its "a bare language" example counts the absences, so it changes
from thirteen). The user book's doctor table ([chapter 2](../user/02-running-generators.md)) lists
the rows, so it changes too.

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
