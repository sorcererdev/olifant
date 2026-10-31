# 4. Steered conjure

`conjure` writes the body of the function under your cursor (or of the test, from its name)
with a model, runs the tests, and lands the result. With a decision model it is **steered**:
after each red round a small typed model, Jev, says whether the conjure is converging, and
the engine climbs a ladder of stronger models, or pauses and hands you the situation. This
chapter is the code behind that. The user-side behaviour, with every field of the pause, is
[chapter 18 of the user book](../user/18-tdd-in-the-editor.md#conjuring); the tour's
[TDD in the editor](01-tour.md#tdd-in-the-editor-serversiteunittestsstubconjuremutateorder)
section covers the other three commands.

Run it. In the python-htmx sandbox, with a `[model]` and (for steering) a `[decide] model`
and a `[model] ladder` configured ([user chapter 12](../user/12-holes-and-models.md)):

```sh
scripts/try-pack.sh python-htmx /tmp/bistro && cd /tmp/bistro
olifant gen python-htmx/command --set app=bistro --set service=booking --set command=close_table
# in Neovim with durandal.nvim: <leader>ms in `handle` (stub), <leader>mc on the test, <leader>mc in `handle`
tail -f .olifant/state/steps.log
```

`steps.log` shows the steering as it happens: `conjure-next again 0.90 1 qwen` (Jev's choice,
confidence, round, rung), `conjure-pause budget`, `conjure-resume climb`, and the final
`conjure ... conjured by qwen → claude · 3 rounds · green`. The same words appear in the user
book's [step log](../user/18-tdd-in-the-editor.md#the-step-log).

## The modules

| module | what it holds |
|---|---|
| `Server/Conjure.hs` (1,462 lines) | the command: `conjureAt` (`:167`) and `resumeConjure` (`:177`) both end in `conjureWith` (`:356`); building the hole (`functionConjure` `:1303`, `testConjure` `:1330`); the fill (`fill` `:1423`); the steering hook (`steer` `:1140`); the wire (`wireFor` `:989`) and the port method (`portMethodPlan` `:1071`) |
| `Server/ConjureNext.hs` (1,042) | the **pure rules**: the policy (`policy` `:495`), Jev's question (`conjureNextQuestion` `:633`) and how to read its answer (`nextFrom` `:663`), the offers (`offersAt` `:440`), the diagnosis (`diagnosis` `:543`), the pause's wire shape (`PausedAnswer`, `pausedJson` `:850`), `PausedConjure`, the round records |
| `Server/ConjureScan.hs` (402) | **pure reading of source** by the pack's shapes: the fakes a test builds and never passes in (`unpassedFake` `:55`, `fakeIn`), the service's ports (`portsIn` `:172`), a fake's port (`derivePort` `:188`), a constructor's parameters (`ctorParamsIn`), Jev's port question (`conjurePortQuestion` `:246`), a new method's signature (`methodSignature` `:293`) |
| `Generation/Steer.hs` | the contract between a fill loop and its steerer: `RoundState`, `Steer = Again | Climb filler | Pause why`, `Steering` |
| `Generation/FillPhase.hs` | the fill loop that calls the hook (`stSteer`, `:79`) |
| `Server/Site.hs`, `Unit.hs`, `Tests.hs`, `Stub.hs` | what conjure shares with the other commands: the cursor's project, language, module and test file; the unit; the test run; the stub |
| `Decide.hs` | the typed question and answer (`Question`, `Answer`, `choice`, `confident`), `curlDecider`, `jevDecider`, `recordedDecider` |

`Conjure` does the IO and holds the state across rounds; `ConjureNext` and `ConjureScan` are
pure (their specs construct plain values). When you change a rule, change it in `ConjureNext`;
when you change what the hook does with the rule's answer, change `steer`.

## One conjure, end to end

The editor sends `olifant/conjure {textDocument, position, selection?, resume?}`
(`Lsp.hs:856`). `Lsp` runs it in the background under `$/progress` "conjuring", through a
snapshot of the server state (`withSnapshot`), and either `conjureAt` or, when the request has
a `resume`, `resumeConjure`. The answer is `conjureJson` (`Lsp.hs:1029`): a landed conjure, or
a paused one.

**1. The site** (`conjureWith`, `:358`). `siteAt` (`Server/Site.hs:61`) finds the project, the
language, the module and its test file (`[test] tests_for`, inverted for a test file), over the
editor's text. Then `unitAt` (`Server/Unit.hs:53`) finds the innermost definition at the line
from the language's `[unit] kinds`; on a test, `testUnitAt` (`:75`) finds the test by `[test]
unit` instead, because hspec's `it` is no `[unit]` kind. A conjure is **of a test** when it is
in a test file, or in a module that holds its own tests (Rust's `mod tests`: the test file is
the module). A selection replaces the unit's body with the selected lines. The refusals
(`outside any function`, `no test file yet`, `is on one line`, `no model configured`) are all
`Left` sentences from here.

**2. The hole.** The unit's body (found by `opens_block`, `bodyOf`) becomes one `block` hole
named `body`, in a template made with `holeOver`. `functionConjure` (`:1295`) words the
instruction from the **before-run** (`runOver` of the test file, over the editor's text):
"make the failing tests pass … write the least code that does it", with the failing tests'
messages and sources as context, the passing ones "to keep passing", and the body on screen as
"rejected by the person" when `isStubIn` says it is not a stub. The hole verifies by
`VerifyTests`: every test in the file must pass. `testConjure` (`:1322`) is for a test: the
module under test in full as context, the notes in the stub's body, and it verifies by
`VerifyRedAt tid line True`: the test must fail at its assertion (or pass: an error *before*
the assertion is asked again).

**3. The fill.** `fill` (`:1415`) calls `planWith` (`App.hs`) with `poStance = ConjureStance`
(the model is asked for the least code that passes), `poContext` the files above, `poSteering`
the hook, and `poFiller` the rung's filler wrapped to remember each round's attempt. The fill
phase's usual machinery applies: the placeholder, `[holes.block]`'s parse check, the cuts to a
hole, the imports, the check. A `Written` result is the conjured file's text; `Halted` is a
pause (every red hole paused).

**4. The landing.** The after-run runs the test file over the working files. The conjure lands
as one workspace edit over every file it wrote (the unit's file first; `smallestChange`,
`Edits.hs`), **whatever the after-run says**, unless the document moved on while the model
wrote (`ssLiveDoc`). `testSummary` and `functionSummary` word the result (`conjured · green`,
`red at the assertion`, `red: NameError date`). The step log gets one `conjure` line.

## Steering

Without `[decide] model` and a `[model] ladder` rung, none of this exists: `steeredFor`
(`:1116`) returns `Nothing`, `fill` is given no steering, and the model's `retries` run as for
any fill. With both, `steeredFor` builds `Steered`: the decider (`deciderUnder`,
`Server/State.hs:288`: a Jev backend gets `jevDecider`, any other chat backend `curlDecider`),
the ladder as `(Rung, ModelFiller)` pairs (the client's `model` replaces the first rung's name),
`[model] rounds` (default 6) and `[model] budget` (default 180 s).

The fill loop (`FillPhase.hs:79`) calls `stSteer` after every **red** round (never an
unrunnable one or a green one) with a `RoundState`: the round number, the filler's name, and
each red hole's why and the verify's `TestRun`. `steer` (`Conjure.hs:1133`) answers `Again`,
`Climb filler'` or `Pause why`. In order:

1. **Cancelled?** Answer `Again`; the loop stops at its own next turn.
2. **Record the round** (`roundRecord`) onto `cHistory`, and **scan** the before-run and every
   run so far (`scanFlags`): a failing test whose source builds a fake it never hands to any
   call is *flagged*. If there is a flag, derive the wire's `(param, port)` for it
   (`derive`: by rule from `[conjure]` shapes, else Jev's `conjure-port` question).
3. **The policy** (`policy`, `ConjureNext.hs:495`), in this order: a flagged test that failed,
   whose class lacks the port, with a wire generator applying and none run yet, is **wired**
   (`PolicyWire`) when the pair is derived and a `CausePort` pause otherwise; a flagged test
   still failing that no wire can help is `CauseTest` (unsatisfiable); a port the unit takes
   missing a method its body calls is `CauseMethod`; the same body twice is `CauseRepeat`; bodies
   that pass one test and fail the other `CauseTrade` (`isRepeat` `:268`, `isTrade` `:279`);
   a spent grant (`spent`) `CauseBudget`.
4. **Only if no rule decides, ask Jev** (`PolicyAsk`). `conjureNextQuestion` (`:633`) is a
   typed choice, `again | climb | ask` (`climb` only when the next rung is not `never`), with
   the rounds so far (each one's first line and failing tests), the failures, the passing
   tests, every involved test's source, the unit's header, the hint and whether the rounds trade
   or repeat. `nextFrom` (`:663`) reads the answer: `climb` at 0.6 (`conjureClimbGate`) climbs
   an `auto` rung and pauses (`CauseClimb`) at an `ask` rung; `ask` at 0.5 (`conjureAskGate`)
   pauses from round 2; anything else, including "unsure", is `Again`. Jev never writes code and
   never sees the project: it sees what `RoundView` summarises.
5. **Act.** `Climb` swaps in the next rung's filler (`record` wraps it as the first was) and
   carries the previous attempts into its prompt; `Pause` stores the pause (below); a wire
   pauses the loop with `Pause "wire"` and `cWire` set, so the wire runs and the fill starts
   again from that round over the wired files.

All the mutable state the hook keeps across rounds is one `IORef Counters` (`Conjure.hs:111`):
the rung, rounds and seconds used, the last round's view, the wire and what is wired, every
file written (`cWrites`, root-relative) with its editor version when first read, the offers of
a pause, the flagged tests. That record is the thing to read before changing the hook.

## The model ladder

The ladder is `[model] ladder = [...]` of backend names under `[model.backends.*]` (parsed in
`Model.hs`: `mcLadder`; the `escalate` key of each backend: `never`, `ask` (the default) or
`auto`). `Rung` pairs a name with its `Escalate`. The rule is in `nextFrom`: Jev may choose
`climb`; whether that climbs by itself (`auto`), pauses to ask you (`ask`) or is not offered
(`never`) is the *rung's* policy, not Jev's. A `climb` with no stronger rung left, asked of
`resumeConjure`, is refused: `there is no stronger model in [model] ladder`. The same ladder
is what a plan's reconcile climbs ([chapter 3](03-plans.md)), with `[plans] ladder` as an
override.

## Pause and resume

A pause is a value, `PausedConjure` (`ConjureNext.hs:964`), kept in
`ssPaused :: IORef (Int, Map Text PausedConjure)` (`Server/State.hs:123`) under a token. It
holds everything needed to continue without the editor: the working writes, the rung, the
round history, the offers shown, the wire and what was wired, the file's editor version when
the pause was made. `pausedJson` (`:850`) is the wire shape the editor draws.

A pause lives 10 minutes (`pausedLifetime`, `:1029`), and `pruned` drops any pause older than
that or whose file the editor has at another version. `conjureWith` prunes first, on every
call. `resumeConjure` (`:178`) then answers a `resume {token, choice, arg?, land?, hint?}`:

- it refuses, in one sentence and keeping the pause, a choice the pause did not offer
  (`refused`: `wireOf`, `portOf`, `reconjureOf`, a keep with no body);
- it ends the pause (and says so) when the pause is gone, the file changed in the editor
  (`ssDocs` version differs from `pdVersion`), or any file the conjure wrote changed since
  (`movedSince`);
- otherwise it applies the choice to the stored `PausedConjure` (`answered`: accepting a
  proposal puts the new text into the working writes) and calls `conjureWith` again with
  `Just (re, pd)`.

The choices: `continue` (a fresh grant of rounds and budget on the same rung, a `hint` added to
the next prompt as "The person says: …", never logged), `climb`, `stop` (nothing lands, or with
`land: true` the chosen round's body and the wire's files land, red), `wire`, `port` (add the
method a port lacks), `reconjure` (write a wrong test again, answering a `Proposal`), `accept`
and `reject` (the proposal). `Lsp.resumeOf` (`Lsp.hs:1005`) parses them; note that the
`InvalidParams` text lists seven and omits `port`.

A token is **taken** on resume (`takeToken`) and works once, except that `reject`, a refused
resume and a climb with nothing above it put it back, restarting its lifetime (`touch`, `keep`).
`stop` is the one choice that needs no steering on resume (a project that dropped its `[decide]
model` meanwhile can still stop).

### The wire, and the port method

Two behaviours go past writing a body. Both lean on the **pack's generators**, run in-process:

- **A wire** (`wireFor`, `:989`) is the generator the project's `.olifant/config.toml` names with
  `[conjure] wire = "<id>"`. If the unit's path matches its `[applies_at] path` template, the
  inputs that binds (`app`, `service`) and the derived `param`/`port` are given to the wire
  generator, planned over the working files (`wirePlan`, `:1045`); its writes join the
  conjure's, and land with it as one edit.
- **A port method** (`portMethodPlan`, `:1071`): when the check says a port the unit takes lacks
  a method (`CauseMethod`), the pause offers `add <m> to <Port>`, but only when the language's
  `[conjure.port] add` names a generator (`methodWall` gives no signature otherwise, so the pause
  asks the person to add it). On `port`, that generator is placed on the line after the port's
  block (`portEnd`, `ConjureScan.hs:350`; `blankAt` `:360` makes that line blank, since a snippet
  replaces the line it is placed on), its holes `name`, `params`, `returns` filled from the
  signature `methodSignature` made from `[types]` facts. Then the generator `[conjure.port]
  adapters` names, if any, is planned over that, the rung's model writing the bodies it leaves.
  Its inputs are what it declares out of: the slots of its own `[applies_at] path` matched on the
  port's file (`matchPathSlots`), the service's sets, `name`, `params`, `returns` and
  `signature` (`[types] method` rendered). The result is a `Proposal` to accept or reject like a
  re-conjured test. In python-htmx, `add = "port-method"`, `adapters = "port-method-adapters"`.
- **Advice for a test's body** (`advisedIn`, `ConjureScan.hs:413`): `testConjure` adds to the
  model's instruction the `say` of the first `[[conjure.advice]]` whose `when` matches a line of
  the test file. python-htmx's two tell the model to build the command (or query) through the
  stub's `command()` helper.

### Two things called `[conjure]`

`[conjure] wire = "..."` in the **project's** `.olifant/config.toml` (`Catalog.hs:305`,
`catalogConjureWire`) is a project setting naming a generator. `[conjure]` in a **language file**
(`langConjure`, `ConjureDef`) is a table of shapes a pack gives its language. They share a
name, a section header and no code. The user book spells the difference out
([chapter 9](../user/09-packs-and-language-adapters.md#conjures-shapes-conjure)); in code the
first is `Maybe Text` on `Catalog`, the second `Maybe ConjureDef` on `Language`.

## The `[conjure]` shapes, and the floor

`ConjureDef` (`Language/Adapter.hs`) is `{cjService, cjPort, cjFake, cjConstructor, cjAddMethod,
cjAdvice}`, each optional, parsed by `conjureFrom` (`Adapter.hs:580`). Which function reads what:

| key | type | read by |
|---|---|---|
| `service` | path template over `app`, `service` | `servicePaths` (`ConjureScan.hs:400`): what binds `app`/`service`; `serviceBase` in `Conjure.hs` |
| `[conjure.port]` `files`, `start`, `method` | `PortShape` | `portsIn` (`:174`): the ports of a service, the methods of each; `portEnd` where a port's block ends |
| `[conjure.port]` `add`, `adapters` | `AddMethod` | `portMethodPlan`: the generators that add a method a port lacks; `methodWall` offers the port row only with `add` |
| `[[conjure.advice]]` `when`, `say` | `[Advice]` | `advisedIn`: a sentence for `testConjure`'s instruction |
| `[conjure.fake]` `bound`, `classes`, `prefixes`, `keyword`, `stub` | `FakeShape` | `fakeIn`/`unpassedFake` (`:55`): a test's fake; `derivePort` strips a prefix to name the port; `handFake` hands a fake to a helper |
| `[conjure.constructor]` `files`, `owner`, `start`, `param` | `CtorShape` | `ctorParamsIn`: what siblings call a port's parameter (`deriveParam`); `takesPortIn`: whether the class takes it |
| `[types]` | `TypesDef` | `methodSignature` (`:293`), `methodText`: a new port method's signature from the call and the fake |
| `[check] undefined`, `missing_member`; `[test] undefined`, `[test.failure]` | the toolchain's words | `undefinedLabelIn` (`ConjureNext.hs:359`), `missingMemberIn` (`ConjureScan.hs:276`), `failureLineWith`: a `NameError`, a missing member, the failing assertion's line |

A language with **no** `[conjure]` conjures at the **floor**: a test's body, or a function's,
from the test and the unit. No ports, no wire, no scan (`fakeIn Nothing` finds nothing), no
method pause. Haskell and Rust ship at the floor, which is why conjure works on a hspec `it`
and a `#[test] fn` with no pack: `isStubIn` (the language's `stub_lines`), `testUnitAt` (its
`[test] unit` and `above`), `[test] assertion` (where a test must fail), `[imports]` and
`opens_block`/`empty_statement` do the rest. `LoopSpec`'s `conjureFloorSpec` (`:281`) is the
spec: a scripted runner and filler against a real hspec project and a real Rust one, asserting
the one edit.

### No Python in conjure

`ConjureNeutralSpec` (`src/Olifant/ConjureNeutralSpec.hs`) is the guard. It reads six source
files (`Server/Conjure.hs`, `ConjureScan.hs`, `ConjureNext.hs`, `Site.hs`, `Unit.hs` and
`Language/Test.hs`), drops comments, and fails if any line contains one of a list of literals:
`"def `, `"class `, `__init__`, `Protocol`, `".py"`, `__test.py`, `"assert `, `"test_"`,
`lambda`, `"self`, `NameError`, `AssertionError`, `pyright`, `pytest`, `"#"`. Its `allowed` list
is empty; adding to it is the admission that a language leaked back in. The list is of *strings
that were once in the code*, not a proof: it catches a Python literal in those six files and
nothing else (a literal in `Stub.hs` is invisible to it). It also holds `"app"` and `"service"`,
python-htmx's layout, which the port method once passed by name.

Its second check, **the engine holds no pack**, reads every engine source under `src/` and `app/`
(specs and `testdata` left out). It fails on a line that names a shipped pack (a hyphenated name
anywhere, `elm`/`rails`/`redwood` as a string literal or before `/`), a shipped generator id
with a hyphen (read from `packs/*/generators/` when it runs, so a new pack's generators are
covered), or `def command(`, `def query(`, `booking_repository`. One-word ids (`port`, `wire`,
`query`) are not checked: they are the engine's own words too. `packAllowed` lists the lines
that remain, all in the migrate subsystem (`App.hs`, `Cli.hs`, `Cli/Options.hs`,
`Server/Actions.hs`, `Server/Preview.hs`: the strangler pack's name and its
`extract-command`/`extract-query`), pending their own change. The broader rule, that the
engine never branches on a language's name, is `NeutralSpec` ([chapter 5](05-languages.md#the-rule-and-the-specs-that-hold-it)).

## Testing it

- `Server/ConjureNextSpec` (33 examples): the policy, the question's wording, `nextFrom`'s gates,
  offers, diagnosis sentences, round records. Start here for a rule.
- `Server/ConjureScanSpec` (19): fakes, ports, constructors, signatures, a port's end, advice, on text.
- `LspSpec`'s `conjureSpec` (`LspSpec.hs:1024`): `olifant/conjure` over pipes with a scripted
  runner and filler: pause as a result, resume `stop` keeping a body, a token works once,
  `reconjure` answers a proposal, reject returns the pause, a refused re-conjure keeps its
  proposal, an unoffered arg is refused, a test conjure whose fake is never handed in wires the
  port.
- `LoopSpec` `conjureFloorSpec`; `ConjureNeutralSpec`; `NeutralSpec`.
- `packs/python-htmx/tests/mutate/` is for mutate, not conjure. There is no spec that conjures
  against a **real** model; `ssTestFiller` and `scDecideAnswers` (`ServerConfig`) are the seams
  that let specs script the filler and Jev (`recordedDecider` reads a file of answers).

Run one with `cabal test --test-options='--match "olifant/conjure"'`, in the main checkout.

## The seams, in one place

- **Config:** `[decide] model` (Jev's backend), `[model] ladder`, `rounds`, `budget`, `retries`,
  `timeout`; each backend's `escalate`; `[conjure] wire` in the project config.
- **Language/pack keys:** `[unit]`, `[test]` (`unit`, `above`, `assertion`, `name`, `stub`,
  `failure`, `undefined`), `[check] undefined`/`missing_member`, `stub_lines`, `opens_block`,
  `empty_statement`, `[imports]`, `[types]`, `[conjure]`.
- **Ports:** `pTester`/`pTesterWithin` (the runner), `pSyntax`, `pFiles`; `ssTestFiller`,
  `scFills`, `scDecideAnswers` for specs.
- **Wire:** `olifant/conjure` request and answer; `$/progress` "conjuring" (`roundsProgress`,
  `worded`).

## Rough spots

- **`Conjure.hs` is one 1,454-line module and `conjureWith` is about 600 lines of `ExceptT`.**
  Its branches (stop, reject, reconjure of a flagged test, port, reconjure, the normal fill)
  are `case resuming of` arms that each end in `Paused`/`Landed`. The pure parts live in
  `ConjureNext`; the sequence is not decomposed further.
- **The wire's and the port method's input names are a contract.** The wire generator is given
  `param` and `port` (with its path's slots); the port method's generators `name`, `params`,
  `returns`, `signature`. These are the engine's words (`[conjure.fake] keyword` and `[types]
  method` use them too), so a pack's generators must use them; a generator that does not
  declare one is simply not given it.
- **`ResumePort` is accepted and undocumented on the wire.** `Lsp.resumeOf` parses `"port"`, the
  user book mentions `wire` but not `port` in the choice list, and the `InvalidParams` message
  lists seven choices.
- **`Resume.reLand` defaults to `False` and `reArg` is overloaded:** a re-conjure's test id, a
  wire's `"param: Port"`, a port method's `"Port.method"`, a keep's round number. `refused`
  (`Conjure.hs:202`) is where each reading is checked.
- **A cancelled conjure keeps no pause, but a cancelled *resume* has already taken the token.**
  The pause is gone and the editor says "conjure again".
- **Pauses live in memory.** A server restart forgets every pause (`ssPaused` is an `IORef`), by
  design; the user sees `this paused conjure is gone`.
- **The scan is line-regex based.** `unpassedFake` finds a fake variable and looks for it
  anywhere later in the test (`passedIn`); a fake handed over by a helper the test never names,
  or passed under a different variable, is a false flag or a missed one.
- **Jev's two conjure questions are separate decisions.** `conjure-next` (is the model
  converging) and `conjure-port` (which port) are both `Question`s on the same `Decider`; the
  gates live in two modules (`conjureAskGate`/`conjureClimbGate` in `ConjureNext`,
  `conjurePortGate` in `ConjureScan`).
