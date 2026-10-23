# 18. TDD in the editor

You build a feature in the editor as a series of small steps: a test, the least code that makes
it pass, a check that the tests would notice the code breaking. Olifant gives you four commands
for those steps, each one request to `olifant lsp`, each open at any time:

| command | key (durandal.nvim) | on a function, method or class | on a test |
|---|---|---|---|
| `:Durandal test` | `<leader>mt` (`<leader>mT` the whole file) | runs its test file (`tests_for`) | runs this one test |
| `:Durandal stub` | `<leader>ms` | appends a new test's stub to its test file | refused: stub from the module |
| `:Durandal conjure` | `<leader>mc` (visual: the selection) | conjures its body to pass its tests | conjures the test's body from its name |
| `:Durandal mutate` | `<leader>mm` | mutates its lines against its test file; lists the survivors | refused: mutate from the module |

The server keeps nothing between them but the step log: there is no state to be in, nothing
gated, nothing to accept. You decide the order. A typical round:

1. Cursor in `handle`, `stub` (`<leader>ms`): `test_handle` opens at the end of its test file,
   with `handle`'s import, a body of `...` and `assert ...`, the cursor on the name. The **name**
   is the spec: keep it or type over it.
2. `conjure` (`<leader>mc`) on the test: the model writes its body from its name. The notification
   says `red at the assertion` (the test fails where it should: there is no code yet). Read the
   assertion; edit it if it is not what you meant.
3. `conjure` in `handle`: the model writes the least code that makes its failing tests pass. The
   notification says `conjured · green`. Read it: if it is too simple, your test was; add a case.
4. `mutate` (`<leader>mm`) in `handle`: `1 survivor · 5/6 killed`, a warning on the survivor's
   line (`> → >= survives`), and the survivors modal. `s` stubs a test that kills it, named from
   the mutant; `conjure` it, as in step 2.

Each step is one undo in the editor and one line in the step log.

## Try it

```
$ scripts/try-pack.sh python-htmx /tmp/bistro
$ cd /tmp/bistro
$ olifant gen python-htmx/command --set app=bistro --set service=booking --set command=close_table
```

The command's test file is a stub. Delete it, open `close_table.py` in Neovim with
durandal.nvim, cursor in `handle`, and `<leader>ms`: the test file is created with the stub of
its first test. Or write the test yourself:

```python
from bistro.booking.commands.close_table import CloseTable, CloseTableInput
from bistro.shared.result import Ok


def test_closes_a_table() -> None:
    close = CloseTable()
    assert close.handle(CloseTableInput(id="t1")) == Ok(None)
```

`<leader>mt` on it runs it (red). With a `[model]` configured
([chapter 12](12-holes-and-models.md)), `<leader>mc` in `handle` conjures the body; with none,
write it yourself, `<leader>mt` again, then `<leader>mm` to see what the tests miss.

## Running a file's tests

`:DurandalTest` (`<leader>mt`) in durandal.nvim runs the test under the cursor; in a module, or
in a test file outside any test, it runs the file's: for a module, its test file (the one
`tests_for` names). `:DurandalTest file` (`<leader>mT`) runs the file's tests, and
`:DurandalTest all` (`:Durandal test all`) the project's. On the wire this is
`olifant/tests/run` with an optional `position` (the test there) and `scope` (`"file"` or
`"all"`; scope wins over position, and neither is the whole file). Each run is a line in the
step log (`test person <what ran> <outcome>`). The run is over the editor's text, unsaved
buffers included, through the `[test]` runner ([below](#configuring-tests-test)). The editor
shows the summary (`7 passed · 2 failed · 1.3 s`, or `broken: <why>` when the tests could not
run: an import error, a collection error), each failed test with its line, message and output,
and an error on each failed test's line, in that test's own file.
A module with no test file yet is refused, naming the file its tests would be in; a test file
whose module is gone runs itself.
To go between a module and its test file, jump: `:DurandalJump` (`<leader>mj`), `olifant/tests/file`
on the wire, opens the module's test file or the test file's module; a test file that does not
exist yet opens as a new, unsaved buffer: `<C-^>` back to the module and stub a test from its
function with `<leader>ms`.

## Stubbing a test

`:Durandal stub` (`<leader>ms`) with the cursor in a function, a method or a class (a decorator
line counts as its def) appends a new test to the end of its test file: `test_<name>`, the unit's
name in snake case (`CloseTable` is `test_close_table`; `_2`, `_3` when the file already has it),
with the unit's import (a test file that does not exist yet is created, starting with it; a file
without it gains it among its imports), a body of `...` and an `assert ...`. The test file opens
with the cursor on the name. Nothing is conjured. On the wire this is `olifant/stub` with
`{textDocument, position}`, answering `{file, line, name, edits}`. It is refused in one sentence:
in a test file (stub writes a test from its module), or outside any function or class. Each stub
is a line in the step log (`stub person <module>::<unit> stubbed <name>`).

A survivor of `mutate` has its own stub ([below](#mutating)): named from the mutant, the mutant as
a comment in its body.

## Conjuring

`:Durandal conjure` (`<leader>mc`) conjures the innermost function, method or class at the cursor with
the model: the unit is found by the language adapter's `[unit] kinds`, and a decorator line counts
as the def it decorates. In a module, its test file runs first. The model gets the unit's body as a
hole, the failing tests with their messages and sources ("make these pass"), the passing ones
("keep these passing"), the test file, the project modules it imports, and the body on screen when
it is not a stub (`raise NotImplementedError`, `...`, `pass`): that body goes in as "rejected by
the person". With no red test it is conjured anyway, told the tests pass. In a test file, on a test,
the model conjures the test's body from its name, seeing the module under test in full, the sibling
tests in the file, and any notes (comments, such as a survivor's `# kills:`) in the stub's body.
The fill pipeline is the usual one (stances, `[model]`, the cuts to a hole). The conjure stance
asks for the least code that passes, so a constant (`return Ok(None)`) is a correct conjure for one
test: every generalisation needs a test that asks for it.

The conjure always lands, as one edit and one undo step, whatever its tests say: nothing is refused
for discipline. The test file runs again with it, and the notification says what the run said:
`conjured · green` or `conjured · red: test_x · message` for a function; for a test, `red at the
assertion`, `red: error before the assertion`, `green: already passes`, each followed by `red:
test_y no longer passes` for a sibling that passed before and fails now. Failures are
diagnostics on their lines, as `:Durandal test` marks them. Conjure again is `conjure` again (the body
on screen goes in as rejected); undo is the editor's. A visual selection (`<leader>mc` in visual
mode, `:'<,'>Durandal conjure`) conjures those lines instead of the unit's body.

On the wire this is `olifant/conjure` with `{textDocument, position, selection?}` (the selection's
`start` and `end` lines, 0-based, inclusive), answering `{file, unit, edits, summary, green,
tests}` (`edits` null when the conjure changed nothing; `tests` the after-run as `olifant/tests/run`
answers it), under `$/progress` `conjuring`. It is refused in one sentence outside any function or
class, in a module with no test file yet (stub one first), on a function in a test file that is
no test, on a one-line def with no selection, with no model configured, when the model gave no
conjure at all, and when the file changed in the editor while the model conjured (your text
stands). Each conjure is a line in the step log (`conjure person <file>::<unit> conjured by <model> ·
<summary>`).

When the last attempt leaves a name undefined (Python's `NameError`, or the checker's "is not
defined"), the summary names it instead of the first failure: `red: NameError date`, or `green ·
NameError date` when no test failed; a test's summary ends in `· NameError date`. That is usually a
missing import line.

**With a decision model.** When the project names one (`[decide] model`) and `[model] ladder` has
a rung, the conjure is steered. After every red round Jev chooses: `again` (the same model once
more), `climb` (the next rung of the ladder, a stronger model) or `ask` (stop and show you), within
`[model] rounds` (default 6) and `[model] budget` (default 180 seconds). A few rules need no
question. Green lands at once. A fill that does not parse or compile, or that leaves a name
undefined, is an ordinary red round: the model is told to fix that first. When the test runner
itself cannot run (no `[test]`, pytest missing), the conjure gets one fill and lands it with a plain
`red: could not run · …`, never a pause, because no hint fixes a missing runner. A spent budget
pauses. A `climb` to a rung whose `escalate` is `ask` (the default) pauses too and offers the climb;
`auto` climbs at once, and `never` keeps Jev from choosing it.

A pause answers `conjure · round 4 · red: test_handle · assert status == 404` with what you may
do. **continue** grants a fresh `rounds` and `budget` on the same model. **climb** moves to the
next rung, carrying the attempts so far into the stronger model's prompt. **stop** lands the last
attempt as one edit and one undo step, as a conjure that ran out of retries does. A **hint**
continues with your words added to the next prompt as "The person says: …". A pause is forgotten
after 10 minutes, or as soon as its file changes in the editor; conjure again then. Without
`[decide] model` nothing of this happens: the model's `retries` run as before and the last attempt
lands. The landing line names the rungs and the rounds: `conjured by qwen → claude · 3 rounds ·
green`.

## Mutating

`:Durandal mutate` (`<leader>mm`) mutates the lines of the innermost function, method or class at
the cursor against its module's test file, and lists the survivors. The test file runs first, as
the editor has it (unsaved text is what is mutated and run): the tests that pass there are the
judges. A mutant one of them fails is **killed**; one they all pass **survives**. The unit's lines
only are mutated, at most 20 mutants, in file order. Nothing is stored or gated. Only Python has
mutation operators; another language answers `no mutation operators for <language>`.

Python's operators: `boundary` (`<` ↔ `<=`, `>` ↔ `>=`), `equality` (`==` ↔ `!=`), `negate` (a
condition), `and-or`, `arith` (an arithmetic swap), `number` (±1), `string` (→ `""`),
`return-none` (`return x` → `return None`), `delete-call` (a call statement removed).

Mutants never touch your file. Each runs in a **shadow**: the project's files as the editor has
them, the mutated file over its own, the test runner rooted there; four at a time. A mutant
that breaks the test file (it no longer imports) or times out counts as killed. Each mutant may
take five times as long as the unmutated run did (at least 5 s, at most 120 s), so one that loops
forever costs seconds.

The notification is the summary: `no survivors · 6/6 killed`, `1 survivor · 5/6 killed`,
`nothing to mutate in handle`. Each survivor is a warning on its line, with the mutation at the
end of it:

```python
        if input.party > table.seats:          > → >= survives
```

(`… survives (likely equivalent)` where the decision model thinks it equivalent.) The survivors
modal opens on them: the list on the left, the mutant in context on the right as a `-`/`+` pair,
footer `s stub · w waive · q close`; `j`/`k` move. `s` (or `<CR>`) on a survivor closes the modal and stubs
a test for it, in the unit's test file. The name comes from the mutant, so you know what the test
is for: a string's first four words (`f"there is no table {input.table_id}"` →
`test_there_is_no_table`), else the operator and the names on the mutated line
(`if input.party > table.seats:` → `test_boundary_party_seats`), with `_2` when the test file has
that name already. The mutant is a comment above the `...` in its body, which `conjure` reads as a
note:

```python
def test_boundary_party_seats() -> None:
    # kills: > → >= on line 52
    ...
    assert ...
```

`conjure` it, then `mutate` the unit again: a test that fails on the mutant kills it. Marks of the
same unit are replaced by each mutate, those of other units stay. A survivor you decide does not
matter you leave: nothing is gated on it, or you waive it.

### A kill shows

When a test run is green (`<leader>mt`, `<leader>mT`), the units of that module whose survivors
are marked are mutated again, silently: no modal, no summary. The marks follow the answers, an
open survivors modal is redrawn (and closes when no survivor is left), and what was killed is
said in one message: `killed: > → >= on line 52`. A red run does nothing, and a unit that cannot
be mutated now (edited into a syntax error, or gone) keeps its marks.

### Waiving a survivor

`w` in the survivors modal waives the survivor under the cursor: it asks for a reason (an empty
or cancelled input does nothing), and the survivor leaves the list and the buffer's warnings. A
waiver is for a mutant you judge equivalent, or not worth a test; nothing is gated either way.
From then on `olifant/mutate` leaves that mutant out of the unit's `survivors` and counts it in
the summary: `no survivors · 5/6 killed · 1 waived`.

A waiver is a line of `.olifant/state/waived.log` (ignored by git, like the step log: a waiver is yours, not the
team's), tab separated: the time, the module's project-relative path, the unit's name, what the mutant changed
from and to, the mutated line (trimmed) and the reason. It is kept by the mutant, not by its line
number, so it holds when you edit above it; a mutant that no longer occurs just stops matching.
To un-waive, delete its line (or edit it so it no longer matches) and mutate the unit again.

On the wire, `olifant/waive` takes `{textDocument, survivor: {line, operator, from, to, mutated},
reason}` (the survivor as `olifant/mutate` answered it; `olifant/mutate` now answers `waived` too)
and answers `{waived: true}`; an empty reason is refused (`a waiver needs a reason: …`).

On the wire this is `olifant/mutate` with `{textDocument, position}`, answering `{file, unit,
lines: {first, last}, killed, survived, survivors: [{line, operator, from, to, mutated, dim}],
summary}` (every `line` 1-based), under `$/progress` `mutating`; `olifant/stub` takes an optional
`survivor: {line, operator, from, to, mutated}`. It is refused in one sentence: `on a test:
mutate a function or class, from its module`, outside any unit, in a module with no test file yet
(stub one first), `could not run: <why>`, and `could not judge: test_x fails before mutation`
(no mutant is run then). Each mutate is a line in the step log (`mutate person <file>::<unit>
<summary>`), and a survivor's stub one more (`stub person <file>::<unit> stubbed <name> (kills
line <n>)`).

### The decision model orders the survivors

With a decision model configured (`[decide] model = "<a backend name>"`, [chapter 17](17-strangling-a-legacy-codebase.md#with-a-model-decisions-progressively)),
each survivor gets two questions in the background: *is it equivalent to the original?*
(`mutant-equivalent`: an equivalent one is dimmed and listed last) and *how much does it
matter?* (`mutant-matters`: high first). The survivors come in this order as far as it is known
when the judge ends; it keeps answering in the background, and the next `mutate` of the unit
orders by the answers that landed. Without one, survivors stay in file order.

The questions are typed (a fixed set of options; "unsure" always allowed; an answer outside the
options changes nothing) and the decision model never writes code. Today any backend from
`[model.backends]` can answer them, a local Ollama model included; a key is needed only if that
backend needs one. A dedicated decision model such as TypeSafe AI's Jev is what the questions
are shaped for, but no Jev backend exists yet.

## The step log

Each command appends one line to `.olifant/state/steps.log`: the time, the command, who, what
it was on, and its outcome, tab-separated.
(`olifant init` puts `.olifant/state/` in `.gitignore`; in a project made before, add it.)

```
2026-10-18T04:06:40Z	stub	person	src/bistro/booking/commands/close_table.py::handle	stubbed test_handle
2026-10-18T04:06:42Z	conjure	person	src/bistro/booking/commands/close_table__test.py::test_handle	conjured by qwen3-coder:30b · red at the assertion
2026-10-18T04:06:45Z	conjure	person	src/bistro/booking/commands/close_table.py::handle	conjured by qwen3-coder:30b · green
2026-10-18T04:06:47Z	mutate	person	src/bistro/booking/commands/close_table.py::handle	1 survivor · 5/6 killed
```

A steered conjure (one with `[decide] model`) adds a line for each of Jev's choices (the choice,
its confidence or `-`, the round and the rung), one for a pause (why), and one for a resume (the
choice only; a hint's words are never logged). Its `conjure` line names the rungs and the rounds:

```
2026-10-22T21:10:03Z	conjure-next	again	0.90	1	qwen
2026-10-22T21:10:09Z	conjure-pause	budget spent
2026-10-22T21:10:30Z	conjure-resume	climb
2026-10-22T21:10:41Z	conjure	person	src/bistro/booking/commands/close_table.py::handle	conjured by qwen → claude · 3 rounds · green
```

It answers what commits cannot: which lines a model conjured, and which you wrote.

## Configuring tests: `[test]`

A language adapter says how to run its tests and where a module's tests live:

```toml
# packs/python-htmx/languages/python.toml
[test]
preset    = "pytest"
tests_for = "{= dir =}/{= stem =}__test.py"
```

- **`preset = "pytest"`**: pytest through `uv`, in Olifant's own venv for the project, its
  output read for which test failed, where and why. You never name the venv.
- **`tests_for`**: the test file of a module, `{= dir =}` its folder and `{= stem =}` its name
  without the extension. `close_table.py`'s tests are `close_table__test.py`.

When no preset fits, `command` (with `{= tests =}` for the test ids), `format` (`pytest`),
`context` and `root` are the overrides, as for `[check]`
([chapter 9](09-packs-and-language-adapters.md#presets-check-and-test)). A generator's hole may
say `verify = "tests"` to be verified by the module's tests ([chapter 12](12-holes-and-models.md)).
`olifant doctor` has a line for the runner (`test runner uv (python)`).

## Limits

- **Mutation is Python only.** The operators are Python's; another language with a `[test]`
  table runs its tests and conjures, and `mutate` says `no mutation operators for <language>`.
- **A stub's import is Python's, and only the unit's** (or its class's): a name your assertion
  uses beyond it you import yourself. A file importing it already (on one line or several, in
  brackets) gains nothing; one importing from its module gains the name there.
- **A conjure sees** the test file (a test's conjure, the module), the project modules either one
  imports, and what those import (ports, models; at most 8 files, 120 lines each).
- **Mutants are capped at 20** per mutate; a long unit is judged in part.
