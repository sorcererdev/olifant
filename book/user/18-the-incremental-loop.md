# 18. The incremental loop

You build a feature in the editor as a series of small steps: a test, the least code that makes
it pass, a check that the tests would notice the code breaking. Each step is a **magic circle**
drawn around a few lines that you read in place and accept, edit, steer or reject. A model may type the code;
you decide what it must do, and you are asked to author something at every step, so a circle
cannot be rubber-stamped.

## What you see

1. Cursor on a function no test calls, `begin`: the stub of its first test opens at the end of
   its test file, with the function's import (`from
   bistro.booking.commands.close_table import CloseTable`: a new file starts with it; a file
   without it gains it among its imports), the cursor on its name. You write the
   test's **name and assertion**; `draft` fills the arrangement between them, verified: the test
   runs and fails *at the assertion*. That test's circle is judged clean at once; `accept` it.
2. The function begins on that red test (as `begin` on a red test does: on a test, it runs, it is
   red, and the editor jumps to the function it calls). Its body is inside a **magic circle**: the
   highlighted span with the HUD line above it; nothing leaves the circle until every survivor is
   killed or waived. The HUD reads `▸ test_closes_a_table · red: NotImplementedError: … · draft`
   (red, and the next step), then `drafting…` while the model writes (only `reject` is open then).
3. The draft lands: `▸ test_closes_a_table · draft 1 · green · judging…`. You read it. It is the
   *least* code that passes (the fake-it stance, on purpose): if it is too simple, your test was;
   add a case.
4. `· 1 survivor · stub a test or waive`. Line 53's `>` → `>=` survives (the note on the line says so,
   `> → >= survives · stub / waive`): no test notices the boundary. You decide it matters. `stub`
   (`<leader>ms`) opens a new test stub; you write its assertion; the arrangement is
   drafted and verified both ways (it passes on your code and fails on the mutant, so the test
   kills it).
5. `clean ✓ accept`. `accept`: the highlight goes, and one line lands in the step log. Or you waive
   a survivor with a one-line reason (`<leader>mw`), edit the span by hand (the test reruns on a pause), ask for
   the `next` candidate, or `reject` to put the body back as it was.

## Try it

```
$ scripts/try-pack.sh python-htmx /tmp/bistro
$ cd /tmp/bistro
$ olifant gen python-htmx/command --set app=bistro --set service=booking --set command=close_table
```

The command's test file is a stub, red until you write its first case. Delete it and `begin` in
`handle` to have the stub of its first test opened for you, or make it yours:

```python
from bistro.booking.commands.close_table import CloseTable, CloseTableInput
from bistro.shared.result import Ok


def test_closes_a_table() -> None:
    close = CloseTable()
    assert close.handle(CloseTableInput(id="t1")) == Ok(None)
```

Open it in Neovim with durandal.nvim, cursor on the test, `<leader>mb`. With a `[model]`
configured ([chapter 12](12-holes-and-models.md)), the draft follows at once; with none, write
the body yourself: the circle still tests and judges it.

## The moves

`:DurandalCircle MOVE` (`:DurandalBlock` is the same command), and each one in the ⚡ menu
(`Circle: MOVE`, first while a magic circle is open). The HUD's state says which are open, and
its last words are the next step when it is yours: `· draft` while red (gone once a model's draft
has failed: read it, edit it or ask for `next`), `· name it, then draft · reject drops it` on a
new test's stub (`▸ test_your_name · assert, then draft · reject drops it` once you named it and
it was tested), `▸ test_boundary_party_seats · assert, then draft · reject drops it` on a
survivor's stub (its name first), `· stub a test or waive`, `✓ accept`.

| move | key | when | what it does |
|---|---|---|---|
| `begin` | `<leader>mb` | on a test, or in a function | on a red test: a circle around the body of the function it calls, red; the draft starts. On a function whose tests pass: a circle around its body, judged at once. On a function no test calls: the stub of its first test at the end of its test file, a circle of its own in the test file (accepted, the function begins on it) |
| `draft` | | red or green, no draft yet | the span as a hole a model fills, verified by the circle's test (below) |
| `next` | `<leader>mn` | after a draft | another candidate; the drafts so far go to the model as *not this, rejected by the person* |
| `test` | | red, green, can't run | run the circle's test on the buffer's text now |
| `stub` | `<leader>ms` | judged, a survivor stands | the survivor on the cursor's line, or the top-ranked one when the line has none: a test stub at the end of the test file, in its own circle (on the wire it is `olifant/block/kill`; `:DurandalCircle kill` still works) |
| `waive` | `<leader>mw` | a survivor on the cursor's line | asks why (`vim.ui.input`); writes the reason on the line |
| `accept` | `<leader>my` | judged, no unwaived survivor | done: the circle's marks go |
| `reject` | `<leader>mx` | any time | the file's text as it was when the circle began |

**Editing is a move too.** Type in the span and pause: the test reruns on the buffer's text
(nothing needs saving), and the circle is red or green as your text is; green is judged. A draft
arriving after you typed is not applied: your text stands. A draft that fails puts the circle back
as it was, tested again first if you typed while the model wrote. Typing outside the span does
nothing.

**One magic circle per file.** A second `begin` in a file where a circle is open is refused (`a
magic circle is open in this file: accept or reject it first`).

## The forcing

The person's part of each circle is the part that cannot be delegated:

- **A test's name and assertion.** The model drafts only the arrangement between them, and a
  test draft must fail *at the assertion* before any code exists: one that errors in its setup,
  or passes already, is a failed draft. A new test's circle is green when its test is red there.
- **Reading a draft.** The drafts stance asks the model for the least code that passes, so a
  constant (`return Ok(None)`) is a correct draft for one test. Every generalisation needs a test
  that asks for it.
- **Each survivor.** Accept is refused while one stands
  (`1 survivor(s): stub a test or waive each, then accept`): stub a test that kills it, or
  waive it with a reason.

## Mutation and waivers

When a circle goes green it is judged: its lines new since the circle began (all the span's, for a
body you began on as it was) are mutated, each mutant run against the circle's test file. The
file runs once unmutated first: the tests that pass there are the judges. A mutant one of them
fails is **killed**; one they all pass **survives**. A test that was already red (the next one,
being written) judges nothing, and a file that does not run at all is `cannot run`, not judged.

Python's operators: `boundary` (`<` ↔ `<=`, `>` ↔ `>=`), `equality` (`==` ↔ `!=`), `negate` (a
condition), `and-or`, `arith` (an arithmetic swap), `number` (±1), `string` (→ `""`),
`return-none` (`return x` → `return None`), `delete-call` (a call statement removed). At most 20
mutants per circle, in file order.

Mutants never touch your file. Each runs in a **shadow**: the project's files as the editor has
them, the mutated file over its own, the test runner rooted there; four at a time. A mutant
that breaks the test file (it no longer imports) or times out counts as killed. Each mutant may
take five times as long as the unmutated run did (at least 5 s, at most 120 s), so one that loops
forever costs seconds.

A survivor is a warning on its line, with the mutation at the end of it:

```python
        if input.party > table.seats:          > → >= survives
```

**Stub a test for it**: `<leader>ms` (`:DurandalCircle stub`) on that line opens the stub at the end of the test file, the
cursor on its name. The name comes from the mutant, so you know what the test is for: a string's
first four words (`f"there is no table {input.table_id}"` → `test_there_is_no_table`), else the
operator and the names on the mutated line (`if input.party > table.seats:` →
`test_boundary_party_seats`), with `_2` when the test file has that name already:

```python
def test_boundary_party_seats() -> None:
    raise NotImplementedError("olifant: arrange: set up what the assertion below needs")
    assert ...
```

Keep the name or type over it (the HUD follows the name you type once it is tested), write the
assertion (`Tab` walks them), then `:DurandalCircle draft` in the test file.
The arrangement is drafted and verified both ways: the new test, and every test that passed
before, must pass on your code, and the new test must fail with the mutant in place (a sibling
test that was red before is passed over). `accept` the test, and the code's circle is judged again without that survivor.

Changed your mind? `reject` in the test file drops the stub (the file as it was before it). Or
delete the stub yourself: once none of its lines is left (retyping its name or its def line is
not deleting it), its circle closes on its own (the
step log says `reject … the stub was deleted`) and the file stays as you left it. Either way the
code's circle still has the survivor; stub again from the module. While the stub's circle is
open, another stub is refused, naming the test file.

**Waive** it: `<leader>mw` (`:DurandalCircle waive`) asks why, and the reason goes on the line, in the style of
rules' allowances:

```python
            return TableNotFound(f"there is no table {input.table_id}")  # olifant: string ok: the wording is not under test
```

The comment travels with the code, so the next judging of that line knows it. Waivers are data:
which operators people waive says which should stay in the preset.

### The decision model orders the survivors

With a decision model configured (`[decide] model = "<a backend name>"`, [chapter 17](17-strangling-a-legacy-codebase.md#with-a-model-decisions-progressively)),
each survivor gets two questions in the background: *is it equivalent to the original?*
(`mutant-equivalent`: an equivalent one is dimmed, a hint rather than a warning, and listed last)
and *how much does it matter?* (`mutant-matters`: high first). The circle is pushed again as the
answers land. Without one, survivors stay in file order. `stub` off a survivor's line takes the
first unwaived survivor in this order.

The questions are typed (a fixed set of options; "unsure" always allowed; an answer outside the
options changes nothing) and the decision model never writes code. Today any backend from
`[model.backends]` can answer them, a local Ollama model included; a key is needed only if that
backend needs one. A dedicated decision model such as TypeSafe AI's Jev is what the questions
are shaped for, but no Jev backend exists yet.

## The step log

Each move appends one line to `.olifant/state/steps.log`: time, block (the circle's number), move,
who, result.
(`olifant init` puts `.olifant/state/` in `.gitignore`; in a project made before, add it.)

```
2026-10-13T04:06:42Z	block 1	begin	person	▸ test_closes_a_table · red: NotImplementedError: … · draft
2026-10-13T04:06:43Z	block 1	draft	qwen3-coder:30b	green
2026-10-13T04:06:44Z	block 1	test	person	green
2026-10-13T04:06:44Z	block 1	judge	mutation	1/1 killed
2026-10-13T04:06:44Z	block 1	accept	person	accepted
```

It answers what commits cannot: which lines a model drafted, which you wrote, what was waived and
why. (`test person` after a draft is the test of the draft as the editor applied it; a result
that finished after the circle moved on is logged `stale: …` and not applied.)

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

## Running a file's tests

Outside a magic circle you can run a file's tests whole: `:DurandalTest` (`<leader>mt`) in
durandal.nvim, `olifant/tests/run` on the wire. In a module it runs the module's test file (the
one `tests_for` names); in a test file, that file. The run is over the editor's text, unsaved
buffers included, through the same `[test]` runner as the loop. The editor shows the summary
(`7 passed · 2 failed · 1.3 s`, or `broken: <why>` when the tests could not run: an import error,
a collection error), each failed test with its line, message and output, and an error on each
failed test's line.
A module with no test file yet is refused, naming the file its tests would be in; a test file
whose module is gone runs itself.

## What works today, and its limits

- **Python only.** The operators are Python's; a language with a `[test]` table but no operators
  is begun and drafted but cannot be judged (`cannot run: no mutation operators for …`), so its
  circles are never accepted.
- **A stub's import is Python's, and only the function's** (or its class's): a name your
  assertion uses beyond it you import yourself. A file importing it already (on one line or
  several, in brackets) gains nothing; one importing from its module gains the name there.
- **A draft that fails its test is written anyway**, its last attempt marked red, for you to
  finish by hand; a model that offered nothing changes nothing. A draft sees the test file (a
  test's draft, the module) beside the rejected drafts.
- **One circle per file**, and one in a test file.
- **The function under test is found by a method call** (`.NAME(` in the test, its assertions
  first). A test of a bare function (`assert add(1, 2) == 3`) is not located yet: begin in the
  function instead. So is the function a new test calls, once you accept it: a new test of a
  bare function leaves you to begin in the function.
- **The span can swell.** A draft that adds an import shows the span from the import line down,
  when the import merged into the file's imports.
- **An edit above the span leaves it stale** until the next test of the circle: its lines are where
  they were.
- **A line with several survivors is waived together**, one reason for each operator's comment.
- **Waive acts on the cursor's line**, not on one survivor of several there. **Stub takes the
  survivor on the cursor's line**, or the top-ranked one when the line has none.
- **Mutants are capped at 20** per circle; a long body is judged in part.

## Mistakes

- **`no test for this module yet; write one …`** — `begin` was in a module whose test file does not
  exist, the cursor on no function. Put it in the function to have its test's stub opened, or
  write the test where the message says, then begin on it.
- **`name the new test first`** — `draft` on a new test's stub (from `begin`) whose name is still
  the stub's (`test_name`): type it (and the assertion), then draft. A stub for a survivor is
  named already.
- **`the magic circle moved on`** — a draft, a test or a judge finished after you rejected or
  accepted the circle, or after another move (a later edit's test): nothing lands, and the step log says
  `stale:` before what it would have been.
- **`your edit is being tested; waive once its verdict lands`** (or `accept`) — you typed in the
  span and its test has not landed yet (or the pause before it has not run): the survivors you
  see are the text's before it. Wait for the HUD to change, then waive or accept what the new
  verdict shows.
- **`a draft is in flight; wait for it, then test or press next`** — `test` while the model
  writes: the draft's own test settles the circle.
- **`no survivor left: accept`** — `stub` with every survivor waived (or none): `accept`.
- **`a magic circle is open in src/…/close_table__test.py: accept or reject it there first`** —
  `stub` while a test's circle is open in the module's test file: finish it there, `reject` it,
  or delete its stub.
- **`stub a test from the code's magic circle, in its module`** — `stub` in a test's circle (its
  stub's test file): go to the module whose circle has the survivor.
- **`every test here passes; write a failing one …`** — `begin` on a green test: a circle starts
  from what is red. Begin in the function instead to judge it.
- **`cannot run: …` in the HUD** — the test file does not import or collect (a syntax error, a
  missing name). Fix it, then `test`.
- **`no model is configured`** — `draft` with no `[model]`: write the body; the magic circle still
  judges it.
- **`the buffer changed while drafting; your text stands`** — you typed while the model wrote. Ask
  again with `draft` or `next`, or keep typing.
