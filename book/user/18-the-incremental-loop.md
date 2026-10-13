# 18. The incremental loop

You build a feature in the editor as a series of small steps: a test, the least code that makes
it pass, a check that the tests would notice the code breaking. Each step is a **block** a few
lines long that you read in place and accept, edit, steer or reject. A model may type the code;
you decide what it must do, and you are asked to author something at every step, so a block
cannot be rubber-stamped.

## What you see

1. Cursor on a function no test calls, `begin`: the stub of its first test opens at the end of
   its test file (a new file starts with the function's import: `from
   bistro.booking.commands.close_table import CloseTable`), the cursor on its name. You write the
   test's **name and assertion**; `draft` fills the arrangement between them, verified: the test
   runs and fails *at the assertion*. That test block is judged clean at once; `accept` it.
2. The function begins on that red test (as `begin` on a red test does: on a test, it runs, it is
   red, and the editor jumps to the function it calls). Its body is a **pending span**, with a soft
   background and a HUD line above it: `▸ test_closes_a_table · red: NotImplementedError: …`, then
   `drafting…` while the model writes (only `reject` is open then).
3. The draft lands: `▸ test_closes_a_table · draft 1 · green · judging…`. You read it. It is the
   *least* code that passes (the fake-it stance, on purpose): if it is too simple, your test was;
   add a case.
4. `· 1 survivor`. Line 53's `>` → `>=` survives: no test notices the boundary. You decide it
   matters. `kill` opens a new test stub; you write its name and assertion; the arrangement is
   drafted and verified both ways (it passes on your code and fails on the mutant).
5. `clean ✓ accept`. `accept`: the highlight goes, and one line lands in the step log. Or you waive
   a survivor with a one-line reason, edit the span by hand (the test reruns on a pause), ask for
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
the body yourself: the block still tests and judges it.

## The moves

`:DurandalBlock MOVE`, and each one in the ⚡ menu (`Block: MOVE`, first while a block is
pending). The HUD's state says which are open.

| move | key | when | what it does |
|---|---|---|---|
| `begin` | `<leader>mb` | on a test, or in a function | on a red test: a block over the body of the function it calls, red; the draft starts. On a function whose tests pass: a block over its body, judged at once. On a function no test calls: the stub of its first test at the end of its test file, a test block of its own (accepted, the function begins on it) |
| `draft` | | red or green, no draft yet | the span as a hole a model fills, verified by the block's test (below) |
| `next` | `<leader>mn` | after a draft | another candidate; the drafts so far go to the model as *not this, rejected by the person* |
| `test` | | red, green, can't run | run the block's test on the buffer's text now |
| `kill` | | a survivor on the cursor's line | a test stub at the end of the test file, in its own block |
| `waive` | | a survivor on the cursor's line | asks why (`vim.ui.input`); writes the reason on the line |
| `accept` | `<leader>my` | judged, no unwaived survivor | done: the block's marks go |
| `reject` | `<leader>mx` | any time | the file's text as it was when the block began |

**Editing is a move too.** Type in the span and pause: the test reruns on the buffer's text
(nothing needs saving), and the block is red or green as your text is; green is judged. A draft
arriving after you typed is not applied: your text stands. Typing outside the span does nothing.

**One block per file.** A second `begin` in a file with a pending block is refused (`a block is
pending in this file: accept or reject it first`).

## The forcing

The person's part of each block is the part that cannot be delegated:

- **A test's name and assertion.** The model drafts only the arrangement between them, and a
  test draft must fail *at the assertion* before any code exists: one that errors in its setup,
  or passes already, is a failed draft. A new test's block is green when its test is red there.
- **Reading a draft.** The drafts stance asks the model for the least code that passes, so a
  constant (`return Ok(None)`) is a correct draft for one test. Every generalisation needs a test
  that asks for it.
- **Each survivor.** Accept is refused while one stands
  (`1 survivor(s): kill or waive each, then accept`): kill it with a test, or waive it with a
  reason.

## Mutation and waivers

When a block goes green it is judged: its lines new since the block began (all the span's, for a
body you began on as it was) are mutated, each mutant run against the block's test file. The
file runs once unmutated first: the tests that pass there are the judges. A mutant one of them
fails is **killed**; one they all pass **survives**. A test that was already red (the next one,
being written) judges nothing, and a file that does not run at all is `cannot run`, not judged.

Python's operators: `boundary` (`<` ↔ `<=`, `>` ↔ `>=`), `equality` (`==` ↔ `!=`), `negate` (a
condition), `and-or`, `arith` (an arithmetic swap), `number` (±1), `string` (→ `""`),
`return-none` (`return x` → `return None`), `delete-call` (a call statement removed). At most 20
mutants per block, in file order.

Mutants never touch your file. Each runs in a **shadow**: the project's files as the editor has
them, the mutated file over its own, the test runner rooted there; four at a time. A mutant
that breaks the test file (it no longer imports) or times out counts as killed. Each mutant may
take five times as long as the unmutated run did (at least 5 s, at most 120 s), so one that loops
forever costs seconds.

A survivor is a warning on its line, with the mutation at the end of it:

```python
        if input.party > table.seats:          > → >= survives
```

**Kill** it: `:DurandalBlock kill` on that line opens the stub at the end of the test file, the
cursor on its name:

```python
def test_name() -> None:
    raise NotImplementedError("olifant: arrange: set up what the assertion below needs")
    assert ...
```

Type the name and the assertion (`Tab` walks them), then `:DurandalBlock draft` in the test file.
The arrangement is drafted and verified both ways: the new test, and every test that passed
before, must pass on your code, and the new test must fail with the mutant in place (a sibling
test that was red before is passed over). `accept` the test, and the code block is judged again without that survivor.

**Waive** it: `:DurandalBlock waive` asks why, and the reason goes on the line, in the style of
rules' allowances:

```python
            return TableNotFound(f"there is no table {input.table_id}")  # olifant: string ok: the wording is not under test
```

The comment travels with the code, so the next judging of that line knows it. Waivers are data:
which operators people waive says which should stay in the preset.

### Jev orders the survivors

With a decision model (`[decide]`, [chapter 17](17-strangling-a-legacy-codebase.md#with-a-model-decisions-progressively)),
each survivor gets two questions in the background: *is it equivalent to the original?*
(`mutant-equivalent`: an equivalent one is dimmed, a hint rather than a warning, and listed last)
and *how much does it matter?* (`mutant-matters`: high first). The block is pushed again as the
answers land. Without one, survivors stay in file order. Jev never writes code.

## The step log

Each move appends one line to `.olifant/state/steps.log`: time, block, move, who, result.
(`olifant init` puts `.olifant/state/` in `.gitignore`; in a project made before, add it.)

```
2026-10-13T04:06:42Z	block 1	begin	person	▸ test_closes_a_table · red: NotImplementedError: …
2026-10-13T04:06:43Z	block 1	draft	qwen3-coder:30b	green
2026-10-13T04:06:44Z	block 1	test	person	green
2026-10-13T04:06:44Z	block 1	judge	mutation	1/1 killed
2026-10-13T04:06:44Z	block 1	accept	person	accepted
```

It answers what commits cannot: which lines a model drafted, which you wrote, what was waived and
why. (`test person` after a draft is the test of the draft as the editor applied it.)

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

## What works today, and its limits

- **Python only.** The operators are Python's; a language with a `[test]` table but no operators
  is begun and drafted but judged with no mutants.
- **A new test file's import is Python's, and only the function's** (or its class's): a name your
  assertion uses beyond it you import yourself.
- **A draft that fails its test is written anyway**, its last attempt marked red, for you to
  finish by hand; a model that offered nothing changes nothing. A draft sees the test file (a
  test's draft, the module) beside the rejected drafts.
- **One block per file**, and one test block in a test file.
- **The function under test is found by a method call** (`.NAME(` in the test, its assertions
  first). A test of a bare function (`assert add(1, 2) == 3`) is not located yet: begin in the
  function instead. So is the function a new test calls, once you accept it: a new test of a
  bare function leaves you to begin in the function.
- **The span can swell.** A draft that adds an import shows the span from the import line down,
  when the import merged into the file's imports.
- **An edit above the span leaves it stale** until the next test of the block: its lines are where
  they were.
- **A line with several survivors is waived together**, one reason for each operator's comment.
- **Waive and kill act on the cursor's line**, not on one survivor of several there.
- **Mutants are capped at 20** per block; a long body is judged in part.

## Mistakes

- **`no test for this module yet; write one …`** — `begin` was in a module whose test file does not
  exist, the cursor on no function. Put it in the function to have its test's stub opened, or
  write the test where the message says, then begin on it.
- **`name the new test first`** — `draft` on a stub whose name is still the stub's: type it (and
  the assertion), then draft.
- **`the block moved on`** — a draft or a test finished after you rejected or accepted the block,
  or after another move: nothing lands.
- **`every test here passes; write a failing one …`** — `begin` on a green test: a block starts
  from what is red. Begin in the function instead to judge it.
- **`cannot run: …` in the HUD** — the test file does not import or collect (a syntax error, a
  missing name). Fix it, then `test`.
- **`no model is configured`** — `draft` with no `[model]`: write the body; the block still judges
  it.
- **`the buffer changed while drafting; your text stands`** — you typed while the model wrote. Ask
  again with `draft` or `next`, or keep typing.
