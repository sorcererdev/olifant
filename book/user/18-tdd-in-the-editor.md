# 18. TDD in the editor

You build a feature in the editor as a series of small steps: a test, code that makes it pass,
a check that the tests would notice the code breaking. Olifant gives you four commands
for those steps, each one request to `olifant lsp`, each open at any time:

| command | key (durandal.nvim) | on a function, method or class | on a test |
|---|---|---|---|
| `:Durandal test` | `<leader>mt` (`<leader>mT` the whole file) | runs its test file (`tests_for`) | runs this one test |
| `:Durandal stub` | `<leader>ms` | appends a new test's stub to its test file | refused: stub from the module |
| `:Durandal conjure` | `<leader>mc` (visual: the selection) | takes one small step there, shown for review | takes one small step there, shown for review |
| `:Durandal mutate` | `<leader>mm` | mutates its lines against its test file; lists the survivors | refused: mutate from the module |

The server keeps nothing between them but the step log, and a conjure's step until you answer
it: there is no state to be in, nothing gated. You decide the order. A typical round:

1. Cursor in `handle`, `stub` (`<leader>ms`): `test_handle` opens at the end of its test file,
   with `handle`'s import, a body of `...` and `assert ...`, the cursor on the name. The **name**
   is the spec: keep it or type over it.
2. `conjure` (`<leader>mc`) on the test: the model takes a step, most likely writing its body
   from its name. The review shows its goal, the diff and the tests: `test_handle` still red
   (there is no code yet). Read the assertion; accept it, or reject it and write it yourself.
3. `conjure` in `handle`: the model takes the next step, filling `handle` so the test passes. The
   review says `+1 passing`. Accept it, edit, and press `conjure` again for the next step.
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
([chapter 12](12-holes-and-models.md)), `<leader>mc` in `handle` takes a step there; with none,
write it yourself, `<leader>mt` again, then `<leader>mm` to see what the tests miss.

## Running a file's tests

`:DurandalTest` (`<leader>mt`) in durandal.nvim runs the test under the cursor; in a module, or
in a test file outside any test, it runs the file's: for a module, its test file (the one
`tests_for` names). `:DurandalTest file` (`<leader>mT`) runs the file's tests, and
`:DurandalTest all` (`:Durandal test all`) the project's: every test file a module's
`tests_for` names, every file named as a test file, and, for a language whose tests live
inside the module (Rust's `mod tests`, where `tests_for` names the module itself), every one
of its files that holds test units. On the wire this is
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
line counts as its def; the innermost unit is the one: a definition its grammar's queries capture, or one of its `[unit] kinds`) adds a new
test to its test file, as the language's `[test.stub]` writes it
([chapter 9](09-packs-and-language-adapters.md#test-formats)). Its name is `[test] name`
over the unit's name in words: pytest's `test_<name>` in snake case (`CloseTable` is
`test_close_table`), hspec's `it "<name>"`, Rust's `fn <name>`; `_2`, `_3` when the file already
has a test of that name. The unit's import (`[imports] write`) joins the file's imports; a test
file that does not exist yet is created from the stub's `file` head (Python: the import; Haskell:
`module …Spec (spec) where`, the import, `import Test.Hspec`, `spec = describe "<Module>" $ do`).
The body is red where the language allows it: Python's `...` and `assert ...`, hspec's
`expectationFailure "write this test"`, Rust's `todo!("write this test")`. Rust's test goes at the
end of the file's own `mod tests` (opened when there is none). The test file opens with the cursor
on the test. Nothing is conjured. On the wire this is `olifant/stub` with `{textDocument,
position}`, answering `{file, line, name, edits}` (`name` as a plan anchor names the test: its
path joined by `.`, `Olifant.Foo.Bar.baz` for hspec). It is refused in one sentence: in a test file
(stub writes a test from its module), outside any function or class, or in a language with no
`[test.stub]` ("no `[test] stub` for …"). A new Haskell Spec module is not in the cabal file's
module list until `olifant sync` adds it. Each stub is a line in the step log (`stub person
<module>::<unit> stubbed <name>`).

A survivor of `mutate` has its own stub ([below](#mutating)): named from the mutant, the mutant as
a comment in its body.

## Conjuring

`:Durandal conjure` (`<leader>mc`) takes **one small step** where the cursor is, the way you
might start a line and let a completion carry on. The writer model reads the situation: the
unit at the cursor (a function, method, class or test, as the grammar's queries or `[unit]
kinds` capture it; a decorator line counts as the def it decorates), the module, its test file
or where its tests would go, how the tests stand now, the project files the module imports,
and your uncommitted changes (`git diff HEAD`, kept to the language's files, at most 300
lines). It names a small goal in one sentence and takes it: fix a syntax error, fill or
continue a body, make a failing test pass, write a missing test, or nothing when the code is
complete and green. A visual selection (`<leader>mc` in visual mode, `:'<,'>Durandal conjure`)
points it at those lines instead.

What is on screen is **your work in progress**. The writer is told to continue your intent and
keep what already works, not to replace it. Half-written lines, a comment saying what you want,
a stub: each is a hint at where you are going. Nothing is refused for red tests, a missing test
file, a green unit or a body that is not a stub. A step that leaves the tests red, or a file
that does not parse yet, is still a step: the review says so and you decide.

**The review.** The step does not touch your files until you accept it. The review window shows
the goal, which model wrote it, the diff of every file it touches (a new test file included),
whether each file parses, and how the tests moved: newly passing, newly failing (they passed
before the step: a regression), still red (failing, and not passing before: already red, new,
or unable to run before), or no tests. Its keys:

| key | what it does |
|---|---|
| `a` | accept: the step lands, one undo step per file, and your hands are back |
| `r` | reject: nothing lands |
| `h` | harder: the same step again from the harder writer (`[conjure] harder`), replacing the one shown; if it fails or has nothing to do, the step shown stays; there is no `h` without it |
| `d` | details: the prompt and the raw reply |
| `q` | leave it: the step stays; `<leader>mc` on the lines it changes reopens it while the buffer is unchanged (durandal.nvim keeps it; after an edit, `<leader>mc` asks for a new step) |

After accept nothing else is offered: edit, then press `<leader>mc` again for the next step.
Undo is the editor's (one step per file) and git's. A step whose file changed while it was
shown is not landed ("the file changed since the step was shown; your text stands"); conjure
again. A new conjure in the same file replaces the step shown there.

**Which model.** `[conjure] writer` in `.olifant/config.toml` names the backend that takes a
step; without it, `[model] default`. `[conjure] harder` names the one `h` asks. With a Claude
Code subscription and no API key, use the `claude` backend kind
([chapter 12](12-holes-and-models.md#configuring-a-model)):

```toml
[model]
default = "haiku"

[model.backends.haiku]
kind  = "claude"
model = "claude-haiku-5-5"

[model.backends.sonnet]
kind  = "claude"
model = "claude-sonnet-5-5"

[conjure]
writer = "haiku"
harder = "sonnet"
```

A name that is no backend, or a `jev` backend, is refused when the config loads. `[conjure]
wire` and `[model] rounds` are gone: a step may write a port and its adapters itself.

**What is kept.** Every answered step adds one line to `.olifant/state/conjure-steps.jsonl`:
the time, file, unit, writer, goal, files, lines changed, the tests' movement, whether every
file parses, seconds, cost, and your choice (`accepted`, `rejected`, `replaced` by a harder
step, or `dropped`). These lines are how the prompt and the choice of model get better from
real use.

On the wire this is `olifant/conjure` with `{textDocument, position, selection?}` (the
selection's `start` and `end` lines, 0-based, inclusive), under `$/progress` `conjuring`. It
answers `{shown: {token, file, unit, writer, goal, edits, dropped, parses, tests: {passing,
failing, stillRed, noTests}, after, cost, seconds, harder, diffs, situation, reply}}` (`edits` a workspace edit over
every file the step writes; `after` the after-run as `olifant/tests/run` answers it), or
`{nothing: goal}`. The answer is the same request with `resume: {token, choice}`, `choice` one
of `accept` (answers `{landed: {file, edits}}`), `reject` (`{rejected: true}`) or `harder` (a
new `shown`). A token answers once.

## Mutating

`:Durandal mutate` (`<leader>mm`) mutates the lines of the innermost function, method or class at
the cursor against its module's test file, and lists the survivors. The test file runs first, as
the editor has it (unsaved text is what is mutated and run): the tests that pass there are the
judges. A mutant one of them fails is **killed**; one they all pass **survives**. The unit's lines
only are mutated, at most 20 mutants, in file order. Nothing is stored or gated. The operators
are the language's `[[mutate]]` tables; a language without any answers `no [[mutate]] operators
for <language>: add them to .olifant/languages/<language>.toml`.

Each operator is a name, the ast-grep `pattern` (or a whole `rule`) of the code it breaks, and
what that code becomes: a `fix` (ast-grep's, using the pattern's metavariables) or
`transform = "increment"` (an integer plus one):

```toml
[[mutate]]
name = "boundary"
pattern = "$A < $B"
fix = "$A <= $B"

[[mutate]]
name = "number"
rule = { kind = "integer" }
transform = "increment"
```

Python ships seventeen (in `library/languages/python.toml`): `boundary` (`<` ↔ `<=`, `>` ↔
`>=`), `equality` (`==` ↔ `!=`), `negate` (a condition), `and-or`, `arith` (an arithmetic swap),
`number` (+1), `string` (→ `""`, not a docstring), `return-none` (`return x` → `return None`),
`delete-call` (a call statement → `pass`). A project adds its own, or replaces the list, in
`.olifant/languages/python.toml`; a later layer's `[[mutate]]` replaces the earlier one's whole.

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
(`if input.party > table.seats:` → `test_boundary_party_seats`), between the text the language's
`[test] name` puts before and after the words (pytest's `test_`), with `_2` when the test file has
a test of that name already (by its `[test] unit` rules). The mutant is a comment above the `...` in its body, which `conjure` reads as a
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
options changes nothing) and the decision model never writes code. Any backend from
`[model.backends]` can answer them, a local Ollama model included; a key is needed only if that
backend needs one. A `kind = "jev"` backend ([chapter 12](12-holes-and-models.md#configuring-a-model)),
TypeSafe AI's Jev, is the decision model the questions are shaped for: it answers them with
weights, and cannot write code.

## The step log

Each command appends one line to `.olifant/state/steps.log`: the time, the command, who, what
it was on, and its outcome, tab-separated.
(`olifant init` puts `.olifant/state/` in `.gitignore`; in a project made before, add it.)

```
2026-10-18T04:06:40Z	stub	person	src/bistro/booking/commands/close_table.py::handle	stubbed test_handle
2026-10-18T04:06:45Z	conjure step claude-haiku-5-5 · write test_handle from its name · 1 file · +0 tests · accepted
2026-10-18T04:06:47Z	mutate	person	src/bistro/booking/commands/close_table.py::handle	1 survivor · 5/6 killed
```

A conjure's step logs one line when you answer it:

```
2026-11-16T22:10:41Z	conjure step claude-haiku-5-5 · fill handle so it closes a table · 1 file · +1 tests · accepted
```

It answers what commits cannot: which lines a model conjured, and which you wrote.

## Configuring tests: `[test]`

A language adapter says how to run its tests and where a module's tests live:

```toml
# library/languages/python.toml, under packs/python-htmx/languages/python.toml
[test]
preset    = "pytest"
tests_for = "{= dir =}/{= stem =}__test.py"
```

- **`preset = "pytest"`**: pytest as the project runs it, its JUnit report read for which test
  failed, where and why. With a `uv.lock`, through `uv`, in Olifant's own venv for the project;
  else the project's `.venv/bin/pytest` or `venv/bin/pytest` (a poetry or pip venv); else
  `pytest` from PATH. You never name the venv. `olifant doctor` says which it chose and why
  (`test runner .venv/bin/pytest (python) — …; .venv/bin/pytest found`). A test's id is
  pytest's: `path::Class::test_y` for a method of any class (a Django `TestCase` subclass too).
- **`tests_for`**: the test file of a module, `{= dir =}` its folder and `{= stem =}` its name
  without the extension. `close_table.py`'s tests are `close_table__test.py`.

When no preset fits, `command` (with `{= tests =}` for the test ids and `{= root =}` for the
package folder on disk, since tests run in a scratch copy; the copy holds the project's
templates, fixtures and other files from disk as well, so a Django view test finds its templates
with nothing listed), `format` (`pytest`), `context` and
`root` are the overrides, as for `[check]`; `command` may be a list of candidates, each used when
its `when` file exists ([chapter 9](09-packs-and-language-adapters.md#test-command-candidates)). A generator's hole may
say `verify = "tests"` to be verified by the module's tests ([chapter 12](12-holes-and-models.md)).
`olifant doctor` has a line for the runner (`test runner uv (python)`).

The preset leaves the project's pytest `addopts` alone: they apply to Olifant's runs too. When
they parallelise (`-n 8` with pytest-xdist) or slow a single test's run, a project layer can turn
them off for Olifant:

```toml
# .olifant/languages/python.toml
[test]
preset  = "pytest"
command = ["{= root =}/.venv/bin/pytest", "-q", "--tb=short", "-p", "no:cacheprovider", "-p", "no:xdist", "-o", "addopts=", "{= tests =}", "-o", "junit_family=xunit1", "--junitxml={= report =}"]
```

A `command` of your own keeps the preset's `format`, so name `{= report =}` as above (pytest
writes the JUnit report there). A `command` that names none (an older recipe with `-rA` and no
report) still works: Olifant then reads pytest's console output as before, and `olifant doctor`
says "reading console output: name {= report =} for JUnit". Set `format = "pytest"` to say so
yourself.

`-o addopts=` drops all of them, the ones the tests need too (pytest-django's `--ds=…`,
`--reuse-db`): put those back after it.

## Limits

- **Mutation ships for Python only.** Another language mutates once its `[[mutate]]` operators
  are written; until then `mutate` says `no [[mutate]] operators for <language>`.
- **A stub's import is only the unit's** (or its class's), by `[imports] write`: a name your
  assertion uses beyond it you import yourself. A file importing it already (on one line or several, in
  brackets) gains nothing; one importing from its module gains the name there.
- **A conjure's step sees** the module, its test file, the project modules the file imports
  (and what those import: at most 8 files, 120 lines each) and your uncommitted changes (300
  lines). The writer answers whole files, so a very large module costs more per step.
- **Mutants are capped at 20** per mutate; a long unit is judged in part.
