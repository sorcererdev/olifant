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
line counts as its def; the innermost unit of the language's `[unit] kinds` is the one) adds a new
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
`start` and `end` lines, 0-based, inclusive) and an optional `resume`, answering `{file, unit,
edits, summary, green, tests, rounds, rungs}` (`edits` a workspace edit over every file the conjure wrote, null when it changed nothing;
`tests` the after-run as `olifant/tests/run` answers it; `rounds` the rounds used and `rungs` the
models tried, in order), under `$/progress` `conjuring`. A steered conjure that pauses answers
instead `{paused: {…}}`, every fact the pause stands on, so the editor can show it without asking
again:

- `token`, `file`, `lines: {first, last}` (1-based), `round`, `rung`, `summary` (the last round in
  a few words) and `why` (the first sentence of the diagnosis, for a one-line notification);
- `title: {unit, round, of, rung, model, seconds}`: the unit as `Class.method` (or the function),
  the round of the grant's `of`, the rung and its backend's model, the seconds used;
- `diagnosis`: one to three sentences the engine writes from the facts below (no model writes
  them), the cause's first; `trigger`: what caused the pause in one line (`repeat`, `trade`,
  `budget spent`, `unsatisfiable test: <id>`, `port not derived`, or Jev's distribution as
  `Jev: ask .68 · again .29`, then `; wire failed: …` when a wire failed);
- `rounds`: every round as `{round, rung, attempt, line, verdict, failing, failed, passed,
  decide}`: the attempt's body, its first line (or `(the same as round N)`), `red` or `green`,
  the failing tests' short names, each failure as `{test, message, summary, at, assertion, got,
  expected, output}` (pytest's whole `E` block, its one line, the line it failed at, the
  assertion's source line, the two sides of an `assert a == b` or an `isinstance`, null for any
  other shape, and the test's whole section), the tests that passed, Jev's answer `{choice,
  confidence, probabilities}` or null, and the round's change: `diff`, the unit's file with that
  round's body against the buffer, and `files: [{file, diff}]`, every file keeping that round
  lands (the unit's file first, then the wire's);
- `tests`: every test involved (failing now, or failed in any round) as `{test, source, status,
  flipped, assertion, note}`: `failing` or `passing`, the last round its status was otherwise
  (0 the run before round 1), the assertion's line, and the scan's note on a fake it never
  hands in;
- `unit: {header, ports}`: the class and its `__init__`, and the ports it takes as `param: Port`;
- `landing: {diff}`: what `keep round N` would land, as a unified diff against the
  buffer; `wired`: each wire as `{param, port, test, files: [{file, diff}]}` (`test`: the flagged
  test the first wire ran for);
- `offers`: the rows in order, each `{choice, arg, label, detail, land, hint}`: what to send back
  (`choice`, and `arg`, `land` or `hint` when the row needs one), a label and what the row will do
  in numbers (rounds, rung, what lands where);
- `expires`: the seconds the pause is kept; `proposal`: `{file, test, before, after, diff,
  refused}`, present only while a re-conjured test waits for you, or after a refused one (then
  `refused` says why: ``it adds `<error>` at line N``);
- `outcome`: what your last choice came to, when the pause would not show it otherwise (a refused
  re-conjure: `Your re-conjure of not_found was refused: it adds … at line 26.`), else null.

To answer it, ask again with the same `textDocument` and `position` and `resume: {token, choice,
arg?, land?, hint?}`, as the offer's row says: `choice` is `"continue"`, `"climb"`, `"stop"`,
`"reconjure"`, `"accept"`, `"reject"`, `"wire"` or `"port"`; `arg` a re-conjure's test id, a wire's
`"param: Port"` (needed for `wire`), a port method's `"Port.method"` or the round a keep lands (`"2"`; `"0"` the wire alone; the pause's round when
absent); `land` a boolean for `stop`, false when absent; an empty
`hint` counts as none. The answer is a landed or a paused result again, and a token works once,
except that `reject` and a refused resume keep it. A resume without a token and a choice, a `land`
that is not a boolean, or a `wire` without its `arg`, is `InvalidParams` (`olifant/conjure resume
needs a token and a choice (continue, climb, stop, reconjure, accept, reject or wire)`; the text omits `port`, which is accepted). These are
refused in one sentence and the pause stays: a `wire` the pause does not offer (`this pause offers
no wire for <arg>`), a `reconjure` of a test it does not offer (`this pause offers no re-conjure of
<arg>`; with no `arg` the first offered test is re-conjured), a `stop` with `land: true` and no
body to keep (`there is no body to keep`, or `there is no round N body to keep`), and `accept` or `reject` with no proposal (`there is no
proposal to accept`). A landed answer carries `stopped`: true when the person stopped it.
A resume is also refused when the pause is gone (`this paused conjure is gone (it expired, was
resumed, or its file changed): conjure again`), when the file changed since (`the file changed
since the conjure paused; conjure again`), and on a climb with no stronger rung (`there is no
stronger model in [model] ladder`). It is refused in one sentence outside any function or
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

Some pauses need no question either; the engine decides them in code, in this order, after the
run before round 1 and after every red round. The **scan** reads each failing test for a fake it
builds (`Mock()`, `MagicMock()`, a `Fake…`, `InMemory…` or `Stub…` class) and never hands to any
call. A flagged test whose class lacks the fake's port is **wired** (below), silently, when a wire
generator applies and the port and its parameter can be named; when they cannot, it pauses
(`port`) and offers a `wire` row for each of the two likeliest ports. A flagged test still failing
when no wire can help (none applies, one ran, or the class takes the port already) is a test no
body can pass: it pauses at once (`test <id>`). The same body twice pauses (`repeat`); bodies that
pass one test and fail the other pause (`trade`); a spent budget pauses (`budget`). Only then is
Jev asked, and only whether the model is converging: it sees every round's first line and failing
tests, the failures, the tests passing now, every involved test's source, the unit's header, and
whether the rounds trade or repeat. An `ask` at 0.5 or more pauses, a `climb` at 0.6 or more
climbs, anything else is `again`.

**A missing port.** With `[conjure] wire = "<generator id>"` in `.olifant/config.toml` (the
python-htmx pack's is `wire`) and a unit whose path matches that generator's `applies_at` path,
a flagged test's port is wired inside the conjure, before round 1: the import, the `__init__`
parameter (added when missing), `self.x = x`, the test helper's fake and `main.py`'s adapter. The
port is the one the fake's class names (`InMemoryBookingRepository` → `BookingRepository`), else
the one whose last word the fake's name ends in (`repository` → `BookingRepository`), else the
service's only port, else Jev's pick among the service's ports (Protocol classes under
`src/{app}/{service}/ports/`) at 0.6 or more; the parameter is the name the sibling commands and
queries give that port in their `__init__` (`bookings`), else the fake's name. The conjure goes on
over the wired files; every file it wrote lands with it, as one edit and one undo step across
files, and the pause lists what was wired, each file as a diff. One wire per conjure; a wire that
writes nothing is not tried again.

**A missing method.** When the check says a port the unit takes has no such method (pyright's
`Cannot access attribute "close" for class "BookingRepository"`, by the pack's `[check]
missing_member`), the conjure pauses (`a port lacks a method: BookingRepository.close`). If the
signature follows from the call and the test's fake (written in the language's own words by
`[types]`) and the pack names a generator for it (`[conjure.port] add`,
[chapter 9](09-packs-and-language-adapters.md#conjures-shapes-conjure)), the pause offers `add close to
BookingRepository`, with the signature in its detail; if not, it says to add the method yourself,
and the signature is not guessed. Choosing it (`port`, `arg` `Port.method`) has that generator
(python-htmx's `port-method`) add the method to the port, then the one `[conjure.port] adapters`
names (python-htmx's `port-method-adapters`) add it to each adapter beside the port (the model
writes their bodies) and the contract test's stub, and answers a **proposal** like a re-conjured
test's: the diff of every file, to `accept` (they land with the conjure) or `reject`. The step log
has `conjure-proposal Port.method`, or `conjure-port-failed Port.method` when it could not.

**A wrong test.** Every pause offers to re-conjure each involved test, the suspect first (a
flagged test, else one failing now). **reconjure** has the model write that test again, told why
it was wrong, that the helper now takes the wired port (when one was wired), and your hint, if
any, and answers a **proposal**: the test file before and after, with a unified diff, and offers
`accept` and `reject`. **accept** writes the new test into the conjure's working files and goes
on; it lands with the rest. **reject** returns the pause as it was. Nothing touches your buffer
before the conjure lands. The re-conjure is refused only for what it adds: the errors the test
file already had (pyright's, matched by file and message, so a line moved does not count) do not
fail its check, and the model is told of them (`the test already has these errors; fix them if you
touch those lines`). Like any test conjure it is asked again with its verify's reason, up to
`[model] retries` more times. When it is still refused, the pause comes back with `outcome` saying
so first; the refused proposal is kept, offered as `see the refused proposal` (choice
`proposal`, answered in the editor; `accept` lands it anyway, as your choice), and the row
`fix the test: <test>, again` takes the re-conjure's place. The re-conjure is told the test's note
as why it is wrong (the pause's first sentence when the scan has none). An accepted proposal is
named in the landing: `· not_found rewritten`.

A conjure you start by hand on a test is scanned after its fill. A test that builds a fake for a
port its class lacks has the port wired as above, before the test lands, and the test is filled
once more over the wired files. The summary ends `<param> wired` (and `; the test still
builds its own fake, re-conjure it` when the second fill still does); with no wire generator it
ends `needs a port the class lacks: <param>: <port>`.

While a steered conjure runs, its progress names the round: `round 3 of 8 · asking local
(qwen3-coder:30b)…`, `round 3 · testing…`; a re-conjure's `fix the test: not_found · asking
local…`. A pause answers `conjure · round 4 · red: test_handle · assert status == 404` with what you may
do. **continue** grants a fresh `rounds` and `budget` on the same model. **climb** moves to the
next rung, carrying the attempts so far into the stronger model's prompt. **wire** runs the
offered wire, then goes on as continue. **stop** lands nothing: your text stands, the wire's
writes are dropped, and the answer says `nothing landed`. **keep round N** lands that
round's body (the last round's unless `arg` names another) and the wire's files as one edit and
one undo step, red and unsaved, and says `kept round N · red: not_found · bookings wired`; `arg`
`"0"` keeps the wire alone (its files, your body standing: `kept the wire alone · …`), refused
`there is no wire to keep` when none ran. The rows say each choice in plain words (`continue`,
`continue with a hint…`, `climb to claude`, `wire bookings: BookingRepository`, `fix the test:
not_found`, `stop`, `keep round 2`), and their details what follows. A **hint**
continues with your words added to the next prompt as "The person says: …". A pause is forgotten
after 10 minutes, or as soon as its file changes in the editor; conjure again then. Without
`[decide] model` nothing of this happens: the model's `retries` run as before and the last attempt
lands. A steered conjure that goes green says `green in round 4 · local · bookings wired ·
not_found rewritten` (the round, the rungs, then what was wired and rewritten).

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
2026-10-18T04:06:42Z	conjure	person	src/bistro/booking/commands/close_table__test.py::test_handle	conjured by qwen3-coder:30b · red at the assertion
2026-10-18T04:06:45Z	conjure	person	src/bistro/booking/commands/close_table.py::handle	conjured by qwen3-coder:30b · green
2026-10-18T04:06:47Z	mutate	person	src/bistro/booking/commands/close_table.py::handle	1 survivor · 5/6 killed
```

A steered conjure (one with `[decide] model`) adds a line for each of Jev's choices (the choice,
its confidence or `-`, the round and the rung), one for a pause (its cause: `test <id>`, `port`,
`repeat`, `trade`, `budget`, `ask` or `climb`), and one for a resume (the choice only; a hint's
words are never logged). The scan logs `conjure-test flagged <test id> <variable> <class>` once
for each test it flags (and `conjure-test ok -` for a hand-started test conjure that passes its
fakes); Jev's pick of a port `conjure-port <choice> <confidence|->`. A wire adds `conjure-wire
<generator> k=v …` (the inputs it ran with) or `conjure-wire-failed <generator>`; a proposal
`conjure-proposal <test id>`; a failed re-conjure `conjure-reconjure-failed <test id>`; a port method
`conjure-proposal Port.method` or `conjure-port-failed Port.method`.
`conjure-resume` takes `reconjure`, `accept`, `reject`, `wire` and `port` besides `continue`, `climb`,
`stop` and `keep N` (a stop that keeps round N's body; `keep 0` the wire alone). A bare stop's `conjure` line says `stopped ·
nothing landed`; a keep's `kept round N · red: …`.
Its `conjure` line names the rungs and the rounds:

```
2026-10-22T21:10:03Z	conjure-next	again	0.90	1	qwen
2026-10-22T21:10:09Z	conjure-pause	budget
2026-10-22T21:10:30Z	conjure-resume	climb
2026-10-22T21:10:41Z	conjure	person	src/bistro/booking/commands/close_table.py::handle	conjured by qwen → claude · 3 rounds · green
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

- **Mutation ships for Python only.** Another language mutates once its `[[mutate]]` operators
  are written; until then `mutate` says `no [[mutate]] operators for <language>`.
- **A stub's import is only the unit's** (or its class's), by `[imports] write`: a name your
  assertion uses beyond it you import yourself. A file importing it already (on one line or several, in
  brackets) gains nothing; one importing from its module gains the name there.
- **A conjure sees** the test file (a test's conjure, the module), the project modules either one
  imports, and what those import (ports, models; at most 8 files, 120 lines each).
- **Mutants are capped at 20** per mutate; a long unit is judged in part.
