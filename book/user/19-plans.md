# 19. Plans

A plan is a markdown file in which you write what you believe about the code and what you mean to
do, in lines the engine can check. You ask the model about it: an `ask`'s answer goes straight
into the plan (one `u` takes it out), a `validate`'s lands as ghost blocks you accept or reject. Plans live under `plans/` of the project (`[plans] dir`);
durandal.nvim treats a markdown file there as a plan, and any other buffer after
`:Durandal plan`.

## The grammar

Prose is ignored. Lines are read by prefix:

| line | shape |
|---|---|
| claim | `- <text> @path:Unit`, then the verdict: `✓ <7hex>` verified at that fingerprint, `✗ stale · why`, `? not found` |
| question | `- ? <text>`: what `ask` answers |
| step | `- [ ] <text> @path:Unit`, or ending `· by hand`; `- [x] <text> @path:Unit ✓ <7hex>` when done, at that fingerprint (an open step may carry the one its tests last ran at); a step may nest |
| section branch | `## <title> · branch <name>`: when that branch is checked out, the section's open steps are offered first |
| test child | `  - test: <sentence> @file:name`, then its state: `○` pending, `●` green, `✗` red |
| also child | `  - also @path:Unit`: units a `do` touched beyond the anchor |
| evidence child | `  - ✓ <evidence> @path:Unit`, `  - ✗ <why>` |
| changed child | `  - ✗ changed: <why> @path:Unit` |
| gone child | `  - ✗ gone: @path:Unit no longer exists` |
| red-again child | `  - ✗ <test> red again` |

An anchor is `@path:Unit`: a path relative to the project root, and a `Name` or `Class.method`.
Python and Haskell units resolve. A path that is absolute or leaves the project is never read: it
is `? not found`. The fingerprint is a short hash of the unit's text, blind to formatting: indentation, blank
lines and a formatter's reflow (a call split over lines, a trailing comma it adds, leading commas)
leave it as it was. A `✓` whose unit has since changed is shown stale. A line inside a code fence is ignored.

A test child's anchor names a test by its dotted name. In Python that is the test (`test_x`,
`TestX.test_y`), as pytest names it. A language whose `[test]` has `unit` patterns
([chapter 9](09-packs-and-language-adapters.md#test-formats)) names tests and their groups by
those: in Haskell, an hspec `describe` or `it`, its enclosing `describe`s joined by `.`.
Anchoring a `describe` runs every test in it, and the child is green when they all pass:

```markdown
- [ ] parse a plan @src/Olifant/Plans.hs:parsePlan
  - test: plans parse @src/Olifant/PlansSpec.hs:Olifant.Plans
```

`Olifant.Plans` is `spec = describe "Olifant.Plans" $ do` in that file. `Olifant.Plans.parsePlan`
is the `describe "parsePlan"` inside it. A test name with spaces or punctuation
(`it "(a) reads a step"`) cannot be an anchor's name, so a step anchors the `describe` around
it. A name the file does not have says so: `Nope is not in src/Olifant/PlansSpec.hs`.

## How a plan reads

The file stays exactly as the grammar has it: `cat`, `git diff`, GitHub and another editor all show
the same text. The engine says what each span of a plan is (standard semantic tokens, a legend of
`plan…` types) and which ranges fold (standard folding ranges); the editor draws it. In
durandal.nvim, off the cursor line:

- **Markers are glyphs.** `- [ ]` is `☐`, `- [x]` is `☑`, `- ?` is `?` in the question colour,
  `- test:` is `·`, `- also` is `+`. The dash of a claim, an evidence line or a journal line, the
  `@path:` of an anchor and the 7-hex after a `✓` are hidden; evidence keeps its `✓`/`✗`. The
  unit's name stays, and `gd` on it goes to the unit as before.
- **Evidence recedes.** Evidence under a question (an anchored claim), `also` lines and journal
  lines are dim; a conclusion (an unanchored claim under a question) is plain text; a question
  stands out. A `✓` whose unit has changed since is dimmer still.
- **Done work folds.** A ticked step with children and an answered question (one with an
  unanchored child under it) are closed when you enter the plan. A question's fold line is the
  question, ` — `, and its conclusion (the last unanchored child, so a note you add later wins),
  cut to the window, then `▸ N` hidden lines; a ticked step's is `☑`, its text, its unit and `✓`.
  A question with only evidence has no conclusion yet, so it stays open. Open steps always read in
  full. Folds close on entry only: one that forms while you look (an answer landing under a
  question, a `do`'s journal lines, a tick, a review's `a`) starts open and stays open until you
  next enter the plan. `zo` opens a fold, `zR` opens all, and `<leader>mpe` toggles the fold under
  the cursor.
- **The cursor line is raw.** On the line you are on, in every mode, the file's text shows, so
  what you edit is what the file has. Moving off it hides the markers again; a line shifts left by
  up to five columns between the two.
- **Diagnostics are signs.** A plan line is prose, so olifant's diagnostics in a plan buffer are
  signs and underlines on the anchor, with no virtual text beside the words. `changed since this
  was verified` is raised only for a claim of your own outside any question; evidence under a
  question and steps raise none (a step answers with the loop, and the dimmed `✓` is the evidence's
  signal), and inside a ticked step only `? not found` is.

```lua
plans = {
  conceal = true,   -- false: keep the colours and folds, hide nothing
  glyphs = {},      -- override: todo, done, question, test, also, dash, path, hex
  fold_done = true, -- false: on entry close only excerpts, leave done work open
}
```

Colours are the standard `@lsp.type.plan…` groups, linked with `default` so a colourscheme
wins: `planEvidence`, `planAlso` and `planJournal` to `Comment`, `planQuestion` to `Title`,
`planAnchorUnit` to `Identifier`, and the `stale` modifier to `Comment`.

## What the server does

- Diagnostics: a missing anchor (`? not found`), and `changed since this was verified` on a claim
  of your own whose unit has changed. Evidence an `ask` left under a question, and steps, raise no
  `changed since`: the evidence's `✓` is dimmed instead and the step answers with the loop below.
- `@` completes files, then units. `gd` on an anchor goes to the unit (`textDocument/definition`).
- Three errands, each one `claude -p` run from the project root, never more than one `do` at a time:
  - **ask** (on a `- ?` line): answers with claims that carry anchors the index resolves.
  - **validate** (on a claim, or a `##` heading for the section): checks claims against the
    units' code and answers verdicts and evidence.
  - **do** (on a step or test child): does the step in the repository, with edit rights and the
    project's test command, which its prompt names exactly (no pipe, no `cd`) as the one shell
    command it may run, the path as you configured it (an absolute program inside the project
    is allowed by its relative path too). Its prompt says up front that files change only through
    its Edit and Write tools, never a shell heredoc, `cat >`, `sed -i` or `python -`. It is told Olifant
    runs the step's tests after it, so tests it could not run are no reason to answer `BLOCKED`. A
    `DONE` of the step's own unit is no `also` line. In a git repository a `do` ends in a review ([below](#reviews-and-the-journal)): its diff, and `revert` to undo it.
  The prompt holds the plan and the bodies of the units it anchors. The answer is read against the
  grammar: lines outside it are dropped, anchors that do not resolve are kept as `? not found`,
  and both are counted in the notification. An answer with nothing in the grammar is an error.
- Every errand that reaches the runner, however it ends, appends one JSON line to
  `.olifant/state/errands.jsonl` (ignored by git): its id (the runner's session when it answered),
  when it started, the verb, plan, row and section, the branch, the model, its status and why, the
  session, seconds, turns, tokens, cost and per-model use, the prompt's length and the units
  grounded, and the blocks, lines, dropped lines, unresolved anchors, changed files and permission
  denials of its answer. The status is `ok`, `empty` (answered, nothing in the grammar), `error`,
  `timeout` or `cancelled`. A line the verb refuses writes nothing. The answer carries the same id.
- `.olifant/state/steps.log` keeps a readable echo of each: `plan-ask`, `plan-validate` or
  `plan-do`, the plan and line, lines returned, anchors resolved, seconds, cost, model and status.
  Measured on a clone of the demo: `plan-ask` 4.1 s, $0.054; `plan-validate`
  10.7 s, $0.077; `plan-do` 5.3 s, $0.060 (model `sonnet`).

## `[plans]`

In `.olifant/config.toml`, all optional:

| key | default | meaning |
|---|---|---|
| `dir` | `"plans"` | the folder, under the project, of plans (durandal.nvim's `plans.dir` must match) |
| `runner` | `"claude"` | the command run; `CLAUDECODE` is unset for it |
| `model` | `"sonnet"` | passed as `--model` |
| `timeout` | `300` | seconds before an errand is killed |
| `do_tools` | `[]` | extra `--allowedTools` entries for `do` (beyond Read, Grep, Glob, Edit, Write and the test command) |
| `ladder` | `[model] ladder` without Jev | the backends a reconcile climbs when Jev is unsure, in order, each with its `escalate` policy |
| `budget` | `[model] budget` (180) | seconds per reconcile, counted from its start and checked before each question; past it, the units not yet asked about are only counted (`more` in the reconcile's answer and its record) and the next reconcile asks again |
| `prices` | the current models at list price | `[plans.prices."<model id>"]` rows for `plans cost`, below |

An unknown key under `[plans]` is a configuration error.

## The loop

A plan mirrors the tree. Each open step is in one of three states: done (the work it describes is
in the tree), in progress (part of it landed) or diverged (the tree holds work no step describes,
a step's unit is gone, or a done step's tests are red again). The engine finds out by
reconciling, request `olifant/plans/reconcile`.

**When it runs.** On entering a plan buffer, and after any tool landing while a plan is open (a
test run, a stub, a conjure, a `do`, an accepted ghost), and one second after a save of any other
file (each plan shown in a window; the files as saved). `<leader>mpc` forces one, and runs the
tests of the steps it finds touched. An automatic reconcile runs no tests.

**What it compares.** The units changed since the baseline (the merge-base with the default
branch; on the default branch itself, the commit that added the plan) against what the plans under
`plans/` already carry at their current fingerprints. A changed unit no plan accounts for, and any
step whose unit changed (done ones too), is looked at. Nothing changed: no model call. The same
situation again replays from a cache. A reading you left or ignored does not return until its unit changes.

**Facts land, readings wait for review.** Facts are what the engine can check alone, and they are
written straight into the buffer in one undo step: a tick when the step's tests are green, the
test states `○ ● ✗` (a run of all a step's tests stamps it `✓ <fp>`, open or done: the fingerprint
the marks were made at), `○` on the children of a step whose unit changed since that stamp, a
re-anchor when a unit only moved.
Readings are a model's judgement, and wait in the plan's reconcile review ([below](#reviews-and-the-journal))
for you to apply with `<leader>mpr`: an `also` child placing a unit under a step, a step line ticked (only a step with
tests: a reading never ticks one without) or unticked, a `✗ changed: <why>` child, a `✗ gone:`
child, a new step for work nothing covers.

**Who decides.** Jev (`[decide] model`) answers two questions: `plan-step` (which open step is a
changed file for, or none; one question per changed file) and `plan-state` (done, in progress or
diverged; for a done step without tests, still done, reopened or diverged). A `done` for an open
step with no test children is taken as in progress: no tick reading. A step with tests keeps its
tick reading, and its tests tick it. Below the gate (0.6)
the same question climbs `[plans] ladder`, else `[model] ladder` without Jev, each backend under
its `escalate` policy: an automatic reconcile climbs `auto` rungs only, `<leader>mpc` also `ask`
rungs, and `never` is never. Words (a `<why>`, a new step's line) come from the first allowed chat
rung in one call. Without one (none allowed, the call failing, or the budget spent) a new step's
line is the unit's header and a `<why>` is "<unit names> changed". A reconcile asks for at most
`[plans] budget` seconds from its start.

```toml
[plans]
ladder = ["local", "sonnet"]   # backends of [model.backends]
budget = 120

[model.backends.local]
escalate = "auto"     # falling back to local needs no keypress
[model.backends.sonnet]
escalate = "ask"      # only <leader>mpc climbs here
```

A backend with no `escalate` key defaults to `ask`, so only `<leader>mpc` climbs to it. (The backends' other keys are as in chapter 12.) The demo's config sets `escalate = "auto"` on its local backend for that reason.

**Tests on a step.** `<leader>ms` on a step stubs a test under it (any language with a `[test.stub]`: Python, Haskell, Rust; the step needs an anchor), the step's text as the
sentence. `<leader>mt` runs the step's tests and ticks it when all are green; a step with no
tests is ticked by keeping its `do` (below) or by `<leader>mpx`. A `do` runs the
step's tests when it finishes, over the files it wrote as they are on disk (the editor reloads
them only afterwards).

**The record.** Every reconcile that asks or answers something (one with nothing to say, or the same situation as last time, writes none) appends a `reconcile` line to `.olifant/state/errands.jsonl`: `id`,
`at`, `plan`, `branch`, `baseline`, `forced`, `changed`, `unaccounted`, `more`, `facts`, `seconds`,
`climbs` (rung and seconds), `decisions` and `blocks`. The accept or reject outcome of a reading or of an errand's ghost block refers to
the reconcile's `blocks` by index.

## Reviews and the journal

**A `do` is reviewed.** Before a `do` the engine takes a reference to the working tree (`git stash
create`, or HEAD when nothing differs; untracked files too). When the `do` answers, or changes the
tree however it ended, the change since that reference becomes a **review**, kept under
`.olifant/state/reviews/<plan>/` until you answer it. The window shows the step, what the model
said, the tests after it and the commands it was refused, in words, beside the diff file by file
(`[` `]`). A refused command over several lines (a heredoc) shows as its first line and a count,
and any row longer than 3 lines is cut there with `… N more lines (d details)`; `d` has them
whole. The answers are:

| key | choice | does |
|---|---|---|
| `a` | keep | the `do`'s plan lines land under the step; the review goes. A step with no test children (none in the plan, none the `do` adds) is ticked by the same edit, as `<leader>mpx` would (`✓` its unit now, a live excerpt frozen; one undo step), and the journal says `kept the do's change · ticked: the step has no tests`. A step with tests is left to its tests |
| `r` | revert | the files are restored from the reference, new ones removed; your edits to those files since the `do` go with it, and the plan, `.olifant/state/` and `.olifant/config.toml` are left alone |
| `o` | reopen | keep it and ask for more in the same thread: a sentence, and a `do` on the step resumes the model's session in a fork; it comes back as a new review |
| `t` | test | run the step's tests now |
| `p` | allow | add the commands it was refused to `[plans] do_tools` in `.olifant/config.toml` for future dos |
| `d`, `q` | | details (prompt, answer, diffs); later (the review stays, with a lens on its step) |

`<leader>mpr` (or the lens) opens the review at the cursor's step, else the plan's first. A repository
with no git has no review: the `do` keeps its ghosts as before. Every `do`, however it ended, is
kept under `.olifant/state/errands/<plan>/`; a step with past dos has a lens `N delegations`.

**Readings are a review too.** The reconcile's readings (a step looks done, open again or wrong,
a unit is gone, a changed unit is in no step) do not appear as ghosts in the buffer: they wait in
**one review per plan**, under the title lens `N readings to review`, and a lens on each step with
a reading. `<leader>mpr` opens it: one line per reading, its plan edit over its unit's diff. `a`
applies it, `r` leaves it, and for a unit in no step `1`-`9` place it under that open step, `n`
makes it a new step and `i` ignores it until the unit changes again. A reading you answered is not
offered again; one you left or ignored comes back only when its unit changes.

**The journal.** The engine writes a dated line under a step when something happens to it, once
each:

```markdown
- [ ] a table closes @src/bistro/booking/commands/close_table.py:CloseTable.handle
  - 25 Oct 21:31 · do (sonnet) · answered: 1 file changed
  - 25 Oct 21:40 · kept the do's change
  - 25 Oct 21:41 · tests red: test_closes_a_table
```

A `do` ending (`answered`, `blocked`, `answered nothing`, `error`, `timed out`, `cancelled`), a review
answered (kept, kept and ticked, reverted, kept and asked for more), a step's tests ticking, unticking or going red or
green. Delete a line you do not want and it is not written again. The store is
`.olifant/state/journal/<plan>.jsonl`. A line you write in the same shape reads as a journal line.

## What errands cost

`olifant plans cost [--by plan|branch] [--plan PATH] [--branch B] [--since YYYY-MM-DD] [--json]`,
run in the project, sets the errand records beside Claude Code's own transcripts of the project
(`~/.claude/projects/<slug>/`, subagents' too). By default it is one line per plan and, under it,
one per landing, the PR the work reached. A sample from a real project, shortened:

```
plan / landing                                             errands  reconciles  reconcile s  accepted     errand $  sessions  session $  total $
plans/raw-votes.md                                              37          72         14.3  33/35 (2 ?)      4.95         0       0.00     4.95
  #126 Migration to add columns for 2018 results                11           3          0.0  9/11 (0 ?)       0.92         0       0.00     0.92
  #127 1 - Fix a migration conflict                              1           3          0.0  1/1 (0 ?)        0.09         0       0.00     0.09
  #133 Add toggle for 18-24 columns                              4           8          0.0  4/4 (0 ?)        1.07         0       0.00     1.07
  (not landed)                                                   0           9         14.3  0/0 (0 ?)        0.00         0       0.00     0.00
```

A landing is a commit on the first-parent line from `HEAD` that touches the plan file; a record
(an errand or a reconcile) belongs to the first landing of its plan at or after it, and a session
to the first landing of any plan at or after its message. So squash merges, merge commits and plain
commits all count, and nothing needs recording at merge time. A landing is labelled `#N` when its
subject ends in `(#N)` (a GitHub squash) or starts `Merge pull request #N`, else by the short
hash; the subject follows, cut to 50 characters. `(not landed)` is the work after the plan's last
landing (on `main`, work still on a feature branch reads that way: the checkout you run it in is
the truth). `(since the last landing)` is the sessions after the last landing of any plan, and
with no landings at all every session is there. A plan shows only when records name it, and a
landing only when something falls in it, so `--since` also hides old PRs. Plans come newest
activity first, landings oldest first. The command never calls the network: `--json` carries each
landing's full `commit`, `subject` and `at` (`plans`, each with `landings`, `open`, then `after`
and `errands`), so a script can join to GitHub. Outside a git repository it says so on stderr and
everything is `(not landed)`.

`--by branch` groups by the branch recorded on each errand instead, newest activity first, which
suits a repo where you work on feature branches:

```
branch        errands  reconciles  reconcile s  accepted    errand $  sessions  session $  total $
plans-stage1       14          22         18.5  9/12 (3 ?)      0.91         4      38.20    39.11
main                3           5          0.0  2/3 (0 ?)       0.19         2       6.02     6.21
```

`--plan PATH` keeps one plan's records. `--branch` keeps records recorded on that branch in either
view. The columns:

- `errands` counts every errand, however it ended; `errand $` is the runner's own cost (0 for an
  errand that failed before it reported one).
- `reconciles` counts the bucket's `reconcile` records; `reconcile s` is the seconds they spent
  in model calls, Jev's rung and the words call included (`--json`: `reconciles`, `reconcile_seconds`).
- `accepted` is ghost blocks accepted / blocks decided (an `ask` written straight in counts as accepted), and in parentheses the blocks of answered
  errands no one has decided yet.
- `sessions` counts Claude Code sessions that are not errands: a session whose id is some errand's
  session is that errand's and is never counted twice, whatever the errand's status. Each message
  goes to its own branch (a session may span several); one with no branch goes to the `?` row.
  `session $` prices each message's tokens (deduped: streaming writes a message more than once).
  A `sessions 0` means no interactive Claude Code ran in the project in that window: every session
  there was an errand's.
- `--since` keeps errands and messages from that day (UTC) on, the day included; `--by branch
  --branch B` keeps one row (`?` for work with no branch). `--json` prints the rows (or `plans`)
  and the raw errand records.
- Both figures are list prices. The table is in code for the current models; override a row or add
  a model in `.olifant/config.toml`, in USD per million tokens, all five keys:

  ```toml
  [plans.prices."claude-sonnet-5-5"]
  input = 2.0
  output = 10.0
  cache_read = 0.2
  cache_write_5m = 2.5
  cache_write_1h = 4.0
  ```

  A model with tokens but no price is named on stderr once and counted as $0, so nothing inflates
  silently. A missing transcript directory is a note on stderr; the errand columns still print.

Read the passive numbers as suggestive, not as a verdict: errands are a slice of a branch's work
while a Claude Code session is all of it, and the time spent in the editor costs nothing here.
The fair comparison is a head-to-head: the same change made both ways.

## In durandal.nvim

| key | |
|---|---|
| `<leader>mpq` | ask, on a `- ?` line: the answer is written under it at once (verified claims with their `✓ <fp>`, accepted to the engine), one undo step; while you type in the plan it waits for insert mode to end, and a plan that is not modifiable gets a ghost instead. `plans = { review_answers = true }` keeps the old ghost block for `<leader>mpy` / `<leader>mpn` |
| `<leader>mpv` | validate, on a claim or a `##` heading |
| `<leader>mpd` | do, on a step or test child |
| `<leader>mpy` / `<leader>mpn` | accept / reject the ghost block on the line: a validate's, a no-git do's, or an ask's with `review_answers` (an accepted verified claim gains its `✓ <fp>`) |
| `<leader>mpc` | force a reconcile (and run the touched steps' tests) |
| `<leader>mpx` | on a step (or a line under it): mark it done (ticked, `✓` its unit now, a live excerpt frozen) or reopen it (code actions "Mark done" / "Reopen step") |
| `<leader>mpe` | on a step: its unit's code under it as an excerpt ("Show the code here"; frozen at once under a ticked step), or, where there is one, open or close its fold; where it cannot, the reason (LSP `disabled`). In a fold of done work or an answered question, it (like `zo`) opens or closes that fold |
| `plans = { conceal, glyphs, fold_done }` | the reading layer ([above](#how-a-plan-reads)): hide markers, paths and hashes off the cursor line; override a glyph; close done work on entry |
| `<leader>ms` / `<leader>mt` | on a step: stub a test under it / run its tests and tick it |
| `<leader>mpr` | review the `do` or the reading of the step here (else the plan's first) |
| `<leader>mp<CR>` | start a step under this line (the engine's "New step below") |
| `<leader>mpf` | the plan's files, in the location list |
| `gd` | on an anchor: go to its unit |

`:Durandal plan` marks the buffer as a plan; `:Durandal plan cancel` cancels the errands running
in it. A finished `do` opens the files it changed in the quickfix list.

Not built yet: the margin (the current step shown while you work in code).
