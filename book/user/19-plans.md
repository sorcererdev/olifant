# 19. Plans

A plan is a markdown file in which you write what you believe about the code and what you mean to
do, in lines the engine can check. You ask the model about it, and its answers land in the plan as
ghost blocks you accept or reject. Plans live under `plans/` of the project (`[plans] dir`);
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
is `? not found`. The fingerprint is a short hash of the unit's text; a `✓` whose unit has since
changed is shown stale. A line inside a code fence is ignored.

## What the server does

- Diagnostics: a missing anchor and a stale verdict, on the anchor.
- `@` completes files, then units. `gd` on an anchor goes to the unit (`textDocument/definition`).
- Three errands, each one `claude -p` run from the project root, never more than one `do` at a time:
  - **ask** (on a `- ?` line): answers with claims that carry anchors the index resolves.
  - **validate** (on a claim, or a `##` heading for the section): checks claims against the
    units' code and answers verdicts and evidence.
  - **do** (on a step or test child): does the step in the repository, with edit rights and the
    project's test command. Use a clean git tree: `git diff` shows it, `git stash` undoes it.
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
| `budget` | `[model] budget` (180) | seconds per reconcile, counted from its start and checked before each question; past it, the rest is one ghost `- ? <n> more changed units not in the plan` (without the "more" when no model is configured at all) |
| `prices` | the current models at list price | `[plans.prices."<model id>"]` rows for `plans cost`, below |

An unknown key under `[plans]` is a configuration error.

## The loop

A plan mirrors the tree. Each open step is in one of three states: done (the work it describes is
in the tree), in progress (part of it landed) or diverged (the tree holds work no step describes,
a step's unit is gone, or a done step's tests are red again). The engine finds out by
reconciling, request `olifant/plans/reconcile`.

**When it runs.** On entering a plan buffer, and after any tool landing while a plan is open (a
test run, a stub, a conjure, a `do`, an accepted ghost). `<leader>mpx` forces one, and runs the
tests of the steps it finds touched. An automatic reconcile runs no tests.

**What it compares.** The units changed since the baseline (the merge-base with the default
branch; on the default branch itself, the commit that added the plan) against what the plans under
`plans/` already carry at their current fingerprints. A changed unit no plan accounts for, and any
step whose unit changed (done ones too), is looked at. Nothing changed: no model call. The same
situation again replays from a cache. A ghost you rejected does not return until its unit changes.

**Facts land, readings are ghosts.** Facts are what the engine can check alone, and they are
written straight into the buffer in one undo step: a tick when the step's tests are green, the
test states `○ ● ✗` (a run of all a step's tests stamps it `✓ <fp>`, open or done: the fingerprint
the marks were made at), `○` on the children of a step whose unit changed since that stamp, a
re-anchor when a unit only moved.
Readings are a model's judgement, and arrive as ghosts you accept with `<leader>mpy` or drop with
`<leader>mpn`: an `also` child placing a unit under a step, a step line ticked or unticked, a
`✗ changed: <why>` child, a `✗ gone:` child, a new step for work nothing covers.

**Who decides.** Jev (`[decide] model`) answers two questions: `plan-step` (which open step is a
changed file for, or none; one question per changed file) and `plan-state` (done, in progress or
diverged; for a done step without tests, still done, reopened or diverged). Below the gate (0.6)
the same question climbs `[plans] ladder`, else `[model] ladder` without Jev, each backend under
its `escalate` policy: an automatic reconcile climbs `auto` rungs only, `<leader>mpx` also `ask`
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
escalate = "ask"      # only <leader>mpx climbs here
```

A backend with no `escalate` key defaults to `ask`, so only `<leader>mpx` climbs to it. (The backends' other keys are as in chapter 12.) The demo's config sets `escalate = "auto"` on its local backend for that reason.

**Tests on a step.** `<leader>ms` on a step stubs a test under it (Python; the step needs an anchor), the step's text as the
sentence. `<leader>mt` runs the step's tests and ticks it when all are green. A `do` runs the
step's tests when it finishes.

**The record.** Every reconcile appends a `reconcile` line to `.olifant/state/errands.jsonl`: `id`,
`at`, `plan`, `branch`, `baseline`, `forced`, `changed`, `unaccounted`, `more`, `facts`, `seconds`,
`climbs` (rung and seconds), `decisions` and `blocks`. A ghost's accept or reject outcome refers to
the reconcile's `blocks` by index.

## What errands cost

`olifant plans cost [--branch B] [--since YYYY-MM-DD] [--json]`, run in the project, sets the
errand records beside Claude Code's own transcripts of the project
(`~/.claude/projects/<slug>/`, subagents' too), one row per branch, newest activity first:

```
branch        errands  reconciles  reconcile s  accepted    errand $  sessions  session $  total $
plans-stage1       14          22         18.5  9/12 (3 ?)      0.91         4      38.20    39.11
main                3           5          0.0  2/3 (0 ?)       0.19         2       6.02     6.21
```

- `errands` counts every errand, however it ended; `errand $` is the runner's own cost (0 for an
  errand that failed before it reported one).
- `reconciles` counts the branch's `reconcile` records; `reconcile s` is the seconds they spent
  in model calls, Jev's rung and the words call included (`--json`: `reconciles`, `reconcile_seconds`).
- `accepted` is ghost blocks accepted / blocks decided, and in parentheses the blocks of answered
  errands no one has decided yet.
- `sessions` counts Claude Code sessions that are not errands: a session whose id is some errand's
  session is that errand's and is never counted twice, whatever the errand's status. Each message
  goes to its own branch (a session may span several); one with no branch goes to the `?` row.
  `session $` prices each message's tokens (deduped: streaming writes a message more than once).
- `--since` keeps errands and messages from that day (UTC) on, the day included; `--branch` keeps
  one row (`?` for work with no branch). `--json` prints the rows and the raw errand records.
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
| `<leader>mpq` | ask, on a `- ?` line |
| `<leader>mpv` | validate, on a claim or a `##` heading |
| `<leader>mpd` | do, on a step or test child |
| `<leader>mpy` / `<leader>mpn` | accept / reject the ghost block on the line (an accepted verified claim gains its `✓ <fp>`) |
| `<leader>mpx` | force a reconcile (and run the touched steps' tests) |
| `<leader>ms` / `<leader>mt` | on a step: stub a test under it / run its tests and tick it |
| `<leader>mpf` | the plan's files, in the location list |
| `gd` | on an anchor: go to its unit |

`:Durandal plan` marks the buffer as a plan; `:Durandal plan cancel` cancels the errands running
in it. A finished `do` opens the files it changed in the quickfix list.

Not built yet: the margin (the current step shown while you work in code).
