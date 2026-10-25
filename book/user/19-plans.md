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
| step | `- [ ] <text> @path:Unit`, or ending `· by hand`; `- [x]` when done; a step may nest |
| test child | `  - test: <sentence> @file:name` |
| also child | `  - also @path:Unit`: units a `do` touched beyond the anchor |
| evidence child | `  - ✓ <evidence> @path:Unit`, `  - ✗ <why>` |

An anchor is `@path:Unit`: a path relative to the project root, and a `Name` or `Class.method`.
Python and Haskell units resolve. A path that is absolute or leaves the project is never read: it
is `? not found`. The fingerprint is a short hash of the unit's text; a `✓` whose unit has since
changed is shown stale. A line inside a code fence is read like any other.

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
| `prices` | the current models at list price | `[plans.prices."<model id>"]` rows for `plans cost`, below |

An unknown key under `[plans]` is a configuration error.

## What errands cost

`olifant plans cost [--branch B] [--since YYYY-MM-DD] [--json]`, run in the project, sets the
errand records beside Claude Code's own transcripts of the project
(`~/.claude/projects/<slug>/`, subagents' too), one row per branch, newest activity first:

```
branch        errands  accepted    errand $  sessions  session $  total $
plans-stage1       14  9/12 (3 ?)      0.91         4      38.20    39.11
main                3  2/3 (0 ?)       0.19         2       6.02     6.21
```

- `errands` counts every errand, however it ended; `errand $` is the runner's own cost (0 for an
  errand that failed before it reported one).
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
| `<leader>mpf` | the plan's files, in the location list |
| `gd` | on an anchor: go to its unit |

`:Durandal plan` marks the buffer as a plan; `:Durandal plan cancel` cancels the errands running
in it. A finished `do` opens the files it changed in the quickfix list.

Not built in this stage: ticking steps from test runs, catching a plan up with changed code,
branches.
