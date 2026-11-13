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

A claim or a step needs no anchor. Write it as you would to a colleague (`- the loader keeps only
the total`) and the engine finds the unit a reader would open to check it, checks the line
against that unit, and writes `@path:Unit ✓ <fp>` into the line when the check says it is right
([Checked as you go](#checked-as-you-go)). An anchor you type yourself is read as before.

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
durandal.nvim the text reads as in `cat` by default: no text is hidden or moved.

- **Markers are glyphs** (only with `plans.conceal = true`, off the cursor line). `- [ ]` is `☐`, `- [x]` is `☑`, `- ?` is `?` in the question colour,
  `- test:` is `·`, `- also` is `+`. The dash of a claim, an evidence line or a journal line, the
  `@path:` of an anchor and the 7-hex after a `✓` are hidden; evidence keeps its `✓`/`✗`. The
  unit's name stays, and `gd` on it goes to the unit as before.
- **Evidence recedes.** Evidence under a question (an anchored claim), `also` lines and journal
  lines are dim; a conclusion (an unanchored claim under a question) is plain text; a question
  stands out. A `✓` whose unit has changed since is dimmer still.
- **Done work folds.** A ticked step with children and an answered question are closed when you
  enter the plan. A question is answered when a line directly under it is evidence (`- ✓`, `- ✗`),
  a verified anchored claim, an `ask`'s answer (anchored claims, whatever their verdicts, then a
  bare conclusion), or a belief of yours that the claim pass has
  checked, right or wrong. A bare line you write under a question is a belief awaiting its check,
  so the question stays open until the check lands. A question's fold line is the question, ` — `,
  and its conclusion (the last unanchored child, so a note you add later wins), cut to the window,
  then `▸ N` hidden lines (the question alone when it has no conclusion); a ticked step's is `☑`,
  its text, its unit and `✓`. Open steps always read in full. Folds close on entry only: one that forms while you look (an answer landing under a
  question, a `do`'s journal lines, a tick, a review's `a`) starts open and stays open until you
  next enter the plan. `zo` opens a fold, `zR` opens all, and `<leader>mpe` toggles the fold under
  the cursor.
- **The cursor line is raw** (with `conceal = true`). On the line you are on, in every mode, the
  file's text shows, so what you edit is what the file has. Moving off it hides the markers again;
  a line shifts left by up to five columns between the two, which is why conceal is off by default.

- **Steps are sentences too.** A checkbox line (`- [ ] `, `- [x] `) is one step, and its
  continuation rows (indented prose directly under it, no blank line between) are part of it: one
  sentence over all its rows, never split and never asked about joins. A nested checkbox is its
  own step. A step written as prose ("Add D and R fields to Comparison 2018") is a step when Jev
  says so ([triage](#checked-as-you-go)). What each kind gets:

  | sentence | route | checked |
  |---|---|---|
  | a claim (kind ≥ 0.80, about the code ≥ 0.70) | verify | right, wrong or could not tell |
  | an unticked checkbox, or a prose step over the same thresholds | progress | done, open, misfit or could not tell |
  | a ticked checkbox | progress | only when you ask |
  | a question (`- ? `, or Jev's) | asked | not yet (answers come in a later milestone) |
  | everything else | aside | only when you ask |

- **Diagnostics are signs.** A plan line is prose, so olifant's diagnostics in a plan buffer are
  signs (one per row, the claim pass's verdicts by glyph) and underlines on the anchor, with no
  virtual text beside the words; a wrong claim is undercurled. `changed since this
  was verified` is raised only for a claim of your own outside any question; evidence under a
  question and steps raise none (a step answers with the loop, and the dimmed `✓` is the evidence's
  signal), and inside a ticked step only `? not found` is.

```lua
plans = {
  conceal = false,  -- true: hide markers, paths and hashes off the cursor line
  glyphs = {},      -- override: todo, done, question, test, also, dash, path, hex,
                    -- and the verdict signs queued ○, checking …, right ✓, wrong ✗, unsure ?, misfit !
  fold_done = true, -- false: on entry close only excerpts, leave done work open
}
```

Colours are the standard `@lsp.type.plan…` groups, linked with `default` so a colourscheme
wins: `planEvidence`, `planAlso` and `planJournal` to `Comment`, `planQuestion` to `Title`,
`planAnchorUnit` to `Identifier`, and the `stale` modifier to `Comment`.

## What the server does

- Diagnostics: a missing anchor (`? not found`), and `changed since this was verified` on a claim
  of your own whose unit has changed; and the claim pass's verdicts on sentences
  ([Checked as you go](#checked-as-you-go)). Evidence an `ask` left under a question, and steps, raise no
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
    its Edit and Write tools, never a shell heredoc, `cat >`, `sed -i` or `python -`. Any other command is refused, and the refusal is final: the runner tells it not to try again. It is told Olifant
    runs the step's tests after it, so tests it could not run are no reason to answer `BLOCKED`. A
    `DONE` of the step's own unit is no `also` line. A file with no unit (a stylesheet, a template, a data file) it reports as `DONE @<path>`, which counts as an answer and leaves no line (a `DONE @<path>:<selector>` that the anchor syntax cannot hold counts the same). In a git repository a `do` ends in a review ([below](#reviews-and-the-journal)): its diff, and `revert` to undo it.
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
| `runner` | `"claude"` | the command run; `CLAUDECODE` is unset for it; it must accept `--permission-prompts` (Claude Code 2.1.259 or later) |
| `model` | `"sonnet"` | passed as `--model` |
| `timeout` | `300` | seconds before an errand is killed |
| `do_tools` | `[]` | extra `--allowedTools` entries for `do` (beyond Read, Grep, Glob, Edit, Write and the test command) |
| `ladder` | `[model] ladder` without Jev | the backends a reconcile climbs when Jev is unsure, in order, each with its `escalate` policy (the claim pass uses neither) |
| `pass_budget` | `30` | seconds an automatic claim pass may run ([below](#checked-as-you-go)), counted from its start and checked before each triage and each check; `0` turns the pass off. Checks it did not reach become backlog |
| `pass_cost` | `0.50` | dollars an automatic claim pass may spend on checks: before each check it adds the mean cost of this pass's checks so far that cost something (a run that never started costs nothing and is left out; else $0.10) and stops if that would pass the cap; `0` checks nothing. A check whose cost the runner did not report counts as $0.10 |
| `check_cost` | `5.00` | dollars one *Check the plan* may spend: before each check it adds the estimates of the checks in flight and the next one, and stops when that would pass the cap; at least 0 |
| `recheck` | `"asked"` | who re-checks a stale verdict: `"asked"`, only *Re-check* and *Check the plan*; `"save"`, each save too, after the owed sentences and under the same caps. Anything else is a configuration error |
| `check_model` | `"sonnet"` | the model a check runs on (`--model`) |
| `check_timeout` | `120` | seconds before a check is killed (at least 1) |
| `not_evidence` | `["docs"]` | folders a check may not read, besides `dir` and `.olifant/` (add `"book"` when a project's book repeats its plans) |
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

## Checked as you go

The **claim pass** reads every sentence of a plan, works out which ones say something about the
code (a claim) or ask it to change (a step), and has a model that searches the repository check
those against the code: a claim for whether it is so, a step for how far the code has got. The verdict shows
in the margin and on hover. Nothing is ever written into the plan: verdicts live in
`.olifant/state/claims/<plan>.json` (ignored by git) and in the margin.

**Sentences.** The pass works on sentences, not lines:

| line | sentences |
|---|---|
| a step (`- [ ] `, `- [x] `), with its continuation rows | one, judged whole |
| a question (`- ? `) | one, the whole line, judged whole |
| `- test:`, `- also`, evidence (`- ✓`, `- ✗`), a journal line, fenced code, front matter, a line that is only a link | none |
| a heading | one (the text after the `#`s, a ` · …` suffix dropped) |
| everything else: paragraphs, `1 - ` and `1. ` lines, `- ` bullets, blockquotes | split into sentences |

A sentence ends at `.`, `?` or `!` followed by a space and a capital letter or a quote; `e.g.`,
`i.e.`, `vs.`, `etc.` and `Dr.` do not end one, and code spans and URLs are read whole.

**A line ends a sentence.** Write one thought per line, with or without a full stop: each line of a
paragraph (or of a bullet's continuation) is its own sentence. A paragraph hard-wrapped mid-sentence
is the exception: a line that does not end with `.`, `?`, `!` or `:` (a closing quote or paren
after one counts) is a *join candidate*, and Jev is asked once per candidate (one small request
each, inside the pass's `pass_budget`) whether the next line continues it (`line-continues`,
joined at probability 0.70 or more; an answer Jev could not give is asked again next pass). The
answer is kept in the plan's store under the two lines' words, so it is asked again
only when one of them changes. With no Jev (no `[decide] model`) nothing joins. So

```
The collector retries 3 times.. On failures it does exponential backoff
every scraper fix means a code change and a deploy
```

is three sentences, while a paragraph wrapped at 90 columns stays one sentence across its rows.
The margin, hover, `gd` and the actions read the same joins as the pass. A sentence is known by its
words (its anchor and verdict left out), so moving it, re-wrapping its paragraph or renaming its
heading keeps what the pass knows about it, and editing a word makes it new. The old words are
kept for a day: undo the edit, or paste a cut sentence back, and its triage and verdict return
without a new check.

A bullet with nothing yet in it (`- `, or only punctuation such as `-  .`) is no sentence: it is
not triaged, checked or marked while you are still typing it.

**Triage.** Each sentence the pass has not seen is classified once:

- The grammar decides explicit lines: a checkbox is a step, `- ? ` is a question.
- Jev decides the rest: `[decide] model` names the backend (`model = "jev"`, with
  `[model.backends.jev] kind = "jev"`); the Jev version is that backend's own `model` key, pinned
  to `jev-1.14.0` by default. A `[decide] model` that names no backend (a version id put there by
  mistake) is a config error, not a quiet fall back to the grammar. One request per
  sentence with its heading and the sentence or question before it: its kind (`claim`,
  `question`, `step` or `prose`, with no "unsure" option) and whether this project's code can
  settle it.
- The route is fixed: a **claim** with kind probability at least 0.80 and about-code at least
  0.70 goes to the claim check (`verify`); a **step** over the same thresholds, and every
  checkbox, to the progress check (`progress`). Questions and everything else are stored and
  never asked about again until their words change.
- With no `[decide] model` the grammar alone classifies: `- ` bullets are claims, checkboxes
  steps, paragraphs prose (never checked).

**The check.** A claim is checked by one `claude -p` run (the `runner`, `check_model`,
`check_timeout`) from the project root with Read, Grep and Glob, that searches the repository for
the code the sentence is about and answers `right`, `wrong` or `cannot_tell`, one sentence of why,
and the code it rests on (path, unit or line). It is told to judge what the sentence says is true
of the code now and to ignore any wish or request in it. The plans folder, `.olifant/` and
`not_evidence` are fenced off: the run's settings deny reading them (a deny rule on Read also
stops Grep and Glob there), and the prompt says why. A sentence with an anchor you typed
(`@path:Unit`) is checked the same way; the anchor is passed as a place to start, not the answer.

**The progress check.** A step is checked by the same run with another question: the prompt
gives the heading, the lead, the step with its continuation rows, whether it is ticked, and a
typed anchor as a place to start, and asks for one of four verdicts with the code it rests on:

- **done**: the code already does all of what the step says (evidence required);
- **open**: it does not yet, and doing it as written would get what the step says (the code it
  would change, when there is any);
- **misfit**: the step as written rests on something that is not so in the code, or would not
  get what it says (the means it names does not cover what it is for; evidence required). A step
  that also states something about the code now is a misfit when that statement is false;
- **cannot_tell**: the code cannot settle it.

A `done` or `misfit` whose citation the engine cannot find becomes could-not-tell, as for a
claim; an `open` citing code that is not there keeps its verdict and drops the citation.

Before a verdict shows, the engine checks every citation: the path is inside the project,
outside the fences (both as written and with symbolic links followed), and exists; the unit is in that file, or, when no unit is named, the line is inside it (a named unit that is not there is not found, whatever the line). Any citation
it cannot find turns the verdict into "Could not tell · cited code not found": a `✓` or `✗` never
rests on code the engine could not find. The first unit cited is kept as the sentence's evidence
unit, with its fingerprint, and every citation found keeps a print: the unit's fingerprint (named,
or around the cited line), else a hash of the whole file.

**One automatic chance.** A sentence you write in the editor is owed one automatic check: the next
save checks it, under `pass_budget` and `pass_cost`. Anything else is **backlog**: a sentence that
was already in the file when you opened it (an old plan, a pull, a paste from outside), and one a
save could not reach because a cap or your cancel stopped it (it had its chance). Backlog is
triaged and marked `○`, never checked by itself; *Check this* or *Check the plan* checks it. A
check that fails (the runner missing, a timeout, a reply with no verdict) is tried once more after
ten minutes, then it is backlog too. So opening a plan costs only triage, and a save costs at most
what you wrote since the last one. A plan stored before this rule (no `owed` in its entries) reads
as backlog.

**When it runs.**

| event | triage | checks |
|---|---|---|
| plan opened | sentences not seen (backlog) | none |
| `olifant/plans/idle` (durandal.nvim: a second after you stop typing, never in insert mode) | sentences not seen (owed) | none |
| save | sentences not seen (owed) | the owed ones, under the caps; with `recheck = "save"`, then the stale ones |
| *Check this*, *Re-check* | (the sentence, if not seen) | that sentence, ignoring both caps |
| *Check the plan* | none | every unchecked and stale sentence, under `check_cost` |

Save checks run one at a time and stop at `pass_budget` seconds or `pass_cost` dollars. One pass
runs per plan. Typing and idling stop nothing: an idle while a save's or an open's pass runs waits
for it to end, then triages. A new save or open replaces the running pass: it starts no new check,
the one in flight finishes and is kept if its sentence is still in the plan (its cost is counted
either way), and what the replaced pass had not reached stays owed for the new one. A sentence one
check is on is not taken by another: each check holds its sentence until its verdict is stored.
Cancelling the "checking" progress stops the check in flight too. Each check is an errand record
(`verb: check`, its `mode`, `verify` or `progress`, and its `verdict`; `auto: true` when the pass
asked), so `plans cost` counts it; each pass that asked Jev or checked anything adds a `pass`
record (its mode, sentences, triaged, routes, checks and verdicts, failures, what is left queued,
seconds, Jev calls, cost).

**Stale verdicts.** A verdict rests on the prints of the code it cited. When any of them differs
from the tree now (the unit's body changed, the unit or file is gone, or, for a whole-file print,
anything in the file changed), the verdict is **stale**: it keeps its sign, dimmed, and the hover
says `Right when checked · the code changed since` (or `Wrong`, `Done`, `Would not work`). A stale
open step shows `○` again: `The code this step would change has changed · Check this`. The server
reads each cited file once and keeps what it read until the file changes on disk (your editor's
file watching: a checkout, a formatter) or in the editor, so editing the plan reads no code, and a
code change turns the margin stale without touching the plan. Nothing is re-bought by itself:
*Re-check* (a code action with its estimate, and `<leader>mps` on a stale sentence) and *Check the
plan* check it again; `[plans] recheck = "save"` lets each save re-check stale verdicts after the
owed sentences, under the same caps. *Show why* says when the check ran (`Checked 2026-11-11 14:02
by …`), so an old verdict reads as old.

Two honest limits. A verdict about absence ("there is no AI in the codebase") rests on whole files
or on nothing, so new code elsewhere does not make it stale. And a citation of a whole file (a
template, a stylesheet, a line outside any unit) goes stale on any edit to that file.

**Check the plan.** One action checks what is left: every sentence routed to a check with none yet
(owed or backlog; ticked steps excluded) and every stale verdict, in plan order, three at a time.
Its title says what and how much: *Check the plan (12 unchecked, 3 stale, ~$1.50)*, the estimate
the sum over those sentences of the mean cost of the last 20 checks of their kind (else $0.10, or
$0.01 for haiku). It stops before the check that would pass `[plans] check_cost` (default $5.00;
there is no seconds cap) and says `Stopped at $5.00 · 9 left`. It runs under the "checking"
progress with a count (`checking 4 of 15`); cancelling it stops it after the checks in flight,
which are kept. A save while it runs does not stop it, and the save's own pass skips the sentences
it holds. A second *Check the plan* on the same plan while one runs is refused (`Already checking
this plan`). When checks can run, the plan's first heading (row 0 when it has none) carries an
information diagnostic, `plan-unchecked`: `12 unchecked, 3 stale · Check the plan` (a part that is
0 is left out).

**How well it does.** Measured on 27 sentences from a real project's plans (labelled from its
code): the kind agrees with the label on 25; no question, step or opinion was sent to a claim
check; every claim checked got the labelled verdict with a citation at the right code but one,
and none was marked right or wrong against its label; a check cost $0.10 on average (sonnet, 15
to 25 seconds). One claim about what a fix would take ("we would have to … redeploy everything")
is read as process and not checked. Contested lines can go either way. The one claim that misses
its citation names a real function in a JavaScript file, but the project lists only Python, so
the engine finds no units in that file and counts the citation as not found. That claim (H4) is a known miss until olifant reads JavaScript (user ruling 2026-11-12; all languages are on the backlog); the gate asserts it reads could-not-tell.

The step gate, on the corpus's 13 step lines (labels from the code): the four known steps are
right (three done; the misfit names the Staff role that `is_staff` misses); of the nine labelled
steps that are not ideas, seven get their label, one is set aside by triage (about-code 0.56, a
known miss, as the process-like claim above is) and one comes back could-not-tell because a cited
line was not in its file; none is marked done or misfit against its label; none of the four idea
lines ("use AI to …") reaches a check; a progress check costs $0.12 on average (the bar is
$0.125).

**The margin.** One diagnostic per sentence, over the sentence's range (source `olifant-plan`):

| code | severity | message |
|---|---|---|
| `claim-queued` | hint | `○ Unchecked · the next save checks it` (owed) or `○ Unchecked · Check this, or Check the plan` (backlog) |
| `claim-checking` | hint | `… Checking` |
| `claim-right` | hint | `✓ Right · <why> · <path>:<unit>` |
| `claim-wrong` | warning | `✗ Wrong · <why> · <path>:<unit>` |
| `claim-unsure` | hint | `? Could not tell · <why>` |
| `step-done` | hint | `✓ Done in the code · <why> · <path>:<unit> · Mark done?` (no `Mark done?` on a prose step) |
| `step-misfit` | warning | `! As written this would not work · <why> · <path>:<unit>` |
| `step-open` | hover only | `Still open · <why>` |
| `step-unsure` | hover only | `Could not tell · <why>` |
| `step-ticked` | warning | `! Ticked, but the code does not do this yet · <why>` (a ticked step you checked by hand) |
| `step-ticked-done` | hover only | `Done in the code · <why> · <path>:<unit>` |
| `claim-right-stale`, `claim-wrong-stale` | hint | `✓ Right when checked · the code changed since · <why> · <path>:<unit>` (`✗ Wrong when checked …`) |
| `step-done-stale`, `step-misfit-stale` | hint | `✓ Done when checked · the code changed since · …` (`! Would not work when checked …`) |
| `step-stale` | hint | `○ The code this step would change has changed · Check this` |
| `plan-unchecked` | information | `12 unchecked, 3 stale · Check the plan`, on the first heading |

`claim-queued` and `plan-unchecked` show only when checks can run (the runner is found and neither
cap is 0); a failed check shows as queued. An open step, and a step the checker could not tell
about, are silent in the margin (an open step is the normal state): their line is on hover only.
Prose, questions and AI off show nothing. Hover (`K`) on a row lists each of its sentences'
lines, without the glyph; when one is checked it ends with
`gd: the code · Keep this verdict: <leader>mpk` (the server does not know your key: it names
durandal.nvim's default). The claim's words carry the
semantic-token modifiers `checked` and `wrong`.

**The actions** (code actions on the sentence under the cursor):

- *Check this (sonnet, ~$0.10)* on any sentence with no check (a claim, a step ticked or not, one
  the triage set aside; not a question): one check now, whatever the caps, a claim check or a
  progress check by its triage. The estimate is the mean cost of the last 20 checks of that kind
  (else $0.10, or $0.01 for haiku), read from `errands.jsonl` once and kept as checks are made.
- *Re-check (sonnet, ~$0.10)* on a stale verdict: the same, again.
- *Check the plan (12 unchecked, 3 stale, ~$1.50)* on any row of a plan with something to check
  ([above](#checked-as-you-go)).
- *Show the code here* on a checked sentence: opens the first cited code.
- *Mark done* on a step the checker found done: today's action ([the loop](#the-loop)). A step
  with no anchor of its own gets the unit its check cited as its working unit: *Mark done* writes
  that anchor with its `✓ <fingerprint>`, *Show the code here* (`<leader>mpe`) shows that unit, a
  stub (`<leader>ms`) goes to its tests, and a `do` on the step is grounded in every unit the
  check cited. A typed anchor still wins.
- *Keep this verdict* on a checked sentence (right or wrong; not on *could not tell*): writes
  the verdict into the plan as one evidence line after the sentence's last row, `- ✓ <why>
  (<path>:<unit>)` or `- ✗ …`, one level under a bullet or step, at the sentence's indent under
  prose. One undo step. A second keep on the same line goes below the first, so they read oldest first. Not offered once the same line is anywhere in the run of evidence lines directly below, nor on a sentence
  edited since its check (its words no longer match). The evidence line is yours: the pass
  never writes or removes it.
- **`gd`** on a checked sentence goes to the code its check cites: every citation in the
  checker's order (the line cited, else the unit's first line), so an editor shows a list when
  there are several. On an anchor `gd` still goes to its unit; on a sentence with no check it
  does nothing.
- *Show why*: the sentence, its verdict and why, and every citation, in
  `.olifant/state/why/<hash>.md`, opened.

**An `ask` under your beliefs.** When a question has beliefs under it with a check, `<leader>mpq`
sends them along: the prompt lists each with its verdict and the code it rests on (`not checked`
for the rest) and asks the answer to add what they miss rather than repeat them.

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

`--checks` shows what the plan checker cost and found instead, one row per plan from its `pass` and
`check` records (honouring `--plan`, `--since`, `--branch` and `--json`):

```
plan        sentences  triage $  claims  right/wrong/?  claims $  steps  done/open/misfit  steps $  automatic $  asked $  failed  total $
plans/p.md          5      0.00       2          1/1/0      0.30      3             1/1/1     0.30         0.30     0.30       1     0.60
plans/q.md          2      0.00       1          0/0/1      0.05      -                 -        -         0.00     0.05       0     0.05
```

`sentences` is what the passes triaged; `triage $` is 0 today (Jev is not priced yet). `claims` and
`steps` count the answered checks of each kind with their verdicts (`?` is could not tell; a `check`
record with no `mode` counts as a claim), and their `$` every check of that kind, failed ones too;
a plan with none of a kind shows `-`. `automatic $` is what the save pass spent, `asked $` what
*Check this*, *Re-check* and *Check the plan* spent, `failed` the checks that did not answer,
`total $` the sum. `--json` prints `{"checks": [...]}`, each row with `plan`, `sentences`,
`triage_cost`, `claims` and `steps` (each `n`, its verdict counts and `cost`), `automatic`,
`asked`, `failed` and `total`. A pass record carries no branch, so `--branch` keeps its sentences
only for `?`.

A claim pass that called a model appends a `resolve` record: `id`, `at`, `plan`, `seen` (the lines
it worked on), `by_rung` (how many resolved in each round, `jev-1`, `jev-2`, `jev-3`, or `none`),
`checked`, `right`, `wrong`, `elsewhere`, `more` (lines left for the budget), `seconds`, `calls` (every Jev round and every chat rung asked, one
`decisions` entry each; a rung that did not answer at the gate has no choice) and
`decisions`. `plans cost` leaves them out of its tables; the `elsewhere` rate is the one to read in
use, as how often Jev's unit was the wrong one.

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
| `plans = { conceal, glyphs, fold_done }` | the reading layer ([above](#how-a-plan-reads)): `conceal = true` hides markers, paths and hashes off the cursor line (off by default); override a glyph or a verdict sign (`glyphs.checking` is a list of spinner frames); close done work on entry |
| the sign column | one sign per row from the claim pass: `○`, a spinner (`⠋⠙⠹…`, 100 ms a frame) while checking, `✓ ✗ ?`, `!` for a misfit or a ticked step not done (a row with several sentences shows its worst, `✗ › ! › ? › checking › ○ › ✓`, a stale verdict last), a stale verdict's glyph dimmed (`DurandalPlanStale`, linked to `Comment`, no undercurl), no sign for the plan's `plan-unchecked` summary, other olifant diagnostics by severity; a row holding two or more checked sentences also shows a dim mark after each (`DurandalPlanMark`, linked to `Comment`; the sign's glyph, the spinner frame while checking), the sign keeping the worst; a wrong claim and a misfit step are undercurled; the sentence being checked is highlighted (`DurandalPlanChecking`: the cursor line's background, dotted underneath) until its verdict lands |
| `<leader>mpk` | keep the checked verdict here as an evidence line (the code action *Keep this verdict*; `keys.plan_keep`) |
| `<leader>mps` | check the sentence here: *Re-check* on a stale verdict, else *Check this* (`keys.plan_check`); says `check: nothing to check here` otherwise |
| `<leader>mpS` | *Check the plan* (`keys.plan_check_all`); says `check: nothing left to check in this plan` otherwise |
| `<leader>ms` / `<leader>mt` | on a step: stub a test under it / run its tests and tick it |
| `<leader>mpr` | review the `do` or the reading of the step here (else the plan's first) |
| `<leader>mp<CR>` | start a step under this line (the engine's "New step below") |
| `<leader>mpf` | the plan's files, in the location list |
| `gd` | on an anchor: go to its unit; on a checked sentence: to the code it cites (several: the quickfix list) |

A plan buffer tells the engine it went quiet (`olifant/plans/idle`) one second after a change,
counted from leaving insert mode, so the claim pass triages while you read, not while you type;
checks wait for a save, and only what you wrote since is checked.

`:Durandal plan` marks the buffer as a plan; `:Durandal plan cancel` cancels the errands running
in it. A finished `do` opens the files it changed in the quickfix list.

Not built yet: the margin (the current step shown while you work in code).
