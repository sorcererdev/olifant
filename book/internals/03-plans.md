# 3. Plans: the loop between a markdown file and the tree

A plan is a markdown file the engine reads as data. You write what you believe about the code
and what you mean to do; the server checks the lines it can check, asks a model about the rest,
and answers with edits to the plan. This chapter follows that loop through the code. The
user-side view is [chapter 19 of the user book](../user/19-plans.md); read its grammar table
first.

Run it once so the pieces have somewhere to hang:

```sh
scripts/try-pack.sh python-htmx /tmp/bistro && cd /tmp/bistro
mkdir plans && cat > plans/close-table.md <<'EOF'
# Close a table

- [ ] a table closes @src/bistro/booking/commands/close_table.py:CloseTable.handle
EOF
git add -A && git commit -qm plan
olifant gen python-htmx/command --set app=bistro --set service=booking --set command=close_table
nvim plans/close-table.md          # durandal.nvim: <leader>mpc forces a reconcile
tail -f .olifant/state/steps.log   # plan-reconcile, plan-ask, plan-do lines as they happen
```

The generator changes `close_table.py` after the plan was committed, so the reconcile has a
changed unit to look at: it finds `CloseTable` new since the commit that added the plan, finds
the step already anchors it, and (with a `[decide] model`) asks whether the step is done. The
facts land in the buffer; any reading arrives as a ghost block. Nothing here needs a model to
run: with none configured the reconcile still does its facts, and a unit no step covers becomes
a new-step ghost worded from the unit's header.

## The modules

| module | what it holds |
|---|---|
| `src/Olifant/Plans.hs` | the **pure parser**: `parsePlan :: Text -> [Line]` (`Plans.hs`), `Kind`, `Anchor`, `Verdict`, sections, test marks, excerpt fences |
| `Server/Plans.hs` | the plan as the server sees it: `resolveAnchor`, `planDiagnostics`, `anchorItems` (completion after `@`), `planFiles`, and `runErrand`, the ask/validate/do runner; also the small git helpers the loop shares (`currentBranch`, `gitOut`, `gitText`) |
| `Server/Errand.hs` | the **runner**: one `claude -p` call (`runClaude`), its JSON read into `RunResult`; the prompts (`errandPrompt`) and the answer reader (`parseAnswer`) |
| `Server/Loop.hs` | the **reconcile** (`reconcile`), the work set (`workSet`; `baseline`; `changedUnits`; `accounted`) and the facts that need no model |
| `Server/LoopTypes.hs` | the loop's data: `ChangedUnit`, `Fact`, `ReadingKind`, `ReconcileBlock`, `ReconcileAnswer` |
| `Server/LoopQuestions.hs` | Jev's two questions (`planStepQuestion`, `planStateQuestion`), the ladder (`rungsFor`, `gate`, `decideOver`) and the one "words" call (`planWords`) |
| `Server/LoopTests.hs` | a step's tests: `stepStub` writes one, `stepTest` runs them, `testFacts` turns results into marks and a tick |
| `Server/Review.hs` | the **do review**: the tree reference taken before a `do` (`takeRef`), the change since it (`changeSince`), `revertChange`, review records and the archive of past dos |
| `Server/ReviewReadings.hs` | a reconcile's readings as **one review per plan** (`readingsReview`, `replaceReadings`, `answerReading`) |
| `Server/Journal.hs` | the plan's dated lines under a step (`record`, `journalLine`, `pendingFacts`) |
| `Server/Telemetry.hs` | `.olifant/state/errands.jsonl`: errand, outcome, reconcile and claim-pass (`PassRecord`, kind `pass`) records (`appendRecord`); the retired resolver's `resolve` lines still parse |
| `Server/Excerpt.hs` | code excerpts kept under a step, and the hover over an anchor |
| `Plans/Sentences.hs` | the **sentences** of a plan, pure: `sentencesWith :: (Int -> Bool) -> [Line] -> [Sentence]` (row and column range, the line's `Kind`, the text with markers stripped, heading, lead, anchor), where the predicate says which rows continue on the next (a row that does not ends its sentence at the line break); `sentences` joins none, `sentencesJoined` joins by a store's answers; `joinCandidates` (the rows that end without `. ? ! :`, with their and the next row's text), `pairKey`, and `sentenceKey`, a sentence's words without anchor or verdict; a span with no letter or digit (an empty `- `, `-  .`) gives no sentence, so nothing downstream sees it |
| `Server/Triage.hs` | **triage**: `byGrammar` (steps, questions), `triageQuestions` (Jev's `sentence-kind` choice with no unsure and its `about-code` Noul, one state), `fromAnswers`, `noJev`, and `route` with the two thresholds `kindThreshold` and `aboutThreshold`; the join question `joinQuestion` (a `line-continues` Noul, no unsure), `joinThreshold` (0.70), `joinsAnswer`, and `decideJoins` (one `weighAll` per candidate while the pass allows; unsure answers dropped; no Jev, none) |
| `Server/Checker.hs` | the **checker**: `Judgement` (right, wrong, cannot tell; done, open, misfit), `CheckMode` (`CheckClaim`, JSON `verify`; `CheckProgress`, `progress`) and `checkModeOf` (a triage of kind `step` is checked for progress), `checkPrompt` and `parseCheckReply` in either mode, `confirm` (every citation checked against the tree and the catalog; a `Print` per citation found, `fileHash` for a whole file), `liveChecker` (the `Check` verb of `runClaude`), `recordedChecker`, `replayChecker`, `checkerUnder`; `UnitRef` |
| `Server/Claims.hs` | the **claim store**, version 2 (`SentenceEntry`: triage, route, `CheckRecord` or `Failure`, `seGone`; `ClaimStore` keyed by `sentenceKey`; `readClaims`/`writeClaims` under `.olifant/state/claims/`; `claimsOf`/`updateClaims` through `ssClaims`: every write is a function of the store as held now, under one lock), `checkable`, `queued` (owed), `unchecked` (owed or not), `stepUnit` and `stepCitations` (a step's working unit from its check), `pruneClaims`, `checkedKeys`, `lineKey`; the **joins** beside the entries (`Joins`, by `pairKey`, the file's optional `"joins"` field, still version 2; `readJoins`, `writeClaimsWith`, `joinsOf`/`updateJoins` held in `ssClaims` under `<plan>#joins`, same lock and file) and `sentencesOf`, the plan's sentences with its joins, which everything reading sentences calls |
| `Server/Stale.hs` | **stale verdicts**: `printsNow` (the cited files as they are now, read through the server's cache `ssCitePrints`), `isStale`, `staleKeys`, `staleIn`, `citedPrints`; `CitedFile` lives in `Server/State` (State is below Checker) |
| `Server/Confine.hs` | `confined` and `safely`, which `Server/Plans` re-exports (here so `LoopQuestions` and `Checker` need not import `Server/Plans`) |
| `Transcripts.hs`, `Cli.hs` `runPlansCost`, `runChecksCost` | `olifant plans cost`: the records beside Claude Code's own transcripts; `--checks`, the checker's cost and verdicts per plan |
| `Landings.hs` | a plan's landings from `git log --first-parent` (`landingsOf`, `parseLandings`), and `landingFor`, the first landing at or after a time |

Two things to know before reading. There are two modules called Plans: `Olifant.Plans` is the
parser and has no IO; `Olifant.Server.Plans` is everything that needs a project. And the loop's
files import each other in a line, not a layer: `Loop` imports `LoopQuestions`, `LoopTests`
and `LoopTypes`; `ReviewReadings` imports `LoopTests` and `LoopTypes`; `Journal` imports
`Review` and `LoopTypes`; `Server/Plans` imports `Review`, `Journal` and `Telemetry`, and
`Loop` imports `Server/Plans` back for `resolveAnchor`, `confined` and the git helpers. There is
no cycle, but `Server/Plans` is the bottom of the pile for helpers that have little to do with
plans. The claim pass adds a second line: `Checker` imports `Errand` and `State`, `Claims`
imports `Checker` and `Triage`, `Server/Plans` imports `Claims` (the margin), and `Loop` imports
all of them. That is why `Checker` reads units itself (`unitsWithKinds`) rather than through
`Server/Plans.resolveAnchor`, and why the state's cache of stores (`ssClaims`) holds `Dynamic`:
`State` is below all of them.

## The grammar is a parser, not a convention

`parsePlan` reads a file once into `[Line]`. A `Line` (`Plans.hs`) has its row, indent, a
`Kind`, the text, an optional `Anchor` (columns of the `@path:Unit` token) and a `Verdict`
(`Verified fp | NotFound | NoVerdict`). `Kind` is `Heading Int | Claim | Question | Step Bool |
TestChild | AlsoChild | Evidence | Journal | Prose`. Everything else in the system works on
that list: it asks `childrenOf` for a step's nested lines, `sectionOf` for the `##` section a
row is in (a section's `secBranch` comes from `## title · branch name`), `withVerdict`,
`withTestState` and `testState` to rewrite or read a line's marks, and `stepDone` for a ticked
step.

Two details matter when you change it. A code fence the file never closes is no fence
(`parseWith` retries without the opener), so a stray fence cannot swallow the plan. And every
function that changes a line returns *text*, not a `Line`: callers build the edit by parsing
the new text again (`applyFacts` in `Loop.hs` does exactly that).

`PlansSpec` (`src/Olifant/PlansSpec.hs`) is the spec for the grammar, one example per row of the
user book's table.

## Anchors: a name, a fingerprint, a unit

`resolveAnchor s p a` (`Server/Plans.hs`) is the one place an anchor becomes a unit. It
confines the path (`confined`: absolute or `..` paths are never read), reads the file
over the editor's text (`readText (prWorkspace p)`, so unsaved buffers count), finds the
language by file name (`languageFor`), and asks `unitsOf` (`Server/Unit.hs`) for the
file's named units, then `unitNamed` for the anchor's name. It returns a `Resolved` with the
unit's `Named` (its lines, `namedDef`), its text and its **fingerprint**.

The fingerprint (`Server/Unit.hs`) is seven hex digits of an FNV hash of the unit's text
with formatting taken out, so a formatter's reflow leaves it alone and a real edit changes it.
That one number is what every verdict means: `✓ a1b2c3d` is "this was true when the unit hashed
to a1b2c3d". `planDiagnostics` (`Server/Plans.hs`) raises `? not found` (a hint) wherever an anchor
does not resolve, and `changed since this was verified` (information, no hex) only on a `Claim`
line the user wrote: one with no `Question` and no ticked `Step` among its ancestors
(`ancestorsIn`). Evidence under a question is history, and a step's changed unit is the
reconcile's business (`○` on its children), so neither raises it; inside a ticked step only
`not found` is raised. It also returns the `AnchorPrints` it read (`(path, unit)` to the unit's
fingerprint now, `Nothing` when not found): `diagnosticsOf` stores them per plan in
`envPlanPrints`, and `planTokens` marks a `✓` `stale` from them, with no I/O of its own. Which languages' units resolve is a
language-file question ([chapter 5](05-languages.md)): the `[unit]` table of the file's
language.

## An errand: ask, validate, do

An errand is one line of a plan sent to a model that can read (and, for `do`, edit) the
repository. `Lsp.errandRequest` (`Lsp.hs`) handles the three `olifant/plans/ask`,
`validate` and `do` requests; it runs `runErrand` (`Server/Plans.hs`) in the background,
and for `do` takes a lock first (`envDoing`: a second `do` while one runs is refused without
starting).

`runErrand` does, in order:

1. **Check the line fits the verb** (`fits`): `ask` wants a `- ?` question, `validate` a claim
   or a heading, `do` a step or a test child.
2. **Ground it.** The anchors on the line, its children and its section are resolved
   (`take groundingLimit`), and each found unit's body is cut (`cutBody`) into a `Grounded`.
3. **Build the prompt** (`errandPrompt`): the plan, the target line, the grounded units, the
   language's `unit_words` (`projectUnitWords`) so the model knows what an anchor may name, and
   for a `do` its tools first (files only by `Edit` and `Write`, never a shell heredoc, `cat >`,
   `sed -i` or `python -`), then the project's exact test command as the one shell command it may
   run (`doTestCommand`, from `[test] command`, the path as configured), and that Olifant runs
   the tests after the change.
4. **Run the runner** (`runClaude`, `Errand.hs`): `[plans] runner` with `-p
   --output-format json --model <model>` and `--allowedTools Read,Grep,Glob` (a `do` adds
   `Edit`, `Write`, the test tool and `[plans] do_tools`, and `--permission-mode acceptEdits`).
   `--permission-prompts none` (every verb): a refused call is denied at once and the runner
   tells the model it will stay denied, so a denial costs one turn and is never retried.
   The test tool (`doTestTools`, `Errand.hs`; `testTools` in `Server/Plans.hs`) is the command as
   the prompt prints it, up to its `{= tests =}` slot, as a prefix pattern
   (`Bash(uv run --frozen pytest -q*)`), and, when its program is an absolute path inside the
   project, the same by its project-relative path. (`allowPattern`, what `p` adds for a
   denial, is still a command's first two words.)
   `CLAUDECODE` is unset for it. It never throws: a failure is a `RunFailure` with a `Status`
   (`Error | Timeout | Cancelled`) and one sentence.
5. **Read the answer** (`parseAnswer`): lines outside the grammar are dropped and counted;
   anchors that do not resolve are kept as `? not found` and counted. The result is `Block`s of
   `Ghost` lines to insert after a row. An answer with nothing in the grammar is `Empty`, an
   error. A `do`'s lines go through `classifyDo`: `DONE @<path>:<Unit>`, `DONE @<path>` for a
   file with no unit (or a path whose unit the anchor syntax cannot hold, such as a CSS selector;
   this answers the `do` without a ghost, see `doAnswered`), `BLOCKED <why>`, or a `- ?` line.
6. **Record it.** Every ending that reached the runner appends an `AnErrand` record to
   `errands.jsonl` and a `plan-<verb>` line to the step log. The id (`errandId`,
   `Telemetry.hs`) is the runner's session id when it answered, so the editor's later
   `olifant/plans/outcome` notification (accepted or rejected, by block index) can name it.
   durandal.nvim sends `accepted` for each block of an `ask` as soon as it lands, since it writes
   the answer straight in (`plans.lua` `M.write_answer`, through `ghosts.lua` `M.accept_id`);
   `plans.review_answers = true` brings back the ghost and the user's choice.

Nothing is written to the plan. A `do` is different only in what surrounds it, below.

The runner is a port by configuration, not by type: `[plans] runner` is a command name, and the
specs point it at `src/Olifant/Server/testdata/claude-shim.sh` (`ErrandSpec.hs`). The shim
answers from environment variables, so `ErrandSpec`, `PlansSpec` and the plans parts of
`LspSpec` run real subprocesses without a model.

## A `do`, and what it leaves behind

A `do` edits the repository, so the engine wraps it in four things.

**A tree reference.** `takeRef` (`Review.hs`) records the tree before the runner starts:
`git stash create` (else HEAD, which changes nothing), plus the untracked files listed and their
blobs written with `git hash-object -w` (at most `untrackedLimit`). After the run
`changeSince` is the diff against that reference plus the new and changed untracked
files, minus the plan, `.olifant/state/` and `.olifant/config.toml`. `revertChange`
undoes it: `git restore --source=<ref> --worktree`, new files removed, untracked ones put back
from their blobs. The index is never touched.

**A review.** A `do` that answered, or changed the tree however it ended, saves a `Review`
(`Review.hs` record `rvId`, `rvStep`, `rvSaid`, `rvDenials`, `rvTests`, `rvRef`, `rvDiff`…) under
`.olifant/state/reviews/<plan>/<id>.json` (`saveReview`) and the server notifies
`olifant/plans/review/ready`. The editor reads it with `olifant/plans/review` (`reviewView`,
`Lsp.hs`; the change is re-derived from the reference on open) and answers with
`olifant/plans/review/choose`: `keep`, `revert`, `reopen`, `test` or `allow` (`chooseReview`,
`Lsp.hs`). `allow` takes the tool permissions the runner refused (`deniedWords`,
`allowPattern`) and writes them into `[plans] do_tools` (`allowInConfig`, a text edit of
`config.toml`). `keep` writes `keepTicked` (`Review.hs`): the `do`'s lines, and on an open step with
no test children (in the plan or among those lines) the step ticked, stamped and its excerpts
frozen by `doneFacts` (`Lsp.hs`, shared with "Mark done"'s `toggleEdit`), one edit through
`planFactsEdit`; its journal choice is `kept-ticked` (`keepEvent`). A `reopen` never ticks.
`keep` and `revert` append an `AnOutcome` record, the label that Jev's
telemetry is graded by. `Lsp` adds a code lens on each open review's step (`textDocument/codeLens`,
refreshed with `workspace/codeLens/refresh` by `lensesChanged`).

**An archive.** Every `do`, however it ended, is also written to `.olifant/state/errands/<plan>/`
with an `index.jsonl` (`archiveErrand`), so a step's past dos can be listed
(`olifant/plans/errands`, `pastDosAt`) and one reopened (`olifant/plans/errand`). A
`Reopen` (`Server/Plans.hs`) resumes the earlier run's session with `--resume <id>
--fork-session`.

**The step's tests, afterwards.** `runErrand` takes a function `after`; the server passes
`stepTestFacts`, which runs the step's test children (below) over the files as the `do` left them
on disk. The editor's text of a file the `do` wrote is older than the disk, so `landed` closes
those documents first. The resulting facts ride the answer.

## Facts and readings

The loop's central distinction is who may write to the plan.

- A **fact** is something the engine can check alone: a re-anchor when a unit only moved, a
  test's `○ ● ✗`, a tick when a step's tests are green, a journal line, an excerpt kept current.
  `Fact` (`LoopTypes.hs`) is `Replace row text | Insert row [text] | ReplaceRows from to
  [text]`. Facts reach the buffer through `workspace/applyEdit` in one undo step (`sendFacts`,
  `Lsp.hs`).
- A **reading** is a model's judgement: a step looks done, a step needs reopening, a unit placed
  under a step, work no step covers. It is a `ReconcileBlock` (a `Block` of ghost lines, the unit
  it is about, its `ReadingKind`) and the editor shows it as a ghost, never applied by the engine.

The reconcile produces both, in that order.

## The reconcile, end to end

`olifant/plans/reconcile` (`Lsp.hs`) calls `reconcile token forced path` (`Loop.hs`)
under a snapshot of the server state, then stores the readings (`replaceReadings`) and sends
the facts. `forced` is the user's `<leader>mpc`; an automatic reconcile (the plugin fires one on
entering a plan buffer, after a tool lands and a second after a save) is not forced. `forced`
changes two things only: it runs the touched steps' tests, and it lets the ladder climb `ask`
rungs.

1. **Read the plan** from the editor's text (`ssDocs`) or the disk, and `parsePlan` it.
2. **The work set** (`workSet`). `baseline` picks the commit to compare with: on
   a branch other than the default, the merge-base with the default (local, else
   `origin/<default>`); on the default branch, the commit that added the plan, HEAD while the
   plan is uncommitted. `changedUnits` lists every unit that differs from that commit
   (`Change`: `Added | Changed baseFp | Removed | Moved path name`; `compareUnits` and `folded`
   fold a removal and an addition of the same fingerprint into a `Moved`). `accounted`
   collects the `(path, unit, fingerprint)` triples that **any** plan under `[plans] dir`
   already carries (verified anchors, test children's units as they are now, and readings you
   rejected: `rejectedReadings`, replays `errands.jsonl`). The units left over are
   *unaccounted* (capped at `unaccountedCap` = 40, the rest counted as `wsMore`); the units
   whose own step's anchor changed are *touched* (`wsTouched`).
3. **Facts first** (`loopFacts`): `reanchorFacts` for moved units, `pendingFacts` to
   mark a changed step's tests `○` when their unit moved on past the stamp, and, when forced,
   `stepTestFacts` for every touched step with tests. Then the excerpts: `excerptFacts`
   re-renders an open step's code fence or freezes a ticked step's (`Server/Excerpt.hs`).
   Questions are asked against the plan **as the facts leave it** (`applyFacts`), so a tick the
   tests just earned is already visible to `plan-state`.
4. **Nothing to ask?** If there are no unaccounted units, no steps to look at and no gone steps,
   the answer is the facts alone, with no record. If the situation is the same as the last
   answer (`key`, built from the plan text, the changed triples and the rejected set), the cache
   in `ssReconciled` replays the last blocks with fresh facts.
5. **Ask.** `rungsFor` (`LoopQuestions.hs`) builds the rungs: Jev (`deciderUnder`, the
   `[decide] model`) first, then the chat ladder (`[plans] ladder`, else `[model] ladder`) minus
   Jev backends, each filtered by its `escalate` policy against `forced`. For each changed file
   with unaccounted units, `planStepQuestion` asks which open step it belongs to (or none), one
   question per file; for each touched or newly placed step `planStateQuestion` asks `done`,
   `in_progress` or `diverged` (a done step: `still_done`, `reopened`, `diverged`).
   `decideOver` tries each rung until an answer clears `gate` (0.6), recording each rung
   it called and its seconds. Every question checks a monotonic clock against
   `[plans] budget` (default `[model] budget`) first; past it, the file is `Unasked` and counted
   in `more`.
6. **Words.** Jev decides; it does not write. One call to the first allowed chat rung
   (`planWords`) numbers every line the ghosts need (a new step's sentence, a `✗ changed: why`)
   and parses `N: <line>` back (`parseWords`). With no chat rung, a new step is the unit's header
   (`headerWords`) and a why is "names changed".
7. **Blocks.** The pieces are assembled into `ReconcileBlock`s: `stateGhosts` (a tick or untick
   line; a `done` for a step **without tests** is dropped, because only tests, keeping its `do`
   or the user tick a step),
   `also` children, `goneGhosts`, `whyGhosts` (`✗ changed:`), and `newSteps` for unplaced units
   placed by `newStepPlace` (the end of the section for the current branch).
8. **Record.** An `AReconcile` record (id, plan, branch, baseline, counts, seconds, every
   rung climbed, every `Decision` with its options, choice, confidence and rung, and the
   blocks' units so a later outcome can name block `n`) is appended, and one `plan-reconcile`
   line goes to the step log. A cancel at either of two checkpoints returns `Left "cancelled"`
   with no record and nothing cached.

`reconcileJson` (`Loop.hs`) is the wire shape; `Lsp` empties `raFacts` and `raBlocks` from it
because facts land by edit and readings by review (next section).

## Readings are a review

A reconcile's blocks do not travel in the response. `replaceReadings`
(`ReviewReadings.hs`) turns the answer into a `ReadingsReview` (one per plan, id
`readingsId` = `"reconcile"`), stored in `.olifant/state/reviews/<plan>/reconcile.json`, replacing
the previous one and deleting it when there are no readings. Each `Reading` keeps its
`readingKey` (kind, path, unit, fingerprint, and for a step's reading the step's words), so a
reading already answered is not offered again, and its edit is **derived from the plan when
shown or answered** (`readingFacts`, `readingRow`): the step is found again by its text, never
at a stale row.

The editor lists the open reviews (`olifant/plans/reviews`: do reviews and the reconcile's
entries), shows one (`olifant/plans/review`) and answers entries with `apply`, `leave`, `place`
(under a given open step), `new` or `ignore` (`choiceOf`, `chooseReading`, `Lsp.hs`).
`outcomeOf` maps `leave` and `ignore` to `rejected`, everything else to `accepted`, and the
answer appends an `AnOutcome` against the reconcile record's block, which is what
`rejectedReadings` replays later to keep a rejected ghost from returning until its unit
changes.

## The journal

`Journal.record` (`Journal.hs`) keeps a dated line per event under a step: a `do` ended
(`doEvent`), a review answered (`keepEvent`), tests ticked, unticked or red (`testEvents`),
a unit moved (`movedEvent`). Each event has a key and is written once, so a line the user
deletes is not written again; the store is `.olifant/state/journal/<plan>.jsonl` with each
entry `Pending | Landed | Lost`. Lines reach the plan as facts: `journalOn` (`Lsp.hs`)
turns the pending entries into `Insert` facts for the plan as it is open now, and
`flushJournal` sends them as their own edit when no other answer is carrying them. A step that
has gone from the plan marks its entries `Lost`.

## Step tests

A step's test children are anchors to tests (`- test: sentence @file:name ○`). `stepStub`
(`LoopTests.hs`) writes a new one: it names the test from the step's words (`testNameFor`,
using the language's `[test] name`), calls `Server/Stub.stubWith` for the file edit, and returns
the child line as an `Insert` fact. `stepTest` runs them: `testsRan` runs each
test alone over the editor's text through the `[test]` runner and `testFacts` turns the
verdicts into marks, stamps the step with its unit's fingerprint, and ticks the step when all
are green. A test that cannot run, or is not in the report, is pending, never keeping its old
mark. Which tests a name can denote, and how `Olifant.Plans.Foo` becomes a `describe` path, is
the language's `[test] unit` rules ([chapter 5](05-languages.md)).

## The claim pass, end to end

The pass reads every sentence, triages the ones it has not seen, and on save checks the ones
written since the last save (owed) with a searching checker: claims for whether they are so,
steps for their progress. It never writes to the plan. `Loop.claimPass`
(`Cancel -> PassMode -> FilePath -> ServerState -> IO (ServerState, Either Text PassAnswer)`) is
the whole of it:

1. **Read.** The plan from `ssDocs` (else disk) through `parsePlan`, and the store from
   `Claims.claimsOf`.
   `[plans] pass_budget = 0` stops here. **Joins:** `joinCandidates` of the lines; those whose
   `pairKey` is not in `joinsOf` go to Jev one request each (`Triage.decideJoins`: a Jev batch
   shares one `state`, and `jevBody` refuses two values for one context name, so pairs cannot
   share a request), while the token and `pass_budget` allow, under `safely`; an unsure answer
   is not kept, so the pair is asked again next pass. The answers, with the held joins cut to
   today's candidates, are kept by `updateJoins` (each request is a Jev call in the record). The sentences are
   `sentencesJoined` of those joins.
2. **Triage.** `pruneClaims` marks a key no sentence has as gone (`seGone`, written as
   `"gone"`), drops it once gone for `keepGone` (a day) and clears the mark on a key that is back,
   so an undo finds its check; it runs in every mode, an idle's too; each sentence whose `sentenceKey` is not
   stored (one per key) goes through `Triage.triage`: `byGrammar` for steps and questions, else
   one `weighAll` request to Jev (`rgJev` of `rungsFor`, i.e. `deciderUnder`) with the two
   `triageQuestions`, read by `fromAnswers`; with no Jev, `noJev` (a `- ` bullet is a claim).
   `route` turns the `Triage` into `Verify`, `Progress`, `Asked` or `Aside`. Each is stored as a
   `SentenceEntry`, with `seOwed = mode /= OpenPass` (written in the editor: owed one automatic
   check; already in the file when opened: backlog; JSON `"owed": true`, absent reads false, so
   a milestone 1 store is all backlog); the new entries are merged once after triage (`updateClaims`, into the
   store as it is then). The budget and the token are asked
   before each sentence.
3. **Check.** Only for `SavePass`: `Claims.queued` (`checkable`: routed `Verify` or
   `Progress`, not a ticked step, no check; owed; no failure in the last `retryAfter`; plan
   order, one per key), then with `[plans] recheck = "save"` the stale sentences
   (`Stale.staleIn`, not ticked steps), one at a time. Each key is taken atomically
   (`holdKey` on `ssChecking`: a key another pass, `checkOne` or a sweep holds is skipped) and
   asked again against the held store (checked since: skipped; a stale key whose check is no
   longer the one it was stale for, `sameCheck`: skipped). Before each: stop when
   the token is tripped, when `pass_budget` seconds are spent, or when the
   spend so far plus the estimate (this pass's mean over the runs that cost something, a run that never started costing $0; else `unpricedCheck`) would pass
   `pass_cost`. `checkSentence` takes the mode (`checkModeOf` of the entry's triage), runs
   `checkerUnder` (the recorded checker when `scCheckAnswers` is set, else `liveChecker`),
   reads the reply with `parseCheckReply` and confirms it with `confirm`; it appends a `check`
   errand record (`erAuto`, `erMode`, `erVerdict`) and puts a priced check's cost into the
   held estimates (`ssEstimates`). The key is let go (`releaseKey`, in a `finally`) only after
   the result is stored, so no other check takes it in between. The checker runs under the state's `ssCancel`, not the pass's
   token: a tripped token stops the pass before the next check, and the check in flight
   finishes. After it, the result is stored only when its key is still among the plan's
   sentences as the editor has them now (`ssLiveDoc`, else the pass's text) and `ssCancel` was
   not tripped (`updateClaims` adjusts that one entry, `landed`); its cost goes into the tally either way. A failed run or a reply with no verdict
   stores a `Failure`; `landed` keeps the sentence owed after a first failure (one retry after
   `retryAfter`) and clears it after a second.
4. **Backlog.** Every queued key the pass did not try (a cap, a cancel) and no one holds gets
   `seOwed = False` in one `updateClaims`, unless the pass was replaced by a newer one (its stop
   token tripped, `ssCancel` not), which takes them on.
5. **End.** A `PassRecord` (kind `pass`) is appended when Jev was asked or a check ran. A pass
   stopped by the token answers `Left "cancelled"`.

`checkOne` is *Check this* and *Re-check*: one sentence by key, triaged first when unseen, checked
whatever its route and the caps (in its triage's mode), recorded as a `check` errand that is not
automatic. It takes its key with `holdKey` (held elsewhere: `Left "this sentence is being
checked now"`, nothing run) and lands the check before `releaseKey`.

**Check the plan.** `Loop.checkPlan` (`Cancel -> (Int -> Int -> IO ()) -> FilePath ->
ServerState -> IO (ServerState, Either Text Sweep)`) takes `sweepTargets`: `unchecked` (owed or
backlog) then the stale keys (`staleIn`), not ticked steps, plan order, one per key, each with
its mode. `sweepWidth` (3) workers (`Async.replicateConcurrently_`) share one `IORef` of the
queue, the dollars spent and the dollars reserved: a worker reserves `estimateFor` the next
target's mode and takes it only while spent + reserved + that estimate stays within `[plans]
check_cost`, else it marks the sweep stopped and leaves the target queued. Each holds its key
(`holdKey`), checks it again against the held store (a stale target re-checked since, by *Check
this* or a save pass, `sameCheck`: skipped), runs `checkSentence` (not automatic), lands
it and lets go; the callback gets (done, total). A check that throws a synchronous exception
(`trySync`: an IO error writing its record, say) lands as a `Failure`, its estimate counted as
spent, so the other workers' checks go on; an asynchronous one (a cancel) still ends the sweep. A tripped token stops new checks; those in
flight land (the checker runs under `ssCancel`, the request's cancel, which kills them). In
`Lsp`, `olifant.checkPlan` runs `checkPlan` on its own thread with a stop token of its own:
cancelling the progress kills the thread `planProgress` runs on, so the handler only trips the
token, waits for the sweep, and refreshes the diagnostics. `Sweep` is checks made, targets left, spent, and whether the ceiling stopped it.
`estimatesOf` reads `errands.jsonl` once per project root into `ssEstimates` (the last 20 priced
`check` records per mode, newest first; no `mode` is `verify`), and `checkSentence` prepends
each priced check after that; `estimateFor` is the mean, else `unpricedCheck` ($0.01 for haiku).

**Stale.** `confirm` keeps a `Print` for every citation found (`cfPrints`): the unit's
`fingerprint` when a unit was named or encloses the cited line, else `fileHash` of the whole text
(FNV-1a 64, 7 hex, as `fingerprint`). `Stale.printsNow` answers, per cited file, a `CitedFile`
(the file's hash, Nothing when it is gone, and every unit's fingerprint), from `ssCitePrints`
while the entry's editor version (Nothing: read from disk) is the document's now, else read
through `prWorkspace` and parsed once (`unitsWithKinds`) and kept. `State.filesChanged` (the
client's watched files: `Lsp.watch` registers `**/*` under each project root on open) drops the
entries of the changed paths and bumps the cache's change count in the same step; `printsNow`
keeps a reading only when the count is what it was before the read (a file changed while it was
read may have been read as it was: the next ask reads it again); an edit of the plan touches none, so a pull after it reads no cited
file. `isStale` is any print whose file or unit is gone or whose hash differs; a check with no
prints (milestone 1) or one that could not tell is never stale. `staleKeys` is the stale entries
of a store.

**The working unit.** `Claims.stepUnit` is a step line's typed anchor, else the `cfUnit` of the
check stored for the `Step` sentence starting on its row (an `Anchor` at no columns);
`stepCitations` is every unit print of that check. `stepStub`, the excerpt (`Lsp.excerptStep`,
which takes the store first) and *Mark done* (`Lsp.doneFacts`, which writes the anchor with its
stamp when the line has none) use `stepUnit`; `runErrand` grounds a step's `do` in
`stepCitations` too, after the typed anchors, deduplicated, up to `groundingLimit`. The
reconcile and `testsRan` are unchanged.

**The checker.** `liveChecker` runs `runClaude` with the verb `Check`: `runnerArgs` gives it
`--model <check_model>`, `--allowedTools Read,Grep,Glob`, `--settings` with a `Read(./<dir>/**)`
deny rule per fence (`checkFences`: the plans dir, `.olifant`, `not_evidence`; a Read rule also
fences Grep and Glob, measured in the redesign's fence check), and `--append-system-prompt`
saying to stay under the current directory and why the fences are not evidence. `checkPrompt`
has two modes. `CheckClaim` gives the heading, the lead, the sentence, a typed anchor as "the
writer pointed at …", the rule (judge what is true of the code now, ignore wishes), and asks for
one fenced JSON object (`right | wrong | cannot_tell`), with the same rule as the progress mode
that `unit` is a name the file defines. `CheckProgress` gives the step (with its
continuation rows), whether it is ticked, the anchor, the three definitions (done, open, misfit;
a misfit's named means must cover what the step is for), a rule for a step that points back at
something unnamed, and a rule that `unit` is a name the file defines, and asks for `done | open |
misfit | cannot_tell`. `parseCheckReply` takes the mode, the last ```json block, else the first
`{` that decodes; it refuses a verdict of the other mode, and a right, wrong, done or misfit with
no evidence. `confirm` checks each citation (inside the root, outside the
fences, lexically and again after `canonicalizePath` of the root and the path, so a symlink into a
fence or out of the root is refused; the file exists, the unit found by `unitLookupIn`, or with no unit named the line inside the file, a bare path
allowed); any miss makes the verdict `JCannotTell` "cited code not found", except for an `open`,
whose missing citations are dropped. The first unit found and its `fingerprint` are the
sentence's evidence unit; every citation found has its `Print`.

**In `Lsp`.** `startPass` (an `Env`, a `PassMode`, the plan) runs the pass in the background:
`OpenPass` on `didOpen`, `SavePass` on `didSave` (both under `planProgress "checking"`),
`IdlePass` on the client's `olifant/plans/idle`. `envPasses` holds one `RunningPass` per plan
(number, mode, its stop, `rpIdleAfter`). A save or an open trips the running pass's stop token
(passed to `claimPass` with `eitherCancel`); an idle while a save's or an open's pass runs only
sets `rpIdleAfter`, and that pass starts an `IdlePass` when it ends. `didChange` stops nothing.
The pass's kill token (its `envCancel`, so the checker's `ssCancel`) is tripped only when the
client cancels the progress or the pass throws. While it runs,
a watcher polls `ssChecking` every 250 ms (stopped by a flag the pass sets when it ends, or the
handler when it throws) and sends `workspace/diagnostic/refresh` when the
sentence being checked changes; after it, `workspace/diagnostic/refresh` and
`semanticTokens/refresh`. Nothing goes out as `workspace/applyEdit`. The margin is
`Server/Plans.claimHints` (inside `planDiagnostics`, which converts its code-point columns to
UTF-16 and passes `checksCanRun`: the runner found or a recording set, neither cap 0, and
`staleKeys`; its second result, the `AnchorPrints`, carries the cited prints too, keyed
`(path, unit or "")`, so a code change that moves them sends `semanticTokens/refresh` as an
anchor's does; and `planSummary`, the `plan-unchecked` line from `planCounts`),
`planTokensWith` (the `checked` and `wrong` modifiers, by `lineKey`) and `claimHover` (called by
`Lsp.planHover` with the sentences on the row and the stale keys; a checked one adds
`keepFooter`). `claimLine` is the one table: a claim's codes, a step's (`step-done`,
`step-misfit`, `step-ticked`; `step-open`, `step-unsure` and `step-ticked-done` are `hoverOnly`:
`claimHints` emits no diagnostic for them and the hover shows them without stripping a glyph),
the stale ones (`claim-right-stale`, `claim-wrong-stale`, `step-done-stale`,
`step-misfit-stale`, `step-stale`), and `claim-queued` for a `checkable` sentence with no check,
saying whether a save checks it (`seOwed`). Every one of
these, and `checkOne` and the pass's live-keys check, builds sentences with `Claims.sentencesOf`,
so the margin and the engine split a plan the same way. `sentenceActions` offers *Re-check* on a stale verdict and *Check this* on any sentence with no
check but a question (both `olifant.checkSentence`, titled with `estimateFor` of the sentence's
mode), *Check the plan* on every row while `sweepTargets` is not empty (`olifant.checkPlan`, its
title from `planCounts` and the summed estimates), *Keep this
verdict* (a `WorkspaceEdit` from `Plans.keepEdit`: one evidence line after the sentence's last
row and after any `- ✓`/`- ✗` lines already directly below it at the evidence indent, so kept verdicts stack oldest first; `Nothing` when the same line is in that run or the check could not tell), *Show the code here* (`citationPlace`) and *Show why* (`whyFile`, the page from
`Plans.whyBody`, which says when the check ran): `olifant.checkSentence` (`checkOne` in the
background), `olifant.checkPlan` (`checkPlan` in the background under `planProgress
"checking"` with `checking N of M` as `Said` events; one per plan through `envSweeps`, not
`envPasses`, so a save does not stop it, and a second is refused with `window/showMessage`
`Already checking this plan`; a ceiling stop says `Stopped at $X · N left`), `olifant.showCode`
and `olifant.showWhy` (`window/showDocument`). **Definition** (`textDocument/definition`) in a plan:
an anchor under the cursor answers with its unit as before; else `Lsp.sentenceCitations` takes the
sentence under the cursor and, when it has a check, resolves each cited unit with no line
(`resolveAnchor`) and answers `Plans.citationLocations` (every citation in the checker's order:
the line cited, else the unit's first line, else 1); else nothing. `planFolds` takes `checkedKeys` of the store, so a
belief folds its question once it has a verdict. An `ask` carries the checked beliefs under its
question (`Plans.beliefsUnder` to `Errand.errandPromptWith`).

## What is in `Lsp`

The plans requests, all in `Lsp.hs` (the line numbers are the handlers):

| method | what it answers |
|---|---|
| `olifant/plans/anchor` | where an anchor on a line points (file, line, unit, fingerprint) |
| `olifant/plans/files` | a plan's anchored lines by section |
| `olifant/plans/ask`, `validate`, `do` | the errands, via `errandRequest` |
| `olifant/plans/errand`, `errands` | one past `do`, or a step's past dos |
| `olifant/plans/reviews`, `review`, `review/choose` | open reviews: a `do`'s and the reconcile's |
| `olifant/plans/review/ready` | **notification**: a `do`'s review is saved |
| `olifant/plans/reconcile` | the reconcile above |
| `olifant/plans/stub`, `test` | a step's test written / run |
| `olifant/plans/outcome` | **notification**: a block was accepted or rejected (an `ask`'s at once, unless the editor reviews answers) |
| `olifant/plans/idle` | **notification** from the client: the plan went quiet; starts the claim pass (`startPass`, `IdlePass`: triage only) |
| `workspace/executeCommand` `olifant.checkSentence`, `olifant.checkPlan`, `olifant.showWhy`, `olifant.showCode` | the claim pass's actions (`sentenceActions` offers them as code actions) |
| `textDocument/diagnostic` in a plan, source `olifant-plan` | the checker's codes: `claim-queued`, `claim-checking`, `claim-right`, `claim-wrong`, `claim-unsure`, `step-done`, `step-misfit`, `step-ticked`, `claim-right-stale`, `claim-wrong-stale`, `step-done-stale`, `step-misfit-stale`, `step-stale`, and `plan-unchecked` (information, the plan's summary); `step-open`, `step-unsure` and `step-ticked-done` are on hover only |
| `textDocument/definition`, `hover`, `completion`, `codeLens`, `foldingRange`, `semanticTokens/full` | `gd` on an anchor, the unit hover, `@` completion, review and delegation lenses, folds (excerpts from `excerptSpans`, ticked steps and answered questions from `planFolds`, all kind `region`), the plan's tokens (`planTokens`, encoded by `encodePlanTokens`; the legend is `planTokenTypes`/`planTokenModifiers`, advertised by `advertise`; any other file answers `[]`) |

`workspace/semanticTokens/refresh` goes to the client when a pulled diagnostics pass changes a plan's
`envPlanPrints` (its anchors' fingerprints and its checks' cited prints), so a `✓` turns stale without a second edit.

All the slow ones go through `inBackground` and `planProgress` like the other slow requests
([tour](01-tour.md#lsp-the-adapter)). `olifant plans cost` is not in the server: `runPlansCost`
(`Cli.hs`) reads `errands.jsonl` with `Telemetry.readRecords` and Claude Code's transcripts
with `Transcripts.readTranscripts`, and prices both from `[plans.prices]`. `--by branch` is
`costRows`; the default view reads each plan's landings through `rtGit` with `Landings.landingsOf`
(`git log --first-parent -- PLAN`, so the runtime's git seam, faked in specs) and buckets records
with `landingRows` (`Cli.hs`): errands and reconciles by `landingFor` over their own plan's
landings, messages over every plan's. Both views build a line with `rowOf` and print with
`rowTable`; `landingTable` is the plan, landing and `(not landed)` layout. `--checks` is
`runChecksCost`: one `ChecksRow` per plan from its `pass` records (sentences triaged) and `check`
errands (by `erMode`, no mode a claim, verdicts from `erVerdict`, automatic or asked by
`erAuto`), printed by `checksTable` or as `{"checks": [...]}`. A failing `git log`
is a stderr note and no landings, never a failure.

## The seams

- **`[plans]`** (`Catalog.hs`, the `PlansConfig` record): `dir`, `runner`, `model`,
  `timeout`, `do_tools`, `ladder`, `budget`, `pass_budget`, `pass_cost`, `check_model`,
  `check_timeout`, `not_evidence`, `recheck` (`Recheck`: `RecheckAsked` or `RecheckSave`),
  `check_cost` (`pcCheckCost`, 5.00, at least 0), `prices`. An unknown key is a configuration error;
  a `ladder` name not under `[model.backends]` is one too (`plansLadderIn`).
- **`[decide] model`** names the backend Jev answers on (a `jev` backend's model defaults to the
  pinned `jev-1.14.0`; `jevDecider` logs the model that answered); `catalogDecideModel` is what
  `rungsFor` reads. Triage asks it with `weighAll`, one request for a sentence's two questions. `[model] ladder`, `[model.backends.*]` and `escalate` are the model chapter's
  ([user chapter 12](../user/12-holes-and-models.md)).
- **`[test]` of the language** is what `stepTest` and a `do`'s test command run;
  **`[unit]`** is what an anchor can name.
- **Ports** (`App.Ports`): `pFiles` (every record and review goes through it, so specs use an
  in-memory filesystem), `pSyntax` (units), `pTester` (tests), `pNow` (ids and journal times).
  The runner and git are not ports: both are real subprocesses (`runBytes`), which is why the
  plans specs are the ones that touch a real git repository.

A plan's rendering classes come from the parser, never from an editor re-parsing the text:
`planTokens` (`Server/Plans.hs`) classifies each span from `parsePlan`'s lines (a line's parent is
the nearest earlier step or question with a smaller indent: `parentIn`), and an editor only maps
the classes to colours, glyphs and conceal.

## Testing it

The specs mirror the modules; run one with `cabal test --test-options='--match "<describe>"'`,
in the main checkout (the git-backed specs refuse worktrees).

| spec | pins |
|---|---|
| `Olifant.PlansSpec` | the grammar |
| `Server/PlansSpec` | anchors, diagnostics, completion |
| `Server/ErrandSpec` | the runner with the shim: result, cost, `CLAUDECODE` unset, timeouts, the answer reader |
| `Server/LoopSpec` | `factsSpec`, `workSetSpec` (baselines, moves, accounted), `questionsSpec`, `reconcileSpec`, `stepTestsSpec`, and the Haskell/Rust step tests |
| `Server/ReviewSpec`, `ReviewReadingsSpec` | the tree reference and revert; readings as a review |
| `Server/JournalSpec`, `TelemetrySpec`, `ExcerptSpec` | the store and its events; records and their readers; fences |
| `Plans/SentencesSpec` | the splitter: steps whole with their continuation rows (a nested checkbox its own step, a blank row ending it), what gives no sentence, paragraphs across rows, numbered lines, abbreviations, versions, code spans, URLs, keys, leads |
| `Server/TriageSpec` | the route table, the grammar, no Jev, the shared state and no unsure |
| `Server/CheckerSpec` | fences, both prompts, both reply readers, `checkModeOf`, `fileHash`, `confirm` (a print per citation; an `open` drops a missing citation) (fenced or missing citations cannot tell, a symlink into a fence or out of the root too), the runner with the shim, recorded replies |
| `Server/StaleSpec` | a changed unit, a gone unit or file, a whole-file print, no prints, the cache read once until `filesChanged`, a change during a read not cached |
| `Server/ClaimsSpec` | store v2: round trip (owed, modes, prints; a milestone 1 store read as backlog), a version-1 file read as empty, atomic writes, concurrent `updateClaims`, orphans, `pruneClaims`, `queued` and `unchecked`, `checkedKeys` |
| `Server/LoopSpec` `claimPassSpec` | the pass: idle triages only, save checks once what was written, an open never checks (an m1 store neither), a step gets a progress check and a ticked one none, what the cost cap leaves is backlog, a failure retried once, `recheck = "save"`, *Check the plan* (three at a time, a check that throws, the ceiling, ticked steps skipped and stale ones checked, a save during it, a cancel), a failure waits ten minutes, a cancel lets the check in flight land (or drops it when its sentence changed), a key held elsewhere is skipped, Check this during a pass, nothing written to the plan, a typed anchor in the prompt, budget 0, no Jev |
| `Server/ProseCorpusSpec` | the milestone gate on the prose corpus (below) |
| `LspSpec` (`plansSpec`, `plansLoopSpec`, `plansStage1Spec`, `doReviewSpec`, `claimPassSpec`) | every request, in process, over pipes |

For a change to the loop, start in `LoopSpec`'s `reconcileSpec`: it builds a git repository in a
temp folder, scripts the model answers with a recorded decider (`scDecideAnswers` in
`ServerConfig`), and asserts on the answer's facts and blocks. Chat rungs answer from
`scChatAnswers` (initializationOptions `chat_answers`) the same way, through
`Rungs.rgChatDecider`. The claim pass's checks answer from `scCheckAnswers` (initializationOptions
`check_answers`): `recordedChecker` reads `{"replies": {KEY: TEXT or {text, cost, seconds,
delay}}}` by `sentenceKey`, and notes each key asked in `FILE.asked` and each prompt in
`FILE.prompts`. Its Jev triage answers from `scDecideAnswers` by question kind
(`sentence-kind`, `about-code`).

**The corpus spec.** `ProseCorpusSpec` measures milestone 1 and the step gate of 2a on `testdata/prose-corpus.json`: the
user's own plan sentences (corpus 2, 27 lines, and two from corpus 3), each labelled with its kind
and, for a claim, its verdict and the code a citation must point into, from nightvote at
`8488bb80`. Each entry becomes a `Sentence` from its line, heading and lead; it is triaged and
routed, and when routed to verify, checked and confirmed against a checkout at that commit
(`OLIFANT_PROSE_CORPUS`; without one, or at another commit, it is pending). It replays Jev from
`testdata/prose-corpus-jev.json` (`replayDecider`) and the checker from
`testdata/prose-corpus-checks.json` (`replayChecker`, keyed by `sentenceKey`); with
`OLIFANT_RECORD_CHECKS=1` it asks the checker live for the sentences the recording has no reply
for and records them (delete a reply to ask it again; a run cut by the suite's 180 s limit
resumes), and with `OLIFANT_RECORD_JEV=1` Jev too. It prints a row per
entry (`OLIFANT_PROSE_TABLE` also writes the table to a file) and asserts the bars: kind agrees
on at least 23 of 27, every labelled claim but R6 and H5 sent to a check (H5, a contested checkbox
line, is a step by the grammar; user ruling), at most one non-claim sent, every
uncontested claim's verdict equal to its label with a citation at the labelled code, no wrong-sure
verdict, and a mean cost a check of at most $0.12. Recorded on 2026-11-09 (third recording, after
two tunings of the triage criteria and one of the check prompt): kind 25/27, no non-claim sent,
the five checks (A3, A7, A8, H4, S1) right on every uncontested claim with citations at the
labelled code, no wrong-sure verdict, $0.10 a check; one bar missed, A4 ("We would have to
diagnose … and redeploy everything …") is a claim at 0.82 but about-code 0.67, so it is not
checked. By user ruling A4 is a known miss (`knownMisses`): the gate asserts it is routed to
`Aside`, so a change that fixes or breaks it shows. The resolver's corpus spec and recording
retired with the resolver; its numbers are in the notes' experiments.

**The step gate (2a).** The 13 step lines carry a `progress` label (done, open, misfit) and an
`idea` flag from the builder (`progressEvidence` says why), and `rows` for continuation rows
(none in this corpus). Each entry routed `Progress` gets the progress check. Bars: H2, R2 and X1
done and S4 misfit with `Staff` in its why (4 of 4); labelled non-idea steps off the label at
most 1, none done or misfit against it; idea lines routed `Progress` at most 2; a progress check
at most $0.12 on average. Recorded 2026-11-11 after two tunings of the progress prompt: 4 of 4
known, 0 ideas routed, no wrong-sure, but two off (R4 set aside by triage, about-code 0.56; R3
could not tell, a cited line not in its file) and $0.122 a check; the claim bars also miss H4,
whose m1 reply cites non-names that `confirm` has refused since 0bf1d39. The gate waits for a
ruling, so it fails with `OLIFANT_PROSE_CORPUS` set and is pending without it.

## Rough spots

- **`Server/Plans.hs` holds more than plans.** The git helpers and `safely` are imported by
  `Loop`, `Review` callers and `Excerpt` from there; a `Git` module would say what they are.
- **Two `Change` types.** `LoopTypes.Change` (`Added | Changed | Removed | Moved`) is a unit's
  difference from the baseline; `Review.Change` (`Modified | Added | Deleted`) is a file's. Both
  have an `Added`. Modules that import both qualify; a new one has to remember.
- **`Journal` is a module and a `Kind` constructor**, and `pendingFacts` is a function of both
  `Loop` and `Journal` (the loop's re-mark of pending test children, the journal's lines as
  facts).
- **`runClaude` is named for one runner.** `[plans] runner` is any command that takes the flags
  of `runnerArgs` (`--permission-prompts` needs Claude Code 2.1.259 or later) and prints Claude
  Code's JSON; nothing checks that, and `denialsIn` reads
  Claude's `permission_denials` shape.
- **A review is a snapshot of the whole tree.** Reverting a `do` restores the reference, not a
  reverse patch, so a file you edited after the `do` is reverted with it, and two open reviews
  of the same files clobber each other (known since the do review's report).
- **Reconcile's cache key includes the whole plan text.** Any edit of the plan, even a prose
  line, makes the next reconcile ask again. The "same situation replays" promise holds only for
  the unchanged plan.
- **The budget is checked before each question, not during.** A slow rung can overrun it by one
  call's timeout (`[model] timeout`, default 120 s).
- **Branch work depends on finding the default branch.** `defaultBranch` (`Loop.hs`) tries
  `origin/HEAD`, then a local `main`, then `master`; a repository with none of them (a trunk
  called `develop`, say) is reconciled against the commit that added the plan, on every branch.
- **The fences stop at the root by prompt only.** The deny rules fence the plans dir,
  `.olifant` and `not_evidence` for Read, Grep and Glob (a Read rule covers all three); leaving the
  repository is fenced only by the appended system prompt, and `confirm` refuses a citation outside
  the root. In the fence check haiku globbed a parent folder once. The deny rules are path
  patterns: a symlink into a fence (`notes -> plans`) can be read by the checker, and is caught
  only when `confirm` canonicalises the cited path.
- **A known miss.** A4, a claim about what fixing a collector takes, scores about-code 0.67
  against the 0.70 threshold and is set aside; the gate pins that (`knownMisses`) rather than
  moving the threshold.
- **The triage texts are tuned on 27 lines.** The kind and about-code criteria (`kindOptions`,
  `aboutTrue`, `aboutFalse` in `Triage.hs`) carry examples taken from the very corpus that gates
  them, so the gate flatters them; a second corpus would tell.
- **The splitter is a heuristic.** A sentence ends at `.`, `?` or `!` before a capital; a
  lowercase start after a full stop (`it works. then…`) stays one sentence, and an abbreviation not
  in the list (`approx. The`) splits.
- **Absence verdicts do not go stale.** A verdict about absence ("there is no AI in the
  codebase") rests on whole files or on nothing, so new code elsewhere does not make it stale.
- **A whole-file print is coarse.** A citation of a path or of a line outside any unit is
  printed with `fileHash`, so any edit to that file turns the verdict stale.
- **A unit field that names no unit voids a verdict.** `confirm` refuses a citation whose `unit`
  the file does not define, whatever its line; both prompts say so. A file in a language the
  project does not name has no units at all, so a right unit there is not found either (H4 in the
  corpus cites `askAboutUnsaved` in a `.js` file of a Python-only project).
- **`[conjure] wire` and the language's `[conjure]` table are different things** with the same
  name; see [chapter 4](04-conjure.md#two-things-called-conjure).
