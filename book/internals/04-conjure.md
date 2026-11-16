# 4. Conjure: one step

`conjure` takes **one small step** at the cursor. The writer model reads the situation there:
the unit, its test file (or where its tests would go), the tests' state, the files it imports
and the person's uncommitted changes. It names a small goal and answers whole files. The
engine checks the answer, then shows it for review: the goal, the diff across every file
touched, whether each file parses, and how the tests moved. The person accepts it (it lands),
rejects it, or asks the harder writer to take the same step again. Nothing is refused for red
tests, a missing test file or a body that is not a stub. A step that leaves tests red is still
a step. The user side is [chapter 18 of the user book](../user/18-tdd-in-the-editor.md#conjuring).

This replaced the round loop (rounds, a ladder, Jev steering, the pause window, wires and port
methods) in November 2026. The measurement that decided it is in the notes repository
(`durandal/experiments/conjure-2026-11-2026-11-15.md`), and the harness is
`experiments/conjure-2026-11/` here. It found most of the old loop's misses came from the
prompt framing: it sent the person's body as "rejected by the person", asked for "the least
code that makes this test pass", and refused when there was no test file. Reframed as the
person's work in progress, one haiku call was forward on every case.

Run it. In a project with a `[model]` whose default (or `[conjure] writer`) is a `claude` or
other prompt backend ([user chapter 12](../user/12-holes-and-models.md#configuring-a-model)):

```sh
# in Neovim with durandal.nvim: <leader>mc in a function
tail -f .olifant/state/steps.log .olifant/state/conjure-steps.jsonl
```

## The modules

| module | what it holds |
|---|---|
| `Server/Step.hs` | **pure**: the prompt (`stepSystem`, `Situation`, `situationText`), the reply (`StepReply`, `readStep`), which answered paths may land (`keptPath`), the tests' change (`TestDelta`, `testDelta`), the step log's words (`stepLogLine`), the session block (`sessionFilter`), and `StepHeld`, a shown step as the state keeps it |
| `Server/ConjureStep.hs` | **IO**: `conjureAt` (facts, writer, check, show), `answerStep` (accept, reject, harder), `writerFor`, the edits (`editsOf`), the moved-file check (`movedSince`), the record (`record`), `sessionDiff`, and the wire (`stepJson`, `stepResumeOf`) |
| `Server/Site.hs`, `Unit.hs`, `Tests.hs` | shared with the other editor commands: the cursor's project, language, module and test file (`siteAt`); the unit (`testOrUnitAt`); the test run (`runOver`); the imported files (`importedContext`) |
| `Model.hs` | the backends; `askBackendReply` returns a `ChatReply` (text, cost, seconds); the `claude` kind runs `claude -p` |
| `Catalog.hs` | `[conjure] writer` and `harder` (`catalogConjureWriter`, `catalogConjureHarder`), checked against `[model.backends]` at load |

`ConjureScan.hs` and `Sibling.hs` remain: `Evidence.hs` (doctor's "sibling tests" row) reads a
sibling test through them. The step itself does not read the pack's `[conjure]` shapes.

## One step, end to end

The editor sends `olifant/conjure {textDocument, position, selection?}` (`Lsp.hs`). `Lsp` runs
it in the background under `$/progress` "conjuring", through a snapshot of the server state,
and answers `stepJson`.

**1. The facts** (`conjureAt`). `siteAt` finds the project, language, module and test file over
the editor's text. `testOrUnitAt` finds the unit at the line; a selection takes its lines
instead. The only refusals are no unit at the line ("…: put the cursor in one to conjure it")
and no writer ("no model is configured…"). A one-line unit, a module with no test file, a green
unit: all are sites. Any step still shown in this file is dropped first (`dropShown`, recorded
as `dropped`).

**2. The situation.** The before-run is `runOver` of the test file when it exists. The
imported files come from `importedContext`. The session block is `sessionDiff`:
`git diff HEAD` per file, most recently modified first, kept to the language's extensions
(from `langFiles`) without `book/`, `.olifant/`, lockfiles and binary diffs, cut at 300 lines,
then the untracked files by name (`untrackedLine`: the language's, at most 20). Not a git repository gives an empty block. `situationText`
words it all. The language and the test file's path come from the language and the site, so no
language is named in the engine (`ConjureNeutralSpec` holds `ConjureStep.hs` and `Step.hs` to
that).

**3. The writer** (`writerFor`). The name is `[conjure] writer`, else `[model] default` (the
harder writer: `[conjure] harder`). The backend is asked with `askBackendReply` under `[model]
timeout`. The `claude` kind passes `claudeArgs`, so no CLAUDE.md, hooks, plugins or tools ride
along. Specs replace the writer with `ssTestWriter`, or `scStepReplies` (the `step_replies`
init option: a file whose text every writer answers).

**4. The check** (`takeStep`). `readStep` reads `<goal>` and each `<file path="…">` block,
through prose or a markdown fence around them. `<goal>nothing</goal>` with no files answers
`StepNothing`. Each answered path goes through `keptPath`: inside the project, not under
`.git/` or `.olifant/`. Others are listed as `dropped`. Each kept file's base is the editor's
document (its version and text) when open, else the workspace's text, else nothing (a new
file). Each file is parsed with its language's grammar (`errorLines`). The test file runs again
over the step's files (`runOver` with `overlayMany`) when it exists or the step wrote it;
`testDelta` compares the runs. Newly failing means it passed before (a regression); a test
failing after that did not pass before (red already, new, or the file could not run because of
a syntax error) is still red.

**5. Shown.** The step is kept as a `StepHeld` in `ssShown` under `step-N` and answered as
`StepShown`. `editsOf` turns its writes into `FileEdit`s: a create for a new file, else
`smallestChange` at the version it was read at.

**6. The answer** (`answerStep`, a request with `resume: {token, choice}`). Every choice takes
the token, so a token answers once (`stepGone` after).

- **accept**: `movedSince` checks every written file is still at the version (or text) it was
  read at. If so the edits land as `acceptedEdit`. If not, nothing lands, the person's text
  stands, and the step is recorded as `dropped`.
- **reject**: nothing lands.
- **harder**: refused without `[conjure] harder`. Otherwise the same situation text goes to the
  harder writer. Only when it answers a step is the first one taken and recorded as `replaced`,
  and the new one shown under a new token; a failure or nothing to do leaves the first to answer.

**7. The record** (`record`). Every final choice appends one JSON line to
`.olifant/state/conjure-steps.jsonl`: time, file, unit, writer, goal, files, lines changed, the
tests' delta, whether all files parse, seconds, cost and the choice. The step log gets
`stepLogLine`: `conjure step claude-haiku-5-5 · fill the body · 2 files · +3 tests · accepted`.
The JSON lines are the corpus for refining the prompt and the writer choice from real use.

## The wire

```
request  olifant/conjure {textDocument, position, selection?, resume?: {token, choice: accept|reject|harder}}
answer   {"shown": {token, file, unit, writer, goal, edits, dropped, parses: [{file, ok}],
                    tests: {passing, failing, stillRed, noTests}, after, cost, seconds, harder,
                    diffs: [{file, diff}], situation, reply}}
       | {"nothing": goal}
       | {"landed": {file, edits}}
       | {"rejected": true}
```

`edits` is a workspace edit (`acceptedEdit`); `after` is `testsJson` of the after-run, or null;
`diffs` each touched file's `unifiedDiff` (the review draws them); `situation` and `reply` the
text the writer read and its raw answer (the review's `d`).
Any other choice is `InvalidParams` naming the three.

## Testing it

- `Server/StepSpec`: the reply reader (fences, prose, nothing), `keptPath`, the prompt's
  framing (no "least code", no language named), `testDelta`, the log line, the session filter,
  and `sessionDiff` on a temp repository.
- `ServerSpec` `stepSpecs`: a whole step over the python-htmx fixture with `ssTestWriter`:
  shown, no test file, red, a dropped path, nothing, unsaved text at its version, a failing
  writer, accept, reject, harder, a used token, a new conjure dropping the old step, a file
  moved before accept, and the wire's shape.
- `LoopSpec` `conjureFloorSpec`: a step lands in a hspec spec and in a Rust module's own
  `mod tests`.
- `LspSpec` `conjureSpec`: over pipes with `step_replies`: shown, accepted, a wrong choice
  refused.
- `ModelSpec`: the `claude` kind (its flags, a missing runner, a fake runner's reply, an error
  reply, fills through it). `CatalogSpec`: `[conjure] writer` and `harder`.

Run them with `cabal test spec --test-options='-m "is one step" -m Olifant.Server.Step'`, in
the main checkout.

## The seams, in one place

- **Config:** `[model]` (`default`, `timeout`, backends), `[conjure] writer`, `[conjure]
  harder`. `[model] rounds` and `[conjure] wire` are refused with a sentence naming what
  replaced them. `ladder`, `budget` and `escalate` stay for plans.
- **Ports:** `pTester`/`pTesterWithin` (the runner), `pSyntax`, `pFiles`; `ssTestWriter` and
  `scStepReplies` for specs.
- **State:** `ssShown` (steps by token, in memory: a restart forgets them).

## Rough spots

- **The step does not read sibling tests or the pack's `[conjure]` shapes.** Doctor still
  reports them. Feeding a sibling's facts into the situation, or retiring the shapes, is open.
- **The writer answers whole files.** A large module costs tokens both ways. The measurement's
  files were small; real use will say whether a unit-scoped answer is needed.
- **Jev does not read the situation yet.** The design's milestone 3 adds Jev picking the writer
  (local, default or harder) and a context cap for a local model.
