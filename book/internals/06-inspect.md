# 6. Inspect: the checker on code

Inspect is the plans' claim loop pointed at source files: on save, the units that differ from `HEAD`
are screened, sent to one find call, and the findings are placed, matched against what is stored,
filtered by Jev and verified before anything is shown. The user side is
[chapter 20 of the user book](../user/20-inspect.md). The design and its measurements are in the
notes (`code-checker-design-2026-11-15.md`, the 1a report).

The code is a seam on purpose: olifant will be split into smaller projects, and inspect should come
out whole. It lives in `src/Olifant/Inspect/`, has its own store and config table, and shares
exactly one function with plans (`Server/Loop.hs` `checkWith`). Inspect never reads the plans' store,
and plans never read inspect's.

## The modules

| module | owns |
|---|---|
| `Inspect/Steering.hs` | `.olifant/inspect.md`: `steering` reads the rules (headings under `## Rules`), `ruleText`, `rulesSection`, the two nitpick sections |
| `Inspect/Store.hs` | `InspectFinding`, `Status`, `Place`, `InspectStore`; `readInspectStore`, `updateInspectStore` (one lock, write-then-rename), `dismiss`, `shownIn` (the margin of one file) |
| `Inspect/Screen.hs` | the unfinished screen: `unitFacts` (parse errors, `TODO`/`FIXME`, empty body), `stopsEarly` (Jev), `unfinished` |
| `Inspect/Find.hs` | the find call: `findPrompt` (1a's prompt word for word), `parseFind`, `packUnits`, `liveFinder`, `recordedFinder` |
| `Inspect/Judge.hs` | `place`, `samePairs`, `sameProblem`, `matchStored` (the design's step-4 table), `nitpick`, `keepOf`, `citationsOk`, `claimVerdict` |
| `Inspect/Pass.hs` | `inspectPass`, the orchestrator; `changedOf`; the caps (`spentToday`, `findEstimate`, `claimEstimate`) |

The config is `InspectConfig` in `Catalog.hs` (`inspectIn`, beside `plansIn`), read into `catalogInspect`.

Two small tree-sitter calls were added for the screen, in `cbits/olifant_ts.c` and
`Language/TreeSitter.hs`: `errorLines` (the lines of `ERROR` and missing nodes) and `bodyEmpty`
(whether the node spanning a body's bytes holds only comments and lone strings). Both decide
by node type names, so no language is named in the code. The body node is `DefSpan.dsBody`
(`Language/Captures.hs`, the grammar's `body` field). A language with no `body` field (most
Haskell) simply has no empty-body fact. There is no placeholder list in any language (a
ruling at the 1a checkpoint): a body of `pass` is Jev's to judge.

## One pass, end to end

`inspectPass :: Cancel -> ServerState -> Project -> InspectDeps -> FilePath -> IO InspectOutcome`. `InspectDeps`
carries the four things a spec replaces: the Jev `Decider`, the `Finder`, the claim `Checker`, and
the time zone (`day_cost` is a local day).

1. **Skip** (`InspectSkipped why`, nothing written) when the file matches `exclude`, is refused by
   `Judge.readablePath` (`.env*`, a fence, outside the root), has no language, has no units, or
   the store does not read. The same check gates every file the pass reads for Jev's questions.
   Any Jev question that comes back `Unsure` (Jev failed) throws `JevSilent`, and `inspectPass` returns
   `InspectSkipped "Jev did not answer"` with nothing stored.
2. **Changed units.** `changedOf` reads the file at `HEAD` (`git show HEAD:./path` through
   `Process.gitIn`) and pairs its units with today's by name through `Server/Loop.hs`
   `compareUnits`, the plans' own comparison: `Added` or `Changed` units count. No `HEAD` text
   (untracked, no commit) means every unit. Units whose fingerprint is already in
   `vsInspectted` are dropped.
3. **Screen.** A code fact skips the unit without asking Jev. Otherwise one `weigh` of
   `stopsEarly` (its state: language, file, the unit's text, the enclosing unit's def line, the
   facts), unfinished at P ≥ 0.80. An unfinished unit leaves no record.
4. **Find.** `packUnits` cuts the finished units into calls of at most 600 lines. Before each
   call the estimate (`findEstimate`: the mean of the last 20 priced `inspect` errands, else $0.04 or
   $0.005 for haiku) is checked against `pass_cost` and `day_cost`. A pack that does not fit,
   and every pack after it, goes into `vsCapped`. Each call is logged as an `AnErrand` with verb
   `inspect` and mode `find`. The prompt carries the rule findings `Server/Fixes.hs` `findingsFor`
   reports, and the store's open and waiting findings on the pack's units (with ids, for
   still/no longer holds).
5. **Place.** `place` keeps a finding in a changed unit when its line range overlaps one (1a's
   rule; the innermost unit holding its first line is preferred), else `Elsewhere`. A rule that
   is not a heading in `inspect.md` is dropped.
6. **Match.** `samePairs` picks stored findings that share the file, the rule or a cited line. For
   each, one `sameProblem` question (P ≥ 0.5 is a match). `matchStored` turns a match into `Merge`,
   `Drop` or `New`. A merged `Withheld` finding is asked the nitpick question again only when the
   hash of (unit fingerprint, rule text, criteria wording) in `vfAskedOn` changed. A merged
   `Refuted` or `Unsure` one is verified again only when `citationsOk` gives different prints.
7. **Filter.** `nitpick` (1a's wording; `inspect.md`'s two sections replace the criteria) gives
   `vfKeep = 1 − P(nitpick)`. Below `cut` is `Withheld`. Otherwise the finding is `Waiting` to be
   verified. Elsewhere findings skip Jev.
8. **Verify,** best keep first, until `max_shown` of this pass's findings of a shown kind are
   `Open`. `citationsOk` sends each evidence range's first line through `Server/Checker.hs`
   `confirm` (fences, `.env*`, symlinks, the line inside the file), which is the clamp ruling: the
   end line is never checked. A `bug` or `assumption` then goes through `checkWith "inspect-check"`
   (the plans' cascade, the statement as a bare prose `Sentence`, as 1a measured it), within the
   caps (`claimEstimate`, else $0.016).
9. **Close.** Ids the find call says no longer hold become `Resolved`. Findings in units that left
   the file become `Gone`. The file is read again: a unit whose fingerprint moved while the pass
   ran loses its new findings and is not marked inspected. Then `vsInspectted` and `vsCapped` are updated.

The pass works on a copy of the store and ends with one `updateInspectStore`, which applies
`mergeInto base mine current`. A finding the pass changed is taken only if its status in the store
is still the one the pass read, so a dismissal made while a call was running stands.

## In the language server

`Lsp.hs` owns the editor side:

- **The trigger.** The `didSave` handler calls `inspectLater`. It runs only when `[inspect] enabled`,
  `deciderFor` gives a decider, and the file is not a plan. Through `Jobs.submit` with key
  `inspect <path>` it waits `settle` seconds, and a newer save restarts the wait. The work ignores
  the job's token, so a pass that has started is not stopped by the next save. Each pass gets
  its own `Cancel`.
- **One pass at a time.** `envInspectLock` serialises passes, so each sees what the one before
  stored. The in-flight check runs before waiting for the lock.
- **Calls in flight.** `envInspectFlight` holds, per file, the `(unit, fingerprint)` pairs the
  running find call was sent and that call's own kill (each call gets its own `Cancel`, so a kill
  never reaches the pass's claim checks; a claim check that does not finish leaves its finding
  `Waiting`). A new pass leaves
  out the units whose fingerprint still holds (`vdSkip`). When none holds, it kills the old
  call. `unitFingerprints` (in `Inspect/Pass.hs`) reads the file's units now.
- **After a pass.** A pass that changed the store sends `workspace/diagnostic/refresh`. A pass
  the day cap stopped (`voDayCapped`) sends one `window/showMessage` a local day
  (`capNoticeDue`, `envInspectCapDay`).
- **The margin.** `diagnosticsOf` appends `inspectDiagnosticsFor`, which is `Inspect/Margin.hs`
  `inspectDiagnostics` over the store and the file's units now (fingerprint and first line).
  `shownLines` moves a finding with its unit (`Place.plUnitStart`) and drops one whose unit is
  gone; `inspectActions` uses the same lines. Each diagnostic carries its
  finding id in `data`.
- **The actions.** `inspectActions` offers the four actions on a shown finding's line. The commands
  are `olifant.inspectShowWhy` (`inspectWhyFile` writes `whyText`, opened with `window/showDocument`)
  and `olifant.inspectDismiss` (`dismiss` through `updateInspectStore`, then a refresh and `inspectLater`, so
  a waiting finding can take the freed place). *Show code* reuses `olifant.showCode`.
  `inspectContext` is the shared lookup: project, `[inspect]`, the relative path, the store.
- **Doctor.** `Doctor.hs` `inspectRow` (only when on) reads the store's JSON and version itself.
  Importing `Inspect/Store.hs` there would close a cycle (Store, `Server/Checker.hs`, `App.hs`,
  `Doctor.hs`).

durandal.nvim's `lua/durandal/inspect.lua` draws the sign. It wraps Neovim's `signs` and
`virtual_text` handlers to drop `olifant-inspect`, because a namespace's config cannot filter by
source.

## The seam with plans: `checkWith`

`checkSentence` is now `checkWith "check"`. The verb goes to the errand record, and only
`"check"` moves `ssEstimates`, so inspect's claim checks (`inspect-check`) never change what a plan's
*Check this* says it will cost. `olifant plans cost` drops records whose verb is `inspect` or
`inspect-check` (`Cli.hs` `inspectRecord`), and `olifant inspect cost` (`runInspectCost`) counts only those.

## Testing it

- `LspSpec` `inspectSpec` drives a real server in-process: settle, no Jev, not enabled, a plan's save,
  the cap message (`inProcessShowing`: lsp-test drops `window/showMessage` unless told), the
  pulled hint, the four actions, *Dismiss here*, *Show why*, hover untouched.
- `Inspect/*Spec.hs` unit specs never call a model. `PassSpec` builds a git repository from the
  python-htmx fixture (`LspSpec.withPython`), commits, edits, and drives `inspectPass` with
  `recordedDecider` (answers by question kind), `recordedFinder` (replies by `findKey`, the
  sorted `file:unit` pairs; `"delay"` holds a reply back for in-flight tests) and
  `recordedChecker`.
- `FindSpec` checks `findPrompt` against `testdata/find-prompt.golden`, generated from 1a's
  Python `find_prompt` with the same inputs.
- `CorpusSpec` is the replay gate on 1a's 58 bistro cases. It is pending unless
  `OLIFANT_INSPECT_CORPUS=<notes>/experiments`. Each case clones the conventions base, applies its
  patch, writes 1a's `inspect.md`, and runs `inspectPass` on each changed file. The find calls replay 1a's
  sonnet findings (`inspect-corpus-finds.json`, split by file), the claim checks 1a's verdicts
  (`inspect-corpus-checks.json`), and Jev `inspect-corpus-jev.json`. `OLIFANT_RECORD_JEV=1` asks Jev only
  what is missing (`recordMissing`); `OLIFANT_RECORD_CHECKS=1` asks sonnet for a claim 1a never
  checked; `OLIFANT_INSPECT_CASES=vm02,b01` looks at a few cases. The data comes from
  `experiments/vet-1b-2026-11/finds.py`. Bars: at least 16/18 must-rule violations and 6/8 bugs
  shown, at most 0.5 findings shown per rule-following twin and per near miss. At 1b: 17/18,
  6/8, 0.00 and 0.125.

## Rough spots

- The gate's claim checks run with `check_model = "sonnet"` and no Jev guards, so replay stays
  deterministic. Production runs the cascade.
- Jev's answers move a little between wordings of the same state: vm16 kept 0.78 here against
  1a's 0.84, just under `cut`.
- Only the first shown finding on a line gets actions: a second one on the same line is reached
  after the first is dismissed.
- `envInspectFlight` and the cap day live in the server process. A restart forgets them, at worst one
  more message and one call that is not killed.
