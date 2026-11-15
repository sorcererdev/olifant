# 6. Vet: the checker on code

Vet is the plans' claim loop pointed at source files: on save, the units that differ from `HEAD`
are screened, sent to one find call, and the findings are placed, matched against what is stored,
filtered by Jev and verified before anything is shown. The user side is
[chapter 20 of the user book](../user/20-vet.md). The design and its measurements are in the
notes (`code-checker-design-2026-11-15.md`, the 1a report).

The code is a seam on purpose: olifant will be split into smaller projects, and vet should come
out whole. It lives in `src/Olifant/Vet/`, has its own store and config table, and shares
exactly one function with plans (`Server/Loop.hs` `checkWith`). Vet never reads the plans' store,
and plans never read vet's.

## The modules

| module | owns |
|---|---|
| `Vet/Steering.hs` | `.olifant/vet.md`: `steering` reads the rules (headings under `## Rules`), `ruleText`, `rulesSection`, the two nitpick sections |
| `Vet/Store.hs` | `VetFinding`, `Status`, `Place`, `VetStore`; `readVetStore`, `updateVetStore` (one lock, write-then-rename), `dismiss`, `shownIn` (the margin of one file) |
| `Vet/Screen.hs` | the unfinished screen: `unitFacts` (parse errors, `TODO`/`FIXME`, empty body), `stopsEarly` (Jev), `unfinished` |
| `Vet/Find.hs` | the find call: `findPrompt` (1a's prompt word for word), `parseFind`, `packUnits`, `liveFinder`, `recordedFinder` |
| `Vet/Judge.hs` | `place`, `samePairs`, `sameProblem`, `matchStored` (the design's step-4 table), `nitpick`, `keepOf`, `citationsOk`, `claimVerdict` |
| `Vet/Pass.hs` | `vetPass`, the orchestrator; `changedOf`; the caps (`spentToday`, `findEstimate`, `claimEstimate`) |

The config is `VetConfig` in `Catalog.hs` (`vetIn`, beside `plansIn`), read into `catalogVet`.

Two small tree-sitter calls were added for the screen, in `cbits/olifant_ts.c` and
`Language/TreeSitter.hs`: `errorLines` (the lines of `ERROR` and missing nodes) and `bodyEmpty`
(whether the node spanning a body's bytes holds only comments and lone strings). Both decide
by node type names, so no language is named in the code. The body node is `DefSpan.dsBody`
(`Language/Captures.hs`, the grammar's `body` field). A language with no `body` field (most
Haskell) simply has no empty-body fact. There is no placeholder list in any language (a
ruling at the 1a checkpoint): a body of `pass` is Jev's to judge.

## One pass, end to end

`vetPass :: Cancel -> ServerState -> Project -> VetDeps -> FilePath -> IO VetOutcome`. `VetDeps`
carries the four things a spec replaces: the Jev `Decider`, the `Finder`, the claim `Checker`, and
the time zone (`day_cost` is a local day).

1. **Skip** (`VetSkipped why`, nothing written) when the file matches `exclude`, has no language,
   has no units, or the store does not read.
2. **Changed units.** `changedOf` reads the file at `HEAD` (`git show HEAD:./path` through
   `Process.gitIn`) and pairs its units with today's by name through `Server/Loop.hs`
   `compareUnits`, the plans' own comparison: `Added` or `Changed` units count. No `HEAD` text
   (untracked, no commit) means every unit. Units whose fingerprint is already in
   `vsVetted` are dropped.
3. **Screen.** A code fact skips the unit without asking Jev. Otherwise one `weigh` of
   `stopsEarly` (its state: language, file, the unit's text, the enclosing unit's def line, the
   facts), unfinished at P ≥ 0.80. An unfinished unit leaves no record.
4. **Find.** `packUnits` cuts the finished units into calls of at most 600 lines. Before each
   call the estimate (`findEstimate`: the mean of the last 20 priced `vet` errands, else $0.04 or
   $0.005 for haiku) is checked against `pass_cost` and `day_cost`. A pack that does not fit,
   and every pack after it, goes into `vsCapped`. Each call is logged as an `AnErrand` with verb
   `vet` and mode `find`. The prompt carries the rule findings `Server/Fixes.hs` `findingsFor`
   reports, and the store's open and waiting findings on the pack's units (with ids, for
   still/no longer holds).
5. **Place.** `place` keeps a finding in a changed unit when its line range overlaps one (1a's
   rule; the innermost unit holding its first line is preferred), else `Elsewhere`. A rule that
   is not a heading in `vet.md` is dropped.
6. **Match.** `samePairs` picks stored findings that share the file, the rule or a cited line. For
   each, one `sameProblem` question (P ≥ 0.5 is a match). `matchStored` turns a match into `Merge`,
   `Drop` or `New`. A merged `Withheld` finding is asked the nitpick question again only when the
   hash of (unit fingerprint, rule text, criteria wording) in `vfAskedOn` changed. A merged
   `Refuted` or `Unsure` one is verified again only when `citationsOk` gives different prints.
7. **Filter.** `nitpick` (1a's wording; `vet.md`'s two sections replace the criteria) gives
   `vfKeep = 1 − P(nitpick)`. Below `cut` is `Withheld`. Otherwise the finding is `Waiting` to be
   verified. Elsewhere findings skip Jev.
8. **Verify,** best keep first, until `max_shown` of this pass's findings of a shown kind are
   `Open`. `citationsOk` sends each evidence range's first line through `Server/Checker.hs`
   `confirm` (fences, `.env*`, symlinks, the line inside the file), which is the clamp ruling: the
   end line is never checked. A `bug` or `assumption` then goes through `checkWith "vet-check"`
   (the plans' cascade, the statement as a bare prose `Sentence`, as 1a measured it), within the
   caps (`claimEstimate`, else $0.016).
9. **Close.** Ids the find call says no longer hold become `Resolved`. Findings in units that left
   the file become `Gone`. The file is read again: a unit whose fingerprint moved while the pass
   ran loses its new findings and is not marked vetted. Then `vsVetted` and `vsCapped` are updated.

The pass works on a copy of the store and ends with one `updateVetStore`, which applies
`mergeInto base mine current`. A finding the pass changed is taken only if its status in the store
is still the one the pass read, so a dismissal made while a call was running stands.

## The seam with plans: `checkWith`

`checkSentence` is now `checkWith "check"`. The verb goes to the errand record, and only
`"check"` moves `ssEstimates`, so vet's claim checks (`vet-check`) never change what a plan's
*Check this* says it will cost. `olifant plans cost` drops records whose verb is `vet` or
`vet-check` (`Cli.hs` `vetRecord`), and `olifant vet cost` (`runVetCost`) counts only those.

## Testing it

- `Vet/*Spec.hs` unit specs never call a model. `PassSpec` builds a git repository from the
  python-htmx fixture (`LspSpec.withPython`), commits, edits, and drives `vetPass` with
  `recordedDecider` (answers by question kind), `recordedFinder` (replies by `findKey`, the
  sorted `file:unit` pairs; `"delay"` holds a reply back for in-flight tests) and
  `recordedChecker`.
- `FindSpec` checks `findPrompt` against `testdata/find-prompt.golden`, generated from 1a's
  Python `find_prompt` with the same inputs.
- `CorpusSpec` is the replay gate on 1a's 58 bistro cases. It is pending unless
  `OLIFANT_VET_CORPUS=<notes>/experiments`. Each case clones the conventions base, applies its
  patch, writes 1a's `vet.md`, and runs `vetPass` on each changed file. The find calls replay 1a's
  sonnet findings (`vet-corpus-finds.json`, split by file), the claim checks 1a's verdicts
  (`vet-corpus-checks.json`), and Jev `vet-corpus-jev.json`. `OLIFANT_RECORD_JEV=1` asks Jev only
  what is missing (`recordMissing`); `OLIFANT_RECORD_CHECKS=1` asks sonnet for a claim 1a never
  checked; `OLIFANT_VET_CASES=vm02,b01` looks at a few cases. The data comes from
  `experiments/vet-1b-2026-11/finds.py`. Bars: at least 16/18 must-rule violations and 6/8 bugs
  shown, at most 0.5 findings shown per rule-following twin and per near miss. At 1b: 17/18,
  6/8, 0.00 and 0.125.

## Rough spots

- The gate's claim checks run with `check_model = "sonnet"` and no Jev guards, so replay stays
  deterministic. Production runs the cascade.
- Jev's answers move a little between wordings of the same state: vm16 kept 0.78 here against
  1a's 0.84, just under `cut`.
- The editor side (the trigger on save, the margin, the actions) arrives with the editor part of
  milestone 1b.
