# 20. Vet: the checker on code

Vet reads the code you just saved, checks it against the codebase and your project's own
rules, and keeps the few things worth knowing. It is the plans' loop (triage, a checker that
searches, evidence, a quiet margin) applied to source files. It says nothing about a function you
are halfway through. It does not nitpick. It shows a problem once, not on every save.

Vet is off until a project turns it on, and it runs only when a Jev backend is configured: Jev is
what tells a substantial finding from a nitpick, and without it the margin would be noisy.

## Turning it on

```toml
# .olifant/config.toml
[vet]
enabled = true

[decide]
model = "jev"

[model.backends.jev]
kind = "jev"          # the key is read from JEV_API_KEY
```

Then write `.olifant/vet.md` ([The steering file](#the-steering-file)). Without one vet runs with
no project rules: it still finds bugs and wrong assumptions, but it cannot find a broken convention.

## What happens on save

1. **Which code.** The units (functions, methods, classes) of the saved file that differ from
   `HEAD`, by fingerprint, so a formatter's reflow is not a change. Every unit of an untracked
   file counts, and so does every unit in a repository with no commit. A unit vet already looked
   at, as it is now, is not looked at again. After a commit nothing is changed.
2. **Finished?** A unit that does not parse, holds a `TODO` or `FIXME`, or has an empty body
   (only comments or a docstring) is skipped silently. So is one Jev judges to stop before doing
   what its name promises. It is screened again on the next save.
3. **Find.** One `claude -p` call (`find_model`, read-only tools, the plans' fences:
   `[plans] not_evidence`, the plans directory, `.olifant`) reads the changed units, their diff,
   your `vet.md`, the rule findings `olifant check` already reports, and the findings still open
   on those units. It answers whether each open one still holds, and lists new problems of four
   kinds:

   | kind | what |
   |---|---|
   | `bug` | the code does the wrong thing |
   | `assumption` | the code relies on something about other code that is not so |
   | `convention` | the code breaks a rule in `vet.md` |
   | `design` | readability, maintainability, testability or architecture with no rule behind it |

   A large save is split into calls of at most 600 lines each.
4. **Place and match.** A finding that does not touch a changed unit is recorded as `elsewhere`
   and not shown. Jev asks whether a new finding is the same problem as one already stored. If
   so, it is merged (seen once more), or dropped when you dismissed it.
5. **Filter.** Jev asks "is this a nitpick?" with the rule the finding cites (or all your rules).
   A finding below `cut` is `withheld`.
6. **Verify.** The code each finding cites must exist (an end line past the end of the file is
   fine; a start line past it is not). A `bug` or `assumption` then goes through the plans' claim
   check: right is shown, wrong is `refuted`, can't tell is `unsure`. Neither is shown. A pass
   verifies until `max_shown` findings have passed; the rest are `waiting`.

A finding is `resolved` only when the find call, shown it on a later save, says it no longer
holds. A finding whose unit is deleted is `gone`.

## `[vet]`

| key | default | what |
|---|---|---|
| `enabled` | `false` | vet runs on save in this project |
| `kinds` | `["bug", "assumption", "convention"]` | kinds the margin shows (`design` is recorded, listed, not shown) |
| `find_model` | `"sonnet"` | the find call's model (haiku missed a planted bug in 1a's measurements) |
| `cut` | `0.79` | kept when 1 − P(nitpick) is at least this |
| `max_shown` | `3` | findings shown a file |
| `settle` | `2` | seconds after the last save before a pass starts |
| `pass_cost` | `0.25` | dollars a save may spend |
| `day_cost` | `3.00` | dollars a local day may spend, this project |
| `exclude` | `[]` | globs never vetted |

Unknown keys are refused. `cut` is between 0 and 1, `max_shown` at least 1, the costs and
`settle` at least 0.

## The steering file

`.olifant/vet.md` is Markdown you write and change. All of it goes to the find call on every
save. Each heading under `## Rules` is a rule: its name is what a finding cites, and its text is
what Jev reads when it judges that finding.

```markdown
# Bistro conventions

## Rules

### Commands and queries go through ports (must)

A command or query reaches stored data only through a port's Protocol. `sqlite3` is imported in
an adapter (`ports/<port>/sqlite_<port>.py`) and nowhere else.

### Time comes from the clock port (must)

…

## A nitpick is

only wording and formatting

## Worth saying is

anything that changes behaviour or breaks a rule above
```

`## A nitpick is` and `## Worth saying is` replace the two sides of Jev's nitpick question. They
are the lever: in the experiments, rewording them moved the results far more than anything else.
Grades such as *(must)* help the find call; Jev did not act on them.

## What it costs

Measured on 1a's 58 cases (a small Python project): a vetted save is about $0.04 on sonnet
(the find call about $0.037, claim checks under a cent on average), about $0.004 on haiku. A save
with no changed, finished, unvetted unit costs nothing. Jev's questions cost about $0.00005 each and
are not counted against the caps.

Before each paid call vet checks `pass_cost` and `day_cost` (summed from
`.olifant/state/errands.jsonl`, verbs `vet` and `vet-check`). A unit a cap stops is not marked
vetted, so the next save with budget vets it.

```sh
olifant vet cost
```

```
day         finds  checks  find $  check $  total $
2026-11-14  1      1       0.04    0.01     0.05
2026-11-15  1      0       0.03    0.00     0.03
waiting on the cap: 2 units
```

`--since YYYY-MM-DD` keeps the days from then on; `--json` gives the rows. `olifant plans cost`
leaves vet's errands out.

## What it found

```sh
olifant vet list                    # every finding, open first
olifant vet list --status withheld  # what the nitpick filter kept back
olifant vet list --json
```

The statuses are `open`, `waiting`, `withheld`, `refuted`, `unsure`, `elsewhere`,
`dismissed-here`, `dismissed-everywhere`, `resolved` and `gone`. Everything lives in
`.olifant/state/vet/findings.json`: every finding with its Jev score, verdicts and how often it was
seen, so a change to `vet.md` or to `cut` can be judged against the record first. If that file
stops reading (corrupt, or written by a newer olifant), vet pauses for the project rather than
start over: start over and dismissed findings would come back. Move it aside to start again.

## In the editor

Vet runs in the language server (`olifant lsp`), on save. After `settle` seconds with no newer
save of the file, one pass runs: a burst of saves is one pass. Nothing is drawn while it runs.
When it finds something, diagnostics are refreshed, and the file's shown findings arrive as hints
(source `olifant-vet`, code = the kind, on the finding's first line). A finding whose unit changed
since it was vetted keeps its place with the code `<kind>-stale`, until a save vets the unit
again. A save that changes the units a find call is still working on stops that call. Hover and
go-to-definition are untouched.

On a finding's line the code actions are:

| action | what |
|---|---|
| *Show why* | opens `.olifant/state/vet/why-<id>.md`: the statement, the rule quoted, the evidence, the scores |
| *Show code* | opens the first piece of evidence |
| *Dismiss here* | this finding, in this unit, does not come back, whatever you edit |
| *Dismiss everywhere* | the same problem does not come back anywhere in the project |

A dismissal frees a place: the best `waiting` finding of the file is verified next.

The first time a local day's `day_cost` stops a pass, one message says so: `olifant vet: today's
cap ($3.00) is reached; 4 units wait. olifant vet cost shows them.`

In durandal.nvim a finding is one sign a row: `!` bug, `?` assumption, `§` convention
(`vet = { glyphs = { … } }`; highlight `DurandalVetSign`, a stale one `DurandalPlanStale`).
Neovim's own signs and virtual text leave vet's findings out, so each shows once. The words are
in the diagnostic float.

`olifant doctor` shows a `vet` row when vet is on: `on: Jev jev, vet.md with 13 rules`; a
warning when no Jev backend is configured (vet does not run); an error when the store does not
read (vet is paused).
