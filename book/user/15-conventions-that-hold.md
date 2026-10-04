# 15. Conventions that hold

Generating code that follows a project's conventions is half the job. The other half is
keeping them true when people, and agents, write code by hand: a command that imports Diesel
instead of going through a port, a module file nobody declared, a command with no test. The
architecture should survive the lead's vacation.

Olifant keeps conventions in two layers, declared in packs next to the generators (or in a
project's `.olifant/`):

- **Regions**, for anything that can be derived: a module list, a registry. Olifant keeps
  them true and nobody edits them ([chapter 5](05-owned-regions.md)).
- **Rules**, for anything that is a judgement: a query over the code, the reason it matters,
  a severity, and optionally a generator that fixes it. Rules warn and offer; they never edit
  your code.

`olifant check` checks both. This chapter is about rules: how to write one, how to make an
exception, how to bring in a new rule without failing on every old file, and where the
checks run (CI, hooks, the editor). It ends with a short tutorial.

## A rule

rust-htmx has two, in `packs/rust-htmx/rules.toml`:

```toml
[rules.commands-use-ports]
files    = "{service}/*-service/src/*.rs"
exclude  = ["**/lib.rs", "**/service.rs", "**/models.rs", "**/ports.rs"]
match    = { any = [
  { kind = "use_declaration", regex = "diesel" },
  { kind = "scoped_identifier", regex = "^diesel::" },
  { kind = "scoped_identifier", regex = "::(table|dsl)$" },
] }
message  = "Commands talk to ports, not the database (rust-htmx: ports & adapters)"
severity = "warn"

[[rules.commands-use-ports.fixes]]
generator = "rust-htmx/command-dependency"
label     = "Use an existing port"
with      = { service = "{= service =}", command = "{= stem =}" }

[[rules.commands-use-ports.fixes]]
generator = "rust-htmx/service-repository"
label     = "Add a new repository port"
with      = { service = "{= service =}" }

[rules.commands-no-unwrap]
files    = "*/*-service/src/*.rs"
exclude  = ["**/lib.rs", "**/service.rs", "**/models.rs", "**/ports.rs"]
match    = { kind = "call_expression", regex = "\\.unwrap\\(\\)$" }
message  = "Commands return their Failure, they don't panic (use ? and a Failure variant)"
severity = "hint"
```

Start writing Diesel in a command — `let rows = invoices::table.filter(invoices::id.eq(1)).load::<Invoice>(&mut conn);`,
no import needed — in a rust-htmx sandbox (`scripts/try-pack.sh rust-htmx /tmp/rh && cd /tmp/rh`), and:

```
$ olifant check
! billing/billing-service/src/cancel_invoice.rs:15  commands-use-ports — Commands talk to ports, not the database (rust-htmx: ports & adapters)  (fixes: Use an existing port: InvoiceRepository; Add a new repository port)
conventions hold (1 finding, none blocking)
```

The rule matches any Diesel in a command — an import, a `diesel::` path, a schema table — so it
fires where you start, and nested or repeated matches on one line are one finding.

| key | required | what it does |
|---|---|---|
| `files` | yes | the files the rule applies to: a pattern over the project-relative path, as in `[applies_at] path` ([chapter 14](14-editors-and-actions.md#applies_at-where-a-generator-applies)). `*` matches within a segment, `**` across segments, and `{name}` matches one segment (or the part of one between literals) and binds `name`, which becomes an input of the fix |
| `exclude` | no | globs of files to leave out |
| `match` | one of `match`, `missing` | an ast-grep rule, written as in an AST query ([chapter 6](06-queries-and-choices.md)). Each node it matches is a finding. It is a template over the `files` slots, so `regex = "^pub struct {= command\|pascal =} "` works. The file needs a language adapter |
| `missing` | one of `match`, `missing` | a path that must exist, as a template over the file: `{= dir =}` (its directory), `{= stem =}` (its name without extension) and `{= file =}` (its path). `..` is allowed. If the path does not exist, the file has a finding with no line |
| `message` | yes | what is wrong and why it matters, in one sentence |
| `severity` | no | `hint`, `warn` (the default) or `error`. Only an `error` fails `check`. A project can change it ([below](#the-projects-say-rules)) |
| `fix` | no | the id of a generator that fixes it (shorthand for one entry in `fixes`) |
| `fixes` | no | `[[rules.ID.fixes]]` tables, best first: `generator`, `label` (what an editor shows), `with` (its inputs, templates over the `files` slots and `{= dir =}`, `{= stem =}`, `{= file =}`; without it, the slots as they are). A fix naming no generator, or an input the generator doesn't have, makes the rule broken |
| `since`, `why` | no | when the rule arrived (`"2026-10"`) and why (a sentence, or an ADR), shown after the message: `… (since 2026-10: commands are the behaviour; untested ones rot)` |

Rules are read from every pack's `rules.toml`, then the project's `.olifant/rules.toml`; a
project rule with a pack rule's id replaces it.

Rules are ast-grep queries and globs: no compiler and no model, so a check takes a moment, and
an editor can run one on every save.

### The fixes

A rule offers fixes, best first. `commands-use-ports` offers the usual answer before the bigger
one: **use the port the service already has** (`rust-htmx/command-dependency`, "Add Command
Dependency": the field, its import and the wiring), then **add a new repository port**
(`rust-htmx/service-repository`). Each fix's `with` maps the finding to the generator's inputs —
`service` from the path, `command` from the file's name — and an editor asks only for the rest
([chapter 14](14-editors-and-actions.md#rule-findings-as-diagnostics)).

An enum input a fix leaves open is chosen for you when exactly one choice's words appear in the
code the finding is about: `invoices::table` names the `invoice` in `invoice_repository` (a
port's kind, like `repository` or `sender`, is ignored), so the first fix arrives as "Use an
existing port: InvoiceRepository" with `dependency = "invoice_repository"` already set. With no
clear match, the editor asks, offering this service's ports.

### Fixes that remove the code they found

A fix that adds the port but leaves the Diesel behind is half a fix. A fix can say what to do
with the code its finding sits in, as `replace` cases — the first whose node encloses the
finding wins:

```toml
replace = [
  { enclosing = ["use_declaration"], text = "" },          # an import: delete it
  { enclosing = ["let_declaration", "expression_statement"],
    text = "{= ? port_call: stmt | the same as `{= finding =}` but through self.{= dependency =} =}" },
]
context = ["{= service|kebab =}/{= service|kebab =}-service/src/ports/{= dependency =}.rs"]
```

- `enclosing`: node kinds; the nearest one around the finding is replaced, whole lines.
- `text`: what goes there — empty deletes the lines; it is a template over the fix's inputs and
  `{= finding =}` (the node's code), and may hold holes ([chapter 12](12-holes-and-models.md)).
  rust-htmx has a `stmt` hole kind for this: its placeholder ends with `;`.
- `context`: files the model sees when it fills those holes — here the port's trait, so it calls
  methods that exist.

`olifant fix RULE --at FILE:LINE:COL [--fix N] [--plan]` runs one: the replacement first (its
lines must still read what the rule saw, or it is a conflict: check again), then the fix's
generator, as one plan with the usual fill phase. In a rust-htmx sandbox with
`let invoice = invoices::table.find("inv-1").first::<Invoice>(&mut conn).ok();` typed into
`cancel_invoice`:

```
$ olifant fix commands-use-ports --at billing/billing-service/src/cancel_invoice.rs:15:24 --human
…
holes:
  port_call (stmt) billing/billing-service/src/cancel_invoice.rs:19  filled by qwen3-coder:30b (attempt 2): let invoice = self.invoice_repository.get_invoice("inv-1".to_string()).await.map_err(…)?;
```

The first attempt didn't compile (`?` couldn't turn the port's `RepositoryFailure` into
`CancelInvoiceFailure`); the compiler's message went back with the retry. Without a model the
line becomes `todo!("olifant: port_call: the same as `…` but through …");`, which compiles: the
Diesel is gone either way, and the decision is visible. A statement that can't be rewritten
(it uses something that doesn't exist) stays that placeholder.

Nothing runs the fix for you. It is an offer, like an action: you review the plan and apply
it, or not.

### What it does not see

- A `match` rule needs the file's language adapter (`rust` here): in a file without one,
  it finds nothing.
- A rule never sees regions or generated files differently from hand-written code. Exclude
  them in `exclude` if they would match.

## Exceptions

A rule that can't be silenced gets turned off wholesale, and one that is silenced invisibly
stops meaning anything. So every exception is possible, local, and reported.

**One place.** A comment on the line, or on the line above, with a reason after `—` (or `-`,
or `:`):

```rust
use diesel::prelude::*; // olifant:allow commands-use-ports — spike for the demo, removed in #123
```

```
$ olifant check
conventions hold (0 findings, none blocking)
```

**A file.** `olifant:allow-file RULE — reason` in one of the file's first five lines
silences the rule for the whole file. It is also how to silence a `missing` finding, which
has no line:

```rust
// olifant:allow-file commands-have-tests — void is being replaced by refunds (#88)
use std::sync::Arc;
```

Both must be real comments, in the file's language's comment syntax: the same text inside a
string (`"// olifant:allow …"`) is not an allow. A file with no language adapter can't carry
one, so use the project's `[rules]` instead.

**Paths.** In `.olifant/config.toml`, with a reason:

```toml
[[rules.allow]]
rule   = "commands-have-tests"
paths  = ["notifications/**"]
reason = "notifications is tested end to end in the integration suite"
```

**Mistakes are findings too**, `warn`s with a line, so they show where the allow is:

```
! billing/billing-service/src/cancel_invoice.rs:2  allow-without-reason — olifant:allow commands-use-ports needs a reason: `— why`
! billing/billing-service/src/cancel_invoice.rs:2  unknown-rule — olifant:allow names no rule `commands-use-port`
```

An allow without a reason still suppresses the finding; it just warns until it has one. An
allow naming no rule (a typo) suppresses nothing, so the finding it meant to silence shows
too.

**Everything, in one place.** `check --report` lists every exception with where it is and
why, and marks one that no longer suppresses anything as stale (the code it excused is gone,
or never matched):

```
$ olifant check --report
· billing/billing-service/src/cancel_invoice.rs  commands-have-tests — Every command has a test file (…)  (existing)
· billing/billing-service/src/refund_invoice.rs  commands-have-tests — Every command has a test file (…)  (existing)
exceptions:
  commands-have-tests at billing/billing-service/src/void_invoice.rs:1: void is being replaced by refunds (#88)
  commands-have-tests at paths notifications/**: notifications is tested end to end in the integration suite
  commands-use-ports at paths billing/legacy/**: legacy billing talks to the database directly  [stale: suppresses nothing]
debt: commands-have-tests 2 existing
conventions hold (2 findings, 2 existing)
```

(This is the project from the [tutorial](#tutorial-a-rule-of-your-own), with one more
`[[rules.allow]]` for a `billing/legacy/` that doesn't exist.) A plain `check` doesn't list
exceptions; only `--report` does.

`--report --json` has `exceptions` (`rule`, `where`, `reason`, `stale`) and `rules` (`id`,
`severity` as the project has it, and `debt`, [below](#a-new-rule-the-ratchet)).

## The project's say: `[rules]`

How strict to be is the team's choice, per rule, in `.olifant/config.toml`:

```toml
[rules]
"commands-have-tests" = "error"   # or hint, warn, off
"commands-no-unwrap"  = "off"
```

`off` turns a rule off for the project; a rule's own `severity` can't be `off`. An unknown
severity is a config error that stops every command (`` `rules`: `commands-use-ports`:
unknown severity `loud` (hint, warn, error, off) ``).

## `olifant check`

```
olifant check [--json] [--report] [--baseline] [--changed-since REF]
olifant check --file FILE [--content -]
```

Without options, `check` refreshes nothing and writes nothing. It checks every region
(as `sync --check` does) and every rule over the project's files, and prints the findings,
then a verdict:

```
$ olifant check
region modules out of date in billing/billing-service/src/lib.rs (run olifant sync)
check failed
$ echo $?
1
```

It exits 1 when:

- a region is out of date, broken, or not set up;
- a rule is broken (a `rules.toml` that doesn't parse, or a `fix` naming no generator):

  ```
  ✗ rules.toml: rule `commands-have-tests`: fix names no generator `rust-htmx/command-test`
  ```

  A broken rule is left out, so allows that name it are reported as `unknown-rule`;
- a finding is an `error` and not in the baseline.

Warnings and hints never fail it, and neither does a baselined error. A finding's mark is `✗`
for an error, `!` for a warning and `·` for a hint or a baselined finding. The verdict counts
findings: `conventions hold (2 findings, 2 existing)`.

`--file FILE` checks one file's rules, and only rules, for editors; `--content -` reads the
file's text from stdin, so an unsaved buffer can be checked. It prints the findings and
always exits 0:

```
$ olifant check --file billing/billing-service/src/refund_invoice.rs
· billing/billing-service/src/refund_invoice.rs:19  commands-no-unwrap — Commands return their Failure, they don't panic (use ? and a Failure variant)
```

The JSON is described in [chapter 2](02-running-generators.md#do-the-conventions-hold-olifant-check).

## Changing conventions

### A new rule: the ratchet

A new rule, or a stricter one, usually finds plenty of old code that breaks it. Fixing all of
it first means a giant PR; making the rule a warning means nobody notices new breakage. The
**baseline** is the middle: record what is there today, fail only on what is new, and let
the debt go down as people touch the code.

```
$ olifant check --baseline
baselined 2 findings in .olifant/baseline.toml
```

```toml
# Findings recorded by `olifant check --baseline` (spec §5.10): known debt.
# check fails only on findings not listed here; re-run --baseline as debt is paid.

[[baseline]]
rule = "commands-have-tests"
file = "billing/billing-service/src/cancel_invoice.rs"
fingerprint = "1ada832f77b2b12f"
```

Commit it. From then on a baselined finding is shown as `·` with `(existing)`, and never
fails `check`, whatever its severity:

```
· billing/billing-service/src/cancel_invoice.rs  commands-have-tests — Every command has a test file (…)  (existing)
```

- **A fingerprint is what the finding is about**, not where: the rule and the matched code
  (for `missing`, the missing path), whitespace collapsed. Lines can move above it and it is
  still the same finding. A file rename makes it new.
- **Debt only goes down.** Paying one off (fixing the code) makes its entry unused; the next
  `--baseline` leaves it out. `--baseline` writes the whole file from today's findings, so
  run it only to record less debt, not to bless new findings.
- `check --report` shows the debt per rule: `debt: commands-have-tests 2 existing`.

### Ramps, `since` and `why`

A rule can arrive as a `hint`, become a `warn` once people have seen it, and an `error` once
the baseline is small, each step one line in `[rules]`. `since` and `why` go in the message, so
whoever meets the rule meets its reason.

### A branch is judged on what it touched

```
$ olifant check --changed-since main
```

keeps only the findings on lines changed since `main` (by `git diff`), plus every finding in
an untracked file. A finding about a whole file (a `missing` rule) counts when the file has
any change. Regions are not checked in this mode; run `olifant sync --check` alongside it.

A ref git doesn't know fails the check, and git's message is in the JSON's `errors`:
`"git diff --relative --no-color -U0 nosuchref --: fatal: bad revision 'nosuchref'"`.

## In CI and hooks

- **CI on a branch:** `olifant sync --check && olifant check --changed-since origin/main`.
  The pull request is judged on its own lines, and the module lists must be right.
- **CI on `main`, or a merge hook:** `olifant check`. This repository's
  `.githooks/pre-merge-commit` runs it (as `cabal run -v0 olifant -- check --human`, so with
  the code being merged) after the tests, before a merge into `main` completes:

  ```
  pre-merge-commit: checking conventions (olifant check)...
  conventions hold (0 findings, none blocking)
  ```

  A failure aborts the merge: "run `olifant sync` for regions, fix or allow (with a reason)
  any rule errors, commit, then `git merge --continue`".

## In the editor

Durandal ([chapter 14](14-editors-and-actions.md#conventions-on-save)) runs both layers on
save: it refreshes regions as buffer edits (`olifant sync --plan`), and shows the saved
buffer's findings as quiet diagnostics (`olifant check --file FILE --content -`), a
baselined one as a hint marked "(existing)". A finding's fix is first in `<leader>ma`. Nothing
happens while you type.

## Tutorial: a rule of your own

Every command should have a test. In a rust-htmx sandbox:

```sh
scripts/try-pack.sh rust-htmx /tmp/rules && cd /tmp/rules
```

### Step 1: the rule

Tests live in each service's `tests/`, one file per command. Write
`.olifant/rules.toml`:

```toml
[rules.commands-have-tests]
files   = "{service}/*-service/src/{command}.rs"
exclude = ["**/lib.rs", "**/service.rs", "**/models.rs", "**/ports.rs"]
missing = "{= dir =}/../tests/{= stem =}.rs"
message = "Every command has a test file"
since   = "2026-10"
why     = "commands are the behaviour; untested ones rot"
```

```
$ olifant check
! billing/billing-service/src/cancel_invoice.rs  commands-have-tests — Every command has a test file (since 2026-10: commands are the behaviour; untested ones rot)
! billing/billing-service/src/refund_invoice.rs  commands-have-tests — Every command has a test file (since 2026-10: commands are the behaviour; untested ones rot)
! billing/billing-service/src/void_invoice.rs  commands-have-tests — Every command has a test file (since 2026-10: commands are the behaviour; untested ones rot)
! notifications/notifications-service/src/send_email.rs  commands-have-tests — Every command has a test file (since 2026-10: commands are the behaviour; untested ones rot)
conventions hold (4 findings, none blocking)
```

No command has a test yet. A `missing` finding has no line, so it shows only the file.

### Step 2: make it count

Make it an error, in `.olifant/config.toml`:

```toml
[rules]
"commands-have-tests" = "error"
```

```
$ olifant check
✗ billing/billing-service/src/cancel_invoice.rs  commands-have-tests — Every command has a test file (…)
✗ billing/billing-service/src/refund_invoice.rs  commands-have-tests — Every command has a test file (…)
✗ billing/billing-service/src/void_invoice.rs  commands-have-tests — Every command has a test file (…)
✗ notifications/notifications-service/src/send_email.rs  commands-have-tests — Every command has a test file (…)
check failed
$ echo $?
1
```

(`(…)` is the `since` and `why`, shortened here.)

### Step 3: the exceptions, with reasons

`void_invoice` is on its way out. Put a file allow at the top of
`billing/billing-service/src/void_invoice.rs`:

```rust
// olifant:allow-file commands-have-tests — void is being replaced by refunds (#88)
```

`notifications` is tested another way. Add, to `.olifant/config.toml`:

```toml
[[rules.allow]]
rule   = "commands-have-tests"
paths  = ["notifications/**"]
reason = "notifications is tested end to end in the integration suite"
```

```
$ olifant check --report
✗ billing/billing-service/src/cancel_invoice.rs  commands-have-tests — Every command has a test file (…)
✗ billing/billing-service/src/refund_invoice.rs  commands-have-tests — Every command has a test file (…)
exceptions:
  commands-have-tests at billing/billing-service/src/void_invoice.rs:1: void is being replaced by refunds (#88)
  commands-have-tests at paths notifications/**: notifications is tested end to end in the integration suite
check failed
```

### Step 4: baseline the rest

The other two are real debt, to be paid as people work on them, not today:

```
$ olifant check --baseline
baselined 2 findings in .olifant/baseline.toml
$ olifant check
· billing/billing-service/src/cancel_invoice.rs  commands-have-tests — Every command has a test file (…)  (existing)
· billing/billing-service/src/refund_invoice.rs  commands-have-tests — Every command has a test file (…)  (existing)
conventions hold (2 findings, 2 existing)
$ git add -A && git commit -m "commands-have-tests, baselined"
```

### Step 5: new code meets the rule

On a branch, add a command:

```
$ git checkout -b pay-invoice
$ olifant gen rust-htmx/service-command --set service=billing --set command_name=pay_invoice
rust-htmx/service-command (6 edits, 0 conflicts)
  + billing/billing-service/src/pay_invoice.rs (36 lines)  [rust-htmx/service-command]
  …
$ olifant check
· billing/billing-service/src/cancel_invoice.rs  commands-have-tests — Every command has a test file (…)  (existing)
✗ billing/billing-service/src/pay_invoice.rs  commands-have-tests — Every command has a test file (…)
· billing/billing-service/src/refund_invoice.rs  commands-have-tests — Every command has a test file (…)  (existing)
check failed
$ olifant check --changed-since main
✗ billing/billing-service/src/pay_invoice.rs  commands-have-tests — Every command has a test file (…)
check failed
```

The old debt doesn't block the branch; the new command does. Write its test:

```
$ mkdir -p billing/billing-service/tests
$ printf '#[test]\nfn pays_an_invoice() {}\n' > billing/billing-service/tests/pay_invoice.rs
$ olifant check
· billing/billing-service/src/cancel_invoice.rs  commands-have-tests — Every command has a test file (…)  (existing)
· billing/billing-service/src/refund_invoice.rs  commands-have-tests — Every command has a test file (…)  (existing)
conventions hold (2 findings, 2 existing)
```

To offer a fix in the editor, give the rule a `fix`: a project generator that creates
`tests/{= command =}.rs`, taking `service` and `command` as inputs, the slots of `files`.

## Mistakes

A `rules.toml` that doesn't load makes `check` fail, naming it; one rule's mistake stops the
whole file.

| mistake | error |
|---|---|
| both `match` and `missing`, or neither | `` `rules.a`: a rule has exactly one of `match` (an ast-grep rule) or `missing` (a path that must exist) `` |
| `severity = "off"` in a rule | `` `rules.a`: a rule's severity is hint, warn, error (turn it off in the project's [rules]) `` |
| an unknown key | `` `rules.a`: unknown key(s) `mesage`: did you mean `message`?; expected one of `files`, `exclude`, `match`, `missing`, `message`, `severity`, `fix`, `since`, `why` `` |
| no `files` or `message` | `` `rules.x`: missing `message` `` |
| a `fix` naming no generator | `` rules.toml: rule `commands-have-tests`: fix names no generator `rust-htmx/command-test` `` |

(Each but the last is prefixed with the file's path.)

## Habits

- **Write the message as the reason.** "Commands talk to ports, not the database" teaches;
  "no diesel in commands" only forbids.
- **Start as a hint or a warning, with a baseline.** Raise the severity when the baseline is
  small.
- **Give a fix when there is one.** A finding with a fix is one keypress from done.
- **Exceptions are fine; silent ones are not.** Always give the reason, and read
  `check --report` now and then for stale ones.
- **Prefer a region when there is nothing to judge.** A rule that says "add `pub mod x;`"
  should be a region that adds it.
