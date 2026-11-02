# 17. Strangling a legacy codebase

A big ball of mud moves to a new architecture a little at a time, as people work on it, with
no migration project. Two habits do it:

- **Legacy code gets bug fixes only.** New work goes in the new architecture, even when it is
  asked for in a legacy file.
- **When you touch a legacy function, you may move it.** It becomes a command or a query of the
  new architecture, behind a **switch**: a flag, and the new code may decline (`NotYet`), so the
  old path still answers until the new one is trusted.

Olifant makes each of these the easy thing to do: a warning where new work starts in legacy
code, with a fix that puts it in the new architecture; an extract action on every legacy
function; and a report, per branch, of how the migration went. This chapter uses the
`python-strangler` pack on its demo, a Flask monolith (`legacy/`) beside a python-htmx app
(`src/`).

## Try it

```
$ scripts/try-pack.sh python-strangler /tmp/bistro-legacy
$ cd /tmp/bistro-legacy
$ git checkout -b feature
```

The sandbox is a git repo on `main`, configured with both packs and `[migrate]`. Work on a
branch: "new" means new since `main`.

## Adopting it: `olifant init --migrate`

In your own repo:

```
$ olifant init --migrate legacy --pack ~/projects/olifant/packs/python-htmx --human
baselined 0 findings in .olifant/baseline.toml
migrating legacy: legacy is bug-fix-only; new work goes to the new architecture
next: olifant check, and open the project in your editor
```

It adds the target pack, the strangler pack (`python-strangler`), and

```toml
[migrate]
legacy = ["legacy/**"]   # the legacy folders, as globs
# base = "main"          # what "new" is measured from: the merge-base of HEAD and this
# switches = "src/bistro/switches.py"   # the registry; when not set, the strangler pack's [switches] registry glob finds it
# count = ["function_definition"]       # what a legacy function is, as node kinds; when not set, the language's top-level functions (not one nested in another: a Haskell `where`)
```

and records a baseline, so today's code passes ([chapter 2](02-running-generators.md#an-existing-repo-olifant-init---migrate)).

Which packs: `--pack` is the target architecture; without it, the project's one configured pack
that is not a strangler. A **strangler pack** is a pack with a `migrate.toml` (below):
`--strangler PACK` names it; without it, the one the project configures, else the one under
your `[packs]`, else the one in the folder beside the target pack. None, or two, is a usage
error that names them and the flag to give.

## Day to day

### A bug fix stays quiet

Change a line inside `close_table` in `legacy/views.py`: nothing. A function with any line
from `main` is not new, so the bug-fix-only rule doesn't see it. Neither does moving a function,
or renaming its parameters.

### New work is redirected

Add a function to `legacy/views.py`:

```python
def add_to_waitlist(guest, party):
    return {"guest": guest, "party": party, "waiting": True}
```

As you type it, the editor warns:

```
legacy-bug-fix-only — New work goes in the new architecture (legacy is bug-fix-only)
```

Its fixes, in the ⚡ menu: **Create it as a command** and **Create it as a query**. Take one: the
generate picker opens on `python-strangler/delegate-command`, its name set
(`add_to_waitlist`), its app and service chosen where there is only one. Enter, and

- the command is created in the new architecture (`src/bistro/booking/commands/add_to_waitlist.py`,
  and its test, red until you write its first case);
- the legacy function becomes a door to it, with the allowance that says why:

```python
# olifant:allow legacy-bug-fix-only — delegates to booking/commands/add_to_waitlist
def add_to_waitlist(guest, party):
    ...  # the call to the command
```

Callers in the monolith keep calling `add_to_waitlist`; its behaviour lives in the new code.

### Extracting a legacy function

On any legacy function, the ⚡ menu offers **Extract … to a command** and **… to a query**, the
likelier first: a function whose SQL writes (`INSERT`, `UPDATE`, `DELETE`), or that commits,
saves or assigns a global, is a command; one that only selects is a query (the `signs` of
python-strangler's `[[extract.into]]`, below). The picker opens on
it, the service suggested from the tables its SQL names (`UPDATE tables` → the service with a
`table` model; said `suggested`, yours to change; python-strangler's `[extract.owner]`). Enter writes:

- the command or query (python-htmx's, its body yours to port, or a model's);
- a **parity test** beside it, `close_table_parity__test.py`, calling the legacy function and the
  new one with the same inputs;
- the **switch**, at the top of the legacy function, the old body below it unchanged:

```python
def close_table(table_id):
    # olifant:switch close-table → booking/commands/close_table
    if switches.on("close-table"):
        result = CloseTable().handle(CloseTableInput(id=table_id))
        if not isinstance(result, NotYet):
            return {"id": table_id, "open": False}
    """Close a table for the evening: no new bookings on it."""
    ...  # the old body, as it was
```

- a line in the **registry**, `src/bistro/switches.py`, off:

```python
SWITCHES["close-table"] = False
```

A generator function (one that yields) gets `yield from` the new path's items, then `return`.

The new code may decline a case it does not cover yet, by returning `NotYet(reason)`
(`bistro.shared.result`): the old body runs it. So a command can go live for the cases it
handles, and grow.

### Turning a switch on

Make the parity test green, then one line in a PR: `SWITCHES["close-table"] = True`. The legacy
tests still pass: where the new path declines, the old one answers.

### Retiring the old path

Once a switch's flag is on in the registry, a hint on its function: **Switched to the new
path: when its flag is on and trusted, retire the legacy body** (never while it is off: retiring
then would send every caller down the untrusted path). Its fix, **Retire the legacy path**, keeps the function (its callers
keep calling it) and only its new path:

```python
def close_table(table_id):
    result = CloseTable().handle(CloseTableInput(id=table_id))
    if not isinstance(result, NotYet):
        return {"id": table_id, "open": False}
    raise RuntimeError("close_table: the new path declined (NotYet) and the legacy path is retired")
```

The review names the function's callers, which keep working (`close_table stays, so its 4
caller(s) keep working: legacy/app.py:56, legacy/tests/test_views.py:80, …`). Retire when the
new path declines nothing they need. The registry line stays: delete it in the same PR.

## Each PR: the report

`olifant check` ends with the branch's migration:

```
migration: +1 extracted (close-table), 0 legacy functions added, legacy 121 → 122 functions; switches: 1 on, 0 off
```

- **extracted**: switches the base did not have;
- **legacy functions added**: new legacy functions *without* an allowance (a delegate doesn't
  count; it carries `olifant:allow`);
- **legacy N → M functions**: function definitions in the legacy globs (`[migrate] count`'s node
  kinds; without it, the language's top-level functions and methods, as its queries capture them: a method in a class counts, a function nested in another function or method (a Python inner `def`, a Haskell `let` or `where` binding) does not), at the base (over the base's own
  files, so a file deleted since counts there) and now; a file renamed with `git mv` is not new;
- **switches**: the registry's, on and off (a trailing `# comment` on a line is fine).

`--report` lists each switch; `--json` has it all under `migration`. In Neovim,
`:DurandalMigration` shows the line and the switches in a float.

## The rules and generators

| | what |
|---|---|
| `legacy-bug-fix-only` | `{legacy}`, `only = "new"`: a wholly new function or class in legacy code. Fixes: create it as a command or a query, the legacy function delegating |
| `new-code-not-legacy` | the new architecture importing legacy code |
| `switch-retire` | a hint on a switched legacy function, with the retire fix |
| `migrate-new-work` | a hint on a big change in a legacy function (15+ new lines, `[migrate] new_work_lines`): is this new work? With a model, the answer sharpens it (below) |
| `extract-command`, `extract-query` | the command or query, the parity test, the switch |
| `switch` | a switch over a function, and its registry line; refused (with a hint to extract) when its command or query does not exist yet, since its import would break the legacy module |
| `delegate-command`, `delegate-query` | new work's command or query, used by the bug-fix-only fixes |

`only = "new"` and `{legacy}` are general: any rule can use them ([chapter 15](15-conventions-that-hold.md#rules-on-new-code-only--new)).

## The strangler pack's `migrate.toml`

Olifant's strangling knows no pack: the names, files and conventions it needs come from the
strangler pack's `migrate.toml` (the first configured pack that has one). A pack for another
language or architecture writes its own. python-strangler's, in short:

```toml
bug_fix_only = "legacy-bug-fix-only"   # the rule; its allow comment marks a delegate

[switches]
registry = "**/switches.py"            # the registry when [migrate] switches names none
on = "True"                            # the value that means on
line = ['^SWITCHES\["([^"]*)"\][[:space:]]*=[[:space:]]*(.*)$']   # group 1 the name, group 2 the value
block = { open = '^SWITCHES.*\{$', close = '^\}$', line = ['^"([^":]*)"[[:space:]]*:[[:space:]]*([^:,]*),?[[:space:]]*$'] }
guard = '^if switches\.on\('           # the line that opens a switched function's new path

[extract]
function = "function"                  # the input every extract generator takes for the legacy function
file = "file"                          # … and for its file
owner = { input = "service", models = "src/{= app =}/{= service =}/models/*.py", tables = ["…"] }

[[extract.into]]
generator = "extract-command"          # a generator of this pack
name = "command"
means = "changes state"
signs = ['(?i)INSERT ', '(?i)UPDATE ', '(?i)DELETE ', '\.commit\(', '\.save\(', 'global ']

[[extract.into]]
generator = "extract-query"
name = "query"
means = "reads state"
signs = ['(?i)SELECT ', '\.fetch', 'db\.rows\(', 'db\.row\(']
```

| key | what reads it |
|---|---|
| `bug_fix_only` | the report (a function whose line above has `olifant:allow RULE` is a delegate, not legacy added) and the "bug or new?" decision (its answer "new behaviour" turns the hint into this rule's finding, its message plus ": this change adds behaviour"). Absent: the hint stays a hint |
| `[switches] registry`, `on`, `line`, `block` | the report's switches. Each registry line is stripped and its comment cut (the registry's language's `comment`); `line` expressions match anywhere, `block`'s `line` only between a line `open` matches and the next `close` matches. Absent: no registry unless `[migrate] switches` names one, and no switches read |
| `[switches] guard` | a rule fix's `{= finding_switch_path =}`: the block this line opens, a level less indented (python-strangler's retire) |
| `[extract] function`, `file` | the inputs the hint's fixes set and the ⚡ order and decision 3 read |
| `[[extract.into]]` | the hint's fixes (**Extract this as a NAME**, in order) and the ⚡ order: the first whose `signs` the function's code matches (comments cut; a leading `(?i)` matches any case) leads; none, the decider is asked; else the first |
| `[extract.owner]` | decision 3: `input` is the generator's input whose choices are the owners; `models` a glob template over its inputs (the owner among them) whose files' stems are an owner's models; `tables` expressions over the code whose last group is a table. The owner one of whose models a table names (singular or plural) is suggested |

A function's text is found by the language's `[unit] name` ([chapter 9](09-packs-and-language-adapters.md)), its block by indentation.

## With a model: decisions, progressively

Everything above works with no model. With one (`[decide] model`; `[recognize] model` is its
alias, so one model answers every question), four questions get a second opinion, asked in the
background while the deterministic answer stands:

- **bug or new?** A big change in a legacy function: answered "new behaviour", its hint becomes
  the bug-fix-only warning; "bug fix", the hint goes.
- **command or query?** When the code doesn't say (no SQL, no commit), the answer orders the
  extract actions. The question (`command-or-query`) is built from `[[extract.into]]`: the names
  joined by `-or-`, each with what it `means`.
- **which service?** When the tables name no single service's model, the answer is the
  suggestion. The question's id is `[extract.owner] input`.
- **which convention?** When recognition is unsure (a hint, a choice, a rival shape), the
  `recognize` choice asks which of the candidate generators the code is, with the inputs it
  could not read; the convention chosen upgrades the hint to an offer.

An answer outside the options, or none, changes nothing. A `jev` backend ([chapter 12](12-holes-and-models.md)) answers with a confidence, and asks each batch of questions in one call.

## Mistakes

- **No warnings at all in legacy code.** No base: the rule is inactive, and says so once,
  as a hint (`… is inactive: no ref \`main\``). Commit, and set `[migrate] base` if your main
  branch has another name.
- **`rule-failed: … names {legacy}, but the project has no [migrate] legacy`.** A pack's rule
  uses `{legacy}` in a project that isn't migrating: `olifant init --migrate PATH`.
- **The report counts 0 legacy functions, with `(counted 0: …'s grammar is not loaded (see \`olifant doctor\`'s grammar row), so set \`count\`…)`.**
  A language whose grammar does not load (a bare `grammar` name, or a build or fetch that
  failed: doctor's `grammar` row says which) has no queries to count by, so without
  `[migrate] count` it counts nothing: fix the grammar, or list the node kinds of its functions in `count`.
- **The report counts 0 switches.** The registry is not where it is looked for: set
  `[migrate] switches`. Or no configured pack has a `migrate.toml` with `[switches]`, so no
  registry line reads as a switch.
- **`no strangler pack: give --strangler`**, or **`several strangler packs (…)`.** `init --migrate`
  found no pack with a `migrate.toml`, or more than one: name it with `--strangler`.
