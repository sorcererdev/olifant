# python-strangler

Strangle a legacy Python codebase into the python-htmx architecture, a little
in every PR, without dedicated migration time.

- `olifant init --migrate legacy/` in an existing repo: this pack and python-htmx,
  `[migrate]` in `.olifant/config.toml`, and today's findings baselined.
- **Rules:** `legacy-bug-fix-only` (no new definitions in legacy code; edits are
  fine), `new-code-not-legacy` (src/ never imports legacy).
- **Generators:** `extract-command` and `extract-query` (a legacy function becomes
  a command or query, with a parity test and a switch), `switch`, `delegate` (new
  work: the command, and a thin legacy function that calls it), `retire` (a switch
  that is on: the fallback goes).
- **The switches registry** (`[migrate] switches`, e.g. `src/bistro/switches.py`):
  `SWITCHES = {"close-table": False, …}`. Turning one on is a one-line PR.
- **`NotYet`** (python-htmx's `shared/result.py`): the new code declines a case it
  does not cover yet, and the legacy path runs it.

`fixtures/app` is the demo: a Flask big ball of mud (`legacy/`) beside a
python-htmx app (`src/bistro/`). Book: chapter 17, "Strangling a legacy codebase".
