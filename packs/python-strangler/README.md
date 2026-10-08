# python-strangler

Strangle a legacy Python codebase into the python-htmx architecture, a little
in every PR, without dedicated migration time.

- `olifant init --migrate legacy/` in an existing repo: this pack and python-htmx,
  `[migrate]` in `.olifant/config.toml`, and today's findings baselined.
- **Rules:** `legacy-bug-fix-only` (no new definitions in legacy code; edits are
  fine), `new-code-not-legacy` (src/ never imports legacy).
- **Generators:** `extract-command` and `extract-query` (a legacy function becomes
  a command or query, with a parity test and a switch; offered at the function,
  the likelier first), `switch`, `delegate-command` and `delegate-query` (new
  work: the command or query, and the legacy function becomes a door to it —
  the bug-fix-only rule's fixes), `retire` (the `switch-retire` rule's fix: a
  switched function keeps only its new path; the function stays for its callers).
- **The switches registry** (`src/<app>/switches.py`): one line per switch,
  `SWITCHES["close-table"] = False`. Turning one on is a one-line PR.
- **`NotYet`** (python-htmx's `shared/result.py`): the new code declines a case it
  does not cover yet, and the legacy path runs it.

`fixtures/app` is the demo: a Flask big ball of mud (`legacy/`) beside a
python-htmx app (`src/bistro/`). Book: chapter 17, "Strangling a legacy codebase".
