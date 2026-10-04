# python-htmx

Python web apps in a ports-and-adapters shape: FastAPI + htmx + Jinja, uv,
pytest, pyright (strict), SQLite through stdlib `sqlite3`.

The fixture (`fixtures/app/`, a restaurant's table bookings) is the
architecture the generators reproduce:

- `src/bistro/<service>/`: `models.py` (frozen dataclasses), `ports.py`
  (Protocols), one module per command (input, failures as frozen dataclasses
  in a one-per-line union, `handle()` returning `Ok[T] | failures`), and
  `service.py` (commands wired to ports).
- `src/bistro/adapters/`: in-memory and SQLite repositories behind the same
  port, sharing one contract test.
- `src/bistro/web/`: routes and htmx fragments; `failures.py` maps each
  failure to a status with an exhaustive `match`.
- `src/bistro/main.py`: the composition root.

Needs `uv` (`brew install uv`); pyright comes with `uv sync`. Olifant checks
with `uv run pyright --outputjson`, its environment in Olifant's cache.

Run the fixture: `uv run uvicorn bistro.main:app --reload`, then open
http://localhost:8000/.
