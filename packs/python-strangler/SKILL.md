# python-strangler (for agents)

The project is migrating ([migrate] in .olifant/config.toml). Rules:

- Legacy paths ([migrate] legacy) are bug-fix-only: change existing code there,
  but do not add functions or classes. New work goes in src/ as a python-htmx
  command or query (`olifant gen python-strangler/delegate …` makes it, and the
  thin legacy function that calls it).
- Never import legacy from src/.
- To move a legacy function: `olifant gen python-strangler/extract-command`
  (or `extract-query`) with the function and its file; then turn its switch on
  in the registry when its parity test passes.
- `olifant check` ends with the migration line: say what your change extracted.
