# The internals book

How Olifant works, so you can change it. Chapters 1 and 2 are written; the rest are planned.

1. [A tour of the codebase](01-tour.md): the units, ports and adapters, one `gen` call end to end (with region families and `--overlay`), the check loop's compiler supports (pyright's included: `supportSpans`, `indentBlockEnd`, `checkProgram`), `olifant actions` and suggestions, snippets (`gen --at`, `olifant complete`, `uncovered` queries, `after`), `olifant doctor`, and the conventions: the `Rules` unit, `olifant check` (baseline, `--changed-since` and the git port) and `sync --plan`
2. [The Haskell you'll meet here, for a Rust programmer](02-haskell-for-this-codebase.md)
3. The template engine: `{= … =}`, filters, exemplars (planned)
4. From generator to plan: `Compose` and `Planner` (planned)
5. Anchors: phrases, resolution, and the ast-grep adapter (planned)
6. The check loop behind `complete` edits (planned)
7. The CLI: options, output, exit codes (planned)
8. How we work: TDD, colocated tests, blessing snapshots, git hooks, token experiments (planned)
