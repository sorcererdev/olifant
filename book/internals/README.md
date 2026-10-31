# The internals book

How Olifant works, so you can change it. Chapters 1 to 5 are written; the rest are planned.

1. [A tour of the codebase](01-tour.md): the units, ports and adapters, one `gen` call end to end (with region families and `--overlay`), the check loop's compiler supports (pyright's included: `supportSpans`, `indentBlockEnd`, `checkProgram`), `olifant actions` and suggestions, snippets (`gen --at`, `olifant complete`, `uncovered` queries, `after`), `olifant doctor`, the conventions (the `Rules` unit, `olifant check`, `sync --plan`), the language server and the editor's TDD commands
2. [The Haskell you'll meet here, for a Rust programmer](02-haskell-for-this-codebase.md)
3. [Plans: the loop between a markdown file and the tree](03-plans.md): the parser, anchors and fingerprints, errands (`ask`, `validate`, `do`), the reconcile (facts and readings, Jev and the ladder), the `do` review, the journal, telemetry
4. [Steered conjure](04-conjure.md): one conjure end to end, rounds, Jev's question and the policy before it, the model ladder, pause and resume, the wire and the port method, the `[conjure]` shapes and the floor on Haskell and Rust, `ConjureNeutralSpec`
5. [How a language is described](05-languages.md): language, toolchain and pack files layered shipped, pack, project; which engine code reads each key; doctor's capability table; the no-language-branches rule and `NeutralSpec`
6. The template engine: `{= … =}`, filters, exemplars (planned)
7. From generator to plan: `Compose` and `Planner` (planned)
8. Anchors: phrases, resolution, and the ast-grep adapter (planned)
9. The check loop behind `complete` edits (planned)
10. The CLI: options, output, exit codes (planned)
11. How we work: TDD, colocated tests, blessing snapshots, git hooks, token experiments (planned)
