# Olifant conventions

Olifant is a Haskell engine (`src/Olifant/`, `app/`) with a language server (`olifant lsp`), the
conventions packs it ships (`packs/`), and two books (`book/user/`, `book/internals/`) kept in step
with the code. `book/internals/01-tour.md` is the map: its layering diagram and its ports section
are the rules below at length. A **must** rule is one every change is held to; a **taste** rule
is how we like it.

## Rules

### Imports only go down the layers (must)

A module imports only from layers below it in the tour's layering diagram (`app/Main.hs` →
`Cli` → `Lsp` → `Server` → `App`/`Output` → `Catalog` → … → `Naming`/`Toml`/`Glob`/`Process`).
No import sideways within a layer and none upwards. `Server` has no LSP types; `Lsp` is the
only protocol adapter.

### The outside world goes through a port (must)

The filesystem, processes, git, ast-grep, compilers, test runners and models are reached
through a port (a record of functions, such as `Workspace`, the `Syntax` and `Checker` ports,
`pTester`, the model backends), and only `app/Main.hs` and the server's setup choose real
adapters. A module split into a pure part and an IO part (`Server/Step.hs` and
`Server/ConjureStep.hs`) keeps IO out of the pure one.

### No language is named in the engine (must)

The engine learns a language from data: its grammar, the pack, the project's own code, and a
model for the gaps. Code under `src/` does not branch on a language's name, a file extension
or a test framework (`ConjureNeutralSpec` holds conjure to this). A language-specific fact
belongs in a pack or `.olifant/languages/`, not behind an `if` in Haskell.

### Config keys are checked when loaded (must)

An unknown key in `.olifant/config.toml`, a pack or a generator is refused, not ignored. A
retired key is refused with a sentence naming what replaced it (`[model] rounds`,
`[conjure] wire`). A key that names something (a backend, a writer) is checked against what
exists when the config loads, not when it is first used.

### What the person reads says what to do (must)

An error, refusal or notification the person sees is a plain sentence that names the thing
and the next action ("no function or class at line 3: put the cursor in one to conjure it").
It does not leak a Haskell exception, a stack trace or an internal type name.

### Model calls stay cheap and visible (must)

A model is reached through a configured backend; Claude through `claude -p` (no
`ANTHROPIC_API_KEY` is assumed). Every call's cost and seconds are recorded where the person
can see them (the step log, `errands.jsonl`, `conjure-steps.jsonl`). A call does not carry
CLAUDE.md, hooks, plugins or tools it does not need. A loop does not call a model again
without a cap.

### Specs do not reach the network or a real model (must)

Specs use the ports' fakes (`ssTestWriter`, `step_replies`, a fake runner) and temporary
directories. A spec that touches git or the repository runs only in the main checkout.

### The books change with the code (must)

A change to behaviour, a command, a config key or a pack changes the user chapter that covers
it; a new or reshaped module, data flow or seam changes the internals book, naming the
functions and files a reader would open. A chapter that disagrees with the code is a bug.

### Modules are listed by `olifant sync` (must)

The module lists in `olifant.cabal` are a generated region. A module added or removed by hand
there is wrong; `olifant sync` places it.

### Established words first (taste)

Use the field's existing term where one exists (mutation testing's *mutant*, *killed*,
*survived*; LSP's own names). A coined name only where no term exists and nothing conflicts.

### Comments say why (taste)

An exported function has a Haddock line saying what it answers, in the codebase's voice
("The writer for a step (the harder one when HARDER)…"). A comment explains a choice or a
trap, not what the next line does.

## A nitpick is

formatting, import order, naming taste, a missing comment, and wording that does not change
what the person reads

## Worth saying is

a wrong answer or a crash on a real input, a broken rule above, a model call without a cap or
a record, a port bypassed, a language named in the engine, a book chapter the change made
untrue
