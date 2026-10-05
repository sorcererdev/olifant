# Olifant

Olifant is the generator engine of Durandal: conventions written as code, applied from your
editor or the command line, with AI only in small, typed, checked gaps. The human drives.

- **Generators** turn typed inputs into edits across files: create, insert at an anchor
  (`end of struct BillingService`), complete every `match`/`case` the compiler reports, and
  compose with other generators. Re-running one is a no-op.
- **Choices come from your code:** `one_of` queries list the services, commands and ports that
  exist, narrowed by what you picked before.
- **Holes** (`{= ? name: kind | instruction =}`) are the only place a model writes code. A
  configured model (local via Ollama, or Anthropic) fills them; each fill is parsed and
  compile-checked, retried with the compiler's message, and otherwise left as a compiling
  placeholder. Without a model, everything still works.
- **Packs** bundle generators and language adapters for a stack (Elm, Rust/axum/htmx,
  Python/FastAPI/htmx, Haskell, …), each tested against a fixture app.
- **From nothing:** `olifant init --pack python-htmx bistro` makes a running app, tests
  green; each generator then adds a piece of a ports-and-adapters architecture, with
  passing tests for boilerplate and a red stub for behaviour you write.

**Status:** an early, private prototype, changing fast.

## Try it

```sh
brew install ghc cabal-install ast-grep elm        # plus rustup for the Rust pack
cabal build && scripts/install-olifant.sh          # olifant on your PATH
scripts/try-pack.sh rust-htmx /tmp/try-rust        # a sandbox of the Rust pack's fixture app
cd /tmp/try-rust && olifant list && olifant show rust-htmx/command-dependency

brew install uv                                    # for the Python pack
olifant init --pack packs/python-htmx bistro --dir /tmp/bistro   # a new app, tests green
OLIFANT=$(cabal list-bin olifant) scripts/slice-python.sh /tmp/slice gens   # a whole slice, step by step
```

The **[books](book/README.md)** are the documentation: the user book (from running a
generator to writing your own, including holes and a local-model tutorial) and the internals
book (how the engine works).

## Develop

```sh
OLIFANT_REQUIRE_TOOLS=1 cabal test      # the full suite, with every external tool required
cabal run olifant -- sync --check       # owned regions (e.g. module lists) are up to date
```

## License

Apache-2.0 (see [LICENSE](LICENSE)).
