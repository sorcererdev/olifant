# 4. Branches the compiler finds, and todos

Add a variant to a type and every `case` over that type is now incomplete. The compiler
knows exactly which ones — Elm's or GHC's. Olifant uses that: a `complete` edit adds a
branch to every `case` the compiler says is missing the new constructor. This chapter shows
Elm first, then Haskell, which works the same way with two differences: how a constructor
is named, and what happens with no `text`/`template` at all.

```toml
[[edits]]
complete = "Route.{= P =}"            # the constructor this generator introduced
where    = "case in changeRouteTo"    # this body is for that case
text     = """
{= pattern =} ->
    {= P =}.init |> updateWith {= P =} Got{= P =}Msg model"""
```

After the other edits are planned, Olifant copies the project to a scratch directory,
runs `elm make --report=json`, and reads the "missing patterns" it reports. For each one
naming `Route.Settings` it adds a branch, then checks again (Elm reports one module at a
time, so it can take a few rounds). `{= pattern =}` is the missing pattern, written the way
that file refers to it: `Just Route.Settings` in `Main.elm`, just `Settings` in
`Route.elm`.

The plan marks these edits: `(+2 lines at line 59, compiler: changeRouteTo)` in the
human output, `"found_by": "compiler", "site": "changeRouteTo"` in JSON.

## Places nobody told the generator about

Suppose your app has a hand-written function:

```elm
toPath : Route -> String
toPath route =
    case route of
        Home ->
            "/"

        SamplePage ->
            "/sample-page"
```

`elm/page` knows nothing about `toPath`. Run it anyway:

```
$ olifant gen elm/page --set page=settings
elm/page (10 edits, 0 conflicts)
  + src/Page/Settings.elm (42 lines)
  …
  ~ src/Route.elm (+2 lines at line 36, compiler: toPath, todo)
  …
  ? src/Route.elm: toPath needs a branch for Settings (left as a todo)
```

The compiler found `toPath`, and since no `complete` edit's `where` matches it, Olifant
added a placeholder:

```elm
        Settings ->
            Debug.todo "olifant: toPath Settings"
```

The app compiles, and the plan lists the gap (JSON: `"todos": [{"file":
"src/Route.elm", "function": "toPath", "pattern": "Settings", …}]`). Replace the
`Debug.todo` with the real body (`"/settings"`). In measurements, agents given only the
todo list fixed these correctly every time, reading only the file it named.

Treat todos as must-fix: **Elm 0.19.2 compiles `Debug.todo` even with `--optimize`**, so
nothing else stops one reaching production, where it crashes if that branch runs.

## Haskell (GHC)

GHC's `-Wincomplete-patterns` reports the same shape of problem — a `case` missing
constructors — through `-fdiagnostics-as-json`. Olifant's check command for a cabal project
is `cabal repl -v0 lib:NAME --repl-options=-fno-code --repl-options=-fdiagnostics-as-json
--repl-options=-Wincomplete-patterns`, not `cabal build`: with `-fno-code`, `build` always
exits 1 at its link step, while `repl` exits 0 once type-checking succeeds. About 3 seconds
a round on Olifant itself.

GHC names the scrutinee's type, so a `complete` edit names its constructor `Type.Ctor`
instead of Elm's `Module.Ctor`:

```toml
[[edits]]
complete = "{= T =}.{= C =}"   # e.g. CatalogError.PackLocked
```

and it is visible only where GHC reports that type — `CatalogError.PackLocked` can extend a
`case` over `CatalogError`, never one over some other type that happens to have a
constructor of the same bare name.

A `complete` edit with no `text` and no `template` at all leaves a todo branch at *every*
site the compiler finds: there is no body to try, so nothing is ever chosen. That is what
`olifant-dev/error-case` does: it adds the constructor and leaves every `case` for a person
(or a model) to fill in. A real run, adding `PackLocked Text` to `CatalogError`:

```
$ olifant gen olifant-dev/error-case --set path=src/Olifant/Catalog.hs --set type=catalog_error --set name=pack_locked --set fields=Text --human
olifant-dev/error-case (3 edits, 0 conflicts)
  ~ src/Olifant/Catalog.hs (+1 line at line 87)
  ~ src/Olifant/Catalog.hs (+1 line at line 103, compiler: describeCatalogError, todo)
  ~ src/Olifant/Cli.hs (+1 line at line 265, compiler: catalogFailureAt, todo)
  ? src/Olifant/Catalog.hs: describeCatalogError needs a branch for PackLocked _ (left as a todo)
  ? src/Olifant/Cli.hs: catalogFailureAt needs a branch for PackLocked _ (left as a todo)
```

Two GHC rounds, about 7 seconds. The inserted line in `Cli.hs`:

```haskell
  PackLocked _ -> error "olifant: catalogFailureAt PackLocked _"
```

and the result type-checks clean.

Function equations ("In an equation for `f`") are a kind of site the loop cannot place a
branch at — Olifant only knows how to add a `case` alternative, not a new equation. Those
sites are reported as plan warnings instead, naming the file, the function and the pattern,
never guessed at and never silently dropped.

A branch is spaced like the `case`'s own alternatives, Haskell or Elm: a blank line before
the new branch only when one already separates the last two alternatives, none when they
sit flush against each other (ordinary Haskell style), and a blank line when the `case` has
only one alternative to copy the spacing from.

## When Olifant refuses to guess

- **Two candidates.** Elm prints constructors unqualified. If a site could belong to two
  of the generator's constructors (say `Route.Settings` and a `Page` constructor
  `Settings`) and no `where` decides, the run is a conflict naming both. It first rules
  out constructors a file cannot see: `Route.elm` cannot import `Main`, so `Main.Settings`
  is never a candidate there. (That's why `elm/page` writes `complete = "Main.{= P =}"`.)
- **The project does not compile.** A broken module hides the missing cases of every
  module that imports it. If a file Olifant changed is broken, that is a conflict ("the
  generated code does not compile"); if some other file is broken, the plan applies but
  carries a warning that cases could not be checked. Fix the error and run again.
- **Catch-alls hide sites.** A `_ ->` branch means the compiler never complains, so no
  branch is added there. That is why `elm/page` places its `update` branch with an `at`
  phrase: `Main.update` ends in `( _, _ ) ->`. A pack can adopt "no catch-alls" as a
  convention precisely so the compiler can guide its generators.

## What it needs

`elm` on your PATH, and a language adapter with a `[check]` command (the Elm pack has
one). Without `elm` the `complete` edits are a conflict that says so. Checking costs one
`elm make` of the project per round, a second or two for a small app.

## Line numbers

Every edit's line is where its text starts when the plan's edits are applied in order
(`e1`, `e2`, …). Compiler-found branches are applied bottom-up within a file, and numbered
that way, so an earlier line is never shifted by a later edit in the same file.
