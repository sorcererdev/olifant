# 4. Branches the compiler finds, and todos

Add a variant to a type and every `case` over that type is now incomplete. Elm knows
exactly which ones. Olifant uses that: a `complete` edit adds a branch to every `case` the
compiler says is missing the new constructor.

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

In the JSON plan these edits say so: `"found_by": "compiler", "site": "changeRouteTo"`.
(The human output does not mark them yet.)

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
  ~ src/Route.elm (+2 lines at line 36)
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

## Known rough edges

- Line numbers reported for compiler-found edits are those at the moment of insertion,
  not the final file's.
- The human output does not mark which edits the compiler found.
