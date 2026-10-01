# 8. Composing generators

A real task is rarely one generator. A page comes with messages; a model comes with a
controller. Olifant composes in two ways: **on the command line** (`+` and `--steps`), for
one task, and **inside a generator** (`[[uses]]`), for a convention that is always made of
smaller ones. Either way the result is one plan, written all at once or not at all.

```sh
scripts/try-pack.sh elm /tmp/olifant-compose && cd /tmp/olifant-compose
```

## `+`: several steps, one plan

```
$ olifant gen elm/page --set page=settings --set msgs="toggle_dark_mode, set_display_name:String" \
            + elm/page --set page=profile --set msgs="follow"
elm/page + elm/page (24 edits, 0 conflicts)
  + src/Page/Settings.elm (42 lines)  [elm/page (step 1)]
  ~ src/Main.elm (+1 line at line 8)  [elm/page (step 1)]
  …
  ~ src/Page/Settings.elm (+1 line at line 25)  [elm/page (step 1) › elm/msg[1]]
  ~ src/Page/Settings.elm (+1 line at line 26)  [elm/page (step 1) › elm/msg[2]]
  + src/Page/Profile.elm (42 lines)  [elm/page (step 2)]
  …
  ~ src/Page/Profile.elm (+1 line at line 25)  [elm/page (step 2) › elm/msg[1]]
  ~ src/Page/Profile.elm (+2 lines at line 34, compiler: update)  [elm/page (step 2) › elm/msg[1]]
  ~ src/Page/Settings.elm (+2 lines at line 35, compiler: update)  [elm/page (step 1) › elm/msg[2]]
  ~ src/Page/Settings.elm (+2 lines at line 35, compiler: update)  [elm/page (step 1) › elm/msg[1]]
  ~ src/Main.elm (+2 lines at line 114, compiler: view)  [elm/page (step 2)]
  ~ src/Main.elm (+2 lines at line 114, compiler: view)  [elm/page (step 1)]
  ~ src/Main.elm (+2 lines at line 62, compiler: changeRouteTo)  [elm/page (step 2)]
  ~ src/Main.elm (+2 lines at line 62, compiler: changeRouteTo)  [elm/page (step 1)]
```

A standalone `+` starts a new step. Each step gets exactly the `--set` values after it and
before the next `+`; nothing leaks from one step to the next. Global flags (`--plan`,
`--json`, `--human`, `--now`) can go anywhere.

Reading the tags: `elm/page (step 2)` is the second top-level step; `› elm/msg[1]` is the
first item of a `for_each` inside it. The compiler-found branches (chapter 4) come after all
positional edits, because they are found by compiling the result of those edits.

## Order matters, because `requires` sees the plan so far

`elm/msg` requires its page's file. The page does not exist yet, so alone it is refused:

```
$ olifant gen elm/msg --set page=about --set msg=reset
Plan: elm/msg (1 edit, 2 conflicts): nothing written
  ! src/Page/About.elm (conflict, see below)
  ! elm/msg: required files missing: src/Page/About.elm. Create the page first: olifant gen elm/page --set page=about
  ! edit 1: cannot insert into src/Page/About.elm: it does not exist
```

After a step that creates the page, it is fine:

```
$ olifant gen elm/page --set page=about + elm/msg --set page=about --set msg=reset
elm/page + elm/msg (11 edits, 0 conflicts)
  + src/Page/About.elm (42 lines)  [elm/page (step 1)]
  …
  ~ src/Page/About.elm (+1 line at line 25)  [elm/msg (step 2)]
  ~ src/Page/About.elm (+2 lines at line 34, compiler: update)  [elm/msg (step 2)]
  …
```

Steps are planned in order, each seeing the files as the earlier steps left them. Put the
same two steps the other way round (`elm/msg … + elm/page …`) and the `elm/msg` step is a
conflict, so the whole chain writes nothing.

## `--steps`: the same thing as JSON

For many steps, or steps built by a program (an agent, a script), write them as JSON:

```json
[
  {"gen": "elm/page", "set": {"page": "settings", "msgs": ["toggle_dark_mode", "set_display_name:String"]}},
  {"gen": "elm/msg",  "set": {"page": "home", "msg": "refresh"}}
]
```

```
$ olifant gen --steps steps.json --plan
Plan: elm/page + elm/msg (15 edits, 0 conflicts)
  + src/Page/Settings.elm (42 lines)  [elm/page (step 1)]
  …
  ~ src/Page/Home.elm (+1 line at line 25)  [elm/msg (step 2)]
  ~ src/Page/Home.elm (+2 lines at line 34, compiler: update)  [elm/msg (step 2)]
  …
```

`--steps -` reads stdin. Each step is `{"gen": …, "set": {…}}` and nothing else. Values
become the text a `--set` would carry: strings as they are, numbers and booleans as written,
and arrays joined with commas (so `["a", "b"]` is the list `a,b`; an item that itself
contains a top-level comma would split). Mistakes get a usage error (exit 2):

```
$ echo '[{"generator":"elm/msg"}]' | olifant gen --steps -
--steps: Error in $[0]: unknown key(s) ["generator"]; each step is {"gen": GENERATOR, "set": {...}}: use "gen" and "set"

$ olifant gen elm/page --set page=x --steps steps.json
use either --steps or positional generator steps, not both
```

## `[[uses]]`: a generator made of generators

`elm/page` ends like this:

```toml
[inputs.msgs]
type = "list"
optional = true
of = [
  { name = "name", type = "ident" },
  { name = "arg", type = "text", default = "" },
]

[[uses]]
generator = "msg"
for_each  = "msgs"
with      = { page = "{= page =}", msg = "{= item.name =}", arg = "{= item.arg =}" }
```

- `generator` names another generator. Inside a pack a bare name means that pack's generator
  first (`msg` is `elm/msg`); `pack/name` or a project generator's name also work.
- `with` gives the used generator **all** of its inputs. Each value is a template rendered in
  this generator's scope, and the text it produces is parsed by the used generator's input
  types exactly as a `--set` would be.
- `for_each` names a list input (or list-valued derived value). The step runs once per item,
  with `item` and `loop` (`loop.index` from 1, `loop.first`, `loop.last`) in scope for `with`.
  An empty or unset list runs it zero times.

A generator's own `[requires]` and `[[edits]]` come first, then its `[[uses]]` in order. So
`elm/page` creates the page, and only then does `elm/msg` (whose `requires` wants the page)
run.

`show` describes a composite completely, including the used generator's inputs:

```
$ olifant show elm/page
…
uses:
  elm/msg (for each item of msgs)
    with arg={= item.arg =} msg={= item.name =} page={= page =}
    its input --set arg=<text>
    its input --set msg=<ident>
    its input --set page=<ident>
```

### No inherited scope

The used generator sees only what `with` gives it: not the caller's inputs, not its derived
values, not its `--set`s. That is deliberate. It keeps every generator runnable and testable
on its own, and it means a generator's inputs are its whole interface.

In practice this means you pass values explicitly, and you pass them *as text*. A name prints
as snake_case, which the next generator parses back into the same name. A list does not
print at all:

```
$ olifant gen wrap --set page=a --set msgs=x         # with = { msgs = "{= msgs =}" }
error: wrap: cannot print a list
```

To forward a list, render it back into the comma form:

```toml
with = { page = "{= page =}", msgs = "{= for m in msgs =}{= m.name =}{= if m.arg =}:{= m.arg =}{= end =}{= if not loop.last =}, {= end =}{= end =}" }
```

## A project composite

Here is a project generator that builds on a pack's: a page plus extra URLs for it, using
`elm/page` and the `alias` generator from [chapter 7](07-writing-a-generator.md):

```toml
# .olifant/generators/section/generator.toml
description = "A page plus extra URLs that show it"
language = "elm"
examples = [
  "olifant gen section --set page=help --set aliases=\"faq, support\"",
]

[inputs.page]
type = "ident"
prompt = "The new page"

[inputs.aliases]
type = "list"
of = "ident"
optional = true
prompt = "Other route names that show the page"

[[uses]]
generator = "elm/page"
with      = { page = "{= page =}" }

[[uses]]
generator = "alias"
for_each  = "aliases"
with      = { name = "{= item =}", page = "{= page =}" }
```

This version of `alias` takes `page` as an `ident` (and renders it with `|pascal`); why is
explained below.

```
$ olifant gen section --set page=help --set aliases="faq, support"
section (15 edits, 0 conflicts)
  + src/Page/Help.elm (42 lines)  [section › elm/page]
  ~ src/Main.elm (+1 line at line 8)  [section › elm/page]
  …
  ~ src/Route.elm (+1 line at line 11)  [section › alias[1]]
  ~ src/Route.elm (+1 line at line 20)  [section › alias[1]]
  ~ src/Route.elm (+1 line at line 12)  [section › alias[2]]
  ~ src/Route.elm (+1 line at line 22)  [section › alias[2]]
  ~ src/Main.elm (+2 lines at line 108, compiler: view)  [section › elm/page]
  ~ src/Main.elm (+2 lines at line 59, compiler: changeRouteTo)  [section › alias[2]]
  ~ src/Main.elm (+2 lines at line 59, compiler: changeRouteTo)  [section › alias[1]]
  ~ src/Main.elm (+2 lines at line 59, compiler: changeRouteTo)  [section › elm/page]
```

It compiles, and running it again is a no-op (every edit `= … skipped`). `section` passes no
`msgs`, so `elm/page` gets none: the caller's inputs are not inherited even when the names
match. Passing an input the composite does not declare is an error, as for any generator:

```
$ olifant gen section --set page=help --set msgs=foo
error: section: invalid inputs: unknown input `msgs`
```

Errors inside a nested step name the path to it:

```
$ olifant gen section --set page=help        # after changing `with` to {}
error: section › elm/page: invalid inputs: missing `page` (ident)
```

When a generator uses the same generator twice in its `[[uses]]`, the path says which use:
`… › elm/msg (use 2)[1]`.

### `choices_from` sees the steps before it

An enum whose choices come from the project (chapter 6) is resolved when its step comes up,
against the project *and* the files the run has created so far. So a chain can create
something and choose it in the next step:

```
$ olifant gen rust-htmx/service-skeleton --set service=payments + rust-htmx/service-command --set service=payments --set command_name=charge_card --human
rust-htmx/service-skeleton + rust-htmx/service-command (…)
  + payments/payments-service/Cargo.toml (12 lines)  [rust-htmx/service-skeleton (step 1)]
  …
```

`service-command`'s `service` choices come from `*/*-service`; `payments` is one because step
1 creates `payments/payments-service/…`. The same holds inside a composite: a `[[uses]]`
step sees what its parent's own edits, and the uses before it, create. A value that is in
neither the project nor the plan is still refused with the list of choices.

One limit: choices read files that earlier steps *create*; text that earlier steps *insert*
into existing files is not placed until planning, so a command or AST choice over such a file
sees it as it is on disk.

## Cycles

A generator that (directly or not) uses itself is refused before anything is planned, even
if the `for_each` list that would reach the cycle is empty:

```
$ olifant gen ping                  # ping uses pong, pong uses ping
error: generators use each other in a cycle: ping › pong › ping
```

Exit code 1: it is a broken generator, not a bad command line.

## What composition does not do

- **No conditionals.** A `[[uses]]` step runs (once, or per item). To make one optional,
  drive it with an optional list: an empty list runs it zero times.
- **No outputs.** A used generator cannot hand values back. Anything both need is derived in
  the caller and passed down with `with`.
- **Re-running needs the same inputs.** `elm/page --set page=settings` after `elm/page
  --set page=settings --set msgs=toggle_dark_mode` is a conflict (`already exists`), because
  planning the page without its message does not reproduce the file on disk. Olifant only
  treats a re-run as a no-op when it reproduces what is there.
- **A conflicted run prints everything.** A plan with a conflict is not applied, so the human
  output includes the full text of every edit that would have worked, as `--plan` does.
