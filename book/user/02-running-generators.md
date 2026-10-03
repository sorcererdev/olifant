# 2. Running generators

Four commands cover almost everything: `list`, `show`, `gen`, and `gen --plan`.

## What can I run? `olifant list`

```
$ olifant list
elm/msg   A new Msg for an Elm page: the variant plus an update branch returning ( model, Cmd.none )
elm/page  A page in the Elm architecture (Model/Msg/init/update/view), wired into Main and Route like elm-spa-example

Tip: run a whole task in one call: olifant gen A --set k=v + B --set k=v (or --steps FILE with …)
```

Generators from packs are named `pack/name`; your project's own generators (in
`.olifant/generators/`) have no prefix. `olifant list --detail` also prints every
generator's inputs, which saves a round of `show` calls.

## What does it need? `olifant show`

```
$ olifant show elm/page
elm/page: A page in the Elm architecture (Model/Msg/init/update/view), wired into Main and Route like elm-spa-example

inputs:
  --set page=<ident>
      Page name (e.g. counter, article_list)
  --set msgs=<list<name:arg>>  (optional)
      Messages to add to the page, as name or name:PayloadType, comma separated (e.g. toggle_dark_mode, set_display_name:String)

edits:
  create src/Page/{= P =}.elm
  insert src/Main.elm at after last line matching ^import Page\.
  insert src/Main.elm at end of type Page
  …
  complete Route.{= P =} in case in changeRouteTo
  complete Main.{= P =} in case in view

uses:
  elm/msg (for each item of msgs)
    …

examples:
  olifant gen elm/page --set page=settings --set msgs="toggle_dark_mode, set_display_name:String"
```

- **inputs** are what you pass with `--set`. An `ident` is a name in any case
  (`article_list`, `ArticleList` and `article list` are the same name); Olifant renders it
  in whatever case each place needs. An `enum` lists its choices, often found in your project; when
  they depend on an input above (a service's commands), `show` lists them per value, and
  `olifant show GEN --set service=billing` narrows them to that one
  ([chapter 6](06-queries-and-choices.md#dependent-choices)).
- **edits** are what it will do, with `{= … =}` placeholders filled from your inputs.
  `insert … at …` places text by structure ([chapter 3](03-anchors.md)); `complete …`
  adds `case` branches wherever the compiler asks for them ([chapter 4](04-compiler-branches-and-todos.md)).
- **uses** lists the other generators it runs for you: `elm/page` runs `elm/msg` once per
  message.

## Run it: `olifant gen`

```
$ olifant gen elm/page --set page=settings --set msgs="toggle_dark_mode"
elm/page (11 edits, 0 conflicts)
  + src/Page/Settings.elm (42 lines)  [elm/page]
  ~ src/Main.elm (+1 line at line 8)  [elm/page]
  …
  ~ src/Page/Settings.elm (+1 line at line 25)  [elm/page › elm/msg[1]]
  ~ src/Page/Settings.elm (+2 lines at line 34)  [elm/page › elm/msg[1]]
  ~ src/Main.elm (+2 lines at line 108)  [elm/page]
  ~ src/Main.elm (+2 lines at line 59)  [elm/page]
```

`+` is a new file, `~` an insert, `=` an edit skipped because its text is already there,
`!` a conflict. `compiler: view` means the Elm compiler found that spot
([chapter 4](04-compiler-branches-and-todos.md)). `region library-modules: +1 −0` is an owned region being
refreshed ([chapter 5](05-owned-regions.md)); `skipped: unless: …` is an edit whose `unless`
matched ([chapter 6](06-queries-and-choices.md)). The tag on the right says which generator made the edit;
`elm/page › elm/msg[1]` is the first message's `elm/msg` run inside `elm/page`.

`gen` applies by default. Add `--plan` to see the same plan, with the full text of every
edit, without writing anything — unless the plan has a conflict, in which case nothing would
be written anyway, so neither `--plan` nor a plain `gen` prints any edit's text.

## One task, one command: `+`

Chain steps with a standalone `+`. The whole chain is one plan: all of it is written, or
none of it.

```sh
olifant gen elm/page --set page=settings --set msgs="toggle_dark_mode, set_display_name:String" \
          + elm/page --set page=profile --set msgs="follow, got_bio:String"
```

For many steps, or steps built by a program, put them in JSON and pass `--steps FILE`
(`-` reads stdin): `[{"gen": "elm/page", "set": {"page": "settings"}}, …]`.

List inputs take commas; a message's payload type follows a colon. Multi-word types are
parenthesised for you (`got_articles:List String` becomes `GotArticles (List String)`).

## When it says no

Olifant writes nothing unless the whole plan is sound, and every refusal says why:

```
$ olifant gen elm/page
error: elm/page: invalid inputs: missing `page` (ident)
```

```
$ olifant gen elm/page --set page=profile          # in an app whose parser function was renamed
Plan: elm/page (9 edits, 1 conflict): nothing written
  …
  ! src/Route.elm (conflict, see below)
  …
  ! edit 7: cannot place text in src/Route.elm: `end of list in parser`: not found
```

A conflict shows the edits that *would* have worked, so you can see how close it came.

```
$ olifant elm/page --set page=x
error: unknown command `elm/page`; did you mean `olifant gen elm/page --set page=x`?
```

| exit code | meaning |
|---|---|
| 0 | done (including "nothing to do") |
| 1 | a generator or pack is broken |
| 2 | the command line is wrong |
| 3 | no such generator, file or pack |
| 4 | inputs missing or invalid (the JSON lists each one) |
| 5 | conflict: nothing was written |

## For scripts and agents: JSON

When stdout is not a terminal, output is JSON (force either with `--json` / `--human`).
Each edit says how it was placed:

```json
{"id": "e2", "path": "src/Main.elm", "status": "insert", "line": 9, "anchor_used": "after last line matching ^import Page\\."}
{"id": "e9", "path": "src/Main.elm", "status": "insert", "line": 65, "found_by": "compiler", "site": "changeRouteTo", "todo": false}
```

The plan also has `conflicts`, `warnings`, `todos` and `holes` arrays. An applied run leaves
out file contents (you have the files); `--plan` includes them.

## Holes and models: `--model`, `--no-fill`

Some generators leave a small typed gap, a **hole**, for a model to fill: `elm/msg` does
when you give it an `intent`. With a model configured under `[model]` in
`.olifant/config.toml` ([chapter 12](12-holes-and-models.md)), `gen` fills it and lists it
under `holes:`:

```
$ olifant gen elm/msg --set page=home --set msg=reset --set intent="reset count to 0" --human
elm/msg (2 edits, 0 conflicts)
  ~ src/Page/Home.elm (+1 line at line 25)
  ~ src/Page/Home.elm (+2 lines at line 34, compiler: update)
holes:
  update (expr) src/Page/Home.elm:35  filled by qwen3-coder:30b: ( { count = 0 }, Cmd.none )
```

(run on a `Home.elm` whose `Model` is `{ count : Int }`). Each hole's line says where it is
and how it ended: `filled by MODEL: CODE` (with `(attempt N)` after a retry), `unfilled:
WHY` (it was left as a placeholder that compiles: no model is configured, or the model
failed), or `skipped: --no-fill`. In JSON, `holes` has `name`, `kind`, `file`, `line`,
`status`, `model`, `attempts`, `why` and `fill`.

- `--model NAME` fills holes with `[model.backends.NAME]` instead of the default.
- `--no-fill` leaves every hole as its placeholder, and asks no model.

Both apply to the whole run, like `--plan`. `--plan` does run the model, so it shows the
fills it would write. [Chapter 13](13-tutorial-holes.md) is a tutorial with a local
model.

## Running it again

Re-running a generator with the same inputs is a no-op (exit 0, nothing written): inserts
whose text is already in place are skipped (`=`), and a file the run would create is left
alone when planning it from scratch reproduces it exactly, even when later steps (a
page's messages) changed it after creation. A file that was created and then edited by
hand still stops the run (`already exists`), so your edits are never overwritten.
