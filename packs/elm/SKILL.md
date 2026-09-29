---
name: olifant-elm
description: Add pages and messages to an elm-spa-example style Elm app with the olifant elm pack, in one command, without reading or hand-writing the wiring.
---

# Elm pages and messages with Olifant

This app follows elm-spa-example conventions: each page is a `Page.X` module with
`Model` / `Msg` / `init` / `update` / `view`, wired into `src/Main.elm` (import, `Page`
variant, `GotXMsg`, `changeRouteTo`, `update`, `view`) and `src/Route.elm` (variant and
URL parser). The generators write all of that; you don't need to read the code.

## Do the whole task in one call

```sh
olifant gen elm/page --set page=settings --set msgs="toggle_dark_mode, set_display_name:String" \
          + elm/page --set page=profile --set msgs="follow, unfollow, got_bio:String"
```

- `elm/page --set page=NAME` creates `src/Page/Name.elm` at URL `/name` (snake_case →
  PascalCase module, kebab-case URL: `article_list` → `ArticleList` at `/article-list`).
- `msgs` is a comma-separated list of `name` or `name:PayloadType`. Names are snake_case
  (`got_bio` → `GotBio`). Payload types are Elm types; multi-word types are parenthesised
  for you (`got_articles:List String` → `GotArticles (List String)`), and tuples/records
  are fine (`set_pair:(Int, String)`).
- Every new message gets an `update` branch returning `( model, Cmd.none )`.
- Chain several steps with ` + `; the whole chain is one atomic plan (all or nothing).
- To add messages to an existing page: `olifant gen elm/msg --set page=NAME --set msg=NAME [--set arg=Type]`.
- Add `--plan` to preview without writing. Output is JSON when not on a terminal.
- Exit codes: 0 ok, 3 unknown generator, 4 bad inputs (the JSON lists what to fix), 5 conflict (nothing written).
