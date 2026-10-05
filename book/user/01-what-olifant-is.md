# 1. What Olifant is

Olifant writes the boring, convention-following parts of a change for you. You tell it
*what* ("a page called settings, with a `toggle_dark_mode` message"); it knows *how* this
codebase does that and edits every file involved.

Try it (see [the book's README](../README.md) for the sandbox):

```sh
olifant gen elm/page --set page=settings --set msgs="toggle_dark_mode"
git add -A && git diff --cached --stat
```

```
 src/Main.elm          | 12 ++++++++++++
 src/Page/Settings.elm | 46 ++++++++++++++++++++++++++++++++++++++++++++++
 src/Route.elm         |  2 ++
 3 files changed, 60 insertions(+)
```

One command created the page module and wired it into seven places across `Main.elm` and
`Route.elm`: the import, the `Page` and `Msg` variants, and the `changeRouteTo`, `update`
and `view` branches, plus the route and its URL parser. It also added the new message and
its `update` branch. The result compiles.

It can also start from nothing. `olifant init --pack python-htmx bistro` makes a running
web app (uv, pytest and pyright strict, FastAPI and htmx), its tests green and its first
commit made; from there each generator adds a piece of the architecture, a command with
its red test stub or a repository with its passing contract test ([chapter
2](02-running-generators.md#start-a-project-olifant-init)).

## The pieces

- **A generator** is one convention, written down as data: `elm/page` knows how *this
  kind of app* adds a page. It lives in a directory with a `generator.toml` (what to ask
  for, and what to edit) and sometimes templates.
- **A pack** is a set of generators for one kind of codebase (`elm`, `rails`,
  `rust-htmx`, …), plus a small **fixture app** they are tested against, and **language
  adapters** that teach Olifant a language's structure.
- **Inputs** are what you tell a generator (`--set page=settings`). Olifant derives the
  rest: `settings` becomes the module `Settings`, the URL `/settings`, the message
  `GotSettingsMsg`.
- **A plan** is the list of edits a run will make. Olifant always builds the whole plan
  first and then writes it all at once, or, if anything cannot be done, writes nothing.

## What makes it different from a snippet tool

- **It edits existing files by structure, not by markers.** "At the end of `type Page`" or
  "after the last `case` branch in `update`" is found by parsing the code, so no
  `// generator: insert here` comments are needed in your app ([chapter 3](03-anchors.md)).
- **The compiler finds places the generator didn't know about.** When a new `Route`
  variant makes some `case` incomplete, Olifant asks the Elm compiler where, and adds
  the branch (GHC and rustc work the same way) ([chapter 4](04-compiler-branches-and-todos.md)).
- **It is built for agents as much as for people.** Output is JSON when not on a
  terminal, exit codes are stable, every error says what to do, and a whole task
  (several pages, many messages) is one command. In measurements, an agent using the
  Elm pack does a three-page change with ~100k tokens of context, whether it is a large
  or a small model; writing the same change by hand costs 3–5× that
  (`notes/durandal/experiments/token-ab/`).
- **It prefers deterministic code to AI.** A generator's output is exact and repeatable.
  AI is reserved for the small, typed gaps a template cannot know: typed holes, which a
  model (local or remote) fills and the compiler checks, and which compile as placeholders
  when there is no model ([chapter 12](12-holes-and-models.md)).

## Where things are

| what | where |
|---|---|
| the CLI | `olifant` (installed to `~/.local/bin`) |
| packs | `packs/<name>/` in this repo; a project lists the ones it uses in `.olifant/config.toml` |
| a project's own generators | `.olifant/generators/<name>/generator.toml` |
| the design | `~/projects/notes/durandal/generator-spec.md` |
