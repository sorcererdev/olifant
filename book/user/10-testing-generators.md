# 10. Testing generators: snapshots and blessing

A pack generator is tested by running it against the pack's fixture app and comparing what it
wrote with a recorded **snapshot**. Every pack case in this repository runs as part of
`cabal test`:

```
$ export PATH="$HOME/.ghcup/bin:$PATH"
$ cabal test --test-show-details=direct --test-options='--match "packs" --match "elm pack output compiles"'
…
Olifant.GeneratorTesting
  packs
    has pack cases to run [✔]
    elm/msg/got-articles [✔]
    elm/msg/increment-by [✔]
    elm/msg/reset [✔]
    elm/page/counter [✔]
    elm/page/settings-with-msgs [✔]
    olifant-dev/generator/rails-controller [✔]
    olifant-dev/internal-module/marker-rules [✔]
    olifant-dev/unit/workspace-sync [✔]
    rails/model/post [✔]
    redwood/cell/blog-post [✔]
    redwood/cell/blog-posts [✔]
    rust-htmx/service-command/cancel-subscription [✔]
  elm pack output compiles
    elm/msg/got-articles [✔]
    elm/msg/increment-by [✔]
    elm/msg/reset [✔]
    elm/page/counter [✔]
    elm/page/settings-with-msgs [✔]

Finished in 7.6868 seconds
22 examples, 0 failures
```

(Four `Olifant.Catalog` tests whose names also contain "packs" ran too and are left out
above; they make up the 22.)

## A test case

```
packs/<pack>/generators/<generator>/tests/<case>/
  inputs.toml
  snapshot/            the files the run should write, at their project paths
```

```toml
# packs/elm/generators/msg/tests/got-articles/inputs.toml
now = "2026-09-28T12:00:00Z"

[inputs]
page = "home"
msg = "got_articles"
arg = "List String"
```

`now` is required (it pins `{= now =}` and `|timestamp`). Every value under `[inputs]` is a
**string**, written as you would write it after `--set`: a list is `"index, show"`, a bool is
`"true"`. A bare TOML `true` makes the case fail with "needs `now` and an [inputs] table of
strings".

The snapshot holds only the files the run creates or changes. The got-articles case's
snapshot is one file, `snapshot/src/Page/Home.elm`, the fixture's `Home.elm` after the
message was added.

## What a case checks

`runCase` in `src/Olifant/GeneratorTesting.hs`:

1. Loads the fixture app (`fixtures/app/`) into an in-memory workspace; nothing on disk is
   touched.
2. Runs the generator with the case's inputs, applied. Owned regions from the pack's
   `regions.toml` are refreshed too, as in a real `gen`.
3. **Conflicts fail the case**, listing each one; unless *every* conflict is only a missing
   tool (no `ast-grep`, no `elm`), in which case the case is **pending**, not failed.
4. **Snapshot equality**, both ways: every written file must be in the snapshot with the same
   content, and every snapshot file must have been written.
5. **Re-run is a no-op**: the same inputs are run again on the result, and must write nothing.
   This is what catches an `insert` that is not idempotent, or a `create` that would not
   reproduce its own file.

For the Elm pack there is one more group, **"elm pack output compiles"**: for each Elm case,
the fixture with the snapshot laid over it is compiled with the adapter's `[check]` command,
and must compile cleanly. Structure-placed code that is in the right place but syntactically
wrong fails here.

## Writing a new generator, test first

This repository's own pack has a generator for generators. Run it from the repo root (the
walkthrough below was done in a scratch copy of the repo):

```
$ olifant gen olifant-dev/generator --set pack=rails --set name=controller
olifant-dev/generator (3 edits, 0 conflicts)
  + packs/rails/generators/controller/generator.toml (10 lines)
  + packs/rails/generators/controller/templates/main.dt (1 line)
  + packs/rails/generators/controller/tests/example/inputs.toml (5 lines)
```

It writes a skeleton generator (`create = "TODO/{= name|snake =}.txt"`), a template, and a
test case whose `inputs.toml` says how to bless it. Replace the skeleton with the real thing:

```toml
# packs/rails/generators/controller/generator.toml
description = "A controller with empty actions (like `rails g controller`)"
language = "ruby"
version = 1
examples = [
  "olifant gen rails/controller --set name=posts --set actions=\"index, show\"",
]

[inputs.name]
type = "ident"
prompt = "Controller name (plural, e.g. posts)"

[inputs.actions]
type = "list"
of = "ident"
optional = true
prompt = "Action names, comma separated"

[[edits]]
create = "app/controllers/{= name|snake =}_controller.rb"
template = "controller.rb.dt"
```

Delete the skeleton's `templates/main.dt` and write `templates/controller.rb.dt`:

```
class {= name|pascal =}Controller < ApplicationController
{=- for a in actions =}
{= if not loop.first =}
{= end =}  def {= a|snake =}
  end
{=- end =}
end
```

and the case's inputs:

```toml
now = "2026-01-01T00:00:00Z"

[inputs]
name = "posts"
actions = "index, show"
```

With no snapshot yet, the case fails and says why:

```
$ cabal test --test-show-details=direct --test-options='--match "rails"'
…
    rails/controller/example [✘]
…
  1) Olifant.GeneratorTesting.packs rails/controller/example
       expected: CasePassed
        but got: CaseFailed ["app/controllers/posts_controller.rb: generated but not in snapshot"]
```

## Blessing

**Blessing** records the current output as the snapshot:

```
$ OLIFANT_BLESS=1 cabal test --test-options='--match "/Olifant.GeneratorTesting/packs/rails/controller/"'
    rails/controller/example [✔]

$ cat packs/rails/generators/controller/tests/example/snapshot/app/controllers/posts_controller.rb
class PostsController < ApplicationController
  def index
  end

  def show
  end
end
```

A blessed run always passes for the cases it ran: it writes every file the run wrote into
`snapshot/` instead of comparing. So blessing is not a test; **read the snapshot diff** before
you commit it (`git diff packs/`). Narrow `--match` to the cases you meant to change, so an
unrelated regression is not blessed along the way.

After the generator changes, the case says where the output moved:

```
  1) Olifant.GeneratorTesting.packs rails/controller/example
       expected: CasePassed
        but got: CaseFailed ["app/controllers/posts_controller.rb: differs\n  line 3\n  expected:   end\n  actual:       head :ok"]
```

(the first differing line, not a full diff). If the change is intended, bless again.

Blessing also removes snapshot files the run no longer generates: rename the output
(`_controller.rb` → `_ctl.rb`) and bless, and the old `posts_controller.rb` is deleted from
`snapshot/` while the new `posts_ctl.rb` is added. Still read the diff before committing it —
blessing is recording, not checking.

## Missing tools make cases pending

Structural anchors need `ast-grep`, and `complete` edits need the language's compiler. When
they are not installed, the case is pending, not failed:

```
$ PATH="$HOME/.ghcup/bin:/usr/bin:/bin" cabal test --test-show-details=direct --test-options='--match "elm/msg/reset"'
Olifant.GeneratorTesting
  packs
    elm/msg/reset [‐]
      # PENDING: edit 1: cannot place text in src/Page/Home.elm: `end of type Msg`: ast-grep is not installed (brew install ast-grep); elm/msg: cannot complete cases: `elm` is not installed (needed to check elm)
  elm pack output compiles
    elm/msg/reset [‐]
      # PENDING: elm is not installed

Finished in 0.0383 seconds
2 examples, 0 failures, 2 pending
```

"0 failures" there is not a pass. On a machine or CI runner without the tools, look for
`pending` in the summary.

Set `OLIFANT_REQUIRE_TOOLS=1` where the tools are supposed to be there (CI, the pre-merge
hook) and a missing tool fails the case instead of letting it pass as pending:

```
$ PATH="$HOME/.ghcup/bin:/usr/bin:/bin" OLIFANT_REQUIRE_TOOLS=1 cabal test --test-show-details=direct --test-options='--match "elm/msg/reset"'
  packs
    elm/msg/reset [✘]
  elm pack output compiles
    elm/msg/reset [✘]

Failures:

  1) Olifant.GeneratorTesting.packs elm/msg/reset
       required tool missing: edit 1: cannot place text in src/Page/Home.elm: `end of type Msg`: ast-grep is not installed (brew install ast-grep); elm/msg: cannot complete cases: `elm` is not installed (needed to check elm)

  2) Olifant.GeneratorTesting, elm pack output compiles, elm/msg/reset
       required tool missing: elm is not installed

2 examples, 2 failures
```

The pre-merge hook sets `OLIFANT_REQUIRE_TOOLS=1`, so a merge into `main` on a machine
missing `ast-grep` or `elm` fails instead of quietly skipping those cases.

## Other limits of pack tests

- **Command queries run in the fixture directory on disk.** A `choices_from` or region
  `query` with a `command` runs for real, rooted at the case's `fixtures/app/` — like a
  project's own command queries run rooted at the project. The case's in-memory copy of the
  fixture is what gets written to and compared; the command only reads what is on disk.
- **`choices_from` in a `[[uses]]` generator is resolved from the fixture too.** A case for a
  composite whose `[[uses]]` target has a `choices_from` input works the same as it does from
  the CLI.
- **Failure messages are words, not Haskell values.** A case failure (a generation error, or
  one from re-running) prints the same sentence the CLI would, e.g. "undefined variable
  `nope`", not `CaseFailed ["TemplateFailed …"]`.
- **One fixture per pack.** Every case of every generator runs against the same
  `fixtures/app/`. A case that needs a different starting point (a page that already has
  messages) needs the fixture to contain it.

## Trying a pack by hand: `scripts/try-pack.sh`

Tests pin behaviour; a sandbox is for exploring. `scripts/try-pack.sh <pack> [dir]` copies
the pack's fixture app to a new directory (a fresh temp dir if you give none; the directory
must not exist), writes a `.olifant/config.toml` pointing at the pack *in this checkout*,
and commits everything to a new git repo:

```sh
scripts/try-pack.sh rails /tmp/olifant-rails && cd /tmp/olifant-rails
olifant gen rails/model --set name=post --set fields="title, body:text" --plan
git add -A && git diff --cached        # everything the last run changed
git reset --hard && git clean -fd      # start over
```

Because the config points at the checkout, edits to the pack's generators show up in the
sandbox immediately. The `olifant` binary does not: it is whatever `~/.local/bin/olifant` was
built from. Run `scripts/install-olifant.sh` to install the current branch's binary.
`scripts/try-pack.sh` with no arguments lists the packs and their generators.

## Running the suite while you work

- `cabal test` runs everything; `--test-options='--match "rails"'` narrows it (hspec's
  `--match` takes a substring or a `/Group/path/`).
- `scripts/test-watch.sh` reruns the suite on every save of a Haskell source or pack file
  (it uses `ghcid`; extra arguments go to hspec, e.g. `scripts/test-watch.sh --match
  "packs"`).

## The repository's hooks

`.githooks/` holds three hooks, enabled with `git config core.hooksPath .githooks` (already set
in the owner's clone; a fresh clone needs it):

| hook | on `main` only | does |
|---|---|---|
| `pre-merge-commit` | yes | runs `cabal test` with `OLIFANT_REQUIRE_TOOLS=1` (a missing `ast-grep` or `elm` fails instead of pending), then `olifant sync --check` (via `cabal run`, so it checks with the code being merged); either failing aborts the merge ("fix, then run `git merge --continue`"). `git merge --no-verify` skips it |
| `post-commit` | yes | runs `scripts/install-olifant.sh`, so `~/.local/bin/olifant` is always built from `main`; never blocks the commit |
| `post-merge` | yes | the same install, after a merge into `main` |

So pack snapshots, the Elm "compiles" check, and the cabal module lists (chapter 5) are all
enforced at the point a branch lands on `main`.

Git only runs `pre-merge-commit` when a merge creates a merge commit; a fast-forward merge
(`git merge` of a branch that is strictly ahead) skips it, and with it the tests.
`git config branch.main.mergeoptions --no-ff` makes a plain `git merge` into `main` always
create a merge commit, so the hook always runs (already set in the owner's clone; a fresh
clone needs it too).
