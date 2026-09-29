# The Olifant books

Two books, written alongside the code. Each chapter is one Markdown file, starts with
something you can run, and points at the code it describes.

- **[The user book](user/README.md):** using Olifant well, from running a generator to
  writing and testing your own.
- **[The internals book](internals/README.md):** how Olifant works, so you can change it.

Examples use a throwaway sandbox of the Elm pack's fixture app:

```sh
cd ~/projects/olifant
scripts/try-pack.sh elm /tmp/olifant-elm     # copies the fixture, configures the pack, git-commits it
cd /tmp/olifant-elm
```

The sandbox is a git repo, so after any `olifant gen` you can see exactly what changed with
`git add -A && git diff --cached`, and start over with `git reset --hard && git clean -fd`.

You need `olifant` on your PATH (`scripts/install-olifant.sh`, or the git hooks install it
whenever `main` changes), plus `ast-grep` and `elm` for the Elm pack
(`brew install ast-grep elm`).

When a chapter and the code disagree, the code is right and the chapter is a bug: fix it in
the same branch as the code change.
