# Olifant: notes for agents

## The books are part of every change

`book/user/` (using olifant) and `book/internals/` (how it works, so you can change it) are
kept current in the same branch as the code. The user onboards from them to write code, so a
chapter that disagrees with the code is a bug.

- A change to behaviour, commands, config keys or a pack updates the user chapter that covers it.
- A new or reshaped module, data flow or seam updates the internals book (the tour, or the
  chapter for that area), naming the functions and files a reader would open.
- The pre-merge hook refuses a branch that changes `src/`, `app/`, `library/` or `packs/`
  without touching `book/`. When no chapter needs changing (a pure refactor the book does not
  describe, a test-only fix), say why in a `Book: <reason>` trailer on one of the branch's commits.

## Other conventions

- Modules in `olifant.cabal` are a generated region: add new modules with `olifant sync`.
- Run the suite and merge only in the main checkout, never a worktree (the git specs touch the
  real repository).
