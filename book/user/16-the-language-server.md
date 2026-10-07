# 16. The language server

`olifant lsp` is Olifant as a language server: one process for the editor session that speaks
the Language Server Protocol over stdin and stdout. Everything Durandal shows in an editor comes
from it — the ⚡ menu, hints, snippets, ghost text and follow-ups, rule findings, regions kept on
save, the generator picker, doctor. The CLI stays for scripts, agents and CI; the two share the
engine (`Olifant.App`), never each other's code.

Why a server rather than the CLI per keystroke:

- **It holds your unsaved text.** Every open document is an overlay over the disk, so nothing is
  saved before Olifant plans, and a plan reads all your open buffers, not one.
- **It remembers.** Each project's catalog, its file listing, and every answer are cached until
  something changes them. From the caches, every request answers in under a millisecond; the
  first request in a project of 5,000 files takes about 175 ms (measured in Task 16).
- **It cancels.** The next edit supersedes a suggestion still being worked out, and an
  editor's `$/cancelRequest`, or a cancel on the request's progress, stops a request running in
  the background (a ⚡ action being resolved, a snippet being filled; it answers "cancelled"). Either way the work's child processes — cargo, pyright,
  uv, curl — are killed. An answer for an older version of a buffer is never shown, and the
  edits the server returns carry the version they were planned against, so an editor refuses
  them once you have typed on.
- **Any editor can use it.** Neovim first (durandal.nvim); the protocol is the same for the
  rest.

## Starting it

durandal.nvim starts it for you when a file of an Olifant project opens
([chapter 14](14-editors-and-actions.md)). By hand, in another editor:

```
olifant lsp
```

It reads LSP from stdin and writes to stdout; its own log goes to stderr. It needs no
arguments and no root: it finds each file's project itself (below).

`initializationOptions` (all optional):

| key | default | what it does |
|---|---|---|
| `debounce_ms` | `300` | how long typing must pause before slow work (suggestions) starts |
| `model` | the project's `[model]` default | which `[model.backends.NAME]` fills holes ([chapter 12](12-holes-and-models.md)) |
| `fills` | none | a recorded `[fills]` file standing in for the model (tests, demos) |
| `debug` | `false` | answers `olifant/debugState` (tests) |

Everything else comes from each project's `.olifant/config.toml`, as with the CLI.

## Projects across roots

The server holds as many projects as you open, in one repository or several. Each file belongs
to its **nearest project**: the closest folder above it with a `.olifant/` (as the CLI finds it,
[chapter 9](09-packs-and-language-adapters.md#the-project-root)). A Rust service, a Python
service and an Elm frontend in one session each get their own actions, rules, snippets and
regions.

- **Each project keeps its own conventions**: its packs, `.olifant/generators/`, rule severities
  and allowances, `[model]`, language adapters.
- **No inheritance.** A project nested inside another does not take its parent's conventions.
- **Isolation.** Caches and jobs are per project. Slow work (resolves, completion, holes, the
  model, the region refresh after a save) runs off the server's message loop, so a slow check
  in one project never blocks another project's requests.
- **Packs are loaded once per copy.** Two projects using the same pack folder share its parsed
  generators; a project with its own copy has its own.
- **A file in no project** gets nothing: every request answers empty, never an error.

The server loads a project the first time one of its files opens, then asks the editor to watch
its files and its packs (`workspace/didChangeWatchedFiles`, each relative to its own folder, so
a project in another repository is watched too). Editing a `generator.toml`, `rules.toml` or
`config.toml` reloads the projects that use it; files changed outside the editor (by `olifant
gen` in a terminal, or an agent) are seen at once. A project whose configuration no longer loads
(a broken `generator.toml`) keeps serving its last good catalog, if it had one, and every one of
its files gets an error at the top saying why, until the configuration loads again.

## What it answers

Standard LSP where the protocol has it; `olifant/*` methods only where it does not.

| what you see | request | notes |
|---|---|---|
| Rule findings | `textDocument/diagnostic` (pulled) | source `olifant`, the rule's id as the code; a baselined finding is a hint ending "(existing)" |
| A finding's fix | `textDocument/codeAction`, kind `quickfix` | first, `isPreferred`; a fix that needs a choice (which port) carries it in `data`, the editor asks before resolving |
| The ⚡ menu | `textDocument/codeAction`, kind `refactor` | resolved lazily (`codeAction/resolve`): the plan, holes filled by the model, as a `WorkspaceEdit`; the plan itself in `data.plan` |
| ⚡ hints | `textDocument/inlayHint` | one per line with actions; `+N` when several apply |
| Near misses | `textDocument/hover` | the actions on the line and the generators that nearly apply in the file, and why not |
| Snippets you choose | `textDocument/completion` | snippet items (tab stops); `completionItem/resolve` fills holes |
| Ghost text | `textDocument/inlineCompletion` | the snippet body, unfilled; accepting it inserts it and the editor walks the stops |
| Follow-ups | `olifant/suggestion` (pushed), `olifant/suggest` (asked) | `uri`, `version`, `kind`, `label`, `summary`, `line`, `lines`, `at`, `planId` |
| A follow-up's plan | `olifant/plan {planId}` | `{plan, edit}`; `fill: true` fills its holes first |
| Regions on save | `textDocument/willSaveWaitUntil` | the saved file's region edits, in the same write; other files' through `workspace/applyEdit` |
| Every action in a file | `olifant/fileActions {textDocument}` | the shape `olifant actions --file` prints |
| Holes left in the project | `olifant/holes {textDocument}` | path, line, name, kind, instruction; a hole's code action fills it |
| The generator picker | `olifant/generators`, `olifant/choices {generator, sets}`, `olifant/plan {textDocument, generator, sets}` | as `olifant list`, `show`, `gen --plan` |
| The project model | `olifant/model {textDocument}` | [below](#the-project-model) |
| Recognition | `olifant/suggestion` / `olifant/suggest` with kind `recognition` (band, or, reasons, score, sets); `olifant/recognitionFeedback {textDocument, generator, outcome}`; `olifant/plan {…, recognize: true}` | [below](#recognition-this-looks-like-a-command) |
| Doctor | `workspace/executeCommand` `olifant.doctor` (argument: a document's uri) | the checks `olifant doctor --json` prints |
| Progress | `$/progress` | the same words as the CLI's `--progress`: "checking rust…", "asking qwen3-coder:30b to fill body…" |

**Regions are refreshed once per save**: `willSaveWaitUntil` works out the refresh, puts the
saved file's part in the write, and the other files' part follows the save.

**Edits are tied to versions.** A plan is made against the documents as they are; a follow-up's
plan asked for after the document changed is refused ("the document changed since this was
suggested"), and the editor asks again.

**Plans become `WorkspaceEdit`s**: the smallest run of changed lines per file (your cursor and
folds stay put), `CreateFile` for new files.

## The project model

Each project has a model in a vocabulary every pack shares: `service`, `command`, `query`,
`port`, `adapter`, `resource`, `model`, `test`, `page` (and a pack's own, namespaced:
`elm:route`, `haskell:unit`), related by `uses`, `implements`, `calls` and `covers`. A pack's
`model.toml` says which files are which ([chapter 9](09-packs-and-language-adapters.md#the-project-model-modeltoml)).

```
olifant/model { textDocument: { uri: "file:///…/billing/billing-service/src/cancel_invoice.rs" } }
→ { project: "shop",
    elements:  [ { id: "shop:billing/commands/cancel_invoice", kind: "command", name: "cancel_invoice", path: "…", line: … }, … ],
    relations: [ { from: "shop:billing/adapters/in_memory_invoice_repository", kind: "implements", to: "shop:billing/ports/invoice_repository" }, … ],
    problems:  [] }
```

The ids start with the project's name (`name` in `.olifant/config.toml`, else its folder's
name). They are the same however files are listed, and the same words in every project, so a
Python project's model and a Rust project's can be put side by side. Nothing draws the model yet:
it is the seam a view of the whole system — services and how they call each other, what is
tested, what is red — builds on.

In Neovim:

```
:lua vim.print(require("durandal.lsp").client():request_sync("olifant/model", { textDocument = vim.lsp.util.make_text_document_params() }, 10000).result)
```

## Recognition: this looks like a command

Someone who does not know a convention exists writes it their own way. Recognition notices, and
offers the convention: write a command by hand in `booking/commands/close_table.py`

```python
class CloseTable:
    def __init__(self, repo):
        self.repo = repo

    def run(self, table_id):
        table = self.repo.table(table_id)
        self.repo.save(table)
```

pause, and Durandal says, on the line where it starts:

```
⚡ This looks like a command (Close Table) — Tab: review
```

Tab shows the review — the file converted to the convention (its Input, its failure union,
`handle`) and `close_table__test.py` beside it; Tab again applies it. A copy of a command,
renamed, is offered its companions instead (`adds cancel_booking__test.py`), its own text left
alone.

**How it decides**, with no model:

- **Where the file is.** The strongest signal: at a generator's output path
  (`src/*/*/commands/*.py`), or beside the project's own files of that kind.
- **What it is shaped like.** Code as token shapes — keywords and punctuation kept, names as their
  case (`CloseTable` and `SeatParty` read the same), literals as `str`/`num` — compared with the
  template and with the project's own instances of the convention. Rare patterns count more than
  ones in every file (`self.x = x` says little; `@dataclass(frozen=True)` much more).
- **What it declares.** A convention about a struct (a class, a fn) is not what code declaring
  none is: an enum in a service's folder is no command.
- **Only new files.** A file the project had when the server loaded it is what it is; recognition
  is for files written since (unsaved new buffers, files copied in).

**How loud:**

| | |
|---|---|
| at its place, sure enough | an offer: `⚡ … — Tab: review` |
| two conventions both fit here (a port or a repository) | a choice: `⚡ … — Tab: choose` |
| off its place, but shaped like it | a quiet hint: the dim `⚡`, first in `<leader>ma` with its reasons |
| otherwise | nothing |

Inputs come from the path (`booking`, `close_table`), else from the names declared
(`class CloseTable` → `close_table`), else from the folder; what is still missing the picker
asks for.

**It stays quiet when you want it to.** Esc on the line dismisses it, for that file until its
shape changes. Three dismissals in a row make that convention a quiet hint in the project
(`<leader>ma` → *Show recognitions for … again* undoes it). This is kept in
`.olifant/state/recognition.json`, which `olifant init` keeps out of git.

**A model, if you want one.** Recognition works without AI. With

```toml
[recognize]
model = "local"     # a [model.backends.NAME]
```

the model is asked — only when recognition is unsure (a hint, a choice, a shape that favours
another convention) — a typed question: which of these conventions is this code, with which
inputs. The deterministic answer shows first; a yes upgrades it in place. Its inputs are checked
against their types; a model that does not answer changes nothing. `olifant doctor` checks the
backend. `[recognize] enabled = false` turns recognition off for a project; `recognize = false`
in a `generator.toml` for one generator (a project's starter never is recognised).

**Seeing why.** `<leader>ma` on a recognition lists its reasons: `beside 4 commands; 59% shaped
like create_booking.py`. Every offer, accept and dismiss goes to the local action log
(`~/.cache/durandal/actions.jsonl`) with its band and score — never anywhere else.

## Checks scoped to the package

A language adapter may name the files that mark a package's root, `[check] root =
["pyproject.toml"]` (or `["Cargo.toml"]` with `root_pick = "outermost"`, the workspace's),
[chapter 9](09-packs-and-language-adapters.md#the-keys). A check, an `uncovered` query and a
hole's verify then copy and run only the package around the files they judge, not every file
of the language in the project. The CLI gets this too.

## Troubleshooting

- **`:checkhealth durandal`** says whether the server runs, which projects it holds (and how
  often each was loaded), then the doctor's checks from the server.
- **"olifant lsp stopped"**: the server exited (a request that fails answers an error instead;
  this is a crash or a kill). Its stderr is in Neovim's LSP log
  (`:lua vim.cmd.edit(vim.lsp.get_log_path())`). Opening a file of a project starts it again,
  and the features come back; so does `:LspRestart olifant`.
- **"this project's Olifant configuration does not load: …"** at the top of a file: what the
  message says is wrong (a `config.toml`, `generator.toml` or `rules.toml`). `:checkhealth
  durandal` shows it too. Save the fix; the server loads the project again.
- **Nothing in a file**: is it in a project? The nearest `.olifant/` above it decides. A file
  in no project gets nothing, by design.
- **A change to a generator is not seen**: the editor must support dynamic registration of
  watched files with relative patterns (Neovim 0.11+ does). Reopen the file to be sure.
- **Slow ghost text or hints after an edit**: what typing costs is one ast-grep run per
  generator's `[applies_at]`, about 90–100 ms per file on the fixtures; from the caches it is
  under a millisecond.
- **A wrong or missing tool** (cargo, pyright, ast-grep): the doctor's checks run with the
  server's `PATH`, which is the editor's, not always your shell's.
