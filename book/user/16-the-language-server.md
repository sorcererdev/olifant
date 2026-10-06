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
  editor's `$/cancelRequest` stops a request running in the background (a ⚡ action being
  resolved, a snippet being filled). Either way the work's child processes — cargo, pyright,
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
- **Isolation.** Caches and jobs are per project. A slow check in one project never blocks
  another project's requests.
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
| Doctor | `workspace/executeCommand` `olifant.doctor` (argument: a document's uri) | the checks `olifant doctor --json` prints |
| Progress | `$/progress` | the same words as the CLI's `--progress`: "checking rust…", "asking qwen3-coder:30b to fill body…" |

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

The ids are the same however files are listed, and the same words in every project, so a
Python project's model and a Rust project's can be put side by side. Nothing draws the model yet:
it is the seam a view of the whole system — services and how they call each other, what is
tested, what is red — builds on.

In Neovim:

```
:lua vim.print(require("durandal.lsp").client():request_sync("olifant/model", { textDocument = vim.lsp.util.make_text_document_params() }, 10000).result)
```

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
