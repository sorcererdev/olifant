# 11. Olifant and AI agents

Olifant is designed to be driven by an agent (Claude Code, a Durandal harness, a script) as
much as by a person. This chapter covers what the agent sees, what the measurements say
about how agents use it, and how to write prompts and packs so they use it well.

## What an agent sees: JSON

When stdout is not a terminal, every command prints one line of JSON (`--json` / `--human`
override that). A successful `gen`:

```
$ olifant gen elm/page --set page=settings --set msgs=toggle_dark_mode > out.json; echo $?
0
```

```json
{"applied": true, "conflicts": [], "generator": "elm/page", "now": "2026-09-30T01:03:25.683131Z", "plan_version": 2,
 "steps": [{"gen": "elm/page", "set": {"msgs": "toggle_dark_mode", "page": "settings"}}], "todos": [], "warnings": [],
 "edits": [
  {"id": "e1", "path": "src/Page/Settings.elm", "status": "create", "step": "elm/page"},
  {"id": "e2", "path": "src/Main.elm", "status": "insert", "line": 8, "anchor_used": "after last line matching ^import Page\\.", "step": "elm/page"},
  …
  {"id": "e9", "path": "src/Page/Settings.elm", "status": "insert", "line": 34, "found_by": "compiler", "site": "update", "todo": false, "step": "elm/page › elm/msg[1]"},
  {"id": "e10", "path": "src/Main.elm", "status": "insert", "line": 108, "found_by": "compiler", "site": "view", "todo": false, "step": "elm/page"},
  {"id": "e11", "path": "src/Main.elm", "status": "insert", "line": 59, "found_by": "compiler", "site": "changeRouteTo", "todo": false, "step": "elm/page"}
 ]}
```

(Reformatted here; the real output is one line of 1,577 bytes for 11 edits.) An applied run
leaves out file contents, because the agent can read the files and every byte of output is
re-read on every later turn. `--plan` includes them as `content`.

The fields an agent should act on:

| field | meaning |
|---|---|
| `applied` | whether anything was written |
| `conflicts[]` | `{code, message}`: why nothing was written. Codes: `target_exists`, `insert_target_missing`, `anchor_unresolved`, `complete_failed`, `requires_unmet`, `region_broken`, `unsafe_path`, `target_unusable`, `hole_problem` |
| `todos[]` | `{edit, file, function, pattern}`: a `Debug.todo` branch the agent must replace ([chapter 4](04-compiler-branches-and-todos.md)) |
| `holes[]` | `{name, kind, file, line, status, model, attempts, why, fill}`: a typed hole. `filled` holes need a review; an `unfilled` or `skipped` one is a placeholder (`olifant: NAME: …`) the agent should write itself ([chapter 12](12-holes-and-models.md)) |
| `warnings[]` | the plan applied but may be incomplete (e.g. the project did not compile, so cases could not be checked) |
| `edits[].status` | `create`, `insert`, `overwrite`, `region`, `skipped` (with `reason`), `conflict` |

Errors that stop before a plan exists are `{"error": {…}}`, with a stable `code` and extra
fields for the fix:

```json
{"error":{"code":"invalid_inputs","hint":"--set page=<ident>","invalid":[],"message":"elm/page: invalid inputs: unknown input `pg`; missing `page` (ident)","missing":[{"name":"page","type":"ident"}],"step":"elm/page","unknown":["pg"]}}
{"error":{"available":["alias","elm/msg","elm/page","section"],"code":"generator_not_found","hint":"run `olifant list` to see generators","message":"no generator named `elm/pag`"}}
{"error":{"code":"usage","message":"unknown command `elm/page`; did you mean `olifant gen elm/page --set page=x`?"}}
```

Usage errors are JSON on stdout too when stdout is not a terminal, so an agent never has to
parse stderr.

## Exit codes

| code | meaning | what the agent should do |
|---|---|---|
| 0 | done, including "nothing to do" (a re-run skips everything) | report |
| 1 | a generator, pack or config is broken (`generator_invalid`, `template_failed`, `config_invalid`, `composition_cycle`, …) | stop; a person must fix the pack |
| 2 | the command line is wrong (`usage`, `generator_ambiguous`) | fix the command |
| 3 | no such generator, file, pack, language or region | `olifant list` |
| 4 | inputs missing or invalid; `missing`, `invalid`, `unknown` say which | fix the `--set`s |
| 5 | conflict: nothing was written | read `conflicts`; usually a `requires` (do the other step first) or a file that exists |

Because nothing is written unless the whole plan is sound, retrying after a fix is always
safe: there is no half-applied state to clean up.

## One call per task

Everything is built so that a task is one command:

- `+` and `--steps` chain generators into one atomic plan ([chapter 8](08-composing-generators.md));
- composites like `elm/page --set msgs=…` do a page and all its messages;
- `olifant list` ends with a one-line tip saying exactly that;
- `olifant show` prints example commands, a composite's sub-generators and their inputs, and
  live `one_of` choices: grouped by service for an input whose choices depend on the
  service, and narrowed to one service's by `show --set service=billing`
  ([chapter 6](06-queries-and-choices.md#dependent-choices));
- `olifant list --detail` prints every generator's `show` at once.

## Pack skills: `SKILL.md`

A pack can ship a `SKILL.md`: a short, agent-facing guide in the skill format (YAML front
matter with `name` and `description`, then Markdown). The Elm pack's says what the
conventions are, gives the one-call command with a full example, explains the list syntax,
the exit codes, and what to do with todos. Olifant never reads it. It is for the agent
harness: Claude Code can load it as a skill, and Durandal is meant to inject the active
packs' skills into the agent's context. The measurements below are why.

## What the measurements found

The token experiments are in `~/projects/notes/durandal/experiments/token-ab/` (`run-v3.md` to
`run-v7.md`, with raw rows and analyzers). Agents were Claude Sonnet and Haiku; N=3 per cell
unless noted. **Cumulative input** is the sum of input tokens over every API call in the run.
It is the cost that matters, because each turn re-reads the whole context (~20–30k tokens of
agent prompt before any work), so it grows with the number of turns far more than with the
size of any one output.

### The large task: 3 Elm pages with 8 messages

| setup | Sonnet | Haiku |
|---|---|---|
| hand-written (v2, N=2) | 303k, 65s | 530k, 113s |
| Olifant, one `gen` per page or message (v2, N=2) | 286k, 53s | 349k, 78s (1 of 2 correct) |
| + composition available (v3) | 315k, 37s | 241k, 45s |
| + compact output, examples in `show`, tip in `list` (v4) | 171k, 25s | 148k, 32s |
| discovery only, composite `show` (v5 F) | 171k, 28s | 176k, 36s |
| **pack skill preloaded (v5 E)** | **102k, 14s** | **99k, 22s** |
| marker-free pack, skill preloaded (v6 A-E) | 105k, 14s | 103k, 25s |
| marker-free pack, discovery only (v6 A-F) | 174k, 25s | 151k, 32s |

From v3 on, every run was correct and compiled.

- **Composition alone did not cut tokens** (v3): agents still made several calls, including
  `--plan` dry runs. What did (v4) was making the one-call path obvious: the tip in `list`,
  examples in `show`, and compact applied output. Every v4 agent ran exactly `list`, `show
  elm/page`, `show elm/msg`, one chained `gen`, and reported.
- **With the skill preloaded, discovery disappears** (v5 E): one `gen`, 2–3 tool calls. That
  is about a third of hand-writing for Sonnet and a fifth for Haiku, 2–5× faster, and the
  small model costs the same as the large one.
- **Summaries do not replace a call** (v5 F): even when `show elm/page` printed `elm/msg`'s
  inputs, every agent still ran `show elm/msg`. Agents don't trust that a summary is
  complete. `list --detail` was never used.
- **Removing the marker comments cost nothing** (v6 A): with structural `at` phrases and
  compiler-found branches, the numbers matched v5.

### Todos (v6 B)

One page, in an app with a hand-written `toPath : Route -> String` the generator doesn't know
about. 6 of 6 agents ran the generator, read the one `todos` entry, opened only `Route.elm`,
replaced the `Debug.todo` with `"/settings"`, and checked with `elm make`. Cost: 384k (Sonnet,
11 calls) and 487k (Haiku, 18 calls), but most of that was a prompt artefact: this run's guide
was trimmed to bullets without an example command, so agents first tried `olifant elm/page
…`, then `--help` and `list` (3–5 wasted turns). The todo itself cost about 4 turns. The CLI
now answers `olifant elm/page …` with "did you mean `olifant gen elm/page …`?".

### Writing a generator (v6 C)

Task: write `.olifant/generators/route/generator.toml` (a route with no page: `Route` variant,
parser entry, `changeRouteTo` branch) using `olifant anchors`, `olifant where` and
`elm/page`'s TOML as the example. **6 of 6 wrote a working, marker-free generator that also
generalised** to a fresh fixture and a new name (`contact_us` → `ContactUs`, `/contact-us`).

| | calls | cumulative input | time |
|---|---|---|---|
| Sonnet | 12 | 500k | 66s |
| Haiku | 30 | 930k | 121s |

Every agent probed with `olifant anchors elm` and several `olifant where` calls before writing,
and no invalid phrase reached a generator. Haiku's extra turns went mostly to whitespace: the
newline TOML drops after `"""` (one run debugged it with `sed | od -c`). Authoring costs a
small model about twice what it costs a large one, but it succeeds, and it is paid once per
convention.

| task | Haiku ÷ Sonnet (cumulative input) |
|---|---|
| large task, skill preloaded | 0.98 |
| large task, discovery | 0.87 |
| todo fix | 1.27 |
| write a generator | 1.86 |

### `choices_from` (v7)

(The experiment predates `one_of`; the input it measured is now `one_of = "services"`, and
`show` prints the same choices.)

Rust-htmx, discovery only, task "add a command `cancel_subscription` to this app's service"
with a rule against reading or listing project files. X: the pack as it is, so `show` prints
`--set service=<enum(billing)>`. Y: the same pack with `service` as a plain `ident`.

| | calls | cumulative input | time | `gen` calls | broke the no-listing rule |
|---|---|---|---|---|---|
| X Sonnet | 5 | 165k | 21s | 1, 1, 1 | 0 of 3 |
| X Haiku | 5.7 | 134k | 23s | 1, 1, 1 | 0 of 3 |
| Y Sonnet | 12 | 461k (2.8×) | 103s | 5, 4, 4 | 1 of 3 |
| Y Haiku | 7.7 | 189k (1.4×) | 40s | 2, 2, 1 | 2 of 3 |

All 12 were correct. Without the choice in `show`, Sonnet probed with `--plan` (one agent
tried `app, main, core, billing, api, backend` in turn); Haiku mostly trusted the example
line in `show`, which already said `service=billing`. That leak makes the result
conservative. **The cost of not knowing a valid value is a guess-and-check loop, not an
`ls`**, and enumerated inputs also made agents follow the rules.

### What the numbers do not say

The tasks are small and all in fixture apps; N is 3 (2 for the v2 baselines); one model
family. They measure cost and correctness of a well-specified task, not whether an agent
chooses the right generator for a vague request.

## Advice for prompts and packs

For the agent's prompt:

- **Preload the pack's skill** if the harness can. It is the single biggest saving (v5 E vs F).
- If you cannot, say "run `olifant list`, then `olifant show` for the generators you need,
  then do the whole task in one `olifant gen … + …`". Agents follow the tip, but saying it
  saves turns.
- **Include one complete example command.** A guide without one cost 3–5 wasted turns (v6 B).
- Tell the agent it does not need to read the files the generators write, and that
  `todos` are must-fix: replace each `Debug.todo`, reading only the file named.
- Tell it that exit 5 means nothing was written, so it can fix and re-run.

For generators and packs:

- **Make inputs enumerable.** If the valid values exist in the project, use `one_of`
  (v7: 2.8× cheaper for Sonnet), and let an input's query use the inputs above it, so the
  choices are the ones that make sense for the service (or page, or model) already chosen.
- Write `prompt`s with examples in them (`Page name (e.g. counter, article_list)`), and
  `examples` that are real, complete commands: agents copy them.
- Give `[requires]` a `hint` that is a command.
- Prefer composites (`elm/page --set msgs=…`) for things that always go together; one step is
  one less thing to get wrong.
- Keep `SKILL.md` short, lead with the one-call command, and keep it true: when a generator
  changes, the skill is part of the change.

For agents that write generators: give them `olifant anchors <lang>`, `olifant where`, one
existing generator as a model, and the multi-line `text` rule from chapter 3. They will probe
with `where` on their own.
