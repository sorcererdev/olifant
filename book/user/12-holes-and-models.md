# 12. Holes and models

A template knows everything a convention fixes: the file, the place, the names, the shape.
It cannot know the one small thing that depends on meaning, such as which HTTP status a
refund that already happened deserves, or what a new Elm message should do to the model.
A **typed hole** marks that gap in a template. When you run the generator, a model fills
it in.

This chapter explains how holes work. [Chapter 13](13-tutorial-holes.md) is a step-by-step
tutorial with a local model.

## Why holes

Olifant does the planning. It knows which files a generator touches, where text goes, which
`match` or `case` the compiler says is missing an arm, and in what order to apply all of it.
None of that is left to a model. The model gets the small, local, typed gaps that are left
over: one expression in one arm of one `match`, with the file around it.

That is the bet behind holes: if the model never has to plan, a small model is enough. A
30-billion-parameter coding model running on a desktop Mac fills the holes in this chapter in one
or two seconds (`qwen3-coder:30b` through Ollama, [below](#what-the-measurements-say)). A
frontier model works too, but you don't need one.

Holes also keep AI optional. An unfilled hole is written as a **placeholder** that compiles
(`todo!(…)` in Rust, `Debug.todo …` in Elm). With no model configured, with `--no-fill`, or
when the model fails, you get the same output a generator without holes would give: correct
wiring and a marked `todo` for a person.

## The syntax

```
{= ? name: kind | instruction =}
```

- **`name`** identifies the hole: in the plan, in the request to the model, and in a test's
  recorded fills. It is an identifier (`status`, `update`, `body`).
- **`kind`** is the syntactic category the fill must parse as: `expr`, `block`, `type`. The
  language adapter says which kinds it has ([below](#kinds-and-placeholders)).
- **`instruction`** is prose for the model. It is a template too, but it may only use
  outputs (`{= value =}`), not `if` or `for`. `{= pattern =}` in a `complete` body becomes
  the real pattern.

Here is the hole in `rust-htmx/command-failure`, the pack's real generator:

```toml
# packs/rust-htmx/generators/command-failure/generator.toml (excerpt)
# Every match on the enum gets an arm whose value is a hole (spec §6.4): a
# configured model fills it per site; without one it stays a todo!().
[[edits]]
complete = "{= Failure =}::{= failure|pascal =}"
text     = "{= pattern =} => {= ? status: expr | the value this match returns for {= pattern =} ({= message =}), following the other arms =},"
```

A hole without a kind is refused when the generator loads. `olifant list` marks it
`INVALID`, and `gen` stops with exit 1:

```
$ olifant gen badhole --plan --human
error: …/.olifant/generators/badhole/generator.toml: `edits`: [1] `text`: line 1: a hole is `{= ? name: kind | instruction =}`; write [[edits]] (double brackets: an array of tables, one per edit)
```

(The generator's `text` was `x = {= ? answer | the answer =}`. The "write [[edits]]" part of
the message is a hint that doesn't apply here.)

## Kinds and placeholders

Each language adapter declares its hole kinds under `[holes.KIND]`, with two keys
([chapter 9](09-packs-and-language-adapters.md#hole-kinds-holeskind)):

- `placeholder` is what an unfilled hole is written as. It must compile. It can use
  `{= hole =}` (the hole's name) and `{= instruction =}`.
- `wrap` is a piece of code with `{= fill =}` in it. A fill must parse when it is put
  there.

The kinds in this repository's packs:

| adapter | kind | placeholder | a fill must parse in |
|---|---|---|---|
| Rust (`rust-htmx`) | `expr` | `todo!("olifant: {= hole =}: {= instruction\|rust_string =}")` | `fn __olifant() { let _ = {= fill =}; }` |
| | `block` | the same `todo!` | `fn __olifant() { {= fill =} }` |
| | `type` | `() /* olifant: {= hole =} */` | `type __Olifant = {= fill =};` |
| | `ident` | `{= hole =}` | `struct {= fill =};` |
| | `text` | `{= hole =}` | `const _: &str = "{= fill =}";` |
| Elm (`elm`) | `expr` | `Debug.todo "olifant: {= hole =}: {= instruction\|escaped =}"` | `olifantHole =` / `    {= fill =}` |
| Haskell (`olifant-dev`) | `expr` | `error "olifant: {= hole =}: {= instruction\|escaped =}"` | `olifantHole = {= fill =}` |
| | `text` | `{= hole =}` | `olifantHole = "{= fill =}"` |
| Python (`python-htmx`) | `expr` | `getattr(NotImplemented, "olifant: {= hole =}: {= instruction\|python_string =}")` | `def __olifant():` / `    return {= fill =}` |
| | `stmt`, `block` | `raise NotImplementedError("olifant: {= hole =}: {= instruction\|python_string =}")` | `def __olifant():` / `{= fill =}` |
| | `ident` | `{= hole =}` | `class {= fill =}: ...` |
| | `text` | `{= hole =}` | `__olifant = "{= fill =}"` |

Python's `expr` placeholder is an expression of type `Never`: strict pyright accepts it
wherever any type is wanted, it needs no import, and it raises if it runs
([chapter 9](09-packs-and-language-adapters.md#hole-kinds-holeskind)).

Rust's and Python's `ident` and `text` and Haskell's `text` are for snippets' tab stops
([below](#where-a-holes-value-comes-from-holesname)): a name, and text inside a string. Their
placeholder is the bare name, which compiles where a name or a string's contents stand, but
carries no `olifant:`.

Every model placeholder contains `olifant:` and its hole's name, so you can grep for unfilled
holes. The name is also a rule: an adapter whose `placeholder` doesn't use `{= hole =}` fails to
load, because Olifant finds each hole's placeholder by its text, and the name makes it
unique. The instruction goes through `rust_string`, `python_string` or `escaped`
([chapter 7](07-writing-a-generator.md#filters)), so a quote in it cannot end the string
early.

A kind the adapter doesn't declare is a conflict when you run the generator. Nothing is
written (exit 5):

```
$ olifant gen badkind --plan --human
Plan: badkind (1 edit, 1 conflict): nothing written
  ~ src/Page/Home.elm (+4 lines at line 43)
  ! hole: src/Page/Home.elm: `block` is not a hole kind of the elm adapter (kinds: expr)
```

A hole in a file that no language adapter covers is the same kind of conflict ("holes need
a language adapter for …").

## Where holes may go

A hole can go anywhere a template renders text:

- **Inline `text`** of a `create` or an `insert`. [Chapter 13](13-tutorial-holes.md#step-6-write-your-own-generator-with-a-hole)
  writes a project generator whose one `insert` holds a hole.
- **`.dt` templates**, the same way.
- **`complete` bodies.** This is the main use. The compiler finds every `match` or `case`
  that needs a new arm, and each arm's body can hold a hole. Each one is filled with its own
  site in view: the function it is in, and the other arms.

Hole names must be unique within a plan. When a name repeats, each copy gets its file and
line appended: `name[file:line]`. That happens when one `complete` body is used at two
sites, or when a chain adds the same generator twice. Here is a chain of four
`command-failure` steps, all hitting the same `status()`:

```
$ olifant gen rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=already_refunded --set message="Already refunded" + rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=not_found --set message="No such invoice" + rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=not_paid --set message="Not paid yet" + rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=too_late --set message="Too late to refund" --human
rust-htmx/command-failure + rust-htmx/command-failure + rust-htmx/command-failure + rust-htmx/command-failure (8 edits, 0 conflicts)
  ~ billing/billing-service/src/refund_invoice.rs (+2 lines at line 27)  [rust-htmx/command-failure (step 1)]
  …
  ~ billing/billing-service/src/refund_invoice.rs (+1 line at line 45, compiler: status)  [rust-htmx/command-failure (step 4)]
holes:
  status[billing/billing-service/src/refund_invoice.rs:41] (expr) billing/billing-service/src/refund_invoice.rs:41  filled by qwen3-coder:30b: 409
  status[billing/billing-service/src/refund_invoice.rs:42] (expr) billing/billing-service/src/refund_invoice.rs:42  filled by qwen3-coder:30b: 404
  status[billing/billing-service/src/refund_invoice.rs:43] (expr) billing/billing-service/src/refund_invoice.rs:43  filled by qwen3-coder:30b: 400
  status[billing/billing-service/src/refund_invoice.rs:44] (expr) billing/billing-service/src/refund_invoice.rs:44  filled by qwen3-coder:30b: 400
```

```diff
         match self {
-            RefundInvoiceFailure::Unknown(_) => 500
+            RefundInvoiceFailure::Unknown(_) => 500,
+            RefundInvoiceFailure::AlreadyRefunded => 409,
+            RefundInvoiceFailure::NotFound => 404,
+            RefundInvoiceFailure::NotPaid => 400,
+            RefundInvoiceFailure::TooLate => 400,
         }
```

(This run used a fixture whose last arm had no comma. All four holes went to the model in
one request, and the whole command took 3.6 seconds.)

A hole can also go in a snippet's cursor edit, where it is a tab stop
([below](#where-a-holes-value-comes-from-holesname)).

A hole stays local. Its fill replaces exactly its own placeholder and nothing else. If a
change needs code in several places, such as a new constructor parameter plus its call site,
use inputs, queries and composed generators ([chapter 8](08-composing-generators.md)). A hole
never makes edits elsewhere.

## Where a hole's value comes from: `[holes.NAME]`

So far every hole has been the model's. In a **snippet**, a generator whose one edit goes at
the cursor ([chapter 14](14-editors-and-actions.md#snippets)), the holes are the tab stops an
editor walks you through, and not all of them are a model's to fill. `[holes.NAME]` says where
a hole's value comes from:

```toml
# packs/olifant-dev/generators/spec-case/generator.toml (excerpt)
[[edits]]
cursor = true
text = "it \"{= ? behaviour: text | what it does, in words =}\" $ do\n  {= ? body: expr | the test =}"

[holes.behaviour]
source = "yours"

[holes.body]
source = "derived"
value  = "pending"
```

| `source` | what the hole is | in the text `gen` writes | in the snippet an editor expands |
|---|---|---|---|
| `model` (the default) | a gap the model fills, as in the rest of this chapter | the fill, or the kind's placeholder | a tab stop holding the fill, or `⟨name⟩` before there is one |
| `yours` | a plain tab stop | the hole's name (`behaviour`) | a tab stop holding the name |
| `derived` | a tab stop pre-filled from a template (`value`, over the generator's inputs and derived values) | the rendered value (`pending`) | a tab stop holding the value, to keep or change |

**The model never sees a yours or derived hole.** Only model holes go to the fill phase.
A tab stop is `skipped`, with why it is not filled (with `--no-fill`, the why is `--no-fill`,
as for every hole):

```
holes:
  behaviour (text@yours) src/Olifant/SampleUnitSpec.hs:8  skipped: a tab stop (yours)
  body (expr@derived) src/Olifant/SampleUnitSpec.hs:9  skipped: a tab stop (derived)
```

(from `gen olifant-dev/spec-case --at … --plan`, [chapter 14](14-editors-and-actions.md#the-plans-snippet)).
The kind still has to be one of the adapter's, since that is how the hole is written down,
but its placeholder is not used: the text has the name or the value instead. A snippet can
mix sources: `match-arm`'s one hole, `status`, has no `[holes.status]`, so the model fills
it when the snippet is expanded, and the expansion offers the fill as a tab stop to keep or
type over.

`[holes.NAME]` names a hole in the generator's own templates; one that names no hole there is
a load error (`[holes.mesage]: no hole called mesage in this generator's templates`), and
so are an unknown `source` and a `derived` one without `value`. There is no `choice` source
yet (`` `choice` holes are not built yet: make it an input with one_of = "QUERY" (editors
list one item per choice) ``): a choice is an `enum` input whose values come from a query
([chapter 6](06-queries-and-choices.md#one_of-inputs-whose-values-come-from-the-project)), and
an editor's menu has one item per value. Spec §4.9 plans `choice` tab stops, and filling a
model hole on demand from inside an expanded snippet; neither is built.

## What happens in a `gen`

Filling is the last phase of `gen`, after everything else is planned:

```
render (each hole becomes a marker)
  → positional edits → complete edits (checks see placeholders) → regions
  → name the holes, write their placeholders
  → no conflicts and a model configured?
       → ONE request with every hole in the plan
       → each fill: stays inside its hole → parses as its kind (wrap) → compiles with the plan ([check])
       → failing holes: ask again with the reason (up to `retries` more times)
       → still failing: keep the placeholder, report why
  → write (or, with --plan, don't)
```

### One request per plan

All of a plan's holes go to the model in **one** request, so the four `status` arms above
were decided together and stay consistent with each other. For each hole, the request has:

- its name, kind and file, and its instruction, rendered (`{= pattern =}` is the real
  pattern);
- **the file around it, with the plan applied**: up to 100 lines on each side, with the hole
  shown as `⟦name⟧`. Other holes in that text appear as their placeholders. In a `complete`
  body, this is how the model sees the enclosing function and the sibling arms;
- the generator's inputs, as `name = value` lines (for a chain, every step's inputs).

A short system prompt sets the rules: reply with one tagged block per hole,
`<fill name="NAME">CODE</fill>`, where each CODE is only the hole's code, as plain source with
no escaping, valid as its kind, in the surrounding style, with no markdown.
([Below](#what-is-sent-to-the-model) is a real request.)

### Validation: contained, parses, compiles

A fill is accepted only if it passes three checks:

1. **It stays inside its hole.** A fill whose brackets don't balance (outside string
   literals) would close the code around it, and is refused as "the fill reaches outside
   its hole (unbalanced brackets)". A fill containing `olifant:` is the model copying the
   placeholder back, and is refused as "the fill is the placeholder, not code".
2. **It parses as its kind.** The fill is placed in the kind's `wrap`, and the adapter's
   grammar (through ast-grep) must find no syntax error. `409` is an `expr`, and
   `409; drop(x)` is not.
3. **The plan compiles with it.** Every fill goes into the files, and the adapter's
   `[check]` (`cargo check`, `elm make`) runs over the project as the plan leaves it, the
   same check `complete` edits use ([chapter 4](04-compiler-branches-and-todos.md)). If that
   breaks anything, each fill is checked again on its own, so the one that breaks a file is
   blamed, even when the broken file belongs to another hole. Fills that pass alone but
   break the build together are all refused ("this fill and another do not compile
   together").

A language without a `[check]` validates by parsing alone. If the parser or the check cannot
run at all (no `ast-grep`, no `cargo`), the fill is accepted, and when a check couldn't run
the plan warns: "holes were filled but not compile-checked: …". The point of the checks is
to reject a wrong fill, not to stop a generation because a tool is missing.


**Layout is repaired, not retried.** A fill's lines lose their common indentation before
they are placed (a model may write at the hole's absolute column), and a fill whose first
line sits less deep than all the rest (written as if the first line were at the marker),
and opens no block (it does not end with `:` or an open bracket, a trailing comment aside),
is read with the rest brought up to it when that compiles, or, when the fill as written has
a syntax error, when that leaves fewer findings or no syntax error (each judged with only
that fill in place, so another hole's errors in the file do not count).
A conjure read that way may still fail on a type error: that error is then the reason, and
the text written is the one read level. `--progress` reports the rescue as
`{"event":"reindent","hole":…,"fill":…}`. Measured on `handle-body`: every
failed qwen3-coder:30b attempt was one of these two layouts, and the model repeated it on
retry; with both repaired, every handler was green in one round.

**A model that answers with the whole file is cut to the hole; one that still answers a
file is asked again.** A local model asked for a test's arrangement often answers with the
whole test file: its imports, its helpers, every test, the stub with a body. Every hole of
lines (`block`, `stmt`) is asked first to *answer with the lines that replace the marker
only*, and its fill is then cut by the lines around the hole. Its anchor is the line opening
the block the hole is in (the test's `def` line): a fill holding it once (whitespace aside)
repeats the file, and loses that line, everything before it, and everything after the block's
end; then, holding the line just below the hole (the test's assertion) at its own base level,
never nested, it loses that line and everything after it. A fill that does not hold the anchor,
or holds it twice, is not cut. A test's arrangement and assertion answered alike are cut once,
then split at the first `assert` (with none after the body's first line, the arrangement is
`pass` and the assertion all of it). An import line before the cut that the file lacks is
kept, and joins the file's imports. A Python fill that, once cut, still holds a test's `def`, or
a `def` or `class` the file already defines, at its own top level, is not placed (a local
import or a new helper is code): the attempt fails with "the model answered with more than the
hole: a whole file", and the retry says *answer with the lines that replace the marker only:
no `def` line, no imports, no other tests*. `--progress` reports
`{"event":"cut","hole":…,"dropped":…,"fill":…}` and `{"event":"whole_file","hole":…}`; a
conjure's step log has `fill olifant cut to the hole (N lines dropped)`.

**Verify: a fill must pass a command.** A model hole may name a command its fill must pass,
usually its tests:

```toml
[holes.body]
source = "model"
verify = "tests"
```

`verify = "tests"` runs the language's `[test]` runner (python's is `preset = "pytest"`)
over the tests of the module the hole is in (the `tests_for` template: `close_table.py`'s
are `close_table__test.py`), and tells the model which test failed and where. A command
is the general form: `verify = ["pytest", "-q", "src/bistro/booking/commands/cancel_booking__test.py"]`.

After a fill parses and compiles, Olifant runs the command in a scratch copy of the project
(the language's files and its check's context, with every standing fill in place), as a
check runs. Exit 0 (or no failing test): the fill stands. Otherwise the
last 40 lines of what it printed go back to the model as "the tests failed: …", and it
retries; when the retries run out the hole keeps its placeholder, with that output as the
reason. A command that runs past 120 seconds is stopped ("the tests timed out (120 s)").
`--progress` reports `{"event":"verify","hole":…}`; editors show it as "Testing". Only
model holes take `verify` (a tab stop is yours to write).

**Auto-import, without a model round.** When a fill fails only because names are not
defined (pyright: `"date" is not defined`), and exactly one import line in the project's
files of that language binds the name (`from datetime import date, time` in a model binds
`date`), Olifant adds that import to the fill and checks it again in the same attempt. An
alias binds its alias (`from datetime import date as d` binds `d`, not `date`). Two modules
binding the name, or none, leave it to the model's retry. `--progress` reports it as
`{"event":"auto_import","hole":…,"import":…}`. It needs the compiler support to read the
name from a message (`supportUndefined`; pyright's does, the others don't yet).

### Conjures: the editor's holes

The editor's `conjure` ([chapter 18](18-tdd-in-the-editor.md#conjuring)) conjures through the same
fill phase: a conjure is a model hole made on demand, of kind `block` over a unit's body or a
test's, so it is parsed, checked, auto-imported and retried as any hole is. Two things differ.

- **The stance.** A conjure's request asks for *the least code that makes this test pass and no
  more: do not generalise beyond what the tests ask*. A constant is a good conjure for one test;
  the next test asks for more. A body on screen that is no stub (another `conjure`) goes with the
  request as *the body on screen, rejected by the person*.
- **The verifier is the unit's tests.** A body is verified by its test file (every test there
  must pass). A test's body is verified the other way: the test must fail *at its assertion*, or
  pass (an error in the setup is a failed attempt). Each failure goes back to the model as the
  reason, as a `verify` command's output does; when the retries run out (or, with a decision
  model, when the person stops a paused conjure) the last attempt lands anyway, and the
  notification says what its tests said.

### Retries with the reason

A hole whose fill failed goes back to the model, in a new request holding only the failing
holes. Each one comes with its previous fill and why it failed:

````
Your previous fill was:
```
( { model | count = sqrtInt model.count }, Cmd.none )
```
It failed: src/Page/Home.elm: NAMING ERROR
Fix it.
````

(That is from a real exchange. The model had invented `sqrtInt`. Its second answer,
`floor (sqrt (toFloat model.count))`, compiled. [Chapter 13](13-tutorial-holes.md#step-4-an-elm-intent-and-a-retry)
shows the run.) A fill that compiles is kept. It is not sent again, and later fills are
checked together with it.

A reply Olifant can't use at all (curl failed, the server answered with an error, neither a
`<fill>` block nor JSON could be found in it) counts as a failed attempt for every hole in the request, and is
retried the same way.

`[model] retries` sets how many extra rounds there are (the default is 2, so three attempts
in all). The plan records which attempt succeeded: `filled by qwen3-coder:30b (attempt 2):
…`.

### Holes never block a generation

A hole the model cannot fill keeps its placeholder, and the rest of the plan is written as
usual. This happens when every attempt fails, when the model returns no fill for it, or
when the model cannot be reached at all. The plan says why:

```
$ olifant gen rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=already_refunded --set message="The invoice was already refunded" --human
rust-htmx/command-failure (2 edits, 0 conflicts)
  ~ billing/billing-service/src/refund_invoice.rs (+2 lines at line 27)
  ~ billing/billing-service/src/refund_invoice.rs (+1 line at line 36, compiler: status)
holes:
  status (expr) billing/billing-service/src/refund_invoice.rs:35  unfilled: curl exited 7: curl: (7) Failed to connect to localhost port 11999 after 0 ms: Couldn't connect to server
```

(The backend's `url` pointed at a port nothing listens on. All three attempts failed at
once, and the command took 0.4 seconds.) The exit code is 0, and the diff is the variant
plus a `todo!()` arm. Even then, the output compiles.

The fill phase runs only when the plan has no conflicts. A plan that won't be written
doesn't spend a model call.

## Re-runs call no model

Running a generator again with the same inputs does nothing ([chapter 2](02-running-generators.md#running-it-again)).
With holes, that still holds. When Olifant compares a plan's text with a file, a hole
matches whatever is in its place, whether that is the placeholder, the model's fill, or your
own edit of either. So the arm is "already there":

```
$ olifant gen rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=already_refunded --set message="The invoice was already refunded" --human
rust-htmx/command-failure (1 edit, 0 conflicts)
  = billing/billing-service/src/refund_invoice.rs (skipped: already present)
```

No hole is planned, so no model is called. (Through a logging proxy, the request count
before and after such a re-run was the same.) The same is true over a placeholder: a re-run
leaves a `todo!()` from an earlier `--no-fill` alone. There is no command that fills a
placeholder later yet (the spec plans `olifant fill`). For now, replace the `todo!()`
yourself, or undo the change (`git checkout -- FILE`) and run `gen` again with a model.

## Configuring a model

Add `[model]` to the project's `.olifant/config.toml`. If you want the same models in every
project, add it to `~/.config/olifant/config.toml` instead. When both have a `[model]`, the
project's is used, as a whole:

```toml
[model]
default = "local"        # the backend gen uses when you don't pass --model
retries = 2              # extra rounds for failing holes (default 2)
timeout = 120            # seconds each model call may take (default 120)
ladder  = ["local", "claude"]   # models a steered conjure climbs, local first (default: just `default`)
rounds  = 6              # rounds a conjure may run (default 6)
budget  = 180            # seconds a conjure may run (default 180)

[model.backends.local]
kind  = "openai-compatible"
url   = "http://localhost:11434/v1"    # Ollama; LM Studio and vLLM serve the same API
model = "qwen3-coder:30b"

[model.backends.claude]
kind        = "anthropic"
model       = "claude-haiku-4-5-20251001"
api_key_env = "ANTHROPIC_API_KEY"      # the default; the key is read from this variable
escalate    = "ask"                    # never, ask (default) or auto: may Jev climb to this rung?
```

There are three kinds of backend:

| `kind` | keys | calls |
|---|---|---|
| `openai-compatible` | `url`, `model` | `POST {url}/chat/completions` with the system and user messages and `"temperature": 0` (no JSON mode, [below](#why-tagged-blocks)) |
| `anthropic` | `model`, `api_key_env` (default `ANTHROPIC_API_KEY`) | `POST https://api.anthropic.com/v1/messages` with `max_tokens` 4096. The key is read from the environment when `gen` runs. It is never stored in config and never printed. curl gets it from a private temporary header file, not from its command line |
| `jev` | `model` (default `jev-latest`), `api_key_env` (default `JEV_API_KEY`) | `POST https://api.typesafe.ai/v1/systemone` with typed questions, all of one decision batch in one call. It answers decisions only (`[decide] model`), never holes. The key is handled as for `anthropic`. A 429 or 529 is retried twice (after 0.5 s, then 2 s); each call writes `decide jev N tokens M ms` to the step log |

All are called with `curl`, which must be on your `PATH` ("curl is not installed"
otherwise). Each call may take `timeout` seconds (`curl --max-time`). A call that runs out
of time is a failed attempt. Raise `timeout` for a slow model, or for the first request
after a local server starts and has to load the model.

Choosing a backend for one run:

- **`--model NAME`** uses `[model.backends.NAME]` for this run. A name that doesn't exist is
  a usage error (exit 2) that lists the names that do:

  ```
  $ olifant gen rust-htmx/command-failure … --model opus --plan --human
  error: --model opus: no such backend under [model.backends] (backends: claude, local, small)
  ```

- **`--no-fill`** leaves every hole as its placeholder, and the plan reports them as
  `skipped: --no-fill`.
- **No `[model]`** (or no `default`, and no `--model`) means no model is called. The holes
  are reported as `unfilled: no model is configured`.

Like `--plan`, `--model` and `--no-fill` apply to the whole run, wherever they appear in a
`+` chain.

### The ladder (conjures with a decision model)

The ladder and the budget apply only to the editor's `conjure`, and only with `[decide] model`
set. Without it, `retries` rules as before. `ladder` lists backends, local first; `rounds` and
`budget` cap how long a conjure runs. Each `openai-compatible` or `anthropic` backend takes
`escalate`, which says what happens when Jev wants to climb to it:

| `escalate` | meaning |
|---|---|
| `never` | the rung is listed, but Jev may not climb to it (you still can) |
| `ask` (default) | Jev's wish to climb pauses the conjure and asks you |
| `auto` | Jev climbs |

A `jev` backend cannot be a rung: it answers decisions, not prompts. The default `ask` keeps
code from leaving your machine unless the config says it may.

### What is sent to the model

**A request contains your code.** For every hole, the request holds up to 201 lines of the
file the hole is in, as the plan would leave it, along with the generator's inputs, and
the whole of each file the generator's `context` globs match (shown as "Related file …";
a rule fix's `context` works the same way). When a hole's language has `imports`
([chapter 9](09-packs-and-language-adapters.md)), the message says once, with an example,
that a fill may start with import lines, and a retry adds "If it failed on a name that is
not defined, add that name's import line first." Measured on qwen3-coder:30b filling
`python-htmx/repository`'s six codec holes: a one-line note per hole got no imports in
three rounds (every decode failed on `date`); the rule with an example, plus the retry
line, got all six in two rounds. With
the `anthropic` backend, or any `openai-compatible` URL that isn't on your machine, that
text leaves your machine. If your code must not, use a local model, or `--no-fill`. Here is
the start of a real request body to Ollama:

```json
{"messages": [{"content": "You fill typed holes in generated code. …", "role": "system"}, {"content": "Generator inputs:\n  command = refund_invoice\n…", "role": "user"}], "model": "qwen3-coder:30b", "temperature": 0}
```

(captured for `rust-htmx/failure-arms` by pointing a backend's `url` at a local server that
logs what it receives). The system message, verbatim:

```
You fill typed holes in generated code. Each hole is marked ⟦name⟧ in its file.
Reply with one block per hole, nothing else:
<fill name="NAME">
CODE
</fill>
CODE is plain source (no escaping) and replaces the marker exactly: only that code, valid as the hole's kind in the file's language,
matching the surrounding style. No surrounding code, no explanations, no markdown.
```

And the user message, for `elm/msg --set page=home --set msg=stamp --set intent="set count
to the current time in milliseconds"` (the file is the fixture's `Home.elm` with
`{ count : Int }`, so it fits whole):

````
Generator inputs:
  intent = set count to the current time in milliseconds
  msg = stamp
  page = home

Hole `update` (kind: expr) in src/Page/Home.elm
Instruction: the update branch for Stamp: set count to the current time in milliseconds; a ( Model, Cmd Msg )
The file around it:
```
module Page.Home exposing (Model, Msg, init, update, view)

import Html exposing (Html, h1, text)
…
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Stamp ->
            ⟦update⟧
…
```

Reply with <fill name="update">…</fill>.
````

(The last line names every hole in the request, one block each.) The model's fill was
`( { model | count = Time.now |> Date.toMillis }, Cmd.none )`. It doesn't compile (there is no `Time` or `Date` import, and `Time.now` is a `Task`), so it
was retried twice and the hole stayed a `Debug.todo`. That is the right outcome: the current
time needs a command and a second message, which is more than one expression can do. See
[writing good instructions](#writing-good-instructions).

A good reply to the `failure-arms` request above is:

```
<fill name="status">
409
</fill>
```

The reply is read leniently. A `<think>…</think>` block before it is dropped. `<fill>`
blocks are found anywhere in the reply, so prose around them doesn't matter, and the one
newline after `<fill name="…">` and the one before `</fill>` belong to the block, not the
code. Inside a block the code is taken as it is: quotes, backslashes and braces need no
escaping.

If the reply has no `<fill>` blocks, it is read as JSON, the format Olifant asked for before:
`{"fills": {NAME: CODE}}`, or fills at the top level without `"fills"`; JSON inside a
markdown code fence, or between the first `{` and the last `}` of a reply with prose around
it, is found; a fill that is itself fenced is unfenced; and a number or boolean counts as
code (`"status": 409` is the fill `409`).

### Why tagged blocks

Until token experiment v12, Olifant asked for JSON and turned on the server's JSON mode
(`response_format`). v12 filled function-body holes in six tasks with four models (144 runs),
asking for JSON in one arm and tagged blocks in the other, and ran hidden tests on the
results. For `qwen3-coder:30b`, Haiku and Sonnet the two formats passed the same number of
tests. For the small thinking model `qwen3:8b`, JSON mode failed 18 of 33 calls (empty
replies after spending the 4,096-token cap thinking, and server errors in JSON mode), and
tagged blocks failed none of 30. Code inside JSON also has to be escaped, which is one more
thing a small model can get wrong. So requests now ask for tagged blocks with no JSON mode,
and the JSON reader stays as a fallback.

## The plan's holes

Every plan has a `holes` array (the human output lists the holes under the edits). Each
entry:

| field | meaning |
|---|---|
| `name` | the hole's name, with `[file:line]` when the name repeats |
| `kind` | its kind |
| `source` | where its value comes from: `model`, or `yours` or `derived` for a snippet's tab stop ([above](#where-a-holes-value-comes-from-holesname)) |
| `file`, `line` | where its placeholder or fill is |
| `placeholder` | the placeholder's text, as the adapter rendered it, whatever the status: what an editor looks for to show the hole (Durandal draws it as `⟨name⟩`, [chapter 14](14-editors-and-actions.md#ghost-text-and-follow-ups)) |
| `status` | `filled`, `unfilled` (it has its placeholder because something failed or no model is configured) or `skipped` (`--no-fill`, or a tab stop) |
| `model` | the model that filled it (`null` unless filled) |
| `attempts` | which attempt succeeded (`null` unless filled) |
| `why` | why it is `unfilled` or `skipped` (`a tab stop (yours)`) |
| `fill` | the code that was filled in (`null` unless filled) |

From `gen … --plan --json` with a model:

```json
"holes": [{"attempts": 1, "file": "billing/billing-service/src/refund_invoice.rs", "fill": "409", "kind": "expr", "line": 35, "model": "qwen3-coder:30b", "name": "status", "placeholder": "todo!(\"olifant: status: the value this match returns for RefundInvoiceFailure::AlreadyRefunded, following the other arms\")", "source": "model", "status": "filled", "why": null}]
```

and with the `anthropic` backend and no key in the environment:

```json
"holes": [{"attempts": null, "file": "billing/billing-service/src/refund_invoice.rs", "fill": null, "kind": "expr", "line": 35, "model": null, "name": "status", "placeholder": "todo!(\"olifant: status: …\")", "source": "model", "status": "unfilled", "why": "ANTHROPIC_API_KEY is not set"}]
```

The human form is one line per hole, with the fill after a colon (a fill of several lines is
joined with ` ⏎ `):

```
holes:
  update (expr) src/Page/Home.elm:35  filled by qwen3-coder:30b (attempt 2): ({ model | count = floor (sqrt (toFloat model.count)) }, Cmd.none)
```

With `--plan`, each edit's text (and its `content` in JSON) shows the fill where the
placeholder would be:

```
--- billing/billing-service/src/refund_invoice.rs
RefundInvoiceFailure::AlreadyRefunded => 409,
```

**`--plan` calls the model.** The fill phase runs, so `--plan` tells you what would be
filled. Then `gen` without `--plan` asks again. Models are not fully repeatable, even at
temperature 0 ([below](#limits)), so the applied fill can differ from the one the plan
showed. Read the diff after applying, and pass `--no-fill` with `--plan` when you only want
to see the edits.

## Writing good instructions

The instruction is all the model knows about what you meant. The rest of its context is
code. What worked in the runs behind this chapter:

- **Say what the value is, not just what to do.** `command-failure`'s instruction says "the
  value this match returns for `{= pattern =}` (`{= message =}`)". With the message and the
  sibling arm `Unknown(_) => 500` in view, `qwen3-coder:30b` answered 409, 404, 400 and 400
  (409 and 400 in the pack's two test cases).
- **Name the type when the file doesn't show it.** `elm/msg`'s instruction ends with "a
  ( Model, Cmd Msg )".
- **Ask for one concrete form.** A project generator whose hole asked for "a function of
  type Int -> Int that keeps a count between 0 and 99; a lambda or a composition" was
  filled in 11 of 24 runs. Most failures answered `Basics.max 0 (Basics.min 99)`, which has
  the wrong type, and gave the same answer on both retries. Asking instead for "a lambda of
  one argument, n, of type Int -> Int, that keeps a count between 0 and 99" gave
  `clamp 0 99` in 5 runs out of 5
  ([chapter 13](13-tutorial-holes.md#step-6-write-your-own-generator-with-a-hole)).
- **Watch for wording that stalls the model.** The same instruction written as "a lambda
  (`\n -> …`) of type …", with a literal backslash, ran every call until the timeout, in
  all four runs tried; the version without the backslash answered in under a second. The
  cause isn't known. If a hole times out every time, try rewording its instruction.
- **Keep it to one expression's worth.** "set count to the current time in milliseconds"
  needs `Task.perform` and a second message: an edit to the file, not a hole's value. No
  number of retries fills it. When a fill would need new imports, new messages or new
  functions, the generator should create those itself (inputs, more edits), and the hole
  should be what's left.
- **Pass meaning through inputs.** `elm/msg` has an optional `intent` input, and the hole
  is only there when you give one:

  ```toml
  text = """
  {= pattern =} ->
      {= if intent =}{= ? update: expr | the update branch for {= pattern =}: {= intent =}; a ( Model, Cmd Msg ) =}{= else =}( model, Cmd.none ){= end =}"""
  ```

  Without `--set intent=…`, the branch is `( model, Cmd.none )` as before, and no model is
  asked anything.

## Limits

- **Compile errors are attributed per file.** A check reports broken files, not lines, so
  when the fills together break the build, each fill is checked again on its own. The
  culprit is found, at the cost of one more compile per hole in that round.
- **Elm's retry message is short.** For Elm the reason passed back is the error's title
  (`NAMING ERROR`, `TYPE MISMATCH`), not the compiler's explanation. A model that doesn't
  already know what it got wrong tends to repeat the same answer.
- **Branch-body holes can't use a constructor's payload.** The pattern is the compiler's,
  with `_` for every field:

  ```
  $ olifant gen elm/msg --set page=home --set msg=increment_by --set arg=Int --set intent="add the payload to count" --no-fill --human
  …
  +        IncrementBy _ ->
  +            Debug.todo "olifant: update: the update branch for IncrementBy _: add the payload to count; a ( Model, Cmd Msg )"
  ```

  The payload has no name, so no fill can use it.
- **Fills vary between runs.** Requests use temperature 0, but the same request gave
  different answers on different runs: 11 filled, 12 unfilled and 1 timed out, out of 24 identical
  runs of the example above. Record the fills you want to keep in tests ([below](#testing)), and
  review the diff.
- **`--plan` shows repeated-name holes as placeholders.** When a hole's name repeats
  (`status[file:line]`), the `holes` lines show each fill, but the edits' text in `--plan`
  still shows the placeholder, with the bare name (`todo!("olifant: status: …")`). The files
  are written with the fills. This is a bug. The same happens to every model hole in a
  snippet's edit (`gen --at`, [chapter 14](14-editors-and-actions.md#rough-edges-of-snippets)),
  repeated or not: its `--plan` text has `todo!("olifant: status: …")` where `files`, the
  `snippet` and `holes` have `409`.
- **A slow model holds up `gen`.** Each call may take up to `timeout` seconds, and there are
  up to three. A busy local server makes `gen` wait, and calls that time out leave their
  holes unfilled (`curl: (28) Operation timed out …`). That happened in several runs behind
  this chapter, while the same Ollama was serving another program and switching between
  two models.
- **Holes are filled only by `gen`.** No command fills the placeholders a `--no-fill` run
  left behind yet (see [re-runs](#re-runs-call-no-model)).

## What the measurements say

From the runs behind this chapter and [chapter 13](13-tutorial-holes.md), on one Mac mini
(64 GB) with Ollama 0.35.0, the model already loaded:

| generator | model | fill | attempts | `gen` wall time |
|---|---|---|---|---|
| `command-failure`, `already_refunded` (`--plan`) | `qwen3-coder:30b` | `409` | 1 | 0.5 s |
| `command-failure` ×4 (one request) | `qwen3-coder:30b` | `409`, `404`, `400`, `400` | 1 each | 3.6 s |
| `command-failure` ×4 (one request) | `qwen3:8b` | `409`, `404`, `400`, `410` | 1 each | 50.4 s |
| `elm/msg`, "reset count to 0" | `qwen3-coder:30b` | `( { count = 0 }, Cmd.none )` | 1 | 1.5 s |
| `elm/msg`, "set count to the integer square root of count" | `qwen3-coder:30b` | `({ model \| count = floor (sqrt (toFloat model.count)) }, Cmd.none)` | 2 | 2.4 s |
| `elm/msg`, "set count to the current time in milliseconds" | `qwen3-coder:30b` | none (unfilled: `NAMING ERROR`) | 3 tried | 4.1 s |

The wall time includes Olifant's own work and every `cargo check` or `elm make`. Through a
logging proxy, the model's part of a `qwen3-coder:30b` request was 0.6 to 2.7 seconds.
`qwen3:8b` is a thinking model and much slower here, but it filled the same Rust holes on the
first attempt. Earlier runs of the same commands gave the same fills, and none of these
needed a retry except the square root.

The wall times above are from runs where the server was answering promptly. In other runs,
when the same Ollama was also serving another program with a second model, calls timed out
and left their holes unfilled.

## Testing

A pack test case with holes has a `fills.toml`. The test replays it as the model, so the
test never calls a real one:

```toml
# packs/rust-htmx/generators/command-failure/tests/already-refunded/fills.toml
# Recorded from qwen3-coder:30b via Ollama, 2026-10-02 (first attempt).
[fills]
status = "409"
```

The keys are hole names, as the plan reports them. Each recorded fill still goes through
parse and compile validation, so a recording that stops compiling fails the case. A case
whose generator asks for a hole that isn't in `fills.toml` also fails ("no recorded fill
for `status` in fills.toml").

A case with a `fills.toml` is also run a second time with no model. Its output, every hole
left as a placeholder, must compile: AI stays optional. [Chapter 10](10-testing-generators.md#holes-fillstoml-and-placeholder-output)
covers both checks.
