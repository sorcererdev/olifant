# 13. Tutorial: holes with a local model

This tutorial runs typed holes ([chapter 12](12-holes-and-models.md)) end to end, on your
own machine, with a free local model. You will:

1. install Ollama and a coding model;
2. point a sandbox project at it;
3. run a real generator, see its plan, apply it, and check that it compiles;
4. give an Elm message an intent, and watch a fill fail and get retried;
5. run without a model, and see that the output still compiles;
6. write your own generator with a hole;
7. test it with a recorded fill, so the test needs no model;
8. configure the Anthropic backend.

Every command and output below is from a real run on a Mac (Apple silicon) with Ollama
0.35.0 and `qwen3-coder:30b`. Your model may answer differently; that is part of what you
will see.

## Step 1: install and serve Ollama

[Ollama](https://ollama.com) runs open models locally and serves them over an
OpenAI-compatible API, which is one of Olifant's two backend kinds. On a Mac:

```sh
brew install ollama
brew services start ollama        # or run `ollama serve` in a terminal of its own
ollama pull qwen3-coder:30b       # about 18 GB
```

`qwen3-coder:30b` is a mixture-of-experts coding model: 30 billion parameters, but only a
few billion active per token, so it answers quickly once loaded. Loaded, it took 22 GB here
(`ollama ps`). With less memory, try a smaller model such as `qwen3:8b` (step 3 compares
them).

Check that the server is up and has the model:

```
$ ollama list
NAME               ID              SIZE      MODIFIED
qwen3:8b           500a1f067a9f    5.2 GB    2 days ago
qwen3-coder:30b    06c1097efce0    18 GB     2 days ago
$ curl -s localhost:11434/v1/models
{"object":"list","data":[{"id":"qwen3:8b","object":"model","created":1790817122,"owned_by":"library"},{"id":"qwen3-coder:30b","object":"model","created":1790816339,"owned_by":"library"}]}
```

The `/v1/models` answer is the API Olifant will call. LM Studio and vLLM serve the same API,
so any of them works the same way from here on; only the `url` and `model` change.

## Step 2: a sandbox, and `[model]`

Make a throwaway project from the `rust-htmx` pack's fixture app
([chapter 10](10-testing-generators.md#trying-a-pack-by-hand-scriptstry-packsh)). It is a
git repo, so every step's changes show up in `git diff`:

```sh
scripts/try-pack.sh rust-htmx /tmp/olifant-holes && cd /tmp/olifant-holes
```

Add a model to its `.olifant/config.toml`, below the `[packs]` line the script wrote:

```toml
[model]
default = "local"

[model.backends.local]
kind  = "openai-compatible"
url   = "http://localhost:11434/v1"
model = "qwen3-coder:30b"
```

`local` is a name you choose. `default` makes it the model every `gen` uses; without
`default`, holes are filled only when you pass `--model local`. Commit it, so `git diff`
shows only what the generators change:

```sh
git commit -qam "model"
```

The first request after Ollama starts has to load the model into memory, which can take a
minute or two. If that first `gen` reports a timeout, run it again, or raise `[model]
timeout` (seconds, default 120).

## Step 3: a generator with a hole

`rust-htmx/command-failure` adds a failure to a command's error enum. The compiler then
finds every `match` on that enum, and each new arm's value is a hole:

```toml
text = "{= pattern =} => {= ? status: expr | the value this match returns for {= pattern =} ({= message =}), following the other arms =},"
```

In the fixture, `refund_invoice.rs` maps each failure to an HTTP status:

```rust
impl RefundInvoiceFailure {
    pub fn status(&self) -> u16 {
        match self {
            RefundInvoiceFailure::Unknown(_) => 500,
        }
    }
}
```

**Plan first.** `--plan` shows everything the run would do, fills included, and writes
nothing:

```
$ olifant gen rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=already_refunded --set message="The invoice was already refunded" --plan --human
Plan: rust-htmx/command-failure (2 edits, 0 conflicts)
  ~ billing/billing-service/src/refund_invoice.rs (+2 lines at line 27)
  ~ billing/billing-service/src/refund_invoice.rs (+1 line at line 36, compiler: status)
holes:
  status (expr) billing/billing-service/src/refund_invoice.rs:35  filled by qwen3-coder:30b: 409

--- billing/billing-service/src/refund_invoice.rs
#[error("The invoice was already refunded")]
AlreadyRefunded,

--- billing/billing-service/src/refund_invoice.rs
RefundInvoiceFailure::AlreadyRefunded => 409,
```

Read it from the top:

- Edit 1 inserts the variant at the end of the enum (an `at` phrase, [chapter 3](03-anchors.md)).
- Edit 2 is the arm `cargo check` asked for in `status()` (`compiler: status`,
  [chapter 4](04-compiler-branches-and-todos.md)).
- `holes:` lists the one hole, `status`, at line 35, filled by the model with `409`
  (Conflict), on the first attempt. Before that, `409` was checked: it parses as a Rust
  expression, and the whole workspace compiles with it.

That plan took 0.5 seconds with the model already loaded, including the model call and the
`cargo check` runs that found the arm and checked the fill.

**Apply.** The same command without `--plan`:

```
$ olifant gen rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=already_refunded --set message="The invoice was already refunded" --human
rust-htmx/command-failure (2 edits, 0 conflicts)
  ~ billing/billing-service/src/refund_invoice.rs (+2 lines at line 27)
  ~ billing/billing-service/src/refund_invoice.rs (+1 line at line 36, compiler: status)
holes:
  status (expr) billing/billing-service/src/refund_invoice.rs:35  filled by qwen3-coder:30b: 409
```

This is a second request to the model. It answered `409` again, but it doesn't have to, so
review what was written:

```
$ git add -A && git diff --cached
diff --git a/billing/billing-service/src/refund_invoice.rs b/billing/billing-service/src/refund_invoice.rs
index d79a253..3daf33f 100644
--- a/billing/billing-service/src/refund_invoice.rs
+++ b/billing/billing-service/src/refund_invoice.rs
@@ -24,12 +24,15 @@ impl RefundInvoice {
 pub enum RefundInvoiceFailure {
     #[error("Something went wrong")]
     Unknown(String),
+    #[error("The invoice was already refunded")]
+    AlreadyRefunded,
 }
 
 impl RefundInvoiceFailure {
     pub fn status(&self) -> u16 {
         match self {
             RefundInvoiceFailure::Unknown(_) => 500,
+            RefundInvoiceFailure::AlreadyRefunded => 409,
         }
     }
 }
```

```
$ cargo check --workspace
…
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 3.62s
```

**Run it again.** Nothing happens, and the model isn't asked:

```
$ olifant gen rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=already_refunded --set message="The invoice was already refunded" --human
rust-htmx/command-failure (1 edit, 0 conflicts)
  = billing/billing-service/src/refund_invoice.rs (skipped: already present)
```

The arm's planned text is `RefundInvoiceFailure::AlreadyRefunded => <hole>,`, and a hole
matches whatever is in its place, so the arm counts as already there.

**Several holes, one request.** A chain of four failures ([chapter 8](08-composing-generators.md))
plans four arms with four holes. They share the name `status`, so each gets its file and
line, and they go to the model together:

```
$ git reset -q --hard
$ olifant gen rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=already_refunded --set message="Already refunded" + rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=not_found --set message="No such invoice" + rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=not_paid --set message="Not paid yet" + rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=too_late --set message="Too late to refund" --human
…
holes:
  status[billing/billing-service/src/refund_invoice.rs:41] (expr) billing/billing-service/src/refund_invoice.rs:41  filled by qwen3-coder:30b: 409
  status[billing/billing-service/src/refund_invoice.rs:42] (expr) billing/billing-service/src/refund_invoice.rs:42  filled by qwen3-coder:30b: 404
  status[billing/billing-service/src/refund_invoice.rs:43] (expr) billing/billing-service/src/refund_invoice.rs:43  filled by qwen3-coder:30b: 400
  status[billing/billing-service/src/refund_invoice.rs:44] (expr) billing/billing-service/src/refund_invoice.rs:44  filled by qwen3-coder:30b: 400
```

That took 3.6 seconds. With a second backend, `[model.backends.small]` with
`model = "qwen3:8b"`, the same chain with `--model small` gave 409, 404, 400 and 410 in 50.4
seconds: a smaller model that thinks before it answers.

## Step 4: an Elm intent, and a retry

`elm/msg` adds a message to an Elm page. Its update branch is `( model, Cmd.none )` unless
you give it an `intent`; then the branch is a hole, and the intent goes into its
instruction:

```toml
{= if intent =}{= ? update: expr | the update branch for {= pattern =}: {= intent =}; a ( Model, Cmd Msg ) =}{= else =}( model, Cmd.none ){= end =}
```

Make an Elm sandbox the same way (`scripts/try-pack.sh elm /tmp/olifant-elm-holes`), add the
same `[model]`, and give `src/Page/Home.elm` a model worth updating: change its `Model` to
`{ count : Int }` and its `init` to `( { count = 0 }, Cmd.none )`. Commit both.

**A fill on the first attempt:**

```
$ olifant gen elm/msg --set page=home --set msg=reset --set intent="reset count to 0" --human
elm/msg (2 edits, 0 conflicts)
  ~ src/Page/Home.elm (+1 line at line 25)
  ~ src/Page/Home.elm (+2 lines at line 34, compiler: update)
holes:
  update (expr) src/Page/Home.elm:35  filled by qwen3-coder:30b: ( { count = 0 }, Cmd.none )
```

```diff
+    | Reset
…
+        Reset ->
+            ( { count = 0 }, Cmd.none )
```

`elm make src/Main.elm` compiles it (1.5 seconds for the whole `gen`).

**A retry.** Reset the sandbox (`git reset --hard`) and ask for something the model gets
wrong the first time:

```
$ olifant gen elm/msg --set page=home --set msg=isqrt --set intent="set count to the integer square root of count" --human
elm/msg (2 edits, 0 conflicts)
  ~ src/Page/Home.elm (+1 line at line 25)
  ~ src/Page/Home.elm (+2 lines at line 34, compiler: update)
holes:
  update (expr) src/Page/Home.elm:35  filled by qwen3-coder:30b (attempt 2): ({ model | count = floor (sqrt (toFloat model.count)) }, Cmd.none)
```

`(attempt 2)` means the first fill was rejected. An earlier run of the same command went
through a logging proxy between Olifant and Ollama, and also succeeded on attempt 2. Its
exchange was:

1. Olifant sent one request: the inputs, the hole's name, kind and instruction, and
   `Home.elm` with `⟦update⟧` where the branch's value goes.
2. The model answered `( { model | count = sqrtInt model.count }, Cmd.none )`. That parses,
   but `elm make` fails: Elm has no `sqrtInt`.
3. Olifant sent a second request for that hole alone, with "Your previous fill was: … It
   failed: src/Page/Home.elm: NAMING ERROR. Fix it."
4. The model answered `( { model | count = floor (sqrt (toFloat model.count)) }, Cmd.none )`,
   which compiles.

The whole `gen` took 2.4 seconds.

**A hole that cannot be filled.** Some intents don't fit in one expression:

```
$ olifant gen elm/msg --set page=home --set msg=stamp --set intent="set count to the current time in milliseconds" --human
elm/msg (2 edits, 0 conflicts)
  ~ src/Page/Home.elm (+1 line at line 25)
  ~ src/Page/Home.elm (+2 lines at line 34, compiler: update)
holes:
  update (expr) src/Page/Home.elm:35  unfilled: src/Page/Home.elm: NAMING ERROR; src/Page/Home.elm: NAMING ERROR
```

```diff
+        Stamp ->
+            Debug.todo "olifant: update: the update branch for Stamp: set count to the current time in milliseconds; a ( Model, Cmd Msg )"
```

Reading the time in Elm takes a command (`Task.perform Tick Time.now`) and a second message
to receive the result. In a logged run of the same command, the model tried
`Time.now |> Date.toMillis` and then `Time.posixToMillis (Time.now)` twice. Neither compiles,
and after three attempts (4.1 seconds for this run) the hole kept its placeholder. The message was still added and the file compiles. This is
the boundary holes are designed around: a hole is one local value, and a change that needs
new messages or imports belongs to the generator ([chapter 12](12-holes-and-models.md#writing-good-instructions)).

## Step 5: no model at all

`--no-fill` skips the fill phase. So does a project with no `[model]`. In the Rust sandbox:

```
$ git reset -q --hard
$ olifant gen rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=already_refunded --set message="The invoice was already refunded" --no-fill --human
rust-htmx/command-failure (2 edits, 0 conflicts)
  ~ billing/billing-service/src/refund_invoice.rs (+2 lines at line 27)
  ~ billing/billing-service/src/refund_invoice.rs (+1 line at line 36, compiler: status)
holes:
  status (expr) billing/billing-service/src/refund_invoice.rs:35  skipped: --no-fill
```

```diff
+    #[error("The invoice was already refunded")]
+    AlreadyRefunded,
…
+            RefundInvoiceFailure::AlreadyRefunded => todo!("olifant: status: the value this match returns for RefundInvoiceFailure::AlreadyRefunded (The invoice was already refunded), following the other arms"),
```

The placeholder is the Rust adapter's `[holes.expr]`: a `todo!()` naming the hole and
holding its instruction. `cargo check --workspace` finishes with no errors, because
`todo!()` has every type. The Elm equivalent is a `Debug.todo`:

```
$ olifant gen elm/msg --set page=home --set msg=reset --set intent="reset count to 0" --no-fill --human
…
holes:
  update (expr) src/Page/Home.elm:35  skipped: --no-fill
```

```diff
+        Reset ->
+            Debug.todo "olifant: update: the update branch for Reset: reset count to 0; a ( Model, Cmd Msg )"
```

and `elm make src/Main.elm` prints `Success! Compiled 2 modules.` So a generator with holes
works with no AI at all: the wiring is complete, and the decisions are marked
`olifant: …` for a person. `grep -rn 'olifant:' src` finds them.

A re-run doesn't fill them later; it sees the arm as already there:

```
$ olifant gen rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=already_refunded --set message="The invoice was already refunded" --human
rust-htmx/command-failure (1 edit, 0 conflicts)
  = billing/billing-service/src/refund_invoice.rs (skipped: already present)
```

To have the model fill it after all, undo the change (`git checkout -- .`) and run `gen`
with the model.

## Step 6: write your own generator with a hole

Back in the Elm sandbox, write a project generator that adds a helper function to a page.
The function's signature is fixed by the inputs, and its body is a hole:

```toml
# .olifant/generators/helper/generator.toml
description = "A top-level helper function in a page module, its body written by the model"
language = "elm"
version = 1
examples = [
  "olifant gen helper --set page=home --set name=clamp_count --set type=\"Int -> Int\" --set does=\"keeps a count between 0 and 99\"",
]

[inputs.page]
type   = "ident"
prompt = "The page module (home for src/Page/Home.elm)"

[inputs.name]
type   = "ident"
prompt = "The function's name"

[inputs.type]
type   = "text"
prompt = "Its type annotation (Int -> Int)"

[inputs.does]
type   = "text"
prompt = "What it does, in a few words"

[[edits]]
insert = "src/Page/{= page|pascal =}.elm"
at     = "end of file"
text   = """

{= name|camel =} : {= type =}
{= name|camel =} =
    {= ? body: expr | a function of type {= type =} that {= does =}; a lambda or a composition, using only elm/core =}
"""
```

The hole is an `expr`, the Elm adapter's one kind. The instruction repeats the type, because
the model sees the file, not the inputs' meaning, and asks for a form that fits the
signature on the line above.

Plan it:

```
$ olifant gen helper --set page=home --set name=clamp_count --set type="Int -> Int" --set does="keeps a count between 0 and 99" --plan --human
Plan: helper (1 edit, 0 conflicts)
  ~ src/Page/Home.elm (+4 lines at line 43)
holes:
  body (expr) src/Page/Home.elm:46  filled by qwen3-coder:30b: Basics.max 0 >> Basics.min 99

--- src/Page/Home.elm

clampCount : Int -> Int
clampCount =
    Basics.max 0 >> Basics.min 99
```

That works: `Basics.max 0 >> Basics.min 99` has type `Int -> Int`. Run the same plan again,
though:

```
$ olifant gen helper --set page=home --set name=clamp_count --set type="Int -> Int" --set does="keeps a count between 0 and 99" --plan --human
Plan: helper (1 edit, 0 conflicts)
  ~ src/Page/Home.elm (+4 lines at line 43)
holes:
  body (expr) src/Page/Home.elm:46  unfilled: src/Page/Home.elm: TYPE MISMATCH

--- src/Page/Home.elm

clampCount : Int -> Int
clampCount =
    Debug.todo "olifant: body: a function of type Int -> Int that keeps a count between 0 and 99; a lambda or a composition, using only elm/core"
```

Same request, different answer. Through a logging proxy, the failing runs answered
`Basics.max 0 (Basics.min 99)`, which applies `max 0` to a function, and gave the same answer
on both retries, since all Elm's report says is `TYPE MISMATCH`. Over 24 runs of this
instruction (across three builds of Olifant made while this chapter was written), 11 filled,
12 stayed unfilled and 1 timed out. This instruction doesn't steer the model enough.

**Fix the instruction, not the output.** Change it to ask for one concrete form:

```toml
    {= ? body: expr | a lambda of one argument, n, of type {= type =}, that {= does =}, using only elm/core =}
```

Five runs in a row then gave the same fill, in 0.6 to 0.9 seconds each:

```
$ olifant gen helper --set page=home --set name=clamp_count --set type="Int -> Int" --set does="keeps a count between 0 and 99" --human
helper (1 edit, 0 conflicts)
  ~ src/Page/Home.elm (+4 lines at line 43)
holes:
  body (expr) src/Page/Home.elm:46  filled by qwen3-coder:30b: clamp 0 99
```

```diff
@@ -40,3 +40,7 @@ view _ =
     { title = "Home"
     , body = [ h1 [] [ text "Home" ] ]
     }
+
+clampCount : Int -> Int
+clampCount =
+    clamp 0 99
```

It isn't the lambda the instruction asked for, but it is the best answer (`clamp` is in
`elm/core`'s `Basics`), it compiles (`elm make src/Main.elm`: `Success! Compiled 4
modules.`), and the checks only care that it does. Run it again and the function is
already there:

```
$ olifant gen helper --set page=home --set name=clamp_count --set type="Int -> Int" --set does="keeps a count between 0 and 99" --human
helper (1 edit, 0 conflicts)
  = src/Page/Home.elm (skipped: already present)
```

One more wording was tried: "a lambda (`\n -> …`) of type …", with a literal backslash.
It made every call run until the timeout, in all four runs, for reasons unknown. Change one
thing at a time, run it several times, and keep the wording that fills reliably.

## Step 7: test it with recorded fills

A project generator can't have pack tests; a pack generator can
([chapter 10](10-testing-generators.md)). So move it into a pack. This walkthrough used a
scratch clone of the Olifant repository, with the generator as `packs/elm/generators/helper/`
(the same `generator.toml`, its example changed to `olifant gen elm/helper …`).

Give it a test case, with the inputs from step 6:

```toml
# packs/elm/generators/helper/tests/clamp-count/inputs.toml
now = "2026-10-03T12:00:00Z"

[inputs]
page = "home"
name = "clamp_count"
type = "Int -> Int"
does = "keeps a count between 0 and 99"
```

and **record the fill** the model gave, from the plan's `holes` line (or the `fill` field
in JSON), so the test never calls a model:

```toml
# packs/elm/generators/helper/tests/clamp-count/fills.toml
# Recorded from qwen3-coder:30b via Ollama, 2026-10-03 (first attempt).
[fills]
body = "clamp 0 99"
```

The key is the hole's name. With no snapshot yet, the case fails and says why:

```
$ cabal test --test-show-details=direct --test-options='--match "/Olifant.GeneratorTesting/packs/elm/helper/"'
…
    elm/helper/clamp-count [✘]
…
        but got: CaseFailed ["src/Page/Home.elm: generated but not in snapshot"]
```

Bless it, and read what was recorded:

```
$ OLIFANT_BLESS=1 cabal test --test-show-details=direct --test-options='--match "/Olifant.GeneratorTesting/packs/elm/helper/"'
…
    elm/helper/clamp-count [✔]
$ tail -4 packs/elm/generators/helper/tests/clamp-count/snapshot/src/Page/Home.elm

clampCount : Int -> Int
clampCount =
    clamp 0 99
```

Now all three groups the case belongs to pass:

```
$ cabal test --test-show-details=direct --test-options='--match "elm/helper"'
…
Olifant.GeneratorTesting
  packs
    elm/helper/clamp-count [✔]
  elm pack output compiles
    elm/helper/clamp-count [✔]
  placeholder output compiles
    elm/helper/clamp-count [✔]

Finished in 0.9406 seconds
3 examples, 0 failures
```

- **packs** ran the generator with the recorded fill standing in for the model, compared the
  result with the snapshot, and ran it again to check that nothing changed.
- **elm pack output compiles** compiled the fixture with the snapshot laid over it.
- **placeholder output compiles** ran the generator again with no model at all, and
  compiled the result, with `Debug.todo "olifant: body: …"` in the hole. This group exists
  only for cases with a `fills.toml`, and it is the guarantee that the generator still
  works without AI.

A recorded fill is still checked. Record the wrong answer from step 6 instead:

```toml
[fills]
body = "Basics.max 0 (Basics.min 99)"
```

and the case fails with the compiler's reason, as a model's fill would:

```
  1) Olifant.GeneratorTesting.packs elm/helper/clamp-count
       expected: CasePassed
        but got: CaseFailed ["src/Page/Home.elm: TYPE MISMATCH"]
```

A recording under the wrong name fails too, before anything is compiled:

```
        but got: CaseFailed ["no recorded fill for `body` in fills.toml"]
```

When you change the generator's instruction, its old recordings still pass, since the test
never asks the model. Re-record when you want the test to reflect what the model does
now.

## Step 8: the Anthropic backend

The other backend kind calls Anthropic's Messages API. Add it next to `local`:

```toml
[model.backends.claude]
kind        = "anthropic"
model       = "claude-haiku-4-5-20251001"
api_key_env = "ANTHROPIC_API_KEY"
```

The key is never written in the config: `api_key_env` names the environment variable to read
it from (`ANTHROPIC_API_KEY` is the default), when `gen` runs. Olifant hands it to `curl`
in a temporary header file, so it never appears on a command line or in output. Use it for
one run with `--model claude`, or make it the `default`.

Without the variable set, the hole isn't filled, and the plan says why:

```
$ olifant gen rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=already_refunded --set message="The invoice was already refunded" --model claude --plan --human
Plan: rust-htmx/command-failure (2 edits, 0 conflicts)
  ~ billing/billing-service/src/refund_invoice.rs (+2 lines at line 27)
  ~ billing/billing-service/src/refund_invoice.rs (+1 line at line 36, compiler: status)
holes:
  status (expr) billing/billing-service/src/refund_invoice.rs:35  unfilled: ANTHROPIC_API_KEY is not set

--- billing/billing-service/src/refund_invoice.rs
#[error("The invoice was already refunded")]
AlreadyRefunded,

--- billing/billing-service/src/refund_invoice.rs
RefundInvoiceFailure::AlreadyRefunded => todo!("olifant: status: the value this match returns for RefundInvoiceFailure::AlreadyRefunded (The invoice was already refunded), following the other arms"),
```

The exit code is 0: a missing key, like a model that is down, never blocks a generation.
In JSON the hole is `{"status": "unfilled", "why": "ANTHROPIC_API_KEY is not set", …}`. A
backend name that isn't configured is an error before anything is planned:

```
$ olifant gen rust-htmx/command-failure … --model opus --plan --human
error: --model opus: no such backend under [model.backends] (backends: claude, down, local, small)
```

(exit 2; `down` and `small` are two more backends this sandbox had.) With the key set, `gen
--model claude` sends the same request as before, to `https://api.anthropic.com/v1/messages`.
That means **the code around each hole leaves your machine**
([chapter 12](12-holes-and-models.md#what-is-sent-to-the-model)). Choose the backend
accordingly.

## Where to go from here

- [Chapter 12](12-holes-and-models.md) explains each stage of the fill phase, every config
  key, and the limits.
- [Chapter 10](10-testing-generators.md#holes-fillstoml-and-placeholder-output) covers
  `fills.toml` and the placeholder compile check in full.
- The internals book's tour follows a hole through the code: markers, placeholders, the fill
  loop and the `curl` backends.
