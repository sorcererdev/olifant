# 10. Testing generators: snapshots and blessing

A pack generator is tested by running it against the pack's fixture app and comparing what it
wrote with a recorded **snapshot**. Every pack case in this repository runs as part of
`cabal test`:

```
$ export PATH="$HOME/.ghcup/bin:$PATH"
$ cabal test --test-show-details=direct --test-options='--match "packs" --match "elm pack output compiles" --match "rust pack output compiles" --match "placeholder output compiles"'
…
Olifant.GeneratorTesting
  packs
    has pack cases to run [✔]
    elm/msg/got-articles [✔]
    elm/msg/increment-by [✔]
    elm/msg/reset [✔]
    elm/msg/with-intent [✔]
    elm/page/counter [✔]
    elm/page/settings-with-msgs [✔]
    olifant-dev/error-case/sample-locked [✔]
    olifant-dev/generator/rails-controller [✔]
    olifant-dev/internal-module/marker-rules [✔]
    olifant-dev/unit/workspace-sync [✔]
    rails/model/post [✔]
    redwood/cell/blog-post [✔]
    redwood/cell/blog-posts [✔]
    rust-htmx/command-dependency/adapter-chosen [✔]
    rust-htmx/command-dependency/commented-hint [✔]
    rust-htmx/command-dependency/first-dependency [✔]
    rust-htmx/command-dependency/grouped-use [✔]
    rust-htmx/command-dependency/second-dependency [✔]
    rust-htmx/command-dependency/shared-after-another [✔]
    rust-htmx/command-dependency/shared-dependency [✔]
    rust-htmx/command-failure/already-refunded [✔]
    rust-htmx/command-failure/quoted-message [✔]
    rust-htmx/service-command/cancel-subscription [✔]
    rust-htmx/service-command/fresh-service [✔]
    rust-htmx/service-repository/notification-repo [✔]
    rust-htmx/service-skeleton/user-accounts [✔]
    rust-htmx/web-htmx-component/status-badge [✔]
    rust-htmx/web-htmx-resource-plural/workers [✔]
    rust-htmx/web-htmx-resource-singular/worker [✔]
    rust-htmx/wire-command/cancel-invoice [✔]
    rust-htmx/wire-command/hand-spelling [✔]
    rust-htmx/wire-command/mixed-fields [✔]
  elm pack output compiles
    elm/msg/got-articles [✔]
    elm/msg/increment-by [✔]
    elm/msg/reset [✔]
    elm/msg/with-intent [✔]
    elm/page/counter [✔]
    elm/page/settings-with-msgs [✔]
  rust pack output compiles
    rust-htmx/command-dependency/adapter-chosen [✔]
    rust-htmx/command-dependency/commented-hint [✔]
    rust-htmx/command-dependency/first-dependency [✔]
    rust-htmx/command-dependency/grouped-use [✔]
    rust-htmx/command-dependency/second-dependency [✔]
    rust-htmx/command-dependency/shared-after-another [✔]
    rust-htmx/command-dependency/shared-dependency [✔]
    rust-htmx/command-failure/already-refunded [✔]
    rust-htmx/command-failure/quoted-message [✔]
    rust-htmx/service-command/cancel-subscription [✔]
    rust-htmx/service-command/fresh-service [✔]
    rust-htmx/service-repository/notification-repo [✔]
    rust-htmx/service-skeleton/user-accounts [✔]
    rust-htmx/web-htmx-component/status-badge [✔]
    rust-htmx/web-htmx-resource-plural/workers [✔]
    rust-htmx/web-htmx-resource-singular/worker [✔]
    rust-htmx/wire-command/cancel-invoice [✔]
    rust-htmx/wire-command/hand-spelling [✔]
    rust-htmx/wire-command/mixed-fields [✔]
  placeholder output compiles
    elm/msg/with-intent [✔]
    rust-htmx/command-failure/already-refunded [✔]
    rust-htmx/command-failure/quoted-message [✔]

Finished in 75.4916 seconds
65 examples, 0 failures
```

(Four `Olifant.Catalog` tests whose names also contain "packs" ran too and are left out
above; they make up the 65.)

## A test case

```
packs/<pack>/generators/<generator>/tests/<case>/
  inputs.toml
  given/               optional: files laid over the fixture, for this case only
  fills.toml           optional: recorded fills for the generator's holes (chapter 12)
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

`given/` is for a case that needs a different starting point from the pack's fixture: the
state a re-run, an earlier generator or a hand edit leaves. Its files, at their project
paths, replace the fixture's file of the same path or add a new one, for that case only.
`command-dependency`'s `grouped-use` case has one file in `given/`: the fixture's
`void_invoice.rs` with a multi-line `use crate::{` / `refund_invoice::RefundInvoice,` / `};`
block added as its last `use`. The case adds `email_sender` to `void_invoice`, and its
snapshot pins the import after the block, not inside it:

```
$ diff given/billing/billing-service/src/void_invoice.rs snapshot/billing/billing-service/src/void_invoice.rs
8a9,10
> use crate::ports::email_sender::EmailSender;
> 
10c12,14
< pub struct VoidInvoice {}
---
> pub struct VoidInvoice {
>     pub email_sender: Arc<dyn EmailSender>,
> }
```

(run in `packs/rust-htmx/generators/command-dependency/tests/grouped-use/`; line 7 of the
given file is the block's `};`). The other cases with a `given/`:

| case | `given/` holds |
|---|---|
| `command-dependency/commented-hint` | `notifications-service`'s `ports.rs` with a `worksite_repository` port added, so the dependency the template's commented-out hint names exists and the output compiles |
| `command-dependency/shared-after-another` | `service.rs`, `void_invoice.rs` and `main.rs` after `email_sender` was added to `void_invoice`; the case then adds `invoice_repository`, which `refund_invoice` already shares |
| `command-dependency/adapter-chosen` | `main.rs` with `invoice_repository`'s `todo!()` replaced by `Arc::new(InMemoryInvoiceRepository::default())` |
| `wire-command/cancel-invoice` | the fixture's empty `cancel_invoice.rs` with an `email_sender` field and its `use` typed in by hand, not yet wired |
| `wire-command/mixed-fields` | a `void_invoice.rs` with two hand-added fields |
| `wire-command/hand-spelling` | a `cancel_invoice.rs` with a grouped `use crate::ports::{…}` and a field typed `Arc<dyn InvoiceRepository + Send + Sync>` |

The snapshot is compared against what the run writes, as always; a `given/` file the run
leaves alone does not need to be in the snapshot.

### A snippet case: `at`

A snippet ([chapter 14](14-editors-and-actions.md#writing-a-snippet)) goes at a cursor, so its
case says where the cursor is, as `gen --at` takes it:

```toml
# packs/rust-htmx/generators/failure-variant/tests/blank-line/inputs.toml
now = "2026-10-04T12:00:00Z"
# The cursor: the blank line after `Unknown(String),` (Esc strips its indent).
at = "billing/billing-service/src/refund_invoice.rs:27:1"

[inputs]
```

`at` is `FILE:LINE:COL`, a project path and a 1-based line and column. Anything else fails the
case with the message a malformed `inputs.toml` gets ("cannot read …/inputs.toml (needs
`now` and an [inputs] table of strings)"), which doesn't mention `at`. The case's `given/` holds the cursor's state:
here `refund_invoice.rs` with the blank line 27. The snapshot is the file with that line
replaced, each tab stop written as its name or value (`#[error("message")]` / `Variant,`), as
`gen --at` writes it. `at` also gives `uncovered` queries their cursor
([chapter 6](06-queries-and-choices.md#uncovered-the-cases-a-match-at-the-cursor-lacks)), so
`match-arm`'s case gets its one choice, `RefundInvoiceFailure::AlreadyRefunded`, from the
compiler, and its `fills.toml` has the model's `status`.

The five snippet cases in this repository:

| case | cursor | `given/` holds |
|---|---|---|
| `rust-htmx/failure-variant/blank-line` | `refund_invoice.rs:27:1` | a blank line in the `Failure` enum |
| `rust-htmx/match-arm/uncovered-variant` | `refund_invoice.rs:35:1` | an `AlreadyRefunded` variant, and a blank line after `status()`'s last arm |
| `rust-htmx/add-port/blank-field-line` | `cancel_invoice.rs:7:1` | `CancelInvoice`'s struct opened onto three lines |
| `olifant-dev/spec-case/after-it` | `SampleUnitSpec.hs:8:7` | `  it "` typed as line 8 |
| `python-htmx/failure-variant/after-a-failure` | `create_booking.py:31:1` | the blank line `o` made below the last failure's `reason: str` |

A snippet whose `[applies_at]` has `after` ([chapter 14](14-editors-and-actions.md#after-a-node-after))
is placed in a case as `gen --at` places it: indented like the node the cursor follows, when
it follows one. `after-a-failure`'s snapshot has the new class at column 0, below the
four-space `reason: str`, with two blank lines above it and two below:

```
$ diff given/src/bistro/booking/commands/create_booking.py snapshot/src/bistro/booking/commands/create_booking.py
34a35,37
> @dataclass(frozen=True)
> class Name:
>     reason: str
35a39
> 
```

(run in `packs/python-htmx/generators/failure-variant/tests/after-a-failure/`.) An `at` on a
line that follows no such node gets the usual indentation, from the line above.

Two of the checks below don't apply to a snippet case:

- **It is not re-run.** After the first run the cursor's line is the expansion, so a second
  run at the same cursor would be another snippet, not a no-op.
- **It is not compiled on its own** in "rust pack output compiles", nor type-checked in the
  "python pack" group. A snippet is unfinished by
  design: `failure-variant`'s output has a variant no `match` covers, and `add-port`'s a field
  nobody wires, until the follow-up runs. The "placeholder output compiles" group still runs
  a snippet case that has a `fills.toml`, with the cursor placed as in the case, so
  `match-arm`'s arm must compile with its `status` left as a placeholder.

## What a case checks

`runCase` in `src/Olifant/GeneratorTesting.hs`:

1. Loads the fixture app (`fixtures/app/`), with the case's `given/` files laid over it, into
   an in-memory workspace; nothing on disk is touched.
2. Runs the generator with the case's inputs, applied. Owned regions from the pack's
   `regions.toml` are refreshed too, as in a real `gen`.
3. **Conflicts fail the case**, listing each one; unless *every* conflict is only a missing
   tool (no `ast-grep`, no `elm`), in which case the case is **pending**, not failed. A
   generator that cannot run because a query's tool is missing is pending the same way
   (below); any other error stopping the generator fails the case.
4. **Snapshot equality**, both ways: every written file must be in the snapshot with the same
   content, and every snapshot file must have been written.
5. **Re-run is a no-op**: the same inputs are run again on the result, and must write nothing.
   This is what catches an `insert` that is not idempotent, or a `create` that would not
   reproduce its own file. A snippet case is not re-run ([above](#a-snippet-case-at)).

A case with a `fills.toml` replays it as the model in steps 2 and 5, and fails if any hole
ends up unfilled ([below](#holes-fillstoml-and-placeholder-output)).

For the Elm pack there is one more group, **"elm pack output compiles"**: for each Elm case,
the fixture with the case's `given/` and then the snapshot laid over it is compiled with the adapter's `[check]` command,
and must compile cleanly. Structure-placed code that is in the right place but syntactically
wrong fails here.

The rust-htmx pack has a group like it, **"rust pack output compiles"**, and it works the
same way: each rust-htmx case's fixture, `given/` and snapshot go to `compileClean` with the
pack's own Rust adapter, so the check is the adapter's `[check]`: `cargo check --workspace
--exclude diesel-* --message-format=json` (chapter 9). For that, the fixture is a real cargo
workspace:

- the root `Cargo.toml`'s `members` are the services (`billing/billing-service`,
  `notifications/notifications-service`), below the `##PLOP NEW PACKAGE HOOK##` line that
  `service-skeleton` and `service-repository` insert new crates at; `main` and `web-htmx`
  (axum; the fixture holds only part of `web-htmx`) are not members;
- every service crate has its real `Cargo.toml` (workspace dependencies: `async-trait`,
  `thiserror`, `tokio`, `tracing`);
- a stub `db` crate (`db/src/lib.rs` is one comment) lets the Diesel adapters' manifests
  load. Those adapters need libpq and the template's real `db`, so the check excludes
  `diesel-*`; the in-memory adapters `service-repository` writes are compiled.

A missing import, a struct literal missing a field, a `match` missing an arm, or a port that
names a type nobody defines fails here even when the snapshot matches. When rustc reports
broken files, the failure lists them, one per line, with rustc's message. Here is
`compileClean` called from `cabal repl -v0 lib:olifant` on the fixture, with a type error
typed into `refund_invoice.rs` and a call to an undefined function into `send_email.rs`:

```
billing/billing-service/src/refund_invoice.rs: mismatched types
notifications/notifications-service/src/send_email.rs: cannot find function `undefined_fn` in this scope
```

A `match` missing an arm is not a broken file to `rustc-json` (it is a site, chapter 4), so
that failure shows the end of cargo's own output instead (`` error: could not compile
`billing-service` (lib) due to 1 previous error; 2 warnings emitted ``).

cargo builds into `~/.cache/olifant/check/rust` (the adapter's `{= cache =}`), shared by
every case and every run, so after the first the whole group (19 cases) takes about 7
seconds. The fixture has no `Cargo.lock`, so each check resolves the dependencies again,
and the first run needs network access to fetch them.

Two things follow from the check. The pack's fixture compiles on its own, and so does every
`given/`: a starting point the fixture lacks, such as a field typed in by hand but not yet
wired, goes in the case's `given/` (`wire-command/cancel-invoice` above), not in the shared
fixture. And a generator's output has to compile, which is why `service-repository` differs
from the plop template it was ported from: the template's port imports `crate::models::X`
but never creates `X`. The pack's generator appends a stub to the service's `models.rs`
(and `models.rs` is in its `[requires]`):

```
$ olifant gen rust-htmx/service-repository --set service=notifications --set aggregate=delivery_log --human
rust-htmx/service-repository (10 edits, 0 conflicts)
  + notifications/notifications-service/src/ports/delivery_log_repository.rs (19 lines)
  ~ notifications/notifications-service/src/models.rs (+4 lines at line 1)
  …
$ git diff -- notifications/notifications-service/src/models.rs
…
@@ -0,0 +1,4 @@
+#[derive(Clone, Debug, PartialEq)]
+pub struct DeliveryLog {
+    pub id: String,
+}
```

The edit has `unless = { matches = "^[[:space:]]*pub (struct|enum|type) DeliveryLog…" }`
(the name rendered from `aggregate`; [chapter 6](06-queries-and-choices.md)), so a
`models.rs` that already declares the type keeps its own:

```
$ olifant gen rust-htmx/service-repository --set service=notifications --set aggregate=delivery_log --plan --human
Plan: rust-htmx/service-repository (10 edits, 0 conflicts)
  + notifications/notifications-service/src/ports/delivery_log_repository.rs (19 lines)
  = notifications/notifications-service/src/models.rs (skipped: unless: line 1 of notifications/notifications-service/src/models.rs matches)
  …
```

(run with `pub struct DeliveryLog {` on line 1 of `models.rs`). The port's `pub mod` line
in `ports.rs` comes from the pack's `ports` region, refreshed at the end of the run: the
plan's last line, left out here
([chapter 5](05-owned-regions.md#families-one-region-per-matching-file)).

### The Python pack

`python-htmx`'s fixture (`packs/python-htmx/fixtures/app/`) is a small, real app: a
restaurant's table bookings, in the layout the pack's generators write.

```
pyproject.toml                 uv project: FastAPI, Jinja, uvicorn, python-multipart; dev:
                               pyright, pytest; pyright strict; tests are src/**/*__test.py
uv.lock
src/bistro/
  booking/                     a service: no service.py, just these folders
    models/                    one frozen dataclass per module: table, slot, booking
    ports/                     one folder per port: its Protocol and its adapters beside it
      booking_repository/      booking_repository.py (the Protocol), in_memory_…, sqlite_…,
                               booking_repository__test.py (one contract test, both adapters)
      id_generator/            id_generator.py, in_memory_id_generator.py, uuid_id_generator.py
    commands/                  create_booking.py (Input, failures, their union, handle) and
                               create_booking__test.py beside it
    queries/                   get_day_schedule.py and its test
  shared/result.py             Ok[T]
  web/
    app.py                     create_app(routers): the home page, then each resource's routes
    resources.py               RESOURCES, a region kept equal to web/*/routes.py
    templates/                 base.html (htmx), home.html
    bookings/                  routes.py (bookings_routes(query, command)), failures.py,
                               templates/, routes__test.py
  main.py                      the composition root: adapters, then commands and queries,
                               then routers; nothing runs at import
```

Its conventions are what the generators rely on:

- **Failures are values.** `handle()` returns `Ok[Booking] | CreateBookingFailure`, never
  raises; each failure is a frozen dataclass with a `reason: str`.
- **A union is one member per line**, in parentheses, so a new member is a new line
  (`union NAME`, [chapter 9](09-packs-and-language-adapters.md#a-fourth-example-python)).
- **A site names a failure through its command's module** (`create_booking.SlotTaken`, or an
  alias's `cb.SlotTaken`), so a new failure needs no import anywhere.
- **Calls are one argument per line** where generators add arguments (`main.py`'s
  constructions, a test's factory): Olifant inserts whole lines.
- **A port's fake is `in_memory_<port>.py` and builds with no arguments**, so any test can
  pass one.

It runs: `uv run uvicorn --factory --app-dir src bistro.main:create --reload`, then
http://localhost:8000/ links each resource; /bookings shows the day's grid. A booking
answers 201, and the same slot again 409, from `failures.py`'s `match`.

Since the fixture *is* the architecture, the **"python pack"** group checks it as well as
the cases' output:

```
$ cabal test --test-show-details=direct --test-options='--match "python pack"'
…
  python pack
    olifant init makes a project where pytest and pyright are green (north-star project 2 §1) [✔]
    an unfilled expr hole leaves the code after it checked [✔]
    the python slice: from an empty folder, the stubs red, then green, check holding, and served [✔]
    its fixture is pyright-strict clean [✔]
    its fixture's own tests pass [✔]
    tests keep the test policy: python-htmx/command/new-command [✔]
    …
    output type-checks: python-htmx/web-resource/new-resource [✔]
…
```

- **Pyright-strict clean** and **its own tests pass**: the fixture through the adapter's
  `[check]`, and `uv run --frozen pytest -q` in a temp copy.
- **Output type-checks.** Each case that is not a snippet: the fixture, its `given/` and its
  snapshot, through pyright. A starter's case (`starter = true`) is checked alone, with no
  fixture under it.
- **Tests keep the test policy** (the pack's rule: boilerplate comes with passing tests,
  behaviour with a red stub wired up). A case whose `inputs.toml` says `run_tests = true`
  has its project's tests run (fixture, `given/`, snapshot); exactly `expect_failing = N`
  of them must fail (default 0). The `port` and `repository` cases expect 0 (their contract
  tests pass as made); `command`, `query` and `web-resource` expect their stubs:

  ```toml
  # packs/python-htmx/generators/command/tests/new-command/inputs.toml
  now = "2026-10-05T12:00:00Z"
  # A behaviour stub: its one test fails until the case and handle are written.
  run_tests = true
  expect_failing = 1
  ```

- **`olifant init` makes a green project**: the starter, then `uv sync`, `pytest` (3 pass)
  and `pyright`, in a temp directory.
- **An unfilled expr hole leaves the code after it checked.** The placeholder must stand for
  any type *and* not hide what follows ([chapter 9](09-packs-and-language-adapters.md#hole-kinds-holeskind)).
- **The python slice** runs `scripts/slice-python.sh` (below).

Each needs `uv`; without it, the group is pending with `uv is not installed (brew install
uv)`.

### The slice: a whole app from an empty folder

`scripts/slice-python.sh DIR STAGE` builds table bookings the way a person would with this
pack, in about five seconds, each step a commit:

```
olifant init: python-htmx           typed: 04-ports              (each __init__'s ports)
service and models                  wired                        (wire, per parameter)
typed: 01-fields                    the bookings resource        (web-resource, two commands)
repositories and a port             typed: 05-remove             (a port method)
typed: 02-next-id                   remove in the adapters       (port-method-adapters)
next_id in the adapters             typed: 06-behaviour          (the tests and handle bodies)
typed: 03-holes                     (what the model fills: codecs, next_id, statuses)
commands and a query
```

What a person types is recorded as `packs/python-htmx/slice/*.diff`, applied between the
generator calls, because the generators read it (the repository reads the model's fields,
`wire` the `__init__`'s parameters). The script runs with `--no-fill`; `03-holes` and the
behaviour patch carry what a model fills, so the run needs no Ollama. The stages:

- **`gens`**: pyright clean, and the red tests are exactly the stubs, by name (seven: two
  commands, the query, the routes, the `next_id` contract and `remove`'s, once per adapter);
- **`behaviour`**: 31 green, pyright clean, `olifant check` holds;
- **`serve`**: uvicorn answers `GET /bookings?day=2026-10-09` with 200.

The spec test runs all three with the freshly built binary (`cabal list-bin olifant`). To
try it by hand: `OLIFANT=$(cabal list-bin olifant) scripts/slice-python.sh /tmp/slice gens`,
then `cd /tmp/slice/bistro && git log --oneline`.

## Holes: `fills.toml` and placeholder output

A generator with holes ([chapter 12](12-holes-and-models.md)) asks a model for code. A test
must not: it would be slow, need a server, and give a different answer from one run to the
next. So a case records the fills once, in `fills.toml`, and the test replays them:

```toml
# packs/elm/generators/msg/tests/with-intent/fills.toml
# Recorded from qwen3-coder:30b via Ollama, 2026-10-02 (first attempt).
[fills]
update = "( { count = 0 }, Cmd.none )"
```

```toml
# packs/elm/generators/msg/tests/with-intent/inputs.toml
now = "2026-10-02T12:00:00Z"

[inputs]
page = "home"
msg = "reset"
intent = "reset count to 0"
```

Under `[fills]`, each key is a hole's name as the plan reports it (`update`, or
`status[file:line]` for a repeated name) and each value is that hole's code, as a string.
The recorded fills stand in for the model and nothing else changes: each fill must still
parse as its kind and compile with the plan, through the adapter's real `[check]`. The case
then fails in three ways:

- a hole has no recorded fill: `no recorded fill for `update` in fills.toml`;
- a recorded fill is rejected (it no longer parses or compiles): the reason, as the plan
  would report it;
- the snapshot differs, as for any case. The snapshot holds the *filled* output
  (`with-intent`'s has `( { count = 0 }, Cmd.none )` in the `Reset` branch).

The replayed model is called `recorded`, and it gets no retries: a recorded fill is right
or the case fails. The re-run step still has to be a no-op, which it is: the filled arm is
already there, so no hole is planned the second time.

**Recording a fill.** Run the generator against the fixture with a real model (in a
`scripts/try-pack.sh` sandbox, with the case's inputs), take each fill from the plan's
`holes` (the `fill` field in JSON, or after `filled by MODEL:` in human output,
[chapter 12](12-holes-and-models.md#the-plans-holes)), and write it into `fills.toml` with a
comment saying where it came from. Then bless the snapshot. [Chapter 13](13-tutorial-holes.md#step-7-test-it-with-recorded-fills)
does this end to end.

**Placeholder output compiles.** A generator with holes must still work without a model. The
group **"placeholder output compiles"** takes every case that has a `fills.toml`, plans it
again with no model, checks that at least one hole is unfilled, and compiles the fixture with
the case's `given/` and that plan's output laid over it, every hole as its adapter's
placeholder. A placeholder that doesn't compile, such as a `todo!()` in a type position or
a `Debug.todo` message with an unescaped quote, fails here. It uses the pack's own language
and compiler (Elm and `elm` for the Elm pack, Python and `uv` for python-htmx, Rust and
`cargo` otherwise). It is meant to be pending when that compiler is missing, like the other
compile groups, but today it fails instead ([below](#other-limits-of-pack-tests)).
For python-htmx this is where the `Any` placeholder
(`getattr(NotImplemented, "olifant: …")`) proves itself: in `failures.py` it stands for an
`int`, under strict pyright, with no import, and the code after it is still checked.

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

A query is treated the same way. An AST query needs `ast-grep`, and a command query its
program; when the tool is missing the generator cannot run at all, so the case is pending
with the query's error. `wire-command` reads the command struct's fields with an AST query
(`deps`), so with neither `ast-grep` nor `cargo` on the PATH:

```
$ PATH="$HOME/.ghcup/bin:/usr/bin:/bin" cabal test --test-show-details=direct --test-options='--match "wire-command/cancel-invoice"'
Olifant.GeneratorTesting
  packs
    rust-htmx/wire-command/cancel-invoice [‐]
      # PENDING: rust-htmx/wire-command: query `deps`: billing/billing-service/src/cancel_invoice.rs: `struct CancelInvoice`: ast-grep is not installed (brew install ast-grep)
  rust pack output compiles
    rust-htmx/wire-command/cancel-invoice [‐]
      # PENDING: cargo is not installed (brew install rustup; rustup default stable)

Finished in 0.0619 seconds
2 examples, 0 failures, 2 pending
```

Any other query failure (a command that exits non-zero, a template error) still fails the
case.

The Rust compile check needs `cargo`; without it (here `ast-grep` is on the PATH, rustup's
`cargo` is not) the pack case runs and the compile check is pending, with the install hint:

```
$ PATH="$HOME/.ghcup/bin:/opt/homebrew/bin:/usr/bin:/bin" cabal test --test-show-details=direct --test-options='--match "wire-command/cancel-invoice"'
Olifant.GeneratorTesting
  packs
    rust-htmx/wire-command/cancel-invoice [✔]
  rust pack output compiles
    rust-htmx/wire-command/cancel-invoice [‐]
      # PENDING: cargo is not installed (brew install rustup; rustup default stable)

Finished in 0.1090 seconds
2 examples, 0 failures, 1 pending
```

Homebrew's `rustup` is keg-only: its `cargo` is in `/opt/homebrew/opt/rustup/bin`, which is
not on the PATH unless you put it there.

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

The same run of the Rust case with `OLIFANT_REQUIRE_TOOLS=1` fails both, the query too:

```
  1) Olifant.GeneratorTesting.packs rust-htmx/wire-command/cancel-invoice
       required tool missing: rust-htmx/wire-command: query `deps`: billing/billing-service/src/cancel_invoice.rs: `struct CancelInvoice`: ast-grep is not installed (brew install ast-grep)
…
  2) Olifant.GeneratorTesting, rust pack output compiles, rust-htmx/wire-command/cancel-invoice
       required tool missing: cargo is not installed (brew install rustup; rustup default stable)
```

The Python checks need `uv`. Without it (here with `ast-grep` on the PATH), the
`failure-arms` cases and the "python pack" group are pending:

```
    python-htmx/failure-arms/new-failure [‐]
      # PENDING: python-htmx/failure-arms: cannot complete cases: `uv` is not installed (needed to check python); run `olifant doctor`
…
  python pack
    its fixture is pyright-strict clean [‐]
      # PENDING: uv is not installed (brew install uv)
```

The pre-merge hook sets `OLIFANT_REQUIRE_TOOLS=1`, so a merge into `main` on a machine
missing `ast-grep`, `elm`, `cargo` or `uv` fails instead of quietly skipping those cases. The hook
puts `~/.ghcup/bin` and `/opt/homebrew/opt/rustup/bin` on its PATH.

## Other limits of pack tests

- **Command queries run in the fixture directory on disk.** A `one_of` or region
  `query` with a `command` runs for real, rooted at the case's `fixtures/app/` — like a
  project's own command queries run rooted at the project. The case's in-memory copy of the
  fixture is what gets written to and compared; the command only reads what is on disk. A
  case's `given/` files are in memory only, so a command does not see them.
- **`one_of` in a `[[uses]]` generator is resolved from the fixture too.** A case for a
  composite whose `[[uses]]` target has a `one_of` input works the same as it does from
  the CLI, dependent choices included: `GeneratorTestingSpec` runs `wire-command` with a
  hand-added `clock` field (through `preparePack`, the same setup `runCase` uses) and
  expects "expected one of email_sender, invoice_repository (the ports of service=billing);
  got `clock`".
- **Failure messages are words, not Haskell values.** A case failure (a generation error, or
  one from re-running) prints the same sentence the CLI would, e.g. "undefined variable
  `nope`", not `CaseFailed ["TemplateFailed …"]`.
- **A missing compiler fails "placeholder output compiles".** Without the compiler,
  `complete` finds no site and an `uncovered` query no case, so the plan has no hole, and the
  group's first check ("at least one hole is unfilled") fails before it gets to the
  compiler. With `ast-grep` on the PATH but no `uv`, `cargo` or `elm`, all eight cases in the
  group fail there (`not expected: []`), while the other groups have the same cases
  pending.
- **One fixture per pack.** Every case of every generator runs against the same
  `fixtures/app/`. A case that needs a different starting point (a page that already has
  messages) adds the changed files under its own `given/`, not to the fixture.

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

For a Python pack (a fixture with a `pyproject.toml`), `try-pack.sh` also runs `uv sync` in
the sandbox before committing it, so `uv run pytest` and `uv run pyright` work there at once.
The `.venv` it makes is in the fixture's `.gitignore`, and Olifant never looks inside it.
Without `uv`, it says so (`note: uv is not installed (brew install uv); the Python checks
will not run`) and makes the sandbox anyway. A pack with a `languages/` directory and no
generators yet can be tried too.

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
| `pre-merge-commit` | yes | runs `cabal test` with `OLIFANT_REQUIRE_TOOLS=1` (a missing `ast-grep`, `elm`, `cargo` or `uv` fails instead of pending), then `olifant check` (owned regions and rules, [chapter 15](15-conventions-that-hold.md); via `cabal run`, so it checks with the code being merged); either failing aborts the merge ("fix, then run `git merge --continue`"). `git merge --no-verify` skips it |
| `post-commit` | yes | runs `scripts/install-olifant.sh`, so `~/.local/bin/olifant` is always built from `main`; never blocks the commit |
| `post-merge` | yes | the same install, after a merge into `main` |

So pack snapshots, the Elm and Rust "compiles" checks, the Python pack's checks, and the cabal module lists (chapter 5) are all
enforced at the point a branch lands on `main`.

Git only runs `pre-merge-commit` when a merge creates a merge commit; a fast-forward merge
(`git merge` of a branch that is strictly ahead) skips it, and with it the tests.
`git config branch.main.mergeoptions --no-ff` makes a plain `git merge` into `main` always
create a merge commit, so the hook always runs (already set in the owner's clone; a fresh
clone needs it too).
