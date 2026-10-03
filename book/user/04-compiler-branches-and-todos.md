# 4. Branches the compiler finds, and todos

Add a variant to a type and every `case` over that type is now incomplete. The compiler
knows exactly which ones — Elm's, GHC's or rustc's. Olifant uses that: a `complete` edit adds a
branch to every `case` the compiler says is missing the new constructor. This chapter shows
Elm first, then Haskell, which works the same way with two differences: how a constructor
is named, and what happens with no `text`/`template` at all, then Rust, where a `case` is a
`match` and its branches are arms.

```toml
[[edits]]
complete = "Route.{= P =}"            # the constructor this generator introduced
where    = "case in changeRouteTo"    # this body is for that case
text     = """
{= pattern =} ->
    {= P =}.init |> updateWith {= P =} Got{= P =}Msg model"""
```

After the other edits are planned, Olifant copies the project to a scratch directory,
runs `elm make --report=json`, and reads the "missing patterns" it reports. For each one
naming `Route.Settings` it adds a branch, then checks again (Elm reports one module at a
time, so it can take a few rounds). `{= pattern =}` is the missing pattern, written the way
that file refers to it: `Just Route.Settings` in `Main.elm`, just `Settings` in
`Route.elm`.

The plan marks these edits: `(+2 lines at line 59, compiler: changeRouteTo)` in the
human output, `"found_by": "compiler", "site": "changeRouteTo"` in JSON.

## Places nobody told the generator about

Suppose your app has a hand-written function:

```elm
toPath : Route -> String
toPath route =
    case route of
        Home ->
            "/"

        SamplePage ->
            "/sample-page"
```

`elm/page` knows nothing about `toPath`. Run it anyway:

```
$ olifant gen elm/page --set page=settings
elm/page (10 edits, 0 conflicts)
  + src/Page/Settings.elm (42 lines)
  …
  ~ src/Route.elm (+2 lines at line 36, compiler: toPath, todo)
  …
  ? src/Route.elm: toPath needs a branch for Settings (left as a todo)
```

The compiler found `toPath`, and since no `complete` edit's `where` matches it, Olifant
added a placeholder:

```elm
        Settings ->
            Debug.todo "olifant: toPath Settings"
```

The app compiles, and the plan lists the gap (JSON: `"todos": [{"file":
"src/Route.elm", "function": "toPath", "pattern": "Settings", …}]`). Replace the
`Debug.todo` with the real body (`"/settings"`). In measurements, agents given only the
todo list fixed these correctly every time, reading only the file it named.

Treat todos as must-fix: **Elm 0.19.2 compiles `Debug.todo` even with `--optimize`**, so
nothing else stops one reaching production, where it crashes if that branch runs.

## Haskell (GHC)

GHC's `-Wincomplete-patterns` reports the same shape of problem — a `case` missing
constructors — through `-fdiagnostics-as-json`. Olifant's check command for a cabal project
is `cabal repl -v0 lib:NAME --repl-options=-fno-code --repl-options=-fdiagnostics-as-json
--repl-options=-Wincomplete-patterns`, not `cabal build`: with `-fno-code`, `build` always
exits 1 at its link step, while `repl` exits 0 after type-checking, even when the code has
errors: those arrive as JSON diagnostics, which is how Olifant sees them. About 3 seconds
a round on Olifant itself.

GHC names the scrutinee's type, so a `complete` edit names its constructor `Type.Ctor`
instead of Elm's `Module.Ctor`. It applies wherever `Type` appears in the type GHC prints, so
a `case` over a `Maybe CatalogError` or a `(CatalogError, Bool)` gets its branch too
(`Just (PackLocked _)`, `((PackLocked _), _)`):

```toml
[[edits]]
complete = "{= T =}.{= C =}"   # e.g. CatalogError.PackLocked
```

and it is visible only where GHC reports that type — `CatalogError.PackLocked` can extend a
`case` over `CatalogError`, never one over some other type that happens to have a
constructor of the same bare name.

A `complete` edit with no `text` and no `template` at all leaves a todo branch at *every*
site the compiler finds: there is no body to try, so nothing is ever chosen. That is what
`olifant-dev/error-case` does: it adds the constructor and leaves every `case` for a person
(or a model) to fill in. A real run, adding `PackLocked Text` to `CatalogError`:

```
$ olifant gen olifant-dev/error-case --set path=src/Olifant/Catalog.hs --set type=catalog_error --set name=pack_locked --set fields=Text --human
olifant-dev/error-case (3 edits, 0 conflicts)
  ~ src/Olifant/Catalog.hs (+1 line at line 87)
  ~ src/Olifant/Catalog.hs (+1 line at line 103, compiler: describeCatalogError, todo)
  ~ src/Olifant/Cli.hs (+1 line at line 265, compiler: catalogFailureAt, todo)
  ? src/Olifant/Catalog.hs: describeCatalogError needs a branch for PackLocked _ (left as a todo)
  ? src/Olifant/Cli.hs: catalogFailureAt needs a branch for PackLocked _ (left as a todo)
```

Two GHC rounds, about 7 seconds. The inserted line in `Cli.hs`:

```haskell
  PackLocked _ -> error "olifant: catalogFailureAt PackLocked _"
```

and the result type-checks clean.

Only `case` and `\case` alternatives are extended. GHC also reports gaps in a function's
equations ("In an equation for `f`"), in a lambda (`\(SampleMissing p) -> p`) and in a
pattern binding (`let SampleMissing n = e`); Olifant cannot add to those, so each becomes a
plan warning naming the file, the line, what GHC called it and the pattern, never guessed
at:

```
  ? warning: olifant-dev/error-case: src/Olifant/SampleErrors.hs (line 41): an equation for `label` needs a case for SampleLocked _, which olifant cannot extend yet
```

The same goes for a `case` that needs one of the run's constructors where none of its edits
can appear (another type's constructor with the same name): a warning, never a silent skip.
A `case` already missing other patterns gets only the run's own: GHC lists every missing
pattern, one per line, and Olifant adds just those naming its constructors.

A branch is spaced like the `case`'s own alternatives, Haskell or Elm: a blank line before
the new branch only when one already separates the last two alternatives, none when they
sit flush against each other (ordinary Haskell style), and a blank line when the `case` has
only one alternative to copy the spacing from.

## Rust (cargo)

The rust-htmx pack's adapter checks with `cargo check --message-format=json` (format
`rustc-json`; the `[check]` is in [chapter 9](09-packs-and-language-adapters.md#a-third-example-rust)).
`rust-htmx/command-failure` adds a failure to a command's `…Failure` enum, and leaves the
rest to the compiler and, for one value per site, to a model:

```toml
# packs/rust-htmx/generators/command-failure/generator.toml (excerpt)
[derive]
service_dir = "{= service|kebab =}/{= service|kebab =}-service"
Failure = "{= command|pascal =}Failure"

[[edits]]
insert = "{= service_dir =}/src/{= command|snake =}.rs"
at     = "end of enum {= Failure =}"
text   = """
#[error("{= message|rust_string =}")]
{= failure|pascal =},"""

# Every match on the enum gets an arm whose value is a hole (spec §6.4): a
# configured model fills it per site; without one it stays a todo!().
[[edits]]
complete = "{= Failure =}::{= failure|pascal =}"
text     = "{= pattern =} => {= ? status: expr | the value this match returns for {= pattern =} ({= message =}), following the other arms =},"
```

The body is `{= pattern =} => …,`, and the arm's value is a **typed hole**: which HTTP status
a refund that already happened deserves is a decision, not boilerplate, so a model makes it
when one is configured ([chapter 12](12-holes-and-models.md)). This section runs with no
model; chapter 12 shows the same generator with one.

Its `service` and `command` are `one_of` queries, the same as `command-dependency`'s
([chapter 6](06-queries-and-choices.md)). In the fixture, `refund_invoice.rs` ends with the
command's failures and a mapping from each to an HTTP status:

```rust
#[derive(Error, Debug, PartialEq)]
pub enum RefundInvoiceFailure {
    #[error("Something went wrong")]
    Unknown(String),
}

impl RefundInvoiceFailure {
    pub fn status(&self) -> u16 {
        match self {
            RefundInvoiceFailure::Unknown(_) => 500,
        }
    }
}
```

In a sandbox (`scripts/try-pack.sh rust-htmx /tmp/olifant-fail && cd /tmp/olifant-fail`):

```
$ olifant gen rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=already_refunded --set message="The invoice was already refunded" --plan --human
Plan: rust-htmx/command-failure (2 edits, 0 conflicts)
  ~ billing/billing-service/src/refund_invoice.rs (+2 lines at line 27)
  ~ billing/billing-service/src/refund_invoice.rs (+1 line at line 36, compiler: status)
holes:
  status (expr) billing/billing-service/src/refund_invoice.rs:35  unfilled: no model is configured

--- billing/billing-service/src/refund_invoice.rs
#[error("The invoice was already refunded")]
AlreadyRefunded,

--- billing/billing-service/src/refund_invoice.rs
RefundInvoiceFailure::AlreadyRefunded => todo!("olifant: status: the value this match returns for RefundInvoiceFailure::AlreadyRefunded (The invoice was already refunded), following the other arms"),
```

Applied (the same command without `--plan`), `git add -A && git diff --cached` shows both:

```diff
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
+            RefundInvoiceFailure::AlreadyRefunded => todo!("olifant: status: the value this match returns for RefundInvoiceFailure::AlreadyRefunded (The invoice was already refunded), following the other arms"),
         }
     }
 }
```

and the workspace builds:

```
$ cargo check --workspace
…
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 2.97s
```

The arm's value is the hole's placeholder, a `todo!()` that names the hole and carries its
instruction (the Rust adapter's `[holes.expr]`, [chapter 9](09-packs-and-language-adapters.md#hole-kinds-holeskind)).
It compiles, so the generator works with no model at all; the plan lists it under `holes`
rather than as a todo branch, because the generator did write a body. Run it again and the
variant is `skipped: already present`, and the compiler finds nothing to add.

The message is free text inside a Rust string, so the generator writes it through the
`rust_string` filter ([chapter 7](07-writing-a-generator.md#filters)), and the hole's
`placeholder` does the same for its instruction, which holds the message too. The pack's `quoted-message` case uses
`--set failure=too_late --set message='Too "late" for {refunds}'`:

```diff
+    #[error("Too \"late\" for {{refunds}}")]
+    TooLate,
```

which `thiserror` prints as `Too "late" for {refunds}`, and which compiles.

What Olifant read is rustc's E0004. Without the `complete` edit, the same variant gives:

```
error[E0004]: non-exhaustive patterns: `&RefundInvoiceFailure::AlreadyRefunded` not covered
  --> billing/billing-service/src/refund_invoice.rs:33:15
   |
33 |         match self {
   |               ^^^^ pattern `&RefundInvoiceFailure::AlreadyRefunded` not covered
…
help: ensure that all possible cases are being handled by adding a match arm with a wildcard pattern or an explicit pattern as shown
   |
34 ~             RefundInvoiceFailure::Unknown(_) => 500,
35 ~             &RefundInvoiceFailure::AlreadyRefunded => todo!(),
   |
```

- **The site** runs from the scrutinee (`self`, line 33) to the `match`'s last arm, which is
  the line rustc's own suggestion edits (34). The new arm goes after it.
- **The pattern** is rustc's, less the `&` it prints for a reference scrutinee (`self` is
  `&RefundInvoiceFailure`); match ergonomics makes `RefundInvoiceFailure::AlreadyRefunded`
  enough. Variants with fields come as rustc writes them (`Unknown(_)`, `{ .. }`).
- **Visibility is by type name**, as with GHC: `complete = "RefundInvoiceFailure::AlreadyRefunded"`
  (or `RefundInvoiceFailure.AlreadyRefunded`) can extend a `match` whose type rustc reports
  as `RefundInvoiceFailure` or `&RefundInvoiceFailure`, never a `match` on another enum with
  an `AlreadyRefunded` of its own.
- **Commas.** Arms are separated by commas, and the last arm of a `match` may leave its
  comma out. Before adding an arm, Olifant gives the last one its comma, unless it has one
  or ends a block (`}`). A one-arm `match` gets no blank line before the new arm (Elm's
  rule would add one; Rust's arms sit together).
- **Three at a time.** rustc names at most three missing patterns ("`A`, `B`, `C` and 2
  more not covered"). Olifant places those, checks again, and the next round names the
  rest; nothing is guessed.

The last two show up together when one run adds four failures (a chain of four steps,
[chapter 8](08-composing-generators.md)). This sandbox has a model configured, so
`--no-fill` is what keeps every hole a placeholder:

```
$ olifant gen rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=already_refunded --set message="Already refunded" + rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=not_found --set message="No such invoice" + rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=not_paid --set message="Not paid yet" + rust-htmx/command-failure --set service=billing --set command=refund_invoice --set failure=too_late --set message="Too late to refund" --no-fill --human
rust-htmx/command-failure + rust-htmx/command-failure + rust-htmx/command-failure + rust-htmx/command-failure (8 edits, 0 conflicts)
  ~ billing/billing-service/src/refund_invoice.rs (+2 lines at line 27)  [rust-htmx/command-failure (step 1)]
  ~ billing/billing-service/src/refund_invoice.rs (+2 lines at line 29)  [rust-htmx/command-failure (step 2)]
  ~ billing/billing-service/src/refund_invoice.rs (+2 lines at line 31)  [rust-htmx/command-failure (step 3)]
  ~ billing/billing-service/src/refund_invoice.rs (+2 lines at line 33)  [rust-htmx/command-failure (step 4)]
  ~ billing/billing-service/src/refund_invoice.rs (+1 line at line 42, compiler: status)  [rust-htmx/command-failure (step 3)]
  ~ billing/billing-service/src/refund_invoice.rs (+1 line at line 42, compiler: status)  [rust-htmx/command-failure (step 2)]
  ~ billing/billing-service/src/refund_invoice.rs (+1 line at line 42, compiler: status)  [rust-htmx/command-failure (step 1)]
  ~ billing/billing-service/src/refund_invoice.rs (+1 line at line 45, compiler: status)  [rust-htmx/command-failure (step 4)]
holes:
  status[billing/billing-service/src/refund_invoice.rs:41] (expr) billing/billing-service/src/refund_invoice.rs:41  skipped: --no-fill
  status[billing/billing-service/src/refund_invoice.rs:42] (expr) billing/billing-service/src/refund_invoice.rs:42  skipped: --no-fill
  status[billing/billing-service/src/refund_invoice.rs:43] (expr) billing/billing-service/src/refund_invoice.rs:43  skipped: --no-fill
  status[billing/billing-service/src/refund_invoice.rs:44] (expr) billing/billing-service/src/refund_invoice.rs:44  skipped: --no-fill
```

The first round placed the three rustc named (all at line 42, bottom-up); the second found
`TooLate` (line 45). The four holes share a name, so each gets its file and line. It
compiles. With a model configured and a `status()` whose only arm had no comma, the same
chain gave:

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

The last arm got its comma before the new arms went in.

Only `match` arms are added. A refutable `let` (rustc's E0005) is a plan warning, as GHC's
pattern bindings are. Give the fixture's `RefundInvoiceFailure` a `detail()` that binds with
`let RefundInvoiceFailure::Unknown(detail) = self;` (irrefutable while `Unknown` is the only
variant), and the same run warns:

```
  ? warning: rust-htmx/command-failure: billing/billing-service/src/refund_invoice.rs (line 40): a `let` binding needs a case for &RefundInvoiceFailure::AlreadyRefunded, which olifant cannot extend yet
```

Treat the `todo!()`s as must-fix, like `Debug.todo`: they compile, and panic if the arm runs.
`grep -rn 'olifant:'` finds them, whether a todo branch or an unfilled hole left them.

## When Olifant refuses to guess

- **Two candidates.** Elm prints constructors unqualified. If a site could belong to two
  of the generator's constructors (say `Route.Settings` and a `Page` constructor
  `Settings`) and no `where` decides, the run is a conflict naming both. It first rules
  out constructors a file cannot see: `Route.elm` cannot import `Main`, so `Main.Settings`
  is never a candidate there. (That's why `elm/page` writes `complete = "Main.{= P =}"`.)
- **The project does not compile.** A broken module hides the missing cases of every
  module that imports it. If a file Olifant changed is broken, that is a conflict ("the
  generated code does not compile"); if some other file is broken, the plan applies but
  carries a warning that cases could not be checked. Fix the error and run again.
- **Catch-alls hide sites.** A `_ ->` branch means the compiler never complains, so no
  branch is added there. That is why `elm/page` places its `update` branch with an `at`
  phrase: `Main.update` ends in `( _, _ ) ->`. A pack can adopt "no catch-alls" as a
  convention precisely so the compiler can guide its generators.

## What it needs

`elm` on your PATH, and a language adapter with a `[check]` command (the Elm pack has
one). Without `elm` the `complete` edits are a conflict that says so. Checking costs one
`elm make` of the project per round, a second or two for a small app. The same goes for
`cabal` (GHC) and `cargo` (Rust). cargo builds into a cache directory that outlives the
check's scratch copy (`~/.cache/olifant/check/rust`), so only the first round on a machine
compiles the dependencies; after that the `command-failure` plan above takes well under a
second.

## Line numbers

Every edit's line is where its text starts when the plan's edits are applied in order
(`e1`, `e2`, …). Compiler-found branches are applied bottom-up within a file, and numbered
that way, so an earlier line is never shifted by a later edit in the same file.
