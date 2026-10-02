# 2. The Haskell you'll meet here, for a Rust programmer

This is how Olifant loads a generator, reading its templates and exemplars from disk:

```haskell
-- src/Olifant/Catalog.hs:189
loadGenerator :: Monad m => Workspace m -> Catalog -> CatalogEntry -> m (Either CatalogError Generator)
loadGenerator root cat entry = sequenceA <$> traverse resolve (entryDefinition entry)
```

By the end of this chapter that line should read as plain English: "for every template
source inside the generator definition, run `resolve` (which may read a file and may fail).
Then turn the result, a generator full of successes and failures, into either the first
failure or a generator full of templates."

This chapter covers only what Olifant actually uses. There are no type classes of our own,
no GADTs, no lenses, no `mtl`, and only one monad transformer, in one module. Each idiom
comes with a real excerpt and the nearest Rust equivalent. The last section decodes three
real type signatures, so if you want a target first, skip ahead to
[Reading a type signature](#reading-a-type-signature) and come back.

## The compiler settings

Every component in `olifant.cabal` imports the `common shared` stanza:

```cabal
common shared
  default-language:   GHC2021
  default-extensions:
    OverloadedStrings
    LambdaCase
    DerivingStrategies
  ghc-options:        -Wall -Wcompat -Wincomplete-uni-patterns -Wincomplete-record-updates
```

**`GHC2021`** is a language edition, like Rust's `edition = "2021"`. It turns on a set of
extensions that are now uncontroversial. The ones you'll see here:
- `ImportQualifiedPost`: `import Data.Text qualified as T`
- `TupleSections`: `(p,)`
- `ScopedTypeVariables`: `Left (e :: SomeException)` in `app/Main.hs:42`
- `DeriveFunctor`, `DeriveFoldable`, `DeriveTraversable`: used by the trick below

The three extras are:
- **`OverloadedStrings`**: a string literal can be `Text`, `String`, `ByteString`, or an
  aeson key. See [Text vs String](#text-string-and-overloadedstrings).
- **`LambdaCase`**: `\case`, an anonymous function that immediately matches on its argument.
- **`DerivingStrategies`**: you write `deriving stock (Eq, Show)` rather than `deriving (Eq,
  Show)`, which says explicitly that GHC generates the code.

One module adds its own extension: `src/Olifant/Template/Parse.hs:1` has `{-# LANGUAGE
MultiWayIf #-}` for an `if | cond -> … | cond -> …` at `:49`. The spec files add `{-#
OPTIONS_GHC -Wno-incomplete-uni-patterns #-}` so that tests may write `let Right x = …`
(see [Partiality](#partiality-and-error)).

`-Wall` is on, and warnings are treated seriously: a non-exhaustive `case` warns, like
Rust's hard error on non-exhaustive `match`.

## Modules, exports and imports

A module is a file, and its name is its path: `src/Olifant/Generation/Plan.hs` is
`Olifant.Generation.Plan`. The export list is the public API:

```haskell
-- src/Olifant/Catalog.hs:3
module Olifant.Catalog
  ( Catalog
  , catalogEntries
  …
  , CatalogEntry (..)
  , CatalogError (..)
```

- `CatalogEntry (..)` exports the type *and* its constructors and fields, like a `pub
  struct` with `pub` fields.
- `Catalog` without `(..)` exports only the type name. Outside code can hold a `Catalog`
  and use the accessor functions listed separately (`catalogEntries`), but can't build one
  or pattern-match on it. That's a `pub struct` with private fields.
- `src/Olifant/Naming.hs:3` exports `Name` the same way, so the only way to make a `Name` is
  `parseName`. That's a smart constructor, like a Rust newtype with a private field and a
  `fn parse(…) -> Result<Self, _>`.

A facade re-exports whole modules with `module X` in its export list:

```haskell
-- src/Olifant/Language.hs:3
module Olifant.Language
  ( module Olifant.Language.Adapter
  , module Olifant.Language.Rule
  , module Olifant.Language.Syntax
  , module Olifant.Language.Check
  ) where
```

That's Rust's `pub use adapter::*;` in a `mod.rs`.

Imports come in three styles, all visible in `src/Olifant/Generation.hs:21`:

```haskell
import Control.Monad (filterM, foldM, forM_, when) -- named items: use filterM directly
import Data.Map.Strict qualified as M             -- qualified: always M.insert, M.lookup
import Olifant.Generation.Plan                    -- everything the module exports
```

The convention here is to import types unqualified and functions from containers qualified:

```haskell
import Data.Text (Text)
import Data.Text qualified as T
```

You write `Text` in signatures and `T.pack`, `T.strip` in code. `M` is always
`Data.Map.Strict`, `S` is `Data.Set`, `T` is `Data.Text`, and `TE` is
`Data.Text.Encoding`. A bare `import Olifant.Catalog` brings in everything that module
exports, so when you can't find where a name comes from, check the unqualified whole-module
imports.

## Functions, application, `$` and `.`

`f x y` calls `f` with `x` and `y`. There are no parentheses or commas. Parentheses only
group: `render env (genEdits gen)`. Every function is curried, so `render env` on its own is
a function waiting for the template. That's why `traverse (render env) xs` works without a
closure.

- **`f $ x`** is `f (x)`. It saves parentheses that would otherwise wrap everything to the
  right.
- **`f . g`** is composition: `\x -> f (g x)`.
- **`` `elem` ``** in backticks uses a two-argument function as an infix operator: ``p `elem`
  targets``.
- **`(<> ": ")`** is an operator *section*, a partially applied operator: `\x -> x <> ": "`.
  `<>` joins `Text`, lists, and anything else that can be appended.

`app/Main.hs:25` uses all of these at once:

```haskell
    try . runCli
      Runtime
        { … }
      $ args
```

That's `try (runCli Runtime{…} args)`, which runs the CLI and catches any exception. In
Rust, `std::panic::catch_unwind(|| run_cli(runtime, args))` is the closest thing.

## `data`, `newtype`, records

### `newtype`

A `newtype` is a single-field wrapper with no runtime cost, exactly like a Rust tuple struct
`struct GeneratorId(String)`:

```haskell
-- src/Olifant/Generator/Definition.hs:27
newtype GeneratorId = GeneratorId {generatorIdText :: Text}
  deriving stock (Eq, Ord, Show)
```

The first `GeneratorId` names the type and the second names the constructor. They usually
share a name. `generatorIdText` is the accessor, a function `GeneratorId -> Text`.

### Records

A `data` type with named fields is a record:

```haskell
-- src/Olifant/Generation/Plan.hs:36
data PlannedEdit = PlannedEdit
  { editStep :: [Text]
  , editNumber :: Int
  , editPath :: FilePath
  , editAction :: EditAction
  , editAnchor :: Maybe Text
  , editSite :: Maybe Site
  }
  deriving stock (Eq, Show)
```

Three things differ from Rust structs:

1. **Fields are functions.** `editPath e`, not `e.path`. Because they're top-level
   functions, two records in one module can't share a field name. That's why every field
   has a prefix: `editPath`, `planEdits`, `genId`, `entryDefinition`, `rtOut`. The prefix
   tells you the type. `rtOut rt` is the `rtOut` field of a `Runtime`.
2. **Construction can be positional.** `Plan (GeneratorId "sync") [] now edits conflicts []
   writes` (`src/Olifant/Cli.hs:232`) fills `Plan`'s seven fields in declaration order.
   Named construction (`Runtime { rtRoot = …, … }` in `app/Main.hs:26`) also works. To read
   a positional one, open the type definition next to it.
3. **Record update** makes a copy with some fields changed:

   ```haskell
   -- src/Olifant/Cli.hs:99
   Right d -> let e = e0 {entryDefinition = d} in …
   ```

   In Rust that's `CatalogEntry { definition: d, ..e0 }`. It also works on an expression,
   which you'll see in the middle of a line in the planner:

   ```haskell
   -- src/Olifant/Generation/Planner.hs:79
   (planned path n p (Skipped AlreadyPresent)) {editAnchor = Just (locUsed loc)}
   ```

   `planned` (`Plan.hs:49`) is a helper that builds a `PlannedEdit` with no anchor and no
   site. The update sets the anchor.

`-Wincomplete-record-updates` (in the cabal flags) matters for sum types that have fields.
`EditWith` has `editOnExists` only on its `Create` constructor
(`src/Olifant/Generator/Definition.hs:67`), so `x {editOnExists = …}` would crash on an
`Insert`. The flag makes GHC warn about that.

### `type` synonyms

`type` is an alias, not a new type, like Rust's `type`:

```haskell
type GeneratorDef = GeneratorWith SourceRef      -- src/Olifant/Generator/Definition.hs:48
type Env = Map Text Value                        -- src/Olifant/Template/Value.hs:26
type StepPath = [Text]                           -- src/Olifant/Generation/Compose.hs:31
```

`FilePath` is itself a synonym for `String`.

## Sum types and pattern matching

A `data` with `|` is a Rust `enum`:

```haskell
-- src/Olifant/Generation/Plan.hs:54
data EditAction = WillCreate Text | WillOverwrite Text | WillInsert Int Text | WillRefresh Text Int Int Text | Skipped SkipReason | Conflicted
```

`case … of` is `match`:

```haskell
-- src/Olifant/Cli/Output.hs:193
    contentOf = \case
      WillCreate c -> Just c
      WillOverwrite c -> Just c
      WillInsert _ s -> Just s
      WillRefresh _ _ _ c -> Just c
      Skipped _ -> Nothing
      Conflicted -> Nothing
```

**`\case`** (LambdaCase) is shorthand for `\x -> case x of`. It's everywhere, most often
after `>>=` (see [Monads](#monads-and-monad-m-)) and as a whole function body:
`describeConflict = \case …` (`src/Olifant/Generation/Plan.hs:80`) is a function defined
entirely by its match arms.

Functions can also match in their definition, one clause per case, tried top to bottom:

```haskell
-- src/Olifant/Generation/Compose.hs:111
renderRequires :: Env -> Maybe Requires -> Either TemplateError ([FilePath], Maybe Text)
renderRequires _ Nothing = Right ([], Nothing)
renderRequires env (Just r) =
```

Other pattern forms you'll see:

| pattern | example | Rust |
|---|---|---|
| wildcard | `Skipped _` | `Skipped(_)` |
| any fields | `GeneratorAmbiguous {} -> …` (`Cli.hs:192`) | `GeneratorAmbiguous { .. }` |
| as-pattern | `acc@(current, edits, conflicts, n)` (`Planner.hs:39`), `` err@(GeneratorNotFound _ _) `` (`Catalog.hs:290`) | `acc @ (current, ..)` |
| nested | `ScalarInput (EnumQ q valueT Nothing) ->` (`Generator/Choices.hs:22`) | same |
| record | `InputSpec {inputType = ScalarInput (EnumQ _ _ Nothing)}` (`Generator/Choices.hs:32`) | `InputSpec { input_type: …, .. }` |

## Guards, `where`, `let`

**Guards** are conditions on a clause or a match arm. `otherwise` is just `True`:

```haskell
-- src/Olifant/Generator/Inputs.hs:46
resolveInput sets spec = case maybe (inputDefault spec) Just (M.lookup name sets) of
  Just raw -> case parseValue (inputType spec) raw of
    Right (VList []) | not (inputOptional spec) -> Left (InvalidInput name "needs at least one item (it is not optional)")
    parsed -> bimap (InvalidInput name) (name,) parsed
  Nothing
    | inputOptional spec -> Right (name, emptyValue (inputType spec))
    | otherwise -> Left (MissingInput spec)
  where
    name = inputName spec
```

The nested `case` inside the `Just raw ->` branch is a required list's extra rule: an empty
`VList` is fine for an optional input (the `Nothing` branch already handles "not given" the
same way), but a *required* list that was given and parsed to empty — `--set fields=`, say —
fails with its own message instead of silently becoming `[]`.

**Pattern guards** bind while they test. They're like Rust's `if let … && let …` chains:

```haskell
-- src/Olifant/Generation/Planner.hs:46
      PlanEdit path edit
        | Just u <- renderedUnless edit, Just why <- matchUnless (fromMaybe "" (renderedPath edit)) u current ->
            pure (current, planned path n' (fromMaybe "" (renderedPath edit)) (Skipped why) : edits, conflicts, n')
      PlanEdit path (RenderedCreate p content onExists _) -> pure (record n' (decideCreate path n' p content onExists current))
      PlanEdit path (RenderedInsert p anchors snippet _) -> record n' <$> decideInsert path n' p anchors snippet current
```

That reads: "if this edit has an `unless` `u`, *and* `matchUnless` finds a reason `why` to
skip it (a file matching the `exists` glob, or a line of the target matching the `matches`
regex), skip the edit." There's no `| otherwise ->` here: when the guard doesn't hold, Haskell falls
through to the *next* equation for `PlanEdit`, the same way a non-exhaustive guard falls
through to the next `match` arm's own pattern rather than needing a catch-all inside the one
arm. The two equations after it hand a `RenderedCreate`/`RenderedInsert` to its own small
function (`decideCreate`/`decideInsert`, just below), so neither has to pattern-match a
`RenderedComplete` branch it can never actually receive (that case is already spent, two
lines up).

**`where`** attaches helper definitions to a whole function, after its body. They can see
the function's arguments and every guard can use them. `name = inputName spec` above is one.
Big functions here keep their story at the top and their helpers in `where`, often with
their own `where`s. `generate` (`Generation.hs:109`) and `resolveOne`
(`Anchor/Resolve.hs:69`) are good examples. Read the body first, then look helpers up as
they appear.

**`let`** comes in two forms. Inside `do` it's a statement (`let fmt = …`, `Cli.hs:51`).
As an expression it's `let … in …`:

```haskell
-- src/Olifant/Cli/Output.hs:263
whereContext loc content =
  let ls = T.lines content
      i = locLine loc
   in (drop (max 0 (i - 3)) (take i ls), take 3 (drop i ls))
```

Everything is immutable. `let` and `where` name values, they never assign.
"Updating" state means passing a new value along, which is why the planner threads
`current` through a fold instead of mutating a map.

## `Maybe` and `Either`

`Maybe a` is `Option<A>`: `Just x` / `Nothing`. `Either e a` is `Result<A, E>` *with the
type arguments the other way round*: `Left` is the error and `Right` the success, by
convention ("right" as in correct).

The three helpers you'll see most:

| Haskell | Rust | real use |
|---|---|---|
| `fromMaybe d m` | `m.unwrap_or(d)` | `fromMaybe (if rtStdoutIsTty rt then Human else Json) (optFormat opts)` (`Cli.hs:51`) |
| `maybe d f m` | `m.map_or(d, f)` | `now <- maybe (rtNow rt) pure (genNow g)` (`Cli.hs:144`): use `--now` if given, otherwise ask the clock |
| `either f g e` | `match e { Err(x) => f(x), Ok(y) => g(y) }` | `either (const e) (\d -> e {entryDefinition = d}) <$> …` (`Cli.hs:88`): on error keep the entry as it was, on success update it |

`first` from `Data.Bifunctor` changes the error side of an `Either`. It's `map_err`:

```haskell
-- src/Olifant/Generator/Load.hs:40
tmpl :: Text -> Either Text Template
tmpl = first describeTemplateError . parseTemplate
```

That parses a template, and if it fails, turns the `TemplateError` into readable `Text`.
`bimap f g` maps both sides (`Inputs.hs:49`). (`first` also works on pairs, and
`firstPass` in `src/Olifant/Generation.hs:90` is simply a local variable name that happens
to contain the word. Context tells you which.)

### `Either` with `do`: the `?` operator

In a `do` block over `Either`, each `x <- action` either unwraps a `Right` or stops the
whole block with that `Left`. That's Rust's `?`. `pure x` is `Ok(x)`:

```haskell
-- src/Olifant/Generator/Load.hs:27
parseGenerator :: GeneratorId -> Text -> Either Text GeneratorDef
parseGenerator gid src = do
  top <- parseToml src
  onlyKeysWith topAdvice topKeys top
  description <- required "description" text top
  language <- required "language" text top
  version <- maybe 1 fromInteger <$> optional "version" int top
  …
  pure (Generator gid description language version inputs derived requires edits uses examples)
```

In Rust:

```rust
fn parse_generator(gid: GeneratorId, src: &str) -> Result<GeneratorDef, String> {
    let top = parse_toml(src)?;
    only_keys(&["description", …], &top)?;
    let description = required("description", text, &top)?;
    …
    Ok(Generator { … })
}
```

A line with no `<-` (the `onlyKeys` one) runs for its effect, which here is failing or not.
All of `Olifant.Toml`'s decoders (`required`, `optional`, `text`, `list`, `table`) return
`Either Text a`, so every TOML loader reads like this. `buildEnv` (`Inputs.hs:33`) is the
same shape. It ends with `unless (null problems) (Left (InputProblems problems))`, which is
Rust's `if !problems.is_empty() { return Err(…) }`.

## Functor, Applicative, Traversable

These three words describe what you can do with a "container" or an "effect". You only
need them at the level below.

### `fmap`, `<$>`, `<&>`: map over the inside

`fmap f x` applies `f` to what's inside `x` and keeps the wrapper: `Option::map`,
`Result::map`, `Iterator::map`, or `.map` on a future. `<$>` is `fmap` as an operator, and
`<&>` is the same with the arguments flipped:

```haskell
-- src/Olifant/Generation.hs:76
      inspected <- traverse (\p -> (p,) <$> inspectTarget p) (nub (targets ++ required))
```

`inspectTarget p` is an action returning an `Either Text (Maybe Text)`. `(p,) <$>` pairs
its result with `p` without running anything new.

```haskell
-- src/Olifant/Catalog.hs:117
    packsIn base configPath =
      readText root configPath <&> \case
        Nothing -> Right []
        Just src -> …
```

That reads the file, then maps the result with this `\case`. `x <&> \case …` is the usual
way here to post-process an action's result when the function is long.

### `<*>`: build from several fallible parts

`<$>` followed by `<*>` fills a constructor's arguments from several wrapped values:

```haskell
-- src/Olifant/Generation/Render.hs:45
  Create target src onExists u -> RenderedCreate <$> path target <*> render env src <*> pure onExists <*> traverse (traverse (render env)) u
```

Each argument is an `Either TemplateError …`, and the whole expression is `Left` if any of
them is. In Rust:

```rust
Create { target, src, on_exists, unless } => Ok(RenderedCreate(
    path(target)?, render(env, src)?, on_exists,
    unless.map(|u| u.try_map(|t| render(env, t))).transpose()?)),
// where Unless::try_map renders the template inside either variant, Exists or Matches
```

`pure onExists` wraps a plain value so it fits in the chain.

### `traverse`: map with an effect, then collect

`traverse f xs` applies a fallible or effectful `f` to every element and collects the
results *inside* the effect. Over a list with `Either`, it's
`.iter().map(f).collect::<Result<Vec<_>, _>>()`:

```haskell
-- src/Olifant/Generation/Render.hs:43
renderEdits :: Text -> Env -> [EditWith Template] -> Either TemplateError [RenderedEdit]
renderEdits language env = traverse $ \case
```

Render every edit, and fail with the first error.

Over a list with `IO` (or any monad `m`), it runs the actions in order and collects their
results, like a `for` loop that pushes into a `Vec`. `Generation.hs:76` above is one.

Over a `Maybe`: `traverse (render env) m` with `m :: Maybe Template` gives `Either … (Maybe
Text)`. That's `Option::map(…).transpose()`. The `unless` above is one layer deeper,
`u :: Maybe (Unless Template)`, where `data Unless t = UnlessExists t | UnlessMatches t`
(`Generator/Definition.hs:81`) derives `Traversable` too. So `traverse (traverse (render
env)) u`: the inner `traverse` renders the template inside whichever `Unless` it is, the
outer one handles the `Maybe`, and the result is `Either … (Maybe (Unless Text))`.

### `sequence` / `sequenceA`: flip the layers

`sequence` turns "a list of results" into "a result of a list": `[Either e a]` becomes
`Either e [a]`. It's `collect::<Result<Vec<_>,_>>()` on values you already have:

```haskell
-- src/Olifant/Cli.hs:139
        case sequence loaded of
          Left (step, err) -> catalogFailureAt rt fmt (if length ss > 1 then Just step else Nothing) err
          Right gens0 -> fmap sequence (traverse (resolveChoicesFor rt) gens0) >>= \case
```

The second line stacks both. `traverse (resolveChoicesFor rt) gens0` runs an `IO (Either
Text Generator)` action for each generator, giving `IO [Either Text Generator]`.
`fmap sequence` then flips the inner list, giving `IO (Either Text [Generator])`.
`sequenceA` is the same thing and needs slightly less of the wrapper type. Here the two are
interchangeable.

### The Traversable trick: same shape, sources then templates

A generator exists in two states. Read from `generator.toml`, its edit sources are
*references*: a template file name, an exemplar path, or inline text. Ready to run, they're
*parsed templates*. Olifant uses one type for both, parameterised by the source type:

```haskell
-- src/Olifant/Generator/Definition.hs:30
-- | A generator whose edit sources are @src@: file references when loaded
-- from TOML ('GeneratorDef'), parsed templates once resolved ('Generator').
data GeneratorWith src = Generator
  { genId :: GeneratorId
  , genDescription :: Text
  …
  , genEdits :: [EditWith src]
  , genUses :: [UseStep]
  , genExamples :: [Text] -- ^ example commands, shown by `olifant show`
  }
  deriving stock (Show, Functor, Foldable, Traversable)

type GeneratorDef = GeneratorWith SourceRef

type Generator = GeneratorWith Template
```

`EditWith src` (`:65`) likewise has `editSource :: src` and derives the same three classes.
Deriving `Traversable` makes GHC write `traverse` for the type. It visits every field whose
type mentions `src`, which here means each edit's source, and leaves every other field
alone. So:

- `fmap f :: GeneratorWith a -> GeneratorWith b` changes every source with a pure function.
  Tests use this to build generators from inline text:
  `either (error . T.unpack) (fmap inlineOnly) (parseGenerator …)` (`GenerationSpec.hs:40`).
- `traverse resolve` does the same with an effectful, fallible function. With `resolve ::
  SourceRef -> m (Either CatalogError Template)`, `traverse resolve def` has type `m
  (GeneratorWith (Either CatalogError Template))`: one action that resolves every source,
  giving a generator whose sources are each a success or a failure.
- `sequenceA <$>` then flips `GeneratorWith (Either e Template)` into `Either e
  (GeneratorWith Template)`: the first failure, or a runnable generator.

That's the line this chapter opened with:

```haskell
loadGenerator root cat entry = sequenceA <$> traverse resolve (entryDefinition entry)
```

In Rust you would write `impl<S> Generator<S> { fn try_map_sources<T, E>(self, f: impl FnMut(S)
-> Result<T, E>) -> Result<Generator<T>, E> }` by hand, and an async version too. Here the
compiler writes it, and it can't forget a field.

Why bother? Because `Generator` can't hold an unresolved file reference: the types rule it
out. And `olifant show` can describe a `GeneratorDef` without loading any templates.
Regions use the same trick. `RegionDefWith src` (`src/Olifant/Region.hs:29`) goes from
`FilePath` (as parsed) to `Template` in `Catalog.regionsIn`:

```haskell
-- src/Olifant/Catalog.hs:308
        resolved <- traverse (fmap sequenceA . traverse template) defs
        pure (first (path,) (sequence resolved))
```

For each region definition, `traverse template` reads its template file, and `sequenceA`
flips the inside. Then `sequence` flips the whole list, and `first (path,)` tags any error
with the file it came from.

## Monads and `Monad m =>`

### `do` in `IO`

`IO a` is an action that, when run, does I/O and yields an `a`. Only `main` actually runs
one. A `do` block in `IO` reads like ordinary imperative code:

```haskell
-- app/Main.hs:17
main = do
  args <- getArgs
  cwd <- getCurrentDirectory
  tty <- hIsTerminalDevice stdout
```

`x <- action` runs the action and names its result. That's `let x = action.await;` in async
Rust, and the analogy holds up well. `pure x` wraps a value without doing anything, and
it's how you "return" from a `do` block. It is *not* an early return: `pure` in the middle
of a block just produces a value that is ignored.

`>>=` ("bind") is what `<-` desugars to: `action >>= f` runs `action` and passes its result
to `f`. You'll see it written out when the next step is a match:

```haskell
-- src/Olifant/Cli.hs:55
    openProjectCatalog (rtRoot rt') (CatalogPaths …) >>= \case
      Left err -> catalogFailure rt' fmt err
      Right cat -> runCommand rt' cwd fmt cat (optCommand opts)
```

In Rust: `match open_project_catalog(…).await { Err(e) => …, Ok(cat) => … }`. `a >> b` runs
`a` and discards its result, then runs `b`, like `a; b`:
`rtOut rt text >> pure ExitSuccess` (`Cli.hs:46`).

### `Monad m =>`: code generic over the effect

The core functions don't say `IO`. They say "some monad `m`":

```haskell
-- src/Olifant/Generation.hs:69
generate :: Monad m => Workspace m -> Resolve m -> Tools m -> [Step] -> UTCTime -> Mode -> m (Either GenerationError Plan)
```

`Monad m =>` is a constraint, like a trait bound: `fn generate<M: Monad>(…)`. It means
`generate` can use `do`, `<-` and `pure`, but nothing specific to `IO`: no files, no
processes, no clock. It can only reach the outside world through the functions in the
`Workspace m`, `Resolve m` and `Tools m` it was given, and those decide what `m` is.

This is why the ports are parameterised by `m`. Think of Rust's sync/async split, where the
same logic has to be written twice because async is a different "colour" of function.
Haskell can abstract over the colour: `generate` is written once, and the caller decides
whether `m` is `IO` or something else. In production every port is an `IO` adapter, so
`m = IO`. In tests:

- `memoryWorkspace` gives a `Workspace IO` backed by an `IORef` (a mutable cell, like
  `RefCell`), so most tests still run in `IO` but never touch the disk.
- Where a fake needs no state at all, the tests pick `m = Identity`, the "no effect" monad
  whose `do` is just ordinary evaluation:

  ```haskell
  -- src/Olifant/AnchorSpec.hs:58
  fakeSyntax :: Syntax Identity
  fakeSyntax = Syntax $ \_ rule _ -> Identity . Right $
  ```

  ```haskell
  -- src/Olifant/AnchorSpec.hs:83
      let resolve anchors = runIdentity (resolveLayers fakeSyntax (Just caseTarget) anchors source)
  ```

  `runIdentity` unwraps the result. The test has no `IO` at all, and the type guarantees
  that `resolveLayers` did nothing but compute.

A no-op adapter only needs `pure`, so it asks for less than `Monad`. `Applicative m =>` is
enough:

```haskell
-- src/Olifant/Language/Syntax.hs:17
noSyntax :: Applicative m => Syntax m
noSyntax = Syntax (\_ _ _ -> pure (Left (SyntaxToolMissing "ast-grep is not installed (brew install ast-grep)")))
```

### Loops: `foldM`, `mapM_`, `forM_`, `when`, `unless`

There are no `for` loops. The monadic list functions play that role:

| Haskell | Rust | example |
|---|---|---|
| `mapM_ f xs` / `forM_ xs f` | `for x in xs { f(x).await; }` | `forM_ (M.toList writes) (uncurry (writeText ws))` (`Generation.hs:107`) |
| `when c a` / `unless c a` | `if c { a.await }` / `if !c { … }` | `when (mode == Apply && not (hasConflicts plan)) $ …` (`Generation.hs:106`) |
| `filterM p xs` | filter with an async predicate | `filterM (fmap isFile . inspectPath ws) (parents p)` (`Generation.hs:143`) |
| `zipWithM f xs ys` | `for (x, y) in zip { v.push(f(x, y).await) }` | `zipWithM (expandUse env) [1 :: Int ..] (genUses gen)` (`Compose.hs:77`) |
| `foldM f acc xs` | `for x in xs { acc = f(acc, x).await; }` | below |

`foldM` is the workhorse. The planner is one:

```haskell
-- src/Olifant/Generation/Planner.hs:35
decideItems :: Monad m => Locate m -> [FilePath] -> [PlanItem] -> Map FilePath (Maybe Text) -> Map FilePath Text -> Set FilePath -> m ([PlannedEdit], [Conflict], Map FilePath (Maybe Text), Int)
decideItems locate diskPaths items original blocked onDisk = finish <$> foldM step (original, [], [], 0 :: Int) items
  where
    finish (current, edits, conflicts, n) = (reverse edits, reverse conflicts, current, n + 1)
    step acc@(current, edits, conflicts, n) = let n' = n + 1 in \case
```

The accumulator is a 4-tuple: file contents so far, edits and conflicts (built backwards by
`:` and reversed at the end, because prepending to a linked list is cheap), and the edit
counter. `foldM` over `Either` instead of `IO` is `try_fold`: `foldM step env ready`
(`src/Olifant/Generator/Inputs.hs:136`) stops at the first `Left`.

`[1 :: Int ..]` is an infinite list 1, 2, 3, …. It's fine, because `zip` and `zipWithM`
stop at the shorter list and lists are lazy. Numbering things with `zip [1 ..] xs` is the
idiom for Rust's `.enumerate()` (starting at 1).

## `ExceptT`: `?` inside another monad

`expand` has to do two kinds of thing at once. It calls `Resolve m`, which runs in `m`
(disk, in production). It also fails early on bad inputs, a template error or a cycle. The
code could nest `m (Either e a)` by hand everywhere, or it can use `ExceptT e m a`: "an `m`
action that can also fail with `e`". This is the only module that does.

```haskell
-- src/Olifant/Generation/Compose.hs:69
expand :: Monad m => Resolve m -> UTCTime -> [GeneratorId] -> StepPath -> Generator -> Map Text Text -> ExceptT CompositionError m [PlanItem]
expand resolve now ancestors path gen sets = do
  when (genId gen `elem` ancestors) $ throwE (StepCycle path)
  checkCycles resolve (genId gen : ancestors) path gen
  env <- except (first (StepInputs path) (buildEnv gen sets now))
  …
    expandUse env k u = do
      childSets <- except (first (StepTemplate path) (renderUse env u))
      child <- ExceptT (first (failure (useGenerator u)) <$> resolve (packOf (genId gen)) (useGenerator u))
```

The vocabulary, with async Rust `-> Result<_, CompositionError>` as the analogy:

| `ExceptT` | meaning | Rust inside `async fn … -> Result<T, E>` |
|---|---|---|
| `throwE e` | fail now | `return Err(e)` |
| `except r` (`r :: Either e a`) | use a pure result | `r?` |
| `ExceptT act` (`act :: m (Either e a)`) | use an effectful result | `act.await?` |
| `lift act` (`act :: m a`) | run an action that can't fail | `act.await` |
| `runExceptT x` | back to `m (Either e a)` | calling the function |

`checkCycles` (`Compose.hs:99`) shows `lift`: `found <- lift (resolve …)` gets the
`Either` as a plain value and inspects it instead of failing on it. `expandSteps` (`:61`)
calls `runExceptT` once at the top, and nothing outside `Compose` ever sees `ExceptT`.

## `Map` and `Set`

`Data.Map.Strict` is an ordered map (a balanced tree), like `BTreeMap`. `Data.Set` is
`BTreeSet`. Because they're ordered, `M.toList` and `M.keys` come out sorted, which Olifant
relies on for deterministic output. Both are immutable: `M.insert k v m` returns a new map
and shares most of the structure with the old one.

The calls you'll see:
- `M.fromList`, `M.toList`, `M.lookup` (returns `Maybe`), `M.insert`
- `M.findWithDefault d k m` (`.get(k).unwrap_or(d)`)
- `M.member`, `M.union` (left side wins)
- `M.difference`, `M.intersectionWith`, both used for snapshot comparison in
  `GeneratorTesting.hs:107`
- `S.fromList`, `S.member`, `S.toList`

The planner's central type is `Map FilePath (Maybe Text)`. A path maps to `Just` its
contents, or to `Nothing` for "absent" (not yet created, or pretended absent for the re-run
pass). So `M.lookup p m :: Maybe (Maybe Text)` has three cases: not tracked, tracked but
absent, present. That's why you see comparisons like `M.lookup p originals /= Just (Just c)`
(`Generation.hs:104`).

`nub` (from `Data.List`) removes duplicates from a list, keeping the first. It is O(n²), so
large lists go through a `Set` instead. `Generation/Regions.hs:34` says so:

```haskell
      -- A set, not `nub`: projects can hold hundreds of thousands of files.
      let universe = S.toList (S.fromList (disk ++ [p | (p, Just _) <- M.toList current0]))
```

## Text, String and OverloadedStrings

There are three string types in play:

- **`String`** is `[Char]`, a linked list of characters. It's slow, but it's what the
  standard library, `getArgs` and `System.FilePath` use. `FilePath` is `String`, so paths
  are `String` throughout Olifant.
- **`Text`** is a packed UTF-8 string, like Rust's `String`. File contents, messages and
  template values are all `Text`.
- **`ByteString`** is raw bytes, like `Vec<u8>`. It's used for process I/O (`runBytes`,
  `src/Olifant/Process.hs:18`) and decoded with `TE.decodeUtf8'`.

`T.pack :: String -> Text` and `T.unpack :: Text -> String` convert between them. You'll
see them wherever a path goes into a message (`T.pack path`) or rendered text becomes a
path (`normalisePath . T.unpack`).

With **`OverloadedStrings`** a literal like `"src"` takes whatever string type is expected,
whether `Text`, `String`, `ByteString` or an aeson `Key`. Occasionally nothing fixes the
type, and you'll see an annotation. aeson's `.=` accepts any JSON-able value, so:

```haskell
-- src/Olifant/Cli/Output.hs:156
      WillCreate c -> ["status" .= ("create" :: Text)] ++ content c
```

## List comprehensions

`[expr | generators and filters]` is Python-style, and more common here than
`map`/`filter` chains:

```haskell
-- src/Olifant/Generation.hs:74
      let targets = nub [p | PlanEdit _ e <- items, Just p <- [renderedPath e]]
```

That reads: for each item that is a `PlanEdit` (other items are skipped, because a failed
pattern match in a comprehension just skips), take its edit `e`. If `renderedPath e` is
`Just p`, keep `p`.

`Just p <- [x]` is **the** idiom to learn. It wraps a `Maybe` in a one-element list so the
pattern can filter it. Together that's `filter_map`. In Rust:

```rust
items.iter().filter_map(|i| match i { PlanEdit(_, e) => rendered_path(e), _ => None })
```

The same idiom picks out "edits that have a site that is a todo" in `Cli/Output.hs:178`:
`[… | e <- planEdits plan, Just s <- [editSite e], siteTodo s]`.

## Tuples and `(p,)`

Tuples are Rust tuples: `(a, b)`. `fst` and `snd` take the first and second of a pair.
`TupleSections` lets you leave a slot open, so `(p,)` is `\x -> (p, x)` and `(,) a b` is
`(a, b)`. With `<$>` it attaches a key to an action's result:
`(p,) <$> inspectTarget p` gives `(p, result)`. `uncurry f (a, b)` is `f a b`. Functions
with several results return tuples, like `decideItems`' 4-tuple above.

## Type classes you'll meet (and the ones we don't write)

Olifant defines no type classes of its own. It *derives* the standard ones:
- `Eq`: `==`, like `PartialEq`
- `Ord`: needed to be a `Map` key
- `Show`: `show` gives a debug string, like `Debug`, which the tests use for failure
  messages
- `Enum`, `Bounded`: `[minBound .. maxBound]` lists every constructor, as in `allCases`,
  `src/Olifant/Naming.hs:30`

There are two hand-written instances, both for aeson's `FromJSON` (like implementing
serde's `Deserialize` by hand): `StepSpec` (`src/Olifant/Cli/Options.hs:44`) and `Found`
(`src/Olifant/Language/AstGrep.hs:24`).

## Partiality and `error`

`error "…"` is `panic!`. Olifant's library code avoids it entirely — there's no `error "…"`
anywhere under `src/Olifant/` outside the `*Spec.hs` files. Failures are values (`Either`,
`Conflict`, `Miss`), and `app/Main.hs` catches anything that escapes as an internal error.

That wasn't always true: `branchFor` (`Generation/Complete.hs:100`) used to pick its first
candidate with a local `head'` that called `error` on an empty list, trusting a comment that
said the list was never empty "by construction" — true, but not something the type checker
could see. The caller now only ever builds a `NonEmpty` list of candidates in the first
place (`Data.List.NonEmpty.nonEmpty` in a list comprehension's guard, `Complete.hs:71`), so
`branchFor` takes a `NonEmpty` and reaches for `NE.head`, a *total* function — the type
itself says "this is never empty", and the `head'`/comment pair is gone. This is the general
move for turning "trust me" into something GHC checks: find the type that makes the
impossible case unrepresentable, and push it as far upstream (to where the list is *built*,
not where it's *read*) as it will go.

Tests are looser: `let Right entry = findEntry cat ref` (`CatalogSpec.hs:49`) is an
`unwrap()`, allowed by the spec files' `-Wno-incomplete-uni-patterns`.

## Tests: hspec, hspec-discover, Hedgehog

Every unit's tests live next to it: `src/Olifant/Naming.hs` is tested by
`src/Olifant/NamingSpec.hs`. A spec module exports one value, `spec`:

```haskell
-- src/Olifant/NamingSpec.hs:16
spec :: Spec
spec = do
  describe "parseName" $ do
    it "reads every supported case as the same words" $
      map (fmap nameWords . parseName) ["blog_post", "blog-post", "BlogPost", "blogPost", "BLOG_POST", "Blog Post", "blog post"]
        `shouldBe` replicate 7 (Right ["blog", "post"])
```

- **hspec** gives `describe`, `it`, and assertions: `shouldBe` (`assert_eq!`),
  `shouldSatisfy x p` (`assert!(p(x))`), `shouldReturn` (run an `IO`, compare its result),
  `expectationFailure` (`panic!` with a message), and `pendingWith` (skip with a reason).
  The pack tests use `pendingWith` when `elm`, `ast-grep` or `cargo` isn't installed.
- **hspec-discover** finds the specs. `src/TestDriver.hs` is a single line, `{-# OPTIONS_GHC
  -F -pgmF hspec-discover #-}`, which runs a preprocessor that generates a `main` calling
  every `*Spec` module's `spec` wrapped in `describe "<module minus Spec>"`. That's why
  `--match "Olifant.Naming"` selects one unit. There is no list of tests to maintain, only
  the `test-modules` region in `olifant.cabal`, which Olifant maintains itself.
- **Hedgehog** does property tests, like `proptest`. `hspec-hedgehog`'s `hedgehog` turns a
  property into an `it`:

  ```haskell
  -- src/Olifant/NamingSpec.hs:35
      it "round-trips through every case" $ hedgehog $ do
        ws <- forAll (Gen.list (Range.linear 1 4) (Gen.text (Range.linear 2 8) Gen.lower))
        c <- forAll (Gen.element allCases)
        let n = name (T.intercalate "_" ws)
        parseName (render c n) === Right n
  ```

  `forAll gen` draws a random value (and shrinks it on failure), and `===` is the
  assertion. `evalIO` runs an `IO` action inside a property. `GenerationSpec.hs:243` uses it
  to run `generate` against a memory workspace.

The test suite compiles `src/` directly (`hs-source-dirs: src` in the `test-suite` stanza).
Specs can therefore import internal modules when they need to: `LanguageSpec` imports
`Olifant.Language.Elm` to test the report parser against real `elm` output. Most specs go
through the facade.

## Cabal: the commands you need

Run these from the repo root, with GHC on your `PATH` (`export
PATH="$HOME/.ghcup/bin:$HOME/.local/bin:$PATH"`, which the scripts also do):

```sh
cabal build                                           # library + executable (+ tests: cabal.project has tests: True)
cabal test                                            # the whole suite: every pack's snapshot tests, and the Elm and Rust compile checks
cabal test --test-show-details=direct \
           --test-options='--match "Olifant.Naming"'  # one unit, with each example printed
OLIFANT_BLESS=1 cabal test                            # rewrite pack snapshots from current output
cabal run olifant -- gen elm/page --plan …            # run the working tree's olifant (args after --)
cabal list-bin olifant                                # where that binary is
scripts/test-watch.sh --match "Olifant.Anchor"        # ghcid: re-run specs on every save
cabal repl test:spec                                  # a GHCi session with everything loaded
```

`cabal` is `cargo`. `olifant.cabal` is `Cargo.toml`, `cabal.project` is the workspace
file, and `dist-newstyle/` is `target/`. Dependencies are declared in the `common shared`
stanza, so every component gets all of them.

## Reading a type signature

Three signatures from the code, decoded. The method is always the same: constraints before
`=>`, then arguments separated by `->`, and the last type is the result. Anything in the
result that isn't a plain type (`m (…)`, `Either …`) tells you how the function can fail or
what it can do.

### `generate`

```haskell
-- src/Olifant/Generation.hs:69
generate :: Monad m => Workspace m -> Resolve m -> Tools m -> [Step] -> UTCTime -> Mode -> m (Either GenerationError Plan)
```

"For any monad `m`, given:
- a workspace in `m`
- a way to resolve `[[uses]]` names in `m`
- tools in `m`
- the steps
- the time
- plan-only or apply

`generate` returns an `m` action. Running it yields either a `GenerationError` (the run
couldn't be planned at all: bad inputs, a missing generator, a cycle) or a `Plan`."

Two things follow from the types alone:
- Conflicts are *not* in the `Left`. A plan with conflicts is still a `Right Plan`, and
  `hasConflicts` tells them apart.
- The only way `generate` can write is through `writeText` in the `Workspace m` you gave
  it.

In Rust: `async fn generate<M>(ws: &Workspace<M>, resolve: &Resolve<M>, tools: &Tools<M>,
steps: &[Step], now: DateTime<Utc>, mode: Mode) -> Result<Plan, GenerationError>`.

### `resolveLayers`

```haskell
-- src/Olifant/Anchor/Resolve.hs:60
resolveLayers :: Monad m => Syntax m -> Maybe Language -> [RenderedAnchor] -> Text -> m (Either [Miss] Located)
```

"Given:
- a syntax port
- maybe a language adapter (`Nothing` when no adapter claims the file)
- a list of anchors to try in order
- the file's text

it returns an action yielding either *every* miss (one per anchor, so the error can
explain each) or the one `Located` place to insert."

The `Maybe Language` tells you phrases that don't need a language (markers, line regexes)
still work for unknown file types. The list-of-misses error tells you it tries all anchors
before giving up.

### `completeEdits`

```haskell
-- src/Olifant/Generation/Complete.hs:38
completeEdits :: Monad m => Tools m -> Workspace m -> [(StepPath, CompleteSpec)] -> Int -> Map FilePath (Maybe Text) -> m ([PlannedEdit], [Conflict], [Text], Map FilePath (Maybe Text))
```

"Given:
- the tools (for the checker and adapters)
- the workspace (to list and read the project's sources)
- the `complete` specs, each tagged with the step that asked for it
- the number to give the first new edit
- the file contents so far

it returns an action yielding four things: the edits it added, conflicts, warnings, and the
file contents after them."

The tuple result is the phase shape. The same contents map goes in and comes out, which is
how `generate` chains the phases. Conflicts and warnings are returned as data, never thrown,
so a failed check becomes a `CompleteFailed` conflict in the plan rather than an error.
There's no `Either` in the result at all: `completeEdits` can't fail as a function, it can
only report.

Next: [chapter 3](README.md) (planned), the template engine.
