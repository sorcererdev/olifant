# 3. Where code goes: `at` phrases

An `insert` edit says where its text goes with a short phrase:

```toml
[[edits]]
insert = "src/Main.elm"
at     = "end of type Page"
text   = "| {= P =} {= P =}.Model"
```

Olifant parses `src/Main.elm`, finds `type Page`, and puts the new line after its last
variant, indented like that variant. Nothing in your app marks the spot.

## Try a phrase: `olifant where`

```
$ olifant where src/Main.elm "end of case in update"
src/Main.elm:76

            ( GotSamplePageMsg subMsg, SamplePage page ) ->
                SamplePage.update subMsg page |> updateWith SamplePage GotSamplePageMsg model
  ▶         (inserted here)

            ( _, _ ) ->
                ( model, Cmd.none )
```

Note what it did: it went after the last *real* branch, before the catch-all `( _, _ )`,
and it skipped the `case urlRequest of` nested inside the first branch. That is the
difference between a structural anchor and "the last line matching some text".

## The grammar: `<position> <target>`

| position | means |
|---|---|
| `end of X` | after X's last item |
| `start of X` | before X's first item |
| `after X` | after the whole of X |
| `before X` | before the whole of X |

Targets come from the file's **language adapter**. `olifant anchors elm` lists them:

```
$ olifant anchors elm
elm: at = "<start of|end of|before|after> <target>"

targets:
  case in FUNCTION        e.g. end of case in update
  imports                 e.g. end of imports
  list in FUNCTION        e.g. end of list in parser
  record NAME             e.g. end of record Model
  type NAME               e.g. end of type Msg

built in (any file):
  before|after marker TEXT
  before|after first line matching REGEX
  before|after last line matching REGEX
```

Words in capitals are filled in by you (`case in update`, `type Route`). The built-in
targets work in any file, even one with no adapter:

- `after last line matching ^import Page\.` (a POSIX regular expression per line)
- `before marker -- TODO: routes` (an exact comment line, if you have one)

Mistakes are caught with a suggestion:

```
$ olifant where src/Main.elm "end of cases in update"
error: `end of cases in update`: unknown target `cases in update` for elm; did you mean `end of case in update`? (targets: case in FUNCTION, imports, list in FUNCTION, record NAME, type NAME)
```

## Fallbacks

`at` can be a list, tried in order; the first that resolves wins and the plan records
which one (`anchor_used`):

```toml
at = ["end of type Page", "before marker -- pages"]
```

Use this when a structural target needs a tool (below) that might not be installed.

## Rules worth knowing

- **Whole lines.** Olifant inserts whole lines, so the spot must own its line. With
  `oneOf [ a, b ]` written on one line, `end of list in parser` refuses ("the last item
  shares its line with other code") instead of producing broken code. A separator such as
  `,` or `|` may share the line, so Elm's leading-comma lists work.
- **Exactly one.** A target must match once. If `update` had two top-level `case`s,
  `case in update` would be ambiguous and the edit a conflict.
- **Already there.** If the text is already at that spot (or just beside it), the edit is
  skipped, so re-runs are no-ops.
- **Tools.** Language targets need `ast-grep` (and a C compiler, the first time, to build
  the Elm grammar). Without them the edit misses with "ast-grep is not installed (brew
  install ast-grep)" and the next fallback is tried. Line and marker targets need nothing.

## Multi-line `text`

TOML drops the newline right after an opening `"""`. To start an insert with a blank line
(to separate it from the previous `case` branch), leave an empty line:

```toml
text = """

( Got{= P =}Msg subMsg, {= P =} page ) ->
    {= P =}.update subMsg page |> updateWith {= P =} Got{= P =}Msg model"""
```

`complete` edits ([next chapter](04-compiler-branches-and-todos.md)) add that separator
themselves.
