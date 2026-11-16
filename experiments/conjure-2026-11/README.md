# Conjure 2026-11: one step, three prompt shapes, four writers (2026-11-15)

Not product code. Measures which writer and prompt shape give the best forward rate per dollar and
second on cursor situations in the bistro demo (brief `notes/durandal/conjure-measure-brief-2026-11-15.md`,
write-up `notes/durandal/experiments/conjure-2026-11-2026-11-15.md`).

- `make_cases.py` builds the 14 case trees from the demo at the `fill` stage and writes `cases.json`
  (id, kind, file, line, body span, reference next step). Add a situation by adding a row to `CASES`.
- `Conjure.hs` runs each case x writer x shape (A today's prompt via the engine's `conjureSystemPrompt`,
  `userPrompt` and a copy of `functionConjure`; B strategist then writer; C writer states the goal and
  takes it), applies the step in a copy, runs every test file apart with the engine's runner (`runOver`),
  checks `ast.parse`, measures scope (`git diff`), then (`judge`) asks `claude-opus-5-5` for a blind
  yes/no per run.
- `replies.json`: every model reply keyed `VARIANT|MODEL|CASE` (`STRATEGIST|...`, `JUDGE|...`), with
  cost (`total_cost_usd`), seconds and, for the local model, Ollama's peak RSS. `results.json`: the cases,
  each run's facts and diff, the verdicts. `summarise.py` prints the tables in the write-up.

Run from the olifant checkout (recordings are relative to it):

```
SP=<scratch dir>; export OLIFANT=$(cabal list-bin olifant)
bash scripts/slice-python.sh $SP/demo gens && bash scripts/slice-python.sh $SP/demo behaviour && bash scripts/slice-python.sh $SP/demo fill
python3 experiments/conjure-2026-11/make_cases.py $SP/demo/bistro $SP/trees
cabal build lib:olifant
cabal exec -- ghc -package olifant -package-id <aeson id> -package-id <scientific id> -package bytestring \
  -package containers -package process -package directory -package filepath -package text -package time \
  -XGHC2021 -O0 -outputdir $SP/build -o $SP/conjure experiments/conjure-2026-11/Conjure.hs
export CONJURE_TREES=$SP/trees CONJURE_WORK=$SP/work
CONJURE_LIVE=1 perl -e 'alarm 590; exec @ARGV' $SP/conjure write claude-haiku-5-5   # then claude-sonnet-5-5,
CONJURE_LIVE=1 perl -e 'alarm 590; exec @ARGV' $SP/conjure write qwen3-coder:30b    # claude-sonnet-5-5@low; local last
CONJURE_LIVE=1 perl -e 'alarm 590; exec @ARGV' $SP/conjure judge
python3 experiments/conjure-2026-11/summarise.py
```

(aeson and scientific have two versions in the package db: take the ids from `dist-newstyle/cache/plan.json`,
`sn-...` and `scntfc-...`.) A run already in `results.json` is kept, so an interrupted chunk resumes.
Without `CONJURE_LIVE=1` every reply replays from `replies.json` and an unrecorded one fails: delete
`results.json` and rerun the same lines to replay offline. `CONJURE_CASES=a,b` limits a run to those cases;
the judge replays only over the same set of runs it judged (its labels follow the set). Live, a call is
refused once the recorded cost passes `CONJURE_CAP` (default $8).

- Claude is called the way `src/Olifant/Server/Errand.hs` does (`claude -p --output-format json --model M
  --permission-prompts none`, alarm 420), plus `--system-prompt` (the shape's own), `--tools ""`,
  `--no-session-persistence`, `--setting-sources ""` and `CLAUDE_CODE_DISABLE_CLAUDE_MDS=1`, so the global
  CLAUDE.md and plugin hooks are not sent with the prompt. `@low` adds `--effort low`.
- The local model is called on Ollama's OpenAI-compatible endpoint with the body `chatBody` sends
  (temperature 0), under `perl -e 'alarm 90'`. Peak RSS is sampled from `ps` (Ollama's `llama-server`)
  every half second: `/usr/bin/time -l` on curl would measure curl, not the model.
- A's fill is read by the engine's `decodeFills` and placed by its `fillLines`, with the fill phase's
  `reindented` rescue when the placed file does not parse. A on a module with no test file is recorded
  as today's refusal ("no test file for this module yet"), not asked.
