# Vet 1a: the claim-check harness (2026-11-15)

Not product code. `ClaimCheck.hs` runs the engine's own `checkSentence` (the plans cascade: haiku,
Jev guards, sonnet on doubt) on code-review findings, for the vet milestone 1a measurements
(`notes/durandal/experiments/vet-1a-2026-11-15/`, plan `notes/durandal/code-checker-1a-plan-2026-11-15.md`).

```
IN.json   [{"id", "tree", "file", "line", "statement"}]
OUT.json  {id: {"verdict": "right"|"wrong"|"cannot_tell"|"failed", "cost", "secs", "escalated", "citations", "why", "model"}}
```

Run from the olifant checkout (the recordings are relative to it):

```
SP=<dir>; cabal build lib:olifant
cabal exec -- ghc -package olifant -package-id <aeson id> -package-id <scientific id> -package bytestring \
  -package directory -package filepath -package text -package time -XGHC2021 -O0 -outputdir $SP/build \
  -o $SP/claimcheck experiments/vet-1a-2026-11/ClaimCheck.hs
VET_LIVE=1 JEV_API_KEY=... $SP/claimcheck IN.json OUT.json     # asks what is not recorded
$SP/claimcheck IN.json OUT.json                                 # replays; an unrecorded item fails
```

(`aeson` and `scientific` have two versions in the package db: the ids are `sn-...` and `scntfc-...`
in `dist-newstyle/cache/plan.json`, `pkg-name` aeson and scientific.)

- Each item's tree is loaded as a project. The harness writes `plans/vet-1a.md` (a stub) and
  overwrites `.olifant/config.toml` in the tree with a minimal one (`languages = ["python","haskell"]`,
  a `jev` backend, `[decide] model = "jev"`), so the project's own config (packs, backends) never
  decides how the check runs; `[plans]` is the shipped default (the cascade).
- `claims.json`: every model reply, `replayChecker` format, keyed `MODEL|ID|sentence-key` (the cascade
  asks two models the same sentence). `claims-results.json`: each item's finished result (what a replay
  returns; the Jev guard answers are not recorded apart, only their outcome, `escalated`).
- Live, it stops before an item once the recorded reply cost passes `VET_CLAIMS_CAP` (default $4).
- A claim check is one prose `Sentence`, kind `CheckClaim`, no web.
