# The cascade experiment (plans m2c, 2026-11-14)

Can haiku, guarded by Jev, do the plan checks at under half sonnet's price? Every check the
prose-corpus gates recorded for sonnet (29: 22 code, 7 web) is done again by haiku under the
same prompt and tools (`[plans] check_model = "haiku"` through `liveChecker`; a web check keeps
web tools and no file tools, a code check the reverse, ruling C). Each reply goes through the
engine's `confirm`, then one Jev request (`jev-1.14.0`) asks three Nouls about it:
`why-fits`, `evidence-fits`, `verdict-follows`, over the sentence, the kind of check, the
verdict (with what it means), the reason (and answer), and the evidence.

Escalation to sonnet (its recorded reply): a citation failed, the verdict is cannot_tell, or any
guard is under the threshold. Cost = haiku + sonnet when escalated (Jev unpriced, about 3,200
input tokens a request); seconds = haiku + the Jev request + sonnet when escalated.

**evidence-fits on a web check** (builder's call): a web check cites no code, so its evidence is
the pages it cites (title and address) and the Noul asks whether they are the named library's own
documentation, release notes or migration guide for the version named. Same Noul name, the
instructions and criteria chosen by `ckWeb`.

Run (`OLIFANT_PROSE_CORPUS` = electionnighttracker at 8488bb80; built against the in-place lib):

```
SP=<scratch>; cabal build lib:olifant
cabal exec -- ghc -package olifant -package-id <aeson, bytestring, directory, filepath, scientific, text, time ids from dist-newstyle/cache/plan.json> \
  -XGHC2021 -O0 -outputdir $SP/build -o $SP/cascade experiments/cascade-2026-11/Cascade.hs
OLIFANT_PROSE_CORPUS=~/projects/electionnighttracker $SP/cascade        # replays (free)
CASCADE_LIVE=1 ...                                                         # asks what is not recorded
```

Recordings: `haiku-checks.json` (haiku replies, `replayChecker` format), `guard-jev.json` (Jev
answers by question key), `guard-times.json` (seconds of each guard request). `results.txt` is
the run's output; a replay reproduces it exactly.

Result: threshold 0.6 meets every bar (25/25 where sonnet agreed, no new wrong-sure, mean $0.0377
= 46% of sonnet's $0.0816, median 14.6 s = 1.42x sonnet's 10.3 s). 0.7 misses the cost bar by
a hair ($0.0412 against $0.0408), 0.8 misses cost and time. Live spend $0.294 (haiku) plus 36
Jev requests. Report: notes/durandal/plan-prose-2c-report.md.
