"""Conjure 2026-11: the tables from results.json and replies.json (run from the olifant checkout)."""
import json, collections
H = "experiments/conjure-2026-11/"
res = json.load(open(H + "results.json")); rep = json.load(open(H + "replies.json"))
runs, judged = res["runs"], res.get("judged", {})
writers = ["qwen3-coder:30b", "claude-haiku-5-5", "claude-sonnet-5-5@low", "claude-sonnet-5-5"]
kinds = ["nonsense", "stub", "half", "bug", "syntax", "no-test", "green"]
fw = lambda k: judged.get(k, {}).get("forward")
print("| writer | shape | forward | forward (no no-test) | $ per step | s per step | mean lines | outside unit | regressions | unparsed |")
print("|---|---|---|---|---|---|---|---|---|---|")
for w in writers:
    for s in "ABC":
        ks = [k for k, v in runs.items() if v["writer"] == w and v["variant"] == s]
        yes = sum(fw(k) == "yes" for k in ks)
        kt = [k for k in ks if runs[k]["kind"] != "no-test"]
        yt = sum(fw(k) == "yes" for k in kt)
        us = [runs[k] for k in ks if runs[k]["usable"]]
        n = len(us) or 1
        print(f"| {w} | {s} | {yes}/{len(ks)} ({100*yes/len(ks):.0f}%) | {yt}/{len(kt)} ({100*yt/len(kt):.0f}%) | {sum(v['cost'] for v in us)/n:.4f} | {sum(v['secs'] for v in us)/n:.1f} | {sum(v['lines_changed'] for v in us)/n:.1f} | {sum(v['outside_unit'] for v in us)} | {sum(bool(v['newly_failing']) for v in us)} | {sum(not v['parses'] for v in us)} |")
print()
print("| writer | " + " | ".join(kinds) + " |"); print("|---|" + "---|" * len(kinds))
for w in writers:
    row = []
    for kd in kinds:
        ks = [k for k, v in runs.items() if v["writer"] == w and v["kind"] == kd]
        row.append(f"{sum(fw(k)=='yes' for k in ks)}/{len(ks)}")
    print(f"| {w} | " + " | ".join(row) + " |")
print()
print("| shape | " + " | ".join(kinds) + " |"); print("|---|" + "---|" * len(kinds))
for s in "ABC":
    row = []
    for kd in kinds:
        ks = [k for k, v in runs.items() if v["variant"] == s and v["kind"] == kd]
        row.append(f"{sum(fw(k)=='yes' for k in ks)}/{len(ks)}")
    print(f"| {s} | " + " | ".join(row) + " |")
print()
by = collections.defaultdict(float)
for k, v in rep.items():
    by[k.split("|")[0] if k.split("|")[0] in ("JUDGE", "STRATEGIST") else k.split("|")[1]] += v["cost"]
print("cost by caller:", {k: round(v, 4) for k, v in by.items()}, "total", round(sum(by.values()), 4))
print("wall (sum of call seconds):", round(sum(v["secs"] for v in rep.values()), 1))
loc = [v for v in runs.values() if "rss_mb" in v]
if loc:
    print("local: peak RSS MB min/max", round(min(v["rss_mb"] for v in loc)), round(max(v["rss_mb"] for v in loc)), "; within 60 s", sum(v["within_60s"] for v in loc), "/", len(loc), "; max s", round(max(v["secs"] for v in loc), 1))
print("unjudged:", [k for k in runs if fw(k) not in ("yes", "no")])
