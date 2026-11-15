"""Vet 1b: the replay gate's data, from the 1a pipeline run on sonnet (not product code).

Writes three files into src/Olifant/Vet/testdata/:
  vet-corpus-finds.json   {case: {file: reply text}}   1a's one find call a case, split by the finding's file
  vet-corpus-checks.json  {"replies": {sentence key: {"text", "cost"}}}   1a's claim verdicts, as checker replies
  vet-corpus-labels.json  {case: {"set", "hits": [statement], "patch", "changed": [file]}}   1a's judged hits
                          (judge + hand overrides), and the case's patch and changed Python files

Usage (from the olifant checkout):
  python3 -I experiments/vet-1b-2026-11/finds.py ~/projects/notes/durandal/experiments/vet-1a-2026-11-15
"""
import json, os, sys, tomllib

def main(exp):
    rec = os.path.join(exp, "pipeline", "rec-sonnet")
    judged = json.load(open(os.path.join(exp, "pipeline", "judged-sonnet.json")))
    overrides = json.load(open(os.path.join(exp, "pipeline", "overrides-sonnet.json")))
    finds, replies, labels = {}, {}, {}
    for name in sorted(os.listdir(rec)):
        r = json.load(open(os.path.join(rec, name)))
        cid = r["case"]
        by_file = {}
        for f in r["findings"]:
            by_file.setdefault(f["file"], []).append({k: f[k] for k in ("kind", "file", "lines", "statement", "rule", "evidence")})
            if f.get("claim") in ("right", "wrong", "cannot_tell"):
                ev = [{"path": e["file"], "line": e["lines"][0]} for e in f["evidence"]] or [{"path": f["file"], "line": f["lines"][0]}]
                body = {"verdict": f["claim"], "why": "1a's claim check", "evidence": ev if f["claim"] != "cannot_tell" else []}
                replies[" ".join(f["statement"].split())] = {"text": "```json\n" + json.dumps(body) + "\n```", "cost": 0.008}
        finds[cid] = {p: json.dumps({"findings": fs, "open": []}) for p, fs in by_file.items()}
        shown = [f for f in r["findings"] if f.get("shown")]
        marks = overrides.get(cid, judged.get(cid, []))
        hits = [f["statement"] for f, m in zip(shown, marks) if m.get("hit")]
        case = tomllib.load(open(os.path.join(exp, "..", "code-checker-conventions-2026-11-14", "cases", cid + ".toml"), "rb"))
        labels[cid] = {"set": r["set"], "hits": hits, "patch": case["patch"], "changed": [f for f in case["changed_files"] if f.endswith(".py")]}
    out = "src/Olifant/Vet/testdata"
    json.dump(finds, open(os.path.join(out, "vet-corpus-finds.json"), "w"), indent=1, sort_keys=True)
    json.dump({"replies": replies}, open(os.path.join(out, "vet-corpus-checks.json"), "w"), indent=1, sort_keys=True)
    json.dump(labels, open(os.path.join(out, "vet-corpus-labels.json"), "w"), indent=1, sort_keys=True)
    print(len(finds), "cases;", sum(len(v) for v in finds.values()), "file replies;", len(replies), "claim replies;",
          sum(len(v["hits"]) for v in labels.values()), "hits")

if __name__ == "__main__":
    main(os.path.expanduser(sys.argv[1]))
