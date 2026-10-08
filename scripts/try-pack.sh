#!/usr/bin/env bash
# Make a throwaway sandbox for manually trying a pack's generators.
#
#   scripts/try-pack.sh                 # list packs you can try
#   scripts/try-pack.sh elm             # sandbox in a fresh temp dir
#   scripts/try-pack.sh elm ~/tmp/elm   # sandbox at a path you choose (must not exist)
#
# The sandbox is a copy of packs/<pack>/fixtures/app, configured to use the
# pack from this checkout, and committed to a fresh git repo so that after
# `olifant gen …` you can `git add -A && git diff --cached` to see exactly what
# changed, and
# `git reset --hard && git clean -fd` to start over.
set -euo pipefail
repo="$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"

list_packs() {
  for dir in "$repo"/packs/*/; do
    pack=$(basename "$dir")
    gens=$(ls "$dir/generators" 2>/dev/null | tr '\n' ' ')
    printf '  %-12s %s\n' "$pack" "$gens"
  done
}

if [ $# -lt 1 ]; then
  echo "usage: scripts/try-pack.sh <pack> [sandbox-dir]"
  echo "packs (and their generators):"
  list_packs
  exit 2
fi

pack=$1
pack_dir="$repo/packs/$pack"
fixture="$pack_dir/fixtures/app"
[ -d "$pack_dir/languages" ] || [ -d "$pack_dir/generators" ] || { echo "no pack named '$pack'. Packs:" >&2; list_packs >&2; exit 2; }

if [ $# -ge 2 ]; then
  sandbox=$2
  [ -e "$sandbox" ] && { echo "$sandbox already exists; pick a new path" >&2; exit 2; }
  mkdir -p "$sandbox"
else
  sandbox=$(mktemp -d "${TMPDIR:-/tmp}/olifant-$pack.XXXXXX")
fi
sandbox=$(cd "$sandbox" && pwd -P)

[ -d "$fixture" ] && cp -R "$fixture/." "$sandbox/"
mkdir -p "$sandbox/.olifant"
printf '[packs]\n%s = { path = "%s" }\n' "$pack" "$pack_dir" > "$sandbox/.olifant/config.toml"
# the packs its generators build on ([[uses]] another pack's generator)
for used in $(grep -ho 'generator *= *"[a-z0-9-]*/' "$pack_dir"/generators/*/generator.toml 2>/dev/null | sed 's/.*"\(.*\)\//\1/' | sort -u); do
  [ "$used" != "$pack" ] && [ -d "$repo/packs/$used" ] && printf '%s = { path = "%s" }\n' "$used" "$repo/packs/$used" >> "$sandbox/.olifant/config.toml"
done
# the pack's own settings for a sandbox (python-strangler: [migrate])
[ -f "$pack_dir/try.toml" ] && { echo; cat "$pack_dir/try.toml"; } >> "$sandbox/.olifant/config.toml"

# A Python project: its environment (pyright, pytest, the app's deps), so
# `uv run pytest` works and Olifant's checks start warm.
if [ -f "$sandbox/pyproject.toml" ]; then
  if command -v uv > /dev/null; then
    (cd "$sandbox" && uv sync --quiet)
  else
    echo "note: uv is not installed (brew install uv); the Python checks will not run" >&2
  fi
fi

git -C "$sandbox" init -q -b main
git -C "$sandbox" add -A
git -C "$sandbox" -c user.name=olifant -c user.email=olifant@localhost commit -qm "fixture: $pack"

olifant_bin=$(command -v olifant || echo "(not on PATH: run scripts/install-olifant.sh)")
cat <<MSG
Sandbox for pack '$pack' ready:
  $sandbox

  cd $sandbox
  olifant list
  olifant show $pack/<generator>
  olifant gen $pack/<generator> --set …      (add --plan to preview)
  git add -A && git diff --cached            see everything the generator changed (incl. new files)
  git reset --hard && git clean -fd          start over

Generators: $(ls "$pack_dir/generators" 2>/dev/null | tr '\n' ' ')
Using: $olifant_bin  (built from main by the hooks; run scripts/install-olifant.sh to use this branch)
MSG
