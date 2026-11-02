#!/usr/bin/env bash
# Lists any line-number reference left in book/internals/ (`Name.hs:123`, `(:123)`) and exits
# non-zero if there is one. References name the file and the function or type instead.
# Read-only. Run from anywhere.
set -u
root="$(cd "$(dirname "$0")/.." && pwd)"
pattern='[A-Za-z0-9_]+\.(hs|py|lua|md|toml|sh|cabal):[0-9]+|\(`?~?:[0-9]+|`~?:[0-9]+(-[0-9]+)?`'
# An `--at FILE:LINE:COL` command line is an example of the CLI, not a reference.
hits="$(grep -nEr "$pattern" "$root/book/internals" --include='*.md' | grep -vE 'olifant (actions|complete|gen|uncovered) .*--at [^ ]+:[0-9]+:[0-9]+' || true)"
if [ -n "$hits" ]; then
  echo "$hits"
  echo "book-refs: line-number references above; name the file and function instead" >&2
  exit 1
fi
echo "book-refs: none"
