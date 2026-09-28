#!/usr/bin/env bash
# Re-run the test suite on every save (Haskell sources and pack files).
# Extra args go to hspec, e.g.:  scripts/test-watch.sh --match "Olifant.Naming"
set -euo pipefail
cd "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"
export PATH="$HOME/.ghcup/bin:$HOME/.local/bin:$PATH"

hspec_args=""
for a in "$@"; do hspec_args+=" \"${a//\"/\\\"}\""; done

exec ghcid \
  --command "cabal repl test:spec" \
  --test ":main${hspec_args}" \
  --reload=packs \
  --warnings
