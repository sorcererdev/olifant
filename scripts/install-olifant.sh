#!/usr/bin/env bash
# Build and install the `olifant` binary into ~/.local/bin (on PATH via ~/.zshrc).
# Run by the post-commit / post-merge hooks on main; safe to run by hand.
set -euo pipefail
cd "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"
export PATH="$HOME/.ghcup/bin:$PATH"

log=$(mktemp)
trap 'rm -f "$log"' EXIT
if cabal install -v0 exe:olifant --installdir="$HOME/.local/bin" --overwrite-policy=always >"$log" 2>&1; then
  echo "olifant: installed $(git rev-parse --short HEAD) to ~/.local/bin/olifant"
else
  grep -v "ld: warning" "$log" >&2 || true
  echo "olifant: install failed" >&2
  exit 1
fi
