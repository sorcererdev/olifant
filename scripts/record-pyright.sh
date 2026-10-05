#!/bin/sh
# Record pyright's JSON report for a sample project (testdata for the
# pyright-json support): paths are made relative to the project.
# Usage: scripts/record-pyright.sh DIR
set -e
dir=$(cd "$1" && pwd -P)
cd "$dir"
uvx --quiet pyright --outputjson > report.raw.json || true
sed "s#$dir/##g" report.raw.json > report.json
rm report.raw.json
