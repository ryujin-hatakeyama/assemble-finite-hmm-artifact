#!/bin/sh

set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
experiment="$project_root/_build_model_generated_aggregation/experiment_model_generated_aggregation.exe"
archive="$project_root/results/aggregation_scope/model_generated_20261006"
temporary_root=$(mktemp -d "${TMPDIR:-/tmp}/finite-hmm-aggregation.XXXXXX")

cleanup () {
  rm -rf "$temporary_root"
}
trap cleanup EXIT HUP INT TERM

(
  cd "$temporary_root"
  "$experiment"
)

generated="$temporary_root/results/aggregation_scope/model_generated_20261006"
for filename in raw_per_sequence.csv aggregate.csv config.txt report.md; do
  if ! cmp -s "$archive/$filename" "$generated/$filename"; then
    echo "archived semantic result differs: $filename" >&2
    diff -u "$archive/$filename" "$generated/$filename" || true
    exit 1
  fi
  echo "verified archived semantic result: $filename"
done
