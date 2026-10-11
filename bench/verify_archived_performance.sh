#!/bin/sh

set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
archive="$project_root/results/performance/cp5e-native-final-20260929"
temporary_root=$(mktemp -d "${TMPDIR:-/tmp}/finite-hmm-performance.XXXXXX")

cleanup () {
  rm -rf "$temporary_root"
}
trap cleanup EXIT HUP INT TERM

(
  cd "$archive"
  shasum -a 256 -c SHA256SUMS >/dev/null
)
echo "verified archived performance checksums"

# Recompute every derived product from the archived timing rows and
# compile-only RSS logs, without writing into the archive.
generated="$temporary_root/cp5e-native-final-20260929"
mkdir -p "$generated"
(
  cd "$archive"
  tar cf - raw runs
) | (
  cd "$generated"
  tar xf -
)

python3 "$project_root/bench/verify_kernel_evaluation.py" "$generated/raw/all.csv" >/dev/null
python3 "$project_root/bench/summarize_kernel_evaluation.py" "$generated" >/dev/null
python3 "$project_root/bench/analyze_kernel_evaluation.py" "$generated" >/dev/null

for filename in summary/metrics.csv summary/process_runtime.csv \
                analysis/pairwise_runtime.csv analysis/condition_comparison.csv \
                analysis/report.md; do
  if ! cmp -s "$archive/$filename" "$generated/$filename"; then
    echo "archived performance result differs: $filename" >&2
    diff -u "$archive/$filename" "$generated/$filename" || true
    exit 1
  fi
  echo "verified archived performance result: $filename"
done
