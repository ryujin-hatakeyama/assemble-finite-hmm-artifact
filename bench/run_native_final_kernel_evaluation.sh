#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
cd "$ROOT_DIR"

# CP5E defaults.  Eight independent process repetitions form two complete
# four-order blocks for the balanced implementation ordering.  The 30 timed
# calls inside each process stabilize that process's median; statistical
# aggregation is performed across process medians, not across all 240 calls.
PROCESS_REPETITIONS=${PROCESS_REPETITIONS:-8}
RUN_REPETITIONS=${RUN_REPETITIONS:-30}
WARMUP=${WARMUP:-5}
T_LEN=${T_LEN:-300}
PARAMETERS=${PARAMETERS:-"0.60 0.70 0.80 0.95"}
MEASURE_RSS=${MEASURE_RSS:-1}
METAOCAML_VERSION=${METAOCAML_VERSION:-BER-N153}
if git -C "$ROOT_DIR" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  SOURCE_SHORT=$(git -C "$ROOT_DIR" rev-parse --short=12 HEAD)
else
  SOURCE_SHORT=source
fi
RUN_ID=${RUN_ID:-"cp5e-native-$(date '+%Y%m%d')-$SOURCE_SHORT"}
OUT_DIR=${OUT_DIR:-"results/performance/$RUN_ID"}

if ! command -v ocamlopt >/dev/null 2>&1; then
  echo "error: ocamlopt is not available in PATH" >&2
  exit 1
fi
if ! ocamlfind query metaocaml >/dev/null 2>&1; then
  echo "error: ocamlfind cannot find the metaocaml package in the active switch" >&2
  exit 1
fi

# Build once up front so native-linking problems fail before a result directory
# containing partial measurements is created.
make bench-kernel-evaluation-native

PROCESS_REPETITIONS="$PROCESS_REPETITIONS" \
RUN_REPETITIONS="$RUN_REPETITIONS" \
WARMUP="$WARMUP" \
T_LEN="$T_LEN" \
PARAMETERS="$PARAMETERS" \
MEASURE_RSS="$MEASURE_RSS" \
BACKEND=native \
IMPL_ORDER_MODE=balanced \
METAOCAML_VERSION="$METAOCAML_VERSION" \
RUN_ID="$RUN_ID" \
OUT_DIR="$OUT_DIR" \
  "$SCRIPT_DIR/run_final_kernel_evaluation.sh"

python3 "$SCRIPT_DIR/analyze_kernel_evaluation.py" "$OUT_DIR"
