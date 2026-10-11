#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
cd "$ROOT_DIR"

PROCESS_REPETITIONS=${PROCESS_REPETITIONS:-3}
RUN_REPETITIONS=${RUN_REPETITIONS:-30}
WARMUP=${WARMUP:-5}
T_LEN=${T_LEN:-300}
PARAMETERS=${PARAMETERS:-"0.60 0.70 0.80 0.95"}
MEASURE_RSS=${MEASURE_RSS:-1}
BACKEND=${BACKEND:-bytecode}
IMPL_ORDER_MODE=${IMPL_ORDER_MODE:-fixed}
METAOCAML_VERSION=${METAOCAML_VERSION:-unavailable}
if git -C "$ROOT_DIR" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  SOURCE_COMMIT=$(git -C "$ROOT_DIR" rev-parse HEAD)
  SOURCE_SHORT=$(git -C "$ROOT_DIR" rev-parse --short=12 HEAD)
  if ! git -C "$ROOT_DIR" diff --quiet || ! git -C "$ROOT_DIR" diff --cached --quiet; then
    echo "error: final evaluation requires a clean tracked working tree" >&2
    exit 1
  fi
else
  SOURCE_COMMIT=unavailable
  SOURCE_SHORT=source
fi
RUN_ID=${RUN_ID:-"$(date '+%Y%m%d-%H%M%S')-$SOURCE_SHORT-final"}
OUT_DIR=${OUT_DIR:-"results/performance/$RUN_ID"}

case "$OUT_DIR" in
  /*) BASE_OUT=$OUT_DIR ;;
  *) BASE_OUT="$ROOT_DIR/$OUT_DIR" ;;
esac

if [ -e "$BASE_OUT" ]; then
  echo "error: refusing to overwrite existing final evaluation directory: $BASE_OUT" >&2
  exit 1
fi

mkdir -p "$BASE_OUT/raw" "$BASE_OUT/runs" "$BASE_OUT/summary" "$BASE_OUT/metadata"

{
  echo "run_id=$RUN_ID"
  echo "git_commit=$SOURCE_COMMIT"
  echo "timestamp=$(date '+%Y-%m-%dT%H:%M:%S%z')"
  echo "backend=$BACKEND"
  echo "impl_order_mode=$IMPL_ORDER_MODE"
  echo "process_repetitions=$PROCESS_REPETITIONS"
  echo "run_repetitions=$RUN_REPETITIONS"
  echo "warmup=$WARMUP"
  echo "t_len=$T_LEN"
  echo "parameters=$PARAMETERS"
  echo "models=original variant"
  echo "queries=forward viterbi"
  echo "weights_modes=constant parameterized"
  echo "impls=handwritten kernel packed embedded"
  echo "measure_rss=$MEASURE_RSS"
  echo "observation_workload=observation[0]=(1,1); observation[1..T-1]=(21,21); T=t_len"
  echo "kernel_reference_timed_region=ordinary inference call, including emission-column construction per invocation"
  echo "handwritten_packed_timed_region=inference call with packed transition and emission columns prepared before timing"
  echo "staged_timed_region=generated inference call with specialization and packed emission columns prepared before timing"
  echo "runtime_comparison_scope=end-to-end implementation runtimes with different preparation boundaries; not an isolated transition-lowering speedup"
  echo "ocamlc_version=$(ocamlc -version 2>/dev/null || echo unavailable)"
  echo "ocamlopt_version=$(ocamlopt -version 2>/dev/null || echo unavailable)"
  echo "metaocaml_version=$METAOCAML_VERSION"
  echo "ocamlfind_version=$(ocamlfind printconf version 2>/dev/null || echo unavailable)"
  echo "os_name=$(uname -s)"
  echo "os_release=$(uname -r)"
  echo "architecture=$(uname -m)"
  if command -v sw_vers >/dev/null 2>&1; then
    echo "macos_product_version=$(sw_vers -productVersion)"
    echo "macos_build_version=$(sw_vers -buildVersion)"
  fi
  if command -v sysctl >/dev/null 2>&1; then
    echo "cpu_brand=$(sysctl -n machdep.cpu.brand_string 2>/dev/null || echo unavailable)"
  fi
} > "$BASE_OUT/metadata/conditions.txt"

if [ "$SOURCE_COMMIT" = unavailable ]; then
  echo "not a Git worktree" > "$BASE_OUT/metadata/git_status.txt"
else
  git -C "$ROOT_DIR" status --short --untracked-files=all > "$BASE_OUT/metadata/git_status.txt"
fi

run_child () {
  label=$1
  weights=$2
  parameter=$3
  child="$BASE_OUT/runs/$label"
  echo "[final] $label" >&2
  RUN_ID="$RUN_ID-$label" \
  OUT_DIR="$child" \
  PROCESS_REPETITIONS="$PROCESS_REPETITIONS" \
  RUN_REPETITIONS="$RUN_REPETITIONS" \
  WARMUP="$WARMUP" \
  T_LEN="$T_LEN" \
  PARAMETER="$parameter" \
  MODELS="original variant" \
  QUERIES="forward viterbi" \
  WEIGHTS_MODES="$weights" \
  IMPLS="handwritten kernel packed embedded" \
  MEASURE_RSS="$MEASURE_RSS" \
  BACKEND="$BACKEND" \
  IMPL_ORDER_MODE="$IMPL_ORDER_MODE" \
    "$SCRIPT_DIR/run_kernel_evaluation.sh"
}

run_child constant constant 0.80

for parameter in $PARAMETERS; do
  label=$(printf 'parameterized-p%s' "$parameter" | tr '.' '_')
  run_child "$label" parameterized "$parameter"
done

combined="$BASE_OUT/raw/all.csv"
first=1
for csv in "$BASE_OUT"/runs/*/raw/all.csv; do
  if [ "$first" -eq 1 ]; then
    cat "$csv" > "$combined"
    first=0
  else
    awk 'NR>1' "$csv" >> "$combined"
  fi
done

python3 "$SCRIPT_DIR/verify_kernel_evaluation.py" "$combined"
python3 "$SCRIPT_DIR/summarize_kernel_evaluation.py" "$BASE_OUT"

echo "final evaluation written to $BASE_OUT" >&2
