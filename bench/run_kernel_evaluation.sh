#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
cd "$ROOT_DIR"

# Defaults are a branch-covering smoke run, not the final paper experiment.
PROCESS_REPETITIONS=${PROCESS_REPETITIONS:-1}
RUN_REPETITIONS=${RUN_REPETITIONS:-2}
WARMUP=${WARMUP:-1}
T_LEN=${T_LEN:-30}
PARAMETER=${PARAMETER:-0.80}
MODELS=${MODELS:-"original"}
QUERIES=${QUERIES:-"forward viterbi"}
WEIGHTS_MODES=${WEIGHTS_MODES:-"constant parameterized"}
IMPLS=${IMPLS:-"handwritten kernel packed embedded"}
MEASURE_RSS=${MEASURE_RSS:-0}
BACKEND=${BACKEND:-bytecode}
IMPL_ORDER_MODE=${IMPL_ORDER_MODE:-fixed}
if git -C "$ROOT_DIR" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  SOURCE_COMMIT=$(git -C "$ROOT_DIR" rev-parse HEAD)
  SOURCE_SHORT=$(git -C "$ROOT_DIR" rev-parse --short=12 HEAD)
else
  SOURCE_COMMIT=unavailable
  SOURCE_SHORT=source
fi
RUN_ID=${RUN_ID:-"$(date '+%Y%m%d-%H%M%S')-$SOURCE_SHORT"}
OUT_DIR=${OUT_DIR:-"results/performance/$RUN_ID"}

case "$BACKEND" in
  bytecode)
    BENCH_TARGET=bench-kernel-evaluation
    BENCH_EXE=./_build_staged/bench_kernel_evaluation.byte
    ;;
  native)
    BENCH_TARGET=bench-kernel-evaluation-native
    BENCH_EXE=./_build_staged_native/bench_kernel_evaluation.native
    ;;
  *)
    echo "error: BACKEND must be bytecode or native, got: $BACKEND" >&2
    exit 1
    ;;
esac

case "$IMPL_ORDER_MODE" in
  fixed|balanced) ;;
  *)
    echo "error: IMPL_ORDER_MODE must be fixed or balanced, got: $IMPL_ORDER_MODE" >&2
    exit 1
    ;;
esac

if [ -e "$OUT_DIR" ]; then
  echo "error: refusing to overwrite existing evaluation directory: $OUT_DIR" >&2
  exit 1
fi

make "$BENCH_TARGET"

mkdir -p "$OUT_DIR/raw" "$OUT_DIR/rss" "$OUT_DIR/metadata"
{
  echo "run_id=$RUN_ID"
  echo "git_commit=$SOURCE_COMMIT"
  echo "timestamp=$(date '+%Y-%m-%dT%H:%M:%S%z')"
  echo "backend=$BACKEND"
  echo "benchmark_executable=$BENCH_EXE"
  echo "process_repetitions=$PROCESS_REPETITIONS"
  echo "run_repetitions=$RUN_REPETITIONS"
  echo "warmup=$WARMUP"
  echo "t_len=$T_LEN"
  echo "parameter=$PARAMETER"
  echo "models=$MODELS"
  echo "queries=$QUERIES"
  echo "weights_modes=$WEIGHTS_MODES"
  echo "impls=$IMPLS"
  echo "impl_order_mode=$IMPL_ORDER_MODE"
  echo "measure_rss=$MEASURE_RSS"
  echo "observation_workload=observation[0]=(1,1); observation[1..T-1]=(21,21); T=t_len"
  echo "kernel_reference_timed_region=ordinary inference call, including emission-column construction per invocation"
  echo "handwritten_packed_timed_region=inference call with packed transition and emission columns prepared before timing"
  echo "staged_timed_region=generated inference call with specialization and packed emission columns prepared before timing"
  echo "runtime_comparison_scope=end-to-end implementation runtimes with different preparation boundaries; not an isolated transition-lowering speedup"
} > "$OUT_DIR/metadata/conditions.txt"

# Four-order Williams-style rotation.  When all four implementations are
# requested, each implementation occupies every execution position once in
# each four-process block.  Other implementation subsets retain their given
# order rather than silently changing the requested experiment.
impls_for_process () {
  process_run=$1
  if [ "$IMPL_ORDER_MODE" != "balanced" ] || [ "$IMPLS" != "handwritten kernel packed embedded" ]; then
    printf '%s\n' "$IMPLS"
    return
  fi
  case $(( (process_run - 1) % 4 )) in
    0) printf '%s\n' "handwritten kernel embedded packed" ;;
    1) printf '%s\n' "kernel packed handwritten embedded" ;;
    2) printf '%s\n' "packed embedded kernel handwritten" ;;
    3) printf '%s\n' "embedded handwritten packed kernel" ;;
  esac
}

combined="$OUT_DIR/raw/all.csv"
first=1

process_run=1
while [ "$process_run" -le "$PROCESS_REPETITIONS" ]; do
  process_impls=$(impls_for_process "$process_run")
  echo "[order] process_run=$process_run impls=$process_impls" >&2
  for model in $MODELS; do
    for query in $QUERIES; do
      for weights in $WEIGHTS_MODES; do
        for impl in $process_impls; do
          stem="${model}_${query}_${weights}_${impl}_t${T_LEN}_p${PARAMETER}_r${process_run}"
          csv="$OUT_DIR/raw/${stem}.csv"
          log="$OUT_DIR/raw/${stem}.log"
          echo "[runtime] $stem" >&2
          "$BENCH_EXE" \
            "$model" "$query" "$weights" "$impl" \
            "$T_LEN" "$RUN_REPETITIONS" "$WARMUP" "$PARAMETER" full \
            > "$csv" 2> "$log"
          if [ "$first" -eq 1 ]; then
            awk -v process_run="$process_run" 'NR==1 {print "process_run," $0; next} {print process_run "," $0}' "$csv" > "$combined"
            first=0
          else
            awk -v process_run="$process_run" 'NR>1 {print process_run "," $0}' "$csv" >> "$combined"
          fi

          if [ "$MEASURE_RSS" -eq 1 ] && { [ "$impl" = "packed" ] || [ "$impl" = "embedded" ]; }; then
            rss_csv="$OUT_DIR/rss/${stem}.csv"
            rss_log="$OUT_DIR/rss/${stem}.log"
            echo "[compile-rss] $stem" >&2
            /usr/bin/time -l \
              "$BENCH_EXE" \
                "$model" "$query" "$weights" "$impl" \
                "$T_LEN" 1 0 "$PARAMETER" compile-only \
              > "$rss_csv" 2> "$rss_log"
          fi
        done
      done
    done
  done
  process_run=$((process_run + 1))
done

python3 bench/verify_kernel_evaluation.py "$combined"
echo "wrote $combined" >&2
