#!/usr/bin/env python3
import csv
import math
import re
import statistics
import sys
from collections import defaultdict
from pathlib import Path


def fail(message: str) -> None:
    raise SystemExit(f"summary failed: {message}")


def as_float(row, field):
    try:
        return float(row[field])
    except (KeyError, ValueError) as exc:
        fail(f"bad {field}: {row.get(field)!r} ({exc})")


def as_int(row, field):
    try:
        return int(row[field])
    except (KeyError, ValueError) as exc:
        fail(f"bad {field}: {row.get(field)!r} ({exc})")


def percentile(values, q):
    if not values:
        return math.nan
    xs = sorted(values)
    if len(xs) == 1:
        return xs[0]
    pos = (len(xs) - 1) * q
    lo = int(math.floor(pos))
    hi = int(math.ceil(pos))
    if lo == hi:
        return xs[lo]
    frac = pos - lo
    return xs[lo] * (1.0 - frac) + xs[hi] * frac


def mad(values):
    if not values:
        return math.nan
    center = statistics.median(values)
    return statistics.median(abs(x - center) for x in values)


def parameter_key(weights, parameter_text):
    if weights == "constant":
        return ""
    try:
        return f"{float(parameter_text):.2f}"
    except ValueError as exc:
        fail(f"bad parameter {parameter_text!r}: {exc}")


def condition_key(row):
    return (
        row["model"],
        row["query"],
        row["weights"],
        parameter_key(row["weights"], row["parameter"]),
        as_int(row, "t_len"),
        row["impl"],
    )


def parse_rss_logs(base):
    # macOS /usr/bin/time -l: '<bytes>  maximum resident set size'
    mac_re = re.compile(r"^\s*(\d+)\s+maximum resident set size\s*$")
    # Also accept GNU time output if the harness is moved later.
    gnu_re = re.compile(r"^\s*Maximum resident set size \(kbytes\):\s*(\d+)\s*$")
    # Model names in RSS log stems: original/variant for current runs, and the
    # historical grid3/grid4 (same models) recorded in archived evidence.
    stem_re = re.compile(
        r"^(original|variant|grid3|grid4)_(forward|viterbi)_(constant|parameterized)_"
        r"(packed|embedded)_t(\d+)_p([^_]+)_r(\d+)\.log$"
    )
    result = defaultdict(list)
    for path in sorted((base / "runs").glob("*/rss/*.log")):
        match = stem_re.match(path.name)
        if not match:
            fail(f"unexpected RSS log name: {path}")
        model, query, weights, impl_short, t_len, parameter_text, process_run = match.groups()
        rss_bytes = None
        with path.open(encoding="utf-8", errors="replace") as f:
            for line in f:
                m = mac_re.match(line)
                if m:
                    rss_bytes = int(m.group(1))
                    break
                m = gnu_re.match(line)
                if m:
                    rss_bytes = int(m.group(1)) * 1024
                    break
        if rss_bytes is None:
            fail(f"maximum resident set size not found in {path}")
        impl = "staged_" + impl_short
        key = (
            model,
            query,
            weights,
            "" if weights == "constant" else f"{float(parameter_text):.2f}",
            int(t_len),
            impl,
        )
        result[key].append((int(process_run), rss_bytes))
    return result


def main():
    if len(sys.argv) != 2:
        raise SystemExit("usage: summarize_kernel_evaluation.py EVALUATION_DIR")
    base = Path(sys.argv[1]).resolve()
    raw = base / "raw" / "all.csv"
    if not raw.is_file():
        fail(f"missing combined CSV: {raw}")

    with raw.open(newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f))
    if not rows:
        fail("combined CSV has no data rows")

    by_condition = defaultdict(list)
    phase_by_process = {}
    for row in rows:
        key = condition_key(row)
        by_condition[key].append(row)
        process_key = key + (as_int(row, "process_run"),)
        phase_values = (
            as_float(row, "model_build_sec"),
            as_float(row, "transition_build_sec"),
            as_float(row, "lowering_prep_sec"),
            as_float(row, "code_construction_sec"),
            as_float(row, "stage_run_sec"),
            as_int(row, "residual_source_bytes"),
            as_int(row, "distinct_weights"),
            as_int(row, "states"),
            as_int(row, "edges"),
        )
        previous = phase_by_process.get(process_key)
        if previous is not None and previous != phase_values:
            fail(f"preparation metrics changed within one process: {process_key}")
        phase_by_process[process_key] = phase_values

    rss = parse_rss_logs(base)
    has_rss = bool(rss)

    summary_dir = base / "summary"
    summary_dir.mkdir(parents=True, exist_ok=True)
    output = summary_dir / "metrics.csv"
    process_output = summary_dir / "process_runtime.csv"
    fields = [
        "model", "query", "weights", "parameter", "t_len", "impl",
        "states", "edges", "distinct_weights", "run_samples", "process_samples",
        "model_build_sec_median", "transition_build_sec_median",
        "lowering_prep_sec_median", "code_construction_sec_median",
        "stage_run_sec_median", "staging_total_sec_median",
        "residual_source_bytes",
        # The primary runtime statistics are computed across independent process
        # medians.  The pooled median is retained only as a diagnostic.
        "run_time_sec_median", "run_time_sec_q1", "run_time_sec_q3",
        "run_time_sec_process_mad", "run_time_sec_pooled_median",
        "result_score", "rss_samples",
        "compile_peak_rss_bytes_median", "compile_peak_rss_bytes_max",
    ]
    process_fields = [
        "model", "query", "weights", "parameter", "t_len", "impl",
        "process_run", "run_samples", "run_time_sec_median",
        "run_time_sec_q1", "run_time_sec_q3", "result_score",
    ]

    process_rows = []
    summary_rows = []

    for key in sorted(by_condition):
        model, query, weights, parameter, t_len, impl = key
        group = by_condition[key]
        scores = [as_float(row, "result_score") for row in group]
        score0 = scores[0]
        if any(not math.isclose(score0, score, rel_tol=1e-12, abs_tol=1e-12) for score in scores[1:]):
            fail(f"result score changed across repetitions for {key}")

        runtime_by_process = defaultdict(list)
        for row in group:
            runtime_by_process[as_int(row, "process_run")].append(as_float(row, "run_time_sec"))
        if not runtime_by_process:
            fail(f"no runtime process samples for {key}")
        run_counts = {len(values) for values in runtime_by_process.values()}
        if len(run_counts) != 1:
            fail(f"runtime repetitions differ across processes for {key}: {sorted(run_counts)}")

        process_medians = []
        pooled_run_times = []
        for process_run in sorted(runtime_by_process):
            values = runtime_by_process[process_run]
            pooled_run_times.extend(values)
            process_median = statistics.median(values)
            process_medians.append(process_median)
            process_rows.append({
                "model": model,
                "query": query,
                "weights": weights,
                "parameter": parameter,
                "t_len": t_len,
                "impl": impl,
                "process_run": process_run,
                "run_samples": len(values),
                "run_time_sec_median": f"{process_median:.17g}",
                "run_time_sec_q1": f"{percentile(values, 0.25):.17g}",
                "run_time_sec_q3": f"{percentile(values, 0.75):.17g}",
                "result_score": f"{score0:.17g}",
            })

        phase_rows = [
            (pkey[-1], values) for pkey, values in phase_by_process.items()
            if pkey[:-1] == key
        ]
        if not phase_rows:
            fail(f"no process-level preparation metrics for {key}")
        phase_processes = {process_run for process_run, _ in phase_rows}
        runtime_processes = set(runtime_by_process)
        if phase_processes != runtime_processes:
            fail(
                f"runtime/preparation process sets differ for {key}: "
                f"runtime={sorted(runtime_processes)} prep={sorted(phase_processes)}"
            )
        phase_values = [values for _, values in phase_rows]
        model_build = [x[0] for x in phase_values]
        transition_build = [x[1] for x in phase_values]
        lowering_prep = [x[2] for x in phase_values]
        code_construction = [x[3] for x in phase_values]
        stage_run = [x[4] for x in phase_values]
        source_sizes = {x[5] for x in phase_values}
        distinct_weights = {x[6] for x in phase_values}
        states = {x[7] for x in phase_values}
        edges = {x[8] for x in phase_values}
        if len(source_sizes) != 1 or len(distinct_weights) != 1 or len(states) != 1 or len(edges) != 1:
            fail(f"structural metric changed across processes for {key}")

        rss_values = [value for _, value in rss.get(key, [])]
        if has_rss and impl.startswith("staged_") and not rss_values:
            fail(f"RSS was measured but no RSS sample matches {key}")

        model_build_median = statistics.median(model_build)
        transition_build_median = statistics.median(transition_build)
        lowering_prep_median = statistics.median(lowering_prep)
        code_construction_median = statistics.median(code_construction)
        stage_run_median = statistics.median(stage_run)
        staging_total = lowering_prep_median + code_construction_median + stage_run_median
        process_runtime_median = statistics.median(process_medians)

        summary_rows.append({
            "model": model,
            "query": query,
            "weights": weights,
            "parameter": parameter,
            "t_len": t_len,
            "impl": impl,
            "states": next(iter(states)),
            "edges": next(iter(edges)),
            "distinct_weights": next(iter(distinct_weights)),
            "run_samples": len(pooled_run_times),
            "process_samples": len(process_medians),
            "model_build_sec_median": f"{model_build_median:.17g}",
            "transition_build_sec_median": f"{transition_build_median:.17g}",
            "lowering_prep_sec_median": f"{lowering_prep_median:.17g}",
            "code_construction_sec_median": f"{code_construction_median:.17g}",
            "stage_run_sec_median": f"{stage_run_median:.17g}",
            "staging_total_sec_median": f"{staging_total:.17g}",
            "residual_source_bytes": next(iter(source_sizes)),
            "run_time_sec_median": f"{process_runtime_median:.17g}",
            "run_time_sec_q1": f"{percentile(process_medians, 0.25):.17g}",
            "run_time_sec_q3": f"{percentile(process_medians, 0.75):.17g}",
            "run_time_sec_process_mad": f"{mad(process_medians):.17g}",
            "run_time_sec_pooled_median": f"{statistics.median(pooled_run_times):.17g}",
            "result_score": f"{score0:.17g}",
            "rss_samples": len(rss_values),
            "compile_peak_rss_bytes_median": "" if not rss_values else str(int(statistics.median(rss_values))),
            "compile_peak_rss_bytes_max": "" if not rss_values else str(max(rss_values)),
        })

    with output.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        writer.writerows(summary_rows)

    with process_output.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=process_fields)
        writer.writeheader()
        writer.writerows(process_rows)

    print(
        f"SUMMARY rows={len(summary_rows)} process_rows={len(process_rows)} "
        f"csv={output} process_csv={process_output}"
    )


if __name__ == "__main__":
    main()
