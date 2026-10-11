#!/usr/bin/env python3
import csv
import math
import statistics
import sys
from collections import defaultdict
from pathlib import Path

EXPECTED_IMPLS = {
    "handwritten_packed",
    "kernel_reference",
    "staged_packed",
    "staged_embedded",
}

PAIR_SPECS = [
    ("handwritten_packed", "kernel_reference", "kernel_vs_handwritten"),
    ("handwritten_packed", "staged_packed", "staged_packed_vs_handwritten"),
    ("handwritten_packed", "staged_embedded", "staged_embedded_vs_handwritten"),
    ("kernel_reference", "staged_packed", "staged_packed_vs_kernel"),
    ("kernel_reference", "staged_embedded", "staged_embedded_vs_kernel"),
    ("staged_packed", "staged_embedded", "embedded_vs_packed"),
]


def fail(message: str) -> None:
    raise SystemExit(f"analysis failed: {message}")


def f(row, key):
    try:
        return float(row[key])
    except (KeyError, ValueError) as exc:
        fail(f"bad {key}: {row.get(key)!r} ({exc})")


def i(row, key):
    try:
        return int(row[key])
    except (KeyError, ValueError) as exc:
        fail(f"bad {key}: {row.get(key)!r} ({exc})")


def optional_int(row, key):
    text = row.get(key, "").strip()
    return None if not text else int(text)


def condition_key(row):
    return (row["model"], row["query"], row["weights"], row["parameter"], i(row, "t_len"))


def percentile(values, q):
    if not values:
        return None
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


def paired_process_speedups(process_by_condition, key, baseline_impl, target_impl):
    impls = process_by_condition.get(key)
    if impls is None:
        fail(f"missing process runtime samples for {key}")
    baseline = impls.get(baseline_impl)
    target = impls.get(target_impl)
    if baseline is None or target is None:
        fail(f"missing paired process implementation for {key}: {baseline_impl} vs {target_impl}")
    if set(baseline) != set(target):
        fail(
            f"paired process sets differ for {key}: {baseline_impl}={sorted(baseline)} "
            f"{target_impl}={sorted(target)}"
        )
    ratios = []
    wins = 0
    for process_run in sorted(baseline):
        denominator = target[process_run]
        if denominator == 0.0:
            fail(f"zero target runtime for {key}, process {process_run}, {target_impl}")
        value = baseline[process_run] / denominator
        ratios.append(value)
        if value > 1.0:
            wins += 1
    return {
        "samples": len(ratios),
        "median": statistics.median(ratios),
        "q1": percentile(ratios, 0.25),
        "q3": percentile(ratios, 0.75),
        "wins": wins,
    }


def fmt_float(value):
    return "" if value is None else f"{value:.17g}"


def fmt_int(value):
    return "" if value is None else str(value)


def ratio(numerator, denominator):
    return None if denominator == 0.0 else numerator / denominator


def break_even(baseline_runtime, target_runtime, target_staging):
    saving = baseline_runtime - target_runtime
    if saving <= 0.0:
        return saving, "target_not_faster", None, None
    exact = target_staging / saving
    return saving, "finite", exact, math.ceil(exact)


def markdown_num(value, scale=1.0, digits=3):
    if value is None:
        return "—"
    return f"{value * scale:.{digits}f}"


def markdown_be(status, ceiling):
    if status != "finite":
        return "—"
    return str(ceiling)


def main():
    if len(sys.argv) != 2:
        raise SystemExit("usage: analyze_kernel_evaluation.py EVALUATION_DIR")

    base = Path(sys.argv[1]).resolve()
    metrics_path = base / "summary" / "metrics.csv"
    if not metrics_path.is_file():
        fail(f"missing summary metrics: {metrics_path}")

    with metrics_path.open(newline="", encoding="utf-8") as fh:
        rows = list(csv.DictReader(fh))
    if not rows:
        fail("metrics.csv has no data rows")

    process_path = base / "summary" / "process_runtime.csv"
    if not process_path.is_file():
        fail(f"missing process-level runtime summary: {process_path}")
    with process_path.open(newline="", encoding="utf-8") as fh:
        process_rows = list(csv.DictReader(fh))
    if not process_rows:
        fail("process_runtime.csv has no data rows")

    process_by_condition = defaultdict(lambda: defaultdict(dict))
    for row in process_rows:
        key = condition_key(row)
        impl = row["impl"]
        process_run = i(row, "process_run")
        if process_run in process_by_condition[key][impl]:
            fail(f"duplicate process runtime for {key}, {impl}, process {process_run}")
        process_by_condition[key][impl][process_run] = f(row, "run_time_sec_median")

    by_condition = defaultdict(dict)
    for row in rows:
        key = condition_key(row)
        impl = row["impl"]
        if impl in by_condition[key]:
            fail(f"duplicate implementation {impl} for {key}")
        by_condition[key][impl] = row

    for key, impls in by_condition.items():
        actual = set(impls)
        if actual != EXPECTED_IMPLS:
            fail(f"implementation set for {key}: expected {sorted(EXPECTED_IMPLS)}, got {sorted(actual)}")
        scores = [f(impls[name], "result_score") for name in sorted(EXPECTED_IMPLS)]
        if any(not math.isclose(scores[0], score, rel_tol=1e-12, abs_tol=1e-12) for score in scores[1:]):
            fail(f"result scores disagree for {key}")

    analysis_dir = base / "analysis"
    analysis_dir.mkdir(parents=True, exist_ok=True)

    pairwise_path = analysis_dir / "pairwise_runtime.csv"
    pair_fields = [
        "model", "query", "weights", "parameter", "t_len", "comparison",
        "baseline_impl", "target_impl", "baseline_run_sec", "target_run_sec",
        "target_over_baseline_runtime_ratio", "speedup_baseline_over_target",
        "runtime_saving_sec_per_run", "target_staging_total_sec",
        "paired_process_samples", "paired_speedup_median", "paired_speedup_q1",
        "paired_speedup_q3", "target_faster_processes",
        "break_even_status", "break_even_runs_exact", "break_even_runs_ceiling",
    ]
    pair_rows = []

    condition_path = analysis_dir / "condition_comparison.csv"
    condition_fields = [
        "model", "query", "weights", "parameter", "t_len", "states", "edges", "distinct_weights",
        "handwritten_run_sec", "kernel_run_sec", "packed_run_sec", "embedded_run_sec",
        "kernel_over_handwritten_runtime_ratio",
        "handwritten_over_packed_speedup", "kernel_over_packed_speedup",
        "handwritten_over_packed_paired_median", "handwritten_over_packed_paired_q1",
        "handwritten_over_packed_paired_q3", "handwritten_over_packed_paired_wins",
        "kernel_over_packed_paired_median", "kernel_over_packed_paired_q1",
        "kernel_over_packed_paired_q3", "kernel_over_packed_paired_wins",
        "paired_process_samples",
        "handwritten_over_embedded_speedup", "kernel_over_embedded_speedup",
        "embedded_over_packed_runtime_ratio",
        "packed_staging_total_sec", "embedded_staging_total_sec", "embedded_over_packed_staging_ratio",
        "packed_residual_source_bytes", "embedded_residual_source_bytes", "embedded_over_packed_source_ratio",
        "packed_compile_peak_rss_bytes_median", "embedded_compile_peak_rss_bytes_median", "embedded_over_packed_rss_ratio",
        "handwritten_to_packed_break_even_status", "handwritten_to_packed_break_even_runs_exact", "handwritten_to_packed_break_even_runs_ceiling",
        "kernel_to_packed_break_even_status", "kernel_to_packed_break_even_runs_exact", "kernel_to_packed_break_even_runs_ceiling",
        "handwritten_to_embedded_break_even_status", "handwritten_to_embedded_break_even_runs_exact", "handwritten_to_embedded_break_even_runs_ceiling",
        "kernel_to_embedded_break_even_status", "kernel_to_embedded_break_even_runs_exact", "kernel_to_embedded_break_even_runs_ceiling",
    ]
    condition_rows = []

    for key in sorted(by_condition):
        model, query, weights, parameter, t_len = key
        impls = by_condition[key]

        for baseline_impl, target_impl, comparison in PAIR_SPECS:
            baseline = impls[baseline_impl]
            target = impls[target_impl]
            baseline_run = f(baseline, "run_time_sec_median")
            target_run = f(target, "run_time_sec_median")
            target_staging = f(target, "staging_total_sec_median") if target_impl.startswith("staged_") else 0.0
            saving = baseline_run - target_run
            status = "not_applicable"
            be_exact = None
            be_ceiling = None
            if baseline_impl in {"handwritten_packed", "kernel_reference"} and target_impl.startswith("staged_"):
                saving, status, be_exact, be_ceiling = break_even(baseline_run, target_run, target_staging)
            paired = paired_process_speedups(process_by_condition, key, baseline_impl, target_impl)
            pair_rows.append({
                "model": model,
                "query": query,
                "weights": weights,
                "parameter": parameter,
                "t_len": t_len,
                "comparison": comparison,
                "baseline_impl": baseline_impl,
                "target_impl": target_impl,
                "baseline_run_sec": fmt_float(baseline_run),
                "target_run_sec": fmt_float(target_run),
                "target_over_baseline_runtime_ratio": fmt_float(ratio(target_run, baseline_run)),
                "speedup_baseline_over_target": fmt_float(ratio(baseline_run, target_run)),
                "runtime_saving_sec_per_run": fmt_float(saving),
                "target_staging_total_sec": fmt_float(target_staging),
                "paired_process_samples": paired["samples"],
                "paired_speedup_median": fmt_float(paired["median"]),
                "paired_speedup_q1": fmt_float(paired["q1"]),
                "paired_speedup_q3": fmt_float(paired["q3"]),
                "target_faster_processes": paired["wins"],
                "break_even_status": status,
                "break_even_runs_exact": fmt_float(be_exact),
                "break_even_runs_ceiling": fmt_int(be_ceiling),
            })

        h = impls["handwritten_packed"]
        k = impls["kernel_reference"]
        p = impls["staged_packed"]
        e = impls["staged_embedded"]
        h_run = f(h, "run_time_sec_median")
        k_run = f(k, "run_time_sec_median")
        p_run = f(p, "run_time_sec_median")
        e_run = f(e, "run_time_sec_median")
        p_stage = f(p, "staging_total_sec_median")
        e_stage = f(e, "staging_total_sec_median")
        p_source = i(p, "residual_source_bytes")
        e_source = i(e, "residual_source_bytes")
        p_rss = optional_int(p, "compile_peak_rss_bytes_median")
        e_rss = optional_int(e, "compile_peak_rss_bytes_median")

        hp_paired = paired_process_speedups(process_by_condition, key, "handwritten_packed", "staged_packed")
        kp_paired = paired_process_speedups(process_by_condition, key, "kernel_reference", "staged_packed")
        if hp_paired["samples"] != kp_paired["samples"]:
            fail(f"paired process sample counts differ for {key}")

        _, hp_status, hp_exact, hp_ceil = break_even(h_run, p_run, p_stage)
        _, kp_status, kp_exact, kp_ceil = break_even(k_run, p_run, p_stage)
        _, he_status, he_exact, he_ceil = break_even(h_run, e_run, e_stage)
        _, ke_status, ke_exact, ke_ceil = break_even(k_run, e_run, e_stage)

        condition_rows.append({
            "model": model,
            "query": query,
            "weights": weights,
            "parameter": parameter,
            "t_len": t_len,
            "states": i(p, "states"),
            "edges": i(p, "edges"),
            "distinct_weights": i(p, "distinct_weights"),
            "handwritten_run_sec": fmt_float(h_run),
            "kernel_run_sec": fmt_float(k_run),
            "packed_run_sec": fmt_float(p_run),
            "embedded_run_sec": fmt_float(e_run),
            "kernel_over_handwritten_runtime_ratio": fmt_float(ratio(k_run, h_run)),
            "handwritten_over_packed_speedup": fmt_float(ratio(h_run, p_run)),
            "kernel_over_packed_speedup": fmt_float(ratio(k_run, p_run)),
            "handwritten_over_packed_paired_median": fmt_float(hp_paired["median"]),
            "handwritten_over_packed_paired_q1": fmt_float(hp_paired["q1"]),
            "handwritten_over_packed_paired_q3": fmt_float(hp_paired["q3"]),
            "handwritten_over_packed_paired_wins": hp_paired["wins"],
            "kernel_over_packed_paired_median": fmt_float(kp_paired["median"]),
            "kernel_over_packed_paired_q1": fmt_float(kp_paired["q1"]),
            "kernel_over_packed_paired_q3": fmt_float(kp_paired["q3"]),
            "kernel_over_packed_paired_wins": kp_paired["wins"],
            "paired_process_samples": hp_paired["samples"],
            "handwritten_over_embedded_speedup": fmt_float(ratio(h_run, e_run)),
            "kernel_over_embedded_speedup": fmt_float(ratio(k_run, e_run)),
            "embedded_over_packed_runtime_ratio": fmt_float(ratio(e_run, p_run)),
            "packed_staging_total_sec": fmt_float(p_stage),
            "embedded_staging_total_sec": fmt_float(e_stage),
            "embedded_over_packed_staging_ratio": fmt_float(ratio(e_stage, p_stage)),
            "packed_residual_source_bytes": p_source,
            "embedded_residual_source_bytes": e_source,
            "embedded_over_packed_source_ratio": fmt_float(ratio(e_source, p_source)),
            "packed_compile_peak_rss_bytes_median": fmt_int(p_rss),
            "embedded_compile_peak_rss_bytes_median": fmt_int(e_rss),
            "embedded_over_packed_rss_ratio": fmt_float(None if p_rss is None or e_rss is None else ratio(e_rss, p_rss)),
            "handwritten_to_packed_break_even_status": hp_status,
            "handwritten_to_packed_break_even_runs_exact": fmt_float(hp_exact),
            "handwritten_to_packed_break_even_runs_ceiling": fmt_int(hp_ceil),
            "kernel_to_packed_break_even_status": kp_status,
            "kernel_to_packed_break_even_runs_exact": fmt_float(kp_exact),
            "kernel_to_packed_break_even_runs_ceiling": fmt_int(kp_ceil),
            "handwritten_to_embedded_break_even_status": he_status,
            "handwritten_to_embedded_break_even_runs_exact": fmt_float(he_exact),
            "handwritten_to_embedded_break_even_runs_ceiling": fmt_int(he_ceil),
            "kernel_to_embedded_break_even_status": ke_status,
            "kernel_to_embedded_break_even_runs_exact": fmt_float(ke_exact),
            "kernel_to_embedded_break_even_runs_ceiling": fmt_int(ke_ceil),
        })

    with pairwise_path.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=pair_fields)
        writer.writeheader()
        writer.writerows(pair_rows)

    with condition_path.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=condition_fields)
        writer.writeheader()
        writer.writerows(condition_rows)

    report_path = analysis_dir / "report.md"
    with report_path.open("w", encoding="utf-8") as fh:
        fh.write("# Finite-kernel evaluation analysis\n\n")
        fh.write("The fixed observation workload starts with `(1,1)` and uses `(21,21)` for the remaining `T-1` observations, where `T` is the condition's `t_len`. Each timed Kernel-reference invocation constructs emission columns inside ordinary inference. Handwritten-Packed and staged Packed/Embedded prepare their emission columns before the timed invocation; staged specialization is also outside runtime timing. Kernel-reference/staged ratios are therefore end-to-end implementation comparisons with different preparation boundaries, not like-for-like measurements of transition lowering alone.\n\n")
        fh.write("Runtime columns are medians of independent process medians. `K/P proc` and `H/P proc` are medians of paired per-process speedup ratios; brackets show the process-level IQR. `wins` is the number of independent process pairs in which staged Packed was faster. Break-even counts use only specialization cost (`lowering_prep + code_construction + stage_run`), excluding model/transition construction. A dash means the staged target is not faster than that baseline.\n\n")
        fh.write("| model | query | weights | p | H ms | K ms | P ms | E ms | K/P proc [IQR] | H/P proc [IQR] | wins H→P | P stage s | E stage s | E/P code | E/P RSS | BE K→P | BE H→P |\n")
        fh.write("|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|\n")
        for row in condition_rows:
            parameter = row["parameter"] if row["parameter"] else "—"
            fh.write(
                f"| {row['model']} | {row['query']} | {row['weights']} | {parameter} | "
                f"{markdown_num(float(row['handwritten_run_sec']), 1000)} | "
                f"{markdown_num(float(row['kernel_run_sec']), 1000)} | "
                f"{markdown_num(float(row['packed_run_sec']), 1000)} | "
                f"{markdown_num(float(row['embedded_run_sec']), 1000)} | "
                f"{markdown_num(float(row['kernel_over_packed_paired_median']))} "
                f"[{markdown_num(float(row['kernel_over_packed_paired_q1']))}, {markdown_num(float(row['kernel_over_packed_paired_q3']))}] | "
                f"{markdown_num(float(row['handwritten_over_packed_paired_median']))} "
                f"[{markdown_num(float(row['handwritten_over_packed_paired_q1']))}, {markdown_num(float(row['handwritten_over_packed_paired_q3']))}] | "
                f"{row['handwritten_over_packed_paired_wins']}/{row['paired_process_samples']} | "
                f"{markdown_num(float(row['packed_staging_total_sec']))} | "
                f"{markdown_num(float(row['embedded_staging_total_sec']))} | "
                f"{markdown_num(float(row['embedded_over_packed_source_ratio']))} | "
                f"{markdown_num(None if not row['embedded_over_packed_rss_ratio'] else float(row['embedded_over_packed_rss_ratio']))} | "
                f"{markdown_be(row['kernel_to_packed_break_even_status'], None if not row['kernel_to_packed_break_even_runs_ceiling'] else int(row['kernel_to_packed_break_even_runs_ceiling']))} | "
                f"{markdown_be(row['handwritten_to_packed_break_even_status'], None if not row['handwritten_to_packed_break_even_runs_ceiling'] else int(row['handwritten_to_packed_break_even_runs_ceiling']))} |\n"
            )

        k_p = [float(r["kernel_over_packed_paired_median"]) for r in condition_rows]
        h_p = [float(r["handwritten_over_packed_paired_median"]) for r in condition_rows]
        e_p_code = [float(r["embedded_over_packed_source_ratio"]) for r in condition_rows]
        e_p_stage = [float(r["embedded_over_packed_staging_ratio"]) for r in condition_rows]
        fh.write("\n## Cross-condition medians\n\n")
        fh.write(f"- Paired process median Kernel-reference / staged-Packed runtime ratio: {statistics.median(k_p):.3f}×.\n")
        fh.write(f"- Paired process median Handwritten-Packed / staged-Packed runtime ratio: {statistics.median(h_p):.3f}×.\n")
        fh.write(f"- Embedded / Packed residual-source-size ratio: {statistics.median(e_p_code):.3f}×.\n")
        fh.write(f"- Embedded / Packed specialization-cost ratio: {statistics.median(e_p_stage):.3f}×.\n")

        finite_kp = [int(r["kernel_to_packed_break_even_runs_ceiling"]) for r in condition_rows if r["kernel_to_packed_break_even_status"] == "finite"]
        finite_hp = [int(r["handwritten_to_packed_break_even_runs_ceiling"]) for r in condition_rows if r["handwritten_to_packed_break_even_status"] == "finite"]
        fh.write(f"- Kernel→Packed finite break-even conditions: {len(finite_kp)}/{len(condition_rows)}")
        if finite_kp:
            fh.write(f", median ceiling {statistics.median(finite_kp):g} runs")
        fh.write(".\n")
        fh.write(f"- Handwritten→Packed finite break-even conditions: {len(finite_hp)}/{len(condition_rows)}")
        if finite_hp:
            fh.write(f", median ceiling {statistics.median(finite_hp):g} runs")
        fh.write(".\n")

    print(
        f"ANALYSIS conditions={len(condition_rows)} comparisons={len(pair_rows)} "
        f"condition_csv={condition_path} pairwise_csv={pairwise_path} report={report_path}"
    )


if __name__ == "__main__":
    main()
