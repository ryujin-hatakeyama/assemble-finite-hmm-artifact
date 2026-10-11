#!/usr/bin/env python3
import csv
import math
import sys
from collections import defaultdict
from pathlib import Path


def fail(message: str) -> None:
    raise SystemExit(f"verification failed: {message}")


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


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit("usage: verify_kernel_evaluation.py CSV")
    path = Path(sys.argv[1])
    if not path.is_file():
        fail(f"missing CSV: {path}")

    with path.open(newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f))
    if not rows:
        fail("CSV has no data rows")

    groups = defaultdict(list)
    for row in rows:
        score = as_float(row, "result_score")
        run_time = as_float(row, "run_time_sec")
        if not math.isfinite(score):
            fail(f"non-finite result_score in row: {row}")
        if not math.isfinite(run_time) or run_time < 0.0:
            fail(f"invalid run_time_sec in row: {row}")

        model_build = as_float(row, "model_build_sec")
        transition_build = as_float(row, "transition_build_sec")
        lowering_prep = as_float(row, "lowering_prep_sec")
        for field, value in (
            ("model_build_sec", model_build),
            ("transition_build_sec", transition_build),
            ("lowering_prep_sec", lowering_prep),
        ):
            if not math.isfinite(value) or value < 0.0:
                fail(f"invalid {field} in row: {row}")

        impl = row["impl"]
        code_construction = as_float(row, "code_construction_sec")
        stage_run = as_float(row, "stage_run_sec")
        residual_size = as_int(row, "residual_source_bytes")
        if impl in {"handwritten_packed", "kernel_reference"}:
            if code_construction != 0.0 or stage_run != 0.0 or residual_size != 0:
                fail(f"{impl} unexpectedly has staged preparation metrics")
        elif impl in {"staged_packed", "staged_embedded"}:
            if code_construction < 0.0 or stage_run <= 0.0 or residual_size <= 0:
                fail(f"invalid staged preparation metrics for {impl}: {row}")
        else:
            fail(f"unknown impl {impl!r}")

        key = (
            row["process_run"],
            row["model"],
            row["query"],
            row["weights"],
            row["parameter"],
            row["t_len"],
            row["repetition"],
        )
        groups[key].append(row)

    compared_groups = 0
    for key, group in groups.items():
        by_impl = {row["impl"]: row for row in group}
        expected = {"handwritten_packed", "kernel_reference", "staged_packed", "staged_embedded"}
        if expected.issubset(by_impl):
            scores = {impl: as_float(by_impl[impl], "result_score") for impl in expected}
            reference = scores["kernel_reference"]
            for impl, score in scores.items():
                if not math.isclose(reference, score, rel_tol=1e-12, abs_tol=1e-12):
                    fail(f"score mismatch for {key}: {scores}")
            compared_groups += 1

    if compared_groups == 0:
        fail("no group contained handwritten_packed, kernel_reference, staged_packed, and staged_embedded")

    print(f"VERIFIED rows={len(rows)} compared_groups={compared_groups} csv={path}")


if __name__ == "__main__":
    main()
