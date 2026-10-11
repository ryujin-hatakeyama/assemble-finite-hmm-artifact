#!/usr/bin/env python3
"""Create paper-ready figures for the finite-HMM CP5E native evaluation.

By default the script selects the latest results/performance/cp5e-native-final-*
directory in this artifact and writes figures to <result-dir>/figures/.

Pass --out-dir to keep generated figures outside archived evidence.

Examples:
    python3 bench/plot_kernel_evaluation.py \
        --result-dir results/performance/cp5e-native-final-20260929 \
        --out-dir /tmp/assemble-figures --model variant

Dependencies:
    Python 3.10+, numpy, pandas, matplotlib
"""

from __future__ import annotations

import argparse
from pathlib import Path
import sys

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter, LogLocator


IMPL_ORDER = [
    "handwritten_packed",
    "kernel_reference",
    "staged_packed",
    "staged_embedded",
]

IMPL_LABEL = {
    "handwritten_packed": "Handwritten",
    "kernel_reference": "Kernel",
    "staged_packed": "Packed",
    "staged_embedded": "Embedded",
}

IMPL_COLOR = {
    "handwritten_packed": "#2F2F2F",
    "kernel_reference": "#8A8A8A",
    "staged_packed": "#0072B2",
    "staged_embedded": "#D55E00",
}

IMPL_MARKER = {
    "handwritten_packed": "o",
    "kernel_reference": "s",
    "staged_packed": "^",
    "staged_embedded": "D",
}

# Current model identifiers and their display labels.
MODEL_LABEL = {
    "original": "Original model",
    "variant": "Variant model",
}

# The archived evidence was recorded when the models were called grid3 and
# grid4. Those historical values are mapped to the current identifiers only
# when result tables are read; the archived files themselves are unchanged.
HISTORICAL_MODEL_IDS = {
    "grid3": "original",
    "grid4": "variant",
}

QUERY_COLOR = {
    "forward": "#0072B2",
    "viterbi": "#D55E00",
}

QUERY_MARKER = {
    "forward": "o",
    "viterbi": "s",
}


PAPER_RC = {
    "font.family": "sans-serif",
    "font.size": 9.0,
    "axes.labelsize": 9.0,
    "axes.titlesize": 10.0,
    "legend.fontsize": 8.0,
    "xtick.labelsize": 8.5,
    "ytick.labelsize": 8.5,
    "axes.linewidth": 0.8,
    "xtick.major.width": 0.8,
    "ytick.major.width": 0.8,
    "pdf.fonttype": 42,
    "ps.fonttype": 42,
    "savefig.bbox": "tight",
    "savefig.pad_inches": 0.03,
}


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument(
        "--result-dir",
        type=Path,
        default=None,
        help="CP5E result directory. Default: latest cp5e-native-final-* result.",
    )
    p.add_argument(
        "--out-dir",
        type=Path,
        default=None,
        help="Output directory. Default: <result-dir>/figures.",
    )
    p.add_argument(
        "--model",
        default="variant",
        choices=sorted(MODEL_LABEL),
        help=(
            "Model shown in figures (default: variant). Archived results that "
            "record grid3/grid4 are read as original/variant."
        ),
    )
    p.add_argument(
        "--formats",
        default="pdf,png",
        help="Comma-separated output formats (default: pdf,png).",
    )
    p.add_argument(
        "--dpi",
        type=int,
        default=220,
        help="PNG resolution (default: 220).",
    )
    return p.parse_args()


def latest_cp5e_result(artifact_root: Path) -> Path:
    base = artifact_root / "results" / "performance"
    dirs = sorted(p for p in base.glob("cp5e-native-final-*") if p.is_dir())
    if not dirs:
        raise FileNotFoundError(f"No cp5e-native-final-* directory under {base}")
    return dirs[-1]


def load_tables(result_dir: Path) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    metrics_path = result_dir / "summary" / "metrics.csv"
    comparison_path = result_dir / "analysis" / "condition_comparison.csv"
    pairwise_path = result_dir / "analysis" / "pairwise_runtime.csv"
    missing = [p for p in (metrics_path, comparison_path, pairwise_path) if not p.is_file()]
    if missing:
        raise FileNotFoundError("Missing required result files:\n  " + "\n  ".join(map(str, missing)))
    return tuple(
        current_model_ids(pd.read_csv(path))
        for path in (metrics_path, comparison_path, pairwise_path)
    )


def current_model_ids(df: pd.DataFrame) -> pd.DataFrame:
    df["model"] = df["model"].replace(HISTORICAL_MODEL_IDS)
    return df


def condition_sort_key(weights: str, parameter: float | str | None) -> tuple[int, float]:
    if weights == "constant":
        return (0, 0.0)
    return (1, float(parameter))


def condition_label(weights: str, parameter: float | str | None) -> str:
    if weights == "constant":
        return "constant"
    return rf"$p={float(parameter):.2f}$"


def ordered_conditions(df: pd.DataFrame) -> list[tuple[str, float | None, str]]:
    items: list[tuple[str, float | None, str]] = []
    seen: set[tuple[str, float | None]] = set()
    for row in df[["weights", "parameter"]].drop_duplicates().itertuples(index=False):
        p = None if pd.isna(row.parameter) else float(row.parameter)
        key = (str(row.weights), p)
        if key not in seen:
            seen.add(key)
            items.append((str(row.weights), p, condition_label(str(row.weights), p)))
    items.sort(key=lambda z: condition_sort_key(z[0], z[1]))
    return items


def select_condition(df: pd.DataFrame, weights: str, parameter: float | None) -> pd.DataFrame:
    part = df[df["weights"].eq(weights)]
    if weights == "constant":
        return part[part["parameter"].isna()]
    return part[np.isclose(part["parameter"].astype(float), float(parameter))]


def finish_axes(ax: plt.Axes) -> None:
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(direction="out", length=3)


def save_figure(fig: plt.Figure, out_base: Path, formats: list[str], dpi: int) -> None:
    out_base.parent.mkdir(parents=True, exist_ok=True)
    for fmt in formats:
        fmt = fmt.strip().lower()
        if not fmt:
            continue
        kwargs = {"format": fmt}
        if fmt in {"png", "jpg", "jpeg"}:
            kwargs["dpi"] = dpi
        fig.savefig(out_base.with_suffix(f".{fmt}"), **kwargs)
    plt.close(fig)


def plot_runtime(
    metrics: pd.DataFrame,
    *,
    model: str,
    query: str,
    out_dir: Path,
    formats: list[str],
    dpi: int,
) -> None:
    df = metrics[(metrics["model"] == model) & (metrics["query"] == query)].copy()
    df = df[df["impl"].isin(IMPL_ORDER)]
    if df.empty:
        raise RuntimeError(f"No runtime rows for model={model}, query={query}")

    conditions = ordered_conditions(df)
    y0 = np.arange(len(conditions), dtype=float)
    offsets = np.linspace(-0.24, 0.24, len(IMPL_ORDER))

    fig, ax = plt.subplots(figsize=(5.45, 3.35))

    for y in y0:
        ax.axhline(y, color="#ECECEC", linewidth=0.7, zorder=0)

    for impl, off in zip(IMPL_ORDER, offsets):
        xs: list[float] = []
        xerr_lo: list[float] = []
        xerr_hi: list[float] = []
        ys: list[float] = []
        for i, (weights, parameter, _) in enumerate(conditions):
            row = select_condition(df[df["impl"] == impl], weights, parameter)
            if len(row) != 1:
                raise RuntimeError(
                    f"Expected one row for {model}/{query}/{weights}/{parameter}/{impl}; "
                    f"found {len(row)}"
                )
            r = row.iloc[0]
            med = 1000.0 * float(r["run_time_sec_median"])
            q1 = 1000.0 * float(r["run_time_sec_q1"])
            q3 = 1000.0 * float(r["run_time_sec_q3"])
            xs.append(med)
            xerr_lo.append(max(0.0, med - q1))
            xerr_hi.append(max(0.0, q3 - med))
            ys.append(y0[i] + off)

        ax.errorbar(
            xs,
            ys,
            xerr=np.vstack([xerr_lo, xerr_hi]),
            fmt=IMPL_MARKER[impl],
            color=IMPL_COLOR[impl],
            markerfacecolor=IMPL_COLOR[impl],
            markeredgecolor="white",
            markeredgewidth=0.45,
            markersize=5.2,
            elinewidth=0.9,
            capsize=1.8,
            capthick=0.8,
            linewidth=0,
            label=IMPL_LABEL[impl],
            zorder=3,
        )

    ax.set_yticks(y0, [c[2] for c in conditions])
    ax.invert_yaxis()
    ax.set_xlabel("Runtime (ms), median with IQR")
    ax.set_title(f"{MODEL_LABEL[model]}, {query.capitalize()}  ($T=300$)", loc="left", pad=32)
    ax.grid(axis="x", color="#E3E3E3", linewidth=0.7)
    ax.legend(
        loc="lower center",
        bbox_to_anchor=(0.5, 1.015),
        ncol=4,
        frameon=False,
        handletextpad=0.35,
        columnspacing=1.0,
    )
    finish_axes(ax)
    fig.subplots_adjust(top=0.73, left=0.18, right=0.98, bottom=0.17)
    save_figure(fig, out_dir / f"{model}_{query}_runtime", formats, dpi)


def plot_packed_over_embedded_runtime_ratio(
    comparison: pd.DataFrame,
    *,
    model: str,
    out_dir: Path,
    formats: list[str],
    dpi: int,
) -> None:
    """Plot Packed condition median runtime / Embedded condition median runtime.

    Each condition median is the median over processes of the per-process
    median runtime (packed_run_sec and embedded_run_sec in
    condition_comparison.csv). This is a ratio of implementation medians, not
    a median of paired per-process ratios.
    """
    df = comparison[comparison["model"] == model].copy()
    if df.empty:
        raise RuntimeError(f"No comparison rows for model={model}")

    conditions = ordered_conditions(df)
    x = np.arange(len(conditions), dtype=float)

    fig, ax = plt.subplots(figsize=(5.45, 3.0))
    ax.axhline(1.0, color="#555555", linewidth=0.9, linestyle="--", zorder=1)

    offsets = {"forward": -0.07, "viterbi": 0.07}
    for query in ("forward", "viterbi"):
        qdf = df[df["query"] == query]
        ratios: list[float] = []
        xs: list[float] = []
        for i, (weights, parameter, _) in enumerate(conditions):
            row = select_condition(qdf, weights, parameter)
            if len(row) != 1:
                raise RuntimeError(
                    f"Expected one condition row for {model}/{query}/{weights}/{parameter}; "
                    f"found {len(row)}"
                )
            r = row.iloc[0]
            ratios.append(packed_over_embedded_runtime_ratio(r))
            xs.append(x[i] + offsets[query])

        ax.plot(
            xs,
            ratios,
            QUERY_MARKER[query],
            color=QUERY_COLOR[query],
            markerfacecolor=QUERY_COLOR[query],
            markeredgecolor="white",
            markeredgewidth=0.45,
            markersize=5.5,
            linewidth=0,
            label=query.capitalize(),
            zorder=3,
        )

    ax.set_xticks(x, [c[2] for c in conditions])
    ax.set_ylabel("Packed median / Embedded median\nruntime ratio")
    ax.set_title(
        f"{MODEL_LABEL[model]}: Packed / Embedded ratio of median runtimes",
        loc="left",
        pad=7,
    )
    ax.text(
        0.995,
        1.015,
        "above 1: Embedded median lower",
        transform=ax.get_yaxis_transform(),
        ha="right",
        va="bottom",
        fontsize=7.5,
        color="#555555",
    )
    ax.grid(axis="y", color="#E3E3E3", linewidth=0.7)
    ax.legend(frameon=False, loc="best")
    finish_axes(ax)
    fig.subplots_adjust(left=0.16, right=0.98, top=0.86, bottom=0.18)
    save_figure(fig, out_dir / f"{model}_packed_over_embedded_runtime_ratio", formats, dpi)


def packed_over_embedded_runtime_ratio(row: pd.Series) -> float:
    return float(row["packed_run_sec"]) / float(row["embedded_run_sec"])


def plot_embedded_overhead(
    comparison: pd.DataFrame,
    *,
    model: str,
    out_dir: Path,
    formats: list[str],
    dpi: int,
) -> None:
    df = comparison[comparison["model"] == model].copy()
    if df.empty:
        raise RuntimeError(f"No comparison rows for model={model}")

    metrics = [
        ("embedded_over_packed_staging_ratio", "Staging time"),
        ("embedded_over_packed_source_ratio", "Generated source"),
        ("embedded_over_packed_rss_ratio", "Compile peak RSS"),
    ]

    rows: list[tuple[str, float, float, float, str]] = []
    for column, label in metrics:
        for query in ("forward", "viterbi"):
            vals = df[df["query"] == query][column].dropna().astype(float).to_numpy()
            if len(vals) == 0:
                continue
            q1, med, q3 = np.quantile(vals, [0.25, 0.5, 0.75])
            rows.append((f"{label} — {query.capitalize()}", med, q1, q3, query))

    rows = rows[::-1]
    y = np.arange(len(rows), dtype=float)

    fig, ax = plt.subplots(figsize=(5.45, 3.35))
    ax.axvline(1.0, color="#555555", linewidth=0.9, linestyle="--", zorder=1)

    for yi, (label, med, q1, q3, query) in zip(y, rows):
        ax.errorbar(
            med,
            yi,
            xerr=np.array([[med - q1], [q3 - med]]),
            fmt=QUERY_MARKER[query],
            color=QUERY_COLOR[query],
            markerfacecolor=QUERY_COLOR[query],
            markeredgecolor="white",
            markeredgewidth=0.45,
            markersize=5.5,
            elinewidth=1.0,
            capsize=2.0,
            capthick=0.8,
            zorder=3,
        )

    ax.set_yticks(y, [r[0] for r in rows])
    ax.set_xscale("log")
    ax.set_xlabel("Embedded / Packed ratio (median, IQR across conditions)")
    ax.set_title(f"{MODEL_LABEL[model]}: specialization overhead of Embedded", loc="left", pad=7)
    ax.xaxis.set_major_locator(LogLocator(base=10.0))
    ax.xaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:g}×"))
    ax.grid(axis="x", which="major", color="#E3E3E3", linewidth=0.7)
    finish_axes(ax)
    fig.subplots_adjust(left=0.35, right=0.98, top=0.87, bottom=0.18)
    save_figure(fig, out_dir / f"{model}_embedded_overhead", formats, dpi)


def write_numeric_summary(
    comparison: pd.DataFrame,
    pairwise: pd.DataFrame,
    *,
    model: str,
    out_dir: Path,
) -> None:
    c = comparison[comparison["model"] == model].copy()
    p = pairwise[pairwise["model"] == model].copy()

    lines = [f"model={model}"]
    # Paired statistics: per-process baseline/target runtime ratios, then the
    # median over processes; the line reports the median over conditions.
    # Kernel reference builds emission columns inside its timed call, so K/P
    # is an end-to-end comparison, not an isolated staging effect.
    for comp, ratio_name in (
        ("staged_packed_vs_handwritten", "Handwritten-Packed / Packed"),
        ("staged_packed_vs_kernel", "Kernel reference / Packed"),
    ):
        vals = p[p["comparison"] == comp]["paired_speedup_median"].dropna().astype(float)
        if len(vals):
            lines.append(
                f"median over conditions of paired per-process {ratio_name} runtime ratio "
                f"[{comp}] = {np.median(vals):.6f}"
            )

    for query in ("forward", "viterbi"):
        qdf = c[c["query"] == query]
        values = []
        for weights, parameter, _ in ordered_conditions(qdf):
            row = select_condition(qdf, weights, parameter)
            if len(row) == 1:
                values.append(f"{packed_over_embedded_runtime_ratio(row.iloc[0]):.6f}")
        if values:
            lines.append(
                f"{query} Packed median / Embedded median runtime ratio by setting "
                f"(constant, then increasing p) = {', '.join(values)}"
            )

    path = out_dir / f"{model}_figure_summary.txt"
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print("\n".join(lines))


def main() -> int:
    args = parse_args()
    plt.rcParams.update(PAPER_RC)

    script_path = Path(__file__).resolve()
    if args.result_dir is None:
        result_dir = latest_cp5e_result(script_path.parent.parent)
    else:
        result_dir = args.result_dir.expanduser().resolve()

    out_dir = (
        args.out_dir.expanduser().resolve()
        if args.out_dir is not None
        else result_dir / "figures"
    )
    formats = [x.strip().lower() for x in args.formats.split(",") if x.strip()]

    metrics, comparison, pairwise = load_tables(result_dir)

    plot_runtime(
        metrics,
        model=args.model,
        query="forward",
        out_dir=out_dir,
        formats=formats,
        dpi=args.dpi,
    )
    plot_runtime(
        metrics,
        model=args.model,
        query="viterbi",
        out_dir=out_dir,
        formats=formats,
        dpi=args.dpi,
    )
    plot_packed_over_embedded_runtime_ratio(
        comparison,
        model=args.model,
        out_dir=out_dir,
        formats=formats,
        dpi=args.dpi,
    )
    plot_embedded_overhead(
        comparison,
        model=args.model,
        out_dir=out_dir,
        formats=formats,
        dpi=args.dpi,
    )
    write_numeric_summary(comparison, pairwise, model=args.model, out_dir=out_dir)

    print(f"result_dir: {result_dir}")
    print(f"figures:    {out_dir}")
    for p in sorted(out_dir.glob(f"{args.model}_*")):
        print(f"  {p.name}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:
        print(f"error: {exc}", file=sys.stderr)
        raise
