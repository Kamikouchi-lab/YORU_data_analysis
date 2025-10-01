#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Bar: total (per method) in grayscale + Group points connected by dotted lines (black).
- File path hardcoded (no args)
- Black & white
- Output: PDF only
- Uses adjust_spines() / myax() helpers
"""

import pandas as pd
import matplotlib.pyplot as plt

# ==== Settings ====
INPUT_XLSX = "TableS4_fly_wing_extesniton_asoid_resuls_table.xlsx"   # put this .py next to the .xlsx or change path
SHEET_NAME = "fly_wing_extesniton_asoid_resul"
METRIC = "f1_score"   # "precision", "recall", "accuracy", or "f1_score"
OUT_PDF = "wing_extension_total_bar_with_groups_bw.pdf"
FIGSIZE = (6, 4)
# ==================

# --- Styling helpers ---
def adjust_spines(ax=None, spines=('left', 'bottom')):
    """枠線・目盛を整える。指定 spines を外側に出し、その他は非表示。"""
    ax = ax or plt.gca()
    ax.spines['top'].set_linewidth(0)
    ax.spines['left'].set_linewidth(1.5)
    ax.spines['left'].set_color('k')
    ax.spines['bottom'].set_linewidth(1.5)
    ax.spines['bottom'].set_color('k')
    ax.tick_params(direction='out', length=6, width=1.5, color='k')
    for loc, spine in ax.spines.items():
        if loc in spines:
            spine.set_position(('outward', 2.0))
        else:
            spine.set_color('none')

def myax(ax=None):
    """上・右枠を非表示、点線グリッドなし、adjust_spines を適用。"""
    ax = ax or plt.gca()
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    adjust_spines(ax, spines=('left', 'bottom'))

def main() -> None:
    df = pd.read_excel(INPUT_XLSX, sheet_name=SHEET_NAME)

    # Validate
    required_cols = {"method", "group", METRIC}
    missing = required_cols - set(df.columns)
    if missing:
        raise ValueError(f"Missing columns in data: {missing}")

    # Preserve original appearance order, but force "YORU" to the first position if present
    seen = set()
    methods_order = []
    for m in df["method"]:
        if m not in seen:
            seen.add(m)
            methods_order.append(m)

    if any(m == "YORU" for m in methods_order):
        methods = ["YORU"] + [m for m in methods_order if m != "YORU"]
    else:
        methods = methods_order

    # Totals per method in the specified order
    totals = (
        df[df["group"].str.lower() == "total"][["method", METRIC]]
        .set_index("method")
        .reindex(methods)
    )

    # Groups (exclude total)
    groups = [g for g in df["group"].unique() if str(g).lower() != "total"]

    # ==== Plot ====
    plt.figure(figsize=FIGSIZE)
    ax = plt.gca()
    myax(ax)

    # Bars (grayscale, no hatching)
    x = range(len(methods))
    bar_vals = totals[METRIC].values
    ax.bar(x, bar_vals, color="0.3", edgecolor="k", linewidth=1.2)

    # Group points + dotted connectors in black
    group_positions = {}
    for g in groups:
        xy = []
        for xi, m in enumerate(methods):
            y = df[(df["method"] == m) & (df["group"] == g)][METRIC]
            if not y.empty:
                yy = float(y.iloc[0])
                ax.scatter([xi], [yy], marker="o", s=28, facecolors="black", edgecolors="black")
                xy.append((xi, yy))
        group_positions[g] = xy

    for g, xy in group_positions.items():
        if len(xy) >= 2:
            xs = [p[0] for p in xy]
            ys = [p[1] for p in xy]
            ax.plot(xs, ys, linestyle="dotted", color="black", linewidth=1.2)

    ax.set_ylim(0, 1)
    ax.set_xlabel("method", color="black")
    ax.set_ylabel(METRIC, color="black")
    ax.set_xticks(list(x))
    ax.set_xticklabels(methods)

    plt.tight_layout()
    plt.savefig(OUT_PDF, bbox_inches="tight")
    print(f"Saved: {OUT_PDF}")

if __name__ == "__main__":
    main()
