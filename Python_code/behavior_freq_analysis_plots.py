#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path

# ===== 設定 =====
INPUT_CSV = "behavior_rate_f1_score.csv"
OUT_PDF   = "scatter_f1_vs_behavior_per_min.pdf"
OUT_CSV   = "correlation_test_summary.csv"
FIGSIZE   = (5.5, 4.0)
MARKER_SIZE = 36
EDGE_WIDTH  = 1.5
PERM_N      = 10000
PERM_SEED   = 42
# =================

def adjust_spines(ax=None, spines=('left', 'bottom')):
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
    ax = ax or plt.gca()
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    adjust_spines(ax, spines=('left', 'bottom'))

# ---- Spearman（scipyあり/なし両対応） ----
def _spearman_rho_numpy(x: np.ndarray, y: np.ndarray) -> float:
    rx = pd.Series(x).rank(method="average").to_numpy()
    ry = pd.Series(y).rank(method="average").to_numpy()
    return np.corrcoef(rx, ry)[0, 1]

def spearman_with_perm(x, y, B=PERM_N, seed=PERM_SEED):
    """ρ, p（scipy優先）, 置換z, 置換p を返す"""
    rho_perm = None
    p_perm = None
    z_perm = None
    # 置換（常に実行して z を得る）
    rng = np.random.default_rng(seed)
    rho_obs = _spearman_rho_numpy(x, y)
    null_vals = np.empty(B, dtype=float)
    for b in range(B):
        y_perm = rng.permutation(y)
        null_vals[b] = _spearman_rho_numpy(x, y_perm)
    # two-sided p（+1補正）
    p_perm = (np.sum(np.abs(null_vals) >= abs(rho_obs)) + 1) / (B + 1)
    # z = (観測値 - 帰無平均) / 帰無SD
    z_perm = (rho_obs - null_vals.mean()) / null_vals.std(ddof=1)
    rho_perm = rho_obs

    # scipy があればその p も併記
    p_scipy = None
    try:
        from scipy import stats
        rho_s, p_s = stats.spearmanr(x, y, nan_policy="omit")
        return float(rho_s), float(p_s), float(z_perm), float(p_perm)
    except Exception:
        return float(rho_perm), float(p_perm), float(z_perm), float(p_perm)

# ---- Kendall（C:一致ペア数も算出） ----
def kendall_with_concordance(x, y):
    try:
        from scipy import stats
        kt = stats.kendalltau(x, y, alternative='two-sided', nan_policy='omit')
        tau, p = float(kt.statistic), float(kt.pvalue)
        # concordant/discordant を数える（O(n^2)）
        n = len(x)
        C = D = T = U = 0
        for i in range(n-1):
            for j in range(i+1, n):
                xi, xj = x[i], x[j]
                yi, yj = y[i], y[j]
                sx = np.sign(xi - xj)
                sy = np.sign(yi - yj)
                s = sx * sy
                if s > 0:
                    C += 1
                elif s < 0:
                    D += 1
                else:
                    # 同順位（どちらかが同じ）
                    if xi == xj and yi == yj:
                        T += 1
                    else:
                        U += 1
        total_pairs = n * (n - 1) // 2
        conc_rate = C / total_pairs if total_pairs > 0 else np.nan
        # scipyはz値も返すAPIがないので近似zを計算（ties無視の簡便式）
        # 注意: 小標本/多数の同順位では近似精度が落ちます。
        if total_pairs > 0:
            var_tau = (2*(2*n+5)) / (9*n*(n-1))
            z = tau / np.sqrt(var_tau)
        else:
            z = np.nan
        return tau, p, z, C, conc_rate, total_pairs
    except Exception:
        return np.nan, np.nan, np.nan, np.nan, np.nan, np.nan

def main():
    # データ読み込み
    csv_path = Path(INPUT_CSV)
    if not csv_path.exists():
        raise FileNotFoundError(f"CSVが見つかりません: {csv_path.resolve()}")

    df = pd.read_csv(csv_path)

    required = ["behavior_per_min", "f1_score"]
    missing = [c for c in required if c not in df.columns]
    if missing:
        raise ValueError(f"CSVに必要な列がありません: {missing}  （必要列: {required}）")

    data = df[required].astype(float).dropna()
    x = data["behavior_per_min"].to_numpy()
    y = data["f1_score"].to_numpy()
    if len(x) < 2:
        raise ValueError("相関と回帰直線の計算には2点以上が必要です。")

    # Pearson（図用）
    r = np.corrcoef(x, y)[0, 1]

    # Spearman
    rho, p_s, z_s, p_perm = spearman_with_perm(x, y, B=PERM_N, seed=PERM_SEED)

    # Kendall（Concordance付き）
    tau, p_k, z_k, C, conc_rate, total_pairs = kendall_with_concordance(x, y)

    # === 図 ===
    slope, intercept = np.polyfit(x, y, 1)
    x_line = np.linspace(np.nanmin(x), np.nanmax(x), 200)
    y_line = slope * x_line + intercept

    plt.close("all")
    fig, ax = plt.subplots(figsize=FIGSIZE)
    myax(ax)
    ax.scatter(x, y, s=MARKER_SIZE, facecolors='none',
               edgecolors='blue', linewidths=EDGE_WIDTH)
    ax.plot(x_line, y_line, color='gray', linewidth=1.5)
    ax.set_ylim(0, 1)
    ax.set_xlim(0, 1)
    ax.set_xlabel("behavior_per_min")
    ax.set_ylabel("f1_score")

    # 右下に相関注記（Spearmanを主表示）
    ax.text(0.98, 0.02,
            f"Spearman ρ = {rho:.3f}\n"
            f"p = {p_s:.4g} (perm p = {p_perm:.4g})\n"
            f"Pearson r = {r:.3f}",
            transform=ax.transAxes, ha='right', va='bottom', color='black')

    fig.tight_layout()
    fig.savefig(OUT_PDF, dpi=300)
    print(f"Saved: {OUT_PDF}")

    # === 要求された出力形式 ===
    rows = []
    rows.append({
        "method": "Spearman",
        "statistic": rho,                     # ρ
        "statistical_value": z_s,             # permutation z
        "p_value": p_s,                       # scipy p（なければpermと同じ）
        "Concordance": "NA"
    })
    rows.append({
        "method": "Kendall",
        "statistic": tau,                     # τ
        "statistical_value": z_k,             # 近似z
        "p_value": p_k,
        "Concordance": f"{C} / {total_pairs} = {conc_rate:.3f}"
    })
    out = pd.DataFrame(rows)
    out.to_csv(OUT_CSV, index=False)
    print("\n=== Correlation test summary ===")
    print(out.to_string(index=False))
    print(f"\nSaved: {OUT_CSV}")

if __name__ == "__main__":
    main()
