# -*- coding: utf-8 -*-
"""
共通仕様
- 左端: YORU（色で強調）
- 右側: keypoint_moseq の syllable（高い順）→ 上位 25 本のみ
- 指標: F1 と Accuracy
- 図サイズは全 Dataset で同一（ant_trophallaxis / wing_extension 共通）
- ラベル文字を大きめに調整
- グラフ体裁: adjust_spines / myax を適用
- seaborn 不使用（matplotlib のみ）
"""

import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import re

# ================= 設定（ここだけ変えればOK） =================
INPUT_XLSX  = "./data/TableS23_keypoint_moseq.xlsx"  # 入力Excel
SHEET_NAME  = None         # 先頭シートを使う場合は None、名前指定なら "Sheet1" など
OUTPUT_DIR  = "./outputs/plots_dataset_bars_pdf_color"  # 出力フォルダ（自動作成）

YORU_COLOR   = "#4D4D4D"   # YORU の色（強調）
KPM_COLOR    = "#A8A8A8"   # syllable 群の色
TOP_SYLLABLES = 25         # 表示する syllable 上位本数

# 図サイズ（全Dataset共通）
FIG_WIDTH  = 10.0
FIG_HEIGHT = 3.0

# 文字サイズ・余白
XTICK_ROT_DEG   = 90       # x軸ラベルの回転
XTICK_FONTSIZE  = 12       # x軸ラベル
YLABEL_FONTSIZE = 13       # y軸ラベル
TITLE_FONTSIZE  = 15       # タイトル
BOTTOM_PAD      = 0.25     # 下余白（ラベル衝突回避）

# 余白（棒とプロット端の隙間の最小化用）
BAR_WIDTH     = 0.9        # align='edge' 時の幅（0<width<=1）
LEFT_MARGIN   = 0.02       # 左端余白（単位：x座標、0でピッタリ）
RIGHT_MARGIN  = 0.02       # 右端余白（小さいほど詰める）
# ===================================


# --- 体裁調整関数 ---
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
    """上・右枠を非表示、点線グリッド、adjust_spines を適用。"""
    ax = ax or plt.gca()
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    # ax.grid(True, axis='y', linestyle='dotted', linewidth=1.0)
    adjust_spines(ax, spines=('left', 'bottom'))


# --- ユーティリティ ---
def normalize_cols(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    out.columns = [c.strip().lower().replace(" ", "_") for c in df.columns]
    return out

def find_col(df: pd.DataFrame, candidates):
    for c in candidates:
        if c in df.columns:
            return c
    return None

def get_yoru_value(sub: pd.DataFrame, method_col: str, metric_col: str):
    yoru = sub[sub[method_col].str.lower() == "yoru"]
    if yoru.empty:
        return None
    return float(yoru[metric_col].astype(float).mean())

def get_kpm_syllables(sub: pd.DataFrame, method_col: str, syll_col: str, metric_col: str):
    """keypoint_moseq の syllable ごとの平均値を高い順で返す [(syllable, value), ...]"""
    kpm = sub[sub[method_col].str.lower() == "keypoint_moseq"]
    if kpm.empty:
        return []
    if syll_col is None:
        kpm = kpm.reset_index(drop=True)
        kpm["__syll__"] = kpm.index.astype(str)
        grp = kpm.groupby("__syll__", as_index=False)[metric_col].mean().rename(columns={"__syll__": "syllable"})
    else:
        grp = kpm.groupby(syll_col, as_index=False)[metric_col].mean().rename(columns={syll_col: "syllable"})
    grp = grp.sort_values(metric_col, ascending=False).reset_index(drop=True)
    return list(zip(grp["syllable"].astype(str).tolist(), grp[metric_col].astype(float).tolist()))

def _extract_sylnum(label: str) -> int | None:
    """
    軽めの正規表現でラベルから syllable 番号(整数)を抽出。
    例: "KM s12", "syllable 7", "s#003", "S 45" などを許容。
    抽出できなければ None を返す。
    """
    m = re.search(r'(?:syl{0,1}lable|syl|s)\s*#?\s*(\d+)', label, flags=re.IGNORECASE)
    return int(m.group(1)) if m else None


def write_syllable_ranking_csv(labels, values, dataset_str: str, out_dir: str):
    """
    棒グラフ用の labels/values から、F1順の syllable 番号ランキングCSVを出力。
    - YORU 行は除外（syllable番号が取れないため）
    - 同スコアは元の順を維持（stable sort）
    出力: <out_dir>/<dataset_str>_syllables_by_f1.csv
    """
    rows = []
    for lab, val in zip(labels, values):
        sid = _extract_sylnum(lab)
        if sid is not None:
            rows.append({"syllable": sid, "f1": float(val)})

    if not rows:
        # 何も取れないケースはスキップ
        return

    df = pd.DataFrame(rows)
    df = df.sort_values("f1", ascending=False, kind="mergesort").reset_index(drop=True)
    df.insert(0, "rank", df.index + 1)

    out_csv = os.path.join(out_dir, f"{dataset_str}_syllables_by_f1.csv")
    df.to_csv(out_csv, index=False, encoding="utf-8")
    print(f"[CSV] Wrote syllable ranking by F1: {os.path.abspath(out_csv)}")


# --- 入力読み込み ---
os.makedirs(OUTPUT_DIR, exist_ok=True)
xls = pd.ExcelFile(INPUT_XLSX)
sheet_to_use = SHEET_NAME or xls.sheet_names[0]
df = pd.read_excel(xls, sheet_to_use)
df = normalize_cols(df)

# 必須列
dataset_col = find_col(df, ["dataset"])
method_col  = find_col(df, ["method", "model", "approach"])
syll_col    = find_col(df, ["syllable", "syllable_id", "cluster", "label"])
f1_col      = find_col(df, ["f1", "f1_score", "f1score"])
acc_col     = find_col(df, ["accuracy", "acc"])
missing = [name for name, col in [
    ("Dataset", dataset_col), ("Method", method_col),
    ("F1", f1_col), ("Accuracy", acc_col)
] if col is None]
if missing:
    raise ValueError(f"Missing required columns: {', '.join(missing)}; found: {list(df.columns)}")

df[method_col] = df[method_col].astype(str).str.strip()

# --- 図作成（全Datasetで同一 figsize・左右余白圧縮・y軸0-1固定） ---
for ds, sub in df.groupby(dataset_col):
    ds_str = str(ds)

    for metric_col, metric_name in [(f1_col, "F1"), (acc_col, "Accuracy")]:
        yoru_val = get_yoru_value(sub, method_col, metric_col)
        kpm_vals = get_kpm_syllables(sub, method_col, syll_col, metric_col)

        # 上位 25 syllable に限定
        kpm_vals = kpm_vals[:TOP_SYLLABLES]

        labels, values = [], []
        if yoru_val is not None:
            labels.append("YORU")
            values.append(yoru_val)
        for s, v in kpm_vals:
            labels.append("Syllable" + str(s))
            values.append(float(v))
        if not labels:
            continue

        n = len(labels)
        x = np.arange(n)  # 0..n-1（棒の左端基準）

        # 色（YORUのみ強調色）
        colors = [YORU_COLOR] + [KPM_COLOR]*(n-1) if labels[0] == "YORU" else [KPM_COLOR]*n

        plt.figure(figsize=(FIG_WIDTH, FIG_HEIGHT))
        ax = plt.gca()
        ax.bar(x, values, width=BAR_WIDTH, align='edge', color=colors)

        # 目盛りはバーの中央に
        ax.set_xticks(x + BAR_WIDTH/2.0)
        

        ax.set_ylabel(metric_name, fontsize=YLABEL_FONTSIZE)
        ax.set_title(f"{ds_str} — {metric_name}", fontsize=TITLE_FONTSIZE)

        # --- y軸 0–1 固定 ---
        ax.set_ylim(0.0, 1.0)

        # --- 左右余白最小化（左0、右n-RIGHT_MARGIN） ---
        ax.set_xlim(0.0 - LEFT_MARGIN, n - RIGHT_MARGIN)

        # 体裁適用 + 余白（下側）
        myax(ax)
        plt.gcf().subplots_adjust(bottom=BOTTOM_PAD)
        plt.tight_layout()
        ax.set_xticklabels(labels, rotation=XTICK_ROT_DEG, ha="right", fontsize=XTICK_FONTSIZE)

        out_pdf = os.path.join(OUTPUT_DIR, f"{ds_str}_{metric_name}.pdf")
        
        plt.savefig(out_pdf)
        plt.close()

        # === 追加: F1 のときだけ syllable ランキングCSVを出力 ===
        if metric_name.lower() in ("f1", "f1_score", "f1score"):
            # ここで使う labels / values は、そのデータセットの KM + YORU を並べた棒グラフに使ったもの
            write_syllable_ranking_csv(labels, values, ds_str, OUTPUT_DIR)

print(f"Saved PDFs to: {os.path.abspath(OUTPUT_DIR)}")