# -*- coding: utf-8 -*-
"""
GT / YORU / KM を統合して、0–1200 s の 0/1 エソグラムを描画。
上から GT → YORU → KM (ランキングCSVの順 or F1上位TOP_N)。
- 全ライン黒、枠なし、x軸非表示
- 背景ハイライト（GT=1 区間）は設定でオン/オフ
- YORU: class_name に 'trophallaxis' があれば 1（重複frameはOR集約）
"""

import ast
import os
import glob
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# ===== 設定 =====
GT_CSV   = "./data/ground_truth/20231004_2.csv"
KM_CSV   = "./data/keypoint_moseq/20231004_2.csv"
YORU_CSV = "./data/yoru/20231004_2.csv"

FPS     = 30.0
TMIN    = 0.0
TMAX    = 1200.0
TOP_N   = 10

OUT_IMG = "./outputs/trophallaxis_ethogram_top10_0-1200s_ranked.pdf"
SCALE_BAR_SEC = 60.0

# 先に作成した syllable ランキングCSV（空文字なら自動探索）
SYLLABLE_RANKING_CSV = "./data/ant_trophallaxis_syllables_by_f1.csv"  # 例: "./outputs/20231004_2_syllables_by_f1.csv"

# 背景ハイライト（GT=1 の時間帯）のオン/オフと見た目
HIGHLIGHT_GT    = False
HIGHLIGHT_COLOR = "#cccccc"
HIGHLIGHT_ALPHA = 0.5
# ==============

# ---------- 共通 ----------
def crop_series(t, x, tmin, tmax):
    m = (t >= tmin) & (t < tmax)
    return t[m], x[m]

def resample_bool_by_frame(t_src, b_src, tmin, tmax, fps):
    n = int(np.round((tmax - tmin) * fps))
    if n <= 0:
        return np.zeros(0, dtype=bool)
    if len(t_src) == 0:
        return np.zeros(n, dtype=bool)
    grid_t = tmin + (np.arange(n) + 0.5) / fps
    idx = np.clip(np.searchsorted(t_src, grid_t) - 1, 0, len(t_src) - 1)
    return b_src[idx].astype(bool)

def step_from_bool(bool_arr, tmin, fps, base_y):
    n = len(bool_arr)
    if n == 0:
        return np.array([tmin]), np.array([base_y])
    t = np.arange(n + 1) / fps + tmin
    y = np.r_[bool_arr.astype(int), int(bool_arr[-1])]
    return t, y + base_y

def on_intervals_from_bool(b, tmin, fps):
    if len(b) == 0:
        return []
    b_ext = np.r_[0, b.astype(int), 0]
    diff = np.diff(b_ext)
    starts = np.where(diff == 1)[0]
    ends   = np.where(diff == -1)[0]
    return [(tmin + i0 / fps, tmin + i1 / fps) for i0, i1 in zip(starts, ends)]

# ---------- ローダ ----------
def load_gt(gt_path):
    df = pd.read_csv(gt_path)
    time_candidates = ["time", "t", "time (s)", "sec", "seconds", "timestamp", "time_s", "秒"]
    time_col = next((c for c in df.columns if str(c).strip().lower() in time_candidates), None)
    if time_col is None:
        raise ValueError("GT CSV に time 列が見つかりません")
    beh_col = next((c for c in df.columns
                    if c != time_col and pd.api.types.is_numeric_dtype(df[c]) and
                    set(pd.to_numeric(df[c], errors="coerce").dropna().unique()).issubset({0, 1})), None)
    if beh_col is None:
        for c in df.columns:
            if str(c).strip().lower() in ["trophallaxis", "behavior", "behaviour", "state", "label"]:
                vals = pd.to_numeric(df[c], errors="coerce").dropna()
                if len(vals) > 0 and set(vals.unique()).issubset({0, 1}):
                    beh_col = c
                    break
    if beh_col is None:
        raise ValueError("GT CSV に 0/1 の行動列が見つかりません")
    t = pd.to_numeric(df[time_col], errors="coerce").astype(float).values
    y = (pd.to_numeric(df[beh_col], errors="coerce").fillna(0) > 0.5).astype(int).values
    return t, y, beh_col

def parse_class_name_to01(x):
    if pd.isna(x):
        return 0
    s = str(x).strip()
    s_low = s.lower()
    if (s_low.startswith("[") and s_low.endswith("]")) or (s_low.startswith("(") and s_low.endswith(")")):
        try:
            seq = ast.literal_eval(s)
            tokens = [str(t).strip().lower() for t in seq]
            return 1 if any("trophallaxis" in t for t in tokens) else 0
        except Exception:
            pass
    for sep in [",", ";", "|", "/"]:
        if sep in s:
            tokens = [t.strip().lower() for t in s.split(sep)]
            return 1 if any("trophallaxis" in t for t in tokens) else 0
    if " " in s.strip():
        tokens = [t.strip().lower() for t in s.split()]
        return 1 if any("trophallaxis" in t for t in tokens) else 0
    return 1 if "trophallaxis" in s_low else 0

def load_yoru(yoru_path, fps):
    """class_name に 'trophallaxis' があれば 1。frame が重複する場合はフレーム単位で OR 集約。"""
    df = pd.read_csv(yoru_path)
    cname_col = next((c for c in df.columns if str(c).strip().lower() == "class_name"), None)
    if cname_col is None:
        raise ValueError("YORU CSV に 'class_name' 列がありません。")
    df["_wing01"] = df[cname_col].apply(parse_class_name_to01).astype(int)

    time_col  = next((c for c in df.columns if str(c).strip().lower() in
                      ["time", "t", "time (s)", "sec", "seconds", "timestamp", "time_s", "秒"]), None)
    frame_col = next((c for c in df.columns if str(c).strip().lower() in
                      ["frame", "frame_idx", "frame_id", "フレーム"]), None)

    if frame_col is not None:
        df["_frame_i"] = pd.to_numeric(df[frame_col], errors="coerce").fillna(0).astype(int)
        agg = df.groupby("_frame_i")["_wing01"].max().sort_index()
        frames = agg.index.values
        y01 = agg.values.astype(int)
        t = frames / fps
    elif time_col is not None:
        t = pd.to_numeric(df[time_col], errors="coerce").astype(float).values
        y01 = df["_wing01"].values.astype(int)
    else:
        y01 = df["_wing01"].values.astype(int)
        t = np.arange(len(df), dtype=float) / fps
    return t, y01

def load_km_as_series(km_path, fps):
    df = pd.read_csv(km_path)
    if "syllable" not in df.columns:
        alt = next((c for c in df.columns if str(c).strip().lower() in ["syllable", "state", "label", "class"]), None)
        if alt is None:
            raise ValueError("KM CSV に syllable 列が見つかりません")
        df = df.rename(columns={alt: "syllable"})
    n = len(df)
    t = np.arange(n, dtype=float) / fps
    s = pd.to_numeric(df["syllable"], errors="coerce").astype(int).values
    return t, s

# ---------- メトリクス ----------
def f1_prec_rec(y_true, y_pred):
    tp = int(np.sum(y_true & y_pred))
    fp = int(np.sum(~y_true & y_pred))
    fn = int(np.sum(y_true & ~y_pred))
    prec = tp/(tp+fp) if (tp+fp)>0 else 0.0
    rec  = tp/(tp+fn) if (tp+fn)>0 else 0.0
    f1   = 2*prec*rec/(prec+rec) if (prec+rec)>0 else 0.0
    return f1, prec, rec

# ---------- ランキングCSV 読み込み（あれば使用） ----------
def find_latest_ranking_csv(base_dir):
    pats = [
        os.path.join(base_dir, "*_syllables_by_f1.csv"),
        os.path.join(base_dir, "*syllables_by_f1.csv"),
    ]
    cands = []
    for p in pats:
        cands.extend(glob.glob(p))
    if not cands:
        return None
    cands.sort(key=lambda p: os.path.getmtime(p), reverse=True)
    return cands[0]

def load_order_from_ranking_csv(path_or_empty, out_img_path, top_n):
    """
    path_or_empty が空なら OUT_IMG と同フォルダから *_syllables_by_f1.csv を探索。
    見つかれば 'syllable' 列の順序を list[int] で返す（top_n で切り詰め）。
    失敗なら (None, None) を返す。
    """
    path = (path_or_empty or "").strip()
    if not path:
        base_dir = os.path.dirname(os.path.abspath(out_img_path)) or "."
        auto = find_latest_ranking_csv(base_dir)
        if auto:
            path = auto

    if not path or not os.path.exists(path):
        return None, None

    try:
        df = pd.read_csv(path)
        cols_low = {c.lower(): c for c in df.columns}
        if "syllable" not in cols_low:
            return None, None
        syll_col = cols_low["syllable"]
        order = pd.to_numeric(df[syll_col], errors="coerce").dropna().astype(int).tolist()
        if len(order) == 0:
            return None, None
        if top_n is not None and top_n > 0:
            order = order[:top_n]
        return order, path
    except Exception:
        return None, None

# ========== メイン ==========
os.makedirs(os.path.dirname(OUT_IMG) or ".", exist_ok=True)

# GT
t_gt, y_gt01, gt_name = load_gt(GT_CSV)
t_gt, y_gt01 = crop_series(t_gt, y_gt01, TMIN, TMAX)
gt_bool = resample_bool_by_frame(t_gt, y_gt01.astype(bool), TMIN, TMAX, FPS)

# YORU
t_yoru, y_yoru01 = load_yoru(YORU_CSV, FPS)
t_yoru, y_yoru01 = crop_series(t_yoru, y_yoru01, TMIN, TMAX)
yoru_bool = resample_bool_by_frame(t_yoru, y_yoru01.astype(bool), TMIN, TMAX, FPS)

# KM
t_km, s_km = load_km_as_series(KM_CSV, FPS)
t_km, s_km = crop_series(t_km, s_km, TMIN, TMAX)

# ---- ランキングCSVを優先して順序を決める ----
order_from_csv, used_csv_path = load_order_from_ranking_csv(SYLLABLE_RANKING_CSV, OUT_IMG, TOP_N)

if order_from_csv is not None:
    top = order_from_csv
    print(f"[INFO] Use syllable order from CSV: {os.path.abspath(used_csv_path)}")
    metrics = None  # CSV使用時は計算表は不要
else:
    print("[WARN] Ranking CSV not found or invalid. Fallback to F1-based ranking.")
    syllables = np.unique(s_km)
    records = []
    for k in syllables:
        km_bool_k = resample_bool_by_frame(t_km, (s_km == k), TMIN, TMAX, FPS)
        f1, p, r = f1_prec_rec(gt_bool, km_bool_k)
        records.append((int(k), f1, p, r))
    metrics = (pd.DataFrame(records, columns=["syllable", "F1", "Precision", "Recall"])
               .sort_values(["F1", "Precision", "Recall"], ascending=False))
    top = metrics.head(min(TOP_N, len(metrics)))["syllable"].tolist()

# ========== 描画 ==========
n_rows = 2 + len(top)
fig_h = max(3.8, 0.5 * n_rows)
fig, ax = plt.subplots(figsize=(14, fig_h))

# 枠線なし & x軸非表示
for spine in ax.spines.values():
    spine.set_visible(False)
ax.set_xlabel("")
ax.set_xticks([])
ax.tick_params(axis='x', which='both', length=0, labelbottom=False)

line_kw = dict(where="post", color="black", linewidth=1.5)

# 背景ハイライト（GT=1 の区間：設定フラグで制御）
if HIGHLIGHT_GT:
    for t0, t1 in on_intervals_from_bool(gt_bool, TMIN, FPS):
        ax.axvspan(t0, t1, facecolor=HIGHLIGHT_COLOR, alpha=HIGHLIGHT_ALPHA, zorder=0)

y_positions, y_labels = [], []
row_gap = 1.2
y = 0.0

# 1) GT
t_plot, y_plot = step_from_bool(gt_bool, TMIN, FPS, y)
ax.step(t_plot, y_plot, **line_kw)
y_positions.append(y + 0.5)
y_labels.append("Manual Annotation")
y -= row_gap

# 2) YORU
t_plot, y_plot = step_from_bool(yoru_bool, TMIN, FPS, y)
ax.step(t_plot, y_plot, **line_kw)
y_positions.append(y + 0.5)
y_labels.append("YORU")
y -= row_gap

# 3) KM（CSVの順 or F1上位）
for k in top:
    km_bool_k = resample_bool_by_frame(t_km, (s_km == k), TMIN, TMAX, FPS)
    t_plot, y_plot = step_from_bool(km_bool_k, TMIN, FPS, y)
    ax.step(t_plot, y_plot, **line_kw)
    y_positions.append(y + 0.5)
    y_labels.append(f"syllable {k}")
    y -= row_gap

ax.set_xlim(TMIN, TMAX)
ax.set_ylim(y - 0.6, 1.2)
ax.set_yticks(y_positions)
ax.set_yticklabels(y_labels)

title_suffix = "CSV order" if order_from_csv is not None else "F1-based order"
ax.set_title(f"Ethogram — GT (top), YORU, KM Top {len(top)}  [{TMIN:.0f}-{TMAX:.0f}s]  (black, no frame/x-axis) — {title_suffix}")

# スケールバー（右下）
pad_x = (TMAX - TMIN) * 0.02
bar_x1 = TMAX - pad_x - SCALE_BAR_SEC
bar_x2 = TMAX - pad_x
bar_y  = y - 0.2
ax.plot([bar_x1, bar_x2], [bar_y, bar_y], lw=3, color="black")
ax.text((bar_x1 + bar_x2)/2, bar_y - 0.25, f"{SCALE_BAR_SEC:.0f} s",
        ha="center", va="top", color="black")

fig.tight_layout()
fig.savefig(OUT_IMG, dpi=300)
print(f"[OK] Saved: {OUT_IMG}")

if order_from_csv is None and metrics is not None:
    print("\nTop-by-F1 (0–1200 s):")
    print(metrics.head(len(top)).to_string(index=False))
else:
    print("\nTop (from CSV):")
    print(pd.DataFrame({"syllable": top}).to_string(index=False))
