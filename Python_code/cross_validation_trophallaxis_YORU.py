# -*- coding: utf-8 -*-
"""
Compute Accuracy / Precision / Recall / F1 by comparing YORU predictions
against BORIS (Ground Truth), using a file list provided in an Excel file.

- No command-line args: edit the "SETTINGS" section below.
- Excel must have columns:
    - 'yoru' (preferred) or 'asoid' (fallback): path/name to YORU CSV
    - 'boris': path/name to BORIS CSV
- BORIS CSV must contain: ["time", BEHAVIOR_COL] with 0/1 values at BORIS_HZ (e.g., 10 Hz).
- YORU CSV is detection-form: multiple rows per frame. If ANY detection with
  class name BEHAVIOR_CLASS_NAME appears in a frame => that frame label = 1, else 0.
  Expected columns include e.g. ["frame", "class_name", ...]. (If "class" only,
  also supported via CLASS_ID mapping.)

Pipeline:
  1) BORIS 10 Hz -> upsample to TARGET_HZ (e.g., 30 Hz) by forward-fill (step=1/TARGET_HZ).
  2) YORU detections -> per-frame 0/1 -> convert to time = frame / VIDEO_FPS.
     Then upsample/snap to TARGET_HZ grid (forward-fill).
  3) Align on time (inner-join) and compute metrics.
  4) Output CSV summary.

Author: ChatGPT
"""

from __future__ import annotations
from pathlib import Path
import pandas as pd
import numpy as np
from sklearn.metrics import precision_score, recall_score, f1_score, accuracy_score


# ========== SETTINGS (edit here) ==========
# Paths (use absolute or relative). Excel must include columns: asoid, boris, (optional: no)
EXCEL_FILE = Path(r"./data/file_list.xlsx")


# Output
OUTPUT_CSV = Path(r"./data/metrics_summary_all.csv")

# Directories containing the CSVs listed in the Excel (set to the actual locations).
# If paths in Excel are already full paths, these BASE_* dirs can be left as ".".
BASE_YORU_DIR = Path("./data/YORU_analysis_data")   # directory for YORU/A‑SOiD CSVs
BASE_BORIS_DIR = Path("./data/BORIS_data")   # directory for BORIS CSVs

# Label name to compare (column that has 0/1 labels)
BEHAVIOR = "trophallaxis"

# Label to compare (column name in BORIS CSV)
BEHAVIOR_COL = "trophalaxis"

# YORU detection settings
BEHAVIOR_CLASS_NAME = "trophallaxis"   # mark 1 if any detection has this class_name
# If your YORU CSV uses numeric 'class' instead of text 'class_name',
# set CLASS_ID_FOR_BEHAVIOR to the integer id; otherwise leave as None.
CLASS_ID_FOR_BEHAVIOR = None  # e.g., 0 or 1; or None if not used

# Sampling assumptions
BORIS_HZ  = 10            # BORIS sampling rate (Hz)
TARGET_HZ = 30            # comparison grid rate (Hz)
VIDEO_FPS = 30            # video fps to convert YORU 'frame' -> 'time' (sec)

# ==========================================


def _require_columns(df: pd.DataFrame, needed: list[str], path: Path) -> None:
    missing = [c for c in needed if c not in df.columns]
    if missing:
        raise ValueError(f"{path} is missing required column(s): {missing} (has: {list(df.columns)})")


def upsample_to_hz(df_time_label: pd.DataFrame, target_hz: int) -> pd.DataFrame:
    """
    Input: columns ['time', 'label'] sorted arbitrarily.
    Output: snap to dense target_hz timeline with forward-fill of labels.
    """
    df = df_time_label.sort_values("time").drop_duplicates(subset="time", keep="last").reset_index(drop=True)
    if df.empty:
        return pd.DataFrame(columns=["time", "label"])
    step = 1.0 / float(target_hz)
    min_t, max_t = float(df["time"].iloc[0]), float(df["time"].iloc[-1])
    # include last point with a tiny epsilon
    timeline = np.arange(min_t, max_t + 1e-9, step)
    dense = pd.DataFrame({"time": timeline})
    dense = pd.merge_asof(
        dense, df.sort_values("time"),
        on="time", direction="backward"
    )
    dense["time"] = dense["time"].round(6)
    return dense


def load_boris(csv_path: Path, behaviour_col: str, boris_hz: int, target_hz: int) -> pd.DataFrame:
    """
    BORIS CSV must have columns: ['time', behaviour_col] with 0/1 labels.
    Return DataFrame with ['time','label'] at target_hz grid via forward-fill.
    """
    df = pd.read_csv(csv_path)
    _require_columns(df, ["time", behaviour_col], csv_path)
    df = df[["time", behaviour_col]].copy().rename(columns={behaviour_col: "label"})
    # Snap to comparison grid
    return upsample_to_hz(df, target_hz)


def load_yoru_detection_to_labels(csv_path: Path,
                                  class_name_target: str | None,
                                  class_id_target: int | None,
                                  video_fps: int,
                                  target_hz: int) -> pd.DataFrame:
    """
    Convert YORU detection CSV (multiple rows per frame) to per-frame 0/1 labels,
    then to ['time','label'] at target_hz grid.

    Strategy:
      - group by 'frame'
      - label = 1 if any row matches class_name_target (if provided)
                OR any row matches class_id_target (if provided)
                (if both provided, either match triggers 1)
      - Otherwise label = 0
      - frames with no rows in CSV will not be present → we expand to [0..max_frame] and fill 0
    """
    df = pd.read_csv(csv_path)
    if "frame" not in df.columns:
        raise ValueError(f"{csv_path} must have 'frame' column for YORU detections. Found: {list(df.columns)}")

    has_name = "class_name" in df.columns and class_name_target is not None
    has_id   = "class" in df.columns and class_id_target is not None
    if not (has_name or has_id):
        # Try to guess: if class_name exists, default to name; else if class exists, warn to set id.
        if "class_name" in df.columns and class_name_target is not None:
            has_name = True
        elif "class" in df.columns:
            raise ValueError(
                f"{csv_path} has only numeric 'class' column. "
                f"Please set CLASS_ID_FOR_BEHAVIOR to the correct integer id."
            )
        else:
            raise ValueError(
                f"{csv_path} must include either 'class_name' or 'class' to identify {class_name_target}."
            )

    # Group per frame and compute label
    # First, aggregate frames present in CSV
    df_grp = df.groupby("frame", as_index=True)

    def frame_label(g):
        ok = False
        if has_name:
            ok = ok or (g["class_name"].astype(str) == class_name_target).any()
        if has_id:
            ok = ok or (g["class"].astype(int) == int(class_id_target)).any()
        return 1 if ok else 0

    labels_present = df_grp.apply(frame_label).rename("label")

    # Expand to full [0..max_frame] so frames with no detections become 0
    max_frame = int(df["frame"].max())
    all_frames = pd.Index(np.arange(0, max_frame + 1), name="frame")
    labels_full = labels_present.reindex(all_frames, fill_value=0).to_frame()

    # Convert frame -> time
    labels_full = labels_full.reset_index()
    labels_full["time"] = labels_full["frame"].astype(float) / float(video_fps)
    labels_time = labels_full[["time", "label"]].copy()

    # Snap to TARGET_HZ grid via forward-fill
    return upsample_to_hz(labels_time, target_hz)


def calc_metrics(y_true: np.ndarray, y_pred: np.ndarray) -> dict[str, float]:
    return {
        "accuracy":  accuracy_score(y_true, y_pred),
        "precision": precision_score(y_true, y_pred, zero_division=0),
        "recall":    recall_score(y_true, y_pred, zero_division=0),
        "f1":        f1_score(y_true, y_pred, zero_division=0),
    }


def align_and_metrics(boris_df: pd.DataFrame, yoru_df: pd.DataFrame, how: str = "inner") -> tuple[dict, np.ndarray, np.ndarray]:
    """
    Align by time, compute metrics. Returns (metrics_dict, y_true, y_pred).
    'how' can be 'inner' (intersection) or 'left' (BORIS as master).
    """
    merged = boris_df.merge(yoru_df, on="time", suffixes=("_gt", "_pred"), how=how)
    merged = merged.dropna(subset=["label_gt", "label_pred"])
    if merged.empty:
        return {"accuracy": np.nan, "precision": np.nan, "recall": np.nan, "f1": np.nan, "frames": 0}, np.array([], dtype=int), np.array([], dtype=int)
    y_true = merged["label_gt"].astype(int).to_numpy()
    y_pred = merged["label_pred"].astype(int).to_numpy()
    m = calc_metrics(y_true, y_pred)
    m["frames"] = int(len(merged))
    return m, y_true, y_pred


def main():
    # Read Excel
    df_list = pd.read_excel(EXCEL_FILE)
    # Prefer 'yoru', fallback to 'asoid' for backward-compat
    yoru_col = "yoru" if "yoru" in df_list.columns else ("asoid" if "asoid" in df_list.columns else None)
    if yoru_col is None:
        raise ValueError(f"Excel must contain 'yoru' (preferred) or 'asoid' column. Found: {list(df_list.columns)}")
    if "boris" not in df_list.columns:
        raise ValueError(f"Excel must contain 'boris' column. Found: {list(df_list.columns)}")

    rows = []
    all_true, all_pred = [], []

    for i, row in df_list.iterrows():
        yoru_name  = str(row[ioru_col := yoru_col]).strip()
        boris_name = str(row["boris"]).strip()

        yoru_path  = (BASE_YORU_DIR  / yoru_name).resolve()
        boris_path = (BASE_BORIS_DIR / boris_name).resolve()

        if not yoru_path.exists():
            print(f"[WARN] YORU file not found: {yoru_path}")
            continue
        if not boris_path.exists():
            print(f"[WARN] BORIS file not found: {boris_path}")
            continue

        try:
            gt_df = load_boris(boris_path, BEHAVIOR_COL, BORIS_HZ, TARGET_HZ)
            pr_df = load_yoru_detection_to_labels(
                yoru_path,
                class_name_target=BEHAVIOR_CLASS_NAME,
                class_id_target=CLASS_ID_FOR_BEHAVIOR,
                video_fps=VIDEO_FPS,
                target_hz=TARGET_HZ
            )
            m, y_true, y_pred = align_and_metrics(gt_df, pr_df, how="inner")
            m.update({"file": yoru_name})
            rows.append(m)
            if m["frames"] > 0:
                all_true.append(y_true)
                all_pred.append(y_pred)
        except Exception as e:
            print(f"[ERROR] {yoru_path.name}: {e}")
            continue

    if not rows:
        raise RuntimeError("No valid file pairs were processed. Check paths and Excel contents.")

    # Overall
    if all_true:
        y_true_all = np.concatenate(all_true)
        y_pred_all = np.concatenate(all_pred)
        overall = calc_metrics(y_true_all, y_pred_all)
        overall.update({"file": "ALL", "frames": int(len(y_true_all))})
    else:
        overall = {"file": "ALL", "accuracy": np.nan, "precision": np.nan, "recall": np.nan, "f1": np.nan, "frames": 0}
    rows.append(overall)

    out_df = pd.DataFrame(rows).set_index("file")
    OUTPUT_CSV.parent.mkdir(parents=True, exist_ok=True)
    out_df.to_csv(OUTPUT_CSV, float_format="%.6f")
    print(f"Saved → {OUTPUT_CSV.resolve()}")
    print(out_df)


if __name__ == "__main__":
    main()