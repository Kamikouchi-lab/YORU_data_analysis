#!/usr/bin/env python3
import os
import json
import glob
import pandas as pd
from tqdm import tqdm
from sklearn.metrics import (
    precision_recall_fscore_support,
    confusion_matrix,
)

# ─────────── ここでファイルパスを指定 ───────────
GT_JSON_PATH    = "./calms21_task1/calms21_task1_test.json"
PRED_CSV_DIR    = "./predictions/test"
OUTPUT_CSV_PATH = "./results/evaluation_results_test.csv"
# ───────────────────────────────────────────────


def load_ground_truth(json_path):
    """CalMS21 Task1 のトレーニングアノテーションを読み込む."""
    with open(json_path, 'r') as f:
        data = json.load(f)
    group = next(iter(data))
    gt_dict = {}
    class_map = {}
    for seq_key, seq_data in data[group].items():
        if not seq_key.startswith('task1/test/'):
            continue
        video_name = os.path.basename(seq_key)
        gt_dict[video_name] = seq_data['annotations']
        meta = seq_data.get('metadata', {})
        if 'vocab' in meta:
            class_map = {v: k for k, v in meta['vocab'].items()}
    return gt_dict, class_map


def load_predictions(pred_csv, n_frames, default_label):
    """
    同一フレームの複数検出は confidence 最大を採用、
    検出なしフレームは default_label（other）を補完。
    """
    df = pd.read_csv(pred_csv)
    best = df.loc[df.groupby('frame')['confidence'].idxmax()]
    pred_dict = {int(row.frame): int(row['class']) for _, row in best.iterrows()}
    return [pred_dict.get(i, default_label) for i in range(n_frames)]


def evaluate_metrics(y_true, y_pred, class_map, n_frames):
    """
    各クラスの指標を計算し、DataFrameで返す。
    フレーム数(n_frames)も結果に含む。
    """
    labels = sorted(class_map.keys())
    prec, rec, f1, sup = precision_recall_fscore_support(
        y_true, y_pred, labels=labels, zero_division=0
    )
    cm = confusion_matrix(y_true, y_pred, labels=labels)
    total = cm.sum()

    rows = []
    for idx, lbl in enumerate(labels):
        tp = cm[idx, idx]
        fp = cm[:, idx].sum() - tp
        fn = cm[idx, :].sum() - tp
        tn = total - tp - fp - fn
        acc = (tp + tn) / total
        name = class_map.get(lbl, str(lbl))
        rows.append({
            'Video': None,
            'Frames': n_frames,
            'Class': name,
            'Accuracy': acc,
            'Precision': prec[idx],
            'Recall': rec[idx],
            'F1-Score': f1[idx],
            'Support': sup[idx]
        })
    return pd.DataFrame(rows)


def main():
    # Ground truth 読み込み
    gt_dict, class_map = load_ground_truth(GT_JSON_PATH)

    all_results = []
    y_true_all = []
    y_pred_all = []

    os.makedirs(os.path.dirname(OUTPUT_CSV_PATH), exist_ok=True)
    file_list = glob.glob(os.path.join(PRED_CSV_DIR, "*.csv"))
    # 動画ごとに進捗バーを表示
    for filepath in tqdm(file_list, desc="Processing videos", unit="video"):
        video_name = os.path.splitext(os.path.basename(filepath))[0]
        if video_name not in gt_dict:
            tqdm.write(f"Skip: '{video_name}' not in ground truth")
            continue
        y_true = gt_dict[video_name]
        n_frames = len(y_true)
        default_label = next(
            (lbl for lbl, name in class_map.items() if name.lower() == 'other'),
            max(class_map.keys(), default=3)
        )
        y_pred = load_predictions(filepath, n_frames, default_label)
        y_true_all.extend(y_true)
        y_pred_all.extend(y_pred)

        df_metrics = evaluate_metrics(y_true, y_pred, class_map, n_frames)
        df_metrics['Video'] = video_name
        all_results.append(df_metrics)

    if not all_results:
        print("No valid prediction files found.")
        return

    # 各動画ごとの結果を結合
    result_df = pd.concat(all_results, ignore_index=True)

    # 全動画のまとめ
    total_frames = len(y_true_all)
    df_global = evaluate_metrics(y_true_all, y_pred_all, class_map, total_frames)
    df_global['Video'] = 'ALL'

    result_df = pd.concat([result_df, df_global], ignore_index=True)

    # CSV 出力
    result_df.to_csv(OUTPUT_CSV_PATH, index=False)
    print(f"Results written to {OUTPUT_CSV_PATH}")

if __name__ == "__main__":
    main()
