# -*- coding: utf-8 -*-
import numpy as np
import matplotlib.pyplot as plt
import sleap

# ===== 軸スタイル =====
def adjust_spines(ax=None, spines=('left', 'bottom')):
    if ax is None:
        ax = plt.gca()
    ax.spines['top'].set_linewidth(0)
    ax.spines['right'].set_linewidth(0)
    ax.spines['left'].set_linewidth(1.5); ax.spines['left'].set_color('k')
    ax.spines['bottom'].set_linewidth(1.5); ax.spines['bottom'].set_color('k')
    ax.tick_params(direction='out', length=6, width=1.5, color='k')
    for loc, spine in ax.spines.items():
        if loc in spines:
            spine.set_position(('outward', 2.0))
        else:
            spine.set_color('none')

def myax(ax=None):
    if ax is None:
        ax = plt.gca()
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    # ax.grid(linestyle='dotted', linewidth=1.0)
    adjust_spines(ax, spines=('left', 'bottom'))

# ===== データ読み込み =====
METRICS_DIR = r"./models/250419_090425.centered_instance.n=213"
SPLIT = "val"
m = sleap.load_metrics(METRICS_DIR, split=SPLIT)

for k in ("dist.dists", "dist.per_node_dists", "oks_voc.dists"):
    if k in m:
        d = np.asarray(m[k]); break
else:
    raise KeyError("距離配列が見つかりません。")

if d.ndim != 2: raise ValueError(f"shape={d.shape}")
if d.shape[0] < d.shape[1]: d = d.T
N, K = d.shape

node_names = None
for nk in ("node_names", "dist.node_names", "instance_centered.node_names"):
    if nk in m and len(m[nk]) == K:
        node_names = list(m[nk]); break
if node_names is None:
    node_names = [f"node{i+1}" for i in range(K)]

per_node = []
for i in range(K):
    xi = d[:, i]
    xi = xi[np.isfinite(xi)]
    if xi.size == 0: xi = np.array([np.nan])
    per_node.append(xi)

# ===== 描画 =====
fig, ax = plt.subplots(figsize=(7, 6), dpi=200)

bp = ax.boxplot(
    per_node, vert=False, showfliers=False, whis=(5, 95),
    widths=0.6, patch_artist=True, zorder=3
)

# ★ 箱（facecolor）は色付き、中央値ラインは黒
colors = plt.rcParams["axes.prop_cycle"].by_key().get("color", ["C0"])
for i, box in enumerate(bp["boxes"]):
    box.set_facecolor(colors[i % len(colors)])
    box.set_alpha(0.65)

# ★ 中央値ラインを黒・太めに
for med in bp["medians"]:
    med.set_color('k')
    med.set_linewidth(1.6)

# 散布：濃く（alpha↑）、少しジッタ
for i, xi in enumerate(per_node):
    xi = xi[np.isfinite(xi)]
    if xi.size == 0: continue
    y = np.full_like(xi, i + 1, dtype=float) + (np.random.rand(xi.size) - 0.5) * 0.20
    ax.scatter(xi, y, s=12, alpha=0.60, zorder=2)  # ← 濃く：alpha=0.60

ax.set_yticks(range(1, K + 1)); ax.set_yticklabels(node_names)
ax.set_xlabel("Error (px)")
ax.set_title("Node distances (ground truth vs prediction)")
ax.set_xlim(left=0)

myax(ax)
fig.tight_layout()
fig.savefig("node_distances_styled.pdf")
plt.close(fig)
print("Saved: node_distances_styled.pdf")
