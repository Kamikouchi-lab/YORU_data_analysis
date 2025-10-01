# ----- Packages -----
library(tidyverse)
library(ggplot2)

# ----- Settings -----
path_to_csv <- "./data/tableS7_rev2.csv"  # ←CSVパス
out_dir     <- "./outputs"                # 出力先
width_svg   <- 12.0                       # 横並びなので横長に
height_svg  <- 3
width_pdf   <- 12.0
height_pdf  <- 3

# 凡例表示名とモデル順序
text_label_legend <- c("YOLOv5n", "YOLOv5s", "YOLOv5m", "YOLOv5l", "YOLOv5x")
model_levels      <- c("YOLOv5n","YOLOv5s","YOLOv5m","YOLOv5l","YOLOv5x")

# 見た目
Setting <- list(
  theme_bw(),
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines")),
  theme(axis.title.y = element_text(size = 15, vjust = 2)),
  theme(plot.title  = element_text(hjust = 0.5)),
  theme(axis.text.y = element_text(size = 12, colour = "black")),
  theme(axis.text.x = element_text(size = 12, colour = "black")),
  theme(strip.text.x = element_text(size = 15)),
  theme(strip.text.y = element_text(size = 15)),
  theme(legend.position = "right")
)

# ファイル名サニタイズ
sanitize_filename <- function(s) {
  s |>
    stringr::str_replace_all("[^??w??-]+", "_") |>
    stringr::str_replace_all("_+", "_") |>
    stringr::str_replace("^_|_$", "")
}

# ===== Load & Preprocess =====
df_raw <- readr::read_csv(path_to_csv, show_col_types = FALSE)

df <- df_raw %>%
  rename(
    dataset      = `Dataset`,
    model        = `Pre-trained model`,
    train_images = `Number of training images`,
    precision    = `Precision (%)`,
    recall       = `Recall (%)`
  ) %>%
  mutate(
    model        = factor(model, levels = model_levels),
    train_images = as.numeric(train_images),
    precision    = as.numeric(precision),
    recall       = as.numeric(recall)
  ) %>%
  arrange(dataset, model, train_images)

# y軸（%）
y_scale <- scale_y_continuous(limits = c(0, 101), breaks = seq(0, 100, 25), expand = c(0, 0))

# データセットごとの x 軸レベル
x_levels_for_dataset <- function(dataset_name) {
  if (identical(dataset_name, "ant - group (6 ants)")) {
    c(100, 200, 500, 1000)
  } else {
    c(200, 500, 1000, 1500, 2000)
  }
}

# Precision & Recall を横並び（facet）で1枚に出力（SVG & PDF）
plot_precision_recall_side_by_side <- function(df_subset) {
  dataset_name <- unique(df_subset$dataset)
  lv <- x_levels_for_dataset(dataset_name)
  
  # long 形式へ
  sub_long <- df_subset %>%
    select(dataset, model, train_images, precision, recall) %>%
    pivot_longer(cols = c(precision, recall), names_to = "metric", values_to = "value") %>%
    mutate(
      train_images_f = factor(train_images, levels = lv),
      metric = factor(metric, levels = c("precision", "recall"),
                      labels = c("Precision", "Recall"))
    )
  
  p <- ggplot(
    sub_long,
    aes(x = train_images_f, y = value, fill = model)
  ) +
    geom_col(
      position = position_dodge(0.8),
      width = 0.8,
      alpha = 0.80,           # ご指定
      colour = "#434343",     # 境界線色
      linewidth = 0.4
    ) +
    facet_grid(. ? metric) +  # ← 横並びに変更
    scale_x_discrete(limits = as.character(lv), drop = FALSE) +
    y_scale +
    labs(
      title = dataset_name,
      x = "Training images",
      y = "Value (%)",
      fill = NULL
    ) +
    # 既定の離散色（色補正なし）
    scale_fill_discrete(limits = model_levels, labels = text_label_legend) +
    Setting
  
  # 出力パス
  base <- file.path(out_dir, sanitize_filename(dataset_name))
  svg_path <- paste0(base, "_precision_recall.svg")
  pdf_path <- paste0(base, "_precision_recall.pdf")
  
  # SVG 出力
  ggsave(svg_path, p, width = width_svg, height = height_svg, device = "svg")
  # PDF 出力（フォント埋め込みを安定させるなら cairo_pdf）
  ggsave(pdf_path, p, width = width_pdf, height = height_pdf, device = cairo_pdf)
  
  message("Saved: ", svg_path)
  message("Saved: ", pdf_path)
}

# ===== Run =====
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

df %>%
  group_split(dataset) %>%
  walk(plot_precision_recall_side_by_side)
