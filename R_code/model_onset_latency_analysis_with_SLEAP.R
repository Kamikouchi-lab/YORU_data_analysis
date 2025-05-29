library(ARTool)
library(lawstat)
library(ggplot2) 
library(tidyverse)
library(ggsignif)
library(RVAideMemoire)
library(emmeans)
library(rcompanion)
library(psych)
library(car)
library(RColorBrewer)

###Setting of the Graph###
#label
# lab_homo = expression(paste(italic({piezo ^ {"KO"}})))
# print(lab_homo)
# lab_hetero = expression(paste(italic({piezo ^ KO/"+"})))
# lab_csh = "Canton-S"

Setting <- list(theme(plot.margin = unit(c(1, 1, 1, 1), "lines")),
                theme_bw(), 
                theme(axis.title.y = element_text(size = 15)),
                theme(axis.title.x = element_text(size = 15, vjust = -0.5)),
                theme(axis.text.y = element_text(size = 15, colour = "black")),
                theme(axis.title.x = element_blank()),
                theme(axis.text.x = element_text(size = 15, colour = "black", angle = 45, hjust = 1)),
                theme(strip.text.x = element_text(size = 18)),
                theme(strip.text.y = element_text(size = 18)),
                scale_fill_manual(values = c("#f39800", "#101a43")),
                scale_color_manual(values = c("#000000", "#000000")),
                theme(legend.position = "none"),
                theme(text = element_text(size = 8)))

fps_labs <- as_labeller(c("100fps" = "640x480 (100fps)"))  

#read to CSV
data_path = "../for_data/results_SLEAP_comparison_data.csv"
df_data <- readr::read_csv(data_path)
head(df_data)
#Changing label weights
df_data <- transform(df_data, model = factor(model, levels = c("YORU", "SLEAP")))
df_data$diff_ms <- df_data$diff_s * 1000


# ────────── ★ ここで要約統計量を計算 ★ ──────────
summary_stats <- df_data |>
  group_by(fps, model) |>               # fps × model で集計（必要に応じて変更）
  summarise(
    n        = n(),
    mean_ms  = mean(diff_ms, na.rm = TRUE),
    sd_ms    = sd(diff_ms,  na.rm = TRUE),
    se_ms    = sd_ms / sqrt(n),         # 標準誤差（任意）
    .groups  = "drop"
  )

# コンソールに表示
print(summary_stats)

# CSV に保存したい場合
write_csv(summary_stats, "../for_data/latency_summary_stats.csv")

gp = ggplot(data = df_data, aes(x = model, y = diff_ms)) +
  geom_violin(aes(fill = model)) +
  facet_grid(. ~ fps, labeller = labeller(fps = fps_labs)) +
  labs(title="", x = "Training models", y = "Latency (ms)") +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 50), expand = c(0, 0)) +
  Setting
gp

ggsave("../graph/SLEAP_compariton.png", gp, width = 3, height = 4, dpi = 500)
ggsave("../graph/SLEAP_compariton.svg", gp, width = 3, height = 4, dpi = 500)