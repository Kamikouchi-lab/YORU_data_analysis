library(ggplot2)
library(readr)
library(dplyr)
library(grDevices)
library(cowplot) 

library(ggplot2)
library(readr)
library(dplyr)# if you still want to combine plots, otherwise not required

#――――――――――――――――――――――――
# settings from your original script
csv_path <- "./outputs/individual_no_output_all_data.csv"
out_png  <- "./outputs/line_count_with_errorbars.png"
out_svg  <- "./outputs/line_count_with_errorbars.svg"

Setting <- list(
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines")),
  theme_bw(),
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 60, 10), expand = c(0, 0)),
  theme(axis.title.y = element_text(size = 15)),
  theme(axis.title.x = element_text(size = 15, vjust = -0.5)),
  theme(axis.text.y = element_text(size = 15, colour = "black")),
  theme(axis.text.x = element_text(size = 15, colour = "black")),
  theme(text = element_text(size = 12))
)
#――――――――――――――――――――――――


# データ読み込み＋summary
summary_df <- read_csv(csv_path, show_col_types = FALSE) %>%
  group_by(folder_name) %>%
  summarise(
    mean_count = mean(count),
    sd_count   = sd(count),
    .groups     = "drop"
  ) %>%
  # 文字列 "5females" から数字だけ抜き出して新しい列に
  mutate(n_females = as.numeric(sub("females", "", folder_name)))

# 確認：数値型になっているか
str(summary_df$n_females)
#→ num [1:7] 5 10 20 30 40 50 60

# プロット：x に数値型の n_females を渡す
p_line <- ggplot(summary_df, aes(x = n_females, y = mean_count)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 0.5) +
  geom_point(size = 3) +
  geom_errorbar(aes(
    ymin = mean_count - sd_count,
    ymax = mean_count + sd_count
  ), width = 3) +
  scale_x_continuous(
    breaks = c(5,10,20,30,40,50,60),
    labels = c(5,10,20,30,40,50,60)
  ) +
  scale_y_continuous(
    breaks = c(5,10,20,30,40,50,60),
    labels = c(5,10,20,30,40,50,60)
  ) +
  labs(
    x = "Number of individuals (Ground-truth)",
    y = "Number of individuals (YORU detection)"
  ) +
  Setting

# 5) 保存
ggsave(out_png, plot = p_line, width = 5, height = 5, dpi = 500)
ggsave(out_svg, plot = p_line, width = 5, height = 5, dpi = 500)

# プロットを画面にも出力
print(p_line)
