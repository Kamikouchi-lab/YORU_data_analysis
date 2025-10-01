library(ggplot2)
library("ggsignif")
library(tidyverse)
library(RVAideMemoire)
library(dplyr)
library(emmeans)
library(lawstat)
library(rcompanion)
library(car)
library(psych)

text_label_legend <- c("Running",
                       "Stop",
                       "Whisker-On",
                       "Whisker-Off",
                       "Eye-Open",
                       "Eye-Closed",
                       "Grooming-On",
                       "Grooming-Off")

Setting <- list(theme(plot.margin = unit(c(1, 1, 1, 1), "lines")),
                theme_bw(), 
                theme(axis.title.y = element_text(size = 15, vjust = 2)),
                theme(plot.title = element_text(hjust = 0.5)),
                theme(axis.title.x = element_blank()),
                theme(axis.ticks.x = element_blank()),
                theme(axis.text.y = element_text(size = 12, colour = "black")),
                scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.25), expand = c(0, 0)),
                # scale_x_continuous(limits = c(0, 2200), breaks = seq(0, 2000, 500), expand = c(0, 0)),
                theme(axis.text.x = element_blank()),
                theme(strip.text.x = element_text(size = 12)),
                theme(strip.text.y = element_text(size = 15)),
                theme(legend.position = "none"))


#read to CSV
data_path = "../datas/mouse_detection_iou_results.csv"
df_data <- readr::read_csv(data_path)
head(df_data)
colnames(df_data)

df_data_run = df_data[df_data$class_name == "run",]
df_data_stop = df_data[df_data$class_name == "stop",]
df_data_whiON = df_data[df_data$class_name == "whiskerON",]
df_data_whiOFF = df_data[df_data$class_name == "whiskerOFF",]
df_data_open = df_data[df_data$class_name == "open",]
df_data_closed = df_data[df_data$class_name == "closed",]
df_data_GR = df_data[df_data$class_name == "GR",]
df_data_noGR = df_data[df_data$class_name == "noGR",]

describe(df_data_run)
describe(df_data_stop)
describe(df_data_whiON)
describe(df_data_whiOFF)
describe(df_data_open)
describe(df_data_closed)
describe(df_data_GR)
describe(df_data_noGR)

# 出力ファイルパス（必要に応じて変更）
out_csv <- "../datas/mouse_detection_iou_describe_summary.csv"

# クラスごと（class_nameごと）に describe を実行し、結合
per_class_desc <- df_data %>%
  group_by(class_name) %>%
  group_modify(?{
    num_only <- dplyr::select(.x, where(is.numeric))
    if (ncol(num_only) == 0) {
      return(tibble::tibble()) # 数値列が無い場合は空
    }
    d <- psych::describe(num_only, fast = FALSE)
    d <- tibble::as_tibble(d, rownames = "variable")
    d
  }) %>%
  ungroup() %>%
  relocate(class_name, .before = variable)

# 全体（全クラスまとめて）の describe
overall_desc <- df_data %>%
  dplyr::select(where(is.numeric)) %>%
  { 
    d <- psych::describe(., fast = FALSE)
    tibble::as_tibble(d, rownames = "variable") %>%
      mutate(class_name = "ALL", .before = 1)
  }

# 結合して書き出し
desc_all <- bind_rows(overall_desc, per_class_desc)

# 小数点桁数などを整えたい場合はここで round() などを適用可能
# 例: desc_all <- desc_all %>% mutate(across(where(is.numeric), ?round(., 6)))

readr::write_csv(desc_all, out_csv)

message("Saved describe summary to: ", out_csv)

df_data <- df_data %>%
  transform(class_names = factor(class_name, levels = c("run", "stop", "whiskerON", "whiskerOFF","open", "closed", "GR", "noGR")))




class_labs <- as_labeller(c("run" = "Running",
                            "stop" = "Stop",
                            "whiskerON" = "Whisker-On",
                            "whiskerOFF" = "Whisker-Off",
                            "open" = "Eye-Open",
                            "closed" = "Eye-Closed",
                            "GR" = "Grooming-On",
                            "noGR" = "Grooming-Off")) 



gp_all = ggplot(data = df_data, aes(x = class_names, y = value, fill = class_names)) + 
  geom_violin(color = "#434343", alpha = 0.8) +
  geom_point(size = 0.01) +
  facet_grid(. ? class_names, labeller = labeller(class_names = class_labs), scales = "free_x") +
  labs(title="Mice - Treadmill", y = "IOU")+
  scale_color_discrete(name = element_blank(), labels = text_label_legend)+
  Setting
gp_all


ggsave("../graph/mouse_treadmill_IOU_graph.png", gp_all, width = 10, height = 3, dpi = 500)
ggsave("../graph/mouse_treadmill_IOU_graph.svg", gp_all, width = 10, height = 3, dpi = 500)
