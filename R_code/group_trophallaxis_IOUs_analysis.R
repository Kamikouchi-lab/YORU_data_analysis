library(ggplot2)
library("ggsignif")
library(tidyverse)
library(RVAideMemoire)
library(psych)
library(car)

text_label_legend <- c("YOLOv5n", "YOLOv5s","YOLOv5m","YOLOv5l","YOLOv5x")

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
                theme(strip.text.x = element_text(size = 15)),
                theme(strip.text.y = element_text(size = 15)),
                theme(legend.position = "right"))


#read to CSV
data_path = "../datas/iou_data_group_trophallaxis.csv"
df_data <- readr::read_csv(data_path) 
head(df_data)

df_data <- df_data %>%
  transform(models = factor(model, levels = c("yolo5n", "yolo5s", "yolo5m", "yolo5l","yolo5x"))) %>%
  mutate(train_no = as.character(training_no)) %>%
  transform(train_no = factor(train_no, levels = c("100", "200", "500", "1000")))

df_data_trophallaxis =  df_data[df_data$class_name=="trophallaxis",]


# 集計結果を保存するリストを作成
results <- list()

# モデルごとの処理
models <- c("yolo5n", "yolo5s", "yolo5m", "yolo5l", "yolo5x")
training_levels <- c("100", "200", "500", "1000")


for (training_no_f in training_levels) {
  for (model_f in models) {
    # フィルタリング
    filtered_data <- df_data_trophallaxis %>%
      filter(model == model_f, train_no == training_no_f)
    
    # 必要な統計量を計算
    if (nrow(filtered_data) > 0) {
      summary_stats <- describe(filtered_data$value) # metric_columnは数値列名に置き換えてください
      results[[paste(model_f, training_no_f, sep = "_")]] <- data.frame(
        Model = model_f,
        TrainingNo = training_no_f,
        n = summary_stats$n,
        median = median(filtered_data$value, na.rm = TRUE),
        mean = summary_stats$mean,
        SD = summary_stats$sd
      )
    }
  }
}

# リストをデータフレームに変換
summary_df <- do.call(rbind, results)

# CSVに保存
write.csv(summary_df, "summary_statistics_trophallaxis.csv", row.names = FALSE)

# 集計結果のプレビュー
print(summary_df)

df_data_filtered <- df_data %>%
  # dplyr::mutate(training_no = as.character(training_no)) %>%
  dplyr::filter(model == "yolov5s")  

# definition of labels
# ap_labs <- as_labeller(c(`ap_50` = "AP@50", `ap_75` = "AP@75"))
class_labs <- as_labeller(c(`0` = "no", `1` = "trophallaxis")) 
train_no_labs <- as_labeller(c(`100` = "100 images", `200` = "200 images", `500` = "500 images", `1000` = "1000 images")) 

# Only trophallaxis
df_tro_data = filter(df_data, class == "1")

gp_tro = ggplot(data = df_tro_data, aes(x = train_no, y = value, fill = model)) + 
  # geom_boxplot() +
  geom_violin(color = "#434343", alpha = 0.8) +
  # geom_jitter(size = 0.01) +
  # geom_line()+
  facet_grid(class ? train_no, labeller = labeller(class = class_labs, train_no = train_no_labs), scales = "free_x") +
  labs(title="ant - group (6 ants)", x = "Training images", y = "IOU")+
  scale_fill_discrete(name = element_blank(), labels = text_label_legend)+
  Setting
gp_tro




ggsave("../graph/model_evaluation_iou_tro_group.png", gp_tro, width = 12, height = 3, dpi = 500)
ggsave("../graph/model_evaluation_iou_tro_group.svg", gp_tro, width = 12, height = 3, dpi = 500)