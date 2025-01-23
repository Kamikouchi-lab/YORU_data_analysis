# 必要なライブラリの読み込み
library(ggplot2)
library(dplyr)

# データの読み込み
data <- read.csv("./data/BORIS_data/courtship_movies_id15.csv", header = TRUE)

# Behaviorがfinishiの行をフィルタし、そのImage indexを取得
final_image_index <- data %>% 
  filter(Behavior == "finish") %>% 
  pull(Image.index)

# スカラー値としてのImage indexを表示
print(final_image_index)

# 行動タイプ（Behavior type）に基づいて、各行動の開始時間と終了時間を抽出
data$start_time <- ifelse(data$Behavior.type == "START", data$Image.index, NA)
data$end_time <- ifelse(data$Behavior.type == "STOP", data$Image.index, NA)

# 行動の開始時間と終了時間を用いて、各行動セグメントのデータを作成
data$start_time <- zoo::na.locf(data$start_time, na.rm = FALSE)
data_r <- data %>%
  filter(!is.na(end_time))

# startからstopまでのフレームを取得
wing_extension_BORIS <- do.call(rbind, lapply(1:nrow(data_r), function(i) {
  start_frame <- data_r$start_time[i]
  stop_frame <- data_r$end_time[i]
  data.frame(frame = seq(start_frame, stop_frame))
}))
wing_extension_BORIS$label <-"wing_extension"

# 1つ目のデータの読み込み
data1 <- read.csv("./data/YORU_data/cs-h_movie_0015 23-10-27 15-40-40.csv")

# wing_extensionクラスのデータをフィルタリング
wing_extension_YORU <- subset(data1, class_name == "wing_extension")
wing_extension_YORU <- wing_extension_YORU %>% 
  filter(frame <= final_image_index)


gp <- ggplot() +
  
  geom_tile(data = wing_extension_YORU, aes(x = frame, y = 0), fill = "#2EA9DF") +
  geom_tile(data = wing_extension_BORIS, aes(x = frame, y = 1), fill = "#E87A90") +
  # geom_rect(aes(xmin = 3840, xmax = 4140, ymin = -0.55, ymax = 1.55), color = "black",fill = NA, alpha = 0.4) +
  labs(title = "Ethogram of Wing Extension Behavior",
       x = "Time (seconds)",
       y = "Behavior") +
  scale_y_continuous(breaks = c(0, 1), labels = c("YORU", "Manual")) +  # y軸のラベルを非表示にします
  scale_x_continuous(expand = c(0, 1), limits = c(0, 14400), breaks = seq(0, 14400, 3600), labels = seq(0, 480, 120)) + # 上記で作成したlabelsを利用。
  theme(title.y = element_text(colour = "white")) +
  theme_classic()
gp

gp_zoomin <- gp + coord_cartesian(xlim = c(8100, 8700)) +
  geom_rect(aes(xmin = 8234, xmax = 8253, ymin = -0.55, ymax = 1.55), color = "#222222",fill = NA, alpha = 0.2, linetype = 6, linewidth =0.5) +
  geom_rect(aes(xmin = 8516, xmax = 8535, ymin = -0.55, ymax = 1.55), color = "#222222",fill = NA, alpha = 0.2, linetype = 6, linewidth =0.5) +

  scale_x_continuous(expand = c(0, 0), breaks = seq(8100, 8700, 150), labels = seq(270, 290, 5)) +
  # scale_x_continuous(expand = c(0, 0), breaks = seq(8100, 8700, 20)) +
  theme(axis.line.y = element_line(colour = "white")) +
  theme(title = element_text(colour = "white")) +
  theme(axis.ticks.y = element_line(colour = "white"))
gp_zoomin

gp <- gp +
  geom_rect(aes(xmin = 8100, xmax =8700, ymin = -0.55, ymax = 1.55), color = "#222222",fill = NA, alpha = 0.2) +
  theme(axis.line.y = element_line(colour = "white")) +
  theme(title = element_text(colour = "white")) +
  theme(axis.ticks.y = element_line(colour = "white"))
gp

ggsave("./outputs/wing_extension_graph_compare.png",gp,  width = 7, height = 1.2)
ggsave("./outputs/wing_extension_graph_compare.svg",gp,  width = 7, height = 1.2)

ggsave("./outputs/wing_extension_graph_compare_zoomin.png",gp_zoomin,  width = 7, height = 1.2)
ggsave("./outputs/wing_extension_graph_compare_zoomin.svg",gp_zoomin,  width = 7, height = 1.2)
