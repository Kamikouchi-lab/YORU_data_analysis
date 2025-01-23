library(ggplot2)
library(dplyr)

data_boris <- read.csv("./data/BORIS_data/20231003_2_BORIS.csv", header = TRUE)

final_image_index <- data_boris %>% 
  filter(Behavior == "finish") %>% 
  pull(Image.index)

print(final_image_index)

data_boris$start_time <- ifelse(data_boris$Behavior.type == "START", data_boris$Image.index, NA)
data_boris$end_time <- ifelse(data_boris$Behavior.type == "STOP", data_boris$Image.index, NA)

data_boris$start_time <- zoo::na.locf(data_boris$start_time, na.rm = FALSE)
data_boris_r <- data_boris %>%
  filter(!is.na(end_time))

# startからstopまでのフレームを取得
trophallaxis_BORIS <- do.call(rbind, lapply(1:nrow(data_boris_r), function(i) {
  start_frame <- data_boris_r$start_time[i]
  stop_frame <- data_boris_r$end_time[i]
  data.frame(frame = seq(start_frame, stop_frame))
}))
trophallaxis_BORIS$label <-"trophallaxis"


# Load YORU data
data_yoru <- read.csv("./data/YORU_data/20231003_2.csv")

# trophallaxisクラスのデータをフィルタリング
trophallaxis_YORU <- subset(data_yoru, class_name == "trophallaxis")
trophallaxis_YORU <- trophallaxis_YORU %>% 
  filter(frame <= final_image_index)

gp <- ggplot() +
  
  geom_tile(data = trophallaxis_YORU, aes(x = frame, y = 0), fill = "#2EA9DF") +
  geom_tile(data = trophallaxis_BORIS, aes(x = frame, y = 1), fill = "#E87A90") +
  # geom_rect(aes(xmin = 3840, xmax = 4140, ymin = -0.55, ymax = 1.55), color = "black",fill = NA, alpha = 0.4) +
  labs(title = "Ethogram of Wing Extension Behavior",
       x = "Time (minutes)",
       y = "Behavior") +
  scale_y_continuous(breaks = c(0, 1), labels = c("YORU", "Manual")) +  # y軸のラベルを非表示にします
  scale_x_continuous(expand = c(0, 1), limits = c(0, 54000), breaks = seq(0, 54000, 9000), labels = seq(0, 30, 5)) + # 上記で作成したlabelsを利用。
  theme_classic()
gp

gp_zoomin <- gp + coord_cartesian(xlim = c(21600, 25200)) +
  geom_rect(aes(xmin = 21940, xmax = 22120, ymin = -0.55, ymax = 1.55), color = "#222222",fill = NA, linetype = 6, linewidth =0.5) +
  geom_rect(aes(xmin = 22400, xmax = 22500, ymin = -0.55, ymax = 1.55), color = "#222222",fill = NA, linetype = 6, linewidth =0.5) +
  geom_rect(aes(xmin = 22870, xmax = 22970, ymin = -0.55, ymax = 1.55), color = "#222222",fill = NA, linetype = 6, linewidth =0.5) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(21600, 25200, 1800), labels = seq(12, 14, 1)) +
  theme(axis.line.y = element_line(colour = "white")) +
  theme(title = element_text(colour = "white")) +
  theme(axis.ticks.y = element_line(colour = "white"))
gp_zoomin

gp <- gp +
  geom_rect(aes(xmin = 21600, xmax = 25200, ymin = -0.55, ymax = 1.55), color = "#222222",fill = NA) +
  # geom_rect(aes(xmin = 5300, xmax = 5400, ymin = -0.55, ymax = 1.55), color = "#222222",fill = NA) +
  theme(axis.line.y = element_line(colour = "white")) +
  theme(title = element_text(colour = "white")) +
  theme(axis.ticks.y = element_line(colour = "white"))
gp

ggsave("./outputs/trophallaxis_graph_compare.png",gp,  width = 7, height = 1.2)
ggsave("./outputs/trophallaxis_graph_compare.svg",gp,  width = 7, height = 1.2)

ggsave("./outputs/trophallaxis_graph_compare_zoomin.png",gp_zoomin,  width = 7, height = 1.2)
ggsave("./outputs/trophallaxis_graph_compare_zoomin.svg",gp_zoomin,  width = 7, height = 1.2)

