library(ggplot2)
library(dplyr)

data_boris <- read.csv("./data/BORIS_data/20529_3_on.csv", header = TRUE)

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
orientation_BORIS <- do.call(rbind, lapply(1:nrow(data_boris_r), function(i) {
  start_frame <- data_boris_r$start_time[i]
  stop_frame <- data_boris_r$end_time[i]
  data.frame(frame = seq(start_frame, stop_frame))
}))
orientation_BORIS$label <-"orientation"


# Load YORU data
data_yoru <- read.csv("./data/YORU_data/200529_3_on.csv")

# orientationクラスのデータをフィルタリング
orientation_YORU <- subset(data_yoru, class_name == "orientation")
orientation_YORU <- orientation_YORU %>% 
  filter(frame <= final_image_index)


# Load def data
data_def <- read.csv("./data/Def_data/2_processed_200529_3_onleft2_processed_200529_3_onright2.csv")

# orientationクラスのデータをフィルタリング
orientation_Def <- subset(data_def, detection == "orientation")
orientation_Def <- orientation_Def %>% 
  filter(frame <= final_image_index)

gp <- ggplot() +
  
  geom_tile(data = orientation_YORU, aes(x = frame, y = 0), fill = "#2EA9DF") +
  geom_tile(data = orientation_BORIS, aes(x = frame, y = 1), , fill = "#E87A90") +
  geom_tile(data = orientation_Def, aes(x = frame, y = 2), fill = "#90B44B") +
  # geom_rect(aes(xmin = 3840, xmax = 4140, ymin = -0.55, ymax = 1.55), color = "black",fill = NA, alpha = 0.4) +
  labs(title = "Ethogram of Wing Extension Behavior",
       x = "Time (s)",
       y = "Behavior") +
  scale_y_continuous(breaks = c(0, 1, 2), labels = c("YORU", "Manual", "Def")) +  # y軸のラベルを非表示にします
  scale_x_continuous(expand = c(0, 1), limits = c(0, 9000), breaks = seq(0, 9000, 900), labels = seq(0, 300, 30)) + # 上記で作成したlabelsを利用。
  theme_classic()
gp

gp_zoomin <- gp + coord_cartesian(xlim = c(1800, 2700)) +
  geom_rect(aes(xmin = 1930, xmax = 2030, ymin = -0.55, ymax = 2.55), color = "#222222",fill = NA, alpha = 0.2, linetype = 6, linewidth =0.5) +
  # geom_rect(aes(xmin = 3903, xmax = 3904, ymin = -0.55, ymax = 1.55), color = NA, fill = "#222222", alpha = 0.1) +
  # geom_rect(aes(xmin = 3905, xmax = 3906, ymin = -0.55, ymax = 1.55), color = NA, fill = "#222222", alpha = 0.1) +
  # geom_rect(aes(xmin = 3907, xmax = 3908, ymin = -0.55, ymax = 1.55), color = NA, fill = "#222222", alpha = 0.1) +
  geom_rect(aes(xmin = 2250, xmax = 2350, ymin = -0.55, ymax = 2.55), color = "#222222",fill = NA, alpha = 0.2, linetype = 6, linewidth =0.5) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1800, 2700, 300), labels = seq(60, 90, 10)) +
  # scale_x_continuous(expand = c(0, 0), breaks = seq(1800, 2700, 100)) +
  theme(axis.line.y = element_line(colour = "white")) +
  theme(title = element_text(colour = "white")) +
  theme(axis.ticks.y = element_line(colour = "white"))
gp_zoomin

gp <- gp +
  geom_rect(aes(xmin = 1800, xmax = 2700, ymin = -0.55, ymax = 2.55), color = "#222222",fill = NA, alpha = 0.2) +
  # geom_rect(aes(xmin = 5300, xmax = 5400, ymin = -0.55, ymax = 1.55), color = "#222222",fill = NA, alpha = 0.2) +
  theme(axis.line.y = element_line(colour = "white")) +
  theme(title = element_text(colour = "white")) +
  theme(axis.ticks.y = element_line(colour = "white"))
gp

ggsave("./outputs/orientation_graph_compare.png",gp,  width = 7, height = 1.6)
ggsave("./outputs/orientation_graph_compare.svg",gp,  width = 7, height = 1.6)

ggsave("./outputs/orientation_graph_compare_zoomin.png",gp_zoomin,  width = 7, height = 1.6)
ggsave("./outputs/orientation_graph_compare_zoomin.svg",gp_zoomin,  width = 7, height = 1.6)

