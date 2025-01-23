library(ARTool)
library(lawstat)
library(ggplot2) 
library(tidyverse)
library(ggsignif)
library(emmeans)
library(rcompanion)
library(psych)
library(car)
library(RColorBrewer)

###Setting of the Graph###

Setting <- list(theme(plot.margin = unit(c(1, 1, 1, 1), "lines")),
                theme_bw(), 
                theme(axis.title.y = element_text(size = 15)),
                theme(axis.title.x = element_text(size = 15, vjust = -0.5)),
                theme(axis.text.y = element_text(size = 15, colour = "black")),
                theme(axis.title.x = element_blank()),
                theme(axis.text.x = element_text(size = 15, colour = "black", angle = 45, hjust = 1)),
                theme(strip.text.x = element_text(size = 18)),
                theme(strip.text.y = element_text(size = 18)),
                theme(legend.position = "none"),
                theme(text = element_text(size = 8)))

#read to CSV
data_path = "./data/results_fps_data.csv"
df_4080_data <- readr::read_csv(data_path)
head(df_4080_data)
#Changing label weights
df_4080_data <- transform(df_4080_data, fps = factor(fps, levels = c("30fps", "60fps", "100fps", "130fps", "160fps", "200fps")))
df_4080_data$diff_ms <- df_4080_data$diff_s * 1000

df_30 = df_4080_data[df_4080_data$fps=="30fps",]
df_60 = df_4080_data[df_4080_data$fps=="60fps",]
df_100 = df_4080_data[df_4080_data$fps=="100fps",]
df_130 = df_4080_data[df_4080_data$fps=="130fps",]
df_160 = df_4080_data[df_4080_data$fps=="160fps",]
df_200 = df_4080_data[df_4080_data$fps=="200fps",]

print("df_30")
describe(df_30)
print("df_60")
describe(df_60)
print("df_100")
describe(df_100)
print("df_130")
describe(df_130)
print("df_160")
describe(df_160)
print("df_200")
describe(df_200)

# df_4080_data <- df_4080_data[df_4080_data$size <= 650 , ]

size_labs <- as_labeller(c("1280" = "1280x1024", "640" = "640x480"))  

# df_tro_data = filter(df_all_data, class == "0")
gp = ggplot(data = df_4080_data, aes(x = fps, y = diff_ms, fill = fps)) + 
  geom_violin(aes(fill = fps)) +
  scale_fill_brewer(palette="YlOrRd") +
  facet_grid(. ? size, labeller = labeller(size = size_labs)) +
  labs(title="", x = "Training models", y = "Duration (ms)")+
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 50), expand = c(0, 0)) +
  scale_x_discrete(labels = c("yolov5n" = "YOLOv5n", "yolov5s"="YOLOv5s", "yolov5m"="YOLOv5m", "yolov5l"="YOLOv5l", "yolov5x"="YOLOv5x")) +
  Setting
gp


ggsave("./outputs/fps_latency.png", gp, width = 7, height = 4, dpi = 500)
ggsave("./outputs/fps_latency.svg", gp, width = 7, height = 4, dpi = 500)


# gp_640 = ggplot(data = df_4080_data, aes(x = fps, y = diff_s)) +
#   geom_violin(aes(fill = fps)) +
#   # geom_jitter(size = 0.01, width =0.1) +
#   scale_y_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 0.02), expand = c(0, 0)) +
#   labs(y = "Duration (sec)") +
#   # scale_x_discrete(labels = c("yolov5n" = "YOLOv5n", "yolov5s"="YOLOv5s", "yolov5m"="YOLOv5m", "yolov5l"="YOLOv5l", "yolov5x"="YOLOv5x")) +
#   Setting
# gp_640
# 
# 
# ggsave("../graph/YORU_fps_latency_640.png", gp_640, width = 10, height = 6, dpi = 300)