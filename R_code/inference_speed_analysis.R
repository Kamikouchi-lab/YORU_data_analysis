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
data_path = "../reshape_csv/results_data_rtx4080.csv"
df_4080_data <- readr::read_csv(data_path)
head(df_4080_data)
#Changing label weights
df_4080_data <- transform(df_4080_data, model = factor(model, levels = c("yolov5n", "yolov5s", "yolov5m", "yolov5l", "yolov5x")))
df_4080_data$time_ms <- df_4080_data$time * 1000

df_640_4080_data <- df_4080_data[df_4080_data$size <= 650 , ]

df_640_yolov5n = df_640_4080_data[df_640_4080_data$model=="yolov5n",]
df_640_yolov5s = df_640_4080_data[df_640_4080_data$model=="yolov5s",]
df_640_yolov5m = df_640_4080_data[df_640_4080_data$model=="yolov5m",]
df_640_yolov5l= df_640_4080_data[df_640_4080_data$model=="yolov5l",]
df_640_yolov5x = df_640_4080_data[df_640_4080_data$model=="yolov5x",]

describe(df_640_yolov5n)
describe(df_640_yolov5s)
describe(df_640_yolov5m)
describe(df_640_yolov5l)
describe(df_640_yolov5x)

df_1280_4080_data <- df_4080_data[df_4080_data$size >= 650 , ]


df_1280_yolov5n = df_1280_4080_data[df_1280_4080_data$model=="yolov5n",]
df_1280_yolov5s = df_1280_4080_data[df_1280_4080_data$model=="yolov5s",]
df_1280_yolov5m = df_1280_4080_data[df_1280_4080_data$model=="yolov5m",]
df_1280_yolov5l= df_1280_4080_data[df_1280_4080_data$model=="yolov5l",]
df_1280_yolov5x = df_1280_4080_data[df_1280_4080_data$model=="yolov5x",]

describe(df_1280_yolov5n)
describe(df_1280_yolov5s)
describe(df_1280_yolov5m)
describe(df_1280_yolov5l)
describe(df_1280_yolov5x)

gp_640 = ggplot(data = df_640_4080_data, aes(x = model, y = time_ms)) +
  geom_violin(aes(fill = model)) +
  # geom_jitter(size = 0.01, width =0.1) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 10), expand = c(0, 0)) +
  labs(y = "Duration (ms)") +
  scale_x_discrete(labels = c("yolov5n" = "YOLOv5n", "yolov5s"="YOLOv5s", "yolov5m"="YOLOv5m", "yolov5l"="YOLOv5l", "yolov5x"="YOLOv5x")) +
  Setting
gp_640

gp_1280 = ggplot(data = df_1280_4080_data, aes(x = model, y = time_ms)) +
  geom_violin(aes(fill = model)) +
  # geom_jitter(size = 0.01, width =0.1) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 10), expand = c(0, 0)) +
  labs(y = "Duration (ms)") +
  scale_x_discrete(labels = c("yolov5n" = "YOLOv5n", "yolov5s"="YOLOv5s", "yolov5m"="YOLOv5m", "yolov5l"="YOLOv5l", "yolov5x"="YOLOv5x")) +
  Setting
gp_1280

size_labs <- as_labeller(c("1280" = "1280x1024", "640" = "640x480"))  

# df_tro_data = filter(df_all_data, class == "0")
gp = ggplot(data = df_4080_data, aes(x = model, y = time_ms, fill = model)) + 
  geom_violin(aes(fill = model)) +
  # scale_fill_brewer(palette="YlOrRd") +
  facet_grid(. ~ size, labeller = labeller(size = size_labs)) +
  labs(title="", x = "Training models", y = "Latency (ms)")+
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 10), expand = c(0, 0)) +
  scale_x_discrete(labels = c("yolov5n" = "YOLOv5n", "yolov5s"="YOLOv5s", "yolov5m"="YOLOv5m", "yolov5l"="YOLOv5l", "yolov5x"="YOLOv5x")) +
  Setting
gp


ggsave("../graph/latency_yolo_frame.png", gp, width = 10, height = 4, dpi = 500)
ggsave("../graph/latency_yolo_frame.svg", gp, width = 10, height = 4, dpi = 500)


# ggsave("../graph/latency_640.png", gp_640, width = 10, height = 6, dpi = 300)
# ggsave("../graph/latency_640.svg", gp_640, width = 10, height = 6, dpi = 300)
# ggsave("../graph/latency_1280.png", gp_1280, width = 10, height = 6, dpi = 300)
# ggsave("../graph/latency_1280.svg", gp_1280, width = 10, height = 6, dpi = 300)