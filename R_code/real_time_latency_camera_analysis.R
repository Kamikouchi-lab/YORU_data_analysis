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
data_path = "./data/results_camera_data.csv"
df_4080_data <- readr::read_csv(data_path)
head(df_4080_data)
#Changing label weights
df_4080_data <- transform(df_4080_data, camera = factor(camera, levels = c("ELP", "IS")))
df_4080_data$diff_ms <- df_4080_data$diff_s * 1000

df_ELP = df_4080_data[df_4080_data$camera=="ELP",]
df_IS = df_4080_data[df_4080_data$camera=="IS",]

describe(df_ELP)
describe(df_IS)

# df_4080_data <- df_4080_data[df_4080_data$size <= 650 , ]

size_labs <- as_labeller(c("1280" = "1280x1024 (100fps)", "640" = "640x480 (100fps)"))  

custom_palette <- brewer.pal(n = 4, name = "Paired")[3:4]

gp = ggplot(data = df_4080_data, aes(x = camera, y = diff_ms)) + 
  geom_violin(aes(fill = camera)) +
  facet_grid(. ? size, labeller = labeller(size = size_labs)) +
  labs(title="", x = "Training models", y = "Duration (ms)")+
  scale_fill_manual(values = custom_palette) +
  scale_x_discrete(labels = c("IS" = "1", "ELP"="2")) +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 50), expand = c(0, 0)) +
  Setting
gp

gp2 = ggplot(data = df_4080_data, aes(x = camera, y = diff_ms)) + 
  geom_violin(aes(fill = camera)) +
  facet_grid(. ? size, labeller = labeller(size = size_labs)) +
  labs(title="", x = "Training models", y = "Latency (ms)")+
  scale_fill_manual(values = custom_palette) +
  # scale_x_discrete(labels = c("IS" = "1", "ELP"="2")) +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 50), expand = c(0, 0)) +
  Setting
gp2


ggsave("./outputs/camera_latency.png", gp, width = 3, height = 4, dpi = 500)
ggsave("./outputs/camera_latency.svg", gp, width = 3, height = 4, dpi = 500)