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
data_path = "./data/results_memory_data.csv"
df_4080_data <- readr::read_csv(data_path)
head(df_4080_data)
#Changing label weights
df_4080_data <- transform(df_4080_data, memory = factor(memory, levels = c("16GB", "32GB")))
df_4080_data$diff_ms <- df_4080_data$diff_s * 1000

df_16GB = df_4080_data[df_4080_data$memory=="16GB",]
df_32GB = df_4080_data[df_4080_data$memory=="32GB",]

describe(df_16GB)
describe(df_32GB)

# df_4080_data <- df_4080_data[df_4080_data$size <= 650 , ]
size_labs <- as_labeller(c("1280" = "1280x1024 (100fps)", "640" = "640x480 (100fps)"))  

custom_palette <- brewer.pal(n = 4, name = "Paired")[1:2]

gp = ggplot(data = df_4080_data, aes(x = memory, y = diff_ms)) + 
  geom_violin(aes(fill = memory)) +
  facet_grid(. ? size, labeller = labeller(size = size_labs)) +
  labs(title="", x = "Training models", y = "Duration (ms)")+
  scale_fill_manual(values = custom_palette) +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 50), expand = c(0, 0)) +
  scale_x_discrete(labels = c("32GB" = "1", "16GB"="2")) +
  Setting
gp

gp2 = ggplot(data = df_4080_data, aes(x = memory, y = diff_ms)) + 
  geom_violin(aes(fill = memory)) +
  facet_grid(. ? size, labeller = labeller(size = size_labs)) +
  labs(title="", x = "Training models", y = "Duration (ms)")+
  scale_fill_manual(values = custom_palette) +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 50), expand = c(0, 0)) +
  # scale_x_discrete(labels = c("32GB" = "1", "16GB"="2")) +
  Setting
gp2

ggsave("./outputs/memory_latency.png", gp, width = 3, height = 4, dpi = 500)
ggsave("./outputs/memory_latency.svg", gp, width = 3, height = 4, dpi = 500)