library(ggplot2)
library("ggsignif")
library(tidyverse)
library(RVAideMemoire)

text_label_legend <- c("YOLOv5n", "YOLOv5s","YOLOv5m","YOLOv5l","YOLOv5x")

Setting <- list(theme(plot.margin = unit(c(1, 1, 1, 1), "lines")),
                theme_bw(), 
                theme(axis.title.y = element_text(size = 15)),
                theme(axis.title.x = element_text(size = 15, vjust = -0.5)),
                theme(axis.text.y = element_text(size = 15, colour = "black")),
                scale_y_continuous(limits = c(0.6, 1.05), breaks = seq(0.6, 1, 0.2), expand = c(0, 0)),
                scale_x_continuous(limits = c(0, 2200), breaks = seq(0, 2000, 500), expand = c(0, 0)),
                theme(axis.text.x = element_text(size = 15, colour = "black", angle = 45, hjust = 1)),
                theme(strip.text.x = element_text(size = 18)),
                theme(strip.text.y = element_text(size = 18)),
                theme(legend.position = c(0.87, 0.1), legend.justification = "center"),
                theme(text = element_text(size = 8)))


#read to CSV
data_path = "../datas/wing_extension_ap.csv"
df_data <- readr::read_csv(data_path)
head(df_data)

df_data <- transform(df_data, models = factor(models, levels = c("yolov5n", "yolov5s", "yolov5m", "yolov5l","yolov5x")))


df_fly<- df_data %>%
  filter(class %in% c(0))
head(df_fly)

df_wing<- df_data %>%
  filter(class %in% c(1))
head(df_wing)

df_cop<- df_data %>%
  filter(class %in% c(2))
head(df_cop)


df_all_data = df_data %>%
  pivot_longer(cols = c('ap_50', 'ap_75'),
               names_to = "threshold",
               values_to = "AP_value")

# definition of labels??`
ap_labs <- as_labeller(c(`ap_50` = "AP@50", `ap_75` = "AP@75"))  
class_labs <- as_labeller(c(`0` = "fly", `1` = "wing extension", `2` = "copulation"))  
gp_all = ggplot(data = df_all_data, aes(x = no_of_images, y = AP_value, color = models)) + 
  geom_point(size = 1.5) +
  geom_line(linewidth = 0.3, alpha = 0.5)+
  facet_grid(class ~ threshold, labeller = labeller(class = class_labs, threshold = ap_labs)) +
  labs(title="", x = "Training images", y = "Value")+
  scale_color_discrete(name = element_blank(), labels = text_label_legend)+
  Setting
gp_all



ggsave("../graph/model_evaluation_group_wing_extension.png", gp_all, width = 5, height = 9, dpi = 500)
ggsave("../graph/model_evaluation_group_wing_extension.svg", gp_all, width = 5, height = 9, dpi = 500)