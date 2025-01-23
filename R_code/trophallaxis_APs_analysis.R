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
                scale_y_continuous(limits = c(0.40, 1.05), breaks = seq(0.40, 1, 0.2), expand = c(0, 0)),
                scale_x_continuous(limits = c(0, 2200), breaks = seq(0, 2000, 500), expand = c(0, 0)),
                theme(axis.text.x = element_text(size = 15, colour = "black", angle = 45, hjust = 1)),
                theme(strip.text.x = element_text(size = 18)),
                theme(strip.text.y = element_text(size = 18)),
                theme(legend.position = c(0.87, 0.16), legend.justification = "center"),
                theme(text = element_text(size = 8)))


#read to CSV
data_path = "../datas/ant_tro.csv"
df_data <- readr::read_csv(data_path)
head(df_data)

df_data <- transform(df_data, models = factor(models, levels = c("yolov5n", "yolov5s", "yolov5m", "yolov5l","yolov5x")))


df_0<- df_data %>%
  filter(class %in% c(0))
head(df_0)

df_1<- df_data %>%
  filter(class %in% c(1))
head(df_1)


gp_ap50_0 = ggplot(data = df_0, aes(x = no_of_images, y = ap_50, color = models)) + 
  geom_point() +
  geom_line()+
  labs(title="AP50 class: 0", x = "Training images", y = "Value")+
  scale_color_discrete(name = element_blank(), labels = text_label_legend)+
  # scale_color_discrete(name = element_blank()) +
  Setting
gp_ap50_0


gp_ap50_1 = ggplot(data = df_1, aes(x = no_of_images, y = ap_50, color = models)) + 
  geom_point() +
  geom_line()+
  labs(title="AP50 class: 1 extension", x = "Training images", y = "Value")+
  scale_color_discrete(name = element_blank(), labels = text_label_legend)+
  # scale_color_discrete(name = element_blank()) +
  Setting
gp_ap50_1

gp_ap75_0 = ggplot(data = df_0, aes(x = no_of_images, y = ap_75, color = models)) + 
  geom_point() +
  geom_line()+
  labs(title="AP75 class: 0", x = "Training images", y = "Value")+
  scale_color_discrete(name = element_blank(), labels = text_label_legend)+
  # scale_color_discrete(name = element_blank()) +
  Setting
gp_ap75_0


gp_ap75_1 = ggplot(data = df_1, aes(x = no_of_images, y = ap_75, color = models)) + 
  geom_point() +
  geom_line()+
  labs(title="AP75 class: 1 extension", x = "Training images", y = "Value")+
  scale_color_discrete(name = element_blank(), labels = text_label_legend)+
  # scale_color_discrete(name = element_blank()) +
  Setting
gp_ap75_1



# legend 
# g1<- ggplotGrob(gp_ap50_0)
# id.legend <- grep("guide", g1$layout$name)
# legend <- g1[["grobs"]][[id.legend]]
# 
# # install.packages("gridExtra")
# library(gridExtra)
# layout1<- rbind(c(1, 3), 
#                 c(2, 4))
# graph_1 =grid.arrange(gp_ap50_0 + theme(legend.position="none"),
#                       gp_ap50_1 + theme(legend.position="none"),
#                       gp_ap75_0 + theme(legend.position="none"),
#                       gp_ap75_1 + theme(legend.position="none"),
#                       layout_matrix =layout1)
# # graph_1$widths = grid::unit.pmax(gp_ap50_0$widths, gp_ap75_0$widths)
# 
# layout2<- rbind(c(1, 3, 5), 
#                 c(2, 4, 6))
# graph_2 =grid.arrange(gp_ap50_0 + theme(legend.position="none"),
#                       gp_ap50_1 + theme(legend.position="none"),
#                       gp_ap75_0 + theme(legend.position="none"),
#                       gp_ap75_1 + theme(legend.position="none"),
#                       legend,
#                       layout_matrix =layout2)

df_all_data = df_data %>%
  pivot_longer(cols = c('ap_50', 'ap_75'),
               names_to = "threshold",
               values_to = "AP_value")

# definition of labels
ap_labs <- as_labeller(c(`ap_50` = "AP@50", `ap_75` = "AP@75"))  
class_labs <- as_labeller(c(`0` = "trophallaxis", `1` = "no"))  

gp_all = ggplot(data = df_all_data, aes(x = no_of_images, y = AP_value, color = models)) + 
  geom_point() +
  geom_line()+
  facet_grid(class ? threshold, labeller = labeller(class = class_labs, threshold = ap_labs)) +
  labs(title="trophallaxis model", x = "Training images", y = "Value")+
  scale_color_discrete(name = element_blank(), labels = text_label_legend)+
  Setting
gp_all

# Only trophallaxis
df_tro_data = filter(df_all_data, class == "0")
gp_tro = ggplot(data = df_tro_data, aes(x = no_of_images, y = AP_value, color = models)) + 
  geom_point(size = 1.5) +
  geom_line(linewidth = 0.3, alpha = 0.5)+
  facet_grid(class ? threshold, labeller = labeller(class = class_labs, threshold = ap_labs)) +
  labs(title="", x = "Training images", y = "Value")+
  scale_color_discrete(name = element_blank(), labels = text_label_legend)+
  Setting
gp_tro



ggsave("../graph/model_evaluation_trophallaxis.png", gp_tro, width = 5, height = 6, dpi = 500)
ggsave("../graph/model_evaluation_trophallaxis.svg", gp_tro, width = 5, height = 6, dpi = 500)