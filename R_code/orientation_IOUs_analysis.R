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
  data_path = "../datas/iou_data_orientation.csv"
  df_data <- readr::read_csv(data_path) 
  head(df_data)
  
  df_data <- df_data %>%
    transform(models = factor(model, levels = c("yolov5n", "yolov5s", "yolov5m", "yolov5l","yolov5x"))) %>%
    mutate(train_no = as.character(training_no)) %>%
    transform(train_no = factor(train_no, levels = c("200", "500", "1000", "1500","2000")))
  
  head(df_data)
  
  df_data_orientation =  df_data[df_data$class_name=="orientation",]

  
  # 集計結果を保存するリストを作成
  results <- list()
  
  # モデルごとの処理
  models <- c("yolov5n", "yolov5s", "yolov5m", "yolov5l", "yolov5x")
  training_levels <- c("200", "500", "1000", "1500", "2000")
  
  
  for (training_no_f in training_levels) {
    for (model_f in models) {
      # フィルタリング
      filtered_data <- df_data_orientation %>%
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
  write.csv(summary_df, "summary_statistics_orientation.csv", row.names = FALSE)
  
  # 集計結果のプレビュー
  print(summary_df)

  
  # 
  # df_data_orientation_5n = df_data_orientation[ df_data_orientation$models=="yolov5n",]
  # 
  # df_data_orientation_5n_200 =   df_data_orientation_5n[ df_data_orientation_5n$training_no==200,]
  # df_data_orientation_5n_500 = df_data_orientation_5n[ df_data_orientation_5n$training_no==500,]
  # df_data_orientation_5n_1000 = df_data_orientation_5n[ df_data_orientation_5n$training_no==1000,]
  # df_data_orientation_5n_1500 = df_data_orientation_5n[ df_data_orientation_5n$training_no==1500,]
  # df_data_orientation_5n_2000 = df_data_orientation_5n[ df_data_orientation_5n$training_no==2000,]
  # 
  # describe(df_data_orientation_5n_200)
  # describe(df_data_orientation_5n_500)
  # describe(df_data_orientation_5n_1000)
  # describe(df_data_orientation_5n_1500)
  # describe(df_data_orientation_5n_2000)
  # 
  # df_data_orientation_5s = df_data_orientation[ df_data_orientation$models=="yolov5s",]
  # 
  # df_data_orientation_5s_200 =   df_data_orientation_5s[ df_data_orientation_5s$training_no==200,]
  # df_data_orientation_5s_500 = df_data_orientation_5s[ df_data_orientation_5s$training_no==500,]
  # df_data_orientation_5s_1000 = df_data_orientation_5s[ df_data_orientation_5s$training_no==1000,]
  # df_data_orientation_5s_1500 = df_data_orientation_5s[ df_data_orientation_5s$training_no==1500,]
  # df_data_orientation_5s_2000 = df_data_orientation_5s[ df_data_orientation_5s$training_no==2000,]
  # 
  # describe(df_data_orientation_5s_200)
  # describe(df_data_orientation_5s_500)
  # describe(df_data_orientation_5s_1000)
  # describe(df_data_orientation_5s_1500)
  # describe(df_data_orientation_5s_2000)
  # 
  # df_data_orientation_5m = df_data_orientation[ df_data_orientation$models=="yolov5m",]
  # 
  # df_data_orientation_5m_200 =   df_data_orientation_5m[ df_data_orientation_5m$training_no==200,]
  # df_data_orientation_5m_500 = df_data_orientation_5m[ df_data_orientation_5m$training_no==500,]
  # df_data_orientation_5m_1000 = df_data_orientation_5m[ df_data_orientation_5m$training_no==1000,]
  # df_data_orientation_5m_1500 = df_data_orientation_5m[ df_data_orientation_5m$training_no==1500,]
  # df_data_orientation_5m_2000 = df_data_orientation_5m[ df_data_orientation_5m$training_no==2000,]
  # 
  # describe(df_data_orientation_5m_200)
  # describe(df_data_orientation_5m_500)
  # describe(df_data_orientation_5m_1000)
  # describe(df_data_orientation_5m_1500)
  # describe(df_data_orientation_5m_2000)
  # 
  # 
  # df_data_orientation_5l = df_data_orientation[ df_data_orientation$models=="yolov5l",]
  # 
  # df_data_orientation_5l_200 =   df_data_orientation_5l[ df_data_orientation_5l$training_no==200,]
  # df_data_orientation_5l_500 = df_data_orientation_5l[ df_data_orientation_5l$training_no==500,]
  # df_data_orientation_5l_1000 = df_data_orientation_5l[ df_data_orientation_5l$training_no==1000,]
  # df_data_orientation_5l_1500 = df_data_orientation_5l[ df_data_orientation_5l$training_no==1500,]
  # df_data_orientation_5l_2000 = df_data_orientation_5l[ df_data_orientation_5l$training_no==2000,]
  # 
  # describe(df_data_orientation_5l_200)
  # describe(df_data_orientation_5l_500)
  # describe(df_data_orientation_5l_1000)
  # describe(df_data_orientation_5l_1500)
  # describe(df_data_orientation_5l_2000)
  # 
  # 
  # df_data_orientation_5x = df_data_orientation[ df_data_orientation$models=="yolov5x",]
  # 
  # df_data_orientation_5x_200 =   df_data_orientation_5x[ df_data_orientation_5x$training_no==200,]
  # df_data_orientation_5x_500 = df_data_orientation_5x[ df_data_orientation_5x$training_no==500,]
  # df_data_orientation_5x_1000 = df_data_orientation_5x[ df_data_orientation_5x$training_no==1000,]
  # df_data_orientation_5x_1500 = df_data_orientation_5x[ df_data_orientation_5x$training_no==1500,]
  # df_data_orientation_5x_2000 = df_data_orientation_5x[ df_data_orientation_5x$training_no==2000,]
  # 
  # describe(df_data_orientation_5x_200)
  # describe(df_data_orientation_5x_500)
  # describe(df_data_orientation_5x_1000)
  # describe(df_data_orientation_5x_1500)
  # describe(df_data_orientation_5x_2000)
  
  df_data_filtered <- df_data %>%
    # dplyr::mutate(training_no = as.character(training_no)) %>%
    dplyr::filter(model == "yolov5s") 

  # definition of labels
  # ap_labs <- as_labeller(c(`ap_50` = "AP@50", `ap_75` = "AP@75"))
  class_labs <- as_labeller(c(`0` = "orientation", `1` = "no")) 
  train_no_labs <- as_labeller(c(`200` = "200 images", `500` = "500 images", `1000` = "1000 images", `1500` = "1500 images", `2000` = "2000 images")) 
  
  # Only orientation
  df_ori_data = filter(df_data, class == "0")
  
  gp_ori = ggplot(data = df_ori_data, aes(x = train_no, y = value, fill = models)) + 
    # geom_boxplot() +
    geom_violin(color = "#434343", alpha = 0.8) +

    facet_grid(class ? train_no, labeller = labeller(class = class_labs, train_no = train_no_labs), scales = "free_x") +
    labs(title="zebrafish - orientation", x = "Training images", y = "IOU")+
    scale_color_discrete(name = element_blank(), labels = text_label_legend)+
    Setting
  gp_ori
  
  gp_v5s = ggplot(data = df_data_filtered, aes(x = train_no, y = value)) + 
    # geom_boxplot() +
    geom_violin() +
    # geom_line()+
    facet_grid(. ? class, labeller = labeller(class = class_labs, train_no = train_no_labs)) +
    labs(title="zebra orientation model", x = "Training images", y = "Value")+
    scale_color_discrete(name = element_blank(), labels = text_label_legend)+
    Setting
  gp_v5s
  
  
  
  ggsave("../graph/model_evaluation_iou_orientation.png", gp_ori, width = 12, height = 3, dpi = 500)
  ggsave("../graph/model_evaluation_iou_orientation.svg", gp_ori, width = 12, height = 3, dpi = 500)