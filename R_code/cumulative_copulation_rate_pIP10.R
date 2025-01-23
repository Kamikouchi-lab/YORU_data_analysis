###import libraries###
# install.packages("survRM2")
# install.packages("survminer")
# install.packages("svglite")
# install.packages("tidyverse")
# install.packages('readr', dependencies = TRUE, repos='http://cran.rstudio.com/')
library(survival)
library(survRM2)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(survminer)
library(svglite)
library(gt)
library(tidyverse)
library(gridExtra)

###Setting of the Graph###

Setting <- list(theme(plot.margin = unit(c(1, 1, 1, 1), "lines")),
                theme_classic(),
                scale_color_manual(values = c("#2EA9DF", "#E87A90", "#90B44B")),
                theme(axis.title.y = element_text(size = 20, vjust = 2)),
                theme(axis.text.y = element_text(size = 20, colour = "black")),
                theme(axis.text.x = element_text(size = 20, colour = "black")),
                theme(axis.title.x = element_text(size = 20)))

###Read data###
data_csv_path <- "./data/analysis_data.csv"
data <- readr::read_csv(data_csv_path) %>%
  dplyr::filter(group != "pIP10_GFP_later")
data <-  transform(data, group = factor(group, levels = c("pIP10_GtACR1_event","pIP10_GtACR1_later", "pIP10_GFP_event")))
head(data)


###Analyze log-rank###
data$surv_obj <- with(data, Surv(courtship_duration_sec, event))
tau <- 1800 # analysis limit time (sec)
fit <- survfit(surv_obj ~ group, data = data)
# print(rmst_res)




###Calculate sample size###
group_sizes <- data %>%
  group_by(group) %>%
  summarise(n = n())


# Add sample size in labels
labels <- paste(group_sizes$group, "(n =", group_sizes$n, ")")

# creating graph
gp <- ggplot(data, aes(x=courtship_duration_sec, colour=group)) +
  stat_ecdf(geom = "step", size = 2) +
  theme_classic() +
  coord_cartesian(xlim = c(0, 1800), ylim = c(0, 1)) +
  scale_x_continuous(
    breaks = seq(0, 1800, by = 300), # per 300 s
    labels = seq(0, 1800, by = 300) / 60, # seconds to minutes
    expand = c(0, 0),
    name = "Copulation Latency (minutes)" 
    ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.2), # per 300 s
    expand = c(0.01, 0),
    name = "Cumulative Copulation" 
  ) +
  scale_colour_discrete(labels = labels) + # add labelsSetting
  Setting
gp

ggsave(filename="./output/cumulative_copulation_rate.svg", plot=gp
       ,width=8, height=4, dpi=300, units="in", device="svg")
ggsave(filename="./output/cumulative_copulation_rate.png", plot=gp
       ,width=8, height=4, dpi=300)


###Kaplan-Meier estimator and log-rank test###
# Kaplan-Meier estimator
time_limit = 1800
data$limited_latency = pmin(data$courtship_duration_sec, time_limit)
data$status_limited = as.integer(data$courtship_duration_sec <= time_limit & data$event == 1)

data$surv_obj <- with(data, Surv(limited_latency, status_limited))
# data$surv_obj <- with(data, Surv(latency, status_limited))
fit <- survfit(surv_obj ~ group, data = data)
# graph
gp2 <- ggsurvplot(fit, data = data, pval = TRUE, conf.int = TRUE,
           xlab = "Copulation latency (seconds)", ylab = "copulation rate")
# gp2

# log-rank test
logrank_test <- survdiff(surv_obj ~ group, data = data)
print(logrank_test)

# pairwise_log-rank test (https://www.rdocumentation.org/packages/survminer/versions/0.4.9/topics/pairwise_survdiff)
pairwise_results <- pairwise_survdiff(surv_obj ~ group, data = data, p.adjust.method = "BH")
# results
print(pairwise_results)

# # p-adjusts by BH
p_values <- pairwise_results$p.value
# p_adjusted <- p.adjust(p_values, method = "BH")
print(p_values)
# print(p_adjusted)


###add statistical data in graph###

# # pairwise_results ~~~~~O~~~[~v~y~A~̖~~O~~~擾~i~T~~~v~~~Ƃ~~āj
# row_names <- rownames(pairwise_results$p.value)
# col_names <- colnames(pairwise_results$p.value)
# matrix_p <- matrix(p_adjusted, nrow=3, ncol=3)
# rownames(matrix_p) <- row_names
# colnames(matrix_p) <- col_names
# print(matrix_p)

# Cox proportional hazards model to get Hazard Ratios
cox_model <- coxph(surv_obj ~ group, data = data)
summary(cox_model)




# grid.arrange(gp2$plot, p_adjusted_table, ncol = 2)

packageVersion("survival")