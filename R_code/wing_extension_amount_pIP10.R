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
library(easystats)
# install.packages("easystats")

###Setting of the Graph###
#label

Setting <- list(theme(plot.margin = unit(c(1, 1, 1, 1), "lines")),
                theme_classic(),
                scale_fill_manual(values = c("#2EA9DF", "#E87A90", "#90B44B")),
                scale_color_manual(values = c("#222222", "#222222", "#222222")),
                theme(axis.title.y = element_text(size = 20, vjust = 2)),
                labs(title = "TITLE"), 
                theme(plot.title = element_text(size = 20, colour = "white")),
                theme(axis.text.y = element_text(size = 20, colour = "black")),
                theme(axis.text.x = element_text(size = 20, colour = "black", angle = 45, hjust = 1)),
                theme(axis.title.x = element_blank()),
                theme(axis.ticks.x = element_blank()),
                theme(legend.position = "none"))

#read to CSV
df_data <- readr::read_csv("./outputs/reshape_data.csv") %>%
  filter(group != "pIP10_GFP_later")
head(df_data)
#Changing label weights
# df_data <- transform(df_data, group = factor(group, levels = c("pIP10_GtACR1_event", "pIP10_GFP_event")))


###wingextesnion amount###
gp2 = ggplot(data = df_data) +
  stat_qq(aes(sample = rate, color = group)) + 
  facet_wrap( ~ group, scales = "free")
gp2

sw_test_results = df_data %>%  
  group_by(group) %>% 
  summarise(statistic = shapiro.test(rate)$statistic, 
            p.value = shapiro.test(rate)$p.value,
            .groups     = "drop")
head(sw_test_results)

bartlett.test(data = df_data, rate~group)

df_event_gtacr1 = df_data[df_data$group=="pIP10_GtACR1_event",]
df_later_gtacr1 = df_data[df_data$group=="pIP10_GtACR1_later",]
df_event_gfp = df_data[df_data$group=="pIP10_GFP_event",]
# df_later_gfp = df_data[df_data$group=="pIP10_GFP_later",]

describe(df_event_gtacr1)
describe(df_later_gtacr1)
describe(df_event_gfp)
# describe(df_later_gfp)


# ART ANOVA
df_data$group <- as.factor(df_data$group)
model = art(data = df_data, rate ~ group)
summary(model)
anova(model)
amount_results = art.con(model, "group", adjust = "BH")
print(amount_results)
amount_report_df <- as.data.frame(amount_results)
write_csv(amount_report_df, file = "./outputs/result_amount_artanova_test.csv")
print(amount_report_df)

# Partial eta-squared
model.anova = anova(model)
print(model.anova, verbose=TRUE)
model.anova$eta.sq.part = with(model.anova, `Sum Sq`/(`Sum Sq` + `Sum Sq.res`))
model.anova

df_data <- transform(df_data, group = factor(group, levels = c("pIP10_GtACR1_event","pIP10_GtACR1_later", "pIP10_GFP_event")))

gp3 = ggplot(data = df_data, aes(x = group, y = rate)) +
  geom_boxplot(aes(fill = group), outlier.shape = NA) +
  geom_jitter(size = 1, width =0.1, color = "#444444") +
  
  
  #1-2
  geom_signif(
    y_position = 0.70,
    xmin = 1.1,
    xmax = 1.9,
    annotation = "*",
    tip_length = 0.03,
    size = 0.5,
    textsize = 6) + 
  
  #2-3
  geom_signif(
    y_position = 0.70,
    xmin = 2.1,
    xmax = 2.9,
    annotation = "n.s.",
    tip_length = 0.03,
    size = 0.5,
    textsize = 6) + 
  
  #1-3
  geom_signif(
    y_position = 0.79,
    xmin = 1,
    xmax = 3,
    annotation = "*",
    tip_length = 0.03,
    size = 0.5,
    textsize = 6) + 

  #changing the x orders
  #change the color pallete
  scale_y_continuous(limits = c(0, 0.85), breaks = seq(0, 0.8, 0.2), expand = c(0, 0)) +
  theme_classic() +
  labs(y = "Wing Extension Ratio") +
  Setting +
  theme(axis.text.x = element_blank())
  # theme(axis.title.y = element_text("Wing extension amount"))
gp3

ggsave("./outputs/wing_extension_amounts.png", gp3, width = 4, height = 4, dpi = 500)
ggsave("./outputs/wing_extension_amounts.svg", gp3, width = 4, height = 4, dpi = 500)


# パッケージの読み込み
library(effsize)

# すべてのペアの Cohen's d を計算
cohens_d_results <- pairwise.t.test(df_data$rate, df_data$group, p.adjust.method = "none")$p.value %>%
  as.data.frame() %>%
  rownames_to_column(var = "Comparison") %>%
  mutate(
    Cohens_d = apply(., 1, function(x) {
      groups <- unlist(strsplit(as.character(x[1]), "-"))
      effsize::cohen.d(
        df_data$rate[df_data$group == groups[1]],
        df_data$rate[df_data$group == groups[2]]
      )$estimate
    })
  )

print(cohens_d_results)
