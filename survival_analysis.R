library(ggplot2)
library(tidyverse)
library(dplyr)
library(cowplot)
library(ggpubr)
library(nlme)

setwd('C:/Users/Tyler/Desktop/Toxic Flies Paper/data/Nicotine Survival/run2')
data <- read.csv('nicotine_survival2.csv', stringsAsFactors = FALSE)
str(data)
data$vial <- recode(data$vial, '1'='V1','2'='V2','3'='V3','4'='V4','5'='V5','6'='V6','7'='V7','8'='V8','9'='V9','10'='V10')
data$treatment <- recode(data$treatment, "0.0 ul/ml" = "0.0 mM", "0.2 ul/ml" = "1.25 mM", "0.4 ul/ml" = "2.50 mM")

#-------------------------------- percent survival analysis -------------------------------

survival_data <- data %>%
  group_by(treatment, vial) %>%
  summarise(fly_total = sum(adults)) %>%
  select(treatment, vial, fly_total)

survival_data["percent_survival"] <- 0

survival_data$percent_survival[survival_data$treatment == '1.25 mM'] <- (survival_data$fly_total[survival_data$treatment == '1.25 mM']/30)*100
survival_data$percent_survival[survival_data$treatment == '2.50 mM'] <- (survival_data$fly_total[survival_data$treatment == '2.50 mM']/24)*100
survival_data$percent_survival[survival_data$treatment == '0.0 mM'] <- (survival_data$fly_total[survival_data$treatment == '0.0 mM']/30)*100

survival_summary <- survival_data %>%
  group_by(treatment) %>%
  summarise(avg_survival = mean(percent_survival), n = n(), stdev = sd(percent_survival), SE = stdev/(sqrt(n)))

#analysis

#data not normally distributed
shapiro.test(survival_data$percent_survival)

compare_means(percent_survival ~ treatment, survival_data, method = "kruskal.test")
compare_means(percent_survival ~ treatment, survival_data, method = "wilcox.test", paired = FALSE)
wilcox.test(survival_data$percent_survival[survival_data$treatment == "0.0 mM"], survival_data$percent_survival[survival_data$treatment == "1.25 mM"])

# boxplot of percent survival across treatments

ggplot(data = survival_data, mapping = aes(x = treatment, y = percent_survival, fill = treatment)) +
  geom_boxplot(color = "black", alpha = .8) +
  geom_jitter(aes(fill = treatment), color = 'black', shape = 21, size = 3.5, width = 0.2, alpha = .6) +
  ylab("Percent Survival") +
  xlab("Nicotine Concentration (mM)") +
  ggtitle("A4 Survival to Adulthood") +
  theme_pubr()+
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("darkorchid2", "green2", "deepskyblue"))
  
