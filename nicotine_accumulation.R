library(dplyr)
library(ggplot2)
library(tidyverse)
library(FSA)
library(ggpubr)

setwd("C:/Users/Tyler/Desktop/Toxic Flies Paper/data/Hemolymph Conc/run 2")
total_data <- read.csv("data_final.csv")

trimmed_data <- total_data %>% 
  mutate(ng.vial = ng.fly*num.individuals) %>%
  filter(treatment != "control") %>%
  select(label, treatment, sample.type, stage, num.individuals, GC.MS.Area, ng.fly, ng.vial)

trimmed_data$stage <- factor(trimmed_data$stage, levels = c("L3", "A1", "A3"))
trimmed_data$stage <- recode(trimmed_data$stage, "A1" = "Day 1 Adult", "A3" = "Day 3 Adult")

mean_bystage <- trimmed_data %>%
  group_by(sample.type, treatment, stage) %>%
  summarise(vial.count = n(), flynum.treatment = sum(num.individuals), ng.treatment = sum(ng.vial), stage.mean = ng.treatment/flynum.treatment, stdev = sd(ng.vial), SE = stdev/(sqrt(vial.count))) %>%
  select(treatment, sample.type, stage, flynum.treatment, stage.mean, SE) 

my_comparisons <- list(c("L3", "D1 Adult"), c("L3", "D3 Adullt"), c("D1 Adult", "D3 Adult"))

ggplot(data = trimmed_data, aes(x = stage, y = ng.fly)) +
  geom_boxplot(aes(fill = treatment), color = 'black', alpha = .6, width = .5) +
  geom_jitter(aes(fill = treatment), shape = 21, size = 3, alpha = .8, width = .08, color = 'black') +
  stat_compare_means(comparisons = my_comparisons) +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(0, NA)) +
  xlab('Developmental Stage') +
  ylab("Ng Nicotine Per fly") +
  theme_pubr(base_size = 6) +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c('green2'))
  
ggsave('fig 2.pdf', width = 9, height = 6, units = 'cm', dpi = 300)
 
  
