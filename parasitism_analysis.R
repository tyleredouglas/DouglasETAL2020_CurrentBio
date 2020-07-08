library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)

setwd("C:/Users/Tyler/Desktop/Toxic Flies Paper/figures")
total_data <-read.csv('Prelim_expt2_log.csv')

#formatting data:

fly_sum <- total_data %>% 
  group_by(Treatment, Vial) %>% 
  summarize(fly_total = sum(Flies), wasp_total = sum(Wasps)) %>% 
  data.frame 

#convert to percentages

fly_sum$fly_total[fly_sum$Treatment=='b']<-(fly_sum$fly_total[fly_sum$Treatment=='b']/19)*100
fly_sum$fly_total[fly_sum$Treatment=='g']<-(fly_sum$fly_total[fly_sum$Treatment=='g']/19)*100
fly_sum$fly_total[fly_sum$Treatment=='e']<-(fly_sum$fly_total[fly_sum$Treatment=='e']/22)*100
fly_sum$fly_total[fly_sum$Treatment=='j']<-(fly_sum$fly_total[fly_sum$Treatment=='j']/22)*100

fly_sum$wasp_total[fly_sum$Treatment=='e'] <- (fly_sum$wasp_total[fly_sum$Treatment=='e']/19)*100
fly_sum$wasp_total[fly_sum$Treatment=='j'] <- (fly_sum$wasp_total[fly_sum$Treatment=='j']/22)*100

fly_sum$Treatment<-recode(fly_sum$Treatment, "b"="Control, No Wasps","g"="Nicotine, No Wasps","e"="Control, Wasps",
                          "j"="Nicotine, Wasps")

fly_stats <- fly_sum %>%
  group_by(Treatment) %>%
  summarise(n = n(), mean = mean(fly_total), stdev = sd(fly_total), SE = stdev/(sqrt(n))) %>%
  select(Treatment, mean, SE)

wasp_stats <- fly_sum %>%
  filter(Treatment == "Control, Wasps" | Treatment == "Nicotine, Wasps") %>%
  group_by(Treatment) %>%
  summarise(n = n(), mean = mean(wasp_total), stdev = sd(wasp_total), SE = stdev/(sqrt(n))) %>%
  select(Treatment, mean, SE)

compare_means(fly_total ~ Treatment, fly_sum, method = "wilcox.test", paired = FALSE)


long_format <- fly_sum %>% 
  pivot_longer(c('fly_total', 'wasp_total'), names_to = 'flywasp', values_to = 'alive')

my_comparisons <- list(c("Control, No Wasps", "Nicotine, No Wasps"), c("Control, Wasps", "Nicotine, Wasps"))
flies <- subset(long_format, flywasp == "fly_total")
wasps <- subset(long_format, flywasp == "wasp_total")


ggplot(data = long_format, aes(x = Treatment, y = alive)) +
  geom_boxplot(data = long_format, aes(fill = flywasp), position = position_dodge(preserve = 'single'), outlier.shape = NA, alpha = .6, width = .9) +
  geom_jitter(data = long_format, aes(fill = flywasp), color = 'black', shape = 21, size = 3.5, alpha = .8, position = position_jitterdodge()) +
  stat_compare_means(data = flies, comparisons = my_comparisons, label.x.npc = "left") +
  stat_compare_means(data = wasps, comparisons = my_comparisons, label.y = 60) +
  theme_pubr(x.text.angle = 60) +
  scale_x_discrete(expand = c(0.1, 0)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  xlab("Treatment") +
  ylab("Percent Survival") +
  ggtitle("Percent Survival of D. melanogaster and L. heterotoma Across Treatments") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "right") +
  scale_fill_manual(values = c('darkorchid2', 'green2'), labels = c("D. mel", "L. het"))




