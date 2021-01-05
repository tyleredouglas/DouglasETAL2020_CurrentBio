library(ggplot2)
library(tidyverse)
library(dplyr)
library(cowplot)
library(ggpubr)
library(lme4)
library(multcomp)

#import data
setwd('~/Desktop/Tyler Manuscript Docs')
data <- read.csv('dose_response_daily.csv', stringsAsFactors = FALSE)
data <-  separate(data, vial, c("strain", "replicate", "dose"), sep = "_")
data[is.na(data)] <- 0
str(data)

#create a dataframe with a row for each individual adult that emerges on a given day from a given vial
individual_adults <- tibble(strain = character(), dose = character(), replicate = character(), day = integer())

for (i in 1:nrow(data)) {
  row <- data[i,]
  num_adults <- as.integer(row[5])
  if (num_adults > 0) {
    for (i in 1:num_adults) {
      individual_adults <- add_row(individual_adults, strain = as.character(row[1]), replicate = as.character(row[2]), dose = as.character(row[3]), day = as.integer(row[4]))
    }
  }
}

#format individual_adults to make dose, replicate and strain factors. should dose be numeric?
individual_adults <- as.data.frame(lapply(individual_adults, unlist))
individual_adults$dose <- as.factor(individual_adults$dose)
individual_adults$replicate <- as.factor(individual_adults$replicate)
individual_adults$strain <- as.factor(individual_adults$strain)
individual_adults <- droplevels(individual_adults)

#summary of individual_adults
adult_means <- individual_adults %>%
  group_by(strain, dose) %>%
  summarise(mean_day = mean(day), n = n(), stdev = sd(day), SE = stdev/(sqrt(n)))

#normality failed
shapiro.test(individual_adults$day)

#mixed effect generalized linear model with replicate as random effect

w118_dev_model <- glmer(day ~ dose + (1 | replicate), data = individual_adults[individual_adults$strain == "w118",], family = poisson())
summary(w118_dev_model)
summary(glht(w118_dev_model, mcp(dose="Tukey")))

a4_dev_model <- glmer(day ~ dose + (1 | replicate), data = individual_adults[individual_adults$strain == "A4",], family = poisson())
summary(a4_dev_model)
summary(glht(a4_dev_model, mcp(dose="Tukey")))

a3_dev_model <- glmer(day ~ dose + (1 | replicate), data = individual_adults[individual_adults$strain == "A3",], family = poisson())
summary(a3_dev_model)
summary(glht(a3_dev_model, mcp(dose="Tukey")))

#does either fly strain or dose impact developmental rate? not completely sure that i've
#modeled this correctly

strain_dose_day <- glmer(day ~ dose*strain + (1 |replicate), data = individual_adults, family = poisson())
summary(strain_dose_day)

#plotting a4 data at 0, 1.25, 1.5mM
a4_for_fig <- subset(individual_adults, subset = (dose == "0.0"|dose == "1.25"|dose == "2.5"))
a4_for_fig <- subset(a4_for_fig, subset = (strain == "A4"))

ggplot(data = a4_for_fig, aes(x = dose, y = day)) +
  geom_boxplot(position = position_dodge(preserve = 'single'), outlier.shape = NA, alpha = .6, width = .9) +
  geom_jitter(color = 'black', shape = 21, size = 3.5, alpha = .8) +
  theme_pubr() +
  scale_x_discrete(expand = c(0.1, 0)) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 1)) +
  xlab("Nicotine Dose (mM)") +
  ylab("Days to Eclosion") +
  ggtitle("A4 Days to Eclosion") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

ggplot(data = long_format, aes(x = Treatment, y = alive)) +
  geom_boxplot(data = long_format, aes(fill = flywasp), position = position_dodge(preserve = 'single'), outlier.shape = NA, alpha = .6, width = .9) +
  geom_jitter(data = long_format, aes(fill = flywasp), color = 'black', shape = 21, size = 3.5, alpha = .8, position = position_jitterdodge()) +
  stat_compare_means(data = flies, comparisons = my_comparisons, label.x.npc = "left") +
  stat_compare_means(data = wasps, comparisons = my_comparisons, label.y = 60) +
  theme_pubr(x.text.angle = 60) +
  scale_x_discrete(expand = c(0.1, 0)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  xlab("Nicotine Dose (mM)") +
  ylab("Days to Eclosion") +
  ggtitle("A4 Days to Eclosion") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "right") +
  scale_fill_manual(values = c('darkorchid2', 'green2'), labels = c("D. mel", "L. het"))

