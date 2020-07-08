library(ggplot2)
library(tidyverse)
library(dplyr)
library(cowplot)
library(ggpubr)
library(lme4)
library(multcomp)

#import data
setwd('C:/Users/Tyler/Desktop/Toxic Flies Paper/data/Nicotine Survival/run2')
data <- read.csv('nicotine_survival2.csv', stringsAsFactors = FALSE)
str(data)
data$vial <- recode(data$vial, '1'='V1','2'='V2','3'='V3','4'='V4','5'='V5','6'='V6','7'='V7','8'='V8','9'='V9','10'='V10')
data$treatment <- recode(data$treatment, "0.0 ul/ml" = "0.0 mM", "0.1 ul/ml" = "0.62 mM", "0.2 ul/ml" = "1.25 mM", "0.4 ul/ml" = "2.50 mM")

#create a dataframe with a row for each individual adult that emerges on a given day from a given vial
individual_adults <- tibble(treatment=character(), vial=character(),day=integer())

for (i in 1:nrow(data)) {
  row <- data[i,]
  num_adults <- as.integer(row[5])
  if (num_adults > 0) {
    for (i in 1:num_adults) {
      individual_adults <- add_row(individual_adults, treatment = row[1], vial = row[2], day = row[3])
    }
  }
}

#format individual_adults to remove 0.62 mM treatment 
individual_adults <- as.data.frame(lapply(individual_adults, unlist))
individual_adults$treatment <- as.factor(individual_adults$treatment)

individual_adults$treatment2 <- individual_adults$treatment
individual_adults <- individual_adults %>% unite('vial', vial, treatment2, sep = '-', remove = TRUE)
individual_adults <- individual_adults %>% filter(treatment != '0.62 mM')
individual_adults <- droplevels(individual_adults)

#summary of individual_adults
adult_means <- individual_adults %>%
  group_by(treatment) %>%
  summarise(mean_day = mean(day), n = n(), stdev = sd(day), SE = stdev/(sqrt(n)))

#normality failed
shapiro.test(individual_adults$day)

#mixed effect linear model

dev_model <- glmer(day ~ treatment + (1|vial), data = individual_adults,family = poisson())
summary(dev_model)
summary(glht(dev_model, mcp(treatment="Tukey")))

