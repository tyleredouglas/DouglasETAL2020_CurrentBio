library(ggplot2)
library(tidyverse)
library(dplyr)
library(cowplot)
library(ggpubr)
library(nlme)
library(lme4)
library(MASS)

setwd('C:/Users/debbi/Desktop/Tyler Manuscript docs/data')
response_data <- read.csv('dose_response.csv', stringsAsFactors = TRUE)
response_data$percent_survival <- (response_data$total.adults/20)
response_data <-  separate(response_data, vial, c("strain", "replicate", "dose"), sep = "_")
str(response_data)
response_data$dose <- as.numeric(response_data$dose)
response_data$strain <- as.factor(response_data$strain)
response_data$replicate <- as.factor(response_data$replicate)
str(response_data)

#normalize proportion survival by max survival on control food for each strain
#max A4 control percent survival: 55
#max A3 control percent survival: 70
#max w118 control percent survival: 95

response_data$normalized_survival[response_data$strain == "A4"] <- (response_data$percent_survival[response_data$strain == "A4"]/.55)
response_data$normalized_survival[response_data$strain == "A3"] <- (response_data$percent_survival[response_data$strain == "A3"]/.70)
response_data$normalized_survival[response_data$strain == "w118"] <- (response_data$percent_survival[response_data$strain == "w118"]/.95)

data_summary <- response_data %>%
  group_by(strain, dose) %>%
  summarise(avg_survival = mean(percent_survival), avg_normalized = mean(normalized_survival), n = n(),
            sd_raw = sd(percent_survival), SE_raw = sd_raw/(sqrt(n)), 
            sd_norm = sd(normalized_survival), SE_norm = sd_norm/(sqrt(n)))

#generate glmm's for each strain based on normalized data, binomial family, replicates as random effect
w118_response <- glmer(normalized_survival ~ dose + (1|replicate), data = response_data[response_data$strain == "w118",], family = binomial())
summary(w118_response)

a4_response <- glmer(normalized_survival ~ dose + (1|replicate), data = response_data[response_data$strain == "A4",], family = binomial())
summary(a4_response)

a3_response <- glmer(normalized_survival ~ dose + (1|replicate), data = response_data[response_data$strain == "A3",], family = binomial())
summary(a3_response)

# define function to calculate ld50 
#this equation is taken from a forum where someone adapted the dose.p function
#from the MASS library, which is compatible with the glm object class but not with
#a glmm produced by the glmer library. I don't fully understand 
#the math underlying this, but the outputs it generates do make sense visually when
#compared to my ggplot curves

dose.p.glmm <-  function(obj, cf = 1:2, p = 0.5) {
  f <- family(obj)
  eta <- f$linkfun(p)
  b <- fixef(obj)[cf]
  x.p <- (eta - b[1L])/b[2L]
  names(x.p) <- paste("p = ", format(p), ":", sep = "")
  pd <- -cbind(1, x.p)/b[2L]
  SE <- sqrt(((pd %*% vcov(obj)[cf, cf]) * pd) %*% c(1, 1))
  res <- structure(x.p, SE = matrix(SE), p = p)
  class(res) <- "glm.dose"
  res
}

#calculate normalized ld50 for each strain based on respective glmm's generated above

dose.p.glmm (w118_response, cf=1:2, p=0.5)
dose.p.glmm(a4_response, cf=1:2, p=0.5)
dose.p.glmm(a3_response, cf=1:2, p=0.5)

#plot normalized ld50 w/geom_point of mean values at each dose w/error bars
pd <- position_dodge(0.1)

ggplot(data = data_summary, aes(x = dose, y = avg_normalized, color = strain)) +
  geom_point(position = pd, size = 5) +
  geom_jitter() +
  geom_errorbar(aes(ymin = avg_normalized - SE_norm, ymax = avg_normalized + SE_norm), position = pd) +
  geom_smooth(data = response_data, aes(x = dose, y = normalized_survival, color = strain),
              method = "glm", method.args = list(family = binomial(link = "logit")), se = FALSE) +
  geom_vline(xintercept = 0.74, colour = "red", linetype = "dotted", size = 1) +
  geom_vline(xintercept = 1.65, colour = "blue", linetype = "dotted", size = 1) +
  geom_vline(xintercept = 1.68, colour = "green", linetype = "dotted", size = 1) +
  theme_pubr(base_size = 12) +
  theme(axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 6)) +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 0.25)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .1)) +
  theme(legend.title = element_blank()) +
  ggtitle("Nicotine Dose Reponse Curve of A4, A3, and W118") +
  xlab("Nicotine Dose (mM)") +
  ylab("Proportion Survival to Adulthood")

ggsave('normalized_dose_response.pdf', dpi = 300, height = 10, width = 20)

#=========================SAME AS ABOVE BUT W/RAW DATA==========================

#glmm's of raw data
w118_raw <- glmer(percent_survival ~ dose + (1|replicate), data = response_data[response_data$strain == "w118",], family = binomial())
summary(w118_raw)

#calculated intercept for A4 not significant
A4_raw <- glmer(percent_survival ~ dose + (1|replicate), data = response_data[response_data$strain == "A4",], family = binomial())
summary(A4_raw)

#fails w/error: pwrssUpdate did not converge in (maxit) iterations. wrong family? gaussian?
A3_raw <- glmer(percent_survival ~ dose + (1|replicate), data = response_data[response_data$strain == "A3",], family = binomial(link = "logit"))
summary(A3_raw)

#cannot calculate ld50 based on raw data without successfully generated models

#plotting raw dose response data

ggplot(data = data_summary, aes(x = dose, y = avg_survival, color = strain)) +
  geom_point(position = pd, size = 5) +
  geom_jitter() +
  geom_errorbar(aes(ymin = avg_survival - SE_raw, ymax = avg_survival + SE_raw), position = pd) +
  geom_smooth(data = response_data, aes(x = dose, y = percent_survival, color = strain),
              method = "glm", method.args = list(family = binomial), se = FALSE) +
  theme_pubr(base_size = 12) +
  theme(axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 6)) +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 0.25)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .1)) +
  theme(legend.title = element_blank()) +
  ggtitle("Nicotine Dose Reponse Curve of A4, A3, and W118") +
  xlab("Nicotine Dose (mM)") +
  ylab("Proportion Survival to Adulthood")

ggsave('raw_dose_response.pdf', dpi = 300, height = 10, width = 20)

# are the dose responses significantly different between strains?

strain_model <- glmer(normalized_survival ~ dose*strain + (1|replicate), data = response_data, family = binomial())





  
  


