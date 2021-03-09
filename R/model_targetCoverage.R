# Model for graminoid's cover ratio ####
# Markus Bauer



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(tidyverse)
library(ggbeeswarm)
library(DHARMa)
library(car)
library(emmeans)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Uni/2017_Projekt_8_Schnalzaue/3_Aufnahmen_und_Ergebnisse/2018_floodplain_Schnalz/data/processed")

### Load data ###
sites <- read_csv2("data_processed_sites.csv", col_names = T, col_types = 
                     cols(
                       .default = col_double(),
                       id = col_factor(),
                       treatment = col_factor()
                     )
                   ) %>% 
  select(id, treatment, targetClass, nontargetClass, targetOrder, nontargetOrder, targetAlliance, nontargetAlliance, targetAssociation, nontargetAssociation) %>%
  pivot_longer(c(targetClass, nontargetClass, targetOrder, nontargetOrder, targetAlliance, nontargetAlliance, targetAssociation, nontargetAssociation), names_to = "type", values_to = "value") %>%
  separate(type, c("target", "type"), sep = "target") %>%
  mutate(target = as_factor(paste0(target, "target"))) %>%
  mutate(type = as_factor(str_to_lower(type)))
  


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
plot(value ~ treatment, sites)
plot(value ~ target, sites)
plot(value ~ type, sites)
#2way
ggplot(sites, aes(target, value, color = treatment)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T)
ggplot(sites, aes(type, value, color = treatment)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T)
ggplot(sites, aes(type, value, color = target)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T)
#3way
ggplot(sites, aes(treatment, value, color = target)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T) + facet_grid(~type)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
dotchart((sites$value), groups = factor(sites$treatment), main = "Cleveland dotplot")
boxplot(sites$value);#identify(rep(1, length(edata$rgr13)), edata$rgr13, labels = c(edata$n))
plot(table((sites$value)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(sites, aes(value)) + geom_density()
ggplot(sites, aes(log(value + 1))) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure
m1 <- lm(log(value + 1) ~ treatment * target * type, sites)
simulateResiduals(m1, plot = T) #not good
m2 <- lm(log(value + 1) ~ (treatment + target + type)^2, sites)
simulateResiduals(m2, plot = T) #not good
m3 <- lm(log(value + 1) ~ treatment + target + type + treatment:type, sites)
simulateResiduals(m3, plot = T) #not good

#--> no model is useable
