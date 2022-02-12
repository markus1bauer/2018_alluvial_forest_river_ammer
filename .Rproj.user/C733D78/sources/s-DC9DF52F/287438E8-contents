# Model for graminoid's cover ratio ####
# Markus Bauer



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation #########################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(here)
library(tidyverse)
library(DHARMa)
library(car)
library(emmeans)

### Start ###
rm(list = ls())
setwd(here("data/processed"))
### Load data ###
sites <- read_csv2(here("data_processed_sites.csv"), col_names = TRUE, col_types = 
                     cols(
                       .default = col_double(),
                       id = col_factor(),
                       treatment = col_factor()
                     )) %>% 
  select(id, treatment, chwetRichness) %>%
  rename(value = chwetRichness)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ##########################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration ##################################################

#### a Graphs -----------------------------------------------------------
plot(value ~ treatment, sites)

##### b Outliers, zero-inflation, transformations? ----------------------
dotchart((sites$value), groups = factor(sites$treatment), main = "Cleveland dotplot")
boxplot(sites$value);#identify(rep(1, length(edata$rgr13)), edata$rgr13, labels = c(edata$n))
plot(table((sites$value)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(sites, aes(value)) + geom_density()


## 2 Model building #####################################################

#### a models -----------------------------------------------------------
#random structure
m1 <- lm(value ~ treatment, sites)
simulateResiduals(m1, plot = TRUE)

#### b comparison -------------------------------------------------------

#### c model check ------------------------------------------------------
simulationOutput <- simulateResiduals(m1, plot = TRUE)
plotResiduals(main = "treatment", simulationOutput$scaledResiduals, sites$treatment)


## 3 Chosen model output ################################################

### Model output --------------------------------------------------------
summary(m1)[1]
summary(m1)
car::Anova(m1, type = 2)

### Effect sizes --------------------------------------------------------
(emm <- emmeans(m1, revpairwise ~ treatment, type = "response"))
plot(emm, comparison = TRUE)
