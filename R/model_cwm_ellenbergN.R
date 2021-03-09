# Model for Ellenberg Ellenberg N value ####
# Markus Bauer



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
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
  select(id, treatment, cwmAbuN) %>%
  rename(value = cwmAbuN)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
plot(value ~ treatment, sites)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
dotchart((sites$value), groups = factor(sites$treatment), main = "Cleveland dotplot")
boxplot(sites$value);#identify(rep(1, length(edata$rgr13)), edata$rgr13, labels = c(edata$n))
plot(table((sites$value)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(sites, aes(value)) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure
m1 <- lm(value ~ treatment, sites)
simulateResiduals(m1, plot = T)

#### b comparison -----------------------------------------------------------------------------------------

#### c model check -----------------------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m1, plot = T)
plotResiduals(main = "treatment", simulationOutput$scaledResiduals, sites$treatment)


## 3 Chosen model output ################################################################################

### Model output ---------------------------------------------------------------------------------------------
summary(m1)[1]
summary(m1)
car::Anova(m1, type = 2)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m1, revpairwise ~ treatment, type = "response"))
plot(emm, comparison = T)
