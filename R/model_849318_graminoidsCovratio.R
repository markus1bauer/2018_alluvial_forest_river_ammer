# Model for graminoid's cover ratio ####
# Markus Bauer
# Citation: Markus Bauer & Harald Albrecht (2020) Basic and Applied Ecology 42, 15-26
# https://doi.org/10.1016/j.baae.2019.11.003



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(ggbeeswarm)
library(lmerTest)
library(DHARMa)
library(MuMIn)
library(car)
library(emmeans)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Uni/2018_Projekt_9_Masterthesis/3_Aufnahmen_und_Ergebnisse/Basic_Appl_Ecol/data/processed")

### Load data ###
sites <- read_csv2("data_processed_sites849318.csv", col_names = T, na = "na", col_types = 
                    cols(
                      .default = col_double(),
                      ID = col_factor(),
                      plot = col_factor(),
                      block = col_factor(),
                      dataset = col_factor(),
                      year = col_factor()
                    )        
)

(sites <- select(sites, ID, plot, block, year, graminoidCovratio))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
plot(graminoidCovratio ~ year, sites)
plot(graminoidCovratio ~ block, sites)
#2way (block:year):
ggplot(sites, aes(block, graminoidCovratio, color = year)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((sites$graminoidCovratio), groups = factor(sites$year), main = "Cleveland dotplot")
dotchart((sites$graminoidCovratio), groups = factor(sites$block), main = "Cleveland dotplot")
par(mfrow=c(1,1));
boxplot(sites$graminoidCovratio);#identify(rep(1, length(edata$rgr13)), edata$rgr13, labels = c(edata$graminoidCovratio))
par(mfrow = c(2,2));
plot(table((sites$graminoidCovratio)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(sites, aes(graminoidCovratio)) + geom_density()
ggplot(sites, aes(log(graminoidCovratio))) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure
m1 <- lmer(graminoidCovratio ~ year + (1|block/plot), sites, REML = F)
VarCorr(m1)
#1w-model
m2 <- lmer((graminoidCovratio) ~ year + (1|block/plot), sites, REML = F)
isSingular(m2)
simulateResiduals(m2, plot = T)

#### b comparison -----------------------------------------------------------------------------------------
rm(m1)

#### c model check -----------------------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m2, plot = T)
par(mfrow=c(2,2));
plotResiduals(main = "year", simulationOutput$scaledResiduals, sites$year)
plotResiduals(main = "block", simulationOutput$scaledResiduals, sites$block)


## 3 Chosen model output ################################################################################

### Model output ---------------------------------------------------------------------------------------------
m2 <- lmer((graminoidCovratio) ~ year + (1|block/plot), sites, REML = F)
MuMIn::r.squaredGLMM(m2) #R2m = 0.08, R2c = 0.68
VarCorr(m2)
sjPlot::plot_model(m2, type = "re", show.values = T)
car::Anova(m2, type = 3)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m2, revpairwise ~ year, type = "response"))
plot(emm, comparison = T)
contrast(emmeans(m2, ~ year, type = "response"), "trt.vs.ctrl", ref = 3)
