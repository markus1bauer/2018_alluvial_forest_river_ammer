# Model for functional dispersion all ####
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
setwd("Z:/Documents/0_Uni/2018_Projekt_9_Masterthesis/3_Aufnahmen_und_Ergebnisse/2020_monitoring_Garchinger_Heide/data/processed")

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

(sites <- select(sites, ID, plot, block, year, fdisAbuAll))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
plot(fdisAbuAll ~ year, sites)
plot(fdisAbuAll ~ block, sites)
#2way (block:year):
ggplot(sites, aes(block, fdisAbuAll, color = year)) + geom_boxplot() + geom_quasirandom(dodge.width = .7, groupOnX = T)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((sites$fdisAbuAll), groups = factor(sites$year), main = "Cleveland dotplot")
dotchart((sites$fdisAbuAll), groups = factor(sites$block), main = "Cleveland dotplot")
par(mfrow=c(1,1));
boxplot(sites$fdisAbuAll);#identify(rep(1, length(edata$rgr13)), edata$rgr13, labels = c(edata$fdisAbuAll))
plot(table((sites$fdisAbuAll)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(sites, aes(fdisAbuAll)) + geom_density()
ggplot(sites, aes(log(fdisAbuAll))) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure
m1 <- lmer(fdisAbuAll ~ year + (1|block/plot), sites, REML = F)
VarCorr(m1)
#1w-model
m2 <- lmer(log(fdisAbuAll) ~ year + (1|block/plot), sites, REML = F)
isSingular(m2)
simulateResiduals(m2, plot = T)

#### b comparison -----------------------------------------------------------------------------------------
rm(m1)

#### c model check -----------------------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m2, plot = T)
par(mfrow=c(2,2));
plotResiduals(main = "year", simulationOutput$scaledResiduals,sites$year)
plotResiduals(main = "block", simulationOutput$scaledResiduals, sites$block)


## 3 Chosen model output ################################################################################

### Model output ---------------------------------------------------------------------------------------------
m2 <- lmer(log(fdisAbuAll) ~ year + (1|block/plot), sites, REML = F)
MuMIn::r.squaredGLMM(m2) #R2m = 0.10, R2c = 0.10
VarCorr(m2)
sjPlot::plot_model(m2, type = "re", show.values = T)
car::Anova(m2, type = 3)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m2, revpairwise ~ year, type = "response"))
plot(emm, comparison = T)
contrast(emmeans(m2, ~ year, type = "response"), "trt.vs.ctrl", ref = 3)



## FDis: Individually ###########################################################################################################################

###1984-1993-2018 (FDis: Individually)------------------------------------------------------------------------------------------------------------
#edataT <- edataT[-c(),]
par(mfrow=c(2,2));dotchart(fdataT$fd, groups=factor(fdataT$year), ylab="year",xlab="FDis",main="Cleveland dotplot");dotchart(fdataT$fd, groups=factor(fdataT$block), ylab="block",xlab="FDis",main="Cleveland dotplot");dotchart(fdataT$fd, groups=factor(fdataT$type), ylab="type",xlab="FDis",main="Cleveland dotplot")
plot(fdataT$fd ~ fdataT$year);plot(fdataT$fd ~ fdataT$type);plot(fdataT$fd ~ fdataT$block);interaction.plot(fdataT$year,fdataT$type,fdataT$fd,mean)
by(fdataT$fd,list(fdataT$year,fdataT$type),stat.desc)
#mean(edataT$fd) + qt( c(0.025, 0.975), length(edataT$fd) - 1) * (sd(edataT$fd) / sqrt(length(edataT$fd))) #CI95
####Model
m1 <- lmer(fd ~ year * type + (1|block/plot), fdataT)
anova(m1, test="F", type=3) ##year: F = 3.3, p = 0.039; type: F=211.5, p<0.001
r.squaredGLMM(m1) # 0.53, 0.58
####Model validation
m <- m1;simulationOutput <- simulateResiduals(fittedModel = m);plot(simulationOutput)
ES <- simulationOutput$scaledResiduals;par(mfrow=c(2,2));plot(ES ~ fdataT$year,ylab="Standardized residuals");abline(h=0.5,lty=2);plot(ES ~ fdataT$type,ylab="Standardized residuals");abline(h=0.5,lty=2);plot(ES ~ fdataT$block,ylab="Standardized residuals");abline(h=0.5,lty=2);plot(ES ~ fdataT$plot,ylab="Standardized residuals");abline(h=0.5,lty=2)
plot(cooks.distance(m),type = "h",ylim=c(0,2.5));abline(h=c(0,1), lty=2);which(cooks.distance(m)>1);largeResid <- which(ES > 1 | ES < -1);largeResid;edataT[largeResid,]
###2003-2018 (FDis: Individually)----------------------------------------------------------------------------------------------------------------------------
#edataT <- edataT[-c(),]
par(mfrow=c(2,2));dotchart(fdataB$fd, groups=factor(fdataB$year), ylab="year",xlab="FDis",main="Cleveland dotplot");dotchart(fdataB$fd, groups=factor(fdataB$block), ylab="block",xlab="FDis",main="Cleveland dotplot");dotchart(fdataB$fd, groups=factor(fdataB$type), ylab="type",xlab="FDis",main="Cleveland dotplot")
par(mfrow=c(2,2));plot(fdataB$fd ~ fdataB$year);plot(fdataB$fd ~ fdataB$type);plot(fdataB$fd ~ fdataB$block);interaction.plot(fdataB$year,fdataB$type,fdataB$fd,mean)
by(fdataB$fd,list(fdataB$year,fdataB$type),stat.desc)
#mean(edataT$fd) + qt( c(0.025, 0.975), length(edataT$fd) - 1) * (sd(edataT$fd) / sqrt(length(edataT$fd))) #CI95
####Model
m1 <- lmer(fd ~ year * type + (1|block/plot), fdataB)
anova(m1, test="F", type=3) ##year: F = 2.0, p = 0.163; type: F=159.7, p<0.001
summary(glht(m1,linfct=mcp(type="Tukey"))) # SLA-SM-H : a-a-b
r.squaredGLMM(m1) # 0.53, 0.59
####Model validation
m <- m1;simulationOutput <- simulateResiduals(fittedModel = m);plot(simulationOutput)
ES <- simulationOutput$scaledResiduals;par(mfrow=c(2,2));plot(ES ~ fdataB$year,ylab="Standardized residuals");abline(h=0.5,lty=2);plot(ES ~ fdataB$type,ylab="Standardized residuals");abline(h=0.5,lty=2);plot(ES ~ fdataB$block,ylab="Standardized residuals");abline(h=0.5,lty=2);plot(ES ~ fdataB$plot,ylab="Standardized residuals");abline(h=0.5,lty=2)
plot(cooks.distance(m),type = "h",ylim=c(0,2.5));abline(h=c(0,1), lty=2);which(cooks.distance(m)>1);largeResid <- which(ES > 1 | ES < -1);largeResid;edataT[largeResid,]


