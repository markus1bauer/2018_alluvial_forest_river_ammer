# Model for functional dispersion all ####
# Markus Bauer



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation #########################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)
library(DHARMa)
library(car)
library(emmeans)

### Start ###
rm(list = ls())
setwd(here("data", "processed"))

### Load data ###
sites <- read_csv("data_processed_sites.csv", col_names = TRUE,
                   col_types =
                     cols(
                       .default = col_double(),
                       id = col_factor(),
                       treatment = col_factor()
                     )) %>%
  select(id, treatment, fdisAbuLdmc, fdisAbuHeight, fdisAbuSeedmass) %>%
  pivot_longer(c(fdisAbuLdmc, fdisAbuHeight, fdisAbuSeedmass),
               names_to = "type", values_to = "value") %>%
  mutate(type = str_replace(type, "fdisAbu", "")) %>%
  mutate(type = as_factor(str_to_lower(type)))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ##########################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration ##################################################

#### a Graphs -----------------------------------------------------------
#simple effects:
plot(value ~ treatment, sites)
plot(value ~ type, sites)
#2way
ggplot(sites, aes(type, value, color = treatment)) +
  geom_boxplot() +
  geom_quasirandom(dodge.width = .7, groupOnX = TRUE)

##### b Outliers, zero-inflation, transformations? ----------------------
dotchart((sites$value), groups = factor(sites$treatment),
         main = "Cleveland dotplot")
dotchart((sites$value), groups = factor(sites$type),
         main = "Cleveland dotplot")
par(mfrow = c(1, 1))
boxplot(sites$value)
plot(table((sites$value)), type = "h",
     xlab = "Observed values", ylab = "Frequency")
ggplot(sites, aes(value)) + geom_density()
ggplot(sites, aes(log(value))) + geom_density()


## 2 Model building #####################################################

#### a models -----------------------------------------------------------
#random structure
m1 <- lm(value ~ treatment * type, sites)
simulateResiduals(m1, plot = TRUE)

#### b comparison -------------------------------------------------------

#### c model check ------------------------------------------------------
simulation_output <- simulateResiduals(m1, plot = TRUE)
plotResiduals(main = "treatment",
              simulation_output$scaledResiduals, sites$treatment)
plotResiduals(main = "type", simulation_output$scaledResiduals, sites$type)


## 3 Chosen model output ################################################

### Model output --------------------------------------------------------
summary(m1)[1]
summary(m1)
(table <- car::Anova(m1, type = 2))
tidytable <- broom::tidy(table)

### Effect sizes --------------------------------------------------------
(emm <- emmeans(m1, revpairwise ~ treatment, type = "response"))
plot(emm, comparison = TRUE)

### Save ###
write.csv(tidytable, here("outputs", "statistics", "table_anova_fdis_single.csv"))