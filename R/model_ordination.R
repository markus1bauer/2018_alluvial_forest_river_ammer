# Model for ordination ####
# Markus Bauer



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation #########################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)
library(vegan)

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
  select(id, treatment, treeCover, shrubCover, barrierDistance, herbHeight)

species <- read_csv("data_processed_species.csv", col_names = TRUE, na = "na",
                     col_types =
                       cols(
                         .default = col_double(),
                         name = col_factor()
                       )) %>%
  pivot_longer(-name, "site", "value") %>%
  pivot_wider(site) %>%
  column_to_rownames("site")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ##########################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 NMDS ##############################################################

#### a ordination -------------------------------------------------------
(ordi <- metaMDS(species, try = 99, previous.best = TRUE, na.rm = TRUE))
stressplot(ordi) # stress: 0.xx

#### b environmental factors --------------------------------------------
(ef <- envfit(ordi ~ treeCover + treatment + shrubCover +
                barrierDistance + herbHeight,
              data = sites, permu = 999, na.rm = TRUE))
plot(ordi, type = "n")
plot(ef, add = TRUE, p. = .15)
text(ordi, dis = "sites", cex = .7)
ordiellipse(ordi, sites$treatment, kind = "sd", draw = "lines", label = TRUE)


### 2 PERMANOVA #########################################################

(disp <- betadisper(vegdist(species), sites$treatment))
permutest(disp) # similar dispersion -> PERMANOVA possible
(permanova <- adonis(species ~ treatment,
                     data = sites,
                     strata = sites$id,
                     permutations = 999,
                     method = "bray"))
densityplot(permustats(permanova))
