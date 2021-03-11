# Model for ordination ####
# Markus Bauer



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(tidyverse)
library(vegan)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Uni/2017_Projekt_8_Schnalzaue/3_Aufnahmen_und_Ergebnisse/2018_floodplain_Schnalz/data/processed")

### Load data ###
sites <- read_csv2("data_processed_sites.csv", col_names = T, col_types = 
                     cols(
                       .default = col_double(),
                       id = col_factor(),
                       treatment = col_factor()
                     )) %>% 
  select(id, treatment, treeCover, shrubCover, barrierDistance, herbHeight)

species <- read_csv2("data_processed_species.csv", col_names = T, na = "na", col_types = 
                       cols(
                         .default = col_double(),
                         name = col_factor(),
                         abb = col_factor()
                       )) %>%
  select(-name) %>%  
  pivot_longer(-abb, "site", "value") %>%
  pivot_wider(site, abb) %>%
  column_to_rownames("site")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 NMDS #####################################################################################

#### a ordination ----------------------------------------------------------------------------------------
(ordi <- metaMDS(species, try = 99, previous.best = T, na.rm = T))
stressplot(ordi) # stress: 0.xx

#### b environmental factors ----------------------------------------------------------------------------------------
(ef <- envfit(ordi ~ treeCover + treatment + shrubCover + barrierDistance + herbHeight, 
              data = sites, permu = 999, na.rm = T))
plot(ordi, type = "n")
plot(ef, add = T, p. = .15)
text(ordi, dis = "sites", cex = .7)
ordiellipse(ordi, sites$treatment, kind = "sd", draw = "lines", label = T)


### 2 PERMANOVA #####################################################################################

(disp <- betadisper(vegdist(species), sites$treatment))
permutest(disp) # similar dispersion -> PERMANOVA possible
(permanova <- adonis(species ~ treatment, 
                     data = sites, 
                     strata = sites$id, 
                     permutations = 999,
                     method = "bray"))
densityplot(permustats(permanova)) 

