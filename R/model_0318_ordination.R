# Model for ordination 03-18 ####
# Markus Bauer
# Citation: Markus Bauer & Harald Albrecht (2020) Basic and Applied Ecology 42, 15-26
# https://doi.org/10.1016/j.baae.2019.11.003



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(vegan)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Uni/2018_Projekt_9_Masterthesis/3_Aufnahmen_und_Ergebnisse/2020_monitoring_Garchinger_Heide/data/processed")

### Load data ###
sites <- read_csv2("data_processed_sites0318.csv", col_names = T, na = "na", col_types = 
                    cols(
                      .default = col_double(),
                      ID = col_factor(),
                      plot = col_factor(),
                      block = col_factor(),
                      dataset = col_factor(),
                      year = col_factor()
                    )        
)

sites <- sites %>%
    select(ID, plot, block, year)

species <- read_csv2("data_processed_species0318.csv", col_names = T, na = "na", col_types = 
                      cols(
                        .default = col_double(),
                        name = col_factor()
                      )        
)

###Exclude rare species (presence in less than 6 plots)
notrare <- species %>%
  pivot_longer(-name, "site","value") %>%
  mutate(presence = if_else(value > 0, 1, 0)) %>%
  group_by(name) %>%
  summarise(sum = sum(presence)) %>%
  mutate(notrare = if_else(sum > 5, 1, 0)) %>%
  filter(notrare == 1)
species <- semi_join(species, notrare)

setwd("Z:/Documents/0_Uni/2018_Projekt_9_Masterthesis/3_Aufnahmen_und_Ergebnisse/2020_monitoring_Garchinger_Heide/data/raw")
traits <- read_csv2("data_raw_traits.csv", col_names = T, na = "na", col_types = 
                     cols(
                       .default = col_double(),
                       name = col_factor(),
                       abb = col_factor(),
                       family = col_factor(),
                       rlg = col_factor(),
                       rlb = col_factor(),
                       target = col_factor()
                     )        
)
traits <- select(traits, name, abb, family)
traits <- semi_join(traits, species, by = "name") %>%
  select(name, family, abb) %>%
  mutate(family = if_else(family == "Poaceae" | family == "Cyperaceae" | family == "Juncaeae", 
                          "Graminoids", if_else(family == "Fabaceae", 
                                                "Legumes", "Forbs")))

species <- species %>%  
  pivot_longer(-name, "site", "value") %>%
  pivot_wider(site, name) %>%
  column_to_rownames("site")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 NMDS #####################################################################################

#### a ordination ----------------------------------------------------------------------------------------
(ordi <- metaMDS(species, try = 99, previous.best = T, na.rm = T))
stressplot(ordi) # stress: 0.xx

#### b environmental factors ----------------------------------------------------------------------------------------
(ef <- envfit(ordi ~ year, 
              data = sites, permu = 999, na.rm = T))
plot(ordi, type = "n")
plot(ef, add = T, p. = .05)
text(ordi, dis = "sites", cex = .7)
ordiellipse(ordi, sites$year, kind = "sd", draw = "lines", label = T)


### 2 PERMANOVA #####################################################################################

(disp.year <- betadisper(dist, sites$year))
permutest(disp.year) # similar dispersion -> PERMANOVA possible
(permanova.year <- adonis(species ~ year, data = sites, 
                          strata = sites$plot, permutations = 999, method = "bray"))
densityplot(permustats(permanova.year)) # significant
