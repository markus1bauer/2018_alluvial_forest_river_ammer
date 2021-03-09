# Prepare site data ####
# Markus Bauer



### Packages ###
library(tidyverse)
library(vegan)
library(FD) #dbFD()
library(naniar) #are_na()

### Start ###
#installr::updateR(browse_news = F, install_R = T, copy_packages = T, copy_Rprofile.site = T, keep_old_packages = T, update_packages = T, start_new_R = F, quit_R = T)
rm(list = ls())
setwd("Z:/Documents/0_Uni/2017_Projekt_8_Schnalzaue/3_Aufnahmen_und_Ergebnisse/2018_floodplain_Schnalz/data/raw")


### 1 Load data #####################################################################################

### a Sites -------------------------------------------------------------------------------------------
sites <- read_csv2("data_raw_sites.csv", col_names = T, col_types = 
                    cols(
                      .default = col_double(),
                      id = col_factor(),
                      treatment = col_factor(),
                      plotHeight = col_factor(),
                      bankType = col_factor(),
                      structureType = col_character(),
                      photoID = col_character(),
                      surveyDate = col_date()
                      )
                   ) %>%
  select(id, treatment, barrierDistance, treeCover, shrubCover, ldmc, height, seedmass) %>%
  filter(treatment != "infront_dam")

### b Species -------------------------------------------------------------------------------------------
species <- read_csv2("data_raw_species.csv", col_names = T, col_types = 
                    cols(
                      .default = col_double(),
                      name = col_factor(),
                      abb = col_factor(),
                      layer = col_factor()
                      )
                    ) %>%
  select(-abb, -(Extra1:Extra4)) %>%
  filter(layer == "h") %>%
  group_by(name) %>%
  mutate(sum = sum(c_across(IN1:AC6))) %>%
  mutate(presence = if_else(sum > 0, 1, 0)) %>%
  filter(presence == 1) %>%
  ungroup() %>%
  select(-sum, -presence, -layer)


### c Traits -------------------------------------------------------------------------------------------
traits <- read_csv2("data_raw_traits.csv", col_names = T, col_types = 
                      cols(
                        .default = col_double(),
                        name = col_factor(),
                        descriptor = col_character(),
                        nomenclature = col_factor(),
                        abb = col_factor(),
                        nameTraits = col_factor(),
                        layer = col_factor(),
                        lifeform = col_factor(),
                        lifeformEllenberg = col_factor(),
                        flood = col_factor(),
                        chwet = col_factor(),
                        legal = col_factor(),
                        rlg = col_factor(),
                        rlb = col_factor(),
                        neophyte = col_factor()
                      )
                   ) %>%
  filter(layer == "h")

traits <- left_join(species, traits, by = "name") %>%
  select(name, abb, l, f, n, flood, chwet, ldmc, height, seedmass, sociality)

#miss_var_summary(traits)
#vis_miss(traits, cluster = F)
#gg_miss_var(traits)
#gg_miss_case(traits, order_cases = F)
#gg_miss_upset(traits)


### 2 Create variables #####################################################################################

### a Dummies for confidence interval -------------------------------------------------------------------------------------------
sites$conf.low <- c(1:length(sites$id))
sites$conf.high <- c(1:length(sites$id))

### 2 Coverages #####################################################################################

### a Querco-Fagetea' coverage -------------------------------------------------------------------------------------------
data <- species %>%
  mutate(type = traits$target) %>%
  pivot_longer(names_to = "id", values_to = "n", cols = starts_with("X")) %>%
  group_by(id, type) %>%
  summarise(total = sum(n)) %>%
  filter(type == "yes") %>%
  select(-type) %>%
  ungroup() %>%
  mutate(targetCov = round(total, 3), .keep = "unused")
sites <- left_join(sites, data, by = "id")

### b Querco-Fagetea' coverage -------------------------------------------------------------------------------------------
### c Querco-Fagetea' coverage -------------------------------------------------------------------------------------------
### d Querco-Fagetea' coverage -------------------------------------------------------------------------------------------


### 3 Species richness #####################################################################################

specRich <- left_join(species, traits, by = "name") %>%
  select(starts_with("IN"), starts_with("AC"), name, flood, chwet)

### a total species richness -------------------------------------------------------------------------------------------
specRich_all <- specRich %>%
  pivot_longer(names_to = "id", values_to = "n", cols = starts_with("AC") | starts_with("IN")) %>%
  group_by(id) %>%
  mutate(n = if_else(n > 0, 1, 0)) %>%
  summarise(total = sum(n)) %>%
  group_by(id) %>%
  summarise(speciesRichness = sum(total)) %>%
  ungroup()

### b Ellenberg flood indicators (species richness) -------------------------------------------------------------------------------------------
specRich_flood <- specRich %>%
  pivot_longer(names_to = "id", values_to = "n", cols = starts_with("AC") | starts_with("IN")) %>%
  group_by(id, flood) %>%
  mutate(n = if_else(n > 0, 1, 0)) %>%
  summarise(total = sum(n)) %>%
  filter(flood == "yes") %>%
  group_by(id) %>%
  summarise(floodRichness = sum(total)) %>%
  ungroup()

### c Ellenberg changing wetness indicators (species richness) -------------------------------------------------------------------------------------------
specRich_chwet <- specRich %>%
  pivot_longer(names_to = "id", values_to = "n", cols = starts_with("IN") | starts_with("AC")) %>%
  group_by(id, chwet) %>%
  mutate(n = if_else(n > 0, 1, 0)) %>%
  summarise(total = sum(n)) %>%
  filter(chwet == "yes") %>%
  group_by(id) %>%
  summarise(chwetRichness = sum(total)) %>%
  ungroup()

### d implement in sites data set -------------------------------------------------------------------------------------------
sites <- left_join(sites, specRich_all, by = "id")
sites <- left_join(sites, specRich_flood, by = "id")
sites <- left_join(sites, specRich_chwet, by = "id")
rm(list=setdiff(ls(), c("sites", "species", "traits")))


### 6 CWM of Ellenberg #####################################################################################

### a N value -------------------------------------------------------------------------------------------
Ntraits <- traits %>%
  select(name, n) %>%
  filter(n > 0) 
Nspecies <- semi_join(species, Ntraits, by = "name") %>%
  pivot_longer(-name, "site", "value") %>%
  pivot_wider(site, name) %>%
  column_to_rownames("site")
Ntraits <- column_to_rownames(Ntraits, "name")
### Calculate CWM ###
Nweighted <- dbFD(Ntraits, Nspecies, w.abun = T,
                  calc.FRic = F, calc.FDiv = F, corr = "sqrt")

### b F value -------------------------------------------------------------------------------------------
Ftraits <- traits %>%
  select(name, f) %>%
  filter(f > 0) 
Fspecies <- semi_join(species, Ftraits, by = "name") %>%
  pivot_longer(-name, "site", "value") %>%
  pivot_wider(site, name) %>%
  column_to_rownames("site")
Ftraits <- column_to_rownames(Ftraits, "name")
### Calculate CWM ###
Fweighted <- dbFD(Ftraits, Fspecies, w.abun = T,
                  calc.FRic = F, calc.FDiv = F, corr = "sqrt")


### 8 CWM and FDis of functional plant traits #####################################################################################

### a All -------------------------------------------------------------------------------------------
Tspecies <- semi_join(species, traitsLHS, by = "name")
Ttraits <- semi_join(traitsLHS, Tspecies, by = "name")
Tspecies <- Tspecies %>%
  pivot_longer(-name, "site", "value") %>%
  pivot_wider(site, name) %>%
  column_to_rownames("site")
Ttraits <- column_to_rownames(Ttraits, "name")
log_Ttraits <- log(Ttraits)
TdiversityAbu <- dbFD(log_Ttraits, Tspecies, w.abun = T,
                      calc.FRic = F, calc.FDiv = F, corr = "cailliez")
sites$fdisAbuLHS <- TdiversityAbu$FDis

### b SLA -------------------------------------------------------------------------------------------
Tspecies <- semi_join(species, traitsSLA, by = "name")
Ttraits <- semi_join(traitsSLA, Tspecies, by = "name")
Tspecies <- Tspecies %>%
  pivot_longer(-name, "site", "value") %>%
  pivot_wider(site, name) %>%
  column_to_rownames("site")
Ttraits <- column_to_rownames(Ttraits, "name")
log_Ttraits <- log(Ttraits)
TdiversityAbu <- dbFD(log_Ttraits, Tspecies, w.abun = T, 
                      calc.FRic = F, calc.FDiv = F, corr = "sqrt");
sites$fdisAbuSla <- TdiversityAbu$FDis
sites$cwmAbuSla <- exp(as.numeric(as.character(TdiversityAbu$CWM$sla)))

### b Seed mass -------------------------------------------------------------------------------------------
Tspecies <- semi_join(species, traitsSM, by = "name")
Ttraits <- semi_join(traitsSM, Tspecies, by = "name")
Tspecies <- Tspecies %>%
  pivot_longer(-name, "site", "value") %>%
  pivot_wider(site, name) %>%
  column_to_rownames("site")
Ttraits <- column_to_rownames(Ttraits, "name")
log_Ttraits <- log(Ttraits)
TdiversityAbu <- dbFD(log_Ttraits, Tspecies, w.abun = T, 
                      calc.FRic = F, calc.FDiv = F, corr = "sqrt");
sites$fdisAbuSeedmass <- TdiversityAbu$FDis
sites$cwmAbuSeedmass <- exp(as.numeric(as.character(TdiversityAbu$CWM$seedmass)))

### c Canopy height -------------------------------------------------------------------------------------------
Tspecies <- semi_join(species, traitsH, by = "name")
Ttraits <- semi_join(traitsH, Tspecies, by = "name")
Tspecies <- Tspecies %>%
  pivot_longer(-name, "site", "value") %>%
  pivot_wider(site, name) %>%
  column_to_rownames("site")
Ttraits <- column_to_rownames(Ttraits, "name")
log_Ttraits <- log(Ttraits)
TdiversityAbu <- dbFD(log_Ttraits, Tspecies, w.abun = T, 
                      calc.FRic = F, calc.FDiv = F, corr = "sqrt");
sites$fdisAbuHeight <- TdiversityAbu$FDis
sites$cwmAbuHeight <- exp(as.numeric(as.character(TdiversityAbu$CWM$height)))
rm(TdiversityAbu, log_Ttraits, Ttraits, Tspecies, traitsLHS, traitsSLA, traitsSM, traitsH)

traits <- select(traits, -targetArrhenatherion, -table30, -table33, -table34, -nitrogenIndicator, -nitrogenIndicator2, leanIndicator)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save processed data ##############################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


setwd("Z:/Documents/0_Donaudeiche/3_Aufnahmen_und_Ergebnisse/2022_Danube_old_dikes/data/processed")
write_csv2(sites, "data_processed_sites.csv")
write_csv2(species, "data_processed_species.csv")
write_csv2(traits, "data_processed_traits.csv")
