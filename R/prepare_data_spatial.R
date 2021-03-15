# Prepare spatial data ####
# Markus Bauer


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(tidyverse)
library(sf)
library(ggmap)

### Start ###
#installr::updateR(browse_news = F, install_R = T, copy_packages = T, copy_Rprofile.site = T, keep_old_packages = T, update_packages = T, start_new_R = F, quit_R = T)
rm(list = ls())
setwd("Z:/Documents/0_Uni/2017_Projekt_8_Schnalzaue/3_Aufnahmen_und_Ergebnisse/2018_floodplain_Schnalz/data/raw")
register_google(key = "AIzaSyB5nQU_dgB_kPsQkk-_cq7pA0g1-2qka4E")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Load data ##########################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## 1 Base map #################################################################################################

ger <- raster::getData('GADM', country = 'DEU', level = 0)
ger <- st_as_sf(ger)
ger <- st_set_crs(ger, 4326)

background_toner <- get_map(
  location = c(lon = 10.95948, lat = 47.77405),
  zoom = 15, 
  scale = 1,
  maptype = "toner",
  source = "stamen"
)
ggmap(background_toner)

background_terrain <- get_map(
  location = c(left = 10.950, bottom = 47.769, right = 10.970, top = 47.778),
  zoom = 15, 
  scale = 1,
  maptype = "terrain",
  source = "stamen"
)
ggmap(background_terrain)


## 2 Sites #################################################################################################

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
                     )) %>%
  select(id, easting, northing, treatment) %>%
  rename(Floodplain = "treatment") %>%
  mutate(Floodplain = fct_recode(Floodplain, "Inactive" = "behind_dam", "Active" = "no_dam")) %>%
  filter(Floodplain != "infront_dam") %>%
  st_as_sf(coords = c("easting", "northing"), crs = 31468) %>%
  st_transform(crs = 4326)
coord <- as_tibble(st_coordinates(sites))
sites2 <- st_drop_geometry(sites)
sites2$lon <- coord$X
sites2$lat <- coord$Y
sites2 <- as_tibble(sites2)
rm(coord)

### Digitize the weir ####
require(uavRmp)
require(mapview)
require(sp)
sites3 <- as_Spatial(sites)
vecDraw(
  zoom = 16, 
  maplayer = "OpenStreetMap"
  )



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save ##############################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


save(background_toner, file = "Z:/Documents/0_Uni/2017_Projekt_8_Schnalzaue/3_Aufnahmen_und_Ergebnisse/2018_floodplain_Schnalz/data/processed/shp_files/background_toner.rda")
save(background_terrain, file = "Z:/Documents/0_Uni/2017_Projekt_8_Schnalzaue/3_Aufnahmen_und_Ergebnisse/2018_floodplain_Schnalz/data/processed/shp_files/background_terrain.rda")
st_write(ger, layer = "germany.shp", driver = "ESRI Shapefile",
         dsn = "Z:/Documents/0_Uni/2017_Projekt_8_Schnalzaue/3_Aufnahmen_und_Ergebnisse/2018_floodplain_Schnalz/data/processed/shp_files")
st_write(sites, layer = "sites.shp", driver = "ESRI Shapefile",
         dsn = "Z:/Documents/0_Uni/2017_Projekt_8_Schnalzaue/3_Aufnahmen_und_Ergebnisse/2018_floodplain_Schnalz/data/processed/shp_files")
setwd("Z:/Documents/0_Uni/2017_Projekt_8_Schnalzaue/3_Aufnahmen_und_Ergebnisse/2018_floodplain_Schnalz/data/processed/shp_files")
write_csv2(sites2, file = "sites2.csv")
