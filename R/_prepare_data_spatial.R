# Prepare spatial data ####
# Markus Bauer


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)
library(sf)
library(ggmap)

### Start ###
rm(list = ls())
setwd(here("data", "raw"))
register_google(key = "AIzaSyB5nQU_dgB_kPsQkk-_cq7pA0g1-2qka4E")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Load data ##########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## 1 Base map ##########################################################

ger <- raster::getData("GADM", country = "DEU", level = 0)
ger <- ger %>%
  st_as_sf() %>%
  st_set_crs(4326)

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


## 2 Sites #############################################################

sites <- read_csv("data_raw_sites.csv", col_names = TRUE,
                   col_types =
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
  mutate(Floodplain = fct_recode(Floodplain,
                                 "Inactive" = "behind_dam",
                                 "Active" = "no_dam")) %>%
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



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save ###############################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


save(background_toner,
     file = here("data", "processed", "spatial", "background_toner.rda"))
save(background_terrain,
     file = here("data", "processed", "spatial", "background_terrain.rda"))
st_write(ger, layer = "germany.shp", driver = "ESRI Shapefile",
         dsn = here("data", "processed", "spatial"))
st_write(sites, layer = "sites.shp", driver = "ESRI Shapefile",
         dsn = here("data", "processed", "spatial"))
write_csv(sites2, file = here("data", "processed", "spatial", "sites2.csv"))
