# Show map of the Schnalz floodplain ####
# Markus Bauer



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(tidyverse)
library(sf)
library(ggmap)
library(ggrepel)
library(RColorBrewer)
library(patchwork)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Uni/2017_Projekt_8_Schnalzaue/3_Aufnahmen_und_Ergebnisse/2018_floodplain_Schnalz/data/processed/shp_files")


### Load data ###
ger <- st_read("germany.shp")
sites <- st_read("sites.shp")
sites2 <- read_csv2("sites2.csv", col_names = T, col_types = 
                      cols(
                        id = col_factor(),
                        Floodplain = col_factor()
                        )
                    ) 
load("background_toner.rda")
load("background_terrain.rda")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ##############################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
themeMB <- function(){
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid = element_line(colour = NA),
    text  = element_text(size = 10, color = "black"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "none",
    plot.margin = margin(.5, 0, 0, 0, "cm")
  )
}


## 1 Preparation ##############################################################################

### a Map of project site -----------------------------------------------------------------------
(sitesGraph <- ggmap(background_terrain, 
                      base_layer = ggplot(sites2, aes(x = lon, y = lat, shape = Floodplain))) +
   geom_point(size = 2, color = "black") +
   geom_point(aes(x = 10.9595, y = 47.77385), pch = 15, size = 2, color = "black") +
   annotate("text", x = 10.9595, y = 47.7736, label = "Weir", size = 3) +
   coord_sf(crs = st_crs(4326)) +
   annotate("text", x = 10.9553, y = 47.7743, label = "Inactive floodplain", size = 3) +
   annotate("text", x = 10.9652, y = 47.77475, label = "Active floodplain", size = 3) +
   ggspatial::annotation_scale(location = "br", pad_y = unit(0.6, "cm"), pad_x = unit(0.7, "cm"),
                               width_hint = 0.4, height = unit(0.2, "cm")) +
   ggspatial::annotation_north_arrow(location = "br", pad_y = unit(1.1, "cm"), pad_x = unit(0.6, "cm"), 
                                     which_north = "true", style = ggspatial::north_arrow_fancy_orienteering(), height = unit(1, "cm"), width = unit(1, "cm")) +
   themeMB()
  )

### b Germany -----------------------------------------------------------------------
gerGraph <- ggplot() +
   geom_sf(data = ger, fill = "transparent", colour = "black") +
   geom_point(aes(x = 10.95948, y = 47.77405), size = 1, colour = "white") +
   themeMB() +
   theme(
     plot.background = element_blank()
   )

### c Inset -----------------------------------------------------------------------
sitesGraph + inset_element(gerGraph, .01, .65, .3, .99, on_top = T)



# 2 Save ##############################################################################

ggsave("figure_1_map_(300dpi_12x10cm).tiff", 
       dpi = 300, width = 12, height = 10, units = "cm",
       path = "Z:/Documents/0_Uni/2017_Projekt_8_Schnalzaue/3_Aufnahmen_und_Ergebnisse/2018_floodplain_Schnalz/outputs/figures")
