# Show Figure functional dispersion (all traits combined) ####
# Markus Bauer
# Citation: Markus Bauer & Harald Albrecht (2020) Basic and Applied Ecology 42, 15-26
# https://doi.org/10.1016/j.baae.2019.11.003



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(ggbeeswarm)
library(lme4)
library(emmeans)
library(ggeffects)
library(cowplot)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Uni/2018_Projekt_9_Masterthesis/3_Aufnahmen_und_Ergebnisse/2020_monitoring_Garchinger_Heide/data/processed")

### Load data ###
sites849318 <- read_csv2("data_processed_sites849318.csv", col_names = T, na = "na", col_types = 
                          cols(
                            .default = col_double(),
                            ID = col_factor(),
                            plot = col_factor(),
                            block = col_factor(),
                            dataset = col_factor(),
                            year = col_factor()
                          )        
)

sites0318 <- read_csv2("data_processed_sites0318.csv", col_names = T, na = "na", col_types = 
                        cols(
                          .default = col_double(),
                          ID = col_factor(),
                          plot = col_factor(),
                          block = col_factor(),
                          dataset = col_factor(),
                          year = col_factor()
                        )        
)

(sites849318 <- select(sites849318, ID, plot, block, year, fdisAbuAll))
(sites0318 <- select(sites0318, ID, plot, block, year, fdisAbuAll))

#### Chosen model ###
m849318 <- lmer(log(fdisAbuAll) ~ year + (1|block/plot), sites849318, REML = F)
m0318 <- lmer(log(fdisAbuAll) ~ year + (1|block/plot), sites0318, REML = F)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plotten ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
themeMB <- function(){
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size = 10, color = "black"),
    axis.line.y = element_line(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(.5, 0, 0, 0, "cm")
  )
}

###1984-1993-2018 (FDis: overall)----------------------------------------------------------------------------------------------------------------------------
graph849318 <- ggplot(sites849318, aes(year, fdisAbuAll)) +
  geom_boxplot(colour = "black") +
  labs(x = "", y = "FDis (total)", size = 3) +
  scale_y_continuous(limits = c(0, 2.5), breaks = seq(0, 100, 0.5)) +
  themeMB()

###2003-2018 (FDis: overall)----------------------------------------------------------------------------------------------------------------------------
graph0318 <- ggplot(sites0318, aes(year, fdisAbuAll))+
  geom_boxplot(colour = "black")+
  labs(x = "", y = "", size = 3)+
  scale_y_continuous(limits = c(0, 2.5)) +
  themeMB() +
  theme(axis.line.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

###Multiplot----------------------------------------------------------------------------------------------------------------------
plot_grid(graph849318, graph0318, 
               rel_widths = c(1.28,1), ncol = 2, align = "h",
               labels = "AUTO", label_size = 8) +
  theme(plot.margin = unit(c(0, 0, -.4, 0), "cm"))
setwd("Z:/Documents/0_Uni/2018_Projekt_9_Masterthesis/3_Aufnahmen_und_Ergebnisse/2020_monitoring_Garchinger_Heide/outputs/figures")
ggsave("figure_6e_fdis_all_(800dpi_8x5cm).tiff", 
       dpi = 800, width = 8, height = 5, units = "cm")

