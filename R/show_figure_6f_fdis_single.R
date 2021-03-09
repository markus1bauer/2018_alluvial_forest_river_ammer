# Show Figure functional dispersion (all traits singularily) ####
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

(sites849318 <- sites849318 %>% 
    select(ID, plot, block, year, conf.low, conf.high, fdisAbuSla, fdisAbuSeedmass, fdisAbuHeight) %>%
    pivot_longer(c(fdisAbuSla, fdisAbuSeedmass, fdisAbuHeight), names_to = "type", values_to = "n")
)
(sites0318 <- sites0318 %>% 
    select(ID, plot, block, year, conf.low, conf.high, fdisAbuSla, fdisAbuSeedmass, fdisAbuHeight) %>%
    pivot_longer(c(fdisAbuSla, fdisAbuSeedmass, fdisAbuHeight), names_to = "type", values_to = "n")
)
sites849318$type <- factor(sites849318$type)
sites0318$type <- factor(sites0318$type)

sites849318$type <- fct_relevel(sites849318$type, c("fdisAbuSla","fdisAbuSeedmass","fdisAbuHeight"))
sites0318$type <- fct_relevel(sites0318$type, c("fdisAbuSla","fdisAbuSeedmass","fdisAbuHeight"))

#### Chosen model ###
m849318 <- lmer(log(n) ~ year * type + (1|block/plot), sites849318, REML = F)
m0318<- lmer((n) ~ year * type + (1|block/plot), sites0318, REML = F)



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
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(.5, 0, 0, 0, "cm")
  )
}
pd <- position_dodge(.6)

###1984-1993-2018 -----------------------------------------------------------------------------------------------------------------------
graph849318 <- ggplot(sites849318, aes(x = year, y = n, fill = type))+
  geom_boxplot(colour = "black") +
  scale_y_continuous(limits = c(0, 2.2), breaks = seq(0, 100, 0.5)) +
  scale_x_discrete(labels = c("1984","1993","2018")) +
  scale_fill_manual(breaks = c("fdisAbuSla","fdisAbuSeedmass","fdisAbuHeight"),
                    values = c("grey80","grey60","grey20"),
                    labels = c("SLA","Seed mass","Canopy height")) +
  labs(x = "", y = "FDis", fill = "") +
  guides(fill = F) +
  themeMB()

###2003-2018 -------------------------------------------------------------------------------------------------------------------------
graph0318 <- ggplot(sites0318, aes(year, n, fill = type))+
  geom_boxplot(colour = "black") +
  scale_y_continuous(limits = c(0, 2.2), breaks = seq(0, 100, 0.5)) +
  scale_x_discrete(labels = c("2003","2018")) +
  scale_fill_manual(breaks = c("fdisAbuSla","fdisAbuSeedmass","fdisAbuHeight"),
                     values = c("grey80","grey60","grey20"),
                     labels = c("SLA","Seed mass","Canopy height")) +
  labs(x = "", y = "", fill = "")+
  guides(fill = F) +
  themeMB() +
  theme(axis.line.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
###Multiplot ----------------------------------------------------------------------------------------------------------------------
legend <- get_legend(graph849318 + 
                       guides(fill = guide_legend(nrow = 1)) +
                       theme(legend.position = "bottom", 
                             legend.direction = "horizontal", 
                             legend.justification = "center"))
prow <- plot_grid(graph849318, graph0318, 
                  rel_widths = c(1.28,1), ncol = 2, align = "h",
                  labels = "AUTO", label_size = 8) +
  theme(plot.margin = unit(c(0,0,-.2,0), "cm"))
p <- plot_grid(prow, legend, 
               rel_heights = c(1,.1), nrow = 2);p
setwd("Z:/Documents/0_Uni/2018_Projekt_9_Masterthesis/3_Aufnahmen_und_Ergebnisse/2020_monitoring_Garchinger_Heide/outputs/figures")
ggsave("figure_6f_fdis_single_(800dpi_8x6cm).tiff",
       dpi = 800, width = 8, height = 6, units = "cm")
 