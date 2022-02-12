# Show Figure CWM canopy height ####
# Markus Bauer



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation #########################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)

### Start ###
rm(list = ls())
setwd(here("data/processed"))

### Load data ###
sites <- read_csv2(here("data_processed_sites.csv"), col_names = TRUE,
                   col_types =
                     cols(
                       .default = col_double(),
                       id = col_factor(),
                       treatment = col_factor()
                     )) %>%
  select(id, treatment, cwmAbuHeight) %>%
  mutate(treatment = fct_relevel(treatment, c("no_dam", "behind_dam")),
         treatment = fct_recode(treatment, "Active" = "no_dam", "Inactive" = "behind_dam"))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plotten #############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


theme_mb <- function() {
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

ggplot(sites, aes(treatment, cwmAbuHeight)) +
  geom_boxplot(colour = "black") +
  geom_quasirandom(color = "black", dodge.width = .6, size = .8) +
  scale_y_continuous(limits = c(0.3, 0.6), breaks = seq(0, 100, 0.1))+
  labs(x = "", y = "CWM canopy heihgt [m]", size = 3)+
  theme_mb()

ggsave(here("outputs/figures/figure_6_cwm_height_(800dpi_6x6cm).tiff"),
       dpi = 800, width = 6, height = 6, units = "cm")
