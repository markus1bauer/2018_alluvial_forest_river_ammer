# Show Figure functional dispersion (all traits singularily) ####
# Markus Bauer



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation #########################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(tidyverse)
library(ggbeeswarm)

### Start ###
rm(list = ls())
setwd("data/processed")

### Load data ###
sites <- read_csv2(here("data_processed_sites.csv"), col_names = TRUE,
                   col_types =
                     cols(
                       .default = col_double(),
                       id = col_factor(),
                       treatment = col_factor()
                       )) %>%
  select(id, treatment, fdisAbuLdmc, fdisAbuHeight, fdisAbuSeedmass) %>%
  mutate(treatment = fct_relevel(treatment, c("no_dam", "behind_dam")),
         treatment = fct_recode(treatment, 
                                "Active" = "no_dam", 
                                "Inactive" = "behind_dam")) %>%
  pivot_longer(c(fdisAbuLdmc, fdisAbuHeight, fdisAbuSeedmass),
               names_to = "type", values_to = "value") %>%
  mutate(type = as_factor(type),
         type = fct_relevel(type, c("fdisAbuLdmc", 
                                    "fdisAbuHeight", 
                                    "fdisAbuSeedmass")))



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

pd <- position_dodge(.6)

ggplot(sites, aes(x = treatment, y = value, fill = type)) +
  geom_boxplot(colour = "black") +
  geom_quasirandom(color = "black", dodge.width = .8, size = .8) +
  scale_y_continuous(limits = c(.1, .9), breaks = seq(0, 100, 0.2)) +
  scale_fill_manual(breaks = c("fdisAbuLdmc",
                               "fdisAbuHeight",
                               "fdisAbuSeedmass"),
                    values = c("grey80", "grey60", "grey20"),
                    labels = c("LDMC", "Canopy height", "Seed mass")) +
  labs(x = "", y = "FDis", fill = "") +
  theme_mb()


ggsave(here("outputs/figures/figure_6_fdis_single_(800dpi_12x6cm).tiff"),
       dpi = 800, width = 12, height = 6, units = "cm")
