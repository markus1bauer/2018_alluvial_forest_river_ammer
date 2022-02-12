# Show Figure Ellenberg indicator species richness ####
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
setwd(here("data", "processed"))

### Load data ###
sites <- read_csv2("data_processed_sites.csv", col_names = TRUE,
                   col_types =
                     cols(
                       .default = col_double(),
                       id = col_factor(),
                       treatment = col_factor()
                     )) %>%
  select(id, treatment, floodRichness, chwetRichness) %>%
  pivot_longer(cols = c(floodRichness, chwetRichness),
               names_to = "indicator", values_to = "n") %>%
  mutate(indicator = fct_relevel(indicator,
                                 c("floodRichness", "chwetRichness")),
         indicator = fct_recode(indicator,
                                "Flood indicator" = "floodRichness",
                                "Periodical wet conditions" = "chwetRichness"),
         treatment = fct_relevel(treatment, c("no_dam", "behind_dam")),
         treatment = fct_recode(treatment,
                                "Active" = "no_dam",
                                "Inactive" = "behind_dam"))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plotten #############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


theme_mb <- function() {
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white"),
    text  = ggplot2::element_text(size = 10, color = "black"),
    axis.line.y = ggplot2::element_line(),
    axis.line.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    legend.key = ggplot2::element_rect(fill = "white"),
    legend.position = "right",
    legend.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
    plot.margin = ggplot2::margin(.5, 0, 0, 0, "cm")
  )
}

pd <- position_dodge(.6)

ggplot(sites, aes(treatment, n)) +
  geom_boxplot(color = "black") +
  geom_quasirandom(color = "black", dodge.width = .6, size = .8) +
  facet_wrap(~indicator) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 100, 2)) +
  labs(x = "", y = "Species richness [#]", shape = "") +
  guides(shape = FALSE) +
  theme_mb()

ggsave(here("outputs", "figures",
            "figure_4_indicator_species_800dpi_10x6cm.tiff"),
       dpi = 800, width = 10, height = 6, units = "cm")
