# Show Figure Ellenberg values of F and N ####
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
  select(id, treatment, cwmAbuF, cwmAbuN) %>%
  pivot_longer(cols = c(cwmAbuF, cwmAbuN), 
               names_to = "indicator", values_to = "n") %>%
  mutate(indicator = fct_recode(indicator, "Moisture" = "cwmAbuF", "Nutrients" = "cwmAbuN"),
         treatment = fct_relevel(treatment, c("no_dam", "behind_dam")),
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
pd <- position_dodge(.6)

ggplot(sites, aes(treatment, n))+
  geom_boxplot(colour = "black") +
  geom_quasirandom(color = "black", dodge.width = .6, size = 0.8)+
  facet_wrap(~indicator) +
  scale_y_continuous(limits = c(5, 9), breaks = seq(0, 100, 1))+
  labs(x = "", y = "CWM Ellenberg value", shape = "")+
  guides(shape = FALSE) +
  theme_mb()

ggsave(here("outputs/figures/figure_5_Ellenberg_values_(800dpi_10x6cm).tiff"),
       dpi = 800, width = 10, height = 6, units = "cm")
