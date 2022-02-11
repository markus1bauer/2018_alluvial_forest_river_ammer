# Show Figure FDis of all three traits ####
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
sites <- read_csv2(here("data_processed_sites.csv"), col_names = TRUE, col_types = 
                     cols(
                       .default = col_double(),
                       id = col_factor(),
                       treatment = col_factor()
                     )) %>% 
  select(id, treatment, fdisAbuLHS) %>%
  mutate(treatment = fct_relevel(treatment, c("no_dam", "behind_dam"))) %>%
  mutate(treatment = fct_recode(treatment, "Active" = "no_dam", "Inactive" = "behind_dam"))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plotten #############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


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

ggplot(sites, aes(treatment, fdisAbuLHS)) +
  geom_boxplot(colour = "black") +
  geom_quasirandom(color = "black", dodge.width = .6, size = .8) +
  scale_y_continuous(limits = c(.6, 1.4), breaks = seq(0, 100, .2)) +
  labs(x = "", y = "FDis", size = 3) +
  themeMB()

ggsave(here("outputs/figures/figure_6_fdis_all_(800dpi_6x6cm).tiff"),
       dpi = 800, width = 6, height = 6, units = "cm")
