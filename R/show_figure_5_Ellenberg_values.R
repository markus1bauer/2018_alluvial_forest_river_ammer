# Show Figure Ellenberg values of F and N ####
# Markus Bauer



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(tidyverse)
library(ggbeeswarm)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Uni/2017_Projekt_8_Schnalzaue/3_Aufnahmen_und_Ergebnisse/2018_floodplain_Schnalz/data/processed")

### Load data ###
sites <- read_csv2("data_processed_sites.csv", col_names = T, col_types = 
                     cols(
                       .default = col_double(),
                       id = col_factor(),
                       treatment = col_factor()
                     )) %>% 
  select(id, treatment, cwmAbuF, cwmAbuN) %>%
  pivot_longer(cols = c(cwmAbuF, cwmAbuN), names_to = "indicator", values_to = "n") %>%
  mutate(indicator = fct_recode(indicator, "F value" = "cwmAbuF", "N value" = "cwmAbuN"))



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
pd <- position_dodge(.6)

ggplot(sites, aes(treatment, n))+
  geom_boxplot() +
  geom_quasirandom(color = "grey70", dodge.width = .6, size = 0.7)+
  facet_wrap(~indicator) +
  scale_y_continuous(limits = c(5,9), breaks = seq(0, 100, 1))+
  scale_x_discrete(labels = c("inactive","active")) +
  labs(x = "", y = "Ellenberg value", shape = "")+
  guides(shape = F) +
  themeMB()

setwd("Z:/Documents/0_Uni/2017_Projekt_8_Schnalzaue/3_Aufnahmen_und_Ergebnisse/2018_floodplain_Schnalz/outputs/figures")
ggsave("figure_5_Ellenberg_values_(800dpi_10x6cm).tiff",
       dpi = 800, width = 10, height = 6, units = "cm")
