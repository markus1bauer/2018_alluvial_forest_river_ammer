# Show Figure target species coverage ####
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
  select(id, treatment, targetClass, nontargetClass, targetOrder, nontargetOrder, targetAlliance, nontargetAlliance, targetAssociation, nontargetAssociation) %>%
  pivot_longer(c(targetClass, nontargetClass, targetOrder, nontargetOrder, targetAlliance, nontargetAlliance, targetAssociation, nontargetAssociation), names_to = "type", values_to = "value") %>%
  separate(type, c("target", "type"), sep = "target") %>%
  mutate(target = as_factor(paste0(target, "target"))) %>%
  mutate(type = as_factor(str_to_lower(type))) %>%
  mutate(type = fct_recode(type,
                           "Querco-Fagetea" = "class",
                           "Fagetalia sylvaticae" = "order",
                           "Alno-Ulmion minoris" = "alliance",
                           "Alnetum incanae" = "association"
                           )) %>%
  mutate(target = fct_recode(target,
                           "Character species of target vegetation" = "target",
                           "Character species of other plant communities" = "nontarget"
                           )) %>%
  mutate(treatment = fct_relevel(treatment, c("no_dam", "behind_dam"))) %>%
  mutate(treatment = fct_recode(treatment, "Active" = "no_dam", "Inactive" = "behind_dam"))



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
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(.5, 0, 0, 0, "cm")
  )
}
pd <- position_dodge(.6)

ggplot(sites, aes(treatment, value, colour = target))+
  geom_boxplot() +
  #geom_quasirandom(color = "black", dodge.width = .9, size = 0.7)+
  facet_wrap(~type) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 20))+
  scale_colour_manual(values = c("black", "grey50")) +
  labs(x = "", y = "Coverage [%]", colour = "") +
  themeMB()

setwd("Z:/Documents/0_Uni/2017_Projekt_8_Schnalzaue/3_Aufnahmen_und_Ergebnisse/2018_floodplain_Schnalz/outputs/figures")
ggsave("figure_3_target_coverage_(800dpi_12x10cm).tiff",
       dpi = 800, width = 12, height = 10, units = "cm")
