# Show Figure target species coverage ####
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
sites <- read_csv("data_processed_sites.csv", col_names = TRUE,
                   col_types =
                     cols(
                       .default = col_double(),
                       id = col_factor(),
                       treatment = col_factor()
                     )) %>%
  select(id, treatment, targetClass, nontargetClass,
         targetOrder, nontargetOrder, targetAlliance,
         nontargetAlliance, targetAssociation,
         nontargetAssociation) %>%
  pivot_longer(c(targetClass, nontargetClass, targetOrder,
                 nontargetOrder, targetAlliance, nontargetAlliance,
                 targetAssociation, nontargetAssociation),
               names_to = "type", values_to = "value") %>%
  separate(type, c("target", "type"), sep = "target") %>%
  mutate(target = as_factor(paste0(target, "target")),
         type = as_factor(str_to_lower(type)),
         type = fct_recode(type,
                           "Querco-Fagetea" = "class",
                           "Fagetalia sylvaticae" = "order",
                           "Alno-Ulmion minoris" = "alliance",
                           "Alnetum incanae" = "association"
                           ),
         target = fct_recode(target,
                           "Character species of target vegetation" = "target",
                           "Character species of other plant communities" = "nontarget"),
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
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = ggplot2::element_blank(),
    legend.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
    plot.margin = ggplot2::margin(.5, 0, 0, 0, "cm")
  )
}

pd <- position_dodge(.6)

ggplot(sites, aes(treatment, value, colour = target)) +
  geom_boxplot() +
  #geom_quasirandom(color = "black", dodge.width = .9, size = 0.7) +
  facet_wrap(~type) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_colour_manual(values = c("black", "grey50")) +
  labs(x = "", y = "Coverage [%]", colour = "") +
  theme_mb()

### Save ###
ggsave(here("outputs", "figures",
            "figure_3_target_coverage_800dpi_12x10cm.tiff"),
       dpi = 800, width = 12, height = 10, units = "cm")
