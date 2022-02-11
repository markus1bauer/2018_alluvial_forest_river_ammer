# Show Figure ordination ####
# Markus Bauer



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(here)
library(tidyverse)
library(vegan)

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
  select(id, treatment, treeCover, shrubCover, barrierDistance, herbHeight) %>%
  mutate(treatment = fct_relevel(treatment, c("no_dam", "behind_dam")),
         treatment = fct_recode(treatment,
                                "Active" = "no_dam",
                                "Inactive" = "behind_dam"))


species <- read_csv2("data_processed_species.csv", col_names = TRUE, na = "na",
                     col_types =
                       cols(
                         .default = col_double(),
                         name = col_factor(),
                         abb = col_factor()
                       )) %>%
  pivot_longer(-name, "site", "value") %>%
  pivot_wider(site, name) %>%
  column_to_rownames("site")

#### a Choosen model ---------------------------------------------------
(ordi <- metaMDS(species, try = 99, previous.best = TRUE, na.rm = TRUE))
ef <- envfit(ordi ~ (herbHeight) + (treeCover) + (barrierDistance),
             data = sites, na.rm = TRUE) #Modell, Daten und Variablen eingeben



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ###############################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


theme_mb <- function() {
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size = 10, color = "black"),
    axis.line.y = element_line(),
    axis.line.x = element_line(),
    legend.key = element_rect(fill = "white"),
    legend.position = "bottom",
    legend.margin = margin(-.4, 0, 0, 0, "cm"),
    plot.margin = margin(.1, .15, 0, 0, "cm")
  )
}

data_scores <- as.data.frame(scores(ordi)) #input of model
data_scores$site <- rownames(data_scores)
data_scores$variable <- sites$treatment #Write data and 1. variable
### Create Ellipses ###
data_scores_mean <- aggregate(data_scores[1:2],
                             list(group = data_scores$variable),
                             mean)
vegan_cov_ellipse <- function(cov, center = c(0, 0), scale = 1, npoints = 100) {
  theta <- (0:npoints) * 2 * pi / npoints
  circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(circle %*% chol(cov)))
}

df_ell <- data.frame()
for (g in levels(data_scores$variable)){
  df_ell <- rbind(df_ell,
                  cbind(as.data.frame(
                    with(data_scores[data_scores$variable == g, ],
                         vegan_cov_ellipse(cov.wt(cbind(NMDS1, NMDS2),
                                                wt = rep(1 / length(NMDS1), 
                                                         length(NMDS1))
                                                )$cov,
                                         center = c(mean(NMDS1), mean(NMDS2)))))
                                , variable = g))
}

data_ef <- as.data.frame(ef$vectors$arrows * ((sqrt(ef$vectors$r))))
data_ef$variables <- rownames(data_ef)

### Plot ###
ggplot() +
  geom_label(aes(x = NMDS1, y = NMDS2, label = site, fill = variable),
             data = data_scores,
             size = 3, colour = "white", label.size = 0) +
  geom_path(aes(x = NMDS1, y = NMDS2, colour = variable), data = df_ell,
            size = 1, show.legend = FALSE) +
  geom_segment(aes(x = 0, xend = (NMDS1), y = 0, yend = (NMDS2)),
               data = data_ef,
               arrow = arrow(length = unit(0.2, "cm")),
               colour = "black") +
  scale_fill_manual(values = c("black", "grey50")) +
  scale_colour_manual(values = c("black", "grey50")) +
  annotate("text", x = -.62, y = .35, size = 4,
           label = "Vegetation height*") +
  annotate("text", x = -.13, y = .5, size = 4,
           label = "Distance to weir (n.s.)") +
  annotate("text", x = .6, y = -.4, size = 4,
           label = "Tree cover*") +
  annotate("text", x = -.65, y = .6, size = 4,
           label = "2D stress = 0.12") +
  coord_equal() +
  scale_y_continuous(limits = c(-.42, .6), breaks = seq(-1, 100, .2)) +
  scale_x_continuous(limits = c(-.86, .8), breaks = seq(-1, 100, .2)) +
  labs(fill = "", colour = "") +
  theme_mb()

ggsave(here("outputs", "figures", "figure_2_ordination_sites_(800dpi_12x10cm).tiff"),
       dpi = 800, width = 12, height = 10, units = "cm")
