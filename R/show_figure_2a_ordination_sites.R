# Show Figure ordination 1984-1993-2018 ####
# Markus Bauer
# Citation: Markus Bauer & Harald Albrecht (2020) Basic and Applied Ecology 42, 15-26
# https://doi.org/10.1016/j.baae.2019.11.003



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(vegan)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Uni/2018_Projekt_9_Masterthesis/3_Aufnahmen_und_Ergebnisse/2020_monitoring_Garchinger_Heide/data/processed")

### Load data ###
sites <- read_csv2("data_processed_sites849318.csv", col_names = T, na = "na", col_types = 
                        cols(
                          .default = col_double(),
                          ID = col_factor(),
                          plot = col_factor(),
                          block = col_factor(),
                          dataset = col_factor(),
                          year = col_factor()
                        )        
)

(sites <- sites %>%
    select(ID, plot, block, year) %>%
    filter(ID != "X84I1", ID != "X84I2",ID != "X84II1",ID != "X84II2",ID != "X93I1", ID != "X93I2",ID != "X93II1",ID != "X93II2",ID != "X18I1", ID != "X18I2",ID != "X18II1",ID != "X18II2")
)

species <- read_csv2("data_processed_species849318.csv", col_names = T, na = "na", col_types = 
                          cols(
                            .default = col_double(),
                            name = col_factor()
                          )        
)

species <- select(species, -X84I1, -X84I2, -X84II1, -X84II2, -X93I1, -X93I2, -X93II1, -X93II2, -X18I1, -X18I2, -X18II1, -X18II2)

###Exclude rare species (presence in less than 6 plots)
notrare <- species %>%
  pivot_longer(-name, "site","value") %>%
  mutate(presence = if_else(value > 0, 1, 0)) %>%
  group_by(name) %>%
  summarise(sum = sum(presence)) %>%
  mutate(notrare = if_else(sum > 5, 1, 0)) %>%
  filter(notrare == 1)
species <- semi_join(species, notrare)

species <- species %>%  
  pivot_longer(-name, "site", "value") %>%
  pivot_wider(site, name) %>%
  column_to_rownames("site")

#### a Choosen model ----------------------------------------------------------------------------------------
(ordi <- metaMDS(species, try = 99, previous.best = T, na.rm = T))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


themeMB <- function(){
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size = 10, color = "black"),
    axis.line.y = element_line(),
    axis.line.x = element_line(),
    axis.ticks.x = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.position = "bottom",
    legend.margin = margin(-.4, 0, 0, 0, "cm"),
    plot.margin = margin(.1, .15, 0, 0, "cm")
  )
}

data.scores <- as.data.frame(scores(ordi)) #input of model
data.scores$site <- rownames(data.scores)
data.scores$year <- sites$year #Write data and 1. variable
### Create Ellipses ###
data.scores.mean = aggregate(data.scores[1:2], list(group = data.scores$year), mean)
veganCovEllipse <- function(cov, center = c(0,0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi / npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

df_ell <- data.frame() #Write 1. variable 3 times
for(g in levels(data.scores$year)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(data.scores[data.scores$year == g,],
                                                   veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2), wt = rep(1 / length(NMDS1), length(NMDS1)))$cov, center = c(mean(NMDS1), mean(NMDS2)))))
                                ,year = g))
}
### Plot ###
ggplot() +
  geom_point(aes(x = NMDS1, y = NMDS2, shape = year, colour = year),#Write 1. variable
             data = data.scores, size = 3)+
  geom_path(aes(x = NMDS1, y = NMDS2, colour = year),#Write 1. variable
            data = df_ell, size = 1, linetype = 1)+
  scale_color_manual(values = c("grey80","grey60","grey20"))+
  scale_shape_manual(values = c(16,16,15))+
  annotate("text", x = -.25, y = .55, label = "2D stress = 0.27") +
  coord_fixed(ratio = 1, xlim = c(-.5,.6), ylim = c(-.5,.6))+
  guides(colour = guide_legend(reverse = F, title=""), 
         shape = guide_legend(reverse = F, title = ""))+
  themeMB()
setwd("Z:/Documents/0_Uni/2018_Projekt_9_Masterthesis/3_Aufnahmen_und_Ergebnisse/2020_monitoring_Garchinger_Heide/outputs/figures")
ggsave("figure_2a_ordination_sites_(800dpi_8x8cm).tiff",
       dpi = 800, width = 8, height = 8, units = "cm")
