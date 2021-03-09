# Model for ordination ####
# Markus Bauer



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(vegan)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Uni/2017_Projekt_8_Schnalzaue/3_Aufnahmen_und_Ergebnisse/2018_floodplain_Schnalz/data/processed")

### Load data ###
sites <- read_csv2("data_processed_sites.csv", col_names = T, na = "na", col_types = 
                     cols(
                       .default = col_double(),
                       ID = col_factor(),
                       plot = col_factor(),
                       block = col_factor(),
                       dataset = col_factor(),
                       year = col_factor()
                     )        
)

sites <- sites %>%
  select(ID, plot, block, year)

species <- read_csv2("data_processed_species0318.csv", col_names = T, na = "na", col_types = 
                       cols(
                         .default = col_double(),
                         name = col_factor()
                       )        
)

###Exclude rare species (presence in less than 6 plots)
notrare <- species %>%
  pivot_longer(-name, "site","value") %>%
  mutate(presence = if_else(value > 0, 1, 0)) %>%
  group_by(name) %>%
  summarise(sum = sum(presence)) %>%
  mutate(notrare = if_else(sum > 5, 1, 0)) %>%
  filter(notrare == 1)
species <- semi_join(species, notrare)

setwd("Z:/Documents/0_Uni/2018_Projekt_9_Masterthesis/3_Aufnahmen_und_Ergebnisse/2020_monitoring_Garchinger_Heide/data/raw")
traits <- read_csv2("data_raw_traits.csv", col_names = T, na = "na", col_types = 
                      cols(
                        .default = col_double(),
                        name = col_factor(),
                        abb = col_factor(),
                        family = col_factor(),
                        rlg = col_factor(),
                        rlb = col_factor(),
                        target = col_factor()
                      )        
)
traits <- select(traits, name, abb, family)
traits <- semi_join(traits, species, by = "name") %>%
  select(name, family, abb) %>%
  mutate(family = if_else(family == "Poaceae" | family == "Cyperaceae" | family == "Juncaeae", 
                          "Graminoids", if_else(family == "Fabaceae", 
                                                "Legumes", "Forbs")))

species <- species %>%  
  pivot_longer(-name, "site", "value") %>%
  pivot_wider(site, name) %>%
  column_to_rownames("site")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 NMDS #####################################################################################

edata <- edataIN[-c(13:16),]
vdataIN <- vdataIN[vdataIN$layer=="K",]
vdataIN <- vdataIN[,c(2,15:30)]
row.names(vdataIN) <- vdataIN$nmds;vdataIN$nmds <- NULL
vdata <- vdataIN[,c(1:12)]#nur SO und SU Spalten, mit Nuller-Zeilen
vdataIN <- vdataIN[rowSums(vdataIN)>0,]
vdata <- vdata[rowSums(vdata)>0,]
## F?r Artenzahlen
vOH <- vdataIN[,c(1:6)];vUK <- vdataIN[,c(7:12)];vOV <- vdataIN[,c(13:16)]
vdataOH <- vdataIN[,c(1:6)];vdataUK <- vdataIN[,c(7:12)];vdataOV <- vdataIN[,c(13:16)]
vdataIN <- t(vdataIN);vdata <- t(vdata)
vdataOH <- t(vdataOH);vdataUK <- t(vdataUK);vdataOV <- t(vdataOV)
vOH[vOH>0]=1;vUK[vUK>0]=1;vOV[vOV>0]=1
vOH <- rowSums(vOH);vUK <- rowSums(vUK);vOV <- rowSums(vOV)
vOH[vOH>0]=1;vUK[vUK>0]=1;vOV[vOV>0]=1


##############################################################################################
# Statistik ###################################################################################
################################################################################################
library(vegan)

## Artenzahlen

sum(vOH);sum(vUK);sum(vOV) #OH: 63; UK: 75; OV: 67 Arten (inactive, active, not used)

## Ordination

###OH,UK und OV
#vdataIN <- wisconsin(sqrt(vdataIN))
dist1 <- vegdist(vdataIN,method = "bray")
m1 <- metaMDS(dist1, previous.best = T, try = 50)
m1;stressplot(m1) #Stress 0.13
ef1 <- envfit(m1 ~ barrierDistance + treatment + treeCover + shrubCover + herbCover + herbHeight + deadwoodCover + countChwet + countFlood + log(height) + log(ldmc) + log(seedmass), data = edataIN, permu=999, na.rm=T)
ef1 #sig: treeCover, herbHeight, treatmen; fast: barrierDistance, deadwoodCover, seedmass
ef1a <- envfit(m1 ~ herbHeight + treeCover + log(height) + barrierDistance + log(seedmass),data = edataIN, permu=999, na.rm=T);ef1a
plot(m1,type="n");plot(ef1a,add=T);text(m1,dis="sites",cex=0.7);ordiellipse(m1,edataIN$treatment, kind="sd",draw="lines",label=T)
###OH und UK
#vdata <- wisconsin(sqrt(vdata))
dist2 <- vegdist(vdata,method = "bray")
m2 <- metaMDS(dist2, previous.best = T, try = 50)
m2;stressplot(m2) #Stress 0.12
ef2 <- envfit(m2 ~ barrierDistance + treatment + treeCover + shrubCover + herbCover + herbHeight + deadwoodCover + countChwet + countFlood + log(height) + log(ldmc) + log(seedmass), data = edata, permu=999, na.rm=T)
ef2 #sig: treeCover, herbHeight, treatment
ef2a <- envfit(m2 ~ herbHeight + treeCover + log(height) + barrierDistance + log(seedmass), data = edata, permu=999, na.rm=T);ef2a
plot(m2,type="n");plot(ef2a,add=T);text(m2,dis="sites",cex=0.7);ordiellipse(m2,edata$treatment, kind="sd",draw="lines",label=T, display = "sites")
ordisurf(m2, edata$treeCover)
ordisurf(m2, edata$herbHeight)
ordisurf(m2, edata$seedmass)
ordisurf(m2, edata$height)
ordisurf(m2, edata$barrierDistance)

## PERMANOVA

###OH und UK
permutest(betadisper(dist2, edata$treatment)) #gleiche Varianzen
perm2 <- adonis(vdata ~ treatment, data = edata, permutations = 999,method = "bray");perm2
densityplot(permustats(perm2)) #sig: p = 0.002**



#################################################################################################
# Plotten ######################################################################################
#################################################################################################
library(ggplot2);library(grid)

## OH,UK und OV
data.scores <- as.data.frame(scores(m1)) #Modell eingeben
data.scores$site <- rownames(data.scores)
data.scores$treatment <- edataIN$treatment #Daten und Hauptvariable eingeben
data.scores.mean = aggregate(data.scores[1:2],list(group=data.scores$treatment),mean) #Hauptvariable ?ndern
veganCovEllipse <- function(cov,center=c(0,0),scale=1,npoints=100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
df_ell <- data.frame() # Hauptvariable eingeben, 2 mal
for(g in levels(data.scores$treatment)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(data.scores[data.scores$treatment==g,],
                                                   veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(NMDS1),length(NMDS1)))$cov,center=c(mean(NMDS1),mean(NMDS2)))))
                                ,treatment=g)) #Hauptvariable eingeben
}
ggplot() +
  geom_point(aes(x=NMDS1,y=NMDS2, shape=treatment,colour=treatment),
             data = data.scores,size = 4)+
  geom_path(aes(x=NMDS1,y=NMDS2, line = treatment),data = df_ell, size = 1)+
    #scale_linetype_discrete(breaks = c("behind_dam","no_dam","infront_dam"),labels = c("Hinter Damm","Kein Damm", "Vor Damm"))+
  scale_shape_manual(values = c(15,0,16), breaks = c("behind_dam","no_dam","infront_dam"),labels = c("Hinter Damm","Kein Damm", "Vor Damm"))+
  scale_colour_manual(values = c("black","black","grey30"), breaks = c("behind_dam","no_dam","infront_dam"),labels = c("Hinter Damm","Kein Damm", "Vor Damm"))+
  annotate("text",x=-0.3,y=0.6,label="2D-Stress = 0.13") +
  labs(shape="",linetype="",colour="")+
  coord_equal(ylim=c(-0.5,0.6))+
  guides(shape = guide_legend(nrow=3))+
  theme(legend.position = "right", legend.key=element_rect(fill="white"), panel.background=element_rect(fill="white"),panel.border=element_rect(linetype = 1,fill=NA))
ggsave("NMDSIN.tiff",dpi=800,width=11,height=10, units = "cm", path = "C:/Users/HB/Documents/0_Uni/10_Semester/Projekt_Schnalzaue/3_Aufnahmen_und_Ergebnisse/Ergebnisse")

## Inaktive und aktive Aue
data.scores <- as.data.frame(scores(m2)) #Modell eingeben
data.scores$site <- rownames(data.scores)
data.scores$treatment <- edata$treatment #Daten und Hauptvariable eingeben
ef <- envfit(m2 ~ (herbHeight) + (treeCover) + log(height) + (barrierDistance), data=edata, na.rm=T) #Modell, Daten und Variablen eingeben
data.ef <- as.data.frame(ef$vectors$arrows*((sqrt(ef$vectors$r))))
data.ef$variables <- rownames(data.ef)
data.scores.mean = aggregate(data.scores[1:2],list(group=data.scores$treatment),mean) #Hauptvariable ?ndern
veganCovEllipse <- function(cov,center=c(0,0),scale=1,npoints=100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
df_ell <- data.frame() # Hauptvariable eingeben, 3 mal
for(g in levels(data.scores$treatment)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(data.scores[data.scores$treatment==g,],
                                                   veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(NMDS1),length(NMDS1)))$cov,center=c(mean(NMDS1),mean(NMDS2)))))
                                ,treatment=g)) #Hauptvariable eingeben
}
ggplot() +
  #geom_point(aes(x=NMDS1,y=NMDS2, shape = treatment),
             #data = data.scores, size = 4)+
  geom_label(aes(x=NMDS1,y=NMDS2,label=site, fill=treatment),
             data = data.scores, size = 3,colour = "white", label.size=0)+
  geom_path(aes(x=NMDS1,y=NMDS2, line = treatment),data = df_ell, size = 1)+
  geom_segment(aes(x=0,xend=(data.ef$NMDS1),y=0,yend=(data.ef$NMDS2)),
               data = data.ef, arrow=arrow(length=unit(0.2,"cm")),colour="black",inherit_aes=FALSE)+
  scale_fill_manual(values = c("black","grey30"), breaks = c("behind_dam","no_dam"),labels = c("Inactive","Active floodplain"))+
  #annotate("text",x=data.ef$NMDS1,y=data.ef$NMDS2,label=data.ef$variables)+
  annotate("text",x=0.15,y=0.6,size=4,label="Plant height (n.s.)")+
  annotate("text",x=0.49,y=0.25,size=4,label="Distance to weir (n.s.)")+
  annotate("text",x=0.61,y=0.09,size=4,label="Tree cover*")+
  annotate("text",x=-0.41,y=-0.4,size=4,label="Vegetation height*")+
  annotate("text",x=-0.53,y=0.6,size=4,label="2D stress = 0.12") +
  coord_equal(ylim=c(-0.4,0.6))+
  labs(fill="")+
  theme(legend.position="bottom", legend.key=element_rect(fill="white"), panel.background=element_rect(fill="white"),panel.border=element_rect(linetype = 1,fill=NA))
ggsave("Paper_NMDS_(800dpi_12.5x10cm).tiff",dpi=800,width=12.5,height=10, units = "cm", path = "C:/Users/HB/Documents/0_Uni/10_Semester/Projekt_Schnalzaue/3_Aufnahmen_und_Ergebnisse/Ergebnisse")
