#############################################################################################
# Datenvorbereitung
# Statistik
# Plotten
# summarySE() programmieren
###############################################################################################


##############################################################################################
# Datenvorbereitung ##########################################################################
###############################################################################################

# Für Durchführung verwenden (Traits, Speclist, Treatment)
setwd("C:/Users/HB/Documents/0_Uni/10_Semester/Projekt_Schnalzaue/3_Aufnahmen_und_Ergebnisse")
edata <- read.table("standort.txt",header=T)
edata$treatment <- ordered(edata$treatment, levels=c("behind_dam","no_dam","infront_dam"))
treatIN <- edata$treatment
treat <- treatIN[-(13:16)]
vdata <- read.table("vegetation.txt",header=T,na.strings="na",row.names=1)
speclist <- vdata[(vdata$layer=="K"),];speclist <- speclist[!(speclist$lifeform=="wood"),]
ldmc <- speclist$ldmc;height <- speclist$height;seedmass <- speclist$seedmass
name <- as.character(speclist$name)
name[1] <- "Aconitum_lycoctonum_subsp._vulparia"
name[19] <- "Carex_ornithopoda_subsp._ornithopodioides"
name[67] <- "Listera_ovata"
name[75] <- "Potentilla_tabernaemontani"
speclist <- speclist[,-c(1:14,31)]
speclist[speclist>0]=1
traitsIN <- data.frame(ldmc,height,seedmass)
NAs <- as.numeric(which(is.na(rowSums(traitsIN))));NAs
speclist <- speclist[-c(NAs),]
nameIN <- name[-c(NAs)]
traitsIN <- na.omit(traitsIN)
row.names(speclist) <-nameIN
row.names(traitsIN) <- nameIN
speclistIN <- t(speclist)
speclist <- speclistIN[-c(13:16),]
zeros <- which(colSums(speclist)==0);zeros
speclist <- speclist[,-zeros]
traits <- traitsIN[-zeros,]
name <- nameIN[-zeros]
#hist(traits$ldmc,breaks = 20)
#hist(log(traits$ldmc),breaks = 20)
#hist(traits$height,breaks = 20)
#hist(log(traits$height),breaks = 20)
#hist(traits$ldmc,breaks = 20)
#hist(log(traits$ldmc),breaks = 20)
traits <- log(traits)
traitsIN <- log(traitsIN)

## Folgendes habe ich gemacht und dann in die Excel-Datentabelle kopiert
library(traits)
###LDMC
ldmc <- leda("ldmc_geo")
ldmc <- ldmc[,c(1,4)]
ldmc$sbs_name <- gsub(" ","_",ldmc$sbs_name)
names(ldmc)[names(ldmc)=="single_value_[mg/g]"] <- "value"
which(!(name %in% ldmc$sbs_name))
#Beispiel um Datensatz zu durchsuchen: ldmc[grep("Aconitum",ldmc$sbs_name),]
#insgesamt 12 Arten fehlen: Aquilegia atrata; Calamagrostis varia, Cardamine trifolia, Carduus personata
#Knautia dipsacifolia; Lonicera xylosteum; Mentha aquatica x spicata, Picea abies
#Pleurospermum austriacum, Rosa canina; Salix eleagnos, Senecio ovatus
ldmc <- ldmc[c(which(ldmc$sbs_name %in% name)),]
ldmc <- tapply(ldmc$value,ldmc$sbs_name,median)
###Canopy height
height <- leda("canopy_height");height <- height[,c(1,4)]
height$sbs_name <- gsub(" ","_",height$sbs_name)
names(height)[names(height)=="single_value_[m]"] <- "value"
which(!(name %in% height$sbs_name))
height <- height[c(which(height$sbs_name %in% name)),]
height <- tapply(height$value,height$sbs_name,median)
###Seed mass
seedmass <- leda("seed_mass");seedmass <- seedmass[,c(1,5)]
seedmass$sbs_name <- gsub(" ","_",seedmass$sbs_name)
names(seedmass)[names(seedmass)=="single_value_[mg]"] <- "value"
which(!(name %in% seedmass$sbs_name))
seedmass <- seedmass[c(which(seedmass$sbs_name %in% name)),]
seedmass <- tapply(seedmass$value,seedmass$sbs_name,median)


############################################################################################
# Statistik ################################################################################
#############################################################################################
library(FD);library(pastecs);library(reshape);library(compute.es)

## CWM
CWM <- dbFD(traits,speclist);CWM <- CWM$CWM
CWMIN <- dbFD(traitsIN,speclistIN);CWMIN <-CWMIN$CWM

## CWM-LDMC
by(exp(CWM$ldmc),treat,stat.desc)
hist((CWM$ldmc),breaks=20)
bartlett.test(CWM$ldmc ~ treat) # Var. gleich
m1 <- lm(CWM$ldmc ~ treat);anova(m1) # kein Unterschied
par(mfrow=c(2,2));plot(m1) # passt
mes(2.309043e+02,2.327548e+02,1.526603e+01,1.249597e+01,6,6) #d=-0.13, r=-0.07
par(mfrow=c(1,1));plot(CWMIN$ldmc ~ treatIN,ylab="CWM [mg/g]");text(2,5.35,"LDMC: F(1,10) = 0.06, p = 0.810")

## CWM-Height
by(exp(CWM$height),treat,stat.desc)
hist(CWM$height,breaks=20) #nicht norm.
bartlett.test(CWM$height ~ treat) # Var. gleich
m2 <- lm(CWM$height ~ treat);anova(m2) # Unterschied
par(mfrow=c(2,2));plot(m2) # passt
mes(0.496430576,0.4231886858,0.038119217,0.0299000593,6,6) #d=2.14, r=0.73
par(mfrow=c(1,1));plot(CWMIN$height ~ treatIN,ylab="CWM [m]");text(1.5,-0.55,"Height: F(1,10) = 13.64, p = 0.004**")

## CWM-Seed mass
by(exp(CWM$seedmass),treat,stat.desc)
hist(CWM$seedmass,breaks=20)
bartlett.test(CWM$seedmass ~ treat) # Var. gleich
m3 <- lm((CWM$seedmass) ~ treat);anova(m3) # kein Unterschied
par(mfrow=c(2,2));plot(m3) # passt
mes(1.20514898,1.03619374,0.20890414,0.12463718,6,6) #d=0.98, r=0.44
par(mfrow=c(1,1));plot(CWMIN$seedmass ~ treatIN,ylab="CWM [mg]");text(1.7,-0.4,"Seed mass: F(1,10) = 2.98, p = 0.115")

## FDis-LDMC
traitsLIN <- data.frame(traitsIN[,-c(2,3)],row.names = nameIN)
FDisLIN <- dbFD(traitsLIN,speclistIN);FDisLIN <- FDisLIN$FDis
traitsL <- data.frame(traits[,-c(2,3)],row.names = name)
FDisL <- dbFD(traitsL,speclist); FDisL <- FDisL$FDis
by(FDisL,treat,stat.desc)
hist(FDisL,breaks = 20)
bartlett.test(FDisL ~ treat) # gleich
m4 <- lm(log(FDisL) ~ treat);anova(m4) # kein Unterschied
par(mfrow=c(2,2));plot(m4) # passt
mes(0.85744737,0.76778996,0.11262270,0.06491564,6,6) #d=0.98, r=0.44
par(mfrow=c(1,1));plot(FDisLIN ~ treatIN,ylab="FDis");text(2,1.05,"LDMC: F(1,10)=2.95, p = 0.117")

## FDis-Height
traitsHIN <- data.frame(traitsIN[,-c(1,3)],row.names = nameIN)
FDisHIN <- dbFD(traitsHIN,speclistIN);FDisHIN <- FDisHIN$FDis
traitsH <- data.frame(traits[,-c(1,3)],row.names = name)
FDisH <- dbFD(traitsH,speclist); FDisH <- FDisH$FDis
by(FDisH,treat,stat.desc)
hist(FDisH,breaks = 20)
bartlett.test(FDisH ~ treat) # gleich
m5 <- lm(FDisH ~ treat,weights = 1/FDisH);anova(m5) # kein Unterschied
par(mfrow=c(2,2));plot(m5) # schlecht
kruskal.test(FDisH ~ treat) # kein Unterschied
mes(0.76720304,0.698956712,0.15242573,0.080278289,6,6) #d=0.56, r=0.27
par(mfrow=c(1,1));plot(FDisHIN ~ treatIN,ylab="FDis");text(2.4,0.96,"Height: chi-squared = 0.23, p = 0.631")

## FDis-Seed mass
traitsSIN <- data.frame(traitsIN[,-c(1,2)],row.names = nameIN)
FDisSIN <- dbFD(traitsSIN,speclistIN);FDisSIN <- FDisSIN$FDis
traitsS <- data.frame(traits[,-c(1,2)],row.names = name)
FDisS <- dbFD(traitsS,speclist); FDisS <- FDisS$FDis
by(FDisS,treat,stat.desc)
hist(FDisS,breaks = 20)
bartlett.test(FDisS ~ treat) # gleich
m5 <- lm(FDisS ~ treat,weights = FDisS);anova(m5) # Unterschied
par(mfrow=c(2,2));plot(m5) # passt
mes(0.65838485,0.783444774,0.10253531,0.079812599,6,6) #d=-1.36, r=-0.56
par(mfrow=c(1,1));plot(FDisSIN ~ treatIN,ylab="FDis");text(2,0.55,"Seed mass: F(1,10) = 5.00, p = 0.049*")


############################################################################################
## Plotten ###################################################################################
#################################################################################################
library(ggplot2)

pdata <- data.frame(treat,CWM,FDisH,FDisL,FDisS)

### CWM-LDMC
CLGraph <- ggplot(pdata,aes(treat,ldmc))+
  geom_boxplot(colour="black")+
  annotate("text", x = 2.2, y = 5.54, label = "n.s.",size=3)+
  labs(x = "", y = "log(CWM [mg/g])")+
  scale_y_continuous(breaks=seq(5.3, 5.6, 0.1))+
  scale_x_discrete(labels=c("Inactive", "Active"))+
  theme(axis.text.x  = element_text(size=9,colour="black"), axis.title.y = element_text(size=9), axis.line.y=element_line(linetype = 1), axis.ticks.x = element_blank(), panel.background=element_rect(fill="white"));CLGraph
ggsave("Paper_CWM_LDMC_800dpi_(4x4cm).tiff",dpi = 800, width=4,height=4, units = "cm",path = "C:/Users/HB/Documents/0_Uni/10_Semester/Projekt_Schnalzaue/3_Aufnahmen_und_Ergebnisse/Ergebnisse")

### CWM-Height
CHGraph <- ggplot(pdata,aes(treat,height))+
  geom_boxplot(colour="black")+
  annotate("text", x = 1, y = -0.58, label = "a",size=3)+
  annotate("text", x = 2, y = -0.76, label = "b",size=3)+
  labs(x = "", y = "log(CWM [m])")+
  scale_y_continuous(breaks=seq(-1.2,-0.5,0.1))+ 
  scale_x_discrete(labels=c("Inactive","Active"))+
  theme(axis.text.x  = element_text(size=9,colour="black"), axis.title.y = element_text(size=9), axis.line.y=element_line(linetype = 1), axis.ticks.x = element_blank(), panel.background=element_rect(fill="white"));CHGraph
ggsave("Paper_CWM_Height_800dpi_(4x4cm).tiff",dpi = 800, width=4,height=4, units = "cm",path = "C:/Users/HB/Documents/0_Uni/10_Semester/Projekt_Schnalzaue/3_Aufnahmen_und_Ergebnisse/Ergebnisse")

### CWM-Seed mass
CSGraph <- ggplot(pdata,aes(treat,seedmass))+
  geom_boxplot(colour="black")+
  annotate("text", x = 2.2, y = 0.47, label = "n.s.",size=3)+
  labs(x = "", y = "log(CWM [mg])")+
  scale_y_continuous(breaks=seq(-0.5, 1, 0.2))+
  scale_x_discrete(labels=c("Inactive","Active"))+
  theme(axis.text.x  = element_text(size=9,colour="black"), axis.title.y = element_text(size=9), axis.line.y=element_line(linetype = 1), axis.ticks.x = element_blank(), panel.background=element_rect(fill="white"));CSGraph
ggsave("Paper_CWM_Seedmass_800dpi_(4x4cm).tiff",dpi = 800, width=4,height=4, units = "cm",path = "C:/Users/HB/Documents/0_Uni/10_Semester/Projekt_Schnalzaue/3_Aufnahmen_und_Ergebnisse/Ergebnisse")

### FDis-LDMC
FLGraph <- ggplot(pdata,aes(treat,FDisL))+
  geom_boxplot(colour="black")+
  annotate("text", x = 2.2, y = 1.2, label = "n.s.",size=3)+
  labs(x = "", y = "FDis",size=3)+
  scale_y_continuous(breaks=seq(0, 1.2, 0.5))+
  scale_x_discrete(labels=c("Inactive","Active"))+
  expand_limits(y=c(0,1.2))+
  theme(axis.title.y = element_text(size=9),axis.text.x  = element_text(size=9,colour="black"), axis.line.y=element_line(linetype = 1), axis.ticks.x = element_blank(), panel.background=element_rect(fill="white"));FLGraph
ggsave("Paper_FDis_LDMC_800dpi_(4x4cm).tiff",dpi = 800, width=4,height=4, units = "cm",path = "C:/Users/HB/Documents/0_Uni/10_Semester/Projekt_Schnalzaue/3_Aufnahmen_und_Ergebnisse/Ergebnisse")

### FDis-Height
FHGraph <- ggplot(pdata,aes(treat,FDisH))+
  geom_boxplot(colour="black")+
  annotate("text", x = 2.2, y = 1.2, label = "n.s.",size=3)+
  labs(x = "", y = "FDis")+
  scale_x_discrete(labels=c("Inactive","Active"))+
  scale_y_continuous(breaks=seq(0, 1.2, 0.5))+
  expand_limits(y=c(0,1.2))+
  theme(axis.title.y = element_text(size=9),axis.text.x  = element_text(size=9,colour="black"), axis.line.y=element_line(linetype = 1), axis.ticks.x = element_blank(), panel.background=element_rect(fill="white"));FHGraph
ggsave("Paper_FDis_Height_800dpi_(4x4cm).tiff",dpi = 800, width=4,height=4, units = "cm",path = "C:/Users/HB/Documents/0_Uni/10_Semester/Projekt_Schnalzaue/3_Aufnahmen_und_Ergebnisse/Ergebnisse")

### FDis-Seed mass
FSGraph <- ggplot(pdata,aes(treat,FDisS))+
  geom_boxplot(colour="black")+
  annotate("text", x = 1, y = 0.85, label = "a",size=3)+
  annotate("text", x = 2, y = 0.95, label = "b",size=3)+
  labs(x = "", y = "FDis")+
  scale_y_continuous(breaks=seq(0, 1.2, 0.5))+
  scale_x_discrete(labels=c("Inacitve","Active"))+
  expand_limits(y=c(0,1.2))+
  theme(axis.title.y = element_text(size=9),axis.text.x  = element_text(size=9,colour="black"), axis.line.y=element_line(linetype = 1), axis.ticks.x = element_blank(), panel.background=element_rect(fill="white"));FSGraph
ggsave("Paper_FDis_Seedmass_800dpi_(4x4cm).tiff",dpi = 800, width=4,height=4, units = "cm",path = "C:/Users/HB/Documents/0_Uni/10_Semester/Projekt_Schnalzaue/3_Aufnahmen_und_Ergebnisse/Ergebnisse")
