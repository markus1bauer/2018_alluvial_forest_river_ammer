###############################################################################################
# Datenvorbereitung
# Statistik
# Plotten
# summarySE() programmieren
############################################################################################


#############################################################################################
# Datenvorbereitung ###########################################################################
################################################################################################
library(reshape)

setwd("C:/Users/HB/Documents/0_Uni/10_Semester/Projekt_Schnalzaue/3_Aufnahmen_und_Ergebnisse")
vdata <- read.table("vegetation.txt",header=T,na.strings="na",row.names=1)
edata <- read.table("standort.txt",header=T)
vdata <- subset(vdata,layer=="K")
edata$treatment <- ordered(edata$treatment, levels=c("behind_dam","no_dam","infront_dam"))
treatIN <- edata$treatment
treat <- treatIN[-(13:16)]
K84IN <- subset(vdata,soc>=84000);K84IN <- K84IN[,-c(1:14,31)];K84IN <- colSums(K84IN)
O843IN <- subset(vdata,soc>=84300 & soc<84400);O843IN <- O843IN[,-c(1:14,31)];O843IN <- colSums(O843IN)
V8433IN <- subset(vdata,soc>=84330 & soc<84340);V8433IN <- V8433IN[,-c(1:14,31)];V8433IN <- colSums(V8433IN)
AIN <- subset(vdata,name2==c("Thalictrum_aquilegiifolium","Alnus_incana"));AIN <- AIN[,-c(1:14,31)];AIN <- colSums(AIN)
K8o4IN <- subset(vdata,soc>=81000 & soc<84000);K8o4IN <- K8o4IN[,-c(1:14,31)];K8o4IN <- colSums(K8o4IN)
O84o3IN <- subset(vdata,soc>=84100 & soc<84300);O84o3IN <- O84o3IN[,-c(1:14,31)];O84o3IN <- colSums(O84o3IN)
V843o3IN <- subset(vdata,soc>=84310 & soc<84330);V843o3IN <- V843o3IN[,-c(1:14,31)];V843o3IN <- colSums(V843o3IN)
AoIN <- subset(vdata,name2==c("Carex_sylvatica","Prunus_padus"))
AoIN <- AoIN[,-c(1:14,31)];AoIN <- colSums(AoIN)
K84 <- subset(vdata,soc>=84000);K84 <- K84[,-c(1:14,27:31)];K84 <- colSums(K84)
O843 <- subset(vdata,soc>=84300 & soc<84400);O843 <- O843[,-c(1:14,27:31)];O843 <- colSums(O843)
V8433 <- subset(vdata,soc>=84330 & soc<84340);V8433 <- V8433[,-c(1:14,27:31)];V8433 <- colSums(V8433)
A <- subset(vdata,name2==c("Thalictrum_aquilegiifolium","Alnus_incana"));
A <- A[,-c(1:14,27:31)];A <- colSums(A)
K8o4 <- subset(vdata,soc>=81000 & soc<84000);K8o4 <- K8o4[,-c(1:14,27:31)];K8o4 <- colSums(K8o4)
O84o3 <- subset(vdata,soc>=84100 & soc<84300);O84o3 <- O84o3[,-c(1:14,27:31)];O84o3 <- colSums(O84o3)
V843o3 <- subset(vdata,soc>=84310 & soc<84330);V843o3 <- V843o3[,-c(1:14,27:31)];V843o3 <- colSums(V843o3)
Ao <- subset(vdata,name2==c("Prunus_padus","Aegopodium_podagraria","Brachypodim_sylvaticum","Carex_sylvatica","Paris_quadrifolia","Stachys_sylvatica"));Ao <- Ao[,-c(1:14,27:31)];Ao <- colSums(Ao)
pdata <- data.frame(treat,K84, K8o4,O843, O84o3, V8433, V843o3, A, Ao)
pdata <- melt(pdata, id= "treat", measured=c("K84","K8o4","O843","O84o3","V8433","V843o3","A","Ao"))
colnames(pdata)[colnames(pdata)=="value"] <- "aimCover";colnames(pdata)[colnames(pdata)=="variable"] <- "soc_cat"
pdata$socLevel <- c(rep("Querco-Fagetea",24),rep("Fagetalia sylvaticae",24),rep("Alno-Ulmion minoris",24),rep("Alnetum incanae",24))
pdata$socCat <- as.factor(c(rep("Ja",12),rep("Nein",12),rep("Ja",12),rep("Nein",12),rep("Ja",12),rep("Nein",12),rep("Ja",12),rep("Nein",12)))
pdata$socLevel <- factor(pdata$socLevel, levels = c("Querco-Fagetea", "Fagetalia sylvaticae", "Alno-Ulmion minoris","Alnetum incanae"))


##################################################################################################
# Statistik #################################################################################
##################################################################################################

## Artenzahlen
vdataOH <- vdata[,c(15:20)];vdataUK <- vdata[,c(21:26)];vdataOV <- vdata[,c(27:30)]
vdataOH[vdataOH>0]=1;vdataUK[vdataUK>0]=1;vdataOV[vdataOV>0]=1
vdataOH$name2 <- vdata$name2; vdataOH$soc <- vdata$soc; vdataOH <- na.omit(vdataOH, cols="soc");vdataOH$typAbu <- rowSums(vdataOH[1:6]);vdataOH <- subset(vdataOH,vdataOH$typAbu>0)
vdataUK$name2 <- vdata$name2;vdataUK$soc <- vdata$soc; vdataUK <- na.omit(vdataUK, cols="soc");vdataUK$typAbu <- rowSums(vdataUK[1:6]);vdataUK <- subset(vdataUK,vdataUK$typAbu>0)
vdataOV$name2 <- vdata$name2;vdataOV$soc <- vdata$soc; vdataOV <- na.omit(vdataOV, cols="soc");vdataOV$typAbu <- rowSums(vdataOV[1:4]);vdataOV <- subset(vdataOV,vdataOV$typAbu>0)
table(vdataOH$soc > 84000);table(vdataUK$soc > 84000);table(vdataOV$soc > 84000)
table(vdataOH$soc >= 81000 & vdataOH$soc < 84000);table(vdataUK$soc>=81000 & vdataUK$soc<84000);table(vdataOV$soc>= 81000 & vdataOH$soc<84000)
#Querco-Fagetea
##OH: 21, 0 andere | UK: 23, 18 andere | OV: 20, 9 andere
table(vdataOH$soc >= 84300 & vdataOH$soc < 84400);table(vdataUK$soc>=84300 & vdataUK$soc<84400);table(vdataOV$soc>=84300 & vdataOV$soc<84400)
table(vdataOH$soc >= 84100 & vdataOH$soc < 84300);table(vdataUK$soc>=84100 & vdataUK$soc<84300);table(vdataOV$soc>=84100 & vdataOV$soc<84300)
#Fagetalia sylvaticae
##OH: 16, 0 andere | UK: 18, 1 andere | OV: 14, 1 andere
table(vdataOH$soc >= 84330 & vdataOH$soc < 84340);table(vdataUK$soc>=84330&vdataUK$soc<84340);table(vdataOV$soc>=84330&vdataOV$soc<84340)
table(vdataOH$soc >= 84310 & vdataOH$soc < 84330);table(vdataUK$soc>=84310&vdataUK$soc<84330);table(vdataOV$soc>=84310&vdataOV$soc<84330)
#Alno-Ulmion minoris
##OH: 5, 1 andere | UK: 6, 2 andere | OV: 5, 0 andere
table(vdataOH$name2==c("Thalictrum_aquilegiifolium","Alnus_incana"));table(vdataUK$name2==c("Thalictrum_aquilegiifolium","Alnus_incana"));table(vdataOV$name2==c("Thalictrum_aquilegiifolium","Alnus_incana"))
table(vdataOH$name2==c("Prunus_padus","Aegopodium_podagraria","Brachypodim_sylvaticum","Carex_sylvatica","Paris_quadrifolia","Stachys_sylvatica"));table(vdataUK$name2==c("Prunus_padus","Aegopodium_podagraria","Brachypodim_sylvaticum","Carex_sylvatica","Paris_quadrifolia","Stachys_sylvatica"));table(vdataOV$name2==c("Prunus_padus","Aegopodium_podagraria","Brachypodim_sylvaticum","Carex_sylvatica","Paris_quadrifolia","Stachys_sylvatica"))
#Alnetum incanae
##OH: 0, 1(1) andere | UK 1, 1(2) andere | OV: 0, 0(2) andere


##################################################################################################
# Plotten #################################################################################
##################################################################################################
library(ggplot2);library(plyr)

pdatac <- summarySE(pdata, measurevar="aimCover", groupvars=c("treat","socCat","socLevel"))
pd <- position_dodge(0.15) 
Graph <- ggplot(pdatac, aes(treat, aimCover, shape=socCat))+
  geom_point(position=pd, aes(group=pdatac$treat)) +
  geom_point(position=pd, size=3)+
  geom_errorbar(aes(ymin=aimCover-se, ymax=aimCover+se), width=.5, position=pd) +
  facet_wrap(~socLevel,nrow=2,ncol=2)+ 
  scale_shape_discrete(labels=c("Character species of target vegetation","Character species of other plant communities"))+
  scale_x_discrete(labels=c("Inactive","Active"))+
  labs(x="",y="Coverage [%]",linetype="")+
  theme(axis.text.x  = element_text(size=12,colour = "black"), axis.title.y = element_text(size=12), axis.line.y=element_line(linetype = 1), axis.ticks.x = element_blank(),legend.text = element_text(size=10), legend.direction = "vertical", legend.title=element_blank(), legend.position="bottom", legend.key=element_rect(fill="white"), panel.background=element_rect(fill="white"));Graph
ggsave("Paper_plant_sociology_800dpi_(10x10cm).tiff",dpi = 800, width=10,height=10, units = "cm",path = "C:/Users/HB/Documents/0_Uni/10_Semester/Projekt_Schnalzaue/3_Aufnahmen_und_Ergebnisse/Ergebnisse")


##############################################################################################################
# summarySE() programmieren ####################################################################################################
##############################################################################################################

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
