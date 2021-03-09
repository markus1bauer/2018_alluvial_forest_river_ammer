###############################################################################################
# Datenvorbereitung
# Statistik
# Plotten
# summarySE() programmieren
#################################################################################################


#################################################################################################
# Datenvorbereitung ###########################################################################
#################################################################################################

setwd("C:/Users/HB/Documents/0_Uni/10_Semester/Projekt_Schnalzaue/3_Aufnahmen_und_Ergebnisse")
edataIN <- read.table("standort.txt",header=T,na.strings="na", dec = ".")
edata <- edataIN[-c(13:16),]
vdata <- read.table("vegetation.txt",header=T, na.strings="na", dec =".")
vdata <- vdata[vdata$layer=="K",]
wfdata <- vdata[vdata$chwet==1,];wfdataIN <- wfdata[,-c(1:15,32)];wfdata <- wfdata[,-c(1:15,28:32)]
wfdataIN[wfdataIN>0.00]=1;wfdataIN <- t(wfdataIN);edataIN$chwet <- rowSums(wfdataIN)
wfdata[wfdata>0.00]=1;wfdata <- t(wfdata);edata$chwet <- rowSums(wfdata)
uedata <- vdata[vdata$flood==1,];uedataIN <- uedata[,-c(1:15,32)];uedata <- uedata[,-c(1:15,28:32)]
uedataIN[uedataIN>0.00]=1;uedataIN <- t(uedataIN);edataIN$flood <-rowSums(uedataIN)
uedata[uedata>0.00]=1;uedata <- t(uedata);edata$flood <- rowSums(uedata)


#############################################################################################################
# Statistik #####################################################################################
################################################################################################
library(compute.es);library(pastecs)

## Wechselfeuchtezeiger
plot(chwet ~ treatment,edata)
by(edata$chwet,edata$treatment,stat.desc)
hist(edata$chwet,breaks=20)
m1 <- glm(chwet ~ treatment, poisson, edata)
summary(m1) #n.s., keine overdispersion
plot(chwet ~ treatment, edataIN);text(2,8,"z(1,10) = -0.378 , p = 0.706 n.s.")
mes(5.5,5,2.0736441,1.8973666,6,6) #d=0.25, r=0.12

## Überflutungszeiger
plot(flood ~ treatment, edata)
by(edata$flood,edata$treatment,stat.desc)
hist(edata$flood,breaks=20)
m2 <- glm(flood ~ treatment, poisson, edata)
summary(m2) #n.s.
plot(flood ~ treatment, edataIN);text(2,6,"z(1,10) = 0 , p = 1 n.s.")
kruskal.test(flood ~ treatment,edata) #n.s.
mes(3,3,1.2649111,1.0954451,6,6) #d=0, r=0


###############################################################################################
# Plotten ##################################################################################
################################################################################################
library(ggplot2);library(plyr)

pdatac <- summarySE(edata, measurevar="chwet", groupvars="treatment")
wfGraph <- ggplot(pdatac, aes(treatment, chwet))+
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=chwet-se, ymax=chwet+se), width=.2, position=position_dodge(.9)) +
  annotate("text", x = 2.2, y = 7, label = "n.s.",size=3)+
  labs(x="",y="Number of species",size=4)+
  scale_y_continuous(breaks=seq(0,10,1))+
  scale_x_discrete(labels=c("Inactive","Active")) +
  theme(axis.text.x = element_text(colour="black",size=10), axis.title.y = element_text(size=10), axis.line.y=element_line(linetype = 1),axis.ticks.x = element_blank(), legend.direction = "vertical", legend.position = "bottom",legend.key=element_rect(fill="white"),panel.background=element_rect(fill="white"));wfGraph
ggsave("Paper_Chwet_800dpi_(6.25x6.25cm).tiff",width=6.25,height=6.25,units="cm",dpi = 800, path = "C:/Users/HB/Documents/0_Uni/10_Semester/Projekt_Schnalzaue/3_Aufnahmen_und_Ergebnisse/Ergebnisse")
pdatac <- summarySE(edata, measurevar="flood", groupvars="treatment")
ueGraph <- ggplot(pdatac, aes(treatment, flood))+
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=flood-se, ymax=flood+se), width=.2, position=position_dodge(.9)) +
  annotate("text", x = 2.2, y = 7, label = "n.s.",size=3)+
  labs(x="",y="Number of species")+
  scale_y_continuous(breaks = seq(0,10,1))+
  scale_x_discrete(labels=c("Inactive","Active")) +
  theme(axis.text.x = element_text(colour="black",size=10), axis.title.y = element_text(size=10), axis.line.y=element_line(linetype = 1), axis.ticks.x = element_blank(), legend.direction = "vertical", legend.position = "bottom",legend.key=element_rect(fill="white"),panel.background=element_rect(fill="white"));ueGraph
ggsave("Paper_Flood_800dpi_(6.25x6.25cm).tiff",width=6.25,height=6.25,units="cm",dpi = 800, path = "C:/Users/HB/Documents/0_Uni/10_Semester/Projekt_Schnalzaue/3_Aufnahmen_und_Ergebnisse/Ergebnisse")


###############################################################################################################
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