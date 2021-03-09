###############################################################################################
# Vorbereitung
# Deskriptive Statistik
# Plotten
## Plotten für Paper
###############################################################################################


##############################################################################################
# Datenvorbereitung ###########################################################################
##################################################################################################

setwd("C:/Users/HB/Documents/0_Uni/10_Semester/Projekt_Schnalzaue/3_Aufnahmen_und_Ergebnisse")
vdata <- read.table("vegetation.txt",header=T,na.strings="na",row.names=1)
edata <- read.table("standort.txt",header=T)
treat <- edata$treatment
speclist <- vdata[,c(15:30)]
speclist[speclist>0]=1
vdata$f <- as.numeric(vdata$f);vdata$l <- as.numeric(vdata$l);vdata$n <- as.numeric(vdata$n)
dataf <- vdata[,10]*speclist
dataf <- t(dataf)
datal <- vdata[,9]*speclist
datal <- t(datal)
datan <- vdata[,11]*speclist
datan <- t(datan)


################################################################################################
# Deskriptive Statistik #######################################################################
#################################################################################################

## Berechnung der Zeigerwerte
###Feuchtezahl
datafda <- subset(dataf,treat=="behind_dam")
fda <- c()
for (i in 1:9){
  fda <- c(fda,length(which(datafda==i)))
}
fda <- fda/sum(fda)
datafno <- subset(dataf,treat=="no_dam")
fno <- c()
for (i in 1:9){
  fno <- c(fno,length(which(datafno==i)))
}
fno <- fno/sum(fno)
datafin <- subset(dataf,treat=="infront_dam")
fin <- c()
for (i in 1:9){
  fin <- c(fin,length(which(datafin==i)))
}
fin <- fin/sum(fin)
###Lichtzahl
datalda <- subset(datal,treat=="behind_dam")
lda <- c()
for (i in 1:9){
  lda <- c(lda,length(which(datalda==i)))
}
lda <- lda/sum(lda)
datalno <- subset(datal,treat=="no_dam")
lno <- c()
for (i in 1:9){
  lno <- c(lno,length(which(datalno==i)))
}
lno <- lno/sum(lno)
datalin <- subset(datal,treat=="infront_dam")
lin <- c()
for (i in 1:9){
  lin <- c(lin,length(which(datalin==i)))
}
lin <- lin/sum(lin)
###Stickstoffzahl
datanda <- subset(datan,treat=="behind_dam")
nda <- c()
for (i in 1:9){
  nda <- c(nda,length(which(datanda==i)))
}
nda <- nda/sum(nda)
datanno <- subset(datan,treat=="no_dam")
nno <- c()
for (i in 1:9){
  nno <- c(nno,length(which(datanno==i)))
}
nno <- nno/sum(nno)
datanin <- subset(datan,treat=="infront_dam")
nin <- c()
for (i in 1:9){
  nin <- c(nin,length(which(datanin==i)))
}
nin <- nin/sum(nin)

## Einfache Plots
###Feuchtezahl
plot(fin,type="n",ylim=c(0,0.45),xlab="Feuchtezahl",ylab="Standardisierte Artenanzahl")
lines(fda,lty=1)
lines(fno,lty=2)
lines(fin,lty=3)
legend(1,0.45,c("behind_dam","no_dam","infront_dam"),lty=c(1,2,3))
axis(1,at=c(1:9),labels=c("1","2","3","4","5","6","7","8","9"))
###Lichtzahl
plot(lno,type="n",ylim=c(0,0.45),xlab="Lichtzahl",ylab="Standardisierte Artenanzahl")
lines(lda,lty=1)
lines(lno,lty=2)
lines(lin,lty=3)
legend(1,0.45,c("behind_dam","no_dam","infront_dam"),lty=c(1,2,3))
axis(1,at=c(1:9),labels=c("1","2","3","4","5","6","7","8","9"))
##Stickstoffzahl
plot(nin,type="n",ylim=c(0,0.45),xlab="Stickstoffzahl",ylab="Standardisierte Artenanzahl")
lines(nda,lty=1)
lines(nno,lty=2)
lines(nin,lty=3)
legend(1,0.45,c("behind_dam","no_dam","infront_dam"),lty=c(1,2,3))
axis(1,at=c(1:9),labels=c("1","2","3","4","5","6","7","8","9"))


##############################################################################################
## Plotten ######################################################################################
#################################################################################################
library(ggplot2);library(reshape)

## Vorbereitung
eno <- c(1:9);eno <- as.integer(eno)
pdata.f <- data.frame(fda,fno,fin,eno)
pdata.l <- data.frame(lda,lno,lin,eno)
pdata.n <- data.frame(nda,nno,nin,eno)
pdata.f <- melt(pdata.f, id = "eno", measured = c("fda","fno","fin"))
pdata.n <- melt(pdata.n, id = "eno", measured = c("nda","nno","nin"))
pdata.l <- melt(pdata.l, id = "eno", measured = c("lda","lno","lin"))

## Plotten
fGraph <- ggplot(pdata.f, aes(eno,value,linetype = variable))+
  geom_line()+
  labs(x="Feuchtezahl",y="Standardisierte Artenanzahl")+
  expand_limits(y=c(0,0.45))+
  scale_x_continuous(breaks=seq(1,9,1))+
  scale_linetype_discrete(labels=c("Hinter Damm", "Kein Damm", "Vor Damm"))+
  theme(axis.title = element_text(size = 14),axis.text = element_text(size = 12),legend.text = element_text(size=7.5),axis.line.y=element_line(linetype = 1),legend.title=element_blank(),legend.key=element_rect(fill="white"), legend.position="bottom", panel.background=element_rect(fill="white"));fGraph
ggsave("Ellen_Feuchte.tiff",dpi = 800, width=7.8,height=8, units = "cm",path = "C:/Users/HB/LRZ Sync+Share/Projektarbeit Schnalzaue/3_Aufnahmen_und_Ergebnisse")
nGraph <- ggplot(pdata.n, aes(eno,value,linetype = variable))+
  geom_line()+
  labs(x="Stickstoffzahl",y="Standardisierte Artenanzahl")+
  expand_limits(y=c(0,0.45))+
  scale_x_continuous(breaks=seq(1,9,1))+
  scale_linetype_discrete(labels=c("Hinter Damm", "Kein Damm", "Vor Damm"))+
  theme(axis.title = element_text(size = 14),axis.text = element_text(size = 12),legend.text = element_text(size=7.5),legend.title=element_blank(),legend.key=element_rect(fill="white"), legend.position="bottom", axis.line.y=element_line(linetype = 1), panel.background=element_rect(fill="white"));nGraph
ggsave("Ellen_Stickstoff.tiff",dpi = 800, width=7.8,height=8, units = "cm",path = "C:/Users/HB/LRZ Sync+Share/Projektarbeit Schnalzaue/3_Aufnahmen_und_Ergebnisse")
lGraph <- ggplot(pdata.l, aes(eno,value,linetype = variable))+
  geom_line()+
  labs(x="Lichtzahl",y="Standardisierte Artenanzahl")+
  expand_limits(y=c(0,0.45))+
  scale_x_continuous(breaks=seq(1,9,1))+
  scale_linetype_discrete(labels=c("Hinter Damm", "Kein Damm", "Vor Damm"))+
  theme(axis.title = element_text(size = 14),axis.text = element_text(size = 12),legend.text = element_text(size=7.5),legend.title=element_blank(),legend.key=element_rect(fill="white"), legend.position="bottom", axis.line.y=element_line(linetype = 1), panel.background=element_rect(fill="white"));lGraph
ggsave("Ellen_Licht.tiff",dpi = 800, width=7.8,height=8, units = "cm",path = "C:/Users/HB/LRZ Sync+Share/Projektarbeit Schnalzaue/3_Aufnahmen_und_Ergebnisse")

## Vorbereitung fürs Paper
eno <- c(1:9);eno <- as.integer(eno)
paper.f <- data.frame(fda,fno,eno)
paper.n <- data.frame(nda,nno,eno)
paper.f <- melt(paper.f, id = "eno", measured = c("fda","fno"))
paper.n <- melt(paper.n, id = "eno", measured = c("lda","lno"))

## Paper
fGraph <- ggplot(paper.f, aes(eno,value,linetype = variable))+
  geom_line()+
  labs(x="Moisture value",y="Standardised number of species")+
  expand_limits(y=c(0,0.45))+
  scale_x_continuous(breaks=seq(1,9,1))+
  scale_linetype_discrete(labels=c("Inactive", "Active floodplain"))+
  theme(axis.title = element_text(size = 10),axis.text = element_text(size = 12),legend.text = element_text(size=10),axis.line.y=element_line(linetype = 1),legend.title=element_blank(),legend.key=element_rect(fill="white"), legend.position="bottom", panel.background=element_rect(fill="white"));fGraph
ggsave("Paper_moisture_(800dpi_7.8x8cm).tiff",dpi = 800, width=7.8,height=8, units = "cm",path = "C:/Users/HB/Documents/0_Uni/10_Semester/Projekt_Schnalzaue/3_Aufnahmen_und_Ergebnisse/Ergebnisse")
nGraph <- ggplot(paper.n, aes(eno,value,linetype = variable))+
  geom_line()+
  labs(x="Nutrient value",y="Standardised number of species")+
  expand_limits(y=c(0,0.45))+
  scale_x_continuous(breaks=seq(1,9,1))+
  scale_linetype_discrete(labels=c("Inactive", "Active floodplain"))+
  theme(axis.title = element_text(size = 10),axis.text = element_text(size = 12),legend.text = element_text(size=10),axis.line.y=element_line(linetype = 1),legend.title=element_blank(),legend.key=element_rect(fill="white"), legend.position="bottom", panel.background=element_rect(fill="white"));nGraph
ggsave("Paper_nutrient_(800dpi_7.8x8cm).tiff",dpi = 800, width=7.8,height=8, units = "cm",path = "C:/Users/HB/Documents/0_Uni/10_Semester/Projekt_Schnalzaue/3_Aufnahmen_und_Ergebnisse/Ergebnisse")

