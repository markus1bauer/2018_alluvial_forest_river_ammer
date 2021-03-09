setwd("C:/Users/HB/Documents/0_Uni/10_Semester/Projekt_Schnalzaue/3_Aufnahmen_und_Ergebnisse")
edataIN <- read.table("standort.txt",header=T,na.strings="na", dec = ".")
edata <- edataIN[-c(13:16),]
vdata <- read.table("vegetation.txt",header=T, na.strings="na", dec =".")
tree <- vdata[vdata$layer=="B",];tree <- tree[,-c(1:15,28:32)];tree[tree>0]=1
shrub <- vdata[vdata$layer=="S",];shrub <- shrub[,-c(1:15,28:32)];shrub[shrub>0]=1
herb <- vdata[vdata$layer=="K",];herb <- herb[,-c(1:15,28:32)];herb[herb>0]=1
colSums(tree)
colSums(shrub)
colSums(herb)
