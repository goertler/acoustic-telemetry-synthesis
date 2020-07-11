# pca on loadings
source("biostats.R")
ls()
library(vegan)
Z.rot = mod5$Estimates$Z

# multivariate normality
#library(MVN) # not available (for R version 3.6.1)
#results<-mvn(data=Z.rot, mvnTest="royston")
hist(log(Z.rot[,1]))
hist(log(Z.rot[,2]))
hist(log(Z.rot[,3]))
hist(log(Z.rot[,4]))
hist(log(Z.rot[,5]))

# homogeneity in variance
boxplot(Z.rot[,1])
boxplot(Z.rot[,2])
boxplot(Z.rot[,3])
boxplot(Z.rot[,4])
boxplot(log(Z.rot[,5]))

# log fixes normality issue for trend loadings in 5, but issue will negative values
log(Z.rot+10)
# TD 2013 min = -6.46423

# pca
pca<-prcomp(log(Z.rot+10), scale=TRUE)
summary(pca)
# consider using 2 axis if enough cumulative proportion (aim for >75%)
pca.eig<-pca$sdev^2
pca.eigenval(pca)
screeplot(pca, bstick = TRUE)
ordi.monte(Z.rot, ord='pca', dim=5) #only PC1 significant (TD 2013)
#pca$eig[1:5]/sum(pca$eig)*100
pca$rotation
pca.eigenvec(pca, dim=5, digits = 3, cutoff = .1)
pca.structure(pca, Z.rot, dim = 5, cutoff = .4)
ordiplot(pca, choices = c(1,2))
biplot(pca)
# meta data for ploting

# from gabe
releases <- readRDS("/Users/pascalegoertler/Downloads/releases.rds")
# from jeremy
jsats <- read.csv("diversity_fish_metadata.csv")

# adding symbol for route
#colors<-c("#800000", "#a9a9a9", "#000075", "#4363d8", "#f58231")
pch<-c(21,22,23,24,25)
Route<-c("SacRiver", "cendel", "SacRS", "both", "Yolo_Bypass")
pch.rt<-data.frame(pch, Route)

grps<-read.csv("JSATS_CV_4DFA.csv")
grps<-merge(grps, pch.rt, by="Route")
jsats<-jsats[,c(2,6,13,16)]
jsats<-unique(jsats[,1:4])
grps<-merge(jsats, grps, by="FishID", all.y=TRUE)

# 2013 TD needs extra work bc Yolo fish dont have FishIDs
grps = grps[,c(1:5,11,12,14,15)]
# add MJ info
wateryr = 2013
Route = "Yolo_Bypass"
run="Fall"
FishID = c("X4842","X4843","X4844","X4845","X4847","X4848","X4849","X4850","X4851","X4854","X4855","X4857","X4858","X4859","X4861","X4862","X4863","X4864","X4865")
pch=25
River_temp = 14.8
Transport_distance = 296.16
Length = c(114,110,119,108,125,123,111,113,114,115,120,123,117,107,125,119,116,112,121)
Release_Group_v2 = "Tidal Delta"

yolo = data.frame(FishID, Length, River_temp, Transport_distance, Route, run, pch, wateryr, Release_Group_v2)
grps<- rbind(grps, yolo)

# bg for run
bg<-c("#800000", "#a9a9a9", "#000075", "#4363d8", "#f58231")
run<-c("Fall", "FallSpring", "Spring", "SpringW", "Winter")
bg.run<-data.frame(bg, run)
grps<-merge(grps, bg.run, by="run")

# gradient color for temp (could also try fish length)
grPal <- colorRampPalette(c("white","black"))
grps$col<-grPal(10)[as.numeric(cut(grps$River_temp, breaks=10))]

# double check order is the same between dataframes (after merging color)
FishID<-rownames(dat.z)

grps_16up<-grps_16up[order(grps_16up[,9], FishID),]
grps_13up<-grps_13up[order(grps_13up[,9], FishID),]
grps_17up<-grps_17up[order(grps_17up[,9], FishID),]# fine
grps_13md<-grps_13md[order(grps_13md[,9], FishID),]
grps_17md<-grps_17md[order(grps_17md[,9], FishID),]# fine

FishID<-rownames(dat.z) # didnt do a great job with naming...
grps_13td<-grps_13td[order(grps_13td[,2], FishID),]# fine

# final plots
grps_13td$bg<-as.character(grps_13td$bg)

pl <- ordiplot(pca, choices = c(1,2), type = "none")

points(pl, "sites", pch=grps_13td$pch, cex=grps_13td$Transport_distance/250, bg=grps_13td$col, col=grps_13td$bg)

points(pl, "sites", pch=grps_13td$pch, cex=grps_13td$Length/50, bg=grps_13td$col, col=grps_13td$bg, lwd=2)

text(pl, "species", col="blue", cex=0.9)
