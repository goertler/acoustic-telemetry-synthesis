# plot results
# trial with 2013-upper river release - best model = 2 trends

# 2013-upper river release - best model = 2 trends
# 2013-mid river release - best model = 4 trends
# 2016-mid river release - best model = 4 trends

# ordination
# from cutom_Dmatrix_for_DFA.R
library(vegan)

ordiplot((mod3$Estimates$Z), choices=c(1,2))#, type="text", display="fish.ID")
arrows(0,0,mod3$Estimates$Z[1,],mod3$Estimates$Z[2,])#,Z.rot[,4],Z.rot[,5],Z.rot[,6],Z.rot[,7],Z.rot[,8],Z.rot[,9])
text(mod3$Estimates$Z[,1],mod3$Estimates$Z[,2], row.names(dat.z), col=grps_13up$mo_arrive) # fish ID and month of arrival to estuary

# for more than one trend
#ZmatFactorGen(Data=dat.z,NumStates=2) #all done internally
#ZmatGen(Data=dat.z,NumStates=2)
#H.inv = varimax(mod2$Estimates$Z)$rotmat
#Z.rot = mod2$Estimates$Z %*% H.inv #maximum variance explained
#trends.rot = solve(H.inv) %*% t(mod2$Estimates$u)

N.ts = dim(dat.z)[1]
TT = dim(dat.z)[2]
N.trends=4
minZ = 0
ylims = c(-1.1*max(abs(mod4$Estimates$Z)), 1.1*max(abs(mod4$Estimates$Z)))
Z.rot = mod4$Estimates$Z
row.names(Z.rot) = row.names(dat.z)

par(mfrow=c(4,1))
# loadings
for(i in 1:N.trends) {
  plot(c(1:N.ts)[abs(Z.rot[,i])>minZ], as.vector(Z.rot[abs(Z.rot[,i])>minZ,i]),
       type="h", lwd=4, xlab="", ylab="", xaxt="n", ylim=ylims, xlim=c(0,N.ts+1),
       col=as.numeric(grps_16up$Route))
  for(j in 1:N.ts) {
    if(Z.rot[j,i] > minZ) {text(j, -0.05, srt=90, adj=1, cex=0.9, col="black")}
    abline(h=0, lwd=1, col="gray")
  } # end j loop
  mtext(paste("Factor loadings on trend",i,sep=" "),side=3,line=.5)
} # end i loop

# getting fish IDs
#png(filename = "2013_upper_loadings.png", width = 8, height = 8, units = "in", pointsize = 12, bg = "white", res = 350)
#png(filename = "2013_mid_loadings.png", width = 8, height = 8, units = "in", pointsize = 12, bg = "white", res = 350)
#png(filename = "2016_upper_loadings.png", width = 8, height = 8, units = "in", pointsize = 12, bg = "white", res = 350)
#png(filename = "2017_upper_loadings.png", width = 8, height = 12, units = "in", pointsize = 12, bg = "white", res = 350)

# adding color for route
colors<-c("#800000", "#a9a9a9", "#000075", "#4363d8", "#f58231")
Route<-c("SacRiver", "cendel", "SacRS", "both", "Yolo_Bypass")
col.rt<-data.frame(colors, Route)

grps<-read.csv("JSATS_CV_4DFA.csv")
grps<-merge(grps, col.rt, by="Route")
grps$colors<-as.character(grps$colors)
# double check order is the same between dataframes (after merging color)
FishID<-rownames(dat.z)
ID<-data.frame(FishID,1)
grps_16up<-grps_16up[order(grps_16up[,9], FishID),]

# final loading figure
png(filename = "2016_up_loadings.png", width = 8, height = 12, units = "in", pointsize = 12, bg = "white", res = 350)

par(mfrow=c(4,1))
for(i in 1:N.trends) {
  plot(c(1:N.ts)[abs(Z.rot[,i])>minZ], as.vector(Z.rot[abs(Z.rot[,i])>minZ,i]),
       type="h", lwd=4, xlab="", ylab="", xaxt="n", ylim=ylims, xlim=c(0,N.ts+1),
       col=grps_16up$colors)
  for(j in 1:N.ts) {
    #browser()
    if(Z.rot[j,i] > minZ) {text(j, -0.05, labels = row.names(Z.rot)[j], srt=90, adj=1, cex=0.5, col="black")}
    abline(h=0, lwd=1, col="gray")
  } # end j loop
  mtext(paste("Factor loadings on trend",i,sep=" "),side=3,line=.5)
} # end i loop

dev.off()

# trends
#png(filename = "2013_upper_trends.png", width = 8, height = 8, units = "in", pointsize = 12, bg = "white", res = 350)
#png(filename = "2013_mid_trends.png", width = 8, height = 8, units = "in", pointsize = 12, bg = "white", res = 350)
#png(filename = "2016_upper_trends.png", width = 8, height = 8, units = "in", pointsize = 12, bg = "white", res = 350)
#png(filename = "2017_upper_trends.png", width = 8, height = 12, units = "in", pointsize = 12, bg = "white", res = 350)
png(filename = "2016_up_trends.png", width = 8, height = 12, units = "in", pointsize = 12, bg = "white", res = 350)


par(mfrow=c(4,1))
for(i in 1:dim(t(mod4$Estimates$u))[2]) {
  # set up plot area
  plot(t(mod4$Estimates$u)[,i],
       ylim=c(-1.1,1.1)*max(abs(t(mod4$Estimates$u))), 
       type="n", lwd=2, bty="L", 
       xlab="", ylab="", xaxt="n", yaxt="n")
  # draw zero-line
  abline(h=0, col="gray")
  # plot trend line
  par(new=TRUE)
  plot(t(mod4$Estimates$u)[,i],
       ylim=c(-1.1,1.1)*max(abs(t(mod4$Estimates$u))), 
       type="l", lwd=2, bty="L", 
       xlab="", ylab="", xaxt="n")
  # add panel labels
  mtext(paste("Trend",i,sep=" "), side=3, line=0.5)
  axis(1,12*(0:dim(dat.z)[2]):dim(dat.z)[2]) # writes days on x-axis
} # end i loop (trends)


dev.off()
