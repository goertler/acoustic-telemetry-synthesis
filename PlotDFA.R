# plot results
# trial with 2013-upper river release - best model = 2 trends

# ordination
# from cutom_Dmatrix_for_DFA.R
library(vegan)

ordiplot((mod2$Estimates$Z), choices=c(1,2))#, type="text", display="fish.ID")
arrows(0,0,mod2$Estimates$Z[1,],mod2$Estimates$Z[2,])#,Z.rot[,4],Z.rot[,5],Z.rot[,6],Z.rot[,7],Z.rot[,8],Z.rot[,9])
text(mod2$Estimates$Z[,1],mod2$Estimates$Z[,2], row.names(dat.z), col=grps_13up$mo_arrive) # fish ID and month of arrival to estuary

# for more than one trend
ZmatFactorGen(Data=dat.z,NumStates=2)
ZmatGen(Data=dat.z,NumStates=2)
#H.inv = varimax(mod2$Estimates$Z)$rotmat
#Z.rot = mod2$Estimates$Z %*% H.inv #maximum variance explained
#trends.rot = solve(H.inv) %*% t(mod2$Estimates$u)

N.ts = dim(dat.t)[1]
TT = dim(dat.t)[2]
N.trends=2
minZ = 0
ylims = c(-1.1*max(abs(Z.rot)), 1.1*max(abs(Z.rot)))

par(mfrow=c(2,1))
# loadings
for(i in 1:N.trends) {
  plot(c(1:N.ts)[abs(Z.rot[,i])>minZ], as.vector(Z.rot[abs(Z.rot[,i])>minZ,i]),
       type="h", lwd=4, xlab="", ylab="", xaxt="n", ylim=ylims, xlim=c(0,N.ts+1))
  for(j in 1:N.ts) {
    if(Z.rot[j,i] > minZ) {text(j, -0.05, srt=90, adj=1, cex=0.9, col="black"[j])}
    abline(h=0, lwd=1, col="gray")
  } # end j loop
  mtext(paste("Factor loadings on trend",i,sep=" "),side=3,line=.5)
} # end i loop

# trends
for(i in 1:dim(t(mod2$Estimates$u))[2]) {
  # set up plot area
  plot(t(mod2$Estimates$u)[,i],
       ylim=c(-1.1,1.1)*max(abs(t(mod2$Estimates$u))), 
       type="n", lwd=2, bty="L", 
       xlab="", ylab="", xaxt="n", yaxt="n")
  # draw zero-line
  abline(h=0, col="gray")
  # plot trend line
  par(new=TRUE)
  plot(t(mod2$Estimates$u)[,i],
       ylim=c(-1.1,1.1)*max(abs(t(mod2$Estimates$u))), 
       type="l", lwd=2, bty="L", 
       xlab="", ylab="", xaxt="n")
  # add panel labels
  mtext(paste("Trend",i,sep=" "), side=3, line=0.5)
  axis(1,12*(0:dim(dat.z)[2]):dim(dat.z)[2]) # writes days on x-axis
} # end i loop (trends)



