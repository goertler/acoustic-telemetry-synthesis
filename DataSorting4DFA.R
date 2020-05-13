setwd("~/Documents/acoustic-telemetry-synthesis")

# 2017
grps<-read.csv("JSATS_CV_4DFA.csv")
grps_17<-subset(grps, wateryr==2017)
grps_17up<-subset(grps_17, Release_Group_v2=="Upper Sacramento River")
grps_17td<-subset(grps_17, Release_Group_v2=="Tidal Delta")
grps_17md<-subset(grps_17, Release_Group_v2=="Mid-Sacramento River")

#dat.17<-read.csv("JSATS17_DistTravelbyday.csv")
# new data - post TG/CM update
dat.17<-read.csv("JSATS17_DistTravelbyday_mat.csv")

NameList<-unique(grps_17md$FishID)
#NameList_v2 = paste("X",NameList, sep="") # no necessary now that we are using FishId instead of Tag
# need to replace "-" with "."
NameList_v2 = gsub('-', '.', NameList)
dat_17md <-dat.17[,colnames(dat.17)%in%NameList_v2] #101

NameList<-unique(grps_17up$FishID)
NameList_v2 = gsub('-', '.', NameList) 
dat_17up <-dat.17[,colnames(dat.17)%in%NameList_v2] #348

NameList<-unique(grps_17td$FishID)
NameList_v2 = gsub('-', '.', NameList)
dat_17td <-dat.17[,colnames(dat.17)%in%NameList] #99

# I was supposed to loose ~100 fish with latest QC... need to check NAs
sapply(dat_17up, function(x)all(is.na(x)))
sapply(dat_17md, function(x)all(is.na(x)))
sapply(dat_17td, function(x)all(is.na(x))) # there is something wrong here - all Mok2017### are gone now

dat_17md = as.data.frame(sapply(dat_17md, as.numeric)) 
dat_17up = as.data.frame(sapply(dat_17up, as.numeric))
#dat_17td = as.data.frame(sapply(dat_17td, as.numeric))

dat_17md<-dat_17md[rowSums(is.na(dat_17md)) != ncol(dat_17md), ] #55
dat_17up<-dat_17up[rowSums(is.na(dat_17up)) != ncol(dat_17up), ] #122
#dat_17td<-dat_17td[rowSums(is.na(dat_17td)) != ncol(dat_17td), ]

dat.t<-t(dat_17md)
dat.t<-t(dat_17up)
#dat.t<-t(dat_17td)

Sigma = sqrt(apply(dat.t, 1, var, na.rm=TRUE))
y.bar = apply(dat.t, 1, mean, na.rm=TRUE)
dat.z = (dat.t - y.bar) * (1/Sigma)
rownames(dat.z)=rownames(dat.t)

# need to run DynamicFactorAnalysis_TMB_UsingUnstructuredCorr.R first

mod1<-runDFA(obs=dat.z,NumStates=1,ErrStruc='DE') #33679.27(up), 2980.19(md)
mod2<-runDFA(obs=dat.z,NumStates=2,ErrStruc='DE') #31824.12(up), 2930.602
mod3<-runDFA(obs=dat.z,NumStates=3,ErrStruc='DE') #30530.39(up), 2858.807
mod4<-runDFA(obs=dat.z,NumStates=4,ErrStruc='DE') #29372.04(up), 2765.999
mod5<-runDFA(obs=dat.z,NumStates=5,ErrStruc='DE') #28123.34(up), 2663.01
mod6<-runDFA(obs=dat.z,NumStates=6,ErrStruc='DE') #27134.63(up), 2545.6
mod7<-runDFA(obs=dat.z,NumStates=7,ErrStruc='DE') #26341.15(up), 2308.023
mod8<-runDFA(obs=dat.z,NumStates=8,ErrStruc='DE') #25618.96(up), 2091.139
mod9<-runDFA(obs=dat.z,NumStates=9,ErrStruc='DE') #24801.12(up), 1828.876
mod10<-runDFA(obs=dat.z,NumStates=10,ErrStruc='DE') #23969.8(up), 1564.239
mod11<-runDFA(obs=dat.z,NumStates=11,ErrStruc='DE') #23044.02(up), 1187.336
mod12<-runDFA(obs=dat.z,NumStates=12,ErrStruc='DE') #22331.66(up), 551.8398
mod13<-runDFA(obs=dat.z,NumStates=13,ErrStruc='DE') #21221.41(up), -6.566599
mod14<-runDFA(obs=dat.z,NumStates=14,ErrStruc='DE') #20283.56(up), -1734.077
mod15<-runDFA(obs=dat.z,NumStates=15,ErrStruc='DE') #19104.11(up), -10175.76

#2013
grps_13<-subset(grps, wateryr==2013)
grps_13up<-subset(grps_13, Release_Group_v2=="Upper Sacramento River") #21
grps_13td<-subset(grps_13, Release_Group_v2=="Tidal Delta") #46
grps_13md<-subset(grps_13, Release_Group_v2=="Mid-Sacramento River") #13

dat.13<-read.csv("JSATS13_DistTravelbyday_mat.csv")

NameList<-unique(grps_13md$FishID)
NameList_v2 = gsub('-', '.', NameList)
dat_13md <-dat.13[,colnames(dat.13)%in%NameList_v2] #13

NameList<-unique(grps_13up$FishID)
NameList_v2 = gsub('-', '.', NameList) 
dat_13up <-dat.13[,colnames(dat.13)%in%NameList_v2] #21

NameList<-unique(grps_13td$FishID)
NameList_v2 = gsub('-', '.', NameList)
dat_13td <-dat.13[,colnames(dat.13)%in%NameList] #0 lost all td fish again...

write.csv(grps_13td, "tidalDelta2013.csv")
# need to add 19 from yolo to td

dat_13md = as.data.frame(sapply(dat_13md, as.numeric)) 
dat_13up = as.data.frame(sapply(dat_13up, as.numeric))

dat_13md<-dat_13md[rowSums(is.na(dat_13md)) != ncol(dat_13md), ] #33
dat_13up<-dat_13up[rowSums(is.na(dat_13up)) != ncol(dat_13up), ] #96

dat.t<-t(dat_13md)
dat.t<-t(dat_13up)

Sigma = sqrt(apply(dat.t, 1, var, na.rm=TRUE))
y.bar = apply(dat.t, 1, mean, na.rm=TRUE)
dat.z = (dat.t - y.bar) * (1/Sigma)
rownames(dat.z)=rownames(dat.t)

mod1<-runDFA(obs=dat.z,NumStates=1,ErrStruc='DE') #522.7845(md), 1324.039
mod2<-runDFA(obs=dat.z,NumStates=2,ErrStruc='DE') #522.9197(md), 1260.028
mod3<-runDFA(obs=dat.z,NumStates=3,ErrStruc='DE') #520.8782(md), 1268.495
mod4<-runDFA(obs=dat.z,NumStates=4,ErrStruc='DE') #523.2938(md), 1283.239
mod5<-runDFA(obs=dat.z,NumStates=5,ErrStruc='DE') #531.4604(md), 1354.936

# 2013 middle river release = 3 trends
# 2013 upper river release = 2 trends


