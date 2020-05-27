setwd("~/Documents/acoustic-telemetry-synthesis")

#### 2017
grps<-read.csv("JSATS_CV_4DFA.csv")
grps_17<-subset(grps, wateryr==2017)
grps_17up<-subset(grps_17, Release_Group_v2=="Upper Sacramento River")
#grps_17td<-subset(grps_17, Release_Group_v2=="Tidal Delta") # no longer running these - all Mok fish
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

#NameList<-unique(grps_17td$FishID)
#NameList_v2 = gsub('-', '.', NameList)
#dat_17td <-dat.17[,colnames(dat.17)%in%NameList] #99

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

# tim reccomended different error structure:
mod1<-runDFA(obs=dat.z,NumStates=1,ErrStruc='DUE') #(up)17708.02, (md)2377.986
mod2<-runDFA(obs=dat.z,NumStates=2,ErrStruc='DUE') #(up)4305.22, (md)2340.518
mod3<-runDFA(obs=dat.z,NumStates=3,ErrStruc='DUE') #(up)7804.498, (md)2268.015
mod4<-runDFA(obs=dat.z,NumStates=4,ErrStruc='DUE') #(up)3968.671, (md)2404.455
mod5<-runDFA(obs=dat.z,NumStates=5,ErrStruc='DUE') #(up)7483.714, (md)

#### 2013
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
yolo.13<-read.csv("Yolo13_DistTravelbyday.csv") #19
#colnames(dat_13yolo)[colnames(dat_13yolo) == 'dat.13all[, 1]'] <- 'Date'
#dat_13td<-merge(dat_13td, dat_13yolo, by="Date", all=TRUE)

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

mod1<-runDFA(obs=dat.z,NumStates=1,ErrStruc='DUE') #(md) 34.9934, (up) 1294.592
mod2<-runDFA(obs=dat.z,NumStates=2,ErrStruc='DUE') #(md) -90.40034, (up) 1401.51
mod3<-runDFA(obs=dat.z,NumStates=3,ErrStruc='DUE') #(md) 467.3408, (up) 1352.82
mod4<-runDFA(obs=dat.z,NumStates=4,ErrStruc='DUE') #(md) -187.5152, (up) 1079.169
mod5<-runDFA(obs=dat.z,NumStates=5,ErrStruc='DUE') #(md) -283.2182, (up) 954.0951

#### 2016
grps_16<-subset(grps, wateryr==2016)
grps_16up<-subset(grps_16, Release_Group_v2=="Upper Sacramento River") #134
grps_16md<-subset(grps_16, Release_Group_v2=="Mid-Sacramento River") #15

dat.16<-read.csv("JSATS16_DistTravelbyday_mat.csv")

# need to sort out ybus data (all td and add to md)
grps_yb<-read.csv("ybusCV.csv")
grps_16td<-subset(grps_yb, Release_Group=="Tidal Delta")
grps_16md.yb<-subset(grps_yb, Release_Group=="Middle Sacramento River")

dat.16<-read.csv("JSATS16_DistTravelbyday.csv")
dat.16.yb<-read.csv("YBUS16_DistTravelbyday.csv")

NameList<-unique(grps_16up$FishID)
NameList_v2 = gsub('-', '.', NameList) 
dat_16up <-dat.16[,colnames(dat.16)%in%NameList_v2] #134

dat_16up = as.data.frame(sapply(dat_16up, as.numeric))

dat_16up<-dat_16up[rowSums(is.na(dat_16up)) != ncol(dat_16up), ] #75

dat.t<-t(dat_16up)

Sigma = sqrt(apply(dat.t, 1, var, na.rm=TRUE))
y.bar = apply(dat.t, 1, mean, na.rm=TRUE)
dat.z = (dat.t - y.bar) * (1/Sigma)
rownames(dat.z)=rownames(dat.t)

mod1<-runDFA(obs=dat.z,NumStates=1,ErrStruc='DE') #7690.899
mod2<-runDFA(obs=dat.z,NumStates=2,ErrStruc='DE') #7279.164
mod3<-runDFA(obs=dat.z,NumStates=3,ErrStruc='DE') #7033.348
mod4<-runDFA(obs=dat.z,NumStates=4,ErrStruc='DE') #6909.009
mod5<-runDFA(obs=dat.z,NumStates=5,ErrStruc='DE') #6819.389
mod6<-runDFA(obs=dat.z,NumStates=6,ErrStruc='DE') #6748.028
mod7<-runDFA(obs=dat.z,NumStates=7,ErrStruc='DE') #6686.242
mod8<-runDFA(obs=dat.z,NumStates=8,ErrStruc='DE') #6603.917
mod9<-runDFA(obs=dat.z,NumStates=9,ErrStruc='DE') #6488.887
mod10<-runDFA(obs=dat.z,NumStates=10,ErrStruc='DE') #6339.859
mod11<-runDFA(obs=dat.z,NumStates=11,ErrStruc='DE') #6211.851
mod12<-runDFA(obs=dat.z,NumStates=12,ErrStruc='DE') #6030.455
mod13<-runDFA(obs=dat.z,NumStates=13,ErrStruc='DE') #5876.904
mod14<-runDFA(obs=dat.z,NumStates=14,ErrStruc='DE') #5651.252
mod15<-runDFA(obs=dat.z,NumStates=15,ErrStruc='DE') #5381.767

