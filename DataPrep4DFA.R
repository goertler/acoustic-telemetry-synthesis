# 2017
grps<-read.csv("JSATS_CV_4DFA.csv")
grps_17<-subset(grps, wateryr==2017)
grps_17up<-subset(grps_17, Release_Group_v2=="Upper Sacramento River")
grps_17td<-subset(grps_17, Release_Group_v2=="Tidal Delta")
grps_17md<-subset(grps_17, Release_Group_v2=="Mid-Sacramento River")

dat.17<-read.csv("JSATS17_DistTravelbyday.csv")

NameList<-unique(grps_17md$TagID)
NameList_v2 = paste("X",NameList, sep="") 
dat_17md <-dat.17[,colnames(dat.17)%in%NameList_v2] 

NameList<-unique(grps_17up$TagID)
NameList_v2 = paste("X",NameList, sep="") 
dat_17up <-dat.17[,colnames(dat.17)%in%NameList_v2] 

NameList<-unique(grps_17td$TagID)
NameList_v2 = paste("X",NameList, sep="") 
dat_17td <-dat.17[,colnames(dat.17)%in%NameList_v2] 

dat_17md = as.data.frame(sapply(dat_17md, as.numeric)) 
dat_17up = as.data.frame(sapply(dat_17up, as.numeric))
dat_17td = as.data.frame(sapply(dat_17td, as.numeric))

dat_17md<-dat_17md[rowSums(is.na(dat_17md)) != ncol(dat_17md), ]
dat_17up<-dat_17up[rowSums(is.na(dat_17up)) != ncol(dat_17up), ]
dat_17td<-dat_17td[rowSums(is.na(dat_17td)) != ncol(dat_17td), ]

dat.t<-t(dat_17md)
dat.t<-t(dat_17up)
dat.t<-t(dat_17td)

Sigma = sqrt(apply(dat.t, 1, var, na.rm=TRUE))
y.bar = apply(dat.t, 1, mean, na.rm=TRUE)
dat.z = (dat.t - y.bar) * (1/Sigma)
rownames(dat.z)=rownames(dat.t)

mod1<-runDFA(obs=dat.z,NumStates=1,ErrStruc='DE') #
mod2<-runDFA(obs=dat.z,NumStates=2,ErrStruc='DE') #
mod3<-runDFA(obs=dat.z,NumStates=3,ErrStruc='DE') #
mod4<-runDFA(obs=dat.z,NumStates=4,ErrStruc='DE') #
mod5<-runDFA(obs=dat.z,NumStates=5,ErrStruc='DE') #
