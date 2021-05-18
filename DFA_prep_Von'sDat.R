# DFA with Von's data matrix
setwd("C:/Users/pgoertler/Desktop/acoustic-telemetry-synthesis/results")
dat.13<- read.csv("dfa_2013.csv")
dat.16<- read.csv("dfa_2016.csv")
dat.17<- read.csv("dfa_2017.csv")# need to transpose in Excel becuase R was adding an "x" in front of dates

head(dat.13)
head(dat.16)
head(dat.17)
length(unique(dat.13$FishID)) #1290 (that is all JSATs, should only be 76  
length(unique(dat.16$FishID)) #1713 (that is all JSATs, should only be 149  
length(unique(dat.17$FishID)) #1714 (that is all JSATs, should only be 442  

#### 2017 

# subset by those that are detected at Chips/Benicia and dont go backwards
setwd("C:/Users/pgoertler/Desktop/acoustic-telemetry-synthesis/")
sub <- read.csv("dat4Von.csv")
head(sub)

grps_17<-subset(sub, Year==2017) #442
grps_17up<-subset(grps_17, Release_Group_SAIL=="Upper Sacramento River")#337
grps_17md<-subset(grps_17, Release_Group_SAIL=="Middle Sacramento River")#50
grps_17td<-subset(grps_17, Release_Group_SAIL=="Tidal Delta")#55

setwd("C:/Users/pgoertler/Desktop/acoustic-telemetry-synthesis/results")
dat.17<- read.csv("dfa_2017_t.csv")# need to transpose in Excel becuase R was adding an "x" in front of dates

NameList<-unique(grps_17up$FishID)
NameList_v2 = gsub('-', '.', NameList)
dat_17up <-dat.17[,colnames(dat.17)%in%NameList_v2] #337


summary(dat_17up)
sapply(dat_17up, mean, na.rm = T)  
sapply(dat_17up, function(x)all(is.na(x)))
dat_17up = as.data.frame(sapply(dat_17up, as.numeric))
dat_17up<-dat_17up[rowSums(is.na(dat_17up)) != ncol(dat_17up), ]

dat.t<-t(dat_17up)

Sigma = sqrt(apply(dat.t, 1, var, na.rm=TRUE))
y.bar = apply(dat.t, 1, mean, na.rm=TRUE)
dat.z = (dat.t - y.bar) * (1/Sigma)
rownames(dat.z)=rownames(dat.t)

# Tim's DFA function 
source("DynamicFactorAnalysis_TMB_UsingUnstructuredCorr.R")

mod1<-runDFA(obs=dat.z,NumStates=1,ErrStruc='DE') #
mod2<-runDFA(obs=dat.z,NumStates=2,ErrStruc='DE') #
mod3<-runDFA(obs=dat.z,NumStates=3,ErrStruc='DE') #
mod4<-runDFA(obs=dat.z,NumStates=4,ErrStruc='DE') #
mod5<-runDFA(obs=dat.z,NumStates=5,ErrStruc='DE') #