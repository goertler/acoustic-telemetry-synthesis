# DFA with Von's data matrix
library(readr)
dfa_2013 <- read_csv("results/dfa_2013.csv")
dfa_2016 <- read_csv("results/dfa_2016.csv")
dfa_2017 <- read_csv("results/dfa_2017.csv")

length(unique(dfa_2013$FishID)) #90 (that is all JSATs, should only be 76  
length(unique(dfa_2016$FishID)) #149 (that is all JSATs, should only be 149  
length(unique(dfa_2017$FishID)) #452 (that is all JSATs, should only be 442  

#### 2017 

# subset by those that are detected at Chips/Benicia and dont go backwards
setwd("C:/Users/pgoertler/Desktop/acoustic-telemetry-synthesis/")
sub <- read.csv("dat4Von.csv")
head(sub)

grps_17<-subset(sub, Year==2017) #442
grps_17up<-subset(grps_17, Release_Group_SAIL=="Upper Sacramento River")#337
grps_17md<-subset(grps_17, Release_Group_SAIL=="Middle Sacramento River")#50
grps_17td<-subset(grps_17, Release_Group_SAIL=="Tidal Delta")#55

# hack to make my old code work... 
dat.17 <- t(dfa_2017)
header.true <- function(df) {
  colnames(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

dat.17 <- header.true (dat.17)

NameList<-unique(grps_17up$FishID)
dat_17up <-dat.17[,colnames(dat.17)%in%NameList] #337

ncol(dat_17up)#337

dat_17up <- data.frame(dat_17up)
summary(dat_17up)

sapply(dat_17up, function(x)all(is.na(x)))
dat_17up = as.data.frame(sapply(dat_17up, as.numeric))
sapply(dat_17up, mean, na.rm = TRUE)  
dat_17up<-dat_17up[rowSums(is.na(dat_17up)) != ncol(dat_17up), ]

# plot
matplot(y = dat_17up, type = 'l', lty = 1)

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

