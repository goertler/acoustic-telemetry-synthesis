# load data
setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections")

dat<-read.csv("dat.tt_v4.csv")
head(dat)
str(dat)

Year<-c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
WaterYear<-c("Dry", "Critically Dry", "Dry", "Below Normal", "Wet", "Below Normal", "Dry", "Critically Dry", "Critically Dry", "Below Normal", "Wet")
W.Y <- data.frame(Year, WaterYear)
dat <- merge(dat[,-1], W.Y, by="Year")

setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms")
buff<-read.csv("buffering.cv.chips_v5.csv")
head(buff)

#############################################################################################
### code from Zuur 2010 
### Outliers
par(mfrow= c (1,2), mar = c(5,4,2,1))
boxplot(dat$travel_time)
dotchart(dat$travel_time)

## conclusion - NO

boxplot(buff$CV)
dotchart(buff$CV)
max(buff$CV, na.rm=TRUE)#0.9514288
min(buff$CV, na.rm=TRUE)#0.003637925

## conclusion - NO

################################################################################################
### Homogeneity
library(lattice)

bwplot(travel_time ~ Group | origin, data = dat,
       strip = strip.custom(bg = 'white'),
       cex = .5, layout = c(3, 1),
       par.settings = list(
         box.rectangle = list(col = 1),
         box.umbrella  = list(col = 1),
         plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))
## conclusion? - not great
summarise(group_by(dat,origin), count=length(unique(FishID)))

bwplot(travel_time ~ tt.grp | TagType, data = dat,
       strip = strip.custom(bg = 'white'),
       cex = .5, layout = c(2, 1),
       par.settings = list(
         box.rectangle = list(col = 1),
         box.umbrella  = list(col = 1),
         plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))

## conclusion? - ok

bwplot(travel_time ~ Year | Release_Group_SAIL, data = dat,
       strip = strip.custom(bg = 'white'),
       cex = .5, layout = c(3, 1),
       par.settings = list(
         box.rectangle = list(col = 1),
         box.umbrella  = list(col = 1),
         plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))

## conclusion? - ok for middle and upper, but not tidal

bwplot(travel_time ~ Route | Release_Group_SAIL, data = dat,
       strip = strip.custom(bg = 'white'),
       cex = .5, layout = c(3, 1),
       par.settings = list(
         box.rectangle = list(col = 1),
         box.umbrella  = list(col = 1),
         plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))

## conclusion? - not great

bwplot(travel_time ~ Route | WaterYear, data = dat,
       strip = strip.custom(bg = 'white'),
       cex = .5, layout = c(4, 1),
       par.settings = list(
         box.rectangle = list(col = 1),
         box.umbrella  = list(col = 1),
         plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))

## conclusion? - ok
buff$Year <- as.factor(buff$Year)

bwplot(CV ~ Year | Release_Group_SAIL, data = buff,
       strip = strip.custom(bg = 'white'),
       cex = .5, layout = c(4, 1),
       par.settings = list(
          box.rectangle = list(col = 1),
          box.umbrella  = list(col = 1),
          plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))

## conclusion? - matches travel time

bwplot(CV ~ Length.Route | Release_Group_SAIL, data = buff,
       strip = strip.custom(bg = 'white'),
       cex = .5, layout = c(4, 1),
       par.settings = list(
          box.rectangle = list(col = 1),
          box.umbrella  = list(col = 1),
          plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))
## conclusion? - ok

##########################################################################################
###Normal distribution?

hist(dat$travel_time,
     breaks = 30,
     main = "", ylab = "Frequency")

## conclusion? - NO, poisson distribution

histogram( ~ travel_time | WaterYear, type = "count",
           ylab = "Frequency",
           nint=30,layout=c(1,4),
           strip.left = strip.custom(bg = 'white'),
           strip = F,
           col.line = "black", col = "white",
           scales = list(x = list(relation = "same"),
                         y = list(relation = "same"),
                         draw = TRUE),
           subset = WaterYear =="Critically Dry" | WaterYear == "Dry" |WaterYear == "Below Normal"| WaterYear =="Wet",
           data = dat)

## conclusion? - wet is Multimodal, 2011 and 2017 seem to be very different

histogram( ~ travel_time | Release_Group_SAIL, type = "count",
           ylab = "Frequency",
           nint=30,layout=c(1,3),
           strip.left = strip.custom(bg = 'white'),
           strip = F,
           col.line = "black", col = "white",
           scales = list(x = list(relation = "same"),
                         y = list(relation = "same"),
                         draw = TRUE),
           subset = Release_Group_SAIL =="Upper Sacramento River" | Release_Group_SAIL == "Middle Sacramento River" |Release_Group_SAIL == "Tidal Delta",
           data = dat)

## conclusion? - upper is Multimodal

histogram( ~ travel_time | TagType, type = "count",
           ylab = "Frequency",
           nint=30,layout=c(1,2),
           strip.left = strip.custom(bg = 'white'),
           strip = F,
           col.line = "black", col = "white",
           scales = list(x = list(relation = "same"),
                         y = list(relation = "same"),
                         draw = TRUE),
           subset = TagType =="Vemco" | TagType == "JSATS",
           data = dat)

## conclusion? - JSATS is Multimodal

hist(buff$CV,
     breaks = 30,
     main = "", ylab = "Frequency")

## conclusion? - YES

##########################################################################################
### Zeros
min(dat$travel_time) # 1.76162
min(buff$CV, na.rm=TRUE)#0.003637925

## conclusion - not an issue for this dataset

##########################################################################################
###Collinearity

library(car)

## conclusion - all variables are categorical - will test if use discharge or temperature data

##########################################################################################
###Relationships between Y and X variables

#Figure 10

Sparrows <- read.table(file = "SparrowsElphick.txt", header = TRUE)
source(file = "HighstatLib.R")
MyNames <- c("wing chord", "tarsus length", "head length",
             "culmen length", "nalospi to bill tip", "weightt")
pairs(Sparrows[,c(1, 3, 4, 5, 6, 7)],
      lower.panel = panel.cor,
      cex.labels=1.3,
      labels=MyNames)

# step 6?

pairs(buff[,c(5:10)])

plot(buff$CV, buff$count)
plot(buff$CV, buff$mean)
plot(buff$CV, buff$Length.Route)

# removed polled CV
buff.1 <- subset(buff, Length.Route==1)
pairs(buff.1[,c(5:10)])

plot(buff.1$CV, buff.1$count) # no clear relationship, but one group with much higher n
hist(buff.1$count)
plot(buff.1$CV, buff.1$mean) # looks random
hist(buff.1$CV) # still normal

mod = lm(CV ~ Length.Route + count + mean, data = buff)
summary(mod)
vif(mod) # less than three
#Length.Route        count         mean 
#1.664093     1.489445     1.202379 

## conclusion?
# fine

##########################################################################################
###Interactions

#Figure 11
Sparrows <- read.table(file = "SparrowsElphick.txt", header = TRUE)

#Take the data from species 1, Sex = 0 and Wing length >= 65
I1 <- Sparrows$SpeciesCode == 1 & Sparrows$Sex != "0" & Sparrows$wingcrd < 65
Wing1<- Sparrows$wingcrd[I1]
Wei1 <- Sparrows$wt[I1]
Mon1 <- factor(Sparrows$Month[I1])
Sex1<- factor(Sparrows$Sex[I1])


#Define Month and Sex as categorical variables
fMonth1 <- factor(Mon1,levels=c(5,6,7,8,9),
                  labels=c("May","Jun","Jul","Aug","Sep"))
fSex1   <- factor(Sex1, levels=c(4,5),labels=c("Male","Female"))



#Make the coplot
coplot(Wei1 ~ Wing1 | fMonth1 * fSex1, ylab = "Weight (g)",
       xlab = "Wing length (mm)",
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

# step 7?
buff$Release_Group_SAIL <- as.factor(buff$Release_Group_SAIL)
buff.na<-na.omit(buff)
coplot(CV ~ Length.Route | Year * Release_Group_SAIL, 
       data = buff.na,
       panel = function(x, y, ...) {
          tmp <- lm(y ~ x, na.action = na.omit)
          abline(tmp)
          points(x, y) })
coplot(CV ~ Length.Route | Year * Release_Group_SAIL, 
       data = buff.na)

## conclusion?
# buffering pattern seems consistant across years and rel groups,
# although sample size is not balanced

##########################################################################################
###Independence

par(mfrow = c(2, 1), mar = c(5, 4, 3, 2))
plot(dat$Year,dat$travel_time, type = "l", xlab = "Time (Year)")
acf(dat$travel_time, main = "ACF")

par(mfrow = c(2, 1), mar = c(5, 4, 3, 2))
plot(buff.na$Year,buff.na$CV, type = "l", xlab = "Time (Year)")
acf(buff.na$CV, main = "ACF")

plot(buff.na$X,buff.na$CV, type = "l", xlab = "Time (Year)")
acf(buff.na$CV, main = "ACF")

## conclusion - may want to add day of release, although that will likely confound with run

# may want to consider more categorical varibales? 
library(dplyr)
summarise(group_by(buff,Length.Route), max=max(CV, na.rm=TRUE))
summarise(group_by(buff,Length.Route), min=min(CV, na.rm=TRUE))

buff.1$Loop.ID<-as.factor(as.character(buff.1$Loop.ID))
plot(buff.1$Loop.ID, buff.1$CV)
str(buff.1)

plot(dat$Route, dat$travel_time)
summary(aov(travel_time~Route, data= dat))
TukeyHSD(aov(travel_time~Route, data= dat))
rtbygrp<-summarise(group_by(dat,Route,Group,Year,Release_Group_SAIL), mean=mean(travel_time, na.rm=TRUE))

# boxplot
str(dat)
col.dat<-unique(dat[,c(5,6)])
col<-c("#800000","#800000","#800000","#800000","#800000","#800000",
       "#800000","#800000","#f58231","#f58231","#f58231","#ffe119",
       "#f58231","#dcbeff","#ffe119","#f58231","#ffe119","#ffe119","#000075","#000075",
       "#000075","#000075","#000075","#ffe119","#a9a9a9")
col4dat<-data.frame(col.dat,col)
dat4boxplot<-merge(dat, col4dat, by="Group")

library(ggplot2)
ggplot(dat4boxplot, aes(Route, travel_time)) + 
   geom_boxplot(aes(colour = col)) + 
   facet_grid(Release_Group_SAIL~.)

boxplot(travel_time~Route, data = dat4boxplot,
        col=dat4boxplot$col)

# Violin Plots
library(vioplot)
x1 <- dat4boxplot$travel_time[dat4boxplot$Route=="SacR"]
x2 <- dat4boxplot$travel_time[dat4boxplot$Route=="CenDel"]
x3 <- dat4boxplot$travel_time[dat4boxplot$Route=="SacRSlough"]
x4 <- dat4boxplot$travel_time[dat4boxplot$Route=="Yolo"]
x5 <- dat4boxplot$travel_time[dat4boxplot$Route=="Yolo, SacRSlough"]
x6 <- dat4boxplot$travel_time[dat4boxplot$Route=="SacRSlough, CenDel"]
x7 <- dat4boxplot$travel_time[dat4boxplot$Route=="Yolo, SacRSlough, CenDel"]

vioplot(x1, x2, x3, x4, x5, x6, x7, 
        names=c("Sacramento River", "Central Delta", 
                "Sac. R. Sloughs", "Yolo Bypass", "Sloughs + Bypass",
                "Sloughs + Central Delta", "All"),
        col="gold")
title("Violin Plots of time per route")