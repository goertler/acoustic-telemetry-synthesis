#Files: 
setwd("C:/Users/pgoertler/Documents/fishtrackr")
dat.mj <- read.csv("chiptt_MJ.csv")
head(dat.mj)
dat.mj$TagType <- "Vemco"
dat.mj$FishID = paste(dat.mj$X, "MJ", sep = ".")
dat.mj$tt.grp <- "Chips"

setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections/YBUS/Acoustic.Telemetry.Data/DWR_UCD_Yolo.Bypass_Receivers 8-19-16")
dat.ybus <- read.csv("travel_time_YBUS.final.csv")
head(dat.ybus)
dat.ybus$TagType <- "Vemco"
dat.ybus$tt.grp <- "Chips"
dat.ybus$FishID = paste(dat.ybus$X, "YBUS", sep = ".")
colnames(dat.ybus)[13] <- "Route"

# need to update b/c water year was wrong
setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections")
dat.cm <- read.csv("travel.time_CM.Vemco_v3.csv")
head(dat.cm)
dat.cm$TagType <- "Vemco"
colnames(dat.cm)[10] <- "travel_time"

dat.jsats <- read.csv("travel.time.jsats.FIN_v2.csv") #added missing
head(dat.jsats)
dat.jsats$TagType <- "JSATS"
colnames(dat.jsats)[4] <- "travel_time"
colnames(dat.jsats)[1] <- "TagID"

# combine
dat.tt<-rbind(dat.mj[,c(2,6:10,15:18)], dat.ybus[,c(2,7:11,13:16)],
              dat.cm[,c(2,3,6:8,10,11,13,17,20)], dat.jsats[,c(1,2,4,5,7,8,13:16)])
write.csv(dat.tt, "dat.tt_v4.csv")
# summary for methods
length(unique(dat.tt$FishID)) #1814
summarise(group_by(dat.tt,Route), count=length(unique(FishID)))
#SacR                      1054
#SacRSlough                 268
#Yolo                       282
#CenDel                     173
#SacRSlough, CenDel          19
#Yolo, SacRSlough            17
#Yolo, SacRSlough, CenDel     1
summarise(group_by(dat.tt,TagType), count=length(unique(FishID)))
#JSATS     793
#Vemco    1021
summarise(group_by(dat.tt,Year), count=length(unique(FishID)))
#2007    5
#2008    81
#2009    58
#2010    48
#2011    104
#2012    30
#2013    109
#2014    34
#2015    92
#2016   811
#2017   442
summarise(group_by(dat.tt,tt.grp), count=length(unique(FishID)))
#Ben.Ant    32
#Ben.Dec   213
#Chips    1569

summary.table<-summarise(group_by(dat.tt,Route,Year,Release_Group_SAIL), count=length(unique(FishID)))
write.csv(summary.table, "summary.table_v3.csv")

#SD vs. water year
boxplot(travel_time~Year, data= dat.tt)

Year<-c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
WaterYear<-c("Dry", "Critically Dry", "Dry", "Below Normal", "Wet", "Below Normal", "Dry", "Critically Dry", "Critically Dry", "Below Normal", "Wet")

W.Y <- data.frame(Year, WaterYear)
dat.tt.yr <- merge(dat.tt, W.Y, by="Year")

dat.tt.yr$WaterYear = factor(dat.tt.yr$WaterYear, c("Critically Dry", "Dry", "Below Normal", "Wet"))

boxplot(travel_time~WaterYear, data= dat.tt.yr,
        ylab = "Travel Time (days)", xlab = "Sacramento Valley Water Year Index")

summary(aov(travel_time~WaterYear, data= dat.tt.yr))
summarise(group_by(dat.tt.yr,WaterYear), count=length(unique(FishID)))
#Critically Dry   207
#Dry              172
#Below Normal     889
#Wet              546

#summary.dat <- data.frame(aggregate(travel_time~Year, data= dat.tt.yr, 
#                         function (x) c(mean=mean(x), sd = sd(x))))

summary.dat <-summarise(group_by(dat.tt.yr, Year), tt.sd=sd(travel_time))

summary.dat.yr <- merge(summary.dat, W.Y, by="Year")
summary.dat.yr$WaterYear = factor(summary.dat.yr$WaterYear, c("Critically Dry", "Dry", "Below Normal", "Wet"))

tiff(filename="SDvs.WaterYear_v4.tif", units="in", bg="white", height=9, width =12, res=300)

boxplot(tt.sd ~ WaterYear, data = summary.dat.yr,
        ylab = "SD in Travel Time (days)", xlab = "Sacramento Valley Water Year Index")
dev.off()

plot(dat.tt.yr$WaterYear, dat.tt.yr$travel_time)

# significant?

mod<-aov(tt.sd~WaterYear, data=summary.dat.yr)
mod<-aov(travel_time~WaterYear, data=dat.tt.yr)
summary(mod)
TukeyHSD(mod)

# dates for covariate data
library(tidyverse)

#mj's data
dat.mj$releasetime <-as.Date(dat.mj$releasetime)
dat.mj$time <-as.Date(dat.mj$time)

mj.dates.min <- dat.mj %>% 
  group_by(Year) %>% 
  summarise(date_min = if(all(is.na(releasetime))) NA else min(releasetime, na.rm = TRUE), 
            date_min_na = is.na(date_min))
mj.dates.max <- dat.mj %>% 
  group_by(Year) %>% 
  summarise(date_max = if(all(is.na(time))) NA else max(time, na.rm = TRUE), 
            date_max_na = is.na(date_max))
#check
min(dat.mj$releasetime)#"2012-03-30"
max(dat.mj$time) #"2013-04-01"

#ybus
min(as.Date(dat.ybus$rel)) #"2016-02-21""
max(as.Date(dat.ybus$time.chip)) #"2016-04-02"

#cm's data
# not included in dat.cm, see Travel_Time_CM.R
summary(dat.vemco$time.rel)

#year is wrong
date <- as.character(unique(dat.vemco$time.rel))
Year <- c(2007,2007,2007,2007,2007,2007,2007,2007,2007,2007,
          2007,2007,2007,2007,2007,2008,2008,2008,2008,2008,
          2008,2009,2009,2009,2009,2009,2009,2010,2010,2010,
          2010,2010,2010,2010,2011,2011,2011,2011)
WR <- data.frame(date, Year)
WR$Year <- as.factor(WR$Year)
dat.vemco = unique(dat.vemco)
dat.vemco$date<-as.character(dat.vemco$time.rel)
dat.vemco_v2<-merge(dat.vemco, WR, by="date", all=TRUE, sort=FALSE)
head(dat.vemco_v2)

head(travel.time.dat) #from Travel)Time_CM.R

cm.07<-subset(travel.time.dat, Year==2007)
min(cm.07$time.rel) #"2007-01-15"
max(cm.07$time.max.ben, na.rm=TRUE) #"2007-02-28 01:00:27 -08"
max(cm.07$time.max.chip, na.rm=TRUE) #"2007-02-24 16:55:50 -08"
max(cm.07$time.max.antBr, na.rm=TRUE) #
max(cm.07$time.max.decIs, na.rm=TRUE) #"2007-02-21 00:20:25 -08"

cm.08<-subset(travel.time.dat, Year==2008)
min(cm.08$time.rel) #"2007-12-07 17:30:00 -08"
max(cm.08$time.max.ben, na.rm=TRUE) #"2008-03-04 06:50:30 -08"
max(cm.08$time.max.chip, na.rm=TRUE) #"2008-02-25 13:54:03 -08"
max(cm.08$time.max.antBr, na.rm=TRUE) #"2008-02-05 00:48:51 -08"
max(cm.08$time.max.decIs, na.rm=TRUE) #"2008-02-24 22:52:17 -08"

cm.09<-subset(travel.time.dat, Year==2009)
min(cm.09$time.rel) #"2008-12-13 17:45:00 -08"
max(cm.09$time.max.ben, na.rm=TRUE) #"2009-03-19 11:24:00 -08"
max(cm.09$time.max.chip, na.rm=TRUE) #"2009-02-26 23:47:11 -08"
max(cm.09$time.max.antBr, na.rm=TRUE) #
max(cm.09$time.max.decIs, na.rm=TRUE) #"2009-03-11 09:19:19 -08"

cm.10<-subset(travel.time.dat, Year==2010)
min(cm.10$time.rel) #"2009-12-15 17:59:00 -08"
max(cm.10$time.max.ben, na.rm=TRUE) #"2010-03-19 09:30:46 -08"
max(cm.10$time.max.chip, na.rm=TRUE) #"2010-03-18 07:36:52 -08"
max(cm.10$time.max.antBr, na.rm=TRUE) #
max(cm.10$time.max.decIs, na.rm=TRUE) #"2010-03-31 14:04:34 -08"

cm.11<-subset(travel.time.dat, Year==2011)
min(cm.11$time.rel) #"2010-12-09 08:00:00 -08"
max(cm.11$time.max.ben, na.rm=TRUE) #"2011-05-10 10:00:30 -08"
max(cm.11$time.max.chip, na.rm=TRUE) #"2011-05-10 01:02:56 -08"
max(cm.11$time.max.antBr, na.rm=TRUE) #
max(cm.11$time.max.decIs, na.rm=TRUE) #"2011-05-09 13:13:05 -08"

j.16<-subset(fin.chip, Year==2016)
summary(j.16) # 2016-02-17 18:00:00.00-2016-05-20 10:02:31.64
j.17<-subset(fin.chip, Year==2017)
summary(j.17) # 2017-02-02 17:30:00.00-2017-06-18 14:10:49.31
j.12<-subset(fin.ben, Year==2012)
summary(j.12)# 2012-04-19 08:40:00.00-2012-05-15 15:25:32.55
j.13<-subset(fin.ben, Year==2013)
summary(j.13)# 2013-02-07 17:30:00.00 -2013-05-17 08:28:15.34
j.14<-subset(fin.ben, Year==2014)
summary(j.14)#2014-02-10 17:30:00.00-2014-04-27 05:34:52.10 
j.15<-subset(fin.ben, Year==2015)
summary(j.15)#2015-02-04 17:30:00.00-2015-04-24 09:57:09.53
j.16.2<-subset(fin.ben, Year==2016)
summary(j.16.2)#2016-02-17 18:00:00 -2016-05-20 20:13:58.94  
j.17.2<-subset(fin.ben, Year==2017)
summary(j.17.2)#2017-02-02 17:30:00.00-2017-06-19 12:33:46.29

# discharge @ freport
setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/FPT")

dat.07<- read.csv("SACRAMENTO RIVER AT FREEPORT (FPT).csv")
dat.07$Year<-"2007"
dat.07$DATE...TIME..PST.<-as.Date(dat.07$DATE...TIME..PST.)
dat.08<- read.csv("SACRAMENTO RIVER AT FREEPORT (FPT) (1).csv")
dat.08$Year<-"2008"
dat.08$DATE...TIME..PST.<-as.Date(dat.08$DATE...TIME..PST.)
dat.09<- read.csv("SACRAMENTO RIVER AT FREEPORT (FPT) (2).csv")
dat.09$Year<-"2009"
dat.09$DATE...TIME..PST.<-as.Date(dat.09$DATE...TIME..PST.)
dat.10<- read.csv("SACRAMENTO RIVER AT FREEPORT (FPT) (3).csv")
dat.10$Year<-"2010"
dat.10$DATE...TIME..PST.<-as.Date(dat.10$DATE...TIME..PST.)
dat.11<- read.csv("SACRAMENTO RIVER AT FREEPORT (FPT) (4).csv")
dat.11$Year<-"2011"
dat.11$DATE...TIME..PST.<-as.Date(dat.11$DATE...TIME..PST.)
dat.12<- read.csv("SACRAMENTO RIVER AT FREEPORT (FPT) (5).csv")
dat.12$Year<-"2012"
dat.12$DATE...TIME..PST.<-as.Date(dat.12$DATE...TIME..PST.)
dat.13<- read.csv("SACRAMENTO RIVER AT FREEPORT (FPT) (6).csv")
dat.13$Year<-"2013"
dat.13$DATE...TIME..PST.<-as.Date(dat.13$DATE...TIME..PST.)
dat.14<- read.csv("SACRAMENTO RIVER AT FREEPORT (FPT) (7).csv")
dat.14$Year<-"2014"
dat.14$DATE...TIME..PST.<-as.Date(dat.14$DATE...TIME..PST.)
dat.15<- read.csv("SACRAMENTO RIVER AT FREEPORT (FPT) (8).csv")
dat.15$Year<-"2015"
dat.15$DATE...TIME..PST.<-as.Date(dat.15$DATE...TIME..PST.)
dat.16<- read.csv("SACRAMENTO RIVER AT FREEPORT (FPT) (9).csv")
dat.16$Year<-"2016"
dat.16$DATE...TIME..PST.<-as.Date(dat.16$DATE...TIME..PST.)
dat.17<- read.csv("SACRAMENTO RIVER AT FREEPORT (FPT) (10).csv")
dat.17 <- dat.17[,-3] # weird empty column...
dat.17$Year<-"2017"
dat.17$DATE...TIME..PST.<-as.Date(dat.17$DATE...TIME..PST.)

fpt.dat <- rbind(dat.07, dat.08, dat.09, dat.10, dat.11, dat.12, dat.13,
                 dat.14, dat.15, dat.16, dat.17)

head(fpt.dat)
str(fpt.dat)
unique(fpt.dat$FLOW.CFS) # symbols in a numeric dataset! 
fpt.dat$FLOW.CFS <- as.numeric(gsub(",","",fpt.dat$FLOW.CFS))

write.csv(fpt.dat, "dat.fpt.csv")

summary.fpt <-summarise(group_by(fpt.dat, Year), cfs.mean=mean(FLOW.CFS, na.rm = TRUE))

#setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections")
#travel.time<-read.csv("dat.tt_v2.csv")
#head(travel.time)

summary.tt <-summarise(group_by(dat.tt, Year), tt.sd=sd(travel_time))
summary.fpt.tt <- merge(summary.tt, summary.fpt, by="Year")
summary.fpt.tt$pch = c(15,16,17,18,25,7,9,10,11,12,14)
summary.fpt.tt$col = c("#a9a9a9", "#ffe119", "#a9a9a9", "#000000", "#4363d8", "#000000", "#a9a9a9", "#ffe119", "#ffe119", "#000000", "#4363d8")

tiff(filename="SDtravletimevs.CFS_v2.tif", units="in", bg="white", height=9, width =12, res=300)

plot(summary.fpt.tt$cfs.mean, summary.fpt.tt$tt.sd,
     ylab = "SD in Travel Time (days)", xlab = "Mean CFS at Freeport",
     pch=summary.fpt.tt$pch, col = summary.fpt.tt$col, cex=2)

legend("bottomright", legend = c("2007", "2008", "2009", "2010", 
                                 "2011", "2012", "2013", "2014", 
                                 "2015", "2016", "2017","Critically Dry", 
                                 "Dry", "Below Normal", "Wet"), 
       col =c("#000000","#000000","#000000","#000000","#000000",
              "#000000","#000000","#000000","#000000","#000000",
              "#000000","#ffe119","#a9a9a9","#000000", "#4363d8"), 
       pch = c(15,16,17,18,25,7,9,10,11,12,14,3,3,3,3), ncol=2)

dev.off()

# add temp @ FPT?
library(weathermetrics)
temp.10<-read.csv("FPT_Temp_10.csv")
temp.10$date<-as.Date(temp.10$date)
temp.10$temp<-fahrenheit.to.celsius(temp.10$VALUE, round = 2)
temp.10$Year<-"2010"

temp.11<-read.csv("FPT_Temp_11.csv")
temp.11$date<-as.Date(temp.11$date)
temp.11$temp<-fahrenheit.to.celsius(temp.11$VALUE, round = 2)
temp.11$Year<-"2011"

temp.12<-read.csv("FPT_Temp_12.csv")
temp.12$date<-as.Date(temp.12$date)
temp.12$temp<-fahrenheit.to.celsius(temp.12$VALUE, round = 2)
temp.12$Year<-"2012"

temp.13<-read.csv("FPT_Temp_13.csv")
temp.13$date<-as.Date(temp.13$date)
temp.13$temp<-fahrenheit.to.celsius(temp.13$VALUE, round = 2)
temp.13$Year<-"2013"

temp.14<-read.csv("FPT_Temp_14.csv")
temp.14$date<-as.Date(temp.14$date)
temp.14$temp<-fahrenheit.to.celsius(temp.14$VALUE, round = 2)
temp.14$Year<-"2014"

temp.15<-read.csv("FPT_Temp_15.csv")
temp.15$date<-as.Date(temp.15$date)
temp.15$temp<-fahrenheit.to.celsius(temp.15$VALUE, round = 2)
temp.15$Year<-"2015"

temp.17<-read.csv("FPT_Temp_17.csv")
temp.17$date<-as.Date(temp.17$date)
temp.17$temp<-fahrenheit.to.celsius(temp.17$VALUE, round = 2)
temp.17$Year<-"2017"

temp.fpt<-rbind(temp.10[,c(7,11,12)], temp.11[,c(7,11,12)],
                temp.12[,c(7,11,12)], temp.13[,c(7,11,12)],
                temp.14[,c(5,10,11)], temp.15[,c(7,11,12)],
                temp.17[,c(7,11,12)])
temp.fpt$Year<-as.factor(temp.fpt$Year)
summary.fpt.temp <-summarise(group_by(temp.fpt, Year), temp.mean=mean(temp, na.rm = TRUE))

summary.fpt.tt<-merge(summary.fpt.tt, summary.fpt.temp, by="Year", all.x=TRUE)
summary.fpt.tt[is.na(summary.fpt.tt)] <- 0

rbPal <- colorRampPalette(c('blue','red'))
summary.fpt.tt$Col <- rbPal(10)[as.numeric(cut(summary.fpt.tt$temp.mean,breaks = 10))]
summary.fpt.tt$PCH = c(15, 16, 15, 17, 18, 17, 15, 16, 16, 17, 18)

tiff(filename="SDtravletimevs.CFS&Temp.tif", units="in", bg="white", height=9, width =12, res=300)

plot(summary.fpt.tt$cfs.mean, summary.fpt.tt$tt.sd,
     ylab = "SD in Travel Time (days)", xlab = "Mean CFS at Freeport",
     pch=summary.fpt.tt$PCH, col=summary.fpt.tt$Col, cex=2, lwd=2)

legend("bottomright", legend = c("Critically Dry", 
                                 "Dry", "Below Normal", "Wet",
                                 "no data", "10.7-10.9°C", "13.2°C",
                                 "15.1-16.2°C"), 
       col =c("#000000","#000000","#000000","#000000","#0000FF",
              "#AA0055","#E2001C","#FF0000"), 
       pch = c(15,16,17,18,3,3,3,3), ncol=2)

dev.off()


# covariate plot
setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms")
dat.cov<-read.csv("USGS_FPT.csv")
head(dat.cov)
dat.cov$datetime<-as.Date(dat.cov$datetime)

max(dat.cov$Temp.Maximum, na.rm=TRUE)
min(dat.cov$Temp.Minimum, na.rm=TRUE)

max(dat.cov$Mean.Discharge.TidallyFiltered, na.rm = TRUE)
min(dat.cov$Mean.Discharge.TidallyFiltered, na.rm = TRUE)


tiff(filename="COV.tif", units="in", bg="white", height=9, width =12, res=300)

par(mar = c(4, 4, 0.5, 4))

plot(dat.cov$datetime, dat.cov$Temp.Maximum, col="#800000",
     ylim=c(-5,27), pch=6, ylab="Temperature", xlab="Date")
par(new=TRUE)
plot(dat.cov$datetime, dat.cov$Temp.Minimum, col="#e6194B",
     ylim=c(-5,27), pch=2, ylab="", xlab="")
par(new=TRUE)
plot(dat.cov$datetime,dat.cov$Mean.Discharge.TidallyFiltered,
     ylab="", xlab="", axes=FALSE, col="#000075")
mtext(text = "Mean Discharge (Tidally Filtered)",
      side = 4, line = 2)
axis(4, c(0,43000,86000))
legend("topleft", "SACRAMENTO RIVER, FREEPORT CA", cex=0.5, bty="n")

dev.off()

# final summary for methods/results
setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections")
dat.t<-read.csv("dat.tt_v4.csv")

head(dat.t)
str(dat.t)
length(unique(dat.t$FishID))#1814
library(dplyr)
summarise(group_by(dat.t,tt.grp,TagType), count=length(unique(FishID)))
summarise(group_by(dat.t,TagType), count=length(unique(FishID)))
summary(aov(travel_time~TagType, data= dat.t)) #Pr(>F)=<2e-16 ***
TukeyHSD(aov(travel_time~tt.grp, data= dat.t))
#                      diff       lwr       upr     p adj
#Ben.Dec-Ben.Ant  0.9235339 -5.583212 7.4302798 0.9407356
#Chips-Ben.Ant   -1.0302768 -7.158785 5.0982318 0.9178722
#Chips-Ben.Dec   -1.9538108 -4.459909 0.5522879 0.1605060

summarise(group_by(dat.t,Route,TagType), count=length(unique(FishID)))
dat.t$Year<-as.factor(dat.t$Year)
summary(aov(travel_time~Year, data= dat.t)) #<2e-16
TukeyHSD(aov(travel_time~Year, data= dat.t)) 

Year<-c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
WaterYear<-c("Dry", "Critically Dry", "Dry", "Below Normal", "Wet", "Below Normal", "Dry", "Critically Dry", "Critically Dry", "Below Normal", "Wet")
W.Y <- data.frame(Year, WaterYear)
dat.t.yr <- merge(dat.t, W.Y, by="Year")

summary(aov(travel_time~WaterYear, data= dat.t.yr)) #<2e-16
TukeyHSD(aov(travel_time~WaterYear, data= dat.t.yr))

#                                diff       lwr        upr     p adj
#Critically Dry-Below Normal 10.752902  8.233996 13.2718084 0.0000000
#Dry-Below Normal             8.266253  5.547402 10.9851034 0.0000000
#Wet-Below Normal            16.574983 14.800297 18.3496686 0.0000000
#Dry-Critically Dry          -2.486650 -5.854191  0.8808922 0.2288720
#Wet-Critically Dry           5.822080  3.157930  8.4862310 0.0000001
#Wet-Dry                      8.308730  5.454792 11.1626674 0.0000000

summarise(group_by(dat.t,Release_Group_SAIL), count=length(unique(FishID)))
summarise(group_by(dat.t,Release_Group_SAIL,TagType), count=length(unique(FishID)))

summary(aov(travel_time~Release_Group_SAIL, data= dat.t)) #<2e-16
TukeyHSD(aov(travel_time~Release_Group_SAIL, data= dat.t))#all significant

route.sum<-summarise(group_by(dat.t,Release_Group_SAIL, Route, Year), count=length(unique(FishID)))
write.csv(route.sum, "routing.summary_21.csv")
von<-summarise(group_by(dat.t,tt.grp,TagType,Year), count=length(unique(FishID)))

selected<-c("2013","2016","2017")
dat4Von<-dat.t[dat.t$Year %in% selected,]
write.csv(dat4Von, "dat4Von.csv")

# need to run through Zuur diagnostics with travel time
# go to Diagnostics_TT.R

id.dat<-read.csv("dat4Von.csv")
id.dat<-id.dat[,c(3,10,11)]
write.csv(id.dat, "IDs4Von.csv")

summary.dat <-summarise(group_by(dat.t.yr, Year), tt.sd=sd(travel_time))

summary.dat.yr <- merge(summary.dat, W.Y, by="Year")

boxplot(tt.sd ~ WaterYear, data = summary.dat.yr,
        ylab = "SD in Travel Time (days)", xlab = "Sacramento Valley Water Year Index")

summary(aov(tt.sd~WaterYear, data= summary.dat.yr)) #0.286
TukeyHSD(aov(tt.sd~WaterYear, data= summary.dat.yr))

