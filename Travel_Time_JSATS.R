setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections")

# 29 JSATS datasets
w.17<-read.csv("JSATS_Filter_Winter_H_2017_20200115_1129.csv")
sb.17<-read.csv("JSATS_Filter_SB_Spring_2017_20200115_1129.csv")
w.13<-read.csv("JSATS_Filter_Winter_H_2013_20200115_1129.csv")
w.14<-read.csv("JSATS_Filter_Winter_H_2014_20200115_1129.csv")
w.15<-read.csv("JSATS_Filter_Winter_H_2015_20200115_1129.csv")
w.16<-read.csv("JSATS_Filter_Winter_H_2016_20200115_1129.csv")
m.16<-read.csv("JSATS_Filter_MillCk_Wild_2016_20200115_1129.csv")
d.16<-read.csv("JSATS_Filter_MillCk_Wild_2016_DS_20200115_1129.csv")
m.17<-read.csv("JSATS_Filter_MillCk_Wild_2017_20200115_1129.csv")
n.16<-read.csv("JSATS_Filter_Nimbus_Fall_2016_20200115_1129.csv")
n.17<-read.csv("JSATS_Filter_Nimbus_Fall_2017_20200115_1129.csv")
r.17<-read.csv("JSATS_Filter_RBDD_2017_20200115_1129.csv")
sb.15<-read.csv("JSATS_Filter_SB_Spring_2015_20200115_1129.csv")
sb.16<-read.csv("JSATS_Filter_SB_Spring_2016_20200115_1129.csv")
f.13<-read.csv("JSATS_Filter_FR_Spring_2013_20200115_1129.csv")
f.14<-read.csv("JSATS_Filter_FR_Spring_2014_20200115_1129.csv")
f.15<-read.csv("JSATS_Filter_FR_Spring_2015_20200115_1129.csv")
m.13<-read.csv("JSATS_Filter_MillCk_Wild_2013_20200115_1129.csv")
m.14<-read.csv("JSATS_Filter_MillCk_Wild_2014_20200115_1129.csv")
m.15<-read.csv("JSATS_Filter_MillCk_Wild_2015_20200115_1129.csv")
c.16<-read.csv("JSATS_Filter_ColemanFall_2016_20200115_1129.csv")
c.17<-read.csv("JSATS_Filter_ColemanFall_2017_20200115_1129.csv")
d.17<-read.csv("JSATS_Filter_DeerCk_Wild_2017_20200115_1129.csv")
fd.13<-read.csv("JSATS_Filter_Fall_Delta_2013_20200115_1129.csv")
fd.14<-read.csv("JSATS_Filter_Fall_Delta_2014_20200115_1129.csv")
f.12<-read.csv("JSATS_Filter_FR_Fall_2012_20200115_1129.csv")
b.14<-read.csv("JSATS_Filter_BattleCk_Wild_2014_20200115_1129.csv")
c.12<-read.csv("JSATS_Filter_ColemanFall_2012_20200115_1129.csv")
c.13<-read.csv("JSATS_Filter_ColemanFall_2013_20200115_1129.csv")

# only keep FishID, TagID, Date/time, temp, location
jsats<-rbind(w.17[,c(1,2,4,9,13:16)], sb.17[,c(1,2,4,9,13:16)], w.13[,c(1,2,4,9,13:16)],
             w.14[,c(1,2,4,9,13:16)], w.15[,c(1,2,4,9,13:16)],w.16[,c(1,2,4,9,13:16)],
             m.16[,c(1,2,4,9,13:16)], d.16[,c(1,2,4,9,13:16)], m.17[,c(1,2,4,9,13:16)],
             n.16[,c(1,2,4,9,13:16)], n.17[,c(1,2,4,9,13:16)], r.17[,c(1,2,4,9,13:16)],
             sb.15[,c(1,2,4,9,13:16)], sb.16[,c(1,2,4,9,13:16)], f.13[,c(1,2,4,9,13:16)],
             f.14[,c(1,2,4,9,13:16)], f.15[,c(1,2,4,9,13:16)], m.13[,c(1,2,4,9,13:16)],
             m.14[,c(1,2,4,9,13:16)], m.15[,c(1,2,4,9,13:16)], c.16[,c(1,2,4,9,13:16)],
             c.17[,c(1,2,4,9,13:16)], d.17[,c(1,2,4,9,13:16)], fd.13[,c(1,2,4,9,13:16)],
             fd.14[,c(1,2,4,9,13:16)], f.12[,c(1,2,4,9,13:16)], b.14[,c(1,2,4,9,13:16)],
             c.12[,c(1,2,4,9,13:16)], c.13[,c(1,2,4,9,13:16)])
head(jsats)             

unique(jsats$LOC)             
unique(jsats$GEN)           
unique(jsats$RKM)
unique(jsats$GenRKM)

sapply(X = jsats, FUN = function(x) sum(is.na(x)))

# deal with date/time
# yyyy-MM-dd'T'HH:mm:ss.SSS
# "US/Pacific" or "PST8PDT"?
op <- options("digits.secs" = 3)
jsats$time<-strptime(jsats$DateTime_PST,format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8")
str(jsats)
#time.na <- jsats[is.na(jsats$time),] # chat with Cyril & fixed problem
#unique(time.na$FishID) # all fish are impacted

# remove fish not released in Sacramento basin
# unique locations
loc<-jsats[!duplicated(jsats[,c('LOC','GEN','RKM','GenRKM')]),c(5:8)]
# Mok at 119.453
rel.loc<-aggregate(RKM ~ FishID, jsats, function(x) max(x))
rel.mok<-subset(rel.loc, RKM<120) # Cyril already removed?
# Check on DCC gates fish
# CM suggestion: Look for fish that were detected at either "Mok_NF", "Mok_SF", "Mok_SF_end", or "Mok_NF_end". 
Mok<-subset(jsats, GEN == "Mok_NF_end")
Mok.fish<-unique(Mok$FishID) # only 8
# WR2016-070 (GeorgMk), WR2016-243(GeorgMk), WR2016-365(GeorgMk), SB2016-167(GeorgMk), CFR2016-083(GeorgMk), Delta2013-043(GeorgMk), Delta2013-099(GeorgMk), CFC2013-162(GeorgMk)  
Mok.fish.dat<-jsats[jsats$FishID %in% Mok.fish,]

# number at Benicia, but not Chips
# ChippsE, ChippsW
chip<-subset(jsats, GEN == "ChippsE" | GEN == "ChippsW")
chip<-subset(jsats, LOC == "Chipps2.1" | LOC == "Chipps2.2" | LOC == "Chipps2.3"
             | LOC == "Chipps2.4"| LOC == "Chipps2.5" | LOC == "Chipps1.1"| LOC == "Chipps1.2"
             | LOC == "Chipps1.3"| LOC == "Chipps1.4"| LOC == "Chipps1.5")
length(unique(chip$FishID)) # 575 (only fish from 2016 & 2017)
chip.1<-subset(jsats, GEN == "ChippsE")
chip.2<-subset(jsats, GEN == "ChippsW")
length(unique(chip.1$FishID))#523
length(unique(chip.2$FishID))#547
length(unique(chip.1[chip.1%in%chip.2]))
# Chipps not used in later years (only in 2016 and 2017)

# BeniciaW
ben<-subset(jsats, GEN == "BeniciaW")
length(unique(ben$FishID)) # 747
length(unique(jsats$FishID)) #6863
chip.fish<-chip$FishID
ben.fish<-ben$FishID
 
length(unique(chip.fish[chip.fish%in%ben.fish]))#498 (fish detected at both)
# double check
length(setdiff(ben.fish, chip.fish)) #249
length(setdiff(chip.fish, ben.fish)) # 77

# Cyril suggests compromise with DeckerIsland and AntiochBridge
antBr<-subset(jsats, GEN == "AntiochBridge")
decIs<-subset(jsats, GEN == "DeckerIsland")
length(unique(antBr$FishID)) #125
length(unique(decIs$FishID)) #607
antBr<-within(antBr, Year<-format(antBr$time, "%Y"))
unique(antBr$Year) # gain 2013, 2014, 2015
decIs<-within(decIs, Year<-format(decIs$time, "%Y"))
unique(decIs$Year) # gain 2013, 2014, 2015

ant.fish<-unique(antBr$FishID)
dec.fish<-unique(decIs$FishID)
length(setdiff(ant.fish, chip.fish)) #74
length(setdiff(dec.fish, chip.fish)) #290
length(setdiff(ant.fish, dec.fish)) #89

# travel time
# min time (release)
#rel.date<-aggregate(as.list(time) ~ FishID, jsats, function(x) min(x))

#library(data.table)
#DT <- data.table(jsats)
#rel.date<-DT[ , list(time = min(time)), by = FishID]
#rel.date<-setDT(jsats)[order(time), head(.SD, 1L), by = FishID]

#library(dplyr)
#rel.date<-jsats %>% 
#  arrange(time) %>% 
#  distinct(FishID, .keep_all = TRUE)
#rel.date<-ddply(jsats, "FishID", summarise, min.date=min(time))

#rel.date<-jsats[with(jsats, ave(time, FishID, FUN = function(x) rank(x)==1)),]
#rel.date<-jsats[sapply(split(jsats,jsats$FishID), function(x) row.names(x)[which.min(x$time)] ),]

#jsats$Time <- format(as.POSIXct(jsats$DateTime_PST,format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8"),"%H:%M:%S")
#jsats$Date <- format(as.POSIXct(jsats$DateTime_PST,format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8"),"%Y:%m:%d")
#rel.date<- jsats[,"min_time" := min(Time[which(min_date==Date)]), by=FishID]

#for (jsats in as.list(time)) print(jsats)
# time is my nightmare, but Mike fixed the loop output.
#unique(jsats$FishID)
#jsats.small <- subset(jsats, FishID %in% c("WR2014-077", "WR2014-078", "WR2014-079", "WR2014-080"))
#head(jsats.small)

rel.date <- data.frame(FishID = NA, time = strptime("01/01/1900 11:11:11",format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8"))

for(i in unique(jsats$FishID)){
  temp.dat <- jsats[jsats$FishID == i,]
  min.time <- min(temp.dat$time)
  temp.results <- data.frame(FishID = i, time = as.POSIXlt(min.time,format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8"))
  rel.date<-rbind(rel.date, temp.results)
}

#rel.date$time<-strptime(rel.date$time,format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8")
head(rel.date)
rel.date<-rel.date[-1,]
length(unique(rel.date$FishID)) #6863

# max time at Chips
# CM suggested min instead
chip.min <- data.frame(FishID = NA, time = strptime("01/01/1900 11:11:11",format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8"))

for(i in unique(chip$FishID)){
  temp.dat <- chip[chip$FishID == i,]
  min.time <- min(temp.dat$time)
  temp.results <- data.frame(FishID = i, time = as.POSIXlt(min.time,format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8"))
  chip.min<-rbind(chip.min, temp.results)
}
head(chip.min)
chip.min<-chip.min[-1,]
length(unique(chip.min$FishID)) #575

# max time at Benicia
ben.min <- data.frame(FishID = NA, time = strptime("01/01/1900 11:11:11",format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8"))

for(i in unique(ben$FishID)){
  temp.dat <- ben[ben$FishID == i,]
  min.time <- min(temp.dat$time)
  temp.results <- data.frame(FishID = i, time = as.POSIXlt(min.time,format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8"))
  ben.min<-rbind(ben.min, temp.results)
}
head(ben.min)
ben.min<-ben.min[-1,]
length(unique(ben.min$FishID)) #747

#DeckerIsland 
decIs.min <- data.frame(FishID = NA, time = strptime("01/01/1900 11:11:11",format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8"))

for(i in unique(decIs$FishID)){
  temp.dat <- decIs[decIs$FishID == i,]
  min.time <- min(temp.dat$time)
  temp.results <- data.frame(FishID = i, time = as.POSIXlt(min.time,format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8"))
  decIs.min<-rbind(decIs.min, temp.results)
}
head(decIs.min)
decIs.min<-decIs.min[-1,]
length(unique(decIs.min$FishID)) #607

#AntiochBridge
antBr.min <- data.frame(FishID = NA, time = strptime("01/01/1900 11:11:11",format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8"))

for(i in unique(antBr$FishID)){
  temp.dat <- antBr[antBr$FishID == i,]
  min.time <- min(temp.dat$time)
  temp.results <- data.frame(FishID = i, time = as.POSIXlt(min.time,format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8"))
  antBr.min<-rbind(antBr.min, temp.results)
}
head(antBr.min)
antBr.min<-antBr.min[-1,]
length(unique(antBr.min$FishID)) #125

# difference
chip.rel<-rel.date[rel.date$FishID %in% chip.fish,]
length(unique(chip.fish))
length(unique(chip.rel$FishID))
length(unique(chip.min$FishID))

colnames(chip.rel)[2] <- "time.min"
colnames(chip.min)[2] <- "time.max"
fin.chip<-merge(chip.rel, chip.min, by="FishID")
#fin.chip$travel_time<-fin.chip$time.max-fin.chip$time.min
# CM suggestion
fin.chip$travel_time <- as.numeric(difftime(fin.chip$time.max, fin.chip$time.min, units = "days"))
write.csv(fin.chip, "chiptt.csv")

ben.rel<-rel.date[rel.date$FishID %in% ben.fish,]
length(unique(ben.fish))
length(unique(ben.rel$FishID))
length(unique(ben.min$FishID))

colnames(ben.rel)[2] <- "time.min"
colnames(ben.min)[2] <- "time.max"
fin.ben<-merge(ben.rel, ben.min, by="FishID")
#fin.ben$travel_time<-fin.ben$time.max-fin.ben$time.min
# CM suggestion
fin.ben$travel_time <- as.numeric(difftime(fin.ben$time.max, fin.ben$time.min, units = "days"))
write.csv(fin.ben, "bentt.csv")

summary(fin.chip)
max(fin.chip$travel_time)
min(fin.chip$travel_time)
hist(as.numeric(fin.chip$travel_time))
sapply(X = fin.chip, FUN = function(x) sum(is.na(x)))

summary(fin.ben)
max(fin.ben$travel_time)
min(fin.ben$travel_time)
hist(as.numeric(fin.ben$travel_time))
sapply(X = fin.ben, FUN = function(x) sum(is.na(x)))

# newest version
# setup data
colnames(chip.min)[2] <- "time.max.chip"
colnames(ben.min)[2] <- "time.max.ben"
colnames(antBr.min)[2] <- "time.max.antBr"
colnames(decIs.min)[2] <- "time.max.decIs"

travel.time.dat<-merge(rel.date, ben.min, by="FishID", all.x=TRUE)
travel.time.dat<-merge(travel.time.dat, chip.min, by="FishID", all.x=TRUE)
travel.time.dat<-merge(travel.time.dat, antBr.min, by="FishID", all.x=TRUE)
travel.time.dat<-merge(travel.time.dat, decIs.min, by="FishID", all.x=TRUE)

# conditional calc
travel.time.dat$travel_time.chip <- as.numeric(difftime(travel.time.dat$time.max.chip, travel.time.dat$time, units = "days"))

travel.time.dat$travel_time.ben <- as.numeric(difftime(travel.time.dat$time.max.ben, travel.time.dat$time, units = "days"))

travel.time.dat$travel_time.ant <-as.numeric(difftime(travel.time.dat$time.max.antBr, travel.time.dat$time, units = "days"))

travel.time.dat$travel_time.dec <- as.numeric(difftime(travel.time.dat$time.max.decIs, travel.time.dat$time, units = "days"))

sapply(X = travel.time.dat, FUN = function(x) sum(is.na(x)))
travel.time.dat_v2<-travel.time.dat[rowSums(is.na(travel.time.dat[,c(3:10)])) != ncol(travel.time.dat[,c(3:10)]), ]
write.csv(travel.time.dat_v2, "travel.time.dat.csv")

# examined for alternatives
# subset 767 that were dectected at Chipps or Benicia and either Antioch or Decker (average)
# Chips 575, Benicia-Antioch 27, Benicia-Decker 165 (14 also detected at Antioch, but chose Decker)
# did not use Benicia only 57, Antioch only 21, or Decker only 113
ttf<-read.csv("Travel.Time.Fin.csv")

# check for issues
head(ttf)
hist(ttf$travel_time)
boxplot(ttf$travel_time~ttf$grp.tt)
summary(aov(ttf$travel_time~ttf$grp.tt))
new.ttf<-merge(travel.time.dat_v2, ttf, by="FishID", all.y=TRUE)
new.ttf<-within(new.ttf, Year<-format(new.ttf$time, "%Y"))
summary(aov(new.ttf$travel_time~new.ttf$Year))
library("ggpubr")
ggboxplot(new.ttf, x = "Year", y = "travel_time", color = "grp.tt") 

interaction.plot(x.factor = new.ttf$grp.tt, trace.factor = new.ttf$Year, 
                 response = new.ttf$travel_time, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Reciever", ylab="Travel Time")

summary(aov(new.ttf$travel_time~new.ttf$grp.tt + new.ttf$Year)) # both are significant
summary(aov(new.ttf$travel_time~new.ttf$grp.tt * new.ttf$Year))
pairwise.t.test(new.ttf$travel_time, new.ttf$grp.tt,
                p.adjust.method = "BH")

# just look at 2016 & 2017 Chips compared to other travel time calc
# of 575, 42 also at Benicia/Antioch and 273 Benicia/Decker (6 all comparisons)
test<-read.csv("travel_time_check.csv")
head(test)
plot(test$FishID, test$travel_time, col="red")
par(new=TRUE)
plot(test$FishID, test$ben.dec, col="blue")
par(new=TRUE)
plot(test$FishID, test$ben.ant, col="green")


# from investigations on line 274
head(travel.time.dat_v2)
# remove if not detected at Chips or Benicia
travel.time.dat_v3<-travel.time.dat[rowSums(is.na(travel.time.dat[,c(7,8)])) != ncol(travel.time.dat[,c(7,8)]), ]

travel.time.dat_v3$travel.time<-ifelse(!is.na(travel.time.dat_v3$travel_time.chip), travel.time.dat_v3$travel_time.chip,
                                       ifelse(!is.na(travel.time.dat_v3$travel_time.dec),
                                              (travel.time.dat_v3$travel_time.ben+travel.time.dat_v3$travel_time.dec)/2,
                                              (travel.time.dat_v3$travel_time.ben+travel.time.dat_v3$travel_time.ant)/2))

travel.time.dat_v3$tt.grp<-ifelse(!is.na(travel.time.dat_v3$travel_time.chip), "Chips",
                                       ifelse(!is.na(travel.time.dat_v3$travel_time.dec), "Ben.Dec",
                                              "Ben.Ant"))

length(!is.na(travel.time.dat_v3$travel.time)) #824 (was expecting 767)
head(travel.time.dat_v3)
travel.time.dat_v4 <- travel.time.dat_v3[!(is.na(travel.time.dat_v3$travel.time)),] #767
write.csv(travel.time.dat_v4, "travel.time.jsats.csv")

# release location, SAIL categories
rel.loc <- data.frame(FishID = NA, loc = NA)

for(i in unique(jsats$FishID)){
  temp.dat <- jsats[jsats$FishID == i,]
  max.loc <- max(temp.dat$RKM)
  temp.results <- data.frame(FishID = i, loc = max.loc)
  rel.loc<-rbind(rel.loc, temp.results)
}

head(rel.loc)
rel.loc<-rel.loc[-1,]
length(unique(rel.loc$FishID)) #6863
length(unique(rel.loc$loc))

loc.dat<-unique(jsats[,c(5:8)])
head(loc.dat)
#write.csv(loc.dat, "loc.dat.csv")
rel.loc.dat<-loc.dat[loc.dat$RKM %in% rel.loc$loc,]

rel.loc.dat$Release_Group_SAIL <- ifelse(rel.loc.dat$RKM>500, "Upper Sacramento River", 
                                         ifelse(rel.loc.dat$RKM>207, "Middle Sacramento River", "Tidal Delta"))
head(rel.loc.dat)
colnames(rel.loc)[2] <- "RKM"
rel.loc<-merge(rel.loc, rel.loc.dat[,c(3,5)], by="RKM")

write.csv(rel.loc, "release.location.all.csv")

#fin.chip<-merge(fin.chip, rel.loc, by="FishID", all.x=TRUE)
#fin.ben<-merge(fin.ben, rel.loc, by="FishID", all.x=TRUE)
travel.time.dat_v4<-merge(travel.time.dat_v4, rel.loc, by="FishID", all.x=TRUE)
write.csv(travel.time.dat_v4, "travel.time.jsats.csv")

# water year
fin.chip<-within(fin.chip, Year<-format(fin.chip$time.min, "%Y"))
unique(fin.chip$Year)
fin.ben<-within(fin.ben, Year<-format(fin.ben$time.min, "%Y"))
unique(fin.ben$Year)
# checking...
fin.ben<-within(fin.ben, Year.2<-format(fin.ben$time.max, "%Y"))
fin.ben$issue<-fin.ben$Year==fin.ben$Year.2
length(fin.ben[,9]==TRUE)

travel.time.dat_v4<-within(travel.time.dat_v4, Year<-format(travel.time.dat_v4$time, "%Y"))
unique(travel.time.dat_v4$Year)
write.csv(travel.time.dat_v4, "travel.time.jsats.csv")

# length, weight, transport distance and water temp at release
# from jeremy
jsats.meta<-read.csv("diversity.metadata.20200805.csv") #newest version
head(jsats.meta)
length(unique(jsats.meta$FishID)) #5831
metafish<-unique(jsats.meta$FishID)
tt.fish<-(travel.time.dat_v4$FishID)
#length(setdiff(chip.fish, metafish))
#length(setdiff(ben.fish, metafish)) # missing 20 fish
#a<-setdiff(chip.fish, metafish)
#b<-setdiff(ben.fish, metafish)
#setdiff(b,a)
# missing 22 fish = "MC2017-001"  "MC2017-004"  "MC2017-005"  "MC2017-011"  "MC2017-012"  "MC2017-027" 
"FR2014-250"  "CFR2016-215" "CFR2016-244" "CFR2016-302" "FR2012-022"  "FR2012-041" 
"FR2012-053"  "CFC2012-045" "CFC2012-050" "CFC2012-067" "CFC2012-082" "CFC2012-101"
"CFC2012-115" "CFC2012-143" "CFR2016-232" "CFR2016-242"

length(setdiff(tt.fish, metafish)) # fixed with new version
# still missing 11
#"CFR2016-215" "CFR2016-232" "CFR2016-242" "CFR2016-244" "CFR2016-302"
#"MC2017-001"  "MC2017-004"  "MC2017-005"  "MC2017-011"  "MC2017-012" 
#"MC2017-027"

travel.time.dat_v5<-merge(travel.time.dat_v4, jsats.meta, by="FishID", all.x=TRUE)
head(travel.time.dat_v5) # added 209 rows... # fixed

write.csv(travel.time.dat_v5, "travel.time.jsats.csv")

# run
unique(travel.time.dat_v5$Group)
unique(travel.time.dat_v5$StudyID)

StudyID<-c("Nimbus_Fall_2016", "Nimbus_Fall_2017", "ColemanFall_2013", "ColemanFall_2017",
           "ColemanFall_2016", "Fall_Delta_2013", "FR_Spring_2013", "Winter_H_2013",
           "Winter_H_2014", "Winter_H_2015", "Winter_H_2016", "Winter_H_2017")
Group<-c("Fall", "Fall", "Fall", "Fall", "Fall", "Fall", "Spring", "Winter", "Winter", "Winter",
       "Winter", "Winter")
run.dat<-data.frame(StudyID, Group)

travel.time.dat_v5$origin<-ifelse(!is.na(travel.time.dat_v5$Group), 
                                   "wild", "hatchery")
travel.time.dat_v6<-merge(travel.time.dat_v5, run.dat, by="StudyID", all.x=TRUE)
# didnt exactly work...
travel.time.dat_v6$Group<-ifelse(!is.na(travel.time.dat_v6$Group.x), 
                                 as.character(travel.time.dat_v6$Group.x), 
                                 as.character(travel.time.dat_v6$Group.y))
write.csv(travel.time.dat_v6, "travel.time.jsats.csv")

# route
tt.dat<-read.csv("travel.time.jsats.csv")
head(tt.dat)
tt.dat<-tt.dat[,c(2,3,13:17,20,21,27,30,32,34)]

# load reciever assignments 
r.a<-read.csv("jsats_lwriv_rassign.csv")
str(r.a)
r.a$GEN<-as.character(r.a$GEN)
head(r.a)
tt.fish<-unique(tt.dat$FishID)
jsats.tt<-jsats[jsats$FishID %in% tt.fish,]
length(unique(jsats.tt$FishID)) # 767
jsats.tt$GEN<-as.character(jsats.tt$GEN)
jsats.tt.ra<-merge(jsats.tt, r.a, by="GEN", all.x=TRUE)
head(jsats.tt.ra)
unique(jsats.tt.ra$Route)
unique(jsats.tt.ra$Entrance_Exit)
length(is.na(jsats.tt.ra$Route))
length(is.na(jsats.tt.ra$Entrance_Exit))
recievers.gen<-unique(jsats.tt.ra[,c(1,10,11)]) # NAs are upper/middle river
write.csv(jsats.tt.ra, "travel.time.jsats.wRoute.csv")

# sort if detected at slough, entrance or exit sites
Fish.List <- data.frame(FishID = unique(jsats.tt.ra$FishID), Ent.code = NA, Exit.Code = NA, Slough.Count = NA, antislough.code=NA)
unique(jsats.tt.ra$Entrance_Exit)
for(i in 1:nrow(Fish.List)){
  temp.dat <- subset(jsats.tt.ra, FishID == Fish.List[i, "FishID"])
  temp.ent <- subset(temp.dat, Entrance_Exit == "enter")
  temp.exit <- subset(temp.dat, Entrance_Exit == "exit")
  temp.slough.count <- subset(temp.dat, Entrance_Exit == "slough")
  temp.antislough.code <- subset(temp.dat, Entrance_Exit == "anti-slough")
  if(nrow(temp.ent) >= 1)
    Fish.List[i, "Ent.code"] <- 1
  else
    Fish.List[i, "Ent.code"] <- 0  
  if(nrow(temp.exit) >= 1)
    Fish.List[i, "Exit.Code"] <- 1
  else
    Fish.List[i, "Exit.Code"] <- 0  
  if(nrow(temp.slough.count) > 1)
    Fish.List[i, "Slough.Count"] <- 1
  else
    Fish.List[i, "Slough.Count"] <- 0  
  if(nrow(temp.antislough.code) >= 1)
    Fish.List[i, "antislough.code"] <- 1
  else
    Fish.List[i, "antislough.code"] <- 0
}

Fish.List <- transform(Fish.List, Total.count = Ent.code+ Exit.Code+Slough.Count)
head(Fish.List)
length(unique(Fish.List$FishID)) #767

# skip to 513
# subset fish that are definitely slough or not slough
slough=subset(Fish.List, Total.count>2)
length(unique(slough$FishID)) #80
river=subset(Fish.List, Total.count<2)
length(unique(river$FishID)) #538

# subset fish that are on the boarder
question=subset(Fish.List, Total.count==2)
length(unique(question$FishID)) #149

# put data back in for questionable fish
question.dat<-merge(question, jsats.tt.ra, by="FishID", all.x=TRUE)
length(unique(question.dat$FishID)) #149
# add frequency of detections
library("dplyr")
question.dat.2<-na.omit(question.dat) # remove upper/middle river GEN
freq.slough<-summarise(group_by(question.dat.2, FishID, GEN, Route, Entrance_Exit), freq.det=length(DateTime_PST))
length(unique(freq.slough$FishID)) #149

write.csv(freq.slough, "freq.slough_jsats.csv")

#rules:
#1)at least two detections within a slough (already filtered above) OR 2) two or more sites associated
#with sloughs (slough/exit/entrance) with greater than two detections (unique locations) within a complex
# didnt use code from before, too many combinations to code (WR 2017 really interesting...)
# manual inspection for 149 questionable fish
mi<-subset(jsats, FishID == c("WR2017-009","WR2017-033"))
m12<-subset(jsats, FishID == c("WR2017-119","WR2017-152", "WR2017-223", "WR2017-403"))
# missing entrance for Yolo -- added and reran 435-480

slough=subset(Fish.List, Total.count>=2)
length(unique(slough$FishID)) #311
river=subset(Fish.List, Total.count<2)
length(unique(river$FishID)) #456

question.dat<-merge(slough, jsats.tt.ra, by="FishID", all.x=TRUE)
length(unique(question.dat$FishID)) #311

question.dat.2<-na.omit(question.dat) # remove upper/middle river GEN
freq.slough<-summarise(group_by(question.dat.2, FishID, GEN, Route, Entrance_Exit), freq.det=length(DateTime_PST))
length(unique(freq.slough$FishID)) #311
head(freq.slough)
freq.slough$grp.route<-ifelse(freq.slough$Route == "Georgiana/DCC" | freq.slough$Route == "Threemile", "CenDel",
                              ifelse(freq.slough$Route == "Mainstem", "SacR",
                                     ifelse(freq.slough$Route == "Yolo", "Yolo", "SacRSlough")))

 
# must have entrance/slough/exit within complex (grp.route) combination
SacRSl<-subset(freq.slough, grp.route=="SacRSlough")
length(unique(SacRSl$FishID)) #253

# skip to 543
# if there is more than one row with the same FishID, then it used at least two entrance/slough/exit within complex
SacRSl.dup<-SacRSl[duplicated(SacRSl$FishID), ]
length(unique(SacRSl.dup$FishID)) #149
# check if that worked
SacRsl.fish<-unique(SacRSl.dup$FishID)
SacRSl.check<-SacRSl[SacRSl$FishID %in% SacRsl.fish,] # nope

freq.slough_v2<-subset(freq.slough, freq.slough$grp.route != "SacR")

Route.List <- data.frame(FishID = NA, combination = NA, Route = NA)
for(j in unique(freq.slough_v2$grp.route)){
  temp.dat2 <- freq.slough_v2[freq.slough_v2$grp.route == j,]

for(i in unique(temp.dat2$FishID)){
  temp.dat <- temp.dat2[temp.dat2$FishID == i,]
  #combination<-ifelse(temp.dat$Entrance_Exit == "enter" & temp.dat$Entrance_Exit == "slough", TRUE,
   #                      ifelse(temp.dat$Entrance_Exit == "enter" & temp.dat$Entrance_Exit == "exit", TRUE,
    #                            ifelse(temp.dat$Entrance_Exit == "exit" & temp.dat$Entrance_Exit == "slough", TRUE,
     #                                  ifelse(temp.dat$Entrance_Exit == "exit" & temp.dat$Entrance_Exit == "slough" & temp.dat$Entrance_Exit == "enter", TRUE,
      #                                        FALSE))))
  
  if(identical(sort(droplevels(unique(temp.dat$Entrance_Exit))), sort(as.factor(c("exit")))))
    temp.combination <- "NO"
  else
  if(identical(sort(droplevels(unique(temp.dat$Entrance_Exit))), sort(as.factor(c("enter")))))
    temp.combination <- "NO"
  else
   if(identical(sort(droplevels(unique(temp.dat$Entrance_Exit))), sort(as.factor(c("exit", "Mainstem")))))
     temp.combination <- "NO"
   else
  if(identical(sort(droplevels(unique(temp.dat$Entrance_Exit))), sort(as.factor(c("enter", "Mainstem")))))
    temp.combination <- "NO"
  else
    temp.combination<-"YES"
  
  temp.results <- data.frame(FishID = i, combination = temp.combination, Route = j)
  Route.List<-rbind(Route.List, temp.results)
}
}

head(Route.List)

length(unique(Route.List$FishID)) #311
unique(Route.List$combination)
Route.List<-Route.List[-1,]

#example.dat <- subset(SacRSl, FishID == "CFC2013-216")
#example.dat2 <- subset(SacRSl, FishID == "Delta2013-012")
#str(droplevels(example.dat2))

Route.List.2 <- subset(Route.List, Route.List$combination != "NO") # lost 216 rows
length(unique(Route.List.2$FishID)) # 279
Route.List.3 <- aggregate(Route ~ FishID, data = Route.List.2, toString)
length(unique(Route.List.3$FishID)) # 279
unique(Route.List.3$Route)

# add it to travel time data
head(tt.dat)     
tt.dat.route<-merge(tt.dat, Route.List.3, by="FishID", all.x = TRUE)
head(tt.dat.route)
tt.dat.route[["Route"]][is.na(tt.dat.route[["Route"]])] <- "SacR"
write.csv(tt.dat.route, "travel.time.jsats.FIN.csv")
