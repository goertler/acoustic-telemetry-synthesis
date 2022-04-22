setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections/Missing_files")
# missing jsats data
fr.12<-read.csv("JSATS_Filter_Fall_Delta_2012_20200115_1129.csv")
fr.13<-read.csv("JSATS_Filter_FR_Spring_Delta_2013_20200115_1129.csv")
fr.14<-read.csv("JSATS_Filter_FR_Spring_Delta_2014_20200115_1129.csv")
fr.15<-read.csv("JSATS_Filter_FR_Spring_Delta_2015_20200115_1129.csv")
t.13<-read.csv("JSATS_Filter_Tisdale_RST_2013_20200115_1129.csv")

head(fr.13)
jsats<-rbind(fr.12[,c(1,2,4,9,13:16)],fr.13[,c(1,2,4,9,13:16)], fr.14[,c(1,2,4,9,13:16)], fr.15[,c(1,2,4,9,13:16)],
             t.13[,c(1,2,4,9,13:16)])

head(jsats)             

unique(jsats$LOC)             
unique(jsats$GEN)           
unique(jsats$RKM)
unique(jsats$GenRKM)

sapply(X = jsats, FUN = function(x) sum(is.na(x)))

op <- options("digits.secs" = 3)
jsats$time<-strptime(jsats$DateTime_PST,format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8")
str(jsats)

loc<-jsats[!duplicated(jsats[,c('LOC','GEN','RKM','GenRKM')]),c(5:8)]
# no Chipps in this data

# BeniciaW
ben<-subset(jsats, GEN == "BeniciaW")
length(unique(ben$FishID)) # 41
length(unique(jsats$FishID)) #1054

antBr<-subset(jsats, GEN == "AntiochBridge")
decIs<-subset(jsats, GEN == "DeckerIsland")
length(unique(antBr$FishID)) #6
length(unique(decIs$FishID)) #39

ben.fish<-unique(ben$FishID)
ant.fish<-unique(antBr$FishID)
dec.fish<-unique(decIs$FishID)
length(setdiff(ant.fish, ben.fish)) #2
length(setdiff(dec.fish, ben.fish)) #17

# travel time
# release date/time
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
length(unique(rel.date$FishID)) #1054

# Benicia
ben.min <- data.frame(FishID = NA, time = strptime("01/01/1900 11:11:11",format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8"))

for(i in unique(ben$FishID)){
  temp.dat <- ben[ben$FishID == i,]
  min.time <- min(temp.dat$time)
  temp.results <- data.frame(FishID = i, time = as.POSIXlt(min.time,format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8"))
  ben.min<-rbind(ben.min, temp.results)
}
head(ben.min)
ben.min<-ben.min[-1,]
length(unique(ben.min$FishID)) #41

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
length(unique(decIs.min$FishID)) #39

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
length(unique(antBr.min$FishID)) #6

# difference
colnames(ben.min)[2] <- "time.max.ben"
colnames(antBr.min)[2] <- "time.max.antBr"
colnames(decIs.min)[2] <- "time.max.decIs"

travel.time.dat<-merge(rel.date, ben.min, by="FishID", all.x=TRUE)
travel.time.dat<-merge(travel.time.dat, antBr.min, by="FishID", all.x=TRUE)
travel.time.dat<-merge(travel.time.dat, decIs.min, by="FishID", all.x=TRUE)

head(travel.time.dat)
travel.time.dat<-travel.time.dat[!is.na(travel.time.dat$time.max.ben), ]
travel.time.dat$travel_time.ben <- as.numeric(difftime(travel.time.dat$time.max.ben, travel.time.dat$time, units = "days"))
travel.time.dat$travel_time.ant <-as.numeric(difftime(travel.time.dat$time.max.antBr, travel.time.dat$time, units = "days"))
travel.time.dat$travel_time.dec <- as.numeric(difftime(travel.time.dat$time.max.decIs, travel.time.dat$time, units = "days"))

travel.time.dat$travel.time<-ifelse(!is.na(travel.time.dat$travel_time.dec),
                                              (travel.time.dat$travel_time.ben+travel.time.dat$travel_time.dec)/2,
                                              (travel.time.dat$travel_time.ben+travel.time.dat$travel_time.ant)/2)

travel.time.dat$tt.grp<-ifelse(!is.na(travel.time.dat$travel_time.dec), "Ben.Dec",
                                         "Ben.Ant")
travel.time.dat<-travel.time.dat[!is.na(travel.time.dat$travel.time), ]

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
length(unique(rel.loc$FishID)) #1054
length(unique(rel.loc$loc)) #21

rel.loc.dat<-loc[loc$RKM %in% rel.loc$loc,]

rel.loc.dat$Release_Group_SAIL <- ifelse(rel.loc.dat$RKM>500, "Upper Sacramento River", 
                                         ifelse(rel.loc.dat$RKM>203, "Middle Sacramento River", "Tidal Delta"))
head(rel.loc.dat)
colnames(rel.loc)[2] <- "RKM"
rel.loc<-merge(rel.loc, rel.loc.dat[,c(3,5)], by="RKM")
travel.time.dat_v2<-merge(travel.time.dat, rel.loc, by="FishID", all.x=TRUE)
head(travel.time.dat_v2)

# year
travel.time.dat_v2<-within(travel.time.dat_v2, Year<-format(travel.time.dat_v2$time, "%Y"))
unique(travel.time.dat_v2$Year)

#length, weight, transport distance and water temp at release
jsats.meta<-read.csv("diversity.metadata.20200805.csv") #newest version
head(jsats.meta)
length(unique(jsats.meta$FishID)) #5831

travel.time.dat_v3<-merge(travel.time.dat_v2, jsats.meta, by="FishID", all.x=TRUE)
head(travel.time.dat_v3)

# run & origin
unique(travel.time.dat_v3$Group)
unique(travel.time.dat_v3$StudyID)
# metadata is missing for Tisdale2013
StudyID<-c("FR_Spring_Delta_2013", "FR_Spring_Delta_2015", NA)
Group<-c("Spring","Spring","Tisdale2013")
origin<-"hatchery"
run.dat<-data.frame(StudyID, Group, origin)
travel.time.dat_v4<-merge(travel.time.dat_v3,run.dat, by="StudyID", all.x=TRUE)

# route
tt.dat<-travel.time.dat_v4[,c(2,3,10:14,17,18,24,27,29,30)]
head(tt.dat)
write.csv(tt.dat, "travel_time_missing_JSATS.csv")

# load reciever assignments 
r.a<-read.csv("jsats_lwriv_rassign.csv")
str(r.a)
r.a$GEN<-as.character(r.a$GEN)
head(r.a)

tt.fish<-unique(tt.dat$FishID) #26
jsats.tt<-jsats[jsats$FishID %in% tt.fish,]
length(unique(jsats.tt$FishID)) # 26
jsats.tt$GEN<-as.character(jsats.tt$GEN)
jsats.tt.ra<-merge(jsats.tt, r.a, by="GEN", all.x=TRUE)
head(jsats.tt.ra)
unique(jsats.tt.ra$Route)
unique(jsats.tt.ra$Entrance_Exit)
recievers.gen<-unique(jsats.tt.ra[,c(1,10,11)]) #

# sort if detected at slough, entrance or exit sites
Fish.List <- data.frame(FishID = unique(jsats.tt.ra$FishID), Ent.code = NA, Exit.Code = NA, Slough.Count = NA, antislough.code=NA)

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
length(unique(Fish.List$FishID)) #26

slough=subset(Fish.List, Total.count>=2)
length(unique(slough$FishID)) #19
river=subset(Fish.List, Total.count<2)
length(unique(river$FishID)) #7
question.dat<-merge(slough, jsats.tt.ra, by="FishID", all.x=TRUE)
length(unique(question.dat$FishID)) #19

question.dat.2<-na.omit(question.dat) # remove upper/middle river GEN
freq.slough<-summarise(group_by(question.dat.2, FishID, GEN, Route, Entrance_Exit), freq.det=length(DateTime_PST))

head(freq.slough)
freq.slough$grp.route<-ifelse(freq.slough$Route == "Georgiana/DCC" | freq.slough$Route == "Threemile", "CenDel",
                              ifelse(freq.slough$Route == "Mainstem", "SacR",
                                     ifelse(freq.slough$Route == "Yolo", "Yolo", "SacRSlough")))


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
Route.List<-Route.List[-1,]
length(unique(Route.List$FishID)) #19
unique(Route.List$combination)

Route.List.2 <- subset(Route.List, Route.List$combination != "NO") # lost 4 rows
length(unique(Route.List.2$FishID)) # 19
Route.List.3 <- aggregate(Route ~ FishID, data = Route.List.2, toString)
length(unique(Route.List.3$FishID)) # 19
unique(Route.List.3$Route)

# add it to travel time data
head(tt.dat)     
tt.dat.route<-merge(tt.dat, Route.List.3, by="FishID", all.x = TRUE)
head(tt.dat.route)
tt.dat.route[["Route"]][is.na(tt.dat.route[["Route"]])] <- "SacR"
length(unique(tt.dat.route$FishID)) #26
write.csv(tt.dat.route, "travel.time.jsats.GS.csv")

summarise(group_by(tt.dat.route,Route), count=length(unique(FishID)))

# add to the rest of the jsats data
dat.tt<-read.csv("travel.time.jsats.FIN.csv")
head(dat.tt)
# add studyID back, remove time at release, colname(Group.y)
colnames(tt.dat.route)[12] <- "Group"
tt.dat.route<-tt.dat.route[,-2]
SID<-travel.time.dat_v4[!duplicated(travel.time.dat_v4[,c('StudyID', 'FishID')]),c(1:2)]
tt.dat.route<-merge(tt.dat.route, SID, by="FishID", all.x=TRUE)
dat.tt2<-rbind(dat.tt[,-1], tt.dat.route)

write.csv(dat.tt2, "travel.time.jsats.FIN_v2.csv")
length(unique(dat.tt2$FishID))
summarise(group_by(dat.tt2,Route), count=length(unique(FishID)))
summarise(group_by(dat.tt2,tt.grp), count=length(unique(FishID)))

# look at Gabe's metadata for Tisdale fish

gs.dat<-readRDS("releases.rds")
head(gs.dat)
gs.dat %>%
  group_by(StudyID) %>%
  summarise(max = max(Length), min = min(Length), n = n())

# methods summary
dat.tt2<-read.csv("travel.time.jsats.FIN_v2.csv")
head(dat.tt2)

summarise(group_by(dat.tt2,Route), count=length(unique(FishID)))
length(unique(dat.tt2$FishID)) #793
summarise(group_by(dat.tt2,tt.grp), count=length(unique(FishID)))
