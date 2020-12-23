# skip to 70
setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections")
cm.dat<-read.csv("CALFED_late_falls.csv")

head(cm.dat)
unique(cm.dat$DetectDate)             
unique(cm.dat$Location)           
unique(cm.dat$General_Location)

sapply(X = cm.dat, FUN = function(x) sum(is.na(x)))

length(unique(cm.dat$FishID)) #1555
loc<-cm.dat[!duplicated(cm.dat[,c('Location','General_Location')]),c(3:4)]
chip<-subset(cm.dat, General_Location == "Chipps Island")
length(unique(chip$FishID)) #351
ben<-subset(cm.dat, General_Location == "Benicia Bridge")
length(unique(ben$FishID)) #287

chip.fish<-chip$FishID
ben.fish<-ben$FishID

length(unique(chip.fish[chip.fish%in%ben.fish]))#251 (loose 36 fish without ben.fish)

#going with Chips first, CM suggests to keep ben fish where applicable 


# release info
str(cm.dat)
cm.dat$time<-strptime(cm.dat$DetectDate,format="%m/%d/%Y %H:%M:%S", tz="Etc/GMT+8")

rel.date <- data.frame(FishID = NA, time = strptime("1900-01-01 11:11:11",format="%Y/%m/%d %H:%M:%S", tz="Etc/GMT+8"))

for(i in unique(cm.dat$FishID)){
  temp.dat <- cm.dat[cm.dat$FishID == i,]
  min.time <- min(temp.dat$time)
  temp.results <- data.frame(FishID = i, time = as.POSIXlt(min.time,format="%Y/%m/%d %H:%M:%OS", tz="Etc/GMT+8"))
  rel.date<-rbind(rel.date, temp.results)
}

#rel.date$time<-strptime(rel.date$time,format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8")
head(rel.date)
rel.date<-rel.date[-1,]
length(unique(rel.date$FishID)) #1555

###################################################
loc.dat<-read.csv("dbo_Monitor_Locations.csv")
head(loc.dat)
unique(loc.dat$LocationType) # Acoustic Release = San_Pablo_Bay ???
loc.dat<-loc.dat[,c(3:5,8,10)]
cm.dat.loc<-merge(cm.dat, loc.dat, by="Location", all.x = TRUE)
head(cm.dat.loc)
# checking...
all(as.character(cm.dat.loc$General_Location.x) == as.character(cm.dat.loc$General_Location.y))

# release location, SAIL categories
rel.loc <- data.frame(FishID = NA, loc = NA)

for(i in unique(cm.dat.loc$FishID)){
  temp.dat <- cm.dat.loc[cm.dat.loc$FishID == i,]
  max.loc <- max(temp.dat$RiverKm)
  temp.results <- data.frame(FishID = i, loc = max.loc)
  rel.loc<-rbind(rel.loc, temp.results)
}

head(rel.loc)
rel.loc<-rel.loc[-1,]
length(unique(rel.loc$FishID)) #1555
length(unique(rel.loc$loc))
##########################################################
### release data was not included in data above
# new data
setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections/Query3")
dat.vemco<-read.csv("Query3.csv")
head(dat.vemco)
length(unique(dat.vemco$FishID))# 1555

unique(dat.vemco$DetectDate)             
unique(dat.vemco$Location)           
unique(dat.vemco$General_Location)
unique(dat.vemco$Release_Location)
unique(dat.vemco$Date_Released)
sapply(X = dat.vemco, FUN = function(x) sum(is.na(x)))

# date and location of release already included
str(dat.vemco)
dat.vemco$time<-strptime(dat.vemco$DetectDate,format="%m/%d/%Y %H:%M:%S", tz="Etc/GMT+8")
dat.vemco$time.rel<-strptime(dat.vemco$Date_Released,format="%m/%d/%Y %H:%M:%S", tz="Etc/GMT+8")

# release loc
rel.dat<-unique(dat.vemco[,c(1,6,9)])
min(rel.dat$Riverkm)
rel.dat$Release_Group_SAIL <- ifelse(rel.dat$Riverkm>500, "Upper Sacramento River", 
                                         ifelse(rel.dat$Riverkm>207, "Middle Sacramento River", "Tidal Delta"))

# water year 
### incorrect for years with December release
#yr.dat<-unique(dat.vemco[,c(1,11)])
#yr.dat<-within(yr.dat, Year<-format(yr.dat$time.rel, "%Y"))
#unique(yr.dat$Year)
time.rel <- as.character(unique(dat.vemco$time.rel))
Year <- c(2007,2007,2007,2007,2007,2007,2007,2007,2007,2007,
          2007,2007,2007,2007,2007,2008,2008,2008,2008,2008,
          2008,2009,2009,2009,2009,2009,2009,2010,2010,2010,
          2010,2010,2010,2010,2011,2011,2011,2011)
WR <- data.frame(time.rel, Year)
WR$Year <- as.factor(WR$Year)
FishID<-unique(dat.vemco[, c(1,11)])
FishID$time.rel<-as.character(FishID$time.rel)
dat.sum.vemco<-merge(WR, FishID)

# run
dat.sum.vemco<-merge(dat.sum.vemco, rel.dat, by="FishID")
dat.sum.vemco$origin = "hatchery"
dat.sum.vemco$Group = "late-fall"
length(unique(dat.sum.vemco$FishID)) #1555
head(dat.sum.vemco)
# fix date format post-merge
dat.sum.vemco<-dat.sum.vemco[,c(-2)]
FishID<-unique(dat.vemco[, c(1,11)])
dat.sum.vemco<-merge(dat.sum.vemco, FishID, by="FishID")

# CM - drop fish released from BtlCkCNFHWeir during the 2010 and 2011 study years from the analysis (i.e., Dec 2009 and beyond). 
dat.sum.vemco$Release_Location<-as.character(dat.sum.vemco$Release_Location)
cm.subset<-subset(dat.sum.vemco, Release_Location == "BtlCkCNFHWeir" | Release_Location == "BtlCkCNFHWeir                                     ")
unique(cm.subset$Year)
length (unique(cm.subset$FishID)) #454
#dat.sum.vemco$Year<-as.numeric(dat.sum.vemco$Year)
new.data <- dat.sum.vemco[!(dat.sum.vemco$Release_Location == "BtlCkCNFHWeir" | dat.sum.vemco$Release_Location == "BtlCkCNFHWeir                                     " & dat.sum.vemco$Year > 2007)  , ]
unique(new.data$Year)
unique(new.data$Release_Location)
length(unique(new.data$FishID)) #1101

# length, weight, transport distance and water temp at release
setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections")
meta<-read.csv("Late-fall_metadata.csv") 
head(meta)
length(unique(meta$FishID)) #1350 ??? --> check to see if missing fish matter after travel time code

# travel time
unique(dat.vemco$General_Location)  
loc<-unique(dat.vemco[,c(3,4,9)]) # Riverkm only for release locations

# use different data for location of detections
loc.dat<-read.csv("dbo_Monitor_Locations.csv")
head(loc.dat)
unique(loc.dat$LocationType) # Acoustic Release = San_Pablo_Bay ???
#loc.dat<-loc.dat[,c(3:5,8,10)]
cm.dat.loc<-merge(cm.dat, loc.dat, by="Location", all.x = TRUE)
head(cm.dat.loc)
length(unique(cm.dat.loc$FishID)) #1555

chip<-subset(cm.dat.loc, General_Location.x == "Chipps Island")
antBr<-subset(cm.dat.loc, General_Location.x == "Antioch_Br")
decIs<-subset(cm.dat.loc, General_Location.x == "Decker_IsN" | General_Location.x == "Decker_IsS")
ben<-subset(cm.dat.loc, General_Location.x == "Benicia Bridge")
length(unique(chip$FishID)) #351
length(unique(antBr$FishID)) #15
length(unique(decIs$FishID)) #403
length(unique(ben$FishID)) #287

chip.min <- data.frame(FishID = NA, time = strptime("1900-01-01 11:11:11",format="%Y/%m/%d %H:%M:%S", tz="Etc/GMT+8"))

for(i in unique(chip$FishID)){
  temp.dat <- chip[chip$FishID == i,]
  min.time <- min(temp.dat$time)
  temp.results <- data.frame(FishID = i, time = as.POSIXlt(min.time,format="%Y/%m/%d %H:%M:%S", tz="Etc/GMT+8"))
  chip.min<-rbind(chip.min, temp.results)
}
head(chip.min)
chip.min<-chip.min[-1,]
length(unique(chip.min$FishID)) #351

# max time at Benicia
ben.min <- data.frame(FishID = NA, time = strptime("1900-01-01 11:11:11",format="%Y/%m/%d %H:%M:%S", tz="Etc/GMT+8"))

for(i in unique(ben$FishID)){
  temp.dat <- ben[ben$FishID == i,]
  min.time <- min(temp.dat$time)
  temp.results <- data.frame(FishID = i, time = as.POSIXlt(min.time,format="%Y/%m/%d %H:%M:%S", tz="Etc/GMT+8"))
  ben.min<-rbind(ben.min, temp.results)
}
head(ben.min)
ben.min<-ben.min[-1,]
length(unique(ben.min$FishID)) #287

#DeckerIsland 
decIs.min <- data.frame(FishID = NA, time = strptime("1900-01-01 11:11:11",format="%Y/%m/%d %H:%M:%S", tz="Etc/GMT+8"))

for(i in unique(decIs$FishID)){
  temp.dat <- decIs[decIs$FishID == i,]
  min.time <- min(temp.dat$time)
  temp.results <- data.frame(FishID = i, time = as.POSIXlt(min.time,format="%Y/%m/%d %H:%M:%S", tz="Etc/GMT+8"))
  decIs.min<-rbind(decIs.min, temp.results)
}
head(decIs.min)
decIs.min<-decIs.min[-1,]
length(unique(decIs.min$FishID)) #403

#AntiochBridge
antBr.min <- data.frame(FishID = NA, time = strptime("1900-01-01 11:11:11",format="%Y/%m/%d %H:%M:%S", tz="Etc/GMT+8"))

for(i in unique(antBr$FishID)){
  temp.dat <- antBr[antBr$FishID == i,]
  min.time <- min(temp.dat$time)
  temp.results <- data.frame(FishID = i, time = as.POSIXlt(min.time,format="%Y/%m/%d %H:%M:%S", tz="Etc/GMT+8"))
  antBr.min<-rbind(antBr.min, temp.results)
}
head(antBr.min)
antBr.min<-antBr.min[-1,]
length(unique(antBr.min$FishID)) #15

colnames(chip.min)[2] <- "time.max.chip"
colnames(ben.min)[2] <- "time.max.ben"
colnames(antBr.min)[2] <- "time.max.antBr"
colnames(decIs.min)[2] <- "time.max.decIs"

head(dat.sum.vemco)

travel.time.dat<-merge(dat.sum.vemco, ben.min, by="FishID", all.x=TRUE)
travel.time.dat<-merge(travel.time.dat, chip.min, by="FishID", all.x=TRUE)
travel.time.dat<-merge(travel.time.dat, antBr.min, by="FishID", all.x=TRUE)
travel.time.dat<-merge(travel.time.dat, decIs.min, by="FishID", all.x=TRUE)
head(travel.time.dat)

# conditional calc
travel.time.dat$travel_time.chip <- as.numeric(difftime(travel.time.dat$time.max.chip, travel.time.dat$time.rel, units = "days"))
travel.time.dat$travel_time.ben <- as.numeric(difftime(travel.time.dat$time.max.ben, travel.time.dat$time.rel, units = "days"))
travel.time.dat$travel_time.ant <-as.numeric(difftime(travel.time.dat$time.max.antBr, travel.time.dat$time.rel, units = "days"))
travel.time.dat$travel_time.dec <- as.numeric(difftime(travel.time.dat$time.max.decIs, travel.time.dat$time.rel, units = "days"))

travel.time.dat_v2<-travel.time.dat[rowSums(is.na(travel.time.dat[,c(9,10)])) != ncol(travel.time.dat[,c(9,10)]), ]

travel.time.dat_v2$travel.time<-ifelse(!is.na(travel.time.dat_v2$travel_time.chip), travel.time.dat_v2$travel_time.chip,
                                       ifelse(!is.na(travel.time.dat_v2$travel_time.dec),
                                              (travel.time.dat_v2$travel_time.ben+travel.time.dat_v2$travel_time.dec)/2,
                                              (travel.time.dat_v2$travel_time.ben+travel.time.dat_v2$travel_time.ant)/2))
head(travel.time.dat_v2)
travel.time.dat_v2$tt.grp<-ifelse(!is.na(travel.time.dat_v2$travel_time.chip), "Chips",
                                  ifelse(!is.na(travel.time.dat_v2$travel_time.dec), "Ben.Dec",
                                         "Ben.Ant"))

travel.time.dat_v3<-travel.time.dat_v2[,c(1:8,17,18)]
head(travel.time.dat_v3)
write.csv(travel.time.dat_v3, "travel.time_CM.Vemco_v2.csv")
meta_2<-meta[,c(1:3,7:9)]
travel.time.dat_v4<-merge(travel.time.dat_v3, meta_2, by="FishID", all.x=TRUE)
head(travel.time.dat_v4)
length(unique(travel.time.dat_v4$FishID)) #387
sapply(X = travel.time.dat_v4, FUN = function(x) sum(is.na(x)))

# missing data
# still need water temp at release
missing.meta <- travel.time.dat_v4[rowSums(is.na(travel.time.dat_v4)) > 0,]
write.csv(missing.meta, "missing.meta.csv") # followed up with CM

# trucking distance and temp
#truck.temp<-read.csv("Copy of Temp_and_trucking_distance_at_release.csv") #updated
truck.temp<-read.csv("Temp_and_trucking_distance_at_release_CM_v2.csv")
head(truck.temp)
truck.temp=truck.temp[,c(3,5,7)]
travel.time.dat_v5<-merge(travel.time.dat_v4, truck.temp, by="FishID", all.x = TRUE)
missing.meta <- travel.time.dat_v5[rowSums(is.na(travel.time.dat_v5)) > 0,]
write.csv(missing.meta, "missing.meta.csv")

write.csv(travel.time.dat_v5, "travel.time_CM.Vemco_v2.csv")

# route
# load reciever assignments 
r.a<-read.csv("cm_lwriv_rassign.csv")
str(r.a)
r.a$General_Location<-as.character(r.a$General_Location)
head(r.a)
#travel.time.dat_v5<-read.csv("travel.time_CM.Vemco.csv")
head(travel.time.dat_v5)

tt.dat<-travel.time.dat_v5[!is.na(travel.time.dat_v5$travel.time), ]
tt.fish<-unique(tt.dat$FishID) 
cm.dat.tt<-cm.dat[cm.dat$FishID %in% tt.fish,]
length(unique(cm.dat.tt$FishID)) # 383

cm.dat.tt$General_Location<-as.character(cm.dat.tt$General_Location)
cm.dat.tt.ra<-merge(cm.dat.tt, r.a, by="General_Location", all.x=TRUE)
head(cm.dat.tt.ra)
unique(cm.dat.tt.ra$Route)
unique(cm.dat.tt.ra$Entrance_Exit)
recievers.gen<-unique(cm.dat.tt.ra[,c(1,6,7)]) #

# sort if detected at slough, entrance or exit sites
Fish.List <- data.frame(FishID = unique(cm.dat.tt.ra$FishID), Ent.code = NA, Exit.Code = NA, Slough.Count = NA, antislough.code=NA)

for(i in 1:nrow(Fish.List)){
  temp.dat <- subset(cm.dat.tt.ra, FishID == Fish.List[i, "FishID"])
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
length(unique(Fish.List$FishID)) #383

slough=subset(Fish.List, Total.count>=2)
length(unique(slough$FishID)) #164
river=subset(Fish.List, Total.count<2)
length(unique(river$FishID)) #219
question.dat<-merge(slough, cm.dat.tt.ra, by="FishID", all.x=TRUE)
length(unique(question.dat$FishID)) #164

question.dat.2<-na.omit(question.dat) # remove upper/middle river GEN
freq.slough<-summarise(group_by(question.dat.2, FishID, General_Location, Route, Entrance_Exit), freq.det=length(time))

head(freq.slough)
freq.slough$grp.route<-ifelse(freq.slough$Route == "Georgiana/DCC" | freq.slough$Route == "Threemile", "CenDel",
                              ifelse(freq.slough$Route == "Mainstem", "SacR", "SacRSlough"))


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
length(unique(Route.List$FishID)) #164
unique(Route.List$combination)

Route.List.2 <- subset(Route.List, Route.List$combination != "NO") # lost 34 rows
length(unique(Route.List.2$FishID)) # 158
Route.List.3 <- aggregate(Route ~ FishID, data = Route.List.2, toString)
length(unique(Route.List.3$FishID)) # 158
unique(Route.List.3$Route) #"SacRSlough","CenDel","SacRSlough, CenDel"

# add it to travel time data
head(tt.dat)     
tt.dat.route<-merge(tt.dat, Route.List.3, by="FishID", all.x = TRUE)
head(tt.dat.route)
tt.dat.route[["Route"]][is.na(tt.dat.route[["Route"]])] <- "SacR"
length(unique(tt.dat.route$FishID)) #383
write.csv(tt.dat.route, "travel.time_CM.Vemco_v3.csv")

summarise(group_by(tt.dat.route,Route), count=length(unique(FishID)))
summarise(group_by(tt.dat.route,tt.grp), count=length(unique(FishID)))


# updated meta data
#tt.dat.route2<-read.csv("travel.time_CM.Vemco.csv")
head(tt.dat.route)
tt.dat.route2<-tt.dat.route[,c(1:15,18)]

truck.temp<-read.csv("Temp_and_trucking_distance_at_release_CM_v2.csv")
head(truck.temp)
truck.temp<-truck.temp[,c(3,5,7)]
length(unique(truck.temp$FishID)) #929
tt.dat.route2<-merge(tt.dat.route2, truck.temp, by="FishID", all.x = TRUE)
tt.dat.route2$Year<-as.numeric(as.character(tt.dat.route2$Year))
tt.dat.route3<- tt.dat.route2[!(tt.dat.route2$Release_Location == "BtlCkCNFHWeir" | 
                                  tt.dat.route2$Release_Location == "BtlCkCNFHWeir                                     " 
                                & tt.dat.route2$Year > 2007)  , ]
length(unique(tt.dat.route3$FishID))#296
sapply(X = tt.dat.route2, FUN = function(x) sum(is.na(x)))

### didnt work
# had to deal with some NAs manually 
write.csv(tt.dat.route3, "travel.time_CM.Vemco_v3.csv")

summarise(group_by(tt.dat.route3,Route), count=length(unique(FishID)))
summarise(group_by(tt.dat.route3,tt.grp), count=length(unique(FishID)))
#CenDel                48
#SacR                 174
#SacRSlough            63
#SacRSlough, CenDel    11
#Ben.Ant     1
#Ben.Dec    26
#Chips     269


