####### lines 1-91 from csvs_tidying.R
library(readr)
library(dplyr)
library(reshape2)
library(tidyr)
library(lubridate)
options(stringsAsFactors = FALSE)
Sys.timezone(location = FALSE)
Sys.setenv(TZ = "Pacific/Pitcairn")
Sys.timezone()

setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections/YBUS/CFS_analysis")

dwrfiles <- list.files(path = "../Acoustic.Telemetry.Data/DWR_UCD_Yolo.Bypass_Receivers 8-19-16/ReceiverLogs/", pattern = "*.csv", full.names = TRUE) #create list of files by character name; change working directory first

usgsfiles <- list.files(path = "../Acoustic.Telemetry.Data/USGS Telemetry.reciever.data.8-1-16/USGS Receiver Files 8-1-16/Chipps.I upto 8-1-16/", pattern = "*.csv", full.names = TRUE)

usgsfiles2 = list.files(path = "../Acoustic.Telemetry.Data/USGS Telemetry.reciever.data.8-1-16/USGS Receiver Files 8-1-16/", pattern = "*.csv", full.names = TRUE)

files = c(dwrfiles, usgsfiles, usgsfiles2)

d <- plyr::adply(files, 1, read.csv) #combine into one big dataframe

d <- tbl_df(d)
d
d <- d[ , c(2:4, 9)] # select Time.corrected through Transmitter columns + station.name column
colnames(d) <- c("DateTimeUTC", "Monitor" , "TagID", "Station_raw")
d # 69974 obs; Chipps included
## I'm seeing 1,145,986 x 4 (PG)
d <- separate(data=d, col=Monitor, into=c("one" , "Receiver"), sep="\\-") # get rid of VR2W-
d <- separate(data=d, col=TagID, into=c("one1" , "Map" , "TagID"), sep="\\-") # get rid of code space
d <- select(d, DateTimeUTC, Receiver, TagID, Station_raw)
d

#################################################
# create clean copy for further manipulation #

f <- d
str(f)
f$DateTimeUTC <-ymd_hms(f$DateTimeUTC) # Convert to POSIXct
f$Receiver <- as.numeric(f$Receiver) # Convert Receiver S/Ns to numeric
f$TagID <- as.numeric(f$TagID) # Convert tagIDs to numeric
str(f)

f <- arrange(f, DateTimeUTC) # arrange by ascending date; check the year range
nacheck <- which(is.na(f$DateTimeUTC)) # should be empty ##yes (PG)
tail(f$DateTimeUTC)

####

datecheck <- f$DateTimeUTC
datecheck <- as.POSIXlt(datecheck)
range(datecheck)  #currently contains from 9/11/2014 - 7/6/2016; need to subset 
##yes (PG)

## Filter for receivers we want ## decided not to run this, skip to line 123
#library(readxl)

#recs = read_excel("StationIDs.xlsx")
#head(recs)
#recs = select(recs, Receiver, Station)

#length(unique(recs$Receiver)) ## 116 (PG)
#length(unique(f$Receiver)) ## 70 (PG)

#f <- filter(f, Receiver %in% recs$Receiver)

#length(unique(f$Receiver)) ## 70 (PG)

#f <- left_join(f, recs) # we loose a lot of rows here (PG)
#unique(f$Station)
## See which receivers aren't represented (presumably 0 detections)
#a = f %>% 
#  select(Receiver, Station)
#a = anti_join(recs, a)
#a # all either at the I80 array or at Chipps; this is probably okay.
## 46, not sure where they are (PG)
# Export for SBM 2020:
#saveRDS(f, "~/NonDropboxRepos/SBM_flow-surv/data/YBUS2016/YSacReleases_dets.rds")

## Filter for Tags We Want # missing time (only date)
library(readxl)
tag.df <- read_excel("TagIDs.xlsx")
str(tag.df)
tag.df = select(tag.df, TagID = `TAG ID`, Release = `RELEASE EFFORT`, RelLoc = `RELEASE LOCATION`,
                RelDate = `RELEASE DATE`)
tag.df
length(unique(tag.df$TagID))
# 1197 (PG)
unique(tag.df$RelDate) # 2016-02-21 to 2016-03-18


####### need to add detection data to 1197 from 2016 (tag.df)
head(f)
#head(tag.df)
#new.dat<-merge(f, tag.df, by="TagID", all.y = TRUE)
#head(new.dat)
#datecheck <- new.dat$DateTimeUTC
#datecheck <- as.POSIXlt(datecheck)
#range(datecheck, na.rm = TRUE) # "2016-02-23 00:37:50 UTC" "2016-05-02 02:59:53 UTC"

# need to add time to release date # didnt work, release date not in f
#unique(new.dat$RelDate)

#rel.min <- data.frame(TagID = NA, time = strptime("1900-01-01 11:11:11",format="%Y-%m-%d %H:%M:%OS", tz="UTC"))

#for(i in unique(new.dat$TagID)){
#  temp.dat <- new.dat[new.dat$TagID == i,]
#  min.time <- min(temp.dat$DateTimeUTC)
#  temp.results <- data.frame(TagID = i, time = as.POSIXlt(min.time, format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
#  rel.min<-rbind(rel.min, temp.results)
#}
#head(rel.min)
#rel.min<-rel.min[-1,]
#length(unique(rel.min$TagID)) #1197
#write.csv(rel.min, "YBUS.16_rel.csv")

# double check
check<-merge(new.dat, rel.min, by="TagID", all=TRUE)
check$check<-ifelse(check$RelDate == as.Date(check$time.rel), TRUE, FALSE)
head(check) # zero data, this is not corect

# found this dataset
setwd("~/YBUS/Fish.Release.Data")
library(readxl)
tag.df_v2 <- read_excel("YBUS_TAG_IDS_(1) - Copy.xlsx")
head(tag.df_v2)
tag.df_v2$date<-as.Date(tag.df_v2$date)
str(tag.df_v2)
tag.df_v2$rel <- paste(tag.df_v2$date, tag.df_v2$time, sep=' ')
tag.df_v2$rel <- ymd_hms(tag.df_v2$rel)
length(unique(tag.df_v2$`TAG ID`)) #1197

# first chip detection

setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections/YBUS/Acoustic.Telemetry.Data/USGS Telemetry.reciever.data.8-1-16")
YBUS.rec<-read_excel("YBUS Receiver Sites 8-1-16 CVallee.xlsx")
View(YBUS.rec)
YBUS.rec<-YBUS.rec[,c(1:2)]
unique(YBUS.rec$...2)
colnames(YBUS.rec) <- c("Receiver", "Site_Name")
loc<-"Chips"
Site_Name <-c("MAL.1","MAL.2","MAL.3","MAL.4","MAL.5","MAL.6",
              "MAL.7a", "MAL.7b", "MAL.8a", "MAL.8b", "MAL.9a",
              "MAL.9b", "MAL.10a", "MAL.10b", "MAL.11a", "MAL.11b",  
              "MAL.12a","MAL.12b","MAL.13", "MAL.14", "MAL.15","MAL.16", "MAL.17", "MAL.18")

new.loc<-data.frame(loc, Site_Name)
YBUS.rec<-merge(YBUS.rec, new.loc, by="Site_Name", all=TRUE)

#
new.dat.2<-merge(f, YBUS.rec, by="Receiver", all.x=TRUE)
head(new.dat.2)
chip<-subset(new.dat.2, loc=="Chips")

chip.min <- data.frame(TagID = NA, time = strptime("1900-01-01 11:11:11",format="%Y-%m-%d %H:%M:%OS", tz="UTC"))

for(i in unique(chip$TagID)){
  temp.dat <- chip[chip$TagID == i,]
  min.time <- min(temp.dat$DateTimeUTC)
  temp.results <- data.frame(TagID = i, time = as.POSIXlt(min.time, format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
  chip.min<-rbind(chip.min, temp.results)
}
head(chip.min)
chip.min<-chip.min[-1,]
length(unique(chip.min$TagID)) #663
# 55% made it to Chips (Feb to April)

# travel time
#colnames(rel.min)[2] <- "time.rel"
# 12 fish with zero value
tag.df_v2<- tag.df_v2[,c(3,4,9)]
colnames (tag.df_v2)[1] <- "TagID"
colnames(chip.min)[2] <- "time.chip"
tt.dat<-merge(chip.min, tag.df_v2, by="TagID")
head(tt.dat)
length(unique(tt.dat$TagID)) #663

# only want 2016
tt.dat<-within(tt.dat, Year<-format(tt.dat$rel, "%Y"))
unique(tt.dat$Year)

tt.dat$travel_time <- as.numeric(difftime(tt.dat$time.chip, tt.dat$rel, tz= "UTC", units = "days"))
head(tt.dat)
# Tag ID - 39131 detected at Chips before released (negative travel time)

# Tag ID - 39131 check
head(f)
tag.check<-subset(f, TagID == "39131")
head(tag.check)# chose to remove, detected in Sac twice after Chips
tt.dat<-tt.dat[!(tt.dat$TagID == "39131"),]

write.csv(tt.dat, "travel_time_YBUS.csv")

# start master file
#tag.df2<-merge(tag.df[,c(1:3)], rel.min, by="TagID")

# release loc from SAIL
head(tt.dat)
colnames (tt.dat)[3] <- "RelLoc"
unique(tt.dat$RelLoc)
tt.dat$Release_Group_SAIL <- ifelse(tt.dat$RelLoc == "TOE DRAIN/TULE CANAL" | tt.dat$RelLoc == "YOLO",
                                    "Tidal Delta", "Middle Sacramento River")

# run
tt.dat$Group <- "Late-Fall"
tt.dat$origin <- "hatchery"

write.csv(tt.dat, "travel_time_YBUS.csv")

# transport distance
# Coleman National Fish Hatchery in Anderson, California
# 285.056 ("YOLO" or Freemont Weir)
# "TOE DRAIN/TULE CANAL" = Rd22

# temperature at release

# length, weight (length average is in report)
# 108 to 218 mm in fork length and from 13.4 to 116.8 g in weight, with means of 165.3 mm and 50.0 g

# route
head(new.dat.2)
unique(new.dat.2$Site_Name)
loc.dat<-unique(new.dat.2[,c(1,4:6)]) #74

# lat/lon from Tracy's shiny
setwd("~/acoustic-telemetry-synthesis/data")
lat.lon<-read.csv("all_receivers.csv")
head(lat.lon)
colnames (lat.lon)[1] <- "Site_Name"
loc.dat.latlon<-merge(loc.dat, lat.lon, by="Site_Name") #48

all.rec<-unique(loc.dat$Receiver)
dat.rec<-unique(loc.dat.latlon$Receiver)
length(setdiff(all.rec,dat.rec)) #22

# found this file...
setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections/YBUS/Acoustic.Telemetry.Data/DWR_UCD_Yolo.Bypass_Receivers 8-19-16")
ucd.station<-read_excel("2016_YBUS_Station_Log.xlsx")
head(ucd.station)
colnames(ucd.station) <- c("VUE.Station.Name","KMZ.Name","Reciever.Model","Receiver",
                           "Firmware.Update","MAP","Lat","Lon","Date.Initialized","Date.Deployed",
                           "Date.Retrieved","Date.Offloaded","Notes")

ucd.station<-ucd.station[c(3:39),c(4,7:8)]
ucd.station<-data.frame(na.omit(ucd.station))
head(loc.dat)
loc.dat2<-merge(loc.dat, ucd.station, by="Receiver", all.x=TRUE)
head(loc.dat2)
head(loc.dat.latlon)
loc.dat2<-loc.dat2[,c(1,5,6)]
loc.dat.latlon<-loc.dat.latlon[,c(2,5,6)]

loc.fin<-rbind(loc.dat2,loc.dat.latlon)
loc.fin<-na.omit(loc.fin) 
write.csv(loc.fin, "loc.fin.csv")
# looked on a map to assign route
location<-read.csv("loc.fin.csv")
head(location)
unique(location$route)
unique(location$exit.enter)
unique(location$notes)
head(new.dat.2)
route.dat<-merge(new.dat.2[,c(1:3)], location, by="Receiver", all.x=TRUE)
head(route.dat)

### start here, need to find MJ file (line 93 in Travel_Time_MJ)
setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections/YBUS/CFS_analysis")
tt.dat<-read.csv("travel_time_YBUS.csv")
length(unique(tt.dat$TagID)) #662

route.dat.sub <- route.dat[route.dat$TagID %in% tt.dat$TagID,]

length(unique(route.dat.sub$TagID))#662
head(route.dat.sub)
route.dat.yolo<-subset(route.dat.sub, route.2 =="Yolo")
length(unique(route.dat.yolo$TagID)) #512
route.dat.SacRSlough<-subset(route.dat.sub, route.2 =="SacRSlough")
length(unique(route.dat.SacRSlough$TagID)) #362
route.dat.CenDel<-subset(route.dat.sub, route.2 =="CenDel")
length(unique(route.dat.CenDel$TagID)) #299

# sort if detected at slough, entrance or exit sites
Fish.List <- data.frame(TagID = unique(route.dat.yolo$TagID), Ent.code = NA, Exit.Code = NA, Slough.Count = NA, antislough.code=NA)

for(i in 1:nrow(Fish.List)){
  temp.dat <- subset(route.dat.yolo, TagID == Fish.List[i, "TagID"])
  temp.ent <- subset(temp.dat, exit.enter == "enter")
  temp.exit <- subset(temp.dat, exit.enter == "exit")
  temp.slough.count <- subset(temp.dat, exit.enter == "slough")
  temp.antislough.code <- subset(temp.dat, exit.enter == "anti-slough")
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
length(unique(Fish.List$TagID)) #662

Fish.List.Yolo = Fish.List # many detected only at the exit of Yolo, need to compare with release
Fish.List.SacRSlough = Fish.List # no internal slough detections
# need to investiage TagID 	36588, 36601, 36595, 36448
Fish.List.CenDel = Fish.List # all detected @ 2 or greater are in slough or entrance/exit

# checking release location for Yolo route
new.yolo<-merge(Fish.List.Yolo, tt.dat[,c(2,4,8)], by="TagID", all.x=TRUE)
# going to include 1s if released in Yolo (no anti-sough, so think its right)

new.yolo$route <- ifelse(new.yolo$Total.count == 2, "Yolo",
                         ifelse(new.yolo$Total.count == 1 & new.yolo$Release_Group_SAIL == "Tidal Delta", "Yolo",
                                "SacR"))

Fish.List.CenDel$route <- ifelse(Fish.List.CenDel$Total.count >= 2, "CenDel",
                                 "SacR")

# 36588, 36601, 36595, 36448 
head(route.dat)
route.check <- subset(route.dat, route.dat$TagID == 36588 | 
                        route.dat$TagID == 36601 | 
                        route.dat$TagID == 36595| 
                        route.dat$TagID == 36448)

#all detected at entrance & exit, but also anti-slough
# 36595, 36448 only detected at anti-slough once
Fish.List.SacRSlough$route <- ifelse(Fish.List.SacRSlough$Total.count == 2 & Fish.List.SacRSlough$antislough.code == 0, "SacRSlough",
                                "SacR")
route.list<-rbind(Fish.List.SacRSlough[,c(1,7)], 
                  Fish.List.CenDel[,c(1,7)], new.yolo[,c(1,9)])

route.list <- unique(route.list)
# double check
write.csv(route.list, "route.list.YBUS.csv")
route.list_v2 <- read.csv("route.list.YBUS.csv")
head(route.list_v2)
head(tt.dat)

tt.final <- merge(tt.dat, route.list_v2, by="TagID", all.x=TRUE)
unique(tt.final$route)
tt.final$route[is.na(tt.final$route)] <- "SacR"

write.csv(tt.final, "travel_time_YBUS.final.csv")

hist(tt.final$travel_time)
summarise(group_by(tt.final,route), count=length(unique(TagID)))
#CenDel        69
#SacR         344
#SacRSlough    62
#Yolo         187
length(unique(tt.final$TagID)) #662
