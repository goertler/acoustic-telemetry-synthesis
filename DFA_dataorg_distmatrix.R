
#setwd("C:/Users/TGrimes/Desktop/")
setwd("C:/Users/fish/Downloads/DFA/DFA")
setwd("C:/Users/pgoertler/Desktop/Telemetry Synthesis/DFA.CMedits")

#devtools::install_github("Myfanwy/fishtrackr")

#library(fishtrackr)
library(dplyr)
library(padr)
library(data.table)
library(tidyr)
library(lubridate)

## Load receiver rkm corrections. These were not used afterall. 
#rkm_correct <- read.csv('Adjusted_genloc_rkm_for_Sac_fish_going_downstream.csv')
## IN GENERAL, RKM ARE GOOD, EXCEPT FOR FISH THAT TRAVELED THROUGH YOLO BYPASS OR DCC...THOSE GET WONKY, BUT ARE LIKELY THE EXCEPTION TO MOST TRAVEL PATHWAYS ##

## Load receiver distance matrices
#dist_DCC_open_Yolo_Tisd_closed <- read.csv("JSATs_dist_matrix_DCC_open-Yolo-Tisdale_closed_new.csv", stringsAsFactors = F)
dist_DCC_Yolo_Tisd_closed  <- read.csv("JSATs_dist_matrix_DCC-Yolo-Tisdale_closed_new.csv", stringsAsFactors = F)
dist_DCC_Yolo_Tisd_open <- read.csv("JSATs_dist_matrix_DCC-Yolo-Tisdale_open_new.csv", stringsAsFactors = F)

## Load tag metadata

tagging_meta <- read.csv("Tagging_Fish_Metadata.txt", stringsAsFactors = F)
tagging_meta$Rel_datetime <- as.POSIXct(tagging_meta$Rel_datetime, tz="Etc/GMT+8", format = "%m/%d/%Y %H:%M:%S")

## Load detections
temp = list.files(path = "C:/Users/TGrimes/Desktop/detection_files/study-detections",full.names = T, pattern="*.csv")
temp = list.files(path = "C:/Users/fish/Downloads/DFA/DFA/study-detections",full.names = T, pattern="*.csv")
temp = list.files(path = "C:/Users/pgoertler/Desktop/Telemetry Synthesis/DFA.CMedits/study-detections",full.names = T, pattern="*.csv")

for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i], stringsAsFactors = F))

## Bind all detections into one mother dataframe
all_detects <- do.call("rbind", mget(temp))

## remove temp files
for (i in 1:length(temp)) assign(temp[i], rm)

#### The following few lines of code massage things that could probably be rectified in the JSATS database ####
## Change fish_type "Chinook" to "RBDD Chinook" to be more informative
tagging_meta[tagging_meta$Fish_Type == "Chinook" & tagging_meta$StudyID == "RBDD-2017", "Fish_Type"] <- "RBDD Chinook"

## Change Fish_Type "Fall run Chinook" for StudyID "ColemanFall_2016" to more informative "CNFH Fall Chinook"
tagging_meta[tagging_meta$Fish_Type == "Fall run Coleman", "Fish_Type"] <- "CNFH Fall Chinook"

## Merge in pertinent data for grouping by runs/years
all_detects <- merge(all_detects, tagging_meta[, c("FishID", "StudyID", "Fish_Type", "Rel_loc", "Rel_rkm")], by = "FishID")

all_detects$DetectDate<- as.POSIXct(all_detects$DateTime_PST, tz="Etc/GMT+8", format = "%m/%d/%Y %H:%M:%OS")

all_detects <- select(all) # PG - got an error here
all_detects<- select(all_detects,-c(19:22))
all_detects <- merge(all_detects, tagging_meta[, c("FishID", "StudyID", "Fish_Type", "Rel_loc", "Rel_rkm")], by = "FishID")

## delta and yolo measurements for non-mainstem routes were done from downstream going up, and should be
## upstream going down for this analysis. Therefore, I have remeasured some of the most frequented delta receivers from upstream going down.

## Remove detections at some key locations which seem to have a lot of false detects
all_detects <- all_detects[!all_detects$GEN %in% c("Battle_Conf", "ButteBr", "AbvColusaBrRT", "BattleCk2", "BattleCk3",
                                                   "Sac_Ist_Bridge_Rel","MillCk2_Rel","FR_Vance_Rel", "MinerNorth"),] #added Sac_Ist_Bridge_Rel,MillCk2_Rel,and FR_Vance_Rel because they are not in the distance matrix

## Remove detections for certain fish, seems to have bad data (maybe 2 tags in system with that tag code? Or just false detects)
all_detects <- all_detects[!all_detects$FishID %in% c("CFC2013-080",
                                                      "CFR2016-079"),]

## Now fix some GEN loc names that don't exactly match in detection files vs dist matrices
all_detects[which(all_detects$GEN == "BattleCk_RST"),"GEN"] <- "BattleCk_RST_Rel"
all_detects[which(all_detects$GEN == "FR Gridley Release"),"GEN"] <- "Gridley_Rel"
all_detects[which(all_detects$GEN == "RBDD Release"),"GEN"] <- "RedBluffDivDam"
## Fixing more GEN loc names that don't match
all_detects[which(all_detects$GEN == "MillCk_RST"),"GEN"] <- "MillCk_RST_Rel"
all_detects[which(all_detects$GEN == "I80-50_Br"),"GEN"] <- "I80_Br"
all_detects[which(all_detects$GEN == "BeniciaW"),"GEN"] <- "Benicia"
all_detects[which(all_detects$GEN == "BlwGeorg_1"),"GEN"] <- "BlwGeorgiana"
all_detects[which(all_detects$GEN == "SutterBypass_Weir2_RST_Rel"),"GEN"] <- "SutterBypass Weir2 RST"
all_detects[which(all_detects$GEN == "GeorgSl_1"),"GEN"] <- "Georgiana_Slough"
all_detects[which(all_detects$GEN == "FR_Gridley_Rel"),"GEN"] <- "Gridley_Rel"
all_detects[which(all_detects$GEN == "RBDD_Rel"),"GEN"] <- "RedBluffDivDam"
all_detects[which(all_detects$GEN == "RBDD1"),"GEN"] <- "RedBluffDivDam"
all_detects[which(all_detects$GEN == "RBDD2"),"GEN"] <- "RedBluffDivDam"
all_detects[which(all_detects$GEN == "FR_Boyds_Rel"),"GEN"] <- "FR Boyds Release"
all_detects[which(all_detects$GEN == "Tower_Bridge_Rel"),"GEN"] <- "TowerBridge"
all_detects[which(all_detects$GEN == "AR_Sunrise_Ramp_Rel"),"GEN"] <- "AR_Sunrise_Ramp"
all_detects[which(all_detects$GEN == "DeerCk_RST_Rel"),"GEN"] <- "DeerCk_RST"
all_detects[which(all_detects$GEN == "FreeportDiv"),"GEN"] <- "Freeport"
## combine certain locations where receivers are too close together
all_detects[which(all_detects$GEN == "ChippsE"),"GEN"] <- "ChippsW"

## remove mokbase since it is one site we don't have distance matrix for
all_detects <- all_detects[which(all_detects$GEN != "MokBase"),]



## First merge in the new correct rkms
all_detects <- all_detects[order(all_detects$FishID, all_detects$DetectDate),] 
all_detects$prev_FishID <- NA
all_detects$prev_GEN <- NA
all_detects$prev_DetectDate <- NA
all_detects[,c("prev_FishID", "prev_GEN", "prev_DetectDate")] <- as.data.frame(shift(x = all_detects[,c("FishID", "GEN", "DetectDate")], n = 1, fill = NA, type = "lag") ) 
first_detects <- all_detects[which(all_detects$FishID != all_detects$prev_FishID | all_detects$GEN != all_detects$prev_GEN),] #first detections or first detections at a new receiver
first_detects$movement <- paste(first_detects$prev_GEN,"-", first_detects$GEN, sep = " ")
## use appropriate dist matrix based on determined route (copying route from CV file)
#setwd("S:/Science/STAFF/Grimes/Telemetry Synthesis/CV and Heatmap code")
## Using this file to bind routing determination
route<- read.csv("JSATS_CV.csv")
## Selecting relevant coulmns to bind
route<- route%>%select(FishID, Route)%>%
  distinct()
## Joining the routing determination to the detection data
## and removing fish that have no routing determination
first_detects_routes<- left_join(first_detects, route, by="FishID")%>%
  filter(!is.na(Route))
length(unique(first_detects_routes$FishID))#748fish- was 868
test <- merge(first_detects_routes, dist_DCC_Yolo_Tisd_closed[,c("Name", "Total_Length_m")], by.x = "movement", by.y = "Name", all.x = T)
## removes unneccessary columns from distance matrices to prepare for a join with the detection data

test1<- filter(first_detects_routes,Route!="Yolo_Bypass")
test1<- merge(test1, dist_DCC_Yolo_Tisd_closed[,c("Name","Total_Length_m")], by.x  ="movement", by.y = "Name", all.x = T)
yolo<- filter(first_detects_routes, Route=="Yolo_Bypass")
yolo<- merge(yolo, dist_DCC_Yolo_Tisd_open[,c("Name","Total_Length_m")], by.x  ="movement", by.y = "Name", all.x = T)
first_detects_routes<- bind_rows(test1, yolo)
first_detects_routes <- first_detects_routes[order(first_detects_routes$FishID, first_detects_routes$DetectDate),]
first_detects_routes[which(first_detects_routes$FishID != first_detects_routes$prev_FishID), "Total_Length_m"] <- 0 

first_detects1 <- first_detects_routes %>%
  group_by(FishID) %>%
  mutate(cum_length = cumsum(Total_Length_m))

View(aggregate(first_detects1$cum_length, by = list(first_detects1$FishID), max))
first_detects1$date <- as.Date(first_detects1$DetectDate)
first_detects1$mov_rate <- (first_detects1$Total_Length_m/1000) / as.numeric(difftime(first_detects1$DetectDate,first_detects1$prev_DetectDate, units = "days") )
#View(aggregate(first_detects1$Total_Length_m, by = list(FishID = first_detects1$FishID, date = first_detects1$date), sum))
#unique(first_detects1$movement[!(first_detects1$movement %in% dist_DCC_Yolo_Tisd_closed$Name)])#used to determine with GEN names that didn't match dist matrix names
#df<- first_detects1%>%
 # filter(Total_Length_m==0)%>%
  #ungroup()%>%
  #distinct(movement)

#### break up by study year 

## add water year
first_detects1<- first_detects1%>%
  mutate(wateryr=year(DetectDate))

jsats13<- filter(first_detects1,wateryr==2013)%>%
  droplevels()
length(unique(jsats13$FishID))#61, 81 originally
jsats16<- filter(first_detects1,wateryr==2016)%>%
  droplevels()
length(unique(jsats16$FishID))#149, 149 originally
jsats17<- filter(first_detects1,wateryr==2017)%>%
  droplevels()
length(unique(jsats17$FishID))#449, 549 originally

## Formatting for 2013 DFA analysis
str(jsats13)
jsats13$Date<- as_date(ymd_hms(jsats13$DetectDate))
jsats13<- arrange(jsats13, FishID, DetectDate)#order by tagid and datetime 
jsats13_test<- jsats13 %>%
  group_by(FishID) %>%
  arrange(DetectDate, .by_group = TRUE) %>% # in date/time order
  #mutate(diff = abs(lag(UpdateRkm, default = first(UpdateRkm))- UpdateRkm)) %>% #adds column for absolute value of distance traveled between detections
  select(FishID, Date, Total_Length_m) 

str(jsats13_test)
dfa.jsats13<-  jsats13_test %>% 
  group_by(FishID, Date)%>% 
  summarise(Dist= sum(Total_Length_m))%>% #using sum distance to account for multiple detections per day
  ungroup()%>%
  mutate(Group=1:nrow(.))%>% #create "groups" for each day of detection for each individual
  group_by(FishID)%>%
  pad(interval="day")%>% #adds in days where fish wasn't detected
  fill(Group, .direction = "up")%>% #assigns group to missing detection day by using the following detection day
  group_by(Group)%>%
  mutate(Dist=mean(Dist, na.rm=T)/n())%>% #averages the distance travelled among days with no detection history based on next detection
  ungroup()%>%
  select(-Group)%>%
  spread(key = FishID, value = Dist)

write.csv(dfa.jsats13,"JSATS13_DistTravelbyday_mat.csv", row.names=FALSE)

## Formatting for 2016 DFA analysis
jsats16$Date<- as_date(ymd_hms(jsats16$DetectDate))
jsats16<- arrange(jsats16, FishID, DetectDate)#order by tagid and datetime 
jsats16_test<- jsats16 %>%
  group_by(FishID) %>%
  arrange(DetectDate, .by_group = TRUE) %>% # in date/time order
  #mutate(diff = abs(lag(UpdateRkm, default = first(UpdateRkm))- UpdateRkm)) %>% #adds column for absolute value of distance traveled between detections
  select(FishID, Date, Total_Length_m) 
dfa.jsats16<-  jsats16_test %>% 
  group_by(FishID, Date)%>% 
  summarise(Dist= sum(Total_Length_m))%>% #using sum distance to account for multiple detections per day
  ungroup()%>%
  mutate(Group=1:nrow(.))%>% #create "groups" for each day of detection for each individual
  group_by(FishID)%>%
  pad(interval="day")%>% #adds in days where fish wasn't detected
  fill(Group, .direction = "up")%>% #assigns group to missing detection day by using the following detection day
  group_by(Group)%>%
  mutate(Dist=mean(Dist, na.rm=T)/n())%>% #averages the distance travelled among days with no detection history based on next detection
  ungroup()%>%
  select(-Group)%>%
  spread(key = FishID, value = Dist)

write.csv(dfa.jsats16,"JSATS16_DistTravelbyday_mat.csv", row.names=FALSE)

## Formatting for 2017 DFA analysis
jsats17$Date<- as_date(ymd_hms(jsats17$DetectDate))
jsats17<- arrange(jsats17, FishID, DetectDate)#order by tagid and datetime 
jsats17_test<- jsats17 %>%
  group_by(FishID) %>%
  arrange(DetectDate, .by_group = TRUE) %>% # in date/time order
  #mutate(diff = abs(lag(UpdateRkm, default = first(UpdateRkm))- UpdateRkm)) %>% #adds column for absolute value of distance traveled between detections
  select(FishID, Date, Total_Length_m) 
dfa.jsats17<-  jsats17_test %>% 
  group_by(FishID, Date)%>% 
  summarise(Dist= sum(Total_Length_m))%>% #using sum distance to account for multiple detections per day
  ungroup()%>%
  mutate(Group=1:nrow(.))%>% #create "groups" for each day of detection for each individual
  group_by(FishID)%>%
  pad(interval="day")%>% #adds in days where fish wasn't detected
  fill(Group, .direction = "up")%>% #assigns group to missing detection day by using the following detection day
  group_by(Group)%>%
  mutate(Dist=mean(Dist, na.rm=T)/n())%>% #averages the distance travelled among days with no detection history based on next detection
  ungroup()%>%
  select(-Group)%>%
  spread(key = FishID, value = Dist)

write.csv(dfa.jsats17,"JSATS17_DistTravelbyday_mat.csv", row.names=FALSE)


#### Repeat for YBUS data

ybus<- read.csv("ybus_detections.csv", stringsAsFactors = F)
route<- read.csv("ybusCV.csv", stringsAsFactors = F)

## Merge pertinent info
ybus<- rename(ybus, TagID=Transmitter)
ybus<- left_join(ybus, route, by= "TagID")
ybus<-select(ybus,-c(1,6,8,11))
str(ybus)

## Reformat dates, make sure we have hours/mins/secs
sum(is.na(ybus$DateTime))#before = 0
ybus$DetectDate <- as.POSIXct(ybus$DateTime, format = "%Y-%m-%d %H:%M:%S", tz='EST')# 'EST' fixed the NA dates
sum(is.na(ybus$DetectDate))#after = 1165 -- now 0

## delta and yolo measurements for non-mainstem routes were done from downstream going up, and should be
## upstream going down for this analysis. Therefore, I have remeasured some of the most frequented delta receivers from upstream going down.
library(stringr)
## Now fix some GEN loc names that don't  match in detection files vs dist matrices
ybus[which(ybus$GEN == "3A"|ybus$GEN == "3B"|ybus$GEN == "3C"|ybus$GEN == "3D"),"GEN"] <- "Hood"
ybus[which(ybus$GEN == "MAL.2"|
             ybus$GEN == "MAL.7b"|ybus$GEN == "MAL.11a"|ybus$GEN == "MAL.6"|
             ybus$GEN == "MAL.10b"|ybus$GEN == "MAL.8b"|ybus$GEN == "MAL.4"|
             ybus$GEN == "MAL.14"|ybus$GEN == "MAL.7a"|ybus$GEN == "MAL.8a"|
             ybus$GEN == "MAL.1"|ybus$GEN == "MAL.13"|ybus$GEN == "MAL.17"|
             ybus$GEN == "MAL.11b"|ybus$GEN == "MAL.5"|ybus$GEN == "MAL.12a"|
             ybus$GEN == "MAL.18"|ybus$GEN == "MAL.10a"|ybus$GEN == "MAL.12b"),"GEN"] <- "ChippsE"
ybus[which(ybus$GEN == "Below Lisbon Weir"),"GEN"] <- "YB_LisbonWeir"
ybus[which(ybus$GEN == "Base of Toe Drain"),"GEN"] <- "ToeDrainBase"
ybus[which(ybus$GEN == "CCH.2"|ybus$GEN == "CCH.1"|
             ybus$GEN == "Cache Slough East 1"|ybus$GEN == "Cache Slough East 2"|
             ybus$GEN == "Cache Slough West 1"|ybus$GEN == "Cache Slough West 2"),"GEN"] <- "YB_LibertyIsBase"
ybus[which(ybus$GEN == "GEO.u"|ybus$GEN == "GEO.d"),"GEN"] <- "Georgiana_Slough"
ybus[which(ybus$GEN == "BSBGU.2"|ybus$GEN =="SBGD.2"),"GEN"] <- "BlwGeorgiana"
ybus[which(ybus$GEN == "MOK.u"|ybus$GEN == "MOK.d"),"GEN"] <- "SouthofBW"
ybus[which(ybus$GEN == "SACD.1"|ybus$GEN == "SACD.2"|ybus$GEN == "SACD.3"|
             ybus$GEN == "SACD.4"),"GEN"] <- "SR_Mouth"
ybus[which(ybus$GEN == "SACU.1"|ybus$GEN == "SACU.2"|
             ybus$GEN == "SACU.3"|ybus$GEN == "SACU.4"),"GEN"] <- "Blw_Steamboat"
ybus[which(ybus$GEN == "STMU.1"|ybus$GEN=="STMU.2"),"GEN"] <- "SteamboatSlough"
ybus[which(ybus$GEN == "STMD.1"|ybus$GEN == "STMD.2"|
             ybus$GEN == "STMD.3"|ybus$GEN == "STMD.4"),"GEN"] <- "BaseSutterSteam"
ybus[which(ybus$GEN == "RSTR (Rotary Screw Trap)"),"GEN"] <- "ToeDrainRST"
ybus[which(ybus$GEN == "Road 22"),"GEN"] <- "YB_OldRiverRd"
ybus[which(ybus$GEN == "SBE.u"|ybus$GEN=="SBE.d"),"GEN"] <- "SutterSlough"
ybus[which(ybus$GEN == "SBGU.2"),"GEN"] <- "BlwGeorgiana"
ybus[which(ybus$GEN == "Below Lisbon Weir"),"GEN"] <- "YB_LisbonWeir"
ybus[which(ybus$GEN %in% c("I-80 #1","I-80 #4","I-80 Bridge","I-80 #2","I-80 #7","I-80 #3",
                           "I-80 #6","I-80 #10","I-80 #11","I-80 #8","I-80 #9","I-80 #5")),"GEN"] <- "I80_yolo"
## receivers that probably don't matter for analysis
ybus <- ybus[!ybus$GEN %in% c("2A", "2B", "2C", "2D"),]
ybus <- ybus[!ybus$GEN %in% c("5D","5A", "5B","5C"),]
ybus <- ybus[!ybus$GEN %in% c("1A", "1C", "Above Swanstons Rd. Crossing"),]
## Receivers for which we dont have metadata
missing <- c("BF.1","BF.2","BF.4","LPS.u","LPS.d")
ybus <- ybus[!ybus$GEN %in% missing,]

## First merge in the new correct rkms
all_detects <- ybus[order(ybus$FishID, ybus$DetectDate),] 
all_detects$prev_FishID <- NA
all_detects$prev_GEN <- NA
all_detects[,c("prev_FishID", "prev_GEN")] <- as.data.frame(shift(x = all_detects[,c("FishID", "GEN")], n = 1, fill = NA, type = "lag") ) 
first_detects <- all_detects[all_detects$FishID != all_detects$prev_FishID | all_detects$GEN != all_detects$prev_GEN,] #first detections or first detections at a new receiver
first_detects$movement <- paste(first_detects$prev_GEN,"-", first_detects$GEN, sep = " ")

## removes unneccessary columns from distance matrices to prepare for a join with the detection data

test<- filter(first_detects,Route!="Yolo_Bypass")
test<- merge(test, dist_DCC_Yolo_Tisd_closed[,c("Name","Total_Length_m")], by.x  ="movement", by.y = "Name", all.x = T)
yolo<- filter(first_detects, Route=="Yolo_Bypass")
yolo<- merge(yolo, dist_DCC_Yolo_Tisd_open[,c("Name","Total_Length_m")], by.x  ="movement", by.y = "Name", all.x = T)
first_detects<- bind_rows(test, yolo)
first_detects <- first_detects[order(first_detects$FishID, first_detects$DetectDate),]
first_detects[which(first_detects$FishID != first_detects$prev_FishID), "Total_Length_m"] <- 0 

## Manually add in some distances not in matrix
first_detects[first_detects$movement == "YB_OldRiverRd - I80_yolo", "Total_Length_m"] <- 12880
first_detects[first_detects$movement == "I80_yolo - YB_LisbonWeir", "Total_Length_m"] <- 10966
first_detects[first_detects$movement == "I80_yolo - ToeDrainRST", "Total_Length_m"] <- 25396
first_detects[first_detects$movement == "I80_yolo - ToeDrainBase", "Total_Length_m"] <- 32551
first_detects[first_detects$movement == "I80_yolo - YB_LibertyIsBase", "Total_Length_m"] <- 40798

first_detects1 <- first_detects %>%
  group_by(FishID) %>%
  mutate(cum_length = cumsum(Total_Length_m))

## used to determine with GEN names that didn't match dist matrix names
unique(first_detects1$movement[!(first_detects1$movement %in% dist_DCC_Yolo_Tisd_closed$Name)])
df<- first_detects1%>%
 filter(Total_Length_m==0)%>%
ungroup()%>%
distinct(movement)

## Formatting for DFA analysis
str(first_detects1)
first_detects1$Date<- as_date(ymd_hms(first_detects1$DetectDate))
first_detects1<- arrange(first_detects1, FishID, DetectDate)#order by tagid and datetime 
ybus_test<- first_detects1 %>%
  group_by(FishID) %>%
  arrange(DetectDate, .by_group = TRUE) %>% # in date/time order
  #mutate(diff = abs(lag(UpdateRkm, default = first(UpdateRkm))- UpdateRkm)) %>% #adds column for absolute value of distance traveled between detections
  select(FishID, Date, Total_Length_m) 

str(ybus_test)
dfa.ybus<-  ybus_test %>% 
  group_by(FishID, Date)%>% 
  summarise(Dist= sum(Total_Length_m))%>% #using sum distance to account for multiple detections per day
  ungroup()%>%
  mutate(Group=1:nrow(.))%>% #create "groups" for each day of detection for each individual
  group_by(FishID)%>%
  pad(interval="day")%>% #adds in days where fish wasn't detected
  fill(Group, .direction = "up")%>% #assigns group to missing detection day by using the following detection day
  group_by(Group)%>%
  mutate(Dist=mean(Dist, na.rm=T)/n())%>% #averages the distance travelled among days with no detection history based on next detection
  ungroup()%>%
  select(-Group)%>%
  spread(key = FishID, value = Dist)

#there are NA values in the column Date - fixed line 247
#datetime variable does not vary for 7 of the groups, no padding applied on this / these group(s)
ndates<-summarise(ybus_test, "FishID", ndates = length(unique(Date)))
ndates<-summarise(all_detects, "FishID", ndates = length(unique(DetectDate)))

write.csv(dfa.ybus,"ybus_DistTravelbyday_mat.csv", row.names=FALSE)
