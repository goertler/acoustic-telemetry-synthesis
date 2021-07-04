# Clean ybus data - prep for distance matrix calcs
#  M. Johnston
# Thu May 20 11:08:43 2021 ------------------------------

#### Repeat for YBUS data (ybus data = 2016 only)

ybus<- read.csv("data/detection_data/ybus_detections.csv", stringsAsFactors = F)
route <- read.csv("data/CV_data/ybusCV.csv", stringsAsFactors = F)

## Merge pertinent info
ybus <- dplyr::rename(ybus, TagID = Transmitter) 
ybus <- dplyr::left_join(ybus, route, by = "TagID")
ybus <- ybus[, -c(1, 6, 8, 11)]
str(ybus)

## Reformat dates, make sure we have hours/mins/secs
ybus$DetectDate <- as.POSIXct(ybus$DateTime, format = "%Y-%m-%d %H:%M:%S", tz='EST')
sum(is.na(ybus$DetectDate)) # need to deal with NAs in date
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
all_detects <- ybus[order(ybus$FishID, ybus$DetectDate), ] 
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
