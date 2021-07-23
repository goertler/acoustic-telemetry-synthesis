# Clean ybus data - prep for distance matrix calcs
#  M. Johnston
# Thu May 20 11:08:43 2021 ------------------------------
# edits PG - 
#### Repeat for YBUS data (ybus data = 2016 only)
setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections/YBUS/Acoustic.Telemetry.Data/DWR_UCD_Yolo.Bypass_Receivers 8-19-16")
#ybus<- read.csv("ybus_detections.csv", stringsAsFactors = F) # I have this file name from July 2020 (PG)
#route <- read.csv("data/CV_data/ybusCV.csv", stringsAsFactors = F)
YBUS.rec.det.rel <- read.csv("Vemco.dat_16.csv") # made this in Travel_Time_YBUS.R 7/20/21
head(YBUS.rec.det.rel) 

## Reformat dates, make sure we have hours/mins/secs
YBUS.rec.det.rel$DetectDate <- as.POSIXct(YBUS.rec.det.rel$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz='EST')
sum(is.na(YBUS.rec.det.rel$DetectDate)) # need to deal with NAs in date #0 (PG)

# different names for release locations
unique(YBUS.rec.det.rel$RELEASE.LOCATION) # in different column, need to add to "Name"
# "TISDALE"              "TOE DRAIN/TULE CANAL" "YOLO"                 "VERONA" 
rel.ybus <- unique(YBUS.rec.det.rel[,c(2,9:13)])
rel.ybus$DetectDate <- as.POSIXct(paste(rel.ybus$date, rel.ybus$time), format = "%Y-%m-%d %H:%M:%S", tz='EST')
rel.ybus$Name <- ifelse(rel.ybus$RELEASE.LOCATION == "TISDALE", "Tisdale Weir",
                        ifelse(rel.ybus$RELEASE.LOCATION == "TOE DRAIN/TULE CANAL", "Yolo Bypass @ I5",
                               ifelse(rel.ybus$RELEASE.LOCATION == "VERONA", "Verona", 
                                      ifelse(rel.ybus$RELEASE.LOCATION == "YOLO", "Yolo Bypass @ Fremont Weir", "NA"))))
str(rel.ybus)
YBUS.rec.det.rel <- YBUS.rec.det.rel[,-c(1,9:13)]
YBUS.rec.det.rel_v2 <- dplyr::bind_rows(YBUS.rec.det.rel, rel.ybus[,c(1,7,8)])

# check site names for new detection file
dm_yoloace<-read.csv("Distance_Matrix_YBUS_corr_07_21.csv")
length(setdiff(YBUS.rec.det.rel_v2$Name, dm_yoloace$Location)) #4, compare original site names instead of pairs
#"BCW2" "BCE"  "BCW"  "BCE2" # edits VUE Station Name vs. KMZ Name
YBUS.rec.det.rel_v2[which(YBUS.rec.det.rel_v2$Name == "BCW2"),"Name"] <- "YB_BCW2"
YBUS.rec.det.rel_v2[which(YBUS.rec.det.rel_v2$Name == "BCW"),"Name"] <- "YB_BCW"
YBUS.rec.det.rel_v2[which(YBUS.rec.det.rel_v2$Name == "BCE2"),"Name"] <- "YB_BCE2"
YBUS.rec.det.rel_v2[which(YBUS.rec.det.rel_v2$Name == "BCE"),"Name"] <- "YB_BCE"

## Merge pertinent info
setwd("C:/Users/pgoertler/OneDrive - deltacouncil/DFA final/acoustic-telemetry-synthesis")
sub <- read.csv("dat4Von.csv")
head(sub) 
ybus <- dplyr::left_join(YBUS.rec.det.rel_v2, sub, by = "TagID")
ybus <- ybus[, -c(9,10)] # wasn't clear what this is removing, so I took out row IDs (PG)
str(ybus)
write.csv(ybus, "ybus_clean.csv")

## First merge in the new correct rkms
all_detects <- ybus[order(ybus$FishID, ybus$DetectDate), ] 
all_detects$prev_FishID <- NA
all_detects$prev_GEN <- NA
library(data.table)
all_detects[,c("prev_FishID", "prev_GEN")] <- as.data.frame(shift(x = all_detects[,c("FishID", "Name")], n = 1, fill = NA, type = "lag") ) 
first_detects <- all_detects[all_detects$FishID != all_detects$prev_FishID | all_detects$Name != all_detects$prev_GEN,] #first detections or first detections at a new receiver
first_detects$movement <- paste(first_detects$prev_GEN,"-", first_detects$Name, sep = "")

## removes unnecessary columns from distance matrices to prepare for a join with the detection data

test<- filter(first_detects,Route!="Yolo_Bypass")
test<- merge(test, dm_yoloace[,c("Name_corr","Total_Length")], by.x  ="movement", by.y = "Name_corr", all.x = T)# changed to Vemco distance data (PG)
yolo<- filter(first_detects, Route=="Yolo_Bypass")
yolo<- merge(yolo, dm_yoloace[,c("Name_corr","Total_Length")], by.x  ="movement", by.y = "Name_corr", all.x = T)
first_detects<- bind_rows(test, yolo)
first_detects <- first_detects[order(first_detects$FishID, first_detects$DetectDate),]
first_detects[which(first_detects$FishID != first_detects$prev_FishID), "Total_Length"] <- 0 

# check pairs
chk.pairs <- subset(first_detects,is.na(Total_Length))
length(unique(chk.pairs$movement)) #0

first_detects1 <- first_detects %>%
  group_by(FishID) %>%
  mutate(cum_length = cumsum(Total_Length))

## used to determine with GEN names that didn't match dist matrix names
unique(first_detects1$movement[!(first_detects1$movement %in% dm_yoloace$Location)])
df<- first_detects1%>%
  filter(Total_Length==0)%>%
  ungroup()%>%
  distinct(movement)

## Formatting for DFA analysis
str(first_detects1)
library(lubridate)
first_detects1$Date<- as_date(ymd_hms(first_detects1$DetectDate))
first_detects1<- arrange(first_detects1, FishID, DetectDate)#order by tagid and datetime 
ybus_test<- first_detects1 %>%
  group_by(FishID) %>%
  arrange(DetectDate, .by_group = TRUE) %>% # in date/time order
  #mutate(diff = abs(lag(UpdateRkm, default = first(UpdateRkm))- UpdateRkm)) %>% #adds column for absolute value of distance traveled between detections
  select(FishID, Date, Total_Length) 
