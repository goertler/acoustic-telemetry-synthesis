# Clean ybus data - prep for distance matrix calcs
#  M. Johnston
# Fri Feb 25 09:15:19 2022 ------------------------------
library(lubridate)
library(stringr)
library(dplyr)
source("R/utils.R")

#TODO: # Fri Apr 15 12:42:47 2022 ------------------------------
#  - bring in fishID key to clean_yoloace and remake results

ybus = read.csv("data/YBUS/ybus_detections.csv", stringsAsFactors = FALSE)
ybus = dplyr::rename(ybus, TagID = Transmitter) 
relgen = readxl::read_excel("data/YBUS/ybusTagIDs.xlsx") # has relGEN
fishID = read.csv("data/common_data/FishID_key.csv") # has fishID
fishID = subset(fishID, TagType == "Vemco" & Year == 2016)

relgen$DateTime_PST = as.POSIXct(paste(as.character(relgen$`RELEASE DATE`), 
                                  relgen$`RELEASE TIME`, sep = " "), 
                            format = "%Y-%m-%d %H%M",
                            tz = "Etc/GMT+8")

relgen$GEN = relgen$`RELEASE LOCATION`
relgen$TagID = relgen$`TAG ID`

# filter relgen down to only the fish in fishID key and vice-versa
fish = relgen[ , c("TagID", "DateTime_PST", "GEN")]

stopifnot(all(!colSums(is.na(fish)))) # Says stop if it's not true that all of the column sums are 0 (false).   all exits as soon as it sees a single false;
# any exits as soon as it sees a single true.  They're high-performance because you don't need to calculate the entire vector. 
sum(unique(ybus$TagID) %in% fish$TagID)
setdiff(ybus$TagID, fish$TagID) # 39131 is missing from FishID_key; this one's track is backwards, gets excluded

ybus$DateTime_PST <- as.POSIXct(ybus$DateTime, format = "%Y-%m-%d %H:%M:%S", tz="Etc/GMT+8")

tmp = rbind(fish, 
            ybus[ , c("TagID", "DateTime_PST", "GEN")])
str(tmp)

ybus2 = merge(tmp, fishID[ , c("TagID", "FishID")], by = "TagID", all = FALSE)

stopifnot(all(!colSums(is.na(ybus2))))

dist_ybus = read.csv("data/distance_matrices/Distance_Matrix_YBUS_corr_07_21.csv")
dist_ybus = select(dist_ybus, Name = Name_corr, Total_Length_m = Total_Length, Location)

setdiff(ybus2$GEN, dist_ybus$Location)
setdiff(dist_ybus$Location, ybus2$GEN)

ybus2$GEN[ybus2$GEN == "TISDALE"] <- "Tisdale Weir"
ybus2$GEN[ybus2$GEN == "VERONA"] <- "Verona"
ybus2$GEN[ybus2$GEN == "TOE DRAIN/TULE CANAL"] <- "Yolo Bypass @ I5"
ybus2$GEN[ybus2$GEN == "YOLO"] <- "Yolo Bypass @ Fremont Weir"
ybus2$GEN[ybus2$GEN == "Cache Slough East 1"] <- "YB_BCE"
ybus2$GEN[ybus2$GEN == "Cache Slough East 2"] <- "YB_BCE2"
ybus2$GEN[ybus2$GEN == "Cache Slough West 1"] <- "YB_BCW"
ybus2$GEN[ybus2$GEN == "Cache Slough West 2"] <- "YB_BCW2"
ybus2$GEN[ybus2$GEN %in% c("I-80 #8", "I-80 #9", "I-80 #7", "I-80 #10", "I-80 #6", "I-80 #2", 
                           "I-80 #1", "I-80 #11", "I-80 Bridge", "I-80 #5", "I-80 #4", "I-80 #3")] <- "YBUS_I80_180_1"

ybus2$GEN[ybus2$GEN == "Road 22"] <- "RD22"
ybus2$GEN[ybus2$GEN == "Base of Toe Drain"] <- "YB_ToeDrain_Base"
ybus2$GEN[ybus2$GEN == "Below Lisbon Weir"] <- "Lisbon Weir 180kHz"
ybus2$GEN[ybus2$GEN == "Above Swanstons Rd. Crossing"] <- "Abv_swanston_180"
ybus2$GEN[ybus2$GEN == "RSTR (Rotary Screw Trap)"] <- "Abv_rstr"

stopifnot(all(setdiff(ybus2$GEN, dist_ybus$Location)))
setdiff(dist_ybus$Location, ybus2$GEN) # it's okay that none of these are in the detections records - they are grouped locations, or HR2s from other studies

# save cleaned data
saveRDS(ybus2, "data_clean/YBUS/ybus_clean.rds")
