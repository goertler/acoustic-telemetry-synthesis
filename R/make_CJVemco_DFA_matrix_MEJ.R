#-------------------------------------------------------#
# Distance Matrix calcs
# refactored, M. Johnston
# Mon Feb 15 11:52:30 2021 ------------------------------

## In the clean_all_detects.R script:
 # - only need fish (JSATS) from: 2013, 2016, and 2017
 # - only need fish (all groups) that reach either Ben or Chipps recs
 # - End Recs: "BeniciaW"  "ChippsW"
 # - Rename some receiver locations to agree with the names in the distance matrices

#-------------------------------------------------------#
# Objective:  using the detections and the distance matrices appropriate to a fish's route, calculate the distance traveled by each fish on each day.  

# If detections are separated by many days, spread the distance evenly across the interval.

# Input needed: cleaned detections data frame with FishID, DateTime_PST, GEN, Year, and RKM; Distance matrix dataframe with columns "Name" (movement name) and "Distance_m"

# Final Final Output needed: tabular form, column for each FishID, row for each day, distance (abs(distance_traveled_in_meters))) in each cell. Different files for each year - 3 years (2013, 2016, 2017) - water year is fine.

#-------------------------------------------------------#
# Re-factored approach to creating distance traveled matrix: dbd_allfish function
#-------------------------------------------------------#

# 1. order detections by FishID and date; filter down to the first detection at each receiver

# 2. create lagged detection columns; create movement column by pasting

# 3. join with distance matrix data to get distance each movement represents

# group by fishID & date; the total distance traveled column = the distance traveled from the previous recorded movement to that date

# 4. Create column of lagged difftime = number of days elapsed since recorded previous movement

# 5. create vector of full dates in between recorded detections for a given movement

# 6. create vector of corresponding distances per day by dividing total distance by difftime

# 7. join to a final data frame
#-------------------------------------------------------#
source("R/utils.R")
library(lubridate)

# load distance matrix (emailed by CJM on 2021-09-)
mat  <- read.table("data/distance_matrices/Vemco_dist_matrix_DCC-Yolo-Tisdale_closed.txt",
                         sep =  ",",
                         header = TRUE)

mat$Total_Length_m = mat$Total_Length # rename so that the dpd_allfish fxn works

# matrix data checks
summary(mat$Total_Length)
stopifnot(sum(rowSums(is.na(mat))) == 0) # there should be no NAs

## Load detections of interest
v = read.csv("data/detection_data/Query3.csv") # uploaded to Sharepoint by Pascale; see README
v$DateTime_PST = force_tz(mdy_hms(v$DetectDate), tzone = "Etc/GMT+8")
v$Date_Released = force_tz(mdy_hms(v$Date_Released), tzone = "Etc/GMT+8") # Release detection
v$DetectDate = force_tz(mdy_hms(v$DetectDate), tzone = "Etc/GMT+8") # re-format
stopifnot(identical(v$DateTime_PST, v$DetectDate))

# TagIDs to keep
keep = read.csv("data/travel_time/travel.time_CM.Vemco_v3.csv") # 296 fish
keep = keep[ , c("FishID", "Release_Location", "Riverkm")]

keepID = unique(keep$FishID)
stopifnot(len(keepID) == sum(keepID %in% unique(v$FishID))) # make sure all the ones we want are found in the detection files

# subset down to only these fish
v = v[v$FishID %in% keepID, ]

# add release detection
# build release detection df
reldet = merge(keep, v[ !duplicated(v$FishID) , c("FishID", "Date_Released")]) # Bring in date_released col
table(reldet$Release_Location, reldet$Riverkm)

# re-structure dataframe to match v
reldet$Lat = reldet$Lon = 0
reldet$DateTime_PST = reldet$DetectDate = reldet$Date_Released
reldet$General_Location = reldet$Location = reldet$Release_Location

sum(!(colnames(reldet) %in% colnames(v)))
identical(sort(colnames(v)) , sort(colnames(reldet)))

# place them in the same order
vnames = colnames(v)
reldet = reldet[ , vnames]

# add release detection df
v2 = rbind(v, reldet)
v2$GEN = v2$General_Location
v2$RKM = v2$Riverkm

#-------------------------------------------------------#
#firsttest
test = dpd_allfish(v2[v2$FishID == "LFC0687", ], dm = mat)
# big test: all fish
bigtest = dpd_allfish(v2, mat) # 296 fish

saveRDS(bigtest, "data_clean/CM_vemco_distance_per_day.rds")

bigtest = readRDS("data_clean/CM_vemco_distance_per_day.rds") # 2007-2011

dt = bigtest[lubridate::year(bigtest$Date) == 2007, ]

library(dplyr)

dt %>% 
  group_by(FishID) %>% 
  arrange(Date) %>% 
  padr::pad(interval = "day",
            start_val = min(dt$Date),
            end_val = max(dt$Date)) %>% 
  ungroup() -> dt07

dt07 = tidyr::pivot_wider(dt07, names_from = Date, values_from = Distance_m)
dim(dt07)
dt07[1:10, 1:5]
dates = as.character(sort(as.Date(colnames(dt07)[2:336])))

dt07 = dt07[ , c("FishID", dates)]

write.csv(dt07, "results/CM_vemco_2007.csv", row.names = FALSE)

#-------------------------------------------------------#
dt08 = bigtest[lubridate::year(bigtest$Date) == 2008, ]

dt08 %>% 
  group_by(FishID) %>% 
  arrange(Date) %>% 
  padr::pad(interval = "day",
            start_val = min(dt08$Date),
            end_val = max(dt08$Date)) %>% 
  ungroup() -> dt08

csn(dt08)

dt08 = tidyr::pivot_wider(dt08, names_from = Date, values_from = Distance_m)
dim(dt08)
dt08[110:115, 1:5]
dates = as.character(sort(as.Date(colnames(dt08)[2:367])))
dt08 = dt08[ , c("FishID", dates)]

write.csv(dt08, "results/CM_vemco_2008.csv", row.names = FALSE)

#-------------------------------------------------------#

dt09 = bigtest[lubridate::year(bigtest$Date) == 2009, ]

dt09 %>% 
  group_by(FishID) %>% 
  arrange(Date) %>% 
  padr::pad(interval = "day",
            start_val = min(dt09$Date),
            end_val = max(dt09$Date)) %>% 
  ungroup() -> dt09

csn(dt09)

dt09 = tidyr::pivot_wider(dt09, names_from = Date, values_from = Distance_m)

dt09[1:10, 1:5]
dim(dt09)
dates = as.character(sort(as.Date(colnames(dt09)[2:366])))
dt09 = dt09[ , c("FishID", dates)]

write.csv(dt09, "results/CM_vemco_2009.csv", row.names = FALSE)

#-------------------------------------------------------#
# DFA Matrix for Yolo/ACE 2012-2013 fish
# Fri Jul  2 14:39:21 2021 ------------------------------

# load distance matrix & prep for dpd_allfish function
# This is not finalized - left as an example only; current results are incorrect as of 
# Sat Jul  3 22:10:25 2021 ------------------------------
#-------------------------------------------------------#
dm_yoloace = read.csv("data/distance_matrices/Distance_Matrix_MJ_corr_mean.csv")
dm_yoloace = dm_yoloace[ , c("Name_corr", "mean_Total_Length")]
colnames(dm_yoloace) = c("Name", "Total_Length_m")
dm_yoloace$Name = gsub("-", " - ", dm_yoloace$Name)

## Load clean detections of interest
yolo_ace = readRDS("data_clean/yoloace_dfa_detects.rds") # created in R/clean_yolo_ace.R
yolo_ace$DetectDate = as.Date(yolo_ace$DateTime_PST)
yolo_ace$Rel_rkm = yolo_ace$Rkm
#-------------------------------------------------------#
# 2012 fish
ya12 = yolo_ace[lubridate::year(yolo_ace$DetectDate) == 2012, ]
ya12 = dpd_allfish(ya12, dm = dm_yoloace) 

library(dplyr)
yamin = min(ya12$Date)
yamax = max(ya12$Date)

ya12 %>% 
  group_by(FishID) %>% 
  arrange(Date) %>% 
  padr::pad(interval = "day",
            start_val = yamin,
            end_val = yamax) %>% 
  ungroup() -> ya

ya = tidyr::pivot_wider(ya, names_from = Date, values_from = Distance_m)
ya[1:10, 1:5]
dim(ya)
dates = as.character(sort(as.Date(colnames(ya)[2:length(ya)])))

ya12 = ya[ , c("FishID", dates)]
write.csv(ya12, "results/yolo_ace2012.csv")

#-------------------------------------------------------#
# 2013 fish
ya13 = yolo_ace[lubridate::year(yolo_ace$DetectDate) == 2013, ]
ya13 = dpd_allfish(ya13, dm = dm_yoloace) 

library(dplyr)
yamin = min(ya13$Date)
yamax = max(ya13$Date)

ya13 %>% 
  group_by(FishID) %>% 
  arrange(Date) %>% 
  padr::pad(interval = "day",
            start_val = yamin,
            end_val = yamax) %>% 
  ungroup() -> ya

ya = tidyr::pivot_wider(ya, names_from = Date, values_from = Distance_m)
ya[1:10, 1:5]
dim(ya)
dates = as.character(sort(as.Date(colnames(ya)[2:length(ya)])))

ya13 = ya[ , c("FishID", dates)]
write.csv(ya13, "results/yolo_ace2013.csv")

