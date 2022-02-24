#-------------------------------------------------------#
# Distance Matrix calcs for JSATs data
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

# load distance matrix (using DCC closed only)
dm_closed  <- read.csv("data/distance_matrices/JSATs_dist_matrix_DCC-Yolo-Tisdale_closed_new.csv", stringsAsFactors = FALSE)

## Load clean JSATs detections of interest
jsats = readRDS("data_clean/jsats_dfa_detects.rds") # created in R/clean_all_detects.R, which sources clean_tagging_metadata.R
jsats$DetectDate = as.Date(jsats$DateTime_PST)

#-------------------------------------------------------#

# big test: all fish
bigtest = dpd_allfish(jsats, dm_closed) # 4K+ fish
saveRDS(bigtest, "data_clean/distance_per_day.rds")

bigtest = readRDS("data_clean/distance_per_day.rds")
dt16 = bigtest[lubridate::year(bigtest$Date) == 2016, ]

library(dplyr)

dt16 %>% 
  group_by(FishID) %>% 
  arrange(Date) %>% 
  padr::pad(interval = "day",
            start_val = min(dt16$Date),
            end_val = max(dt16$Date)) %>% 
  ungroup() -> dt16

dt16 = tidyr::pivot_wider(dt16, names_from = Date, values_from = Distance_m)

dt16[1:10, 1:5]
dates = as.character(sort(as.Date(colnames(dt16)[2:124])))

dt16 = dt16[ , c("FishID", dates)]

write.csv(dt16, "results/dfa_2016.csv", row.names = FALSE)
#-------------------------------------------------------#
dt13 = bigtest[lubridate::year(bigtest$Date) == 2013, ]

dt13 %>% 
  group_by(FishID) %>% 
  arrange(Date) %>% 
  padr::pad(interval = "day",
            start_val = min(dt13$Date),
            end_val = max(dt13$Date)) %>% 
  ungroup() -> dt13

csn(dt13)

dt13 = tidyr::pivot_wider(dt13, names_from = Date, values_from = Distance_m)

dt13[1:10, 1:5]
dim(dt13)
dates = as.character(sort(as.Date(colnames(dt13)[2:122])))
dt13 = dt13[ , c("FishID", dates)]

write.csv(dt13, "results/dfa_2013.csv", row.names = FALSE)

#-------------------------------------------------------#

dt17 = bigtest[lubridate::year(bigtest$Date) == 2017, ]

dt17 %>% 
  group_by(FishID) %>% 
  arrange(Date) %>% 
  padr::pad(interval = "day",
            start_val = min(dt17$Date),
            end_val = max(dt17$Date)) %>% 
  ungroup() -> dt17

csn(dt17)

dt17 = tidyr::pivot_wider(dt17, names_from = Date, values_from = Distance_m)

dt17[1:10, 1:5]
dim(dt17)
dates = as.character(sort(as.Date(colnames(dt17)[2:149])))
dt17 = dt17[ , c("FishID", dates)]

write.csv(dt17, "results/dfa_2017.csv", row.names = FALSE)

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

