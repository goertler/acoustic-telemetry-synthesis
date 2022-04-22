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
#dm_closed  <- read.csv("data/distance_matrices/JSATs_dist_matrix_DCC-Yolo-Tisdale_closed_new.csv", stringsAsFactors = FALSE)
setwd("C:/Users/pgoertler/OneDrive - deltacouncil/acoustic-telemetry-synthesis")
dm_yoloace<-read.csv("Distance_Matrix_YBUS_corr_07_21.csv")

## Load Routes
#route <- read.csv("data/CV_data/JSATS_CV.csv", stringsAsFactors = FALSE)
setwd("C:/Users/pgoertler/OneDrive - deltacouncil/DFA final/acoustic-telemetry-synthesis")
sub <- read.csv("dat4Von.csv")

route = sub[ , c("FishID", "Route")]
stopifnot(sum(duplicated(route$FishID)) == 0) # make sure each row is distinct

## Load clean JSATs detections of interest
#jsats = readRDS("data_clean/jsats_dfa_detects.rds") # created in R/clean_all_detects.R, which sources clean_tagging_metadata.R
jsats$DetectDate = as.Date(jsats$DateTime_PST)
ybus = read.csv("ybus_clean.csv")
str(ybus)
ybus$DetectDate = as.Date(ybus$DetectDate)
colnames(ybus)[8] <- "Name_corr"
#-------------------------------------------------------#

# big test: all fish
bigtest = dpd_allfish(ybus, dm_yoloace) # 4K+ fish
saveRDS(bigtest, "data_clean/distance_per_day_ybus.rds")

#bigtest = readRDS("data_clean/distance_per_day.rds")
#dt16 = bigtest[lubridate::year(bigtest$Date) == 2016, ]

library(dplyr)

bigtest %>% 
  group_by(FishID) %>% 
  arrange(Date) %>% 
  padr::pad(interval = "day",
            start_val = min(bigtest$Date),
            end_val = max(bigtest$Date)) %>% 
  ungroup() -> bigtest

bigtest = tidyr::pivot_wider(bigtest, names_from = Date, values_from = Distance_m)

#bigtest[1:10, 1:5]
dates = as.character(sort(as.Date(colnames(bigtest)[2:73])))

bigtest = bigtest[ , c("FishID", dates)]

write.csv(bigtest, "results/dfa_2016_ybus.csv", row.names = FALSE)
