#-------------------------------------------------------#
# Distance Matrix calcs for JSATs data
# refactored, M. Johnston
# Mon Feb 15 11:52:30 2021 ------------------------------

## In the clean_all_detects.R script:
 # - only need fish (JSATS) from: 2013-2017
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
dm_closed  <- read.csv("data_clean/JSATS/JSATs_dist_matrix_DCC-Yolo-Tisdale_closed_new.csv", stringsAsFactors = FALSE)

## Load clean JSATs detections of interest
jsats = readRDS("data_clean/JSATS/jsats_detects2013-2017.rds") #
jsats$DetectDate = as.Date(jsats$DateTime_PST)

#-------------------------------------------------------#

# big test: all fish
f1 = split(jsats, jsats$FishID)
f1 = f1[sapply(f1, nrow) > 0] # only keep obs with > 1 det

f2 = lapply(f1, dpd_allfish, distance_matrix = dm_closed)

f2 = do.call(rbind, f2)

# subset the dataset to only the years we need
years = 2013:2017

f3 = subset(f2, lubridate::year(f2$Date) %in% years)

f3_split = split(f3, lubridate::year(f3$Date)) 

ans = lapply(f3_split, make_matrix)

mapply(write.csv, 
       x = ans, 
       file = paste0("results/JSATS/", names(ans), "_jsats_dpd.csv"), 
       row.names = FALSE)

