#-------------------------------------------------------#
# Distance Matrix calcs
# refactored, M. Johnston
# Mon Feb 15 11:52:30 2021 ------------------------------

## In the clean_all_detects.R script:
 # - only need fish (JSATS) from: 2013, 2016, and 2017
 # - only need fish (all groups) that reach either Ben or Chipps recs
 # - End Recs: "BeniciaW"  "ChippsW"

#-------------------------------------------------------#
# Objective:  using the detections and the distance matrices appropriate to a fish's route, calculate the distance traveled by each fish on each day.  

# If detections are separated by many days, spread the distance evenly across the interval.

# Output: tabular form, column for each FishID, row for each day, distance (abs(distance_traveled_in_meters))) in each cell.
#-------------------------------------------------------#
# Re-factored approach to creating distance traveled matrix:

# Filter down to only fish detected to either Benicia or Chipps.  No vemco receivers at Benicia (ybus data and my fish).  JSATs: ChippsE, ChippsW, or Benicia.

# rename some receiver locations to agree with the names in the distance matrices

# order detections by FishID and date; filter down to the first detection at each receiver

# create lagged detection columns; create movement column by pasting

# join with distance matrix data to get distance each movement represents

# group by fishID & date; the total distance traveled column = the distance traveled from the previous recorded movement to that date

# Create column of lagged difftime = number of days elapsed since recorded previous movement

# 

# FishID in columns, dates in the rows, distance traveled in cells (absolute)

# Different files for each year - 3 years (2013, 2016, 2017) - water year is fine.

# columns needed:
# FishID, DetectDateTime, GEN, Movement, Distance_m.

source("R/utils.R")

# load distance matrix (using DCC closed only)
dm_closed  <- read.csv("data/distance_matrices/JSATs_dist_matrix_DCC-Yolo-Tisdale_closed_new.csv", stringsAsFactors = FALSE)

## Load Routes
route <- read.csv("data/CV_data/JSATS_CV.csv", stringsAsFactors = FALSE)
route = route[ , c("FishID", "Route")]
stopifnot(sum(duplicated(route$FishID)) == 0) # make sure each row is distinct

## Load clean JSATs detections of interest
jsats = readRDS("data_clean/jsats_dfa_detects.rds") # created in R/clean_all_detects.R, which sources clean_tagging_metadata.R
jsats$DetectDate = as.Date(jsats$DateTime_PST)

#-------------------------------------------------------#
test_3fish = jsats[jsats$FishID %in% c(unique(jsats$FishID)[1:3]), ]

test = dpd_allfish(test_3fish) 

bigtest = dpd_allfish(jsats) # 
