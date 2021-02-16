#-------------------------------------------------------#
# Distance Matrix calcs
# refactored, M. Johnston
# Mon Feb 15 11:52:30 2021 ------------------------------

source("R/utils.R")

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

# get fish paths using tag_tales() function

# order detections by FishID and date; create lagged detection columns, filter down to the first detection at each receiver, create movement column

# join with distance matrix data to get distance each movement represents

# Pad fish movement days; average total distance traveled across that time period

# FishID in columns, dates in the rows, distance traveled in cells (absolute)

# Different files for each year - 3 years (2013, 2016, 2017) - water year is fine.

# columns needed:
# FishID, DetectDateTime, GEN, Movement, Distance_m.

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
str(jsats)
csn(jsats)
# make tagtales fish paths


tt = jsats[jsats$FishID == "ARF2017-005", ]
tt = tt[order(tt$DateTime_PST), c("FishID", "DateTime_PST", "GEN", "Rel_rkm")]

tt$visitID = data.table::rleidv(tt, "GEN")

tt1 = split(tt, tt$visitID) # list of data frames

# subset each data frame down to two rows: the one that has the minimum ts and the one that has the max ts

tt2 = lapply(tt1, FUN = test_fl_onefish)

tt3 = do.call(rbind, tt2)

str(tt3) 

# make movements
test$movement = paste(dplyr::lag(test$GEN), test$GEN, sep = " - ")

a = test[!duplicated(test$GEN),] #Retain only the first appearances of 'arrival'

test[ , c("GEN", "arrival", "departure", "movement")]

a[ , c("GEN", "arrival", "departure", "movement")]

        a = a[order(a$arrival),] #Sort each sub-group by 'arrival'
        cbind(TagID = a$TagID[1], #obtain TagID, station, and ttime of the sub-group,
            Last_Station = a$Station[NROW(a)],
            ttime = (as.numeric(as.POSIXct(a$arrival[NROW(a)])) - as.numeric(releasetime))/(60*60*24))
        





# get route infor for fish ids
first_detects_routes <- merge(test, route, all.x = TRUE, by = "FishID") 
first_detects_routes <- first_detects_routes[!is.na(first_detects_routes$Route), ]


# pull movements from the matrix
first_detects_routes <-
  merge(
    first_detects_routes,
    dm_closed[, c("Name", "Total_Length_m")],
    by.x  = "movement",
    by.y = "Name",
    all.x = TRUE
  )

# Make sure that movements denote the day on which they arrive at the second location

first_detects_routes = first_detects_routes[order(first_detects_routes$FishID, first_detects_routes$first_det), ]

# calculate the number of days between first and second location

# insert distance row with NA for location but value for distance that equals total distance/days between first and second location.
