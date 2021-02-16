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
tt3 = tt3[!duplicated(tt3$visitID, fromLast = TRUE), ] # keeps departure
str(tt3) 

# make movements
tt3$movement = paste(dplyr::lag(tt3$GEN), tt3$GEN, sep = " - ")

# get route infor for fish ids
first_detects_routes <- merge(tt3, route, all.x = TRUE, by = "FishID") 
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

first_detects_routes = first_detects_routes[order(first_detects_routes$FishID, first_detects_routes$DateTime_PST), ]

# calculate the number of days between first and second location

first_detects_routes$Date = as.Date(first_detects_routes$DateTime_PST)

library(dplyr)
ff = first_detects_routes
ff %>% 
  group_by(FishID, Date) %>% 
  summarise(tot_distance = sum(Total_Length_m)) %>% 
  ungroup() -> ff

ff = as.data.frame(ff)
ff$timdiff = as.numeric(abs(difftime(dplyr::lag(ff$Date), ff$Date, units = "days")))

ff = ff[!is.na(ff$tot_distance), ]
dates = as.Date(padr::pad(ff, interval = "day")$Date)

ff$dist_day = ff$tot_distance/ff$timdiff

dists = rep(ff$dist_day, ff$timdiff)

fin = data.frame(FishID = ff$FishID[1],
           Date = dates, 
           Distance_m = dists)

sum(fin$Distance_m) == sum(ff$tot_distance) #hs it kinda worked
