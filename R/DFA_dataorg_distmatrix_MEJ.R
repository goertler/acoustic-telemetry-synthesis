#-------------------------------------------------------#
# Distance Matrix calcs
# refactored, M. Johnston
# Thu Jul 23 14:39:06 2020 ------------------------------

source("R/utils.R")

#-------------------------------------------------------#
# Objective:  using the detections and the distance matrices appropriate to a fish's route, calculate the distance traveled by each fish on each day.  

# If detections are separated by many days, spread the distance evenly across the interval.

# Output: tabular form, column for each FishID, row for each day, distance (abs(distance_traveled_in_meters))) in each cell.

## Previous approach: 

### -- load detections and merge with tagging metadata.  Rename some receiver locations ("GEN") to fit with what's in the distance matrices.

### -- order detections by ID and date; create lagged detection columns for "previous FishID", "previous LOC", and "previous detect date time".

### -- filter down to the "first detection" at each receiver, and create a movement column with each fish's first movement (Release -> rec1, rec1 -> rec2, etc)

### -- join this table of first detections with the "routes" index, to get route info for each fish

### -- pipe operations - unreliable/opaque

#########################################################
#-------------------------------------------------------#
# Re-factored approach to creating distance traveled matrix

# Filter down to only fish detected to either Benicia or Chipps.  No vemco receivers at Benicia (ybus data and my fish).  JSATs: ChippsE, ChippsW, or Benicia.

# rename some receiver locations to agree with the names in the distance matrices

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

## Load and clean detections; merge with tagging metadata; select relevant columns
source("R/clean_all_detects.R") # creates the all_detects object; sources clean_tagging_metadata.R

#########################################################
# Create lagged dataframe of detections:
### shifts the three columns down by 1; the first row will be an NA
all_detects[, c("prev_FishID", "prev_GEN", "prev_DetectDate")] <- 
  as.data.frame(
      data.table::shift(
           x = all_detects[, c("FishID", "GEN", "DetectDate")], # this, lagged by 1
           n = 1,
          fill = NA,
          type = "lag"
                        )
                )
#########################################################
# make data frame of first detection at new receivers; does this by only keeping the rows where there is a new fish (cell doesn't match the previous ID)  OR a new receiver

first_detects <-
  all_detects[which(
    all_detects$FishID != all_detects$prev_FishID |
      all_detects$GEN != all_detects$prev_GEN), ]   #  first detections or first detections at a new receiver


# make movement column; this is needed in order to merge with the distance matrix later.
first_detects$movement <-
  paste(first_detects$prev_GEN, "-", 
        first_detects$GEN, sep = " ")


## Selecting relevant columns to bind: joining routes and first detects
stopifnot(diff(len(route$FishID), len(first_detects$FishID)) != 0) # if we're going to join these, they need to have the same number of fish

route = route[ , c("FishID", "Route")]
stopifnot(sum(duplicated(route$FishID)) == 0) # make sure each row is distinct

## Joining the routing determination to the detection data
## and removing fish that have no routing determination

first_detects_routes <- merge(first_detects, route, all.x = TRUE, by = "FishID") 
first_detects_routes <- first_detects_routes[!is.na(first_detects_routes$Route), ]

len(first_detects_routes$FishID) #748fish- was 868
summary(tapply(first_detects_routes$movement, first_detects_routes$FishID, len))
# ~20 moves per fish on average
len(first_detects_routes$GEN) # 96 represented locations

#-------------------------------------------------------#
# Merge distance matrix with detection routes

# checks that all movements in the first_detects_routes df are found in the distance matrix
stopifnot(sum(unique(first_detects_routes$movement) %in% dm_closed$Name) == len(first_detects_routes$movement))

# bring in total length in meters of each movement
first_detects_routes <-
  merge(
    first_detects_routes,
    dm_closed[, c("Name", "Total_Length_m")],
    by.x  = "movement",
    by.y = "Name",
    all.x = TRUE
  )


# order data frame by FishID, then by detection date/time
first_detects_routes <-
  first_detects_routes[order(first_detects_routes$FishID,
                             first_detects_routes$DetectDate), ]

# insert 0s for rows between individual fish
first_detects_routes[which(first_detects_routes$FishID != first_detects_routes$prev_FishID), 
                     "Total_Length_m"] <- 0 

# add date and water year columns; subset down to water years of interest
first_detects1 = add_date_wy_and_subset(first_detects_routes, "DetectDate", 
                                        wyears = c(2013, 2014, 2017))

# adds a cumulative total length traveled by fish
rm(`%>%`)

library(dplyr)

first_detects1  %>%
  group_by(FishID) %>%
  arrange(DetectDate) %>% 
  mutate(sum_tot_length = cumsum(Total_Length_m)) %>% 
  ungroup() %>% 
  arrange(FishID, DetectDate) -> first_detects1

first_detects1 = data.frame(first_detects1)


#### QAQC checks 
len(first_detects1$FishID) # 553 fish total
tapply(first_detects1$FishID, first_detects1$wateryr, len)
tapply(first_detects1$Date, first_detects1$wateryr, range)
summary(tapply(first_detects1$movement, first_detects1$FishID, len)) 
summary(tapply(first_detects1$sum_tot_length, first_detects1$FishID, mean))
summary(tapply(first_detects1$sum_tot_length, first_detects1$FishID, len))

library(ggplot2)

ggplot(first_detects1, aes(x = DetectDate, y = sum_tot_length/1000)) +
  geom_point(alpha = 0.15, aes(color = Fish_Type)) +
  facet_wrap(~wateryr, scales = "free", ncol = 1) + 
  theme_minimal()# only 1 fish type in 2014?


saveRDS(first_detects1, "data_clean/first_detects1.rds") # last object prior to distance calculations

# Starting pt halfway through analysis
# Wed Jan  6 14:32:12 2021 ------------------------------

# start with a dataframe of FishID, Date, movement, total_length, and sum_tot_length.

# some fish have one or more movements on a single day; some have none.
# each movement is associated with a particular distance, which we pulled from the distance matrix.
# Might be easier to look at total distance travelled by a fish and spread it out evenly, but that would smooth over the variation that we're interested in; need take it on a movement-by-movement basis.

# still, let's start there; let's pretend that a fish goes 100 miles total.  The movement starts on 2017-05-25 and finishes on 2017-06-08, but they're only detected on two days in there (beginning and end).

# to spread out the movement, you would calculate how many days there are, and how many miles, and then divide the miles by the number of days:

100/as.numeric(difftime(as.Date("2017-06-08"), as.Date("2017-05-25")))

# to do this in a dataframe of movements:

# on 2017-05-27, fish went 
View(first_detects1[first_detects1$FishID == "ARF2017-005", ])

# Goal: end with a dataframe where fishID is a row, and each day is a column; the entry for each day should be a linear approximation of movement, as derived from the detection histories.


## For 1 fish:
# step1: add in all missing dates for individual fish
# step2: enter 0 for Total Length traveled on those new dates
# step3: replace the 0s by the quotient of the abs()

first_detects1 = readRDS("data_clean/first_detects1.rds")
# Next step: calculate daily distance traveled by each fish
d1 = first_detects1[ , c("FishID", "Date", "movement", "Total_Length_m", "sum_tot_length")]

# movements with 0
d1 = d1[d1$Total_Length_m != 0 , ]

test = subset(d1, FishID == "ARF2017-005")

pad_dates1fish = function(df) {
  
  df = test # function testing
  splitdf = padr::pad(df, interval = "day", group = "FishID") # counts every GoldenGate detection as a movement of 826 meters; can't be what we want
  splitdf$Total_Length_m[is.na(splitdf$Total_Length_m)] <- 0
  # splitdf[[cml_sum_col]] = 
  
}

test2 = pad_dates1fish(test, "sum_tot_length")
str(test2)

#########################################################
# End point for refactor so far - M.J.
#########################################################




#write.csv(dfa.ybus,"results/ybus_DistTravelbyday_mat_v2020.csv", row.names=FALSE)
