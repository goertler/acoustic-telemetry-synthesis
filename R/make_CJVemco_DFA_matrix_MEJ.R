#-------------------------------------------------------#
# Distance Matrix calcs
# refactored, M. Johnston
# Mon Feb 15 11:52:30 2021 ------------------------------
source("R/utils.R")
#-------------------------------------------------------#
# Objective:  using the detections and the distance matrices appropriate to a fish's route, calculate the distance traveled by each fish on each day.  

# If detections are separated by many days, spread the distance evenly across the interval.

# Input needed: cleaned detections data frame with FishID, DateTime_PST, GEN, Year, and RKM; Distance matrix dataframe with columns "Name" (movement name) and "Distance_m"

# Final Final Output needed: tabular form, column for each FishID, row for each day, distance (abs(distance_traveled_in_meters))) in each cell. Different files for each year - 3 years (2013, 2016, 2017) - water year is fine.

#-------------------------------------------------------#
v2 = readRDS("data_clean/v2.rds") 
mat = readRDS("data_clean/CM_vemco_distance_matrix_DCC-Yolo-Tisdale_closed_clean.rds")
#-------------------------------------------------------#
# Small tests
test1 = dpd_allfish(detdf = v2[v2$FishID == unique(v2$FishID)[2], ],
                    distance_matrix = mat)
test = dpd_allfish(detdf = v2[v2$FishID == "LFC1508", ], 
                   distance_matrix = mat)

# big test: all fish
f1 = split(v2, v2$FishID)
f1 = f1[sapply(f1, nrow) > 0] # only keep obs with > 1 det

f2 = lapply(f1, dpd_allfish, distance_matrix = mat)

f2 = do.call(rbind, f2)


# subset the dataset to only the years we need
years = 2007:2011

f3 = subset(f2, lubridate::year(f2$Date) %in% years)

f3_split = split(f3, lubridate::year(f3$Date)) # can split by anything - whatever we need

ans = lapply(f3_split, make_matrix)

mapply(write.csv, 
       x = ans, 
       file = paste0("results/CJVemco_DFA_", names(ans), ".csv"), 
       row.names = FALSE)
