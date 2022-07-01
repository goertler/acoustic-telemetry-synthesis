#-------------------------------------------------------#
# Distance Matrix calcs
# refactored, M. Johnston
# Thu Feb 24 14:15:03 2022 ------------------------------
library(telemetry)
source("R/utils.R")
#-------------------------------------------------------#
# Objective:  using the detections and the distance matrices appropriate to a fish's route, calculate the distance traveled by each fish on each day.  

# If detections are separated by many days, spread the distance evenly across the interval.

# Input needed: cleaned detections data frame with FishID, DateTime_PST, GEN, Year, and RKM; Distance matrix dataframe with columns "Name" (movement name) and "Distance_m"

# Final Final Output needed: tabular form, column for each FishID, row for each day, distance (abs(distance_traveled_in_meters))) in each cell. Different files for each year - 3 years (2013, 2016, 2017) - water year is fine.

#-------------------------------------------------------#
v2 = readRDS("data_clean/CMVemco/v2.rds") 
mat = readRDS("data_clean/CMVemco/CM_vemco_distance_matrix_DCC-Yolo-Tisdale_closed_clean.rds")
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

ans4 = lapply(f2, hs)

ans5 = data.table::rbindlist(ans4, idcol = TRUE)

colnames(ans5) <- c("FishID", "date_time", "prop_dist")

head(ans5)

write.csv(ans5, "results/CMVemco/cmvemco_dpd_refactor.csv")