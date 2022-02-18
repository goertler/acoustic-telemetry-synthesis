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
if(FALSE) { # ran on 2022-02-17; need to add to makefile
# inputs, made in R/clean_CM_Vemco_data.R
v2 = readRDS("data_clean/v2.rds") 
mat = readRDS("data_clean/CM_vemco_distance_matrix_DCC-Yolo-Tisdale_closed_clean.rds")
#-------------------------------------------------------#
# Small tests
test1 = dpd_allfish(detdf = v2[v2$FishID == unique(v2$FishID)[2], ],
                    distance_matrix = mat)
test = dpd_allfish(detdf = v2[v2$FishID == "LFC0687", ], 
                   distance_matrix = mat)

# big test: all fish
f1 = split(v2, v2$FishID)
f1 = f1[sapply(f1, nrow) > 0] # only keep obs with > 1 det

f2 = lapply(f1, dpd_allfish, distance_matrix = mat)

f2 = do.call(rbind, f2)
saveRDS(f2, "data_clean/CJVemco_distance_per_day2022-02-17.rds")
}
#-------------------------------------------------------#

# split into different years, make a data frame with days as columns
# columns should go from minimum of data series for that year to maximum, with padded days in between for missing days and NAs for the fish with no data on those days
f2 = readRDS("data_clean/CJVemco_distance_per_day2022-02-17.rds")

# subset the dataset to only the years we need
years = 2007:2009

f3 = subset(f2, lubridate::year(f2$Date) %in% years)

f3_split = split(f3, lubridate::year(f3$Date)) # can split by anything - whatever we need

ans = lapply(f3_split, make_matrix)

mapply(write.csv, x = ans, file = paste0("results/CJVemco_DFA_", names(ans), ".csv"), row.names = FALSE)

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

