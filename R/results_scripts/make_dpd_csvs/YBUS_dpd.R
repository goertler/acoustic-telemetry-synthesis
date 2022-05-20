#-------------------------------------------------------#
# Distance Matrix calcs for YBUS data
# refactored, M. Johnston
# Fri Apr 15 12:36:50 2022 ------------------------------

source("R/utils.R")
library(telemetry)
library(data.table)
#-------------------------------------------------------#

dm_ybus = read.csv("data/distance_matrices/Distance_Matrix_YBUS_corr_07_21.csv")
dm_ybus = dm_ybus[ , c("Name_corr", "Total_Length")]
colnames(dm_ybus) = c("Name", "Total_Length_m")
dm_ybus$Name = gsub("-", " - ", dm_ybus$Name)

## Load clean detections of interest
ybus = readRDS("data_clean/ybus/ybus_clean.rds") # created in R/clean_ybus.R
ybus$DetectDate = as.Date(ybus$DateTime_PST)

#check these FishIDs
# chk = c("168.YBUS", "246.YBUS", "326.YBUS", "365.YBUS", "413.YBUS",
#   "439.YBUS")

#-------------------------------------------------------#
if(FALSE) {
  f1 = tag_tales(ybus, 
               TagID_col = "FishID", 
               Station_col = "GEN", 
               Datetime_col = "DateTime_PST",
               Threshold = 60*60*3, # 3hr visit threshold
               allow_overlap = FALSE)
  saveRDS(f1, "data_clean/YBUS/ybus_tagtales.rds")
  
}

f1 = readRDS("data_clean/YBUS/ybus_tagtales.rds")


if(FALSE) { # pre-refactor - dormant until after refactor, # Fri May 20 12:46:04 2022 ------------------------------
  
# big test: all fish
f1 = f1[sapply(f1, nrow) > 0] # only keep obs with > 1 det

f2 = lapply(f1, dpd_allfish, distance_matrix = dm_ybus)

f2 = do.call(rbind, f2)

f3_split = split(f2, lubridate::year(f2$Date))

ans = lapply(f3_split, make_matrix)

mapply(write.csv,
       x = ans,
       file = paste0("results/YBUS/", names(ans), "_ybus_dpd.csv"),
       row.names = FALSE)


## debugging movements
View(ans$`2016`[ans$`2016`$FishID %in% chk, ])
View(ybus[ybus$FishID == chk[2], ])
# debugging dpd_allfish
chk1 = ybus[ybus$FishID == chk[2], ]
debug(assign_station_visits) # this all looks fine - move on to next fxn

debugonce(make_movements)
chk2 = assign_station_visits(chk1)
chk3 = make_movements(chk2, distance_matrix = dm_ybus)

debugonce(calc_dist_per_day)
chk4 = calc_dist_per_day(chk3)

debugonce(pad_days)
pad_days(chk4)
}
