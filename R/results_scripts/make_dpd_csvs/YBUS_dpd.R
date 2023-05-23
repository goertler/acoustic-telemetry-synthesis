#-------------------------------------------------------#
# Distance Matrix calcs for YBUS data
# refactored, M. Johnston
# Tue Jun 28 10:41:48 2022 ------------------------------
library(telemetry)
source("R/utils.R")
#-------------------------------------------------------#

dm_ybus = read.csv("data/distance_matrices/Distance_Matrix_YBUS_corr_07_21.csv")
dm_ybus = dm_ybus[ , c("Name_corr", "Total_Length")]
colnames(dm_ybus) = c("Name", "Total_Length_m")
dm_ybus$Name = gsub("-", " - ", dm_ybus$Name)

## Load clean detections of interest
ybus = readRDS("data_clean/ybus/ybus_clean.rds") # created in R/clean_ybus.R
ybus$DetectDate = as.Date(ybus$DateTime_PST)

# from enviro_dat branch
#check these fishIDs
c("168.YBUS", "246.YBUS", "326.YBUS", "365.YBUS", "413.YBUS",
  "439.YBUS")
#-------------------------------------------------------#
# big test: all fish
f1 = split(ybus, ybus$FishID)
f1 = f1[sapply(f1, nrow) > 0] # only keep obs with > 1 det
=======


# all fish
f1 = ybus

f1 = split(f1, f1$FishID)

# from enviro_dat branch
f3_split = split(f2, lubridate::year(f2$Date))
#-------------------------------------------------------#
test = lapply(f1, dpd_allfish, distance_matrix = dm_ybus)


ans4 = lapply(test, hs)
  
ans5 = data.table::rbindlist(ans4, idcol = TRUE)
  
colnames(ans5) <- c("FishID", "date_time", "prop_dist")
  
head(ans5)

# from enviro_dat branch
mapply(write.csv,
       x = ans,
       file = paste0("results/YBUS/", names(ans), "_ybus_dpd.csv"),
       row.names = FALSE)
#-------------------------------------------------------#
write.csv(ans5, "results/YBUS/2016_ybus_dpd_refactor.csv")

