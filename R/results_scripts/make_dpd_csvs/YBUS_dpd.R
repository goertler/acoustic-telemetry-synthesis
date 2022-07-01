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


# all fish
f1 = ybus

f1 = split(f1, f1$FishID)

test = lapply(f1, dpd_allfish, distance_matrix = dm_ybus)

ans4 = lapply(test, hs)
  
ans5 = data.table::rbindlist(ans4, idcol = TRUE)
  
colnames(ans5) <- c("FishID", "date_time", "prop_dist")
  
head(ans5)

write.csv(ans5, "results/YBUS/2016_ybus_dpd_refactor.csv")
