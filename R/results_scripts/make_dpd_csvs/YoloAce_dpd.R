#-------------------------------------------------------#
# Distance Matrix calcs for YoloAce data
# refactored, M. Johnston
# Fri Feb 25 08:48:44 2022 ------------------------------

source("R/utils.R")
#-------------------------------------------------------#

dm_yoloace = read.csv("data/distance_matrices/Distance_Matrix_MJ_corr_mean.csv")
dm_yoloace = dm_yoloace[ , c("Name_corr", "mean_Total_Length")]
colnames(dm_yoloace) = c("Name", "Total_Length_m")
dm_yoloace$Name = gsub("-", " - ", dm_yoloace$Name)

## Load clean detections of interest
yolo_ace = readRDS("data_clean/YoloAce/yoloace_dfa_detects.rds") # created in R/clean_yolo_ace.R
yolo_ace$DetectDate = as.Date(yolo_ace$DateTime_PST)
yolo_ace$Rel_rkm = yolo_ace$Rkm
#-------------------------------------------------------#
# big test: all fish
f1 = split(yolo_ace, yolo_ace$FishID)
f1 = f1[sapply(f1, nrow) > 0] # only keep obs with > 1 det

f2 = lapply(f1, dpd_allfish, distance_matrix = dm_yoloace)

f2 = do.call(rbind, f2)

# subset the dataset to only the years we need
years = c(2012, 2013)

f3 = subset(f2, lubridate::year(f2$Date) %in% years)

f3_split = split(f3, lubridate::year(f3$Date)) 

ans = lapply(f3_split, make_matrix)

mapply(write.csv, 
       x = ans, 
       file = paste0("results/YoloAce/distance_per_day/", names(ans), "_yoloace_dpd.csv"), 
       row.names = FALSE)
