#-------------------------------------------------------#
# Distance Matrix calcs for YoloAce data
# refactored, M. Johnston
# Fri Feb 25 08:48:44 2022 ------------------------------
library(telemetry)
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

f2 = lapply(f1, dpd_allfish, distance_matrix = dm_yoloace)

ans = lapply(f2, hs)

ans5 = data.frame(data.table::rbindlist(ans, idcol = TRUE))
colnames(ans5) <- c("FishID", "date_time", "prop_dist")

write.csv(ans5, "results/YoloAce/yoloace_dpd_refactored.csv")