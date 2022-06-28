#-------------------------------------------------------#
# Distance Matrix calcs for YBUS data
# refactored, M. Johnston
# Tue Jun 28 10:41:48 2022 ------------------------------

source("R/utils.R")
library(telemetry)

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

f2 = lapply(f1, add_lag_col, order_by = 'DateTime_PST', 
            col_to_lag = 'DateTime_PST', 
            lagged_col_name = 'next_arrival')

f3 = lapply(f2, make_movements, col_to_lead = 'GEN', lagged_col_name = 'movement')

tt3 = lapply(f3, rm_nas_and_merge, dist_mat = dm_ybus, na_col = "next_arrival")

tt4 = lapply(tt3, function(x) x[x$Total_Length_m != 0, ])

ans3 = lapply(tt4, hs)

ans = data.table::rbindlist(ans3, idcol = TRUE)
str(ans)

ans = data.frame(ans)
head(ans)
colnames(ans) <- c("FishID", "date_time", "prop_dist")
write.csv(ans, "results/YBUS/2016_ybus_dpd_refactor.csv")
