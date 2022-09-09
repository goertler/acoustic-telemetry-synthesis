# Tests
library(telemetry)
source("R/utils.R")

d = readRDS("data_clean/YBUS/ybus_clean.rds")
x = subset(d, FishID == "10.YBUS")

#saveRDS(x, "~/NonDropboxRepos/telemetry/inst/10.ybus_test_data.rds")

y = tag_tales(x, "FishID", "GEN", Datetime_col = "DateTime_PST")

dm_ybus = read.csv("data/distance_matrices/Distance_Matrix_YBUS_corr_07_21.csv")
dm_ybus = dm_ybus[ , c("Name_corr", "Total_Length")]
colnames(dm_ybus) = c("Name", "Total_Length_m")
dm_ybus$Name = gsub("-", " - ", dm_ybus$Name)


# test on one
chk2 = add_lag_col(x, 
                   order_by = 'DateTime_PST', 
                   col_to_lag = 'DateTime_PST' ,  
                   lagged_col_name = 'next_arrival')

head(chk2)
chk3 = make_movements(chk2, 'GEN', 'movement')
head(chk3)
chk4 = rm_nas_and_merge(chk3, dist_mat = dm_ybus)
head(chk4)
chk4.5 = chk4[chk4$Total_Length_m != 0, ]

chk5 = mapply(div_dist,
              start = chk4.5$DateTime_PST,
              end = chk4.5$next_arrival,
              distance = chk4.5$Total_Length_m,
              time_units = "hour",
              SIMPLIFY = FALSE)


head(chk5)

# check days
chk6 = mapply(div_dist,
                   start = chk4.5$DateTime_PST,
                   end = chk4.5$next_arrival,
                   distance = chk4.5$Total_Length_m,
                   time_units = "day",
                   SIMPLIFY = FALSE)


all.equal(sum(chk5$prop_dist), sum(chk4.5$Total_Length_m), tolerance = 0.001) # might want to fix eventually; can end up way off with additive small differences.


# all fish
f1 = d

f1 = split(f1, f1$FishID)

f2 = lapply(f1, add_lag_col, order_by = 'DateTime_PST', 
            col_to_lag = 'DateTime_PST', 
             lagged_col_name = 'next_arrival')

f3 = lapply(f2, make_movements, col_to_lead = 'GEN', lagged_col_name = 'movement')

tt3 = lapply(f3, rm_nas_and_merge, dist_mat = dm_ybus, na_col = "next_arrival")

tt4 = lapply(tt3, function(x) x[x$Total_Length_m != 0, ])

ans3 = lapply(tt4, function(x) try(hs(x)))

lapply(ans3, tail)

##-----------------------------------------

# load distance matrix (using DCC closed only)
dm_closed  <- read.csv("data/distance_matrices/JSATs_dist_matrix_DCC-Yolo-Tisdale_closed_new.csv", stringsAsFactors = FALSE)

## Load clean JSATs detections of interest
jsats = readRDS("data_clean/JSATS/jsats_detects2013-2017.rds") #
jsats$DetectDate = as.Date(jsats$DateTime_PST)
#-------------------------------------------------------#

# big test: all fish
f1 = split(jsats, jsats$FishID)
f1 = f1[sapply(f1, nrow) > 0] # only keep obs with > 1 det

ans = lapply(f1, add_lag_col, "DateTime_PST", "DateTime_PST", "new_arrival")

f2 = lapply(f1, dpd_allfish, distance_matrix = dm_closed)

"Tisdale2013-022"

mapply(div_dist, 
       start = x$DateTime_PST, 
       end = x$next_arrival, 
       distance = x$Total_Length_m, 
       time_units = "hour",
       SIMPLIFY = FALSE)

