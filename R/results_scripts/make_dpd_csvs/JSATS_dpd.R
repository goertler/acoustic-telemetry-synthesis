#-------------------------------------------------------#
# Distance Matrix calcs for JSATs data
# refactored, M. Johnston
# Mon Feb 15 11:52:30 2021 ------------------------------
library(telemetry)
source("R/utils.R")

# load distance matrix (using DCC closed only)
dm_closed  <- read.csv("data/distance_matrices/JSATs_dist_matrix_DCC-Yolo-Tisdale_closed_new.csv", stringsAsFactors = FALSE)

## Load clean JSATs detections of interest
jsats = readRDS("data_clean/JSATS/jsats_detects2013-2017.rds") #
jsats$DetectDate = as.Date(jsats$DateTime_PST)
csn(jsats)
key = read.csv("data/common_data/FishID_key.csv")
jsats = jsats[jsats$FishID %in% key$FishID, ]

#-------------------------------------------------------#

# big test: all fish
f1 = split(jsats, jsats$FishID)
f1 = f1[sapply(f1, nrow) > 0] # only keep obs with > 1 det

f2 = lapply(f1, dpd_allfish, distance_matrix = dm_closed)

ans4 = lapply(f2, function(x) try(hs(x)))
idx = sapply(ans4, is, "try-error")
lapply(f2[idx], hs) # only calling it with the ones that tripped an error
chk_f1 = f1[idx] # list of data frames that throw error

# see if there are NAs
lapply(chk_f1, function(x) csn(x)) # no NAS; they're being introduced by dpd_allfish

dpd_allfish(chk_f1[[9]], dm_closed) # this movement isn't in the matrix

chk = f2[idx]
lapply(chk, function(x) all(is.na(x[1, ]))) # do all of these have NAs in their first row? Y

test = f2[["FR-Delta2015-194"]]
div_dist(
         start = test$DateTime_PST, 
         end = test$next_arrival, 
         distance = test$Total_Length_m, 
         time_units = "hour")

hs(test)

names(idx[1])
chk = names(idx[idx])
plot(jsats$RKM[jsats$FishID == chk[7]])
csn(jsats[jsats$FishID == chk[8], ])

ans5 = data.table::rbindlist(ans4, idcol = TRUE)

colnames(ans5) <- c("FishID", "date_time", "prop_dist")

head(ans5)

write.csv(ans5, "results/JSATS/jsats_dpd_refactor.csv")

