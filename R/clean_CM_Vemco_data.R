# M. Johnston
# Clean CJVemco_distance matrix
# Fri Feb 11 09:26:16 2022 ------------------------------

source("R/utils.R")
library(lubridate)

## In the clean_all_detects.R script:
 # - only need fish (JSATS) from: 2013, 2016, and 2017
 # - only need fish (all groups) that reach either Ben or Chipps recs
 # - End Recs: "BeniciaW"  "ChippsW"
 # - Rename some receiver locations to agree with the names in the distance matrices

# load distance matrix (emailed by CJM on 2021-09-)
mat <- read.csv("data/distance_matrices/Vemco_dist_matrix_DCC-Yolo-Tisdale_closed.txt",
                         sep =  ",",
                         header = TRUE)

# add in battle creek movement that's missing from matrix
battle = data.frame(ObjectID = range(mat$ObjectID)[2] + 1,
                    Name = as.character("BtlCkAbatPnd - BattleCk10"),
                    OriginID = NA,
                    DestinationID = NA,
                    DestinationRank = NA,
                    Total_Length = as.numeric(6893.039)) # from Pascale, 2/17

mat = rbind(mat, battle)
# chk - this should be there:
mat[mat$Name == "SR_OrdBend - SR_I-80/50Br", ]
mat[mat$Name == "BtlCkAbatPnd - BattleCk10", ]
mat$Total_Length_m = mat$Total_Length # rename so that the dpd_allfish fxn works

# matrix data checks
summary(mat$Total_Length)
stopifnot(sum(rowSums(is.na(mat))) == 3) # there should be exactly 3 NAs (the ones above)

saveRDS(mat, "data_clean/CM_vemco_distance_matrix_DCC-Yolo-Tisdale_closed_clean.rds")

## Load detections of interest
v = as.data.frame(data.table::fread("data/detection_data/Query3.csv")) # uploaded to Sharepoint by Pascale; see README
v$DateTime_PST = force_tz(mdy_hms(v$DetectDate), tzone = "Etc/GMT+8")
v$Date_Released = force_tz(mdy_hms(v$Date_Released), tzone = "Etc/GMT+8") # Release detection
v$DetectDate = force_tz(mdy_hms(v$DetectDate), tzone = "Etc/GMT+8") # re-format
stopifnot(identical(v$DateTime_PST, v$DetectDate))

loc = read.csv("data_clean/exclude_loc_clean.csv") # from Pascale on 2/2/22

loc.keep = loc$General_Location[loc$exclude == "N"]
stopifnot(length(loc.keep) == sum(loc.keep %in% unique(v$General_Location)))
v = v[v$General_Location %in% loc.keep, ]

# TagIDs to keep
keep = read.csv("data/travel_time/travel.time_CM.Vemco_v3.csv") # 296 fish
keep = keep[ , c("FishID", "Release_Location", "Riverkm")]

keepID = unique(keep$FishID)
stopifnot(len(keepID) == sum(keepID %in% unique(v$FishID))) # make sure all the ones we want are found in the detection files

# subset down to only these fish
v = v[v$FishID %in% keepID, ]

# add release detection
# build release detection df
reldet = merge(keep, v[ !duplicated(v$FishID) , c("FishID", "Date_Released")]) # Bring in date_released col
table(reldet$Release_Location, reldet$Riverkm)

# re-structure dataframe to match v
reldet$Lat = reldet$Lon = 0
reldet$DateTime_PST = reldet$DetectDate = reldet$Date_Released
reldet$General_Location = reldet$Location = reldet$Release_Location

sum(!(colnames(reldet) %in% colnames(v)))
identical(sort(colnames(v)) , sort(colnames(reldet)))

# place them in the same order
vnames = colnames(v)
reldet = reldet[ , vnames]

# add release detection df
v2 = rbind(v, reldet)
v2$GEN = v2$General_Location
v2$RKM = v2$Riverkm

# test data frame:
saveRDS(v2, "data_clean/v2.rds")
