# M. Johnston
# Clean CJVemco_distance matrix
# Wed Feb 23 10:34:55 2022 ------------------------------

source("R/utils.R")
library(lubridate)

## Load detections of interest
v = as.data.frame(data.table::fread("data/detection_data/Query3.csv")) # uploaded to Sharepoint by Pascale; see README
v$DateTime_PST = force_tz(mdy_hms(v$DetectDate), tzone = "Etc/GMT+8")
v$Date_Released = force_tz(mdy_hms(v$Date_Released), tzone = "Etc/GMT+8") # Release detection
v$DetectDate = force_tz(mdy_hms(v$DetectDate), tzone = "Etc/GMT+8") # re-format
stopifnot(identical(v$DateTime_PST, v$DetectDate))

loc = read.csv("data_clean/CMVemco/exclude_loc_clean.csv") # from R/cleaning_scripts/CMVemco/clean_CMVemco_dm.R

loc.keep = loc$General_Location[loc$exclude == "N"]
stopifnot(length(loc.keep) == sum(loc.keep %in% unique(v$General_Location)))
v = v[v$General_Location %in% loc.keep, ]

# TagIDs to keep
keep = read.csv("data/travel_time/travel.time_CM.Vemco_v3.csv") # 296 fish, 2007-2011
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
saveRDS(v2, "data_clean/CMVemco/v2.rds")
