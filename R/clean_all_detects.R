#-------------------------------------------------------#
# Fix GEN locations in all_detects
# M. Johnston - from T. Grimes & C. Michel's code; has not been altered
# DFA analysis
# Fri Jul 24 14:01:50 2020 ------------------------------

# this script gets sourced in line 30 of the DFA_dataorg_distmatrix_MEJ.R script
# calls: R/clean_tagging_metadata
source("R/utils.R")
#-------------------------------------------------------#
if(FALSE){
# OLD METHOD: build from raw detection files
  ## Load detections
temp = list.files(path = "data/detection_data/study-detections-JSATS", 
                  full.names = TRUE, 
                  pattern="*.csv") 

## Bind all detections into one mother dataframe
all_detects <- do.call("rbind", lapply(temp, 
                                       read.csv, 
                                       stringsAsFactors = FALSE))

# fix date formats
all_detects$DetectDate <- as.POSIXct(all_detects$DateTime_PST, 
                                       tz = "Etc/GMT+8", 
                                      format = "%m/%d/%Y %H:%M:%OS")
}
#-------------------------------------------------------#

# NEW METHOD: use Pascale's pre-made .csv (not uploaded to github b/c 550MB)
all_detects = data.table::fread("data/detection_data/all_JSATS.csv")
all_detects = as.data.frame(all_detects)

all_detects$DetectDate = as.Date(as.POSIXct(all_detects$DateTime_PST,
                                            tz = "Etc/GMT+8",
                                            format = "%m/%d/%Y %H:%M:%OS"))

sort(unique(all_detects$DetectDate))
csn(all_detects) # problem is not in the dates, but the times

all_detects$DateTime_PST = as.POSIXct(all_detects$DateTime_PST, 
                                       tz = "Etc/GMT+8", 
                                      format = "%m/%d/%Y %H:%M:%OS")

csn(all_detects)

# load and clean tagging metadata
source("R/clean_tagging_metadata.R") # creates tagging_meta object

## Merge in pertinent data for grouping by runs/years
all_detects <-
  merge(all_detects, tagging_meta[, c("FishID", 
                                      "StudyID", 
                                      "Fish_Type", 
                                      "Rel_loc", 
                                      "Rel_rkm")], 
        by = "FishID",
        all.x = TRUE)

# Get rid of unused columns
all_detects  = all_detects[ , c("FishID",
                                "DateTime_PST",
                                "StudyID",
                                "GEN",
                                "RKM",
                                "Fish_Type",
                                "Rel_loc",
                                "Rel_rkm"
                                )]


## Remove detections at some key locations which seem to have a lot of false detects
all_detects <-
  all_detects[!all_detects$GEN %in% c(
    "Battle_Conf",
    "ButteBr",
    "AbvColusaBrRT",
    "BattleCk2",
    "BattleCk3",
    "Sac_Ist_Bridge_Rel",
    "MillCk2_Rel",
    "FR_Vance_Rel",
    "MinerNorth"
  ), ] # added Sac_Ist_Bridge_Rel,MillCk2_Rel,and FR_Vance_Rel because they are not in the distance matrix

## Remove detections for certain fish, seems to have bad data (maybe 2 tags in system with that tag code? Or just false detects)
all_detects <- all_detects[!all_detects$FishID %in% c(
  "CFC2013-080",
  "CFR2016-079"
  ), ]

## Now fix some GEN loc names that don't exactly match in detection files vs dist matrices
all_detects[which(all_detects$GEN == "BattleCk_RST"), "GEN"] <-
  "BattleCk_RST_Rel"
all_detects[which(all_detects$GEN == "FR Gridley Release"), "GEN"] <-
  "Gridley_Rel"
all_detects[which(all_detects$GEN == "RBDD Release"), "GEN"] <-
  "RedBluffDivDam"
## Fixing more GEN loc names that don't match
all_detects[which(all_detects$GEN == "MillCk_RST"), "GEN"] <-
  "MillCk_RST_Rel"
all_detects[which(all_detects$GEN == "I80-50_Br"), "GEN"] <-
  "I80_Br"
all_detects[which(all_detects$GEN == "BeniciaW"), "GEN"] <-
  "Benicia"
all_detects[which(all_detects$GEN == "BlwGeorg_1"), "GEN"] <-
  "BlwGeorgiana"
all_detects[which(all_detects$GEN == "SutterBypass_Weir2_RST_Rel"), "GEN"] <-
  "SutterBypass Weir2 RST"
all_detects[which(all_detects$GEN == "GeorgSl_1"), "GEN"] <-
  "Georgiana_Slough"
all_detects[which(all_detects$GEN == "FR_Gridley_Rel"), "GEN"] <-
  "Gridley_Rel"
all_detects[which(all_detects$GEN == "RBDD_Rel"), "GEN"] <-
  "RedBluffDivDam"
all_detects[which(all_detects$GEN == "RBDD1"), "GEN"] <-
  "RedBluffDivDam"
all_detects[which(all_detects$GEN == "RBDD2"), "GEN"] <-
  "RedBluffDivDam"
all_detects[which(all_detects$GEN == "FR_Boyds_Rel"), "GEN"] <-
  "FR Boyds Release"
all_detects[which(all_detects$GEN == "Tower_Bridge_Rel"), "GEN"] <-
  "TowerBridge"
all_detects[which(all_detects$GEN == "AR_Sunrise_Ramp_Rel"), "GEN"] <-
  "AR_Sunrise_Ramp"
all_detects[which(all_detects$GEN == "DeerCk_RST_Rel"), "GEN"] <-
  "DeerCk_RST"
all_detects[which(all_detects$GEN == "FreeportDiv"), "GEN"] <-
  "Freeport"

## combine certain locations where receivers are too close together
all_detects[which(all_detects$GEN == "ChippsE"), "GEN"] <- "ChippsW" # this is how it is in the dist matrix
all_detects[which(all_detects$GEN == "GoldenGateE"), "GEN"] <- "GoldenGateW"

## remove mokbase since it is one site we don't have distance matrix for
all_detects <- all_detects[which(all_detects$GEN != "MokBase"),]

all_detects$Year = lubridate::year(all_detects$DateTime_PST)
len(all_detects$FishID[all_detects$GEN == "Benicia" & all_detects$Year == 2016])


#-------------------------------------------------------#
# Subset down to the years and fish we need for the DFA:

all_detects = all_detects[all_detects$Year %in% c(2013, 2016, 2017), ]

DFAids = unique(all_detects$FishID[all_detects$GEN %in% c("Benecia", "ChippsW")]) # 575 fish

dfa_detects = all_detects[(all_detects$FishID %in% DFAids), ]

write.csv(data.frame(FishID = DFAids), "results/JSATS_FishIDs_for_DFA_analysis.csv", row.names = FALSE) # for Pascale

saveRDS(dfa_detects, "data_clean/jsats_dfa_detects.rds")

# # prepare lagged columns
# all_detects$prev_FishID <- NA
# all_detects$prev_GEN <- NA
# all_detects$prev_DetectDate <- NA



#########################################################
if(FALSE){
# test lag shift
tt = subset(all_detects, FishID %in% c("ARF2016-114", "SB2017-167", "WR2017-555"))

tt[, c("prev_FishID", "prev_GEN", "prev_DetectDate")] <- # populate this with...
  as.data.frame(
      data.table::shift(
           x = tt[, c("FishID", "GEN", "DetectDate")], # this, lagged by 1
           n = 1,
          fill = NA,
          type = "lag"
                        )
                )

tt = tt[ , c(1, 6, 8, 12:14)]
# vet(tt, 12) # visually inspected to see if the lag worked; not a great test


# this should give us only the first detections of each fish or the first detection at a new rec
ttfd <-
  tt[which(
    tt$FishID != tt$prev_FishID |
      tt$GEN != tt$prev_GEN
  ),]   #  first detections or first detections at a new receiver; by fish, each row should have a different receiver than the last one


# make movement column; this is needed in order to merge with the distance matrix later.
ttfd$movement <-
  paste(ttfd$prev_GEN, "-", 
        ttfd$GEN, sep = " ")

library(dplyr)

route <- route %>%
  select(FishID, Route) %>%
  distinct()

## Joining the routing determination to the detection data
## and removing fish that have no routing determination

first_detects_routes <-
  left_join(ttfd, route, by = "FishID") %>%
  filter(!is.na(Route))

fdr <-
  merge(
    first_detects_routes,
    dm_closed[, c("Name", "Total_Length_m")], # only merging the total length
    by.x  = "movement",
    by.y = "Name",
    all.x = TRUE # we're keeping all rows from x, not y
  )

# end with a df where each row is a fish's movement with a distance associated

#/end test lag shift
}

